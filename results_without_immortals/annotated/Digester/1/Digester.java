package org.apache.commons.digester3;

import static java.lang.String.format;
import java.io.File;
import java.io.FileInputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.Reader;
import java.lang.reflect.InvocationTargetException;
import java.net.MalformedURLException;
import java.net.URL;
import java.net.URLConnection;
import java.util.ArrayList;
import java.util.Collections;
import java.util.EmptyStackException;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Stack;
import java.util.concurrent.Callable;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Future;
import javax.xml.parsers.ParserConfigurationException;
import javax.xml.parsers.SAXParser;
import javax.xml.parsers.SAXParserFactory;
import javax.xml.validation.Schema;
import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;
import org.xml.sax.Attributes;
import org.xml.sax.ContentHandler;
import org.xml.sax.EntityResolver;
import org.xml.sax.ErrorHandler;
import org.xml.sax.InputSource;
import org.xml.sax.Locator;
import org.xml.sax.SAXException;
import org.xml.sax.SAXNotRecognizedException;
import org.xml.sax.SAXNotSupportedException;
import org.xml.sax.SAXParseException;
import org.xml.sax.XMLReader;
import org.xml.sax.helpers.DefaultHandler;
import java.io.*;

/**
 * <p>
 * A <strong>Digester</strong> processes an XML input stream by matching a series of element nesting patterns to execute
 * Rules that have been added prior to the start of parsing.
 * </p>
 * <p>
 * See the <a href="package-summary.html#package_description">Digester Developer Guide</a> for more information.
 * </p>
 * <p>
 * <strong>IMPLEMENTATION NOTE</strong> - A single Digester instance may only be used within the context of a single
 * thread at a time, and a call to <code>parse()</code> must be completed before another can be initiated even from the
 * same thread.
 * </p>
 * <p>
 * A Digester instance should not be used for parsing more than one input document. The problem is that the Digester
 * class has quite a few member variables whose values "evolve" as SAX events are received during a parse. When reusing
 * the Digester instance, all these members must be reset back to their initial states before the second parse begins.
 * The "clear()" method makes a stab at resetting these, but it is actually rather a difficult problem. If you are
 * determined to reuse Digester instances, then at the least you should call the clear() method before each parse, and
 * must call it if the Digester parse terminates due to an exception during a parse.
 * </p>
 * <p>
 * <strong>LEGACY IMPLEMENTATION NOTE</strong> - When using the legacy XML schema support (instead of using the
 * {@link Schema} class), a bug in Xerces 2.0.2 prevents the support of XML schema. You need Xerces 2.1/2.3 and up to
 * make this class work with the legacy XML schema support.
 * </p>
 * <p>
 * This package was inspired by the <code>XmlMapper</code> class that was part of Tomcat 3.0 and 3.1, but is organized
 * somewhat differently.
 * </p>
 */
public class Digester extends DefaultHandler {

    /**
     * Construct a new Digester with default properties.
     */
    public Digester() {
        super();
    }

    /**
     * Construct a new Digester, allowing a SAXParser to be passed in. This allows Digester to be used in environments
     * which are unfriendly to JAXP1.1 (such as WebLogic 6.0). This may help in places where you are able to load JAXP
     * 1.1 classes yourself.
     *
     * @param parser The SAXParser used to parse XML streams
     */
    public Digester(SAXParser parser) {
        super();
        this.parser = parser;
    }

    /**
     * Construct a new Digester, allowing an XMLReader to be passed in. This allows Digester to be used in environments
     * which are unfriendly to JAXP1.1 (such as WebLogic 6.0). Note that if you use this option you have to configure
     * namespace and validation support yourself, as these properties only affect the SAXParser and emtpy constructor.
     *
     * @param reader The XMLReader used to parse XML streams
     */
    public Digester(XMLReader reader) {
        super();
        this.reader = reader;
    }

    /**
     * The body text of the current element.
     */
    private StringBuilder bodyText = new StringBuilder();

    /**
     * The stack of body text string buffers for surrounding elements.
     */
    private final Stack<StringBuilder> bodyTexts = new Stack<StringBuilder>();

    /**
     * Stack whose elements are List objects, each containing a list of Rule objects as returned from Rules.getMatch().
     * As each xml element in the input is entered, the matching rules are pushed onto this stack. After the end tag is
     * reached, the matches are popped again. The depth of is stack is therefore exactly the same as the current
     * "nesting" level of the input xml.
     *
     * @since 1.6
     */
    private final Stack<List<Rule>> matches = new Stack<List<Rule>>();

    /**
     * The class loader to use for instantiating application objects. If not specified, the context class loader, or the
     * class loader used to load Digester itself, is used, based on the value of the <code>useContextClassLoader</code>
     * variable.
     */
    private ClassLoader classLoader = null;

    /**
     * Has this Digester been configured yet.
     */
    private boolean configured = false;

    /**
     * The EntityResolver used by the SAX parser. By default it use this class
     */
    private EntityResolver entityResolver;

    /**
     * The URLs of entityValidator that have been registered, keyed by the public identifier that corresponds.
     */
    private final HashMap<String, URL> entityValidator = new HashMap<String, URL>();

    /**
     * The application-supplied error handler that is notified when parsing warnings, errors, or fatal errors occur.
     */
    private ErrorHandler errorHandler = null;

    /**
     * The SAXParserFactory that is created the first time we need it.
     */
    private SAXParserFactory factory = null;

    /**
     * The Locator associated with our parser.
     */
    private Locator locator = null;

    /**
     * The current match pattern for nested element processing.
     */
    private String match = "";

    /**
     * Do we want a "namespace aware" parser.
     */
    private boolean namespaceAware = false;

    /**
     * The executor service to run asynchronous parse method.
     * @since 3.1
     */
    private ExecutorService executorService;

    /**
     * Registered namespaces we are currently processing. The key is the namespace prefix that was declared in the
     * document. The value is an Stack of the namespace URIs this prefix has been mapped to -- the top Stack element is
     * the most current one. (This architecture is required because documents can declare nested uses of the same prefix
     * for different Namespace URIs).
     */
    private final HashMap<String, Stack<String>> namespaces = new HashMap<String, Stack<String>>();

    /**
     * Do we want a "XInclude aware" parser.
     */
    private boolean xincludeAware = false;

    /**
     * The parameters stack being utilized by CallMethodRule and CallParamRule rules.
     *
     * @since 2.0
     */
    private final Stack<Object[]> params = new Stack<Object[]>();

    /**
     * The SAXParser we will use to parse the input stream.
     */
    private SAXParser parser = null;

    /**
     * The public identifier of the DTD we are currently parsing under (if any).
     */
    private String publicId = null;

    /**
     * The XMLReader used to parse digester rules.
     */
    private XMLReader reader = null;

    /**
     * The "root" element of the stack (in other words, the last object that was popped.
     */
    private Object root = null;

    /**
     * The <code>Rules</code> implementation containing our collection of <code>Rule</code> instances and associated
     * matching policy. If not established before the first rule is added, a default implementation will be provided.
     */
    private Rules rules = null;

    /**
     * The XML schema to use for validating an XML instance.
     *
     * @since 2.0
     */
    private Schema schema = null;

    /**
     * The object stack being constructed.
     */
    private final Stack<Object> stack = new Stack<Object>();

    /**
     * Do we want to use the Context ClassLoader when loading classes for instantiating new objects. Default is
     * <code>true</code>.
     */
    private boolean useContextClassLoader = true;

    /**
     * Do we want to use a validating parser.
     */
    private boolean validating = false;

    /**
     * The Log to which most logging calls will be made.
     */
    private Log log = LogFactory.getLog("org.apache.commons.digester3.Digester");

    /**
     * The Log to which all SAX event related logging calls will be made.
     */
    private Log saxLog = LogFactory.getLog("org.apache.commons.digester3.Digester.sax");

    /**
     * The schema language supported. By default, we use this one.
     */
    protected static final String W3C_XML_SCHEMA = "http://www.w3.org/2001/XMLSchema";

    /**
     * An optional class that substitutes values in attributes and body text. This may be null and so a null check is
     * always required before use.
     */
    private Substitutor substitutor;

    /** Stacks used for interrule communication, indexed by name String */
    private final HashMap<String, Stack<Object>> stacksByName = new HashMap<String, Stack<Object>>();

    /**
     * If not null, then calls by the parser to this object's characters, startElement, endElement and
     * processingInstruction methods are forwarded to the specified object. This is intended to allow rules to
     * temporarily "take control" of the sax events. In particular, this is used by NodeCreateRule.
     * <p>
     * See setCustomContentHandler.
     */
    private ContentHandler customContentHandler = null;

    /**
     * Object which will receive callbacks for every pop/push action on the default stack or named stacks.
     */
    private StackAction stackAction = null;

    /**
     * Return the currently mapped namespace URI for the specified prefix, if any; otherwise return <code>null</code>.
     * These mappings come and go dynamically as the document is parsed.
     *
     * @param prefix Prefix to look up
     * @return the currently mapped namespace URI for the specified prefix
     */
    public String findNamespaceURI(String prefix) {
        writeline("/home/ubuntu/results/coverage/Digester/Digester_1_10.coverage", "ba43788c-970f-4d15-a3e4-7c986697655e");
        Stack<String> nsStack = namespaces.get(prefix);
        writeline("/home/ubuntu/results/coverage/Digester/Digester_1_10.coverage", "cf7f7c51-bc76-4444-a0e6-8edd059c14b4");
        if (nsStack == null) {
            writeline("/home/ubuntu/results/coverage/Digester/Digester_1_10.coverage", "4615ac78-82cf-477c-ba12-6e5a6a5c2eb0");
            return null;
        }
        writeline("/home/ubuntu/results/coverage/Digester/Digester_1_10.coverage", "64a88cdd-02d3-4f6f-97c2-03186c85f784");
        try {
            writeline("/home/ubuntu/results/coverage/Digester/Digester_1_10.coverage", "ea19415a-3f65-40b8-9c8f-ed1eb0bd1781");
            return (nsStack.peek());
        } catch (EmptyStackException e) {
            writeline("/home/ubuntu/results/coverage/Digester/Digester_1_10.coverage", "cdd7e6c3-3fc5-4fc3-ba15-a716182ac20a");
            return null;
        }
    }

    /**
     * Return the class loader to be used for instantiating application objects when required. This is determined based
     * upon the following rules:
     * <ul>
     * <li>The class loader set by <code>setClassLoader()</code>, if any</li>
     * <li>The thread context class loader, if it exists and the <code>useContextClassLoader</code> property is set to
     * true</li>
     * <li>The class loader used to load the Digester class itself.
     * </ul>
     *
     * @return the class loader to be used for instantiating application objects.
     */
    public ClassLoader getClassLoader() {
        writeline("/home/ubuntu/results/coverage/Digester/Digester_1_10.coverage", "43c14e42-eb96-4070-b5ad-f0a98749311a");
        if (this.classLoader != null) {
            writeline("/home/ubuntu/results/coverage/Digester/Digester_1_10.coverage", "1d473185-59fb-45c0-b818-8f783af39a2b");
            return (this.classLoader);
        }
        writeline("/home/ubuntu/results/coverage/Digester/Digester_1_10.coverage", "dd69518f-a23b-4f61-8d2c-c4d9fee31d14");
        if (this.useContextClassLoader) {
            writeline("/home/ubuntu/results/coverage/Digester/Digester_1_10.coverage", "66e52912-2276-40ab-a8ed-e3820bf604d2");
            ClassLoader classLoader = Thread.currentThread().getContextClassLoader();
            writeline("/home/ubuntu/results/coverage/Digester/Digester_1_10.coverage", "27b6eb29-1e49-4768-b70a-962c81e5a311");
            if (classLoader != null) {
                writeline("/home/ubuntu/results/coverage/Digester/Digester_1_10.coverage", "a9435ae6-b377-42b6-b54b-56ec1f192370");
                return (classLoader);
            }
        }
        writeline("/home/ubuntu/results/coverage/Digester/Digester_1_10.coverage", "1e30a1e2-dac2-4ae4-8612-3f973e756a01");
        return (this.getClass().getClassLoader());
    }

    /**
     * Set the class loader to be used for instantiating application objects when required.
     *
     * @param classLoader The new class loader to use, or <code>null</code> to revert to the standard rules
     */
    public void setClassLoader(ClassLoader classLoader) {
        writeline("/home/ubuntu/results/coverage/Digester/Digester_1_10.coverage", "27f4dae3-9b45-4eb8-9fa1-33659f547dc0");
        this.classLoader = classLoader;
    }

    /**
     * Return the current depth of the element stack.
     *
     * @return the current depth of the element stack.
     */
    public int getCount() {
        writeline("/home/ubuntu/results/coverage/Digester/Digester_1_10.coverage", "5d0d8d42-749c-4f33-944f-f1c2d1987259");
        return (stack.size());
    }

    /**
     * Return the name of the XML element that is currently being processed.
     *
     * @return the name of the XML element that is currently being processed.
     */
    public String getCurrentElementName() {
        writeline("/home/ubuntu/results/coverage/Digester/Digester_1_10.coverage", "ae5cba5a-0f68-4fdd-a101-d817d2500fa9");
        String elementName = match;
        writeline("/home/ubuntu/results/coverage/Digester/Digester_1_10.coverage", "170931b2-7669-4744-a081-4d8a05a3c4dc");
        int lastSlash = elementName.lastIndexOf('/');
        writeline("/home/ubuntu/results/coverage/Digester/Digester_1_10.coverage", "90c1b7f5-4909-451a-9756-f4ffb22fde9a");
        if (lastSlash >= 0) {
            writeline("/home/ubuntu/results/coverage/Digester/Digester_1_10.coverage", "1cadae31-c508-4dcc-82d7-c66e7bfefc5b");
            elementName = elementName.substring(lastSlash + 1);
        }
        writeline("/home/ubuntu/results/coverage/Digester/Digester_1_10.coverage", "537530fe-95c2-4342-b259-8ae7bb705980");
        return (elementName);
    }

    /**
     * Return the error handler for this Digester.
     *
     * @return the error handler for this Digester.
     */
    public ErrorHandler getErrorHandler() {
        writeline("/home/ubuntu/results/coverage/Digester/Digester_1_10.coverage", "0b4baf69-d787-4a9b-83f4-5c05419e8753");
        return (this.errorHandler);
    }

    /**
     * Set the error handler for this Digester.
     *
     * @param errorHandler The new error handler
     */
    public void setErrorHandler(ErrorHandler errorHandler) {
        writeline("/home/ubuntu/results/coverage/Digester/Digester_1_10.coverage", "7fc68205-a11d-4ba1-ac34-fefecd20b98a");
        this.errorHandler = errorHandler;
    }

    /**
     * Return the SAXParserFactory we will use, creating one if necessary.
     *
     * @return the SAXParserFactory we will use, creating one if necessary.
     */
    public SAXParserFactory getFactory() {
        writeline("/home/ubuntu/results/coverage/Digester/Digester_1_10.coverage", "fb0c3129-2a39-4009-9a30-f95cddd54e6d");
        if (factory == null) {
            writeline("/home/ubuntu/results/coverage/Digester/Digester_1_10.coverage", "e7825cff-5e39-4f48-995b-3eb89c67841c");
            factory = SAXParserFactory.newInstance();
            writeline("/home/ubuntu/results/coverage/Digester/Digester_1_10.coverage", "62bdc310-1dbc-4826-a433-4045b2a7850c");
            factory.setNamespaceAware(namespaceAware);
            writeline("/home/ubuntu/results/coverage/Digester/Digester_1_10.coverage", "9c61938d-0cfc-42c0-a00d-b139ea64ffdd");
            factory.setXIncludeAware(xincludeAware);
            writeline("/home/ubuntu/results/coverage/Digester/Digester_1_10.coverage", "30271c52-289b-45a8-b198-dcdc2d586d52");
            factory.setValidating(validating);
            writeline("/home/ubuntu/results/coverage/Digester/Digester_1_10.coverage", "133e50e9-f755-480a-b8b1-01235a3b56a9");
            factory.setSchema(schema);
        }
        writeline("/home/ubuntu/results/coverage/Digester/Digester_1_10.coverage", "b3f5aeab-0394-4d71-8769-1d9613efefc8");
        return (factory);
    }

    /**
     * Returns a flag indicating whether the requested feature is supported by the underlying implementation of
     * <code>org.xml.sax.XMLReader</code>. See <a href="http://www.saxproject.org">the saxproject website</a> for
     * information about the standard SAX2 feature flags.
     *
     * @param feature Name of the feature to inquire about
     * @return true, if the requested feature is supported by the underlying implementation of
     *         <code>org.xml.sax.XMLReader</code>, false otherwise
     * @throws ParserConfigurationException if a parser configuration error occurs
     * @throws SAXNotRecognizedException if the property name is not recognized
     * @throws SAXNotSupportedException if the property name is recognized but not supported
     */
    public boolean getFeature(String feature) throws ParserConfigurationException, SAXNotRecognizedException, SAXNotSupportedException {
        writeline("/home/ubuntu/results/coverage/Digester/Digester_1_10.coverage", "8cef283c-a5e3-4f16-b105-bd9c641717e6");
        return (getFactory().getFeature(feature));
    }

    /**
     * Sets a flag indicating whether the requested feature is supported by the underlying implementation of
     * <code>org.xml.sax.XMLReader</code>. See <a href="http://www.saxproject.org">the saxproject website</a> for
     * information about the standard SAX2 feature flags. In order to be effective, this method must be called
     * <strong>before</strong> the <code>getParser()</code> method is called for the first time, either directly or
     * indirectly.
     *
     * @param feature Name of the feature to set the status for
     * @param value The new value for this feature
     * @throws ParserConfigurationException if a parser configuration error occurs
     * @throws SAXNotRecognizedException if the property name is not recognized
     * @throws SAXNotSupportedException if the property name is recognized but not supported
     */
    public void setFeature(String feature, boolean value) throws ParserConfigurationException, SAXNotRecognizedException, SAXNotSupportedException {
        writeline("/home/ubuntu/results/coverage/Digester/Digester_1_10.coverage", "37003a42-be54-440a-8541-3d4fbcc60ee9");
        getFactory().setFeature(feature, value);
    }

    /**
     * Return the current Logger associated with this instance of the Digester
     *
     * @return the current Logger associated with this instance of the Digester
     */
    public Log getLogger() {
        writeline("/home/ubuntu/results/coverage/Digester/Digester_1_10.coverage", "d5fb038d-8c2f-4389-b417-37ec9e01a0dd");
        return log;
    }

    /**
     * Set the current logger for this Digester.
     *
     * @param log the current logger for this Digester.
     */
    public void setLogger(Log log) {
        writeline("/home/ubuntu/results/coverage/Digester/Digester_1_10.coverage", "d0acc8de-e202-400e-88ad-54473a501e52");
        this.log = log;
    }

    /**
     * Gets the logger used for logging SAX-related information. <strong>Note</strong> the output is finely grained.
     *
     * @return the logger used for logging SAX-related information
     * @since 1.6
     */
    public Log getSAXLogger() {
        writeline("/home/ubuntu/results/coverage/Digester/Digester_1_10.coverage", "d92d8048-5349-47f9-a9b2-4e30ccb91842");
        return saxLog;
    }

    /**
     * Sets the logger used for logging SAX-related information. <strong>Note</strong> the output is finely grained.
     *
     * @param saxLog the logger used for logging SAX-related information, not null
     * @since 1.6
     */
    public void setSAXLogger(Log saxLog) {
        writeline("/home/ubuntu/results/coverage/Digester/Digester_1_10.coverage", "881695a0-87b3-447d-8ea4-87a742b281e6");
        this.saxLog = saxLog;
    }

    /**
     * Return the current rule match path
     *
     * @return the current rule match path
     */
    public String getMatch() {
        writeline("/home/ubuntu/results/coverage/Digester/Digester_1_10.coverage", "9eb5fbf6-c604-41f0-9c3e-28a0d64766ae");
        return match;
    }

    /**
     * Return a Stack whose elements are List objects, each containing a list of
     * Rule objects as returned from Rules.getMatch().
     *
     * @return a Stack whose elements are List objects, each containing a list of
     *         Rule objects as returned from Rules.getMatch().
     * @since 3.0
     */
    public Stack<List<Rule>> getMatches() {
        writeline("/home/ubuntu/results/coverage/Digester/Digester_1_10.coverage", "738406f3-89b2-4752-8aee-4b251850ac0b");
        return matches;
    }

    /**
     * Return the "namespace aware" flag for parsers we create.
     *
     * @return the "namespace aware" flag for parsers we create.
     */
    public boolean getNamespaceAware() {
        writeline("/home/ubuntu/results/coverage/Digester/Digester_1_10.coverage", "65584b19-5355-4601-9571-d1fd4fd7cea9");
        return (this.namespaceAware);
    }

    /**
     * Set the "namespace aware" flag for parsers we create.
     *
     * @param namespaceAware The new "namespace aware" flag
     */
    public void setNamespaceAware(boolean namespaceAware) {
        writeline("/home/ubuntu/results/coverage/Digester/Digester_1_10.coverage", "659988b3-535e-442e-9562-0d47a00fa1e3");
        this.namespaceAware = namespaceAware;
    }

    /**
     * Return the XInclude-aware flag for parsers we create. XInclude functionality additionally requires
     * namespace-awareness.
     *
     * @return The XInclude-aware flag
     * @see #getNamespaceAware()
     * @since 2.0
     */
    public boolean getXIncludeAware() {
        writeline("/home/ubuntu/results/coverage/Digester/Digester_1_10.coverage", "0271a44d-b2f7-4901-b5d9-b2fb5c22a1f8");
        return (this.xincludeAware);
    }

    /**
     * Set the XInclude-aware flag for parsers we create. This additionally requires namespace-awareness.
     *
     * @param xincludeAware The new XInclude-aware flag
     * @see #setNamespaceAware(boolean)
     * @since 2.0
     */
    public void setXIncludeAware(boolean xincludeAware) {
        writeline("/home/ubuntu/results/coverage/Digester/Digester_1_10.coverage", "7bdcafda-1f9c-43e2-a37f-44f2fca4529f");
        this.xincludeAware = xincludeAware;
    }

    /**
     * Set the public id of the current file being parse.
     *
     * @param publicId the DTD/Schema public's id.
     */
    public void setPublicId(String publicId) {
        writeline("/home/ubuntu/results/coverage/Digester/Digester_1_10.coverage", "88a8d3c4-d03e-4639-bbf0-dd13cd4458e7");
        this.publicId = publicId;
    }

    /**
     * Return the public identifier of the DTD we are currently parsing under, if any.
     *
     * @return the public identifier of the DTD we are currently parsing under, if any.
     */
    public String getPublicId() {
        writeline("/home/ubuntu/results/coverage/Digester/Digester_1_10.coverage", "936ea22d-0674-40b7-9776-3dabbe614028");
        return (this.publicId);
    }

    /**
     * Return the namespace URI that will be applied to all subsequently added <code>Rule</code> objects.
     *
     * @return the namespace URI that will be applied to all subsequently added <code>Rule</code> objects.
     */
    public String getRuleNamespaceURI() {
        writeline("/home/ubuntu/results/coverage/Digester/Digester_1_10.coverage", "e94d2395-bcf3-4368-a195-2f2ec3955180");
        return (getRules().getNamespaceURI());
    }

    /**
     * Set the namespace URI that will be applied to all subsequently added <code>Rule</code> objects.
     *
     * @param ruleNamespaceURI Namespace URI that must match on all subsequently added rules, or <code>null</code> for
     *            matching regardless of the current namespace URI
     */
    public void setRuleNamespaceURI(String ruleNamespaceURI) {
        writeline("/home/ubuntu/results/coverage/Digester/Digester_1_10.coverage", "9601b471-ddeb-4d44-acaa-456f2200698f");
        getRules().setNamespaceURI(ruleNamespaceURI);
    }

    /**
     * Return the SAXParser we will use to parse the input stream.
     *
     * If there is a problem creating the parser, return <code>null</code>.
     *
     * @return the SAXParser we will use to parse the input stream
     */
    public SAXParser getParser() {
        writeline("/home/ubuntu/results/coverage/Digester/Digester_1_10.coverage", "e83081a0-0b5f-4f0a-bec8-e0d7c9e9bc4b");
        if (parser != null) {
            writeline("/home/ubuntu/results/coverage/Digester/Digester_1_10.coverage", "dadebf06-b08d-408a-befb-828fab5e0b25");
            return (parser);
        }
        writeline("/home/ubuntu/results/coverage/Digester/Digester_1_10.coverage", "f82c2eb4-f351-4fbc-a5ad-a24fafcd0f05");
        try {
            writeline("/home/ubuntu/results/coverage/Digester/Digester_1_10.coverage", "6de0091b-4981-4618-972e-7ce29ca3fbec");
            parser = getFactory().newSAXParser();
        } catch (Exception e) {
            writeline("/home/ubuntu/results/coverage/Digester/Digester_1_10.coverage", "fea3b695-0913-44d8-ba8c-03a950f224f2");
            log.error("Digester.getParser: ", e);
            writeline("/home/ubuntu/results/coverage/Digester/Digester_1_10.coverage", "b59e57c8-07bd-45e6-9c27-56715900b4b5");
            return (null);
        }
        writeline("/home/ubuntu/results/coverage/Digester/Digester_1_10.coverage", "286e3aea-3403-44ee-801b-feeddb52e6cc");
        return (parser);
    }

    /**
     * Return the current value of the specified property for the underlying <code>XMLReader</code> implementation.
     *
     * See <a href="http://www.saxproject.org">the saxproject website</a> for information about the standard SAX2
     * properties.
     *
     * @param property Property name to be retrieved
     * @return the current value of the specified property for the underlying <code>XMLReader</code> implementation.
     * @throws SAXNotRecognizedException if the property name is not recognized
     * @throws SAXNotSupportedException if the property name is recognized but not supported
     */
    public Object getProperty(String property) throws SAXNotRecognizedException, SAXNotSupportedException {
        writeline("/home/ubuntu/results/coverage/Digester/Digester_1_10.coverage", "a287303e-8abf-4071-b578-ee876d69bfc7");
        return (getParser().getProperty(property));
    }

    /**
     * Set the current value of the specified property for the underlying <code>XMLReader</code> implementation. See <a
     * href="http://www.saxproject.org">the saxproject website</a> for information about the standard SAX2 properties.
     *
     * @param property Property name to be set
     * @param value Property value to be set
     * @throws SAXNotRecognizedException if the property name is not recognized
     * @throws SAXNotSupportedException if the property name is recognized but not supported
     */
    public void setProperty(String property, Object value) throws SAXNotRecognizedException, SAXNotSupportedException {
        writeline("/home/ubuntu/results/coverage/Digester/Digester_1_10.coverage", "768a418a-38ce-4d4d-983b-009623745856");
        getParser().setProperty(property, value);
    }

    /**
     * Return the <code>Rules</code> implementation object containing our rules collection and associated matching
     * policy. If none has been established, a default implementation will be created and returned.
     *
     * @return the <code>Rules</code> implementation object.
     */
    public Rules getRules() {
        writeline("/home/ubuntu/results/coverage/Digester/Digester_1_10.coverage", "e92f860e-0787-4ae1-b765-57f99bca024f");
        if (this.rules == null) {
            writeline("/home/ubuntu/results/coverage/Digester/Digester_1_10.coverage", "dcb42ea9-040b-49ed-914a-06ee045ac5e3");
            this.rules = new RulesBase();
            writeline("/home/ubuntu/results/coverage/Digester/Digester_1_10.coverage", "2dc1b634-b6cc-401e-8703-d27fa282c732");
            this.rules.setDigester(this);
        }
        writeline("/home/ubuntu/results/coverage/Digester/Digester_1_10.coverage", "a195db64-7f97-475f-8ccf-2af0898cff9e");
        return (this.rules);
    }

    /**
     * Set the <code>Rules</code> implementation object containing our rules collection and associated matching policy.
     *
     * @param rules New Rules implementation
     */
    public void setRules(Rules rules) {
        writeline("/home/ubuntu/results/coverage/Digester/Digester_1_10.coverage", "92864bd0-3171-4b82-a2e1-78992bad86b8");
        this.rules = rules;
        writeline("/home/ubuntu/results/coverage/Digester/Digester_1_10.coverage", "c7eeba86-6ad0-45e9-a664-f881a1567fdc");
        this.rules.setDigester(this);
    }

    /**
     * Return the XML Schema used when parsing.
     *
     * @return The {@link Schema} instance in use.
     * @since 2.0
     */
    public Schema getXMLSchema() {
        writeline("/home/ubuntu/results/coverage/Digester/Digester_1_10.coverage", "0916751f-c3b1-4806-a714-f85e5817c6e6");
        return (this.schema);
    }

    /**
     * Set the XML Schema to be used when parsing.
     *
     * @param schema The {@link Schema} instance to use.
     * @since 2.0
     */
    public void setXMLSchema(Schema schema) {
        writeline("/home/ubuntu/results/coverage/Digester/Digester_1_10.coverage", "116c792e-7e51-4256-a39f-c9b3f9c6d64a");
        this.schema = schema;
    }

    /**
     * Return the boolean as to whether the context ClassLoader should be used.
     *
     * @return true, if the context ClassLoader should be used, false otherwise.
     */
    public boolean getUseContextClassLoader() {
        writeline("/home/ubuntu/results/coverage/Digester/Digester_1_10.coverage", "7d2feab4-6a5b-4896-a48e-9f7f5ecdbf13");
        return useContextClassLoader;
    }

    /**
     * Determine whether to use the Context ClassLoader (the one found by calling
     * <code>Thread.currentThread().getContextClassLoader()</code>) to resolve/load classes that are defined in various
     * rules. If not using Context ClassLoader, then the class-loading defaults to using the calling-class' ClassLoader.
     *
     * @param use determines whether to use Context ClassLoader.
     */
    public void setUseContextClassLoader(boolean use) {
        writeline("/home/ubuntu/results/coverage/Digester/Digester_1_10.coverage", "f3d87f15-03b4-4518-90c8-3e20111738a5");
        useContextClassLoader = use;
    }

    /**
     * Return the validating parser flag.
     *
     * @return the validating parser flag.
     */
    public boolean getValidating() {
        writeline("/home/ubuntu/results/coverage/Digester/Digester_1_10.coverage", "49b31369-9993-4f2f-876e-af9b13cfa6ed");
        return (this.validating);
    }

    /**
     * Set the validating parser flag. This must be called before <code>parse()</code> is called the first time. 
     * By default the value of this is set to false.
     * 
     * It essentially just controls the DTD validation. To use modern schema languages use the 
     * {@link #setXMLSchema(Schema)} method to associate a schema to a parser.
     *
     * @param validating The new validating parser flag.
     * @see javax.xml.parsers.SAXParserFactory#setValidating(boolean) for more detail.
     */
    public void setValidating(boolean validating) {
        writeline("/home/ubuntu/results/coverage/Digester/Digester_1_10.coverage", "92cc9c9e-a6f0-48e6-9712-d27c7bc74064");
        this.validating = validating;
    }

    /**
     * Return the XMLReader to be used for parsing the input document.
     *
     * FIXME: there is a bug in JAXP/XERCES that prevent the use of a parser that contains a schema with a DTD.
     *
     * @return the XMLReader to be used for parsing the input document.
     * @throws SAXException if no XMLReader can be instantiated
     */
    public XMLReader getXMLReader() throws SAXException {
        writeline("/home/ubuntu/results/coverage/Digester/Digester_1_10.coverage", "2b0d936f-92c3-4f2a-bb03-062310f63e7b");
        if (reader == null) {
            writeline("/home/ubuntu/results/coverage/Digester/Digester_1_10.coverage", "3f8f11a8-0ab9-4ba9-a1a2-f82e8d2b6b91");
            reader = getParser().getXMLReader();
        }
        writeline("/home/ubuntu/results/coverage/Digester/Digester_1_10.coverage", "33305f4a-b2ef-4f18-bed5-f279c45d3fc8");
        reader.setDTDHandler(this);
        writeline("/home/ubuntu/results/coverage/Digester/Digester_1_10.coverage", "4a24d5c1-a549-4d77-9e1b-0e68b049b599");
        reader.setContentHandler(this);
        writeline("/home/ubuntu/results/coverage/Digester/Digester_1_10.coverage", "55262f6b-0794-45c3-a087-43320e34fc5a");
        if (entityResolver == null) {
            writeline("/home/ubuntu/results/coverage/Digester/Digester_1_10.coverage", "ae50cfcf-41b9-4d31-a807-cce450fc53ba");
            reader.setEntityResolver(this);
        } else {
            writeline("/home/ubuntu/results/coverage/Digester/Digester_1_10.coverage", "45b1a615-c7d3-4454-a68f-dfb7200528b0");
            reader.setEntityResolver(entityResolver);
        }
        writeline("/home/ubuntu/results/coverage/Digester/Digester_1_10.coverage", "b57ed18f-1597-4f8d-afef-e7ca17e1ed37");
        if (this.errorHandler != null) {
            writeline("/home/ubuntu/results/coverage/Digester/Digester_1_10.coverage", "f1263d44-fb49-44d9-aa27-dba5ae3b5cdc");
            reader.setErrorHandler(this.errorHandler);
        } else {
            writeline("/home/ubuntu/results/coverage/Digester/Digester_1_10.coverage", "c23ca795-9a9d-4c1d-8b38-1ed8b5c085b4");
            reader.setErrorHandler(this);
        }
        writeline("/home/ubuntu/results/coverage/Digester/Digester_1_10.coverage", "80ced67b-6106-4363-8faa-2dd1ae66392e");
        return reader;
    }

    /**
     * Gets the <code>Substitutor</code> used to convert attributes and body text.
     *
     * @return the <code>Substitutor</code> used to convert attributes and body text,
     *         null if not substitutions are to be performed.
     */
    public Substitutor getSubstitutor() {
        writeline("/home/ubuntu/results/coverage/Digester/Digester_1_10.coverage", "ce9d1f6a-d589-4048-a037-51e3ec850d85");
        return substitutor;
    }

    /**
     * Sets the <code>Substitutor</code> to be used to convert attributes and body text.
     *
     * @param substitutor the Substitutor to be used to convert attributes and body text or null if not substitution of
     *            these values is to be performed.
     */
    public void setSubstitutor(Substitutor substitutor) {
        writeline("/home/ubuntu/results/coverage/Digester/Digester_1_10.coverage", "1ba93b76-505f-420d-8a5f-bca6d32fed21");
        this.substitutor = substitutor;
    }

    /**
     * returns the custom SAX ContentHandler where events are redirected.
     *
     * @return the custom SAX ContentHandler where events are redirected.
     * @see #setCustomContentHandler(ContentHandler)
     * @since 1.7
     */
    public ContentHandler getCustomContentHandler() {
        writeline("/home/ubuntu/results/coverage/Digester/Digester_1_10.coverage", "15498127-f7e9-4c8c-9cf2-4c7509a85694");
        return customContentHandler;
    }

    /**
     * Redirects (or cancels redirecting) of SAX ContentHandler events to an external object.
     * <p>
     * When this object's customContentHandler is non-null, any SAX events received from the parser will simply be
     * passed on to the specified object instead of this object handling them. This allows Rule classes to take control
     * of the SAX event stream for a while in order to do custom processing. Such a rule should save the old value
     * before setting a new one, and restore the old value in order to resume normal digester processing.
     * <p>
     * An example of a Rule which needs this feature is NodeCreateRule.
     * <p>
     * Note that saving the old value is probably not needed as it should always be null; a custom rule that wants to
     * take control could only have been called when there was no custom content handler. But it seems cleaner to
     * properly save/restore the value and maybe some day this will come in useful.
     * <p>
     * Note also that this is not quite equivalent to
     *
     * <pre>
     * digester.getXMLReader().setContentHandler( handler )
     * </pre>
     *
     * for these reasons:
     * <ul>
     * <li>Some xml parsers don't like having setContentHandler called after parsing has started. The Aelfred parser is
     * one example.</li>
     * <li>Directing the events via the Digester object potentially allows us to log information about those SAX events
     * at the digester level.</li>
     * </ul>
     *
     * @param handler the custom SAX ContentHandler where events are redirected.
     * @since 1.7
     */
    public void setCustomContentHandler(ContentHandler handler) {
        writeline("/home/ubuntu/results/coverage/Digester/Digester_1_10.coverage", "458aa221-ff76-48d4-b14c-165b92c9d4b5");
        customContentHandler = handler;
    }

    /**
     * Define a callback object which is invoked whenever an object is pushed onto a digester object stack,
     * or popped off one.
     *
     * @param stackAction the callback object which is invoked whenever an object is pushed onto a digester
     *        object stack, or popped off one.
     * @since 1.8
     */
    public void setStackAction(StackAction stackAction) {
        writeline("/home/ubuntu/results/coverage/Digester/Digester_1_10.coverage", "fc9d62ed-5eb2-499f-ac86-6c116d6b04ec");
        this.stackAction = stackAction;
    }

    /**
     * Return the callback object which is invoked whenever an object is pushed onto a digester object stack,
     * or popped off one.
     *
     * @return the callback object which is invoked whenever an object is pushed onto a digester object stack,
     *         or popped off one.
     * @see #setStackAction(StackAction)
     * @since 1.8
     */
    public StackAction getStackAction() {
        writeline("/home/ubuntu/results/coverage/Digester/Digester_1_10.coverage", "125cba88-b8bb-4db4-929a-67e39827c90c");
        return stackAction;
    }

    /**
     * Get the most current namespaces for all prefixes.
     *
     * @return Map A map with namespace prefixes as keys and most current namespace URIs for the corresponding prefixes
     *         as values
     * @since 1.8
     */
    public Map<String, String> getCurrentNamespaces() {
        writeline("/home/ubuntu/results/coverage/Digester/Digester_1_10.coverage", "fe1edf39-c1a2-4d19-9ccc-08883a50af45");
        if (!namespaceAware) {
            writeline("/home/ubuntu/results/coverage/Digester/Digester_1_10.coverage", "e7cb4778-85c4-4304-bed7-d0843b854dea");
            log.warn("Digester is not namespace aware");
        }
        writeline("/home/ubuntu/results/coverage/Digester/Digester_1_10.coverage", "d3b12a7c-5924-4696-b0cd-40fb43cbaba9");
        Map<String, String> currentNamespaces = new HashMap<String, String>();
        writeline("/home/ubuntu/results/coverage/Digester/Digester_1_10.coverage", "bb031a51-53ed-467d-b8a8-030ec66c35e3");
        for (Map.Entry<String, Stack<String>> nsEntry : namespaces.entrySet()) {
            writeline("/home/ubuntu/results/coverage/Digester/Digester_1_10.coverage", "98fea98d-daec-4a21-af4e-39f78eef8451");
            try {
                writeline("/home/ubuntu/results/coverage/Digester/Digester_1_10.coverage", "e627887d-c16a-4638-be0b-f3bf8af20f82");
                currentNamespaces.put(nsEntry.getKey(), nsEntry.getValue().peek());
            } catch (RuntimeException e) {
                writeline("/home/ubuntu/results/coverage/Digester/Digester_1_10.coverage", "b8da215a-7ed6-48fd-b542-c3b4c1fed881");
                log.error(e.getMessage(), e);
                writeline("/home/ubuntu/results/coverage/Digester/Digester_1_10.coverage", "e13fcc24-9c17-42b2-8ce8-e026bf5b429b");
                throw e;
            }
        }
        writeline("/home/ubuntu/results/coverage/Digester/Digester_1_10.coverage", "a4a5fce9-00b9-4e0b-9a13-072e185abe0a");
        return currentNamespaces;
    }

    /**
     * Returns the executor service used to run asynchronous parse method.
     *
     * @return the executor service used to run asynchronous parse method
     * @since 3.1
     */
    public ExecutorService getExecutorService() {
        writeline("/home/ubuntu/results/coverage/Digester/Digester_1_10.coverage", "f2aa153d-ea53-45b1-ab22-43f30d4013a4");
        return executorService;
    }

    /**
     * Sets the executor service to run asynchronous parse method.
     *
     * @param executorService the executor service to run asynchronous parse method
     * @since 3.1
     */
    public void setExecutorService(ExecutorService executorService) {
        writeline("/home/ubuntu/results/coverage/Digester/Digester_1_10.coverage", "c6d954f8-8153-4b07-99b1-380a8db48c06");
        this.executorService = executorService;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public void characters(char buffer[], int start, int length) throws SAXException {
        writeline("/home/ubuntu/results/coverage/Digester/Digester_1_10.coverage", "69954d96-425a-4bda-97af-b80bd4952fca");
        if (customContentHandler != null) {
            writeline("/home/ubuntu/results/coverage/Digester/Digester_1_10.coverage", "9e3ec8b8-21cb-4e96-b206-959b43a808cd");
            customContentHandler.characters(buffer, start, length);
            writeline("/home/ubuntu/results/coverage/Digester/Digester_1_10.coverage", "b35ab5e5-3a2f-4c44-b184-7f0a5088fb6a");
            return;
        }
        writeline("/home/ubuntu/results/coverage/Digester/Digester_1_10.coverage", "7ee7fc65-db8d-4bc8-a646-6ca5c0a35561");
        if (saxLog.isDebugEnabled()) {
            writeline("/home/ubuntu/results/coverage/Digester/Digester_1_10.coverage", "ada94aff-b259-4acd-91f1-ef5cebfdbb77");
            saxLog.debug("characters(" + new String(buffer, start, length) + ")");
        }
        writeline("/home/ubuntu/results/coverage/Digester/Digester_1_10.coverage", "feadfd94-0b7c-4205-875c-cd957faab207");
        bodyText.append(buffer, start, length);
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public void endDocument() throws SAXException {
        writeline("/home/ubuntu/results/coverage/Digester/Digester_1_10.coverage", "53f3f466-7222-43e3-8d7c-d5b14c92a364");
        if (saxLog.isDebugEnabled()) {
            writeline("/home/ubuntu/results/coverage/Digester/Digester_1_10.coverage", "45fe99dd-19ec-4c19-9644-b0fb5e988fcc");
            if (getCount() > 1) {
                writeline("/home/ubuntu/results/coverage/Digester/Digester_1_10.coverage", "ed3a2f9f-0f38-4c0a-a01c-d43cc9d4b8e8");
                saxLog.debug("endDocument():  " + getCount() + " elements left");
            } else {
                writeline("/home/ubuntu/results/coverage/Digester/Digester_1_10.coverage", "42b51446-ad0a-476d-91b5-e7b86301c491");
                saxLog.debug("endDocument()");
            }
        }
        writeline("/home/ubuntu/results/coverage/Digester/Digester_1_10.coverage", "6f99fb90-506f-4b2f-91fe-a134aab2e0d8");
        for (Rule rule : getRules().rules()) {
            writeline("/home/ubuntu/results/coverage/Digester/Digester_1_10.coverage", "0396b420-6a99-439b-9306-df8f3f97c0f1");
            try {
                writeline("/home/ubuntu/results/coverage/Digester/Digester_1_10.coverage", "0c9bd504-e83f-4179-ad79-ee63c45d6c0a");
                rule.finish();
            } catch (Exception e) {
                writeline("/home/ubuntu/results/coverage/Digester/Digester_1_10.coverage", "a7f41c62-e5aa-47e0-bf70-bb9ac9f3d1f9");
                log.error("Finish event threw exception", e);
                writeline("/home/ubuntu/results/coverage/Digester/Digester_1_10.coverage", "cbc7e498-0838-4db5-a356-f076f07b8c8e");
                throw createSAXException(e);
            } catch (Error e) {
                writeline("/home/ubuntu/results/coverage/Digester/Digester_1_10.coverage", "b246f95d-1779-4b68-88be-20f8ae5813d5");
                log.error("Finish event threw error", e);
                writeline("/home/ubuntu/results/coverage/Digester/Digester_1_10.coverage", "36016529-6a54-446c-8894-3072bc5204d0");
                throw e;
            }
        }
        writeline("/home/ubuntu/results/coverage/Digester/Digester_1_10.coverage", "69a90b15-fb1d-40b8-b5b2-6cb0b9af36a2");
        clear();
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public void endElement(String namespaceURI, String localName, String qName) throws SAXException {
        writeline("/home/ubuntu/results/coverage/Digester/Digester_1_10.coverage", "ce153f72-8c5a-4867-b593-79bbfaf80ff7");
        if (customContentHandler != null) {
            writeline("/home/ubuntu/results/coverage/Digester/Digester_1_10.coverage", "8d1c6404-fa28-48e8-8fac-10d683aa8b54");
            customContentHandler.endElement(namespaceURI, localName, qName);
            writeline("/home/ubuntu/results/coverage/Digester/Digester_1_10.coverage", "85aa3229-0435-4efa-9237-eda00f99545f");
            return;
        }
        writeline("/home/ubuntu/results/coverage/Digester/Digester_1_10.coverage", "93163d5e-493e-4c2f-9480-ee57508a93b3");
        boolean debug = log.isDebugEnabled();
        writeline("/home/ubuntu/results/coverage/Digester/Digester_1_10.coverage", "14b25ccc-6ef7-4e6f-a3b7-ea6b869e2ada");
        if (debug) {
            writeline("/home/ubuntu/results/coverage/Digester/Digester_1_10.coverage", "d0b3c9d1-7a1b-4377-ae88-fb9a49dd76dd");
            if (saxLog.isDebugEnabled()) {
                writeline("/home/ubuntu/results/coverage/Digester/Digester_1_10.coverage", "7b6f990e-dbbc-48ba-b7fc-9f09b700fc3e");
                saxLog.debug("endElement(" + namespaceURI + "," + localName + "," + qName + ")");
            }
            writeline("/home/ubuntu/results/coverage/Digester/Digester_1_10.coverage", "a858e224-fd63-49a1-8ea4-1c6796458133");
            log.debug("  match='" + match + "'");
            writeline("/home/ubuntu/results/coverage/Digester/Digester_1_10.coverage", "e776ef60-1620-4f7f-872a-bf64fb9e9337");
            log.debug("  bodyText='" + bodyText + "'");
        }
        writeline("/home/ubuntu/results/coverage/Digester/Digester_1_10.coverage", "3b00e289-214b-45b8-bca4-984e206e506b");
        String name = localName;
        writeline("/home/ubuntu/results/coverage/Digester/Digester_1_10.coverage", "1bd8a5c4-cb58-4bbf-ba99-d569bdc79884");
        if ((name == null) || (name.length() < 1)) {
            writeline("/home/ubuntu/results/coverage/Digester/Digester_1_10.coverage", "23d37c49-ad1b-441e-b27b-52e58907edda");
            name = qName;
        }
        writeline("/home/ubuntu/results/coverage/Digester/Digester_1_10.coverage", "08439bad-2ac3-4bbf-b54e-7458c606976c");
        List<Rule> rules = matches.pop();
        writeline("/home/ubuntu/results/coverage/Digester/Digester_1_10.coverage", "9fd6ffcd-46a8-4059-8e27-72505520d45d");
        if ((rules != null) && (rules.size() > 0)) {
            writeline("/home/ubuntu/results/coverage/Digester/Digester_1_10.coverage", "39de605b-85b8-4c9e-a8aa-5c8620f8d299");
            String bodyText = this.bodyText.toString();
            writeline("/home/ubuntu/results/coverage/Digester/Digester_1_10.coverage", "5bf1d68c-8be8-4e40-a552-4145e496b5e1");
            Substitutor substitutor = getSubstitutor();
            writeline("/home/ubuntu/results/coverage/Digester/Digester_1_10.coverage", "74e2f671-9193-46eb-b26e-d8a06198bb9d");
            if (substitutor != null) {
                writeline("/home/ubuntu/results/coverage/Digester/Digester_1_10.coverage", "b42011f3-e997-41ff-9455-024e92ac6491");
                bodyText = substitutor.substitute(bodyText);
            }
            writeline("/home/ubuntu/results/coverage/Digester/Digester_1_10.coverage", "e07b9939-4d5f-4a92-bebf-0f137fcb9a37");
            for (int i = 0; i < rules.size(); i++) {
                writeline("/home/ubuntu/results/coverage/Digester/Digester_1_10.coverage", "9049cb4c-e0fa-4b0d-84c8-ee652c670815");
                try {
                    writeline("/home/ubuntu/results/coverage/Digester/Digester_1_10.coverage", "28a422a0-df8a-47b7-98c3-df72e6fef5bc");
                    Rule rule = rules.get(i);
                    writeline("/home/ubuntu/results/coverage/Digester/Digester_1_10.coverage", "5362ccfa-afa3-4487-a17e-bec998585cfa");
                    if (debug) {
                        writeline("/home/ubuntu/results/coverage/Digester/Digester_1_10.coverage", "6988f56e-c389-4f92-9d6f-01ff1395b328");
                        log.debug("  Fire body() for " + rule);
                    }
                    writeline("/home/ubuntu/results/coverage/Digester/Digester_1_10.coverage", "666dee4a-d01f-4b8a-8858-5a1e3ab32a3d");
                    rule.body(namespaceURI, name, bodyText);
                } catch (Exception e) {
                    writeline("/home/ubuntu/results/coverage/Digester/Digester_1_10.coverage", "63737b3d-4c18-48ae-a7da-a35bc41bca2d");
                    log.error("Body event threw exception", e);
                    writeline("/home/ubuntu/results/coverage/Digester/Digester_1_10.coverage", "9b82434a-2f8a-4f22-844c-fd556d862985");
                    throw createSAXException(e);
                } catch (Error e) {
                    writeline("/home/ubuntu/results/coverage/Digester/Digester_1_10.coverage", "85ccefcc-7842-4a12-a07e-d89af3223393");
                    log.error("Body event threw error", e);
                    writeline("/home/ubuntu/results/coverage/Digester/Digester_1_10.coverage", "3a8117dd-3b66-4776-bf98-30ca8743dd55");
                    throw e;
                }
            }
        } else {
            writeline("/home/ubuntu/results/coverage/Digester/Digester_1_10.coverage", "4e884944-8e55-449c-bbd7-22d9ff1aa00b");
            if (debug) {
                writeline("/home/ubuntu/results/coverage/Digester/Digester_1_10.coverage", "8aa41fc8-83f5-42c9-a74c-0dfd0926ccee");
                log.debug("  No rules found matching '" + match + "'.");
            }
        }
        writeline("/home/ubuntu/results/coverage/Digester/Digester_1_10.coverage", "b3656eb7-1e67-4160-a25c-050747f43b74");
        bodyText = bodyTexts.pop();
        writeline("/home/ubuntu/results/coverage/Digester/Digester_1_10.coverage", "9ab21671-efe3-44f3-ae78-c170276dda29");
        if (debug) {
            writeline("/home/ubuntu/results/coverage/Digester/Digester_1_10.coverage", "4a86a1eb-5cca-48a8-9dc0-97250f21e125");
            log.debug("  Popping body text '" + bodyText.toString() + "'");
        }
        writeline("/home/ubuntu/results/coverage/Digester/Digester_1_10.coverage", "591aff73-b492-4e05-8d70-e84635525d13");
        if (rules != null) {
            writeline("/home/ubuntu/results/coverage/Digester/Digester_1_10.coverage", "58643e42-9c49-40dd-b8ba-c0f62d3bb354");
            for (int i = 0; i < rules.size(); i++) {
                writeline("/home/ubuntu/results/coverage/Digester/Digester_1_10.coverage", "3edd0ca6-5588-47fc-855e-55ec26cca141");
                int j = (rules.size() - i) - 1;
                writeline("/home/ubuntu/results/coverage/Digester/Digester_1_10.coverage", "4f863cb8-1b69-49b1-9577-3d77e929746a");
                try {
                    writeline("/home/ubuntu/results/coverage/Digester/Digester_1_10.coverage", "b947a3e4-62ab-4313-9660-e2664626a442");
                    Rule rule = rules.get(j);
                    writeline("/home/ubuntu/results/coverage/Digester/Digester_1_10.coverage", "65809c4e-b415-4c86-968b-cc3e54a96005");
                    if (debug) {
                        writeline("/home/ubuntu/results/coverage/Digester/Digester_1_10.coverage", "85e604ed-00eb-4a16-abe3-f6ae6ccfcde7");
                        log.debug("  Fire end() for " + rule);
                    }
                    writeline("/home/ubuntu/results/coverage/Digester/Digester_1_10.coverage", "7c70ccca-61b5-490a-b9c9-4c7ef5213778");
                    rule.end(namespaceURI, name);
                } catch (Exception e) {
                    writeline("/home/ubuntu/results/coverage/Digester/Digester_1_10.coverage", "a6e752af-6430-470f-829e-5841d7cd5dcc");
                    log.error("End event threw exception", e);
                    writeline("/home/ubuntu/results/coverage/Digester/Digester_1_10.coverage", "1f1e8b13-518a-4169-8523-e8f6ac48d058");
                    throw createSAXException(e);
                } catch (Error e) {
                    writeline("/home/ubuntu/results/coverage/Digester/Digester_1_10.coverage", "d543a259-796d-467d-bd5c-d380e73aa6f4");
                    log.error("End event threw error", e);
                    writeline("/home/ubuntu/results/coverage/Digester/Digester_1_10.coverage", "c70ce0b5-3cae-4b03-8a31-4c8557789a29");
                    throw e;
                }
            }
        }
        writeline("/home/ubuntu/results/coverage/Digester/Digester_1_10.coverage", "2e575fd0-7441-4e28-9027-4ad014901dea");
        int slash = match.lastIndexOf('/');
        writeline("/home/ubuntu/results/coverage/Digester/Digester_1_10.coverage", "8810cb30-7ac6-491e-b76e-2ff3f7ff1c92");
        if (slash >= 0) {
            writeline("/home/ubuntu/results/coverage/Digester/Digester_1_10.coverage", "73e8199d-225c-45f1-8129-466cc64d1d5e");
            match = match.substring(0, slash);
        } else {
            writeline("/home/ubuntu/results/coverage/Digester/Digester_1_10.coverage", "9f5aa097-6315-4d8a-b6f4-80656ec8ded2");
            match = "";
        }
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public void endPrefixMapping(String prefix) throws SAXException {
        writeline("/home/ubuntu/results/coverage/Digester/Digester_1_10.coverage", "5ce3c7af-e2b5-4a4b-9bae-841e6a2ef0b7");
        if (saxLog.isDebugEnabled()) {
            writeline("/home/ubuntu/results/coverage/Digester/Digester_1_10.coverage", "5c07696a-0a15-4deb-934c-a705fd15888c");
            saxLog.debug("endPrefixMapping(" + prefix + ")");
        }
        writeline("/home/ubuntu/results/coverage/Digester/Digester_1_10.coverage", "b43a3e9a-6daa-45a4-815b-4e77c23241b5");
        Stack<String> stack = namespaces.get(prefix);
        writeline("/home/ubuntu/results/coverage/Digester/Digester_1_10.coverage", "49cf4b95-9bec-46d9-b817-ef70fc36642d");
        if (stack == null) {
            writeline("/home/ubuntu/results/coverage/Digester/Digester_1_10.coverage", "f829cedd-32bf-4acd-9f62-46653b736ecc");
            return;
        }
        writeline("/home/ubuntu/results/coverage/Digester/Digester_1_10.coverage", "e262ed2f-68c8-4f37-8d55-1cd34d855d75");
        try {
            writeline("/home/ubuntu/results/coverage/Digester/Digester_1_10.coverage", "4fd3b3d1-da5d-4b3f-9537-0a8fa639cbc3");
            stack.pop();
            writeline("/home/ubuntu/results/coverage/Digester/Digester_1_10.coverage", "d5f5fc69-933c-4d49-af82-0db3384a4fc6");
            if (stack.empty()) {
                writeline("/home/ubuntu/results/coverage/Digester/Digester_1_10.coverage", "7d8a026b-c1b5-4d2d-a42c-253db5265858");
                namespaces.remove(prefix);
            }
        } catch (EmptyStackException e) {
            writeline("/home/ubuntu/results/coverage/Digester/Digester_1_10.coverage", "e9eb6f5d-a56c-49d0-b9c7-a2fdc6eff775");
            throw createSAXException("endPrefixMapping popped too many times");
        }
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public void ignorableWhitespace(char buffer[], int start, int len) throws SAXException {
        writeline("/home/ubuntu/results/coverage/Digester/Digester_1_10.coverage", "0462764c-cd1d-422e-80c9-0ecf02061518");
        if (saxLog.isDebugEnabled()) {
            writeline("/home/ubuntu/results/coverage/Digester/Digester_1_10.coverage", "5884660c-866c-4baf-ac1d-4b6d032a242b");
            saxLog.debug("ignorableWhitespace(" + new String(buffer, start, len) + ")");
        }
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public void processingInstruction(String target, String data) throws SAXException {
        writeline("/home/ubuntu/results/coverage/Digester/Digester_1_10.coverage", "11cfd027-583c-4d28-88d4-57bf38e3dc98");
        if (customContentHandler != null) {
            writeline("/home/ubuntu/results/coverage/Digester/Digester_1_10.coverage", "48f0acf5-1297-4e33-806a-99368f72d0a5");
            customContentHandler.processingInstruction(target, data);
            writeline("/home/ubuntu/results/coverage/Digester/Digester_1_10.coverage", "4124be0c-02b9-4dcc-89e9-3c8de8865c08");
            return;
        }
        writeline("/home/ubuntu/results/coverage/Digester/Digester_1_10.coverage", "d07f2f3c-5743-45f2-a0a1-dc839f991a6a");
        if (saxLog.isDebugEnabled()) {
            writeline("/home/ubuntu/results/coverage/Digester/Digester_1_10.coverage", "9f46f01d-ab29-47c1-b970-b4adb9faf2e3");
            saxLog.debug("processingInstruction('" + target + "','" + data + "')");
        }
    }

    /**
     * Gets the document locator associated with our parser.
     *
     * @return the Locator supplied by the document parser
     */
    public Locator getDocumentLocator() {
        writeline("/home/ubuntu/results/coverage/Digester/Digester_1_10.coverage", "d1a74cd9-855b-4cdf-bbd9-f92a0e1fb68a");
        return locator;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public void setDocumentLocator(Locator locator) {
        writeline("/home/ubuntu/results/coverage/Digester/Digester_1_10.coverage", "3b5e3f21-0639-4a25-bede-09a5836e707c");
        if (saxLog.isDebugEnabled()) {
            writeline("/home/ubuntu/results/coverage/Digester/Digester_1_10.coverage", "76bd2c9d-ad07-40e0-a379-6e8eacd291ba");
            saxLog.debug("setDocumentLocator(" + locator + ")");
        }
        writeline("/home/ubuntu/results/coverage/Digester/Digester_1_10.coverage", "c0d2eab7-d998-400d-88f5-78b64dbcec4e");
        this.locator = locator;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public void skippedEntity(String name) throws SAXException {
        writeline("/home/ubuntu/results/coverage/Digester/Digester_1_10.coverage", "2d7e1cef-0dd5-4e0d-8019-b2f2c936760c");
        if (saxLog.isDebugEnabled()) {
            writeline("/home/ubuntu/results/coverage/Digester/Digester_1_10.coverage", "5ef260ed-87f0-46da-925b-c93f0c180d3b");
            saxLog.debug("skippedEntity(" + name + ")");
        }
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public void startDocument() throws SAXException {
        writeline("/home/ubuntu/results/coverage/Digester/Digester_1_10.coverage", "04efb935-6b39-4d2b-9547-2e6a34f2ea51");
        if (saxLog.isDebugEnabled()) {
            writeline("/home/ubuntu/results/coverage/Digester/Digester_1_10.coverage", "6363c405-808c-4a68-a6fa-36af90351899");
            saxLog.debug("startDocument()");
        }
        writeline("/home/ubuntu/results/coverage/Digester/Digester_1_10.coverage", "64d6bf3e-0e02-4bc1-997f-e4919772ea8b");
        configure();
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public void startElement(String namespaceURI, String localName, String qName, Attributes list) throws SAXException {
        writeline("/home/ubuntu/results/coverage/Digester/Digester_1_10.coverage", "2615d43b-ac11-42c8-a2e0-b2ee38bfa683");
        boolean debug = log.isDebugEnabled();
        writeline("/home/ubuntu/results/coverage/Digester/Digester_1_10.coverage", "1c56a8a1-c266-4c6d-875f-71a7a6669e9d");
        if (customContentHandler != null) {
            writeline("/home/ubuntu/results/coverage/Digester/Digester_1_10.coverage", "20d0226f-8b4f-480b-a966-0d2c37159201");
            customContentHandler.startElement(namespaceURI, localName, qName, list);
            writeline("/home/ubuntu/results/coverage/Digester/Digester_1_10.coverage", "fd373d63-4be5-451c-b018-1a7efdec2741");
            return;
        }
        writeline("/home/ubuntu/results/coverage/Digester/Digester_1_10.coverage", "67f8269f-d846-42b4-8aaa-b7aa5fa3db85");
        if (saxLog.isDebugEnabled()) {
            writeline("/home/ubuntu/results/coverage/Digester/Digester_1_10.coverage", "7b9f942c-6417-4c26-8f49-b09404aa9e64");
            saxLog.debug("startElement(" + namespaceURI + "," + localName + "," + qName + ")");
        }
        writeline("/home/ubuntu/results/coverage/Digester/Digester_1_10.coverage", "e8654a11-47ac-492d-beee-e34e86c43db3");
        bodyTexts.push(bodyText);
        writeline("/home/ubuntu/results/coverage/Digester/Digester_1_10.coverage", "906f79b9-eec3-41f3-b60c-e56c0f4d4011");
        if (debug) {
            writeline("/home/ubuntu/results/coverage/Digester/Digester_1_10.coverage", "fb9dbd32-b827-4d72-a1a9-39fc01611715");
            log.debug("  Pushing body text '" + bodyText.toString() + "'");
        }
        writeline("/home/ubuntu/results/coverage/Digester/Digester_1_10.coverage", "c797b4c3-aa05-4869-aa82-23049deb98ae");
        bodyText = new StringBuilder();
        writeline("/home/ubuntu/results/coverage/Digester/Digester_1_10.coverage", "f1c0e402-14e4-4c00-84f9-774245756ae0");
        String name = localName;
        writeline("/home/ubuntu/results/coverage/Digester/Digester_1_10.coverage", "70028855-dadb-44ab-b8f5-458e8cd3c1a0");
        if ((name == null) || (name.length() < 1)) {
            writeline("/home/ubuntu/results/coverage/Digester/Digester_1_10.coverage", "36b2555f-bcdf-4b58-ba3c-5567e0fbdb3f");
            name = qName;
        }
        writeline("/home/ubuntu/results/coverage/Digester/Digester_1_10.coverage", "c487e861-5061-4c39-a256-5aa0d155b46c");
        StringBuilder sb = new StringBuilder(match);
        writeline("/home/ubuntu/results/coverage/Digester/Digester_1_10.coverage", "43119958-28d2-44f6-b9b8-07a3f4d8f88f");
        if (match.length() > 0) {
            writeline("/home/ubuntu/results/coverage/Digester/Digester_1_10.coverage", "2b0de4f7-48f5-4f0f-af93-5f1fa2592f7f");
            sb.append('/');
        }
        writeline("/home/ubuntu/results/coverage/Digester/Digester_1_10.coverage", "00cfa55c-ef8c-48ed-a8d3-c0dcdbb115d8");
        sb.append(name);
        writeline("/home/ubuntu/results/coverage/Digester/Digester_1_10.coverage", "196c943c-e42c-4a25-bd0e-9886d7d500e9");
        match = sb.toString();
        writeline("/home/ubuntu/results/coverage/Digester/Digester_1_10.coverage", "814031c1-b6f8-4f4c-9651-604cd896fbb2");
        if (debug) {
            writeline("/home/ubuntu/results/coverage/Digester/Digester_1_10.coverage", "06129d0f-0147-45be-83e9-0ee04ee02e06");
            log.debug("  New match='" + match + "'");
        }
        writeline("/home/ubuntu/results/coverage/Digester/Digester_1_10.coverage", "515bb894-2329-4f3b-a4a2-bb0289dae469");
        List<Rule> rules = getRules().match(namespaceURI, match, localName, list);
        writeline("/home/ubuntu/results/coverage/Digester/Digester_1_10.coverage", "952c90a1-877f-4eea-a3a0-0d459772a77d");
        matches.push(rules);
        writeline("/home/ubuntu/results/coverage/Digester/Digester_1_10.coverage", "e8e62006-deee-498d-9a86-af8d9a19c5eb");
        if ((rules != null) && (rules.size() > 0)) {
            writeline("/home/ubuntu/results/coverage/Digester/Digester_1_10.coverage", "ad6799c4-050b-4eec-9cfd-b85d45463b31");
            Substitutor substitutor = getSubstitutor();
            writeline("/home/ubuntu/results/coverage/Digester/Digester_1_10.coverage", "6b58087d-0be6-4684-a481-78b6c3ae21ce");
            if (substitutor != null) {
                writeline("/home/ubuntu/results/coverage/Digester/Digester_1_10.coverage", "af25d52b-ea22-4627-a9ec-94f5dfe2079a");
                list = substitutor.substitute(list);
            }
            writeline("/home/ubuntu/results/coverage/Digester/Digester_1_10.coverage", "377177f4-b1b7-45a8-af29-91b1823a2d9c");
            for (int i = 0; i < rules.size(); i++) {
                writeline("/home/ubuntu/results/coverage/Digester/Digester_1_10.coverage", "651d6d3e-76e6-4a8e-b20f-629f0a652499");
                try {
                    writeline("/home/ubuntu/results/coverage/Digester/Digester_1_10.coverage", "24202cb8-94f1-4752-a256-dc40c1bacfdf");
                    Rule rule = rules.get(i);
                    writeline("/home/ubuntu/results/coverage/Digester/Digester_1_10.coverage", "7a974768-c971-4500-a6fe-071e4f6cc2e0");
                    if (debug) {
                        writeline("/home/ubuntu/results/coverage/Digester/Digester_1_10.coverage", "93834a03-63a6-4638-a2e6-782c9ee4d1c0");
                        log.debug("  Fire begin() for " + rule);
                    }
                    writeline("/home/ubuntu/results/coverage/Digester/Digester_1_10.coverage", "e5ec94af-f91a-475b-8469-81c1be46c4ae");
                    rule.begin(namespaceURI, name, list);
                } catch (Exception e) {
                    writeline("/home/ubuntu/results/coverage/Digester/Digester_1_10.coverage", "e5e5029b-502a-479d-927d-a4fea174c289");
                    log.error("Begin event threw exception", e);
                    writeline("/home/ubuntu/results/coverage/Digester/Digester_1_10.coverage", "2ddaa34f-e831-4368-9c0b-65770d145d25");
                    throw createSAXException(e);
                } catch (Error e) {
                    writeline("/home/ubuntu/results/coverage/Digester/Digester_1_10.coverage", "0278eb17-1284-4a2e-8bf4-5d0d6d6b30be");
                    log.error("Begin event threw error", e);
                    writeline("/home/ubuntu/results/coverage/Digester/Digester_1_10.coverage", "9539b7e7-37b8-435d-8e39-b0f9d2ccbb32");
                    throw e;
                }
            }
        } else {
            writeline("/home/ubuntu/results/coverage/Digester/Digester_1_10.coverage", "90c9af9a-0ae0-4bc4-b6bb-25986455b4bd");
            if (debug) {
                writeline("/home/ubuntu/results/coverage/Digester/Digester_1_10.coverage", "c92203fd-c38a-407c-9ba4-ac5a2c1e7243");
                log.debug("  No rules found matching '" + match + "'.");
            }
        }
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public void startPrefixMapping(String prefix, String namespaceURI) throws SAXException {
        writeline("/home/ubuntu/results/coverage/Digester/Digester_1_10.coverage", "4597eb3a-bbfc-4915-a51b-4b955916b0a3");
        if (saxLog.isDebugEnabled()) {
            writeline("/home/ubuntu/results/coverage/Digester/Digester_1_10.coverage", "ebaaae4e-3432-49c3-b1a6-65de14130880");
            saxLog.debug("startPrefixMapping(" + prefix + "," + namespaceURI + ")");
        }
        writeline("/home/ubuntu/results/coverage/Digester/Digester_1_10.coverage", "bc627de9-8b0a-4f12-8172-195e25d98418");
        Stack<String> stack = namespaces.get(prefix);
        writeline("/home/ubuntu/results/coverage/Digester/Digester_1_10.coverage", "bc7f8991-99a9-4ab6-ac57-82a4d6c4506c");
        if (stack == null) {
            writeline("/home/ubuntu/results/coverage/Digester/Digester_1_10.coverage", "871f0516-e585-4cf5-a14a-a0f262816344");
            stack = new Stack<String>();
            writeline("/home/ubuntu/results/coverage/Digester/Digester_1_10.coverage", "9dc4f826-204f-4ab1-ac2d-218e790d53f6");
            namespaces.put(prefix, stack);
        }
        writeline("/home/ubuntu/results/coverage/Digester/Digester_1_10.coverage", "d83ab6f5-8193-4d43-ada1-4deabf5e94b9");
        stack.push(namespaceURI);
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public void notationDecl(String name, String publicId, String systemId) {
        writeline("/home/ubuntu/results/coverage/Digester/Digester_1_10.coverage", "150ef9f7-03f1-40cd-b6d2-76c024eeb8f7");
        if (saxLog.isDebugEnabled()) {
            writeline("/home/ubuntu/results/coverage/Digester/Digester_1_10.coverage", "3ca7434e-5e3d-4918-90e0-4a76ff86c03f");
            saxLog.debug("notationDecl(" + name + "," + publicId + "," + systemId + ")");
        }
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public void unparsedEntityDecl(String name, String publicId, String systemId, String notation) {
        writeline("/home/ubuntu/results/coverage/Digester/Digester_1_10.coverage", "416832f9-fdf9-4257-aacc-16a26076c96b");
        if (saxLog.isDebugEnabled()) {
            writeline("/home/ubuntu/results/coverage/Digester/Digester_1_10.coverage", "d9610b82-d53b-4082-bbde-5106eec2ea75");
            saxLog.debug("unparsedEntityDecl(" + name + "," + publicId + "," + systemId + "," + notation + ")");
        }
    }

    /**
     * Set the <code>EntityResolver</code> used by SAX when resolving public id and system id. This must be called
     * before the first call to <code>parse()</code>.
     *
     * @param entityResolver a class that implement the <code>EntityResolver</code> interface.
     */
    public void setEntityResolver(EntityResolver entityResolver) {
        writeline("/home/ubuntu/results/coverage/Digester/Digester_1_10.coverage", "2d3a3ee7-24df-4598-9a67-41f36d5c6c52");
        this.entityResolver = entityResolver;
    }

    /**
     * Return the Entity Resolver used by the SAX parser.
     *
     * @return the Entity Resolver used by the SAX parser.
     */
    public EntityResolver getEntityResolver() {
        writeline("/home/ubuntu/results/coverage/Digester/Digester_1_10.coverage", "04f799a4-b500-418e-886b-986cca0df0fa");
        return entityResolver;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public InputSource resolveEntity(String publicId, String systemId) throws SAXException {
        writeline("/home/ubuntu/results/coverage/Digester/Digester_1_10.coverage", "4a1088ef-d23a-4173-bbac-f99a172c7f82");
        if (saxLog.isDebugEnabled()) {
            writeline("/home/ubuntu/results/coverage/Digester/Digester_1_10.coverage", "e45e6c8f-d6e6-4fe7-a9fd-583929f6c799");
            saxLog.debug("resolveEntity('" + publicId + "', '" + systemId + "')");
        }
        writeline("/home/ubuntu/results/coverage/Digester/Digester_1_10.coverage", "9bb72676-628a-44c6-aa66-df30478ab1d3");
        if (publicId != null) {
            writeline("/home/ubuntu/results/coverage/Digester/Digester_1_10.coverage", "d675ada1-30b1-43d3-bb9a-7b962af12dc0");
            this.publicId = publicId;
        }
        writeline("/home/ubuntu/results/coverage/Digester/Digester_1_10.coverage", "1d61161f-8bdc-406a-aa04-c5962ba92f09");
        URL entityURL = null;
        writeline("/home/ubuntu/results/coverage/Digester/Digester_1_10.coverage", "809c5dc6-af77-4948-828b-c4068b29ff64");
        if (publicId != null) {
            writeline("/home/ubuntu/results/coverage/Digester/Digester_1_10.coverage", "13d8bae9-a71c-4361-ad66-918c918b36b5");
            entityURL = entityValidator.get(publicId);
        }
        writeline("/home/ubuntu/results/coverage/Digester/Digester_1_10.coverage", "44eb010a-702f-4ec8-b187-359c434277a0");
        if (entityURL == null && systemId != null) {
            writeline("/home/ubuntu/results/coverage/Digester/Digester_1_10.coverage", "05f16635-19ca-46c1-944d-dd3b6686db07");
            entityURL = entityValidator.get(systemId);
        }
        writeline("/home/ubuntu/results/coverage/Digester/Digester_1_10.coverage", "470246e1-6c05-46ed-b3bf-11ffd3c7fc35");
        if (entityURL == null) {
            writeline("/home/ubuntu/results/coverage/Digester/Digester_1_10.coverage", "91469ca4-6ebf-4586-94e2-00060c089cf2");
            if (systemId == null) {
                writeline("/home/ubuntu/results/coverage/Digester/Digester_1_10.coverage", "34ed5c5b-6866-496d-9f0e-1b5f73fd456a");
                if (log.isDebugEnabled()) {
                    writeline("/home/ubuntu/results/coverage/Digester/Digester_1_10.coverage", "83bf9e6d-9aca-4a93-a0eb-118dcdd3b956");
                    log.debug(" Cannot resolve null entity, returning null InputSource");
                }
                writeline("/home/ubuntu/results/coverage/Digester/Digester_1_10.coverage", "4a55fa20-4750-462e-bee3-df4a9fb199f3");
                return (null);
            }
            writeline("/home/ubuntu/results/coverage/Digester/Digester_1_10.coverage", "f9c66732-9a51-4492-8e72-2d31f5d50ed2");
            if (log.isDebugEnabled()) {
                writeline("/home/ubuntu/results/coverage/Digester/Digester_1_10.coverage", "f8b13e25-0c95-491c-9df7-643365d89122");
                log.debug(" Trying to resolve using system ID '" + systemId + "'");
            }
            writeline("/home/ubuntu/results/coverage/Digester/Digester_1_10.coverage", "326bab47-472b-47e5-b544-08b3550b2e98");
            try {
                writeline("/home/ubuntu/results/coverage/Digester/Digester_1_10.coverage", "6997f899-9afb-4c29-940e-b60b9931fd95");
                entityURL = new URL(systemId);
            } catch (MalformedURLException e) {
                writeline("/home/ubuntu/results/coverage/Digester/Digester_1_10.coverage", "f15ce674-c7f3-4f31-89d2-53cf4095944f");
                throw new IllegalArgumentException("Malformed URL '" + systemId + "' : " + e.getMessage());
            }
        }
        writeline("/home/ubuntu/results/coverage/Digester/Digester_1_10.coverage", "98946edc-1a43-44ef-b841-75cd81be968f");
        if (log.isDebugEnabled()) {
            writeline("/home/ubuntu/results/coverage/Digester/Digester_1_10.coverage", "3efb4b3f-1a95-4452-8763-b43269e5b9cd");
            log.debug(" Resolving to alternate DTD '" + entityURL + "'");
        }
        writeline("/home/ubuntu/results/coverage/Digester/Digester_1_10.coverage", "bc50c5c8-edfc-4ff6-a488-daf3ae537cfc");
        try {
            writeline("/home/ubuntu/results/coverage/Digester/Digester_1_10.coverage", "696d60a5-48af-416a-b84b-8b9dc0275c5b");
            return createInputSourceFromURL(entityURL);
        } catch (Exception e) {
            writeline("/home/ubuntu/results/coverage/Digester/Digester_1_10.coverage", "253439cf-90b2-464e-9677-11f5a658bbf8");
            throw createSAXException(e);
        }
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public void error(SAXParseException exception) throws SAXException {
        writeline("/home/ubuntu/results/coverage/Digester/Digester_1_10.coverage", "337f28ce-f40e-43bf-afb7-2b4292b6fded");
        log.error("Parse Error at line " + exception.getLineNumber() + " column " + exception.getColumnNumber() + ": " + exception.getMessage(), exception);
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public void fatalError(SAXParseException exception) throws SAXException {
        writeline("/home/ubuntu/results/coverage/Digester/Digester_1_10.coverage", "d32243b2-98b3-41b0-85b3-380f5c7b0c12");
        log.error("Parse Fatal Error at line " + exception.getLineNumber() + " column " + exception.getColumnNumber() + ": " + exception.getMessage(), exception);
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public void warning(SAXParseException exception) throws SAXException {
        writeline("/home/ubuntu/results/coverage/Digester/Digester_1_10.coverage", "e803cc51-4195-4f18-ae23-df6ed2e530a5");
        log.warn("Parse Warning Error at line " + exception.getLineNumber() + " column " + exception.getColumnNumber() + ": " + exception.getMessage(), exception);
    }

    /**
     * Parse the content of the specified file using this Digester. Returns the root element from the object stack (if
     * any).
     *
     * @param <T> the type used to auto-cast the returned object to the assigned variable type
     * @param file File containing the XML data to be parsed
     * @return the root element from the object stack (if any)
     * @throws IOException if an input/output error occurs
     * @throws SAXException if a parsing exception occurs
     */
    public <T> T parse(File file) throws IOException, SAXException {
        writeline("/home/ubuntu/results/coverage/Digester/Digester_1_10.coverage", "eac32a10-0e48-41c0-91d6-03aec1bdfa55");
        if (file == null) {
            writeline("/home/ubuntu/results/coverage/Digester/Digester_1_10.coverage", "3786b6f9-5cb0-44c0-8cc1-b7923b853b8e");
            throw new IllegalArgumentException("File to parse is null");
        }
        writeline("/home/ubuntu/results/coverage/Digester/Digester_1_10.coverage", "4c57c752-bb31-4b05-9063-504b1fff31e9");
        InputSource input = new InputSource(new FileInputStream(file));
        writeline("/home/ubuntu/results/coverage/Digester/Digester_1_10.coverage", "7b0345f8-3bd2-4a76-92eb-aa366dcabefb");
        input.setSystemId(file.toURI().toURL().toString());
        writeline("/home/ubuntu/results/coverage/Digester/Digester_1_10.coverage", "c4539396-8e9b-4f53-9f92-5c52c34cbe32");
        return (this.<T>parse(input));
    }

    /**
     * Creates a Callable instance that parse the content of the specified reader using this Digester.
     *
     * @param <T> The result type returned by the returned Future's {@code get} method
     * @param file File containing the XML data to be parsed
     * @return a Future that can be used to track when the parse has been fully processed.
     * @see Digester#parse(File)
     * @since 3.1
     */
    public <T> Future<T> asyncParse(final File file) {
        writeline("/home/ubuntu/results/coverage/Digester/Digester_1_10.coverage", "29278949-bb65-467f-9625-17dd81d4cef4");
        return asyncParse(new Callable<T>() {

            public T call() throws Exception {
                return Digester.this.<T>parse(file);
            }
        });
    }

    /**
     * Parse the content of the specified input source using this Digester. Returns the root element from the object
     * stack (if any).
     *
     * @param <T> the type used to auto-cast the returned object to the assigned variable type
     * @param input Input source containing the XML data to be parsed
     * @return the root element from the object stack (if any)
     * @throws IOException if an input/output error occurs
     * @throws SAXException if a parsing exception occurs
     */
    public <T> T parse(InputSource input) throws IOException, SAXException {
        writeline("/home/ubuntu/results/coverage/Digester/Digester_1_10.coverage", "d8ed7153-09f7-4abf-b950-0e2d146ad481");
        if (input == null) {
            writeline("/home/ubuntu/results/coverage/Digester/Digester_1_10.coverage", "ca0f629c-7cf6-4a62-920b-03e276e4925f");
            throw new IllegalArgumentException("InputSource to parse is null");
        }
        writeline("/home/ubuntu/results/coverage/Digester/Digester_1_10.coverage", "b376f820-1ab4-4b67-a923-487e2352d8c0");
        configure();
        writeline("/home/ubuntu/results/coverage/Digester/Digester_1_10.coverage", "5d09adc6-c3b7-42e7-b261-b3f130303b50");
        String systemId = input.getSystemId();
        writeline("/home/ubuntu/results/coverage/Digester/Digester_1_10.coverage", "217da3f0-1122-4743-80dd-d1dbcd7f2df4");
        if (systemId == null) {
            writeline("/home/ubuntu/results/coverage/Digester/Digester_1_10.coverage", "5b23a1f7-6695-4751-b040-ef9253b1e40f");
            systemId = "(already loaded from stream)";
        }
        writeline("/home/ubuntu/results/coverage/Digester/Digester_1_10.coverage", "0b71280f-c8f6-4be2-ae6b-12eb835c7587");
        try {
            writeline("/home/ubuntu/results/coverage/Digester/Digester_1_10.coverage", "80a66be3-3086-417c-8fe2-4755f4e5f30d");
            getXMLReader().parse(input);
        } catch (IOException e) {
            writeline("/home/ubuntu/results/coverage/Digester/Digester_1_10.coverage", "49c7689c-f60f-41bb-8a72-c5aab5ecb097");
            log.error(format("An error occurred while reading stream from '%s', see nested exceptions", systemId), e);
            writeline("/home/ubuntu/results/coverage/Digester/Digester_1_10.coverage", "04b385da-6d69-4760-bc54-9fab25758413");
            throw e;
        }
        writeline("/home/ubuntu/results/coverage/Digester/Digester_1_10.coverage", "77188f6a-fa5e-4d22-9f04-64e1b9e9ec78");
        cleanup();
        writeline("/home/ubuntu/results/coverage/Digester/Digester_1_10.coverage", "608c413c-8a49-4ab1-bf81-38037394e99d");
        return this.<T>getRoot();
    }

    /**
     * Creates a Callable instance that parse the content of the specified reader using this Digester.
     *
     * @param <T> The result type returned by the returned Future's {@code get} method
     * @param input Input source containing the XML data to be parsed
     * @return a Future that can be used to track when the parse has been fully processed.
     * @see Digester#parse(InputSource)
     * @since 3.1
     */
    public <T> Future<T> asyncParse(final InputSource input) {
        writeline("/home/ubuntu/results/coverage/Digester/Digester_1_10.coverage", "b4d39e32-8c21-496e-a7cc-879185996e59");
        return asyncParse(new Callable<T>() {

            public T call() throws Exception {
                return Digester.this.<T>parse(input);
            }
        });
    }

    /**
     * Parse the content of the specified input stream using this Digester. Returns the root element from the object
     * stack (if any).
     *
     * @param <T> the type used to auto-cast the returned object to the assigned variable type
     * @param input Input stream containing the XML data to be parsed
     * @return the root element from the object stack (if any)
     * @throws IOException if an input/output error occurs
     * @throws SAXException if a parsing exception occurs
     */
    public <T> T parse(InputStream input) throws IOException, SAXException {
        writeline("/home/ubuntu/results/coverage/Digester/Digester_1_10.coverage", "7ed4eb6c-4951-4454-b813-171f01d81c42");
        if (input == null) {
            writeline("/home/ubuntu/results/coverage/Digester/Digester_1_10.coverage", "3780c843-4d5b-4c19-b1c3-a64ef8c72dbb");
            throw new IllegalArgumentException("InputStream to parse is null");
        }
        writeline("/home/ubuntu/results/coverage/Digester/Digester_1_10.coverage", "f3be60b2-8661-4ba3-97ed-f743babcbabb");
        return (this.<T>parse(new InputSource(input)));
    }

    /**
     * Creates a Callable instance that parse the content of the specified reader using this Digester.
     *
     * @param <T> The result type returned by the returned Future's {@code get} method
     * @param input Input stream containing the XML data to be parsed
     * @return a Future that can be used to track when the parse has been fully processed.
     * @see Digester#parse(InputStream)
     * @since 3.1
     */
    public <T> Future<T> asyncParse(final InputStream input) {
        writeline("/home/ubuntu/results/coverage/Digester/Digester_1_10.coverage", "239f2818-e82d-4c81-b88b-03fb735ffd2b");
        return asyncParse(new Callable<T>() {

            public T call() throws Exception {
                return Digester.this.<T>parse(input);
            }
        });
    }

    /**
     * Parse the content of the specified reader using this Digester. Returns the root element from the object stack (if
     * any).
     *
     * @param <T> the type used to auto-cast the returned object to the assigned variable type
     * @param reader Reader containing the XML data to be parsed
     * @return the root element from the object stack (if any)
     * @throws IOException if an input/output error occurs
     * @throws SAXException if a parsing exception occurs
     */
    public <T> T parse(Reader reader) throws IOException, SAXException {
        writeline("/home/ubuntu/results/coverage/Digester/Digester_1_10.coverage", "f1704d82-4f80-49c5-aefd-3bf7e1cf57d4");
        if (reader == null) {
            writeline("/home/ubuntu/results/coverage/Digester/Digester_1_10.coverage", "87260beb-d8ae-4289-9bc4-3ac14022e19c");
            throw new IllegalArgumentException("Reader to parse is null");
        }
        writeline("/home/ubuntu/results/coverage/Digester/Digester_1_10.coverage", "87d60289-0000-46e1-addc-2d0db103512f");
        return (this.<T>parse(new InputSource(reader)));
    }

    /**
     * Creates a Callable instance that parse the content of the specified reader using this Digester.
     *
     * @param <T> The result type returned by the returned Future's {@code get} method
     * @param reader Reader containing the XML data to be parsed
     * @return a Future that can be used to track when the parse has been fully processed.
     * @see Digester#parse(Reader)
     * @since 3.1
     */
    public <T> Future<T> asyncParse(final Reader reader) {
        writeline("/home/ubuntu/results/coverage/Digester/Digester_1_10.coverage", "55cb0a65-6e27-461d-bdc7-601d9386f954");
        return asyncParse(new Callable<T>() {

            public T call() throws Exception {
                return Digester.this.<T>parse(reader);
            }
        });
    }

    /**
     * Parse the content of the specified URI using this Digester. Returns the root element from the object stack (if
     * any).
     *
     * @param <T> the type used to auto-cast the returned object to the assigned variable type
     * @param uri URI containing the XML data to be parsed
     * @return the root element from the object stack (if any)
     * @throws IOException if an input/output error occurs
     * @throws SAXException if a parsing exception occurs
     */
    public <T> T parse(String uri) throws IOException, SAXException {
        writeline("/home/ubuntu/results/coverage/Digester/Digester_1_10.coverage", "361e134d-2457-48f5-bef1-6e4a0b6bb24e");
        if (uri == null) {
            writeline("/home/ubuntu/results/coverage/Digester/Digester_1_10.coverage", "867d2f8a-3984-4680-b831-458959756ee6");
            throw new IllegalArgumentException("String URI to parse is null");
        }
        writeline("/home/ubuntu/results/coverage/Digester/Digester_1_10.coverage", "4848d380-a0c4-498d-bd8c-cc6bbd265ad6");
        return (this.<T>parse(createInputSourceFromURL(uri)));
    }

    /**
     * Creates a Callable instance that parse the content of the specified reader using this Digester.
     *
     * @param <T> The result type returned by the returned Future's {@code get} method
     * @param uri URI containing the XML data to be parsed
     * @return a Future that can be used to track when the parse has been fully processed.
     * @see Digester#parse(String)
     * @since 3.1
     */
    public <T> Future<T> asyncParse(final String uri) {
        writeline("/home/ubuntu/results/coverage/Digester/Digester_1_10.coverage", "1c03a543-f8e9-4044-aad7-01cbb7f40b22");
        return asyncParse(new Callable<T>() {

            public T call() throws Exception {
                return Digester.this.<T>parse(uri);
            }
        });
    }

    /**
     * Parse the content of the specified URL using this Digester. Returns the root element from the object stack (if
     * any).
     *
     * @param <T> the type used to auto-cast the returned object to the assigned variable type
     * @param url URL containing the XML data to be parsed
     * @return the root element from the object stack (if any)
     * @throws IOException if an input/output error occurs
     * @throws SAXException if a parsing exception occurs
     * @since 1.8
     */
    public <T> T parse(URL url) throws IOException, SAXException {
        writeline("/home/ubuntu/results/coverage/Digester/Digester_1_10.coverage", "de04a9b1-4bf5-49ed-8cfa-49a49366fe70");
        if (url == null) {
            writeline("/home/ubuntu/results/coverage/Digester/Digester_1_10.coverage", "3a4fdd9e-a6ae-4405-8047-660ebd8533bb");
            throw new IllegalArgumentException("URL to parse is null");
        }
        writeline("/home/ubuntu/results/coverage/Digester/Digester_1_10.coverage", "cf14cc53-9f9a-4f78-8956-52c58827b6ad");
        return (this.<T>parse(createInputSourceFromURL(url)));
    }

    /**
     * Creates a Callable instance that parse the content of the specified reader using this Digester.
     *
     * @param <T> The result type returned by the returned Future's {@code get} method
     * @param url URL containing the XML data to be parsed
     * @return a Future that can be used to track when the parse has been fully processed.
     * @see Digester#parse(URL)
     * @since 3.1
     */
    public <T> Future<T> asyncParse(final URL url) {
        writeline("/home/ubuntu/results/coverage/Digester/Digester_1_10.coverage", "c449e063-b83c-44f2-acb2-28cc20e5700f");
        return asyncParse(new Callable<T>() {

            public T call() throws Exception {
                return Digester.this.<T>parse(url);
            }
        });
    }

    /**
     * Execute the parse in async mode.
     *
     * @param <T> the type used to auto-cast the returned object to the assigned variable type
     * @param callable
     * @return a Future that can be used to track when the parse has been fully processed.
     * @since 3.1
     */
    private <T> Future<T> asyncParse(Callable<T> callable) {
        writeline("/home/ubuntu/results/coverage/Digester/Digester_1_10.coverage", "4c70f48c-da0c-409c-b08f-ef4034af655d");
        if (executorService == null) {
            writeline("/home/ubuntu/results/coverage/Digester/Digester_1_10.coverage", "748c3e72-6463-45c3-b1e2-922a64e83a8e");
            throw new IllegalStateException("ExecutorService not set");
        }
        writeline("/home/ubuntu/results/coverage/Digester/Digester_1_10.coverage", "56ee4aec-0e97-4e71-a17b-4ecc2faba5cc");
        return executorService.submit(callable);
    }

    /**
     * <p>
     * Register the specified DTD URL for the specified public identifier. This must be called before the first call to
     * <code>parse()</code>.
     * </p>
     * <p>
     * <code>Digester</code> contains an internal <code>EntityResolver</code> implementation. This maps
     * <code>PUBLICID</code>'s to URLs (from which the resource will be loaded). A common use case for this method is to
     * register local URLs (possibly computed at runtime by a classloader) for DTDs. This allows the performance
     * advantage of using a local version without having to ensure every <code>SYSTEM</code> URI on every processed xml
     * document is local. This implementation provides only basic functionality. If more sophisticated features are
     * required, using {@link #setEntityResolver} to set a custom resolver is recommended.
     * </p>
     * <p>
     * <strong>Note:</strong> This method will have no effect when a custom <code>EntityResolver</code> has been set.
     * (Setting a custom <code>EntityResolver</code> overrides the internal implementation.)
     * </p>
     *
     * @param publicId Public identifier of the DTD to be resolved
     * @param entityURL The URL to use for reading this DTD
     * @since 1.8
     */
    public void register(String publicId, URL entityURL) {
        writeline("/home/ubuntu/results/coverage/Digester/Digester_1_10.coverage", "2d009148-62cd-4d9c-9468-70ae090be8e2");
        if (log.isDebugEnabled()) {
            writeline("/home/ubuntu/results/coverage/Digester/Digester_1_10.coverage", "148e2d96-6af7-4577-a68f-488a6a73382e");
            log.debug("register('" + publicId + "', '" + entityURL + "'");
        }
        writeline("/home/ubuntu/results/coverage/Digester/Digester_1_10.coverage", "d0459ee2-9490-429b-a62b-f2e8cdfd76c3");
        entityValidator.put(publicId, entityURL);
    }

    /**
     * <p>
     * Convenience method that registers the string version of an entity URL instead of a URL version.
     * </p>
     *
     * @param publicId Public identifier of the entity to be resolved
     * @param entityURL The URL to use for reading this entity
     */
    public void register(String publicId, String entityURL) {
        writeline("/home/ubuntu/results/coverage/Digester/Digester_1_10.coverage", "a20061e1-8c14-4432-ba83-0a94b8495511");
        if (log.isDebugEnabled()) {
            writeline("/home/ubuntu/results/coverage/Digester/Digester_1_10.coverage", "1c6be944-84ff-45fb-bd38-5ccfbb5be112");
            log.debug("register('" + publicId + "', '" + entityURL + "'");
        }
        writeline("/home/ubuntu/results/coverage/Digester/Digester_1_10.coverage", "9bedfc5a-77d3-473f-a213-43b5d8cfaef0");
        try {
            writeline("/home/ubuntu/results/coverage/Digester/Digester_1_10.coverage", "39289eb6-68f1-47c8-bc88-7e455e73d4aa");
            entityValidator.put(publicId, new URL(entityURL));
        } catch (MalformedURLException e) {
            writeline("/home/ubuntu/results/coverage/Digester/Digester_1_10.coverage", "f8bb716e-103d-4356-a564-be5af2d02277");
            throw new IllegalArgumentException("Malformed URL '" + entityURL + "' : " + e.getMessage());
        }
    }

    /**
     * Convenience method that registers DTD URLs for the specified public identifiers.
     *
     * @param entityValidator The URLs of entityValidator that have been registered, keyed by the public
     *                        identifier that corresponds.
     * @since 3.0
     */
    public void registerAll(Map<String, URL> entityValidator) {
        writeline("/home/ubuntu/results/coverage/Digester/Digester_1_10.coverage", "6c5e4694-9362-4225-ad8a-a9d982da1dc5");
        this.entityValidator.putAll(entityValidator);
    }

    /**
     * <p>
     * <code>List</code> of <code>InputSource</code> instances created by a <code>createInputSourceFromURL()</code>
     * method call. These represent open input streams that need to be closed to avoid resource leaks, as well as
     * potentially locked JAR files on Windows.
     * </p>
     */
    protected List<InputSource> inputSources = new ArrayList<InputSource>(5);

    /**
     * Given a URL, return an InputSource that reads from that URL.
     * <p>
     * Ideally this function would not be needed and code could just use <code>new InputSource(entityURL)</code>.
     * Unfortunately it appears that when the entityURL points to a file within a jar archive a caching mechanism inside
     * the InputSource implementation causes a file-handle to the jar file to remain open. On Windows systems this then
     * causes the jar archive file to be locked on disk ("in use") which makes it impossible to delete the jar file -
     * and that really stuffs up "undeploy" in webapps in particular.
     * <p>
     * In JDK1.4 and later, Apache XercesJ is used as the xml parser. The InputSource object provided is converted into
     * an XMLInputSource, and eventually passed to an instance of XMLDocumentScannerImpl to specify the source data to
     * be converted into tokens for the rest of the XMLReader code to handle. XMLDocumentScannerImpl calls
     * fEntityManager.startDocumentEntity(source), where fEntityManager is declared in ancestor class XMLScanner to be
     * an XMLEntityManager. In that class, if the input source stream is null, then:
     *
     * <pre>
     * URL location = new URL( expandedSystemId );
     * URLConnection connect = location.openConnection();
     * if ( connect instanceof HttpURLConnection )
     * {
     *     setHttpProperties( connect, xmlInputSource );
     * }
     * stream = connect.getInputStream();
     * </pre>
     *
     * This method pretty much duplicates the standard behaviour, except that it calls URLConnection.setUseCaches(false)
     * before opening the connection.
     *
     * @param url The URL has to be read
     * @return The InputSource that reads from the input URL
     * @throws IOException if any error occurs while reading the input URL
     * @since 1.8
     */
    public InputSource createInputSourceFromURL(URL url) throws IOException {
        writeline("/home/ubuntu/results/coverage/Digester/Digester_1_10.coverage", "9b3f3343-53b6-4824-b9d9-573352a9878b");
        URLConnection connection = url.openConnection();
        writeline("/home/ubuntu/results/coverage/Digester/Digester_1_10.coverage", "a6e726aa-3e36-46b1-a0c4-33491698a059");
        connection.setUseCaches(false);
        writeline("/home/ubuntu/results/coverage/Digester/Digester_1_10.coverage", "afa39583-f542-460c-a354-a795163b0100");
        InputStream stream = connection.getInputStream();
        writeline("/home/ubuntu/results/coverage/Digester/Digester_1_10.coverage", "b64deeb9-d510-454f-a986-8258f5f2270d");
        InputSource source = new InputSource(stream);
        writeline("/home/ubuntu/results/coverage/Digester/Digester_1_10.coverage", "5cd1eb06-78a8-4443-a8bf-27ab560050f6");
        source.setSystemId(url.toExternalForm());
        writeline("/home/ubuntu/results/coverage/Digester/Digester_1_10.coverage", "20f87034-b3ad-47ba-a45c-d1335d3165fe");
        inputSources.add(source);
        writeline("/home/ubuntu/results/coverage/Digester/Digester_1_10.coverage", "30d6ab97-32e2-4d3e-88fe-123dc65cf665");
        return source;
    }

    /**
     * <p>
     * Convenience method that creates an <code>InputSource</code> from the string version of a URL.
     * </p>
     *
     * @param url URL for which to create an <code>InputSource</code>
     * @return The InputSource that reads from the input URL
     * @throws IOException if any error occurs while reading the input URL
     * @since 1.8
     */
    public InputSource createInputSourceFromURL(String url) throws IOException {
        writeline("/home/ubuntu/results/coverage/Digester/Digester_1_10.coverage", "e07fa608-e4a9-43e8-ac0a-64b0f264a502");
        return createInputSourceFromURL(new URL(url));
    }

    /**
     * <p>
     * Register a new Rule matching the specified pattern. This method sets the <code>Digester</code> property on the
     * rule.
     * </p>
     *
     * @param pattern Element matching pattern
     * @param rule Rule to be registered
     */
    public void addRule(String pattern, Rule rule) {
        writeline("/home/ubuntu/results/coverage/Digester/Digester_1_10.coverage", "90709d62-4f99-4ba3-b105-48c743d71c56");
        rule.setDigester(this);
        writeline("/home/ubuntu/results/coverage/Digester/Digester_1_10.coverage", "ef9a8db9-4d96-4a90-97a7-7cdc5adfc238");
        getRules().add(pattern, rule);
    }

    /**
     * Register a set of Rule instances defined in a RuleSet.
     *
     * @param ruleSet The RuleSet instance to configure from
     */
    public void addRuleSet(RuleSet ruleSet) {
        writeline("/home/ubuntu/results/coverage/Digester/Digester_1_10.coverage", "d913a799-f0dd-4b66-aa47-1b2dfdad3dc7");
        String oldNamespaceURI = getRuleNamespaceURI();
        writeline("/home/ubuntu/results/coverage/Digester/Digester_1_10.coverage", "6e4f9ed6-a557-446b-98c4-d8f07d857274");
        String newNamespaceURI = ruleSet.getNamespaceURI();
        writeline("/home/ubuntu/results/coverage/Digester/Digester_1_10.coverage", "2d416aa4-3727-4bbb-b21a-0d5a458c5164");
        if (log.isDebugEnabled()) {
            writeline("/home/ubuntu/results/coverage/Digester/Digester_1_10.coverage", "8c31b4c5-b89e-4010-8929-d8f37ee2ef81");
            if (newNamespaceURI == null) {
                writeline("/home/ubuntu/results/coverage/Digester/Digester_1_10.coverage", "9b521497-0488-47b9-bf0c-ad2d8d0affa1");
                log.debug("addRuleSet() with no namespace URI");
            } else {
                writeline("/home/ubuntu/results/coverage/Digester/Digester_1_10.coverage", "4e61ce4d-b396-45dc-a14f-6d79901380fd");
                log.debug("addRuleSet() with namespace URI " + newNamespaceURI);
            }
        }
        writeline("/home/ubuntu/results/coverage/Digester/Digester_1_10.coverage", "63dab78d-b6ce-49eb-90d3-305c61a2e01a");
        setRuleNamespaceURI(newNamespaceURI);
        writeline("/home/ubuntu/results/coverage/Digester/Digester_1_10.coverage", "c259d21e-b2a8-47c1-a969-d72bd9f0955a");
        ruleSet.addRuleInstances(this);
        writeline("/home/ubuntu/results/coverage/Digester/Digester_1_10.coverage", "14109d8f-c847-418f-9bd0-5f7074239d93");
        setRuleNamespaceURI(oldNamespaceURI);
    }

    /**
     * Add a "bean property setter" rule for the specified parameters.
     *
     * @param pattern Element matching pattern
     * @see BeanPropertySetterRule
     */
    public void addBeanPropertySetter(String pattern) {
        writeline("/home/ubuntu/results/coverage/Digester/Digester_1_10.coverage", "9a1a7d7d-529a-4b5a-b347-aa3cb1183f83");
        addRule(pattern, new BeanPropertySetterRule());
    }

    /**
     * Add a "bean property setter" rule for the specified parameters.
     *
     * @param pattern Element matching pattern
     * @param propertyName Name of property to set
     * @see BeanPropertySetterRule
     */
    public void addBeanPropertySetter(String pattern, String propertyName) {
        writeline("/home/ubuntu/results/coverage/Digester/Digester_1_10.coverage", "1ea61fe8-da12-42ca-b664-2b0bae2956cb");
        addRule(pattern, new BeanPropertySetterRule(propertyName));
    }

    /**
     * Add an "call method" rule for a method which accepts no arguments.
     *
     * @param pattern Element matching pattern
     * @param methodName Method name to be called
     * @see CallMethodRule
     */
    public void addCallMethod(String pattern, String methodName) {
        writeline("/home/ubuntu/results/coverage/Digester/Digester_1_10.coverage", "8644db56-6c9a-44fe-96ed-76763e0a17a0");
        addRule(pattern, new CallMethodRule(methodName));
    }

    /**
     * Add an "call method" rule for the specified parameters.
     *
     * @param pattern Element matching pattern
     * @param methodName Method name to be called
     * @param paramCount Number of expected parameters (or zero for a single parameter from the body of this element)
     * @see CallMethodRule
     */
    public void addCallMethod(String pattern, String methodName, int paramCount) {
        writeline("/home/ubuntu/results/coverage/Digester/Digester_1_10.coverage", "4f1572d2-0324-476e-b6fa-5df9f7cde31f");
        addRule(pattern, new CallMethodRule(methodName, paramCount));
    }

    /**
     * Add an "call method" rule for the specified parameters. If <code>paramCount</code> is set to zero the rule will
     * use the body of the matched element as the single argument of the method, unless <code>paramTypes</code> is null
     * or empty, in this case the rule will call the specified method with no arguments.
     *
     * @param pattern Element matching pattern
     * @param methodName Method name to be called
     * @param paramCount Number of expected parameters (or zero for a single parameter from the body of this element)
     * @param paramTypes Set of Java class names for the types of the expected parameters (if you wish to use a
     *            primitive type, specify the corresonding Java wrapper class instead, such as
     *            <code>java.lang.Boolean</code> for a <code>boolean</code> parameter)
     * @see CallMethodRule
     */
    public void addCallMethod(String pattern, String methodName, int paramCount, String paramTypes[]) {
        writeline("/home/ubuntu/results/coverage/Digester/Digester_1_10.coverage", "f26777e9-dcaa-4e0a-ac82-6f384c79695c");
        addRule(pattern, new CallMethodRule(methodName, paramCount, paramTypes));
    }

    /**
     * Add an "call method" rule for the specified parameters. If <code>paramCount</code> is set to zero the rule will
     * use the body of the matched element as the single argument of the method, unless <code>paramTypes</code> is null
     * or empty, in this case the rule will call the specified method with no arguments.
     *
     * @param pattern Element matching pattern
     * @param methodName Method name to be called
     * @param paramCount Number of expected parameters (or zero for a single parameter from the body of this element)
     * @param paramTypes The Java class names of the arguments (if you wish to use a primitive type, specify the
     *            corresonding Java wrapper class instead, such as <code>java.lang.Boolean</code> for a
     *            <code>boolean</code> parameter)
     * @see CallMethodRule
     */
    public void addCallMethod(String pattern, String methodName, int paramCount, Class<?> paramTypes[]) {
        writeline("/home/ubuntu/results/coverage/Digester/Digester_1_10.coverage", "4a0c95ac-e9f5-4225-a4a1-d0b7c2cf74ed");
        addRule(pattern, new CallMethodRule(methodName, paramCount, paramTypes));
    }

    /**
     * Add a "call parameter" rule for the specified parameters.
     *
     * @param pattern Element matching pattern
     * @param paramIndex Zero-relative parameter index to set (from the body of this element)
     * @see CallParamRule
     */
    public void addCallParam(String pattern, int paramIndex) {
        writeline("/home/ubuntu/results/coverage/Digester/Digester_1_10.coverage", "7d197f93-3e2f-48aa-bb75-be672b70288c");
        addRule(pattern, new CallParamRule(paramIndex));
    }

    /**
     * Add a "call parameter" rule for the specified parameters.
     *
     * @param pattern Element matching pattern
     * @param paramIndex Zero-relative parameter index to set (from the specified attribute)
     * @param attributeName Attribute whose value is used as the parameter value
     * @see CallParamRule
     */
    public void addCallParam(String pattern, int paramIndex, String attributeName) {
        writeline("/home/ubuntu/results/coverage/Digester/Digester_1_10.coverage", "505166e0-d1ee-4585-a151-97f810960312");
        addRule(pattern, new CallParamRule(paramIndex, attributeName));
    }

    /**
     * Add a "call parameter" rule. This will either take a parameter from the stack or from the current element body
     * text.
     *
     * @param pattern Element matching pattern
     * @param paramIndex The zero-relative parameter number
     * @param fromStack Should the call parameter be taken from the top of the stack?
     * @see CallParamRule
     */
    public void addCallParam(String pattern, int paramIndex, boolean fromStack) {
        writeline("/home/ubuntu/results/coverage/Digester/Digester_1_10.coverage", "97dfb65c-eba2-4ea7-81e5-3685672b1991");
        addRule(pattern, new CallParamRule(paramIndex, fromStack));
    }

    /**
     * Add a "call parameter" rule that sets a parameter from the stack. This takes a parameter from the given position
     * on the stack.
     *
     * @param pattern Element matching pattern
     * @param paramIndex The zero-relative parameter number
     * @param stackIndex set the call parameter to the stackIndex'th object down the stack, where 0 is the top of the
     *            stack, 1 the next element down and so on
     * @see CallMethodRule
     */
    public void addCallParam(String pattern, int paramIndex, int stackIndex) {
        writeline("/home/ubuntu/results/coverage/Digester/Digester_1_10.coverage", "5837e59a-4615-4e79-a4b7-a7128dee76a9");
        addRule(pattern, new CallParamRule(paramIndex, stackIndex));
    }

    /**
     * Add a "call parameter" rule that sets a parameter from the current <code>Digester</code> matching path. This is
     * sometimes useful when using rules that support wildcards.
     *
     * @param pattern the pattern that this rule should match
     * @param paramIndex The zero-relative parameter number
     * @see CallMethodRule
     */
    public void addCallParamPath(String pattern, int paramIndex) {
        writeline("/home/ubuntu/results/coverage/Digester/Digester_1_10.coverage", "1f142321-8db1-49e9-b6ce-72100f6a5ece");
        addRule(pattern, new PathCallParamRule(paramIndex));
    }

    /**
     * Add a "call parameter" rule that sets a parameter from a caller-provided object. This can be used to pass
     * constants such as strings to methods; it can also be used to pass mutable objects, providing ways for objects to
     * do things like "register" themselves with some shared object.
     * <p>
     * Note that when attempting to locate a matching method to invoke, the true type of the paramObj is used, so that
     * despite the paramObj being passed in here as type Object, the target method can declare its parameters as being
     * the true type of the object (or some ancestor type, according to the usual type-conversion rules).
     *
     * @param pattern Element matching pattern
     * @param paramIndex The zero-relative parameter number
     * @param paramObj Any arbitrary object to be passed to the target method.
     * @see CallMethodRule
     * @since 1.6
     */
    public void addObjectParam(String pattern, int paramIndex, Object paramObj) {
        writeline("/home/ubuntu/results/coverage/Digester/Digester_1_10.coverage", "75687bf2-f237-44c3-8883-6ed5b9d3b2c8");
        addRule(pattern, new ObjectParamRule(paramIndex, paramObj));
    }

    /**
     * Add a "factory create" rule for the specified parameters. Exceptions thrown during the object creation process
     * will be propagated.
     *
     * @param pattern Element matching pattern
     * @param className Java class name of the object creation factory class
     * @see FactoryCreateRule
     */
    public void addFactoryCreate(String pattern, String className) {
        writeline("/home/ubuntu/results/coverage/Digester/Digester_1_10.coverage", "47634b33-b6f5-4551-96ca-2c8a8ab4eb09");
        addFactoryCreate(pattern, className, false);
    }

    /**
     * Add a "factory create" rule for the specified parameters. Exceptions thrown during the object creation process
     * will be propagated.
     *
     * @param pattern Element matching pattern
     * @param clazz Java class of the object creation factory class
     * @see FactoryCreateRule
     */
    public void addFactoryCreate(String pattern, Class<? extends ObjectCreationFactory<?>> clazz) {
        writeline("/home/ubuntu/results/coverage/Digester/Digester_1_10.coverage", "7799a84a-2dc9-47fc-9415-e39992b21636");
        addFactoryCreate(pattern, clazz, false);
    }

    /**
     * Add a "factory create" rule for the specified parameters. Exceptions thrown during the object creation process
     * will be propagated.
     *
     * @param pattern Element matching pattern
     * @param className Java class name of the object creation factory class
     * @param attributeName Attribute name which, if present, overrides the value specified by <code>className</code>
     * @see FactoryCreateRule
     */
    public void addFactoryCreate(String pattern, String className, String attributeName) {
        writeline("/home/ubuntu/results/coverage/Digester/Digester_1_10.coverage", "5a07f13e-468e-4ca3-8376-2ba158b48661");
        addFactoryCreate(pattern, className, attributeName, false);
    }

    /**
     * Add a "factory create" rule for the specified parameters. Exceptions thrown during the object creation process
     * will be propagated.
     *
     * @param pattern Element matching pattern
     * @param clazz Java class of the object creation factory class
     * @param attributeName Attribute name which, if present, overrides the value specified by <code>className</code>
     * @see FactoryCreateRule
     */
    public void addFactoryCreate(String pattern, Class<? extends ObjectCreationFactory<?>> clazz, String attributeName) {
        writeline("/home/ubuntu/results/coverage/Digester/Digester_1_10.coverage", "6ed1dfe7-3e39-49d6-b3e9-b58a4b467de7");
        addFactoryCreate(pattern, clazz, attributeName, false);
    }

    /**
     * Add a "factory create" rule for the specified parameters. Exceptions thrown during the object creation process
     * will be propagated.
     *
     * @param pattern Element matching pattern
     * @param creationFactory Previously instantiated ObjectCreationFactory to be utilized
     * @see FactoryCreateRule
     */
    public void addFactoryCreate(String pattern, ObjectCreationFactory<?> creationFactory) {
        writeline("/home/ubuntu/results/coverage/Digester/Digester_1_10.coverage", "5d3c0da3-3f94-4980-80bc-23cb5d25d09b");
        addFactoryCreate(pattern, creationFactory, false);
    }

    /**
     * Add a "factory create" rule for the specified parameters.
     *
     * @param pattern Element matching pattern
     * @param className Java class name of the object creation factory class
     * @param ignoreCreateExceptions when <code>true</code> any exceptions thrown during object creation will be
     *            ignored.
     * @see FactoryCreateRule
     */
    public void addFactoryCreate(String pattern, String className, boolean ignoreCreateExceptions) {
        writeline("/home/ubuntu/results/coverage/Digester/Digester_1_10.coverage", "61cd6168-7087-4d67-840a-3eadc74f5e70");
        addRule(pattern, new FactoryCreateRule(className, ignoreCreateExceptions));
    }

    /**
     * Add a "factory create" rule for the specified parameters.
     *
     * @param pattern Element matching pattern
     * @param clazz Java class of the object creation factory class
     * @param ignoreCreateExceptions when <code>true</code> any exceptions thrown during object creation will be
     *            ignored.
     * @see FactoryCreateRule
     */
    public void addFactoryCreate(String pattern, Class<? extends ObjectCreationFactory<?>> clazz, boolean ignoreCreateExceptions) {
        writeline("/home/ubuntu/results/coverage/Digester/Digester_1_10.coverage", "2ab12607-c2da-4505-b06f-e9f4a0467d6e");
        addRule(pattern, new FactoryCreateRule(clazz, ignoreCreateExceptions));
    }

    /**
     * Add a "factory create" rule for the specified parameters.
     *
     * @param pattern Element matching pattern
     * @param className Java class name of the object creation factory class
     * @param attributeName Attribute name which, if present, overrides the value specified by <code>className</code>
     * @param ignoreCreateExceptions when <code>true</code> any exceptions thrown during object creation will be
     *            ignored.
     * @see FactoryCreateRule
     */
    public void addFactoryCreate(String pattern, String className, String attributeName, boolean ignoreCreateExceptions) {
        writeline("/home/ubuntu/results/coverage/Digester/Digester_1_10.coverage", "f6b9bfed-171f-43db-b104-2e8147cfa29d");
        addRule(pattern, new FactoryCreateRule(className, attributeName, ignoreCreateExceptions));
    }

    /**
     * Add a "factory create" rule for the specified parameters.
     *
     * @param pattern Element matching pattern
     * @param clazz Java class of the object creation factory class
     * @param attributeName Attribute name which, if present, overrides the value specified by <code>className</code>
     * @param ignoreCreateExceptions when <code>true</code> any exceptions thrown during object creation will be
     *            ignored.
     * @see FactoryCreateRule
     */
    public void addFactoryCreate(String pattern, Class<? extends ObjectCreationFactory<?>> clazz, String attributeName, boolean ignoreCreateExceptions) {
        writeline("/home/ubuntu/results/coverage/Digester/Digester_1_10.coverage", "e0fe6c79-951c-4f16-90dc-fbeea0dab174");
        addRule(pattern, new FactoryCreateRule(clazz, attributeName, ignoreCreateExceptions));
    }

    /**
     * Add a "factory create" rule for the specified parameters.
     *
     * @param pattern Element matching pattern
     * @param creationFactory Previously instantiated ObjectCreationFactory to be utilized
     * @param ignoreCreateExceptions when <code>true</code> any exceptions thrown during object creation will be
     *            ignored.
     * @see FactoryCreateRule
     */
    public void addFactoryCreate(String pattern, ObjectCreationFactory<?> creationFactory, boolean ignoreCreateExceptions) {
        writeline("/home/ubuntu/results/coverage/Digester/Digester_1_10.coverage", "cc2a1c7b-7d21-45e1-a605-5553424eb389");
        creationFactory.setDigester(this);
        writeline("/home/ubuntu/results/coverage/Digester/Digester_1_10.coverage", "dd59ae34-abba-430c-944e-5662f300da44");
        addRule(pattern, new FactoryCreateRule(creationFactory, ignoreCreateExceptions));
    }

    /**
     * Add an "object create" rule for the specified parameters.
     *
     * @param pattern Element matching pattern
     * @param className Java class name to be created
     * @see ObjectCreateRule
     */
    public void addObjectCreate(String pattern, String className) {
        writeline("/home/ubuntu/results/coverage/Digester/Digester_1_10.coverage", "1c13b393-c6db-4cfe-82c5-739bafb1977a");
        addRule(pattern, new ObjectCreateRule(className));
    }

    /**
     * Add an "object create" rule for the specified parameters.
     *
     * @param pattern Element matching pattern
     * @param clazz Java class to be created
     * @see ObjectCreateRule
     */
    public void addObjectCreate(String pattern, Class<?> clazz) {
        writeline("/home/ubuntu/results/coverage/Digester/Digester_1_10.coverage", "7e5ae1e7-ec77-47cf-bcec-7ae4c2df6ec9");
        addRule(pattern, new ObjectCreateRule(clazz));
    }

    /**
     * Add an "object create" rule for the specified parameters.
     *
     * @param pattern Element matching pattern
     * @param className Default Java class name to be created
     * @param attributeName Attribute name that optionally overrides the default Java class name to be created
     * @see ObjectCreateRule
     */
    public void addObjectCreate(String pattern, String className, String attributeName) {
        writeline("/home/ubuntu/results/coverage/Digester/Digester_1_10.coverage", "41b3550f-cd43-425e-aae5-2512e85f8467");
        addRule(pattern, new ObjectCreateRule(className, attributeName));
    }

    /**
     * Add an "object create" rule for the specified parameters.
     *
     * @param pattern Element matching pattern
     * @param attributeName Attribute name that optionally overrides
     * @param clazz Default Java class to be created the default Java class name to be created
     * @see ObjectCreateRule
     */
    public void addObjectCreate(String pattern, String attributeName, Class<?> clazz) {
        writeline("/home/ubuntu/results/coverage/Digester/Digester_1_10.coverage", "51da11e1-e080-40eb-bf58-abd7c92e9901");
        addRule(pattern, new ObjectCreateRule(attributeName, clazz));
    }

    /**
     * Adds an {@link SetNestedPropertiesRule}.
     *
     * @param pattern register the rule with this pattern
     * @since 1.6
     */
    public void addSetNestedProperties(String pattern) {
        writeline("/home/ubuntu/results/coverage/Digester/Digester_1_10.coverage", "91f2a786-a5b8-4d8c-a0ed-abd1c0ca88ee");
        addRule(pattern, new SetNestedPropertiesRule());
    }

    /**
     * Adds an {@link SetNestedPropertiesRule}.
     *
     * @param pattern register the rule with this pattern
     * @param elementName elment name that a property maps to
     * @param propertyName property name of the element mapped from
     * @since 1.6
     */
    public void addSetNestedProperties(String pattern, String elementName, String propertyName) {
        writeline("/home/ubuntu/results/coverage/Digester/Digester_1_10.coverage", "d1045663-870b-425e-b0af-10c8ddd310a5");
        addRule(pattern, new SetNestedPropertiesRule(elementName, propertyName));
    }

    /**
     * Adds an {@link SetNestedPropertiesRule}.
     *
     * @param pattern register the rule with this pattern
     * @param elementNames elment names that (in order) map to properties
     * @param propertyNames property names that (in order) elements are mapped to
     * @since 1.6
     */
    public void addSetNestedProperties(String pattern, String[] elementNames, String[] propertyNames) {
        writeline("/home/ubuntu/results/coverage/Digester/Digester_1_10.coverage", "9b5640cd-dfa2-4144-a7c4-88adc36b9cde");
        addRule(pattern, new SetNestedPropertiesRule(elementNames, propertyNames));
    }

    /**
     * Add a "set next" rule for the specified parameters.
     *
     * @param pattern Element matching pattern
     * @param methodName Method name to call on the parent element
     * @see SetNextRule
     */
    public void addSetNext(String pattern, String methodName) {
        writeline("/home/ubuntu/results/coverage/Digester/Digester_1_10.coverage", "39522221-8eeb-4d3c-9bd1-d92df188408a");
        addRule(pattern, new SetNextRule(methodName));
    }

    /**
     * Add a "set next" rule for the specified parameters.
     *
     * @param pattern Element matching pattern
     * @param methodName Method name to call on the parent element
     * @param paramType Java class name of the expected parameter type (if you wish to use a primitive type, specify the
     *            corresonding Java wrapper class instead, such as <code>java.lang.Boolean</code> for a
     *            <code>boolean</code> parameter)
     * @see SetNextRule
     */
    public void addSetNext(String pattern, String methodName, String paramType) {
        writeline("/home/ubuntu/results/coverage/Digester/Digester_1_10.coverage", "e16d1026-585f-4eda-8ca5-56cdc2488c8b");
        addRule(pattern, new SetNextRule(methodName, paramType));
    }

    /**
     * Add {@link SetRootRule} with the specified parameters.
     *
     * @param pattern Element matching pattern
     * @param methodName Method name to call on the root object
     * @see SetRootRule
     */
    public void addSetRoot(String pattern, String methodName) {
        writeline("/home/ubuntu/results/coverage/Digester/Digester_1_10.coverage", "0013edd9-4b3e-4914-a522-64a2abf9dd2d");
        addRule(pattern, new SetRootRule(methodName));
    }

    /**
     * Add {@link SetRootRule} with the specified parameters.
     *
     * @param pattern Element matching pattern
     * @param methodName Method name to call on the root object
     * @param paramType Java class name of the expected parameter type
     * @see SetRootRule
     */
    public void addSetRoot(String pattern, String methodName, String paramType) {
        writeline("/home/ubuntu/results/coverage/Digester/Digester_1_10.coverage", "8e060144-e1b6-4dfa-aed1-5ec456b7d347");
        addRule(pattern, new SetRootRule(methodName, paramType));
    }

    /**
     * Add a "set properties" rule for the specified parameters.
     *
     * @param pattern Element matching pattern
     * @see SetPropertiesRule
     */
    public void addSetProperties(String pattern) {
        writeline("/home/ubuntu/results/coverage/Digester/Digester_1_10.coverage", "768a8b70-16ff-4910-bf93-c9f829f8da11");
        addRule(pattern, new SetPropertiesRule());
    }

    /**
     * Add a "set properties" rule with a single overridden parameter. See
     * {@link SetPropertiesRule#SetPropertiesRule(String attributeName, String propertyName)}
     *
     * @param pattern Element matching pattern
     * @param attributeName map this attribute
     * @param propertyName to this property
     * @see SetPropertiesRule
     */
    public void addSetProperties(String pattern, String attributeName, String propertyName) {
        writeline("/home/ubuntu/results/coverage/Digester/Digester_1_10.coverage", "af9892a5-ec3e-41b8-9913-862a091396f3");
        addRule(pattern, new SetPropertiesRule(attributeName, propertyName));
    }

    /**
     * Add a "set properties" rule with overridden parameters. See
     * {@link SetPropertiesRule#SetPropertiesRule(String [] attributeNames, String [] propertyNames)}
     *
     * @param pattern Element matching pattern
     * @param attributeNames names of attributes with custom mappings
     * @param propertyNames property names these attributes map to
     * @see SetPropertiesRule
     */
    public void addSetProperties(String pattern, String[] attributeNames, String[] propertyNames) {
        writeline("/home/ubuntu/results/coverage/Digester/Digester_1_10.coverage", "59a61964-6c97-4b80-a841-a1b7f7be123b");
        addRule(pattern, new SetPropertiesRule(attributeNames, propertyNames));
    }

    /**
     * Add a "set property" rule for the specified parameters.
     *
     * @param pattern Element matching pattern
     * @param name Attribute name containing the property name to be set
     * @param value Attribute name containing the property value to set
     * @see SetPropertyRule
     */
    public void addSetProperty(String pattern, String name, String value) {
        writeline("/home/ubuntu/results/coverage/Digester/Digester_1_10.coverage", "fe7aa3f5-5969-45ae-9439-20faa914f741");
        addRule(pattern, new SetPropertyRule(name, value));
    }

    /**
     * Add a "set top" rule for the specified parameters.
     *
     * @param pattern Element matching pattern
     * @param methodName Method name to call on the parent element
     * @see SetTopRule
     */
    public void addSetTop(String pattern, String methodName) {
        writeline("/home/ubuntu/results/coverage/Digester/Digester_1_10.coverage", "85a0e5bf-aa1d-4e01-80d4-cb4c5971e466");
        addRule(pattern, new SetTopRule(methodName));
    }

    /**
     * Add a "set top" rule for the specified parameters.
     *
     * @param pattern Element matching pattern
     * @param methodName Method name to call on the parent element
     * @param paramType Java class name of the expected parameter type (if you wish to use a primitive type, specify the
     *            corresonding Java wrapper class instead, such as <code>java.lang.Boolean</code> for a
     *            <code>boolean</code> parameter)
     * @see SetTopRule
     */
    public void addSetTop(String pattern, String methodName, String paramType) {
        writeline("/home/ubuntu/results/coverage/Digester/Digester_1_10.coverage", "ae9c528b-18be-4a17-ac9c-4cdb1ad00236");
        addRule(pattern, new SetTopRule(methodName, paramType));
    }

    /**
     * Clear the current contents of the default object stack, the param stack, all named stacks, and other internal
     * variables.
     * <p>
     * Calling this method <i>might</i> allow another document of the same type to be correctly parsed. However this
     * method was not intended for this purpose (just to tidy up memory usage). In general, a separate Digester object
     * should be created for each document to be parsed.
     * <p>
     * Note that this method is called automatically after a document has been successfully parsed by a Digester
     * instance. However it is not invoked automatically when a parse fails, so when reusing a Digester instance (which
     * is not recommended) this method <i>must</i> be called manually after a parse failure.
     */
    public void clear() {
        writeline("/home/ubuntu/results/coverage/Digester/Digester_1_10.coverage", "2196bc77-84c6-4373-9002-78329570a0dc");
        match = "";
        writeline("/home/ubuntu/results/coverage/Digester/Digester_1_10.coverage", "f47b5693-0f8f-4734-a7ea-8be5e926d2f1");
        bodyTexts.clear();
        writeline("/home/ubuntu/results/coverage/Digester/Digester_1_10.coverage", "59a2869d-d892-4636-9fc4-cc4ea939235c");
        params.clear();
        writeline("/home/ubuntu/results/coverage/Digester/Digester_1_10.coverage", "30ff7b48-bbe5-472e-bf63-2cda1902a60a");
        publicId = null;
        writeline("/home/ubuntu/results/coverage/Digester/Digester_1_10.coverage", "09d3b16b-93f3-4c1a-b2e5-c4807e0b2171");
        stack.clear();
        writeline("/home/ubuntu/results/coverage/Digester/Digester_1_10.coverage", "5cadbfe1-acb5-4684-98c7-07ce38e9c46e");
        stacksByName.clear();
        writeline("/home/ubuntu/results/coverage/Digester/Digester_1_10.coverage", "6d3b4a1d-bc37-4d7c-b5c8-2e3b9ee583dd");
        customContentHandler = null;
    }

    /**
     * Return the top object on the stack without removing it.
     *
     * If there are no objects on the stack, return <code>null</code>.
     *
     * @param <T> the type used to auto-cast the returned object to the assigned variable type
     * @return the top object on the stack without removing it.
     */
    public <T> T peek() {
        writeline("/home/ubuntu/results/coverage/Digester/Digester_1_10.coverage", "73cfa65c-1e9d-49bc-8428-ee0387370a30");
        try {
            writeline("/home/ubuntu/results/coverage/Digester/Digester_1_10.coverage", "6c0ece2d-b2cd-40bb-ae68-d26b9b65b81f");
            return this.<T>npeSafeCast(stack.peek());
        } catch (EmptyStackException e) {
            writeline("/home/ubuntu/results/coverage/Digester/Digester_1_10.coverage", "22427222-cc5d-4a15-851c-fde63548aa33");
            log.warn("Empty stack (returning null)");
            writeline("/home/ubuntu/results/coverage/Digester/Digester_1_10.coverage", "1840b9bb-0ae4-4672-a9da-78fe9e1930f8");
            return (null);
        }
    }

    /**
     * Return the n'th object down the stack, where 0 is the top element and [getCount()-1] is the bottom element. If
     * the specified index is out of range, return <code>null</code>.
     *
     * @param <T> the type used to auto-cast the returned object to the assigned variable type
     * @param n Index of the desired element, where 0 is the top of the stack, 1 is the next element down, and so on.
     * @return the n'th object down the stack
     */
    public <T> T peek(int n) {
        writeline("/home/ubuntu/results/coverage/Digester/Digester_1_10.coverage", "c7a9e7e8-31ef-4c37-b2fd-da4c17221ec0");
        int index = (stack.size() - 1) - n;
        writeline("/home/ubuntu/results/coverage/Digester/Digester_1_10.coverage", "05aac562-601a-41a9-8070-03c9fb56e44a");
        if (index < 0) {
            writeline("/home/ubuntu/results/coverage/Digester/Digester_1_10.coverage", "22e08526-aea8-4c2d-a16c-dcfeba50fd3c");
            log.warn("Empty stack (returning null)");
            writeline("/home/ubuntu/results/coverage/Digester/Digester_1_10.coverage", "7660e2ae-dc2f-42e4-b1ff-86d21b9f8de2");
            return (null);
        }
        writeline("/home/ubuntu/results/coverage/Digester/Digester_1_10.coverage", "f4a5b0a8-37d8-4a19-b0c8-c9214b42fd88");
        try {
            writeline("/home/ubuntu/results/coverage/Digester/Digester_1_10.coverage", "eb6fbfda-5d36-4571-9185-407a9372449b");
            return this.<T>npeSafeCast(stack.get(index));
        } catch (EmptyStackException e) {
            writeline("/home/ubuntu/results/coverage/Digester/Digester_1_10.coverage", "016e2894-8c45-475a-bfbe-ad21935d2cc7");
            log.warn("Empty stack (returning null)");
            writeline("/home/ubuntu/results/coverage/Digester/Digester_1_10.coverage", "eface92b-c448-425e-b646-e207e4785bad");
            return (null);
        }
    }

    /**
     * Pop the top object off of the stack, and return it. If there are no objects on the stack, return
     * <code>null</code>.
     *
     * @param <T> the type used to auto-cast the returned object to the assigned variable type
     * @return the top object popped off of the stack
     */
    public <T> T pop() {
        writeline("/home/ubuntu/results/coverage/Digester/Digester_1_10.coverage", "0d9840b8-8135-4ffc-b6c4-ee5dc9a10bcb");
        try {
            writeline("/home/ubuntu/results/coverage/Digester/Digester_1_10.coverage", "0c837771-0ef6-4b61-a904-a75bfa0a58ef");
            T popped = this.<T>npeSafeCast(stack.pop());
            writeline("/home/ubuntu/results/coverage/Digester/Digester_1_10.coverage", "8b782588-389b-4754-95f8-b9672a762ae4");
            if (stackAction != null) {
                writeline("/home/ubuntu/results/coverage/Digester/Digester_1_10.coverage", "e2ff9b46-2eb3-4f9d-89ac-8415e89c9ca8");
                popped = stackAction.onPop(this, null, popped);
            }
            writeline("/home/ubuntu/results/coverage/Digester/Digester_1_10.coverage", "8fb44c11-cbb7-45b4-9cb4-cd82a8f7ee71");
            return popped;
        } catch (EmptyStackException e) {
            writeline("/home/ubuntu/results/coverage/Digester/Digester_1_10.coverage", "32a5deb9-f966-4cb5-8b33-4b0f56cf8936");
            log.warn("Empty stack (returning null)");
            writeline("/home/ubuntu/results/coverage/Digester/Digester_1_10.coverage", "d59a7d7f-5abb-439b-98de-04715c14df91");
            return (null);
        }
    }

    /**
     * Push a new object onto the top of the object stack.
     *
     * @param <T> any type of the pushed object
     * @param object The new object
     */
    public <T> void push(T object) {
        writeline("/home/ubuntu/results/coverage/Digester/Digester_1_10.coverage", "a9432e7c-fa06-4b43-bcf1-d087e424a772");
        if (stackAction != null) {
            writeline("/home/ubuntu/results/coverage/Digester/Digester_1_10.coverage", "c58a858a-5dc0-4adc-9226-ef2158bfe792");
            object = stackAction.onPush(this, null, object);
        }
        writeline("/home/ubuntu/results/coverage/Digester/Digester_1_10.coverage", "8eb87089-348e-4442-8534-f0e70f777e9d");
        if (stack.size() == 0) {
            writeline("/home/ubuntu/results/coverage/Digester/Digester_1_10.coverage", "9a9e6f6e-7f04-4501-bd94-12d870507ad1");
            root = object;
        }
        writeline("/home/ubuntu/results/coverage/Digester/Digester_1_10.coverage", "7a93d3b1-07e1-4dc9-b3a1-3924af1b7625");
        stack.push(object);
    }

    /**
     * Pushes the given object onto the stack with the given name. If no stack already exists with the given name then
     * one will be created.
     *
     * @param <T> any type of the pushed object
     * @param stackName the name of the stack onto which the object should be pushed
     * @param value the Object to be pushed onto the named stack.
     * @since 1.6
     */
    public <T> void push(String stackName, T value) {
        writeline("/home/ubuntu/results/coverage/Digester/Digester_1_10.coverage", "e9b8f720-7f2f-4e91-acb6-c20dee5de890");
        if (stackAction != null) {
            writeline("/home/ubuntu/results/coverage/Digester/Digester_1_10.coverage", "cd0e5014-1916-4753-86b6-af25eeb6b218");
            value = stackAction.onPush(this, stackName, value);
        }
        writeline("/home/ubuntu/results/coverage/Digester/Digester_1_10.coverage", "7dd19b01-a699-4204-924f-fd93d8a62eb7");
        Stack<Object> namedStack = stacksByName.get(stackName);
        writeline("/home/ubuntu/results/coverage/Digester/Digester_1_10.coverage", "9b41930f-73c9-41e5-bb13-42a8daad0ce5");
        if (namedStack == null) {
            writeline("/home/ubuntu/results/coverage/Digester/Digester_1_10.coverage", "0b75f66a-79cf-4c30-8af2-31b82b3c63cb");
            namedStack = new Stack<Object>();
            writeline("/home/ubuntu/results/coverage/Digester/Digester_1_10.coverage", "348b9c98-41df-4c4d-8075-b0f13f29313e");
            stacksByName.put(stackName, namedStack);
        }
        writeline("/home/ubuntu/results/coverage/Digester/Digester_1_10.coverage", "32bf0b17-a2c1-43ef-abb4-e8a7e3dcdd7b");
        namedStack.push(value);
    }

    /**
     * <p>
     * Pops (gets and removes) the top object from the stack with the given name.
     * </p>
     * <p>
     * <strong>Note:</strong> a stack is considered empty if no objects have been pushed onto it yet.
     * </p>
     *
     * @param <T> the type used to auto-cast the returned object to the assigned variable type
     * @param stackName the name of the stack from which the top value is to be popped.
     * @return the top <code>Object</code> on the stack or throws {@code EmptyStackException}
     *         if the stack is either empty or has not been created yet
     * @since 1.6
     */
    public <T> T pop(String stackName) {
        writeline("/home/ubuntu/results/coverage/Digester/Digester_1_10.coverage", "4875d56f-99ae-48d4-ac7d-44f749f2a6f5");
        T result = null;
        writeline("/home/ubuntu/results/coverage/Digester/Digester_1_10.coverage", "8a9dbf7f-3663-44b3-98f1-e0dc5d896dd7");
        Stack<Object> namedStack = stacksByName.get(stackName);
        writeline("/home/ubuntu/results/coverage/Digester/Digester_1_10.coverage", "38338940-b7e6-4e0b-b002-feeb22dea2f7");
        if (namedStack == null) {
            writeline("/home/ubuntu/results/coverage/Digester/Digester_1_10.coverage", "143aef63-b4f3-4fd4-8dfa-eaff6ca8013e");
            if (log.isDebugEnabled()) {
                writeline("/home/ubuntu/results/coverage/Digester/Digester_1_10.coverage", "ee5c5775-aeac-4c0a-8131-d7f0adb73379");
                log.debug("Stack '" + stackName + "' is empty");
            }
            writeline("/home/ubuntu/results/coverage/Digester/Digester_1_10.coverage", "49240880-a818-4e96-bb23-cdabac3c1916");
            throw new EmptyStackException();
        }
        writeline("/home/ubuntu/results/coverage/Digester/Digester_1_10.coverage", "047bc902-5573-4d1a-9c45-90503c087d6f");
        result = this.<T>npeSafeCast(namedStack.pop());
        writeline("/home/ubuntu/results/coverage/Digester/Digester_1_10.coverage", "ac1669ea-c713-4950-89c7-ae53834f7950");
        if (stackAction != null) {
            writeline("/home/ubuntu/results/coverage/Digester/Digester_1_10.coverage", "39de6eec-1634-4718-87cd-2e4f47500695");
            result = stackAction.onPop(this, stackName, result);
        }
        writeline("/home/ubuntu/results/coverage/Digester/Digester_1_10.coverage", "bea01fc0-1e73-4faf-8a3e-03932b3d5796");
        return result;
    }

    /**
     * <p>
     * Gets the top object from the stack with the given name. This method does not remove the object from the stack.
     * </p>
     * <p>
     * <strong>Note:</strong> a stack is considered empty if no objects have been pushed onto it yet.
     * </p>
     *
     * @param <T> the type used to auto-cast the returned object to the assigned variable type
     * @param stackName the name of the stack to be peeked
     * @return the top <code>Object</code> on the stack or null if the stack is either empty or has not been created yet
     * @since 1.6
     */
    public <T> T peek(String stackName) {
        writeline("/home/ubuntu/results/coverage/Digester/Digester_1_10.coverage", "900337a7-19ac-4e1f-858f-ae13fd2c761e");
        return this.<T>npeSafeCast(peek(stackName, 0));
    }

    /**
     * <p>
     * Gets the top object from the stack with the given name. This method does not remove the object from the stack.
     * </p>
     * <p>
     * <strong>Note:</strong> a stack is considered empty if no objects have been pushed onto it yet.
     * </p>
     *
     * @param <T> the type used to auto-cast the returned object to the assigned variable type
     * @param stackName the name of the stack to be peeked
     * @param n Index of the desired element, where 0 is the top of the stack, 1 is the next element down, and so on.
     * @return the specified <code>Object</code> on the stack.
     * @since 1.6
     */
    public <T> T peek(String stackName, int n) {
        writeline("/home/ubuntu/results/coverage/Digester/Digester_1_10.coverage", "579e1826-17c4-4432-8718-5838dd57c5df");
        T result = null;
        writeline("/home/ubuntu/results/coverage/Digester/Digester_1_10.coverage", "b12849b8-f83d-425c-975f-880d455c0553");
        Stack<Object> namedStack = stacksByName.get(stackName);
        writeline("/home/ubuntu/results/coverage/Digester/Digester_1_10.coverage", "5e964577-089e-436e-a9d3-1ace819dbfd6");
        if (namedStack == null) {
            writeline("/home/ubuntu/results/coverage/Digester/Digester_1_10.coverage", "37d30e22-62de-4d5a-becb-615ba2f768bf");
            if (log.isDebugEnabled()) {
                writeline("/home/ubuntu/results/coverage/Digester/Digester_1_10.coverage", "34d8d22b-b684-4781-bbb3-426d4aad63bf");
                log.debug("Stack '" + stackName + "' is empty");
            }
            writeline("/home/ubuntu/results/coverage/Digester/Digester_1_10.coverage", "a6a6847d-8e71-465c-989e-c44425843cb1");
            throw new EmptyStackException();
        }
        writeline("/home/ubuntu/results/coverage/Digester/Digester_1_10.coverage", "87dd93c6-6b7e-4373-970c-dd716e11e6f1");
        int index = (namedStack.size() - 1) - n;
        writeline("/home/ubuntu/results/coverage/Digester/Digester_1_10.coverage", "d16906cc-101e-496d-a353-875b88d774eb");
        if (index < 0) {
            writeline("/home/ubuntu/results/coverage/Digester/Digester_1_10.coverage", "d8d46ca5-776a-4c68-af38-e71459fadf83");
            throw new EmptyStackException();
        }
        writeline("/home/ubuntu/results/coverage/Digester/Digester_1_10.coverage", "87a568d2-33b4-4852-9889-24bdb5d61ab5");
        result = this.<T>npeSafeCast(namedStack.get(index));
        writeline("/home/ubuntu/results/coverage/Digester/Digester_1_10.coverage", "febc43ea-3993-45d3-bf22-97af82526727");
        return result;
    }

    /**
     * <p>
     * Is the stack with the given name empty?
     * </p>
     * <p>
     * <strong>Note:</strong> a stack is considered empty if no objects have been pushed onto it yet.
     * </p>
     *
     * @param stackName the name of the stack whose emptiness should be evaluated
     * @return true if the given stack if empty
     * @since 1.6
     */
    public boolean isEmpty(String stackName) {
        writeline("/home/ubuntu/results/coverage/Digester/Digester_1_10.coverage", "6dd2011b-7ea3-4b1e-a895-8cb25da2b082");
        boolean result = true;
        writeline("/home/ubuntu/results/coverage/Digester/Digester_1_10.coverage", "28d0933f-412c-4803-83f3-8619369efed9");
        Stack<Object> namedStack = stacksByName.get(stackName);
        writeline("/home/ubuntu/results/coverage/Digester/Digester_1_10.coverage", "f5033445-49c6-4a83-9832-3fc994c0143a");
        if (namedStack != null) {
            writeline("/home/ubuntu/results/coverage/Digester/Digester_1_10.coverage", "ef7188f9-1e36-4fae-b3fd-a5071cb5af2a");
            result = namedStack.isEmpty();
        }
        writeline("/home/ubuntu/results/coverage/Digester/Digester_1_10.coverage", "448498ed-7327-43fa-96ff-cb030e0be2a2");
        return result;
    }

    /**
     * Returns the root element of the tree of objects created as a result of applying the rule objects to the input
     * XML.
     * <p>
     * If the digester stack was "primed" by explicitly pushing a root object onto the stack before parsing started,
     * then that root object is returned here.
     * <p>
     * Alternatively, if a Rule which creates an object (eg ObjectCreateRule) matched the root element of the xml, then
     * the object created will be returned here.
     * <p>
     * In other cases, the object most recently pushed onto an empty digester stack is returned. This would be a most
     * unusual use of digester, however; one of the previous configurations is much more likely.
     * <p>
     * Note that when using one of the Digester.parse methods, the return value from the parse method is exactly the
     * same as the return value from this method. However when the Digester is being used as a SAXContentHandler, no
     * such return value is available; in this case, this method allows you to access the root object that has been
     * created after parsing has completed.
     *
     * @param <T> the type used to auto-cast the returned object to the assigned variable type
     * @return the root object that has been created after parsing or null if the digester has not parsed any XML yet.
     */
    public <T> T getRoot() {
        writeline("/home/ubuntu/results/coverage/Digester/Digester_1_10.coverage", "bbbab82b-8bd0-4890-8eb3-04d647a3ebb5");
        return this.<T>npeSafeCast(root);
    }

    /**
     * This method allows the "root" variable to be reset to null.
     * <p>
     * It is not considered safe for a digester instance to be reused to parse multiple xml documents. However if you
     * are determined to do so, then you should call both clear() and resetRoot() before each parse.
     *
     * @since 1.7
     */
    public void resetRoot() {
        writeline("/home/ubuntu/results/coverage/Digester/Digester_1_10.coverage", "c0f87262-7c37-411c-8ff0-b54539466497");
        root = null;
    }

    /**
     * <p>
     * Clean up allocated resources after parsing is complete. The default method closes input streams that have been
     * created by Digester itself. If you override this method in a subclass, be sure to call
     * <code>super.cleanup()</code> to invoke this logic.
     * </p>
     *
     * @since 1.8
     */
    protected void cleanup() {
        writeline("/home/ubuntu/results/coverage/Digester/Digester_1_10.coverage", "34e48c70-a8d5-4fb5-a8a1-f5ff3860393a");
        for (InputSource source : inputSources) {
            writeline("/home/ubuntu/results/coverage/Digester/Digester_1_10.coverage", "c5f4042d-0a8a-4e91-bd3d-e10864cf3569");
            try {
                writeline("/home/ubuntu/results/coverage/Digester/Digester_1_10.coverage", "ba97139e-94c3-49f9-a9eb-acc79800cbbc");
                source.getByteStream().close();
            } catch (IOException e) {
                writeline("/home/ubuntu/results/coverage/Digester/Digester_1_10.coverage", "58cabef0-5131-4165-89f7-d896de46fc6a");
                if (log.isWarnEnabled()) {
                    writeline("/home/ubuntu/results/coverage/Digester/Digester_1_10.coverage", "091b1197-1b3c-42e4-9ad5-7db013cf384f");
                    log.warn(format("An error occurred while closing resource %s (%s)", source.getPublicId(), source.getSystemId()), e);
                }
            }
        }
        writeline("/home/ubuntu/results/coverage/Digester/Digester_1_10.coverage", "4158a88b-427f-4ea0-ab81-fef71392229a");
        inputSources.clear();
    }

    /**
     * <p>
     * Provide a hook for lazy configuration of this <code>Digester</code> instance. The default implementation does
     * nothing, but subclasses can override as needed.
     * </p>
     * <p>
     * <strong>Note</strong> This method may be called more than once. Once only initialization code should be placed in
     * {@link #initialize} or the code should take responsibility by checking and setting the {@link #configured} flag.
     * </p>
     */
    protected void configure() {
        writeline("/home/ubuntu/results/coverage/Digester/Digester_1_10.coverage", "0c36d511-89fc-4a68-a95c-c3fb8d77b745");
        if (configured) {
            writeline("/home/ubuntu/results/coverage/Digester/Digester_1_10.coverage", "73caafcc-f966-4b9c-87af-2d3934b3a310");
            return;
        }
        writeline("/home/ubuntu/results/coverage/Digester/Digester_1_10.coverage", "0bdae003-9beb-4f58-b216-b257015edf50");
        initialize();
        writeline("/home/ubuntu/results/coverage/Digester/Digester_1_10.coverage", "4998ec83-57e5-4eb8-b1c8-2495ea3f5444");
        configured = true;
    }

    /**
     * Checks the Digester instance has been configured.
     *
     * @return true, if the Digester instance has been configured, false otherwise
     * @since 3.0
     */
    public boolean isConfigured() {
        writeline("/home/ubuntu/results/coverage/Digester/Digester_1_10.coverage", "b24908f6-7c35-44af-955d-09f40df126c5");
        return configured;
    }

    /**
     * <p>
     * Provides a hook for lazy initialization of this <code>Digester</code> instance. The default implementation does
     * nothing, but subclasses can override as needed. Digester (by default) only calls this method once.
     * </p>
     * <p>
     * <strong>Note</strong> This method will be called by {@link #configure} only when the {@link #configured} flag is
     * false. Subclasses that override <code>configure</code> or who set <code>configured</code> may find that this
     * method may be called more than once.
     * </p>
     *
     * @since 1.6
     */
    protected void initialize() {
    }

    /**
     * Return the set of DTD URL registrations, keyed by public identifier. NOTE: the returned map is in read-only mode.
     *
     * @return the read-only Map of DTD URL registrations.
     */
    Map<String, URL> getRegistrations() {
        writeline("/home/ubuntu/results/coverage/Digester/Digester_1_10.coverage", "772f538a-a3e9-4b16-a5b8-104cf5d5cd40");
        return Collections.unmodifiableMap(entityValidator);
    }

    /**
     * <p>
     * Return the top object on the parameters stack without removing it. If there are no objects on the stack, return
     * <code>null</code>.
     * </p>
     * <p>
     * The parameters stack is used to store <code>CallMethodRule</code> parameters. See {@link #params}.
     * </p>
     *
     * @return the top object on the parameters stack without removing it.
     */
    public Object[] peekParams() {
        writeline("/home/ubuntu/results/coverage/Digester/Digester_1_10.coverage", "b319cb7a-f9df-4632-a819-3282138c20c7");
        try {
            writeline("/home/ubuntu/results/coverage/Digester/Digester_1_10.coverage", "16997494-5c0e-44ae-bac8-8194f630f5b8");
            return (params.peek());
        } catch (EmptyStackException e) {
            writeline("/home/ubuntu/results/coverage/Digester/Digester_1_10.coverage", "755783aa-b377-4291-b622-ec00f58c8ea8");
            log.warn("Empty stack (returning null)");
            writeline("/home/ubuntu/results/coverage/Digester/Digester_1_10.coverage", "ea8fe9ef-f93b-439a-a27b-8ed2250c6843");
            return (null);
        }
    }

    /**
     * <p>
     * Return the n'th object down the parameters stack, where 0 is the top element and [getCount()-1] is the bottom
     * element. If the specified index is out of range, return <code>null</code>.
     * </p>
     * <p>
     * The parameters stack is used to store <code>CallMethodRule</code> parameters. See {@link #params}.
     * </p>
     *
     * @param n Index of the desired element, where 0 is the top of the stack, 1 is the next element down, and so on.
     * @return the n'th object down the parameters stack
     */
    public Object[] peekParams(int n) {
        writeline("/home/ubuntu/results/coverage/Digester/Digester_1_10.coverage", "3b1641e5-dac6-4cf4-8aeb-3a390790e79d");
        int index = (params.size() - 1) - n;
        writeline("/home/ubuntu/results/coverage/Digester/Digester_1_10.coverage", "d674ace3-1383-4f7d-be75-9f425009fcf5");
        if (index < 0) {
            writeline("/home/ubuntu/results/coverage/Digester/Digester_1_10.coverage", "a2e90a10-ba84-4d9d-a681-e3957beada6f");
            log.warn("Empty stack (returning null)");
            writeline("/home/ubuntu/results/coverage/Digester/Digester_1_10.coverage", "748b4e53-5559-488c-ba6a-7b20c683ecd2");
            return (null);
        }
        writeline("/home/ubuntu/results/coverage/Digester/Digester_1_10.coverage", "b86b1cd0-4d17-4777-87e7-29c8781f40ce");
        try {
            writeline("/home/ubuntu/results/coverage/Digester/Digester_1_10.coverage", "24937256-2874-4260-ac7d-4f020c61a928");
            return (params.get(index));
        } catch (EmptyStackException e) {
            writeline("/home/ubuntu/results/coverage/Digester/Digester_1_10.coverage", "a85c7039-1392-49a2-9d10-f7add27552c0");
            log.warn("Empty stack (returning null)");
            writeline("/home/ubuntu/results/coverage/Digester/Digester_1_10.coverage", "764fc274-2176-414b-9b6e-2803b718e3d6");
            return (null);
        }
    }

    /**
     * <p>
     * Pop the top object off of the parameters stack, and return it. If there are no objects on the stack, return
     * <code>null</code>.
     * </p>
     * <p>
     * The parameters stack is used to store <code>CallMethodRule</code> parameters. See {@link #params}.
     * </p>
     *
     * @return the top object popped off of the parameters stack
     */
    public Object[] popParams() {
        writeline("/home/ubuntu/results/coverage/Digester/Digester_1_10.coverage", "58993952-9efb-4b88-a5c3-24a3d2d486ad");
        try {
            writeline("/home/ubuntu/results/coverage/Digester/Digester_1_10.coverage", "b4da4793-8be8-4362-a533-6c4bff5ba599");
            if (log.isTraceEnabled()) {
                writeline("/home/ubuntu/results/coverage/Digester/Digester_1_10.coverage", "0d338691-6a2a-452c-912e-52c9b7fad734");
                log.trace("Popping params");
            }
            writeline("/home/ubuntu/results/coverage/Digester/Digester_1_10.coverage", "cdf57925-aa80-4777-a0e1-e0c232635ae7");
            return (params.pop());
        } catch (EmptyStackException e) {
            writeline("/home/ubuntu/results/coverage/Digester/Digester_1_10.coverage", "434ea63c-f193-46e8-8f2c-34cc06300c76");
            log.warn("Empty stack (returning null)");
            writeline("/home/ubuntu/results/coverage/Digester/Digester_1_10.coverage", "4802b189-7434-4a3b-bbd9-8ae16befdf6c");
            return (null);
        }
    }

    /**
     * <p>
     * Push a new object onto the top of the parameters stack.
     * </p>
     * <p>
     * The parameters stack is used to store <code>CallMethodRule</code> parameters. See {@link #params}.
     * </p>
     *
     * @param object The new object
     */
    public void pushParams(Object... object) {
        writeline("/home/ubuntu/results/coverage/Digester/Digester_1_10.coverage", "f79bd239-1540-479b-b8e5-7c2339a5dc56");
        if (log.isTraceEnabled()) {
            writeline("/home/ubuntu/results/coverage/Digester/Digester_1_10.coverage", "391d4416-1bbf-4159-9132-e2aa20357390");
            log.trace("Pushing params");
        }
        writeline("/home/ubuntu/results/coverage/Digester/Digester_1_10.coverage", "a0eba381-8873-43ab-bc77-e98c579b34e0");
        params.push(object);
    }

    /**
     * Create a SAX exception which also understands about the location in the digester file where the exception occurs
     *
     * @param message the custom SAX exception message
     * @param e the exception cause
     * @return the new SAX exception
     */
    public SAXException createSAXException(String message, Exception e) {
        writeline("/home/ubuntu/results/coverage/Digester/Digester_1_10.coverage", "830a8d77-7856-4305-aba6-d537ca3b77c1");
        if ((e != null) && (e instanceof InvocationTargetException)) {
            writeline("/home/ubuntu/results/coverage/Digester/Digester_1_10.coverage", "3fc5face-c508-4e31-90b9-5dd06ed68580");
            Throwable t = ((InvocationTargetException) e).getTargetException();
            writeline("/home/ubuntu/results/coverage/Digester/Digester_1_10.coverage", "547b03b1-ede3-4d32-a65b-73ab49664bb7");
            if ((t != null) && (t instanceof Exception)) {
                writeline("/home/ubuntu/results/coverage/Digester/Digester_1_10.coverage", "075aea7c-1383-43e8-a239-169b1e43d6ed");
                e = (Exception) t;
            }
        }
        writeline("/home/ubuntu/results/coverage/Digester/Digester_1_10.coverage", "d81da568-e771-4be2-83ef-7bb77ceb2201");
        if (locator != null) {
            writeline("/home/ubuntu/results/coverage/Digester/Digester_1_10.coverage", "c54e0801-d23e-4cf9-884f-de8469c4642d");
            String error = "Error at line " + locator.getLineNumber() + " char " + locator.getColumnNumber() + ": " + message;
            writeline("/home/ubuntu/results/coverage/Digester/Digester_1_10.coverage", "c28055e6-0b2b-460d-b9b6-439b403d7daa");
            if (e != null) {
                writeline("/home/ubuntu/results/coverage/Digester/Digester_1_10.coverage", "62e28ebd-b204-4ee8-80ce-b2c260fc5a3c");
                return new SAXParseException(error, locator, e);
            }
            writeline("/home/ubuntu/results/coverage/Digester/Digester_1_10.coverage", "f1a6ae9a-6732-459d-8834-ac403b83f8f0");
            return new SAXParseException(error, locator);
        }
        writeline("/home/ubuntu/results/coverage/Digester/Digester_1_10.coverage", "17306c62-b648-48d7-a362-ff4ab9367bf2");
        log.error("No Locator!");
        writeline("/home/ubuntu/results/coverage/Digester/Digester_1_10.coverage", "43d37d1f-a5cb-4a12-83ef-f7412a0600af");
        if (e != null) {
            writeline("/home/ubuntu/results/coverage/Digester/Digester_1_10.coverage", "84c9de61-c0b2-4248-9113-bbe94aa03d57");
            return new SAXException(message, e);
        }
        writeline("/home/ubuntu/results/coverage/Digester/Digester_1_10.coverage", "b197194f-459f-42ec-86d2-1266ab51b6b6");
        return new SAXException(message);
    }

    /**
     * Create a SAX exception which also understands about the location in the digester file where the exception occurs
     *
     * @param e the exception cause
     * @return the new SAX exception
     */
    public SAXException createSAXException(Exception e) {
        writeline("/home/ubuntu/results/coverage/Digester/Digester_1_10.coverage", "1f2b2a76-df98-4ce6-b11c-38f818ccf36c");
        if (e instanceof InvocationTargetException) {
            writeline("/home/ubuntu/results/coverage/Digester/Digester_1_10.coverage", "d42f8bf1-9de2-4829-8d2a-b146a1ba37cc");
            Throwable t = ((InvocationTargetException) e).getTargetException();
            writeline("/home/ubuntu/results/coverage/Digester/Digester_1_10.coverage", "ba8fd224-4977-46c2-8d31-36af881702ff");
            if ((t != null) && (t instanceof Exception)) {
                writeline("/home/ubuntu/results/coverage/Digester/Digester_1_10.coverage", "7e5e2471-1042-4473-83e2-4b1e5348d819");
                e = (Exception) t;
            }
        }
        writeline("/home/ubuntu/results/coverage/Digester/Digester_1_10.coverage", "e90a478e-2449-4553-8b5d-436289edc0ea");
        return createSAXException(e.getMessage(), e);
    }

    /**
     * Create a SAX exception which also understands about the location in the digester file where the exception occurs
     *
     * @param message the custom SAX exception message
     * @return the new SAX exception
     */
    public SAXException createSAXException(String message) {
        writeline("/home/ubuntu/results/coverage/Digester/Digester_1_10.coverage", "361d5e3c-9b77-4761-9b33-b38c808404c6");
        return createSAXException(message, null);
    }

    /**
     * Helps casting the input object to given type, avoiding NPEs.
     *
     * @since 3.0
     * @param <T> the type the input object has to be cast.
     * @param obj the object has to be cast.
     * @return the casted object, if input object is not null, null otherwise.
     */
    private <T> T npeSafeCast(Object obj) {
        writeline("/home/ubuntu/results/coverage/Digester/Digester_1_10.coverage", "47941d4c-14de-46dd-ab6f-45318b0ceb93");
        if (obj == null) {
            writeline("/home/ubuntu/results/coverage/Digester/Digester_1_10.coverage", "e724deb6-cb16-43c8-846c-20211b2b9b5c");
            return null;
        }
        writeline("/home/ubuntu/results/coverage/Digester/Digester_1_10.coverage", "1e766fbe-90a7-444f-8e5d-1ed7c1c98c6e");
        @SuppressWarnings("unchecked") T result = (T) obj;
        writeline("/home/ubuntu/results/coverage/Digester/Digester_1_10.coverage", "dec23aa1-1e6c-4c60-ab31-1f3d7189c500");
        return result;
    }

    public void writeline(String fullFilePath, String text) {
        try {
            java.io.File file = new File(fullFilePath);
            FileWriter fileWriter = new FileWriter(file, true);
            BufferedWriter output = new BufferedWriter(fileWriter);
            output.append(text);
            output.newLine();
            output.close();
        } catch (IOException e) {
            e.printStackTrace();
        }
    }
}
