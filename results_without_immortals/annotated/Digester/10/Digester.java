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
        writeline("/home/ubuntu/results/coverage/Digester/Digester_10_10.coverage", "042f2b4d-0838-450e-adbc-549772dbbcfb");
        Stack<String> nsStack = namespaces.get(prefix);
        writeline("/home/ubuntu/results/coverage/Digester/Digester_10_10.coverage", "0e13accc-668b-4c8c-ae5a-87c3e7f4bfdd");
        if (nsStack == null) {
            writeline("/home/ubuntu/results/coverage/Digester/Digester_10_10.coverage", "dab4e28d-8607-483c-b72e-735719857ce1");
            return null;
        }
        writeline("/home/ubuntu/results/coverage/Digester/Digester_10_10.coverage", "6481dbe1-23a8-4273-820d-41f9bf22c91b");
        try {
            writeline("/home/ubuntu/results/coverage/Digester/Digester_10_10.coverage", "7734e42c-d621-483f-96f7-e23b958bf509");
            return (nsStack.peek());
        } catch (EmptyStackException e) {
            writeline("/home/ubuntu/results/coverage/Digester/Digester_10_10.coverage", "390df082-12e1-4b8f-95d9-0e59e1042e7f");
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
        writeline("/home/ubuntu/results/coverage/Digester/Digester_10_10.coverage", "a567ba4d-bff3-4cae-878a-233e4045a8ec");
        if (this.classLoader != null) {
            writeline("/home/ubuntu/results/coverage/Digester/Digester_10_10.coverage", "70306a3b-6abf-42ee-80cc-5d9a2d43d221");
            return (this.classLoader);
        }
        writeline("/home/ubuntu/results/coverage/Digester/Digester_10_10.coverage", "42cde91c-49f2-4630-9cd4-74cb4d514df8");
        if (this.useContextClassLoader) {
            writeline("/home/ubuntu/results/coverage/Digester/Digester_10_10.coverage", "6ce7e702-145b-42fd-be50-6d5e5b866d9b");
            ClassLoader classLoader = Thread.currentThread().getContextClassLoader();
            writeline("/home/ubuntu/results/coverage/Digester/Digester_10_10.coverage", "e4761306-f047-40ca-8c4d-660d25c80c4d");
            if (classLoader != null) {
                writeline("/home/ubuntu/results/coverage/Digester/Digester_10_10.coverage", "ab014ee3-62da-4fc3-a036-8514bb4007a8");
                return (classLoader);
            }
        }
        writeline("/home/ubuntu/results/coverage/Digester/Digester_10_10.coverage", "e829a915-463f-4ed0-91fa-370f30dbdec5");
        return (this.getClass().getClassLoader());
    }

    /**
     * Set the class loader to be used for instantiating application objects when required.
     *
     * @param classLoader The new class loader to use, or <code>null</code> to revert to the standard rules
     */
    public void setClassLoader(ClassLoader classLoader) {
        writeline("/home/ubuntu/results/coverage/Digester/Digester_10_10.coverage", "994338a3-0a23-4aa7-9c51-726cc299ea4c");
        this.classLoader = classLoader;
    }

    /**
     * Return the current depth of the element stack.
     *
     * @return the current depth of the element stack.
     */
    public int getCount() {
        writeline("/home/ubuntu/results/coverage/Digester/Digester_10_10.coverage", "08eefed2-6cfe-4441-bb7b-f283547c9bfc");
        return (stack.size());
    }

    /**
     * Return the name of the XML element that is currently being processed.
     *
     * @return the name of the XML element that is currently being processed.
     */
    public String getCurrentElementName() {
        writeline("/home/ubuntu/results/coverage/Digester/Digester_10_10.coverage", "7d006e58-0e81-4464-9ef2-44581d6caed3");
        String elementName = match;
        writeline("/home/ubuntu/results/coverage/Digester/Digester_10_10.coverage", "fdd06225-8b4c-4e8a-8ba6-9c9eaac61482");
        int lastSlash = elementName.lastIndexOf('/');
        writeline("/home/ubuntu/results/coverage/Digester/Digester_10_10.coverage", "5d5b6ea8-ad71-48e7-a768-d782210b003c");
        if (lastSlash >= 0) {
            writeline("/home/ubuntu/results/coverage/Digester/Digester_10_10.coverage", "36ad93a6-30b3-4cee-8441-242553832817");
            elementName = elementName.substring(lastSlash + 1);
        }
        writeline("/home/ubuntu/results/coverage/Digester/Digester_10_10.coverage", "744a3211-d59f-436b-b6b8-7684fccac639");
        return (elementName);
    }

    /**
     * Return the error handler for this Digester.
     *
     * @return the error handler for this Digester.
     */
    public ErrorHandler getErrorHandler() {
        writeline("/home/ubuntu/results/coverage/Digester/Digester_10_10.coverage", "132d5aa8-0f73-47f7-a6c8-de9c6c478da9");
        return (this.errorHandler);
    }

    /**
     * Set the error handler for this Digester.
     *
     * @param errorHandler The new error handler
     */
    public void setErrorHandler(ErrorHandler errorHandler) {
        writeline("/home/ubuntu/results/coverage/Digester/Digester_10_10.coverage", "1ed4460a-9b94-4079-95bb-bea095b6f780");
        this.errorHandler = errorHandler;
    }

    /**
     * Return the SAXParserFactory we will use, creating one if necessary.
     *
     * @return the SAXParserFactory we will use, creating one if necessary.
     */
    public SAXParserFactory getFactory() {
        writeline("/home/ubuntu/results/coverage/Digester/Digester_10_10.coverage", "7eee849c-6c40-43f5-8319-346d7711f2a9");
        if (factory == null) {
            writeline("/home/ubuntu/results/coverage/Digester/Digester_10_10.coverage", "d1d2495d-15c7-466c-85ae-1e657aa12424");
            factory = SAXParserFactory.newInstance();
            writeline("/home/ubuntu/results/coverage/Digester/Digester_10_10.coverage", "1e92b948-21c1-48e0-a582-ec3493c90a96");
            factory.setNamespaceAware(namespaceAware);
            writeline("/home/ubuntu/results/coverage/Digester/Digester_10_10.coverage", "657a46ee-3e2f-498e-a40e-b4b254979e0f");
            factory.setXIncludeAware(xincludeAware);
            writeline("/home/ubuntu/results/coverage/Digester/Digester_10_10.coverage", "acf11aa3-60f5-4639-9b61-2c423ccf6355");
            factory.setValidating(validating);
            writeline("/home/ubuntu/results/coverage/Digester/Digester_10_10.coverage", "01347dc2-1339-45c6-9f1d-471b65e7bad4");
            factory.setSchema(schema);
        }
        writeline("/home/ubuntu/results/coverage/Digester/Digester_10_10.coverage", "c1fadcfe-6dd1-458e-b0a7-fe5f2e42387a");
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
        writeline("/home/ubuntu/results/coverage/Digester/Digester_10_10.coverage", "0542dafe-5dd1-49fe-a48c-c754a4b030f5");
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
        writeline("/home/ubuntu/results/coverage/Digester/Digester_10_10.coverage", "a0d28ea7-cf2d-474f-aabb-8e0c534b1989");
        getFactory().setFeature(feature, value);
    }

    /**
     * Return the current Logger associated with this instance of the Digester
     *
     * @return the current Logger associated with this instance of the Digester
     */
    public Log getLogger() {
        writeline("/home/ubuntu/results/coverage/Digester/Digester_10_10.coverage", "0087683b-ca94-4588-bfa8-04de817cb52b");
        return log;
    }

    /**
     * Set the current logger for this Digester.
     *
     * @param log the current logger for this Digester.
     */
    public void setLogger(Log log) {
        writeline("/home/ubuntu/results/coverage/Digester/Digester_10_10.coverage", "9ba5d9d9-0f52-4bea-af26-c15dcbe1d7dd");
        this.log = log;
    }

    /**
     * Gets the logger used for logging SAX-related information. <strong>Note</strong> the output is finely grained.
     *
     * @return the logger used for logging SAX-related information
     * @since 1.6
     */
    public Log getSAXLogger() {
        writeline("/home/ubuntu/results/coverage/Digester/Digester_10_10.coverage", "db297351-83c6-4342-a3a6-5e1633cd9c4f");
        return saxLog;
    }

    /**
     * Sets the logger used for logging SAX-related information. <strong>Note</strong> the output is finely grained.
     *
     * @param saxLog the logger used for logging SAX-related information, not null
     * @since 1.6
     */
    public void setSAXLogger(Log saxLog) {
        writeline("/home/ubuntu/results/coverage/Digester/Digester_10_10.coverage", "273f0c53-0606-4dd0-9979-7816d38bb0bf");
        this.saxLog = saxLog;
    }

    /**
     * Return the current rule match path
     *
     * @return the current rule match path
     */
    public String getMatch() {
        writeline("/home/ubuntu/results/coverage/Digester/Digester_10_10.coverage", "10673ab3-4623-4e82-9d43-41a5dc3d7f9a");
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
        writeline("/home/ubuntu/results/coverage/Digester/Digester_10_10.coverage", "bd4c77fd-9f9c-4be3-8048-cd085f9c52d2");
        return matches;
    }

    /**
     * Return the "namespace aware" flag for parsers we create.
     *
     * @return the "namespace aware" flag for parsers we create.
     */
    public boolean getNamespaceAware() {
        writeline("/home/ubuntu/results/coverage/Digester/Digester_10_10.coverage", "0c1c97ca-8c66-42ba-b489-256768ac7414");
        return (this.namespaceAware);
    }

    /**
     * Set the "namespace aware" flag for parsers we create.
     *
     * @param namespaceAware The new "namespace aware" flag
     */
    public void setNamespaceAware(boolean namespaceAware) {
        writeline("/home/ubuntu/results/coverage/Digester/Digester_10_10.coverage", "e2749225-d5a8-47d8-9bbf-c8ec6daf7298");
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
        writeline("/home/ubuntu/results/coverage/Digester/Digester_10_10.coverage", "4ca0522e-6337-4f0f-9d41-f6340765e0bf");
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
        writeline("/home/ubuntu/results/coverage/Digester/Digester_10_10.coverage", "f847e653-2447-48ab-a7cf-5a31c9c2b824");
        this.xincludeAware = xincludeAware;
    }

    /**
     * Set the public id of the current file being parse.
     *
     * @param publicId the DTD/Schema public's id.
     */
    public void setPublicId(String publicId) {
        writeline("/home/ubuntu/results/coverage/Digester/Digester_10_10.coverage", "c570a014-09e3-465e-91e2-657b49ed32ac");
        this.publicId = publicId;
    }

    /**
     * Return the public identifier of the DTD we are currently parsing under, if any.
     *
     * @return the public identifier of the DTD we are currently parsing under, if any.
     */
    public String getPublicId() {
        writeline("/home/ubuntu/results/coverage/Digester/Digester_10_10.coverage", "603ec314-261e-40c7-ae93-ad14113464ec");
        return (this.publicId);
    }

    /**
     * Return the namespace URI that will be applied to all subsequently added <code>Rule</code> objects.
     *
     * @return the namespace URI that will be applied to all subsequently added <code>Rule</code> objects.
     */
    public String getRuleNamespaceURI() {
        writeline("/home/ubuntu/results/coverage/Digester/Digester_10_10.coverage", "2d8e5e6e-38eb-4242-bab8-47cf0e6c2d2b");
        return (getRules().getNamespaceURI());
    }

    /**
     * Set the namespace URI that will be applied to all subsequently added <code>Rule</code> objects.
     *
     * @param ruleNamespaceURI Namespace URI that must match on all subsequently added rules, or <code>null</code> for
     *            matching regardless of the current namespace URI
     */
    public void setRuleNamespaceURI(String ruleNamespaceURI) {
        writeline("/home/ubuntu/results/coverage/Digester/Digester_10_10.coverage", "6f92cf9c-e3bc-4114-b7c7-f7fefc2e28d3");
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
        writeline("/home/ubuntu/results/coverage/Digester/Digester_10_10.coverage", "7426b309-c984-4b2d-8b1e-566622417d5d");
        if (parser != null) {
            writeline("/home/ubuntu/results/coverage/Digester/Digester_10_10.coverage", "259cc89e-acad-472e-8191-934906afc7ab");
            return (parser);
        }
        writeline("/home/ubuntu/results/coverage/Digester/Digester_10_10.coverage", "77c13793-a8f0-4023-9720-cbacf65552ae");
        try {
            writeline("/home/ubuntu/results/coverage/Digester/Digester_10_10.coverage", "53085190-d9cd-4527-998d-d8dab363659c");
            parser = getFactory().newSAXParser();
        } catch (Exception e) {
            writeline("/home/ubuntu/results/coverage/Digester/Digester_10_10.coverage", "84c77799-188e-4e57-ad71-b69191bb3275");
            log.error("Digester.getParser: ", e);
            writeline("/home/ubuntu/results/coverage/Digester/Digester_10_10.coverage", "1cd5fc55-c9e0-42e8-883c-2d8fcd63ed74");
            return (null);
        }
        writeline("/home/ubuntu/results/coverage/Digester/Digester_10_10.coverage", "b9a02545-1dba-434b-a307-2291192a380f");
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
        writeline("/home/ubuntu/results/coverage/Digester/Digester_10_10.coverage", "21348c71-69ed-4a70-b208-fa08f7ba93a0");
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
        writeline("/home/ubuntu/results/coverage/Digester/Digester_10_10.coverage", "83c3b313-e4f9-4a2e-8f51-bca59293ddff");
        getParser().setProperty(property, value);
    }

    /**
     * Return the <code>Rules</code> implementation object containing our rules collection and associated matching
     * policy. If none has been established, a default implementation will be created and returned.
     *
     * @return the <code>Rules</code> implementation object.
     */
    public Rules getRules() {
        writeline("/home/ubuntu/results/coverage/Digester/Digester_10_10.coverage", "42e997fe-10f4-446a-99bd-f3d247f1e28d");
        if (this.rules == null) {
            writeline("/home/ubuntu/results/coverage/Digester/Digester_10_10.coverage", "9128b54f-73e6-402f-bee8-c086e8ab50bf");
            this.rules = new RulesBase();
            writeline("/home/ubuntu/results/coverage/Digester/Digester_10_10.coverage", "db693613-29db-4a85-9bd5-af12fc94b6dd");
            this.rules.setDigester(this);
        }
        writeline("/home/ubuntu/results/coverage/Digester/Digester_10_10.coverage", "bfb09d2b-6132-4df4-9759-a36bbb5aa235");
        return (this.rules);
    }

    /**
     * Set the <code>Rules</code> implementation object containing our rules collection and associated matching policy.
     *
     * @param rules New Rules implementation
     */
    public void setRules(Rules rules) {
        writeline("/home/ubuntu/results/coverage/Digester/Digester_10_10.coverage", "0f42d62e-f5d7-44d6-bcd9-f787d71e1443");
        this.rules = rules;
        writeline("/home/ubuntu/results/coverage/Digester/Digester_10_10.coverage", "e423c744-54fc-457f-b47a-148d4ca9f72d");
        this.rules.setDigester(this);
    }

    /**
     * Return the XML Schema used when parsing.
     *
     * @return The {@link Schema} instance in use.
     * @since 2.0
     */
    public Schema getXMLSchema() {
        writeline("/home/ubuntu/results/coverage/Digester/Digester_10_10.coverage", "e1ca892b-9a6a-441c-9cba-6bee9ecf1cb4");
        return (this.schema);
    }

    /**
     * Set the XML Schema to be used when parsing.
     *
     * @param schema The {@link Schema} instance to use.
     * @since 2.0
     */
    public void setXMLSchema(Schema schema) {
        writeline("/home/ubuntu/results/coverage/Digester/Digester_10_10.coverage", "99a1b7ed-6ecc-4e9f-af1a-9f2e587485f3");
        this.schema = schema;
    }

    /**
     * Return the boolean as to whether the context ClassLoader should be used.
     *
     * @return true, if the context ClassLoader should be used, false otherwise.
     */
    public boolean getUseContextClassLoader() {
        writeline("/home/ubuntu/results/coverage/Digester/Digester_10_10.coverage", "f53e091c-417e-46cf-ad9e-51c1d080aa4e");
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
        writeline("/home/ubuntu/results/coverage/Digester/Digester_10_10.coverage", "24f8604e-f485-4623-8228-19a88a5a52cd");
        useContextClassLoader = use;
    }

    /**
     * Return the validating parser flag.
     *
     * @return the validating parser flag.
     */
    public boolean getValidating() {
        writeline("/home/ubuntu/results/coverage/Digester/Digester_10_10.coverage", "c8278471-903e-40ea-9b4f-abc9ad7deac7");
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
        writeline("/home/ubuntu/results/coverage/Digester/Digester_10_10.coverage", "ab107297-bbb3-4bb4-bccb-4f66fe1006ef");
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
        writeline("/home/ubuntu/results/coverage/Digester/Digester_10_10.coverage", "83ab5ffd-7849-4a07-bba9-34efb24c3179");
        if (reader == null) {
            writeline("/home/ubuntu/results/coverage/Digester/Digester_10_10.coverage", "c6ae46d0-a8fc-4a20-8178-f088e756f0c9");
            reader = getParser().getXMLReader();
        }
        writeline("/home/ubuntu/results/coverage/Digester/Digester_10_10.coverage", "56da382d-acb5-4b30-b7b7-97dec464243d");
        reader.setDTDHandler(this);
        writeline("/home/ubuntu/results/coverage/Digester/Digester_10_10.coverage", "40f430a7-83b7-44df-b309-eb67472d1da2");
        reader.setContentHandler(this);
        writeline("/home/ubuntu/results/coverage/Digester/Digester_10_10.coverage", "000b9b87-b8f3-42dc-80e7-e310b9930ec3");
        if (entityResolver == null) {
            writeline("/home/ubuntu/results/coverage/Digester/Digester_10_10.coverage", "9729abbc-2ffb-49ca-8c29-d5c3e125d5f0");
            reader.setEntityResolver(this);
        } else {
            writeline("/home/ubuntu/results/coverage/Digester/Digester_10_10.coverage", "e741a913-876a-44dc-bf9f-2dce669c8c91");
            reader.setEntityResolver(entityResolver);
        }
        writeline("/home/ubuntu/results/coverage/Digester/Digester_10_10.coverage", "80e89d7a-3af8-4a7e-ac13-33699f9d678b");
        if (this.errorHandler != null) {
            writeline("/home/ubuntu/results/coverage/Digester/Digester_10_10.coverage", "2a7acd07-b1bc-4d16-bf91-34269c59d50c");
            reader.setErrorHandler(this.errorHandler);
        } else {
            writeline("/home/ubuntu/results/coverage/Digester/Digester_10_10.coverage", "5f5c2948-1bdd-42dd-b1ac-869821282ee2");
            reader.setErrorHandler(this);
        }
        writeline("/home/ubuntu/results/coverage/Digester/Digester_10_10.coverage", "aaa0ccee-9e3c-489f-bb0c-29b42b049265");
        return reader;
    }

    /**
     * Gets the <code>Substitutor</code> used to convert attributes and body text.
     *
     * @return the <code>Substitutor</code> used to convert attributes and body text,
     *         null if not substitutions are to be performed.
     */
    public Substitutor getSubstitutor() {
        writeline("/home/ubuntu/results/coverage/Digester/Digester_10_10.coverage", "0bd1be87-5e56-45a0-9c2e-c3c0fd09935f");
        return substitutor;
    }

    /**
     * Sets the <code>Substitutor</code> to be used to convert attributes and body text.
     *
     * @param substitutor the Substitutor to be used to convert attributes and body text or null if not substitution of
     *            these values is to be performed.
     */
    public void setSubstitutor(Substitutor substitutor) {
        writeline("/home/ubuntu/results/coverage/Digester/Digester_10_10.coverage", "37cd6591-a45c-4206-a12f-8a07809034f2");
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
        writeline("/home/ubuntu/results/coverage/Digester/Digester_10_10.coverage", "95c43097-ef04-4172-8b4f-3e31cf30e4dd");
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
        writeline("/home/ubuntu/results/coverage/Digester/Digester_10_10.coverage", "da6e3d98-a5f4-48b6-8729-141b29cd2528");
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
        writeline("/home/ubuntu/results/coverage/Digester/Digester_10_10.coverage", "dd05100b-5d5a-443c-b631-f784320b052d");
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
        writeline("/home/ubuntu/results/coverage/Digester/Digester_10_10.coverage", "f6853111-498a-4d76-9c7c-72f34e093297");
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
        writeline("/home/ubuntu/results/coverage/Digester/Digester_10_10.coverage", "fe2b1b20-75ec-42b2-9eec-26023816cafe");
        if (!namespaceAware) {
            writeline("/home/ubuntu/results/coverage/Digester/Digester_10_10.coverage", "6137aa3c-10d5-4350-9cb2-5e536b6a5574");
            log.warn("Digester is not namespace aware");
        }
        writeline("/home/ubuntu/results/coverage/Digester/Digester_10_10.coverage", "eed49ce1-28f4-4d73-ae5a-1255d20d4891");
        Map<String, String> currentNamespaces = new HashMap<String, String>();
        writeline("/home/ubuntu/results/coverage/Digester/Digester_10_10.coverage", "890ab6bd-7272-4c4e-ace6-d5eaf3d277d5");
        return currentNamespaces;
    }

    /**
     * Returns the executor service used to run asynchronous parse method.
     *
     * @return the executor service used to run asynchronous parse method
     * @since 3.1
     */
    public ExecutorService getExecutorService() {
        writeline("/home/ubuntu/results/coverage/Digester/Digester_10_10.coverage", "6af73247-8117-4ebb-bc81-a129ee258347");
        return executorService;
    }

    /**
     * Sets the executor service to run asynchronous parse method.
     *
     * @param executorService the executor service to run asynchronous parse method
     * @since 3.1
     */
    public void setExecutorService(ExecutorService executorService) {
        writeline("/home/ubuntu/results/coverage/Digester/Digester_10_10.coverage", "28b364c2-4477-4320-a481-95e5ce0dcab7");
        this.executorService = executorService;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public void characters(char buffer[], int start, int length) throws SAXException {
        writeline("/home/ubuntu/results/coverage/Digester/Digester_10_10.coverage", "c50cc2e3-5b49-448a-8d60-663e3ad73b93");
        if (customContentHandler != null) {
            writeline("/home/ubuntu/results/coverage/Digester/Digester_10_10.coverage", "ec2d7cc7-8864-44c5-89f8-c931a57a43b3");
            customContentHandler.characters(buffer, start, length);
            writeline("/home/ubuntu/results/coverage/Digester/Digester_10_10.coverage", "24d464b1-3d04-4d45-a688-9fed5757a4f4");
            return;
        }
        writeline("/home/ubuntu/results/coverage/Digester/Digester_10_10.coverage", "94322055-8387-47fb-b63b-76df782bb62c");
        if (saxLog.isDebugEnabled()) {
            writeline("/home/ubuntu/results/coverage/Digester/Digester_10_10.coverage", "2042e3c1-4edf-4795-a004-dc1ea9d131ed");
            saxLog.debug("characters(" + new String(buffer, start, length) + ")");
        }
        writeline("/home/ubuntu/results/coverage/Digester/Digester_10_10.coverage", "6b75edf0-2bad-44a5-aec0-7874c786cc41");
        bodyText.append(buffer, start, length);
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public void endDocument() throws SAXException {
        writeline("/home/ubuntu/results/coverage/Digester/Digester_10_10.coverage", "43279574-4cfe-4249-9d45-9b263dbcb289");
        if (saxLog.isDebugEnabled()) {
            writeline("/home/ubuntu/results/coverage/Digester/Digester_10_10.coverage", "cf65b34a-55fa-4019-9e3d-d3461788dbc7");
            if (getCount() > 1) {
                writeline("/home/ubuntu/results/coverage/Digester/Digester_10_10.coverage", "a47a7dee-f2f4-43f7-b84b-f279abc4d14f");
                saxLog.debug("endDocument():  " + getCount() + " elements left");
            } else {
                writeline("/home/ubuntu/results/coverage/Digester/Digester_10_10.coverage", "272d12d2-eaf6-40b3-a935-144e313f57f8");
                saxLog.debug("endDocument()");
            }
        }
        writeline("/home/ubuntu/results/coverage/Digester/Digester_10_10.coverage", "6cee13e2-6625-4f44-ab15-9a8f4e1a2b83");
        clear();
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public void endElement(String namespaceURI, String localName, String qName) throws SAXException {
        writeline("/home/ubuntu/results/coverage/Digester/Digester_10_10.coverage", "3e950192-6798-47d1-a2b4-5b8ef3dd0920");
        if (customContentHandler != null) {
            writeline("/home/ubuntu/results/coverage/Digester/Digester_10_10.coverage", "f855a480-1bb6-4cca-afe4-4f05439751e0");
            customContentHandler.endElement(namespaceURI, localName, qName);
            writeline("/home/ubuntu/results/coverage/Digester/Digester_10_10.coverage", "6aabe725-e604-4fe2-a3cb-f6a15d488d93");
            return;
        }
        writeline("/home/ubuntu/results/coverage/Digester/Digester_10_10.coverage", "5795473c-033e-4307-8ccd-76958a680c88");
        boolean debug = log.isDebugEnabled();
        writeline("/home/ubuntu/results/coverage/Digester/Digester_10_10.coverage", "cea33ccd-679a-43ae-ae56-c7ce48d1ab1c");
        if (debug) {
            writeline("/home/ubuntu/results/coverage/Digester/Digester_10_10.coverage", "6fffdad5-da2b-47fa-9116-3e411cfe6b5f");
            if (saxLog.isDebugEnabled()) {
                writeline("/home/ubuntu/results/coverage/Digester/Digester_10_10.coverage", "cd27e7f1-ff5d-472a-a540-9f657191e1fd");
                saxLog.debug("endElement(" + namespaceURI + "," + localName + "," + qName + ")");
            }
            writeline("/home/ubuntu/results/coverage/Digester/Digester_10_10.coverage", "c0ef7266-a510-43b9-954b-251404d29950");
            log.debug("  match='" + match + "'");
            writeline("/home/ubuntu/results/coverage/Digester/Digester_10_10.coverage", "897c39af-8933-4e5b-8694-6cfc677def5f");
            log.debug("  bodyText='" + bodyText + "'");
        }
        writeline("/home/ubuntu/results/coverage/Digester/Digester_10_10.coverage", "6ed1eae3-ca58-4347-9ef8-e11e306d5df5");
        String name = localName;
        writeline("/home/ubuntu/results/coverage/Digester/Digester_10_10.coverage", "80eca944-3a30-4f4d-ad1c-35c3b175eec2");
        if ((name == null) || (name.length() < 1)) {
            writeline("/home/ubuntu/results/coverage/Digester/Digester_10_10.coverage", "0f4ba709-54dd-4c9f-b2c7-4447e395b103");
            name = qName;
        }
        writeline("/home/ubuntu/results/coverage/Digester/Digester_10_10.coverage", "ef07f75b-8292-47d5-86f6-d78578d3f98c");
        List<Rule> rules = matches.pop();
        writeline("/home/ubuntu/results/coverage/Digester/Digester_10_10.coverage", "c91ce02b-d973-4015-aae5-6771b33b7520");
        if ((rules != null) && (rules.size() > 0)) {
            writeline("/home/ubuntu/results/coverage/Digester/Digester_10_10.coverage", "e2b63c3a-b36a-41d0-9ad7-07916f8c20b0");
            String bodyText = this.bodyText.toString();
            writeline("/home/ubuntu/results/coverage/Digester/Digester_10_10.coverage", "6f828feb-c4d7-4f9c-9640-2e9808159373");
            Substitutor substitutor = getSubstitutor();
            writeline("/home/ubuntu/results/coverage/Digester/Digester_10_10.coverage", "87a59d7f-0789-491f-ada7-0e374627e04a");
            if (substitutor != null) {
                writeline("/home/ubuntu/results/coverage/Digester/Digester_10_10.coverage", "4a2aaf2a-05a8-4ebe-8543-2f7c4f802a50");
                bodyText = substitutor.substitute(bodyText);
            }
            writeline("/home/ubuntu/results/coverage/Digester/Digester_10_10.coverage", "6e514594-ef80-44e0-9d55-92d585b57c93");
            for (int i = 0; i < rules.size(); i++) {
                writeline("/home/ubuntu/results/coverage/Digester/Digester_10_10.coverage", "d0cb663c-f344-45b0-9878-d18cea8ac2ec");
                try {
                    writeline("/home/ubuntu/results/coverage/Digester/Digester_10_10.coverage", "aeb0b331-91c0-4c4a-ba15-b8afa64d14a1");
                    Rule rule = rules.get(i);
                    writeline("/home/ubuntu/results/coverage/Digester/Digester_10_10.coverage", "2ec30c77-e3f2-4a5a-a9c3-96906fd8ca7d");
                    if (debug) {
                        writeline("/home/ubuntu/results/coverage/Digester/Digester_10_10.coverage", "a9a3fa34-0377-4ba0-b97e-9422fbcf71d6");
                        log.debug("  Fire body() for " + rule);
                    }
                    writeline("/home/ubuntu/results/coverage/Digester/Digester_10_10.coverage", "28fd5d9c-54cd-4988-95a6-dc3747ba0feb");
                    rule.body(namespaceURI, name, bodyText);
                } catch (Exception e) {
                    writeline("/home/ubuntu/results/coverage/Digester/Digester_10_10.coverage", "5ab84d94-5fd2-4293-b5b7-76ca92a1e88e");
                    log.error("Body event threw exception", e);
                } catch (Error e) {
                    writeline("/home/ubuntu/results/coverage/Digester/Digester_10_10.coverage", "39363684-20d2-4c15-aff9-83579a2078eb");
                    log.error("Body event threw error", e);
                }
            }
        } else {
            writeline("/home/ubuntu/results/coverage/Digester/Digester_10_10.coverage", "8fc254ae-1f9c-4709-b110-701776977281");
            if (debug) {
                writeline("/home/ubuntu/results/coverage/Digester/Digester_10_10.coverage", "f081d7dd-fb12-45ba-8de6-ad20f5bf9d10");
                log.debug("  No rules found matching '" + match + "'.");
            }
        }
        writeline("/home/ubuntu/results/coverage/Digester/Digester_10_10.coverage", "6b00cbcb-db1e-47c5-8bff-61258ca1edf7");
        bodyText = bodyTexts.pop();
        writeline("/home/ubuntu/results/coverage/Digester/Digester_10_10.coverage", "0f1c5a40-27ea-4bc3-97c9-c2cd8cb58b98");
        if (debug) {
            writeline("/home/ubuntu/results/coverage/Digester/Digester_10_10.coverage", "2ba537b0-970f-4b8b-9273-34224335d323");
            log.debug("  Popping body text '" + bodyText.toString() + "'");
        }
        writeline("/home/ubuntu/results/coverage/Digester/Digester_10_10.coverage", "a9625cef-b102-403c-909d-e5919af0d597");
        if (rules != null) {
            writeline("/home/ubuntu/results/coverage/Digester/Digester_10_10.coverage", "5aaedbed-8b9c-4978-8678-4736cb96bd08");
            for (int i = 0; i < rules.size(); i++) {
                writeline("/home/ubuntu/results/coverage/Digester/Digester_10_10.coverage", "23f19efd-2ce5-4e8b-aa12-4173be572abe");
                int j = (rules.size() - i) - 1;
                writeline("/home/ubuntu/results/coverage/Digester/Digester_10_10.coverage", "184b5c09-4251-4319-b481-498d72cee0cc");
                try {
                    writeline("/home/ubuntu/results/coverage/Digester/Digester_10_10.coverage", "d22f5f2f-2c39-4b89-a0bc-0402e5b26b71");
                    Rule rule = rules.get(j);
                    writeline("/home/ubuntu/results/coverage/Digester/Digester_10_10.coverage", "b4973396-8d22-47e7-a7a2-75dd9d1294d9");
                    if (debug) {
                        writeline("/home/ubuntu/results/coverage/Digester/Digester_10_10.coverage", "87f0ce2f-10e5-4baf-bc9a-540c5af329d7");
                        log.debug("  Fire end() for " + rule);
                    }
                    writeline("/home/ubuntu/results/coverage/Digester/Digester_10_10.coverage", "91382185-f330-43d6-bd63-5801f8d8d4c2");
                    rule.end(namespaceURI, name);
                } catch (Exception e) {
                    writeline("/home/ubuntu/results/coverage/Digester/Digester_10_10.coverage", "5e36f5d0-e45b-4c66-b33b-551c02303f45");
                    log.error("End event threw exception", e);
                } catch (Error e) {
                    writeline("/home/ubuntu/results/coverage/Digester/Digester_10_10.coverage", "a9a75843-3d9e-42e1-99d9-dde641be3117");
                    log.error("End event threw error", e);
                }
            }
        }
        writeline("/home/ubuntu/results/coverage/Digester/Digester_10_10.coverage", "52aa30be-4416-4d58-a172-eb4095b7d3f5");
        int slash = match.lastIndexOf('/');
        writeline("/home/ubuntu/results/coverage/Digester/Digester_10_10.coverage", "bb038443-001d-483f-961e-e68a3e84bd3c");
        if (slash >= 0) {
            writeline("/home/ubuntu/results/coverage/Digester/Digester_10_10.coverage", "0664243d-6191-45d0-b91f-036c8a03777d");
            match = match.substring(0, slash);
        } else {
            writeline("/home/ubuntu/results/coverage/Digester/Digester_10_10.coverage", "6570fcf2-754a-4cc0-95cc-b86003c7d2e4");
            match = "";
        }
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public void endPrefixMapping(String prefix) throws SAXException {
        writeline("/home/ubuntu/results/coverage/Digester/Digester_10_10.coverage", "2f1ed953-bdd5-4442-b579-c7c03ccaee06");
        if (saxLog.isDebugEnabled()) {
            writeline("/home/ubuntu/results/coverage/Digester/Digester_10_10.coverage", "15b6622f-f01c-4e3f-8eca-a60127ff015c");
            saxLog.debug("endPrefixMapping(" + prefix + ")");
        }
        writeline("/home/ubuntu/results/coverage/Digester/Digester_10_10.coverage", "0a814172-cfce-4e10-8265-b7612f8d64ae");
        Stack<String> stack = namespaces.get(prefix);
        writeline("/home/ubuntu/results/coverage/Digester/Digester_10_10.coverage", "36b2e921-a024-4f73-a8ba-73340acf8d0d");
        if (stack == null) {
            writeline("/home/ubuntu/results/coverage/Digester/Digester_10_10.coverage", "451188b0-2205-4881-a2cf-ae113d5a82a4");
            return;
        }
        writeline("/home/ubuntu/results/coverage/Digester/Digester_10_10.coverage", "13462292-5efe-4736-bad5-c7d8e670ab56");
        try {
            writeline("/home/ubuntu/results/coverage/Digester/Digester_10_10.coverage", "8fbc176c-72cd-43e9-aef5-f87dcaffe8a5");
            stack.pop();
            writeline("/home/ubuntu/results/coverage/Digester/Digester_10_10.coverage", "8ff7d387-57a5-4dcd-9443-f5b86ebc8b75");
            if (stack.empty()) {
                writeline("/home/ubuntu/results/coverage/Digester/Digester_10_10.coverage", "c9767c26-e7d3-4d19-90e3-8f12daa779ca");
                namespaces.remove(prefix);
            }
        } catch (EmptyStackException e) {
        }
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public void ignorableWhitespace(char buffer[], int start, int len) throws SAXException {
        writeline("/home/ubuntu/results/coverage/Digester/Digester_10_10.coverage", "14aadf0b-04bf-4d91-a39b-8c65edbd905f");
        if (saxLog.isDebugEnabled()) {
            writeline("/home/ubuntu/results/coverage/Digester/Digester_10_10.coverage", "17da1082-39ca-4176-8b55-57add2ea716f");
            saxLog.debug("ignorableWhitespace(" + new String(buffer, start, len) + ")");
        }
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public void processingInstruction(String target, String data) throws SAXException {
        writeline("/home/ubuntu/results/coverage/Digester/Digester_10_10.coverage", "30b7b3f6-20a5-4d9b-b2b6-f99aacf3ea80");
        if (customContentHandler != null) {
            writeline("/home/ubuntu/results/coverage/Digester/Digester_10_10.coverage", "4d9c44fe-0c95-49a4-acfd-2104eecbe5ac");
            customContentHandler.processingInstruction(target, data);
            writeline("/home/ubuntu/results/coverage/Digester/Digester_10_10.coverage", "c61fa7a4-2a3c-4806-aec6-28baa2608d31");
            return;
        }
        writeline("/home/ubuntu/results/coverage/Digester/Digester_10_10.coverage", "503cb8e3-ab84-4c25-9a67-72c6d08b42db");
        if (saxLog.isDebugEnabled()) {
            writeline("/home/ubuntu/results/coverage/Digester/Digester_10_10.coverage", "1d6b8568-6b75-4ad9-809f-ca09eaa29d0c");
            saxLog.debug("processingInstruction('" + target + "','" + data + "')");
        }
    }

    /**
     * Gets the document locator associated with our parser.
     *
     * @return the Locator supplied by the document parser
     */
    public Locator getDocumentLocator() {
        writeline("/home/ubuntu/results/coverage/Digester/Digester_10_10.coverage", "f4b9388f-60d3-4dc0-b064-22ad7b630e36");
        return locator;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public void setDocumentLocator(Locator locator) {
        writeline("/home/ubuntu/results/coverage/Digester/Digester_10_10.coverage", "21d421df-81d7-4c25-bcfb-cc4e69253f3c");
        if (saxLog.isDebugEnabled()) {
            writeline("/home/ubuntu/results/coverage/Digester/Digester_10_10.coverage", "73ec4473-24d5-40f8-b804-cf848468acbe");
            saxLog.debug("setDocumentLocator(" + locator + ")");
        }
        writeline("/home/ubuntu/results/coverage/Digester/Digester_10_10.coverage", "e693bbf5-1232-476e-9e98-e2166dd9d896");
        this.locator = locator;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public void skippedEntity(String name) throws SAXException {
        writeline("/home/ubuntu/results/coverage/Digester/Digester_10_10.coverage", "eb9cf742-2c5c-4444-83db-7c15e17cd594");
        if (saxLog.isDebugEnabled()) {
            writeline("/home/ubuntu/results/coverage/Digester/Digester_10_10.coverage", "49f23c31-38d6-48f3-9900-85253c89079c");
            saxLog.debug("skippedEntity(" + name + ")");
        }
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public void startDocument() throws SAXException {
        writeline("/home/ubuntu/results/coverage/Digester/Digester_10_10.coverage", "d1be308d-daf9-4ce3-916d-a06e12a62b4e");
        if (saxLog.isDebugEnabled()) {
            writeline("/home/ubuntu/results/coverage/Digester/Digester_10_10.coverage", "0ea05b39-dc90-490d-86b3-811e82f2a3af");
            saxLog.debug("startDocument()");
        }
        writeline("/home/ubuntu/results/coverage/Digester/Digester_10_10.coverage", "c8afb8b7-f561-45d0-9be4-c6de329d9248");
        configure();
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public void startElement(String namespaceURI, String localName, String qName, Attributes list) throws SAXException {
        writeline("/home/ubuntu/results/coverage/Digester/Digester_10_10.coverage", "054f3221-2a8d-4408-9047-9faf8024abec");
        boolean debug = log.isDebugEnabled();
        writeline("/home/ubuntu/results/coverage/Digester/Digester_10_10.coverage", "25e9b77a-45d4-43bb-ae61-a7548bb783fc");
        if (customContentHandler != null) {
            writeline("/home/ubuntu/results/coverage/Digester/Digester_10_10.coverage", "1fac179d-d63c-48e1-8246-fa9b41193d19");
            customContentHandler.startElement(namespaceURI, localName, qName, list);
            writeline("/home/ubuntu/results/coverage/Digester/Digester_10_10.coverage", "30826f30-1de0-41a3-b4a6-cd0f38eda8f9");
            return;
        }
        writeline("/home/ubuntu/results/coverage/Digester/Digester_10_10.coverage", "f156b43d-593f-41aa-95ea-5721761e5c2e");
        if (saxLog.isDebugEnabled()) {
            writeline("/home/ubuntu/results/coverage/Digester/Digester_10_10.coverage", "099d027d-dde6-4696-b82a-679a272a872f");
            saxLog.debug("startElement(" + namespaceURI + "," + localName + "," + qName + ")");
        }
        writeline("/home/ubuntu/results/coverage/Digester/Digester_10_10.coverage", "87cd5957-c925-4b1c-bc6b-8443c81bdbdc");
        bodyTexts.push(bodyText);
        writeline("/home/ubuntu/results/coverage/Digester/Digester_10_10.coverage", "e84ab5d1-2c85-4c0b-be4e-4bee74b59890");
        if (debug) {
            writeline("/home/ubuntu/results/coverage/Digester/Digester_10_10.coverage", "1f6d3177-99f2-4448-90da-bc3889b27bf9");
            log.debug("  Pushing body text '" + bodyText.toString() + "'");
        }
        writeline("/home/ubuntu/results/coverage/Digester/Digester_10_10.coverage", "6e735ca4-7e47-4c0b-b395-ed3cc8481f4f");
        bodyText = new StringBuilder();
        writeline("/home/ubuntu/results/coverage/Digester/Digester_10_10.coverage", "72dd992c-767d-478a-84cc-1b8b4ee997aa");
        String name = localName;
        writeline("/home/ubuntu/results/coverage/Digester/Digester_10_10.coverage", "c94f172d-6fcd-4367-b1e1-428254d721d7");
        if ((name == null) || (name.length() < 1)) {
            writeline("/home/ubuntu/results/coverage/Digester/Digester_10_10.coverage", "c5cbc0db-d8af-4c46-82c9-0dff6265c9cd");
            name = qName;
        }
        writeline("/home/ubuntu/results/coverage/Digester/Digester_10_10.coverage", "61888c79-ea65-4f3c-9ba5-aafb7042e8e4");
        StringBuilder sb = new StringBuilder(match);
        writeline("/home/ubuntu/results/coverage/Digester/Digester_10_10.coverage", "cbe4b5b4-6dc8-4a7f-a6f0-4ef407c34e77");
        if (match.length() > 0) {
            writeline("/home/ubuntu/results/coverage/Digester/Digester_10_10.coverage", "9e2ac5ec-122b-479e-bb71-55196f69bbcb");
            sb.append('/');
        }
        writeline("/home/ubuntu/results/coverage/Digester/Digester_10_10.coverage", "52472ab4-9dbc-482e-a653-cb89c6547b9d");
        sb.append(name);
        writeline("/home/ubuntu/results/coverage/Digester/Digester_10_10.coverage", "8a2c883b-3df4-4ce0-a909-56e24e615f44");
        match = sb.toString();
        writeline("/home/ubuntu/results/coverage/Digester/Digester_10_10.coverage", "f0613885-fec9-4daf-9b13-3da81b9840d7");
        if (debug) {
            writeline("/home/ubuntu/results/coverage/Digester/Digester_10_10.coverage", "0bcd2852-9c06-42ec-be5f-c5776026f772");
            log.debug("  New match='" + match + "'");
        }
        writeline("/home/ubuntu/results/coverage/Digester/Digester_10_10.coverage", "a4c081e1-7de9-4ee3-a074-16795aa5cdec");
        List<Rule> rules = getRules().match(namespaceURI, match, localName, list);
        writeline("/home/ubuntu/results/coverage/Digester/Digester_10_10.coverage", "742d7e14-8a38-4c47-9601-381ffa4436ae");
        matches.push(rules);
        writeline("/home/ubuntu/results/coverage/Digester/Digester_10_10.coverage", "2b3eccdb-af45-4e6a-be60-628363ed5e2d");
        if ((rules != null) && (rules.size() > 0)) {
            writeline("/home/ubuntu/results/coverage/Digester/Digester_10_10.coverage", "866c4e41-d854-4c95-b450-1ad4102d5cf8");
            Substitutor substitutor = getSubstitutor();
            writeline("/home/ubuntu/results/coverage/Digester/Digester_10_10.coverage", "d60b8a15-75ad-4507-a39c-2e18e06513ae");
            if (substitutor != null) {
                writeline("/home/ubuntu/results/coverage/Digester/Digester_10_10.coverage", "1d94e4b5-f028-4664-a37b-74d24e9a35c7");
                list = substitutor.substitute(list);
            }
            writeline("/home/ubuntu/results/coverage/Digester/Digester_10_10.coverage", "3b79c3fe-c7a4-46fc-b5bb-fd1e5ba758e5");
            for (int i = 0; i < rules.size(); i++) {
                writeline("/home/ubuntu/results/coverage/Digester/Digester_10_10.coverage", "6234cb1b-09e7-4575-8073-0eecc299b82e");
                try {
                    writeline("/home/ubuntu/results/coverage/Digester/Digester_10_10.coverage", "1d5ec59a-afde-40c1-80d6-3328832e47df");
                    Rule rule = rules.get(i);
                    writeline("/home/ubuntu/results/coverage/Digester/Digester_10_10.coverage", "0cb7a5c8-ab37-46a8-bd60-9344abba705b");
                    if (debug) {
                        writeline("/home/ubuntu/results/coverage/Digester/Digester_10_10.coverage", "b533337f-8067-4e79-bc21-4d4870a4918c");
                        log.debug("  Fire begin() for " + rule);
                    }
                    writeline("/home/ubuntu/results/coverage/Digester/Digester_10_10.coverage", "9844b2fb-f65d-48d9-bcf1-4a454f6a8aac");
                    rule.begin(namespaceURI, name, list);
                } catch (Exception e) {
                    writeline("/home/ubuntu/results/coverage/Digester/Digester_10_10.coverage", "398c1ccd-5a6a-4f87-9da2-9b968a279327");
                    log.error("Begin event threw exception", e);
                } catch (Error e) {
                    writeline("/home/ubuntu/results/coverage/Digester/Digester_10_10.coverage", "221f833d-18e9-47a7-b2d8-6e570f730b5b");
                    log.error("Begin event threw error", e);
                }
            }
        } else {
            writeline("/home/ubuntu/results/coverage/Digester/Digester_10_10.coverage", "265d1a2f-1ef6-4077-b0b2-c4aa9bc71dca");
            if (debug) {
                writeline("/home/ubuntu/results/coverage/Digester/Digester_10_10.coverage", "03e282f5-1b1d-481f-9a42-f49076535f66");
                log.debug("  No rules found matching '" + match + "'.");
            }
        }
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public void startPrefixMapping(String prefix, String namespaceURI) throws SAXException {
        writeline("/home/ubuntu/results/coverage/Digester/Digester_10_10.coverage", "dcd0f7c4-23f7-4170-91af-03550cb87915");
        if (saxLog.isDebugEnabled()) {
            writeline("/home/ubuntu/results/coverage/Digester/Digester_10_10.coverage", "0170fe92-d078-4d27-b54b-874d27cf3b27");
            saxLog.debug("startPrefixMapping(" + prefix + "," + namespaceURI + ")");
        }
        writeline("/home/ubuntu/results/coverage/Digester/Digester_10_10.coverage", "0ce4f140-97e8-4f8a-a7b2-137654aa2106");
        Stack<String> stack = namespaces.get(prefix);
        writeline("/home/ubuntu/results/coverage/Digester/Digester_10_10.coverage", "a3bb8709-7c81-4a93-9763-00775b391e44");
        if (stack == null) {
            writeline("/home/ubuntu/results/coverage/Digester/Digester_10_10.coverage", "2a779cdb-5309-4654-8e85-7df1eb7901b5");
            stack = new Stack<String>();
            writeline("/home/ubuntu/results/coverage/Digester/Digester_10_10.coverage", "4c50b3e1-ee0e-45be-9f13-5270f5b94f25");
            namespaces.put(prefix, stack);
        }
        writeline("/home/ubuntu/results/coverage/Digester/Digester_10_10.coverage", "d7b6aefb-e5d5-4ffa-b1c6-6abd84ae4312");
        stack.push(namespaceURI);
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public void notationDecl(String name, String publicId, String systemId) {
        writeline("/home/ubuntu/results/coverage/Digester/Digester_10_10.coverage", "5bca94aa-962c-443d-8428-b4193f1b0446");
        if (saxLog.isDebugEnabled()) {
            writeline("/home/ubuntu/results/coverage/Digester/Digester_10_10.coverage", "1c471d32-246e-4a1c-acfd-5c4eba29fc5e");
            saxLog.debug("notationDecl(" + name + "," + publicId + "," + systemId + ")");
        }
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public void unparsedEntityDecl(String name, String publicId, String systemId, String notation) {
        writeline("/home/ubuntu/results/coverage/Digester/Digester_10_10.coverage", "1f6b694e-178d-4b4a-bf9a-d36672f0dea6");
        if (saxLog.isDebugEnabled()) {
            writeline("/home/ubuntu/results/coverage/Digester/Digester_10_10.coverage", "dad83e93-1ff1-4213-809b-d9d18a06c511");
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
        writeline("/home/ubuntu/results/coverage/Digester/Digester_10_10.coverage", "18a0ef83-bb4a-44fe-93fe-914d0bd97d67");
        this.entityResolver = entityResolver;
    }

    /**
     * Return the Entity Resolver used by the SAX parser.
     *
     * @return the Entity Resolver used by the SAX parser.
     */
    public EntityResolver getEntityResolver() {
        writeline("/home/ubuntu/results/coverage/Digester/Digester_10_10.coverage", "56d4e161-6f25-4587-b8c9-1b033228ad6e");
        return entityResolver;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public InputSource resolveEntity(String publicId, String systemId) throws SAXException {
        writeline("/home/ubuntu/results/coverage/Digester/Digester_10_10.coverage", "061f4fe3-e712-4458-941b-c90a6a994818");
        if (saxLog.isDebugEnabled()) {
            writeline("/home/ubuntu/results/coverage/Digester/Digester_10_10.coverage", "e0569ee2-963a-46b6-a191-455da24bf826");
            saxLog.debug("resolveEntity('" + publicId + "', '" + systemId + "')");
        }
        writeline("/home/ubuntu/results/coverage/Digester/Digester_10_10.coverage", "995213b7-08f9-40ac-b7f1-fbe9225b7c83");
        if (publicId != null) {
            writeline("/home/ubuntu/results/coverage/Digester/Digester_10_10.coverage", "5797427d-1234-4524-a6b1-12fac119458f");
            this.publicId = publicId;
        }
        writeline("/home/ubuntu/results/coverage/Digester/Digester_10_10.coverage", "021e509f-2291-41fe-97e8-c1b27a595eda");
        URL entityURL = null;
        writeline("/home/ubuntu/results/coverage/Digester/Digester_10_10.coverage", "b9e81397-76a6-40e2-b4c8-e5a430e75d81");
        if (publicId != null) {
            writeline("/home/ubuntu/results/coverage/Digester/Digester_10_10.coverage", "cb6349d6-f423-4dd7-bbcb-42a9c44e1cd9");
            entityURL = entityValidator.get(publicId);
        }
        writeline("/home/ubuntu/results/coverage/Digester/Digester_10_10.coverage", "63d670f7-f51b-474c-830c-5e75bd62928f");
        if (entityURL == null && systemId != null) {
            writeline("/home/ubuntu/results/coverage/Digester/Digester_10_10.coverage", "80a67ec2-057a-444c-8264-4479a70723e7");
            entityURL = entityValidator.get(systemId);
        }
        writeline("/home/ubuntu/results/coverage/Digester/Digester_10_10.coverage", "0fbc7245-4d1a-41e2-8af7-b50c5ed63452");
        if (entityURL == null) {
            writeline("/home/ubuntu/results/coverage/Digester/Digester_10_10.coverage", "f50af272-7fd6-48ba-a965-3c828d755203");
            if (systemId == null) {
                writeline("/home/ubuntu/results/coverage/Digester/Digester_10_10.coverage", "9db83931-4b0b-4475-8e7d-857fffe4b4a7");
                if (log.isDebugEnabled()) {
                    writeline("/home/ubuntu/results/coverage/Digester/Digester_10_10.coverage", "5cf24309-5ede-4b39-8eff-0c87180f8836");
                    log.debug(" Cannot resolve null entity, returning null InputSource");
                }
                writeline("/home/ubuntu/results/coverage/Digester/Digester_10_10.coverage", "13eda288-10ab-4158-8c1f-d24f8330567e");
                return (null);
            }
            writeline("/home/ubuntu/results/coverage/Digester/Digester_10_10.coverage", "277f3650-729a-477a-b4ab-24a361a9a5f0");
            if (log.isDebugEnabled()) {
                writeline("/home/ubuntu/results/coverage/Digester/Digester_10_10.coverage", "6238bab1-883d-48f5-bca0-1f7c38c1a6b4");
                log.debug(" Trying to resolve using system ID '" + systemId + "'");
            }
            writeline("/home/ubuntu/results/coverage/Digester/Digester_10_10.coverage", "c15fe8b5-62aa-4ca4-a9fc-fc49e18cd5d4");
            try {
                writeline("/home/ubuntu/results/coverage/Digester/Digester_10_10.coverage", "9fcdad11-8d2d-495b-8033-b31ee738349d");
                entityURL = new URL(systemId);
            } catch (MalformedURLException e) {
            }
        }
        writeline("/home/ubuntu/results/coverage/Digester/Digester_10_10.coverage", "0b2410ea-8c62-4366-a450-54709dd056d0");
        if (log.isDebugEnabled()) {
            writeline("/home/ubuntu/results/coverage/Digester/Digester_10_10.coverage", "1b1ec661-b966-4063-af2e-cff163e970d1");
            log.debug(" Resolving to alternate DTD '" + entityURL + "'");
        }
        writeline("/home/ubuntu/results/coverage/Digester/Digester_10_10.coverage", "48277351-21d3-468f-beed-21c35c541422");
        try {
            writeline("/home/ubuntu/results/coverage/Digester/Digester_10_10.coverage", "a37cb6bd-4fec-4e62-8a45-d34473199564");
            return createInputSourceFromURL(entityURL);
        } catch (Exception e) {
        }
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public void error(SAXParseException exception) throws SAXException {
        writeline("/home/ubuntu/results/coverage/Digester/Digester_10_10.coverage", "c252481a-9373-4aad-9ff1-15541a45bc0b");
        log.error("Parse Error at line " + exception.getLineNumber() + " column " + exception.getColumnNumber() + ": " + exception.getMessage(), exception);
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public void fatalError(SAXParseException exception) throws SAXException {
        writeline("/home/ubuntu/results/coverage/Digester/Digester_10_10.coverage", "ca2a4a3c-839a-4697-9bb8-da186ad19a6c");
        log.error("Parse Fatal Error at line " + exception.getLineNumber() + " column " + exception.getColumnNumber() + ": " + exception.getMessage(), exception);
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public void warning(SAXParseException exception) throws SAXException {
        writeline("/home/ubuntu/results/coverage/Digester/Digester_10_10.coverage", "773f3fe5-755c-4e22-8673-e38ce003c316");
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
        writeline("/home/ubuntu/results/coverage/Digester/Digester_10_10.coverage", "4f96f321-2a38-489b-bc0c-da6e5c374777");
        if (file == null) {
        }
        writeline("/home/ubuntu/results/coverage/Digester/Digester_10_10.coverage", "fb5c9115-3e29-4074-8581-ff7590bb9dde");
        InputSource input = new InputSource(new FileInputStream(file));
        writeline("/home/ubuntu/results/coverage/Digester/Digester_10_10.coverage", "73ebde50-1f82-4e1f-9a0b-ab8a1d15252e");
        input.setSystemId(file.toURI().toURL().toString());
        writeline("/home/ubuntu/results/coverage/Digester/Digester_10_10.coverage", "ee5b4ef9-2a65-4242-942c-243cea22b081");
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
        writeline("/home/ubuntu/results/coverage/Digester/Digester_10_10.coverage", "dd232200-801e-45f0-8597-39670d0236c0");
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
        writeline("/home/ubuntu/results/coverage/Digester/Digester_10_10.coverage", "89e9d024-3d43-4e42-9094-a29ae1bdd15a");
        if (input == null) {
        }
        writeline("/home/ubuntu/results/coverage/Digester/Digester_10_10.coverage", "f61d7705-f080-4506-a694-6bc7b9f6ecf8");
        configure();
        writeline("/home/ubuntu/results/coverage/Digester/Digester_10_10.coverage", "3621cd7e-f56d-4f3b-b4d9-ae9adc5babdf");
        String systemId = input.getSystemId();
        writeline("/home/ubuntu/results/coverage/Digester/Digester_10_10.coverage", "d56c2499-e692-4677-be55-840ea407ecab");
        if (systemId == null) {
            writeline("/home/ubuntu/results/coverage/Digester/Digester_10_10.coverage", "f42e8de0-2f8f-4b64-a43a-bdd8dde4d640");
            systemId = "(already loaded from stream)";
        }
        writeline("/home/ubuntu/results/coverage/Digester/Digester_10_10.coverage", "c5445c2b-3c3c-4f48-813e-c0c98aba143c");
        try {
            writeline("/home/ubuntu/results/coverage/Digester/Digester_10_10.coverage", "8cb07b4e-79a5-4761-a7b1-40ce01a90f2d");
            getXMLReader().parse(input);
        } catch (IOException e) {
            writeline("/home/ubuntu/results/coverage/Digester/Digester_10_10.coverage", "58a7e5fd-173e-4388-bb5d-963ced403203");
            log.error(format("An error occurred while reading stream from '%s', see nested exceptions", systemId), e);
        }
        writeline("/home/ubuntu/results/coverage/Digester/Digester_10_10.coverage", "035412f2-36ac-4778-9f95-8d2df694d75b");
        cleanup();
        writeline("/home/ubuntu/results/coverage/Digester/Digester_10_10.coverage", "0ca3093d-99ce-4d46-9580-b50272a98916");
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
        writeline("/home/ubuntu/results/coverage/Digester/Digester_10_10.coverage", "3674b24e-313e-4c63-8264-7ebc463d0997");
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
        writeline("/home/ubuntu/results/coverage/Digester/Digester_10_10.coverage", "a29370a9-a8f5-4601-a93e-395568c4838e");
        if (input == null) {
        }
        writeline("/home/ubuntu/results/coverage/Digester/Digester_10_10.coverage", "454a1ea7-a37b-4790-a7eb-9518c9162f1c");
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
        writeline("/home/ubuntu/results/coverage/Digester/Digester_10_10.coverage", "cabb68a7-35f4-4b4c-b297-eaefb670d00d");
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
        writeline("/home/ubuntu/results/coverage/Digester/Digester_10_10.coverage", "0b6e09b8-29f0-403f-9371-6a37908649a2");
        if (reader == null) {
        }
        writeline("/home/ubuntu/results/coverage/Digester/Digester_10_10.coverage", "363f8aea-9061-403e-8a21-864c58438429");
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
        writeline("/home/ubuntu/results/coverage/Digester/Digester_10_10.coverage", "566590de-5a6a-431d-ba5a-03aefe801c13");
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
        writeline("/home/ubuntu/results/coverage/Digester/Digester_10_10.coverage", "a1d5c833-0957-40ef-a83b-e187732bab12");
        if (uri == null) {
        }
        writeline("/home/ubuntu/results/coverage/Digester/Digester_10_10.coverage", "ec369938-8722-4f47-92c0-01b468ddff7f");
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
        writeline("/home/ubuntu/results/coverage/Digester/Digester_10_10.coverage", "ff38bcf2-9e8d-4e9e-8d5a-87ba4019434d");
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
        writeline("/home/ubuntu/results/coverage/Digester/Digester_10_10.coverage", "5e81ac55-2e88-4031-9629-56596d83d4c5");
        if (url == null) {
        }
        writeline("/home/ubuntu/results/coverage/Digester/Digester_10_10.coverage", "738ab747-5ec3-44b1-adba-40edea4f3980");
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
        writeline("/home/ubuntu/results/coverage/Digester/Digester_10_10.coverage", "966d3801-263a-45a3-9b22-fa61b32a0267");
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
        writeline("/home/ubuntu/results/coverage/Digester/Digester_10_10.coverage", "90c866db-4dd4-4d50-94ba-55668da891c9");
        if (executorService == null) {
        }
        writeline("/home/ubuntu/results/coverage/Digester/Digester_10_10.coverage", "bfb492df-d424-4d2b-94ad-a663d0b336e5");
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
        writeline("/home/ubuntu/results/coverage/Digester/Digester_10_10.coverage", "5783faeb-79ec-4e61-9c67-493b8bb505b3");
        if (log.isDebugEnabled()) {
            writeline("/home/ubuntu/results/coverage/Digester/Digester_10_10.coverage", "6a197fe1-ed5b-458c-9d29-2bbf2c169d3c");
            log.debug("register('" + publicId + "', '" + entityURL + "'");
        }
        writeline("/home/ubuntu/results/coverage/Digester/Digester_10_10.coverage", "46ea074b-f484-45fd-9a44-7274d3462a31");
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
        writeline("/home/ubuntu/results/coverage/Digester/Digester_10_10.coverage", "bf0082cd-c484-49c9-8ecc-5e333c8953c0");
        if (log.isDebugEnabled()) {
            writeline("/home/ubuntu/results/coverage/Digester/Digester_10_10.coverage", "a6390fad-535c-4393-b10a-09a9fc516807");
            log.debug("register('" + publicId + "', '" + entityURL + "'");
        }
        writeline("/home/ubuntu/results/coverage/Digester/Digester_10_10.coverage", "ccc4c7ff-baa8-43df-8cf7-f100ba869bee");
        try {
            writeline("/home/ubuntu/results/coverage/Digester/Digester_10_10.coverage", "7db56fce-38e7-432b-9318-a7595b02fb71");
            entityValidator.put(publicId, new URL(entityURL));
        } catch (MalformedURLException e) {
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
        writeline("/home/ubuntu/results/coverage/Digester/Digester_10_10.coverage", "0be24eef-794f-4f12-af01-4c8bba9f6157");
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
        writeline("/home/ubuntu/results/coverage/Digester/Digester_10_10.coverage", "1532223b-4d7f-43e1-a103-97074c5a8640");
        URLConnection connection = url.openConnection();
        writeline("/home/ubuntu/results/coverage/Digester/Digester_10_10.coverage", "fac07390-d697-4b3d-a303-f319adcc21fa");
        connection.setUseCaches(false);
        writeline("/home/ubuntu/results/coverage/Digester/Digester_10_10.coverage", "39c0dcae-f473-4565-a826-f517faacc392");
        InputStream stream = connection.getInputStream();
        writeline("/home/ubuntu/results/coverage/Digester/Digester_10_10.coverage", "2608209b-6b64-4bb5-8085-a4b0381b3853");
        InputSource source = new InputSource(stream);
        writeline("/home/ubuntu/results/coverage/Digester/Digester_10_10.coverage", "ad1bd532-ba2b-41cd-8264-44ff3fea2443");
        source.setSystemId(url.toExternalForm());
        writeline("/home/ubuntu/results/coverage/Digester/Digester_10_10.coverage", "1dcc0c7b-0aca-4b09-82c6-be6cabaac9e7");
        inputSources.add(source);
        writeline("/home/ubuntu/results/coverage/Digester/Digester_10_10.coverage", "e555dcf6-e701-42bb-bd11-d803f2ebf762");
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
        writeline("/home/ubuntu/results/coverage/Digester/Digester_10_10.coverage", "ad44957d-e92c-42c9-9a92-83d241a7a026");
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
        writeline("/home/ubuntu/results/coverage/Digester/Digester_10_10.coverage", "dc774184-1945-4c15-bb7d-9af56b021f2f");
        rule.setDigester(this);
        writeline("/home/ubuntu/results/coverage/Digester/Digester_10_10.coverage", "9ba1ad26-273e-4bbd-9691-177db3700884");
        getRules().add(pattern, rule);
    }

    /**
     * Register a set of Rule instances defined in a RuleSet.
     *
     * @param ruleSet The RuleSet instance to configure from
     */
    public void addRuleSet(RuleSet ruleSet) {
        writeline("/home/ubuntu/results/coverage/Digester/Digester_10_10.coverage", "64e57e3a-0050-4066-9af5-2842bebc1b02");
        String oldNamespaceURI = getRuleNamespaceURI();
        writeline("/home/ubuntu/results/coverage/Digester/Digester_10_10.coverage", "3e2f4af5-a285-4b4d-a647-6777015894f4");
        String newNamespaceURI = ruleSet.getNamespaceURI();
        writeline("/home/ubuntu/results/coverage/Digester/Digester_10_10.coverage", "4709a246-4583-40ce-bd2d-b2f27f00e341");
        if (log.isDebugEnabled()) {
            writeline("/home/ubuntu/results/coverage/Digester/Digester_10_10.coverage", "1f1c46c1-073a-4a63-9f87-fcd7f7053c48");
            if (newNamespaceURI == null) {
                writeline("/home/ubuntu/results/coverage/Digester/Digester_10_10.coverage", "88f5205c-48a0-4ef6-9c3a-183178289cb3");
                log.debug("addRuleSet() with no namespace URI");
            } else {
                writeline("/home/ubuntu/results/coverage/Digester/Digester_10_10.coverage", "13eae464-70a7-4beb-8e9f-07aeb93f1b11");
                log.debug("addRuleSet() with namespace URI " + newNamespaceURI);
            }
        }
        writeline("/home/ubuntu/results/coverage/Digester/Digester_10_10.coverage", "5e35485f-5c54-4909-b770-70f290812b67");
        setRuleNamespaceURI(newNamespaceURI);
        writeline("/home/ubuntu/results/coverage/Digester/Digester_10_10.coverage", "10ed2d71-a238-4012-a26d-e1f59f207541");
        ruleSet.addRuleInstances(this);
        writeline("/home/ubuntu/results/coverage/Digester/Digester_10_10.coverage", "fe3732ca-f885-4260-bc6f-a9198cdeb7d1");
        setRuleNamespaceURI(oldNamespaceURI);
    }

    /**
     * Add a "bean property setter" rule for the specified parameters.
     *
     * @param pattern Element matching pattern
     * @see BeanPropertySetterRule
     */
    public void addBeanPropertySetter(String pattern) {
        writeline("/home/ubuntu/results/coverage/Digester/Digester_10_10.coverage", "5fdb3797-8b83-49bb-a8f7-fa092dd24c43");
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
        writeline("/home/ubuntu/results/coverage/Digester/Digester_10_10.coverage", "99fe90e2-12e7-46d3-b2a3-9de9c0211dfe");
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
        writeline("/home/ubuntu/results/coverage/Digester/Digester_10_10.coverage", "5af1c8fb-79cc-4589-8d6a-1482a488444b");
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
        writeline("/home/ubuntu/results/coverage/Digester/Digester_10_10.coverage", "53932d4d-35cb-43f6-ba75-8b242960f86b");
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
        writeline("/home/ubuntu/results/coverage/Digester/Digester_10_10.coverage", "1a86390d-0544-466f-b070-d3dda471a86c");
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
        writeline("/home/ubuntu/results/coverage/Digester/Digester_10_10.coverage", "35d16d6c-7ab2-4bdc-9762-bf3a814f5a37");
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
        writeline("/home/ubuntu/results/coverage/Digester/Digester_10_10.coverage", "3a3c4063-2d6a-4f8c-aba9-c731ee3db471");
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
        writeline("/home/ubuntu/results/coverage/Digester/Digester_10_10.coverage", "28cecfe1-d8dd-4fa7-975d-4fe3dae7b31b");
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
        writeline("/home/ubuntu/results/coverage/Digester/Digester_10_10.coverage", "7418580a-dc09-44c1-acdf-653785d9b519");
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
        writeline("/home/ubuntu/results/coverage/Digester/Digester_10_10.coverage", "2c990649-1e9f-4d83-82ef-04addf00bfd2");
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
        writeline("/home/ubuntu/results/coverage/Digester/Digester_10_10.coverage", "3d53a4b0-a947-48b6-bd9a-2c223a6922c3");
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
        writeline("/home/ubuntu/results/coverage/Digester/Digester_10_10.coverage", "c5b4283f-8329-44c2-ab38-dcd21e0eba1f");
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
        writeline("/home/ubuntu/results/coverage/Digester/Digester_10_10.coverage", "5a55dc99-f694-49e3-b4d7-43cc4882c1bb");
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
        writeline("/home/ubuntu/results/coverage/Digester/Digester_10_10.coverage", "a49370b9-f233-416e-8bb0-ec8664363dcc");
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
        writeline("/home/ubuntu/results/coverage/Digester/Digester_10_10.coverage", "5de96f74-1956-4100-8367-7ed3a5c0f416");
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
        writeline("/home/ubuntu/results/coverage/Digester/Digester_10_10.coverage", "ea328aa1-b1e6-4795-90b5-2b8fe52c9988");
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
        writeline("/home/ubuntu/results/coverage/Digester/Digester_10_10.coverage", "c8900538-72f0-4c92-ae20-061721c2b1fe");
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
        writeline("/home/ubuntu/results/coverage/Digester/Digester_10_10.coverage", "278c7c36-3309-4a28-9d20-264a8f59108a");
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
        writeline("/home/ubuntu/results/coverage/Digester/Digester_10_10.coverage", "d3dbdfa5-6fc3-47e8-9015-58cc6c636a0f");
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
        writeline("/home/ubuntu/results/coverage/Digester/Digester_10_10.coverage", "e0161171-44bf-4987-a3bb-0f319dcc87da");
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
        writeline("/home/ubuntu/results/coverage/Digester/Digester_10_10.coverage", "b63eaf0a-0884-4788-968c-925176931fef");
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
        writeline("/home/ubuntu/results/coverage/Digester/Digester_10_10.coverage", "6584f875-6433-4a1e-87ca-e676b1704ded");
        creationFactory.setDigester(this);
        writeline("/home/ubuntu/results/coverage/Digester/Digester_10_10.coverage", "d77bfd8c-e081-4eae-bfc4-aeb749833b62");
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
        writeline("/home/ubuntu/results/coverage/Digester/Digester_10_10.coverage", "2085e315-9f8e-42f1-86d2-447e963f94cf");
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
        writeline("/home/ubuntu/results/coverage/Digester/Digester_10_10.coverage", "579a915b-74a0-4860-ae76-1e14779a006b");
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
        writeline("/home/ubuntu/results/coverage/Digester/Digester_10_10.coverage", "005f8720-dcc3-4dce-ac91-ddf795fde6d7");
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
        writeline("/home/ubuntu/results/coverage/Digester/Digester_10_10.coverage", "638ba7c8-51c9-47ac-aa27-958bba0fbb3b");
        addRule(pattern, new ObjectCreateRule(attributeName, clazz));
    }

    /**
     * Adds an {@link SetNestedPropertiesRule}.
     *
     * @param pattern register the rule with this pattern
     * @since 1.6
     */
    public void addSetNestedProperties(String pattern) {
        writeline("/home/ubuntu/results/coverage/Digester/Digester_10_10.coverage", "d0f10613-230f-4f4d-b533-9a16024d395e");
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
        writeline("/home/ubuntu/results/coverage/Digester/Digester_10_10.coverage", "fd5eb98b-8e13-44ba-9c67-e00fe71d2a51");
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
        writeline("/home/ubuntu/results/coverage/Digester/Digester_10_10.coverage", "0ecd48b1-b864-4467-9e46-338400a7c265");
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
        writeline("/home/ubuntu/results/coverage/Digester/Digester_10_10.coverage", "9b176dbe-62cb-4d28-afd4-9a04b2347c70");
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
        writeline("/home/ubuntu/results/coverage/Digester/Digester_10_10.coverage", "6032923e-7460-4776-b8a1-afe207166265");
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
        writeline("/home/ubuntu/results/coverage/Digester/Digester_10_10.coverage", "c5f27c12-61a6-487b-9829-ac38378e3f49");
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
        writeline("/home/ubuntu/results/coverage/Digester/Digester_10_10.coverage", "ec392965-fdeb-4a57-adec-185587253488");
        addRule(pattern, new SetRootRule(methodName, paramType));
    }

    /**
     * Add a "set properties" rule for the specified parameters.
     *
     * @param pattern Element matching pattern
     * @see SetPropertiesRule
     */
    public void addSetProperties(String pattern) {
        writeline("/home/ubuntu/results/coverage/Digester/Digester_10_10.coverage", "00b75e89-8ab1-4cab-afbd-4964a0fe940f");
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
        writeline("/home/ubuntu/results/coverage/Digester/Digester_10_10.coverage", "071b9d40-0436-4b47-aa7c-5d407cffbaef");
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
        writeline("/home/ubuntu/results/coverage/Digester/Digester_10_10.coverage", "b19332ee-59ce-40a3-ba31-3b4562ca761a");
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
        writeline("/home/ubuntu/results/coverage/Digester/Digester_10_10.coverage", "a03ebc67-230e-4984-9715-1f6fe8ad9752");
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
        writeline("/home/ubuntu/results/coverage/Digester/Digester_10_10.coverage", "8385c9ad-06fa-4df1-bb80-8128e23ca727");
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
        writeline("/home/ubuntu/results/coverage/Digester/Digester_10_10.coverage", "c506c783-27f4-4d68-8b69-8bf6d8e6d2d6");
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
        writeline("/home/ubuntu/results/coverage/Digester/Digester_10_10.coverage", "23f30476-0883-475b-876e-565d41ba2272");
        match = "";
        writeline("/home/ubuntu/results/coverage/Digester/Digester_10_10.coverage", "52bfbf66-5887-4820-8015-478fc871b1c6");
        bodyTexts.clear();
        writeline("/home/ubuntu/results/coverage/Digester/Digester_10_10.coverage", "bda7fd03-05ab-4d1f-931d-bdc5ea8ade67");
        params.clear();
        writeline("/home/ubuntu/results/coverage/Digester/Digester_10_10.coverage", "fc0da456-5d38-4ce1-9bf2-d8fc8cbf9abb");
        publicId = null;
        writeline("/home/ubuntu/results/coverage/Digester/Digester_10_10.coverage", "b169c08f-dc50-4dab-a50e-8988b54d4371");
        stack.clear();
        writeline("/home/ubuntu/results/coverage/Digester/Digester_10_10.coverage", "a114a1cc-b788-419f-b30f-7fbadf240dd0");
        stacksByName.clear();
        writeline("/home/ubuntu/results/coverage/Digester/Digester_10_10.coverage", "e1391337-8a84-4483-b6b0-b7aa821a99e9");
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
        writeline("/home/ubuntu/results/coverage/Digester/Digester_10_10.coverage", "bcc103cd-b5d1-423b-ae46-8d422f9cbb67");
        try {
            writeline("/home/ubuntu/results/coverage/Digester/Digester_10_10.coverage", "3bd5b72f-4b00-4080-997e-4a64615a38ca");
            return this.<T>npeSafeCast(stack.peek());
        } catch (EmptyStackException e) {
            writeline("/home/ubuntu/results/coverage/Digester/Digester_10_10.coverage", "ff53e4e5-96e3-437b-ba73-010587781691");
            log.warn("Empty stack (returning null)");
            writeline("/home/ubuntu/results/coverage/Digester/Digester_10_10.coverage", "b14eef2f-3d52-4eb1-a044-5b17d8aec486");
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
        writeline("/home/ubuntu/results/coverage/Digester/Digester_10_10.coverage", "394dd418-9aac-4feb-90da-9f84fa703730");
        int index = (stack.size() - 1) - n;
        writeline("/home/ubuntu/results/coverage/Digester/Digester_10_10.coverage", "3545a369-6909-4eab-a953-1cb3424cf3fa");
        if (index < 0) {
            writeline("/home/ubuntu/results/coverage/Digester/Digester_10_10.coverage", "8b29bf11-554a-4374-9e74-f878a4e978e7");
            log.warn("Empty stack (returning null)");
            writeline("/home/ubuntu/results/coverage/Digester/Digester_10_10.coverage", "27e8673f-1a8b-47c5-a259-30d45dbc67d2");
            return (null);
        }
        writeline("/home/ubuntu/results/coverage/Digester/Digester_10_10.coverage", "6b0dfe45-62e4-4dd2-b90e-d2f946f35f33");
        try {
            writeline("/home/ubuntu/results/coverage/Digester/Digester_10_10.coverage", "94c47366-339e-4559-a21c-81d08b0662e3");
            return this.<T>npeSafeCast(stack.get(index));
        } catch (EmptyStackException e) {
            writeline("/home/ubuntu/results/coverage/Digester/Digester_10_10.coverage", "9a991e3a-77dd-4369-a3d2-c9fd9d16cf70");
            log.warn("Empty stack (returning null)");
            writeline("/home/ubuntu/results/coverage/Digester/Digester_10_10.coverage", "5251c96b-0fb6-4b12-970e-c230d53e03ae");
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
        writeline("/home/ubuntu/results/coverage/Digester/Digester_10_10.coverage", "486455c9-4008-4dab-8831-7f8b9a6e2165");
        try {
            writeline("/home/ubuntu/results/coverage/Digester/Digester_10_10.coverage", "8de4d9bc-8414-4453-ba93-8b8a33535218");
            T popped = this.<T>npeSafeCast(stack.pop());
            writeline("/home/ubuntu/results/coverage/Digester/Digester_10_10.coverage", "c4c134d8-aafa-4786-8ac0-a0548da7a806");
            if (stackAction != null) {
                writeline("/home/ubuntu/results/coverage/Digester/Digester_10_10.coverage", "d2404921-a326-4372-b1a1-29baff20ab67");
                popped = stackAction.onPop(this, null, popped);
            }
            writeline("/home/ubuntu/results/coverage/Digester/Digester_10_10.coverage", "3303e8c7-2ba6-49c5-977a-b706fc8b7f2f");
            return popped;
        } catch (EmptyStackException e) {
            writeline("/home/ubuntu/results/coverage/Digester/Digester_10_10.coverage", "7b6f858d-2745-440a-a979-31267ff1fc49");
            log.warn("Empty stack (returning null)");
            writeline("/home/ubuntu/results/coverage/Digester/Digester_10_10.coverage", "56eb4821-f02a-4b7c-baa0-08930f6d7ae3");
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
        writeline("/home/ubuntu/results/coverage/Digester/Digester_10_10.coverage", "dc14a07f-5c59-4132-8b16-c045787a205a");
        if (stackAction != null) {
            writeline("/home/ubuntu/results/coverage/Digester/Digester_10_10.coverage", "cea5faa1-9826-4668-a176-f673da468ebc");
            object = stackAction.onPush(this, null, object);
        }
        writeline("/home/ubuntu/results/coverage/Digester/Digester_10_10.coverage", "98b60da9-ca01-4708-9a05-ac73abfc0769");
        if (stack.size() == 0) {
            writeline("/home/ubuntu/results/coverage/Digester/Digester_10_10.coverage", "c33340a3-b959-4eab-8f46-8dadfd97d39d");
            root = object;
        }
        writeline("/home/ubuntu/results/coverage/Digester/Digester_10_10.coverage", "7d9a75d2-6cd6-46fa-8995-7e13ae95f8f0");
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
        writeline("/home/ubuntu/results/coverage/Digester/Digester_10_10.coverage", "b3eeffc7-4bde-4246-8796-24dcf200b587");
        if (stackAction != null) {
            writeline("/home/ubuntu/results/coverage/Digester/Digester_10_10.coverage", "aa0bd051-3e37-4b50-849c-f78ded793d12");
            value = stackAction.onPush(this, stackName, value);
        }
        writeline("/home/ubuntu/results/coverage/Digester/Digester_10_10.coverage", "7caa5d7f-32e7-4650-a924-436ffbfc8113");
        Stack<Object> namedStack = stacksByName.get(stackName);
        writeline("/home/ubuntu/results/coverage/Digester/Digester_10_10.coverage", "35303fa1-aa77-4d2f-8469-2e7627d1c46c");
        if (namedStack == null) {
            writeline("/home/ubuntu/results/coverage/Digester/Digester_10_10.coverage", "ba8eabac-a4fc-4a7b-8402-b32f036915d6");
            namedStack = new Stack<Object>();
            writeline("/home/ubuntu/results/coverage/Digester/Digester_10_10.coverage", "d92fda75-6c94-4697-8454-4c884f75b7a5");
            stacksByName.put(stackName, namedStack);
        }
        writeline("/home/ubuntu/results/coverage/Digester/Digester_10_10.coverage", "6289bb45-361d-473f-8599-a86104dbfc3d");
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
        writeline("/home/ubuntu/results/coverage/Digester/Digester_10_10.coverage", "0fafbb91-b10b-4569-8577-d98a9453107e");
        T result = null;
        writeline("/home/ubuntu/results/coverage/Digester/Digester_10_10.coverage", "54852a7d-19b0-456f-8e0b-9e82e7732310");
        Stack<Object> namedStack = stacksByName.get(stackName);
        writeline("/home/ubuntu/results/coverage/Digester/Digester_10_10.coverage", "71a6b77e-e58a-4c2d-92e5-2dfe291cf39e");
        if (namedStack == null) {
            writeline("/home/ubuntu/results/coverage/Digester/Digester_10_10.coverage", "f9754d8e-e259-404f-b4da-9cd4f2e4d4cb");
            if (log.isDebugEnabled()) {
                writeline("/home/ubuntu/results/coverage/Digester/Digester_10_10.coverage", "af371c8b-cce4-4bad-8b94-b25373a077c4");
                log.debug("Stack '" + stackName + "' is empty");
            }
        }
        writeline("/home/ubuntu/results/coverage/Digester/Digester_10_10.coverage", "e6fa7426-1b29-48af-b879-00f82f045cb9");
        result = this.<T>npeSafeCast(namedStack.pop());
        writeline("/home/ubuntu/results/coverage/Digester/Digester_10_10.coverage", "e8f76860-63bd-4f9c-8baa-e9a8d1a0548b");
        if (stackAction != null) {
            writeline("/home/ubuntu/results/coverage/Digester/Digester_10_10.coverage", "334f1ac3-dd56-423c-91ee-0dd429d344c5");
            result = stackAction.onPop(this, stackName, result);
        }
        writeline("/home/ubuntu/results/coverage/Digester/Digester_10_10.coverage", "c0d32789-6b01-4fca-bf51-99f4ee0134b2");
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
        writeline("/home/ubuntu/results/coverage/Digester/Digester_10_10.coverage", "248ad6c8-5223-48ca-8e74-9d27362b38b3");
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
        writeline("/home/ubuntu/results/coverage/Digester/Digester_10_10.coverage", "0c054c1f-8ca9-46fd-9824-af89f59510cd");
        T result = null;
        writeline("/home/ubuntu/results/coverage/Digester/Digester_10_10.coverage", "597ce808-0514-44e9-a10d-b20c5b4b8e2a");
        Stack<Object> namedStack = stacksByName.get(stackName);
        writeline("/home/ubuntu/results/coverage/Digester/Digester_10_10.coverage", "f1537989-ee1a-4890-8dd7-adb0ce169a40");
        if (namedStack == null) {
            writeline("/home/ubuntu/results/coverage/Digester/Digester_10_10.coverage", "24523d0f-fd48-44be-a879-673db29c4e52");
            if (log.isDebugEnabled()) {
                writeline("/home/ubuntu/results/coverage/Digester/Digester_10_10.coverage", "53da440d-5838-453d-b88a-8d1952ed5ec7");
                log.debug("Stack '" + stackName + "' is empty");
            }
        }
        writeline("/home/ubuntu/results/coverage/Digester/Digester_10_10.coverage", "59b17877-3858-4625-ab19-e808cfb96399");
        int index = (namedStack.size() - 1) - n;
        writeline("/home/ubuntu/results/coverage/Digester/Digester_10_10.coverage", "44fbd047-5baf-4d14-9995-6dce785d20c2");
        if (index < 0) {
        }
        writeline("/home/ubuntu/results/coverage/Digester/Digester_10_10.coverage", "2ec2db64-6e85-4e02-879e-d7702b29499c");
        result = this.<T>npeSafeCast(namedStack.get(index));
        writeline("/home/ubuntu/results/coverage/Digester/Digester_10_10.coverage", "9de9f4bf-44dc-44a7-b820-c5f26f30ccd2");
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
        writeline("/home/ubuntu/results/coverage/Digester/Digester_10_10.coverage", "7fc7b647-50d3-47c8-aa49-7b82c1018b56");
        boolean result = true;
        writeline("/home/ubuntu/results/coverage/Digester/Digester_10_10.coverage", "ef645fa3-ea08-4c62-829a-9f57fd370288");
        Stack<Object> namedStack = stacksByName.get(stackName);
        writeline("/home/ubuntu/results/coverage/Digester/Digester_10_10.coverage", "d61aebef-6acc-4cbd-8dd3-0d45c3c5dd66");
        if (namedStack != null) {
            writeline("/home/ubuntu/results/coverage/Digester/Digester_10_10.coverage", "a1ed9829-c294-4c02-82a2-e345201473db");
            result = namedStack.isEmpty();
        }
        writeline("/home/ubuntu/results/coverage/Digester/Digester_10_10.coverage", "ea5ba0f1-dac0-461a-b81b-69b7fb080777");
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
        writeline("/home/ubuntu/results/coverage/Digester/Digester_10_10.coverage", "64aa0b33-d0e0-4c78-9f13-6fb77982aaef");
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
        writeline("/home/ubuntu/results/coverage/Digester/Digester_10_10.coverage", "028c6445-febf-4b4f-9002-653c60ae11c5");
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
        writeline("/home/ubuntu/results/coverage/Digester/Digester_10_10.coverage", "bdb8232a-d60c-4273-8c69-52ca0054f960");
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
        writeline("/home/ubuntu/results/coverage/Digester/Digester_10_10.coverage", "3ec974ca-fa39-4a84-9c29-2d26c94801da");
        if (configured) {
            writeline("/home/ubuntu/results/coverage/Digester/Digester_10_10.coverage", "d473fcd7-ce72-418d-87fb-7c519bd2d0f5");
            return;
        }
        writeline("/home/ubuntu/results/coverage/Digester/Digester_10_10.coverage", "4f2b0459-dee5-43a6-9dad-5a61c5b32ac8");
        initialize();
        writeline("/home/ubuntu/results/coverage/Digester/Digester_10_10.coverage", "dee76205-1995-4b5d-9604-974905742df8");
        configured = true;
    }

    /**
     * Checks the Digester instance has been configured.
     *
     * @return true, if the Digester instance has been configured, false otherwise
     * @since 3.0
     */
    public boolean isConfigured() {
        writeline("/home/ubuntu/results/coverage/Digester/Digester_10_10.coverage", "a3922fdd-29e4-43e2-a253-60712de4caee");
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
        writeline("/home/ubuntu/results/coverage/Digester/Digester_10_10.coverage", "7bbe9814-6cfe-464b-8611-b9931fbc2317");
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
        writeline("/home/ubuntu/results/coverage/Digester/Digester_10_10.coverage", "3fa6882c-3e6d-4792-90f2-141a22c53af7");
        try {
            writeline("/home/ubuntu/results/coverage/Digester/Digester_10_10.coverage", "b5f5232d-a275-43eb-8742-7e6ab1061b7d");
            return (params.peek());
        } catch (EmptyStackException e) {
            writeline("/home/ubuntu/results/coverage/Digester/Digester_10_10.coverage", "7801188c-7e97-4e4c-be98-1bf023868250");
            log.warn("Empty stack (returning null)");
            writeline("/home/ubuntu/results/coverage/Digester/Digester_10_10.coverage", "31f30520-3336-46cb-8979-937e10ba4168");
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
        writeline("/home/ubuntu/results/coverage/Digester/Digester_10_10.coverage", "0d1dbded-cc8f-44c5-a951-0a85c35d1653");
        int index = (params.size() - 1) - n;
        writeline("/home/ubuntu/results/coverage/Digester/Digester_10_10.coverage", "a08d3b1b-1a40-4a94-a66c-706df25009c7");
        if (index < 0) {
            writeline("/home/ubuntu/results/coverage/Digester/Digester_10_10.coverage", "4f101b6d-6b7f-44cc-a0e4-c13b8c2fdc41");
            log.warn("Empty stack (returning null)");
            writeline("/home/ubuntu/results/coverage/Digester/Digester_10_10.coverage", "c2b6a223-dd63-438d-8b58-23c2e677a1df");
            return (null);
        }
        writeline("/home/ubuntu/results/coverage/Digester/Digester_10_10.coverage", "5a372563-dd10-477e-9a09-7fcd57ad5312");
        try {
            writeline("/home/ubuntu/results/coverage/Digester/Digester_10_10.coverage", "83e35b64-9c47-4009-b379-fd70f94a426c");
            return (params.get(index));
        } catch (EmptyStackException e) {
            writeline("/home/ubuntu/results/coverage/Digester/Digester_10_10.coverage", "917b6921-d451-4ce7-aa2a-adfc3f7a873b");
            log.warn("Empty stack (returning null)");
            writeline("/home/ubuntu/results/coverage/Digester/Digester_10_10.coverage", "0f9d4123-749f-4b92-93c7-a5b4f3c103d6");
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
        writeline("/home/ubuntu/results/coverage/Digester/Digester_10_10.coverage", "c4d6a246-5070-4875-8b25-bc403d4d4c8e");
        try {
            writeline("/home/ubuntu/results/coverage/Digester/Digester_10_10.coverage", "1e1ad29b-7b18-48fe-bad3-0d58ba01e8f0");
            if (log.isTraceEnabled()) {
                writeline("/home/ubuntu/results/coverage/Digester/Digester_10_10.coverage", "d074b9be-2931-4813-a350-42f6127f7b1b");
                log.trace("Popping params");
            }
            writeline("/home/ubuntu/results/coverage/Digester/Digester_10_10.coverage", "8155ba1d-562b-461e-9378-ea85a9bfce0d");
            return (params.pop());
        } catch (EmptyStackException e) {
            writeline("/home/ubuntu/results/coverage/Digester/Digester_10_10.coverage", "6a2d420b-a61b-4a26-b8d5-25dfc95664b7");
            log.warn("Empty stack (returning null)");
            writeline("/home/ubuntu/results/coverage/Digester/Digester_10_10.coverage", "37e30ff7-06f6-4171-be02-a3d8e3c04584");
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
        writeline("/home/ubuntu/results/coverage/Digester/Digester_10_10.coverage", "3d133ea5-70e3-49ac-b1da-1359a1302930");
        if (log.isTraceEnabled()) {
            writeline("/home/ubuntu/results/coverage/Digester/Digester_10_10.coverage", "728a3055-7766-4f0c-be77-6a04add0cca0");
            log.trace("Pushing params");
        }
        writeline("/home/ubuntu/results/coverage/Digester/Digester_10_10.coverage", "646293b0-a56f-41ea-ad7a-dcff9a77c6bf");
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
        writeline("/home/ubuntu/results/coverage/Digester/Digester_10_10.coverage", "052fdf23-1be8-404b-a3c5-cb78b29b89cd");
        if ((e != null) && (e instanceof InvocationTargetException)) {
            writeline("/home/ubuntu/results/coverage/Digester/Digester_10_10.coverage", "b6092833-5be4-4b5c-a1a1-55378c9980e7");
            Throwable t = ((InvocationTargetException) e).getTargetException();
            writeline("/home/ubuntu/results/coverage/Digester/Digester_10_10.coverage", "cc5ed607-743f-42c9-8b59-fd4c58694a6b");
            if ((t != null) && (t instanceof Exception)) {
                writeline("/home/ubuntu/results/coverage/Digester/Digester_10_10.coverage", "ba5aa0c3-0950-4b74-b6d8-9cbc1746fdbf");
                e = (Exception) t;
            }
        }
        writeline("/home/ubuntu/results/coverage/Digester/Digester_10_10.coverage", "36f1529f-a805-41a9-ac9a-e5a282c477b6");
        if (locator != null) {
            writeline("/home/ubuntu/results/coverage/Digester/Digester_10_10.coverage", "e4c053d5-9251-477e-ae4d-e18c05487fb8");
            String error = "Error at line " + locator.getLineNumber() + " char " + locator.getColumnNumber() + ": " + message;
            writeline("/home/ubuntu/results/coverage/Digester/Digester_10_10.coverage", "51f6c91e-c905-44f9-840e-32b29e883b63");
            if (e != null) {
                writeline("/home/ubuntu/results/coverage/Digester/Digester_10_10.coverage", "7ce61765-fb30-4065-90b2-640132e045ed");
                return new SAXParseException(error, locator, e);
            }
            writeline("/home/ubuntu/results/coverage/Digester/Digester_10_10.coverage", "61d2d777-6784-4c7c-9c48-1efafada5180");
            return new SAXParseException(error, locator);
        }
        writeline("/home/ubuntu/results/coverage/Digester/Digester_10_10.coverage", "342bc31d-ec22-45b2-a0c5-dde9634ea364");
        log.error("No Locator!");
        writeline("/home/ubuntu/results/coverage/Digester/Digester_10_10.coverage", "a73f42a7-ceb8-4691-9049-952ce8e8025e");
        if (e != null) {
            writeline("/home/ubuntu/results/coverage/Digester/Digester_10_10.coverage", "d4d2d3aa-b8c5-4a1d-a1f8-3d1a9e5c759d");
            return new SAXException(message, e);
        }
        writeline("/home/ubuntu/results/coverage/Digester/Digester_10_10.coverage", "5272bd7a-2cff-4caf-8beb-4ebb3f79ef66");
        return new SAXException(message);
    }

    /**
     * Create a SAX exception which also understands about the location in the digester file where the exception occurs
     *
     * @param e the exception cause
     * @return the new SAX exception
     */
    public SAXException createSAXException(Exception e) {
        writeline("/home/ubuntu/results/coverage/Digester/Digester_10_10.coverage", "6bdab842-3938-4e91-bd9f-76cab5bba917");
        if (e instanceof InvocationTargetException) {
            writeline("/home/ubuntu/results/coverage/Digester/Digester_10_10.coverage", "fa6142cc-6b93-4a0a-83d4-aa72f59f5d1b");
            Throwable t = ((InvocationTargetException) e).getTargetException();
            writeline("/home/ubuntu/results/coverage/Digester/Digester_10_10.coverage", "880bbc21-0324-49b6-b715-79fe49d63f37");
            if ((t != null) && (t instanceof Exception)) {
                writeline("/home/ubuntu/results/coverage/Digester/Digester_10_10.coverage", "3690437b-cccd-4cb2-ad04-278de4291a34");
                e = (Exception) t;
            }
        }
        writeline("/home/ubuntu/results/coverage/Digester/Digester_10_10.coverage", "2baf5e82-14a4-4eae-b292-3139afebc87a");
        return createSAXException(e.getMessage(), e);
    }

    /**
     * Create a SAX exception which also understands about the location in the digester file where the exception occurs
     *
     * @param message the custom SAX exception message
     * @return the new SAX exception
     */
    public SAXException createSAXException(String message) {
        writeline("/home/ubuntu/results/coverage/Digester/Digester_10_10.coverage", "7179572b-806d-43b0-9527-ebd20724fc27");
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
        writeline("/home/ubuntu/results/coverage/Digester/Digester_10_10.coverage", "a960e0ba-15f7-40aa-85e0-d61270173a6c");
        if (obj == null) {
            writeline("/home/ubuntu/results/coverage/Digester/Digester_10_10.coverage", "b9ab8ccc-2300-41d4-881d-5d843ce2bcd1");
            return null;
        }
        writeline("/home/ubuntu/results/coverage/Digester/Digester_10_10.coverage", "1ae22e23-99af-4321-95cd-a67d03a7c479");
        @SuppressWarnings("unchecked") T result = (T) obj;
        writeline("/home/ubuntu/results/coverage/Digester/Digester_10_10.coverage", "2b40d431-01cb-489e-8f8c-216852e8ae29");
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
