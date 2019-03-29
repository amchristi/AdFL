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
        writeline("/home/ubuntu/results/coverage/Digester/Digester_9_10.coverage", "fdabe007-dcd3-41c6-990c-357fb6ae8375");
        Stack<String> nsStack = namespaces.get(prefix);
        writeline("/home/ubuntu/results/coverage/Digester/Digester_9_10.coverage", "fa8714d2-efe6-45bb-b5a6-a0c2d825e83a");
        if (nsStack == null) {
            writeline("/home/ubuntu/results/coverage/Digester/Digester_9_10.coverage", "29d8e850-ebc9-455a-bdd8-99f78c4521a9");
            return null;
        }
        writeline("/home/ubuntu/results/coverage/Digester/Digester_9_10.coverage", "707de8a2-0eef-41bb-b0a1-ecea2e098817");
        try {
            writeline("/home/ubuntu/results/coverage/Digester/Digester_9_10.coverage", "0968278d-ab6a-427e-b66d-12ba58adc909");
            return (nsStack.peek());
        } catch (EmptyStackException e) {
            writeline("/home/ubuntu/results/coverage/Digester/Digester_9_10.coverage", "e6ae8981-ba26-4b47-b84d-ee283b58bb8f");
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
        writeline("/home/ubuntu/results/coverage/Digester/Digester_9_10.coverage", "50f94188-c2a3-4568-9f57-d8730d49de31");
        if (this.classLoader != null) {
            writeline("/home/ubuntu/results/coverage/Digester/Digester_9_10.coverage", "d24c05d7-9f89-4f46-be14-b45091cfe33f");
            return (this.classLoader);
        }
        writeline("/home/ubuntu/results/coverage/Digester/Digester_9_10.coverage", "fb5569ab-da0b-42fc-86df-b8bb2b8232aa");
        if (this.useContextClassLoader) {
            writeline("/home/ubuntu/results/coverage/Digester/Digester_9_10.coverage", "7983e1cb-e2bc-4080-a283-642f984c87bc");
            ClassLoader classLoader = Thread.currentThread().getContextClassLoader();
            writeline("/home/ubuntu/results/coverage/Digester/Digester_9_10.coverage", "9929d043-050e-4694-9e97-00422d1a7448");
            if (classLoader != null) {
                writeline("/home/ubuntu/results/coverage/Digester/Digester_9_10.coverage", "ff78ed64-1ecf-48b8-8593-2ac531751877");
                return (classLoader);
            }
        }
        writeline("/home/ubuntu/results/coverage/Digester/Digester_9_10.coverage", "f11fa97a-5bf2-42fb-ab8a-72e5128ad3e9");
        return (this.getClass().getClassLoader());
    }

    /**
     * Set the class loader to be used for instantiating application objects when required.
     *
     * @param classLoader The new class loader to use, or <code>null</code> to revert to the standard rules
     */
    public void setClassLoader(ClassLoader classLoader) {
        writeline("/home/ubuntu/results/coverage/Digester/Digester_9_10.coverage", "aae1668d-6fdb-4685-bde3-715fd1f730a2");
        this.classLoader = classLoader;
    }

    /**
     * Return the current depth of the element stack.
     *
     * @return the current depth of the element stack.
     */
    public int getCount() {
        writeline("/home/ubuntu/results/coverage/Digester/Digester_9_10.coverage", "71652db8-bdd6-41ef-a777-2ca825598de9");
        return (stack.size());
    }

    /**
     * Return the name of the XML element that is currently being processed.
     *
     * @return the name of the XML element that is currently being processed.
     */
    public String getCurrentElementName() {
        writeline("/home/ubuntu/results/coverage/Digester/Digester_9_10.coverage", "849094aa-bb6b-43ea-8000-25e88453f134");
        String elementName = match;
        writeline("/home/ubuntu/results/coverage/Digester/Digester_9_10.coverage", "f6de3853-2868-4187-9806-9068ec352189");
        int lastSlash = elementName.lastIndexOf('/');
        writeline("/home/ubuntu/results/coverage/Digester/Digester_9_10.coverage", "ab716c3d-010d-4475-9f3d-e7e7d99bc60f");
        if (lastSlash >= 0) {
            writeline("/home/ubuntu/results/coverage/Digester/Digester_9_10.coverage", "c6674d09-368b-46df-987c-0bcf05e7e3b0");
            elementName = elementName.substring(lastSlash + 1);
        }
        writeline("/home/ubuntu/results/coverage/Digester/Digester_9_10.coverage", "74a681a1-31f7-4f8b-8d28-efb0beb85fbe");
        return (elementName);
    }

    /**
     * Return the error handler for this Digester.
     *
     * @return the error handler for this Digester.
     */
    public ErrorHandler getErrorHandler() {
        writeline("/home/ubuntu/results/coverage/Digester/Digester_9_10.coverage", "50b66b5a-1aa9-4cd5-94e5-a4bb1b27b25d");
        return (this.errorHandler);
    }

    /**
     * Set the error handler for this Digester.
     *
     * @param errorHandler The new error handler
     */
    public void setErrorHandler(ErrorHandler errorHandler) {
        writeline("/home/ubuntu/results/coverage/Digester/Digester_9_10.coverage", "5baa8d75-5360-4572-ba82-34282e37251b");
        this.errorHandler = errorHandler;
    }

    /**
     * Return the SAXParserFactory we will use, creating one if necessary.
     *
     * @return the SAXParserFactory we will use, creating one if necessary.
     */
    public SAXParserFactory getFactory() {
        writeline("/home/ubuntu/results/coverage/Digester/Digester_9_10.coverage", "f1ace00c-2c95-422a-8bac-bdbcfdae70c4");
        if (factory == null) {
            writeline("/home/ubuntu/results/coverage/Digester/Digester_9_10.coverage", "a8e4ef50-5a84-48d2-ba84-a6cd14c1b040");
            factory = SAXParserFactory.newInstance();
            writeline("/home/ubuntu/results/coverage/Digester/Digester_9_10.coverage", "eae078c0-5f25-4c0f-9588-bd6a5423bf11");
            factory.setNamespaceAware(namespaceAware);
            writeline("/home/ubuntu/results/coverage/Digester/Digester_9_10.coverage", "132874a3-5dc1-48c5-8b3d-5d764e324c10");
            factory.setXIncludeAware(xincludeAware);
            writeline("/home/ubuntu/results/coverage/Digester/Digester_9_10.coverage", "bb0715aa-7bc3-4005-b6b9-6ebc2d32a1d1");
            factory.setValidating(validating);
            writeline("/home/ubuntu/results/coverage/Digester/Digester_9_10.coverage", "7d2b02d2-4542-42d2-8566-eaecfdfec2e6");
            factory.setSchema(schema);
        }
        writeline("/home/ubuntu/results/coverage/Digester/Digester_9_10.coverage", "521bdfa0-cb8c-4b33-8632-352b0cd6390c");
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
        writeline("/home/ubuntu/results/coverage/Digester/Digester_9_10.coverage", "78eea74f-a214-4f0f-bec0-9a8a863f9a2c");
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
        writeline("/home/ubuntu/results/coverage/Digester/Digester_9_10.coverage", "5097f9ea-2872-4bb6-82d4-b08ed08e2ef9");
        getFactory().setFeature(feature, value);
    }

    /**
     * Return the current Logger associated with this instance of the Digester
     *
     * @return the current Logger associated with this instance of the Digester
     */
    public Log getLogger() {
        writeline("/home/ubuntu/results/coverage/Digester/Digester_9_10.coverage", "1539ddcd-f52c-44c3-8fe6-a56778e9e7b3");
        return log;
    }

    /**
     * Set the current logger for this Digester.
     *
     * @param log the current logger for this Digester.
     */
    public void setLogger(Log log) {
        writeline("/home/ubuntu/results/coverage/Digester/Digester_9_10.coverage", "097e8e64-7d7e-4602-9462-712015df8a95");
        this.log = log;
    }

    /**
     * Gets the logger used for logging SAX-related information. <strong>Note</strong> the output is finely grained.
     *
     * @return the logger used for logging SAX-related information
     * @since 1.6
     */
    public Log getSAXLogger() {
        writeline("/home/ubuntu/results/coverage/Digester/Digester_9_10.coverage", "0243fcd9-58db-46af-bfed-4d0f083516b7");
        return saxLog;
    }

    /**
     * Sets the logger used for logging SAX-related information. <strong>Note</strong> the output is finely grained.
     *
     * @param saxLog the logger used for logging SAX-related information, not null
     * @since 1.6
     */
    public void setSAXLogger(Log saxLog) {
        writeline("/home/ubuntu/results/coverage/Digester/Digester_9_10.coverage", "64a1d7ed-922a-4d24-8196-14a47e8950e5");
        this.saxLog = saxLog;
    }

    /**
     * Return the current rule match path
     *
     * @return the current rule match path
     */
    public String getMatch() {
        writeline("/home/ubuntu/results/coverage/Digester/Digester_9_10.coverage", "73efaab7-1510-4e6c-a6f2-636d654e1584");
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
        writeline("/home/ubuntu/results/coverage/Digester/Digester_9_10.coverage", "f3f99dba-24ca-46db-b0d8-7a6d6b2c931d");
        return matches;
    }

    /**
     * Return the "namespace aware" flag for parsers we create.
     *
     * @return the "namespace aware" flag for parsers we create.
     */
    public boolean getNamespaceAware() {
        writeline("/home/ubuntu/results/coverage/Digester/Digester_9_10.coverage", "64ee9221-ac4f-4b6d-9cee-d98c67443615");
        return (this.namespaceAware);
    }

    /**
     * Set the "namespace aware" flag for parsers we create.
     *
     * @param namespaceAware The new "namespace aware" flag
     */
    public void setNamespaceAware(boolean namespaceAware) {
        writeline("/home/ubuntu/results/coverage/Digester/Digester_9_10.coverage", "33e17a75-ee85-4a56-9f79-a7c1115dbb08");
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
        writeline("/home/ubuntu/results/coverage/Digester/Digester_9_10.coverage", "e9176235-dfd8-41a7-9e56-9194c541b87c");
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
        writeline("/home/ubuntu/results/coverage/Digester/Digester_9_10.coverage", "8500b51e-743c-4f57-bbb6-86cd66f7cf9a");
        this.xincludeAware = xincludeAware;
    }

    /**
     * Set the public id of the current file being parse.
     *
     * @param publicId the DTD/Schema public's id.
     */
    public void setPublicId(String publicId) {
        writeline("/home/ubuntu/results/coverage/Digester/Digester_9_10.coverage", "a404ec67-58a0-4bd2-87bb-92e855d66e30");
        this.publicId = publicId;
    }

    /**
     * Return the public identifier of the DTD we are currently parsing under, if any.
     *
     * @return the public identifier of the DTD we are currently parsing under, if any.
     */
    public String getPublicId() {
        writeline("/home/ubuntu/results/coverage/Digester/Digester_9_10.coverage", "5329e3ae-bcec-4dc6-903f-631d75414538");
        return (this.publicId);
    }

    /**
     * Return the namespace URI that will be applied to all subsequently added <code>Rule</code> objects.
     *
     * @return the namespace URI that will be applied to all subsequently added <code>Rule</code> objects.
     */
    public String getRuleNamespaceURI() {
        writeline("/home/ubuntu/results/coverage/Digester/Digester_9_10.coverage", "6c9202e5-0fdf-44ec-8be3-0028e742dd6f");
        return (getRules().getNamespaceURI());
    }

    /**
     * Set the namespace URI that will be applied to all subsequently added <code>Rule</code> objects.
     *
     * @param ruleNamespaceURI Namespace URI that must match on all subsequently added rules, or <code>null</code> for
     *            matching regardless of the current namespace URI
     */
    public void setRuleNamespaceURI(String ruleNamespaceURI) {
        writeline("/home/ubuntu/results/coverage/Digester/Digester_9_10.coverage", "e4c89dd3-e807-4bb4-8ddc-69de989e42be");
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
        writeline("/home/ubuntu/results/coverage/Digester/Digester_9_10.coverage", "687b830d-b53f-49b5-80a5-50cfa6c4e7f8");
        if (parser != null) {
            writeline("/home/ubuntu/results/coverage/Digester/Digester_9_10.coverage", "74ba8712-baa9-4205-b2fd-7320f1805955");
            return (parser);
        }
        writeline("/home/ubuntu/results/coverage/Digester/Digester_9_10.coverage", "3553074a-21c2-4be1-a819-1db609983ecc");
        try {
            writeline("/home/ubuntu/results/coverage/Digester/Digester_9_10.coverage", "060d4777-7167-4574-ada4-fffd8d42115d");
            parser = getFactory().newSAXParser();
        } catch (Exception e) {
            writeline("/home/ubuntu/results/coverage/Digester/Digester_9_10.coverage", "7fb893a2-8a09-4026-ace6-3ea4a98d13ca");
            log.error("Digester.getParser: ", e);
            writeline("/home/ubuntu/results/coverage/Digester/Digester_9_10.coverage", "cebe17f6-ba6b-484c-9554-2e5eb956f960");
            return (null);
        }
        writeline("/home/ubuntu/results/coverage/Digester/Digester_9_10.coverage", "ef20e877-ee5a-4034-a434-9617c377ee93");
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
        writeline("/home/ubuntu/results/coverage/Digester/Digester_9_10.coverage", "fb435c9e-95c6-44d6-93a4-21e55844b971");
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
        writeline("/home/ubuntu/results/coverage/Digester/Digester_9_10.coverage", "27f57bea-136f-4299-8a36-de5e751de705");
        getParser().setProperty(property, value);
    }

    /**
     * Return the <code>Rules</code> implementation object containing our rules collection and associated matching
     * policy. If none has been established, a default implementation will be created and returned.
     *
     * @return the <code>Rules</code> implementation object.
     */
    public Rules getRules() {
        writeline("/home/ubuntu/results/coverage/Digester/Digester_9_10.coverage", "1778ea06-1fc1-4f0b-a397-8c6d7539cb25");
        if (this.rules == null) {
            writeline("/home/ubuntu/results/coverage/Digester/Digester_9_10.coverage", "52a8efce-5006-4e8d-ae79-e1ca8b7cca2d");
            this.rules = new RulesBase();
            writeline("/home/ubuntu/results/coverage/Digester/Digester_9_10.coverage", "c6249324-4a68-4fb2-ac2f-31aa55790373");
            this.rules.setDigester(this);
        }
        writeline("/home/ubuntu/results/coverage/Digester/Digester_9_10.coverage", "7ec63b15-1883-45e8-8873-f2f0dda82329");
        return (this.rules);
    }

    /**
     * Set the <code>Rules</code> implementation object containing our rules collection and associated matching policy.
     *
     * @param rules New Rules implementation
     */
    public void setRules(Rules rules) {
        writeline("/home/ubuntu/results/coverage/Digester/Digester_9_10.coverage", "d7e90473-5eed-4211-914e-43ab7240eb9e");
        this.rules = rules;
        writeline("/home/ubuntu/results/coverage/Digester/Digester_9_10.coverage", "3132d70d-62d0-4fea-9859-d3f89be6521b");
        this.rules.setDigester(this);
    }

    /**
     * Return the XML Schema used when parsing.
     *
     * @return The {@link Schema} instance in use.
     * @since 2.0
     */
    public Schema getXMLSchema() {
        writeline("/home/ubuntu/results/coverage/Digester/Digester_9_10.coverage", "c27e3bd9-90ca-4b0b-9806-3a9e58475416");
        return (this.schema);
    }

    /**
     * Set the XML Schema to be used when parsing.
     *
     * @param schema The {@link Schema} instance to use.
     * @since 2.0
     */
    public void setXMLSchema(Schema schema) {
        writeline("/home/ubuntu/results/coverage/Digester/Digester_9_10.coverage", "0e343794-edd5-4eba-8cee-b49535fc32d3");
        this.schema = schema;
    }

    /**
     * Return the boolean as to whether the context ClassLoader should be used.
     *
     * @return true, if the context ClassLoader should be used, false otherwise.
     */
    public boolean getUseContextClassLoader() {
        writeline("/home/ubuntu/results/coverage/Digester/Digester_9_10.coverage", "684db385-5a57-40f2-b4a6-f05c8932430e");
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
        writeline("/home/ubuntu/results/coverage/Digester/Digester_9_10.coverage", "38ac61bb-ffbd-4aa0-b6bd-1c9f2da976ed");
        useContextClassLoader = use;
    }

    /**
     * Return the validating parser flag.
     *
     * @return the validating parser flag.
     */
    public boolean getValidating() {
        writeline("/home/ubuntu/results/coverage/Digester/Digester_9_10.coverage", "c3152561-ce8e-44f5-a1c8-b671541da358");
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
        writeline("/home/ubuntu/results/coverage/Digester/Digester_9_10.coverage", "7b79f090-2758-4d52-8851-1d9c20f9d396");
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
        writeline("/home/ubuntu/results/coverage/Digester/Digester_9_10.coverage", "c319fc6f-9937-4d2b-a6fa-b32df27af93e");
        if (reader == null) {
            writeline("/home/ubuntu/results/coverage/Digester/Digester_9_10.coverage", "e820e6e3-eaa7-44fa-80f7-fe47d1cc0d66");
            reader = getParser().getXMLReader();
        }
        writeline("/home/ubuntu/results/coverage/Digester/Digester_9_10.coverage", "18e90351-3796-4a6e-85a7-e325bf8137f0");
        reader.setDTDHandler(this);
        writeline("/home/ubuntu/results/coverage/Digester/Digester_9_10.coverage", "b54ed292-caa6-4dd0-89a0-353f57352be1");
        reader.setContentHandler(this);
        writeline("/home/ubuntu/results/coverage/Digester/Digester_9_10.coverage", "f1f61e3e-7df4-4ecf-84a6-bc9d243620c1");
        if (entityResolver == null) {
            writeline("/home/ubuntu/results/coverage/Digester/Digester_9_10.coverage", "cc22a9bf-439c-4228-84fb-4d9e935bcd9b");
            reader.setEntityResolver(this);
        } else {
            writeline("/home/ubuntu/results/coverage/Digester/Digester_9_10.coverage", "055e6252-543f-4a62-9316-86f3f7327038");
            reader.setEntityResolver(entityResolver);
        }
        writeline("/home/ubuntu/results/coverage/Digester/Digester_9_10.coverage", "2b4e2460-1c38-446f-b53c-a480cfe9b63c");
        if (this.errorHandler != null) {
            writeline("/home/ubuntu/results/coverage/Digester/Digester_9_10.coverage", "81fb427a-1422-4918-8a28-cb10c8cac0fe");
            reader.setErrorHandler(this.errorHandler);
        } else {
            writeline("/home/ubuntu/results/coverage/Digester/Digester_9_10.coverage", "78031a8e-48e6-4e6e-80d8-88702b8a3ef2");
            reader.setErrorHandler(this);
        }
        writeline("/home/ubuntu/results/coverage/Digester/Digester_9_10.coverage", "9f8800cc-dc0e-4236-96e3-8fe2334c6c15");
        return reader;
    }

    /**
     * Gets the <code>Substitutor</code> used to convert attributes and body text.
     *
     * @return the <code>Substitutor</code> used to convert attributes and body text,
     *         null if not substitutions are to be performed.
     */
    public Substitutor getSubstitutor() {
        writeline("/home/ubuntu/results/coverage/Digester/Digester_9_10.coverage", "0c6c5a48-d3c3-408a-8040-fd2152de3252");
        return substitutor;
    }

    /**
     * Sets the <code>Substitutor</code> to be used to convert attributes and body text.
     *
     * @param substitutor the Substitutor to be used to convert attributes and body text or null if not substitution of
     *            these values is to be performed.
     */
    public void setSubstitutor(Substitutor substitutor) {
        writeline("/home/ubuntu/results/coverage/Digester/Digester_9_10.coverage", "9b76ee9d-8de2-4c9f-9d0c-cc30c2bd9949");
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
        writeline("/home/ubuntu/results/coverage/Digester/Digester_9_10.coverage", "662211cf-883d-41fe-ab66-d6bbdc264102");
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
        writeline("/home/ubuntu/results/coverage/Digester/Digester_9_10.coverage", "a60ffccd-84a9-4a7f-a834-fa0e2b9e7374");
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
        writeline("/home/ubuntu/results/coverage/Digester/Digester_9_10.coverage", "e0e335c6-709f-4873-a1d7-ce99a7a0afde");
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
        writeline("/home/ubuntu/results/coverage/Digester/Digester_9_10.coverage", "55f4a29a-7172-4bfc-b84f-39f5c616b143");
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
        writeline("/home/ubuntu/results/coverage/Digester/Digester_9_10.coverage", "34bedec9-c177-41e9-a6c3-934f8208dfec");
        if (!namespaceAware) {
            writeline("/home/ubuntu/results/coverage/Digester/Digester_9_10.coverage", "ffe9eac0-d504-42ff-bc20-0e9c416893c9");
            log.warn("Digester is not namespace aware");
        }
        writeline("/home/ubuntu/results/coverage/Digester/Digester_9_10.coverage", "66b52f5e-dded-4d84-a028-96a952ee57d4");
        Map<String, String> currentNamespaces = new HashMap<String, String>();
        writeline("/home/ubuntu/results/coverage/Digester/Digester_9_10.coverage", "fb5d41a9-33d5-4e23-a2db-87721f455bdc");
        return currentNamespaces;
    }

    /**
     * Returns the executor service used to run asynchronous parse method.
     *
     * @return the executor service used to run asynchronous parse method
     * @since 3.1
     */
    public ExecutorService getExecutorService() {
        writeline("/home/ubuntu/results/coverage/Digester/Digester_9_10.coverage", "658fd8cc-148e-4afe-a855-3c535c63f613");
        return executorService;
    }

    /**
     * Sets the executor service to run asynchronous parse method.
     *
     * @param executorService the executor service to run asynchronous parse method
     * @since 3.1
     */
    public void setExecutorService(ExecutorService executorService) {
        writeline("/home/ubuntu/results/coverage/Digester/Digester_9_10.coverage", "f7608c83-6537-4526-8ae7-d18f80aea10f");
        this.executorService = executorService;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public void characters(char buffer[], int start, int length) throws SAXException {
        writeline("/home/ubuntu/results/coverage/Digester/Digester_9_10.coverage", "1251b81c-8ac5-4ae9-9c7a-10a488f0dc06");
        if (customContentHandler != null) {
            writeline("/home/ubuntu/results/coverage/Digester/Digester_9_10.coverage", "eb2520a5-2f17-467b-9b6b-78061d130b02");
            customContentHandler.characters(buffer, start, length);
            writeline("/home/ubuntu/results/coverage/Digester/Digester_9_10.coverage", "ca224ae2-86ed-43ae-ab39-0ed6ddd79e7e");
            return;
        }
        writeline("/home/ubuntu/results/coverage/Digester/Digester_9_10.coverage", "ceb72db8-9a22-4ee8-ab57-bee3b4e801bd");
        if (saxLog.isDebugEnabled()) {
            writeline("/home/ubuntu/results/coverage/Digester/Digester_9_10.coverage", "63048b17-151c-41cd-9bcb-a3dccbc9dac4");
            saxLog.debug("characters(" + new String(buffer, start, length) + ")");
        }
        writeline("/home/ubuntu/results/coverage/Digester/Digester_9_10.coverage", "2d79f8ef-894d-46c0-be8c-77aeb91580f2");
        bodyText.append(buffer, start, length);
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public void endDocument() throws SAXException {
        writeline("/home/ubuntu/results/coverage/Digester/Digester_9_10.coverage", "a411649f-4b16-49c9-ad58-3055b0fcedac");
        if (saxLog.isDebugEnabled()) {
            writeline("/home/ubuntu/results/coverage/Digester/Digester_9_10.coverage", "a20e8b99-1aac-497a-af8b-1d66d89fa27b");
            if (getCount() > 1) {
                writeline("/home/ubuntu/results/coverage/Digester/Digester_9_10.coverage", "fef04bee-d972-4fce-b284-09891b4a9814");
                saxLog.debug("endDocument():  " + getCount() + " elements left");
            } else {
                writeline("/home/ubuntu/results/coverage/Digester/Digester_9_10.coverage", "5aa70bb2-cee2-4c74-87f2-e8039e984431");
                saxLog.debug("endDocument()");
            }
        }
        writeline("/home/ubuntu/results/coverage/Digester/Digester_9_10.coverage", "6c72a49c-8190-4649-88c2-f82044e0c2c8");
        clear();
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public void endElement(String namespaceURI, String localName, String qName) throws SAXException {
        writeline("/home/ubuntu/results/coverage/Digester/Digester_9_10.coverage", "b02fd5b5-9111-4d0b-a79f-cd369c7804ec");
        if (customContentHandler != null) {
            writeline("/home/ubuntu/results/coverage/Digester/Digester_9_10.coverage", "a09d6d4e-95ec-4360-be6f-d4581f279b8f");
            customContentHandler.endElement(namespaceURI, localName, qName);
            writeline("/home/ubuntu/results/coverage/Digester/Digester_9_10.coverage", "1d879c44-ff26-4012-982f-e7043243a896");
            return;
        }
        writeline("/home/ubuntu/results/coverage/Digester/Digester_9_10.coverage", "da652a86-f467-47af-87b1-64e0563dcaaa");
        boolean debug = log.isDebugEnabled();
        writeline("/home/ubuntu/results/coverage/Digester/Digester_9_10.coverage", "cfbd55a9-eaff-4bff-86f8-d35f94737528");
        if (debug) {
            writeline("/home/ubuntu/results/coverage/Digester/Digester_9_10.coverage", "3fdc566f-203b-4883-b73d-23b388a10b5e");
            if (saxLog.isDebugEnabled()) {
                writeline("/home/ubuntu/results/coverage/Digester/Digester_9_10.coverage", "facfd4a3-3422-4ae0-867e-ad8c05e3c621");
                saxLog.debug("endElement(" + namespaceURI + "," + localName + "," + qName + ")");
            }
            writeline("/home/ubuntu/results/coverage/Digester/Digester_9_10.coverage", "6fa92496-bf14-424d-b063-b27cc54e530a");
            log.debug("  match='" + match + "'");
            writeline("/home/ubuntu/results/coverage/Digester/Digester_9_10.coverage", "b8503370-3248-4f72-8729-14bb5320a3bb");
            log.debug("  bodyText='" + bodyText + "'");
        }
        writeline("/home/ubuntu/results/coverage/Digester/Digester_9_10.coverage", "df279635-f575-485a-8ae5-307555eac40a");
        String name = localName;
        writeline("/home/ubuntu/results/coverage/Digester/Digester_9_10.coverage", "da388d34-8d4a-4d36-b0c0-3665ee7821eb");
        if ((name == null) || (name.length() < 1)) {
            writeline("/home/ubuntu/results/coverage/Digester/Digester_9_10.coverage", "0bf1aca9-0b4f-4018-aa76-6be1ce3562c6");
            name = qName;
        }
        writeline("/home/ubuntu/results/coverage/Digester/Digester_9_10.coverage", "0c93ee13-cbcb-4caf-ad16-1335d930902e");
        List<Rule> rules = matches.pop();
        writeline("/home/ubuntu/results/coverage/Digester/Digester_9_10.coverage", "be86b446-bcb4-484b-b3e0-477d50596fbb");
        if ((rules != null) && (rules.size() > 0)) {
            writeline("/home/ubuntu/results/coverage/Digester/Digester_9_10.coverage", "3c40abf3-3b98-4f64-ad95-dc9d8d8421b1");
            String bodyText = this.bodyText.toString();
            writeline("/home/ubuntu/results/coverage/Digester/Digester_9_10.coverage", "a06d3203-937f-407d-ade1-dabcfdc796a6");
            Substitutor substitutor = getSubstitutor();
            writeline("/home/ubuntu/results/coverage/Digester/Digester_9_10.coverage", "62b9b858-5f41-4351-b5e2-8e82b8d7283d");
            if (substitutor != null) {
                writeline("/home/ubuntu/results/coverage/Digester/Digester_9_10.coverage", "ff24b704-7b50-480d-9d4c-a5b560975c54");
                bodyText = substitutor.substitute(bodyText);
            }
            writeline("/home/ubuntu/results/coverage/Digester/Digester_9_10.coverage", "e819c0bd-7416-4d1b-8312-924bad5be16d");
            for (int i = 0; i < rules.size(); i++) {
                writeline("/home/ubuntu/results/coverage/Digester/Digester_9_10.coverage", "3b4a5957-d516-4ed5-8745-d38bd4cdcaf2");
                try {
                    writeline("/home/ubuntu/results/coverage/Digester/Digester_9_10.coverage", "a8dbcd6e-39a6-4744-990a-47e66b0c775c");
                    Rule rule = rules.get(i);
                    writeline("/home/ubuntu/results/coverage/Digester/Digester_9_10.coverage", "cb0d5754-fa9c-4d38-b8b4-202497278093");
                    if (debug) {
                        writeline("/home/ubuntu/results/coverage/Digester/Digester_9_10.coverage", "aceb6dd9-596e-4d22-a90d-d7c3583d3e97");
                        log.debug("  Fire body() for " + rule);
                    }
                    writeline("/home/ubuntu/results/coverage/Digester/Digester_9_10.coverage", "8a5740f4-e058-4c0b-8dbc-160f670f148b");
                    rule.body(namespaceURI, name, bodyText);
                } catch (Exception e) {
                    writeline("/home/ubuntu/results/coverage/Digester/Digester_9_10.coverage", "ad5873b6-b8b4-43df-a105-998e6956cf1a");
                    log.error("Body event threw exception", e);
                } catch (Error e) {
                    writeline("/home/ubuntu/results/coverage/Digester/Digester_9_10.coverage", "91dce83d-7a96-427d-ac4c-82007e71bc16");
                    log.error("Body event threw error", e);
                }
            }
        } else {
            writeline("/home/ubuntu/results/coverage/Digester/Digester_9_10.coverage", "c785a841-5987-4b2f-9bb3-6a4e178655f7");
            if (debug) {
                writeline("/home/ubuntu/results/coverage/Digester/Digester_9_10.coverage", "8ab6c61f-aee2-45e6-8582-a97a7126769c");
                log.debug("  No rules found matching '" + match + "'.");
            }
        }
        writeline("/home/ubuntu/results/coverage/Digester/Digester_9_10.coverage", "7d7c217f-ed06-44e0-aecb-2148fff7fe7e");
        bodyText = bodyTexts.pop();
        writeline("/home/ubuntu/results/coverage/Digester/Digester_9_10.coverage", "028a3f57-dd83-42b6-9a67-ce730285264a");
        if (debug) {
            writeline("/home/ubuntu/results/coverage/Digester/Digester_9_10.coverage", "0cc79fc5-8340-43b9-bd44-c9174000a353");
            log.debug("  Popping body text '" + bodyText.toString() + "'");
        }
        writeline("/home/ubuntu/results/coverage/Digester/Digester_9_10.coverage", "0f49196a-96dd-4a4f-a841-2443cf0b5e79");
        if (rules != null) {
            writeline("/home/ubuntu/results/coverage/Digester/Digester_9_10.coverage", "2f5f59f8-f4c5-4073-9292-6899949f2147");
            for (int i = 0; i < rules.size(); i++) {
                writeline("/home/ubuntu/results/coverage/Digester/Digester_9_10.coverage", "c9da6b5e-c60d-429f-9d34-c528b1f0c415");
                int j = (rules.size() - i) - 1;
                writeline("/home/ubuntu/results/coverage/Digester/Digester_9_10.coverage", "2bd34589-80cd-45b8-8cfa-92444b2bae21");
                try {
                    writeline("/home/ubuntu/results/coverage/Digester/Digester_9_10.coverage", "f4f25b4a-4510-4ff4-be4d-a0277c80faca");
                    Rule rule = rules.get(j);
                    writeline("/home/ubuntu/results/coverage/Digester/Digester_9_10.coverage", "3991946e-f0af-47b2-8ebd-70d8cadae531");
                    if (debug) {
                        writeline("/home/ubuntu/results/coverage/Digester/Digester_9_10.coverage", "e8160cbf-045f-4e8a-ace1-b6657e1a036c");
                        log.debug("  Fire end() for " + rule);
                    }
                    writeline("/home/ubuntu/results/coverage/Digester/Digester_9_10.coverage", "5356b019-13ca-4f75-872a-4946ec4311bc");
                    rule.end(namespaceURI, name);
                } catch (Exception e) {
                    writeline("/home/ubuntu/results/coverage/Digester/Digester_9_10.coverage", "b2e8d0d5-7eda-460b-b001-b68de12ead58");
                    log.error("End event threw exception", e);
                } catch (Error e) {
                    writeline("/home/ubuntu/results/coverage/Digester/Digester_9_10.coverage", "86683930-7449-47eb-9afb-05017e6db247");
                    log.error("End event threw error", e);
                }
            }
        }
        writeline("/home/ubuntu/results/coverage/Digester/Digester_9_10.coverage", "e535d82d-babb-40c1-8fb8-7c682641d22e");
        int slash = match.lastIndexOf('/');
        writeline("/home/ubuntu/results/coverage/Digester/Digester_9_10.coverage", "1a4ba710-8531-4899-8d37-617fa49e619a");
        if (slash >= 0) {
            writeline("/home/ubuntu/results/coverage/Digester/Digester_9_10.coverage", "e932ffe4-ac40-41ec-b4ff-41c7cbc761a1");
            match = match.substring(0, slash);
        } else {
            writeline("/home/ubuntu/results/coverage/Digester/Digester_9_10.coverage", "3a6b9f2f-1b1e-444e-8349-91b746e188e5");
            match = "";
        }
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public void endPrefixMapping(String prefix) throws SAXException {
        writeline("/home/ubuntu/results/coverage/Digester/Digester_9_10.coverage", "a8ff536d-9ca3-499d-9c31-b4cd85ed1217");
        if (saxLog.isDebugEnabled()) {
            writeline("/home/ubuntu/results/coverage/Digester/Digester_9_10.coverage", "4b8e8061-1c27-4182-9692-44a4e84325a8");
            saxLog.debug("endPrefixMapping(" + prefix + ")");
        }
        writeline("/home/ubuntu/results/coverage/Digester/Digester_9_10.coverage", "4519b49f-ab56-45c2-9e8a-af0b471d2473");
        Stack<String> stack = namespaces.get(prefix);
        writeline("/home/ubuntu/results/coverage/Digester/Digester_9_10.coverage", "9d1187f1-8b33-4bc0-8a92-f452f6148560");
        if (stack == null) {
            writeline("/home/ubuntu/results/coverage/Digester/Digester_9_10.coverage", "fad0345b-982c-4208-96d2-8a5454fb73a5");
            return;
        }
        writeline("/home/ubuntu/results/coverage/Digester/Digester_9_10.coverage", "08a8e69b-dd09-447d-9b0c-2c25d96f2de0");
        try {
            writeline("/home/ubuntu/results/coverage/Digester/Digester_9_10.coverage", "9d6a832a-f258-4825-8008-82074d4951ff");
            stack.pop();
            writeline("/home/ubuntu/results/coverage/Digester/Digester_9_10.coverage", "f0545e2a-03ad-48b1-acec-85b232c62557");
            if (stack.empty()) {
                writeline("/home/ubuntu/results/coverage/Digester/Digester_9_10.coverage", "413e321f-0995-468d-95e0-fed673959d0e");
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
        writeline("/home/ubuntu/results/coverage/Digester/Digester_9_10.coverage", "1f2b0edb-ce72-4844-8032-af3459209a57");
        if (saxLog.isDebugEnabled()) {
            writeline("/home/ubuntu/results/coverage/Digester/Digester_9_10.coverage", "f8985df3-49c1-4c22-8cb6-137bb2733b00");
            saxLog.debug("ignorableWhitespace(" + new String(buffer, start, len) + ")");
        }
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public void processingInstruction(String target, String data) throws SAXException {
        writeline("/home/ubuntu/results/coverage/Digester/Digester_9_10.coverage", "34c7543a-4b15-431c-be4d-b1e52b46b1ac");
        if (customContentHandler != null) {
            writeline("/home/ubuntu/results/coverage/Digester/Digester_9_10.coverage", "27f320dc-aaab-4576-86a6-fc2487ad77d7");
            customContentHandler.processingInstruction(target, data);
            writeline("/home/ubuntu/results/coverage/Digester/Digester_9_10.coverage", "6cf7feeb-5d9e-48a0-834d-01fafdb676bc");
            return;
        }
        writeline("/home/ubuntu/results/coverage/Digester/Digester_9_10.coverage", "706d8986-1eaa-40d1-bf19-1f1d817d86bc");
        if (saxLog.isDebugEnabled()) {
            writeline("/home/ubuntu/results/coverage/Digester/Digester_9_10.coverage", "a573f87b-9919-420f-b8a7-21b01e37271a");
            saxLog.debug("processingInstruction('" + target + "','" + data + "')");
        }
    }

    /**
     * Gets the document locator associated with our parser.
     *
     * @return the Locator supplied by the document parser
     */
    public Locator getDocumentLocator() {
        writeline("/home/ubuntu/results/coverage/Digester/Digester_9_10.coverage", "068a4f6b-e301-4e0c-b305-3c53a5e24f4b");
        return locator;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public void setDocumentLocator(Locator locator) {
        writeline("/home/ubuntu/results/coverage/Digester/Digester_9_10.coverage", "ea96ae63-1b0e-4c85-9041-1afe388ac977");
        if (saxLog.isDebugEnabled()) {
            writeline("/home/ubuntu/results/coverage/Digester/Digester_9_10.coverage", "216d2b7b-8aad-405b-97e4-ed7870c3c7fc");
            saxLog.debug("setDocumentLocator(" + locator + ")");
        }
        writeline("/home/ubuntu/results/coverage/Digester/Digester_9_10.coverage", "0b64fe57-618e-44b3-9354-174c33085306");
        this.locator = locator;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public void skippedEntity(String name) throws SAXException {
        writeline("/home/ubuntu/results/coverage/Digester/Digester_9_10.coverage", "b740de62-6d4b-452a-86be-1339bb87aaf0");
        if (saxLog.isDebugEnabled()) {
            writeline("/home/ubuntu/results/coverage/Digester/Digester_9_10.coverage", "c7c3042a-7bed-4028-b5f3-f6d30cd2c4f4");
            saxLog.debug("skippedEntity(" + name + ")");
        }
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public void startDocument() throws SAXException {
        writeline("/home/ubuntu/results/coverage/Digester/Digester_9_10.coverage", "393abb79-be3b-4082-a86e-70c93210f1a4");
        if (saxLog.isDebugEnabled()) {
            writeline("/home/ubuntu/results/coverage/Digester/Digester_9_10.coverage", "c7b47cad-ea1f-4944-997a-4fd005002a8a");
            saxLog.debug("startDocument()");
        }
        writeline("/home/ubuntu/results/coverage/Digester/Digester_9_10.coverage", "f17f218a-0ea8-4dae-a83e-eb97d7426387");
        configure();
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public void startElement(String namespaceURI, String localName, String qName, Attributes list) throws SAXException {
        writeline("/home/ubuntu/results/coverage/Digester/Digester_9_10.coverage", "0074d9c1-17ac-4eb3-9c6c-f8bc85a51d5e");
        boolean debug = log.isDebugEnabled();
        writeline("/home/ubuntu/results/coverage/Digester/Digester_9_10.coverage", "2896fc40-c26c-422a-9f3d-1bb2975e227f");
        if (customContentHandler != null) {
            writeline("/home/ubuntu/results/coverage/Digester/Digester_9_10.coverage", "edf8a149-50f0-4abd-b761-8f3b70a186d6");
            customContentHandler.startElement(namespaceURI, localName, qName, list);
            writeline("/home/ubuntu/results/coverage/Digester/Digester_9_10.coverage", "4882f906-c40d-4cf0-b07e-aa26f2e79b71");
            return;
        }
        writeline("/home/ubuntu/results/coverage/Digester/Digester_9_10.coverage", "7c1d1d18-0d53-454e-8bf7-9410097a07a2");
        if (saxLog.isDebugEnabled()) {
            writeline("/home/ubuntu/results/coverage/Digester/Digester_9_10.coverage", "8188b369-4657-4fb7-be7d-8358cfd34137");
            saxLog.debug("startElement(" + namespaceURI + "," + localName + "," + qName + ")");
        }
        writeline("/home/ubuntu/results/coverage/Digester/Digester_9_10.coverage", "fe391961-1d73-488b-9dab-d800b14e9bb7");
        bodyTexts.push(bodyText);
        writeline("/home/ubuntu/results/coverage/Digester/Digester_9_10.coverage", "f793e3f7-9432-4ea1-9a04-3f1627aec8ef");
        if (debug) {
            writeline("/home/ubuntu/results/coverage/Digester/Digester_9_10.coverage", "193596d8-83a7-4018-8265-00699ab3268e");
            log.debug("  Pushing body text '" + bodyText.toString() + "'");
        }
        writeline("/home/ubuntu/results/coverage/Digester/Digester_9_10.coverage", "d66fb4dc-2665-4e23-a13a-683adfdb3bde");
        bodyText = new StringBuilder();
        writeline("/home/ubuntu/results/coverage/Digester/Digester_9_10.coverage", "7f059e2e-8682-4a49-9b8e-9a26a267c048");
        String name = localName;
        writeline("/home/ubuntu/results/coverage/Digester/Digester_9_10.coverage", "7d704e93-4b22-4233-ab23-af3ddf2c68a4");
        if ((name == null) || (name.length() < 1)) {
            writeline("/home/ubuntu/results/coverage/Digester/Digester_9_10.coverage", "85db8f55-b78b-4b87-bc54-df249535dbf3");
            name = qName;
        }
        writeline("/home/ubuntu/results/coverage/Digester/Digester_9_10.coverage", "c36496d7-06fc-4c4c-b63f-55317d80a514");
        StringBuilder sb = new StringBuilder(match);
        writeline("/home/ubuntu/results/coverage/Digester/Digester_9_10.coverage", "1768f3b0-0f73-4e51-b353-f0a299c4843e");
        if (match.length() > 0) {
            writeline("/home/ubuntu/results/coverage/Digester/Digester_9_10.coverage", "20ceec61-2655-4ae9-8933-5f0ab9516205");
            sb.append('/');
        }
        writeline("/home/ubuntu/results/coverage/Digester/Digester_9_10.coverage", "1b951052-3343-40b2-b6c6-80e3f43d413b");
        sb.append(name);
        writeline("/home/ubuntu/results/coverage/Digester/Digester_9_10.coverage", "2a0cc7c5-33a1-4e47-89bc-4df19e4fbd01");
        match = sb.toString();
        writeline("/home/ubuntu/results/coverage/Digester/Digester_9_10.coverage", "64200042-7588-43a6-a9a6-b2d02baecbe0");
        if (debug) {
            writeline("/home/ubuntu/results/coverage/Digester/Digester_9_10.coverage", "f501258c-0f19-4557-a868-ebf6692ef48f");
            log.debug("  New match='" + match + "'");
        }
        writeline("/home/ubuntu/results/coverage/Digester/Digester_9_10.coverage", "8e41e25b-a27a-4ddf-80cf-b4ac505a21aa");
        List<Rule> rules = getRules().match(namespaceURI, match, localName, list);
        writeline("/home/ubuntu/results/coverage/Digester/Digester_9_10.coverage", "792d945b-c5e3-484f-bd7b-b5e2101ad6d3");
        matches.push(rules);
        writeline("/home/ubuntu/results/coverage/Digester/Digester_9_10.coverage", "89d67319-39ff-4255-8089-edaae86f453d");
        if ((rules != null) && (rules.size() > 0)) {
            writeline("/home/ubuntu/results/coverage/Digester/Digester_9_10.coverage", "d2e1e130-d609-40ca-bdc3-29f9dcef562f");
            Substitutor substitutor = getSubstitutor();
            writeline("/home/ubuntu/results/coverage/Digester/Digester_9_10.coverage", "2788caa6-268d-4e5b-a23e-99906731fbd6");
            if (substitutor != null) {
                writeline("/home/ubuntu/results/coverage/Digester/Digester_9_10.coverage", "a0cab860-7efb-4ada-89a6-4683d273afb1");
                list = substitutor.substitute(list);
            }
            writeline("/home/ubuntu/results/coverage/Digester/Digester_9_10.coverage", "437454ae-242d-49b0-a411-854e6c0a38b6");
            for (int i = 0; i < rules.size(); i++) {
                writeline("/home/ubuntu/results/coverage/Digester/Digester_9_10.coverage", "6e94b0fe-4ada-4ff2-aa49-7ca9c967b54b");
                try {
                    writeline("/home/ubuntu/results/coverage/Digester/Digester_9_10.coverage", "e1c34969-bed5-464f-970f-6ff306531319");
                    Rule rule = rules.get(i);
                    writeline("/home/ubuntu/results/coverage/Digester/Digester_9_10.coverage", "9a43ffa6-2910-4b99-a305-489ca98da644");
                    if (debug) {
                        writeline("/home/ubuntu/results/coverage/Digester/Digester_9_10.coverage", "20841c2c-5587-47cf-9d0a-9943f3803b96");
                        log.debug("  Fire begin() for " + rule);
                    }
                    writeline("/home/ubuntu/results/coverage/Digester/Digester_9_10.coverage", "bc46227a-11f8-4ba4-b85a-5fd998c2f71c");
                    rule.begin(namespaceURI, name, list);
                } catch (Exception e) {
                    writeline("/home/ubuntu/results/coverage/Digester/Digester_9_10.coverage", "d21f184f-f92f-4188-8c51-a45b6de09ec6");
                    log.error("Begin event threw exception", e);
                } catch (Error e) {
                    writeline("/home/ubuntu/results/coverage/Digester/Digester_9_10.coverage", "a05ab077-df97-4b59-ae8d-2dcfa24f1887");
                    log.error("Begin event threw error", e);
                }
            }
        } else {
            writeline("/home/ubuntu/results/coverage/Digester/Digester_9_10.coverage", "98204dc6-d215-41b5-9afa-ab4ef378bf88");
            if (debug) {
                writeline("/home/ubuntu/results/coverage/Digester/Digester_9_10.coverage", "2aa74b41-c509-4707-902f-f09ac09d155d");
                log.debug("  No rules found matching '" + match + "'.");
            }
        }
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public void startPrefixMapping(String prefix, String namespaceURI) throws SAXException {
        writeline("/home/ubuntu/results/coverage/Digester/Digester_9_10.coverage", "4d0c4de5-7046-449c-9c40-cc0ad3d0c63c");
        if (saxLog.isDebugEnabled()) {
            writeline("/home/ubuntu/results/coverage/Digester/Digester_9_10.coverage", "085a8b10-8c67-4aab-9709-7a1cb32a599f");
            saxLog.debug("startPrefixMapping(" + prefix + "," + namespaceURI + ")");
        }
        writeline("/home/ubuntu/results/coverage/Digester/Digester_9_10.coverage", "b1037a56-b8c9-4f03-a85b-093793696932");
        Stack<String> stack = namespaces.get(prefix);
        writeline("/home/ubuntu/results/coverage/Digester/Digester_9_10.coverage", "325ef24c-6989-468e-8ada-7b447d597e4d");
        if (stack == null) {
            writeline("/home/ubuntu/results/coverage/Digester/Digester_9_10.coverage", "3fe1c09e-08fc-4cb6-a887-10b5e2f890f6");
            stack = new Stack<String>();
            writeline("/home/ubuntu/results/coverage/Digester/Digester_9_10.coverage", "e1eaf088-8001-4cce-a76b-967c513cf6b0");
            namespaces.put(prefix, stack);
        }
        writeline("/home/ubuntu/results/coverage/Digester/Digester_9_10.coverage", "b3d0c913-92e2-4e57-9647-30d78ea0170c");
        stack.push(namespaceURI);
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public void notationDecl(String name, String publicId, String systemId) {
        writeline("/home/ubuntu/results/coverage/Digester/Digester_9_10.coverage", "fea7f49f-ea58-4936-b0c3-ebc2b6dbcdc3");
        if (saxLog.isDebugEnabled()) {
            writeline("/home/ubuntu/results/coverage/Digester/Digester_9_10.coverage", "3dbdc772-5485-475b-82ba-392b823cdf4b");
            saxLog.debug("notationDecl(" + name + "," + publicId + "," + systemId + ")");
        }
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public void unparsedEntityDecl(String name, String publicId, String systemId, String notation) {
        writeline("/home/ubuntu/results/coverage/Digester/Digester_9_10.coverage", "6a32e64f-dbab-4ef3-978d-731636183bcd");
        if (saxLog.isDebugEnabled()) {
            writeline("/home/ubuntu/results/coverage/Digester/Digester_9_10.coverage", "4bd1b0ba-2547-4572-bf7c-22d0927fe094");
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
        writeline("/home/ubuntu/results/coverage/Digester/Digester_9_10.coverage", "41d16949-7a38-4692-8d43-314f3911fb5f");
        this.entityResolver = entityResolver;
    }

    /**
     * Return the Entity Resolver used by the SAX parser.
     *
     * @return the Entity Resolver used by the SAX parser.
     */
    public EntityResolver getEntityResolver() {
        writeline("/home/ubuntu/results/coverage/Digester/Digester_9_10.coverage", "2044f469-a809-4d94-a479-dbd4125fdcbd");
        return entityResolver;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public InputSource resolveEntity(String publicId, String systemId) throws SAXException {
        writeline("/home/ubuntu/results/coverage/Digester/Digester_9_10.coverage", "b06c5a6e-4586-459c-a3e3-1db3cd60a5fd");
        if (saxLog.isDebugEnabled()) {
            writeline("/home/ubuntu/results/coverage/Digester/Digester_9_10.coverage", "9184a568-71b0-4727-981a-17f860920bc5");
            saxLog.debug("resolveEntity('" + publicId + "', '" + systemId + "')");
        }
        writeline("/home/ubuntu/results/coverage/Digester/Digester_9_10.coverage", "25c6fbb1-7672-4a57-9807-945b3f07927e");
        if (publicId != null) {
            writeline("/home/ubuntu/results/coverage/Digester/Digester_9_10.coverage", "13215854-3cc7-4aa4-80b5-c5b8051f6bc7");
            this.publicId = publicId;
        }
        writeline("/home/ubuntu/results/coverage/Digester/Digester_9_10.coverage", "aa860cd7-1b58-4ba9-a715-72ed868a2dfc");
        URL entityURL = null;
        writeline("/home/ubuntu/results/coverage/Digester/Digester_9_10.coverage", "9aa56807-0d34-45e7-b5d5-04f6682e96fb");
        if (publicId != null) {
            writeline("/home/ubuntu/results/coverage/Digester/Digester_9_10.coverage", "6bbdf1ea-7d6a-4faa-9ecc-ab32efcf08e3");
            entityURL = entityValidator.get(publicId);
        }
        writeline("/home/ubuntu/results/coverage/Digester/Digester_9_10.coverage", "fbb334e1-f71a-48fd-9d6b-b377f50a03c3");
        if (entityURL == null && systemId != null) {
            writeline("/home/ubuntu/results/coverage/Digester/Digester_9_10.coverage", "0da637a1-f71f-4c6e-81a5-fba6ecbef65b");
            entityURL = entityValidator.get(systemId);
        }
        writeline("/home/ubuntu/results/coverage/Digester/Digester_9_10.coverage", "613a5417-5e5b-4a24-a82f-e6083c531146");
        if (entityURL == null) {
            writeline("/home/ubuntu/results/coverage/Digester/Digester_9_10.coverage", "66bba01d-f9f9-46f9-9a21-bca85210e763");
            if (systemId == null) {
                writeline("/home/ubuntu/results/coverage/Digester/Digester_9_10.coverage", "41eb8927-965c-47ca-82d1-ba5628efad2b");
                if (log.isDebugEnabled()) {
                    writeline("/home/ubuntu/results/coverage/Digester/Digester_9_10.coverage", "994a6116-0b90-4386-9701-83a0ff21e54e");
                    log.debug(" Cannot resolve null entity, returning null InputSource");
                }
                writeline("/home/ubuntu/results/coverage/Digester/Digester_9_10.coverage", "99adf9ee-eb16-48bb-b987-038575d6abef");
                return (null);
            }
            writeline("/home/ubuntu/results/coverage/Digester/Digester_9_10.coverage", "66537059-f2b9-480f-899a-71003fcaff05");
            if (log.isDebugEnabled()) {
                writeline("/home/ubuntu/results/coverage/Digester/Digester_9_10.coverage", "1c1cb3fd-ea83-4d57-b79f-9331df6e0971");
                log.debug(" Trying to resolve using system ID '" + systemId + "'");
            }
            writeline("/home/ubuntu/results/coverage/Digester/Digester_9_10.coverage", "a8a72a27-eddc-495c-8e24-16a72fca9096");
            try {
                writeline("/home/ubuntu/results/coverage/Digester/Digester_9_10.coverage", "0eae5d06-6426-42f8-a656-5b2344c45335");
                entityURL = new URL(systemId);
            } catch (MalformedURLException e) {
            }
        }
        writeline("/home/ubuntu/results/coverage/Digester/Digester_9_10.coverage", "10763cef-14af-46e4-b379-7c8c27262149");
        if (log.isDebugEnabled()) {
            writeline("/home/ubuntu/results/coverage/Digester/Digester_9_10.coverage", "e61cfe57-830c-4909-81f7-ca4002b12011");
            log.debug(" Resolving to alternate DTD '" + entityURL + "'");
        }
        writeline("/home/ubuntu/results/coverage/Digester/Digester_9_10.coverage", "50d70f83-099c-4000-b914-92291a0fa19e");
        try {
            writeline("/home/ubuntu/results/coverage/Digester/Digester_9_10.coverage", "e1576aa4-e6df-43ce-9774-55a120fcfd03");
            return createInputSourceFromURL(entityURL);
        } catch (Exception e) {
        }
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public void error(SAXParseException exception) throws SAXException {
        writeline("/home/ubuntu/results/coverage/Digester/Digester_9_10.coverage", "7831f2e8-d351-49f0-be8f-f3be26398bcc");
        log.error("Parse Error at line " + exception.getLineNumber() + " column " + exception.getColumnNumber() + ": " + exception.getMessage(), exception);
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public void fatalError(SAXParseException exception) throws SAXException {
        writeline("/home/ubuntu/results/coverage/Digester/Digester_9_10.coverage", "8d3f7d8b-7ee4-4b3c-9415-117dc355ebc9");
        log.error("Parse Fatal Error at line " + exception.getLineNumber() + " column " + exception.getColumnNumber() + ": " + exception.getMessage(), exception);
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public void warning(SAXParseException exception) throws SAXException {
        writeline("/home/ubuntu/results/coverage/Digester/Digester_9_10.coverage", "9be6955f-1bbf-4192-ab01-71405543a76d");
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
        writeline("/home/ubuntu/results/coverage/Digester/Digester_9_10.coverage", "19adf52d-d8f6-42eb-b7e4-f0fd55cb0a74");
        if (file == null) {
        }
        writeline("/home/ubuntu/results/coverage/Digester/Digester_9_10.coverage", "a981c4b7-15ca-4e5d-8cac-56e68aaefb97");
        InputSource input = new InputSource(new FileInputStream(file));
        writeline("/home/ubuntu/results/coverage/Digester/Digester_9_10.coverage", "959db02a-8e2a-43ae-92fe-f6dfccc6c1f8");
        input.setSystemId(file.toURI().toURL().toString());
        writeline("/home/ubuntu/results/coverage/Digester/Digester_9_10.coverage", "46583168-57ad-4ffc-b224-e8eba4ef69c1");
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
        writeline("/home/ubuntu/results/coverage/Digester/Digester_9_10.coverage", "c61280b4-248f-4567-939d-e3977c5878b9");
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
        writeline("/home/ubuntu/results/coverage/Digester/Digester_9_10.coverage", "3d1d1b32-b381-4d21-8c04-66315db0fa5c");
        if (input == null) {
        }
        writeline("/home/ubuntu/results/coverage/Digester/Digester_9_10.coverage", "7f3dcaef-fe2e-436e-becc-686e1d12d061");
        configure();
        writeline("/home/ubuntu/results/coverage/Digester/Digester_9_10.coverage", "baeec216-834d-4172-b7bd-518f4f7663f9");
        String systemId = input.getSystemId();
        writeline("/home/ubuntu/results/coverage/Digester/Digester_9_10.coverage", "6f7462cb-4582-49c2-98eb-7b6e6be3cf18");
        if (systemId == null) {
            writeline("/home/ubuntu/results/coverage/Digester/Digester_9_10.coverage", "dff97203-2369-41de-8ce5-dddd725b9490");
            systemId = "(already loaded from stream)";
        }
        writeline("/home/ubuntu/results/coverage/Digester/Digester_9_10.coverage", "c6e630fd-cb9d-4c49-b058-b3b3e5e6fbfd");
        try {
            writeline("/home/ubuntu/results/coverage/Digester/Digester_9_10.coverage", "c19d6696-f6cf-4761-8014-fae5c4ecedbe");
            getXMLReader().parse(input);
        } catch (IOException e) {
            writeline("/home/ubuntu/results/coverage/Digester/Digester_9_10.coverage", "cfd41360-7474-44dd-9e2b-484fc8797cb3");
            log.error(format("An error occurred while reading stream from '%s', see nested exceptions", systemId), e);
        }
        writeline("/home/ubuntu/results/coverage/Digester/Digester_9_10.coverage", "2e106dbc-3d68-419b-a76d-01b8dcf5eb8c");
        cleanup();
        writeline("/home/ubuntu/results/coverage/Digester/Digester_9_10.coverage", "8c5d1722-057d-41c3-9af4-6c3ec22eab22");
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
        writeline("/home/ubuntu/results/coverage/Digester/Digester_9_10.coverage", "6e5607bd-bcb7-4cab-8ae4-904ba07f7dd5");
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
        writeline("/home/ubuntu/results/coverage/Digester/Digester_9_10.coverage", "66db8b73-940a-465a-972a-8dd1e7d27c28");
        if (input == null) {
        }
        writeline("/home/ubuntu/results/coverage/Digester/Digester_9_10.coverage", "c4411fac-263f-4a5d-b4f2-abafdbb80c88");
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
        writeline("/home/ubuntu/results/coverage/Digester/Digester_9_10.coverage", "47f4733d-15a3-4957-ae1b-3921f9cbe5e2");
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
        writeline("/home/ubuntu/results/coverage/Digester/Digester_9_10.coverage", "c5f7a905-a45b-40f4-b572-5845acf6da76");
        if (reader == null) {
        }
        writeline("/home/ubuntu/results/coverage/Digester/Digester_9_10.coverage", "8006098c-f711-4f38-a95b-a27b5a551956");
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
        writeline("/home/ubuntu/results/coverage/Digester/Digester_9_10.coverage", "ea3dec52-b451-4715-9d8f-ba8b790d8296");
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
        writeline("/home/ubuntu/results/coverage/Digester/Digester_9_10.coverage", "8395c476-2d8a-41ae-95c0-e4206fc31b13");
        if (uri == null) {
        }
        writeline("/home/ubuntu/results/coverage/Digester/Digester_9_10.coverage", "dffddbeb-ee65-4584-8968-0368e9d774a6");
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
        writeline("/home/ubuntu/results/coverage/Digester/Digester_9_10.coverage", "22925af3-7235-4a35-82e9-dd49c982a201");
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
        writeline("/home/ubuntu/results/coverage/Digester/Digester_9_10.coverage", "b2a3def7-280a-4531-8773-78ef6afe8200");
        if (url == null) {
        }
        writeline("/home/ubuntu/results/coverage/Digester/Digester_9_10.coverage", "c4c40b70-3867-474f-a0a0-d3196a83d32f");
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
        writeline("/home/ubuntu/results/coverage/Digester/Digester_9_10.coverage", "c884e312-f207-43fe-bfcd-062ec07c1379");
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
        writeline("/home/ubuntu/results/coverage/Digester/Digester_9_10.coverage", "bbfab5dd-dbe7-44be-8946-bfc334e157f4");
        if (executorService == null) {
        }
        writeline("/home/ubuntu/results/coverage/Digester/Digester_9_10.coverage", "09bc1f25-df32-43a3-ba85-138a1cd62f79");
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
        writeline("/home/ubuntu/results/coverage/Digester/Digester_9_10.coverage", "17d9b4a6-007e-4c87-8f1c-54d34495d3f8");
        if (log.isDebugEnabled()) {
            writeline("/home/ubuntu/results/coverage/Digester/Digester_9_10.coverage", "6f8fa80d-e98f-421f-adc4-0bd3d8542c6a");
            log.debug("register('" + publicId + "', '" + entityURL + "'");
        }
        writeline("/home/ubuntu/results/coverage/Digester/Digester_9_10.coverage", "1475b8d8-af3e-448d-80e4-c7156ca9e144");
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
        writeline("/home/ubuntu/results/coverage/Digester/Digester_9_10.coverage", "589128cb-8e44-4243-bfb4-5aa029e5e469");
        if (log.isDebugEnabled()) {
            writeline("/home/ubuntu/results/coverage/Digester/Digester_9_10.coverage", "0c9f835a-5e10-4c04-85f2-03e619ad9853");
            log.debug("register('" + publicId + "', '" + entityURL + "'");
        }
        writeline("/home/ubuntu/results/coverage/Digester/Digester_9_10.coverage", "5ab88273-dcf8-4f57-a21a-0ea2eb87f076");
        try {
            writeline("/home/ubuntu/results/coverage/Digester/Digester_9_10.coverage", "1fc6c6b7-c72d-4223-8a55-7dd1ac7093c4");
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
        writeline("/home/ubuntu/results/coverage/Digester/Digester_9_10.coverage", "390e9454-3836-4cf6-9799-7977b5adb0e5");
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
        writeline("/home/ubuntu/results/coverage/Digester/Digester_9_10.coverage", "4e363127-0008-4fa0-b9e8-86913e158039");
        URLConnection connection = url.openConnection();
        writeline("/home/ubuntu/results/coverage/Digester/Digester_9_10.coverage", "9a02c8fb-23a9-489c-9a30-4add7c1bb2f4");
        connection.setUseCaches(false);
        writeline("/home/ubuntu/results/coverage/Digester/Digester_9_10.coverage", "0d114be0-de37-4b91-a9a5-a25324d34068");
        InputStream stream = connection.getInputStream();
        writeline("/home/ubuntu/results/coverage/Digester/Digester_9_10.coverage", "17285faa-0a2c-44ea-9ed4-7a81eb7a0644");
        InputSource source = new InputSource(stream);
        writeline("/home/ubuntu/results/coverage/Digester/Digester_9_10.coverage", "83726ddb-2ca8-4894-ba5e-29d5555689e4");
        source.setSystemId(url.toExternalForm());
        writeline("/home/ubuntu/results/coverage/Digester/Digester_9_10.coverage", "40beab1a-4674-40ba-8bdf-64a8ea7b0994");
        inputSources.add(source);
        writeline("/home/ubuntu/results/coverage/Digester/Digester_9_10.coverage", "284c3b59-1d47-4e05-ac9c-e43cc6195d2b");
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
        writeline("/home/ubuntu/results/coverage/Digester/Digester_9_10.coverage", "636de51a-0a5d-4558-bccc-6cb501dd66de");
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
        writeline("/home/ubuntu/results/coverage/Digester/Digester_9_10.coverage", "a521d7a2-7967-4548-97c7-067149f8bf51");
        rule.setDigester(this);
        writeline("/home/ubuntu/results/coverage/Digester/Digester_9_10.coverage", "aeb15b5a-cbcf-437f-b1c3-c7ccc4419d29");
        getRules().add(pattern, rule);
    }

    /**
     * Register a set of Rule instances defined in a RuleSet.
     *
     * @param ruleSet The RuleSet instance to configure from
     */
    public void addRuleSet(RuleSet ruleSet) {
        writeline("/home/ubuntu/results/coverage/Digester/Digester_9_10.coverage", "5f915311-4aa2-4b1f-a4c1-249f48c0dc66");
        String oldNamespaceURI = getRuleNamespaceURI();
        writeline("/home/ubuntu/results/coverage/Digester/Digester_9_10.coverage", "3a564c88-6186-400f-849f-5a2a7604ddb1");
        String newNamespaceURI = ruleSet.getNamespaceURI();
        writeline("/home/ubuntu/results/coverage/Digester/Digester_9_10.coverage", "ac1e2f99-dfa2-4094-9ab4-cf338a550fed");
        if (log.isDebugEnabled()) {
            writeline("/home/ubuntu/results/coverage/Digester/Digester_9_10.coverage", "9f4e7202-1918-423b-bc3f-cd7678c22251");
            if (newNamespaceURI == null) {
                writeline("/home/ubuntu/results/coverage/Digester/Digester_9_10.coverage", "da45d338-c7ee-4869-ad9d-a10ac7eb9f64");
                log.debug("addRuleSet() with no namespace URI");
            } else {
                writeline("/home/ubuntu/results/coverage/Digester/Digester_9_10.coverage", "0bd76972-b832-4ccc-b527-202b65438818");
                log.debug("addRuleSet() with namespace URI " + newNamespaceURI);
            }
        }
        writeline("/home/ubuntu/results/coverage/Digester/Digester_9_10.coverage", "611fec20-626a-49bb-937e-ee89a39ba518");
        setRuleNamespaceURI(newNamespaceURI);
        writeline("/home/ubuntu/results/coverage/Digester/Digester_9_10.coverage", "fcc9ebe1-a964-41ad-8015-7742dc1d04ed");
        ruleSet.addRuleInstances(this);
        writeline("/home/ubuntu/results/coverage/Digester/Digester_9_10.coverage", "471d4586-9110-4cb9-8549-2671cad1d28f");
        setRuleNamespaceURI(oldNamespaceURI);
    }

    /**
     * Add a "bean property setter" rule for the specified parameters.
     *
     * @param pattern Element matching pattern
     * @see BeanPropertySetterRule
     */
    public void addBeanPropertySetter(String pattern) {
        writeline("/home/ubuntu/results/coverage/Digester/Digester_9_10.coverage", "b327a76a-8186-4979-a9f2-e7e3cea280a9");
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
        writeline("/home/ubuntu/results/coverage/Digester/Digester_9_10.coverage", "26508818-f99b-4d99-85e1-1fda69fde364");
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
        writeline("/home/ubuntu/results/coverage/Digester/Digester_9_10.coverage", "8ace06fe-474c-4d98-a226-3e3b015bceb9");
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
        writeline("/home/ubuntu/results/coverage/Digester/Digester_9_10.coverage", "4f82b495-25d3-4c23-9665-5ee9526f15b1");
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
        writeline("/home/ubuntu/results/coverage/Digester/Digester_9_10.coverage", "2c681bd5-69b8-4262-bdc4-445ac6a5d0cf");
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
        writeline("/home/ubuntu/results/coverage/Digester/Digester_9_10.coverage", "07e5127d-5831-4a2a-86ca-365a6f49998f");
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
        writeline("/home/ubuntu/results/coverage/Digester/Digester_9_10.coverage", "8ab6ca8d-50b3-45b9-b6dd-19fda48c63d1");
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
        writeline("/home/ubuntu/results/coverage/Digester/Digester_9_10.coverage", "df927663-38c4-48be-9f7f-9701e087a87e");
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
        writeline("/home/ubuntu/results/coverage/Digester/Digester_9_10.coverage", "dc19350c-9612-4611-afd8-0c8ace1a9858");
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
        writeline("/home/ubuntu/results/coverage/Digester/Digester_9_10.coverage", "0f5843f1-f5f1-4a30-9ecd-7571cf5eb272");
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
        writeline("/home/ubuntu/results/coverage/Digester/Digester_9_10.coverage", "b8f9ba16-7867-41e0-bdda-acb9d8c76bf1");
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
        writeline("/home/ubuntu/results/coverage/Digester/Digester_9_10.coverage", "8446e4a9-fab0-4b26-9149-f2e4ae3717e7");
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
        writeline("/home/ubuntu/results/coverage/Digester/Digester_9_10.coverage", "d3fd1778-7291-4390-bafc-29b3640a781c");
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
        writeline("/home/ubuntu/results/coverage/Digester/Digester_9_10.coverage", "cfddae95-f519-4fff-b2d8-0a1a5a4ae63f");
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
        writeline("/home/ubuntu/results/coverage/Digester/Digester_9_10.coverage", "ff97b46f-3da8-4cd7-9aaa-444ef0da1817");
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
        writeline("/home/ubuntu/results/coverage/Digester/Digester_9_10.coverage", "2ad89a1c-8a44-4daf-8d32-95abfb74bc64");
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
        writeline("/home/ubuntu/results/coverage/Digester/Digester_9_10.coverage", "9a998884-7804-42e9-9d39-088cb11aa7a1");
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
        writeline("/home/ubuntu/results/coverage/Digester/Digester_9_10.coverage", "2c381ec6-6265-4a52-b773-d2ad75b29fd3");
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
        writeline("/home/ubuntu/results/coverage/Digester/Digester_9_10.coverage", "a00cab59-c91e-4a91-a5be-7dc2220f10ec");
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
        writeline("/home/ubuntu/results/coverage/Digester/Digester_9_10.coverage", "98d6f630-c480-47e0-bef3-0ebcd4c8f753");
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
        writeline("/home/ubuntu/results/coverage/Digester/Digester_9_10.coverage", "3f2fcb20-586a-48ab-aada-9aab6ea0a151");
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
        writeline("/home/ubuntu/results/coverage/Digester/Digester_9_10.coverage", "15a009d8-b093-47f5-8adb-8e82b12b2f6a");
        creationFactory.setDigester(this);
        writeline("/home/ubuntu/results/coverage/Digester/Digester_9_10.coverage", "01bc3214-d8b9-4729-b866-7cd894868117");
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
        writeline("/home/ubuntu/results/coverage/Digester/Digester_9_10.coverage", "6f9c0b54-f740-4c1c-ac80-ec5f7087fbde");
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
        writeline("/home/ubuntu/results/coverage/Digester/Digester_9_10.coverage", "01b9a52b-5064-4d6b-ba71-14c1854279d9");
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
        writeline("/home/ubuntu/results/coverage/Digester/Digester_9_10.coverage", "c81db2e1-d35e-40e4-8d29-22c150ab6235");
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
        writeline("/home/ubuntu/results/coverage/Digester/Digester_9_10.coverage", "c22a0eba-2d03-4830-8174-75971118e6b8");
        addRule(pattern, new ObjectCreateRule(attributeName, clazz));
    }

    /**
     * Adds an {@link SetNestedPropertiesRule}.
     *
     * @param pattern register the rule with this pattern
     * @since 1.6
     */
    public void addSetNestedProperties(String pattern) {
        writeline("/home/ubuntu/results/coverage/Digester/Digester_9_10.coverage", "76b3a705-611b-4bb8-8c1d-f3255703d83d");
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
        writeline("/home/ubuntu/results/coverage/Digester/Digester_9_10.coverage", "e471c16a-8632-47ec-94f6-e52224a168bc");
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
        writeline("/home/ubuntu/results/coverage/Digester/Digester_9_10.coverage", "0fd34139-f605-441f-951e-8632c1cdb661");
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
        writeline("/home/ubuntu/results/coverage/Digester/Digester_9_10.coverage", "df0883d1-9757-470e-ad57-07b524675075");
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
        writeline("/home/ubuntu/results/coverage/Digester/Digester_9_10.coverage", "db03dc47-8935-417d-abbc-346a4ceb41ac");
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
        writeline("/home/ubuntu/results/coverage/Digester/Digester_9_10.coverage", "f9bc6b30-5ed3-45e6-8680-eb80965d4634");
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
        writeline("/home/ubuntu/results/coverage/Digester/Digester_9_10.coverage", "725c99d6-31a0-4f35-81c7-2d1818d6a545");
        addRule(pattern, new SetRootRule(methodName, paramType));
    }

    /**
     * Add a "set properties" rule for the specified parameters.
     *
     * @param pattern Element matching pattern
     * @see SetPropertiesRule
     */
    public void addSetProperties(String pattern) {
        writeline("/home/ubuntu/results/coverage/Digester/Digester_9_10.coverage", "bad35dce-a576-4ab8-873b-7308adfac66e");
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
        writeline("/home/ubuntu/results/coverage/Digester/Digester_9_10.coverage", "8efc3c12-9322-4052-9773-422433510f9f");
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
        writeline("/home/ubuntu/results/coverage/Digester/Digester_9_10.coverage", "89ea26f1-2678-4034-a182-30a261565c98");
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
        writeline("/home/ubuntu/results/coverage/Digester/Digester_9_10.coverage", "d1ec2d33-7222-42f5-9fc3-e1bd4290533a");
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
        writeline("/home/ubuntu/results/coverage/Digester/Digester_9_10.coverage", "b5cafae2-77c2-408c-b033-56f1f3750e61");
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
        writeline("/home/ubuntu/results/coverage/Digester/Digester_9_10.coverage", "1eff9743-f37a-429c-81a9-ec645fcb693f");
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
        writeline("/home/ubuntu/results/coverage/Digester/Digester_9_10.coverage", "48f9cabc-094c-47e3-a42e-fbcda38fa1ba");
        match = "";
        writeline("/home/ubuntu/results/coverage/Digester/Digester_9_10.coverage", "f697aa7c-da4b-4f01-9a31-b80055194c79");
        bodyTexts.clear();
        writeline("/home/ubuntu/results/coverage/Digester/Digester_9_10.coverage", "685cbb37-ee3a-41f5-ba21-0e95072b1254");
        params.clear();
        writeline("/home/ubuntu/results/coverage/Digester/Digester_9_10.coverage", "2ac44572-f8ce-44f2-b483-f57b8bfe8de2");
        publicId = null;
        writeline("/home/ubuntu/results/coverage/Digester/Digester_9_10.coverage", "73c646c8-57e6-47e1-bd90-1cd3ccfb8198");
        stack.clear();
        writeline("/home/ubuntu/results/coverage/Digester/Digester_9_10.coverage", "6f0ee127-3b02-433c-845a-ffe05d5e56b4");
        stacksByName.clear();
        writeline("/home/ubuntu/results/coverage/Digester/Digester_9_10.coverage", "b38399b3-ee14-46e9-afbd-9c3e71a8d973");
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
        writeline("/home/ubuntu/results/coverage/Digester/Digester_9_10.coverage", "03064c5c-f54e-4463-9e76-b487e5d7076a");
        try {
            writeline("/home/ubuntu/results/coverage/Digester/Digester_9_10.coverage", "f0bf895d-d026-4111-a1c0-f2134a129612");
            return this.<T>npeSafeCast(stack.peek());
        } catch (EmptyStackException e) {
            writeline("/home/ubuntu/results/coverage/Digester/Digester_9_10.coverage", "c306e4d0-1145-43ff-a1d6-ca2efe742530");
            log.warn("Empty stack (returning null)");
            writeline("/home/ubuntu/results/coverage/Digester/Digester_9_10.coverage", "b3773381-d3b8-49af-ad4f-e6ab25ff6a6a");
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
        writeline("/home/ubuntu/results/coverage/Digester/Digester_9_10.coverage", "05e78d14-5e0a-43c4-abf4-e535e87b9726");
        int index = (stack.size() - 1) - n;
        writeline("/home/ubuntu/results/coverage/Digester/Digester_9_10.coverage", "bc405334-e5ac-4433-812f-7d4fa4812788");
        if (index < 0) {
            writeline("/home/ubuntu/results/coverage/Digester/Digester_9_10.coverage", "abb6dfed-6017-4695-abf2-f2bdae42e40d");
            log.warn("Empty stack (returning null)");
            writeline("/home/ubuntu/results/coverage/Digester/Digester_9_10.coverage", "f6490b9e-78f1-4ba0-8da7-9ed408cc34a2");
            return (null);
        }
        writeline("/home/ubuntu/results/coverage/Digester/Digester_9_10.coverage", "9d30505f-0c65-42b5-a816-616cd9fc2b0e");
        try {
            writeline("/home/ubuntu/results/coverage/Digester/Digester_9_10.coverage", "3daa46ab-cdc6-42c1-bc9c-f0eb31fd9ee6");
            return this.<T>npeSafeCast(stack.get(index));
        } catch (EmptyStackException e) {
            writeline("/home/ubuntu/results/coverage/Digester/Digester_9_10.coverage", "18445784-7fa6-4bc5-bbbe-6b7b17407981");
            log.warn("Empty stack (returning null)");
            writeline("/home/ubuntu/results/coverage/Digester/Digester_9_10.coverage", "068bc41e-883a-4b64-bdc2-e12648bca2bb");
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
        writeline("/home/ubuntu/results/coverage/Digester/Digester_9_10.coverage", "e63f334b-d448-4cc2-b266-9890b36a90f4");
        try {
            writeline("/home/ubuntu/results/coverage/Digester/Digester_9_10.coverage", "5616628e-16b9-4f49-8e47-f8a8fc7d1be8");
            T popped = this.<T>npeSafeCast(stack.pop());
            writeline("/home/ubuntu/results/coverage/Digester/Digester_9_10.coverage", "e03e9bb7-b205-4cc2-b205-9729b112d9df");
            if (stackAction != null) {
                writeline("/home/ubuntu/results/coverage/Digester/Digester_9_10.coverage", "30fa0751-791d-42c1-91ef-363db908bd64");
                popped = stackAction.onPop(this, null, popped);
            }
            writeline("/home/ubuntu/results/coverage/Digester/Digester_9_10.coverage", "2defa8fd-8f10-47e9-a1ac-07f6e04365a5");
            return popped;
        } catch (EmptyStackException e) {
            writeline("/home/ubuntu/results/coverage/Digester/Digester_9_10.coverage", "0d9edde5-ffcc-4705-b8a8-b0ef8f701b8e");
            log.warn("Empty stack (returning null)");
            writeline("/home/ubuntu/results/coverage/Digester/Digester_9_10.coverage", "3e03b8f7-6bf5-4a9f-b884-3bcc859068ed");
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
        writeline("/home/ubuntu/results/coverage/Digester/Digester_9_10.coverage", "616e09a9-7c02-4318-be6e-88cb983035c0");
        if (stackAction != null) {
            writeline("/home/ubuntu/results/coverage/Digester/Digester_9_10.coverage", "85d5361e-44f9-43c6-bd1c-4c2cd4ab6403");
            object = stackAction.onPush(this, null, object);
        }
        writeline("/home/ubuntu/results/coverage/Digester/Digester_9_10.coverage", "db8e4526-2c92-48bc-9eb1-f921039f43a6");
        if (stack.size() == 0) {
            writeline("/home/ubuntu/results/coverage/Digester/Digester_9_10.coverage", "6cc91403-79a4-4179-934f-abc56f5d3a44");
            root = object;
        }
        writeline("/home/ubuntu/results/coverage/Digester/Digester_9_10.coverage", "f4267da1-b2ed-425e-aee6-308a9e2462fd");
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
        writeline("/home/ubuntu/results/coverage/Digester/Digester_9_10.coverage", "0a16f946-bdd1-4566-b854-70e506f558cb");
        if (stackAction != null) {
            writeline("/home/ubuntu/results/coverage/Digester/Digester_9_10.coverage", "67f36c92-aa78-4a4f-850d-791b73979ffe");
            value = stackAction.onPush(this, stackName, value);
        }
        writeline("/home/ubuntu/results/coverage/Digester/Digester_9_10.coverage", "59cb00bc-ce6b-495a-854c-c00484392982");
        Stack<Object> namedStack = stacksByName.get(stackName);
        writeline("/home/ubuntu/results/coverage/Digester/Digester_9_10.coverage", "10a271f4-676d-411a-b8b2-d61d3c3f7b80");
        if (namedStack == null) {
            writeline("/home/ubuntu/results/coverage/Digester/Digester_9_10.coverage", "6beab5e6-2479-4c4c-9565-c836be0093d2");
            namedStack = new Stack<Object>();
            writeline("/home/ubuntu/results/coverage/Digester/Digester_9_10.coverage", "be7a4ded-0b3f-4ac0-8e91-a206f6eaf453");
            stacksByName.put(stackName, namedStack);
        }
        writeline("/home/ubuntu/results/coverage/Digester/Digester_9_10.coverage", "de52b44e-5ecf-4d55-8f4a-7099b58db978");
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
        writeline("/home/ubuntu/results/coverage/Digester/Digester_9_10.coverage", "03efe495-14df-476a-b000-37b4d875fbed");
        T result = null;
        writeline("/home/ubuntu/results/coverage/Digester/Digester_9_10.coverage", "072d55ad-4f72-4e61-b6bd-dc1f4fdc583d");
        Stack<Object> namedStack = stacksByName.get(stackName);
        writeline("/home/ubuntu/results/coverage/Digester/Digester_9_10.coverage", "dced4d23-443d-4679-b212-999a1597d637");
        if (namedStack == null) {
            writeline("/home/ubuntu/results/coverage/Digester/Digester_9_10.coverage", "b8f20e87-77c2-4667-ae36-f8a93c767fee");
            if (log.isDebugEnabled()) {
                writeline("/home/ubuntu/results/coverage/Digester/Digester_9_10.coverage", "c115b4f5-ba62-46ae-8da4-1b4d6722e469");
                log.debug("Stack '" + stackName + "' is empty");
            }
        }
        writeline("/home/ubuntu/results/coverage/Digester/Digester_9_10.coverage", "f3de756e-8411-460f-85b1-f4d96882dbc8");
        result = this.<T>npeSafeCast(namedStack.pop());
        writeline("/home/ubuntu/results/coverage/Digester/Digester_9_10.coverage", "b352087a-bf39-46e6-8dfb-45bd16b4ff6e");
        if (stackAction != null) {
            writeline("/home/ubuntu/results/coverage/Digester/Digester_9_10.coverage", "f5cece10-0a45-42ad-b6fb-47a893ac2b79");
            result = stackAction.onPop(this, stackName, result);
        }
        writeline("/home/ubuntu/results/coverage/Digester/Digester_9_10.coverage", "c8d02659-e683-43ac-b81a-9fb2e2aa7d4c");
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
        writeline("/home/ubuntu/results/coverage/Digester/Digester_9_10.coverage", "d2dc6333-622f-499f-b500-176b54894238");
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
        writeline("/home/ubuntu/results/coverage/Digester/Digester_9_10.coverage", "a2e451d9-95a3-4dfd-b46d-961d9ce0b812");
        T result = null;
        writeline("/home/ubuntu/results/coverage/Digester/Digester_9_10.coverage", "a0b0cd1b-9cec-497f-9469-f3a38b2ea2e2");
        Stack<Object> namedStack = stacksByName.get(stackName);
        writeline("/home/ubuntu/results/coverage/Digester/Digester_9_10.coverage", "885b43f8-1af9-4122-b90c-576b7d339821");
        if (namedStack == null) {
            writeline("/home/ubuntu/results/coverage/Digester/Digester_9_10.coverage", "3dba90e0-8170-4bb0-859f-d4383bd77bf8");
            if (log.isDebugEnabled()) {
                writeline("/home/ubuntu/results/coverage/Digester/Digester_9_10.coverage", "02492deb-c73d-44c7-b9fe-ddf7ebf9b12d");
                log.debug("Stack '" + stackName + "' is empty");
            }
        }
        writeline("/home/ubuntu/results/coverage/Digester/Digester_9_10.coverage", "7c979da9-eca1-407a-b814-21221c665251");
        int index = (namedStack.size() - 1) - n;
        writeline("/home/ubuntu/results/coverage/Digester/Digester_9_10.coverage", "bb51ade3-91a0-4cb1-b6e6-df1c9affae0d");
        if (index < 0) {
        }
        writeline("/home/ubuntu/results/coverage/Digester/Digester_9_10.coverage", "3a8aaaad-1414-4a04-9ba7-046358d5f53a");
        result = this.<T>npeSafeCast(namedStack.get(index));
        writeline("/home/ubuntu/results/coverage/Digester/Digester_9_10.coverage", "b5923023-a094-4240-b77d-089d272ee3d4");
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
        writeline("/home/ubuntu/results/coverage/Digester/Digester_9_10.coverage", "c8443408-3415-4d0d-a570-a939ab2b9226");
        boolean result = true;
        writeline("/home/ubuntu/results/coverage/Digester/Digester_9_10.coverage", "bd1b6df6-447f-43d1-96b9-e3d522d89931");
        Stack<Object> namedStack = stacksByName.get(stackName);
        writeline("/home/ubuntu/results/coverage/Digester/Digester_9_10.coverage", "d877630c-0513-4265-9101-c6cef522a58d");
        if (namedStack != null) {
            writeline("/home/ubuntu/results/coverage/Digester/Digester_9_10.coverage", "87052d50-7e05-43c6-9c48-1d2853c2e67c");
            result = namedStack.isEmpty();
        }
        writeline("/home/ubuntu/results/coverage/Digester/Digester_9_10.coverage", "18678605-5670-458d-b5af-6e2cd2b8cef8");
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
        writeline("/home/ubuntu/results/coverage/Digester/Digester_9_10.coverage", "fe340730-5b1b-4f29-80d9-0078b0dc073e");
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
        writeline("/home/ubuntu/results/coverage/Digester/Digester_9_10.coverage", "79f7e914-3a86-432c-968f-4056ca8d7d3c");
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
        writeline("/home/ubuntu/results/coverage/Digester/Digester_9_10.coverage", "a7630b71-9b5e-4dd6-98b4-8d480bc02b0a");
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
        writeline("/home/ubuntu/results/coverage/Digester/Digester_9_10.coverage", "5d96eea2-bc71-4851-b9a1-2b3d35182605");
        if (configured) {
            writeline("/home/ubuntu/results/coverage/Digester/Digester_9_10.coverage", "3a1389b9-3f00-4895-b151-163246c87d47");
            return;
        }
        writeline("/home/ubuntu/results/coverage/Digester/Digester_9_10.coverage", "f08f08c8-c41c-4ec6-8109-ed62b18e2429");
        initialize();
        writeline("/home/ubuntu/results/coverage/Digester/Digester_9_10.coverage", "3e37e8d7-43e6-4a5b-b15f-23b04e1711df");
        configured = true;
    }

    /**
     * Checks the Digester instance has been configured.
     *
     * @return true, if the Digester instance has been configured, false otherwise
     * @since 3.0
     */
    public boolean isConfigured() {
        writeline("/home/ubuntu/results/coverage/Digester/Digester_9_10.coverage", "c1b27906-cf8b-42aa-a03d-ad8c81e64d5d");
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
        writeline("/home/ubuntu/results/coverage/Digester/Digester_9_10.coverage", "12c6680f-29ca-4fd0-bdfb-455b7689ba65");
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
        writeline("/home/ubuntu/results/coverage/Digester/Digester_9_10.coverage", "ae1e3d44-bcc8-4d98-b3cd-bc74ff31bfcb");
        try {
            writeline("/home/ubuntu/results/coverage/Digester/Digester_9_10.coverage", "2d4edb4d-0ec0-4ce4-84a9-3218dd4940e2");
            return (params.peek());
        } catch (EmptyStackException e) {
            writeline("/home/ubuntu/results/coverage/Digester/Digester_9_10.coverage", "4a8d8fa1-d9ea-49f9-a6c3-3efd2c20552f");
            log.warn("Empty stack (returning null)");
            writeline("/home/ubuntu/results/coverage/Digester/Digester_9_10.coverage", "acd659ac-f541-4385-b7ff-58a8b480478e");
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
        writeline("/home/ubuntu/results/coverage/Digester/Digester_9_10.coverage", "70986683-a577-42d6-bd8e-4364096c9407");
        int index = (params.size() - 1) - n;
        writeline("/home/ubuntu/results/coverage/Digester/Digester_9_10.coverage", "c5ad213a-4508-40b0-81c4-912b3d5fe06c");
        if (index < 0) {
            writeline("/home/ubuntu/results/coverage/Digester/Digester_9_10.coverage", "8164ea58-1f8a-482e-be9e-47d3699fd455");
            log.warn("Empty stack (returning null)");
            writeline("/home/ubuntu/results/coverage/Digester/Digester_9_10.coverage", "1c8e2e68-8eb9-4a2b-9214-1b48c59b9cc7");
            return (null);
        }
        writeline("/home/ubuntu/results/coverage/Digester/Digester_9_10.coverage", "d9e97069-26eb-4c3f-8f40-8e5c0ebcd728");
        try {
            writeline("/home/ubuntu/results/coverage/Digester/Digester_9_10.coverage", "137467a9-ea2d-477f-ac3e-7a04017a891a");
            return (params.get(index));
        } catch (EmptyStackException e) {
            writeline("/home/ubuntu/results/coverage/Digester/Digester_9_10.coverage", "2368b671-93c3-41d8-b189-4b7f22da7738");
            log.warn("Empty stack (returning null)");
            writeline("/home/ubuntu/results/coverage/Digester/Digester_9_10.coverage", "99af700d-651b-42fc-a1a5-e99e720c301e");
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
        writeline("/home/ubuntu/results/coverage/Digester/Digester_9_10.coverage", "34e8a2da-b7a8-42c4-9b2c-bb30f9733177");
        try {
            writeline("/home/ubuntu/results/coverage/Digester/Digester_9_10.coverage", "ec332ce3-1861-4276-a3cd-647a9e0b5e15");
            if (log.isTraceEnabled()) {
                writeline("/home/ubuntu/results/coverage/Digester/Digester_9_10.coverage", "2a190eed-b5d2-49b7-b030-1929dc3d873a");
                log.trace("Popping params");
            }
            writeline("/home/ubuntu/results/coverage/Digester/Digester_9_10.coverage", "23c6be0f-a3f4-456e-bc64-391f29563327");
            return (params.pop());
        } catch (EmptyStackException e) {
            writeline("/home/ubuntu/results/coverage/Digester/Digester_9_10.coverage", "6b1e4264-3fb2-4526-84bb-2d6b8bc411ae");
            log.warn("Empty stack (returning null)");
            writeline("/home/ubuntu/results/coverage/Digester/Digester_9_10.coverage", "99778f33-25a3-467c-9bab-e09530980af6");
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
        writeline("/home/ubuntu/results/coverage/Digester/Digester_9_10.coverage", "de881cb8-e5a4-471b-a782-5ac1714cb7f4");
        if (log.isTraceEnabled()) {
            writeline("/home/ubuntu/results/coverage/Digester/Digester_9_10.coverage", "4e0f80b7-9f72-406a-9926-60283ecf80ca");
            log.trace("Pushing params");
        }
        writeline("/home/ubuntu/results/coverage/Digester/Digester_9_10.coverage", "8f7a0be2-8db8-48be-b239-576638941dc8");
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
        writeline("/home/ubuntu/results/coverage/Digester/Digester_9_10.coverage", "8f4945a4-5140-4982-883b-b7308cb53eab");
        if ((e != null) && (e instanceof InvocationTargetException)) {
            writeline("/home/ubuntu/results/coverage/Digester/Digester_9_10.coverage", "ae27ba5f-2a23-43c4-b464-8fec4cf758d8");
            Throwable t = ((InvocationTargetException) e).getTargetException();
            writeline("/home/ubuntu/results/coverage/Digester/Digester_9_10.coverage", "3c99e885-aae3-4588-8bc9-915a13b2b067");
            if ((t != null) && (t instanceof Exception)) {
                writeline("/home/ubuntu/results/coverage/Digester/Digester_9_10.coverage", "3e4552cd-751a-4ad0-a113-a694b5f1c11e");
                e = (Exception) t;
            }
        }
        writeline("/home/ubuntu/results/coverage/Digester/Digester_9_10.coverage", "912be72d-d899-4c0e-869c-af9475c5105a");
        if (locator != null) {
            writeline("/home/ubuntu/results/coverage/Digester/Digester_9_10.coverage", "ecd0080d-d3d9-4d76-8b4e-095715ea0624");
            String error = "Error at line " + locator.getLineNumber() + " char " + locator.getColumnNumber() + ": " + message;
            writeline("/home/ubuntu/results/coverage/Digester/Digester_9_10.coverage", "d0b03fb2-9ed3-45ad-a86e-5c5d624c09af");
            if (e != null) {
                writeline("/home/ubuntu/results/coverage/Digester/Digester_9_10.coverage", "073896bb-6a75-4819-aebf-e70fd7600511");
                return new SAXParseException(error, locator, e);
            }
            writeline("/home/ubuntu/results/coverage/Digester/Digester_9_10.coverage", "39da37d7-a710-42cc-baf2-b0d8de4cd9fd");
            return new SAXParseException(error, locator);
        }
        writeline("/home/ubuntu/results/coverage/Digester/Digester_9_10.coverage", "8dd7f0e4-432b-4d0e-8272-c6a8fa6fa632");
        log.error("No Locator!");
        writeline("/home/ubuntu/results/coverage/Digester/Digester_9_10.coverage", "a9180d8c-0c4f-47db-9143-b22803de1ca8");
        if (e != null) {
            writeline("/home/ubuntu/results/coverage/Digester/Digester_9_10.coverage", "f3cccb27-0bd3-4b93-8f77-c249309a3dde");
            return new SAXException(message, e);
        }
        writeline("/home/ubuntu/results/coverage/Digester/Digester_9_10.coverage", "2f8ce9a1-fa21-44a2-ba07-fae643d6baa6");
        return new SAXException(message);
    }

    /**
     * Create a SAX exception which also understands about the location in the digester file where the exception occurs
     *
     * @param e the exception cause
     * @return the new SAX exception
     */
    public SAXException createSAXException(Exception e) {
        writeline("/home/ubuntu/results/coverage/Digester/Digester_9_10.coverage", "01bcff15-973f-4996-ac3a-98c615c00803");
        if (e instanceof InvocationTargetException) {
            writeline("/home/ubuntu/results/coverage/Digester/Digester_9_10.coverage", "f4319c18-81b5-4f0e-8d2c-8db4a69af747");
            Throwable t = ((InvocationTargetException) e).getTargetException();
            writeline("/home/ubuntu/results/coverage/Digester/Digester_9_10.coverage", "0162492d-3a61-4c8e-8c88-05a710a363c1");
            if ((t != null) && (t instanceof Exception)) {
                writeline("/home/ubuntu/results/coverage/Digester/Digester_9_10.coverage", "58002596-60b0-4ff7-b48a-137036cc797f");
                e = (Exception) t;
            }
        }
        writeline("/home/ubuntu/results/coverage/Digester/Digester_9_10.coverage", "ad1a775c-9fb6-4df4-984d-9512fb5932b2");
        return createSAXException(e.getMessage(), e);
    }

    /**
     * Create a SAX exception which also understands about the location in the digester file where the exception occurs
     *
     * @param message the custom SAX exception message
     * @return the new SAX exception
     */
    public SAXException createSAXException(String message) {
        writeline("/home/ubuntu/results/coverage/Digester/Digester_9_10.coverage", "6ce0b3dd-fec2-47c0-bc84-88a20bda8019");
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
        writeline("/home/ubuntu/results/coverage/Digester/Digester_9_10.coverage", "6c64c42e-2636-4d6a-aa4a-d9c6fa2b7bb7");
        if (obj == null) {
            writeline("/home/ubuntu/results/coverage/Digester/Digester_9_10.coverage", "6f29fade-7313-432d-a82d-c1706cc5ef85");
            return null;
        }
        writeline("/home/ubuntu/results/coverage/Digester/Digester_9_10.coverage", "ecf93c6f-80f8-4588-a10e-f28c9fd6eb02");
        @SuppressWarnings("unchecked") T result = (T) obj;
        writeline("/home/ubuntu/results/coverage/Digester/Digester_9_10.coverage", "b2f200e8-b0fb-4473-b3f8-7a85fc92ed0e");
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
