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
        writeline("/home/ubuntu/results/coverage/Digester/Digester_7_10.coverage", "0cb26edf-3860-4272-b0ec-fe26698fcdd0");
        Stack<String> nsStack = namespaces.get(prefix);
        writeline("/home/ubuntu/results/coverage/Digester/Digester_7_10.coverage", "9449bb28-dff3-4c40-8a71-0a3552214c70");
        if (nsStack == null) {
            writeline("/home/ubuntu/results/coverage/Digester/Digester_7_10.coverage", "4ad9202b-a7d0-4b89-b35d-2b90652c6f59");
            return null;
        }
        writeline("/home/ubuntu/results/coverage/Digester/Digester_7_10.coverage", "645a4bbe-5340-42fc-995b-b349f17ccaf8");
        try {
            writeline("/home/ubuntu/results/coverage/Digester/Digester_7_10.coverage", "71b470e5-74b3-4be4-8296-feb5eb3f5dc3");
            return (nsStack.peek());
        } catch (EmptyStackException e) {
            writeline("/home/ubuntu/results/coverage/Digester/Digester_7_10.coverage", "e1658008-6dc5-4a3c-aacf-428946ff2348");
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
        writeline("/home/ubuntu/results/coverage/Digester/Digester_7_10.coverage", "18d4a7a2-fc73-4878-9ad3-7493c14885b2");
        if (this.classLoader != null) {
            writeline("/home/ubuntu/results/coverage/Digester/Digester_7_10.coverage", "c02330da-89b5-423b-937a-2298a2baf4a1");
            return (this.classLoader);
        }
        writeline("/home/ubuntu/results/coverage/Digester/Digester_7_10.coverage", "92fc9fd9-bc4d-415a-8ec5-fe12ced6e345");
        if (this.useContextClassLoader) {
            writeline("/home/ubuntu/results/coverage/Digester/Digester_7_10.coverage", "1e715fa9-1b96-41c6-8bd8-52567eccd9e0");
            ClassLoader classLoader = Thread.currentThread().getContextClassLoader();
            writeline("/home/ubuntu/results/coverage/Digester/Digester_7_10.coverage", "c43babdd-91ac-4a7e-9996-cb9878236adf");
            if (classLoader != null) {
                writeline("/home/ubuntu/results/coverage/Digester/Digester_7_10.coverage", "c55823c5-2e3b-4f24-8809-d43def6ba368");
                return (classLoader);
            }
        }
        writeline("/home/ubuntu/results/coverage/Digester/Digester_7_10.coverage", "5b6eae8a-3c81-4f58-b721-91c949e7bfb0");
        return (this.getClass().getClassLoader());
    }

    /**
     * Set the class loader to be used for instantiating application objects when required.
     *
     * @param classLoader The new class loader to use, or <code>null</code> to revert to the standard rules
     */
    public void setClassLoader(ClassLoader classLoader) {
        writeline("/home/ubuntu/results/coverage/Digester/Digester_7_10.coverage", "18a0bae4-7764-44fb-a62f-d84d2137bd35");
        this.classLoader = classLoader;
    }

    /**
     * Return the current depth of the element stack.
     *
     * @return the current depth of the element stack.
     */
    public int getCount() {
        writeline("/home/ubuntu/results/coverage/Digester/Digester_7_10.coverage", "410a699e-c321-421f-8d4a-daad856f4354");
        return (stack.size());
    }

    /**
     * Return the name of the XML element that is currently being processed.
     *
     * @return the name of the XML element that is currently being processed.
     */
    public String getCurrentElementName() {
        writeline("/home/ubuntu/results/coverage/Digester/Digester_7_10.coverage", "a0481318-fe70-4dee-b79b-fb4e6ecb41d0");
        String elementName = match;
        writeline("/home/ubuntu/results/coverage/Digester/Digester_7_10.coverage", "3f493934-45a6-495b-aaf8-fc2e5a466ae7");
        int lastSlash = elementName.lastIndexOf('/');
        writeline("/home/ubuntu/results/coverage/Digester/Digester_7_10.coverage", "97824040-85d0-43d8-9857-fbbff0468709");
        if (lastSlash >= 0) {
            writeline("/home/ubuntu/results/coverage/Digester/Digester_7_10.coverage", "3f2993ab-e2b2-40a2-b889-dcf275539955");
            elementName = elementName.substring(lastSlash + 1);
        }
        writeline("/home/ubuntu/results/coverage/Digester/Digester_7_10.coverage", "ec2e3f71-e5d2-4d01-ac46-b8eed2ff014a");
        return (elementName);
    }

    /**
     * Return the error handler for this Digester.
     *
     * @return the error handler for this Digester.
     */
    public ErrorHandler getErrorHandler() {
        writeline("/home/ubuntu/results/coverage/Digester/Digester_7_10.coverage", "9005bd3a-4163-4ace-afed-cfc6de254311");
        return (this.errorHandler);
    }

    /**
     * Set the error handler for this Digester.
     *
     * @param errorHandler The new error handler
     */
    public void setErrorHandler(ErrorHandler errorHandler) {
        writeline("/home/ubuntu/results/coverage/Digester/Digester_7_10.coverage", "14c02391-8472-4453-8ca3-f4e989315d2e");
        this.errorHandler = errorHandler;
    }

    /**
     * Return the SAXParserFactory we will use, creating one if necessary.
     *
     * @return the SAXParserFactory we will use, creating one if necessary.
     */
    public SAXParserFactory getFactory() {
        writeline("/home/ubuntu/results/coverage/Digester/Digester_7_10.coverage", "ff715bad-11b4-41a6-9a22-b1794ce9fe26");
        if (factory == null) {
            writeline("/home/ubuntu/results/coverage/Digester/Digester_7_10.coverage", "327284bd-583d-4b13-a019-b90f1a438d7d");
            factory = SAXParserFactory.newInstance();
            writeline("/home/ubuntu/results/coverage/Digester/Digester_7_10.coverage", "76ade60e-09ff-49d4-9013-fe918d02ec7a");
            factory.setNamespaceAware(namespaceAware);
            writeline("/home/ubuntu/results/coverage/Digester/Digester_7_10.coverage", "f9d0e4f4-1b03-48f6-b829-6d2a7f33485e");
            factory.setXIncludeAware(xincludeAware);
            writeline("/home/ubuntu/results/coverage/Digester/Digester_7_10.coverage", "644f1113-72bb-4e59-97ba-05d32b5774f7");
            factory.setValidating(validating);
            writeline("/home/ubuntu/results/coverage/Digester/Digester_7_10.coverage", "5ef52d31-0abb-4f80-a1ec-351c00d66261");
            factory.setSchema(schema);
        }
        writeline("/home/ubuntu/results/coverage/Digester/Digester_7_10.coverage", "0aafdaa0-e929-4174-a389-ab5df44b91d0");
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
        writeline("/home/ubuntu/results/coverage/Digester/Digester_7_10.coverage", "e3d14d79-180d-473e-8330-5183a0e205bd");
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
        writeline("/home/ubuntu/results/coverage/Digester/Digester_7_10.coverage", "d0d61722-cf44-4a2b-af82-75be53a124ca");
        getFactory().setFeature(feature, value);
    }

    /**
     * Return the current Logger associated with this instance of the Digester
     *
     * @return the current Logger associated with this instance of the Digester
     */
    public Log getLogger() {
        writeline("/home/ubuntu/results/coverage/Digester/Digester_7_10.coverage", "6ef61664-ca2e-4a27-8eb9-66d60b1d5b6a");
        return log;
    }

    /**
     * Set the current logger for this Digester.
     *
     * @param log the current logger for this Digester.
     */
    public void setLogger(Log log) {
        writeline("/home/ubuntu/results/coverage/Digester/Digester_7_10.coverage", "46c94edf-e6e8-4227-8c49-9b8f7ea529e4");
        this.log = log;
    }

    /**
     * Gets the logger used for logging SAX-related information. <strong>Note</strong> the output is finely grained.
     *
     * @return the logger used for logging SAX-related information
     * @since 1.6
     */
    public Log getSAXLogger() {
        writeline("/home/ubuntu/results/coverage/Digester/Digester_7_10.coverage", "00603283-bb24-45bf-8302-9cb4c99ce76f");
        return saxLog;
    }

    /**
     * Sets the logger used for logging SAX-related information. <strong>Note</strong> the output is finely grained.
     *
     * @param saxLog the logger used for logging SAX-related information, not null
     * @since 1.6
     */
    public void setSAXLogger(Log saxLog) {
        writeline("/home/ubuntu/results/coverage/Digester/Digester_7_10.coverage", "c7fbb27e-b6e4-4f0b-9dc7-e3d60261406f");
        this.saxLog = saxLog;
    }

    /**
     * Return the current rule match path
     *
     * @return the current rule match path
     */
    public String getMatch() {
        writeline("/home/ubuntu/results/coverage/Digester/Digester_7_10.coverage", "07ae110f-fcfc-4103-be85-4d9ad9227c5e");
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
        writeline("/home/ubuntu/results/coverage/Digester/Digester_7_10.coverage", "9a0a8dda-7fa0-4f68-95de-33867df62177");
        return matches;
    }

    /**
     * Return the "namespace aware" flag for parsers we create.
     *
     * @return the "namespace aware" flag for parsers we create.
     */
    public boolean getNamespaceAware() {
        writeline("/home/ubuntu/results/coverage/Digester/Digester_7_10.coverage", "9d3773d6-230b-403f-be07-9b427b92992a");
        return (this.namespaceAware);
    }

    /**
     * Set the "namespace aware" flag for parsers we create.
     *
     * @param namespaceAware The new "namespace aware" flag
     */
    public void setNamespaceAware(boolean namespaceAware) {
        writeline("/home/ubuntu/results/coverage/Digester/Digester_7_10.coverage", "0fcca20f-06c4-4808-ae0e-49d6cd20f693");
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
        writeline("/home/ubuntu/results/coverage/Digester/Digester_7_10.coverage", "c260fca6-995b-4781-80c8-559dd1bf4a01");
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
        writeline("/home/ubuntu/results/coverage/Digester/Digester_7_10.coverage", "b22a5a51-2e28-48d1-ac39-34941ccfd2ae");
        this.xincludeAware = xincludeAware;
    }

    /**
     * Set the public id of the current file being parse.
     *
     * @param publicId the DTD/Schema public's id.
     */
    public void setPublicId(String publicId) {
        writeline("/home/ubuntu/results/coverage/Digester/Digester_7_10.coverage", "354aa383-f0a5-4e09-aedb-15555e3518f8");
        this.publicId = publicId;
    }

    /**
     * Return the public identifier of the DTD we are currently parsing under, if any.
     *
     * @return the public identifier of the DTD we are currently parsing under, if any.
     */
    public String getPublicId() {
        writeline("/home/ubuntu/results/coverage/Digester/Digester_7_10.coverage", "0b44cbf2-db3b-4de9-87f8-35ff6ad5f34e");
        return (this.publicId);
    }

    /**
     * Return the namespace URI that will be applied to all subsequently added <code>Rule</code> objects.
     *
     * @return the namespace URI that will be applied to all subsequently added <code>Rule</code> objects.
     */
    public String getRuleNamespaceURI() {
        writeline("/home/ubuntu/results/coverage/Digester/Digester_7_10.coverage", "ca6647db-44e7-453c-a268-8d00fd646cc6");
        return (getRules().getNamespaceURI());
    }

    /**
     * Set the namespace URI that will be applied to all subsequently added <code>Rule</code> objects.
     *
     * @param ruleNamespaceURI Namespace URI that must match on all subsequently added rules, or <code>null</code> for
     *            matching regardless of the current namespace URI
     */
    public void setRuleNamespaceURI(String ruleNamespaceURI) {
        writeline("/home/ubuntu/results/coverage/Digester/Digester_7_10.coverage", "928e0f00-0637-482f-aaab-fd089d1db249");
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
        writeline("/home/ubuntu/results/coverage/Digester/Digester_7_10.coverage", "cbaad6a2-ca5d-4deb-b5d4-61ee6255c2ef");
        if (parser != null) {
            writeline("/home/ubuntu/results/coverage/Digester/Digester_7_10.coverage", "49514e9f-e392-4472-95be-abfe4e64dcbb");
            return (parser);
        }
        writeline("/home/ubuntu/results/coverage/Digester/Digester_7_10.coverage", "1a465f92-687a-4d0a-a051-9b3a689f0901");
        try {
            writeline("/home/ubuntu/results/coverage/Digester/Digester_7_10.coverage", "1e883b12-3c48-4583-994b-030f4062c45d");
            parser = getFactory().newSAXParser();
        } catch (Exception e) {
            writeline("/home/ubuntu/results/coverage/Digester/Digester_7_10.coverage", "61547738-be43-4f80-8b3d-8a5f3d3d40db");
            log.error("Digester.getParser: ", e);
            writeline("/home/ubuntu/results/coverage/Digester/Digester_7_10.coverage", "ea912e54-1819-4217-b929-d92a1212a239");
            return (null);
        }
        writeline("/home/ubuntu/results/coverage/Digester/Digester_7_10.coverage", "a0d5567a-0932-457c-a875-ac00c5319b61");
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
        writeline("/home/ubuntu/results/coverage/Digester/Digester_7_10.coverage", "3c18e133-ea57-4ab7-9919-ca8fce7772f6");
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
        writeline("/home/ubuntu/results/coverage/Digester/Digester_7_10.coverage", "5cdd16d2-3b52-4c07-b822-5db04af65d4a");
        getParser().setProperty(property, value);
    }

    /**
     * Return the <code>Rules</code> implementation object containing our rules collection and associated matching
     * policy. If none has been established, a default implementation will be created and returned.
     *
     * @return the <code>Rules</code> implementation object.
     */
    public Rules getRules() {
        writeline("/home/ubuntu/results/coverage/Digester/Digester_7_10.coverage", "7100d0b3-304f-4e00-81e9-28c555532d90");
        if (this.rules == null) {
            writeline("/home/ubuntu/results/coverage/Digester/Digester_7_10.coverage", "7bc77deb-8950-4b42-9159-5dba18edb028");
            this.rules = new RulesBase();
            writeline("/home/ubuntu/results/coverage/Digester/Digester_7_10.coverage", "ff2ce7be-a9ee-404b-ae29-26c4211d5dcf");
            this.rules.setDigester(this);
        }
        writeline("/home/ubuntu/results/coverage/Digester/Digester_7_10.coverage", "3278e05f-66cf-4d6b-8564-15157394357b");
        return (this.rules);
    }

    /**
     * Set the <code>Rules</code> implementation object containing our rules collection and associated matching policy.
     *
     * @param rules New Rules implementation
     */
    public void setRules(Rules rules) {
        writeline("/home/ubuntu/results/coverage/Digester/Digester_7_10.coverage", "5f585ff0-14ed-40a4-8437-8f9ef140542e");
        this.rules = rules;
        writeline("/home/ubuntu/results/coverage/Digester/Digester_7_10.coverage", "29670889-fa45-4d07-8b82-76be9487e5e4");
        this.rules.setDigester(this);
    }

    /**
     * Return the XML Schema used when parsing.
     *
     * @return The {@link Schema} instance in use.
     * @since 2.0
     */
    public Schema getXMLSchema() {
        writeline("/home/ubuntu/results/coverage/Digester/Digester_7_10.coverage", "f24d6f58-907c-4735-a7c5-74194b2f1545");
        return (this.schema);
    }

    /**
     * Set the XML Schema to be used when parsing.
     *
     * @param schema The {@link Schema} instance to use.
     * @since 2.0
     */
    public void setXMLSchema(Schema schema) {
        writeline("/home/ubuntu/results/coverage/Digester/Digester_7_10.coverage", "a02d831d-e497-4ee4-8f64-c61104621173");
        this.schema = schema;
    }

    /**
     * Return the boolean as to whether the context ClassLoader should be used.
     *
     * @return true, if the context ClassLoader should be used, false otherwise.
     */
    public boolean getUseContextClassLoader() {
        writeline("/home/ubuntu/results/coverage/Digester/Digester_7_10.coverage", "0ebf290d-bd44-42bf-9aca-b29fec4b29cf");
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
        writeline("/home/ubuntu/results/coverage/Digester/Digester_7_10.coverage", "d92092ed-35b3-41f4-8835-e142c76cc94f");
        useContextClassLoader = use;
    }

    /**
     * Return the validating parser flag.
     *
     * @return the validating parser flag.
     */
    public boolean getValidating() {
        writeline("/home/ubuntu/results/coverage/Digester/Digester_7_10.coverage", "b83ba213-82df-4326-b2b9-3929c1f1fec8");
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
        writeline("/home/ubuntu/results/coverage/Digester/Digester_7_10.coverage", "2864b945-ff78-4138-bb26-a854c1746729");
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
        writeline("/home/ubuntu/results/coverage/Digester/Digester_7_10.coverage", "e462334a-ed34-460b-9beb-3e8fde52cede");
        if (reader == null) {
            writeline("/home/ubuntu/results/coverage/Digester/Digester_7_10.coverage", "c3d743df-f217-4374-8165-c9ccf3b73450");
            reader = getParser().getXMLReader();
        }
        writeline("/home/ubuntu/results/coverage/Digester/Digester_7_10.coverage", "890994a3-6876-44c5-94d8-df6709cda285");
        reader.setDTDHandler(this);
        writeline("/home/ubuntu/results/coverage/Digester/Digester_7_10.coverage", "a2a69a6d-3a36-4149-98f9-6a8c93f3df24");
        reader.setContentHandler(this);
        writeline("/home/ubuntu/results/coverage/Digester/Digester_7_10.coverage", "e04701b7-080d-4c4c-b661-fb4cc1808a76");
        if (entityResolver == null) {
            writeline("/home/ubuntu/results/coverage/Digester/Digester_7_10.coverage", "72c4b761-acda-4d64-a305-280185ba1d16");
            reader.setEntityResolver(this);
        } else {
            writeline("/home/ubuntu/results/coverage/Digester/Digester_7_10.coverage", "e2881e4f-388e-46c8-a11b-5989355ec1cf");
            reader.setEntityResolver(entityResolver);
        }
        writeline("/home/ubuntu/results/coverage/Digester/Digester_7_10.coverage", "9e57dbee-f7e0-4d57-a74b-7cba95e22043");
        if (this.errorHandler != null) {
            writeline("/home/ubuntu/results/coverage/Digester/Digester_7_10.coverage", "f5e14e51-c717-4ba7-8f12-cd89359ba256");
            reader.setErrorHandler(this.errorHandler);
        } else {
            writeline("/home/ubuntu/results/coverage/Digester/Digester_7_10.coverage", "20968515-5e4f-4d53-b62a-3d6f1f14ec43");
            reader.setErrorHandler(this);
        }
        writeline("/home/ubuntu/results/coverage/Digester/Digester_7_10.coverage", "9c7ec6d2-bdf8-4c5f-96b6-14a6a0704890");
        return reader;
    }

    /**
     * Gets the <code>Substitutor</code> used to convert attributes and body text.
     *
     * @return the <code>Substitutor</code> used to convert attributes and body text,
     *         null if not substitutions are to be performed.
     */
    public Substitutor getSubstitutor() {
        writeline("/home/ubuntu/results/coverage/Digester/Digester_7_10.coverage", "5e47c44a-16f2-424c-8687-277678b33ee9");
        return substitutor;
    }

    /**
     * Sets the <code>Substitutor</code> to be used to convert attributes and body text.
     *
     * @param substitutor the Substitutor to be used to convert attributes and body text or null if not substitution of
     *            these values is to be performed.
     */
    public void setSubstitutor(Substitutor substitutor) {
        writeline("/home/ubuntu/results/coverage/Digester/Digester_7_10.coverage", "23c37d46-daa2-4a57-8b2e-e5905fb00b9c");
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
        writeline("/home/ubuntu/results/coverage/Digester/Digester_7_10.coverage", "2517c1ba-0094-4511-aca7-51813e6faec5");
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
        writeline("/home/ubuntu/results/coverage/Digester/Digester_7_10.coverage", "9cb94169-6f3e-40b6-a776-5823de1a80ac");
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
        writeline("/home/ubuntu/results/coverage/Digester/Digester_7_10.coverage", "de608f62-a28f-4168-95f2-046adf2841dc");
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
        writeline("/home/ubuntu/results/coverage/Digester/Digester_7_10.coverage", "bda1c6e0-11c4-421d-b42d-da708fd03966");
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
        writeline("/home/ubuntu/results/coverage/Digester/Digester_7_10.coverage", "a137af8c-3231-4f04-bb3b-734a2c886452");
        if (!namespaceAware) {
            writeline("/home/ubuntu/results/coverage/Digester/Digester_7_10.coverage", "09867b98-ca13-477d-9c5b-94047879c25c");
            log.warn("Digester is not namespace aware");
        }
        writeline("/home/ubuntu/results/coverage/Digester/Digester_7_10.coverage", "588b214e-9329-4186-9300-9167f0a22c93");
        Map<String, String> currentNamespaces = new HashMap<String, String>();
        writeline("/home/ubuntu/results/coverage/Digester/Digester_7_10.coverage", "f48743ef-e965-435a-bc4f-f610e65910d6");
        return currentNamespaces;
    }

    /**
     * Returns the executor service used to run asynchronous parse method.
     *
     * @return the executor service used to run asynchronous parse method
     * @since 3.1
     */
    public ExecutorService getExecutorService() {
        writeline("/home/ubuntu/results/coverage/Digester/Digester_7_10.coverage", "065db6e3-94d6-4544-b9da-2466ff1404e2");
        return executorService;
    }

    /**
     * Sets the executor service to run asynchronous parse method.
     *
     * @param executorService the executor service to run asynchronous parse method
     * @since 3.1
     */
    public void setExecutorService(ExecutorService executorService) {
        writeline("/home/ubuntu/results/coverage/Digester/Digester_7_10.coverage", "ea97f6ef-12dd-4c1e-b9bc-df8d3f2d5bfe");
        this.executorService = executorService;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public void characters(char buffer[], int start, int length) throws SAXException {
        writeline("/home/ubuntu/results/coverage/Digester/Digester_7_10.coverage", "4cb3bc62-fa9f-44a1-b775-07bc0c6cea7f");
        if (customContentHandler != null) {
            writeline("/home/ubuntu/results/coverage/Digester/Digester_7_10.coverage", "ce335d4b-6c87-488f-af1e-ac14863beaa1");
            customContentHandler.characters(buffer, start, length);
            writeline("/home/ubuntu/results/coverage/Digester/Digester_7_10.coverage", "0a87947b-7db2-419b-8071-a51cef838291");
            return;
        }
        writeline("/home/ubuntu/results/coverage/Digester/Digester_7_10.coverage", "d445476f-e83a-480a-9a01-efc72eb2ae28");
        if (saxLog.isDebugEnabled()) {
            writeline("/home/ubuntu/results/coverage/Digester/Digester_7_10.coverage", "fbd03d38-500e-425c-a77a-c86a49375942");
            saxLog.debug("characters(" + new String(buffer, start, length) + ")");
        }
        writeline("/home/ubuntu/results/coverage/Digester/Digester_7_10.coverage", "ff4cd49a-35b0-47be-b947-e81039821cde");
        bodyText.append(buffer, start, length);
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public void endDocument() throws SAXException {
        writeline("/home/ubuntu/results/coverage/Digester/Digester_7_10.coverage", "8a719561-ad63-4432-9a44-3963e5383317");
        if (saxLog.isDebugEnabled()) {
            writeline("/home/ubuntu/results/coverage/Digester/Digester_7_10.coverage", "13516e4f-beee-4169-ae7e-dac79afbe16c");
            if (getCount() > 1) {
                writeline("/home/ubuntu/results/coverage/Digester/Digester_7_10.coverage", "49a802c3-ca54-4ef3-957c-a9d75684980d");
                saxLog.debug("endDocument():  " + getCount() + " elements left");
            } else {
                writeline("/home/ubuntu/results/coverage/Digester/Digester_7_10.coverage", "af11ddd6-ae12-4290-b80f-695b57e017f1");
                saxLog.debug("endDocument()");
            }
        }
        writeline("/home/ubuntu/results/coverage/Digester/Digester_7_10.coverage", "8e358ad9-d865-4e5f-82cb-df67b00eb025");
        clear();
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public void endElement(String namespaceURI, String localName, String qName) throws SAXException {
        writeline("/home/ubuntu/results/coverage/Digester/Digester_7_10.coverage", "0fba0ff6-c27e-43a5-b53a-3444d2dd8587");
        if (customContentHandler != null) {
            writeline("/home/ubuntu/results/coverage/Digester/Digester_7_10.coverage", "87b9f3f8-fae1-4b4c-9cab-d303ad2d0611");
            customContentHandler.endElement(namespaceURI, localName, qName);
            writeline("/home/ubuntu/results/coverage/Digester/Digester_7_10.coverage", "1a31fd6a-36f6-4731-ae42-b766f7c3768d");
            return;
        }
        writeline("/home/ubuntu/results/coverage/Digester/Digester_7_10.coverage", "af8d7300-86ad-495d-8301-340dc2deb8b6");
        boolean debug = log.isDebugEnabled();
        writeline("/home/ubuntu/results/coverage/Digester/Digester_7_10.coverage", "f88cc298-da88-4ffe-a009-f2a58673656c");
        if (debug) {
            writeline("/home/ubuntu/results/coverage/Digester/Digester_7_10.coverage", "2f606ae9-50f0-46b7-9598-5d2f46d7ffae");
            if (saxLog.isDebugEnabled()) {
                writeline("/home/ubuntu/results/coverage/Digester/Digester_7_10.coverage", "7a50a44c-dbb2-4c23-a04a-5ac1f975d6e3");
                saxLog.debug("endElement(" + namespaceURI + "," + localName + "," + qName + ")");
            }
            writeline("/home/ubuntu/results/coverage/Digester/Digester_7_10.coverage", "fe69e888-0fcc-44b9-86dd-57714c09491d");
            log.debug("  match='" + match + "'");
            writeline("/home/ubuntu/results/coverage/Digester/Digester_7_10.coverage", "8c1dd954-a203-4ca6-91dc-8410616de5b0");
            log.debug("  bodyText='" + bodyText + "'");
        }
        writeline("/home/ubuntu/results/coverage/Digester/Digester_7_10.coverage", "92f96288-9ef0-44bd-9c34-7f9055a4a223");
        String name = localName;
        writeline("/home/ubuntu/results/coverage/Digester/Digester_7_10.coverage", "26a59f91-fc39-4927-adaf-ead9d2f98248");
        if ((name == null) || (name.length() < 1)) {
            writeline("/home/ubuntu/results/coverage/Digester/Digester_7_10.coverage", "02ac3f39-7a32-426e-af17-a0cc56efc637");
            name = qName;
        }
        writeline("/home/ubuntu/results/coverage/Digester/Digester_7_10.coverage", "9e32e560-51df-4860-b063-6ec426dd8121");
        List<Rule> rules = matches.pop();
        writeline("/home/ubuntu/results/coverage/Digester/Digester_7_10.coverage", "e91293bc-42fa-4375-84ca-dc5d351e4314");
        if ((rules != null) && (rules.size() > 0)) {
            writeline("/home/ubuntu/results/coverage/Digester/Digester_7_10.coverage", "fb1ded57-a2ff-45f1-953f-1cadd357fa44");
            String bodyText = this.bodyText.toString();
            writeline("/home/ubuntu/results/coverage/Digester/Digester_7_10.coverage", "e67f70b2-a230-4136-9747-822c0a683522");
            Substitutor substitutor = getSubstitutor();
            writeline("/home/ubuntu/results/coverage/Digester/Digester_7_10.coverage", "d4c3c9f8-7c05-49ea-86b4-d5ef8471afc4");
            if (substitutor != null) {
                writeline("/home/ubuntu/results/coverage/Digester/Digester_7_10.coverage", "03c9fe07-f677-410e-bb9f-c54762a13b96");
                bodyText = substitutor.substitute(bodyText);
            }
            writeline("/home/ubuntu/results/coverage/Digester/Digester_7_10.coverage", "36df865f-a8f3-4d19-9fbf-08fa94e54fae");
            for (int i = 0; i < rules.size(); i++) {
                writeline("/home/ubuntu/results/coverage/Digester/Digester_7_10.coverage", "273cb42b-7634-46a4-a11a-8a063993a593");
                try {
                    writeline("/home/ubuntu/results/coverage/Digester/Digester_7_10.coverage", "655ffa45-d88d-42bd-8c55-55e28f8c320c");
                    Rule rule = rules.get(i);
                    writeline("/home/ubuntu/results/coverage/Digester/Digester_7_10.coverage", "3ee0593b-0cbe-4abe-bcb3-0d8b42ac48c7");
                    if (debug) {
                        writeline("/home/ubuntu/results/coverage/Digester/Digester_7_10.coverage", "ea73bb6e-b4f4-4f77-9fe6-cc34ee60208e");
                        log.debug("  Fire body() for " + rule);
                    }
                    writeline("/home/ubuntu/results/coverage/Digester/Digester_7_10.coverage", "e782b175-a7ef-499b-a20e-63d6971fe4c2");
                    rule.body(namespaceURI, name, bodyText);
                } catch (Exception e) {
                    writeline("/home/ubuntu/results/coverage/Digester/Digester_7_10.coverage", "8ef43bba-70b3-4c40-91f5-f8a29d7a9f3a");
                    log.error("Body event threw exception", e);
                } catch (Error e) {
                    writeline("/home/ubuntu/results/coverage/Digester/Digester_7_10.coverage", "5a83aab8-c145-4803-86e8-1f3fa98ac39d");
                    log.error("Body event threw error", e);
                }
            }
        } else {
            writeline("/home/ubuntu/results/coverage/Digester/Digester_7_10.coverage", "63fc2d32-c88b-4d3f-b29c-8f5dffd71677");
            if (debug) {
                writeline("/home/ubuntu/results/coverage/Digester/Digester_7_10.coverage", "8ab57613-d0c7-4adb-a0df-9820a42dea01");
                log.debug("  No rules found matching '" + match + "'.");
            }
        }
        writeline("/home/ubuntu/results/coverage/Digester/Digester_7_10.coverage", "59206308-87f4-472c-a9a7-0d19c664e24a");
        bodyText = bodyTexts.pop();
        writeline("/home/ubuntu/results/coverage/Digester/Digester_7_10.coverage", "df8829fb-c3cd-4d3e-972e-b4ceed0e156a");
        if (debug) {
            writeline("/home/ubuntu/results/coverage/Digester/Digester_7_10.coverage", "e13d0af4-1a63-46e6-a4f5-3799b4b14a2b");
            log.debug("  Popping body text '" + bodyText.toString() + "'");
        }
        writeline("/home/ubuntu/results/coverage/Digester/Digester_7_10.coverage", "1287dbea-a523-47e2-ac1a-e7da33ec11f5");
        if (rules != null) {
            writeline("/home/ubuntu/results/coverage/Digester/Digester_7_10.coverage", "536d0bd2-40fa-4da6-9721-980e15dc56a4");
            for (int i = 0; i < rules.size(); i++) {
                writeline("/home/ubuntu/results/coverage/Digester/Digester_7_10.coverage", "4701e596-888f-43d3-b9e7-f8ccea97a258");
                int j = (rules.size() - i) - 1;
                writeline("/home/ubuntu/results/coverage/Digester/Digester_7_10.coverage", "d14cbecf-7edf-48fc-9feb-49efd78d088f");
                try {
                    writeline("/home/ubuntu/results/coverage/Digester/Digester_7_10.coverage", "d15f4c3b-8acc-4327-9b8b-4b25ff3e4453");
                    Rule rule = rules.get(j);
                    writeline("/home/ubuntu/results/coverage/Digester/Digester_7_10.coverage", "ede8982b-53f0-4960-88bc-8e92493213bc");
                    if (debug) {
                        writeline("/home/ubuntu/results/coverage/Digester/Digester_7_10.coverage", "7426dde7-bada-48d5-bf5d-4ed7a6051266");
                        log.debug("  Fire end() for " + rule);
                    }
                    writeline("/home/ubuntu/results/coverage/Digester/Digester_7_10.coverage", "99148623-b4d1-4eb2-89e4-0ea99f8b1529");
                    rule.end(namespaceURI, name);
                } catch (Exception e) {
                    writeline("/home/ubuntu/results/coverage/Digester/Digester_7_10.coverage", "7ee0c747-f20d-4a86-9174-3c40d668a45c");
                    log.error("End event threw exception", e);
                } catch (Error e) {
                    writeline("/home/ubuntu/results/coverage/Digester/Digester_7_10.coverage", "8f58e734-d2bf-4138-82d7-a1fa0cc8e180");
                    log.error("End event threw error", e);
                }
            }
        }
        writeline("/home/ubuntu/results/coverage/Digester/Digester_7_10.coverage", "fbb35ac0-a2c2-4bcc-a391-d27175dbebc9");
        int slash = match.lastIndexOf('/');
        writeline("/home/ubuntu/results/coverage/Digester/Digester_7_10.coverage", "fb96fe1b-922c-4c3d-99be-7798eebf4467");
        if (slash >= 0) {
            writeline("/home/ubuntu/results/coverage/Digester/Digester_7_10.coverage", "ad065f6b-3d0a-4d3f-81db-d5297256cf9c");
            match = match.substring(0, slash);
        } else {
            writeline("/home/ubuntu/results/coverage/Digester/Digester_7_10.coverage", "bd361db1-9743-4de0-92af-1aaa6a7fd92d");
            match = "";
        }
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public void endPrefixMapping(String prefix) throws SAXException {
        writeline("/home/ubuntu/results/coverage/Digester/Digester_7_10.coverage", "074c03e0-72b7-4b15-9c13-e9973206ef46");
        if (saxLog.isDebugEnabled()) {
            writeline("/home/ubuntu/results/coverage/Digester/Digester_7_10.coverage", "2e9b3669-8074-4ace-b41c-1dfaf5041c21");
            saxLog.debug("endPrefixMapping(" + prefix + ")");
        }
        writeline("/home/ubuntu/results/coverage/Digester/Digester_7_10.coverage", "fbf49b0b-af12-4103-b094-edff89bbeaaf");
        Stack<String> stack = namespaces.get(prefix);
        writeline("/home/ubuntu/results/coverage/Digester/Digester_7_10.coverage", "77809a42-e927-41cb-8d04-f411a395bad6");
        if (stack == null) {
            writeline("/home/ubuntu/results/coverage/Digester/Digester_7_10.coverage", "c081c7f9-1b0a-4cef-81e7-473601ff0c9f");
            return;
        }
        writeline("/home/ubuntu/results/coverage/Digester/Digester_7_10.coverage", "d3abc9f6-cdb2-4d88-8bd0-96b457373a9d");
        try {
            writeline("/home/ubuntu/results/coverage/Digester/Digester_7_10.coverage", "38e8bb58-ebd9-46ae-9291-9b6ded6ed14a");
            stack.pop();
            writeline("/home/ubuntu/results/coverage/Digester/Digester_7_10.coverage", "b68f63cd-17d4-4f14-bd86-fc123a188a58");
            if (stack.empty()) {
                writeline("/home/ubuntu/results/coverage/Digester/Digester_7_10.coverage", "85ec4204-53f4-4e52-b35f-463d18bbac02");
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
        writeline("/home/ubuntu/results/coverage/Digester/Digester_7_10.coverage", "bfa064be-53f0-4ece-a5ae-b044f89f9dd8");
        if (saxLog.isDebugEnabled()) {
            writeline("/home/ubuntu/results/coverage/Digester/Digester_7_10.coverage", "d0d07525-3bd3-4f87-ba77-8a7c6a1de0ac");
            saxLog.debug("ignorableWhitespace(" + new String(buffer, start, len) + ")");
        }
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public void processingInstruction(String target, String data) throws SAXException {
        writeline("/home/ubuntu/results/coverage/Digester/Digester_7_10.coverage", "5b159f2f-5b4c-4413-8f71-b8c30c5fa50f");
        if (customContentHandler != null) {
            writeline("/home/ubuntu/results/coverage/Digester/Digester_7_10.coverage", "6340e23e-2b13-431a-88ab-7ebbcfb33776");
            customContentHandler.processingInstruction(target, data);
            writeline("/home/ubuntu/results/coverage/Digester/Digester_7_10.coverage", "7ab4e8f3-bf35-4937-9d13-70781845f844");
            return;
        }
        writeline("/home/ubuntu/results/coverage/Digester/Digester_7_10.coverage", "ef05d621-5482-42ce-8396-130b63d841d0");
        if (saxLog.isDebugEnabled()) {
            writeline("/home/ubuntu/results/coverage/Digester/Digester_7_10.coverage", "59b2dd6b-f694-438c-9265-628d09c064c6");
            saxLog.debug("processingInstruction('" + target + "','" + data + "')");
        }
    }

    /**
     * Gets the document locator associated with our parser.
     *
     * @return the Locator supplied by the document parser
     */
    public Locator getDocumentLocator() {
        writeline("/home/ubuntu/results/coverage/Digester/Digester_7_10.coverage", "ef0efa13-f372-4b69-8b62-ce4370f24ec6");
        return locator;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public void setDocumentLocator(Locator locator) {
        writeline("/home/ubuntu/results/coverage/Digester/Digester_7_10.coverage", "7d6cca3f-56e4-4c6a-9b48-82e785d145dd");
        if (saxLog.isDebugEnabled()) {
            writeline("/home/ubuntu/results/coverage/Digester/Digester_7_10.coverage", "81b06cc6-23ae-4b4a-b4d2-a50cb1d75991");
            saxLog.debug("setDocumentLocator(" + locator + ")");
        }
        writeline("/home/ubuntu/results/coverage/Digester/Digester_7_10.coverage", "027f2aad-e0b4-477e-aa9d-a65a6cb427d5");
        this.locator = locator;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public void skippedEntity(String name) throws SAXException {
        writeline("/home/ubuntu/results/coverage/Digester/Digester_7_10.coverage", "e6798d90-2c67-49b7-83dd-b8bc219826ee");
        if (saxLog.isDebugEnabled()) {
            writeline("/home/ubuntu/results/coverage/Digester/Digester_7_10.coverage", "3eb5c47a-d3c1-4619-80aa-b01c19834bd3");
            saxLog.debug("skippedEntity(" + name + ")");
        }
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public void startDocument() throws SAXException {
        writeline("/home/ubuntu/results/coverage/Digester/Digester_7_10.coverage", "fe50c75f-7106-49a1-907a-bf7416a10f8b");
        if (saxLog.isDebugEnabled()) {
            writeline("/home/ubuntu/results/coverage/Digester/Digester_7_10.coverage", "8189670d-6af4-40a6-b783-87549caa8aa2");
            saxLog.debug("startDocument()");
        }
        writeline("/home/ubuntu/results/coverage/Digester/Digester_7_10.coverage", "201f643d-75d0-4797-bf2b-83d520f52ff5");
        configure();
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public void startElement(String namespaceURI, String localName, String qName, Attributes list) throws SAXException {
        writeline("/home/ubuntu/results/coverage/Digester/Digester_7_10.coverage", "6668c102-d4a6-42ee-9433-0fee7c0050a2");
        boolean debug = log.isDebugEnabled();
        writeline("/home/ubuntu/results/coverage/Digester/Digester_7_10.coverage", "91542706-3352-40bd-994b-8409c9137ad0");
        if (customContentHandler != null) {
            writeline("/home/ubuntu/results/coverage/Digester/Digester_7_10.coverage", "dc9d94ac-ec3a-4514-ab1b-45bdb0fa600a");
            customContentHandler.startElement(namespaceURI, localName, qName, list);
            writeline("/home/ubuntu/results/coverage/Digester/Digester_7_10.coverage", "d85133e6-b3b2-40f1-9a9c-ab1cf02d60ab");
            return;
        }
        writeline("/home/ubuntu/results/coverage/Digester/Digester_7_10.coverage", "c56b8a9f-51af-40c4-8fce-ccdfb8d22533");
        if (saxLog.isDebugEnabled()) {
            writeline("/home/ubuntu/results/coverage/Digester/Digester_7_10.coverage", "0810b0f8-f041-4458-8f1f-ed98adfa6b91");
            saxLog.debug("startElement(" + namespaceURI + "," + localName + "," + qName + ")");
        }
        writeline("/home/ubuntu/results/coverage/Digester/Digester_7_10.coverage", "0e1f38b9-1534-472d-bb62-e4b408d2db0a");
        bodyTexts.push(bodyText);
        writeline("/home/ubuntu/results/coverage/Digester/Digester_7_10.coverage", "e3c8d5cf-21d7-441e-a290-9b08dbc7479d");
        if (debug) {
            writeline("/home/ubuntu/results/coverage/Digester/Digester_7_10.coverage", "b64d0aaf-7ce8-40c3-9995-db905747f59f");
            log.debug("  Pushing body text '" + bodyText.toString() + "'");
        }
        writeline("/home/ubuntu/results/coverage/Digester/Digester_7_10.coverage", "d4031c26-cfcf-4dac-a276-20437567710a");
        bodyText = new StringBuilder();
        writeline("/home/ubuntu/results/coverage/Digester/Digester_7_10.coverage", "ec78794f-f2b4-41f9-9f87-6a62620e4fca");
        String name = localName;
        writeline("/home/ubuntu/results/coverage/Digester/Digester_7_10.coverage", "1647cf87-e599-49da-a1ec-5868fe75abb9");
        if ((name == null) || (name.length() < 1)) {
            writeline("/home/ubuntu/results/coverage/Digester/Digester_7_10.coverage", "a2cc4c60-8de4-4fc4-8d84-8d2e3c6c8d04");
            name = qName;
        }
        writeline("/home/ubuntu/results/coverage/Digester/Digester_7_10.coverage", "f0dd4b02-7f45-4eff-87d3-fd550f127edd");
        StringBuilder sb = new StringBuilder(match);
        writeline("/home/ubuntu/results/coverage/Digester/Digester_7_10.coverage", "cdc79fbb-dc6c-4d9d-8043-6803a9d29a98");
        if (match.length() > 0) {
            writeline("/home/ubuntu/results/coverage/Digester/Digester_7_10.coverage", "4cf4a23b-e594-4971-9478-58f4f141d6bf");
            sb.append('/');
        }
        writeline("/home/ubuntu/results/coverage/Digester/Digester_7_10.coverage", "c41423ff-469e-4298-b818-2b4a52c7e4c7");
        sb.append(name);
        writeline("/home/ubuntu/results/coverage/Digester/Digester_7_10.coverage", "d8ee9c55-1af7-4798-a25d-91c02c4aa9a5");
        match = sb.toString();
        writeline("/home/ubuntu/results/coverage/Digester/Digester_7_10.coverage", "8cf6ac51-717f-4c78-85fe-16e3de8ad3dd");
        if (debug) {
            writeline("/home/ubuntu/results/coverage/Digester/Digester_7_10.coverage", "a17f25bc-819f-4701-a2dd-7ef1a665d67b");
            log.debug("  New match='" + match + "'");
        }
        writeline("/home/ubuntu/results/coverage/Digester/Digester_7_10.coverage", "2a057f7a-5139-4fb4-b252-69ad879b1db6");
        List<Rule> rules = getRules().match(namespaceURI, match, localName, list);
        writeline("/home/ubuntu/results/coverage/Digester/Digester_7_10.coverage", "bc119236-890a-484b-83b6-7ebac0cf4a5e");
        matches.push(rules);
        writeline("/home/ubuntu/results/coverage/Digester/Digester_7_10.coverage", "d70d882b-c065-41c6-862d-bf129db3ed86");
        if ((rules != null) && (rules.size() > 0)) {
            writeline("/home/ubuntu/results/coverage/Digester/Digester_7_10.coverage", "a68a8f6a-90ee-4af0-8d6e-17ccfe3751e8");
            Substitutor substitutor = getSubstitutor();
            writeline("/home/ubuntu/results/coverage/Digester/Digester_7_10.coverage", "a49ae1fe-6b79-4989-b8c7-fc786d2782de");
            if (substitutor != null) {
                writeline("/home/ubuntu/results/coverage/Digester/Digester_7_10.coverage", "1773691e-a4d3-485c-98f1-f428355e74ee");
                list = substitutor.substitute(list);
            }
            writeline("/home/ubuntu/results/coverage/Digester/Digester_7_10.coverage", "2093ed61-8000-4e53-a0ac-7a708738e05f");
            for (int i = 0; i < rules.size(); i++) {
                writeline("/home/ubuntu/results/coverage/Digester/Digester_7_10.coverage", "45b65f17-7055-4a33-8299-518960171530");
                try {
                    writeline("/home/ubuntu/results/coverage/Digester/Digester_7_10.coverage", "ad301309-d76d-4c55-b6d0-e74aaff529ef");
                    Rule rule = rules.get(i);
                    writeline("/home/ubuntu/results/coverage/Digester/Digester_7_10.coverage", "b046d6e5-7211-45fc-a9e3-8e59df3613b4");
                    if (debug) {
                        writeline("/home/ubuntu/results/coverage/Digester/Digester_7_10.coverage", "5681f5f1-883e-44f7-a989-0300c845053d");
                        log.debug("  Fire begin() for " + rule);
                    }
                    writeline("/home/ubuntu/results/coverage/Digester/Digester_7_10.coverage", "26644a9c-09ab-4595-b562-bb8548fd4743");
                    rule.begin(namespaceURI, name, list);
                } catch (Exception e) {
                    writeline("/home/ubuntu/results/coverage/Digester/Digester_7_10.coverage", "10bae999-1a1c-43a0-b14b-17a607a1bd5a");
                    log.error("Begin event threw exception", e);
                } catch (Error e) {
                    writeline("/home/ubuntu/results/coverage/Digester/Digester_7_10.coverage", "feffefbc-3ff8-4ef7-b927-620adf003e86");
                    log.error("Begin event threw error", e);
                }
            }
        } else {
            writeline("/home/ubuntu/results/coverage/Digester/Digester_7_10.coverage", "2519c23e-18cb-4d6c-9328-8329456da721");
            if (debug) {
                writeline("/home/ubuntu/results/coverage/Digester/Digester_7_10.coverage", "aba0a09e-c1ad-4a28-ad3a-7b1604f86841");
                log.debug("  No rules found matching '" + match + "'.");
            }
        }
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public void startPrefixMapping(String prefix, String namespaceURI) throws SAXException {
        writeline("/home/ubuntu/results/coverage/Digester/Digester_7_10.coverage", "5483ed19-e4a4-475f-aa3d-c200db6fe8a3");
        if (saxLog.isDebugEnabled()) {
            writeline("/home/ubuntu/results/coverage/Digester/Digester_7_10.coverage", "da7bcc10-4692-4392-8e63-954bebad329d");
            saxLog.debug("startPrefixMapping(" + prefix + "," + namespaceURI + ")");
        }
        writeline("/home/ubuntu/results/coverage/Digester/Digester_7_10.coverage", "d2dd0eff-ed0d-485f-a306-8b4f13596472");
        Stack<String> stack = namespaces.get(prefix);
        writeline("/home/ubuntu/results/coverage/Digester/Digester_7_10.coverage", "c9a7b67d-8038-4e09-839b-86af412eb1d9");
        if (stack == null) {
            writeline("/home/ubuntu/results/coverage/Digester/Digester_7_10.coverage", "d61d0066-4db6-48ec-9759-1ef0eda22df6");
            stack = new Stack<String>();
            writeline("/home/ubuntu/results/coverage/Digester/Digester_7_10.coverage", "7d4af872-f3f9-474d-b8e6-fe287e9ffe85");
            namespaces.put(prefix, stack);
        }
        writeline("/home/ubuntu/results/coverage/Digester/Digester_7_10.coverage", "516ffb53-bd41-454b-a5c0-b45ef8de9a46");
        stack.push(namespaceURI);
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public void notationDecl(String name, String publicId, String systemId) {
        writeline("/home/ubuntu/results/coverage/Digester/Digester_7_10.coverage", "e094f3dd-1e61-48a1-ac91-98ed82a022f2");
        if (saxLog.isDebugEnabled()) {
            writeline("/home/ubuntu/results/coverage/Digester/Digester_7_10.coverage", "22844750-bdc4-41c7-872d-f8016f237138");
            saxLog.debug("notationDecl(" + name + "," + publicId + "," + systemId + ")");
        }
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public void unparsedEntityDecl(String name, String publicId, String systemId, String notation) {
        writeline("/home/ubuntu/results/coverage/Digester/Digester_7_10.coverage", "2730e75d-1572-4466-8014-7ae35965a7db");
        if (saxLog.isDebugEnabled()) {
            writeline("/home/ubuntu/results/coverage/Digester/Digester_7_10.coverage", "40f8bb6d-4653-4ee2-b67d-fed337844942");
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
        writeline("/home/ubuntu/results/coverage/Digester/Digester_7_10.coverage", "7e5480ed-e106-4428-aa8f-ea67e0d21eee");
        this.entityResolver = entityResolver;
    }

    /**
     * Return the Entity Resolver used by the SAX parser.
     *
     * @return the Entity Resolver used by the SAX parser.
     */
    public EntityResolver getEntityResolver() {
        writeline("/home/ubuntu/results/coverage/Digester/Digester_7_10.coverage", "84ac49f2-f7df-44ba-ad99-8b2b15a87054");
        return entityResolver;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public InputSource resolveEntity(String publicId, String systemId) throws SAXException {
        writeline("/home/ubuntu/results/coverage/Digester/Digester_7_10.coverage", "080cc083-dfc2-47f6-8d8d-b2f5b8cc94b0");
        if (saxLog.isDebugEnabled()) {
            writeline("/home/ubuntu/results/coverage/Digester/Digester_7_10.coverage", "84e31366-369e-485a-ba24-5c1c39503027");
            saxLog.debug("resolveEntity('" + publicId + "', '" + systemId + "')");
        }
        writeline("/home/ubuntu/results/coverage/Digester/Digester_7_10.coverage", "6de2ed2d-db18-4b54-85a3-1ed79a9d0ad1");
        if (publicId != null) {
            writeline("/home/ubuntu/results/coverage/Digester/Digester_7_10.coverage", "ac50624f-6753-41c4-acc0-65d09fc8f5cb");
            this.publicId = publicId;
        }
        writeline("/home/ubuntu/results/coverage/Digester/Digester_7_10.coverage", "7d4b0feb-4bf0-45bc-9370-6f7b2dfbda5e");
        URL entityURL = null;
        writeline("/home/ubuntu/results/coverage/Digester/Digester_7_10.coverage", "692058dc-0dbd-4a7b-915d-f785c5790722");
        if (publicId != null) {
            writeline("/home/ubuntu/results/coverage/Digester/Digester_7_10.coverage", "57eca278-f599-4ec8-a312-96cdbad84702");
            entityURL = entityValidator.get(publicId);
        }
        writeline("/home/ubuntu/results/coverage/Digester/Digester_7_10.coverage", "fb8be566-06d8-4ae2-9a73-f866b5721ff1");
        if (entityURL == null && systemId != null) {
            writeline("/home/ubuntu/results/coverage/Digester/Digester_7_10.coverage", "5c9b83ea-b8d8-4d63-8cc7-092b222e92d9");
            entityURL = entityValidator.get(systemId);
        }
        writeline("/home/ubuntu/results/coverage/Digester/Digester_7_10.coverage", "5d17d65f-7c09-43d4-ab4c-32389a99227d");
        if (entityURL == null) {
            writeline("/home/ubuntu/results/coverage/Digester/Digester_7_10.coverage", "36f73967-e280-4cb5-930d-9af8734b16e5");
            if (systemId == null) {
                writeline("/home/ubuntu/results/coverage/Digester/Digester_7_10.coverage", "a09844d6-0cd2-4eeb-95b3-58d9739129a6");
                if (log.isDebugEnabled()) {
                    writeline("/home/ubuntu/results/coverage/Digester/Digester_7_10.coverage", "773c32a1-ade4-47e9-a9b4-c83301d1cc4e");
                    log.debug(" Cannot resolve null entity, returning null InputSource");
                }
                writeline("/home/ubuntu/results/coverage/Digester/Digester_7_10.coverage", "b0a598b0-5f25-4017-b2e1-3e4760b3e182");
                return (null);
            }
            writeline("/home/ubuntu/results/coverage/Digester/Digester_7_10.coverage", "a9700934-7542-4e1c-b837-46aea381b992");
            if (log.isDebugEnabled()) {
                writeline("/home/ubuntu/results/coverage/Digester/Digester_7_10.coverage", "d2936ae0-56c2-4e1f-b67a-c9eb021ad020");
                log.debug(" Trying to resolve using system ID '" + systemId + "'");
            }
            writeline("/home/ubuntu/results/coverage/Digester/Digester_7_10.coverage", "9c106e04-3132-4d0e-b7b1-757edb4e9047");
            try {
                writeline("/home/ubuntu/results/coverage/Digester/Digester_7_10.coverage", "e3b55a39-70c8-43a4-a716-c1de9a7d79ae");
                entityURL = new URL(systemId);
            } catch (MalformedURLException e) {
            }
        }
        writeline("/home/ubuntu/results/coverage/Digester/Digester_7_10.coverage", "40345603-19ed-41e5-89ce-940ae42c608c");
        if (log.isDebugEnabled()) {
            writeline("/home/ubuntu/results/coverage/Digester/Digester_7_10.coverage", "7ef000ad-c038-49a0-a69e-462e4da676e9");
            log.debug(" Resolving to alternate DTD '" + entityURL + "'");
        }
        writeline("/home/ubuntu/results/coverage/Digester/Digester_7_10.coverage", "94ced6ce-b6dd-4b28-bd6c-e7f1ed772532");
        try {
            writeline("/home/ubuntu/results/coverage/Digester/Digester_7_10.coverage", "fbbb1f7e-61e6-4999-bc4c-64798084d49b");
            return createInputSourceFromURL(entityURL);
        } catch (Exception e) {
        }
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public void error(SAXParseException exception) throws SAXException {
        writeline("/home/ubuntu/results/coverage/Digester/Digester_7_10.coverage", "15b39390-2340-426e-9e86-db5e69dbf332");
        log.error("Parse Error at line " + exception.getLineNumber() + " column " + exception.getColumnNumber() + ": " + exception.getMessage(), exception);
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public void fatalError(SAXParseException exception) throws SAXException {
        writeline("/home/ubuntu/results/coverage/Digester/Digester_7_10.coverage", "6f685d8e-7a4e-4ef9-bea9-4285fc02afb6");
        log.error("Parse Fatal Error at line " + exception.getLineNumber() + " column " + exception.getColumnNumber() + ": " + exception.getMessage(), exception);
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public void warning(SAXParseException exception) throws SAXException {
        writeline("/home/ubuntu/results/coverage/Digester/Digester_7_10.coverage", "275042fc-2aa3-4dba-b26b-cc971778a7b8");
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
        writeline("/home/ubuntu/results/coverage/Digester/Digester_7_10.coverage", "a35f8c4f-5ca1-47d3-9493-b794e3c4cd91");
        if (file == null) {
        }
        writeline("/home/ubuntu/results/coverage/Digester/Digester_7_10.coverage", "8b6e53be-3d67-445a-a92b-c58151aac998");
        InputSource input = new InputSource(new FileInputStream(file));
        writeline("/home/ubuntu/results/coverage/Digester/Digester_7_10.coverage", "f345089b-e2f0-4f80-89a9-7bc22d6f7d8c");
        input.setSystemId(file.toURI().toURL().toString());
        writeline("/home/ubuntu/results/coverage/Digester/Digester_7_10.coverage", "487cefec-450a-4c8b-bc48-62ffd437cdb2");
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
        writeline("/home/ubuntu/results/coverage/Digester/Digester_7_10.coverage", "15e87eff-2432-44ca-927d-1b25bccb69ae");
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
        writeline("/home/ubuntu/results/coverage/Digester/Digester_7_10.coverage", "4ad90022-18c9-4ef2-be12-3e1b0a12f695");
        if (input == null) {
        }
        writeline("/home/ubuntu/results/coverage/Digester/Digester_7_10.coverage", "a4aa4878-6f4c-47ce-a832-6e40822b0c5e");
        configure();
        writeline("/home/ubuntu/results/coverage/Digester/Digester_7_10.coverage", "80b26d99-ce5a-444d-b1a4-d6af0d061759");
        String systemId = input.getSystemId();
        writeline("/home/ubuntu/results/coverage/Digester/Digester_7_10.coverage", "eaddb576-9298-49fe-a3c3-f4f40015dce8");
        if (systemId == null) {
            writeline("/home/ubuntu/results/coverage/Digester/Digester_7_10.coverage", "449c8e58-4401-47e5-b580-56854d90adb5");
            systemId = "(already loaded from stream)";
        }
        writeline("/home/ubuntu/results/coverage/Digester/Digester_7_10.coverage", "0b7ea934-f1fe-405c-bc28-c7ddeff62376");
        try {
            writeline("/home/ubuntu/results/coverage/Digester/Digester_7_10.coverage", "1a6dd1ca-f8f5-4118-b012-8d38a362b862");
            getXMLReader().parse(input);
        } catch (IOException e) {
            writeline("/home/ubuntu/results/coverage/Digester/Digester_7_10.coverage", "48ce44cf-e6c3-4ef5-a862-a70bf0abdf41");
            log.error(format("An error occurred while reading stream from '%s', see nested exceptions", systemId), e);
        }
        writeline("/home/ubuntu/results/coverage/Digester/Digester_7_10.coverage", "d02f8370-679d-4bda-89f3-fd1f7a4a8fab");
        cleanup();
        writeline("/home/ubuntu/results/coverage/Digester/Digester_7_10.coverage", "da0ff238-4877-4c8a-8432-eac73192048b");
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
        writeline("/home/ubuntu/results/coverage/Digester/Digester_7_10.coverage", "ee8c07e8-2507-41c2-a80e-a239abb232af");
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
        writeline("/home/ubuntu/results/coverage/Digester/Digester_7_10.coverage", "310e950f-71b2-47ec-b19d-cea63f4cc4c4");
        if (input == null) {
        }
        writeline("/home/ubuntu/results/coverage/Digester/Digester_7_10.coverage", "202a8718-a1b3-4dd9-aa1c-34b195154e73");
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
        writeline("/home/ubuntu/results/coverage/Digester/Digester_7_10.coverage", "cb157a3f-d94a-425c-93e3-d0b1c68d9a47");
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
        writeline("/home/ubuntu/results/coverage/Digester/Digester_7_10.coverage", "94b17a30-4558-4bd6-a851-a14b10b867c6");
        if (reader == null) {
        }
        writeline("/home/ubuntu/results/coverage/Digester/Digester_7_10.coverage", "cd04229e-37cb-422e-a83f-9b445930e1c5");
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
        writeline("/home/ubuntu/results/coverage/Digester/Digester_7_10.coverage", "744090a2-939e-4fc4-9f74-178ddf736815");
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
        writeline("/home/ubuntu/results/coverage/Digester/Digester_7_10.coverage", "a72317bf-65b1-455c-be96-c05467e96a08");
        if (uri == null) {
        }
        writeline("/home/ubuntu/results/coverage/Digester/Digester_7_10.coverage", "d4a876d9-4b22-49d6-8238-b6ac4ac3d7a2");
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
        writeline("/home/ubuntu/results/coverage/Digester/Digester_7_10.coverage", "57be3db7-d3c6-4b8d-be87-9f7d5f071d42");
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
        writeline("/home/ubuntu/results/coverage/Digester/Digester_7_10.coverage", "b7bc3d06-e5ee-469e-a20c-9d2cdb01b6e3");
        if (url == null) {
        }
        writeline("/home/ubuntu/results/coverage/Digester/Digester_7_10.coverage", "6b3b375d-05f1-4cea-a9d4-75499e9fa94c");
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
        writeline("/home/ubuntu/results/coverage/Digester/Digester_7_10.coverage", "30a6dd59-8154-4333-9203-3826841d086e");
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
        writeline("/home/ubuntu/results/coverage/Digester/Digester_7_10.coverage", "cd07cf7a-c3f8-4ba5-8eb2-6900685688f4");
        if (executorService == null) {
        }
        writeline("/home/ubuntu/results/coverage/Digester/Digester_7_10.coverage", "637cd93d-d6b0-4bcc-9f5a-f1c24ecf3f91");
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
        writeline("/home/ubuntu/results/coverage/Digester/Digester_7_10.coverage", "a3a87a4b-e172-4476-a8e4-2d2577817e49");
        if (log.isDebugEnabled()) {
            writeline("/home/ubuntu/results/coverage/Digester/Digester_7_10.coverage", "9121f82b-728f-45b2-b69d-1f4961f156f1");
            log.debug("register('" + publicId + "', '" + entityURL + "'");
        }
        writeline("/home/ubuntu/results/coverage/Digester/Digester_7_10.coverage", "3fe772c8-ef75-4a7e-96ae-87c87990b256");
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
        writeline("/home/ubuntu/results/coverage/Digester/Digester_7_10.coverage", "a5ab0157-e87c-46e2-b0f9-7ef5b8eb99f2");
        if (log.isDebugEnabled()) {
            writeline("/home/ubuntu/results/coverage/Digester/Digester_7_10.coverage", "b478a0e1-cf49-4f51-abf4-885f4bdd6204");
            log.debug("register('" + publicId + "', '" + entityURL + "'");
        }
        writeline("/home/ubuntu/results/coverage/Digester/Digester_7_10.coverage", "7cc3216d-9751-4629-808e-6ade3d2116c7");
        try {
            writeline("/home/ubuntu/results/coverage/Digester/Digester_7_10.coverage", "35c6e714-a1d3-4df6-9bfe-5c59f898bfb2");
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
        writeline("/home/ubuntu/results/coverage/Digester/Digester_7_10.coverage", "070ea485-09b3-424c-b0db-1cb8b8194f6c");
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
        writeline("/home/ubuntu/results/coverage/Digester/Digester_7_10.coverage", "8ccd87e9-7dd8-4839-b852-802647d8f163");
        URLConnection connection = url.openConnection();
        writeline("/home/ubuntu/results/coverage/Digester/Digester_7_10.coverage", "e194a102-01a0-4a23-905d-9c09e7e45d8a");
        connection.setUseCaches(false);
        writeline("/home/ubuntu/results/coverage/Digester/Digester_7_10.coverage", "86d1fbef-80fd-4645-863b-8e963f99c4a9");
        InputStream stream = connection.getInputStream();
        writeline("/home/ubuntu/results/coverage/Digester/Digester_7_10.coverage", "e5563b3f-6b58-4dff-bcb0-8d3260647c48");
        InputSource source = new InputSource(stream);
        writeline("/home/ubuntu/results/coverage/Digester/Digester_7_10.coverage", "de9d485d-5809-4208-9f9d-d3a3a3e12e03");
        source.setSystemId(url.toExternalForm());
        writeline("/home/ubuntu/results/coverage/Digester/Digester_7_10.coverage", "f9f6fce8-d10d-4456-b12e-9cb7e34043bb");
        inputSources.add(source);
        writeline("/home/ubuntu/results/coverage/Digester/Digester_7_10.coverage", "41062645-7a9e-4530-88c3-d9af8aa4c196");
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
        writeline("/home/ubuntu/results/coverage/Digester/Digester_7_10.coverage", "e283eaaf-5bbc-476d-ace2-11a3b7f18b14");
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
        writeline("/home/ubuntu/results/coverage/Digester/Digester_7_10.coverage", "6d9247d5-1943-41c2-945d-3603dd442fcd");
        rule.setDigester(this);
        writeline("/home/ubuntu/results/coverage/Digester/Digester_7_10.coverage", "1eebd463-e0bf-455d-ba0f-0608411dbebc");
        getRules().add(pattern, rule);
    }

    /**
     * Register a set of Rule instances defined in a RuleSet.
     *
     * @param ruleSet The RuleSet instance to configure from
     */
    public void addRuleSet(RuleSet ruleSet) {
        writeline("/home/ubuntu/results/coverage/Digester/Digester_7_10.coverage", "a5cd6656-7723-45e6-9c60-72c7853baba7");
        String oldNamespaceURI = getRuleNamespaceURI();
        writeline("/home/ubuntu/results/coverage/Digester/Digester_7_10.coverage", "88718dc4-2a78-4ff9-a7b6-6c732784c78f");
        String newNamespaceURI = ruleSet.getNamespaceURI();
        writeline("/home/ubuntu/results/coverage/Digester/Digester_7_10.coverage", "0d461ca5-efd5-41ee-90f7-327799b8e9a3");
        if (log.isDebugEnabled()) {
            writeline("/home/ubuntu/results/coverage/Digester/Digester_7_10.coverage", "1403d6bf-b47e-4b18-81da-46053d18ee37");
            if (newNamespaceURI == null) {
                writeline("/home/ubuntu/results/coverage/Digester/Digester_7_10.coverage", "d725f353-73d3-44ce-b1b1-a77eb3e1010e");
                log.debug("addRuleSet() with no namespace URI");
            } else {
                writeline("/home/ubuntu/results/coverage/Digester/Digester_7_10.coverage", "64f217b0-48cd-47c9-866c-fc3965909e34");
                log.debug("addRuleSet() with namespace URI " + newNamespaceURI);
            }
        }
        writeline("/home/ubuntu/results/coverage/Digester/Digester_7_10.coverage", "2c857132-1a7c-4b72-8b18-e7249908967e");
        setRuleNamespaceURI(newNamespaceURI);
        writeline("/home/ubuntu/results/coverage/Digester/Digester_7_10.coverage", "7e32a613-d9e0-45c8-b33b-ad73fba632e1");
        ruleSet.addRuleInstances(this);
        writeline("/home/ubuntu/results/coverage/Digester/Digester_7_10.coverage", "d13a0f91-205e-40fd-bf9e-8b47cb50fe5f");
        setRuleNamespaceURI(oldNamespaceURI);
    }

    /**
     * Add a "bean property setter" rule for the specified parameters.
     *
     * @param pattern Element matching pattern
     * @see BeanPropertySetterRule
     */
    public void addBeanPropertySetter(String pattern) {
        writeline("/home/ubuntu/results/coverage/Digester/Digester_7_10.coverage", "3ad1a290-3e54-4113-a70c-b4d6f7e5797a");
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
        writeline("/home/ubuntu/results/coverage/Digester/Digester_7_10.coverage", "bfca6db6-62c3-448c-8149-88c0f7603899");
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
        writeline("/home/ubuntu/results/coverage/Digester/Digester_7_10.coverage", "e8aa6b1a-a1bb-4289-b9e4-0dee444e0b37");
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
        writeline("/home/ubuntu/results/coverage/Digester/Digester_7_10.coverage", "917f6fbf-0e86-4d1f-884f-0881e62dc788");
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
        writeline("/home/ubuntu/results/coverage/Digester/Digester_7_10.coverage", "9f4fed2f-79b2-4843-9fb4-f6de00d30b59");
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
        writeline("/home/ubuntu/results/coverage/Digester/Digester_7_10.coverage", "f1137ab7-c2af-4d83-9e87-7c5507cb1475");
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
        writeline("/home/ubuntu/results/coverage/Digester/Digester_7_10.coverage", "c06eeae8-360d-42a1-b73d-417780d5036c");
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
        writeline("/home/ubuntu/results/coverage/Digester/Digester_7_10.coverage", "da04b383-61b8-4ad2-9f95-c794cd35d973");
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
        writeline("/home/ubuntu/results/coverage/Digester/Digester_7_10.coverage", "45169cd0-0322-4ba9-b93b-71d3ac400ab8");
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
        writeline("/home/ubuntu/results/coverage/Digester/Digester_7_10.coverage", "31ec9620-7e84-4e18-8926-9f7bcd215aba");
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
        writeline("/home/ubuntu/results/coverage/Digester/Digester_7_10.coverage", "dab0ad43-e986-499e-b88d-d2e8adc04da1");
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
        writeline("/home/ubuntu/results/coverage/Digester/Digester_7_10.coverage", "9301db44-c164-4fe1-bd1e-7c29dd081c7b");
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
        writeline("/home/ubuntu/results/coverage/Digester/Digester_7_10.coverage", "bf4bef1d-de02-430b-bac1-dc533e89b936");
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
        writeline("/home/ubuntu/results/coverage/Digester/Digester_7_10.coverage", "96a12f30-20a9-4517-a4cc-9c898e7e93d5");
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
        writeline("/home/ubuntu/results/coverage/Digester/Digester_7_10.coverage", "6c98684d-6048-46a3-ae87-bb29f4ad74d3");
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
        writeline("/home/ubuntu/results/coverage/Digester/Digester_7_10.coverage", "c4315447-5648-421d-bb08-b8e5d65fd181");
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
        writeline("/home/ubuntu/results/coverage/Digester/Digester_7_10.coverage", "52b69cf1-a26c-4821-8f06-f3c65ab6738b");
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
        writeline("/home/ubuntu/results/coverage/Digester/Digester_7_10.coverage", "d20b52ab-b448-4247-a6be-3d83bcf29c9d");
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
        writeline("/home/ubuntu/results/coverage/Digester/Digester_7_10.coverage", "2938b751-b5e9-42a9-b49c-3e92d172639a");
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
        writeline("/home/ubuntu/results/coverage/Digester/Digester_7_10.coverage", "65b6e3b6-fcb1-458e-b6ea-fafd548b7396");
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
        writeline("/home/ubuntu/results/coverage/Digester/Digester_7_10.coverage", "87a83cc5-31d6-4b32-83ec-f1fc0688dda8");
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
        writeline("/home/ubuntu/results/coverage/Digester/Digester_7_10.coverage", "d2f75e31-7533-4985-b9c2-837cb85e4cc2");
        creationFactory.setDigester(this);
        writeline("/home/ubuntu/results/coverage/Digester/Digester_7_10.coverage", "5650875a-794f-40c5-b8ad-8824b1aebb9f");
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
        writeline("/home/ubuntu/results/coverage/Digester/Digester_7_10.coverage", "ea0818e0-0efe-4501-9209-666daf474f12");
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
        writeline("/home/ubuntu/results/coverage/Digester/Digester_7_10.coverage", "9c5ea02c-8aa7-42ba-ab21-cd45ec0e4ca1");
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
        writeline("/home/ubuntu/results/coverage/Digester/Digester_7_10.coverage", "56c50210-19cc-4f9d-8c81-0194845109d5");
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
        writeline("/home/ubuntu/results/coverage/Digester/Digester_7_10.coverage", "c548b1f8-ae87-4768-8779-8a87721e580a");
        addRule(pattern, new ObjectCreateRule(attributeName, clazz));
    }

    /**
     * Adds an {@link SetNestedPropertiesRule}.
     *
     * @param pattern register the rule with this pattern
     * @since 1.6
     */
    public void addSetNestedProperties(String pattern) {
        writeline("/home/ubuntu/results/coverage/Digester/Digester_7_10.coverage", "b9baf637-5986-4a20-8ae3-92a5b79d293f");
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
        writeline("/home/ubuntu/results/coverage/Digester/Digester_7_10.coverage", "42dd7af8-eb5a-422b-92ff-6ad8c81854d2");
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
        writeline("/home/ubuntu/results/coverage/Digester/Digester_7_10.coverage", "4d35496a-6f92-4aea-9d41-a0ebeff355d9");
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
        writeline("/home/ubuntu/results/coverage/Digester/Digester_7_10.coverage", "7e26662e-ed3a-42e2-8cd1-49a69cde1534");
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
        writeline("/home/ubuntu/results/coverage/Digester/Digester_7_10.coverage", "6d74229d-835b-4c65-90ac-25a443aae45b");
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
        writeline("/home/ubuntu/results/coverage/Digester/Digester_7_10.coverage", "49774f10-7762-4b20-8fb8-419559df704f");
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
        writeline("/home/ubuntu/results/coverage/Digester/Digester_7_10.coverage", "121361d8-7024-4388-81a6-565ab815afec");
        addRule(pattern, new SetRootRule(methodName, paramType));
    }

    /**
     * Add a "set properties" rule for the specified parameters.
     *
     * @param pattern Element matching pattern
     * @see SetPropertiesRule
     */
    public void addSetProperties(String pattern) {
        writeline("/home/ubuntu/results/coverage/Digester/Digester_7_10.coverage", "dd1e04be-c9bf-4a9a-b66e-2ec0379fbeb7");
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
        writeline("/home/ubuntu/results/coverage/Digester/Digester_7_10.coverage", "9c414924-0c8b-4d69-9521-da13e5e68ee3");
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
        writeline("/home/ubuntu/results/coverage/Digester/Digester_7_10.coverage", "93a1a612-5c32-4908-b1a0-2fd2a0972c58");
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
        writeline("/home/ubuntu/results/coverage/Digester/Digester_7_10.coverage", "d7720206-252c-41be-997d-29affe9999e4");
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
        writeline("/home/ubuntu/results/coverage/Digester/Digester_7_10.coverage", "f8fcc4ee-0733-4386-80f5-ec54949f3659");
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
        writeline("/home/ubuntu/results/coverage/Digester/Digester_7_10.coverage", "abd92970-6adf-4302-ae4d-a6e302b8af45");
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
        writeline("/home/ubuntu/results/coverage/Digester/Digester_7_10.coverage", "56c5bf89-6e6b-4bab-8e55-56a8345e2c6c");
        match = "";
        writeline("/home/ubuntu/results/coverage/Digester/Digester_7_10.coverage", "d95d970b-dc59-4874-805e-8106ac924d5e");
        bodyTexts.clear();
        writeline("/home/ubuntu/results/coverage/Digester/Digester_7_10.coverage", "e0dcecaa-4f29-4880-b78f-40a03ea302a1");
        params.clear();
        writeline("/home/ubuntu/results/coverage/Digester/Digester_7_10.coverage", "16353cac-aead-489a-931a-5f4f90ab6dd6");
        publicId = null;
        writeline("/home/ubuntu/results/coverage/Digester/Digester_7_10.coverage", "76cce01f-accf-414a-aa74-8770735b8fb2");
        stack.clear();
        writeline("/home/ubuntu/results/coverage/Digester/Digester_7_10.coverage", "20e89a39-87a9-4365-86c6-5898b2decec4");
        stacksByName.clear();
        writeline("/home/ubuntu/results/coverage/Digester/Digester_7_10.coverage", "9ea1d28e-d6b2-43ba-8683-c0ff7e13d46f");
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
        writeline("/home/ubuntu/results/coverage/Digester/Digester_7_10.coverage", "46beb0a4-39eb-4325-a695-6d1244ba8627");
        try {
            writeline("/home/ubuntu/results/coverage/Digester/Digester_7_10.coverage", "772564e7-f7e5-430b-932f-3e5a69bdcde3");
            return this.<T>npeSafeCast(stack.peek());
        } catch (EmptyStackException e) {
            writeline("/home/ubuntu/results/coverage/Digester/Digester_7_10.coverage", "46947f43-3a8a-4552-b415-4f3193c3e475");
            log.warn("Empty stack (returning null)");
            writeline("/home/ubuntu/results/coverage/Digester/Digester_7_10.coverage", "1e7d2fd9-e4b9-4319-ba34-cb81d7ae3b05");
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
        writeline("/home/ubuntu/results/coverage/Digester/Digester_7_10.coverage", "079fad35-61a3-4b91-a6c9-5f1b1c0e6655");
        int index = (stack.size() - 1) - n;
        writeline("/home/ubuntu/results/coverage/Digester/Digester_7_10.coverage", "666cb7c8-95e9-40ea-8ba1-995b6f68f072");
        if (index < 0) {
            writeline("/home/ubuntu/results/coverage/Digester/Digester_7_10.coverage", "a9ffd785-870a-45b9-bf49-9dde19c833c5");
            log.warn("Empty stack (returning null)");
            writeline("/home/ubuntu/results/coverage/Digester/Digester_7_10.coverage", "c5299bf7-63af-4ba3-b1a7-0a20972be5e8");
            return (null);
        }
        writeline("/home/ubuntu/results/coverage/Digester/Digester_7_10.coverage", "f51e64cd-6979-4e66-92ed-1fd01716ad6d");
        try {
            writeline("/home/ubuntu/results/coverage/Digester/Digester_7_10.coverage", "f76e3bd5-f0bf-48e7-bd9e-32a0a801d959");
            return this.<T>npeSafeCast(stack.get(index));
        } catch (EmptyStackException e) {
            writeline("/home/ubuntu/results/coverage/Digester/Digester_7_10.coverage", "f29121ac-123c-4fb5-84ed-169ea6a70b5a");
            log.warn("Empty stack (returning null)");
            writeline("/home/ubuntu/results/coverage/Digester/Digester_7_10.coverage", "02e59cc0-6d05-40dc-84d0-d1b07b0782b2");
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
        writeline("/home/ubuntu/results/coverage/Digester/Digester_7_10.coverage", "3d682b4f-82c0-4d43-bd97-3a241b6d21c2");
        try {
            writeline("/home/ubuntu/results/coverage/Digester/Digester_7_10.coverage", "c5e21be1-b26e-41ae-8c9a-220182d9c3b1");
            T popped = this.<T>npeSafeCast(stack.pop());
            writeline("/home/ubuntu/results/coverage/Digester/Digester_7_10.coverage", "3bede77b-e7c5-4338-b1df-e33a48413039");
            if (stackAction != null) {
                writeline("/home/ubuntu/results/coverage/Digester/Digester_7_10.coverage", "84ba258c-51d1-4acb-b9ca-f21438e20396");
                popped = stackAction.onPop(this, null, popped);
            }
            writeline("/home/ubuntu/results/coverage/Digester/Digester_7_10.coverage", "130985d4-d547-4f99-910f-50f842cf94d9");
            return popped;
        } catch (EmptyStackException e) {
            writeline("/home/ubuntu/results/coverage/Digester/Digester_7_10.coverage", "a099ffd1-b5f1-4269-99e0-166baeb4c061");
            log.warn("Empty stack (returning null)");
            writeline("/home/ubuntu/results/coverage/Digester/Digester_7_10.coverage", "094722ea-5ac3-4b31-afd2-af4d849a6958");
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
        writeline("/home/ubuntu/results/coverage/Digester/Digester_7_10.coverage", "3559bd7a-6e36-4894-967a-04f6b7fd81c1");
        if (stackAction != null) {
            writeline("/home/ubuntu/results/coverage/Digester/Digester_7_10.coverage", "88e65d52-b834-406a-8494-e3f86ab0419b");
            object = stackAction.onPush(this, null, object);
        }
        writeline("/home/ubuntu/results/coverage/Digester/Digester_7_10.coverage", "e913ca3c-67e2-45e9-8f4b-0d8d0fff1b8a");
        if (stack.size() == 0) {
            writeline("/home/ubuntu/results/coverage/Digester/Digester_7_10.coverage", "0914854e-01e7-45d4-96aa-e71ae847a481");
            root = object;
        }
        writeline("/home/ubuntu/results/coverage/Digester/Digester_7_10.coverage", "5907a69b-9e79-4faa-91b1-3f03d8716b6f");
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
        writeline("/home/ubuntu/results/coverage/Digester/Digester_7_10.coverage", "61438ef0-fa77-4ff7-bfd4-7ace587810f3");
        if (stackAction != null) {
            writeline("/home/ubuntu/results/coverage/Digester/Digester_7_10.coverage", "c66b1902-af4f-4092-8d3e-b6b684320e17");
            value = stackAction.onPush(this, stackName, value);
        }
        writeline("/home/ubuntu/results/coverage/Digester/Digester_7_10.coverage", "8e6cb23e-436c-4cb5-8966-e3f0998121d8");
        Stack<Object> namedStack = stacksByName.get(stackName);
        writeline("/home/ubuntu/results/coverage/Digester/Digester_7_10.coverage", "29f2e80e-023d-4c85-b960-c99b12be8770");
        if (namedStack == null) {
            writeline("/home/ubuntu/results/coverage/Digester/Digester_7_10.coverage", "31d6e9f4-9cf9-4f25-8521-55641679f436");
            namedStack = new Stack<Object>();
            writeline("/home/ubuntu/results/coverage/Digester/Digester_7_10.coverage", "04a7a12f-9835-4e5b-8aba-162b6e0cf702");
            stacksByName.put(stackName, namedStack);
        }
        writeline("/home/ubuntu/results/coverage/Digester/Digester_7_10.coverage", "1d4d8399-9b12-4fe2-bac6-e6c7a8cabab6");
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
        writeline("/home/ubuntu/results/coverage/Digester/Digester_7_10.coverage", "ef2f5141-1fed-435e-8a20-c33064a59716");
        T result = null;
        writeline("/home/ubuntu/results/coverage/Digester/Digester_7_10.coverage", "5d6a0dae-9590-40ae-9ac1-2e8523957a41");
        Stack<Object> namedStack = stacksByName.get(stackName);
        writeline("/home/ubuntu/results/coverage/Digester/Digester_7_10.coverage", "55d55ab1-f029-4068-8916-5a48346ca3a6");
        if (namedStack == null) {
            writeline("/home/ubuntu/results/coverage/Digester/Digester_7_10.coverage", "d5b8eb93-3d90-4384-abaa-a54d344de1ab");
            if (log.isDebugEnabled()) {
                writeline("/home/ubuntu/results/coverage/Digester/Digester_7_10.coverage", "4c028d4c-9b6b-4c26-85cf-dddcc5d85c64");
                log.debug("Stack '" + stackName + "' is empty");
            }
        }
        writeline("/home/ubuntu/results/coverage/Digester/Digester_7_10.coverage", "3a16b473-d9ac-4e2b-ae69-35824d9bd93a");
        result = this.<T>npeSafeCast(namedStack.pop());
        writeline("/home/ubuntu/results/coverage/Digester/Digester_7_10.coverage", "3383145a-be7b-4454-9910-6c03deb2cf4b");
        if (stackAction != null) {
            writeline("/home/ubuntu/results/coverage/Digester/Digester_7_10.coverage", "2187cf91-c491-439d-9f4f-413d9153661d");
            result = stackAction.onPop(this, stackName, result);
        }
        writeline("/home/ubuntu/results/coverage/Digester/Digester_7_10.coverage", "8fc1a097-b05f-41fe-90ce-cef71ef519a1");
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
        writeline("/home/ubuntu/results/coverage/Digester/Digester_7_10.coverage", "6c68f952-66ca-40a0-a177-42af087a6c70");
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
        writeline("/home/ubuntu/results/coverage/Digester/Digester_7_10.coverage", "4c16c3a1-009f-478a-ba6a-4cbe997eea71");
        T result = null;
        writeline("/home/ubuntu/results/coverage/Digester/Digester_7_10.coverage", "f015d018-bb1a-49aa-a028-42ded4190b64");
        Stack<Object> namedStack = stacksByName.get(stackName);
        writeline("/home/ubuntu/results/coverage/Digester/Digester_7_10.coverage", "1e354dab-1903-4e30-b4ee-058d86a3a495");
        if (namedStack == null) {
            writeline("/home/ubuntu/results/coverage/Digester/Digester_7_10.coverage", "ffd31963-1166-4284-9b4d-d6e8e1146795");
            if (log.isDebugEnabled()) {
                writeline("/home/ubuntu/results/coverage/Digester/Digester_7_10.coverage", "5141e548-9225-4400-b703-2484b1898e47");
                log.debug("Stack '" + stackName + "' is empty");
            }
        }
        writeline("/home/ubuntu/results/coverage/Digester/Digester_7_10.coverage", "cef22997-1b11-417d-ad93-8bfdfd969072");
        int index = (namedStack.size() - 1) - n;
        writeline("/home/ubuntu/results/coverage/Digester/Digester_7_10.coverage", "740012bd-747f-4803-8f72-dd8c8bbc5eb2");
        if (index < 0) {
        }
        writeline("/home/ubuntu/results/coverage/Digester/Digester_7_10.coverage", "b72e7cb0-f114-444e-9cb9-343a99e70131");
        result = this.<T>npeSafeCast(namedStack.get(index));
        writeline("/home/ubuntu/results/coverage/Digester/Digester_7_10.coverage", "91de8a1b-5731-42e4-914a-c356c1480324");
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
        writeline("/home/ubuntu/results/coverage/Digester/Digester_7_10.coverage", "c057b10f-5882-43fb-86e8-2cbd064dd3e3");
        boolean result = true;
        writeline("/home/ubuntu/results/coverage/Digester/Digester_7_10.coverage", "5fb05824-534d-4670-8a52-03ae4db2cf40");
        Stack<Object> namedStack = stacksByName.get(stackName);
        writeline("/home/ubuntu/results/coverage/Digester/Digester_7_10.coverage", "63961417-817d-402a-ab72-af727d1e86a3");
        if (namedStack != null) {
            writeline("/home/ubuntu/results/coverage/Digester/Digester_7_10.coverage", "b94fbe0c-354f-45bc-a45a-59691dac605a");
            result = namedStack.isEmpty();
        }
        writeline("/home/ubuntu/results/coverage/Digester/Digester_7_10.coverage", "0f0c3be4-e9d0-4d50-a9ac-a735df82df05");
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
        writeline("/home/ubuntu/results/coverage/Digester/Digester_7_10.coverage", "acb9a796-8d0f-431c-9dc3-117d062e9aad");
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
        writeline("/home/ubuntu/results/coverage/Digester/Digester_7_10.coverage", "c9debc15-e32f-48ff-816d-8af75e640dfe");
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
        writeline("/home/ubuntu/results/coverage/Digester/Digester_7_10.coverage", "5116910b-31e3-46da-a8dd-3488d4a263ed");
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
        writeline("/home/ubuntu/results/coverage/Digester/Digester_7_10.coverage", "3af98e16-3dc8-43e9-ab7b-3f876027c816");
        if (configured) {
            writeline("/home/ubuntu/results/coverage/Digester/Digester_7_10.coverage", "877903a6-40db-4766-9820-bd39d81429b9");
            return;
        }
        writeline("/home/ubuntu/results/coverage/Digester/Digester_7_10.coverage", "af554212-332f-4e05-a6c2-fc5bfbf8454e");
        initialize();
        writeline("/home/ubuntu/results/coverage/Digester/Digester_7_10.coverage", "5d00173e-5669-4ca5-b3a5-5d289da84595");
        configured = true;
    }

    /**
     * Checks the Digester instance has been configured.
     *
     * @return true, if the Digester instance has been configured, false otherwise
     * @since 3.0
     */
    public boolean isConfigured() {
        writeline("/home/ubuntu/results/coverage/Digester/Digester_7_10.coverage", "6a290e95-3179-4768-948f-e05e30937137");
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
        writeline("/home/ubuntu/results/coverage/Digester/Digester_7_10.coverage", "17be53fa-4b36-478c-8e16-4c5b658364cb");
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
        writeline("/home/ubuntu/results/coverage/Digester/Digester_7_10.coverage", "3f98f64b-d5c4-4664-8328-e87b6a20bc6c");
        try {
            writeline("/home/ubuntu/results/coverage/Digester/Digester_7_10.coverage", "9663cd71-9552-4309-9d69-a6aa05d602e3");
            return (params.peek());
        } catch (EmptyStackException e) {
            writeline("/home/ubuntu/results/coverage/Digester/Digester_7_10.coverage", "470e340e-fa49-47ed-95c3-f7428dcacf58");
            log.warn("Empty stack (returning null)");
            writeline("/home/ubuntu/results/coverage/Digester/Digester_7_10.coverage", "119983ef-b03d-4f8d-9520-ae0f905ad9cd");
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
        writeline("/home/ubuntu/results/coverage/Digester/Digester_7_10.coverage", "59022089-9649-4176-b7a2-5774d06edbd2");
        int index = (params.size() - 1) - n;
        writeline("/home/ubuntu/results/coverage/Digester/Digester_7_10.coverage", "14d4171b-c398-48bb-ad8a-1c7b9cdb6efd");
        if (index < 0) {
            writeline("/home/ubuntu/results/coverage/Digester/Digester_7_10.coverage", "5ef40797-659b-41b3-bd81-e2a6ea152eee");
            log.warn("Empty stack (returning null)");
            writeline("/home/ubuntu/results/coverage/Digester/Digester_7_10.coverage", "25f8c5a3-e3ac-40c7-9844-d85e24058c75");
            return (null);
        }
        writeline("/home/ubuntu/results/coverage/Digester/Digester_7_10.coverage", "ac1519e9-a950-4968-bab8-5ecce325c0f2");
        try {
            writeline("/home/ubuntu/results/coverage/Digester/Digester_7_10.coverage", "721b5e88-9a22-4833-ab40-1d18efb255f5");
            return (params.get(index));
        } catch (EmptyStackException e) {
            writeline("/home/ubuntu/results/coverage/Digester/Digester_7_10.coverage", "fca3f389-afbb-4302-8fa9-c99530a51b56");
            log.warn("Empty stack (returning null)");
            writeline("/home/ubuntu/results/coverage/Digester/Digester_7_10.coverage", "3b727912-1ba1-43c1-85dd-b10e15c6afb5");
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
        writeline("/home/ubuntu/results/coverage/Digester/Digester_7_10.coverage", "dc1052e1-2b3e-4d93-8d70-21dc56a9eb08");
        try {
            writeline("/home/ubuntu/results/coverage/Digester/Digester_7_10.coverage", "95434ac5-2ce9-498f-90dc-7b03f7e2d10a");
            if (log.isTraceEnabled()) {
                writeline("/home/ubuntu/results/coverage/Digester/Digester_7_10.coverage", "aa7a5c27-dd90-4123-819a-5834621911d4");
                log.trace("Popping params");
            }
            writeline("/home/ubuntu/results/coverage/Digester/Digester_7_10.coverage", "624b4043-eab6-4fca-a921-8505d80817b0");
            return (params.pop());
        } catch (EmptyStackException e) {
            writeline("/home/ubuntu/results/coverage/Digester/Digester_7_10.coverage", "519fe19f-f6d4-4cc5-803c-48616012cac3");
            log.warn("Empty stack (returning null)");
            writeline("/home/ubuntu/results/coverage/Digester/Digester_7_10.coverage", "eb2660fa-2851-4069-b6c7-3a182f8a40df");
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
        writeline("/home/ubuntu/results/coverage/Digester/Digester_7_10.coverage", "e729ab87-0e08-4953-a06d-9c13d2c3ae60");
        if (log.isTraceEnabled()) {
            writeline("/home/ubuntu/results/coverage/Digester/Digester_7_10.coverage", "eda6da42-6df1-4673-9079-d13a507cb2c8");
            log.trace("Pushing params");
        }
        writeline("/home/ubuntu/results/coverage/Digester/Digester_7_10.coverage", "04f06854-3e48-4f5c-bb99-28833ecc4ab4");
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
        writeline("/home/ubuntu/results/coverage/Digester/Digester_7_10.coverage", "6eea4ef2-5a55-48bc-af24-0e2e1ec39974");
        if ((e != null) && (e instanceof InvocationTargetException)) {
            writeline("/home/ubuntu/results/coverage/Digester/Digester_7_10.coverage", "9f0b255b-2283-47cb-b18a-76f0b2481f76");
            Throwable t = ((InvocationTargetException) e).getTargetException();
            writeline("/home/ubuntu/results/coverage/Digester/Digester_7_10.coverage", "ec7b8797-6610-49fd-a295-7ffd90e0fb81");
            if ((t != null) && (t instanceof Exception)) {
                writeline("/home/ubuntu/results/coverage/Digester/Digester_7_10.coverage", "7893bf65-8807-4ec2-933e-44cc67197598");
                e = (Exception) t;
            }
        }
        writeline("/home/ubuntu/results/coverage/Digester/Digester_7_10.coverage", "5f0c56db-5528-475a-ac87-cee266d46c79");
        if (locator != null) {
            writeline("/home/ubuntu/results/coverage/Digester/Digester_7_10.coverage", "1d1c78f9-5570-4bb0-b27b-bac1ea6df6c3");
            String error = "Error at line " + locator.getLineNumber() + " char " + locator.getColumnNumber() + ": " + message;
            writeline("/home/ubuntu/results/coverage/Digester/Digester_7_10.coverage", "d69b249c-adf4-4488-a0b1-38d13b1031ae");
            if (e != null) {
                writeline("/home/ubuntu/results/coverage/Digester/Digester_7_10.coverage", "2fe193d6-0837-44f0-92b6-5afad19fb911");
                return new SAXParseException(error, locator, e);
            }
            writeline("/home/ubuntu/results/coverage/Digester/Digester_7_10.coverage", "f8b58e6b-3166-4d7e-822b-59eb39b7be4d");
            return new SAXParseException(error, locator);
        }
        writeline("/home/ubuntu/results/coverage/Digester/Digester_7_10.coverage", "7124c8d8-003b-4701-baa3-efe474797647");
        log.error("No Locator!");
        writeline("/home/ubuntu/results/coverage/Digester/Digester_7_10.coverage", "d3bc4a92-94e6-4b45-94d5-7b5c295f0499");
        if (e != null) {
            writeline("/home/ubuntu/results/coverage/Digester/Digester_7_10.coverage", "47daa954-f8cd-4b18-ba63-3f80935be01e");
            return new SAXException(message, e);
        }
        writeline("/home/ubuntu/results/coverage/Digester/Digester_7_10.coverage", "32b989d4-5e5c-4207-a339-a060c30f97ff");
        return new SAXException(message);
    }

    /**
     * Create a SAX exception which also understands about the location in the digester file where the exception occurs
     *
     * @param e the exception cause
     * @return the new SAX exception
     */
    public SAXException createSAXException(Exception e) {
        writeline("/home/ubuntu/results/coverage/Digester/Digester_7_10.coverage", "65092dce-5691-4ebc-a468-39b0b2203b40");
        if (e instanceof InvocationTargetException) {
            writeline("/home/ubuntu/results/coverage/Digester/Digester_7_10.coverage", "460a859c-0b79-4d95-8dca-df890a773f49");
            Throwable t = ((InvocationTargetException) e).getTargetException();
            writeline("/home/ubuntu/results/coverage/Digester/Digester_7_10.coverage", "99682e60-5f27-4bf2-8a27-8bb30f0851eb");
            if ((t != null) && (t instanceof Exception)) {
                writeline("/home/ubuntu/results/coverage/Digester/Digester_7_10.coverage", "86c801b4-b74d-4930-8cf7-0cd5807e98f8");
                e = (Exception) t;
            }
        }
        writeline("/home/ubuntu/results/coverage/Digester/Digester_7_10.coverage", "42a4cabe-4f1e-4c97-acbc-2e931242c3f4");
        return createSAXException(e.getMessage(), e);
    }

    /**
     * Create a SAX exception which also understands about the location in the digester file where the exception occurs
     *
     * @param message the custom SAX exception message
     * @return the new SAX exception
     */
    public SAXException createSAXException(String message) {
        writeline("/home/ubuntu/results/coverage/Digester/Digester_7_10.coverage", "0a47c75e-68ae-450d-b215-59176bba9672");
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
        writeline("/home/ubuntu/results/coverage/Digester/Digester_7_10.coverage", "5481869c-eaa3-43e4-b486-a30bea95c490");
        if (obj == null) {
            writeline("/home/ubuntu/results/coverage/Digester/Digester_7_10.coverage", "4a9b13de-3e8b-4329-aa61-6447d5e9e190");
            return null;
        }
        writeline("/home/ubuntu/results/coverage/Digester/Digester_7_10.coverage", "f7b29554-0a10-433e-8210-7a370deafe48");
        @SuppressWarnings("unchecked") T result = (T) obj;
        writeline("/home/ubuntu/results/coverage/Digester/Digester_7_10.coverage", "369a153e-6be9-427a-9b44-b0fcf9b32ce5");
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
