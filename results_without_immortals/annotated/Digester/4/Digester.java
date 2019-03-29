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
        writeline("/home/ubuntu/results/coverage/Digester/Digester_4_10.coverage", "1e8437c8-8059-4777-b175-736cf10ecab2");
        Stack<String> nsStack = namespaces.get(prefix);
        writeline("/home/ubuntu/results/coverage/Digester/Digester_4_10.coverage", "5b858692-95fc-4494-a101-5ac26627c35c");
        if (nsStack == null) {
            writeline("/home/ubuntu/results/coverage/Digester/Digester_4_10.coverage", "f73ce81d-8088-4332-9672-524dbd86fe50");
            return null;
        }
        writeline("/home/ubuntu/results/coverage/Digester/Digester_4_10.coverage", "ac7896c0-5d8b-4670-ad49-650940214f8f");
        try {
            writeline("/home/ubuntu/results/coverage/Digester/Digester_4_10.coverage", "2eac7fac-2d31-4f2e-b742-b232319d5cf9");
            return (nsStack.peek());
        } catch (EmptyStackException e) {
            writeline("/home/ubuntu/results/coverage/Digester/Digester_4_10.coverage", "6f35d37c-229a-4680-ae92-af535eba08a5");
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
        writeline("/home/ubuntu/results/coverage/Digester/Digester_4_10.coverage", "c5d7e96c-c5f8-4cb0-9279-fd2301ee26ce");
        if (this.classLoader != null) {
            writeline("/home/ubuntu/results/coverage/Digester/Digester_4_10.coverage", "3d3e92b1-0244-4176-ba8f-fd67134adf1d");
            return (this.classLoader);
        }
        writeline("/home/ubuntu/results/coverage/Digester/Digester_4_10.coverage", "d54e20e7-4474-4bb7-a196-31da4ce0d50f");
        if (this.useContextClassLoader) {
            writeline("/home/ubuntu/results/coverage/Digester/Digester_4_10.coverage", "17165e3a-9145-4aeb-bb68-a88b07ed5636");
            ClassLoader classLoader = Thread.currentThread().getContextClassLoader();
            writeline("/home/ubuntu/results/coverage/Digester/Digester_4_10.coverage", "610b4028-d32b-487d-8cea-7bd744baf8dd");
            if (classLoader != null) {
                writeline("/home/ubuntu/results/coverage/Digester/Digester_4_10.coverage", "4b8e8e9d-1efd-4c8a-bcd9-75b9c1511a72");
                return (classLoader);
            }
        }
        writeline("/home/ubuntu/results/coverage/Digester/Digester_4_10.coverage", "efcda268-6e57-4c9b-a33e-895105e60d33");
        return (this.getClass().getClassLoader());
    }

    /**
     * Set the class loader to be used for instantiating application objects when required.
     *
     * @param classLoader The new class loader to use, or <code>null</code> to revert to the standard rules
     */
    public void setClassLoader(ClassLoader classLoader) {
        writeline("/home/ubuntu/results/coverage/Digester/Digester_4_10.coverage", "33624234-c295-42cd-8f6f-c120ab723598");
        this.classLoader = classLoader;
    }

    /**
     * Return the current depth of the element stack.
     *
     * @return the current depth of the element stack.
     */
    public int getCount() {
        writeline("/home/ubuntu/results/coverage/Digester/Digester_4_10.coverage", "12d370d3-bcb9-4578-975f-e410b5db176a");
        return (stack.size());
    }

    /**
     * Return the name of the XML element that is currently being processed.
     *
     * @return the name of the XML element that is currently being processed.
     */
    public String getCurrentElementName() {
        writeline("/home/ubuntu/results/coverage/Digester/Digester_4_10.coverage", "c60c7957-8ef5-4a87-b769-5fdecb6393e4");
        String elementName = match;
        writeline("/home/ubuntu/results/coverage/Digester/Digester_4_10.coverage", "3fb042c5-1d86-4396-8963-bd75665a05a8");
        int lastSlash = elementName.lastIndexOf('/');
        writeline("/home/ubuntu/results/coverage/Digester/Digester_4_10.coverage", "c6142940-8289-47f2-99e5-e71e6e141a38");
        if (lastSlash >= 0) {
            writeline("/home/ubuntu/results/coverage/Digester/Digester_4_10.coverage", "6c97ca06-5c61-490d-bb38-440b60704767");
            elementName = elementName.substring(lastSlash + 1);
        }
        writeline("/home/ubuntu/results/coverage/Digester/Digester_4_10.coverage", "b1037904-80ab-451e-8a79-1cc71ff2de86");
        return (elementName);
    }

    /**
     * Return the error handler for this Digester.
     *
     * @return the error handler for this Digester.
     */
    public ErrorHandler getErrorHandler() {
        writeline("/home/ubuntu/results/coverage/Digester/Digester_4_10.coverage", "d8f5f922-91cf-428d-968e-43622d934e44");
        return (this.errorHandler);
    }

    /**
     * Set the error handler for this Digester.
     *
     * @param errorHandler The new error handler
     */
    public void setErrorHandler(ErrorHandler errorHandler) {
        writeline("/home/ubuntu/results/coverage/Digester/Digester_4_10.coverage", "a75f8076-9d6d-4c41-ad30-ba1b123dd510");
        this.errorHandler = errorHandler;
    }

    /**
     * Return the SAXParserFactory we will use, creating one if necessary.
     *
     * @return the SAXParserFactory we will use, creating one if necessary.
     */
    public SAXParserFactory getFactory() {
        writeline("/home/ubuntu/results/coverage/Digester/Digester_4_10.coverage", "cb29477e-5b58-4ab3-8109-26600c802d2a");
        if (factory == null) {
            writeline("/home/ubuntu/results/coverage/Digester/Digester_4_10.coverage", "2374a071-1d89-4d00-b503-8899d7b1599a");
            factory = SAXParserFactory.newInstance();
            writeline("/home/ubuntu/results/coverage/Digester/Digester_4_10.coverage", "b5b5a577-eee8-46a2-a90c-7505e7bade36");
            factory.setNamespaceAware(namespaceAware);
            writeline("/home/ubuntu/results/coverage/Digester/Digester_4_10.coverage", "f0f7a6eb-9b70-43da-bd40-c1ab96bc1e11");
            factory.setXIncludeAware(xincludeAware);
            writeline("/home/ubuntu/results/coverage/Digester/Digester_4_10.coverage", "fe380ec0-89bd-4344-8a5f-23710c882439");
            factory.setValidating(validating);
            writeline("/home/ubuntu/results/coverage/Digester/Digester_4_10.coverage", "c60c8be6-287d-41b2-9ba8-ca387aada592");
            factory.setSchema(schema);
        }
        writeline("/home/ubuntu/results/coverage/Digester/Digester_4_10.coverage", "abfe4b10-eb67-428a-a22a-f0c9cce4b675");
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
        writeline("/home/ubuntu/results/coverage/Digester/Digester_4_10.coverage", "aeaaef61-4ca7-450f-96ad-fbe8f8b5e7e0");
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
        writeline("/home/ubuntu/results/coverage/Digester/Digester_4_10.coverage", "0a0f7ebb-730a-4306-b3fe-e35ee663bca0");
        getFactory().setFeature(feature, value);
    }

    /**
     * Return the current Logger associated with this instance of the Digester
     *
     * @return the current Logger associated with this instance of the Digester
     */
    public Log getLogger() {
        writeline("/home/ubuntu/results/coverage/Digester/Digester_4_10.coverage", "15b422ff-9818-4ffd-85c5-09f87234edbe");
        return log;
    }

    /**
     * Set the current logger for this Digester.
     *
     * @param log the current logger for this Digester.
     */
    public void setLogger(Log log) {
        writeline("/home/ubuntu/results/coverage/Digester/Digester_4_10.coverage", "387bfcc7-7db7-4dc9-a92d-812aaa1cb263");
        this.log = log;
    }

    /**
     * Gets the logger used for logging SAX-related information. <strong>Note</strong> the output is finely grained.
     *
     * @return the logger used for logging SAX-related information
     * @since 1.6
     */
    public Log getSAXLogger() {
        writeline("/home/ubuntu/results/coverage/Digester/Digester_4_10.coverage", "db1ce4a4-391a-41df-8334-5b02e5e646e6");
        return saxLog;
    }

    /**
     * Sets the logger used for logging SAX-related information. <strong>Note</strong> the output is finely grained.
     *
     * @param saxLog the logger used for logging SAX-related information, not null
     * @since 1.6
     */
    public void setSAXLogger(Log saxLog) {
        writeline("/home/ubuntu/results/coverage/Digester/Digester_4_10.coverage", "7d65fe10-2347-4c27-9b5b-d8dc9a809df3");
        this.saxLog = saxLog;
    }

    /**
     * Return the current rule match path
     *
     * @return the current rule match path
     */
    public String getMatch() {
        writeline("/home/ubuntu/results/coverage/Digester/Digester_4_10.coverage", "34d3f33a-5d27-4aa9-ba7e-3e5864c0aa3d");
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
        writeline("/home/ubuntu/results/coverage/Digester/Digester_4_10.coverage", "0d8fb1d1-2c64-4ffc-88d4-e7f8468a3cc0");
        return matches;
    }

    /**
     * Return the "namespace aware" flag for parsers we create.
     *
     * @return the "namespace aware" flag for parsers we create.
     */
    public boolean getNamespaceAware() {
        writeline("/home/ubuntu/results/coverage/Digester/Digester_4_10.coverage", "4cf796fc-b8f3-4409-bf32-97d74757372f");
        return (this.namespaceAware);
    }

    /**
     * Set the "namespace aware" flag for parsers we create.
     *
     * @param namespaceAware The new "namespace aware" flag
     */
    public void setNamespaceAware(boolean namespaceAware) {
        writeline("/home/ubuntu/results/coverage/Digester/Digester_4_10.coverage", "ad5c722e-ba76-420d-845b-d37116971980");
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
        writeline("/home/ubuntu/results/coverage/Digester/Digester_4_10.coverage", "cd7916cb-aba8-44f6-89ed-3063c845f9ba");
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
        writeline("/home/ubuntu/results/coverage/Digester/Digester_4_10.coverage", "8199c4c0-f253-4801-b6e4-b498455262c4");
        this.xincludeAware = xincludeAware;
    }

    /**
     * Set the public id of the current file being parse.
     *
     * @param publicId the DTD/Schema public's id.
     */
    public void setPublicId(String publicId) {
        writeline("/home/ubuntu/results/coverage/Digester/Digester_4_10.coverage", "803cefe5-710e-4e99-a697-4a2821f0d51b");
        this.publicId = publicId;
    }

    /**
     * Return the public identifier of the DTD we are currently parsing under, if any.
     *
     * @return the public identifier of the DTD we are currently parsing under, if any.
     */
    public String getPublicId() {
        writeline("/home/ubuntu/results/coverage/Digester/Digester_4_10.coverage", "1e0b06f0-3a12-4519-b738-061cee205a0b");
        return (this.publicId);
    }

    /**
     * Return the namespace URI that will be applied to all subsequently added <code>Rule</code> objects.
     *
     * @return the namespace URI that will be applied to all subsequently added <code>Rule</code> objects.
     */
    public String getRuleNamespaceURI() {
        writeline("/home/ubuntu/results/coverage/Digester/Digester_4_10.coverage", "b5fb2441-6134-4d98-965c-e66fa4f65ddc");
        return (getRules().getNamespaceURI());
    }

    /**
     * Set the namespace URI that will be applied to all subsequently added <code>Rule</code> objects.
     *
     * @param ruleNamespaceURI Namespace URI that must match on all subsequently added rules, or <code>null</code> for
     *            matching regardless of the current namespace URI
     */
    public void setRuleNamespaceURI(String ruleNamespaceURI) {
        writeline("/home/ubuntu/results/coverage/Digester/Digester_4_10.coverage", "51e0c801-f33f-48ad-b4dd-b42e3352c2a6");
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
        writeline("/home/ubuntu/results/coverage/Digester/Digester_4_10.coverage", "b1096d7a-b130-40e2-bf2c-668505ad8ab9");
        if (parser != null) {
            writeline("/home/ubuntu/results/coverage/Digester/Digester_4_10.coverage", "a004b94e-0b1e-4a7d-aeae-b755ca83b11a");
            return (parser);
        }
        writeline("/home/ubuntu/results/coverage/Digester/Digester_4_10.coverage", "42db173b-1c0f-4a3a-8313-adc74cf1a932");
        try {
            writeline("/home/ubuntu/results/coverage/Digester/Digester_4_10.coverage", "1573626c-bdcd-439d-bbf9-c54cc0eb8b6d");
            parser = getFactory().newSAXParser();
        } catch (Exception e) {
            writeline("/home/ubuntu/results/coverage/Digester/Digester_4_10.coverage", "01fd6bde-6ba2-44db-a0c4-a12130fc9dc0");
            log.error("Digester.getParser: ", e);
            writeline("/home/ubuntu/results/coverage/Digester/Digester_4_10.coverage", "9e1b36de-99b7-46d7-869d-c700f5db5f06");
            return (null);
        }
        writeline("/home/ubuntu/results/coverage/Digester/Digester_4_10.coverage", "7741e603-c3d0-4b2a-8487-2287656bec2d");
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
        writeline("/home/ubuntu/results/coverage/Digester/Digester_4_10.coverage", "ff083aad-4567-4292-97a5-c17060afe4bb");
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
        writeline("/home/ubuntu/results/coverage/Digester/Digester_4_10.coverage", "2fedf254-4068-4f81-8ff6-43b260722e65");
        getParser().setProperty(property, value);
    }

    /**
     * Return the <code>Rules</code> implementation object containing our rules collection and associated matching
     * policy. If none has been established, a default implementation will be created and returned.
     *
     * @return the <code>Rules</code> implementation object.
     */
    public Rules getRules() {
        writeline("/home/ubuntu/results/coverage/Digester/Digester_4_10.coverage", "a198d986-7c90-411d-b709-bded2e5e4432");
        if (this.rules == null) {
            writeline("/home/ubuntu/results/coverage/Digester/Digester_4_10.coverage", "f0cba01d-bd2b-47b1-aeab-5ed2be3a367c");
            this.rules = new RulesBase();
            writeline("/home/ubuntu/results/coverage/Digester/Digester_4_10.coverage", "f2e58963-0db8-43e0-830e-dcfee5926f72");
            this.rules.setDigester(this);
        }
        writeline("/home/ubuntu/results/coverage/Digester/Digester_4_10.coverage", "e38a3bf5-9d67-4b6d-96ec-b4fed9344852");
        return (this.rules);
    }

    /**
     * Set the <code>Rules</code> implementation object containing our rules collection and associated matching policy.
     *
     * @param rules New Rules implementation
     */
    public void setRules(Rules rules) {
        writeline("/home/ubuntu/results/coverage/Digester/Digester_4_10.coverage", "1ddfe0b7-d5f4-414c-9968-7e9ad232fe54");
        this.rules = rules;
        writeline("/home/ubuntu/results/coverage/Digester/Digester_4_10.coverage", "23db044c-7471-4f1b-95d7-e33bae0f2231");
        this.rules.setDigester(this);
    }

    /**
     * Return the XML Schema used when parsing.
     *
     * @return The {@link Schema} instance in use.
     * @since 2.0
     */
    public Schema getXMLSchema() {
        writeline("/home/ubuntu/results/coverage/Digester/Digester_4_10.coverage", "0603dd52-a8c4-44ce-953b-d826566c9cb6");
        return (this.schema);
    }

    /**
     * Set the XML Schema to be used when parsing.
     *
     * @param schema The {@link Schema} instance to use.
     * @since 2.0
     */
    public void setXMLSchema(Schema schema) {
        writeline("/home/ubuntu/results/coverage/Digester/Digester_4_10.coverage", "c62a82a2-fd5f-4616-bdf1-65ff89427b68");
        this.schema = schema;
    }

    /**
     * Return the boolean as to whether the context ClassLoader should be used.
     *
     * @return true, if the context ClassLoader should be used, false otherwise.
     */
    public boolean getUseContextClassLoader() {
        writeline("/home/ubuntu/results/coverage/Digester/Digester_4_10.coverage", "ce6b88f2-fe4c-4da9-b68b-89b116dc81ef");
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
        writeline("/home/ubuntu/results/coverage/Digester/Digester_4_10.coverage", "8f2ec11a-1682-427e-86ee-09b7335444f4");
        useContextClassLoader = use;
    }

    /**
     * Return the validating parser flag.
     *
     * @return the validating parser flag.
     */
    public boolean getValidating() {
        writeline("/home/ubuntu/results/coverage/Digester/Digester_4_10.coverage", "5b9c25f9-3b16-4b4d-9f16-bb11574d0953");
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
        writeline("/home/ubuntu/results/coverage/Digester/Digester_4_10.coverage", "70e9b6d1-cf46-447e-bbe7-458f0c064b83");
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
        writeline("/home/ubuntu/results/coverage/Digester/Digester_4_10.coverage", "63c6d0c8-a64a-4313-a1d7-f2cbd41c2109");
        if (reader == null) {
            writeline("/home/ubuntu/results/coverage/Digester/Digester_4_10.coverage", "604aae2f-3925-4bc0-81c5-3c69e7bfcf8d");
            reader = getParser().getXMLReader();
        }
        writeline("/home/ubuntu/results/coverage/Digester/Digester_4_10.coverage", "227ffe89-2a6b-46ac-bd02-302690b6b9de");
        reader.setDTDHandler(this);
        writeline("/home/ubuntu/results/coverage/Digester/Digester_4_10.coverage", "7cb1a5af-0b33-4ecb-915d-f93d66af1b84");
        reader.setContentHandler(this);
        writeline("/home/ubuntu/results/coverage/Digester/Digester_4_10.coverage", "8b8ef357-d1a7-409f-9e70-64af260061de");
        if (entityResolver == null) {
            writeline("/home/ubuntu/results/coverage/Digester/Digester_4_10.coverage", "fb0b07a6-1ac7-4aa6-b1e0-1e846d868725");
            reader.setEntityResolver(this);
        } else {
            writeline("/home/ubuntu/results/coverage/Digester/Digester_4_10.coverage", "b1d6497f-0721-454f-a18b-5b7e758693dc");
            reader.setEntityResolver(entityResolver);
        }
        writeline("/home/ubuntu/results/coverage/Digester/Digester_4_10.coverage", "6dcee8b9-da86-4bfb-bbf4-f22a0cca6425");
        if (this.errorHandler != null) {
            writeline("/home/ubuntu/results/coverage/Digester/Digester_4_10.coverage", "6d69d06f-0c12-48e3-aaff-46d1714ee375");
            reader.setErrorHandler(this.errorHandler);
        } else {
            writeline("/home/ubuntu/results/coverage/Digester/Digester_4_10.coverage", "d97bae99-18ed-4a41-8c7f-1bce69fba4e9");
            reader.setErrorHandler(this);
        }
        writeline("/home/ubuntu/results/coverage/Digester/Digester_4_10.coverage", "229bf61d-ec13-4d5e-afb3-f369c033d57f");
        return reader;
    }

    /**
     * Gets the <code>Substitutor</code> used to convert attributes and body text.
     *
     * @return the <code>Substitutor</code> used to convert attributes and body text,
     *         null if not substitutions are to be performed.
     */
    public Substitutor getSubstitutor() {
        writeline("/home/ubuntu/results/coverage/Digester/Digester_4_10.coverage", "74f62dcd-5187-46b1-9d66-d6d116a56734");
        return substitutor;
    }

    /**
     * Sets the <code>Substitutor</code> to be used to convert attributes and body text.
     *
     * @param substitutor the Substitutor to be used to convert attributes and body text or null if not substitution of
     *            these values is to be performed.
     */
    public void setSubstitutor(Substitutor substitutor) {
        writeline("/home/ubuntu/results/coverage/Digester/Digester_4_10.coverage", "1dd4b4e6-a575-490a-a85b-96868b854df8");
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
        writeline("/home/ubuntu/results/coverage/Digester/Digester_4_10.coverage", "312f1919-2df8-4779-aefd-89988f0a7336");
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
        writeline("/home/ubuntu/results/coverage/Digester/Digester_4_10.coverage", "d715fe4c-54d1-4f76-931d-0c72913cde6b");
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
        writeline("/home/ubuntu/results/coverage/Digester/Digester_4_10.coverage", "df111ed7-5dfe-49ad-b642-b3425c5ebcc5");
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
        writeline("/home/ubuntu/results/coverage/Digester/Digester_4_10.coverage", "798f6537-c0be-43b3-ad94-bc4415385aeb");
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
        writeline("/home/ubuntu/results/coverage/Digester/Digester_4_10.coverage", "4f592e61-ea50-44eb-a899-6cab4f4f3080");
        if (!namespaceAware) {
            writeline("/home/ubuntu/results/coverage/Digester/Digester_4_10.coverage", "81a0078b-3006-47e7-8951-9a437b5043f7");
            log.warn("Digester is not namespace aware");
        }
        writeline("/home/ubuntu/results/coverage/Digester/Digester_4_10.coverage", "83d8c0c0-cbff-41a0-bbf7-31b64fee4537");
        Map<String, String> currentNamespaces = new HashMap<String, String>();
        writeline("/home/ubuntu/results/coverage/Digester/Digester_4_10.coverage", "b7698deb-549d-4e42-88fe-36f693229343");
        for (Map.Entry<String, Stack<String>> nsEntry : namespaces.entrySet()) {
            writeline("/home/ubuntu/results/coverage/Digester/Digester_4_10.coverage", "a2a92f50-7e4a-4330-841b-7a0fb06a9be9");
            try {
                writeline("/home/ubuntu/results/coverage/Digester/Digester_4_10.coverage", "da21a7c5-98f9-4dcb-9ef0-72fd955e0fea");
                currentNamespaces.put(nsEntry.getKey(), nsEntry.getValue().peek());
            } catch (RuntimeException e) {
                writeline("/home/ubuntu/results/coverage/Digester/Digester_4_10.coverage", "1fb1359e-c256-4298-b8ff-c7bb9038f86c");
                log.error(e.getMessage(), e);
                writeline("/home/ubuntu/results/coverage/Digester/Digester_4_10.coverage", "ce35be3f-960f-4b74-a659-242040cf41f1");
                throw e;
            }
        }
        writeline("/home/ubuntu/results/coverage/Digester/Digester_4_10.coverage", "1c3c50d7-3334-4624-a79e-6020c012290b");
        return currentNamespaces;
    }

    /**
     * Returns the executor service used to run asynchronous parse method.
     *
     * @return the executor service used to run asynchronous parse method
     * @since 3.1
     */
    public ExecutorService getExecutorService() {
        writeline("/home/ubuntu/results/coverage/Digester/Digester_4_10.coverage", "cd2a5c01-ffcc-41cf-95e5-91a728a66130");
        return executorService;
    }

    /**
     * Sets the executor service to run asynchronous parse method.
     *
     * @param executorService the executor service to run asynchronous parse method
     * @since 3.1
     */
    public void setExecutorService(ExecutorService executorService) {
        writeline("/home/ubuntu/results/coverage/Digester/Digester_4_10.coverage", "08cc51ca-c963-4742-b6ed-0dac3498e64f");
        this.executorService = executorService;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public void characters(char buffer[], int start, int length) throws SAXException {
        writeline("/home/ubuntu/results/coverage/Digester/Digester_4_10.coverage", "a71ba862-c3d3-4387-a4c7-b359eb661860");
        if (customContentHandler != null) {
            writeline("/home/ubuntu/results/coverage/Digester/Digester_4_10.coverage", "ebde55f8-2f4f-4a65-a093-ac195d80b574");
            customContentHandler.characters(buffer, start, length);
            writeline("/home/ubuntu/results/coverage/Digester/Digester_4_10.coverage", "61915f02-1c24-47cf-b50f-7626e7220215");
            return;
        }
        writeline("/home/ubuntu/results/coverage/Digester/Digester_4_10.coverage", "3472f31e-08e2-4f91-9fd7-ed222e7ef168");
        if (saxLog.isDebugEnabled()) {
            writeline("/home/ubuntu/results/coverage/Digester/Digester_4_10.coverage", "34d5b1e9-4c52-4646-bf3d-c38d7fa49ac6");
            saxLog.debug("characters(" + new String(buffer, start, length) + ")");
        }
        writeline("/home/ubuntu/results/coverage/Digester/Digester_4_10.coverage", "f5086134-3445-49ec-bfec-274b5182c6f6");
        bodyText.append(buffer, start, length);
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public void endDocument() throws SAXException {
        writeline("/home/ubuntu/results/coverage/Digester/Digester_4_10.coverage", "d3040645-b1b9-43fe-b3a1-6395a82e4006");
        if (saxLog.isDebugEnabled()) {
            writeline("/home/ubuntu/results/coverage/Digester/Digester_4_10.coverage", "234a7dd9-a0d1-4489-aa21-76e633c9a208");
            if (getCount() > 1) {
                writeline("/home/ubuntu/results/coverage/Digester/Digester_4_10.coverage", "67797e10-ea95-4eb8-b29d-7ee052109038");
                saxLog.debug("endDocument():  " + getCount() + " elements left");
            } else {
                writeline("/home/ubuntu/results/coverage/Digester/Digester_4_10.coverage", "fc64e505-f538-4a2f-affb-b7ce10f17100");
                saxLog.debug("endDocument()");
            }
        }
        writeline("/home/ubuntu/results/coverage/Digester/Digester_4_10.coverage", "90ec6018-d88f-4d0b-9d3b-b91da2ff82f5");
        for (Rule rule : getRules().rules()) {
            writeline("/home/ubuntu/results/coverage/Digester/Digester_4_10.coverage", "f7aca118-d437-4b01-bc94-a38a879307f0");
            try {
                writeline("/home/ubuntu/results/coverage/Digester/Digester_4_10.coverage", "723d2f0e-5f0a-4bdc-af8d-a4abf126a1d8");
                rule.finish();
            } catch (Exception e) {
                writeline("/home/ubuntu/results/coverage/Digester/Digester_4_10.coverage", "4f923e69-e66f-47e0-97ba-ea1337d61147");
                log.error("Finish event threw exception", e);
                writeline("/home/ubuntu/results/coverage/Digester/Digester_4_10.coverage", "c9728acd-a792-4757-9d49-05437e02da48");
                throw createSAXException(e);
            } catch (Error e) {
                writeline("/home/ubuntu/results/coverage/Digester/Digester_4_10.coverage", "47018ea6-1c94-49e6-b28d-453f47d1dc6e");
                log.error("Finish event threw error", e);
                writeline("/home/ubuntu/results/coverage/Digester/Digester_4_10.coverage", "9b5196f0-660a-485d-b6fd-f8cf74dff2a0");
                throw e;
            }
        }
        writeline("/home/ubuntu/results/coverage/Digester/Digester_4_10.coverage", "752b7994-8b16-47e7-853d-fc3a7c4651ef");
        clear();
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public void endElement(String namespaceURI, String localName, String qName) throws SAXException {
        writeline("/home/ubuntu/results/coverage/Digester/Digester_4_10.coverage", "76463c74-b848-4323-96fd-eaf7d7191f59");
        if (customContentHandler != null) {
            writeline("/home/ubuntu/results/coverage/Digester/Digester_4_10.coverage", "4808b59d-4a91-43a1-865c-7446c7469783");
            customContentHandler.endElement(namespaceURI, localName, qName);
            writeline("/home/ubuntu/results/coverage/Digester/Digester_4_10.coverage", "232a3f20-d274-4b5a-b9c6-9790088fa143");
            return;
        }
        writeline("/home/ubuntu/results/coverage/Digester/Digester_4_10.coverage", "4fadadb2-a56a-4c98-90e9-b6d97cde04a6");
        boolean debug = log.isDebugEnabled();
        writeline("/home/ubuntu/results/coverage/Digester/Digester_4_10.coverage", "111168c9-de4d-459e-95c7-ea93ad72abf9");
        if (debug) {
            writeline("/home/ubuntu/results/coverage/Digester/Digester_4_10.coverage", "a1462c20-7a58-4361-b711-01dc880f0cd2");
            if (saxLog.isDebugEnabled()) {
                writeline("/home/ubuntu/results/coverage/Digester/Digester_4_10.coverage", "bf761771-d226-4885-ae1a-c9eb14a4b160");
                saxLog.debug("endElement(" + namespaceURI + "," + localName + "," + qName + ")");
            }
            writeline("/home/ubuntu/results/coverage/Digester/Digester_4_10.coverage", "820a2325-a2c0-45c1-82d2-2fe2833140d0");
            log.debug("  match='" + match + "'");
            writeline("/home/ubuntu/results/coverage/Digester/Digester_4_10.coverage", "1478c7de-4719-40f9-94e1-0c6f8de1ac58");
            log.debug("  bodyText='" + bodyText + "'");
        }
        writeline("/home/ubuntu/results/coverage/Digester/Digester_4_10.coverage", "c7cb9011-56b2-4235-b469-738f665312d3");
        String name = localName;
        writeline("/home/ubuntu/results/coverage/Digester/Digester_4_10.coverage", "ce8c56f5-19d9-412d-a67f-37b2152ac4e9");
        if ((name == null) || (name.length() < 1)) {
            writeline("/home/ubuntu/results/coverage/Digester/Digester_4_10.coverage", "ca14477f-fd34-452a-abbb-bade269af101");
            name = qName;
        }
        writeline("/home/ubuntu/results/coverage/Digester/Digester_4_10.coverage", "6f695e12-e015-44af-98a6-c11b22500afe");
        List<Rule> rules = matches.pop();
        writeline("/home/ubuntu/results/coverage/Digester/Digester_4_10.coverage", "4a94edec-a2b1-4409-948f-99439f75a5fb");
        if ((rules != null) && (rules.size() > 0)) {
            writeline("/home/ubuntu/results/coverage/Digester/Digester_4_10.coverage", "52d4f7f5-c1a8-4676-91d7-9c825e3aa263");
            String bodyText = this.bodyText.toString();
            writeline("/home/ubuntu/results/coverage/Digester/Digester_4_10.coverage", "e73222cc-0d85-4db9-aadf-6e93ef902459");
            Substitutor substitutor = getSubstitutor();
            writeline("/home/ubuntu/results/coverage/Digester/Digester_4_10.coverage", "b6ad80da-ba9a-465f-9a70-785766459b75");
            if (substitutor != null) {
                writeline("/home/ubuntu/results/coverage/Digester/Digester_4_10.coverage", "ea0cba9f-be21-48d7-8a55-e0041fc9ccf2");
                bodyText = substitutor.substitute(bodyText);
            }
            writeline("/home/ubuntu/results/coverage/Digester/Digester_4_10.coverage", "b73cc587-db38-479d-9360-f004635a5200");
            for (int i = 0; i < rules.size(); i++) {
                writeline("/home/ubuntu/results/coverage/Digester/Digester_4_10.coverage", "e30a6d61-0aa1-436c-98c1-d88bc4bc8428");
                try {
                    writeline("/home/ubuntu/results/coverage/Digester/Digester_4_10.coverage", "c13abf57-e3d4-4e7c-9807-8b1067a8a8df");
                    Rule rule = rules.get(i);
                    writeline("/home/ubuntu/results/coverage/Digester/Digester_4_10.coverage", "71af3b6b-10b1-4b15-b168-1ea00d400af9");
                    if (debug) {
                        writeline("/home/ubuntu/results/coverage/Digester/Digester_4_10.coverage", "3451f997-a57b-434f-8a82-fa84491609a2");
                        log.debug("  Fire body() for " + rule);
                    }
                    writeline("/home/ubuntu/results/coverage/Digester/Digester_4_10.coverage", "d082e15a-cb59-4e7c-b536-ec1ea67acc7b");
                    rule.body(namespaceURI, name, bodyText);
                } catch (Exception e) {
                    writeline("/home/ubuntu/results/coverage/Digester/Digester_4_10.coverage", "2e937f5c-5a4e-4296-bbbb-d1eba96d5c50");
                    log.error("Body event threw exception", e);
                    writeline("/home/ubuntu/results/coverage/Digester/Digester_4_10.coverage", "4c0af527-cb45-43d9-90be-07b12822a7bb");
                    throw createSAXException(e);
                } catch (Error e) {
                    writeline("/home/ubuntu/results/coverage/Digester/Digester_4_10.coverage", "8078cd4b-7e40-434f-a39a-5ae48143813e");
                    log.error("Body event threw error", e);
                    writeline("/home/ubuntu/results/coverage/Digester/Digester_4_10.coverage", "31667457-2164-4d9b-856b-32ff8b8b876c");
                    throw e;
                }
            }
        } else {
            writeline("/home/ubuntu/results/coverage/Digester/Digester_4_10.coverage", "ad04e0b8-09d1-4a9b-bc14-bdf60c706c02");
            if (debug) {
                writeline("/home/ubuntu/results/coverage/Digester/Digester_4_10.coverage", "95e1840e-54c6-4e2f-8b58-b9804d1a5245");
                log.debug("  No rules found matching '" + match + "'.");
            }
        }
        writeline("/home/ubuntu/results/coverage/Digester/Digester_4_10.coverage", "33d8e510-5728-43f9-b990-bc7264981a01");
        bodyText = bodyTexts.pop();
        writeline("/home/ubuntu/results/coverage/Digester/Digester_4_10.coverage", "82fca663-899f-491e-a677-080db2709b1f");
        if (debug) {
            writeline("/home/ubuntu/results/coverage/Digester/Digester_4_10.coverage", "a3c1f5c2-6239-4e6f-8f18-b785618a0d76");
            log.debug("  Popping body text '" + bodyText.toString() + "'");
        }
        writeline("/home/ubuntu/results/coverage/Digester/Digester_4_10.coverage", "0ed6f815-3893-4aae-a08a-61eb80bf4932");
        if (rules != null) {
            writeline("/home/ubuntu/results/coverage/Digester/Digester_4_10.coverage", "3d146d91-263b-4082-8461-f247744565a3");
            for (int i = 0; i < rules.size(); i++) {
                writeline("/home/ubuntu/results/coverage/Digester/Digester_4_10.coverage", "8c5caae9-a7c9-4cbf-990c-685c187bec7f");
                int j = (rules.size() - i) - 1;
                writeline("/home/ubuntu/results/coverage/Digester/Digester_4_10.coverage", "c1d1cf7b-e749-4c9b-8e21-9565c4535fee");
                try {
                    writeline("/home/ubuntu/results/coverage/Digester/Digester_4_10.coverage", "9d8c928d-05b9-410a-ab66-975cedfc9121");
                    Rule rule = rules.get(j);
                    writeline("/home/ubuntu/results/coverage/Digester/Digester_4_10.coverage", "c3a89bfb-c466-4419-9141-11b942cbce7f");
                    if (debug) {
                        writeline("/home/ubuntu/results/coverage/Digester/Digester_4_10.coverage", "1fc209dc-deb7-424d-9b74-9c8800cacaeb");
                        log.debug("  Fire end() for " + rule);
                    }
                    writeline("/home/ubuntu/results/coverage/Digester/Digester_4_10.coverage", "6a45c031-7c91-44be-92d7-d94e72eb353f");
                    rule.end(namespaceURI, name);
                } catch (Exception e) {
                    writeline("/home/ubuntu/results/coverage/Digester/Digester_4_10.coverage", "f85e43a6-1310-41f2-bfb8-8ec013a2d547");
                    log.error("End event threw exception", e);
                    writeline("/home/ubuntu/results/coverage/Digester/Digester_4_10.coverage", "6adae623-cadd-4785-a2a9-e360899de72e");
                    throw createSAXException(e);
                } catch (Error e) {
                    writeline("/home/ubuntu/results/coverage/Digester/Digester_4_10.coverage", "3b534cd0-c23a-41d9-9301-05c057f29876");
                    log.error("End event threw error", e);
                    writeline("/home/ubuntu/results/coverage/Digester/Digester_4_10.coverage", "ff3af84d-e0d6-40be-aa74-a0131a2a0fca");
                    throw e;
                }
            }
        }
        writeline("/home/ubuntu/results/coverage/Digester/Digester_4_10.coverage", "3b356f9d-9e81-4838-9b0f-4edf4fd1f908");
        int slash = match.lastIndexOf('/');
        writeline("/home/ubuntu/results/coverage/Digester/Digester_4_10.coverage", "ae14f5cd-14e8-4e44-8a4d-d5234451c276");
        if (slash >= 0) {
            writeline("/home/ubuntu/results/coverage/Digester/Digester_4_10.coverage", "994d5784-dcf5-4b43-a367-1d0aae805792");
            match = match.substring(0, slash);
        } else {
            writeline("/home/ubuntu/results/coverage/Digester/Digester_4_10.coverage", "29b60051-a235-4411-a18a-06bccace4014");
            match = "";
        }
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public void endPrefixMapping(String prefix) throws SAXException {
        writeline("/home/ubuntu/results/coverage/Digester/Digester_4_10.coverage", "a90cf297-f3bf-4896-ace5-df096f40aeb2");
        if (saxLog.isDebugEnabled()) {
            writeline("/home/ubuntu/results/coverage/Digester/Digester_4_10.coverage", "8663771c-01c9-473e-bc39-5851755f5767");
            saxLog.debug("endPrefixMapping(" + prefix + ")");
        }
        writeline("/home/ubuntu/results/coverage/Digester/Digester_4_10.coverage", "3a69d9ef-5ff2-41c6-b278-7030cc15bee6");
        Stack<String> stack = namespaces.get(prefix);
        writeline("/home/ubuntu/results/coverage/Digester/Digester_4_10.coverage", "db87adfd-d9d7-4218-b34d-d5b6a16e6bed");
        if (stack == null) {
            writeline("/home/ubuntu/results/coverage/Digester/Digester_4_10.coverage", "b63716ac-2094-426f-9f14-415909eef7f4");
            return;
        }
        writeline("/home/ubuntu/results/coverage/Digester/Digester_4_10.coverage", "4731c30c-9218-4c12-9405-7e48b242ea29");
        try {
            writeline("/home/ubuntu/results/coverage/Digester/Digester_4_10.coverage", "8553fc7e-0dab-4ec5-b5a6-f43ced214f26");
            stack.pop();
            writeline("/home/ubuntu/results/coverage/Digester/Digester_4_10.coverage", "5dfae951-ea38-4967-96db-e14768e2b605");
            if (stack.empty()) {
                writeline("/home/ubuntu/results/coverage/Digester/Digester_4_10.coverage", "6c9eee19-5fa8-41ee-8e77-77b565a27acb");
                namespaces.remove(prefix);
            }
        } catch (EmptyStackException e) {
            writeline("/home/ubuntu/results/coverage/Digester/Digester_4_10.coverage", "b3732078-2203-4b6c-8c65-4b1be5623c40");
            throw createSAXException("endPrefixMapping popped too many times");
        }
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public void ignorableWhitespace(char buffer[], int start, int len) throws SAXException {
        writeline("/home/ubuntu/results/coverage/Digester/Digester_4_10.coverage", "2a6b9180-1a40-4553-a456-69ebfed7c043");
        if (saxLog.isDebugEnabled()) {
            writeline("/home/ubuntu/results/coverage/Digester/Digester_4_10.coverage", "282f50f3-722c-4cfb-a2bc-c93043d60958");
            saxLog.debug("ignorableWhitespace(" + new String(buffer, start, len) + ")");
        }
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public void processingInstruction(String target, String data) throws SAXException {
        writeline("/home/ubuntu/results/coverage/Digester/Digester_4_10.coverage", "869421f7-2c91-4cf8-9d0b-495da3c9e6e4");
        if (customContentHandler != null) {
            writeline("/home/ubuntu/results/coverage/Digester/Digester_4_10.coverage", "d50d5512-15e4-442b-9cc6-ea1a399f5667");
            customContentHandler.processingInstruction(target, data);
            writeline("/home/ubuntu/results/coverage/Digester/Digester_4_10.coverage", "4b0930e2-263a-450e-bd34-ed9e6cdb9d51");
            return;
        }
        writeline("/home/ubuntu/results/coverage/Digester/Digester_4_10.coverage", "93a83eed-8490-49e1-8629-bad63f7c8e9a");
        if (saxLog.isDebugEnabled()) {
            writeline("/home/ubuntu/results/coverage/Digester/Digester_4_10.coverage", "98686bcb-5aa5-4cae-9760-715fd515e213");
            saxLog.debug("processingInstruction('" + target + "','" + data + "')");
        }
    }

    /**
     * Gets the document locator associated with our parser.
     *
     * @return the Locator supplied by the document parser
     */
    public Locator getDocumentLocator() {
        writeline("/home/ubuntu/results/coverage/Digester/Digester_4_10.coverage", "f1129a7e-8601-4f44-ba3b-7515bc9a4155");
        return locator;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public void setDocumentLocator(Locator locator) {
        writeline("/home/ubuntu/results/coverage/Digester/Digester_4_10.coverage", "aad9109b-7a36-4c79-a06b-b3ef5a31eced");
        if (saxLog.isDebugEnabled()) {
            writeline("/home/ubuntu/results/coverage/Digester/Digester_4_10.coverage", "adcb22f6-601a-49bf-ac1f-ca1aa8cdb64f");
            saxLog.debug("setDocumentLocator(" + locator + ")");
        }
        writeline("/home/ubuntu/results/coverage/Digester/Digester_4_10.coverage", "141a22ef-7230-4a9a-865c-f79006537e3f");
        this.locator = locator;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public void skippedEntity(String name) throws SAXException {
        writeline("/home/ubuntu/results/coverage/Digester/Digester_4_10.coverage", "96bfa192-ec97-411c-ba5b-e987053ae926");
        if (saxLog.isDebugEnabled()) {
            writeline("/home/ubuntu/results/coverage/Digester/Digester_4_10.coverage", "3e11b789-8d66-4ea7-b725-845cab070b4e");
            saxLog.debug("skippedEntity(" + name + ")");
        }
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public void startDocument() throws SAXException {
        writeline("/home/ubuntu/results/coverage/Digester/Digester_4_10.coverage", "05c1e2cc-080a-4cc5-951b-151c01124213");
        if (saxLog.isDebugEnabled()) {
            writeline("/home/ubuntu/results/coverage/Digester/Digester_4_10.coverage", "a4ec4099-8e8a-4619-9f92-fcd3df2a4f82");
            saxLog.debug("startDocument()");
        }
        writeline("/home/ubuntu/results/coverage/Digester/Digester_4_10.coverage", "02f11dd8-72bd-4149-bd1a-5b22df439ed1");
        configure();
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public void startElement(String namespaceURI, String localName, String qName, Attributes list) throws SAXException {
        writeline("/home/ubuntu/results/coverage/Digester/Digester_4_10.coverage", "480b35b3-c010-4ab4-95a7-326c0f771737");
        boolean debug = log.isDebugEnabled();
        writeline("/home/ubuntu/results/coverage/Digester/Digester_4_10.coverage", "a3233014-5851-4c51-bd03-25ca4535910d");
        if (customContentHandler != null) {
            writeline("/home/ubuntu/results/coverage/Digester/Digester_4_10.coverage", "dc2e8948-428e-4594-8177-239664b05541");
            customContentHandler.startElement(namespaceURI, localName, qName, list);
            writeline("/home/ubuntu/results/coverage/Digester/Digester_4_10.coverage", "ea9f0f78-4b05-4dfd-95a8-b549c0504710");
            return;
        }
        writeline("/home/ubuntu/results/coverage/Digester/Digester_4_10.coverage", "40b0ac90-e76f-420d-895e-39020573575b");
        if (saxLog.isDebugEnabled()) {
            writeline("/home/ubuntu/results/coverage/Digester/Digester_4_10.coverage", "a32343b0-7d34-4bd5-b884-c8c3ad4601e9");
            saxLog.debug("startElement(" + namespaceURI + "," + localName + "," + qName + ")");
        }
        writeline("/home/ubuntu/results/coverage/Digester/Digester_4_10.coverage", "632dc2e3-8d00-406e-83ce-119b504467b4");
        bodyTexts.push(bodyText);
        writeline("/home/ubuntu/results/coverage/Digester/Digester_4_10.coverage", "0db20ffb-4eff-47f0-b780-a32b69f57d1d");
        if (debug) {
            writeline("/home/ubuntu/results/coverage/Digester/Digester_4_10.coverage", "085e2529-34b7-4891-8246-55418650c4ba");
            log.debug("  Pushing body text '" + bodyText.toString() + "'");
        }
        writeline("/home/ubuntu/results/coverage/Digester/Digester_4_10.coverage", "68a6b4d3-663a-4d9e-bca3-1208d1d9c9af");
        bodyText = new StringBuilder();
        writeline("/home/ubuntu/results/coverage/Digester/Digester_4_10.coverage", "3c5a736e-17b1-41bb-8a41-3cd4f2fd8639");
        String name = localName;
        writeline("/home/ubuntu/results/coverage/Digester/Digester_4_10.coverage", "4d18efe6-4582-4bb1-bdc7-42afc50a32ba");
        if ((name == null) || (name.length() < 1)) {
            writeline("/home/ubuntu/results/coverage/Digester/Digester_4_10.coverage", "f15b7126-88ec-4c36-a158-de884dbe94c7");
            name = qName;
        }
        writeline("/home/ubuntu/results/coverage/Digester/Digester_4_10.coverage", "d46e5f7a-b90e-4911-b17a-2b2c92eb0261");
        StringBuilder sb = new StringBuilder(match);
        writeline("/home/ubuntu/results/coverage/Digester/Digester_4_10.coverage", "7de01f86-62c0-4c09-8594-97f19e3807b9");
        if (match.length() > 0) {
            writeline("/home/ubuntu/results/coverage/Digester/Digester_4_10.coverage", "03e6f469-940c-40dd-baba-a540f17a24ca");
            sb.append('/');
        }
        writeline("/home/ubuntu/results/coverage/Digester/Digester_4_10.coverage", "bd5bc4a0-a99e-4f7d-908f-84ed75a982d9");
        sb.append(name);
        writeline("/home/ubuntu/results/coverage/Digester/Digester_4_10.coverage", "c6107cad-068a-4370-8e47-6bca16441ec7");
        match = sb.toString();
        writeline("/home/ubuntu/results/coverage/Digester/Digester_4_10.coverage", "d6ec4cd3-4080-4ce8-b757-ac3df105356e");
        if (debug) {
            writeline("/home/ubuntu/results/coverage/Digester/Digester_4_10.coverage", "b9a9629b-089d-4f71-86a1-bc74763bc6fa");
            log.debug("  New match='" + match + "'");
        }
        writeline("/home/ubuntu/results/coverage/Digester/Digester_4_10.coverage", "7c37e3dc-d634-4edf-a33d-bb5ffeaea4bb");
        List<Rule> rules = getRules().match(namespaceURI, match, localName, list);
        writeline("/home/ubuntu/results/coverage/Digester/Digester_4_10.coverage", "50419c4d-02d7-48ea-9840-2d37d2911d70");
        matches.push(rules);
        writeline("/home/ubuntu/results/coverage/Digester/Digester_4_10.coverage", "19477597-4b70-49c4-b0cc-2c242383527c");
        if ((rules != null) && (rules.size() > 0)) {
            writeline("/home/ubuntu/results/coverage/Digester/Digester_4_10.coverage", "fd27c3b7-cec6-4554-af43-7fd85d36bc6e");
            Substitutor substitutor = getSubstitutor();
            writeline("/home/ubuntu/results/coverage/Digester/Digester_4_10.coverage", "30cd76b3-c8b2-4236-a5f3-98b8ef8fa0ab");
            if (substitutor != null) {
                writeline("/home/ubuntu/results/coverage/Digester/Digester_4_10.coverage", "d61c5aae-c330-4159-ab36-51b66038fc00");
                list = substitutor.substitute(list);
            }
            writeline("/home/ubuntu/results/coverage/Digester/Digester_4_10.coverage", "26887e66-9146-4a1a-87f4-5571f59986aa");
            for (int i = 0; i < rules.size(); i++) {
                writeline("/home/ubuntu/results/coverage/Digester/Digester_4_10.coverage", "a87f584e-54ae-46dd-8baa-9061377c04f5");
                try {
                    writeline("/home/ubuntu/results/coverage/Digester/Digester_4_10.coverage", "51e22c31-dfe3-4dca-9659-8611e5c4d43b");
                    Rule rule = rules.get(i);
                    writeline("/home/ubuntu/results/coverage/Digester/Digester_4_10.coverage", "79d1ac7e-2b55-4aca-a9bf-99334d1ced49");
                    if (debug) {
                        writeline("/home/ubuntu/results/coverage/Digester/Digester_4_10.coverage", "a9f2ff73-4d12-4605-8c8a-2859ace2b8ab");
                        log.debug("  Fire begin() for " + rule);
                    }
                    writeline("/home/ubuntu/results/coverage/Digester/Digester_4_10.coverage", "ecb1da36-5752-4741-b881-caa294167fdb");
                    rule.begin(namespaceURI, name, list);
                } catch (Exception e) {
                    writeline("/home/ubuntu/results/coverage/Digester/Digester_4_10.coverage", "d275681e-2062-4ccd-8e71-e375226e0689");
                    log.error("Begin event threw exception", e);
                    writeline("/home/ubuntu/results/coverage/Digester/Digester_4_10.coverage", "30036826-aeec-4c36-974e-734b953c0f82");
                    throw createSAXException(e);
                } catch (Error e) {
                    writeline("/home/ubuntu/results/coverage/Digester/Digester_4_10.coverage", "306b3bf3-03bf-4cf3-b9ae-9e0f9b06d36c");
                    log.error("Begin event threw error", e);
                    writeline("/home/ubuntu/results/coverage/Digester/Digester_4_10.coverage", "dd8d0f4e-04a6-4327-b6f7-5dac704537f4");
                    throw e;
                }
            }
        } else {
            writeline("/home/ubuntu/results/coverage/Digester/Digester_4_10.coverage", "c287a618-3932-4667-b20b-ff75fe9e1cfc");
            if (debug) {
                writeline("/home/ubuntu/results/coverage/Digester/Digester_4_10.coverage", "75ede57e-09a7-4c87-82c2-f8682bed08ca");
                log.debug("  No rules found matching '" + match + "'.");
            }
        }
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public void startPrefixMapping(String prefix, String namespaceURI) throws SAXException {
        writeline("/home/ubuntu/results/coverage/Digester/Digester_4_10.coverage", "a1703b20-5b64-4b0d-9863-943ddff0ce27");
        if (saxLog.isDebugEnabled()) {
            writeline("/home/ubuntu/results/coverage/Digester/Digester_4_10.coverage", "a78bff2b-67e4-4c78-82ee-6c0df4a8e124");
            saxLog.debug("startPrefixMapping(" + prefix + "," + namespaceURI + ")");
        }
        writeline("/home/ubuntu/results/coverage/Digester/Digester_4_10.coverage", "cfcb9873-fc84-494d-8229-0db5c981b071");
        Stack<String> stack = namespaces.get(prefix);
        writeline("/home/ubuntu/results/coverage/Digester/Digester_4_10.coverage", "d7e281d4-8fca-4560-96da-ba12a06bfba2");
        if (stack == null) {
            writeline("/home/ubuntu/results/coverage/Digester/Digester_4_10.coverage", "0dbf70f6-9d8c-4c72-aff0-5897d7dea1fc");
            stack = new Stack<String>();
            writeline("/home/ubuntu/results/coverage/Digester/Digester_4_10.coverage", "88b2f46a-dad6-448c-87af-797a7be9db1f");
            namespaces.put(prefix, stack);
        }
        writeline("/home/ubuntu/results/coverage/Digester/Digester_4_10.coverage", "720fbd90-6e22-46bb-8eba-04100c243d65");
        stack.push(namespaceURI);
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public void notationDecl(String name, String publicId, String systemId) {
        writeline("/home/ubuntu/results/coverage/Digester/Digester_4_10.coverage", "9d1c31fd-c81a-4f0c-8bde-abb6d1bc869b");
        if (saxLog.isDebugEnabled()) {
            writeline("/home/ubuntu/results/coverage/Digester/Digester_4_10.coverage", "7b6e37a7-21a7-4a49-9f91-b0bbffadf5b3");
            saxLog.debug("notationDecl(" + name + "," + publicId + "," + systemId + ")");
        }
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public void unparsedEntityDecl(String name, String publicId, String systemId, String notation) {
        writeline("/home/ubuntu/results/coverage/Digester/Digester_4_10.coverage", "6344d760-6f54-4703-b71c-fba087f0f6db");
        if (saxLog.isDebugEnabled()) {
            writeline("/home/ubuntu/results/coverage/Digester/Digester_4_10.coverage", "953e2f21-1b0c-4525-8227-6a8ba6722f46");
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
        writeline("/home/ubuntu/results/coverage/Digester/Digester_4_10.coverage", "59c00d74-8d9f-4b8a-8871-7bd3ab5d146c");
        this.entityResolver = entityResolver;
    }

    /**
     * Return the Entity Resolver used by the SAX parser.
     *
     * @return the Entity Resolver used by the SAX parser.
     */
    public EntityResolver getEntityResolver() {
        writeline("/home/ubuntu/results/coverage/Digester/Digester_4_10.coverage", "db77ad50-c827-4111-b07d-ee0436f5710b");
        return entityResolver;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public InputSource resolveEntity(String publicId, String systemId) throws SAXException {
        writeline("/home/ubuntu/results/coverage/Digester/Digester_4_10.coverage", "a59a60b7-a2da-4ab3-87ca-e93c826ee377");
        if (saxLog.isDebugEnabled()) {
            writeline("/home/ubuntu/results/coverage/Digester/Digester_4_10.coverage", "c75e9106-645d-483a-8737-0becb546f9e4");
            saxLog.debug("resolveEntity('" + publicId + "', '" + systemId + "')");
        }
        writeline("/home/ubuntu/results/coverage/Digester/Digester_4_10.coverage", "16fad2d2-77d7-4852-b594-6f4dadeeac7d");
        if (publicId != null) {
            writeline("/home/ubuntu/results/coverage/Digester/Digester_4_10.coverage", "55d51e90-4cd2-43c4-9f18-cdd044fc01e2");
            this.publicId = publicId;
        }
        writeline("/home/ubuntu/results/coverage/Digester/Digester_4_10.coverage", "c470921d-ec07-4f5f-ae77-622a934c5b44");
        URL entityURL = null;
        writeline("/home/ubuntu/results/coverage/Digester/Digester_4_10.coverage", "18785648-26cb-4eaa-b5ff-22128d3c8485");
        if (publicId != null) {
            writeline("/home/ubuntu/results/coverage/Digester/Digester_4_10.coverage", "65b20fe0-3f72-4c4c-ba97-a930cf6fbefc");
            entityURL = entityValidator.get(publicId);
        }
        writeline("/home/ubuntu/results/coverage/Digester/Digester_4_10.coverage", "f835188e-bf7e-4efa-aa56-7e75d2eeea50");
        if (entityURL == null && systemId != null) {
            writeline("/home/ubuntu/results/coverage/Digester/Digester_4_10.coverage", "bbcd4669-7ac0-43f4-9823-313e6a8180ca");
            entityURL = entityValidator.get(systemId);
        }
        writeline("/home/ubuntu/results/coverage/Digester/Digester_4_10.coverage", "981814e1-92de-4f46-9332-1d83db571975");
        if (entityURL == null) {
            writeline("/home/ubuntu/results/coverage/Digester/Digester_4_10.coverage", "9ad942af-60ef-4958-a72a-8172b11c20ab");
            if (systemId == null) {
                writeline("/home/ubuntu/results/coverage/Digester/Digester_4_10.coverage", "233ef13a-e696-4a5b-978c-0dc83fb51c08");
                if (log.isDebugEnabled()) {
                    writeline("/home/ubuntu/results/coverage/Digester/Digester_4_10.coverage", "4706b940-1b9c-4e63-91b7-532b3ea4b231");
                    log.debug(" Cannot resolve null entity, returning null InputSource");
                }
                writeline("/home/ubuntu/results/coverage/Digester/Digester_4_10.coverage", "981bbf44-07cc-4c8e-bda9-2f9d9e5d2fea");
                return (null);
            }
            writeline("/home/ubuntu/results/coverage/Digester/Digester_4_10.coverage", "b8cb359c-05da-4ab7-bdef-ed3c97920df1");
            if (log.isDebugEnabled()) {
                writeline("/home/ubuntu/results/coverage/Digester/Digester_4_10.coverage", "b5ef905a-d205-418f-8ee9-42c49a6761ea");
                log.debug(" Trying to resolve using system ID '" + systemId + "'");
            }
            writeline("/home/ubuntu/results/coverage/Digester/Digester_4_10.coverage", "63381e40-0aeb-44c2-8bab-1dbfd1f1fb60");
            try {
                writeline("/home/ubuntu/results/coverage/Digester/Digester_4_10.coverage", "b5c96342-e66c-42e1-a5f9-f61ea88064ae");
                entityURL = new URL(systemId);
            } catch (MalformedURLException e) {
                writeline("/home/ubuntu/results/coverage/Digester/Digester_4_10.coverage", "d602375e-a354-4497-ad1e-8491d81a272e");
                throw new IllegalArgumentException("Malformed URL '" + systemId + "' : " + e.getMessage());
            }
        }
        writeline("/home/ubuntu/results/coverage/Digester/Digester_4_10.coverage", "313d45b4-3256-4661-bc7a-6e2e8eb4f973");
        if (log.isDebugEnabled()) {
            writeline("/home/ubuntu/results/coverage/Digester/Digester_4_10.coverage", "a9d206b2-0bcc-41b9-9098-3903cb63e34f");
            log.debug(" Resolving to alternate DTD '" + entityURL + "'");
        }
        writeline("/home/ubuntu/results/coverage/Digester/Digester_4_10.coverage", "2a3a7c00-d567-4485-8d8e-37a42882c460");
        try {
            writeline("/home/ubuntu/results/coverage/Digester/Digester_4_10.coverage", "3b6f2d11-4221-49c4-acb6-3e8596bdf286");
            return createInputSourceFromURL(entityURL);
        } catch (Exception e) {
            writeline("/home/ubuntu/results/coverage/Digester/Digester_4_10.coverage", "04f4949e-2b78-40f5-9bc0-2dd1d41b5453");
            throw createSAXException(e);
        }
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public void error(SAXParseException exception) throws SAXException {
        writeline("/home/ubuntu/results/coverage/Digester/Digester_4_10.coverage", "4c9aa111-0539-4fae-ad61-45a4d3a4aa73");
        log.error("Parse Error at line " + exception.getLineNumber() + " column " + exception.getColumnNumber() + ": " + exception.getMessage(), exception);
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public void fatalError(SAXParseException exception) throws SAXException {
        writeline("/home/ubuntu/results/coverage/Digester/Digester_4_10.coverage", "5b64ae0a-5376-47db-aa74-3acc34284015");
        log.error("Parse Fatal Error at line " + exception.getLineNumber() + " column " + exception.getColumnNumber() + ": " + exception.getMessage(), exception);
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public void warning(SAXParseException exception) throws SAXException {
        writeline("/home/ubuntu/results/coverage/Digester/Digester_4_10.coverage", "998de509-36a8-4055-8e17-7cac8bfeb8f3");
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
        writeline("/home/ubuntu/results/coverage/Digester/Digester_4_10.coverage", "7bb64d24-6ca8-43d3-9155-af5e97cb9f63");
        if (file == null) {
            writeline("/home/ubuntu/results/coverage/Digester/Digester_4_10.coverage", "edd32b3e-a0ab-4632-958d-b350a2ac821e");
            throw new IllegalArgumentException("File to parse is null");
        }
        writeline("/home/ubuntu/results/coverage/Digester/Digester_4_10.coverage", "fa3776d2-7862-4d30-b485-fbb6a70bb67f");
        InputSource input = new InputSource(new FileInputStream(file));
        writeline("/home/ubuntu/results/coverage/Digester/Digester_4_10.coverage", "46ff5327-778d-4429-b50c-583b520511ce");
        input.setSystemId(file.toURI().toURL().toString());
        writeline("/home/ubuntu/results/coverage/Digester/Digester_4_10.coverage", "66f256d2-b527-44df-ad7e-24f31f419129");
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
        writeline("/home/ubuntu/results/coverage/Digester/Digester_4_10.coverage", "06368794-9982-4465-bda8-a959848b9685");
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
        writeline("/home/ubuntu/results/coverage/Digester/Digester_4_10.coverage", "5d1279cf-3197-45fc-b239-61dcf5903bca");
        if (input == null) {
            writeline("/home/ubuntu/results/coverage/Digester/Digester_4_10.coverage", "d653addd-e7cb-4e70-8c6b-e703c6212a37");
            throw new IllegalArgumentException("InputSource to parse is null");
        }
        writeline("/home/ubuntu/results/coverage/Digester/Digester_4_10.coverage", "799ecac4-cdf9-4e83-ac80-bc1cdf56b78e");
        configure();
        writeline("/home/ubuntu/results/coverage/Digester/Digester_4_10.coverage", "9654d50c-9cee-4ae0-ba70-374143bb031e");
        String systemId = input.getSystemId();
        writeline("/home/ubuntu/results/coverage/Digester/Digester_4_10.coverage", "00eb0548-2b8e-4b13-982d-9d586256f665");
        if (systemId == null) {
            writeline("/home/ubuntu/results/coverage/Digester/Digester_4_10.coverage", "f759c165-0f92-4d35-adf1-760448e9799a");
            systemId = "(already loaded from stream)";
        }
        writeline("/home/ubuntu/results/coverage/Digester/Digester_4_10.coverage", "fe94dad3-5f01-48a8-a28d-956e9ca3dbd6");
        try {
            writeline("/home/ubuntu/results/coverage/Digester/Digester_4_10.coverage", "e5c786d7-b563-41ae-a56f-42ff33cce60e");
            getXMLReader().parse(input);
        } catch (IOException e) {
            writeline("/home/ubuntu/results/coverage/Digester/Digester_4_10.coverage", "d53bd2c9-9a95-4c33-adec-075f9f2fda08");
            log.error(format("An error occurred while reading stream from '%s', see nested exceptions", systemId), e);
            writeline("/home/ubuntu/results/coverage/Digester/Digester_4_10.coverage", "63121914-74f5-446c-ab86-1fcbb78b15ff");
            throw e;
        }
        writeline("/home/ubuntu/results/coverage/Digester/Digester_4_10.coverage", "320cf910-b33e-4f01-a68b-0f990f0c57ca");
        cleanup();
        writeline("/home/ubuntu/results/coverage/Digester/Digester_4_10.coverage", "9b970071-7b4c-49d3-81ba-941fe12c8722");
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
        writeline("/home/ubuntu/results/coverage/Digester/Digester_4_10.coverage", "a00b5da3-2a78-4437-a57a-f3fc0f75809b");
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
        writeline("/home/ubuntu/results/coverage/Digester/Digester_4_10.coverage", "94f11df6-1a46-4514-8882-df3935c3d5e7");
        if (input == null) {
            writeline("/home/ubuntu/results/coverage/Digester/Digester_4_10.coverage", "cc1fbefc-7e2b-4b34-a0f4-5cdafb05dc98");
            throw new IllegalArgumentException("InputStream to parse is null");
        }
        writeline("/home/ubuntu/results/coverage/Digester/Digester_4_10.coverage", "ac035be0-80d5-40bc-890c-57e962d3a1c4");
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
        writeline("/home/ubuntu/results/coverage/Digester/Digester_4_10.coverage", "b74758da-faac-43aa-bdbc-77cf5f2a7ad3");
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
        writeline("/home/ubuntu/results/coverage/Digester/Digester_4_10.coverage", "994bdd87-9e62-42d0-90bf-c250ba7db4a2");
        if (reader == null) {
            writeline("/home/ubuntu/results/coverage/Digester/Digester_4_10.coverage", "30d0af95-fea0-4fbb-9913-8341599e4d1a");
            throw new IllegalArgumentException("Reader to parse is null");
        }
        writeline("/home/ubuntu/results/coverage/Digester/Digester_4_10.coverage", "e44702e4-5756-494d-b3d3-15492ecf1288");
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
        writeline("/home/ubuntu/results/coverage/Digester/Digester_4_10.coverage", "2ce8b355-6ad2-4b53-922c-481fb3020a7c");
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
        writeline("/home/ubuntu/results/coverage/Digester/Digester_4_10.coverage", "a092c3ad-c644-4a27-a04a-71491e4f10c4");
        if (uri == null) {
            writeline("/home/ubuntu/results/coverage/Digester/Digester_4_10.coverage", "e4e74bd0-2bf3-4b96-a168-30a60f68433e");
            throw new IllegalArgumentException("String URI to parse is null");
        }
        writeline("/home/ubuntu/results/coverage/Digester/Digester_4_10.coverage", "023e8709-e5d0-47cb-a2c9-d4c5f71d74ff");
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
        writeline("/home/ubuntu/results/coverage/Digester/Digester_4_10.coverage", "2baec8a7-161f-4193-8720-1725ed3654a7");
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
        writeline("/home/ubuntu/results/coverage/Digester/Digester_4_10.coverage", "f7ec9268-1026-4839-9ca3-9ad3b5582a53");
        if (url == null) {
            writeline("/home/ubuntu/results/coverage/Digester/Digester_4_10.coverage", "f30afbd1-32fa-4745-bbfd-ff91d6fe25e5");
            throw new IllegalArgumentException("URL to parse is null");
        }
        writeline("/home/ubuntu/results/coverage/Digester/Digester_4_10.coverage", "cf672fd3-e903-444a-a16c-c262f5e29c52");
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
        writeline("/home/ubuntu/results/coverage/Digester/Digester_4_10.coverage", "353ce7a4-00ee-44e0-9dc6-0894b7bd796f");
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
        writeline("/home/ubuntu/results/coverage/Digester/Digester_4_10.coverage", "713d815f-08a4-4d98-9f6c-76227b79f72a");
        if (executorService == null) {
            writeline("/home/ubuntu/results/coverage/Digester/Digester_4_10.coverage", "595c8d3e-637b-44c3-96b7-bec0a8802475");
            throw new IllegalStateException("ExecutorService not set");
        }
        writeline("/home/ubuntu/results/coverage/Digester/Digester_4_10.coverage", "e3d67cc1-b035-4df6-b3d7-d517d5014a4b");
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
        writeline("/home/ubuntu/results/coverage/Digester/Digester_4_10.coverage", "fc1db90e-caa8-4961-83e8-e2c0d167f382");
        if (log.isDebugEnabled()) {
            writeline("/home/ubuntu/results/coverage/Digester/Digester_4_10.coverage", "1b351155-6da9-4115-bf18-01ce29bf50b8");
            log.debug("register('" + publicId + "', '" + entityURL + "'");
        }
        writeline("/home/ubuntu/results/coverage/Digester/Digester_4_10.coverage", "982130f0-51a3-448f-8970-7fd4af6efab3");
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
        writeline("/home/ubuntu/results/coverage/Digester/Digester_4_10.coverage", "04cf43ee-f576-4d62-9985-b7eddb981c29");
        if (log.isDebugEnabled()) {
            writeline("/home/ubuntu/results/coverage/Digester/Digester_4_10.coverage", "43cac0a8-eaff-4b81-adfc-a0fbd3aa58ad");
            log.debug("register('" + publicId + "', '" + entityURL + "'");
        }
        writeline("/home/ubuntu/results/coverage/Digester/Digester_4_10.coverage", "74a6797d-e4c6-42f3-b561-00bd86495b72");
        try {
            writeline("/home/ubuntu/results/coverage/Digester/Digester_4_10.coverage", "ab43867b-9f6b-420e-98ac-bdec3373a127");
            entityValidator.put(publicId, new URL(entityURL));
        } catch (MalformedURLException e) {
            writeline("/home/ubuntu/results/coverage/Digester/Digester_4_10.coverage", "7274bf6a-74d8-439b-b255-4a102be5fef7");
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
        writeline("/home/ubuntu/results/coverage/Digester/Digester_4_10.coverage", "c4c62be4-9424-4c5a-b0c8-888cd7311cc9");
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
        writeline("/home/ubuntu/results/coverage/Digester/Digester_4_10.coverage", "d78d53b1-89d6-4e60-867d-9d0b95770571");
        URLConnection connection = url.openConnection();
        writeline("/home/ubuntu/results/coverage/Digester/Digester_4_10.coverage", "560d478f-1fa3-4552-b15d-bd6b598964fd");
        connection.setUseCaches(false);
        writeline("/home/ubuntu/results/coverage/Digester/Digester_4_10.coverage", "a9dcfb23-2f5c-4586-b5e5-6af70a2ce0d8");
        InputStream stream = connection.getInputStream();
        writeline("/home/ubuntu/results/coverage/Digester/Digester_4_10.coverage", "69dec714-2d3a-4707-bf76-bef6bf5cd4bc");
        InputSource source = new InputSource(stream);
        writeline("/home/ubuntu/results/coverage/Digester/Digester_4_10.coverage", "03f47e4b-a966-4d3e-a495-d5c0018ab011");
        source.setSystemId(url.toExternalForm());
        writeline("/home/ubuntu/results/coverage/Digester/Digester_4_10.coverage", "b21ea1dd-a67c-448d-b443-052f55a659ef");
        inputSources.add(source);
        writeline("/home/ubuntu/results/coverage/Digester/Digester_4_10.coverage", "d0c32c3f-f32f-4a71-82b5-7e8e07252afa");
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
        writeline("/home/ubuntu/results/coverage/Digester/Digester_4_10.coverage", "a9ebda59-9427-4733-a559-a31efc4d5dc6");
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
        writeline("/home/ubuntu/results/coverage/Digester/Digester_4_10.coverage", "04c9ed71-835b-4d7f-ad10-bf6e40cd3c92");
        rule.setDigester(this);
        writeline("/home/ubuntu/results/coverage/Digester/Digester_4_10.coverage", "27d1a6eb-a892-4e4f-9f5d-c9d3a876e095");
        getRules().add(pattern, rule);
    }

    /**
     * Register a set of Rule instances defined in a RuleSet.
     *
     * @param ruleSet The RuleSet instance to configure from
     */
    public void addRuleSet(RuleSet ruleSet) {
        writeline("/home/ubuntu/results/coverage/Digester/Digester_4_10.coverage", "8006b76f-9f54-44be-b354-658a74152352");
        String oldNamespaceURI = getRuleNamespaceURI();
        writeline("/home/ubuntu/results/coverage/Digester/Digester_4_10.coverage", "78ea3b57-15d9-4bde-bb37-caf116c65c01");
        String newNamespaceURI = ruleSet.getNamespaceURI();
        writeline("/home/ubuntu/results/coverage/Digester/Digester_4_10.coverage", "2e1c765c-4811-4275-b2f7-8b02367c0db0");
        if (log.isDebugEnabled()) {
            writeline("/home/ubuntu/results/coverage/Digester/Digester_4_10.coverage", "7a80e2d5-0511-4b71-9bcf-6736413ce64b");
            if (newNamespaceURI == null) {
                writeline("/home/ubuntu/results/coverage/Digester/Digester_4_10.coverage", "b92f09f5-8374-4093-a35b-052cb98dab63");
                log.debug("addRuleSet() with no namespace URI");
            } else {
                writeline("/home/ubuntu/results/coverage/Digester/Digester_4_10.coverage", "9478f0e1-d6eb-41d3-b7e1-06bdad366e54");
                log.debug("addRuleSet() with namespace URI " + newNamespaceURI);
            }
        }
        writeline("/home/ubuntu/results/coverage/Digester/Digester_4_10.coverage", "ad7c7434-32da-4b5a-bd4a-95abfda75c2e");
        setRuleNamespaceURI(newNamespaceURI);
        writeline("/home/ubuntu/results/coverage/Digester/Digester_4_10.coverage", "0e16922d-4981-4c44-9cad-df68bbdd2027");
        ruleSet.addRuleInstances(this);
        writeline("/home/ubuntu/results/coverage/Digester/Digester_4_10.coverage", "30112fd1-17c2-4829-8d7d-15390d48e6b0");
        setRuleNamespaceURI(oldNamespaceURI);
    }

    /**
     * Add a "bean property setter" rule for the specified parameters.
     *
     * @param pattern Element matching pattern
     * @see BeanPropertySetterRule
     */
    public void addBeanPropertySetter(String pattern) {
        writeline("/home/ubuntu/results/coverage/Digester/Digester_4_10.coverage", "9d0ca10e-89ea-4aa0-a07e-7157b83a333a");
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
        writeline("/home/ubuntu/results/coverage/Digester/Digester_4_10.coverage", "323bc766-d8e5-48b5-b32e-52ccdc9b282c");
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
        writeline("/home/ubuntu/results/coverage/Digester/Digester_4_10.coverage", "3702d1c9-56f5-45a5-af47-7228736833a5");
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
        writeline("/home/ubuntu/results/coverage/Digester/Digester_4_10.coverage", "d6a95e4c-3ae5-4bf4-a2a2-325cbf5aab39");
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
        writeline("/home/ubuntu/results/coverage/Digester/Digester_4_10.coverage", "9887de57-8ab0-460a-aad5-8edb8411f876");
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
        writeline("/home/ubuntu/results/coverage/Digester/Digester_4_10.coverage", "94a6ba7c-90da-496e-a100-8356727fb2f1");
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
        writeline("/home/ubuntu/results/coverage/Digester/Digester_4_10.coverage", "3071c69a-1123-4a19-b9e7-fbeeab646378");
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
        writeline("/home/ubuntu/results/coverage/Digester/Digester_4_10.coverage", "f656fa7e-c659-48ce-80c3-136f32f561b5");
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
        writeline("/home/ubuntu/results/coverage/Digester/Digester_4_10.coverage", "3195d637-5342-4011-99e4-4c92a0abd23e");
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
        writeline("/home/ubuntu/results/coverage/Digester/Digester_4_10.coverage", "10982ead-4df5-4995-a637-62d4e3e403ee");
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
        writeline("/home/ubuntu/results/coverage/Digester/Digester_4_10.coverage", "4222d1c4-ea35-4189-917d-7c668ba78317");
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
        writeline("/home/ubuntu/results/coverage/Digester/Digester_4_10.coverage", "57b9a424-953e-4478-9983-96906c917348");
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
        writeline("/home/ubuntu/results/coverage/Digester/Digester_4_10.coverage", "a4382c91-798a-4b82-bca9-5958c71aece0");
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
        writeline("/home/ubuntu/results/coverage/Digester/Digester_4_10.coverage", "3490bd38-90e5-4090-9721-65050b4ae0f7");
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
        writeline("/home/ubuntu/results/coverage/Digester/Digester_4_10.coverage", "2c97bb1b-eeaa-4843-a0e2-e9eba0ef62c8");
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
        writeline("/home/ubuntu/results/coverage/Digester/Digester_4_10.coverage", "4521238c-5c00-408d-90fb-668a2242ad91");
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
        writeline("/home/ubuntu/results/coverage/Digester/Digester_4_10.coverage", "ef7bacc4-401c-4209-b6dc-a17c875a3990");
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
        writeline("/home/ubuntu/results/coverage/Digester/Digester_4_10.coverage", "078c9c79-4f9c-488c-b53a-095a3c5cde7e");
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
        writeline("/home/ubuntu/results/coverage/Digester/Digester_4_10.coverage", "0650a1ce-c1f0-4182-a904-b2ed5610ff47");
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
        writeline("/home/ubuntu/results/coverage/Digester/Digester_4_10.coverage", "bde35e47-bb29-4e44-9ac3-d4a38f9816c8");
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
        writeline("/home/ubuntu/results/coverage/Digester/Digester_4_10.coverage", "287426c9-e68d-41f2-a1b8-0ab05959f61d");
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
        writeline("/home/ubuntu/results/coverage/Digester/Digester_4_10.coverage", "5454590c-b5a5-4429-a8f2-2621ea9149e8");
        creationFactory.setDigester(this);
        writeline("/home/ubuntu/results/coverage/Digester/Digester_4_10.coverage", "bb6da06a-78da-45fc-ac50-1dfe37665857");
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
        writeline("/home/ubuntu/results/coverage/Digester/Digester_4_10.coverage", "9ec06d64-e23d-44cf-aa24-cb030057c1bc");
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
        writeline("/home/ubuntu/results/coverage/Digester/Digester_4_10.coverage", "c560f63b-cf36-4d2e-a011-5800b33aaea4");
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
        writeline("/home/ubuntu/results/coverage/Digester/Digester_4_10.coverage", "0a2fb652-e95e-40e4-9b95-40f785dbeee3");
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
        writeline("/home/ubuntu/results/coverage/Digester/Digester_4_10.coverage", "a8e2cbc6-4a8f-4f52-8079-e879323d93c0");
        addRule(pattern, new ObjectCreateRule(attributeName, clazz));
    }

    /**
     * Adds an {@link SetNestedPropertiesRule}.
     *
     * @param pattern register the rule with this pattern
     * @since 1.6
     */
    public void addSetNestedProperties(String pattern) {
        writeline("/home/ubuntu/results/coverage/Digester/Digester_4_10.coverage", "bf43e800-f2b2-46b1-a12d-d30f405c9cbc");
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
        writeline("/home/ubuntu/results/coverage/Digester/Digester_4_10.coverage", "f4d85f3f-68e9-455e-a67a-f951ef45549e");
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
        writeline("/home/ubuntu/results/coverage/Digester/Digester_4_10.coverage", "bcb9bc44-c0cb-4d66-9cf4-a318ab3d5802");
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
        writeline("/home/ubuntu/results/coverage/Digester/Digester_4_10.coverage", "4d003722-3bfc-4876-a51b-1da9bf928da8");
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
        writeline("/home/ubuntu/results/coverage/Digester/Digester_4_10.coverage", "ae2e2c03-9bf1-404c-8366-8ff5678b3b38");
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
        writeline("/home/ubuntu/results/coverage/Digester/Digester_4_10.coverage", "2c0792fd-c2ae-4fa1-b83d-7e67f5c796b3");
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
        writeline("/home/ubuntu/results/coverage/Digester/Digester_4_10.coverage", "c439a6a1-6bf8-46ec-9612-a08b57e20c03");
        addRule(pattern, new SetRootRule(methodName, paramType));
    }

    /**
     * Add a "set properties" rule for the specified parameters.
     *
     * @param pattern Element matching pattern
     * @see SetPropertiesRule
     */
    public void addSetProperties(String pattern) {
        writeline("/home/ubuntu/results/coverage/Digester/Digester_4_10.coverage", "78e70669-2014-4b9d-bb72-a9856e937aaf");
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
        writeline("/home/ubuntu/results/coverage/Digester/Digester_4_10.coverage", "ba3b14f5-4d97-4021-b828-d03d40ee8d5e");
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
        writeline("/home/ubuntu/results/coverage/Digester/Digester_4_10.coverage", "ea8e4ceb-3324-40cf-a08e-108476e1d991");
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
        writeline("/home/ubuntu/results/coverage/Digester/Digester_4_10.coverage", "bd63023e-1e61-4660-a12a-fbb787da996d");
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
        writeline("/home/ubuntu/results/coverage/Digester/Digester_4_10.coverage", "c0f8a277-e5db-4532-97c7-3799a5e87c8b");
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
        writeline("/home/ubuntu/results/coverage/Digester/Digester_4_10.coverage", "e0262431-b3b5-4ab3-a7b3-c1a0c16e3584");
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
        writeline("/home/ubuntu/results/coverage/Digester/Digester_4_10.coverage", "90a408d1-91eb-4580-af88-303cc4ad41c0");
        match = "";
        writeline("/home/ubuntu/results/coverage/Digester/Digester_4_10.coverage", "8471630b-c642-45fe-899c-d38023928d2a");
        bodyTexts.clear();
        writeline("/home/ubuntu/results/coverage/Digester/Digester_4_10.coverage", "8410f11a-7def-47ca-8b2f-8613dcce98b0");
        params.clear();
        writeline("/home/ubuntu/results/coverage/Digester/Digester_4_10.coverage", "7c1c3aa9-87d2-4e47-8b16-0d25ce97408d");
        publicId = null;
        writeline("/home/ubuntu/results/coverage/Digester/Digester_4_10.coverage", "3ef785e7-b7d9-4716-92bc-2b6ecc53ddce");
        stack.clear();
        writeline("/home/ubuntu/results/coverage/Digester/Digester_4_10.coverage", "18897fa5-3e3a-484e-84a3-9bc9349ba801");
        stacksByName.clear();
        writeline("/home/ubuntu/results/coverage/Digester/Digester_4_10.coverage", "2904f1c3-0dee-435c-a0b7-4bd56be2e019");
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
        writeline("/home/ubuntu/results/coverage/Digester/Digester_4_10.coverage", "543ed22f-8abd-42f1-b171-a1c17a1c0125");
        try {
            writeline("/home/ubuntu/results/coverage/Digester/Digester_4_10.coverage", "a3460557-e71c-48c6-83ce-48f71c560465");
            return this.<T>npeSafeCast(stack.peek());
        } catch (EmptyStackException e) {
            writeline("/home/ubuntu/results/coverage/Digester/Digester_4_10.coverage", "81a42b97-61b8-4c34-b3a4-09a92f0bd2f3");
            log.warn("Empty stack (returning null)");
            writeline("/home/ubuntu/results/coverage/Digester/Digester_4_10.coverage", "d9254199-aa21-47fd-9e52-94845e6238ca");
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
        writeline("/home/ubuntu/results/coverage/Digester/Digester_4_10.coverage", "7edb6628-483a-41d8-8570-0d87e0fd4e30");
        int index = (stack.size() - 1) - n;
        writeline("/home/ubuntu/results/coverage/Digester/Digester_4_10.coverage", "4b021b05-6c9b-4639-94b2-b9bc3ff2718d");
        if (index < 0) {
            writeline("/home/ubuntu/results/coverage/Digester/Digester_4_10.coverage", "a7d31087-cb51-4708-b547-fd70b3f4c0bd");
            log.warn("Empty stack (returning null)");
            writeline("/home/ubuntu/results/coverage/Digester/Digester_4_10.coverage", "c42e5f39-a8bb-4449-a0db-3aa5445a8bbb");
            return (null);
        }
        writeline("/home/ubuntu/results/coverage/Digester/Digester_4_10.coverage", "a6e7274f-1eb5-48be-ae40-7837764d8d65");
        try {
            writeline("/home/ubuntu/results/coverage/Digester/Digester_4_10.coverage", "0b61e63c-5a4c-4918-932f-1a7dbadd6f42");
            return this.<T>npeSafeCast(stack.get(index));
        } catch (EmptyStackException e) {
            writeline("/home/ubuntu/results/coverage/Digester/Digester_4_10.coverage", "579a8573-4619-4565-b61c-4b90c74b1f87");
            log.warn("Empty stack (returning null)");
            writeline("/home/ubuntu/results/coverage/Digester/Digester_4_10.coverage", "dea61717-9762-4a21-b7c4-ff262abfbe56");
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
        writeline("/home/ubuntu/results/coverage/Digester/Digester_4_10.coverage", "d0834c3d-8c1f-4d60-8686-ca53bcf5e8d3");
        try {
            writeline("/home/ubuntu/results/coverage/Digester/Digester_4_10.coverage", "0b6666ac-c65f-4423-a62d-9f5a3cfe4bbe");
            T popped = this.<T>npeSafeCast(stack.pop());
            writeline("/home/ubuntu/results/coverage/Digester/Digester_4_10.coverage", "be1fd1fb-6f4c-4d20-9e73-dd1c78a6fe9e");
            if (stackAction != null) {
                writeline("/home/ubuntu/results/coverage/Digester/Digester_4_10.coverage", "85101ee4-d547-4977-b3a4-03e520412d9f");
                popped = stackAction.onPop(this, null, popped);
            }
            writeline("/home/ubuntu/results/coverage/Digester/Digester_4_10.coverage", "f8b03146-d330-4d8d-9596-6dee2a70601c");
            return popped;
        } catch (EmptyStackException e) {
            writeline("/home/ubuntu/results/coverage/Digester/Digester_4_10.coverage", "b84dc38a-903d-44b4-8bc7-6845676dfaee");
            log.warn("Empty stack (returning null)");
            writeline("/home/ubuntu/results/coverage/Digester/Digester_4_10.coverage", "2410de5f-07f2-4ac6-b78e-dff1b822b02d");
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
        writeline("/home/ubuntu/results/coverage/Digester/Digester_4_10.coverage", "f61b407f-19c2-462c-859c-62e01bf11d2a");
        if (stackAction != null) {
            writeline("/home/ubuntu/results/coverage/Digester/Digester_4_10.coverage", "edb57c71-5d28-4981-98fd-164ae17f9706");
            object = stackAction.onPush(this, null, object);
        }
        writeline("/home/ubuntu/results/coverage/Digester/Digester_4_10.coverage", "8b14eae5-2845-40d5-adc0-a2062bebfd87");
        if (stack.size() == 0) {
            writeline("/home/ubuntu/results/coverage/Digester/Digester_4_10.coverage", "b8b8f968-ccec-4052-b1bf-34e1c7ae35ab");
            root = object;
        }
        writeline("/home/ubuntu/results/coverage/Digester/Digester_4_10.coverage", "a5fe353d-b54c-4338-97c7-3a1b8ee358e4");
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
        writeline("/home/ubuntu/results/coverage/Digester/Digester_4_10.coverage", "fcb6d1d8-9f60-4ac7-b6e0-752fc71b6b5a");
        if (stackAction != null) {
            writeline("/home/ubuntu/results/coverage/Digester/Digester_4_10.coverage", "983cdb28-7527-4fd0-a8f9-4cdf41363efc");
            value = stackAction.onPush(this, stackName, value);
        }
        writeline("/home/ubuntu/results/coverage/Digester/Digester_4_10.coverage", "89eb2459-735f-4aaf-bc37-8fb6bb22b846");
        Stack<Object> namedStack = stacksByName.get(stackName);
        writeline("/home/ubuntu/results/coverage/Digester/Digester_4_10.coverage", "bb6a5f56-2037-4916-96ec-b4ce2657f9c6");
        if (namedStack == null) {
            writeline("/home/ubuntu/results/coverage/Digester/Digester_4_10.coverage", "5b9ed6c2-7a95-494d-b82c-cc151b450ee8");
            namedStack = new Stack<Object>();
            writeline("/home/ubuntu/results/coverage/Digester/Digester_4_10.coverage", "90ec8c5a-1c2c-40f8-82b4-f09314c545b2");
            stacksByName.put(stackName, namedStack);
        }
        writeline("/home/ubuntu/results/coverage/Digester/Digester_4_10.coverage", "e734c880-808c-41c1-b393-2751ac40018b");
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
        writeline("/home/ubuntu/results/coverage/Digester/Digester_4_10.coverage", "a3250458-9c66-493a-896a-43e48a17f1c7");
        T result = null;
        writeline("/home/ubuntu/results/coverage/Digester/Digester_4_10.coverage", "abf321f9-7d8a-4c91-9c88-ca8ef3864918");
        Stack<Object> namedStack = stacksByName.get(stackName);
        writeline("/home/ubuntu/results/coverage/Digester/Digester_4_10.coverage", "999c687d-38d6-45f1-ac16-7a55839a2617");
        if (namedStack == null) {
            writeline("/home/ubuntu/results/coverage/Digester/Digester_4_10.coverage", "ae84a22a-38c0-400e-b0a3-12b41a8d7b2d");
            if (log.isDebugEnabled()) {
                writeline("/home/ubuntu/results/coverage/Digester/Digester_4_10.coverage", "38bae30a-299a-49f2-ac08-33db039420b2");
                log.debug("Stack '" + stackName + "' is empty");
            }
            writeline("/home/ubuntu/results/coverage/Digester/Digester_4_10.coverage", "34d1d7da-1e44-4bd5-bdfa-b6840f9c112b");
            throw new EmptyStackException();
        }
        writeline("/home/ubuntu/results/coverage/Digester/Digester_4_10.coverage", "00813a88-6180-46f5-bb93-70b11aca5799");
        result = this.<T>npeSafeCast(namedStack.pop());
        writeline("/home/ubuntu/results/coverage/Digester/Digester_4_10.coverage", "c5aafdfa-ef6b-497d-94e4-f1f8ce6c97bd");
        if (stackAction != null) {
            writeline("/home/ubuntu/results/coverage/Digester/Digester_4_10.coverage", "d174d425-aec1-4000-8177-34f6d982a5ca");
            result = stackAction.onPop(this, stackName, result);
        }
        writeline("/home/ubuntu/results/coverage/Digester/Digester_4_10.coverage", "12480abc-001d-41d4-9633-df1b5480017d");
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
        writeline("/home/ubuntu/results/coverage/Digester/Digester_4_10.coverage", "5469efdf-ed63-4b2f-a8a1-3a3031bfc560");
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
        writeline("/home/ubuntu/results/coverage/Digester/Digester_4_10.coverage", "b638c260-cce2-424b-9192-718a460ce6a6");
        T result = null;
        writeline("/home/ubuntu/results/coverage/Digester/Digester_4_10.coverage", "32cbd473-9851-4ebf-89b0-a845eb4a9d98");
        Stack<Object> namedStack = stacksByName.get(stackName);
        writeline("/home/ubuntu/results/coverage/Digester/Digester_4_10.coverage", "21827d59-199d-41fd-b40d-771cd1f7f393");
        if (namedStack == null) {
            writeline("/home/ubuntu/results/coverage/Digester/Digester_4_10.coverage", "ecc09b44-1f54-46c8-ab4d-fd1f3fae1d44");
            if (log.isDebugEnabled()) {
                writeline("/home/ubuntu/results/coverage/Digester/Digester_4_10.coverage", "2106938d-f30d-4e80-84da-3a89ec80bf0d");
                log.debug("Stack '" + stackName + "' is empty");
            }
            writeline("/home/ubuntu/results/coverage/Digester/Digester_4_10.coverage", "f3387059-2bbf-4fa7-9d58-e880e46cc980");
            throw new EmptyStackException();
        }
        writeline("/home/ubuntu/results/coverage/Digester/Digester_4_10.coverage", "396092ef-370a-4a7c-9602-910cc4bc862f");
        int index = (namedStack.size() - 1) - n;
        writeline("/home/ubuntu/results/coverage/Digester/Digester_4_10.coverage", "dc1084c9-de52-4494-8126-9fd99892a972");
        if (index < 0) {
            writeline("/home/ubuntu/results/coverage/Digester/Digester_4_10.coverage", "d716c2c8-95ba-4ed1-b711-5cff6f9361fa");
            throw new EmptyStackException();
        }
        writeline("/home/ubuntu/results/coverage/Digester/Digester_4_10.coverage", "76359465-e92d-4980-9ad2-d92200008f6e");
        result = this.<T>npeSafeCast(namedStack.get(index));
        writeline("/home/ubuntu/results/coverage/Digester/Digester_4_10.coverage", "563d38c3-2b38-4a12-9a36-06a4b2a4b28b");
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
        writeline("/home/ubuntu/results/coverage/Digester/Digester_4_10.coverage", "d6c06a02-5445-42bf-a034-6b26de3c69af");
        boolean result = true;
        writeline("/home/ubuntu/results/coverage/Digester/Digester_4_10.coverage", "42d48070-f625-4101-932c-6862f69971d4");
        Stack<Object> namedStack = stacksByName.get(stackName);
        writeline("/home/ubuntu/results/coverage/Digester/Digester_4_10.coverage", "00c4a0a8-b1bb-42d3-8d44-51228b56c0c4");
        if (namedStack != null) {
            writeline("/home/ubuntu/results/coverage/Digester/Digester_4_10.coverage", "7e03d03f-18de-400c-ad3d-109f6c710167");
            result = namedStack.isEmpty();
        }
        writeline("/home/ubuntu/results/coverage/Digester/Digester_4_10.coverage", "aa2caa08-14f6-4018-83e6-a83277fd79f4");
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
        writeline("/home/ubuntu/results/coverage/Digester/Digester_4_10.coverage", "177a6ebf-b482-4886-a854-d1fc100de4d9");
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
        writeline("/home/ubuntu/results/coverage/Digester/Digester_4_10.coverage", "39cd7f5d-e02e-4fa0-9a42-39682e998f7a");
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
        writeline("/home/ubuntu/results/coverage/Digester/Digester_4_10.coverage", "919cf18c-e542-40e5-a7b3-6e3cef2a2645");
        for (InputSource source : inputSources) {
            writeline("/home/ubuntu/results/coverage/Digester/Digester_4_10.coverage", "db3af31d-6622-46cf-91ba-16f192062f20");
            try {
                writeline("/home/ubuntu/results/coverage/Digester/Digester_4_10.coverage", "be8ee60e-5fa1-4ea9-8f53-284147df0f75");
                source.getByteStream().close();
            } catch (IOException e) {
                writeline("/home/ubuntu/results/coverage/Digester/Digester_4_10.coverage", "c92a1cbb-006e-4993-8980-d71777996d72");
                if (log.isWarnEnabled()) {
                    writeline("/home/ubuntu/results/coverage/Digester/Digester_4_10.coverage", "a588d6f5-2a53-413d-ac94-422b270b8923");
                    log.warn(format("An error occurred while closing resource %s (%s)", source.getPublicId(), source.getSystemId()), e);
                }
            }
        }
        writeline("/home/ubuntu/results/coverage/Digester/Digester_4_10.coverage", "555a432f-9f66-42e9-b853-ffc19d0bdebd");
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
        writeline("/home/ubuntu/results/coverage/Digester/Digester_4_10.coverage", "85d4cf04-5835-4c25-966c-d800970af27a");
        if (configured) {
            writeline("/home/ubuntu/results/coverage/Digester/Digester_4_10.coverage", "fd9c6596-d7e2-46e6-89d8-5de7e6a49173");
            return;
        }
        writeline("/home/ubuntu/results/coverage/Digester/Digester_4_10.coverage", "461c43a8-19d9-4274-be58-9c2126b27958");
        initialize();
        writeline("/home/ubuntu/results/coverage/Digester/Digester_4_10.coverage", "cfe65896-baa3-4ca5-8903-fa78d2f61b6a");
        configured = true;
    }

    /**
     * Checks the Digester instance has been configured.
     *
     * @return true, if the Digester instance has been configured, false otherwise
     * @since 3.0
     */
    public boolean isConfigured() {
        writeline("/home/ubuntu/results/coverage/Digester/Digester_4_10.coverage", "b02bb160-c7b2-471b-8c7d-28e270306dd2");
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
        writeline("/home/ubuntu/results/coverage/Digester/Digester_4_10.coverage", "b44b9d2f-655d-4d0e-819f-d9893653ba63");
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
        writeline("/home/ubuntu/results/coverage/Digester/Digester_4_10.coverage", "89eff722-1787-438a-a4fc-9b1df2d8f5cc");
        try {
            writeline("/home/ubuntu/results/coverage/Digester/Digester_4_10.coverage", "28764b6b-799e-4a02-8ea1-49c745898759");
            return (params.peek());
        } catch (EmptyStackException e) {
            writeline("/home/ubuntu/results/coverage/Digester/Digester_4_10.coverage", "1c1c2860-0627-4832-a387-3d6d12b15095");
            log.warn("Empty stack (returning null)");
            writeline("/home/ubuntu/results/coverage/Digester/Digester_4_10.coverage", "079ce72f-f68a-48bf-8dae-4283ec0f87bb");
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
        writeline("/home/ubuntu/results/coverage/Digester/Digester_4_10.coverage", "e66150aa-d5e1-4bd9-a442-5bcd81b6a9b3");
        int index = (params.size() - 1) - n;
        writeline("/home/ubuntu/results/coverage/Digester/Digester_4_10.coverage", "50057e58-e457-4c7a-938c-e3331afc6ff2");
        if (index < 0) {
            writeline("/home/ubuntu/results/coverage/Digester/Digester_4_10.coverage", "5c77d66c-953c-4506-bad4-00765cd8012e");
            log.warn("Empty stack (returning null)");
            writeline("/home/ubuntu/results/coverage/Digester/Digester_4_10.coverage", "49d0116a-31f3-43bc-8c84-f9535b79ca66");
            return (null);
        }
        writeline("/home/ubuntu/results/coverage/Digester/Digester_4_10.coverage", "484d7ed9-705c-4f2a-8d23-d3078ec20aaf");
        try {
            writeline("/home/ubuntu/results/coverage/Digester/Digester_4_10.coverage", "a8c818d3-89ac-4d1b-835d-05d36c960114");
            return (params.get(index));
        } catch (EmptyStackException e) {
            writeline("/home/ubuntu/results/coverage/Digester/Digester_4_10.coverage", "de421429-23d9-4b94-beda-3d01eba7db09");
            log.warn("Empty stack (returning null)");
            writeline("/home/ubuntu/results/coverage/Digester/Digester_4_10.coverage", "ad00ce26-2919-4eac-9901-3853d4c5ecdc");
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
        writeline("/home/ubuntu/results/coverage/Digester/Digester_4_10.coverage", "6e28c653-f4dc-4eaa-bdb7-6d438ad11aac");
        try {
            writeline("/home/ubuntu/results/coverage/Digester/Digester_4_10.coverage", "a108bf97-92aa-4071-8352-1af42d2323db");
            if (log.isTraceEnabled()) {
                writeline("/home/ubuntu/results/coverage/Digester/Digester_4_10.coverage", "767c3c99-7168-443e-a2d0-24ce15b50e58");
                log.trace("Popping params");
            }
            writeline("/home/ubuntu/results/coverage/Digester/Digester_4_10.coverage", "de594b28-77ce-4082-a95c-621a0eacc2ec");
            return (params.pop());
        } catch (EmptyStackException e) {
            writeline("/home/ubuntu/results/coverage/Digester/Digester_4_10.coverage", "2e2db546-a9b7-4c24-a71a-1a62fddc2d84");
            log.warn("Empty stack (returning null)");
            writeline("/home/ubuntu/results/coverage/Digester/Digester_4_10.coverage", "adcfbee7-0e2a-42a4-acee-43d9565089a6");
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
        writeline("/home/ubuntu/results/coverage/Digester/Digester_4_10.coverage", "d27164bd-0a6d-40b6-88dc-338864b947ca");
        if (log.isTraceEnabled()) {
            writeline("/home/ubuntu/results/coverage/Digester/Digester_4_10.coverage", "222bf731-081f-484c-a8ad-5393f13af0f8");
            log.trace("Pushing params");
        }
        writeline("/home/ubuntu/results/coverage/Digester/Digester_4_10.coverage", "ccf574ec-a52e-4780-87c3-e2d5b4a3979a");
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
        writeline("/home/ubuntu/results/coverage/Digester/Digester_4_10.coverage", "d7ee17a9-8eda-4680-8234-32208bfe3f1e");
        if ((e != null) && (e instanceof InvocationTargetException)) {
            writeline("/home/ubuntu/results/coverage/Digester/Digester_4_10.coverage", "3e7e7fb6-e642-4568-8a9a-be5c3d0680f7");
            Throwable t = ((InvocationTargetException) e).getTargetException();
            writeline("/home/ubuntu/results/coverage/Digester/Digester_4_10.coverage", "5458e94b-75d5-4b6a-84e8-8149ee4c3b51");
            if ((t != null) && (t instanceof Exception)) {
                writeline("/home/ubuntu/results/coverage/Digester/Digester_4_10.coverage", "3fe3d91f-308c-4a13-bdb5-0709f64b6021");
                e = (Exception) t;
            }
        }
        writeline("/home/ubuntu/results/coverage/Digester/Digester_4_10.coverage", "d8ed68e3-9959-4754-baa5-d68b550a15fe");
        if (locator != null) {
            writeline("/home/ubuntu/results/coverage/Digester/Digester_4_10.coverage", "913c2070-71fb-4fac-b2aa-b55396330de7");
            String error = "Error at line " + locator.getLineNumber() + " char " + locator.getColumnNumber() + ": " + message;
            writeline("/home/ubuntu/results/coverage/Digester/Digester_4_10.coverage", "d7999fdc-568c-46ec-874d-6201946d5385");
            if (e != null) {
                writeline("/home/ubuntu/results/coverage/Digester/Digester_4_10.coverage", "824115a6-0e5b-4aeb-a98d-e44dad647662");
                return new SAXParseException(error, locator, e);
            }
            writeline("/home/ubuntu/results/coverage/Digester/Digester_4_10.coverage", "d200fd3a-78b8-443c-b3e9-e032ca79a245");
            return new SAXParseException(error, locator);
        }
        writeline("/home/ubuntu/results/coverage/Digester/Digester_4_10.coverage", "65460670-69cc-4680-b6c9-6bdfdf979199");
        log.error("No Locator!");
        writeline("/home/ubuntu/results/coverage/Digester/Digester_4_10.coverage", "938a9388-6e22-4590-8657-5e17d0afb912");
        if (e != null) {
            writeline("/home/ubuntu/results/coverage/Digester/Digester_4_10.coverage", "3c3140fa-ab01-4332-bc65-bd836eb1c18b");
            return new SAXException(message, e);
        }
        writeline("/home/ubuntu/results/coverage/Digester/Digester_4_10.coverage", "6453129f-3fd8-4edc-8798-2770ab32afeb");
        return new SAXException(message);
    }

    /**
     * Create a SAX exception which also understands about the location in the digester file where the exception occurs
     *
     * @param e the exception cause
     * @return the new SAX exception
     */
    public SAXException createSAXException(Exception e) {
        writeline("/home/ubuntu/results/coverage/Digester/Digester_4_10.coverage", "5cab6bf2-e9cc-4b23-8cce-c532b6daf832");
        if (e instanceof InvocationTargetException) {
            writeline("/home/ubuntu/results/coverage/Digester/Digester_4_10.coverage", "2a0cc322-f301-4495-8cec-116401d416fc");
            Throwable t = ((InvocationTargetException) e).getTargetException();
            writeline("/home/ubuntu/results/coverage/Digester/Digester_4_10.coverage", "fce6468e-8138-48d8-9f74-f1b3a1eca689");
            if ((t != null) && (t instanceof Exception)) {
                writeline("/home/ubuntu/results/coverage/Digester/Digester_4_10.coverage", "15965f10-e6d0-4bb1-8a7b-c3354920a1db");
                e = (Exception) t;
            }
        }
        writeline("/home/ubuntu/results/coverage/Digester/Digester_4_10.coverage", "8fee7ba7-41ab-427c-9c45-5e38289fed57");
        return createSAXException(e.getMessage(), e);
    }

    /**
     * Create a SAX exception which also understands about the location in the digester file where the exception occurs
     *
     * @param message the custom SAX exception message
     * @return the new SAX exception
     */
    public SAXException createSAXException(String message) {
        writeline("/home/ubuntu/results/coverage/Digester/Digester_4_10.coverage", "751e6adf-89c3-4839-a736-9721e4c7a2ea");
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
        writeline("/home/ubuntu/results/coverage/Digester/Digester_4_10.coverage", "2c2367ca-a140-4dc4-b353-82094d3e1fc0");
        if (obj == null) {
            writeline("/home/ubuntu/results/coverage/Digester/Digester_4_10.coverage", "923557f5-8889-4904-9dcc-d4963b4a4878");
            return null;
        }
        writeline("/home/ubuntu/results/coverage/Digester/Digester_4_10.coverage", "b55605f8-639c-475a-8ec0-16ad77675661");
        @SuppressWarnings("unchecked") T result = (T) obj;
        writeline("/home/ubuntu/results/coverage/Digester/Digester_4_10.coverage", "20b1d82d-feaa-4ada-ac2f-c5de3516ddeb");
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
