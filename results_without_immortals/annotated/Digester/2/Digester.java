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
        writeline("/home/ubuntu/results/coverage/Digester/Digester_2_10.coverage", "9b757c4c-5798-444f-b2a5-ab1f3573b222");
        Stack<String> nsStack = namespaces.get(prefix);
        writeline("/home/ubuntu/results/coverage/Digester/Digester_2_10.coverage", "c31346cc-79ed-45be-b7a9-5984c4fd3c7e");
        if (nsStack == null) {
            writeline("/home/ubuntu/results/coverage/Digester/Digester_2_10.coverage", "6c2bf8a1-f550-4ab0-bf69-7f470f424636");
            return null;
        }
        writeline("/home/ubuntu/results/coverage/Digester/Digester_2_10.coverage", "c31dc8b8-cf92-4070-accb-7ee0a98152fb");
        try {
            writeline("/home/ubuntu/results/coverage/Digester/Digester_2_10.coverage", "27290453-abbe-446d-93b3-86e30acaea8d");
            return (nsStack.peek());
        } catch (EmptyStackException e) {
            writeline("/home/ubuntu/results/coverage/Digester/Digester_2_10.coverage", "0b525805-1370-4bfe-bf21-007d2bb45e0b");
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
        writeline("/home/ubuntu/results/coverage/Digester/Digester_2_10.coverage", "a3b9e75e-821c-4d29-a5d3-e843d6f018c9");
        if (this.classLoader != null) {
            writeline("/home/ubuntu/results/coverage/Digester/Digester_2_10.coverage", "2d0cd554-7a9b-4b6a-a0a8-effa4fcdd5d0");
            return (this.classLoader);
        }
        writeline("/home/ubuntu/results/coverage/Digester/Digester_2_10.coverage", "6a9197d6-16e5-488c-9a02-682602b0267d");
        if (this.useContextClassLoader) {
            writeline("/home/ubuntu/results/coverage/Digester/Digester_2_10.coverage", "c28eb9c3-b0ac-4e50-8f60-076b704c8520");
            ClassLoader classLoader = Thread.currentThread().getContextClassLoader();
            writeline("/home/ubuntu/results/coverage/Digester/Digester_2_10.coverage", "6e7ec145-e2ed-4de6-90ae-1b606f00ecf1");
            if (classLoader != null) {
                writeline("/home/ubuntu/results/coverage/Digester/Digester_2_10.coverage", "2c755507-4d05-4dae-8c69-c1e90ed38176");
                return (classLoader);
            }
        }
        writeline("/home/ubuntu/results/coverage/Digester/Digester_2_10.coverage", "a7193916-90ac-4b23-8a07-ae770483c118");
        return (this.getClass().getClassLoader());
    }

    /**
     * Set the class loader to be used for instantiating application objects when required.
     *
     * @param classLoader The new class loader to use, or <code>null</code> to revert to the standard rules
     */
    public void setClassLoader(ClassLoader classLoader) {
        writeline("/home/ubuntu/results/coverage/Digester/Digester_2_10.coverage", "473b62b3-6829-494f-b350-84271391980e");
        this.classLoader = classLoader;
    }

    /**
     * Return the current depth of the element stack.
     *
     * @return the current depth of the element stack.
     */
    public int getCount() {
        writeline("/home/ubuntu/results/coverage/Digester/Digester_2_10.coverage", "43b3ed49-1e47-4ec7-b179-05e6fee214a1");
        return (stack.size());
    }

    /**
     * Return the name of the XML element that is currently being processed.
     *
     * @return the name of the XML element that is currently being processed.
     */
    public String getCurrentElementName() {
        writeline("/home/ubuntu/results/coverage/Digester/Digester_2_10.coverage", "abec6b41-b307-4b0c-beca-addb8f5a01d2");
        String elementName = match;
        writeline("/home/ubuntu/results/coverage/Digester/Digester_2_10.coverage", "50eeab49-b0ea-4a3e-8752-2b28ead7fd24");
        int lastSlash = elementName.lastIndexOf('/');
        writeline("/home/ubuntu/results/coverage/Digester/Digester_2_10.coverage", "80b91ee2-0cab-4600-b3b8-a100ad44984e");
        if (lastSlash >= 0) {
            writeline("/home/ubuntu/results/coverage/Digester/Digester_2_10.coverage", "36469a61-8e37-4fe7-b451-9ae42437f5a9");
            elementName = elementName.substring(lastSlash + 1);
        }
        writeline("/home/ubuntu/results/coverage/Digester/Digester_2_10.coverage", "6a11309f-bfff-4914-a974-2c58f237623f");
        return (elementName);
    }

    /**
     * Return the error handler for this Digester.
     *
     * @return the error handler for this Digester.
     */
    public ErrorHandler getErrorHandler() {
        writeline("/home/ubuntu/results/coverage/Digester/Digester_2_10.coverage", "1070421e-3cf6-4a8b-879e-e6ce8d1092db");
        return (this.errorHandler);
    }

    /**
     * Set the error handler for this Digester.
     *
     * @param errorHandler The new error handler
     */
    public void setErrorHandler(ErrorHandler errorHandler) {
        writeline("/home/ubuntu/results/coverage/Digester/Digester_2_10.coverage", "3f60a6d0-e623-4bdb-bfda-be591f4fe149");
        this.errorHandler = errorHandler;
    }

    /**
     * Return the SAXParserFactory we will use, creating one if necessary.
     *
     * @return the SAXParserFactory we will use, creating one if necessary.
     */
    public SAXParserFactory getFactory() {
        writeline("/home/ubuntu/results/coverage/Digester/Digester_2_10.coverage", "e686da40-6d4e-4880-9259-9ec688834728");
        if (factory == null) {
            writeline("/home/ubuntu/results/coverage/Digester/Digester_2_10.coverage", "2d8913e0-4146-4ef0-8bc0-e8eea2d2769b");
            factory = SAXParserFactory.newInstance();
            writeline("/home/ubuntu/results/coverage/Digester/Digester_2_10.coverage", "4bf8ff2a-6804-4753-902d-19c6d2aec90c");
            factory.setNamespaceAware(namespaceAware);
            writeline("/home/ubuntu/results/coverage/Digester/Digester_2_10.coverage", "e5c665b8-16a7-408f-b31c-c1b83d9b1e6e");
            factory.setXIncludeAware(xincludeAware);
            writeline("/home/ubuntu/results/coverage/Digester/Digester_2_10.coverage", "22e39a04-ba3e-4eac-8245-24ec43e4d223");
            factory.setValidating(validating);
            writeline("/home/ubuntu/results/coverage/Digester/Digester_2_10.coverage", "4301474a-2841-4bfe-87e9-b99250fa7303");
            factory.setSchema(schema);
        }
        writeline("/home/ubuntu/results/coverage/Digester/Digester_2_10.coverage", "9c617f0c-a7c8-4127-a819-bb12c44f1453");
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
        writeline("/home/ubuntu/results/coverage/Digester/Digester_2_10.coverage", "1727ff94-fb33-475e-98c6-26f68d6bb58f");
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
        writeline("/home/ubuntu/results/coverage/Digester/Digester_2_10.coverage", "1295df98-ab94-4537-a362-2b64d5c0c451");
        getFactory().setFeature(feature, value);
    }

    /**
     * Return the current Logger associated with this instance of the Digester
     *
     * @return the current Logger associated with this instance of the Digester
     */
    public Log getLogger() {
        writeline("/home/ubuntu/results/coverage/Digester/Digester_2_10.coverage", "54c15cb6-eb17-40a6-83bf-5e54b0c17115");
        return log;
    }

    /**
     * Set the current logger for this Digester.
     *
     * @param log the current logger for this Digester.
     */
    public void setLogger(Log log) {
        writeline("/home/ubuntu/results/coverage/Digester/Digester_2_10.coverage", "05de6f00-17bb-4883-a018-df64281564ef");
        this.log = log;
    }

    /**
     * Gets the logger used for logging SAX-related information. <strong>Note</strong> the output is finely grained.
     *
     * @return the logger used for logging SAX-related information
     * @since 1.6
     */
    public Log getSAXLogger() {
        writeline("/home/ubuntu/results/coverage/Digester/Digester_2_10.coverage", "9566974e-65c4-47b5-8ab6-162f7a1cd474");
        return saxLog;
    }

    /**
     * Sets the logger used for logging SAX-related information. <strong>Note</strong> the output is finely grained.
     *
     * @param saxLog the logger used for logging SAX-related information, not null
     * @since 1.6
     */
    public void setSAXLogger(Log saxLog) {
        writeline("/home/ubuntu/results/coverage/Digester/Digester_2_10.coverage", "87326234-d64f-4d50-8f2f-9a4f9f11630d");
        this.saxLog = saxLog;
    }

    /**
     * Return the current rule match path
     *
     * @return the current rule match path
     */
    public String getMatch() {
        writeline("/home/ubuntu/results/coverage/Digester/Digester_2_10.coverage", "7a49b39f-50ed-46ef-a5eb-4aeaffc2eb3a");
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
        writeline("/home/ubuntu/results/coverage/Digester/Digester_2_10.coverage", "f3dc6943-7053-4ec5-b4f5-014e41582c7c");
        return matches;
    }

    /**
     * Return the "namespace aware" flag for parsers we create.
     *
     * @return the "namespace aware" flag for parsers we create.
     */
    public boolean getNamespaceAware() {
        writeline("/home/ubuntu/results/coverage/Digester/Digester_2_10.coverage", "2d0be0a5-5d44-4939-8107-6099cfe723f4");
        return (this.namespaceAware);
    }

    /**
     * Set the "namespace aware" flag for parsers we create.
     *
     * @param namespaceAware The new "namespace aware" flag
     */
    public void setNamespaceAware(boolean namespaceAware) {
        writeline("/home/ubuntu/results/coverage/Digester/Digester_2_10.coverage", "1c30568e-6960-4bb2-b12e-4ed1653bd14a");
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
        writeline("/home/ubuntu/results/coverage/Digester/Digester_2_10.coverage", "dc02d2c2-85b1-4532-8573-0e04e82bffaa");
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
        writeline("/home/ubuntu/results/coverage/Digester/Digester_2_10.coverage", "b8bd1831-98db-4b40-b294-b11fd210f641");
        this.xincludeAware = xincludeAware;
    }

    /**
     * Set the public id of the current file being parse.
     *
     * @param publicId the DTD/Schema public's id.
     */
    public void setPublicId(String publicId) {
        writeline("/home/ubuntu/results/coverage/Digester/Digester_2_10.coverage", "9323c087-623e-4e74-8e86-0d94aeecf4e6");
        this.publicId = publicId;
    }

    /**
     * Return the public identifier of the DTD we are currently parsing under, if any.
     *
     * @return the public identifier of the DTD we are currently parsing under, if any.
     */
    public String getPublicId() {
        writeline("/home/ubuntu/results/coverage/Digester/Digester_2_10.coverage", "8b957082-bb70-49c7-8a05-d5b372d18128");
        return (this.publicId);
    }

    /**
     * Return the namespace URI that will be applied to all subsequently added <code>Rule</code> objects.
     *
     * @return the namespace URI that will be applied to all subsequently added <code>Rule</code> objects.
     */
    public String getRuleNamespaceURI() {
        writeline("/home/ubuntu/results/coverage/Digester/Digester_2_10.coverage", "2c4086e8-a987-4fbf-9845-ba1149a98417");
        return (getRules().getNamespaceURI());
    }

    /**
     * Set the namespace URI that will be applied to all subsequently added <code>Rule</code> objects.
     *
     * @param ruleNamespaceURI Namespace URI that must match on all subsequently added rules, or <code>null</code> for
     *            matching regardless of the current namespace URI
     */
    public void setRuleNamespaceURI(String ruleNamespaceURI) {
        writeline("/home/ubuntu/results/coverage/Digester/Digester_2_10.coverage", "76d47647-b72e-4e0b-ae76-a7f16a0092ff");
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
        writeline("/home/ubuntu/results/coverage/Digester/Digester_2_10.coverage", "32db96fc-6486-46fa-b228-38e0bac93c73");
        if (parser != null) {
            writeline("/home/ubuntu/results/coverage/Digester/Digester_2_10.coverage", "279ce50b-287b-4a9b-a7e8-99123c3ae5c2");
            return (parser);
        }
        writeline("/home/ubuntu/results/coverage/Digester/Digester_2_10.coverage", "e54e067b-e8cd-4336-bad3-784aa0409104");
        try {
            writeline("/home/ubuntu/results/coverage/Digester/Digester_2_10.coverage", "7690cc24-04af-4ab9-9ce2-939a0394fc18");
            parser = getFactory().newSAXParser();
        } catch (Exception e) {
            writeline("/home/ubuntu/results/coverage/Digester/Digester_2_10.coverage", "c077211d-4055-414d-93f2-1cac7c57a9c7");
            log.error("Digester.getParser: ", e);
            writeline("/home/ubuntu/results/coverage/Digester/Digester_2_10.coverage", "7343ff15-d236-4c9e-999c-a6b43906cf17");
            return (null);
        }
        writeline("/home/ubuntu/results/coverage/Digester/Digester_2_10.coverage", "9a86fcc8-6c59-422d-aa0e-7cb6bfb0164b");
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
        writeline("/home/ubuntu/results/coverage/Digester/Digester_2_10.coverage", "de904353-37a7-4e47-aa74-672a57aedfb3");
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
        writeline("/home/ubuntu/results/coverage/Digester/Digester_2_10.coverage", "dd7e7251-22d4-44f1-b109-7398211b86e3");
        getParser().setProperty(property, value);
    }

    /**
     * Return the <code>Rules</code> implementation object containing our rules collection and associated matching
     * policy. If none has been established, a default implementation will be created and returned.
     *
     * @return the <code>Rules</code> implementation object.
     */
    public Rules getRules() {
        writeline("/home/ubuntu/results/coverage/Digester/Digester_2_10.coverage", "b2872da4-e849-4e72-b91d-a8c350905820");
        if (this.rules == null) {
            writeline("/home/ubuntu/results/coverage/Digester/Digester_2_10.coverage", "1b746e4a-5bba-42a3-80a0-6d194d01b360");
            this.rules = new RulesBase();
            writeline("/home/ubuntu/results/coverage/Digester/Digester_2_10.coverage", "76cc0ec3-da0f-4d07-832b-f4f53f4f457c");
            this.rules.setDigester(this);
        }
        writeline("/home/ubuntu/results/coverage/Digester/Digester_2_10.coverage", "f6039c3d-309a-4493-8683-8bbfb3dbb504");
        return (this.rules);
    }

    /**
     * Set the <code>Rules</code> implementation object containing our rules collection and associated matching policy.
     *
     * @param rules New Rules implementation
     */
    public void setRules(Rules rules) {
        writeline("/home/ubuntu/results/coverage/Digester/Digester_2_10.coverage", "6621faac-a1d3-4bfc-af4f-d432b63c5009");
        this.rules = rules;
        writeline("/home/ubuntu/results/coverage/Digester/Digester_2_10.coverage", "e37ee396-af7d-47e3-be12-8adb5bee9c57");
        this.rules.setDigester(this);
    }

    /**
     * Return the XML Schema used when parsing.
     *
     * @return The {@link Schema} instance in use.
     * @since 2.0
     */
    public Schema getXMLSchema() {
        writeline("/home/ubuntu/results/coverage/Digester/Digester_2_10.coverage", "a452ca13-f44f-4910-bab9-c58f312ed7a4");
        return (this.schema);
    }

    /**
     * Set the XML Schema to be used when parsing.
     *
     * @param schema The {@link Schema} instance to use.
     * @since 2.0
     */
    public void setXMLSchema(Schema schema) {
        writeline("/home/ubuntu/results/coverage/Digester/Digester_2_10.coverage", "a0a9889e-9a37-4d66-95f5-0591122d2de2");
        this.schema = schema;
    }

    /**
     * Return the boolean as to whether the context ClassLoader should be used.
     *
     * @return true, if the context ClassLoader should be used, false otherwise.
     */
    public boolean getUseContextClassLoader() {
        writeline("/home/ubuntu/results/coverage/Digester/Digester_2_10.coverage", "6497036e-670d-419a-8d88-c2791d91be6a");
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
        writeline("/home/ubuntu/results/coverage/Digester/Digester_2_10.coverage", "ee24761f-fed0-4301-80e0-d54d595d9746");
        useContextClassLoader = use;
    }

    /**
     * Return the validating parser flag.
     *
     * @return the validating parser flag.
     */
    public boolean getValidating() {
        writeline("/home/ubuntu/results/coverage/Digester/Digester_2_10.coverage", "e3fa2eda-4767-495f-b93c-1a568c214af3");
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
        writeline("/home/ubuntu/results/coverage/Digester/Digester_2_10.coverage", "6da4bcbf-25b5-4faa-9cc2-b614c5ccff57");
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
        writeline("/home/ubuntu/results/coverage/Digester/Digester_2_10.coverage", "0ed5500d-387f-451d-b26f-3635077ad56e");
        if (reader == null) {
            writeline("/home/ubuntu/results/coverage/Digester/Digester_2_10.coverage", "80b47262-4592-4b41-88ba-62a7f332ee0b");
            reader = getParser().getXMLReader();
        }
        writeline("/home/ubuntu/results/coverage/Digester/Digester_2_10.coverage", "5f072e04-632c-43a9-911a-fa1a1c8febfc");
        reader.setDTDHandler(this);
        writeline("/home/ubuntu/results/coverage/Digester/Digester_2_10.coverage", "fafcd1ff-7c1d-434e-a191-7c81e9d31f0f");
        reader.setContentHandler(this);
        writeline("/home/ubuntu/results/coverage/Digester/Digester_2_10.coverage", "ce124d4c-dc65-4d5f-82a6-b45a31239aa4");
        if (entityResolver == null) {
            writeline("/home/ubuntu/results/coverage/Digester/Digester_2_10.coverage", "0d635ea9-2e3a-4c96-a244-5656c24ad3e1");
            reader.setEntityResolver(this);
        } else {
            writeline("/home/ubuntu/results/coverage/Digester/Digester_2_10.coverage", "637a15bf-2d8d-4048-becb-1fa9cef8b14c");
            reader.setEntityResolver(entityResolver);
        }
        writeline("/home/ubuntu/results/coverage/Digester/Digester_2_10.coverage", "001a24d2-0fac-41b8-af3e-9df86131085b");
        if (this.errorHandler != null) {
            writeline("/home/ubuntu/results/coverage/Digester/Digester_2_10.coverage", "ce27a0e6-d8a1-4a5d-b420-e06e2b343287");
            reader.setErrorHandler(this.errorHandler);
        } else {
            writeline("/home/ubuntu/results/coverage/Digester/Digester_2_10.coverage", "1decbd7f-00dc-4b25-b0dd-c79aa416be9f");
            reader.setErrorHandler(this);
        }
        writeline("/home/ubuntu/results/coverage/Digester/Digester_2_10.coverage", "47c52cb0-faab-4ae2-aa05-6114700c30e5");
        return reader;
    }

    /**
     * Gets the <code>Substitutor</code> used to convert attributes and body text.
     *
     * @return the <code>Substitutor</code> used to convert attributes and body text,
     *         null if not substitutions are to be performed.
     */
    public Substitutor getSubstitutor() {
        writeline("/home/ubuntu/results/coverage/Digester/Digester_2_10.coverage", "fe7c30c0-561a-4d47-a44a-c8b3a9a378c1");
        return substitutor;
    }

    /**
     * Sets the <code>Substitutor</code> to be used to convert attributes and body text.
     *
     * @param substitutor the Substitutor to be used to convert attributes and body text or null if not substitution of
     *            these values is to be performed.
     */
    public void setSubstitutor(Substitutor substitutor) {
        writeline("/home/ubuntu/results/coverage/Digester/Digester_2_10.coverage", "6d86a14d-01e8-4aee-931f-481bad97b73a");
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
        writeline("/home/ubuntu/results/coverage/Digester/Digester_2_10.coverage", "79a94f95-aea1-46ff-9796-48d61fb55d83");
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
        writeline("/home/ubuntu/results/coverage/Digester/Digester_2_10.coverage", "57460896-834e-4ae6-af4b-ccc690553fb6");
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
        writeline("/home/ubuntu/results/coverage/Digester/Digester_2_10.coverage", "0295608b-84de-4f61-b1d0-1f063a839a87");
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
        writeline("/home/ubuntu/results/coverage/Digester/Digester_2_10.coverage", "ee50fb94-f8a1-4215-b8a0-799bf7cbd778");
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
        writeline("/home/ubuntu/results/coverage/Digester/Digester_2_10.coverage", "6277d2c8-9710-445c-8669-54a488e7ec98");
        if (!namespaceAware) {
            writeline("/home/ubuntu/results/coverage/Digester/Digester_2_10.coverage", "3c2c1698-1f58-4bfe-afff-ab26eb90aedd");
            log.warn("Digester is not namespace aware");
        }
        writeline("/home/ubuntu/results/coverage/Digester/Digester_2_10.coverage", "97e31c74-db79-44ad-b336-4d9515049dbb");
        Map<String, String> currentNamespaces = new HashMap<String, String>();
        writeline("/home/ubuntu/results/coverage/Digester/Digester_2_10.coverage", "870d35f4-bc8c-43c5-a172-cf45538e1d45");
        for (Map.Entry<String, Stack<String>> nsEntry : namespaces.entrySet()) {
            writeline("/home/ubuntu/results/coverage/Digester/Digester_2_10.coverage", "ec6e009d-e431-4778-8b0e-d95d58613847");
            try {
                writeline("/home/ubuntu/results/coverage/Digester/Digester_2_10.coverage", "bf781c79-2834-4169-acbf-0e79bd51175a");
                currentNamespaces.put(nsEntry.getKey(), nsEntry.getValue().peek());
            } catch (RuntimeException e) {
                writeline("/home/ubuntu/results/coverage/Digester/Digester_2_10.coverage", "8eb6f258-ace6-43be-ad26-3d6e3435181d");
                log.error(e.getMessage(), e);
                writeline("/home/ubuntu/results/coverage/Digester/Digester_2_10.coverage", "e2f38703-db80-44e8-8d07-2da7f05e2011");
                throw e;
            }
        }
        writeline("/home/ubuntu/results/coverage/Digester/Digester_2_10.coverage", "351e4732-be51-4503-a00b-e81c4e6cdc5b");
        return currentNamespaces;
    }

    /**
     * Returns the executor service used to run asynchronous parse method.
     *
     * @return the executor service used to run asynchronous parse method
     * @since 3.1
     */
    public ExecutorService getExecutorService() {
        writeline("/home/ubuntu/results/coverage/Digester/Digester_2_10.coverage", "8c0adec6-f722-48a4-a9ea-e2d5bbaede3c");
        return executorService;
    }

    /**
     * Sets the executor service to run asynchronous parse method.
     *
     * @param executorService the executor service to run asynchronous parse method
     * @since 3.1
     */
    public void setExecutorService(ExecutorService executorService) {
        writeline("/home/ubuntu/results/coverage/Digester/Digester_2_10.coverage", "fff2974d-0f60-4cdc-b5a6-bad8a0cd3c9c");
        this.executorService = executorService;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public void characters(char buffer[], int start, int length) throws SAXException {
        writeline("/home/ubuntu/results/coverage/Digester/Digester_2_10.coverage", "b75b1d09-0bbe-44d7-bad3-85403d386f44");
        if (customContentHandler != null) {
            writeline("/home/ubuntu/results/coverage/Digester/Digester_2_10.coverage", "05372a85-4e47-42e5-9168-81e39e8da30f");
            customContentHandler.characters(buffer, start, length);
            writeline("/home/ubuntu/results/coverage/Digester/Digester_2_10.coverage", "33d254c2-e0f4-4396-abdb-a189ae37cd52");
            return;
        }
        writeline("/home/ubuntu/results/coverage/Digester/Digester_2_10.coverage", "22551e4a-2568-4e4e-9c21-67f911ced871");
        if (saxLog.isDebugEnabled()) {
            writeline("/home/ubuntu/results/coverage/Digester/Digester_2_10.coverage", "c53e89cb-3873-4f37-908b-c4d4e18bb7d2");
            saxLog.debug("characters(" + new String(buffer, start, length) + ")");
        }
        writeline("/home/ubuntu/results/coverage/Digester/Digester_2_10.coverage", "9dbcd101-f1f7-4bcd-892d-c4460fc40185");
        bodyText.append(buffer, start, length);
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public void endDocument() throws SAXException {
        writeline("/home/ubuntu/results/coverage/Digester/Digester_2_10.coverage", "a6b221ae-7881-4884-9b2f-345664798da8");
        if (saxLog.isDebugEnabled()) {
            writeline("/home/ubuntu/results/coverage/Digester/Digester_2_10.coverage", "1168edf8-eebc-4d06-b643-6343490b5ea0");
            if (getCount() > 1) {
                writeline("/home/ubuntu/results/coverage/Digester/Digester_2_10.coverage", "a579ba53-ac23-4a4e-a029-2d34376a31cb");
                saxLog.debug("endDocument():  " + getCount() + " elements left");
            } else {
                writeline("/home/ubuntu/results/coverage/Digester/Digester_2_10.coverage", "c8f68e05-0e0b-438a-bc3c-af3c0c7ec44f");
                saxLog.debug("endDocument()");
            }
        }
        writeline("/home/ubuntu/results/coverage/Digester/Digester_2_10.coverage", "be1195d3-58ce-4520-9593-e06f8844cd36");
        for (Rule rule : getRules().rules()) {
            writeline("/home/ubuntu/results/coverage/Digester/Digester_2_10.coverage", "64a2d25d-b5bc-49fe-9fa8-7bddf19b3d79");
            try {
                writeline("/home/ubuntu/results/coverage/Digester/Digester_2_10.coverage", "10ef5d72-f999-4a76-b457-efff7719604a");
                rule.finish();
            } catch (Exception e) {
                writeline("/home/ubuntu/results/coverage/Digester/Digester_2_10.coverage", "54976886-5444-4e6e-b1f2-58ed127c781d");
                log.error("Finish event threw exception", e);
                writeline("/home/ubuntu/results/coverage/Digester/Digester_2_10.coverage", "4d86fd48-1dce-49b6-91a5-10737eb51b19");
                throw createSAXException(e);
            } catch (Error e) {
                writeline("/home/ubuntu/results/coverage/Digester/Digester_2_10.coverage", "aa991494-091d-4494-b8fd-bfcc1be99fc9");
                log.error("Finish event threw error", e);
                writeline("/home/ubuntu/results/coverage/Digester/Digester_2_10.coverage", "330dac21-560d-4181-8bdc-43805c8a14bf");
                throw e;
            }
        }
        writeline("/home/ubuntu/results/coverage/Digester/Digester_2_10.coverage", "95da27f4-382b-46d0-bffd-8c9e060a0c50");
        clear();
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public void endElement(String namespaceURI, String localName, String qName) throws SAXException {
        writeline("/home/ubuntu/results/coverage/Digester/Digester_2_10.coverage", "9c8db779-6f94-4f33-b8bd-7f19841667bc");
        if (customContentHandler != null) {
            writeline("/home/ubuntu/results/coverage/Digester/Digester_2_10.coverage", "10116f92-cf53-4423-9fd9-a610ac1f492a");
            customContentHandler.endElement(namespaceURI, localName, qName);
            writeline("/home/ubuntu/results/coverage/Digester/Digester_2_10.coverage", "2db0c350-26dc-4123-893a-9bf569c370c6");
            return;
        }
        writeline("/home/ubuntu/results/coverage/Digester/Digester_2_10.coverage", "304b8171-0cef-4d51-83cb-6754e83d3c81");
        boolean debug = log.isDebugEnabled();
        writeline("/home/ubuntu/results/coverage/Digester/Digester_2_10.coverage", "bea50fea-71cc-4528-883c-d0018c79635c");
        if (debug) {
            writeline("/home/ubuntu/results/coverage/Digester/Digester_2_10.coverage", "f110a1aa-1d32-43c9-a932-3d31269a9527");
            if (saxLog.isDebugEnabled()) {
                writeline("/home/ubuntu/results/coverage/Digester/Digester_2_10.coverage", "e6f6c0e7-6bf2-433d-9226-d0ea8fbe5c32");
                saxLog.debug("endElement(" + namespaceURI + "," + localName + "," + qName + ")");
            }
            writeline("/home/ubuntu/results/coverage/Digester/Digester_2_10.coverage", "117f8efc-18ab-4876-9478-240f8a2baea6");
            log.debug("  match='" + match + "'");
            writeline("/home/ubuntu/results/coverage/Digester/Digester_2_10.coverage", "fbb99709-3780-4e41-bde7-b056f65b12f3");
            log.debug("  bodyText='" + bodyText + "'");
        }
        writeline("/home/ubuntu/results/coverage/Digester/Digester_2_10.coverage", "da1daabf-e655-4f34-a41b-c3269dca713b");
        String name = localName;
        writeline("/home/ubuntu/results/coverage/Digester/Digester_2_10.coverage", "e6859369-ed72-47d1-b293-48ad8bbb223c");
        if ((name == null) || (name.length() < 1)) {
            writeline("/home/ubuntu/results/coverage/Digester/Digester_2_10.coverage", "4b62c3fd-b61c-4dbd-b23c-28b779bb9c1f");
            name = qName;
        }
        writeline("/home/ubuntu/results/coverage/Digester/Digester_2_10.coverage", "85a49359-0268-401d-9e33-85a8218eb8f4");
        List<Rule> rules = matches.pop();
        writeline("/home/ubuntu/results/coverage/Digester/Digester_2_10.coverage", "d04ce91b-8f9c-4744-967f-b7474d2b10ef");
        if ((rules != null) && (rules.size() > 0)) {
            writeline("/home/ubuntu/results/coverage/Digester/Digester_2_10.coverage", "7e74a8cb-4d67-40d8-834b-fc6ed7f7c79a");
            String bodyText = this.bodyText.toString();
            writeline("/home/ubuntu/results/coverage/Digester/Digester_2_10.coverage", "c315efec-fc13-4863-a738-564b8316ed12");
            Substitutor substitutor = getSubstitutor();
            writeline("/home/ubuntu/results/coverage/Digester/Digester_2_10.coverage", "38b6fb48-96d5-46bb-b6f2-c90836d40f9b");
            if (substitutor != null) {
                writeline("/home/ubuntu/results/coverage/Digester/Digester_2_10.coverage", "91964815-3ce1-4f5c-906c-cc924a2152d4");
                bodyText = substitutor.substitute(bodyText);
            }
            writeline("/home/ubuntu/results/coverage/Digester/Digester_2_10.coverage", "feddf227-6671-4c8c-a648-89c5649cf29d");
            for (int i = 0; i < rules.size(); i++) {
                writeline("/home/ubuntu/results/coverage/Digester/Digester_2_10.coverage", "38012571-682c-4015-b9fb-cda208e7b974");
                try {
                    writeline("/home/ubuntu/results/coverage/Digester/Digester_2_10.coverage", "ca98872f-4c06-4dac-b024-5663749d80a4");
                    Rule rule = rules.get(i);
                    writeline("/home/ubuntu/results/coverage/Digester/Digester_2_10.coverage", "b417bcb1-06f4-4a09-a441-d967d88ff26d");
                    if (debug) {
                        writeline("/home/ubuntu/results/coverage/Digester/Digester_2_10.coverage", "ea648f5f-3f3a-479c-9e66-126a05d31adf");
                        log.debug("  Fire body() for " + rule);
                    }
                    writeline("/home/ubuntu/results/coverage/Digester/Digester_2_10.coverage", "c98ad1cf-90c7-48d7-a2d2-eb16e81bd5b9");
                    rule.body(namespaceURI, name, bodyText);
                } catch (Exception e) {
                    writeline("/home/ubuntu/results/coverage/Digester/Digester_2_10.coverage", "5d883bf5-c7c3-488a-b488-28d0bfdedfe7");
                    log.error("Body event threw exception", e);
                    writeline("/home/ubuntu/results/coverage/Digester/Digester_2_10.coverage", "131cf767-4aba-469b-8f33-8a0c728a39cd");
                    throw createSAXException(e);
                } catch (Error e) {
                    writeline("/home/ubuntu/results/coverage/Digester/Digester_2_10.coverage", "ee10af61-506b-43f7-9f94-6b6dbf51f658");
                    log.error("Body event threw error", e);
                    writeline("/home/ubuntu/results/coverage/Digester/Digester_2_10.coverage", "c5ef2a67-9904-4b16-86e0-444da408a5cd");
                    throw e;
                }
            }
        } else {
            writeline("/home/ubuntu/results/coverage/Digester/Digester_2_10.coverage", "b1e5a3a9-a026-4a46-b94f-4e462d1d476b");
            if (debug) {
                writeline("/home/ubuntu/results/coverage/Digester/Digester_2_10.coverage", "d187bf65-9580-43ef-a8e8-d07e157ab5b3");
                log.debug("  No rules found matching '" + match + "'.");
            }
        }
        writeline("/home/ubuntu/results/coverage/Digester/Digester_2_10.coverage", "75c369c7-901f-4429-ba45-2a47fa0d0c13");
        bodyText = bodyTexts.pop();
        writeline("/home/ubuntu/results/coverage/Digester/Digester_2_10.coverage", "f5ecc311-1117-41e0-9cef-f49fa6791b25");
        if (debug) {
            writeline("/home/ubuntu/results/coverage/Digester/Digester_2_10.coverage", "8b48a8a3-35c2-420d-83a5-bdb9b0d8b054");
            log.debug("  Popping body text '" + bodyText.toString() + "'");
        }
        writeline("/home/ubuntu/results/coverage/Digester/Digester_2_10.coverage", "be21c6f1-9cc7-4ba3-a86b-f87106045287");
        if (rules != null) {
            writeline("/home/ubuntu/results/coverage/Digester/Digester_2_10.coverage", "1f97406c-dbea-47bd-8e5d-12c9f66e365f");
            for (int i = 0; i < rules.size(); i++) {
                writeline("/home/ubuntu/results/coverage/Digester/Digester_2_10.coverage", "bc7f7c14-7e2a-45d9-8c3b-6d4176798258");
                int j = (rules.size() - i) - 1;
                writeline("/home/ubuntu/results/coverage/Digester/Digester_2_10.coverage", "0c421530-31c9-4b40-82e8-7fb230364614");
                try {
                    writeline("/home/ubuntu/results/coverage/Digester/Digester_2_10.coverage", "cbb6cfb7-f8d5-411d-ad20-bfccff00b8cd");
                    Rule rule = rules.get(j);
                    writeline("/home/ubuntu/results/coverage/Digester/Digester_2_10.coverage", "b0e6f883-687b-42c9-a451-fa4940700126");
                    if (debug) {
                        writeline("/home/ubuntu/results/coverage/Digester/Digester_2_10.coverage", "514fb127-04e0-498e-8983-9fb27d53de6d");
                        log.debug("  Fire end() for " + rule);
                    }
                    writeline("/home/ubuntu/results/coverage/Digester/Digester_2_10.coverage", "feb4a164-ed67-49c2-9aed-48994ed7cf2c");
                    rule.end(namespaceURI, name);
                } catch (Exception e) {
                    writeline("/home/ubuntu/results/coverage/Digester/Digester_2_10.coverage", "338ae524-580f-429d-8091-86cd08ac40fe");
                    log.error("End event threw exception", e);
                    writeline("/home/ubuntu/results/coverage/Digester/Digester_2_10.coverage", "e92c3d8a-6dc9-4f9f-887e-4ad63d355b57");
                    throw createSAXException(e);
                } catch (Error e) {
                    writeline("/home/ubuntu/results/coverage/Digester/Digester_2_10.coverage", "32b516c2-dd02-40da-af1b-ea735e43bbd9");
                    log.error("End event threw error", e);
                    writeline("/home/ubuntu/results/coverage/Digester/Digester_2_10.coverage", "94ac4a43-fd81-429c-9e6e-76d1f7c14115");
                    throw e;
                }
            }
        }
        writeline("/home/ubuntu/results/coverage/Digester/Digester_2_10.coverage", "d302f243-1498-4ae3-9a63-b44c2f0508d5");
        int slash = match.lastIndexOf('/');
        writeline("/home/ubuntu/results/coverage/Digester/Digester_2_10.coverage", "94077297-8c4a-48c9-b030-65ec47a50d4d");
        if (slash >= 0) {
            writeline("/home/ubuntu/results/coverage/Digester/Digester_2_10.coverage", "2675dfdf-e384-4ff6-9a38-b9ab0c56f1fa");
            match = match.substring(0, slash);
        } else {
            writeline("/home/ubuntu/results/coverage/Digester/Digester_2_10.coverage", "8cd6bf51-110e-42cb-87a4-d57b70b61cd2");
            match = "";
        }
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public void endPrefixMapping(String prefix) throws SAXException {
        writeline("/home/ubuntu/results/coverage/Digester/Digester_2_10.coverage", "8bf3c18d-3fb6-473c-9fa2-a2b5801d4510");
        if (saxLog.isDebugEnabled()) {
            writeline("/home/ubuntu/results/coverage/Digester/Digester_2_10.coverage", "a4d15f76-5271-46af-a709-04439e7adb0b");
            saxLog.debug("endPrefixMapping(" + prefix + ")");
        }
        writeline("/home/ubuntu/results/coverage/Digester/Digester_2_10.coverage", "8d356d2a-cf7b-4ace-ad16-c0d29924c636");
        Stack<String> stack = namespaces.get(prefix);
        writeline("/home/ubuntu/results/coverage/Digester/Digester_2_10.coverage", "2662434c-5400-4e54-92fc-a551659032e7");
        if (stack == null) {
            writeline("/home/ubuntu/results/coverage/Digester/Digester_2_10.coverage", "dd4cb4d7-dc41-4fc5-938a-83e90fd3f7e7");
            return;
        }
        writeline("/home/ubuntu/results/coverage/Digester/Digester_2_10.coverage", "5a964b99-5ea6-4d28-833b-fb3fac87fd4f");
        try {
            writeline("/home/ubuntu/results/coverage/Digester/Digester_2_10.coverage", "ab8cbcdd-6e7b-4af3-b20d-75a80da47864");
            stack.pop();
            writeline("/home/ubuntu/results/coverage/Digester/Digester_2_10.coverage", "c7476ff8-961f-4ce3-999d-10851d9a585a");
            if (stack.empty()) {
                writeline("/home/ubuntu/results/coverage/Digester/Digester_2_10.coverage", "18abc46b-faf3-4180-863a-7b106b9aafc3");
                namespaces.remove(prefix);
            }
        } catch (EmptyStackException e) {
            writeline("/home/ubuntu/results/coverage/Digester/Digester_2_10.coverage", "dceacc61-749e-47d1-80e0-b3de52d4fbba");
            throw createSAXException("endPrefixMapping popped too many times");
        }
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public void ignorableWhitespace(char buffer[], int start, int len) throws SAXException {
        writeline("/home/ubuntu/results/coverage/Digester/Digester_2_10.coverage", "9fbe709e-9a19-446c-bdad-d34ef38f997c");
        if (saxLog.isDebugEnabled()) {
            writeline("/home/ubuntu/results/coverage/Digester/Digester_2_10.coverage", "4360e221-52bd-43d7-9567-9a14f279a62f");
            saxLog.debug("ignorableWhitespace(" + new String(buffer, start, len) + ")");
        }
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public void processingInstruction(String target, String data) throws SAXException {
        writeline("/home/ubuntu/results/coverage/Digester/Digester_2_10.coverage", "7f1360e8-e959-4845-9848-ebb18f1f17e7");
        if (customContentHandler != null) {
            writeline("/home/ubuntu/results/coverage/Digester/Digester_2_10.coverage", "69a59a2f-1d34-4575-a2bf-7c9ebf1926d4");
            customContentHandler.processingInstruction(target, data);
            writeline("/home/ubuntu/results/coverage/Digester/Digester_2_10.coverage", "b1f1c9f0-48d7-4e2f-bf3c-b857d68d137d");
            return;
        }
        writeline("/home/ubuntu/results/coverage/Digester/Digester_2_10.coverage", "0f471e76-8f35-4c7d-9cd7-6a17c07cd7db");
        if (saxLog.isDebugEnabled()) {
            writeline("/home/ubuntu/results/coverage/Digester/Digester_2_10.coverage", "65d1314a-550f-4815-836b-566a67a77ee3");
            saxLog.debug("processingInstruction('" + target + "','" + data + "')");
        }
    }

    /**
     * Gets the document locator associated with our parser.
     *
     * @return the Locator supplied by the document parser
     */
    public Locator getDocumentLocator() {
        writeline("/home/ubuntu/results/coverage/Digester/Digester_2_10.coverage", "8c127269-a1a3-4efb-a15e-de8617a18dfe");
        return locator;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public void setDocumentLocator(Locator locator) {
        writeline("/home/ubuntu/results/coverage/Digester/Digester_2_10.coverage", "c087f8c7-d5f6-4648-90fe-89c078fa64fc");
        if (saxLog.isDebugEnabled()) {
            writeline("/home/ubuntu/results/coverage/Digester/Digester_2_10.coverage", "105f37b6-cd4b-4a55-bc0e-1e9c03388cc6");
            saxLog.debug("setDocumentLocator(" + locator + ")");
        }
        writeline("/home/ubuntu/results/coverage/Digester/Digester_2_10.coverage", "25a37bf3-4ab0-4d30-a89c-6247c230534b");
        this.locator = locator;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public void skippedEntity(String name) throws SAXException {
        writeline("/home/ubuntu/results/coverage/Digester/Digester_2_10.coverage", "9b1777e9-2911-4b14-8e5a-3c1656342e9c");
        if (saxLog.isDebugEnabled()) {
            writeline("/home/ubuntu/results/coverage/Digester/Digester_2_10.coverage", "b627c262-5880-427e-866b-5f570c137f98");
            saxLog.debug("skippedEntity(" + name + ")");
        }
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public void startDocument() throws SAXException {
        writeline("/home/ubuntu/results/coverage/Digester/Digester_2_10.coverage", "7f7b213e-9b8a-4c13-ba58-986afea05838");
        if (saxLog.isDebugEnabled()) {
            writeline("/home/ubuntu/results/coverage/Digester/Digester_2_10.coverage", "da041b6e-d144-4b7a-b485-ef7d8d28bf92");
            saxLog.debug("startDocument()");
        }
        writeline("/home/ubuntu/results/coverage/Digester/Digester_2_10.coverage", "7bb0cde3-794f-4307-8635-835aa65983e6");
        configure();
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public void startElement(String namespaceURI, String localName, String qName, Attributes list) throws SAXException {
        writeline("/home/ubuntu/results/coverage/Digester/Digester_2_10.coverage", "e8dc8d66-818b-4761-ad72-c47336b2af56");
        boolean debug = log.isDebugEnabled();
        writeline("/home/ubuntu/results/coverage/Digester/Digester_2_10.coverage", "744de6e1-7b10-41f6-a9d8-c84ae9011f7c");
        if (customContentHandler != null) {
            writeline("/home/ubuntu/results/coverage/Digester/Digester_2_10.coverage", "e9133b3f-ffdf-44e0-bdb3-a7b9f306ccb6");
            customContentHandler.startElement(namespaceURI, localName, qName, list);
            writeline("/home/ubuntu/results/coverage/Digester/Digester_2_10.coverage", "813c74b8-010f-413d-85d4-44235b501be4");
            return;
        }
        writeline("/home/ubuntu/results/coverage/Digester/Digester_2_10.coverage", "a58570f2-efb1-48c5-8873-b207c39327ab");
        if (saxLog.isDebugEnabled()) {
            writeline("/home/ubuntu/results/coverage/Digester/Digester_2_10.coverage", "aca186d3-6cc7-4cc5-8936-cd4dd559095b");
            saxLog.debug("startElement(" + namespaceURI + "," + localName + "," + qName + ")");
        }
        writeline("/home/ubuntu/results/coverage/Digester/Digester_2_10.coverage", "9ab83ef4-7fab-47a2-9d3e-ed12a11335bd");
        bodyTexts.push(bodyText);
        writeline("/home/ubuntu/results/coverage/Digester/Digester_2_10.coverage", "af22b7dc-e3d4-4476-85f5-9fcf2cbda083");
        if (debug) {
            writeline("/home/ubuntu/results/coverage/Digester/Digester_2_10.coverage", "de24634c-090a-4b6c-b87b-e31038fe2547");
            log.debug("  Pushing body text '" + bodyText.toString() + "'");
        }
        writeline("/home/ubuntu/results/coverage/Digester/Digester_2_10.coverage", "eed63004-5851-4a7b-9afc-9e10f043764f");
        bodyText = new StringBuilder();
        writeline("/home/ubuntu/results/coverage/Digester/Digester_2_10.coverage", "ccadf8e1-72c8-497d-8723-7db57c3b661b");
        String name = localName;
        writeline("/home/ubuntu/results/coverage/Digester/Digester_2_10.coverage", "60a45d5b-d0f9-45b0-9790-47847b056364");
        if ((name == null) || (name.length() < 1)) {
            writeline("/home/ubuntu/results/coverage/Digester/Digester_2_10.coverage", "e9cece33-6fc1-460a-850a-616fea7d3df0");
            name = qName;
        }
        writeline("/home/ubuntu/results/coverage/Digester/Digester_2_10.coverage", "afa4ca65-2715-4094-bb5e-e89fe0ab9ceb");
        StringBuilder sb = new StringBuilder(match);
        writeline("/home/ubuntu/results/coverage/Digester/Digester_2_10.coverage", "f770a41e-50f4-4c92-91b8-e6e1aa22b943");
        if (match.length() > 0) {
            writeline("/home/ubuntu/results/coverage/Digester/Digester_2_10.coverage", "d73ddbbe-7ea5-4302-b239-d8c262f0a96c");
            sb.append('/');
        }
        writeline("/home/ubuntu/results/coverage/Digester/Digester_2_10.coverage", "957293f7-36a1-4efd-92d5-672ae3aac376");
        sb.append(name);
        writeline("/home/ubuntu/results/coverage/Digester/Digester_2_10.coverage", "4738dda7-9d08-428f-9120-e0b2530bfe71");
        match = sb.toString();
        writeline("/home/ubuntu/results/coverage/Digester/Digester_2_10.coverage", "7e2a853d-82db-461d-923b-017e4e3532d5");
        if (debug) {
            writeline("/home/ubuntu/results/coverage/Digester/Digester_2_10.coverage", "9edbf559-7e0c-4426-8bd7-268046f87ea0");
            log.debug("  New match='" + match + "'");
        }
        writeline("/home/ubuntu/results/coverage/Digester/Digester_2_10.coverage", "4f09946a-b21f-4194-8829-8b16183042a2");
        List<Rule> rules = getRules().match(namespaceURI, match, localName, list);
        writeline("/home/ubuntu/results/coverage/Digester/Digester_2_10.coverage", "87df9c47-3641-4746-a5bd-44f78384526a");
        matches.push(rules);
        writeline("/home/ubuntu/results/coverage/Digester/Digester_2_10.coverage", "adc08b30-8e9c-42b5-a268-d86bafc6ff82");
        if ((rules != null) && (rules.size() > 0)) {
            writeline("/home/ubuntu/results/coverage/Digester/Digester_2_10.coverage", "7f5016aa-a124-42dd-9db5-1b342bcd9c0a");
            Substitutor substitutor = getSubstitutor();
            writeline("/home/ubuntu/results/coverage/Digester/Digester_2_10.coverage", "a0f3dedd-92ae-456a-914b-81e89f38fcbc");
            if (substitutor != null) {
                writeline("/home/ubuntu/results/coverage/Digester/Digester_2_10.coverage", "8558e739-5dec-4dd1-81aa-15ea0466bc14");
                list = substitutor.substitute(list);
            }
            writeline("/home/ubuntu/results/coverage/Digester/Digester_2_10.coverage", "c35d6343-bdca-4e58-bce2-0b8145639f7d");
            for (int i = 0; i < rules.size(); i++) {
                writeline("/home/ubuntu/results/coverage/Digester/Digester_2_10.coverage", "e4b13fc3-6b3f-4d26-95b0-098f64bfca7c");
                try {
                    writeline("/home/ubuntu/results/coverage/Digester/Digester_2_10.coverage", "b4cd35a7-9c78-452f-b9e7-5a554ef1ec16");
                    Rule rule = rules.get(i);
                    writeline("/home/ubuntu/results/coverage/Digester/Digester_2_10.coverage", "b21c7642-56d5-46d8-a396-c45e1bf677ff");
                    if (debug) {
                        writeline("/home/ubuntu/results/coverage/Digester/Digester_2_10.coverage", "8617a578-23d2-466d-ad08-78b307928887");
                        log.debug("  Fire begin() for " + rule);
                    }
                    writeline("/home/ubuntu/results/coverage/Digester/Digester_2_10.coverage", "bc04463a-3449-40ad-8a94-3da112d61d53");
                    rule.begin(namespaceURI, name, list);
                } catch (Exception e) {
                    writeline("/home/ubuntu/results/coverage/Digester/Digester_2_10.coverage", "bbd66c9f-b51f-45f1-adfb-b7db13d48c6b");
                    log.error("Begin event threw exception", e);
                    writeline("/home/ubuntu/results/coverage/Digester/Digester_2_10.coverage", "fc8a84e0-45ca-422d-a5ac-0b7daaf162d2");
                    throw createSAXException(e);
                } catch (Error e) {
                    writeline("/home/ubuntu/results/coverage/Digester/Digester_2_10.coverage", "2174f370-7993-4d8c-9dde-b1a263d80ea1");
                    log.error("Begin event threw error", e);
                    writeline("/home/ubuntu/results/coverage/Digester/Digester_2_10.coverage", "f3376572-3a46-4ddd-a5ef-c9ef3fa8eb11");
                    throw e;
                }
            }
        } else {
            writeline("/home/ubuntu/results/coverage/Digester/Digester_2_10.coverage", "911ca85f-2d55-4bd6-a86e-b3369e702b6a");
            if (debug) {
                writeline("/home/ubuntu/results/coverage/Digester/Digester_2_10.coverage", "7b4d24fc-b51e-4507-a3ff-e827f56c3cf0");
                log.debug("  No rules found matching '" + match + "'.");
            }
        }
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public void startPrefixMapping(String prefix, String namespaceURI) throws SAXException {
        writeline("/home/ubuntu/results/coverage/Digester/Digester_2_10.coverage", "0efeae94-9439-428f-ac50-66e8b9efcdda");
        if (saxLog.isDebugEnabled()) {
            writeline("/home/ubuntu/results/coverage/Digester/Digester_2_10.coverage", "403e6d3a-74d2-4ed4-ad86-c13359e5c1f1");
            saxLog.debug("startPrefixMapping(" + prefix + "," + namespaceURI + ")");
        }
        writeline("/home/ubuntu/results/coverage/Digester/Digester_2_10.coverage", "b9731975-b0ca-46fe-9b34-4bdb092fd58a");
        Stack<String> stack = namespaces.get(prefix);
        writeline("/home/ubuntu/results/coverage/Digester/Digester_2_10.coverage", "84a348b4-f9de-4c09-824d-d791f9c238bb");
        if (stack == null) {
            writeline("/home/ubuntu/results/coverage/Digester/Digester_2_10.coverage", "3711cbcd-39a8-4fda-98ea-782ba34752d4");
            stack = new Stack<String>();
            writeline("/home/ubuntu/results/coverage/Digester/Digester_2_10.coverage", "2badf0e3-8858-4896-a494-8eac064d3168");
            namespaces.put(prefix, stack);
        }
        writeline("/home/ubuntu/results/coverage/Digester/Digester_2_10.coverage", "003d5609-9742-47af-935c-623bed64f484");
        stack.push(namespaceURI);
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public void notationDecl(String name, String publicId, String systemId) {
        writeline("/home/ubuntu/results/coverage/Digester/Digester_2_10.coverage", "01afea55-59f2-489d-9c61-a1ff3c4e3694");
        if (saxLog.isDebugEnabled()) {
            writeline("/home/ubuntu/results/coverage/Digester/Digester_2_10.coverage", "a39e3c23-8b31-4a44-9c33-1202a43a2716");
            saxLog.debug("notationDecl(" + name + "," + publicId + "," + systemId + ")");
        }
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public void unparsedEntityDecl(String name, String publicId, String systemId, String notation) {
        writeline("/home/ubuntu/results/coverage/Digester/Digester_2_10.coverage", "3a2f4d40-648f-49b7-a272-a812751ac2e3");
        if (saxLog.isDebugEnabled()) {
            writeline("/home/ubuntu/results/coverage/Digester/Digester_2_10.coverage", "b133ae56-f84e-46d4-8630-a448d1488d57");
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
        writeline("/home/ubuntu/results/coverage/Digester/Digester_2_10.coverage", "9ebeb532-7ff4-441b-b079-7a87cfacb098");
        this.entityResolver = entityResolver;
    }

    /**
     * Return the Entity Resolver used by the SAX parser.
     *
     * @return the Entity Resolver used by the SAX parser.
     */
    public EntityResolver getEntityResolver() {
        writeline("/home/ubuntu/results/coverage/Digester/Digester_2_10.coverage", "f4cf0c79-4eba-4b8d-b9e5-4084c6a4e86b");
        return entityResolver;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public InputSource resolveEntity(String publicId, String systemId) throws SAXException {
        writeline("/home/ubuntu/results/coverage/Digester/Digester_2_10.coverage", "5d8c29c2-c411-49be-b944-698722d66346");
        if (saxLog.isDebugEnabled()) {
            writeline("/home/ubuntu/results/coverage/Digester/Digester_2_10.coverage", "6f0b70d1-8047-451f-a502-1cd221681582");
            saxLog.debug("resolveEntity('" + publicId + "', '" + systemId + "')");
        }
        writeline("/home/ubuntu/results/coverage/Digester/Digester_2_10.coverage", "454b7434-c1b6-4317-9d3b-8f1bed3fc719");
        if (publicId != null) {
            writeline("/home/ubuntu/results/coverage/Digester/Digester_2_10.coverage", "268ef4f1-8305-4245-8892-7cc3d82437ad");
            this.publicId = publicId;
        }
        writeline("/home/ubuntu/results/coverage/Digester/Digester_2_10.coverage", "ca04f055-88eb-4605-b6a1-ad6ff87a93dc");
        URL entityURL = null;
        writeline("/home/ubuntu/results/coverage/Digester/Digester_2_10.coverage", "c894f7ad-906d-46b6-abe6-c5e9de66e574");
        if (publicId != null) {
            writeline("/home/ubuntu/results/coverage/Digester/Digester_2_10.coverage", "a571a1f2-c14a-4a21-a139-b27ac90a02f0");
            entityURL = entityValidator.get(publicId);
        }
        writeline("/home/ubuntu/results/coverage/Digester/Digester_2_10.coverage", "16ec8061-c0c5-439b-9f9d-d363beadadf5");
        if (entityURL == null && systemId != null) {
            writeline("/home/ubuntu/results/coverage/Digester/Digester_2_10.coverage", "ba455951-d834-4a95-b582-ce241cb16fa6");
            entityURL = entityValidator.get(systemId);
        }
        writeline("/home/ubuntu/results/coverage/Digester/Digester_2_10.coverage", "f6778100-4301-489b-8513-bed4e26e9824");
        if (entityURL == null) {
            writeline("/home/ubuntu/results/coverage/Digester/Digester_2_10.coverage", "0a2ac1e5-77ac-4d18-9319-aa5310510e9f");
            if (systemId == null) {
                writeline("/home/ubuntu/results/coverage/Digester/Digester_2_10.coverage", "1a5eeea7-699d-4990-b158-c414f6d7056b");
                if (log.isDebugEnabled()) {
                    writeline("/home/ubuntu/results/coverage/Digester/Digester_2_10.coverage", "446d6b2c-5fa4-46a8-9db0-810dae67f3f3");
                    log.debug(" Cannot resolve null entity, returning null InputSource");
                }
                writeline("/home/ubuntu/results/coverage/Digester/Digester_2_10.coverage", "b28c43a7-457a-4f12-9e68-0ef5da5cf481");
                return (null);
            }
            writeline("/home/ubuntu/results/coverage/Digester/Digester_2_10.coverage", "0ccd3383-cb3d-4889-ba52-266df5946666");
            if (log.isDebugEnabled()) {
                writeline("/home/ubuntu/results/coverage/Digester/Digester_2_10.coverage", "dddb2e01-0aef-426e-b77d-f315b67bbab4");
                log.debug(" Trying to resolve using system ID '" + systemId + "'");
            }
            writeline("/home/ubuntu/results/coverage/Digester/Digester_2_10.coverage", "6a9db739-b16d-4cc4-b1c6-cbba3ed16277");
            try {
                writeline("/home/ubuntu/results/coverage/Digester/Digester_2_10.coverage", "1b7b3731-0277-4c58-9654-340a9a410ea3");
                entityURL = new URL(systemId);
            } catch (MalformedURLException e) {
                writeline("/home/ubuntu/results/coverage/Digester/Digester_2_10.coverage", "518e2c53-859c-4cfa-876a-5403630c6865");
                throw new IllegalArgumentException("Malformed URL '" + systemId + "' : " + e.getMessage());
            }
        }
        writeline("/home/ubuntu/results/coverage/Digester/Digester_2_10.coverage", "c6404e75-5d94-4d3c-8320-e3a81c51d69b");
        if (log.isDebugEnabled()) {
            writeline("/home/ubuntu/results/coverage/Digester/Digester_2_10.coverage", "e59b0f47-ddfd-4eeb-b706-d70d927edffc");
            log.debug(" Resolving to alternate DTD '" + entityURL + "'");
        }
        writeline("/home/ubuntu/results/coverage/Digester/Digester_2_10.coverage", "7755a5c4-d507-4c6d-a915-3ed2398de567");
        try {
            writeline("/home/ubuntu/results/coverage/Digester/Digester_2_10.coverage", "ae4ba718-4f96-42c6-a4e7-ff37089df530");
            return createInputSourceFromURL(entityURL);
        } catch (Exception e) {
            writeline("/home/ubuntu/results/coverage/Digester/Digester_2_10.coverage", "3cb4bf47-90d4-4ff8-a672-41175a993762");
            throw createSAXException(e);
        }
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public void error(SAXParseException exception) throws SAXException {
        writeline("/home/ubuntu/results/coverage/Digester/Digester_2_10.coverage", "d8020c99-62d1-42d2-9f08-a7315e1e6aae");
        log.error("Parse Error at line " + exception.getLineNumber() + " column " + exception.getColumnNumber() + ": " + exception.getMessage(), exception);
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public void fatalError(SAXParseException exception) throws SAXException {
        writeline("/home/ubuntu/results/coverage/Digester/Digester_2_10.coverage", "3f61e109-af0c-42d8-8e7b-43f15f7d6540");
        log.error("Parse Fatal Error at line " + exception.getLineNumber() + " column " + exception.getColumnNumber() + ": " + exception.getMessage(), exception);
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public void warning(SAXParseException exception) throws SAXException {
        writeline("/home/ubuntu/results/coverage/Digester/Digester_2_10.coverage", "b4f166cf-f7c7-4635-bc3d-459f1b1d3a4f");
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
        writeline("/home/ubuntu/results/coverage/Digester/Digester_2_10.coverage", "aa57645a-4fde-4660-9594-3268ae114279");
        if (file == null) {
            writeline("/home/ubuntu/results/coverage/Digester/Digester_2_10.coverage", "3be551e1-ea9a-4741-9df4-8933a3117b82");
            throw new IllegalArgumentException("File to parse is null");
        }
        writeline("/home/ubuntu/results/coverage/Digester/Digester_2_10.coverage", "d8181972-d3f8-405b-b681-63334e8d7b63");
        InputSource input = new InputSource(new FileInputStream(file));
        writeline("/home/ubuntu/results/coverage/Digester/Digester_2_10.coverage", "ecbb4e67-e6a7-42d3-9a0c-3cd4b65f5537");
        input.setSystemId(file.toURI().toURL().toString());
        writeline("/home/ubuntu/results/coverage/Digester/Digester_2_10.coverage", "1e1d5d03-eaba-47f6-bb80-f4d24ba9a64e");
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
        writeline("/home/ubuntu/results/coverage/Digester/Digester_2_10.coverage", "862e73fd-a164-45e9-9afe-e8866f868f13");
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
        writeline("/home/ubuntu/results/coverage/Digester/Digester_2_10.coverage", "074c2b30-8da4-4b29-8827-8dc7ccadd3cf");
        if (input == null) {
            writeline("/home/ubuntu/results/coverage/Digester/Digester_2_10.coverage", "f54c931b-e23e-46be-be12-9ac0450b99cd");
            throw new IllegalArgumentException("InputSource to parse is null");
        }
        writeline("/home/ubuntu/results/coverage/Digester/Digester_2_10.coverage", "e2b4fa45-a9aa-4bc0-ae88-a27deef688c2");
        configure();
        writeline("/home/ubuntu/results/coverage/Digester/Digester_2_10.coverage", "bea6f878-3d1d-4585-a6c2-8b92b8da7a5b");
        String systemId = input.getSystemId();
        writeline("/home/ubuntu/results/coverage/Digester/Digester_2_10.coverage", "34ac2228-ca35-454e-8a80-ec1edb3f53cb");
        if (systemId == null) {
            writeline("/home/ubuntu/results/coverage/Digester/Digester_2_10.coverage", "2d075140-85b7-4870-9342-5aedb13a6114");
            systemId = "(already loaded from stream)";
        }
        writeline("/home/ubuntu/results/coverage/Digester/Digester_2_10.coverage", "daac8a2e-d661-415f-a278-1f337db26ceb");
        try {
            writeline("/home/ubuntu/results/coverage/Digester/Digester_2_10.coverage", "6966f91f-4bde-4a16-a544-15fd1b492d8b");
            getXMLReader().parse(input);
        } catch (IOException e) {
            writeline("/home/ubuntu/results/coverage/Digester/Digester_2_10.coverage", "8cc487b6-f09a-4b06-b0c1-c8cf44b3ab8b");
            log.error(format("An error occurred while reading stream from '%s', see nested exceptions", systemId), e);
            writeline("/home/ubuntu/results/coverage/Digester/Digester_2_10.coverage", "c53d9bc0-22c8-4ac0-83a9-1309ea679e0b");
            throw e;
        }
        writeline("/home/ubuntu/results/coverage/Digester/Digester_2_10.coverage", "dcee921c-dbb3-4983-8bee-774a5e6c8ddc");
        cleanup();
        writeline("/home/ubuntu/results/coverage/Digester/Digester_2_10.coverage", "cf1f7308-93c2-4614-a29c-d064f11febce");
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
        writeline("/home/ubuntu/results/coverage/Digester/Digester_2_10.coverage", "f8ed71b7-a78a-473f-aa5f-9b073bd465bd");
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
        writeline("/home/ubuntu/results/coverage/Digester/Digester_2_10.coverage", "fec5cbea-1fab-4a81-b31b-c58c55d37b61");
        if (input == null) {
            writeline("/home/ubuntu/results/coverage/Digester/Digester_2_10.coverage", "052d24b5-ae03-4199-900c-b383dbd1afcf");
            throw new IllegalArgumentException("InputStream to parse is null");
        }
        writeline("/home/ubuntu/results/coverage/Digester/Digester_2_10.coverage", "e9335061-44ec-4d5f-87b9-f21011f94215");
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
        writeline("/home/ubuntu/results/coverage/Digester/Digester_2_10.coverage", "73555b74-98a2-460e-8d2f-85b3e084a25c");
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
        writeline("/home/ubuntu/results/coverage/Digester/Digester_2_10.coverage", "b8881000-d8d8-4acf-939e-e8c23c1f96a9");
        if (reader == null) {
            writeline("/home/ubuntu/results/coverage/Digester/Digester_2_10.coverage", "8f605c24-86e5-4bef-9057-ab1b4d6c2493");
            throw new IllegalArgumentException("Reader to parse is null");
        }
        writeline("/home/ubuntu/results/coverage/Digester/Digester_2_10.coverage", "ac8de9b9-f78b-412f-8d9e-ec1e5f791774");
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
        writeline("/home/ubuntu/results/coverage/Digester/Digester_2_10.coverage", "e5213288-de12-4c95-b134-1b2cf8568cef");
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
        writeline("/home/ubuntu/results/coverage/Digester/Digester_2_10.coverage", "907c7bef-ac53-4671-966f-7561f0f5dc89");
        if (uri == null) {
            writeline("/home/ubuntu/results/coverage/Digester/Digester_2_10.coverage", "48ba8c23-1f6f-4a8b-b1d2-8a08c11fcb8c");
            throw new IllegalArgumentException("String URI to parse is null");
        }
        writeline("/home/ubuntu/results/coverage/Digester/Digester_2_10.coverage", "1c011d0b-511b-4e73-80a5-0fc1a8be5e03");
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
        writeline("/home/ubuntu/results/coverage/Digester/Digester_2_10.coverage", "e27eb7c8-9e8d-4e94-a19e-efcb75fe182c");
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
        writeline("/home/ubuntu/results/coverage/Digester/Digester_2_10.coverage", "52dd13a1-a42d-4bf6-a7b6-73b9aaa2b009");
        if (url == null) {
            writeline("/home/ubuntu/results/coverage/Digester/Digester_2_10.coverage", "7e4b6ffb-5f77-418c-b79c-d570354b2c22");
            throw new IllegalArgumentException("URL to parse is null");
        }
        writeline("/home/ubuntu/results/coverage/Digester/Digester_2_10.coverage", "66ebf27c-fc1c-41b0-8271-e34a31b18e33");
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
        writeline("/home/ubuntu/results/coverage/Digester/Digester_2_10.coverage", "e0770dc8-6baf-4804-a31a-b4a1593af9d1");
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
        writeline("/home/ubuntu/results/coverage/Digester/Digester_2_10.coverage", "936bfa2b-24b0-4fcd-b41c-b431d6f1e9ff");
        if (executorService == null) {
            writeline("/home/ubuntu/results/coverage/Digester/Digester_2_10.coverage", "f1d813c1-2718-4bc3-b132-091c32028935");
            throw new IllegalStateException("ExecutorService not set");
        }
        writeline("/home/ubuntu/results/coverage/Digester/Digester_2_10.coverage", "c66efbd5-3f25-4d4d-8629-e9d455113a7e");
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
        writeline("/home/ubuntu/results/coverage/Digester/Digester_2_10.coverage", "887c39cf-6710-426b-8c20-cf891a047022");
        if (log.isDebugEnabled()) {
            writeline("/home/ubuntu/results/coverage/Digester/Digester_2_10.coverage", "f3f1bf33-dd43-426f-a4b2-d4058552848d");
            log.debug("register('" + publicId + "', '" + entityURL + "'");
        }
        writeline("/home/ubuntu/results/coverage/Digester/Digester_2_10.coverage", "fd07fdc3-f889-4611-930f-541d6e23a3a3");
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
        writeline("/home/ubuntu/results/coverage/Digester/Digester_2_10.coverage", "01d8ccc4-c65e-4b47-b106-c9c5693c1587");
        if (log.isDebugEnabled()) {
            writeline("/home/ubuntu/results/coverage/Digester/Digester_2_10.coverage", "2694b3d2-218d-4a5b-92d1-ae73f8879da5");
            log.debug("register('" + publicId + "', '" + entityURL + "'");
        }
        writeline("/home/ubuntu/results/coverage/Digester/Digester_2_10.coverage", "560c6f8e-0efc-4af1-b10a-574a0368318d");
        try {
            writeline("/home/ubuntu/results/coverage/Digester/Digester_2_10.coverage", "6f244054-72f2-4549-b3bb-2f5667d6ab93");
            entityValidator.put(publicId, new URL(entityURL));
        } catch (MalformedURLException e) {
            writeline("/home/ubuntu/results/coverage/Digester/Digester_2_10.coverage", "7cd76143-bfb7-4762-88de-486e4cb5cc3e");
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
        writeline("/home/ubuntu/results/coverage/Digester/Digester_2_10.coverage", "7a16fe58-d744-41fc-85b0-27524be3a06b");
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
        writeline("/home/ubuntu/results/coverage/Digester/Digester_2_10.coverage", "4680f161-2d13-4634-abac-b3412ef71796");
        URLConnection connection = url.openConnection();
        writeline("/home/ubuntu/results/coverage/Digester/Digester_2_10.coverage", "24497d11-60a4-41bb-9534-530ed4f679bc");
        connection.setUseCaches(false);
        writeline("/home/ubuntu/results/coverage/Digester/Digester_2_10.coverage", "c8e7d5d1-ed05-4a62-9410-943a3e2c3391");
        InputStream stream = connection.getInputStream();
        writeline("/home/ubuntu/results/coverage/Digester/Digester_2_10.coverage", "94de6d03-12c5-461c-9101-99d325183e8e");
        InputSource source = new InputSource(stream);
        writeline("/home/ubuntu/results/coverage/Digester/Digester_2_10.coverage", "6195b5c4-3031-4d28-886e-612f176a7c7a");
        source.setSystemId(url.toExternalForm());
        writeline("/home/ubuntu/results/coverage/Digester/Digester_2_10.coverage", "a869cfb9-9c77-44d1-993f-053a8e748e7c");
        inputSources.add(source);
        writeline("/home/ubuntu/results/coverage/Digester/Digester_2_10.coverage", "7608de8b-ddc3-47cb-aac8-1d928e094c80");
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
        writeline("/home/ubuntu/results/coverage/Digester/Digester_2_10.coverage", "bbcdaf3a-5e7c-4964-b973-61abdffafb9b");
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
        writeline("/home/ubuntu/results/coverage/Digester/Digester_2_10.coverage", "0d6c2f25-84da-495b-a89b-9203fa11eb15");
        rule.setDigester(this);
        writeline("/home/ubuntu/results/coverage/Digester/Digester_2_10.coverage", "2203a88c-da34-4172-a1c4-3815da98f8a1");
        getRules().add(pattern, rule);
    }

    /**
     * Register a set of Rule instances defined in a RuleSet.
     *
     * @param ruleSet The RuleSet instance to configure from
     */
    public void addRuleSet(RuleSet ruleSet) {
        writeline("/home/ubuntu/results/coverage/Digester/Digester_2_10.coverage", "0e1e6999-5de7-4546-9752-f7926bb3b483");
        String oldNamespaceURI = getRuleNamespaceURI();
        writeline("/home/ubuntu/results/coverage/Digester/Digester_2_10.coverage", "e9211925-02e5-43a9-b341-8fada9ac8d84");
        String newNamespaceURI = ruleSet.getNamespaceURI();
        writeline("/home/ubuntu/results/coverage/Digester/Digester_2_10.coverage", "ab246276-5d46-4542-8949-daf16e5604d2");
        if (log.isDebugEnabled()) {
            writeline("/home/ubuntu/results/coverage/Digester/Digester_2_10.coverage", "abc1e940-72cc-464d-b1fe-d7348cd6c29b");
            if (newNamespaceURI == null) {
                writeline("/home/ubuntu/results/coverage/Digester/Digester_2_10.coverage", "b578f281-ff7d-4fe5-a7dc-08bfcb382b40");
                log.debug("addRuleSet() with no namespace URI");
            } else {
                writeline("/home/ubuntu/results/coverage/Digester/Digester_2_10.coverage", "ff7c6707-96be-42f9-967e-c05dbf45a14f");
                log.debug("addRuleSet() with namespace URI " + newNamespaceURI);
            }
        }
        writeline("/home/ubuntu/results/coverage/Digester/Digester_2_10.coverage", "becd2a55-6c60-4787-91c5-1519d2262025");
        setRuleNamespaceURI(newNamespaceURI);
        writeline("/home/ubuntu/results/coverage/Digester/Digester_2_10.coverage", "fae956b4-7578-4dd4-8c50-351319b0289c");
        ruleSet.addRuleInstances(this);
        writeline("/home/ubuntu/results/coverage/Digester/Digester_2_10.coverage", "ca21db36-228a-4ead-b4a1-4f4581abdc34");
        setRuleNamespaceURI(oldNamespaceURI);
    }

    /**
     * Add a "bean property setter" rule for the specified parameters.
     *
     * @param pattern Element matching pattern
     * @see BeanPropertySetterRule
     */
    public void addBeanPropertySetter(String pattern) {
        writeline("/home/ubuntu/results/coverage/Digester/Digester_2_10.coverage", "7aee920a-8304-4107-a1df-153fb434919f");
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
        writeline("/home/ubuntu/results/coverage/Digester/Digester_2_10.coverage", "d3a23279-e714-4069-9a8a-93ea2093e316");
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
        writeline("/home/ubuntu/results/coverage/Digester/Digester_2_10.coverage", "f1d52e34-43dd-47b2-b476-6052b57b0746");
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
        writeline("/home/ubuntu/results/coverage/Digester/Digester_2_10.coverage", "d7839acd-6edc-4a44-b970-ce4a9c9165be");
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
        writeline("/home/ubuntu/results/coverage/Digester/Digester_2_10.coverage", "0454a8db-94a0-475d-9092-6125a914d163");
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
        writeline("/home/ubuntu/results/coverage/Digester/Digester_2_10.coverage", "f2a13863-e034-4dd3-a07c-8f8ddb0c2e01");
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
        writeline("/home/ubuntu/results/coverage/Digester/Digester_2_10.coverage", "8b4f4d4b-c281-4442-ba7d-324f0a6cb41b");
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
        writeline("/home/ubuntu/results/coverage/Digester/Digester_2_10.coverage", "f8293990-5e59-4af5-8505-3087fcadcf2c");
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
        writeline("/home/ubuntu/results/coverage/Digester/Digester_2_10.coverage", "3096f8ce-da6b-4f86-b5b4-ed17bd08b549");
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
        writeline("/home/ubuntu/results/coverage/Digester/Digester_2_10.coverage", "313b990b-dbc2-403b-9db1-5df94f075839");
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
        writeline("/home/ubuntu/results/coverage/Digester/Digester_2_10.coverage", "8a8947ec-75d9-472b-8a9c-bd1281e81e52");
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
        writeline("/home/ubuntu/results/coverage/Digester/Digester_2_10.coverage", "c7363a1e-e79e-4f43-a439-3a40996bdcfe");
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
        writeline("/home/ubuntu/results/coverage/Digester/Digester_2_10.coverage", "ca3c48ae-3978-4f75-989c-70126c93f24e");
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
        writeline("/home/ubuntu/results/coverage/Digester/Digester_2_10.coverage", "bc2ab372-2a87-481f-b6a5-26deeff8fbd7");
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
        writeline("/home/ubuntu/results/coverage/Digester/Digester_2_10.coverage", "e63f17a0-a024-47c9-8bcd-a79ded397085");
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
        writeline("/home/ubuntu/results/coverage/Digester/Digester_2_10.coverage", "e1b3cef7-c6bf-4653-b098-2bea7a68c90e");
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
        writeline("/home/ubuntu/results/coverage/Digester/Digester_2_10.coverage", "fb9a4c00-d502-4564-a9eb-e125089379b2");
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
        writeline("/home/ubuntu/results/coverage/Digester/Digester_2_10.coverage", "06f96f02-4144-4e1e-b66e-9f50002590be");
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
        writeline("/home/ubuntu/results/coverage/Digester/Digester_2_10.coverage", "f77facc3-ea9e-4125-8b93-59d988c7cd97");
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
        writeline("/home/ubuntu/results/coverage/Digester/Digester_2_10.coverage", "5c6b081d-d69d-49e9-8703-fb0837094a45");
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
        writeline("/home/ubuntu/results/coverage/Digester/Digester_2_10.coverage", "9f639643-bacb-4f90-8c79-1c55f307d628");
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
        writeline("/home/ubuntu/results/coverage/Digester/Digester_2_10.coverage", "f42a4ee6-cd0b-40ce-8d3f-ce06a120f5dd");
        creationFactory.setDigester(this);
        writeline("/home/ubuntu/results/coverage/Digester/Digester_2_10.coverage", "56171c38-3eb3-4a5e-9b3b-ad61fb7bfe23");
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
        writeline("/home/ubuntu/results/coverage/Digester/Digester_2_10.coverage", "87e082cf-b17f-4123-b72c-8deae3a31391");
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
        writeline("/home/ubuntu/results/coverage/Digester/Digester_2_10.coverage", "dd29c252-cf77-485b-96a4-3a43b4e43e7f");
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
        writeline("/home/ubuntu/results/coverage/Digester/Digester_2_10.coverage", "77faacb7-015a-4a1d-9562-9532c45974d4");
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
        writeline("/home/ubuntu/results/coverage/Digester/Digester_2_10.coverage", "1e9df43e-93ba-4833-9897-c0e77d2a8cdd");
        addRule(pattern, new ObjectCreateRule(attributeName, clazz));
    }

    /**
     * Adds an {@link SetNestedPropertiesRule}.
     *
     * @param pattern register the rule with this pattern
     * @since 1.6
     */
    public void addSetNestedProperties(String pattern) {
        writeline("/home/ubuntu/results/coverage/Digester/Digester_2_10.coverage", "0aa514af-60fe-40dc-9c56-f920cad59c9a");
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
        writeline("/home/ubuntu/results/coverage/Digester/Digester_2_10.coverage", "c202add5-ea2b-460b-842d-90771e1d6d51");
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
        writeline("/home/ubuntu/results/coverage/Digester/Digester_2_10.coverage", "488d421b-d52f-4a29-a7e4-00abfce31c5a");
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
        writeline("/home/ubuntu/results/coverage/Digester/Digester_2_10.coverage", "2e9b6c6a-9d87-46d4-8133-3dde02934c17");
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
        writeline("/home/ubuntu/results/coverage/Digester/Digester_2_10.coverage", "df6fa4d2-b738-47f0-af2f-41b0d59461cf");
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
        writeline("/home/ubuntu/results/coverage/Digester/Digester_2_10.coverage", "e5283a54-ce9f-4ea0-be2c-d8cf02690403");
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
        writeline("/home/ubuntu/results/coverage/Digester/Digester_2_10.coverage", "b7f22b58-1e79-4a2c-a20a-4e3f6557d014");
        addRule(pattern, new SetRootRule(methodName, paramType));
    }

    /**
     * Add a "set properties" rule for the specified parameters.
     *
     * @param pattern Element matching pattern
     * @see SetPropertiesRule
     */
    public void addSetProperties(String pattern) {
        writeline("/home/ubuntu/results/coverage/Digester/Digester_2_10.coverage", "2c3b0370-0ce2-49bf-8246-2a5a312d5570");
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
        writeline("/home/ubuntu/results/coverage/Digester/Digester_2_10.coverage", "2999a0a7-6145-4c22-a072-6d6fd34132e8");
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
        writeline("/home/ubuntu/results/coverage/Digester/Digester_2_10.coverage", "55d916b9-c8ae-4d61-bf76-bdff546680cb");
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
        writeline("/home/ubuntu/results/coverage/Digester/Digester_2_10.coverage", "f761ecc2-4d4d-42fd-b2da-9e6ceb62325a");
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
        writeline("/home/ubuntu/results/coverage/Digester/Digester_2_10.coverage", "d789e325-327b-406f-b9b2-8d56f5b3de88");
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
        writeline("/home/ubuntu/results/coverage/Digester/Digester_2_10.coverage", "f28ab7fc-9a1b-42fd-98ba-4f7d3e2af07f");
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
        writeline("/home/ubuntu/results/coverage/Digester/Digester_2_10.coverage", "8d1ab4bd-69ab-424a-82e2-10e9f9493570");
        match = "";
        writeline("/home/ubuntu/results/coverage/Digester/Digester_2_10.coverage", "ab623ba5-92da-4e9b-9ab3-ad279b5d394e");
        bodyTexts.clear();
        writeline("/home/ubuntu/results/coverage/Digester/Digester_2_10.coverage", "893c9411-c185-4e49-9ba7-50acfe708876");
        params.clear();
        writeline("/home/ubuntu/results/coverage/Digester/Digester_2_10.coverage", "f9ebd672-f7b0-4144-8b31-fc6866291635");
        publicId = null;
        writeline("/home/ubuntu/results/coverage/Digester/Digester_2_10.coverage", "195b8e85-15ef-4df2-8de9-f3722eb1c7ec");
        stack.clear();
        writeline("/home/ubuntu/results/coverage/Digester/Digester_2_10.coverage", "693d2755-7252-4429-8712-4f7df83623f1");
        stacksByName.clear();
        writeline("/home/ubuntu/results/coverage/Digester/Digester_2_10.coverage", "37cc1475-a698-4232-b49e-bc7835336f35");
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
        writeline("/home/ubuntu/results/coverage/Digester/Digester_2_10.coverage", "e0a38c83-e3a6-4907-9661-9c04942cbaa7");
        try {
            writeline("/home/ubuntu/results/coverage/Digester/Digester_2_10.coverage", "19ef5324-12d5-4ae9-9c0c-915cd28a500f");
            return this.<T>npeSafeCast(stack.peek());
        } catch (EmptyStackException e) {
            writeline("/home/ubuntu/results/coverage/Digester/Digester_2_10.coverage", "cfc58ae0-7835-4d05-86ba-8880e8d921bb");
            log.warn("Empty stack (returning null)");
            writeline("/home/ubuntu/results/coverage/Digester/Digester_2_10.coverage", "888b20e7-867a-4419-9d15-5b329fef4a5a");
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
        writeline("/home/ubuntu/results/coverage/Digester/Digester_2_10.coverage", "b67a64b5-58e4-43a4-8f95-7547b32ff899");
        int index = (stack.size() - 1) - n;
        writeline("/home/ubuntu/results/coverage/Digester/Digester_2_10.coverage", "44a1cd48-add9-43db-a9f7-bdd9e0493b51");
        if (index < 0) {
            writeline("/home/ubuntu/results/coverage/Digester/Digester_2_10.coverage", "9f006d17-c7e5-4ce9-acad-b04029a2fa12");
            log.warn("Empty stack (returning null)");
            writeline("/home/ubuntu/results/coverage/Digester/Digester_2_10.coverage", "708a469d-1a71-41cf-bc4d-1f3422dd9704");
            return (null);
        }
        writeline("/home/ubuntu/results/coverage/Digester/Digester_2_10.coverage", "68c2edf2-fde8-4a39-8065-f05ba9e0a4d7");
        try {
            writeline("/home/ubuntu/results/coverage/Digester/Digester_2_10.coverage", "d0a62daf-fb43-4480-b138-fea5f638e8eb");
            return this.<T>npeSafeCast(stack.get(index));
        } catch (EmptyStackException e) {
            writeline("/home/ubuntu/results/coverage/Digester/Digester_2_10.coverage", "74b2b728-ba73-4043-a2b0-9c1d5286fed7");
            log.warn("Empty stack (returning null)");
            writeline("/home/ubuntu/results/coverage/Digester/Digester_2_10.coverage", "fe701ba4-f7ac-4209-8d21-ec49d34c58c1");
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
        writeline("/home/ubuntu/results/coverage/Digester/Digester_2_10.coverage", "6e991c56-dd1a-477e-b481-a4dd38e72a3b");
        try {
            writeline("/home/ubuntu/results/coverage/Digester/Digester_2_10.coverage", "c6db7527-4291-4821-b562-550a1593bfe3");
            T popped = this.<T>npeSafeCast(stack.pop());
            writeline("/home/ubuntu/results/coverage/Digester/Digester_2_10.coverage", "4e497a41-4179-4080-941e-043935e2f4ab");
            if (stackAction != null) {
                writeline("/home/ubuntu/results/coverage/Digester/Digester_2_10.coverage", "f3986f1c-a160-4e83-bb29-23d351009e2a");
                popped = stackAction.onPop(this, null, popped);
            }
            writeline("/home/ubuntu/results/coverage/Digester/Digester_2_10.coverage", "a5c0e24b-bdc1-4d04-b4fb-3f2c35a5d8db");
            return popped;
        } catch (EmptyStackException e) {
            writeline("/home/ubuntu/results/coverage/Digester/Digester_2_10.coverage", "5ae5f7ad-ca0d-4004-930d-5a6d8124fc15");
            log.warn("Empty stack (returning null)");
            writeline("/home/ubuntu/results/coverage/Digester/Digester_2_10.coverage", "36628645-1c31-4b17-85d3-660b91a2afef");
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
        writeline("/home/ubuntu/results/coverage/Digester/Digester_2_10.coverage", "adb9a103-6823-4717-9839-927962f519b1");
        if (stackAction != null) {
            writeline("/home/ubuntu/results/coverage/Digester/Digester_2_10.coverage", "29255bc2-b63f-48fb-aea4-e72f554b4e2d");
            object = stackAction.onPush(this, null, object);
        }
        writeline("/home/ubuntu/results/coverage/Digester/Digester_2_10.coverage", "701b2d6b-de33-4c11-9521-a170bb5295a1");
        if (stack.size() == 0) {
            writeline("/home/ubuntu/results/coverage/Digester/Digester_2_10.coverage", "53a8392e-624c-4d9e-9815-7c97bb83c720");
            root = object;
        }
        writeline("/home/ubuntu/results/coverage/Digester/Digester_2_10.coverage", "651ebd2c-8637-471e-90be-7811a2889c6e");
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
        writeline("/home/ubuntu/results/coverage/Digester/Digester_2_10.coverage", "de99351f-b28e-4536-bae9-a8c50482d5f3");
        if (stackAction != null) {
            writeline("/home/ubuntu/results/coverage/Digester/Digester_2_10.coverage", "9387780b-d411-4229-ae9d-553a9bfecbec");
            value = stackAction.onPush(this, stackName, value);
        }
        writeline("/home/ubuntu/results/coverage/Digester/Digester_2_10.coverage", "9a425cc0-b32c-4ddc-bb67-2de120b56e3e");
        Stack<Object> namedStack = stacksByName.get(stackName);
        writeline("/home/ubuntu/results/coverage/Digester/Digester_2_10.coverage", "5c3e65b8-243c-4d36-8a17-db357ffd6090");
        if (namedStack == null) {
            writeline("/home/ubuntu/results/coverage/Digester/Digester_2_10.coverage", "83016c4d-377f-4ffd-8cb5-29c94084b352");
            namedStack = new Stack<Object>();
            writeline("/home/ubuntu/results/coverage/Digester/Digester_2_10.coverage", "0d083547-4cb9-424d-a2f6-e47a70d2a505");
            stacksByName.put(stackName, namedStack);
        }
        writeline("/home/ubuntu/results/coverage/Digester/Digester_2_10.coverage", "87379258-16cd-4c59-9b07-edc7b778a712");
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
        writeline("/home/ubuntu/results/coverage/Digester/Digester_2_10.coverage", "3049e64a-bd7a-4704-84ee-97551e631195");
        T result = null;
        writeline("/home/ubuntu/results/coverage/Digester/Digester_2_10.coverage", "b4271468-2762-49c8-9f61-1fe38c3b104b");
        Stack<Object> namedStack = stacksByName.get(stackName);
        writeline("/home/ubuntu/results/coverage/Digester/Digester_2_10.coverage", "71b37922-47d5-438c-9b92-8537670ef36a");
        if (namedStack == null) {
            writeline("/home/ubuntu/results/coverage/Digester/Digester_2_10.coverage", "3cb94f6e-bc7d-49ba-990a-e99089f6e16c");
            if (log.isDebugEnabled()) {
                writeline("/home/ubuntu/results/coverage/Digester/Digester_2_10.coverage", "edc54fd3-81d0-4583-a41a-2b14cbbfa3f7");
                log.debug("Stack '" + stackName + "' is empty");
            }
            writeline("/home/ubuntu/results/coverage/Digester/Digester_2_10.coverage", "9ca6b4b0-4d05-4295-a422-a0b055dc5ec9");
            throw new EmptyStackException();
        }
        writeline("/home/ubuntu/results/coverage/Digester/Digester_2_10.coverage", "c6f17307-65e8-4f30-8892-c208b70c6f11");
        result = this.<T>npeSafeCast(namedStack.pop());
        writeline("/home/ubuntu/results/coverage/Digester/Digester_2_10.coverage", "361f6fb1-591a-4169-aa94-c8457ef51cd5");
        if (stackAction != null) {
            writeline("/home/ubuntu/results/coverage/Digester/Digester_2_10.coverage", "b8fec09d-f129-45fa-b731-daebbcc3baf7");
            result = stackAction.onPop(this, stackName, result);
        }
        writeline("/home/ubuntu/results/coverage/Digester/Digester_2_10.coverage", "1e3c127a-9dae-4cd1-b1ce-6158d88f5bae");
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
        writeline("/home/ubuntu/results/coverage/Digester/Digester_2_10.coverage", "08f0a8af-0a6c-4118-994e-3436980af3e6");
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
        writeline("/home/ubuntu/results/coverage/Digester/Digester_2_10.coverage", "81cb8571-01ac-4aab-8e87-7e8c987ddab2");
        T result = null;
        writeline("/home/ubuntu/results/coverage/Digester/Digester_2_10.coverage", "dd2494a7-31b9-49c4-bd8c-cbdac0cfd720");
        Stack<Object> namedStack = stacksByName.get(stackName);
        writeline("/home/ubuntu/results/coverage/Digester/Digester_2_10.coverage", "c3fe94be-7ab3-4e01-b5a5-d501c2471dcd");
        if (namedStack == null) {
            writeline("/home/ubuntu/results/coverage/Digester/Digester_2_10.coverage", "69ffee1f-dee8-47fe-ad07-5407b9a52537");
            if (log.isDebugEnabled()) {
                writeline("/home/ubuntu/results/coverage/Digester/Digester_2_10.coverage", "f7bf2a2f-8961-4a5b-8f82-2f617f19f8bf");
                log.debug("Stack '" + stackName + "' is empty");
            }
            writeline("/home/ubuntu/results/coverage/Digester/Digester_2_10.coverage", "84b1a6ed-6559-4359-a972-f7425de70389");
            throw new EmptyStackException();
        }
        writeline("/home/ubuntu/results/coverage/Digester/Digester_2_10.coverage", "90a40513-eebb-4c9b-88ef-dd7cdf21c1c2");
        int index = (namedStack.size() - 1) - n;
        writeline("/home/ubuntu/results/coverage/Digester/Digester_2_10.coverage", "a20bd254-c910-454f-b94e-2a34aaab8ed1");
        if (index < 0) {
            writeline("/home/ubuntu/results/coverage/Digester/Digester_2_10.coverage", "07c20688-2049-4806-933a-23f2901cfdda");
            throw new EmptyStackException();
        }
        writeline("/home/ubuntu/results/coverage/Digester/Digester_2_10.coverage", "a22b9423-7cd6-4209-a55d-17e133ee982f");
        result = this.<T>npeSafeCast(namedStack.get(index));
        writeline("/home/ubuntu/results/coverage/Digester/Digester_2_10.coverage", "152da09f-1a6f-4439-9e22-54e50b457ab7");
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
        writeline("/home/ubuntu/results/coverage/Digester/Digester_2_10.coverage", "dce59dd7-0398-4cc4-ac14-b1ecf19e2870");
        boolean result = true;
        writeline("/home/ubuntu/results/coverage/Digester/Digester_2_10.coverage", "5fe9f4bf-6a20-4ca9-ba13-72b0ad2c0e97");
        Stack<Object> namedStack = stacksByName.get(stackName);
        writeline("/home/ubuntu/results/coverage/Digester/Digester_2_10.coverage", "f67cbd73-aae5-4080-ae8d-ba7f1eddce99");
        if (namedStack != null) {
            writeline("/home/ubuntu/results/coverage/Digester/Digester_2_10.coverage", "42bf5b56-0654-4379-8f22-6a6814a6a275");
            result = namedStack.isEmpty();
        }
        writeline("/home/ubuntu/results/coverage/Digester/Digester_2_10.coverage", "27b0a105-205a-4946-853f-3db766c9ce8d");
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
        writeline("/home/ubuntu/results/coverage/Digester/Digester_2_10.coverage", "a1613d35-1ebf-4361-b59f-10b11adc41cd");
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
        writeline("/home/ubuntu/results/coverage/Digester/Digester_2_10.coverage", "3eb55ca8-4a44-41d4-9248-e0929e464bf2");
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
        writeline("/home/ubuntu/results/coverage/Digester/Digester_2_10.coverage", "9db70116-0e97-48e9-aec4-a90121791831");
        for (InputSource source : inputSources) {
            writeline("/home/ubuntu/results/coverage/Digester/Digester_2_10.coverage", "b1099562-6825-41fe-b7ce-83d054307799");
            try {
                writeline("/home/ubuntu/results/coverage/Digester/Digester_2_10.coverage", "442d1da8-fb52-4ff3-9277-a08fdeae19dd");
                source.getByteStream().close();
            } catch (IOException e) {
                writeline("/home/ubuntu/results/coverage/Digester/Digester_2_10.coverage", "656c03ca-5ece-4ac9-b961-0a196d899797");
                if (log.isWarnEnabled()) {
                    writeline("/home/ubuntu/results/coverage/Digester/Digester_2_10.coverage", "1eb70c27-ecb3-4289-b4d3-6465e489664d");
                    log.warn(format("An error occurred while closing resource %s (%s)", source.getPublicId(), source.getSystemId()), e);
                }
            }
        }
        writeline("/home/ubuntu/results/coverage/Digester/Digester_2_10.coverage", "44ea4bc8-266c-4fb7-b87c-a3c2c43db7b5");
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
        writeline("/home/ubuntu/results/coverage/Digester/Digester_2_10.coverage", "c1fa2896-8351-4c1c-821e-68f98cbb2ebf");
        if (configured) {
            writeline("/home/ubuntu/results/coverage/Digester/Digester_2_10.coverage", "3dadaed5-f623-499f-9db7-fcacf12c9605");
            return;
        }
        writeline("/home/ubuntu/results/coverage/Digester/Digester_2_10.coverage", "a511085a-c971-490b-98b7-d788da6e2e2b");
        initialize();
        writeline("/home/ubuntu/results/coverage/Digester/Digester_2_10.coverage", "501d2bf3-2edf-4ac5-95f7-883283117f2f");
        configured = true;
    }

    /**
     * Checks the Digester instance has been configured.
     *
     * @return true, if the Digester instance has been configured, false otherwise
     * @since 3.0
     */
    public boolean isConfigured() {
        writeline("/home/ubuntu/results/coverage/Digester/Digester_2_10.coverage", "ac9d8070-c664-4a6c-a68b-d145e010e1ae");
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
        writeline("/home/ubuntu/results/coverage/Digester/Digester_2_10.coverage", "40011235-2867-4668-8e62-23cabfe8d362");
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
        writeline("/home/ubuntu/results/coverage/Digester/Digester_2_10.coverage", "9a6c4980-2e40-499f-baba-abd13bae3aea");
        try {
            writeline("/home/ubuntu/results/coverage/Digester/Digester_2_10.coverage", "7dcc0c72-fbcc-4faa-8821-ac9c221c2ac1");
            return (params.peek());
        } catch (EmptyStackException e) {
            writeline("/home/ubuntu/results/coverage/Digester/Digester_2_10.coverage", "cc5538bc-bec7-491a-992c-407a9707b419");
            log.warn("Empty stack (returning null)");
            writeline("/home/ubuntu/results/coverage/Digester/Digester_2_10.coverage", "48293ad4-847d-4c95-99ba-32bb1e868734");
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
        writeline("/home/ubuntu/results/coverage/Digester/Digester_2_10.coverage", "e81b6ffc-1cd4-4d9e-a2c4-ea7318a641a5");
        int index = (params.size() - 1) - n;
        writeline("/home/ubuntu/results/coverage/Digester/Digester_2_10.coverage", "3abc0523-459e-4ea4-b4ac-b6605026a3cd");
        if (index < 0) {
            writeline("/home/ubuntu/results/coverage/Digester/Digester_2_10.coverage", "7080a0d7-0992-4046-af60-753dcdcccd6f");
            log.warn("Empty stack (returning null)");
            writeline("/home/ubuntu/results/coverage/Digester/Digester_2_10.coverage", "76527c13-ed82-4f70-b993-acf320882556");
            return (null);
        }
        writeline("/home/ubuntu/results/coverage/Digester/Digester_2_10.coverage", "ca9140b6-434f-4e3b-9653-1b17bfc0ed81");
        try {
            writeline("/home/ubuntu/results/coverage/Digester/Digester_2_10.coverage", "ee059cbb-8d29-4265-a85f-aa9b600d6b9c");
            return (params.get(index));
        } catch (EmptyStackException e) {
            writeline("/home/ubuntu/results/coverage/Digester/Digester_2_10.coverage", "3151048b-66ad-4fdb-8b48-9bd7bc63554b");
            log.warn("Empty stack (returning null)");
            writeline("/home/ubuntu/results/coverage/Digester/Digester_2_10.coverage", "10255d93-e577-4a8c-b7f7-9a539f11bc3e");
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
        writeline("/home/ubuntu/results/coverage/Digester/Digester_2_10.coverage", "1557fa6e-5f86-41cc-96db-63fa7fb978c1");
        try {
            writeline("/home/ubuntu/results/coverage/Digester/Digester_2_10.coverage", "caf23b2c-2caa-4860-81b9-ee86a9e24a3f");
            if (log.isTraceEnabled()) {
                writeline("/home/ubuntu/results/coverage/Digester/Digester_2_10.coverage", "6388bcd1-23fd-4fe7-ac2c-255cff936301");
                log.trace("Popping params");
            }
            writeline("/home/ubuntu/results/coverage/Digester/Digester_2_10.coverage", "8b927e99-3e7d-4f9a-83c8-ccf91895a58e");
            return (params.pop());
        } catch (EmptyStackException e) {
            writeline("/home/ubuntu/results/coverage/Digester/Digester_2_10.coverage", "89394443-ceee-47ea-acc0-cf221546b921");
            log.warn("Empty stack (returning null)");
            writeline("/home/ubuntu/results/coverage/Digester/Digester_2_10.coverage", "faca52e5-796a-4ede-8b27-3891ee9ec571");
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
        writeline("/home/ubuntu/results/coverage/Digester/Digester_2_10.coverage", "f6c15a8e-5b83-4f6a-98c2-44ddd457435e");
        if (log.isTraceEnabled()) {
            writeline("/home/ubuntu/results/coverage/Digester/Digester_2_10.coverage", "7575f150-76af-4d12-b78c-5a8b583caa3a");
            log.trace("Pushing params");
        }
        writeline("/home/ubuntu/results/coverage/Digester/Digester_2_10.coverage", "26b63173-a665-49fe-9fe1-c050fd20add3");
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
        writeline("/home/ubuntu/results/coverage/Digester/Digester_2_10.coverage", "ed34434b-5b7e-4615-ba1e-b255ae7c7a8c");
        if ((e != null) && (e instanceof InvocationTargetException)) {
            writeline("/home/ubuntu/results/coverage/Digester/Digester_2_10.coverage", "404184a6-aa0f-4d15-9dbc-01a27d6d9d16");
            Throwable t = ((InvocationTargetException) e).getTargetException();
            writeline("/home/ubuntu/results/coverage/Digester/Digester_2_10.coverage", "20335f67-3c95-49ef-aa65-b9d7a56dfee8");
            if ((t != null) && (t instanceof Exception)) {
                writeline("/home/ubuntu/results/coverage/Digester/Digester_2_10.coverage", "27139876-55a6-4ec2-9fdd-dba671413bd6");
                e = (Exception) t;
            }
        }
        writeline("/home/ubuntu/results/coverage/Digester/Digester_2_10.coverage", "1c3e2123-4b7f-4ab6-bd23-21a71af45216");
        if (locator != null) {
            writeline("/home/ubuntu/results/coverage/Digester/Digester_2_10.coverage", "a098fd12-fb58-4f1e-ad6d-7d1cfb7bfbdd");
            String error = "Error at line " + locator.getLineNumber() + " char " + locator.getColumnNumber() + ": " + message;
            writeline("/home/ubuntu/results/coverage/Digester/Digester_2_10.coverage", "5a960f4d-b6c3-4de1-903c-bbc04b86baac");
            if (e != null) {
                writeline("/home/ubuntu/results/coverage/Digester/Digester_2_10.coverage", "e6826652-fbad-4aa9-b0d9-f01aee51f7bd");
                return new SAXParseException(error, locator, e);
            }
            writeline("/home/ubuntu/results/coverage/Digester/Digester_2_10.coverage", "7e3ad8ef-40ab-4b35-a282-85c8658fa5e8");
            return new SAXParseException(error, locator);
        }
        writeline("/home/ubuntu/results/coverage/Digester/Digester_2_10.coverage", "5b27e75a-1b46-4584-a51d-a4f22d35dcb2");
        log.error("No Locator!");
        writeline("/home/ubuntu/results/coverage/Digester/Digester_2_10.coverage", "a488065b-c165-4fb0-ba22-c596a4f55b7d");
        if (e != null) {
            writeline("/home/ubuntu/results/coverage/Digester/Digester_2_10.coverage", "84488e10-6914-41e3-8798-41aba312a994");
            return new SAXException(message, e);
        }
        writeline("/home/ubuntu/results/coverage/Digester/Digester_2_10.coverage", "fc379dff-1d07-4c9e-b99a-3126c91dc83e");
        return new SAXException(message);
    }

    /**
     * Create a SAX exception which also understands about the location in the digester file where the exception occurs
     *
     * @param e the exception cause
     * @return the new SAX exception
     */
    public SAXException createSAXException(Exception e) {
        writeline("/home/ubuntu/results/coverage/Digester/Digester_2_10.coverage", "aa9de7ef-e7da-44a3-8a72-775c5f7d6290");
        if (e instanceof InvocationTargetException) {
            writeline("/home/ubuntu/results/coverage/Digester/Digester_2_10.coverage", "4b00533a-6e6a-4235-a473-27de9a0ff850");
            Throwable t = ((InvocationTargetException) e).getTargetException();
            writeline("/home/ubuntu/results/coverage/Digester/Digester_2_10.coverage", "8b1efbaa-43dd-4ca3-aeee-dac50657be36");
            if ((t != null) && (t instanceof Exception)) {
                writeline("/home/ubuntu/results/coverage/Digester/Digester_2_10.coverage", "8b90046e-2cbf-4f00-a85e-a1f11f47931b");
                e = (Exception) t;
            }
        }
        writeline("/home/ubuntu/results/coverage/Digester/Digester_2_10.coverage", "62f91844-b8b7-470a-81fb-83b5123dc006");
        return createSAXException(e.getMessage(), e);
    }

    /**
     * Create a SAX exception which also understands about the location in the digester file where the exception occurs
     *
     * @param message the custom SAX exception message
     * @return the new SAX exception
     */
    public SAXException createSAXException(String message) {
        writeline("/home/ubuntu/results/coverage/Digester/Digester_2_10.coverage", "2727abaf-1df5-4e3c-8440-b16df833e489");
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
        writeline("/home/ubuntu/results/coverage/Digester/Digester_2_10.coverage", "f5c25d97-e8ba-4fee-aea9-621b0083447a");
        if (obj == null) {
            writeline("/home/ubuntu/results/coverage/Digester/Digester_2_10.coverage", "7c4988e1-2afb-430c-9072-0be28154141a");
            return null;
        }
        writeline("/home/ubuntu/results/coverage/Digester/Digester_2_10.coverage", "45fd10a6-2872-4873-b09c-d4e44357a306");
        @SuppressWarnings("unchecked") T result = (T) obj;
        writeline("/home/ubuntu/results/coverage/Digester/Digester_2_10.coverage", "7340e3c9-2278-44f7-becd-25759b02be88");
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
