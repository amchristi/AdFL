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
        writeline("/home/ubuntu/results/coverage/Digester/Digester_5_10.coverage", "eba9d5c4-87c2-4d91-960e-b1ebe98a76d0");
        Stack<String> nsStack = namespaces.get(prefix);
        writeline("/home/ubuntu/results/coverage/Digester/Digester_5_10.coverage", "285fd713-d406-4c38-b9e6-06717ad8d533");
        if (nsStack == null) {
            writeline("/home/ubuntu/results/coverage/Digester/Digester_5_10.coverage", "d58b8884-e6fe-404b-ba73-6e2bece96745");
            return null;
        }
        writeline("/home/ubuntu/results/coverage/Digester/Digester_5_10.coverage", "e6ea4656-a76f-45bf-9ca1-b800984759b4");
        try {
            writeline("/home/ubuntu/results/coverage/Digester/Digester_5_10.coverage", "0dd45abc-0ffd-48b3-836b-edd641803f10");
            return (nsStack.peek());
        } catch (EmptyStackException e) {
            writeline("/home/ubuntu/results/coverage/Digester/Digester_5_10.coverage", "30f8880a-f352-42c6-a424-b56bdc76845f");
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
        writeline("/home/ubuntu/results/coverage/Digester/Digester_5_10.coverage", "e38ec482-e486-47cb-a499-938c777ddd47");
        if (this.classLoader != null) {
            writeline("/home/ubuntu/results/coverage/Digester/Digester_5_10.coverage", "5a7c3b5f-ae32-4490-bc20-da711ca67116");
            return (this.classLoader);
        }
        writeline("/home/ubuntu/results/coverage/Digester/Digester_5_10.coverage", "316ca29b-6659-46df-9085-f63383573bf2");
        if (this.useContextClassLoader) {
            writeline("/home/ubuntu/results/coverage/Digester/Digester_5_10.coverage", "39f24854-e847-48df-8e79-c7b0e7df354b");
            ClassLoader classLoader = Thread.currentThread().getContextClassLoader();
            writeline("/home/ubuntu/results/coverage/Digester/Digester_5_10.coverage", "b1d2671c-d108-45e8-8fb8-70c15d77092f");
            if (classLoader != null) {
                writeline("/home/ubuntu/results/coverage/Digester/Digester_5_10.coverage", "eadba76b-8909-4817-9621-2eefb363ef18");
                return (classLoader);
            }
        }
        writeline("/home/ubuntu/results/coverage/Digester/Digester_5_10.coverage", "7520430a-7f97-4188-ba5b-aa10dd4d126c");
        return (this.getClass().getClassLoader());
    }

    /**
     * Set the class loader to be used for instantiating application objects when required.
     *
     * @param classLoader The new class loader to use, or <code>null</code> to revert to the standard rules
     */
    public void setClassLoader(ClassLoader classLoader) {
        writeline("/home/ubuntu/results/coverage/Digester/Digester_5_10.coverage", "5f855883-f9d2-4b3d-87ec-1053d194a763");
        this.classLoader = classLoader;
    }

    /**
     * Return the current depth of the element stack.
     *
     * @return the current depth of the element stack.
     */
    public int getCount() {
        writeline("/home/ubuntu/results/coverage/Digester/Digester_5_10.coverage", "acb4e01f-89d3-436a-a2c7-1a429b6d42d0");
        return (stack.size());
    }

    /**
     * Return the name of the XML element that is currently being processed.
     *
     * @return the name of the XML element that is currently being processed.
     */
    public String getCurrentElementName() {
        writeline("/home/ubuntu/results/coverage/Digester/Digester_5_10.coverage", "acfc5253-2a7b-4561-b8d1-4235770c6504");
        String elementName = match;
        writeline("/home/ubuntu/results/coverage/Digester/Digester_5_10.coverage", "3ab0297c-e2d5-4c4c-9f17-cacc81a8ee43");
        int lastSlash = elementName.lastIndexOf('/');
        writeline("/home/ubuntu/results/coverage/Digester/Digester_5_10.coverage", "f15da5eb-a080-438f-bebc-7946702cd8c1");
        if (lastSlash >= 0) {
            writeline("/home/ubuntu/results/coverage/Digester/Digester_5_10.coverage", "367563a4-95bb-46e2-bc09-3c3a59981b82");
            elementName = elementName.substring(lastSlash + 1);
        }
        writeline("/home/ubuntu/results/coverage/Digester/Digester_5_10.coverage", "bd49f98d-b11d-4edb-99c9-2d47e3ae88a6");
        return (elementName);
    }

    /**
     * Return the error handler for this Digester.
     *
     * @return the error handler for this Digester.
     */
    public ErrorHandler getErrorHandler() {
        writeline("/home/ubuntu/results/coverage/Digester/Digester_5_10.coverage", "4f0204ca-669f-44e5-8f5e-392b33737737");
        return (this.errorHandler);
    }

    /**
     * Set the error handler for this Digester.
     *
     * @param errorHandler The new error handler
     */
    public void setErrorHandler(ErrorHandler errorHandler) {
        writeline("/home/ubuntu/results/coverage/Digester/Digester_5_10.coverage", "ccf0e087-3c4b-4be0-ac22-9fb45b3231e4");
        this.errorHandler = errorHandler;
    }

    /**
     * Return the SAXParserFactory we will use, creating one if necessary.
     *
     * @return the SAXParserFactory we will use, creating one if necessary.
     */
    public SAXParserFactory getFactory() {
        writeline("/home/ubuntu/results/coverage/Digester/Digester_5_10.coverage", "44785dba-93dd-41ca-9939-6bcaa21f12ac");
        if (factory == null) {
            writeline("/home/ubuntu/results/coverage/Digester/Digester_5_10.coverage", "3d17d953-601e-45f2-94b4-22644c147fdb");
            factory = SAXParserFactory.newInstance();
            writeline("/home/ubuntu/results/coverage/Digester/Digester_5_10.coverage", "4b5a3547-86e6-4e6b-bf95-ea493ee6e287");
            factory.setNamespaceAware(namespaceAware);
            writeline("/home/ubuntu/results/coverage/Digester/Digester_5_10.coverage", "f05dad76-79ca-425b-aaa4-7c74a7e0092f");
            factory.setXIncludeAware(xincludeAware);
            writeline("/home/ubuntu/results/coverage/Digester/Digester_5_10.coverage", "681930ec-a7e1-420b-a2d5-e806bdc9943b");
            factory.setValidating(validating);
            writeline("/home/ubuntu/results/coverage/Digester/Digester_5_10.coverage", "fda91baf-b12b-4a2b-b969-5ab1d8b7f1e1");
            factory.setSchema(schema);
        }
        writeline("/home/ubuntu/results/coverage/Digester/Digester_5_10.coverage", "e082e260-afee-4b8b-8de1-541a0f3b2c45");
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
        writeline("/home/ubuntu/results/coverage/Digester/Digester_5_10.coverage", "a9fe518c-2c9b-4978-970f-d5130b4fcfa0");
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
        writeline("/home/ubuntu/results/coverage/Digester/Digester_5_10.coverage", "007e2f2e-f9a8-41ec-a5a4-e0d77b3e575c");
        getFactory().setFeature(feature, value);
    }

    /**
     * Return the current Logger associated with this instance of the Digester
     *
     * @return the current Logger associated with this instance of the Digester
     */
    public Log getLogger() {
        writeline("/home/ubuntu/results/coverage/Digester/Digester_5_10.coverage", "2fea13a1-662c-4886-9477-bca0fdae7ee7");
        return log;
    }

    /**
     * Set the current logger for this Digester.
     *
     * @param log the current logger for this Digester.
     */
    public void setLogger(Log log) {
        writeline("/home/ubuntu/results/coverage/Digester/Digester_5_10.coverage", "ea2b680b-1b2e-4284-8345-5476ccbe1d83");
        this.log = log;
    }

    /**
     * Gets the logger used for logging SAX-related information. <strong>Note</strong> the output is finely grained.
     *
     * @return the logger used for logging SAX-related information
     * @since 1.6
     */
    public Log getSAXLogger() {
        writeline("/home/ubuntu/results/coverage/Digester/Digester_5_10.coverage", "5018df42-9338-4be3-b340-de60e44a4964");
        return saxLog;
    }

    /**
     * Sets the logger used for logging SAX-related information. <strong>Note</strong> the output is finely grained.
     *
     * @param saxLog the logger used for logging SAX-related information, not null
     * @since 1.6
     */
    public void setSAXLogger(Log saxLog) {
        writeline("/home/ubuntu/results/coverage/Digester/Digester_5_10.coverage", "0eace285-2337-472c-8f98-07933d72ffd1");
        this.saxLog = saxLog;
    }

    /**
     * Return the current rule match path
     *
     * @return the current rule match path
     */
    public String getMatch() {
        writeline("/home/ubuntu/results/coverage/Digester/Digester_5_10.coverage", "085ba1d7-0758-4f4c-a1a9-615b44df70dc");
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
        writeline("/home/ubuntu/results/coverage/Digester/Digester_5_10.coverage", "167a821a-5815-4a21-84f6-d243bcc896c5");
        return matches;
    }

    /**
     * Return the "namespace aware" flag for parsers we create.
     *
     * @return the "namespace aware" flag for parsers we create.
     */
    public boolean getNamespaceAware() {
        writeline("/home/ubuntu/results/coverage/Digester/Digester_5_10.coverage", "374c7a00-ae5b-49bb-852e-8babce7ffe77");
        return (this.namespaceAware);
    }

    /**
     * Set the "namespace aware" flag for parsers we create.
     *
     * @param namespaceAware The new "namespace aware" flag
     */
    public void setNamespaceAware(boolean namespaceAware) {
        writeline("/home/ubuntu/results/coverage/Digester/Digester_5_10.coverage", "cd65f62b-2732-4e26-9f63-de1c6b41446d");
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
        writeline("/home/ubuntu/results/coverage/Digester/Digester_5_10.coverage", "91327404-72a7-4394-89f0-20fc5009750c");
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
        writeline("/home/ubuntu/results/coverage/Digester/Digester_5_10.coverage", "412be2fd-385d-4bec-82bd-701f8f4d3ff4");
        this.xincludeAware = xincludeAware;
    }

    /**
     * Set the public id of the current file being parse.
     *
     * @param publicId the DTD/Schema public's id.
     */
    public void setPublicId(String publicId) {
        writeline("/home/ubuntu/results/coverage/Digester/Digester_5_10.coverage", "4323d18a-13d7-4119-a294-5f3eeb8ae413");
        this.publicId = publicId;
    }

    /**
     * Return the public identifier of the DTD we are currently parsing under, if any.
     *
     * @return the public identifier of the DTD we are currently parsing under, if any.
     */
    public String getPublicId() {
        writeline("/home/ubuntu/results/coverage/Digester/Digester_5_10.coverage", "d443860e-ee4b-40c6-95ca-6a3ef0cecc4b");
        return (this.publicId);
    }

    /**
     * Return the namespace URI that will be applied to all subsequently added <code>Rule</code> objects.
     *
     * @return the namespace URI that will be applied to all subsequently added <code>Rule</code> objects.
     */
    public String getRuleNamespaceURI() {
        writeline("/home/ubuntu/results/coverage/Digester/Digester_5_10.coverage", "6a13cd10-8453-46ef-aa1b-4d19831b98cb");
        return (getRules().getNamespaceURI());
    }

    /**
     * Set the namespace URI that will be applied to all subsequently added <code>Rule</code> objects.
     *
     * @param ruleNamespaceURI Namespace URI that must match on all subsequently added rules, or <code>null</code> for
     *            matching regardless of the current namespace URI
     */
    public void setRuleNamespaceURI(String ruleNamespaceURI) {
        writeline("/home/ubuntu/results/coverage/Digester/Digester_5_10.coverage", "7bb88067-4879-496d-b984-95635060c0db");
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
        writeline("/home/ubuntu/results/coverage/Digester/Digester_5_10.coverage", "761e1c99-c628-47b1-88c3-b09bd49e8bdb");
        if (parser != null) {
            writeline("/home/ubuntu/results/coverage/Digester/Digester_5_10.coverage", "a07dbc27-19ee-4d08-9f04-949c6afce11d");
            return (parser);
        }
        writeline("/home/ubuntu/results/coverage/Digester/Digester_5_10.coverage", "27dc9640-fe34-4d1d-9b94-bceaf4d555d2");
        try {
            writeline("/home/ubuntu/results/coverage/Digester/Digester_5_10.coverage", "8580e25a-0f8b-4179-a784-b3b4f76b8963");
            parser = getFactory().newSAXParser();
        } catch (Exception e) {
            writeline("/home/ubuntu/results/coverage/Digester/Digester_5_10.coverage", "9fce929f-2c2a-43a6-91ab-10df213678c8");
            log.error("Digester.getParser: ", e);
            writeline("/home/ubuntu/results/coverage/Digester/Digester_5_10.coverage", "99e0a780-c5ac-41d1-88d5-a93962e91bc7");
            return (null);
        }
        writeline("/home/ubuntu/results/coverage/Digester/Digester_5_10.coverage", "e9eba507-78fe-4e3b-be19-62c91f717d0b");
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
        writeline("/home/ubuntu/results/coverage/Digester/Digester_5_10.coverage", "b20826ac-9bf1-4079-beb5-63d5c2690c65");
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
        writeline("/home/ubuntu/results/coverage/Digester/Digester_5_10.coverage", "b255671f-b27c-4b60-a5ec-a72649036b38");
        getParser().setProperty(property, value);
    }

    /**
     * Return the <code>Rules</code> implementation object containing our rules collection and associated matching
     * policy. If none has been established, a default implementation will be created and returned.
     *
     * @return the <code>Rules</code> implementation object.
     */
    public Rules getRules() {
        writeline("/home/ubuntu/results/coverage/Digester/Digester_5_10.coverage", "b5a5240a-24df-4bd5-9994-511dc4ee5398");
        if (this.rules == null) {
            writeline("/home/ubuntu/results/coverage/Digester/Digester_5_10.coverage", "ace0b062-0c90-4047-bb5e-c0c02f62bc2f");
            this.rules = new RulesBase();
            writeline("/home/ubuntu/results/coverage/Digester/Digester_5_10.coverage", "f4d919da-a1c5-452c-bf5f-503e582b74fb");
            this.rules.setDigester(this);
        }
        writeline("/home/ubuntu/results/coverage/Digester/Digester_5_10.coverage", "392902e8-96d8-4d94-baac-628f6a5170ec");
        return (this.rules);
    }

    /**
     * Set the <code>Rules</code> implementation object containing our rules collection and associated matching policy.
     *
     * @param rules New Rules implementation
     */
    public void setRules(Rules rules) {
        writeline("/home/ubuntu/results/coverage/Digester/Digester_5_10.coverage", "fa410088-46ff-4655-bb04-c3b292463ef8");
        this.rules = rules;
        writeline("/home/ubuntu/results/coverage/Digester/Digester_5_10.coverage", "08ee6ce7-e292-4d16-9c32-84b96ada86e8");
        this.rules.setDigester(this);
    }

    /**
     * Return the XML Schema used when parsing.
     *
     * @return The {@link Schema} instance in use.
     * @since 2.0
     */
    public Schema getXMLSchema() {
        writeline("/home/ubuntu/results/coverage/Digester/Digester_5_10.coverage", "9508de75-ed84-48c2-ad82-557a1a27908e");
        return (this.schema);
    }

    /**
     * Set the XML Schema to be used when parsing.
     *
     * @param schema The {@link Schema} instance to use.
     * @since 2.0
     */
    public void setXMLSchema(Schema schema) {
        writeline("/home/ubuntu/results/coverage/Digester/Digester_5_10.coverage", "a29617e0-9a54-4abc-a694-54be2dc5a147");
        this.schema = schema;
    }

    /**
     * Return the boolean as to whether the context ClassLoader should be used.
     *
     * @return true, if the context ClassLoader should be used, false otherwise.
     */
    public boolean getUseContextClassLoader() {
        writeline("/home/ubuntu/results/coverage/Digester/Digester_5_10.coverage", "99a9472a-af10-44b1-a401-9a7614f67743");
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
        writeline("/home/ubuntu/results/coverage/Digester/Digester_5_10.coverage", "f3197739-0a4f-4603-96cf-e0980aa32189");
        useContextClassLoader = use;
    }

    /**
     * Return the validating parser flag.
     *
     * @return the validating parser flag.
     */
    public boolean getValidating() {
        writeline("/home/ubuntu/results/coverage/Digester/Digester_5_10.coverage", "9b15abe1-ef58-49a6-82bc-71c2a06d5295");
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
        writeline("/home/ubuntu/results/coverage/Digester/Digester_5_10.coverage", "62acc849-b6ac-4198-931b-7b44a6db32c2");
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
        writeline("/home/ubuntu/results/coverage/Digester/Digester_5_10.coverage", "e23f8c0d-98b4-4020-9365-43af04ee85cc");
        if (reader == null) {
            writeline("/home/ubuntu/results/coverage/Digester/Digester_5_10.coverage", "fd710d4b-1ed5-415d-87b6-5d847f7b000c");
            reader = getParser().getXMLReader();
        }
        writeline("/home/ubuntu/results/coverage/Digester/Digester_5_10.coverage", "bdfe11b1-e4ce-4687-9b86-6875e693a843");
        reader.setDTDHandler(this);
        writeline("/home/ubuntu/results/coverage/Digester/Digester_5_10.coverage", "b7596628-e112-46f6-8438-4bbedd042d48");
        reader.setContentHandler(this);
        writeline("/home/ubuntu/results/coverage/Digester/Digester_5_10.coverage", "e0a060e1-dc64-42a8-8ee2-446a39df3e2b");
        if (entityResolver == null) {
            writeline("/home/ubuntu/results/coverage/Digester/Digester_5_10.coverage", "c674d880-c7be-4fea-9110-895c700f6af7");
            reader.setEntityResolver(this);
        } else {
            writeline("/home/ubuntu/results/coverage/Digester/Digester_5_10.coverage", "48a773f9-ac64-40aa-abe4-bb634d4dadac");
            reader.setEntityResolver(entityResolver);
        }
        writeline("/home/ubuntu/results/coverage/Digester/Digester_5_10.coverage", "29c8c923-f288-4091-9fb6-80625a5a959d");
        if (this.errorHandler != null) {
            writeline("/home/ubuntu/results/coverage/Digester/Digester_5_10.coverage", "98d90fc0-8995-4db2-9cb0-99145cb137b7");
            reader.setErrorHandler(this.errorHandler);
        } else {
            writeline("/home/ubuntu/results/coverage/Digester/Digester_5_10.coverage", "8c08de2b-9181-4ace-9969-aa1f04780cdf");
            reader.setErrorHandler(this);
        }
        writeline("/home/ubuntu/results/coverage/Digester/Digester_5_10.coverage", "650c9bed-220b-492f-aa12-bdd3caea48b7");
        return reader;
    }

    /**
     * Gets the <code>Substitutor</code> used to convert attributes and body text.
     *
     * @return the <code>Substitutor</code> used to convert attributes and body text,
     *         null if not substitutions are to be performed.
     */
    public Substitutor getSubstitutor() {
        writeline("/home/ubuntu/results/coverage/Digester/Digester_5_10.coverage", "14096d57-502d-41fd-89d1-65eccd3b21af");
        return substitutor;
    }

    /**
     * Sets the <code>Substitutor</code> to be used to convert attributes and body text.
     *
     * @param substitutor the Substitutor to be used to convert attributes and body text or null if not substitution of
     *            these values is to be performed.
     */
    public void setSubstitutor(Substitutor substitutor) {
        writeline("/home/ubuntu/results/coverage/Digester/Digester_5_10.coverage", "204220e5-6fc2-42d0-bb3a-50b0a9c2d1d2");
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
        writeline("/home/ubuntu/results/coverage/Digester/Digester_5_10.coverage", "ffbfc6d2-49f9-4eae-b691-7058e717660e");
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
        writeline("/home/ubuntu/results/coverage/Digester/Digester_5_10.coverage", "4bed9843-4543-4bcd-934b-b1c200a95461");
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
        writeline("/home/ubuntu/results/coverage/Digester/Digester_5_10.coverage", "a1dace11-8afe-42e3-9f91-0dd993a28b9a");
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
        writeline("/home/ubuntu/results/coverage/Digester/Digester_5_10.coverage", "5cc9e369-14a2-42ba-843f-30aae59243d8");
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
        writeline("/home/ubuntu/results/coverage/Digester/Digester_5_10.coverage", "4d22b517-18b2-403d-bb0b-673935ed16b9");
        if (!namespaceAware) {
            writeline("/home/ubuntu/results/coverage/Digester/Digester_5_10.coverage", "5351024b-e420-40a3-bead-5e746a8533a9");
            log.warn("Digester is not namespace aware");
        }
        writeline("/home/ubuntu/results/coverage/Digester/Digester_5_10.coverage", "cd8095e9-8e28-4ecc-a1fa-4fadf5722bde");
        Map<String, String> currentNamespaces = new HashMap<String, String>();
        writeline("/home/ubuntu/results/coverage/Digester/Digester_5_10.coverage", "7a319fd8-a53d-439e-9ff8-5bf2b15d22f6");
        for (Map.Entry<String, Stack<String>> nsEntry : namespaces.entrySet()) {
            writeline("/home/ubuntu/results/coverage/Digester/Digester_5_10.coverage", "96dfbf94-3a47-4a0e-a44e-3eb87014b156");
            try {
                writeline("/home/ubuntu/results/coverage/Digester/Digester_5_10.coverage", "d122ca22-16f2-437d-8e02-cf8a6ad31b9c");
                currentNamespaces.put(nsEntry.getKey(), nsEntry.getValue().peek());
            } catch (RuntimeException e) {
                writeline("/home/ubuntu/results/coverage/Digester/Digester_5_10.coverage", "85aa3ec4-6bcc-46dc-bfb6-057c3ef9d684");
                log.error(e.getMessage(), e);
                writeline("/home/ubuntu/results/coverage/Digester/Digester_5_10.coverage", "429d436f-a87f-4e16-b9c9-9a8dc6720c6a");
                throw e;
            }
        }
        writeline("/home/ubuntu/results/coverage/Digester/Digester_5_10.coverage", "b9c403b8-e3df-4b08-a032-79c81f1e9506");
        return currentNamespaces;
    }

    /**
     * Returns the executor service used to run asynchronous parse method.
     *
     * @return the executor service used to run asynchronous parse method
     * @since 3.1
     */
    public ExecutorService getExecutorService() {
        writeline("/home/ubuntu/results/coverage/Digester/Digester_5_10.coverage", "e5f70703-0547-4e19-84f4-a62106efc172");
        return executorService;
    }

    /**
     * Sets the executor service to run asynchronous parse method.
     *
     * @param executorService the executor service to run asynchronous parse method
     * @since 3.1
     */
    public void setExecutorService(ExecutorService executorService) {
        writeline("/home/ubuntu/results/coverage/Digester/Digester_5_10.coverage", "7c3cb186-9bf0-41cf-9124-13ac48d0aa64");
        this.executorService = executorService;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public void characters(char buffer[], int start, int length) throws SAXException {
        writeline("/home/ubuntu/results/coverage/Digester/Digester_5_10.coverage", "6b0c87da-ea5c-4dad-8299-b81fda0777f4");
        if (customContentHandler != null) {
            writeline("/home/ubuntu/results/coverage/Digester/Digester_5_10.coverage", "2118a45c-f494-4a32-8e39-23f8e8d05e1d");
            customContentHandler.characters(buffer, start, length);
            writeline("/home/ubuntu/results/coverage/Digester/Digester_5_10.coverage", "0b228dd9-644d-464e-a2f4-1e7ad53ab04f");
            return;
        }
        writeline("/home/ubuntu/results/coverage/Digester/Digester_5_10.coverage", "a3221454-f0c7-44fe-b544-1ef76f9352c5");
        if (saxLog.isDebugEnabled()) {
            writeline("/home/ubuntu/results/coverage/Digester/Digester_5_10.coverage", "c40cdeb5-f825-4304-9332-12f3f21aaf65");
            saxLog.debug("characters(" + new String(buffer, start, length) + ")");
        }
        writeline("/home/ubuntu/results/coverage/Digester/Digester_5_10.coverage", "1e15254c-3cb3-461c-9fd5-cd6843536692");
        bodyText.append(buffer, start, length);
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public void endDocument() throws SAXException {
        writeline("/home/ubuntu/results/coverage/Digester/Digester_5_10.coverage", "444e864e-71a6-4ba9-b1a2-4f3fbcef222d");
        if (saxLog.isDebugEnabled()) {
            writeline("/home/ubuntu/results/coverage/Digester/Digester_5_10.coverage", "746dd215-a66d-4652-a1c9-1dfd921b0be2");
            if (getCount() > 1) {
                writeline("/home/ubuntu/results/coverage/Digester/Digester_5_10.coverage", "20db5247-27e1-477f-872f-9ab15214e4fd");
                saxLog.debug("endDocument():  " + getCount() + " elements left");
            } else {
                writeline("/home/ubuntu/results/coverage/Digester/Digester_5_10.coverage", "5e554323-46d5-4453-aa11-2512da1d37f0");
                saxLog.debug("endDocument()");
            }
        }
        writeline("/home/ubuntu/results/coverage/Digester/Digester_5_10.coverage", "b338fb7a-d5b0-4933-9942-e5f0dcf88615");
        for (Rule rule : getRules().rules()) {
            writeline("/home/ubuntu/results/coverage/Digester/Digester_5_10.coverage", "a7ac7afe-cccc-4b93-93f3-3035e25eea85");
            try {
                writeline("/home/ubuntu/results/coverage/Digester/Digester_5_10.coverage", "45f4f6a4-46c0-4571-9def-41bb404d3974");
                rule.finish();
            } catch (Exception e) {
                writeline("/home/ubuntu/results/coverage/Digester/Digester_5_10.coverage", "7272773a-a964-4be8-87d1-7087e8bff173");
                log.error("Finish event threw exception", e);
                writeline("/home/ubuntu/results/coverage/Digester/Digester_5_10.coverage", "86a66543-c301-4e8e-9443-1813fa794ff4");
                throw createSAXException(e);
            } catch (Error e) {
                writeline("/home/ubuntu/results/coverage/Digester/Digester_5_10.coverage", "145b70a6-bb73-4c87-b522-a79a16142a3d");
                log.error("Finish event threw error", e);
                writeline("/home/ubuntu/results/coverage/Digester/Digester_5_10.coverage", "6b98bb07-04dc-437c-b7c1-85cf85351112");
                throw e;
            }
        }
        writeline("/home/ubuntu/results/coverage/Digester/Digester_5_10.coverage", "2f2c320d-b663-419b-8676-ead74e014d23");
        clear();
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public void endElement(String namespaceURI, String localName, String qName) throws SAXException {
        writeline("/home/ubuntu/results/coverage/Digester/Digester_5_10.coverage", "8af2f005-63ba-4a94-bcd7-32562636697a");
        if (customContentHandler != null) {
            writeline("/home/ubuntu/results/coverage/Digester/Digester_5_10.coverage", "234b782f-2bd7-4220-a16f-e73eb5949fb6");
            customContentHandler.endElement(namespaceURI, localName, qName);
            writeline("/home/ubuntu/results/coverage/Digester/Digester_5_10.coverage", "e28a2e39-c8fb-4a81-ac8a-8c135c9c7f69");
            return;
        }
        writeline("/home/ubuntu/results/coverage/Digester/Digester_5_10.coverage", "36140f21-17ea-4b0a-b48b-48ba16844376");
        boolean debug = log.isDebugEnabled();
        writeline("/home/ubuntu/results/coverage/Digester/Digester_5_10.coverage", "f749bb3f-5254-4b8a-8371-df655aa5e62e");
        if (debug) {
            writeline("/home/ubuntu/results/coverage/Digester/Digester_5_10.coverage", "5d7befed-0343-4094-846a-ed78ec9abf92");
            if (saxLog.isDebugEnabled()) {
                writeline("/home/ubuntu/results/coverage/Digester/Digester_5_10.coverage", "a1f447ae-ddb7-4666-b9d7-5e630c9f18cf");
                saxLog.debug("endElement(" + namespaceURI + "," + localName + "," + qName + ")");
            }
            writeline("/home/ubuntu/results/coverage/Digester/Digester_5_10.coverage", "ff3d5c5d-fe19-495b-a01c-d0258e1dce8b");
            log.debug("  match='" + match + "'");
            writeline("/home/ubuntu/results/coverage/Digester/Digester_5_10.coverage", "a202fee0-299b-43fa-b7e7-d1e0a9731504");
            log.debug("  bodyText='" + bodyText + "'");
        }
        writeline("/home/ubuntu/results/coverage/Digester/Digester_5_10.coverage", "af56fca6-3671-4fb6-bc55-0982336c8ed9");
        String name = localName;
        writeline("/home/ubuntu/results/coverage/Digester/Digester_5_10.coverage", "b9d29441-bb62-47b2-8fef-1cbe0d9c2541");
        if ((name == null) || (name.length() < 1)) {
            writeline("/home/ubuntu/results/coverage/Digester/Digester_5_10.coverage", "42ec1a4e-bc45-4d4f-a1ce-8c74e93d29fd");
            name = qName;
        }
        writeline("/home/ubuntu/results/coverage/Digester/Digester_5_10.coverage", "fd3ce803-b6d6-422a-b1f0-e404d5151b66");
        List<Rule> rules = matches.pop();
        writeline("/home/ubuntu/results/coverage/Digester/Digester_5_10.coverage", "df080577-d9ec-493e-a86a-d4087402d780");
        if ((rules != null) && (rules.size() > 0)) {
            writeline("/home/ubuntu/results/coverage/Digester/Digester_5_10.coverage", "cd47c10e-96ae-4ac1-9b2d-218b111f41e0");
            String bodyText = this.bodyText.toString();
            writeline("/home/ubuntu/results/coverage/Digester/Digester_5_10.coverage", "858fd600-4947-4e12-93b7-a7468c36e27a");
            Substitutor substitutor = getSubstitutor();
            writeline("/home/ubuntu/results/coverage/Digester/Digester_5_10.coverage", "a14ce5bb-1efd-4f93-8761-dc171a9e039b");
            if (substitutor != null) {
                writeline("/home/ubuntu/results/coverage/Digester/Digester_5_10.coverage", "1c03c5d4-abe6-40a3-8d7f-7b45dc2e4650");
                bodyText = substitutor.substitute(bodyText);
            }
            writeline("/home/ubuntu/results/coverage/Digester/Digester_5_10.coverage", "f389c85e-7c47-4e81-871a-215e4de159e7");
            for (int i = 0; i < rules.size(); i++) {
                writeline("/home/ubuntu/results/coverage/Digester/Digester_5_10.coverage", "cf213f8a-752b-4ac6-b6d3-793a3d9b9f8c");
                try {
                    writeline("/home/ubuntu/results/coverage/Digester/Digester_5_10.coverage", "2fad616a-a392-41d2-ae58-e00bbeea0a30");
                    Rule rule = rules.get(i);
                    writeline("/home/ubuntu/results/coverage/Digester/Digester_5_10.coverage", "f7ec27ac-ae0d-45f7-80ce-ed1535a06d5c");
                    if (debug) {
                        writeline("/home/ubuntu/results/coverage/Digester/Digester_5_10.coverage", "3ffe5078-462a-400f-b9ad-d951b0d5267f");
                        log.debug("  Fire body() for " + rule);
                    }
                    writeline("/home/ubuntu/results/coverage/Digester/Digester_5_10.coverage", "78d1e672-d682-497b-945b-43074845dd6b");
                    rule.body(namespaceURI, name, bodyText);
                } catch (Exception e) {
                    writeline("/home/ubuntu/results/coverage/Digester/Digester_5_10.coverage", "3dbb9541-69a1-4239-a5f8-3ef8aabb79ce");
                    log.error("Body event threw exception", e);
                    writeline("/home/ubuntu/results/coverage/Digester/Digester_5_10.coverage", "a0f88851-2ebe-4524-adc8-958fd85c7f59");
                    throw createSAXException(e);
                } catch (Error e) {
                    writeline("/home/ubuntu/results/coverage/Digester/Digester_5_10.coverage", "b1b1be96-7637-4299-ba2c-14c7378d53a5");
                    log.error("Body event threw error", e);
                    writeline("/home/ubuntu/results/coverage/Digester/Digester_5_10.coverage", "3286f49a-48fe-468e-9f04-d76185088d65");
                    throw e;
                }
            }
        } else {
            writeline("/home/ubuntu/results/coverage/Digester/Digester_5_10.coverage", "8473d9df-f422-46b7-87a6-8ecde5345e4f");
            if (debug) {
                writeline("/home/ubuntu/results/coverage/Digester/Digester_5_10.coverage", "08575ab8-7682-4268-addb-4e5873eb4fef");
                log.debug("  No rules found matching '" + match + "'.");
            }
        }
        writeline("/home/ubuntu/results/coverage/Digester/Digester_5_10.coverage", "b81243a6-175b-4092-80ce-f75c43052e67");
        bodyText = bodyTexts.pop();
        writeline("/home/ubuntu/results/coverage/Digester/Digester_5_10.coverage", "4fe874c4-c6da-481c-ade2-7c235885e426");
        if (debug) {
            writeline("/home/ubuntu/results/coverage/Digester/Digester_5_10.coverage", "e942f384-ce3e-492a-a9f7-60d3a4718d14");
            log.debug("  Popping body text '" + bodyText.toString() + "'");
        }
        writeline("/home/ubuntu/results/coverage/Digester/Digester_5_10.coverage", "5c896707-8ec2-4db8-8999-37531bb37c43");
        if (rules != null) {
            writeline("/home/ubuntu/results/coverage/Digester/Digester_5_10.coverage", "2396999f-b375-42d5-8ef0-40843d551828");
            for (int i = 0; i < rules.size(); i++) {
                writeline("/home/ubuntu/results/coverage/Digester/Digester_5_10.coverage", "15ff84d6-137f-401a-8888-25c6842d8f42");
                int j = (rules.size() - i) - 1;
                writeline("/home/ubuntu/results/coverage/Digester/Digester_5_10.coverage", "6fa49887-03b6-4b3c-8ec7-977e45825639");
                try {
                    writeline("/home/ubuntu/results/coverage/Digester/Digester_5_10.coverage", "594fdf21-f451-4122-8a8f-6f963ff58efe");
                    Rule rule = rules.get(j);
                    writeline("/home/ubuntu/results/coverage/Digester/Digester_5_10.coverage", "23491aeb-3094-4baa-aca6-38c23641799a");
                    if (debug) {
                        writeline("/home/ubuntu/results/coverage/Digester/Digester_5_10.coverage", "6e73eb28-ec6f-4c0f-8082-1599eddd1e75");
                        log.debug("  Fire end() for " + rule);
                    }
                    writeline("/home/ubuntu/results/coverage/Digester/Digester_5_10.coverage", "0c65d2a3-952c-4c91-b89b-e5c67b7c6d0a");
                    rule.end(namespaceURI, name);
                } catch (Exception e) {
                    writeline("/home/ubuntu/results/coverage/Digester/Digester_5_10.coverage", "0e90c621-85e9-4780-a5da-a2c90c785da2");
                    log.error("End event threw exception", e);
                    writeline("/home/ubuntu/results/coverage/Digester/Digester_5_10.coverage", "3131c0d4-509e-4049-94dc-fd8c6c15e843");
                    throw createSAXException(e);
                } catch (Error e) {
                    writeline("/home/ubuntu/results/coverage/Digester/Digester_5_10.coverage", "3be01dec-b311-432f-b8cc-d63a9d94a692");
                    log.error("End event threw error", e);
                    writeline("/home/ubuntu/results/coverage/Digester/Digester_5_10.coverage", "d29db049-ea2f-46b1-a0a1-042c79de4d0d");
                    throw e;
                }
            }
        }
        writeline("/home/ubuntu/results/coverage/Digester/Digester_5_10.coverage", "0ab3a22e-1d77-4eaa-bda1-2449019fb36c");
        int slash = match.lastIndexOf('/');
        writeline("/home/ubuntu/results/coverage/Digester/Digester_5_10.coverage", "6ca1642c-8d9c-45fa-91e8-252e45107f0c");
        if (slash >= 0) {
            writeline("/home/ubuntu/results/coverage/Digester/Digester_5_10.coverage", "72a167fb-bebf-47a5-b511-2e32110f75b7");
            match = match.substring(0, slash);
        } else {
            writeline("/home/ubuntu/results/coverage/Digester/Digester_5_10.coverage", "84f0051e-5215-4317-a2e0-e916f198c3b8");
            match = "";
        }
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public void endPrefixMapping(String prefix) throws SAXException {
        writeline("/home/ubuntu/results/coverage/Digester/Digester_5_10.coverage", "81cdd74b-2f21-4274-895a-de77d961776c");
        if (saxLog.isDebugEnabled()) {
            writeline("/home/ubuntu/results/coverage/Digester/Digester_5_10.coverage", "db0b44d7-a8a5-4ee3-bcf9-266512fb839e");
            saxLog.debug("endPrefixMapping(" + prefix + ")");
        }
        writeline("/home/ubuntu/results/coverage/Digester/Digester_5_10.coverage", "5d976297-77cd-48c0-87b2-96db3732f836");
        Stack<String> stack = namespaces.get(prefix);
        writeline("/home/ubuntu/results/coverage/Digester/Digester_5_10.coverage", "c0276cb8-294e-4478-bb5f-f15f8a52536d");
        if (stack == null) {
            writeline("/home/ubuntu/results/coverage/Digester/Digester_5_10.coverage", "94002266-b7e5-413b-b5a6-488246561f86");
            return;
        }
        writeline("/home/ubuntu/results/coverage/Digester/Digester_5_10.coverage", "cc1bb966-10e7-49ba-b616-10c93746e919");
        try {
            writeline("/home/ubuntu/results/coverage/Digester/Digester_5_10.coverage", "31ec4f83-f854-416c-88c5-31be9c5f1ecd");
            stack.pop();
            writeline("/home/ubuntu/results/coverage/Digester/Digester_5_10.coverage", "1be39be4-2449-4f00-b032-fa773790d6ec");
            if (stack.empty()) {
                writeline("/home/ubuntu/results/coverage/Digester/Digester_5_10.coverage", "494e5fa7-f214-431d-97ca-ef351f54d8e4");
                namespaces.remove(prefix);
            }
        } catch (EmptyStackException e) {
            writeline("/home/ubuntu/results/coverage/Digester/Digester_5_10.coverage", "113b92f7-5b7b-4ccf-864c-ef89edd96224");
            throw createSAXException("endPrefixMapping popped too many times");
        }
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public void ignorableWhitespace(char buffer[], int start, int len) throws SAXException {
        writeline("/home/ubuntu/results/coverage/Digester/Digester_5_10.coverage", "9d60b53f-dc8c-4775-9ccf-3c1ad4114b9c");
        if (saxLog.isDebugEnabled()) {
            writeline("/home/ubuntu/results/coverage/Digester/Digester_5_10.coverage", "738f0668-4923-4aaa-b2b5-b8bf5abd7318");
            saxLog.debug("ignorableWhitespace(" + new String(buffer, start, len) + ")");
        }
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public void processingInstruction(String target, String data) throws SAXException {
        writeline("/home/ubuntu/results/coverage/Digester/Digester_5_10.coverage", "09070f3a-a8a4-408e-8305-27f2983a765a");
        if (customContentHandler != null) {
            writeline("/home/ubuntu/results/coverage/Digester/Digester_5_10.coverage", "1cf7a672-8f40-41bd-8af8-2da3ecf20192");
            customContentHandler.processingInstruction(target, data);
            writeline("/home/ubuntu/results/coverage/Digester/Digester_5_10.coverage", "8ec5117b-2356-4e7f-9a2f-6ac8e32cdea0");
            return;
        }
        writeline("/home/ubuntu/results/coverage/Digester/Digester_5_10.coverage", "42478c14-8a9d-4120-8caf-57c59727ee89");
        if (saxLog.isDebugEnabled()) {
            writeline("/home/ubuntu/results/coverage/Digester/Digester_5_10.coverage", "c3059311-60e5-4db6-859d-2185939f4cfb");
            saxLog.debug("processingInstruction('" + target + "','" + data + "')");
        }
    }

    /**
     * Gets the document locator associated with our parser.
     *
     * @return the Locator supplied by the document parser
     */
    public Locator getDocumentLocator() {
        writeline("/home/ubuntu/results/coverage/Digester/Digester_5_10.coverage", "63d20752-5891-4a82-8ce3-7773a6cc7e78");
        return locator;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public void setDocumentLocator(Locator locator) {
        writeline("/home/ubuntu/results/coverage/Digester/Digester_5_10.coverage", "0e5c9d50-2641-4885-832f-5f0a26ccd7f6");
        if (saxLog.isDebugEnabled()) {
            writeline("/home/ubuntu/results/coverage/Digester/Digester_5_10.coverage", "8b7e3743-c0b2-499b-80e0-ba5537027ef2");
            saxLog.debug("setDocumentLocator(" + locator + ")");
        }
        writeline("/home/ubuntu/results/coverage/Digester/Digester_5_10.coverage", "cf0a8936-1b0e-4328-be90-bab157023b3c");
        this.locator = locator;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public void skippedEntity(String name) throws SAXException {
        writeline("/home/ubuntu/results/coverage/Digester/Digester_5_10.coverage", "c79fdc04-03c4-4777-8d7d-3a8ad8f106dc");
        if (saxLog.isDebugEnabled()) {
            writeline("/home/ubuntu/results/coverage/Digester/Digester_5_10.coverage", "9ddeabbb-d87e-4a6c-bc8f-ec0ba0133742");
            saxLog.debug("skippedEntity(" + name + ")");
        }
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public void startDocument() throws SAXException {
        writeline("/home/ubuntu/results/coverage/Digester/Digester_5_10.coverage", "235492f1-9128-4371-9633-9a1f91e416cf");
        if (saxLog.isDebugEnabled()) {
            writeline("/home/ubuntu/results/coverage/Digester/Digester_5_10.coverage", "4113dfa1-a8ef-4c84-8d1f-1d5d46eedcb0");
            saxLog.debug("startDocument()");
        }
        writeline("/home/ubuntu/results/coverage/Digester/Digester_5_10.coverage", "054e482a-cc98-476c-8a02-4fe07d8f0d19");
        configure();
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public void startElement(String namespaceURI, String localName, String qName, Attributes list) throws SAXException {
        writeline("/home/ubuntu/results/coverage/Digester/Digester_5_10.coverage", "201b19bb-a7a8-45cd-bc0f-12266ff90d6e");
        boolean debug = log.isDebugEnabled();
        writeline("/home/ubuntu/results/coverage/Digester/Digester_5_10.coverage", "dd380971-5ed6-415a-8fe6-4414d466bd82");
        if (customContentHandler != null) {
            writeline("/home/ubuntu/results/coverage/Digester/Digester_5_10.coverage", "89f37741-f3f8-4af1-8908-55e5dbd000a6");
            customContentHandler.startElement(namespaceURI, localName, qName, list);
            writeline("/home/ubuntu/results/coverage/Digester/Digester_5_10.coverage", "04c72a4b-2685-4e41-a0ab-3a38b229a009");
            return;
        }
        writeline("/home/ubuntu/results/coverage/Digester/Digester_5_10.coverage", "5fb94efd-6db8-4fa9-9bbb-52a23e3d2e07");
        if (saxLog.isDebugEnabled()) {
            writeline("/home/ubuntu/results/coverage/Digester/Digester_5_10.coverage", "b7b1d376-b0ba-4062-b09c-6ce42a3e7597");
            saxLog.debug("startElement(" + namespaceURI + "," + localName + "," + qName + ")");
        }
        writeline("/home/ubuntu/results/coverage/Digester/Digester_5_10.coverage", "726b0ffc-6254-482c-a53b-71f693bb9f7e");
        bodyTexts.push(bodyText);
        writeline("/home/ubuntu/results/coverage/Digester/Digester_5_10.coverage", "d28ab9e8-01b0-4184-b72b-756c4d5b022b");
        if (debug) {
            writeline("/home/ubuntu/results/coverage/Digester/Digester_5_10.coverage", "079e91ab-68ab-41d4-b15b-87ae740da307");
            log.debug("  Pushing body text '" + bodyText.toString() + "'");
        }
        writeline("/home/ubuntu/results/coverage/Digester/Digester_5_10.coverage", "9010cc7e-dd50-48af-94cb-23822afdd27c");
        bodyText = new StringBuilder();
        writeline("/home/ubuntu/results/coverage/Digester/Digester_5_10.coverage", "6f826cbd-cb56-4edc-8942-1781ba6a2f32");
        String name = localName;
        writeline("/home/ubuntu/results/coverage/Digester/Digester_5_10.coverage", "dd8694de-ca90-4545-b6c2-34be1c21bd83");
        if ((name == null) || (name.length() < 1)) {
            writeline("/home/ubuntu/results/coverage/Digester/Digester_5_10.coverage", "d92135ec-f27b-4413-a1a4-c3336a5977bd");
            name = qName;
        }
        writeline("/home/ubuntu/results/coverage/Digester/Digester_5_10.coverage", "f5c65d2b-3b41-4a46-98f0-36bfafbd67cd");
        StringBuilder sb = new StringBuilder(match);
        writeline("/home/ubuntu/results/coverage/Digester/Digester_5_10.coverage", "4478ca62-b90b-4d1e-b0a4-74c490d37b8e");
        if (match.length() > 0) {
            writeline("/home/ubuntu/results/coverage/Digester/Digester_5_10.coverage", "0cedb37a-035c-4252-8b03-e6c2c3533a9b");
            sb.append('/');
        }
        writeline("/home/ubuntu/results/coverage/Digester/Digester_5_10.coverage", "78a52111-d275-47e2-8610-3ab5a6bb29e3");
        sb.append(name);
        writeline("/home/ubuntu/results/coverage/Digester/Digester_5_10.coverage", "cccac2ab-fa54-4e00-83b8-f756a63a1edc");
        match = sb.toString();
        writeline("/home/ubuntu/results/coverage/Digester/Digester_5_10.coverage", "4bd61f8b-420d-481b-89ad-d16aab81fe1a");
        if (debug) {
            writeline("/home/ubuntu/results/coverage/Digester/Digester_5_10.coverage", "1952a568-925f-4a9d-874f-0073c9c0f951");
            log.debug("  New match='" + match + "'");
        }
        writeline("/home/ubuntu/results/coverage/Digester/Digester_5_10.coverage", "43ea30ee-73f2-4f53-b186-d5585a61bc53");
        List<Rule> rules = getRules().match(namespaceURI, match, localName, list);
        writeline("/home/ubuntu/results/coverage/Digester/Digester_5_10.coverage", "cd6fae47-a173-4864-bd2c-fc8cbd2af49e");
        matches.push(rules);
        writeline("/home/ubuntu/results/coverage/Digester/Digester_5_10.coverage", "54122647-9609-4ede-9da2-1a9a5621e3ab");
        if ((rules != null) && (rules.size() > 0)) {
            writeline("/home/ubuntu/results/coverage/Digester/Digester_5_10.coverage", "36679d69-ae82-43da-ac3d-43d504b0b9c4");
            Substitutor substitutor = getSubstitutor();
            writeline("/home/ubuntu/results/coverage/Digester/Digester_5_10.coverage", "dac18ece-8268-481d-83f1-b19579a403ba");
            if (substitutor != null) {
                writeline("/home/ubuntu/results/coverage/Digester/Digester_5_10.coverage", "8daf82ee-c45d-41db-969e-fc22187faddb");
                list = substitutor.substitute(list);
            }
            writeline("/home/ubuntu/results/coverage/Digester/Digester_5_10.coverage", "34a8c79e-6535-44fd-b961-810d08631a04");
            for (int i = 0; i < rules.size(); i++) {
                writeline("/home/ubuntu/results/coverage/Digester/Digester_5_10.coverage", "c5802914-a589-49d6-96af-4c5485ab5a49");
                try {
                    writeline("/home/ubuntu/results/coverage/Digester/Digester_5_10.coverage", "98d04cf1-cf5b-4fe5-a992-6b2c4c25878b");
                    Rule rule = rules.get(i);
                    writeline("/home/ubuntu/results/coverage/Digester/Digester_5_10.coverage", "1fdd61a7-5d75-4d00-a0a5-b9e1babef1c9");
                    if (debug) {
                        writeline("/home/ubuntu/results/coverage/Digester/Digester_5_10.coverage", "06db6cf8-2721-4458-8385-aae2f5e6b4d6");
                        log.debug("  Fire begin() for " + rule);
                    }
                    writeline("/home/ubuntu/results/coverage/Digester/Digester_5_10.coverage", "999c22eb-13ae-4a75-a207-cf7a138232ed");
                    rule.begin(namespaceURI, name, list);
                } catch (Exception e) {
                    writeline("/home/ubuntu/results/coverage/Digester/Digester_5_10.coverage", "1fc79207-9c5b-42e5-b4d0-8ab3ca6ec0d2");
                    log.error("Begin event threw exception", e);
                    writeline("/home/ubuntu/results/coverage/Digester/Digester_5_10.coverage", "3cf69b88-9e38-485f-8f15-bd3a2c2dfd8e");
                    throw createSAXException(e);
                } catch (Error e) {
                    writeline("/home/ubuntu/results/coverage/Digester/Digester_5_10.coverage", "e9f16c4d-b4e3-4e48-b32e-c9b1874c14a5");
                    log.error("Begin event threw error", e);
                    writeline("/home/ubuntu/results/coverage/Digester/Digester_5_10.coverage", "7fde3600-27a9-4321-95cf-a9e57a386c66");
                    throw e;
                }
            }
        } else {
            writeline("/home/ubuntu/results/coverage/Digester/Digester_5_10.coverage", "bae6f7df-d7c1-4469-aeec-50e616482d00");
            if (debug) {
                writeline("/home/ubuntu/results/coverage/Digester/Digester_5_10.coverage", "ac6f9d21-efdf-49a9-850e-2e9082be29b6");
                log.debug("  No rules found matching '" + match + "'.");
            }
        }
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public void startPrefixMapping(String prefix, String namespaceURI) throws SAXException {
        writeline("/home/ubuntu/results/coverage/Digester/Digester_5_10.coverage", "b8d18286-8735-421d-a716-014d05dc79a6");
        if (saxLog.isDebugEnabled()) {
            writeline("/home/ubuntu/results/coverage/Digester/Digester_5_10.coverage", "0d09f74e-9f09-458a-93e4-0224095aaee2");
            saxLog.debug("startPrefixMapping(" + prefix + "," + namespaceURI + ")");
        }
        writeline("/home/ubuntu/results/coverage/Digester/Digester_5_10.coverage", "c2a8de25-6402-434c-b416-f4a9ccfc029a");
        Stack<String> stack = namespaces.get(prefix);
        writeline("/home/ubuntu/results/coverage/Digester/Digester_5_10.coverage", "b37b871f-4602-4dca-8b75-36528c7a6a29");
        if (stack == null) {
            writeline("/home/ubuntu/results/coverage/Digester/Digester_5_10.coverage", "360a7154-4857-457c-97f0-5d6bebc04b98");
            stack = new Stack<String>();
            writeline("/home/ubuntu/results/coverage/Digester/Digester_5_10.coverage", "827ca596-0038-467d-826d-b67d6817b2c8");
            namespaces.put(prefix, stack);
        }
        writeline("/home/ubuntu/results/coverage/Digester/Digester_5_10.coverage", "67b20a55-e505-4326-b6d5-191d2cd449ef");
        stack.push(namespaceURI);
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public void notationDecl(String name, String publicId, String systemId) {
        writeline("/home/ubuntu/results/coverage/Digester/Digester_5_10.coverage", "7854ff01-59cc-436a-a3da-9016a608808f");
        if (saxLog.isDebugEnabled()) {
            writeline("/home/ubuntu/results/coverage/Digester/Digester_5_10.coverage", "4f4cc2f5-ae45-4227-9a78-e05c7581d354");
            saxLog.debug("notationDecl(" + name + "," + publicId + "," + systemId + ")");
        }
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public void unparsedEntityDecl(String name, String publicId, String systemId, String notation) {
        writeline("/home/ubuntu/results/coverage/Digester/Digester_5_10.coverage", "5f078f04-1541-446a-b2c4-35f672f23223");
        if (saxLog.isDebugEnabled()) {
            writeline("/home/ubuntu/results/coverage/Digester/Digester_5_10.coverage", "39a5fcfa-f480-46dd-ab8a-8366018e8b21");
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
        writeline("/home/ubuntu/results/coverage/Digester/Digester_5_10.coverage", "5222fd14-8389-412f-baa8-dce7f0b772d9");
        this.entityResolver = entityResolver;
    }

    /**
     * Return the Entity Resolver used by the SAX parser.
     *
     * @return the Entity Resolver used by the SAX parser.
     */
    public EntityResolver getEntityResolver() {
        writeline("/home/ubuntu/results/coverage/Digester/Digester_5_10.coverage", "d5417492-f0f1-42da-a222-fb35257fa4e4");
        return entityResolver;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public InputSource resolveEntity(String publicId, String systemId) throws SAXException {
        writeline("/home/ubuntu/results/coverage/Digester/Digester_5_10.coverage", "67d4d3cb-3e62-46ee-8331-d7ccb9b1d2f1");
        if (saxLog.isDebugEnabled()) {
            writeline("/home/ubuntu/results/coverage/Digester/Digester_5_10.coverage", "841d3a75-7b02-4c71-8787-eb1c8b66fa41");
            saxLog.debug("resolveEntity('" + publicId + "', '" + systemId + "')");
        }
        writeline("/home/ubuntu/results/coverage/Digester/Digester_5_10.coverage", "75bae766-3adb-4bd3-9755-4c1185302e7c");
        if (publicId != null) {
            writeline("/home/ubuntu/results/coverage/Digester/Digester_5_10.coverage", "bd45058d-c76a-4014-ae0c-47f054d01e18");
            this.publicId = publicId;
        }
        writeline("/home/ubuntu/results/coverage/Digester/Digester_5_10.coverage", "6aaa5f10-3623-4a1d-9363-5693485154d8");
        URL entityURL = null;
        writeline("/home/ubuntu/results/coverage/Digester/Digester_5_10.coverage", "f3350c28-d562-4d71-99d2-8b3b3c1f01c6");
        if (publicId != null) {
            writeline("/home/ubuntu/results/coverage/Digester/Digester_5_10.coverage", "2f8943db-7c15-49f5-bfd8-0b1149c512b2");
            entityURL = entityValidator.get(publicId);
        }
        writeline("/home/ubuntu/results/coverage/Digester/Digester_5_10.coverage", "adcac313-0445-4698-9fbb-e1f9ea489fd1");
        if (entityURL == null && systemId != null) {
            writeline("/home/ubuntu/results/coverage/Digester/Digester_5_10.coverage", "2aff8f3b-ef54-4da6-bcfa-b682b23301f9");
            entityURL = entityValidator.get(systemId);
        }
        writeline("/home/ubuntu/results/coverage/Digester/Digester_5_10.coverage", "61838a13-8223-44c6-8dc1-b6b8b9d82581");
        if (entityURL == null) {
            writeline("/home/ubuntu/results/coverage/Digester/Digester_5_10.coverage", "1c241dba-8d74-41b5-8786-bd84492e6548");
            if (systemId == null) {
                writeline("/home/ubuntu/results/coverage/Digester/Digester_5_10.coverage", "b3299620-6dc0-4414-8771-68381f78307a");
                if (log.isDebugEnabled()) {
                    writeline("/home/ubuntu/results/coverage/Digester/Digester_5_10.coverage", "3dad3b74-f904-4f07-9f94-be613398fc31");
                    log.debug(" Cannot resolve null entity, returning null InputSource");
                }
                writeline("/home/ubuntu/results/coverage/Digester/Digester_5_10.coverage", "1ee98cb4-ddd4-41fb-ab66-94fd4009dee9");
                return (null);
            }
            writeline("/home/ubuntu/results/coverage/Digester/Digester_5_10.coverage", "312491cb-d5f4-4216-8951-cf99e5fd9aee");
            if (log.isDebugEnabled()) {
                writeline("/home/ubuntu/results/coverage/Digester/Digester_5_10.coverage", "d3d5e6c9-977b-4249-babf-45b427e52496");
                log.debug(" Trying to resolve using system ID '" + systemId + "'");
            }
            writeline("/home/ubuntu/results/coverage/Digester/Digester_5_10.coverage", "4d204896-640a-49a2-a29f-77f9f482796c");
            try {
                writeline("/home/ubuntu/results/coverage/Digester/Digester_5_10.coverage", "64d5a6d8-c0b1-4412-b99b-0f46d3225921");
                entityURL = new URL(systemId);
            } catch (MalformedURLException e) {
                writeline("/home/ubuntu/results/coverage/Digester/Digester_5_10.coverage", "9b2ef2b7-9f57-48b4-b7c8-3bf81cc0a162");
                throw new IllegalArgumentException("Malformed URL '" + systemId + "' : " + e.getMessage());
            }
        }
        writeline("/home/ubuntu/results/coverage/Digester/Digester_5_10.coverage", "1706d437-361d-4d57-bc2e-5e269d7af3df");
        if (log.isDebugEnabled()) {
            writeline("/home/ubuntu/results/coverage/Digester/Digester_5_10.coverage", "d6c49362-ebf0-4eea-ac04-af5062a8e7c1");
            log.debug(" Resolving to alternate DTD '" + entityURL + "'");
        }
        writeline("/home/ubuntu/results/coverage/Digester/Digester_5_10.coverage", "03cdc98d-7706-473d-86bd-c1050995a058");
        try {
            writeline("/home/ubuntu/results/coverage/Digester/Digester_5_10.coverage", "1ca947b2-7ce9-4a3e-94e6-579b93eb6c8b");
            return createInputSourceFromURL(entityURL);
        } catch (Exception e) {
            writeline("/home/ubuntu/results/coverage/Digester/Digester_5_10.coverage", "65cb4fe5-a87e-4a91-bdf6-de4247c31f2f");
            throw createSAXException(e);
        }
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public void error(SAXParseException exception) throws SAXException {
        writeline("/home/ubuntu/results/coverage/Digester/Digester_5_10.coverage", "d4f99f8a-9880-4889-aba7-a420eeda9700");
        log.error("Parse Error at line " + exception.getLineNumber() + " column " + exception.getColumnNumber() + ": " + exception.getMessage(), exception);
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public void fatalError(SAXParseException exception) throws SAXException {
        writeline("/home/ubuntu/results/coverage/Digester/Digester_5_10.coverage", "8759ad5d-e45c-4fae-a451-9912f765d662");
        log.error("Parse Fatal Error at line " + exception.getLineNumber() + " column " + exception.getColumnNumber() + ": " + exception.getMessage(), exception);
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public void warning(SAXParseException exception) throws SAXException {
        writeline("/home/ubuntu/results/coverage/Digester/Digester_5_10.coverage", "a9a0cf92-cea9-4c1f-96cc-e2b486530781");
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
        writeline("/home/ubuntu/results/coverage/Digester/Digester_5_10.coverage", "1b41bad3-bee4-40cf-869b-ee3dddb08e15");
        if (file == null) {
            writeline("/home/ubuntu/results/coverage/Digester/Digester_5_10.coverage", "9e652a9b-ef1a-47ad-8458-88452d4f8fcd");
            throw new IllegalArgumentException("File to parse is null");
        }
        writeline("/home/ubuntu/results/coverage/Digester/Digester_5_10.coverage", "6375badb-a75f-4206-a45f-7189ac3101a8");
        InputSource input = new InputSource(new FileInputStream(file));
        writeline("/home/ubuntu/results/coverage/Digester/Digester_5_10.coverage", "84a5787f-4ef6-41b3-a380-f0f413bbe258");
        input.setSystemId(file.toURI().toURL().toString());
        writeline("/home/ubuntu/results/coverage/Digester/Digester_5_10.coverage", "303d35ca-0c9e-4fcc-b991-413d7fab5a6d");
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
        writeline("/home/ubuntu/results/coverage/Digester/Digester_5_10.coverage", "c12f5c78-c17f-4564-bfeb-0f36f3743f69");
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
        writeline("/home/ubuntu/results/coverage/Digester/Digester_5_10.coverage", "6f684fb7-3513-4d6a-aea9-eccddf7a9d0e");
        if (input == null) {
            writeline("/home/ubuntu/results/coverage/Digester/Digester_5_10.coverage", "a0865e1f-a783-4554-9990-86b40ddd906c");
            throw new IllegalArgumentException("InputSource to parse is null");
        }
        writeline("/home/ubuntu/results/coverage/Digester/Digester_5_10.coverage", "8cc5e455-3560-4ca2-a266-c895f99e4615");
        configure();
        writeline("/home/ubuntu/results/coverage/Digester/Digester_5_10.coverage", "efdb3479-e84b-47b3-98c1-61a75b9fbba7");
        String systemId = input.getSystemId();
        writeline("/home/ubuntu/results/coverage/Digester/Digester_5_10.coverage", "7fa363e1-03dc-45df-b59f-e99cccefe09f");
        if (systemId == null) {
            writeline("/home/ubuntu/results/coverage/Digester/Digester_5_10.coverage", "6065d01f-925c-4c53-ab3f-f632d80d7e71");
            systemId = "(already loaded from stream)";
        }
        writeline("/home/ubuntu/results/coverage/Digester/Digester_5_10.coverage", "3fcfab21-e384-43b3-9b4a-184b6c1fc783");
        try {
            writeline("/home/ubuntu/results/coverage/Digester/Digester_5_10.coverage", "d313f6bd-64b8-46dc-b92c-578a7aae5cc6");
            getXMLReader().parse(input);
        } catch (IOException e) {
            writeline("/home/ubuntu/results/coverage/Digester/Digester_5_10.coverage", "8f9cc4b2-8232-4df1-8244-48316b31a0d6");
            log.error(format("An error occurred while reading stream from '%s', see nested exceptions", systemId), e);
            writeline("/home/ubuntu/results/coverage/Digester/Digester_5_10.coverage", "809b6dfb-b14b-4174-a615-1b3a77a384d2");
            throw e;
        }
        writeline("/home/ubuntu/results/coverage/Digester/Digester_5_10.coverage", "8f53cfbf-b851-4997-9ca8-0ab4b063129e");
        cleanup();
        writeline("/home/ubuntu/results/coverage/Digester/Digester_5_10.coverage", "f0b47ad0-c445-41af-b8ef-2842e32457c3");
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
        writeline("/home/ubuntu/results/coverage/Digester/Digester_5_10.coverage", "64964ae1-3c2d-496e-b98f-4fa99adacde4");
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
        writeline("/home/ubuntu/results/coverage/Digester/Digester_5_10.coverage", "291283c0-888a-408a-a9f7-272246df3936");
        if (input == null) {
            writeline("/home/ubuntu/results/coverage/Digester/Digester_5_10.coverage", "3bb9540a-8c77-408d-9d3e-03f4f43e1db0");
            throw new IllegalArgumentException("InputStream to parse is null");
        }
        writeline("/home/ubuntu/results/coverage/Digester/Digester_5_10.coverage", "90cf0210-48ca-4f50-ba74-7c671cedb3e9");
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
        writeline("/home/ubuntu/results/coverage/Digester/Digester_5_10.coverage", "f63e8877-5b20-41e8-b928-f36fbe80daa7");
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
        writeline("/home/ubuntu/results/coverage/Digester/Digester_5_10.coverage", "240f6363-1dc9-4abb-96a7-fe45790f9d48");
        if (reader == null) {
            writeline("/home/ubuntu/results/coverage/Digester/Digester_5_10.coverage", "f6465bd4-3810-4607-ab7d-6dafbe2665ae");
            throw new IllegalArgumentException("Reader to parse is null");
        }
        writeline("/home/ubuntu/results/coverage/Digester/Digester_5_10.coverage", "7a09855d-cc7f-47c6-941b-5ef9a9a6a0c5");
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
        writeline("/home/ubuntu/results/coverage/Digester/Digester_5_10.coverage", "6d35b14d-e6f3-49b7-a572-a38e10d8a26c");
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
        writeline("/home/ubuntu/results/coverage/Digester/Digester_5_10.coverage", "fa389a3f-d422-4fb3-9365-0f4692f3f7c9");
        if (uri == null) {
            writeline("/home/ubuntu/results/coverage/Digester/Digester_5_10.coverage", "3393e3ef-e9f7-44f5-a8fd-46e57fc83cfa");
            throw new IllegalArgumentException("String URI to parse is null");
        }
        writeline("/home/ubuntu/results/coverage/Digester/Digester_5_10.coverage", "c088635f-739c-43be-b424-46ce19f76b94");
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
        writeline("/home/ubuntu/results/coverage/Digester/Digester_5_10.coverage", "09d4be33-0701-4535-b8a9-271cfd6619d6");
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
        writeline("/home/ubuntu/results/coverage/Digester/Digester_5_10.coverage", "06076612-85c1-40fa-aee1-06cce19a5344");
        if (url == null) {
            writeline("/home/ubuntu/results/coverage/Digester/Digester_5_10.coverage", "6917d6c7-e4c5-4661-bc2b-7ae1645dcf45");
            throw new IllegalArgumentException("URL to parse is null");
        }
        writeline("/home/ubuntu/results/coverage/Digester/Digester_5_10.coverage", "4b71d069-31d6-4231-a9d9-d4165f9996f6");
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
        writeline("/home/ubuntu/results/coverage/Digester/Digester_5_10.coverage", "03131957-cd2f-4730-aba9-00315f3b1e37");
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
        writeline("/home/ubuntu/results/coverage/Digester/Digester_5_10.coverage", "6ec4f370-2e8d-4a84-9eb6-249c10b940b8");
        if (executorService == null) {
            writeline("/home/ubuntu/results/coverage/Digester/Digester_5_10.coverage", "4659be8f-4d96-41f7-a81a-cb2001bf752e");
            throw new IllegalStateException("ExecutorService not set");
        }
        writeline("/home/ubuntu/results/coverage/Digester/Digester_5_10.coverage", "2259aefa-8793-4362-bd23-0edb6fae4afd");
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
        writeline("/home/ubuntu/results/coverage/Digester/Digester_5_10.coverage", "1e9a6b12-6445-4c2f-a2df-dcf885c2476f");
        if (log.isDebugEnabled()) {
            writeline("/home/ubuntu/results/coverage/Digester/Digester_5_10.coverage", "09eccb7e-648f-4311-a8fe-8fea73143052");
            log.debug("register('" + publicId + "', '" + entityURL + "'");
        }
        writeline("/home/ubuntu/results/coverage/Digester/Digester_5_10.coverage", "cf016b6d-7489-4d34-ab05-f738203c7988");
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
        writeline("/home/ubuntu/results/coverage/Digester/Digester_5_10.coverage", "aff166b7-e3a9-48e7-a6a7-0ce5bbb02855");
        if (log.isDebugEnabled()) {
            writeline("/home/ubuntu/results/coverage/Digester/Digester_5_10.coverage", "3541692e-b448-492d-9872-a11491b36cf5");
            log.debug("register('" + publicId + "', '" + entityURL + "'");
        }
        writeline("/home/ubuntu/results/coverage/Digester/Digester_5_10.coverage", "27ec77f7-5b54-4d5d-a2a2-dea823cb1e3f");
        try {
            writeline("/home/ubuntu/results/coverage/Digester/Digester_5_10.coverage", "781962b4-b50d-4a76-baf6-e34ddaaab949");
            entityValidator.put(publicId, new URL(entityURL));
        } catch (MalformedURLException e) {
            writeline("/home/ubuntu/results/coverage/Digester/Digester_5_10.coverage", "8acc10bf-a6f0-4ef9-b661-9867440f97c0");
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
        writeline("/home/ubuntu/results/coverage/Digester/Digester_5_10.coverage", "481282ab-2aa2-4f5f-bd8b-77778a64a8fa");
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
        writeline("/home/ubuntu/results/coverage/Digester/Digester_5_10.coverage", "d35d1383-bcd0-4af1-b9a9-e7c5180e6995");
        URLConnection connection = url.openConnection();
        writeline("/home/ubuntu/results/coverage/Digester/Digester_5_10.coverage", "15b66f73-163b-4e1e-aeec-65dff7cef169");
        connection.setUseCaches(false);
        writeline("/home/ubuntu/results/coverage/Digester/Digester_5_10.coverage", "fa747bfc-b476-49a5-8e28-890f299ac1ea");
        InputStream stream = connection.getInputStream();
        writeline("/home/ubuntu/results/coverage/Digester/Digester_5_10.coverage", "c7597f79-9131-4ffa-baef-ec3759add4b8");
        InputSource source = new InputSource(stream);
        writeline("/home/ubuntu/results/coverage/Digester/Digester_5_10.coverage", "a368863c-95cc-4622-abcf-240107cb0785");
        source.setSystemId(url.toExternalForm());
        writeline("/home/ubuntu/results/coverage/Digester/Digester_5_10.coverage", "0b11dc8d-af02-490d-a64b-40b481a2d877");
        inputSources.add(source);
        writeline("/home/ubuntu/results/coverage/Digester/Digester_5_10.coverage", "b6c5400e-0043-4ac3-afb8-cc68156d421c");
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
        writeline("/home/ubuntu/results/coverage/Digester/Digester_5_10.coverage", "f2b65a50-d3b0-4814-8802-3d2dd4b4c9bc");
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
        writeline("/home/ubuntu/results/coverage/Digester/Digester_5_10.coverage", "f2d45928-0804-4207-a47f-bb6b7c4ba3f3");
        rule.setDigester(this);
        writeline("/home/ubuntu/results/coverage/Digester/Digester_5_10.coverage", "9b6c276b-af12-4dea-8dc2-12f58b0533ea");
        getRules().add(pattern, rule);
    }

    /**
     * Register a set of Rule instances defined in a RuleSet.
     *
     * @param ruleSet The RuleSet instance to configure from
     */
    public void addRuleSet(RuleSet ruleSet) {
        writeline("/home/ubuntu/results/coverage/Digester/Digester_5_10.coverage", "54688a27-f2f6-428b-bf67-6da1e67aa361");
        String oldNamespaceURI = getRuleNamespaceURI();
        writeline("/home/ubuntu/results/coverage/Digester/Digester_5_10.coverage", "5c4b2c2b-75f8-4fa5-90e6-085f3c313705");
        String newNamespaceURI = ruleSet.getNamespaceURI();
        writeline("/home/ubuntu/results/coverage/Digester/Digester_5_10.coverage", "50e73da1-65df-4782-b674-e523dac9c02c");
        if (log.isDebugEnabled()) {
            writeline("/home/ubuntu/results/coverage/Digester/Digester_5_10.coverage", "ad98ad6e-c879-49c9-aa1b-59eaabc7133e");
            if (newNamespaceURI == null) {
                writeline("/home/ubuntu/results/coverage/Digester/Digester_5_10.coverage", "f5069340-936f-4af0-a5dc-2922cb99f2fa");
                log.debug("addRuleSet() with no namespace URI");
            } else {
                writeline("/home/ubuntu/results/coverage/Digester/Digester_5_10.coverage", "481aaf4b-e42f-495f-a500-eb4035588c5a");
                log.debug("addRuleSet() with namespace URI " + newNamespaceURI);
            }
        }
        writeline("/home/ubuntu/results/coverage/Digester/Digester_5_10.coverage", "cfeb7d9f-314e-4cd2-8cf3-ac26e65a90d2");
        setRuleNamespaceURI(newNamespaceURI);
        writeline("/home/ubuntu/results/coverage/Digester/Digester_5_10.coverage", "e5c8857a-078f-404c-a3f3-f83edf488a11");
        ruleSet.addRuleInstances(this);
        writeline("/home/ubuntu/results/coverage/Digester/Digester_5_10.coverage", "088e1590-3877-4133-a64f-2aa8f0075de8");
        setRuleNamespaceURI(oldNamespaceURI);
    }

    /**
     * Add a "bean property setter" rule for the specified parameters.
     *
     * @param pattern Element matching pattern
     * @see BeanPropertySetterRule
     */
    public void addBeanPropertySetter(String pattern) {
        writeline("/home/ubuntu/results/coverage/Digester/Digester_5_10.coverage", "8528a61a-5d39-45e7-a779-a252707e7d20");
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
        writeline("/home/ubuntu/results/coverage/Digester/Digester_5_10.coverage", "b1865f4a-18ef-4fdf-b5ba-51249a93757f");
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
        writeline("/home/ubuntu/results/coverage/Digester/Digester_5_10.coverage", "91998d67-2da5-4046-9cc4-45239b8bfe9f");
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
        writeline("/home/ubuntu/results/coverage/Digester/Digester_5_10.coverage", "2e9a6485-df31-4de7-8d81-726862183b71");
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
        writeline("/home/ubuntu/results/coverage/Digester/Digester_5_10.coverage", "fadf0695-b2be-4b9e-852f-ea2ac8c14ee3");
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
        writeline("/home/ubuntu/results/coverage/Digester/Digester_5_10.coverage", "c3ad0ba9-a7d4-4d19-81bd-9d30dc324860");
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
        writeline("/home/ubuntu/results/coverage/Digester/Digester_5_10.coverage", "b5d1427f-37fa-42db-99fe-09b668034fa3");
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
        writeline("/home/ubuntu/results/coverage/Digester/Digester_5_10.coverage", "70ac7388-bf74-447e-b7a6-de30d707b56d");
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
        writeline("/home/ubuntu/results/coverage/Digester/Digester_5_10.coverage", "532da38e-1a12-4b82-a6f8-c2b771bc0194");
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
        writeline("/home/ubuntu/results/coverage/Digester/Digester_5_10.coverage", "d1c04007-ad97-4e9d-9f42-ead432431735");
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
        writeline("/home/ubuntu/results/coverage/Digester/Digester_5_10.coverage", "f7ed35c5-f020-4dec-9b1d-939ce26a9656");
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
        writeline("/home/ubuntu/results/coverage/Digester/Digester_5_10.coverage", "d06b18dc-00e5-47d2-b904-ebee580363b6");
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
        writeline("/home/ubuntu/results/coverage/Digester/Digester_5_10.coverage", "617fbd73-7023-46e7-bd90-a8e7e4defc63");
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
        writeline("/home/ubuntu/results/coverage/Digester/Digester_5_10.coverage", "a9f1fb88-e47c-49b9-a2fc-ba3c32e9e2a7");
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
        writeline("/home/ubuntu/results/coverage/Digester/Digester_5_10.coverage", "ee58525c-4a6d-4e42-8caa-541c15d0287f");
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
        writeline("/home/ubuntu/results/coverage/Digester/Digester_5_10.coverage", "1eb3a685-f006-4de1-8bb8-a5fa5dd7375c");
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
        writeline("/home/ubuntu/results/coverage/Digester/Digester_5_10.coverage", "6027becd-4a27-4b1a-b846-66ddd5846033");
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
        writeline("/home/ubuntu/results/coverage/Digester/Digester_5_10.coverage", "0ad1d3cd-4d44-4046-b5df-22dc9a9608e4");
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
        writeline("/home/ubuntu/results/coverage/Digester/Digester_5_10.coverage", "ce7db152-8ee3-4c0b-b08c-aeb82cf5de62");
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
        writeline("/home/ubuntu/results/coverage/Digester/Digester_5_10.coverage", "51be1f0f-643f-4124-bb14-c097222c9c54");
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
        writeline("/home/ubuntu/results/coverage/Digester/Digester_5_10.coverage", "96a4d00f-06ca-4656-a6c2-5e455f3221c9");
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
        writeline("/home/ubuntu/results/coverage/Digester/Digester_5_10.coverage", "d2a6306e-5702-41ba-b3dd-a3e40866aaf9");
        creationFactory.setDigester(this);
        writeline("/home/ubuntu/results/coverage/Digester/Digester_5_10.coverage", "dd0e79aa-f216-40e5-8271-460ece4dfbfa");
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
        writeline("/home/ubuntu/results/coverage/Digester/Digester_5_10.coverage", "721abe05-a4fe-42ac-9ef6-390bddce8b45");
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
        writeline("/home/ubuntu/results/coverage/Digester/Digester_5_10.coverage", "9705f089-0e45-470d-9878-3494da1583dc");
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
        writeline("/home/ubuntu/results/coverage/Digester/Digester_5_10.coverage", "2a95c744-c091-443a-9334-24884c4e285a");
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
        writeline("/home/ubuntu/results/coverage/Digester/Digester_5_10.coverage", "70cee2a8-2335-4c49-a025-f5b67c8e1d40");
        addRule(pattern, new ObjectCreateRule(attributeName, clazz));
    }

    /**
     * Adds an {@link SetNestedPropertiesRule}.
     *
     * @param pattern register the rule with this pattern
     * @since 1.6
     */
    public void addSetNestedProperties(String pattern) {
        writeline("/home/ubuntu/results/coverage/Digester/Digester_5_10.coverage", "e38508d5-ffd9-4d2f-a4ce-442982218b16");
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
        writeline("/home/ubuntu/results/coverage/Digester/Digester_5_10.coverage", "370af6a7-20e8-4fa0-9e57-b9ab77c880d3");
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
        writeline("/home/ubuntu/results/coverage/Digester/Digester_5_10.coverage", "9f0c546d-7e6c-407b-8251-dafb94711484");
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
        writeline("/home/ubuntu/results/coverage/Digester/Digester_5_10.coverage", "fd93f1a7-a403-4964-bc79-90b8f1f21d6c");
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
        writeline("/home/ubuntu/results/coverage/Digester/Digester_5_10.coverage", "9520bc36-1b63-42cc-a744-e3dd6710ada2");
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
        writeline("/home/ubuntu/results/coverage/Digester/Digester_5_10.coverage", "ca6b84c1-bfa8-4bd1-9793-193cd1ef9b33");
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
        writeline("/home/ubuntu/results/coverage/Digester/Digester_5_10.coverage", "9bbcceea-c8d0-4625-b923-bae37803b2dc");
        addRule(pattern, new SetRootRule(methodName, paramType));
    }

    /**
     * Add a "set properties" rule for the specified parameters.
     *
     * @param pattern Element matching pattern
     * @see SetPropertiesRule
     */
    public void addSetProperties(String pattern) {
        writeline("/home/ubuntu/results/coverage/Digester/Digester_5_10.coverage", "8a5f10a5-3a43-4686-a1d0-f10f7b1d4143");
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
        writeline("/home/ubuntu/results/coverage/Digester/Digester_5_10.coverage", "bff8c7f8-48ec-4660-a5c3-75931798d1d4");
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
        writeline("/home/ubuntu/results/coverage/Digester/Digester_5_10.coverage", "9733bff5-35d4-4bdd-9ff9-a73c1c973d15");
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
        writeline("/home/ubuntu/results/coverage/Digester/Digester_5_10.coverage", "5772331c-dd24-4db9-a7fb-391cc94a7fe4");
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
        writeline("/home/ubuntu/results/coverage/Digester/Digester_5_10.coverage", "93013d0d-3eb4-4588-a815-74d101ccb40d");
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
        writeline("/home/ubuntu/results/coverage/Digester/Digester_5_10.coverage", "3bd877ff-a23a-4498-aa76-0570a0305ed1");
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
        writeline("/home/ubuntu/results/coverage/Digester/Digester_5_10.coverage", "6e73ad07-3aa4-4df3-aa00-d9d0d4541ee7");
        match = "";
        writeline("/home/ubuntu/results/coverage/Digester/Digester_5_10.coverage", "3a5065a0-c695-4720-aa95-1e82b387bc14");
        bodyTexts.clear();
        writeline("/home/ubuntu/results/coverage/Digester/Digester_5_10.coverage", "c699b8d7-a68d-4e47-b3c5-ef914321da02");
        params.clear();
        writeline("/home/ubuntu/results/coverage/Digester/Digester_5_10.coverage", "9ad9d90f-5ab4-4822-8050-4fb38963fd82");
        publicId = null;
        writeline("/home/ubuntu/results/coverage/Digester/Digester_5_10.coverage", "c556a0ca-3e76-4151-8c00-4f502f252954");
        stack.clear();
        writeline("/home/ubuntu/results/coverage/Digester/Digester_5_10.coverage", "34eee0e7-db42-4a24-a462-8cf82c39e125");
        stacksByName.clear();
        writeline("/home/ubuntu/results/coverage/Digester/Digester_5_10.coverage", "7259db59-a3c4-40f8-9f17-fd7b1fd07fe0");
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
        writeline("/home/ubuntu/results/coverage/Digester/Digester_5_10.coverage", "d17b39bf-9d1d-415b-aeeb-ddf87875a63c");
        try {
            writeline("/home/ubuntu/results/coverage/Digester/Digester_5_10.coverage", "1f0c79fb-e4b6-41ba-aac8-c2b787be3386");
            return this.<T>npeSafeCast(stack.peek());
        } catch (EmptyStackException e) {
            writeline("/home/ubuntu/results/coverage/Digester/Digester_5_10.coverage", "971c3d20-e1b9-4f38-8eec-f2112577d1ee");
            log.warn("Empty stack (returning null)");
            writeline("/home/ubuntu/results/coverage/Digester/Digester_5_10.coverage", "eba5b057-b376-4cd0-b4f8-88727486c6c3");
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
        writeline("/home/ubuntu/results/coverage/Digester/Digester_5_10.coverage", "ac1df817-4d71-4f6f-af30-b9e39e0e28ea");
        int index = (stack.size() - 1) - n;
        writeline("/home/ubuntu/results/coverage/Digester/Digester_5_10.coverage", "01652b24-6c0a-4062-9e0b-1124f4037168");
        if (index < 0) {
            writeline("/home/ubuntu/results/coverage/Digester/Digester_5_10.coverage", "30d70a05-83a6-4d6b-a0c2-701f35fee4b0");
            log.warn("Empty stack (returning null)");
            writeline("/home/ubuntu/results/coverage/Digester/Digester_5_10.coverage", "1f0829da-4609-4c52-adf7-e28cd519ca2b");
            return (null);
        }
        writeline("/home/ubuntu/results/coverage/Digester/Digester_5_10.coverage", "21038dd6-26d1-47d6-9b5c-8d24b6f9dd1b");
        try {
            writeline("/home/ubuntu/results/coverage/Digester/Digester_5_10.coverage", "5dc9ac19-8c94-4acf-b642-6dfe5f8b9b51");
            return this.<T>npeSafeCast(stack.get(index));
        } catch (EmptyStackException e) {
            writeline("/home/ubuntu/results/coverage/Digester/Digester_5_10.coverage", "366ada85-8112-426d-9ad6-2b1798d33024");
            log.warn("Empty stack (returning null)");
            writeline("/home/ubuntu/results/coverage/Digester/Digester_5_10.coverage", "cdc2deb1-290e-4cdd-a343-62e22d3ba5b3");
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
        writeline("/home/ubuntu/results/coverage/Digester/Digester_5_10.coverage", "9e0de37a-0c02-45ec-ba0c-8e396630f5c0");
        try {
            writeline("/home/ubuntu/results/coverage/Digester/Digester_5_10.coverage", "22e67bca-d777-4d89-8323-abbca538e3ae");
            T popped = this.<T>npeSafeCast(stack.pop());
            writeline("/home/ubuntu/results/coverage/Digester/Digester_5_10.coverage", "8a8d4f44-010e-4983-a21e-db986c837d9a");
            if (stackAction != null) {
                writeline("/home/ubuntu/results/coverage/Digester/Digester_5_10.coverage", "b4fae4a5-75df-49f2-9315-7d4ddaa88c2b");
                popped = stackAction.onPop(this, null, popped);
            }
            writeline("/home/ubuntu/results/coverage/Digester/Digester_5_10.coverage", "812252d2-85da-490f-9d38-d8b6db433b52");
            return popped;
        } catch (EmptyStackException e) {
            writeline("/home/ubuntu/results/coverage/Digester/Digester_5_10.coverage", "65852f90-f06a-409b-aa39-cd9ffa7981f9");
            log.warn("Empty stack (returning null)");
            writeline("/home/ubuntu/results/coverage/Digester/Digester_5_10.coverage", "7aac34f2-0253-4338-992b-1342c64a44c9");
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
        writeline("/home/ubuntu/results/coverage/Digester/Digester_5_10.coverage", "9e474c7e-16df-40ae-9059-695dee1b850c");
        if (stackAction != null) {
            writeline("/home/ubuntu/results/coverage/Digester/Digester_5_10.coverage", "c79b2380-c540-47fe-99a0-a2569b1cee22");
            object = stackAction.onPush(this, null, object);
        }
        writeline("/home/ubuntu/results/coverage/Digester/Digester_5_10.coverage", "3a338317-e610-40dd-8956-732898cdaba5");
        if (stack.size() == 0) {
            writeline("/home/ubuntu/results/coverage/Digester/Digester_5_10.coverage", "54d7a558-e358-4956-a059-403fdec89d60");
            root = object;
        }
        writeline("/home/ubuntu/results/coverage/Digester/Digester_5_10.coverage", "4a5d286f-eecd-4a03-a919-1abc7053f76b");
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
        writeline("/home/ubuntu/results/coverage/Digester/Digester_5_10.coverage", "e11b563e-4830-467a-914b-0e543ef2b7e9");
        if (stackAction != null) {
            writeline("/home/ubuntu/results/coverage/Digester/Digester_5_10.coverage", "a34d23bf-f309-4043-973a-223890b6ddf7");
            value = stackAction.onPush(this, stackName, value);
        }
        writeline("/home/ubuntu/results/coverage/Digester/Digester_5_10.coverage", "a4872bc0-f6cd-4bdd-8b73-3370404b070c");
        Stack<Object> namedStack = stacksByName.get(stackName);
        writeline("/home/ubuntu/results/coverage/Digester/Digester_5_10.coverage", "f3a0806c-58dd-40c2-8c57-340beb189d94");
        if (namedStack == null) {
            writeline("/home/ubuntu/results/coverage/Digester/Digester_5_10.coverage", "fdaa98e6-8426-40e0-a5a2-00bd1008a168");
            namedStack = new Stack<Object>();
            writeline("/home/ubuntu/results/coverage/Digester/Digester_5_10.coverage", "e2d7b1e2-896f-4acd-877a-e3de57ecf178");
            stacksByName.put(stackName, namedStack);
        }
        writeline("/home/ubuntu/results/coverage/Digester/Digester_5_10.coverage", "085d803a-e71b-40c9-8fdf-ebe09fcf82c8");
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
        writeline("/home/ubuntu/results/coverage/Digester/Digester_5_10.coverage", "d4bac2cb-3948-4e89-921c-745fdfcd58d6");
        T result = null;
        writeline("/home/ubuntu/results/coverage/Digester/Digester_5_10.coverage", "9e2eafc0-dad9-43fe-b598-1acc2a4c686e");
        Stack<Object> namedStack = stacksByName.get(stackName);
        writeline("/home/ubuntu/results/coverage/Digester/Digester_5_10.coverage", "fc253683-8a97-4f71-a2d8-f3f4ba879687");
        if (namedStack == null) {
            writeline("/home/ubuntu/results/coverage/Digester/Digester_5_10.coverage", "5a0247ea-0109-45eb-90ae-8f558b0dfa06");
            if (log.isDebugEnabled()) {
                writeline("/home/ubuntu/results/coverage/Digester/Digester_5_10.coverage", "46659d34-a3c0-4398-acae-e70f7050bc8a");
                log.debug("Stack '" + stackName + "' is empty");
            }
            writeline("/home/ubuntu/results/coverage/Digester/Digester_5_10.coverage", "a7ad0aaa-9872-4e3a-8113-6bc78e47339a");
            throw new EmptyStackException();
        }
        writeline("/home/ubuntu/results/coverage/Digester/Digester_5_10.coverage", "cb831c3c-152d-4081-83fb-41a4c027a6e9");
        result = this.<T>npeSafeCast(namedStack.pop());
        writeline("/home/ubuntu/results/coverage/Digester/Digester_5_10.coverage", "5c001b79-8639-4958-b0e9-dcce4a42c4a2");
        if (stackAction != null) {
            writeline("/home/ubuntu/results/coverage/Digester/Digester_5_10.coverage", "8534e025-33a7-406c-9be9-3728719adcff");
            result = stackAction.onPop(this, stackName, result);
        }
        writeline("/home/ubuntu/results/coverage/Digester/Digester_5_10.coverage", "9a51fb36-904d-45d6-9959-2bcf9a275650");
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
        writeline("/home/ubuntu/results/coverage/Digester/Digester_5_10.coverage", "6f459dfc-eab5-4576-94b3-99ce09d1044c");
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
        writeline("/home/ubuntu/results/coverage/Digester/Digester_5_10.coverage", "1331536c-1ce4-4873-b349-57856cd5f8d4");
        T result = null;
        writeline("/home/ubuntu/results/coverage/Digester/Digester_5_10.coverage", "51f12591-4f30-4f38-90e9-a0d4e5dbc5a7");
        Stack<Object> namedStack = stacksByName.get(stackName);
        writeline("/home/ubuntu/results/coverage/Digester/Digester_5_10.coverage", "c8965193-31fa-4b1e-afd3-6914bc4ce912");
        if (namedStack == null) {
            writeline("/home/ubuntu/results/coverage/Digester/Digester_5_10.coverage", "6dfbd2e0-d5dd-48d4-a3e1-ab915f517905");
            if (log.isDebugEnabled()) {
                writeline("/home/ubuntu/results/coverage/Digester/Digester_5_10.coverage", "c81b75ef-6740-4f1e-a477-ada83966758a");
                log.debug("Stack '" + stackName + "' is empty");
            }
            writeline("/home/ubuntu/results/coverage/Digester/Digester_5_10.coverage", "09de8e2b-8e08-46d6-bb91-2cd854f245e5");
            throw new EmptyStackException();
        }
        writeline("/home/ubuntu/results/coverage/Digester/Digester_5_10.coverage", "ab969906-9fef-4058-b7dd-5bfcb84deecf");
        int index = (namedStack.size() - 1) - n;
        writeline("/home/ubuntu/results/coverage/Digester/Digester_5_10.coverage", "216e6890-52f2-4b8e-8e67-f7c627f45983");
        if (index < 0) {
            writeline("/home/ubuntu/results/coverage/Digester/Digester_5_10.coverage", "10a77c49-4217-4a3a-af83-4cb7502a826f");
            throw new EmptyStackException();
        }
        writeline("/home/ubuntu/results/coverage/Digester/Digester_5_10.coverage", "b6357ed8-c370-4f3f-956a-e5d0a21b4237");
        result = this.<T>npeSafeCast(namedStack.get(index));
        writeline("/home/ubuntu/results/coverage/Digester/Digester_5_10.coverage", "fb907640-ca4f-4522-9107-3c388214b157");
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
        writeline("/home/ubuntu/results/coverage/Digester/Digester_5_10.coverage", "47803dee-df2b-4e73-a050-c96ef01758ba");
        boolean result = true;
        writeline("/home/ubuntu/results/coverage/Digester/Digester_5_10.coverage", "a8ee8d61-f048-4794-afeb-5bcc35136933");
        Stack<Object> namedStack = stacksByName.get(stackName);
        writeline("/home/ubuntu/results/coverage/Digester/Digester_5_10.coverage", "ff545ede-d5c0-48a6-98a8-a924f9ee88f4");
        if (namedStack != null) {
            writeline("/home/ubuntu/results/coverage/Digester/Digester_5_10.coverage", "911dff17-95dd-4634-a9c2-95e93c54fbd9");
            result = namedStack.isEmpty();
        }
        writeline("/home/ubuntu/results/coverage/Digester/Digester_5_10.coverage", "0899594b-254c-452b-9e27-7017f6212f91");
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
        writeline("/home/ubuntu/results/coverage/Digester/Digester_5_10.coverage", "7c3f63ef-f677-4fc7-b9b2-5361cbe1f47d");
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
        writeline("/home/ubuntu/results/coverage/Digester/Digester_5_10.coverage", "860701fa-b1de-4b04-a432-a98ac5b5c95b");
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
        writeline("/home/ubuntu/results/coverage/Digester/Digester_5_10.coverage", "e80af344-10d2-467e-9b7c-1a89c40ffa30");
        for (InputSource source : inputSources) {
            writeline("/home/ubuntu/results/coverage/Digester/Digester_5_10.coverage", "2723d33a-4eb4-4eaa-b715-f1bb72b870e6");
            try {
                writeline("/home/ubuntu/results/coverage/Digester/Digester_5_10.coverage", "6a0a4540-2052-4dc5-96d5-fa24bb1bb14b");
                source.getByteStream().close();
            } catch (IOException e) {
                writeline("/home/ubuntu/results/coverage/Digester/Digester_5_10.coverage", "2442e954-84d0-4b0e-8fb3-6fe0b60bcb78");
                if (log.isWarnEnabled()) {
                    writeline("/home/ubuntu/results/coverage/Digester/Digester_5_10.coverage", "b073fc59-0ee0-4526-b1aa-e0ba235ea3d8");
                    log.warn(format("An error occurred while closing resource %s (%s)", source.getPublicId(), source.getSystemId()), e);
                }
            }
        }
        writeline("/home/ubuntu/results/coverage/Digester/Digester_5_10.coverage", "57867505-5129-4050-b1bf-b0f8e844d08e");
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
        writeline("/home/ubuntu/results/coverage/Digester/Digester_5_10.coverage", "0b9492b3-2281-42fd-a9df-52cb2b75c8dc");
        if (configured) {
            writeline("/home/ubuntu/results/coverage/Digester/Digester_5_10.coverage", "7b0a0c27-14bb-4277-bd9e-a3183429bb72");
            return;
        }
        writeline("/home/ubuntu/results/coverage/Digester/Digester_5_10.coverage", "f22def75-269f-4df9-945b-0c8cdc5294df");
        initialize();
        writeline("/home/ubuntu/results/coverage/Digester/Digester_5_10.coverage", "036c721a-924d-4897-9684-758407a8ad9b");
        configured = true;
    }

    /**
     * Checks the Digester instance has been configured.
     *
     * @return true, if the Digester instance has been configured, false otherwise
     * @since 3.0
     */
    public boolean isConfigured() {
        writeline("/home/ubuntu/results/coverage/Digester/Digester_5_10.coverage", "ed019fdd-5f98-4051-aa9a-603396af65c0");
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
        writeline("/home/ubuntu/results/coverage/Digester/Digester_5_10.coverage", "0955c334-250c-4e61-be5d-d11b1a104564");
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
        writeline("/home/ubuntu/results/coverage/Digester/Digester_5_10.coverage", "ace3f9ef-1984-4a00-9d3e-09dd36832562");
        try {
            writeline("/home/ubuntu/results/coverage/Digester/Digester_5_10.coverage", "460570af-b22c-42a7-bf62-7332d6033df5");
            return (params.peek());
        } catch (EmptyStackException e) {
            writeline("/home/ubuntu/results/coverage/Digester/Digester_5_10.coverage", "bce3e706-d8b0-402a-99f4-81ce07ee0032");
            log.warn("Empty stack (returning null)");
            writeline("/home/ubuntu/results/coverage/Digester/Digester_5_10.coverage", "c5bc24cc-9742-43b5-8cac-992b58ab9074");
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
        writeline("/home/ubuntu/results/coverage/Digester/Digester_5_10.coverage", "ec84fe88-89c9-4287-9f88-c63c3c5fd63a");
        int index = (params.size() - 1) - n;
        writeline("/home/ubuntu/results/coverage/Digester/Digester_5_10.coverage", "a93f1fe1-04c2-4c4b-bd4a-85c33ad39ce0");
        if (index < 0) {
            writeline("/home/ubuntu/results/coverage/Digester/Digester_5_10.coverage", "68da1eed-4ba1-4fa5-8123-158efc69c203");
            log.warn("Empty stack (returning null)");
            writeline("/home/ubuntu/results/coverage/Digester/Digester_5_10.coverage", "fdbcdf74-eeeb-44e6-9b04-10b2f8e1696f");
            return (null);
        }
        writeline("/home/ubuntu/results/coverage/Digester/Digester_5_10.coverage", "12f47874-4dbb-47fd-824d-16247ba58733");
        try {
            writeline("/home/ubuntu/results/coverage/Digester/Digester_5_10.coverage", "362319a0-35ea-4578-b99e-6a7292d0951a");
            return (params.get(index));
        } catch (EmptyStackException e) {
            writeline("/home/ubuntu/results/coverage/Digester/Digester_5_10.coverage", "65440aaa-2003-4d2c-97e6-4395686bd6ab");
            log.warn("Empty stack (returning null)");
            writeline("/home/ubuntu/results/coverage/Digester/Digester_5_10.coverage", "f298944f-77c9-46b4-9790-894f4e224072");
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
        writeline("/home/ubuntu/results/coverage/Digester/Digester_5_10.coverage", "4fb90949-1cfb-486a-a89f-c72824173fb4");
        try {
            writeline("/home/ubuntu/results/coverage/Digester/Digester_5_10.coverage", "28c2c46a-b125-4dca-8832-eb39308599d7");
            if (log.isTraceEnabled()) {
                writeline("/home/ubuntu/results/coverage/Digester/Digester_5_10.coverage", "9344b68b-7d14-4d23-bb10-b2e8e8f98c77");
                log.trace("Popping params");
            }
            writeline("/home/ubuntu/results/coverage/Digester/Digester_5_10.coverage", "1579d664-fc8f-4e2c-8c5b-cc055cd7fa45");
            return (params.pop());
        } catch (EmptyStackException e) {
            writeline("/home/ubuntu/results/coverage/Digester/Digester_5_10.coverage", "46ed506d-fd8b-4fbe-8976-d375d046225e");
            log.warn("Empty stack (returning null)");
            writeline("/home/ubuntu/results/coverage/Digester/Digester_5_10.coverage", "64898b10-06c4-4e7a-9b5f-5c2eb78a62af");
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
        writeline("/home/ubuntu/results/coverage/Digester/Digester_5_10.coverage", "c452bf80-8dcb-456d-b6c6-e304f90c7921");
        if (log.isTraceEnabled()) {
            writeline("/home/ubuntu/results/coverage/Digester/Digester_5_10.coverage", "5b904d64-a131-474e-963e-942fb81a7fcd");
            log.trace("Pushing params");
        }
        writeline("/home/ubuntu/results/coverage/Digester/Digester_5_10.coverage", "6e298c0c-f5e3-46f8-b3fd-397a8ab6fa24");
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
        writeline("/home/ubuntu/results/coverage/Digester/Digester_5_10.coverage", "371793c1-402a-48f4-bc18-cbf2c9fa0e81");
        if ((e != null) && (e instanceof InvocationTargetException)) {
            writeline("/home/ubuntu/results/coverage/Digester/Digester_5_10.coverage", "a6a72403-d509-412b-b4c2-a915aec7332d");
            Throwable t = ((InvocationTargetException) e).getTargetException();
            writeline("/home/ubuntu/results/coverage/Digester/Digester_5_10.coverage", "70a1b297-e2ec-4847-848d-4e20cde18c5f");
            if ((t != null) && (t instanceof Exception)) {
                writeline("/home/ubuntu/results/coverage/Digester/Digester_5_10.coverage", "5ad27f42-138c-4d50-8e2d-f598dac8569c");
                e = (Exception) t;
            }
        }
        writeline("/home/ubuntu/results/coverage/Digester/Digester_5_10.coverage", "e90f7625-beed-456c-b7a2-79b72964e135");
        if (locator != null) {
            writeline("/home/ubuntu/results/coverage/Digester/Digester_5_10.coverage", "9fd09be1-50ab-42f7-8fb5-bead7f905273");
            String error = "Error at line " + locator.getLineNumber() + " char " + locator.getColumnNumber() + ": " + message;
            writeline("/home/ubuntu/results/coverage/Digester/Digester_5_10.coverage", "b7be6324-6900-40ea-9140-d100b8788dd7");
            if (e != null) {
                writeline("/home/ubuntu/results/coverage/Digester/Digester_5_10.coverage", "47dc5918-565e-4f05-9ccf-ee3fcd0c3f2d");
                return new SAXParseException(error, locator, e);
            }
            writeline("/home/ubuntu/results/coverage/Digester/Digester_5_10.coverage", "1b98be87-57bd-40df-851b-9bf4db80f2eb");
            return new SAXParseException(error, locator);
        }
        writeline("/home/ubuntu/results/coverage/Digester/Digester_5_10.coverage", "b2030bab-531e-4cd6-9fde-461994fb95da");
        log.error("No Locator!");
        writeline("/home/ubuntu/results/coverage/Digester/Digester_5_10.coverage", "9141380e-2b22-488a-b285-dbb5bd4e50b9");
        if (e != null) {
            writeline("/home/ubuntu/results/coverage/Digester/Digester_5_10.coverage", "25f287bf-1b95-45de-b56c-6e6b5cac4c8e");
            return new SAXException(message, e);
        }
        writeline("/home/ubuntu/results/coverage/Digester/Digester_5_10.coverage", "48d19f5f-8571-4385-8739-a6fe6c9e96ff");
        return new SAXException(message);
    }

    /**
     * Create a SAX exception which also understands about the location in the digester file where the exception occurs
     *
     * @param e the exception cause
     * @return the new SAX exception
     */
    public SAXException createSAXException(Exception e) {
        writeline("/home/ubuntu/results/coverage/Digester/Digester_5_10.coverage", "4e7a50ba-fd11-4156-b18e-6c79aa04fba8");
        if (e instanceof InvocationTargetException) {
            writeline("/home/ubuntu/results/coverage/Digester/Digester_5_10.coverage", "357df58a-82ba-4e8f-b413-c524ed9c450a");
            Throwable t = ((InvocationTargetException) e).getTargetException();
            writeline("/home/ubuntu/results/coverage/Digester/Digester_5_10.coverage", "4f2b3d7f-d88f-4dc7-a2ce-871919ac5991");
            if ((t != null) && (t instanceof Exception)) {
                writeline("/home/ubuntu/results/coverage/Digester/Digester_5_10.coverage", "cd0dade4-163d-4ea9-a937-4bc84a8cb86b");
                e = (Exception) t;
            }
        }
        writeline("/home/ubuntu/results/coverage/Digester/Digester_5_10.coverage", "2e38cf5b-58b3-4567-85c8-c2b245f0a222");
        return createSAXException(e.getMessage(), e);
    }

    /**
     * Create a SAX exception which also understands about the location in the digester file where the exception occurs
     *
     * @param message the custom SAX exception message
     * @return the new SAX exception
     */
    public SAXException createSAXException(String message) {
        writeline("/home/ubuntu/results/coverage/Digester/Digester_5_10.coverage", "fe36cb85-4196-4797-9d9d-d121f4c72420");
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
        writeline("/home/ubuntu/results/coverage/Digester/Digester_5_10.coverage", "32a40ace-7a47-4cc4-b403-39acda246590");
        if (obj == null) {
            writeline("/home/ubuntu/results/coverage/Digester/Digester_5_10.coverage", "57b50d2d-9eae-4a78-9bdf-9b396b9c0f7e");
            return null;
        }
        writeline("/home/ubuntu/results/coverage/Digester/Digester_5_10.coverage", "0b7ca518-37fd-4049-a620-cdbce65a40c7");
        @SuppressWarnings("unchecked") T result = (T) obj;
        writeline("/home/ubuntu/results/coverage/Digester/Digester_5_10.coverage", "288ccd45-b0ce-42d4-84b2-5ccafab02247");
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
