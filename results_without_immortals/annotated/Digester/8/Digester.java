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
        writeline("/home/ubuntu/results/coverage/Digester/Digester_8_10.coverage", "72f2e5d4-c572-4260-abe8-d5022c06b1ec");
        Stack<String> nsStack = namespaces.get(prefix);
        writeline("/home/ubuntu/results/coverage/Digester/Digester_8_10.coverage", "b87c0d64-6cc4-4220-a88c-ba4a33270bef");
        if (nsStack == null) {
            writeline("/home/ubuntu/results/coverage/Digester/Digester_8_10.coverage", "021fdc8d-abd4-472e-856a-59358dada04a");
            return null;
        }
        writeline("/home/ubuntu/results/coverage/Digester/Digester_8_10.coverage", "b9a18da7-6041-4d9d-b805-d9b4c5c19109");
        try {
            writeline("/home/ubuntu/results/coverage/Digester/Digester_8_10.coverage", "394a2f00-ff15-4a7d-a00b-dca0437d7c8a");
            return (nsStack.peek());
        } catch (EmptyStackException e) {
            writeline("/home/ubuntu/results/coverage/Digester/Digester_8_10.coverage", "497c7d16-2fdf-457b-8c38-dd0b46c13608");
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
        writeline("/home/ubuntu/results/coverage/Digester/Digester_8_10.coverage", "6376379a-b1d7-42eb-aed8-7024cb69490e");
        if (this.classLoader != null) {
            writeline("/home/ubuntu/results/coverage/Digester/Digester_8_10.coverage", "f8d798ae-5f8d-40be-961f-7f9e1edefe2d");
            return (this.classLoader);
        }
        writeline("/home/ubuntu/results/coverage/Digester/Digester_8_10.coverage", "18310ac5-9a81-4f52-b8d0-d82ea25f170f");
        if (this.useContextClassLoader) {
            writeline("/home/ubuntu/results/coverage/Digester/Digester_8_10.coverage", "9f86b693-a37b-413a-85fb-a3ed5ba010e8");
            ClassLoader classLoader = Thread.currentThread().getContextClassLoader();
            writeline("/home/ubuntu/results/coverage/Digester/Digester_8_10.coverage", "d5d6278e-c469-44fb-8809-56e55f7ae725");
            if (classLoader != null) {
                writeline("/home/ubuntu/results/coverage/Digester/Digester_8_10.coverage", "f3f50607-49f8-4a5d-baa0-409f74cf652f");
                return (classLoader);
            }
        }
        writeline("/home/ubuntu/results/coverage/Digester/Digester_8_10.coverage", "61310b80-d376-482e-9c02-64e4765c431d");
        return (this.getClass().getClassLoader());
    }

    /**
     * Set the class loader to be used for instantiating application objects when required.
     *
     * @param classLoader The new class loader to use, or <code>null</code> to revert to the standard rules
     */
    public void setClassLoader(ClassLoader classLoader) {
        writeline("/home/ubuntu/results/coverage/Digester/Digester_8_10.coverage", "8f9f3719-065d-402e-a458-e64dedd223d4");
        this.classLoader = classLoader;
    }

    /**
     * Return the current depth of the element stack.
     *
     * @return the current depth of the element stack.
     */
    public int getCount() {
        writeline("/home/ubuntu/results/coverage/Digester/Digester_8_10.coverage", "c4a142db-2adc-4d0c-b345-4077406b3199");
        return (stack.size());
    }

    /**
     * Return the name of the XML element that is currently being processed.
     *
     * @return the name of the XML element that is currently being processed.
     */
    public String getCurrentElementName() {
        writeline("/home/ubuntu/results/coverage/Digester/Digester_8_10.coverage", "f9461abe-e45d-4bee-8123-4afd2156bce9");
        String elementName = match;
        writeline("/home/ubuntu/results/coverage/Digester/Digester_8_10.coverage", "b4c4068a-2fb4-4862-9c82-eeff6fdd74fe");
        int lastSlash = elementName.lastIndexOf('/');
        writeline("/home/ubuntu/results/coverage/Digester/Digester_8_10.coverage", "b8ca6ab1-5a91-4dfc-ad73-c07da5bbfb2a");
        if (lastSlash >= 0) {
            writeline("/home/ubuntu/results/coverage/Digester/Digester_8_10.coverage", "8b7849d3-f129-4b21-969f-fc32c5f08958");
            elementName = elementName.substring(lastSlash + 1);
        }
        writeline("/home/ubuntu/results/coverage/Digester/Digester_8_10.coverage", "b53f04fa-8d72-4f1a-a875-111de8956b17");
        return (elementName);
    }

    /**
     * Return the error handler for this Digester.
     *
     * @return the error handler for this Digester.
     */
    public ErrorHandler getErrorHandler() {
        writeline("/home/ubuntu/results/coverage/Digester/Digester_8_10.coverage", "e45e6ae9-a13f-4dea-ad0c-bf1e365a0ced");
        return (this.errorHandler);
    }

    /**
     * Set the error handler for this Digester.
     *
     * @param errorHandler The new error handler
     */
    public void setErrorHandler(ErrorHandler errorHandler) {
        writeline("/home/ubuntu/results/coverage/Digester/Digester_8_10.coverage", "6998aebd-e695-42f2-bb41-1c544d14833d");
        this.errorHandler = errorHandler;
    }

    /**
     * Return the SAXParserFactory we will use, creating one if necessary.
     *
     * @return the SAXParserFactory we will use, creating one if necessary.
     */
    public SAXParserFactory getFactory() {
        writeline("/home/ubuntu/results/coverage/Digester/Digester_8_10.coverage", "497372ee-6a6a-4abb-b74e-ba0d51927ab2");
        if (factory == null) {
            writeline("/home/ubuntu/results/coverage/Digester/Digester_8_10.coverage", "c2c7a997-acec-4d36-8da2-80934e35c464");
            factory = SAXParserFactory.newInstance();
            writeline("/home/ubuntu/results/coverage/Digester/Digester_8_10.coverage", "12170dfe-b5b3-49b8-834e-2a2b361075e3");
            factory.setNamespaceAware(namespaceAware);
            writeline("/home/ubuntu/results/coverage/Digester/Digester_8_10.coverage", "a765f041-8730-4eac-b812-39cb5366e793");
            factory.setXIncludeAware(xincludeAware);
            writeline("/home/ubuntu/results/coverage/Digester/Digester_8_10.coverage", "c1b39e78-e06e-44fc-8dbf-ecc6db9040bb");
            factory.setValidating(validating);
            writeline("/home/ubuntu/results/coverage/Digester/Digester_8_10.coverage", "f09ee2c3-258d-41db-9fee-a0aa254e4160");
            factory.setSchema(schema);
        }
        writeline("/home/ubuntu/results/coverage/Digester/Digester_8_10.coverage", "b871db36-4ba3-4ede-a1bf-c2b9d3d15e02");
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
        writeline("/home/ubuntu/results/coverage/Digester/Digester_8_10.coverage", "5e4bf4e8-0343-486e-bbb1-1c16a3fdaab1");
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
        writeline("/home/ubuntu/results/coverage/Digester/Digester_8_10.coverage", "bf804eac-25dd-446a-97ed-98cc992fa8e2");
        getFactory().setFeature(feature, value);
    }

    /**
     * Return the current Logger associated with this instance of the Digester
     *
     * @return the current Logger associated with this instance of the Digester
     */
    public Log getLogger() {
        writeline("/home/ubuntu/results/coverage/Digester/Digester_8_10.coverage", "ca43ec3f-1094-4962-91f7-6e517bd1c33c");
        return log;
    }

    /**
     * Set the current logger for this Digester.
     *
     * @param log the current logger for this Digester.
     */
    public void setLogger(Log log) {
        writeline("/home/ubuntu/results/coverage/Digester/Digester_8_10.coverage", "d59632fa-7fcf-457b-b0e7-c05c2a401360");
        this.log = log;
    }

    /**
     * Gets the logger used for logging SAX-related information. <strong>Note</strong> the output is finely grained.
     *
     * @return the logger used for logging SAX-related information
     * @since 1.6
     */
    public Log getSAXLogger() {
        writeline("/home/ubuntu/results/coverage/Digester/Digester_8_10.coverage", "8c15917b-0d21-4667-97da-dc4097fec671");
        return saxLog;
    }

    /**
     * Sets the logger used for logging SAX-related information. <strong>Note</strong> the output is finely grained.
     *
     * @param saxLog the logger used for logging SAX-related information, not null
     * @since 1.6
     */
    public void setSAXLogger(Log saxLog) {
        writeline("/home/ubuntu/results/coverage/Digester/Digester_8_10.coverage", "d9ebc632-2250-4116-8ec5-6e6e1321711b");
        this.saxLog = saxLog;
    }

    /**
     * Return the current rule match path
     *
     * @return the current rule match path
     */
    public String getMatch() {
        writeline("/home/ubuntu/results/coverage/Digester/Digester_8_10.coverage", "a1807fcb-9cbe-4b29-8561-1f7f6a69184a");
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
        writeline("/home/ubuntu/results/coverage/Digester/Digester_8_10.coverage", "d741be48-f0d3-4700-8cb7-ed90e8c8b383");
        return matches;
    }

    /**
     * Return the "namespace aware" flag for parsers we create.
     *
     * @return the "namespace aware" flag for parsers we create.
     */
    public boolean getNamespaceAware() {
        writeline("/home/ubuntu/results/coverage/Digester/Digester_8_10.coverage", "1c33af37-b503-4731-af2a-52540e18bbb3");
        return (this.namespaceAware);
    }

    /**
     * Set the "namespace aware" flag for parsers we create.
     *
     * @param namespaceAware The new "namespace aware" flag
     */
    public void setNamespaceAware(boolean namespaceAware) {
        writeline("/home/ubuntu/results/coverage/Digester/Digester_8_10.coverage", "1a6714d0-6148-4ea6-b100-c2a42261ab41");
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
        writeline("/home/ubuntu/results/coverage/Digester/Digester_8_10.coverage", "904ef384-8406-48b4-8240-4af0b785f025");
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
        writeline("/home/ubuntu/results/coverage/Digester/Digester_8_10.coverage", "6c3ece4c-7c31-49ca-a1ca-c3c2a843814d");
        this.xincludeAware = xincludeAware;
    }

    /**
     * Set the public id of the current file being parse.
     *
     * @param publicId the DTD/Schema public's id.
     */
    public void setPublicId(String publicId) {
        writeline("/home/ubuntu/results/coverage/Digester/Digester_8_10.coverage", "19ac6bb6-82e3-44d0-8677-043dd49a5550");
        this.publicId = publicId;
    }

    /**
     * Return the public identifier of the DTD we are currently parsing under, if any.
     *
     * @return the public identifier of the DTD we are currently parsing under, if any.
     */
    public String getPublicId() {
        writeline("/home/ubuntu/results/coverage/Digester/Digester_8_10.coverage", "1920ec51-f9d7-47eb-ab1c-7c781e28f747");
        return (this.publicId);
    }

    /**
     * Return the namespace URI that will be applied to all subsequently added <code>Rule</code> objects.
     *
     * @return the namespace URI that will be applied to all subsequently added <code>Rule</code> objects.
     */
    public String getRuleNamespaceURI() {
        writeline("/home/ubuntu/results/coverage/Digester/Digester_8_10.coverage", "e69e380e-1638-4b1f-bea2-73995fb99bf9");
        return (getRules().getNamespaceURI());
    }

    /**
     * Set the namespace URI that will be applied to all subsequently added <code>Rule</code> objects.
     *
     * @param ruleNamespaceURI Namespace URI that must match on all subsequently added rules, or <code>null</code> for
     *            matching regardless of the current namespace URI
     */
    public void setRuleNamespaceURI(String ruleNamespaceURI) {
        writeline("/home/ubuntu/results/coverage/Digester/Digester_8_10.coverage", "26ba33dd-b022-4574-9f29-8500c9a55706");
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
        writeline("/home/ubuntu/results/coverage/Digester/Digester_8_10.coverage", "53998da7-5438-4bd2-bd7b-bbcaa6281a25");
        if (parser != null) {
            writeline("/home/ubuntu/results/coverage/Digester/Digester_8_10.coverage", "76437572-68ce-4e83-81f9-d0bc04296d91");
            return (parser);
        }
        writeline("/home/ubuntu/results/coverage/Digester/Digester_8_10.coverage", "0e3285eb-0f65-40d8-bb6f-f79978d73322");
        try {
            writeline("/home/ubuntu/results/coverage/Digester/Digester_8_10.coverage", "1197e698-94ef-409a-8e30-3dd119b5222d");
            parser = getFactory().newSAXParser();
        } catch (Exception e) {
            writeline("/home/ubuntu/results/coverage/Digester/Digester_8_10.coverage", "18aed410-7d7b-423c-8af7-dfbd324f4ddb");
            log.error("Digester.getParser: ", e);
            writeline("/home/ubuntu/results/coverage/Digester/Digester_8_10.coverage", "650859f1-8eef-4994-9ee0-8be1a4541fda");
            return (null);
        }
        writeline("/home/ubuntu/results/coverage/Digester/Digester_8_10.coverage", "05ae4ed2-bbd9-453e-b263-37ecadf7e952");
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
        writeline("/home/ubuntu/results/coverage/Digester/Digester_8_10.coverage", "d904763f-e4d5-4bb5-bb59-76a926318766");
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
        writeline("/home/ubuntu/results/coverage/Digester/Digester_8_10.coverage", "27736635-1e59-484d-97cc-0428a4be3382");
        getParser().setProperty(property, value);
    }

    /**
     * Return the <code>Rules</code> implementation object containing our rules collection and associated matching
     * policy. If none has been established, a default implementation will be created and returned.
     *
     * @return the <code>Rules</code> implementation object.
     */
    public Rules getRules() {
        writeline("/home/ubuntu/results/coverage/Digester/Digester_8_10.coverage", "67c3262c-a95d-4ea0-82cb-ff902585308d");
        if (this.rules == null) {
            writeline("/home/ubuntu/results/coverage/Digester/Digester_8_10.coverage", "f4301d2e-a8d7-4424-9b6c-a8552ba6504d");
            this.rules = new RulesBase();
            writeline("/home/ubuntu/results/coverage/Digester/Digester_8_10.coverage", "ec550f20-6241-457f-8c63-cc2754f5ff74");
            this.rules.setDigester(this);
        }
        writeline("/home/ubuntu/results/coverage/Digester/Digester_8_10.coverage", "45ec7c10-7d73-4dd5-94f7-535619c231b0");
        return (this.rules);
    }

    /**
     * Set the <code>Rules</code> implementation object containing our rules collection and associated matching policy.
     *
     * @param rules New Rules implementation
     */
    public void setRules(Rules rules) {
        writeline("/home/ubuntu/results/coverage/Digester/Digester_8_10.coverage", "dbdff3a5-a974-4bf2-a1f7-a571ddf7e8ac");
        this.rules = rules;
        writeline("/home/ubuntu/results/coverage/Digester/Digester_8_10.coverage", "19e6d6ed-9eef-4d37-95d9-dbaf553f7c25");
        this.rules.setDigester(this);
    }

    /**
     * Return the XML Schema used when parsing.
     *
     * @return The {@link Schema} instance in use.
     * @since 2.0
     */
    public Schema getXMLSchema() {
        writeline("/home/ubuntu/results/coverage/Digester/Digester_8_10.coverage", "5bc555d5-50ae-42b8-8842-54b83946320e");
        return (this.schema);
    }

    /**
     * Set the XML Schema to be used when parsing.
     *
     * @param schema The {@link Schema} instance to use.
     * @since 2.0
     */
    public void setXMLSchema(Schema schema) {
        writeline("/home/ubuntu/results/coverage/Digester/Digester_8_10.coverage", "85414670-daf8-4322-86f2-da22f77e11b6");
        this.schema = schema;
    }

    /**
     * Return the boolean as to whether the context ClassLoader should be used.
     *
     * @return true, if the context ClassLoader should be used, false otherwise.
     */
    public boolean getUseContextClassLoader() {
        writeline("/home/ubuntu/results/coverage/Digester/Digester_8_10.coverage", "305256a0-7b80-4820-b8a4-7d4eeeb7be6f");
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
        writeline("/home/ubuntu/results/coverage/Digester/Digester_8_10.coverage", "1b1d257c-7ac7-41c9-add5-8254c73a1f00");
        useContextClassLoader = use;
    }

    /**
     * Return the validating parser flag.
     *
     * @return the validating parser flag.
     */
    public boolean getValidating() {
        writeline("/home/ubuntu/results/coverage/Digester/Digester_8_10.coverage", "89804e3f-50ab-41cb-8529-8e560c39884e");
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
        writeline("/home/ubuntu/results/coverage/Digester/Digester_8_10.coverage", "b394a08b-84c8-468a-8d3c-0ec864831e58");
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
        writeline("/home/ubuntu/results/coverage/Digester/Digester_8_10.coverage", "431993ee-a115-4131-8b9f-91f512bf443c");
        if (reader == null) {
            writeline("/home/ubuntu/results/coverage/Digester/Digester_8_10.coverage", "8cf3147b-d2fb-426a-bcc6-d403e8e81575");
            reader = getParser().getXMLReader();
        }
        writeline("/home/ubuntu/results/coverage/Digester/Digester_8_10.coverage", "af5ab738-33ca-470d-be8e-c48352c50987");
        reader.setDTDHandler(this);
        writeline("/home/ubuntu/results/coverage/Digester/Digester_8_10.coverage", "6594db16-4777-4cd6-bad9-49621d3c5dbd");
        reader.setContentHandler(this);
        writeline("/home/ubuntu/results/coverage/Digester/Digester_8_10.coverage", "fdca6fcf-5ab1-4c00-ac20-d6a9faf11727");
        if (entityResolver == null) {
            writeline("/home/ubuntu/results/coverage/Digester/Digester_8_10.coverage", "87769b61-35ce-4bd7-8019-3ce335a732bf");
            reader.setEntityResolver(this);
        } else {
            writeline("/home/ubuntu/results/coverage/Digester/Digester_8_10.coverage", "4aeb2f4a-6c0c-481e-97e8-2022b5d6dffe");
            reader.setEntityResolver(entityResolver);
        }
        writeline("/home/ubuntu/results/coverage/Digester/Digester_8_10.coverage", "be2e63e5-b380-4af5-8f5c-1433e2651a99");
        if (this.errorHandler != null) {
            writeline("/home/ubuntu/results/coverage/Digester/Digester_8_10.coverage", "14695eb2-b05f-4eb1-927b-500a6a30c0b2");
            reader.setErrorHandler(this.errorHandler);
        } else {
            writeline("/home/ubuntu/results/coverage/Digester/Digester_8_10.coverage", "954221c3-0601-48d4-b2d7-53a7ce15e77e");
            reader.setErrorHandler(this);
        }
        writeline("/home/ubuntu/results/coverage/Digester/Digester_8_10.coverage", "0ee83552-9d06-490d-8cc5-8931804ad584");
        return reader;
    }

    /**
     * Gets the <code>Substitutor</code> used to convert attributes and body text.
     *
     * @return the <code>Substitutor</code> used to convert attributes and body text,
     *         null if not substitutions are to be performed.
     */
    public Substitutor getSubstitutor() {
        writeline("/home/ubuntu/results/coverage/Digester/Digester_8_10.coverage", "43dc1877-a47b-457d-a3a3-fb31859c1545");
        return substitutor;
    }

    /**
     * Sets the <code>Substitutor</code> to be used to convert attributes and body text.
     *
     * @param substitutor the Substitutor to be used to convert attributes and body text or null if not substitution of
     *            these values is to be performed.
     */
    public void setSubstitutor(Substitutor substitutor) {
        writeline("/home/ubuntu/results/coverage/Digester/Digester_8_10.coverage", "64a10897-5821-4980-a66b-caa40f0afabf");
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
        writeline("/home/ubuntu/results/coverage/Digester/Digester_8_10.coverage", "e4c63d69-396f-4fea-9dea-85a6429afe20");
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
        writeline("/home/ubuntu/results/coverage/Digester/Digester_8_10.coverage", "dc00a7c3-b31e-4e18-a014-89d030ed20c1");
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
        writeline("/home/ubuntu/results/coverage/Digester/Digester_8_10.coverage", "9e705c8e-2904-489b-8d19-85972d04330b");
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
        writeline("/home/ubuntu/results/coverage/Digester/Digester_8_10.coverage", "c49c3717-820d-43c2-b98d-e5fc6ead7406");
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
        writeline("/home/ubuntu/results/coverage/Digester/Digester_8_10.coverage", "bd571e60-2cf9-46ca-a924-b50be060dd9f");
        if (!namespaceAware) {
            writeline("/home/ubuntu/results/coverage/Digester/Digester_8_10.coverage", "dd225939-0946-4ccd-ae29-a1bf0742bafe");
            log.warn("Digester is not namespace aware");
        }
        writeline("/home/ubuntu/results/coverage/Digester/Digester_8_10.coverage", "dc5ed8d6-b68f-4745-ab13-bb5ffcc97cdc");
        Map<String, String> currentNamespaces = new HashMap<String, String>();
        writeline("/home/ubuntu/results/coverage/Digester/Digester_8_10.coverage", "4ef73f88-4f7e-4045-9645-c9cf98d22099");
        return currentNamespaces;
    }

    /**
     * Returns the executor service used to run asynchronous parse method.
     *
     * @return the executor service used to run asynchronous parse method
     * @since 3.1
     */
    public ExecutorService getExecutorService() {
        writeline("/home/ubuntu/results/coverage/Digester/Digester_8_10.coverage", "53fc29db-67c8-4c06-a523-ccf6819e0b38");
        return executorService;
    }

    /**
     * Sets the executor service to run asynchronous parse method.
     *
     * @param executorService the executor service to run asynchronous parse method
     * @since 3.1
     */
    public void setExecutorService(ExecutorService executorService) {
        writeline("/home/ubuntu/results/coverage/Digester/Digester_8_10.coverage", "b39b7e51-e03d-4157-b12a-44b51cec4eb9");
        this.executorService = executorService;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public void characters(char buffer[], int start, int length) throws SAXException {
        writeline("/home/ubuntu/results/coverage/Digester/Digester_8_10.coverage", "ed5080ef-66bd-4fbf-8802-93bfa82312e7");
        if (customContentHandler != null) {
            writeline("/home/ubuntu/results/coverage/Digester/Digester_8_10.coverage", "31ad80d5-7524-46b6-b561-26ebce5b1eac");
            customContentHandler.characters(buffer, start, length);
            writeline("/home/ubuntu/results/coverage/Digester/Digester_8_10.coverage", "cbd7493a-056f-44e0-9ea2-ae5436a6c5d0");
            return;
        }
        writeline("/home/ubuntu/results/coverage/Digester/Digester_8_10.coverage", "543c622f-7a1f-4bcf-b9da-2f462f9fe558");
        if (saxLog.isDebugEnabled()) {
            writeline("/home/ubuntu/results/coverage/Digester/Digester_8_10.coverage", "8316fada-4428-4a07-b506-d073f86e36bb");
            saxLog.debug("characters(" + new String(buffer, start, length) + ")");
        }
        writeline("/home/ubuntu/results/coverage/Digester/Digester_8_10.coverage", "27160eda-ebcd-4f6c-85f4-144f1a7665ac");
        bodyText.append(buffer, start, length);
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public void endDocument() throws SAXException {
        writeline("/home/ubuntu/results/coverage/Digester/Digester_8_10.coverage", "3a76af79-ed71-4cc3-b8ba-cf4a0ad7867d");
        if (saxLog.isDebugEnabled()) {
            writeline("/home/ubuntu/results/coverage/Digester/Digester_8_10.coverage", "24ede7c8-e59e-4f1e-b4d4-69f466239234");
            if (getCount() > 1) {
                writeline("/home/ubuntu/results/coverage/Digester/Digester_8_10.coverage", "783db7d6-6e82-41e5-8048-10d5d97e126a");
                saxLog.debug("endDocument():  " + getCount() + " elements left");
            } else {
                writeline("/home/ubuntu/results/coverage/Digester/Digester_8_10.coverage", "af491d43-339b-4f0c-ad23-3558163111a5");
                saxLog.debug("endDocument()");
            }
        }
        writeline("/home/ubuntu/results/coverage/Digester/Digester_8_10.coverage", "3a5c219d-4491-4d0f-bb86-41a27408b382");
        clear();
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public void endElement(String namespaceURI, String localName, String qName) throws SAXException {
        writeline("/home/ubuntu/results/coverage/Digester/Digester_8_10.coverage", "86c4fb0d-f13b-4a19-b5e1-ce317dcbad02");
        if (customContentHandler != null) {
            writeline("/home/ubuntu/results/coverage/Digester/Digester_8_10.coverage", "395a3fad-0cf9-45fb-afab-f502f13e274b");
            customContentHandler.endElement(namespaceURI, localName, qName);
            writeline("/home/ubuntu/results/coverage/Digester/Digester_8_10.coverage", "826b5569-2415-4ea3-8494-f509864d1675");
            return;
        }
        writeline("/home/ubuntu/results/coverage/Digester/Digester_8_10.coverage", "f3297e0d-0327-4a75-bb18-882390d48370");
        boolean debug = log.isDebugEnabled();
        writeline("/home/ubuntu/results/coverage/Digester/Digester_8_10.coverage", "aeb82d93-106f-4247-b48f-c967b03ecccc");
        if (debug) {
            writeline("/home/ubuntu/results/coverage/Digester/Digester_8_10.coverage", "071ebf73-6144-4413-befd-add440675929");
            if (saxLog.isDebugEnabled()) {
                writeline("/home/ubuntu/results/coverage/Digester/Digester_8_10.coverage", "5003901a-119e-4778-80d5-c966d05d750d");
                saxLog.debug("endElement(" + namespaceURI + "," + localName + "," + qName + ")");
            }
            writeline("/home/ubuntu/results/coverage/Digester/Digester_8_10.coverage", "46abf404-e051-4792-a428-3206a5257221");
            log.debug("  match='" + match + "'");
            writeline("/home/ubuntu/results/coverage/Digester/Digester_8_10.coverage", "73f55a37-0f68-4f63-90f8-6d838f453884");
            log.debug("  bodyText='" + bodyText + "'");
        }
        writeline("/home/ubuntu/results/coverage/Digester/Digester_8_10.coverage", "f2001f43-730e-4d27-83d2-bd6506e8254f");
        String name = localName;
        writeline("/home/ubuntu/results/coverage/Digester/Digester_8_10.coverage", "563b3bbd-6b48-4537-87cb-d1cb4747a648");
        if ((name == null) || (name.length() < 1)) {
            writeline("/home/ubuntu/results/coverage/Digester/Digester_8_10.coverage", "9c826ef3-619d-4f9c-8d02-3372531f7c50");
            name = qName;
        }
        writeline("/home/ubuntu/results/coverage/Digester/Digester_8_10.coverage", "2bcce982-08d4-43ae-852a-290a1b052ca2");
        List<Rule> rules = matches.pop();
        writeline("/home/ubuntu/results/coverage/Digester/Digester_8_10.coverage", "0e850da0-dafe-4c2f-b23b-ed7654bad1ae");
        if ((rules != null) && (rules.size() > 0)) {
            writeline("/home/ubuntu/results/coverage/Digester/Digester_8_10.coverage", "ec2361ad-4238-45f0-8ac3-e9497cccc97b");
            String bodyText = this.bodyText.toString();
            writeline("/home/ubuntu/results/coverage/Digester/Digester_8_10.coverage", "49402840-c174-4418-859c-4f8bfd5db0eb");
            Substitutor substitutor = getSubstitutor();
            writeline("/home/ubuntu/results/coverage/Digester/Digester_8_10.coverage", "ed52c60a-06ac-4a26-92fc-00e79847e580");
            if (substitutor != null) {
                writeline("/home/ubuntu/results/coverage/Digester/Digester_8_10.coverage", "accc17b6-8efa-4f3e-a094-e86915af4a1b");
                bodyText = substitutor.substitute(bodyText);
            }
            writeline("/home/ubuntu/results/coverage/Digester/Digester_8_10.coverage", "99d275dd-f445-4fc2-9197-e2180c6d49ac");
            for (int i = 0; i < rules.size(); i++) {
                writeline("/home/ubuntu/results/coverage/Digester/Digester_8_10.coverage", "b4a615cd-94a8-4732-926e-9909c27fad8a");
                try {
                    writeline("/home/ubuntu/results/coverage/Digester/Digester_8_10.coverage", "d3626227-5390-41f8-adfe-cc7a08a845a7");
                    Rule rule = rules.get(i);
                    writeline("/home/ubuntu/results/coverage/Digester/Digester_8_10.coverage", "66cd96fc-ab02-4e83-b8d8-f8eac0cbb83a");
                    if (debug) {
                        writeline("/home/ubuntu/results/coverage/Digester/Digester_8_10.coverage", "44a3f8b8-b914-41e1-ae29-0987f7f8bcda");
                        log.debug("  Fire body() for " + rule);
                    }
                    writeline("/home/ubuntu/results/coverage/Digester/Digester_8_10.coverage", "2d9af947-688c-4b8e-ae05-7d4a0f191f1b");
                    rule.body(namespaceURI, name, bodyText);
                } catch (Exception e) {
                    writeline("/home/ubuntu/results/coverage/Digester/Digester_8_10.coverage", "a4f2fffa-5197-47e5-b9e4-1ad1184fe660");
                    log.error("Body event threw exception", e);
                } catch (Error e) {
                    writeline("/home/ubuntu/results/coverage/Digester/Digester_8_10.coverage", "d6f7e1dc-1b57-4baa-857e-81a82222905d");
                    log.error("Body event threw error", e);
                }
            }
        } else {
            writeline("/home/ubuntu/results/coverage/Digester/Digester_8_10.coverage", "11ebe053-8726-419e-9770-08174f084f6a");
            if (debug) {
                writeline("/home/ubuntu/results/coverage/Digester/Digester_8_10.coverage", "578e1015-493f-4b3c-a2f3-139554d5fb28");
                log.debug("  No rules found matching '" + match + "'.");
            }
        }
        writeline("/home/ubuntu/results/coverage/Digester/Digester_8_10.coverage", "3f0dfe5f-528c-48c0-ba42-94ab84e31c81");
        bodyText = bodyTexts.pop();
        writeline("/home/ubuntu/results/coverage/Digester/Digester_8_10.coverage", "fbb5f67d-ead0-47ca-98b3-e42c690b58af");
        if (debug) {
            writeline("/home/ubuntu/results/coverage/Digester/Digester_8_10.coverage", "9b27fa1e-4cb4-4863-b82b-d7a4b9d2072e");
            log.debug("  Popping body text '" + bodyText.toString() + "'");
        }
        writeline("/home/ubuntu/results/coverage/Digester/Digester_8_10.coverage", "1399eaac-62a9-45f8-808e-568322cf22bf");
        if (rules != null) {
            writeline("/home/ubuntu/results/coverage/Digester/Digester_8_10.coverage", "59fd1409-689c-41a3-8457-f909dbf8231d");
            for (int i = 0; i < rules.size(); i++) {
                writeline("/home/ubuntu/results/coverage/Digester/Digester_8_10.coverage", "a9b9f629-7af3-4cd7-b5e9-70930231e3eb");
                int j = (rules.size() - i) - 1;
                writeline("/home/ubuntu/results/coverage/Digester/Digester_8_10.coverage", "e9746e2e-3f0a-4e3d-9260-903177d99616");
                try {
                    writeline("/home/ubuntu/results/coverage/Digester/Digester_8_10.coverage", "a5f561f1-4191-4f37-9dc1-6ca4e2a75b78");
                    Rule rule = rules.get(j);
                    writeline("/home/ubuntu/results/coverage/Digester/Digester_8_10.coverage", "fbf46cd0-111c-456c-bcbf-ca2c1876cfa2");
                    if (debug) {
                        writeline("/home/ubuntu/results/coverage/Digester/Digester_8_10.coverage", "0d25b096-0d81-4c60-a2bb-f8d6f9a14afc");
                        log.debug("  Fire end() for " + rule);
                    }
                    writeline("/home/ubuntu/results/coverage/Digester/Digester_8_10.coverage", "0ffa5d80-56ae-4def-a2b1-fe182eb3d691");
                    rule.end(namespaceURI, name);
                } catch (Exception e) {
                    writeline("/home/ubuntu/results/coverage/Digester/Digester_8_10.coverage", "becf84e6-dbda-45f9-a4b1-710560e27706");
                    log.error("End event threw exception", e);
                } catch (Error e) {
                    writeline("/home/ubuntu/results/coverage/Digester/Digester_8_10.coverage", "dd1890ad-d8fb-4d03-b0fb-c366ccff4f57");
                    log.error("End event threw error", e);
                }
            }
        }
        writeline("/home/ubuntu/results/coverage/Digester/Digester_8_10.coverage", "c4dc142d-c792-4825-80c7-4a103e963bf4");
        int slash = match.lastIndexOf('/');
        writeline("/home/ubuntu/results/coverage/Digester/Digester_8_10.coverage", "e3ddb09a-baab-4aeb-b174-5795c066a1b9");
        if (slash >= 0) {
            writeline("/home/ubuntu/results/coverage/Digester/Digester_8_10.coverage", "1b24a8a9-509f-4fe3-85aa-856c4820091e");
            match = match.substring(0, slash);
        } else {
            writeline("/home/ubuntu/results/coverage/Digester/Digester_8_10.coverage", "42d786ef-0d80-4b69-a4e0-279317e756f0");
            match = "";
        }
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public void endPrefixMapping(String prefix) throws SAXException {
        writeline("/home/ubuntu/results/coverage/Digester/Digester_8_10.coverage", "4943ff19-e10e-4690-be14-4122966a6527");
        if (saxLog.isDebugEnabled()) {
            writeline("/home/ubuntu/results/coverage/Digester/Digester_8_10.coverage", "db76e50d-b75d-4520-aa82-2edc252887b9");
            saxLog.debug("endPrefixMapping(" + prefix + ")");
        }
        writeline("/home/ubuntu/results/coverage/Digester/Digester_8_10.coverage", "f933a225-c7ef-4614-aa38-ddff582edd9e");
        Stack<String> stack = namespaces.get(prefix);
        writeline("/home/ubuntu/results/coverage/Digester/Digester_8_10.coverage", "c4524df3-a782-484c-8bad-584754eacbef");
        if (stack == null) {
            writeline("/home/ubuntu/results/coverage/Digester/Digester_8_10.coverage", "93ccc758-c002-482f-877b-09fc15368689");
            return;
        }
        writeline("/home/ubuntu/results/coverage/Digester/Digester_8_10.coverage", "f342ec82-5159-482c-91f7-cd6762c98cd3");
        try {
            writeline("/home/ubuntu/results/coverage/Digester/Digester_8_10.coverage", "6fd4d7d1-a2a4-4702-89d5-6cda5d92e8f5");
            stack.pop();
            writeline("/home/ubuntu/results/coverage/Digester/Digester_8_10.coverage", "18d7d27c-6ce1-493f-ba93-4469ec36284d");
            if (stack.empty()) {
                writeline("/home/ubuntu/results/coverage/Digester/Digester_8_10.coverage", "55cfd20b-e675-4797-bcbf-9ed77a5a297a");
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
        writeline("/home/ubuntu/results/coverage/Digester/Digester_8_10.coverage", "b71a7206-72b4-418a-8ffe-1d9177054926");
        if (saxLog.isDebugEnabled()) {
            writeline("/home/ubuntu/results/coverage/Digester/Digester_8_10.coverage", "b1c3c6f4-2bcb-417f-9f42-2ce37404a570");
            saxLog.debug("ignorableWhitespace(" + new String(buffer, start, len) + ")");
        }
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public void processingInstruction(String target, String data) throws SAXException {
        writeline("/home/ubuntu/results/coverage/Digester/Digester_8_10.coverage", "2bfcb80a-14e4-4be7-bcf0-6c98c5eca9b1");
        if (customContentHandler != null) {
            writeline("/home/ubuntu/results/coverage/Digester/Digester_8_10.coverage", "b25ad8dd-1d64-4089-a1ea-de4c4357342f");
            customContentHandler.processingInstruction(target, data);
            writeline("/home/ubuntu/results/coverage/Digester/Digester_8_10.coverage", "e29c9cb3-dc98-4f45-b9be-2e6a0025f610");
            return;
        }
        writeline("/home/ubuntu/results/coverage/Digester/Digester_8_10.coverage", "02351933-9900-446c-bc88-639925467e08");
        if (saxLog.isDebugEnabled()) {
            writeline("/home/ubuntu/results/coverage/Digester/Digester_8_10.coverage", "a4e5b485-818b-45b6-beae-b0548215ee19");
            saxLog.debug("processingInstruction('" + target + "','" + data + "')");
        }
    }

    /**
     * Gets the document locator associated with our parser.
     *
     * @return the Locator supplied by the document parser
     */
    public Locator getDocumentLocator() {
        writeline("/home/ubuntu/results/coverage/Digester/Digester_8_10.coverage", "006a5e9e-ded0-4222-8bf1-a53f7fc01606");
        return locator;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public void setDocumentLocator(Locator locator) {
        writeline("/home/ubuntu/results/coverage/Digester/Digester_8_10.coverage", "24cd689d-6f47-4d86-9510-3592141d534f");
        if (saxLog.isDebugEnabled()) {
            writeline("/home/ubuntu/results/coverage/Digester/Digester_8_10.coverage", "3b7096d4-e570-424b-8b7f-78b223e8e374");
            saxLog.debug("setDocumentLocator(" + locator + ")");
        }
        writeline("/home/ubuntu/results/coverage/Digester/Digester_8_10.coverage", "0da934bc-0ebe-4e5a-8e45-e8ac18ffc1d2");
        this.locator = locator;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public void skippedEntity(String name) throws SAXException {
        writeline("/home/ubuntu/results/coverage/Digester/Digester_8_10.coverage", "ff544191-4d38-4647-874e-416b1669a669");
        if (saxLog.isDebugEnabled()) {
            writeline("/home/ubuntu/results/coverage/Digester/Digester_8_10.coverage", "ea53dd77-6777-43c8-842a-df8dc98e7a4d");
            saxLog.debug("skippedEntity(" + name + ")");
        }
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public void startDocument() throws SAXException {
        writeline("/home/ubuntu/results/coverage/Digester/Digester_8_10.coverage", "958ef761-746e-4e2a-83d4-c701b6a13959");
        if (saxLog.isDebugEnabled()) {
            writeline("/home/ubuntu/results/coverage/Digester/Digester_8_10.coverage", "17677709-6d21-4ef4-8dc9-c08b3df96314");
            saxLog.debug("startDocument()");
        }
        writeline("/home/ubuntu/results/coverage/Digester/Digester_8_10.coverage", "7c9090cf-b9d0-4ac5-ba70-d80572401414");
        configure();
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public void startElement(String namespaceURI, String localName, String qName, Attributes list) throws SAXException {
        writeline("/home/ubuntu/results/coverage/Digester/Digester_8_10.coverage", "576b08d2-e454-40bf-8165-fcf474251fdb");
        boolean debug = log.isDebugEnabled();
        writeline("/home/ubuntu/results/coverage/Digester/Digester_8_10.coverage", "33601310-1317-4583-9c05-76f9917deae4");
        if (customContentHandler != null) {
            writeline("/home/ubuntu/results/coverage/Digester/Digester_8_10.coverage", "3e9f73cb-9b09-4219-8fb1-c0e6726095d2");
            customContentHandler.startElement(namespaceURI, localName, qName, list);
            writeline("/home/ubuntu/results/coverage/Digester/Digester_8_10.coverage", "1b09bd3c-21cc-4bcd-95c9-f8e33b29ca3e");
            return;
        }
        writeline("/home/ubuntu/results/coverage/Digester/Digester_8_10.coverage", "258f18f4-0781-44ae-a0b3-d45103122384");
        if (saxLog.isDebugEnabled()) {
            writeline("/home/ubuntu/results/coverage/Digester/Digester_8_10.coverage", "67f75b6d-0791-4cdd-aa14-7baa655e465d");
            saxLog.debug("startElement(" + namespaceURI + "," + localName + "," + qName + ")");
        }
        writeline("/home/ubuntu/results/coverage/Digester/Digester_8_10.coverage", "184fe652-3ca4-470c-ad78-67c1b3cb9cdf");
        bodyTexts.push(bodyText);
        writeline("/home/ubuntu/results/coverage/Digester/Digester_8_10.coverage", "95a969a3-990d-4eab-94e9-e7b76eaf1cf4");
        if (debug) {
            writeline("/home/ubuntu/results/coverage/Digester/Digester_8_10.coverage", "4ff7a1a4-4bb2-4d5a-b4ef-114123216e30");
            log.debug("  Pushing body text '" + bodyText.toString() + "'");
        }
        writeline("/home/ubuntu/results/coverage/Digester/Digester_8_10.coverage", "2ab7d245-f390-4e4c-bad2-f222b2e8a75e");
        bodyText = new StringBuilder();
        writeline("/home/ubuntu/results/coverage/Digester/Digester_8_10.coverage", "73644ba9-03f9-42e9-88f6-7a56cb3fc22c");
        String name = localName;
        writeline("/home/ubuntu/results/coverage/Digester/Digester_8_10.coverage", "d871b405-bd83-46b4-932a-0f03975bd442");
        if ((name == null) || (name.length() < 1)) {
            writeline("/home/ubuntu/results/coverage/Digester/Digester_8_10.coverage", "55ddbcf0-9b7d-4cc5-9cc8-712d247b84b5");
            name = qName;
        }
        writeline("/home/ubuntu/results/coverage/Digester/Digester_8_10.coverage", "60d1baa3-b685-4894-b982-3b3104bacabf");
        StringBuilder sb = new StringBuilder(match);
        writeline("/home/ubuntu/results/coverage/Digester/Digester_8_10.coverage", "f345dba0-2db8-42cc-8d91-d436b8708b8b");
        if (match.length() > 0) {
            writeline("/home/ubuntu/results/coverage/Digester/Digester_8_10.coverage", "3ee02a78-69e6-4ffd-8491-ff023ec0486f");
            sb.append('/');
        }
        writeline("/home/ubuntu/results/coverage/Digester/Digester_8_10.coverage", "687e24b0-1b40-489c-8563-dde64c863e6c");
        sb.append(name);
        writeline("/home/ubuntu/results/coverage/Digester/Digester_8_10.coverage", "a38c8a5b-2494-4866-8ec0-9805926aad39");
        match = sb.toString();
        writeline("/home/ubuntu/results/coverage/Digester/Digester_8_10.coverage", "e0b98a58-60ac-4305-b641-21f6475ef6df");
        if (debug) {
            writeline("/home/ubuntu/results/coverage/Digester/Digester_8_10.coverage", "88cdce5b-cc52-43cd-a148-153d82e1ce56");
            log.debug("  New match='" + match + "'");
        }
        writeline("/home/ubuntu/results/coverage/Digester/Digester_8_10.coverage", "1c43d8bd-4d6f-4b42-88ea-f3386f4a9fdc");
        List<Rule> rules = getRules().match(namespaceURI, match, localName, list);
        writeline("/home/ubuntu/results/coverage/Digester/Digester_8_10.coverage", "d222037e-494e-43a1-8168-1127773d4a77");
        matches.push(rules);
        writeline("/home/ubuntu/results/coverage/Digester/Digester_8_10.coverage", "e53ff896-407b-4c70-bb96-d70c1528d7d1");
        if ((rules != null) && (rules.size() > 0)) {
            writeline("/home/ubuntu/results/coverage/Digester/Digester_8_10.coverage", "f75f1923-379b-4a8a-b0ca-91cb585df4ed");
            Substitutor substitutor = getSubstitutor();
            writeline("/home/ubuntu/results/coverage/Digester/Digester_8_10.coverage", "0b31f45d-0e72-43f8-8636-141e3f518b16");
            if (substitutor != null) {
                writeline("/home/ubuntu/results/coverage/Digester/Digester_8_10.coverage", "b8ed804a-a070-430e-982e-3b5e0a93da64");
                list = substitutor.substitute(list);
            }
            writeline("/home/ubuntu/results/coverage/Digester/Digester_8_10.coverage", "e4038566-c546-49b4-9311-02cbe1e8dc0e");
            for (int i = 0; i < rules.size(); i++) {
                writeline("/home/ubuntu/results/coverage/Digester/Digester_8_10.coverage", "f8a54521-cab0-411a-91c0-00dc14721158");
                try {
                    writeline("/home/ubuntu/results/coverage/Digester/Digester_8_10.coverage", "fef18061-12e7-44c4-95f6-ba97dcf2efbd");
                    Rule rule = rules.get(i);
                    writeline("/home/ubuntu/results/coverage/Digester/Digester_8_10.coverage", "8086a7a7-a3f8-459a-92fa-ac76f4144e7c");
                    if (debug) {
                        writeline("/home/ubuntu/results/coverage/Digester/Digester_8_10.coverage", "492a9f23-32d5-4ef8-899a-0cddc0002179");
                        log.debug("  Fire begin() for " + rule);
                    }
                    writeline("/home/ubuntu/results/coverage/Digester/Digester_8_10.coverage", "c5f0a6ec-7d32-4ec8-9513-96deafecc841");
                    rule.begin(namespaceURI, name, list);
                } catch (Exception e) {
                    writeline("/home/ubuntu/results/coverage/Digester/Digester_8_10.coverage", "d9fbe989-2c88-4f8d-9044-610d4a309cec");
                    log.error("Begin event threw exception", e);
                } catch (Error e) {
                    writeline("/home/ubuntu/results/coverage/Digester/Digester_8_10.coverage", "b42652b9-6699-4793-96fc-6564b4c4e5d3");
                    log.error("Begin event threw error", e);
                }
            }
        } else {
            writeline("/home/ubuntu/results/coverage/Digester/Digester_8_10.coverage", "2ea1a36a-c67b-49dd-90af-cc3688428075");
            if (debug) {
                writeline("/home/ubuntu/results/coverage/Digester/Digester_8_10.coverage", "a1fae373-2b87-45c7-bbea-8dfe5af2bd01");
                log.debug("  No rules found matching '" + match + "'.");
            }
        }
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public void startPrefixMapping(String prefix, String namespaceURI) throws SAXException {
        writeline("/home/ubuntu/results/coverage/Digester/Digester_8_10.coverage", "6fbedd88-2307-4866-8913-fbf55b90b1da");
        if (saxLog.isDebugEnabled()) {
            writeline("/home/ubuntu/results/coverage/Digester/Digester_8_10.coverage", "f0de78e1-32c8-44e7-aeef-4eb2f9419212");
            saxLog.debug("startPrefixMapping(" + prefix + "," + namespaceURI + ")");
        }
        writeline("/home/ubuntu/results/coverage/Digester/Digester_8_10.coverage", "df6bb8f4-1953-42c3-965b-8e87872c1b34");
        Stack<String> stack = namespaces.get(prefix);
        writeline("/home/ubuntu/results/coverage/Digester/Digester_8_10.coverage", "fc29c53f-306a-49e6-b3cc-0ff8534774e0");
        if (stack == null) {
            writeline("/home/ubuntu/results/coverage/Digester/Digester_8_10.coverage", "6958172d-6395-414b-a002-81d9c620be8e");
            stack = new Stack<String>();
            writeline("/home/ubuntu/results/coverage/Digester/Digester_8_10.coverage", "c03b8b83-9e61-4c19-b1e2-bd42755a9143");
            namespaces.put(prefix, stack);
        }
        writeline("/home/ubuntu/results/coverage/Digester/Digester_8_10.coverage", "fa1b4afe-97bb-4656-83ca-b2a5b4f59dae");
        stack.push(namespaceURI);
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public void notationDecl(String name, String publicId, String systemId) {
        writeline("/home/ubuntu/results/coverage/Digester/Digester_8_10.coverage", "162746bf-d694-49b8-b095-b744e995fcbe");
        if (saxLog.isDebugEnabled()) {
            writeline("/home/ubuntu/results/coverage/Digester/Digester_8_10.coverage", "3a45e242-ebf9-4c2d-970b-ee218e966155");
            saxLog.debug("notationDecl(" + name + "," + publicId + "," + systemId + ")");
        }
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public void unparsedEntityDecl(String name, String publicId, String systemId, String notation) {
        writeline("/home/ubuntu/results/coverage/Digester/Digester_8_10.coverage", "64449117-2b6c-4530-b596-a6a7e027e9ab");
        if (saxLog.isDebugEnabled()) {
            writeline("/home/ubuntu/results/coverage/Digester/Digester_8_10.coverage", "5a63e2b9-d518-4fca-8c36-82a3dfe0d5df");
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
        writeline("/home/ubuntu/results/coverage/Digester/Digester_8_10.coverage", "7c5212bb-d654-487f-9926-2be192bb8301");
        this.entityResolver = entityResolver;
    }

    /**
     * Return the Entity Resolver used by the SAX parser.
     *
     * @return the Entity Resolver used by the SAX parser.
     */
    public EntityResolver getEntityResolver() {
        writeline("/home/ubuntu/results/coverage/Digester/Digester_8_10.coverage", "7dcca0a4-66e4-4dd2-8920-ae9e502ec01c");
        return entityResolver;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public InputSource resolveEntity(String publicId, String systemId) throws SAXException {
        writeline("/home/ubuntu/results/coverage/Digester/Digester_8_10.coverage", "7308eb79-fa22-4656-8299-8056cd504b01");
        if (saxLog.isDebugEnabled()) {
            writeline("/home/ubuntu/results/coverage/Digester/Digester_8_10.coverage", "607cf24e-8002-4d2f-a635-e4d6189a1778");
            saxLog.debug("resolveEntity('" + publicId + "', '" + systemId + "')");
        }
        writeline("/home/ubuntu/results/coverage/Digester/Digester_8_10.coverage", "c786ac7d-8048-4648-92ab-c8f9ae22bfc7");
        if (publicId != null) {
            writeline("/home/ubuntu/results/coverage/Digester/Digester_8_10.coverage", "0efd9b2e-3561-433c-83a0-298f5cd49d5c");
            this.publicId = publicId;
        }
        writeline("/home/ubuntu/results/coverage/Digester/Digester_8_10.coverage", "2a18682c-d997-4dfa-9255-59d140af1095");
        URL entityURL = null;
        writeline("/home/ubuntu/results/coverage/Digester/Digester_8_10.coverage", "e7cd0c93-8695-409c-84bb-3153c83978a5");
        if (publicId != null) {
            writeline("/home/ubuntu/results/coverage/Digester/Digester_8_10.coverage", "0bfb5fec-b4a9-45cb-9304-12a17d28ea5e");
            entityURL = entityValidator.get(publicId);
        }
        writeline("/home/ubuntu/results/coverage/Digester/Digester_8_10.coverage", "a580b5b1-ad18-4e1e-860c-51c1843bbeb4");
        if (entityURL == null && systemId != null) {
            writeline("/home/ubuntu/results/coverage/Digester/Digester_8_10.coverage", "60d6067c-3898-488f-a0ed-457198381c4c");
            entityURL = entityValidator.get(systemId);
        }
        writeline("/home/ubuntu/results/coverage/Digester/Digester_8_10.coverage", "998ab8af-acae-407d-9274-d2b56bce9a24");
        if (entityURL == null) {
            writeline("/home/ubuntu/results/coverage/Digester/Digester_8_10.coverage", "bf34686c-7b7c-4c52-8263-b403f29feb87");
            if (systemId == null) {
                writeline("/home/ubuntu/results/coverage/Digester/Digester_8_10.coverage", "7842d728-209b-4550-b8a0-ff02a597d320");
                if (log.isDebugEnabled()) {
                    writeline("/home/ubuntu/results/coverage/Digester/Digester_8_10.coverage", "12642b22-28c6-4a35-8ab0-3ead46d4e624");
                    log.debug(" Cannot resolve null entity, returning null InputSource");
                }
                writeline("/home/ubuntu/results/coverage/Digester/Digester_8_10.coverage", "e5d07738-f2f1-46fb-a0d8-e2b15dc63b27");
                return (null);
            }
            writeline("/home/ubuntu/results/coverage/Digester/Digester_8_10.coverage", "6b83c971-861f-457d-a1c3-9661148330d4");
            if (log.isDebugEnabled()) {
                writeline("/home/ubuntu/results/coverage/Digester/Digester_8_10.coverage", "6c13f30f-dd5d-49c3-acbd-0acb3e0767a4");
                log.debug(" Trying to resolve using system ID '" + systemId + "'");
            }
            writeline("/home/ubuntu/results/coverage/Digester/Digester_8_10.coverage", "f8258e87-3525-42d7-bd5f-7eb0a8280574");
            try {
                writeline("/home/ubuntu/results/coverage/Digester/Digester_8_10.coverage", "f8b3fd5f-0bde-44a9-931d-daad40e2ea78");
                entityURL = new URL(systemId);
            } catch (MalformedURLException e) {
            }
        }
        writeline("/home/ubuntu/results/coverage/Digester/Digester_8_10.coverage", "4aa47f49-818e-4249-8076-e674797de190");
        if (log.isDebugEnabled()) {
            writeline("/home/ubuntu/results/coverage/Digester/Digester_8_10.coverage", "d4023f04-633f-40b0-956d-b64cafbdf54e");
            log.debug(" Resolving to alternate DTD '" + entityURL + "'");
        }
        writeline("/home/ubuntu/results/coverage/Digester/Digester_8_10.coverage", "040f3ab0-e5ff-417c-9220-244d8ad8ce71");
        try {
            writeline("/home/ubuntu/results/coverage/Digester/Digester_8_10.coverage", "ae67d317-dc08-4b7b-bdc5-6d14093d7926");
            return createInputSourceFromURL(entityURL);
        } catch (Exception e) {
        }
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public void error(SAXParseException exception) throws SAXException {
        writeline("/home/ubuntu/results/coverage/Digester/Digester_8_10.coverage", "bff25364-dcfe-45f4-9411-40ae47025ef1");
        log.error("Parse Error at line " + exception.getLineNumber() + " column " + exception.getColumnNumber() + ": " + exception.getMessage(), exception);
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public void fatalError(SAXParseException exception) throws SAXException {
        writeline("/home/ubuntu/results/coverage/Digester/Digester_8_10.coverage", "d0fc200d-de98-4227-88b2-47581d146e85");
        log.error("Parse Fatal Error at line " + exception.getLineNumber() + " column " + exception.getColumnNumber() + ": " + exception.getMessage(), exception);
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public void warning(SAXParseException exception) throws SAXException {
        writeline("/home/ubuntu/results/coverage/Digester/Digester_8_10.coverage", "b1f196d3-c545-41db-b736-46760fa3ce0f");
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
        writeline("/home/ubuntu/results/coverage/Digester/Digester_8_10.coverage", "71ec3e5b-bee3-4b63-a67b-cd859758d3fd");
        if (file == null) {
        }
        writeline("/home/ubuntu/results/coverage/Digester/Digester_8_10.coverage", "e86134b7-9911-43fa-b28d-6ded19b30286");
        InputSource input = new InputSource(new FileInputStream(file));
        writeline("/home/ubuntu/results/coverage/Digester/Digester_8_10.coverage", "f2cf539f-8af1-4442-ac40-d919c30ee803");
        input.setSystemId(file.toURI().toURL().toString());
        writeline("/home/ubuntu/results/coverage/Digester/Digester_8_10.coverage", "b7e595d3-8b0e-45a4-aa9b-f4cf8b17014f");
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
        writeline("/home/ubuntu/results/coverage/Digester/Digester_8_10.coverage", "243da162-e96a-41a4-8d3f-269d7f01906d");
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
        writeline("/home/ubuntu/results/coverage/Digester/Digester_8_10.coverage", "48165033-953a-40bb-b9a8-9a85ce6aa902");
        if (input == null) {
        }
        writeline("/home/ubuntu/results/coverage/Digester/Digester_8_10.coverage", "f6bc320c-d503-4d81-8b6a-195b13340c33");
        configure();
        writeline("/home/ubuntu/results/coverage/Digester/Digester_8_10.coverage", "f5ccc1d4-3d75-4c82-aa0f-574083f54707");
        String systemId = input.getSystemId();
        writeline("/home/ubuntu/results/coverage/Digester/Digester_8_10.coverage", "7c72f7b7-af08-405d-afd6-d1833eb5a5f3");
        if (systemId == null) {
            writeline("/home/ubuntu/results/coverage/Digester/Digester_8_10.coverage", "65f2221c-ecdd-4fea-8ee4-b21f13601318");
            systemId = "(already loaded from stream)";
        }
        writeline("/home/ubuntu/results/coverage/Digester/Digester_8_10.coverage", "7f1b33dc-c9f5-4ff9-9717-7b4b23303637");
        try {
            writeline("/home/ubuntu/results/coverage/Digester/Digester_8_10.coverage", "01256580-b92e-4afe-a48a-b3ad2b481510");
            getXMLReader().parse(input);
        } catch (IOException e) {
            writeline("/home/ubuntu/results/coverage/Digester/Digester_8_10.coverage", "5e887759-bf28-4dd6-874a-8b8d226ec8df");
            log.error(format("An error occurred while reading stream from '%s', see nested exceptions", systemId), e);
        }
        writeline("/home/ubuntu/results/coverage/Digester/Digester_8_10.coverage", "a89d3534-c754-4f5f-9191-56b45fd04b31");
        cleanup();
        writeline("/home/ubuntu/results/coverage/Digester/Digester_8_10.coverage", "4ba3c262-16c0-4c9b-b3d6-465417ac1911");
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
        writeline("/home/ubuntu/results/coverage/Digester/Digester_8_10.coverage", "479bdfb5-1d47-478f-86da-5907f9fb62fc");
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
        writeline("/home/ubuntu/results/coverage/Digester/Digester_8_10.coverage", "75ced306-32ce-4f54-aa59-f06b3c331ec8");
        if (input == null) {
        }
        writeline("/home/ubuntu/results/coverage/Digester/Digester_8_10.coverage", "8138962f-78a6-430a-917b-cb76dd09d568");
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
        writeline("/home/ubuntu/results/coverage/Digester/Digester_8_10.coverage", "9d82267b-ed7e-4333-acf6-5fb6f3158b86");
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
        writeline("/home/ubuntu/results/coverage/Digester/Digester_8_10.coverage", "310e8f78-364f-4161-b93d-860dee12a334");
        if (reader == null) {
        }
        writeline("/home/ubuntu/results/coverage/Digester/Digester_8_10.coverage", "32b9ab75-ad93-4730-8a45-53084a60a554");
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
        writeline("/home/ubuntu/results/coverage/Digester/Digester_8_10.coverage", "dd3e5704-1d67-4918-a2a4-93faacdc80dc");
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
        writeline("/home/ubuntu/results/coverage/Digester/Digester_8_10.coverage", "9f19a473-8bc0-4b0b-8c57-98f77c2afbd2");
        if (uri == null) {
        }
        writeline("/home/ubuntu/results/coverage/Digester/Digester_8_10.coverage", "aa128233-07e8-4ba5-9f34-8d7ed530c4e0");
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
        writeline("/home/ubuntu/results/coverage/Digester/Digester_8_10.coverage", "d7f41840-5f4f-4fba-a0aa-3af62decbfc8");
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
        writeline("/home/ubuntu/results/coverage/Digester/Digester_8_10.coverage", "d19b5431-4464-4323-8eea-40a568788c5d");
        if (url == null) {
        }
        writeline("/home/ubuntu/results/coverage/Digester/Digester_8_10.coverage", "53f78693-69a4-44d4-85e6-04fe253abd01");
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
        writeline("/home/ubuntu/results/coverage/Digester/Digester_8_10.coverage", "b6b19418-00c7-4583-9405-a9fd82e6294a");
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
        writeline("/home/ubuntu/results/coverage/Digester/Digester_8_10.coverage", "87250b00-27c8-41a6-9824-c24c0ccaf92e");
        if (executorService == null) {
        }
        writeline("/home/ubuntu/results/coverage/Digester/Digester_8_10.coverage", "b00af617-4f00-4fc4-a6b8-c264687c0aa1");
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
        writeline("/home/ubuntu/results/coverage/Digester/Digester_8_10.coverage", "c002a94c-5b9f-42a7-b98a-612db2df6531");
        if (log.isDebugEnabled()) {
            writeline("/home/ubuntu/results/coverage/Digester/Digester_8_10.coverage", "dc7ea9bf-d7a4-48a2-be39-fc941e0d2843");
            log.debug("register('" + publicId + "', '" + entityURL + "'");
        }
        writeline("/home/ubuntu/results/coverage/Digester/Digester_8_10.coverage", "c42a85f2-efd5-41b9-807a-1481540afe7c");
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
        writeline("/home/ubuntu/results/coverage/Digester/Digester_8_10.coverage", "721820f6-a00c-4baf-bd70-1b4a7d7a4dba");
        if (log.isDebugEnabled()) {
            writeline("/home/ubuntu/results/coverage/Digester/Digester_8_10.coverage", "192acb0e-2ffd-41a6-a81a-230df79affee");
            log.debug("register('" + publicId + "', '" + entityURL + "'");
        }
        writeline("/home/ubuntu/results/coverage/Digester/Digester_8_10.coverage", "e787a9f7-5424-46c9-80f0-ad7dc807419b");
        try {
            writeline("/home/ubuntu/results/coverage/Digester/Digester_8_10.coverage", "46ea2710-e38f-404e-aa21-6933a1d01b61");
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
        writeline("/home/ubuntu/results/coverage/Digester/Digester_8_10.coverage", "2e7a2786-0af0-4afc-8570-a4e30d312dcc");
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
        writeline("/home/ubuntu/results/coverage/Digester/Digester_8_10.coverage", "72d827eb-b9d1-434e-94f7-f8f168abd904");
        URLConnection connection = url.openConnection();
        writeline("/home/ubuntu/results/coverage/Digester/Digester_8_10.coverage", "12f5c45e-941b-4222-a428-842a26cc7a2c");
        connection.setUseCaches(false);
        writeline("/home/ubuntu/results/coverage/Digester/Digester_8_10.coverage", "393603d4-5827-4c3f-86da-fb40ef9fa57b");
        InputStream stream = connection.getInputStream();
        writeline("/home/ubuntu/results/coverage/Digester/Digester_8_10.coverage", "58aca04b-29fc-40da-8187-ea14dfcbde35");
        InputSource source = new InputSource(stream);
        writeline("/home/ubuntu/results/coverage/Digester/Digester_8_10.coverage", "2dc1fd7d-bcda-4eb0-821c-96b54e7c3da6");
        source.setSystemId(url.toExternalForm());
        writeline("/home/ubuntu/results/coverage/Digester/Digester_8_10.coverage", "d7c68edb-6495-4497-a046-ef2b2eba7877");
        inputSources.add(source);
        writeline("/home/ubuntu/results/coverage/Digester/Digester_8_10.coverage", "8ddfedd0-0d3d-437d-b6a8-8bb31515225b");
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
        writeline("/home/ubuntu/results/coverage/Digester/Digester_8_10.coverage", "d9ab9582-65c7-4256-830f-af1ce1f862d6");
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
        writeline("/home/ubuntu/results/coverage/Digester/Digester_8_10.coverage", "3a2a3f3e-ebed-48e7-b343-06b9a790dc34");
        rule.setDigester(this);
        writeline("/home/ubuntu/results/coverage/Digester/Digester_8_10.coverage", "a009f415-2036-46f8-b586-8949b270d0d0");
        getRules().add(pattern, rule);
    }

    /**
     * Register a set of Rule instances defined in a RuleSet.
     *
     * @param ruleSet The RuleSet instance to configure from
     */
    public void addRuleSet(RuleSet ruleSet) {
        writeline("/home/ubuntu/results/coverage/Digester/Digester_8_10.coverage", "3d92b5dc-d302-4c13-a19c-aefba647daf1");
        String oldNamespaceURI = getRuleNamespaceURI();
        writeline("/home/ubuntu/results/coverage/Digester/Digester_8_10.coverage", "b96a9437-4aaf-4c59-acbb-df999339c65b");
        String newNamespaceURI = ruleSet.getNamespaceURI();
        writeline("/home/ubuntu/results/coverage/Digester/Digester_8_10.coverage", "8e9969ba-2083-46b0-b337-7eac534cbbea");
        if (log.isDebugEnabled()) {
            writeline("/home/ubuntu/results/coverage/Digester/Digester_8_10.coverage", "e002b449-cfa0-423f-8809-498db0b5b97e");
            if (newNamespaceURI == null) {
                writeline("/home/ubuntu/results/coverage/Digester/Digester_8_10.coverage", "345ad5c4-a300-4017-8456-b1ae2db49b96");
                log.debug("addRuleSet() with no namespace URI");
            } else {
                writeline("/home/ubuntu/results/coverage/Digester/Digester_8_10.coverage", "4d56a97d-3fc2-4cce-8636-7bbaeb19ac7c");
                log.debug("addRuleSet() with namespace URI " + newNamespaceURI);
            }
        }
        writeline("/home/ubuntu/results/coverage/Digester/Digester_8_10.coverage", "2a99c80f-3526-45b8-b4b4-16f8e11d44f7");
        setRuleNamespaceURI(newNamespaceURI);
        writeline("/home/ubuntu/results/coverage/Digester/Digester_8_10.coverage", "e3c25a4b-5785-4af6-9b78-c2581e86af39");
        ruleSet.addRuleInstances(this);
        writeline("/home/ubuntu/results/coverage/Digester/Digester_8_10.coverage", "55130f42-4f9c-4e05-874a-0081ec68bda7");
        setRuleNamespaceURI(oldNamespaceURI);
    }

    /**
     * Add a "bean property setter" rule for the specified parameters.
     *
     * @param pattern Element matching pattern
     * @see BeanPropertySetterRule
     */
    public void addBeanPropertySetter(String pattern) {
        writeline("/home/ubuntu/results/coverage/Digester/Digester_8_10.coverage", "3eeccb26-0f73-4de4-9b1f-0b86769d1939");
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
        writeline("/home/ubuntu/results/coverage/Digester/Digester_8_10.coverage", "0c917613-5522-472d-a821-52dfb7f7ba00");
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
        writeline("/home/ubuntu/results/coverage/Digester/Digester_8_10.coverage", "605a59a1-6fa6-48ef-aef5-c016b23ae108");
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
        writeline("/home/ubuntu/results/coverage/Digester/Digester_8_10.coverage", "03bdbad1-c162-4087-9a2f-11c4bb64394b");
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
        writeline("/home/ubuntu/results/coverage/Digester/Digester_8_10.coverage", "1e18f892-c6cb-43f3-a3af-7fde6b73c699");
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
        writeline("/home/ubuntu/results/coverage/Digester/Digester_8_10.coverage", "0b1e6e5c-6200-46bc-a74e-bc157d2f3953");
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
        writeline("/home/ubuntu/results/coverage/Digester/Digester_8_10.coverage", "a2e3fd63-8524-4aca-b114-62ad517a6826");
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
        writeline("/home/ubuntu/results/coverage/Digester/Digester_8_10.coverage", "153d7461-d711-4f8d-bfba-be1667a539d3");
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
        writeline("/home/ubuntu/results/coverage/Digester/Digester_8_10.coverage", "96b39618-928e-4c67-a206-571581b68050");
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
        writeline("/home/ubuntu/results/coverage/Digester/Digester_8_10.coverage", "afef3572-3faa-4156-887a-fe531c002fac");
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
        writeline("/home/ubuntu/results/coverage/Digester/Digester_8_10.coverage", "1e12edb7-c395-4e1b-bf71-36b72b8d63da");
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
        writeline("/home/ubuntu/results/coverage/Digester/Digester_8_10.coverage", "22cd2875-8b56-4917-b21b-139a5aab2923");
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
        writeline("/home/ubuntu/results/coverage/Digester/Digester_8_10.coverage", "8484e6b2-787b-463f-bf80-9df04a734a66");
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
        writeline("/home/ubuntu/results/coverage/Digester/Digester_8_10.coverage", "62504689-45bb-46ed-9286-da98ebbc2ac3");
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
        writeline("/home/ubuntu/results/coverage/Digester/Digester_8_10.coverage", "d3c1c71d-a543-478d-9bcf-900dc0b7932a");
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
        writeline("/home/ubuntu/results/coverage/Digester/Digester_8_10.coverage", "e2bd2aae-427e-47ba-bcaf-c13acc54e769");
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
        writeline("/home/ubuntu/results/coverage/Digester/Digester_8_10.coverage", "7a8d47d4-b6bf-421e-8833-bfd798bafc5f");
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
        writeline("/home/ubuntu/results/coverage/Digester/Digester_8_10.coverage", "429ac9cb-8124-41fd-8fe5-9985840aa666");
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
        writeline("/home/ubuntu/results/coverage/Digester/Digester_8_10.coverage", "63e531e3-e9c2-4a0f-8884-daf08709ca96");
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
        writeline("/home/ubuntu/results/coverage/Digester/Digester_8_10.coverage", "57e4651f-eddc-4f9f-b428-c906e2eaf84b");
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
        writeline("/home/ubuntu/results/coverage/Digester/Digester_8_10.coverage", "ff092f3a-8b95-450c-a516-1482dd586035");
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
        writeline("/home/ubuntu/results/coverage/Digester/Digester_8_10.coverage", "45a4cd70-de1d-4520-a067-b1fed5a49599");
        creationFactory.setDigester(this);
        writeline("/home/ubuntu/results/coverage/Digester/Digester_8_10.coverage", "9b77f82e-fc65-4272-a833-97cabd597416");
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
        writeline("/home/ubuntu/results/coverage/Digester/Digester_8_10.coverage", "3c79ba64-db14-44d7-90d0-d563da9140c2");
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
        writeline("/home/ubuntu/results/coverage/Digester/Digester_8_10.coverage", "3b2879fc-c359-44e5-b32c-b75624d656aa");
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
        writeline("/home/ubuntu/results/coverage/Digester/Digester_8_10.coverage", "82210cbe-dca6-4799-b7ad-3167de3f232f");
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
        writeline("/home/ubuntu/results/coverage/Digester/Digester_8_10.coverage", "9faa2a32-4b15-47d2-9761-764780db311a");
        addRule(pattern, new ObjectCreateRule(attributeName, clazz));
    }

    /**
     * Adds an {@link SetNestedPropertiesRule}.
     *
     * @param pattern register the rule with this pattern
     * @since 1.6
     */
    public void addSetNestedProperties(String pattern) {
        writeline("/home/ubuntu/results/coverage/Digester/Digester_8_10.coverage", "83b9cc10-9c9e-463b-89f0-f4718a908feb");
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
        writeline("/home/ubuntu/results/coverage/Digester/Digester_8_10.coverage", "4bcebcbf-a454-45e1-a40d-98e94306eab4");
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
        writeline("/home/ubuntu/results/coverage/Digester/Digester_8_10.coverage", "9814fc75-b9cf-4d2d-9b16-e0f716e7ef35");
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
        writeline("/home/ubuntu/results/coverage/Digester/Digester_8_10.coverage", "3acb30c2-8f82-4882-929b-4629b93415a3");
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
        writeline("/home/ubuntu/results/coverage/Digester/Digester_8_10.coverage", "8d4d6ca2-da50-41ef-9df7-fc3286c5d6d4");
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
        writeline("/home/ubuntu/results/coverage/Digester/Digester_8_10.coverage", "97a9212b-4724-4a6c-82b8-49853708c79f");
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
        writeline("/home/ubuntu/results/coverage/Digester/Digester_8_10.coverage", "ae4ee36c-d897-4235-90f4-3baa49c57dad");
        addRule(pattern, new SetRootRule(methodName, paramType));
    }

    /**
     * Add a "set properties" rule for the specified parameters.
     *
     * @param pattern Element matching pattern
     * @see SetPropertiesRule
     */
    public void addSetProperties(String pattern) {
        writeline("/home/ubuntu/results/coverage/Digester/Digester_8_10.coverage", "5fab460a-7242-49a9-9c33-dc66de32ec1c");
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
        writeline("/home/ubuntu/results/coverage/Digester/Digester_8_10.coverage", "5ac217ab-694a-4c3c-b4f8-3a38406968e6");
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
        writeline("/home/ubuntu/results/coverage/Digester/Digester_8_10.coverage", "00dde779-4119-43de-8ff4-bf746d6dbb71");
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
        writeline("/home/ubuntu/results/coverage/Digester/Digester_8_10.coverage", "6d0c92d4-ae60-4d47-8116-f9f7608015ae");
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
        writeline("/home/ubuntu/results/coverage/Digester/Digester_8_10.coverage", "3698bbe6-edc1-456c-bf76-af1c63a8b0a4");
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
        writeline("/home/ubuntu/results/coverage/Digester/Digester_8_10.coverage", "920002c4-bdb0-445e-88b3-03f006b5c63a");
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
        writeline("/home/ubuntu/results/coverage/Digester/Digester_8_10.coverage", "d64d2712-0b70-4e98-afe2-2a44b3acdb37");
        match = "";
        writeline("/home/ubuntu/results/coverage/Digester/Digester_8_10.coverage", "f54d1a23-a5e3-442f-8d50-1dce192b7dd6");
        bodyTexts.clear();
        writeline("/home/ubuntu/results/coverage/Digester/Digester_8_10.coverage", "35627fef-6b01-4937-980e-9591905b5397");
        params.clear();
        writeline("/home/ubuntu/results/coverage/Digester/Digester_8_10.coverage", "5dcd41f1-1458-467a-9fac-f4e16608cd44");
        publicId = null;
        writeline("/home/ubuntu/results/coverage/Digester/Digester_8_10.coverage", "cdca2919-d2f5-416a-8a5a-02f2eb80b096");
        stack.clear();
        writeline("/home/ubuntu/results/coverage/Digester/Digester_8_10.coverage", "325de017-8a19-44d2-9759-57ad6a93510a");
        stacksByName.clear();
        writeline("/home/ubuntu/results/coverage/Digester/Digester_8_10.coverage", "14ce2109-6615-4dd2-b6aa-ec1638604dac");
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
        writeline("/home/ubuntu/results/coverage/Digester/Digester_8_10.coverage", "294d94f5-93a7-465d-a0b4-3e53bca68dfd");
        try {
            writeline("/home/ubuntu/results/coverage/Digester/Digester_8_10.coverage", "ca142c1b-e680-44ff-9d6b-5074ba9eaaed");
            return this.<T>npeSafeCast(stack.peek());
        } catch (EmptyStackException e) {
            writeline("/home/ubuntu/results/coverage/Digester/Digester_8_10.coverage", "9847b451-c604-44a3-b55f-e3617371c3c6");
            log.warn("Empty stack (returning null)");
            writeline("/home/ubuntu/results/coverage/Digester/Digester_8_10.coverage", "c8fc62f2-c5c2-4d54-9140-58c7d53ca516");
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
        writeline("/home/ubuntu/results/coverage/Digester/Digester_8_10.coverage", "2715f61f-2654-47f4-a864-7763836c6bf7");
        int index = (stack.size() - 1) - n;
        writeline("/home/ubuntu/results/coverage/Digester/Digester_8_10.coverage", "2f45eb1a-1841-4918-9e4a-b1a99ed78218");
        if (index < 0) {
            writeline("/home/ubuntu/results/coverage/Digester/Digester_8_10.coverage", "d28f5c7b-7377-472f-8a91-251d7aacfedd");
            log.warn("Empty stack (returning null)");
            writeline("/home/ubuntu/results/coverage/Digester/Digester_8_10.coverage", "ba91374e-5bc4-4ea5-a347-2ebe5db45478");
            return (null);
        }
        writeline("/home/ubuntu/results/coverage/Digester/Digester_8_10.coverage", "82095952-3b30-46ef-aeab-ffafe0401c02");
        try {
            writeline("/home/ubuntu/results/coverage/Digester/Digester_8_10.coverage", "a2028cdd-bd79-4e87-9f06-7db0437707b0");
            return this.<T>npeSafeCast(stack.get(index));
        } catch (EmptyStackException e) {
            writeline("/home/ubuntu/results/coverage/Digester/Digester_8_10.coverage", "7749e7e6-c70b-4e40-af05-6dcc5090bc21");
            log.warn("Empty stack (returning null)");
            writeline("/home/ubuntu/results/coverage/Digester/Digester_8_10.coverage", "76f25e57-e605-4cbb-bb91-e5bd16a658b0");
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
        writeline("/home/ubuntu/results/coverage/Digester/Digester_8_10.coverage", "11da4b4a-105c-4129-8a06-564f03392a15");
        try {
            writeline("/home/ubuntu/results/coverage/Digester/Digester_8_10.coverage", "326a3c36-d38d-4369-a6dc-aa742263d947");
            T popped = this.<T>npeSafeCast(stack.pop());
            writeline("/home/ubuntu/results/coverage/Digester/Digester_8_10.coverage", "113db02a-0e76-46e8-a521-4b847cd9014c");
            if (stackAction != null) {
                writeline("/home/ubuntu/results/coverage/Digester/Digester_8_10.coverage", "14059a96-b2fb-4f7d-94b0-6f5c559706d3");
                popped = stackAction.onPop(this, null, popped);
            }
            writeline("/home/ubuntu/results/coverage/Digester/Digester_8_10.coverage", "cf7e1546-e177-43d5-be44-8f0cba7e7462");
            return popped;
        } catch (EmptyStackException e) {
            writeline("/home/ubuntu/results/coverage/Digester/Digester_8_10.coverage", "e6341745-2e6e-4211-ae36-8e3af1d2628c");
            log.warn("Empty stack (returning null)");
            writeline("/home/ubuntu/results/coverage/Digester/Digester_8_10.coverage", "46debfb6-d086-405d-95b6-f6520f0b41a2");
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
        writeline("/home/ubuntu/results/coverage/Digester/Digester_8_10.coverage", "6fcec98c-e44b-4b19-bb85-76ab8ce0cf2d");
        if (stackAction != null) {
            writeline("/home/ubuntu/results/coverage/Digester/Digester_8_10.coverage", "41eb6119-b18e-4170-84d4-7c3290a492b0");
            object = stackAction.onPush(this, null, object);
        }
        writeline("/home/ubuntu/results/coverage/Digester/Digester_8_10.coverage", "0d26a634-f83f-49a0-9a1e-f7945aee86b9");
        if (stack.size() == 0) {
            writeline("/home/ubuntu/results/coverage/Digester/Digester_8_10.coverage", "9e2594bb-b651-4aef-a55a-d4ee478dc0a1");
            root = object;
        }
        writeline("/home/ubuntu/results/coverage/Digester/Digester_8_10.coverage", "2b868187-1bbf-4957-b007-95d34161a2b0");
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
        writeline("/home/ubuntu/results/coverage/Digester/Digester_8_10.coverage", "2cf0b0a3-58c0-45fc-aa35-978129826df1");
        if (stackAction != null) {
            writeline("/home/ubuntu/results/coverage/Digester/Digester_8_10.coverage", "53d90ad9-f057-4934-9b3d-c2d32c24ac8c");
            value = stackAction.onPush(this, stackName, value);
        }
        writeline("/home/ubuntu/results/coverage/Digester/Digester_8_10.coverage", "a0ff7b03-fb77-417f-b25a-1fdeb7a5ecfc");
        Stack<Object> namedStack = stacksByName.get(stackName);
        writeline("/home/ubuntu/results/coverage/Digester/Digester_8_10.coverage", "31376258-16f9-4045-81a6-373d708c6f7d");
        if (namedStack == null) {
            writeline("/home/ubuntu/results/coverage/Digester/Digester_8_10.coverage", "7b01c188-b812-4e8a-a6f3-df0f78b1e86c");
            namedStack = new Stack<Object>();
            writeline("/home/ubuntu/results/coverage/Digester/Digester_8_10.coverage", "250be6fe-b64e-4925-ac9c-fe4c511e700c");
            stacksByName.put(stackName, namedStack);
        }
        writeline("/home/ubuntu/results/coverage/Digester/Digester_8_10.coverage", "891b026d-9649-44a7-93a1-eb710df212a7");
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
        writeline("/home/ubuntu/results/coverage/Digester/Digester_8_10.coverage", "04893c78-b69e-40fc-8473-ad2127ef2350");
        T result = null;
        writeline("/home/ubuntu/results/coverage/Digester/Digester_8_10.coverage", "b888e5da-0c24-4761-aa59-81b641b47788");
        Stack<Object> namedStack = stacksByName.get(stackName);
        writeline("/home/ubuntu/results/coverage/Digester/Digester_8_10.coverage", "af51aa30-500b-45a3-b063-8382f0794e60");
        if (namedStack == null) {
            writeline("/home/ubuntu/results/coverage/Digester/Digester_8_10.coverage", "df8c45e8-8e53-4c47-a305-7e37aeb26ed3");
            if (log.isDebugEnabled()) {
                writeline("/home/ubuntu/results/coverage/Digester/Digester_8_10.coverage", "b7e039c1-58be-426f-8422-456d176008a4");
                log.debug("Stack '" + stackName + "' is empty");
            }
        }
        writeline("/home/ubuntu/results/coverage/Digester/Digester_8_10.coverage", "b0ff6c3c-75bd-4d89-963a-756e7d69ece1");
        result = this.<T>npeSafeCast(namedStack.pop());
        writeline("/home/ubuntu/results/coverage/Digester/Digester_8_10.coverage", "bdd07f1c-a1d1-490b-8cff-6ff5dc219313");
        if (stackAction != null) {
            writeline("/home/ubuntu/results/coverage/Digester/Digester_8_10.coverage", "bee8791b-de7b-4c98-9a50-76b316d2cd3a");
            result = stackAction.onPop(this, stackName, result);
        }
        writeline("/home/ubuntu/results/coverage/Digester/Digester_8_10.coverage", "723a008c-66ff-4076-b660-0519a2b4ceb3");
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
        writeline("/home/ubuntu/results/coverage/Digester/Digester_8_10.coverage", "f2cccbad-d0cb-48df-8d39-d8390e99e8de");
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
        writeline("/home/ubuntu/results/coverage/Digester/Digester_8_10.coverage", "fc0e45d3-890b-4e29-97ec-c372bcdefbfc");
        T result = null;
        writeline("/home/ubuntu/results/coverage/Digester/Digester_8_10.coverage", "d9fbdff4-6488-45c4-a437-11af5bd9e5ee");
        Stack<Object> namedStack = stacksByName.get(stackName);
        writeline("/home/ubuntu/results/coverage/Digester/Digester_8_10.coverage", "bd39c34f-cdc7-45a2-818b-c124f26a51fa");
        if (namedStack == null) {
            writeline("/home/ubuntu/results/coverage/Digester/Digester_8_10.coverage", "1776728b-399a-480a-b080-299b10762281");
            if (log.isDebugEnabled()) {
                writeline("/home/ubuntu/results/coverage/Digester/Digester_8_10.coverage", "e382d1a5-c7c9-4487-8735-669df0932cac");
                log.debug("Stack '" + stackName + "' is empty");
            }
        }
        writeline("/home/ubuntu/results/coverage/Digester/Digester_8_10.coverage", "522dfec1-a118-4026-9806-c739a512ff89");
        int index = (namedStack.size() - 1) - n;
        writeline("/home/ubuntu/results/coverage/Digester/Digester_8_10.coverage", "c90ab23f-a800-4da9-ab9c-47ba2e328946");
        if (index < 0) {
        }
        writeline("/home/ubuntu/results/coverage/Digester/Digester_8_10.coverage", "ff64c806-0807-4e21-b3d8-3a9468cc392c");
        result = this.<T>npeSafeCast(namedStack.get(index));
        writeline("/home/ubuntu/results/coverage/Digester/Digester_8_10.coverage", "6651baeb-61ce-468b-9dfc-ee3625d01b63");
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
        writeline("/home/ubuntu/results/coverage/Digester/Digester_8_10.coverage", "75e68dad-8a1c-4241-850b-53dbc5dc6f9b");
        boolean result = true;
        writeline("/home/ubuntu/results/coverage/Digester/Digester_8_10.coverage", "875b0ca0-1e61-4d5d-a613-7155bc3350bc");
        Stack<Object> namedStack = stacksByName.get(stackName);
        writeline("/home/ubuntu/results/coverage/Digester/Digester_8_10.coverage", "38ebb829-006a-482b-a719-1dd7e640f140");
        if (namedStack != null) {
            writeline("/home/ubuntu/results/coverage/Digester/Digester_8_10.coverage", "6ee230de-9a2c-4cfe-8490-8a9b7225fa6e");
            result = namedStack.isEmpty();
        }
        writeline("/home/ubuntu/results/coverage/Digester/Digester_8_10.coverage", "b600b96a-3bc4-4453-ac26-ccd066da7c51");
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
        writeline("/home/ubuntu/results/coverage/Digester/Digester_8_10.coverage", "6aa678ab-a932-410e-aeca-ab4ba8d24f31");
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
        writeline("/home/ubuntu/results/coverage/Digester/Digester_8_10.coverage", "a7279c23-2566-4400-84b6-78b40c3d6b09");
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
        writeline("/home/ubuntu/results/coverage/Digester/Digester_8_10.coverage", "5d923d62-0dbd-40e0-8290-2ea974bd536e");
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
        writeline("/home/ubuntu/results/coverage/Digester/Digester_8_10.coverage", "166c4956-6bb5-4c45-8844-58c7d7be7b34");
        if (configured) {
            writeline("/home/ubuntu/results/coverage/Digester/Digester_8_10.coverage", "2bf1aacf-e94e-4cc8-893a-704d84d1dfc7");
            return;
        }
        writeline("/home/ubuntu/results/coverage/Digester/Digester_8_10.coverage", "a293e8c3-82c8-4fe7-982b-43b2926099ac");
        initialize();
        writeline("/home/ubuntu/results/coverage/Digester/Digester_8_10.coverage", "365cdbe7-6f3f-4692-abc7-6aaf13cf3a0e");
        configured = true;
    }

    /**
     * Checks the Digester instance has been configured.
     *
     * @return true, if the Digester instance has been configured, false otherwise
     * @since 3.0
     */
    public boolean isConfigured() {
        writeline("/home/ubuntu/results/coverage/Digester/Digester_8_10.coverage", "79f2f7fc-a2a7-4088-a4e7-ed0520fbd9df");
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
        writeline("/home/ubuntu/results/coverage/Digester/Digester_8_10.coverage", "5eff2fe9-b114-4ecf-812e-ff16b819aa67");
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
        writeline("/home/ubuntu/results/coverage/Digester/Digester_8_10.coverage", "d524d576-ec05-4d37-acec-8490a0164ad4");
        try {
            writeline("/home/ubuntu/results/coverage/Digester/Digester_8_10.coverage", "fa310c32-002d-49cf-a9ff-94833de609eb");
            return (params.peek());
        } catch (EmptyStackException e) {
            writeline("/home/ubuntu/results/coverage/Digester/Digester_8_10.coverage", "96f23a5b-2dda-40f3-83cb-08da96cd32bd");
            log.warn("Empty stack (returning null)");
            writeline("/home/ubuntu/results/coverage/Digester/Digester_8_10.coverage", "e311ce80-a179-42bd-ab08-40c8461a485c");
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
        writeline("/home/ubuntu/results/coverage/Digester/Digester_8_10.coverage", "e04892cb-6c25-419f-b497-3dda8c1fb64a");
        int index = (params.size() - 1) - n;
        writeline("/home/ubuntu/results/coverage/Digester/Digester_8_10.coverage", "a1814893-aeb5-4256-a115-5a500940d037");
        if (index < 0) {
            writeline("/home/ubuntu/results/coverage/Digester/Digester_8_10.coverage", "2b3a6d42-33ef-466a-ab4a-79f9be86929f");
            log.warn("Empty stack (returning null)");
            writeline("/home/ubuntu/results/coverage/Digester/Digester_8_10.coverage", "1efa8946-2fbc-4ad4-b91b-57b33be6ed0d");
            return (null);
        }
        writeline("/home/ubuntu/results/coverage/Digester/Digester_8_10.coverage", "4747fc93-674a-4b6a-ae9f-8d01c35ed9c6");
        try {
            writeline("/home/ubuntu/results/coverage/Digester/Digester_8_10.coverage", "e7749074-4faa-41ef-b919-0e5e6ac7db63");
            return (params.get(index));
        } catch (EmptyStackException e) {
            writeline("/home/ubuntu/results/coverage/Digester/Digester_8_10.coverage", "dbdea13f-014a-498c-98bd-f3f282d7cafd");
            log.warn("Empty stack (returning null)");
            writeline("/home/ubuntu/results/coverage/Digester/Digester_8_10.coverage", "469b6784-3ff8-4fad-be1b-74bb4c059f76");
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
        writeline("/home/ubuntu/results/coverage/Digester/Digester_8_10.coverage", "fedc4d23-7408-42f1-b473-9f0e21924dd3");
        try {
            writeline("/home/ubuntu/results/coverage/Digester/Digester_8_10.coverage", "2956d52f-7b5a-4449-b3b5-0d578becd7af");
            if (log.isTraceEnabled()) {
                writeline("/home/ubuntu/results/coverage/Digester/Digester_8_10.coverage", "5da4a210-e37d-47e4-b53e-83731ccad8d0");
                log.trace("Popping params");
            }
            writeline("/home/ubuntu/results/coverage/Digester/Digester_8_10.coverage", "716caeaf-0240-40de-94c6-769d4f355965");
            return (params.pop());
        } catch (EmptyStackException e) {
            writeline("/home/ubuntu/results/coverage/Digester/Digester_8_10.coverage", "00ed5f35-1bcb-40e4-aef2-a21aa24a17f6");
            log.warn("Empty stack (returning null)");
            writeline("/home/ubuntu/results/coverage/Digester/Digester_8_10.coverage", "2db36fed-490d-42c8-a33f-7a2790a0608b");
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
        writeline("/home/ubuntu/results/coverage/Digester/Digester_8_10.coverage", "47b4ec57-9868-4262-aa13-f50f99764326");
        if (log.isTraceEnabled()) {
            writeline("/home/ubuntu/results/coverage/Digester/Digester_8_10.coverage", "686efc87-1889-44d0-9a68-e769d4897f9f");
            log.trace("Pushing params");
        }
        writeline("/home/ubuntu/results/coverage/Digester/Digester_8_10.coverage", "eea43ffc-819d-4273-94f6-cd88ae42896d");
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
        writeline("/home/ubuntu/results/coverage/Digester/Digester_8_10.coverage", "b3924656-13c5-4b6b-ac6e-99b16d39266e");
        if ((e != null) && (e instanceof InvocationTargetException)) {
            writeline("/home/ubuntu/results/coverage/Digester/Digester_8_10.coverage", "50fc6585-6ae5-45b9-8f9e-85fc809c2b9e");
            Throwable t = ((InvocationTargetException) e).getTargetException();
            writeline("/home/ubuntu/results/coverage/Digester/Digester_8_10.coverage", "3ff1c392-0824-4770-95d7-41c3e646f479");
            if ((t != null) && (t instanceof Exception)) {
                writeline("/home/ubuntu/results/coverage/Digester/Digester_8_10.coverage", "e4d6ae43-aa87-4364-91d1-7bd61e982ba6");
                e = (Exception) t;
            }
        }
        writeline("/home/ubuntu/results/coverage/Digester/Digester_8_10.coverage", "e0836c79-65f1-4dcc-befd-264e7538d6af");
        if (locator != null) {
            writeline("/home/ubuntu/results/coverage/Digester/Digester_8_10.coverage", "02c5a514-0e38-4122-8255-a60e1409245b");
            String error = "Error at line " + locator.getLineNumber() + " char " + locator.getColumnNumber() + ": " + message;
            writeline("/home/ubuntu/results/coverage/Digester/Digester_8_10.coverage", "9cccc40e-cd6b-44b0-a569-71394cd9d394");
            if (e != null) {
                writeline("/home/ubuntu/results/coverage/Digester/Digester_8_10.coverage", "034223b2-c7dd-49aa-91ee-39f9f769db61");
                return new SAXParseException(error, locator, e);
            }
            writeline("/home/ubuntu/results/coverage/Digester/Digester_8_10.coverage", "498b8b26-5438-409a-a1b2-d9cbd08d3817");
            return new SAXParseException(error, locator);
        }
        writeline("/home/ubuntu/results/coverage/Digester/Digester_8_10.coverage", "e6f0ed34-50bf-4a5f-827d-f6023ae049bf");
        log.error("No Locator!");
        writeline("/home/ubuntu/results/coverage/Digester/Digester_8_10.coverage", "5f80c8ae-2a97-462a-993b-9326bf5ec6b4");
        if (e != null) {
            writeline("/home/ubuntu/results/coverage/Digester/Digester_8_10.coverage", "740a27b7-6a27-460a-80e2-079e7d08a587");
            return new SAXException(message, e);
        }
        writeline("/home/ubuntu/results/coverage/Digester/Digester_8_10.coverage", "28123164-36b1-4edf-a943-b2e43968b7e2");
        return new SAXException(message);
    }

    /**
     * Create a SAX exception which also understands about the location in the digester file where the exception occurs
     *
     * @param e the exception cause
     * @return the new SAX exception
     */
    public SAXException createSAXException(Exception e) {
        writeline("/home/ubuntu/results/coverage/Digester/Digester_8_10.coverage", "e7b19c9b-2736-4fa9-b324-2f8c1b26b7f2");
        if (e instanceof InvocationTargetException) {
            writeline("/home/ubuntu/results/coverage/Digester/Digester_8_10.coverage", "d574d568-9128-4847-958e-d788e1525ce4");
            Throwable t = ((InvocationTargetException) e).getTargetException();
            writeline("/home/ubuntu/results/coverage/Digester/Digester_8_10.coverage", "8fcc9148-601a-4dec-944b-07dae9d159c7");
            if ((t != null) && (t instanceof Exception)) {
                writeline("/home/ubuntu/results/coverage/Digester/Digester_8_10.coverage", "e1ac2183-1645-4b58-9030-48f9e9e7e44c");
                e = (Exception) t;
            }
        }
        writeline("/home/ubuntu/results/coverage/Digester/Digester_8_10.coverage", "ea3b46cc-09e0-427b-90af-774b5ea3c8c2");
        return createSAXException(e.getMessage(), e);
    }

    /**
     * Create a SAX exception which also understands about the location in the digester file where the exception occurs
     *
     * @param message the custom SAX exception message
     * @return the new SAX exception
     */
    public SAXException createSAXException(String message) {
        writeline("/home/ubuntu/results/coverage/Digester/Digester_8_10.coverage", "39d741e6-453f-4782-93d1-77dd22662bf3");
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
        writeline("/home/ubuntu/results/coverage/Digester/Digester_8_10.coverage", "8107a78f-36a2-491b-8f89-156be83ea9bd");
        if (obj == null) {
            writeline("/home/ubuntu/results/coverage/Digester/Digester_8_10.coverage", "fcb4c596-969f-4bf3-9b04-46bcb870c21e");
            return null;
        }
        writeline("/home/ubuntu/results/coverage/Digester/Digester_8_10.coverage", "0041c1fa-eb4a-45c4-a7b5-491682dff8ef");
        @SuppressWarnings("unchecked") T result = (T) obj;
        writeline("/home/ubuntu/results/coverage/Digester/Digester_8_10.coverage", "9307f63e-ac0b-46c9-b7a2-83b8845a03bf");
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
