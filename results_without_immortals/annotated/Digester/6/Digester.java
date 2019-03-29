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
        writeline("/home/ubuntu/results/coverage/Digester/Digester_6_10.coverage", "e69f5340-f737-4361-b170-e957e4ba8fb3");
        Stack<String> nsStack = namespaces.get(prefix);
        writeline("/home/ubuntu/results/coverage/Digester/Digester_6_10.coverage", "a50d3cbc-f333-440f-ba35-2908bba14e51");
        if (nsStack == null) {
            writeline("/home/ubuntu/results/coverage/Digester/Digester_6_10.coverage", "f0165765-2a4b-4eed-8e61-fe67ff45cb8f");
            return null;
        }
        writeline("/home/ubuntu/results/coverage/Digester/Digester_6_10.coverage", "d9454802-a63e-403b-a403-4f453337b0c1");
        try {
            writeline("/home/ubuntu/results/coverage/Digester/Digester_6_10.coverage", "10572378-21e3-403c-8cee-8afc5c0e1fba");
            return (nsStack.peek());
        } catch (EmptyStackException e) {
            writeline("/home/ubuntu/results/coverage/Digester/Digester_6_10.coverage", "a849d2da-0a50-4c77-8996-70a03af98471");
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
        writeline("/home/ubuntu/results/coverage/Digester/Digester_6_10.coverage", "6a3921f4-ab3a-4ec5-bf9a-54c86ec75721");
        if (this.classLoader != null) {
            writeline("/home/ubuntu/results/coverage/Digester/Digester_6_10.coverage", "c6b37c31-6564-4cf5-810a-75cc4d0a9d5f");
            return (this.classLoader);
        }
        writeline("/home/ubuntu/results/coverage/Digester/Digester_6_10.coverage", "bb9eae2c-b743-4ba3-a74e-3ef870dc706b");
        if (this.useContextClassLoader) {
            writeline("/home/ubuntu/results/coverage/Digester/Digester_6_10.coverage", "9c0ee27f-0c7e-49b9-9e2d-7eebf50c801e");
            ClassLoader classLoader = Thread.currentThread().getContextClassLoader();
            writeline("/home/ubuntu/results/coverage/Digester/Digester_6_10.coverage", "c27f109e-3efd-417e-b96a-4a14eda39c74");
            if (classLoader != null) {
                writeline("/home/ubuntu/results/coverage/Digester/Digester_6_10.coverage", "a71c6924-93e3-4920-a7e5-19cf00fcf12c");
                return (classLoader);
            }
        }
        writeline("/home/ubuntu/results/coverage/Digester/Digester_6_10.coverage", "4e8bb916-3fef-4dea-802d-882ddb6794a2");
        return (this.getClass().getClassLoader());
    }

    /**
     * Set the class loader to be used for instantiating application objects when required.
     *
     * @param classLoader The new class loader to use, or <code>null</code> to revert to the standard rules
     */
    public void setClassLoader(ClassLoader classLoader) {
        writeline("/home/ubuntu/results/coverage/Digester/Digester_6_10.coverage", "f3f37055-197c-4a6a-b604-bb354ef9ed2e");
        this.classLoader = classLoader;
    }

    /**
     * Return the current depth of the element stack.
     *
     * @return the current depth of the element stack.
     */
    public int getCount() {
        writeline("/home/ubuntu/results/coverage/Digester/Digester_6_10.coverage", "b1bce913-21f1-4177-a8c6-2bf2ed3978d8");
        return (stack.size());
    }

    /**
     * Return the name of the XML element that is currently being processed.
     *
     * @return the name of the XML element that is currently being processed.
     */
    public String getCurrentElementName() {
        writeline("/home/ubuntu/results/coverage/Digester/Digester_6_10.coverage", "dfe86a0e-197d-4645-8b4c-1ab244dd122f");
        String elementName = match;
        writeline("/home/ubuntu/results/coverage/Digester/Digester_6_10.coverage", "0b0932f4-bd6d-416f-9463-df02d34da446");
        int lastSlash = elementName.lastIndexOf('/');
        writeline("/home/ubuntu/results/coverage/Digester/Digester_6_10.coverage", "19a653f0-e893-4379-bbdc-c352b6a37618");
        if (lastSlash >= 0) {
            writeline("/home/ubuntu/results/coverage/Digester/Digester_6_10.coverage", "7a83f25c-924e-4cf0-98ef-893834ffc66f");
            elementName = elementName.substring(lastSlash + 1);
        }
        writeline("/home/ubuntu/results/coverage/Digester/Digester_6_10.coverage", "3e28321f-eadd-4a96-ab0b-22ccc67e8ce2");
        return (elementName);
    }

    /**
     * Return the error handler for this Digester.
     *
     * @return the error handler for this Digester.
     */
    public ErrorHandler getErrorHandler() {
        writeline("/home/ubuntu/results/coverage/Digester/Digester_6_10.coverage", "07f22e31-1e75-4b2e-86f1-68ca1eaff97f");
        return (this.errorHandler);
    }

    /**
     * Set the error handler for this Digester.
     *
     * @param errorHandler The new error handler
     */
    public void setErrorHandler(ErrorHandler errorHandler) {
        writeline("/home/ubuntu/results/coverage/Digester/Digester_6_10.coverage", "cb680578-7356-4337-b2ab-694c0edd16d0");
        this.errorHandler = errorHandler;
    }

    /**
     * Return the SAXParserFactory we will use, creating one if necessary.
     *
     * @return the SAXParserFactory we will use, creating one if necessary.
     */
    public SAXParserFactory getFactory() {
        writeline("/home/ubuntu/results/coverage/Digester/Digester_6_10.coverage", "92eef27c-2a62-46aa-acbc-8016385044ea");
        if (factory == null) {
            writeline("/home/ubuntu/results/coverage/Digester/Digester_6_10.coverage", "fba7a163-6bb5-4fd6-b119-67c481d87a01");
            factory = SAXParserFactory.newInstance();
            writeline("/home/ubuntu/results/coverage/Digester/Digester_6_10.coverage", "e758bea2-3670-4240-b63e-46bd6b52207c");
            factory.setNamespaceAware(namespaceAware);
            writeline("/home/ubuntu/results/coverage/Digester/Digester_6_10.coverage", "31f23f58-3d86-4407-9469-55e8dcac160e");
            factory.setXIncludeAware(xincludeAware);
            writeline("/home/ubuntu/results/coverage/Digester/Digester_6_10.coverage", "53766b6e-4a84-46b2-82c2-6a3e34725457");
            factory.setValidating(validating);
            writeline("/home/ubuntu/results/coverage/Digester/Digester_6_10.coverage", "abb9d73f-6299-4dac-afcd-4c419379d56b");
            factory.setSchema(schema);
        }
        writeline("/home/ubuntu/results/coverage/Digester/Digester_6_10.coverage", "44e57ce1-7f9b-46b6-8c0b-ad0919904ad8");
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
        writeline("/home/ubuntu/results/coverage/Digester/Digester_6_10.coverage", "1d2fd720-92c5-4745-af5b-d2387364f13f");
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
        writeline("/home/ubuntu/results/coverage/Digester/Digester_6_10.coverage", "fd6590d8-2b60-45ad-a4cc-ec8fa9aca4e1");
        getFactory().setFeature(feature, value);
    }

    /**
     * Return the current Logger associated with this instance of the Digester
     *
     * @return the current Logger associated with this instance of the Digester
     */
    public Log getLogger() {
        writeline("/home/ubuntu/results/coverage/Digester/Digester_6_10.coverage", "699f30ec-b74d-4c11-95e0-04e6be1764c2");
        return log;
    }

    /**
     * Set the current logger for this Digester.
     *
     * @param log the current logger for this Digester.
     */
    public void setLogger(Log log) {
        writeline("/home/ubuntu/results/coverage/Digester/Digester_6_10.coverage", "54713ceb-195a-4065-9b4d-5b8fda790e1f");
        this.log = log;
    }

    /**
     * Gets the logger used for logging SAX-related information. <strong>Note</strong> the output is finely grained.
     *
     * @return the logger used for logging SAX-related information
     * @since 1.6
     */
    public Log getSAXLogger() {
        writeline("/home/ubuntu/results/coverage/Digester/Digester_6_10.coverage", "8bb2efb7-fe5e-46cc-a45a-46b80fac5979");
        return saxLog;
    }

    /**
     * Sets the logger used for logging SAX-related information. <strong>Note</strong> the output is finely grained.
     *
     * @param saxLog the logger used for logging SAX-related information, not null
     * @since 1.6
     */
    public void setSAXLogger(Log saxLog) {
        writeline("/home/ubuntu/results/coverage/Digester/Digester_6_10.coverage", "883b010f-2a67-49e4-a1c1-577227194e93");
        this.saxLog = saxLog;
    }

    /**
     * Return the current rule match path
     *
     * @return the current rule match path
     */
    public String getMatch() {
        writeline("/home/ubuntu/results/coverage/Digester/Digester_6_10.coverage", "3b6784dd-15c7-4257-863d-a0d80459ce58");
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
        writeline("/home/ubuntu/results/coverage/Digester/Digester_6_10.coverage", "7048a47a-5973-47f1-bafe-7e8411ff3af3");
        return matches;
    }

    /**
     * Return the "namespace aware" flag for parsers we create.
     *
     * @return the "namespace aware" flag for parsers we create.
     */
    public boolean getNamespaceAware() {
        writeline("/home/ubuntu/results/coverage/Digester/Digester_6_10.coverage", "8d50a40c-5f3d-4ef5-89e5-b9634dcdf243");
        return (this.namespaceAware);
    }

    /**
     * Set the "namespace aware" flag for parsers we create.
     *
     * @param namespaceAware The new "namespace aware" flag
     */
    public void setNamespaceAware(boolean namespaceAware) {
        writeline("/home/ubuntu/results/coverage/Digester/Digester_6_10.coverage", "8c68a4c1-48ac-49ff-bae2-b60044731314");
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
        writeline("/home/ubuntu/results/coverage/Digester/Digester_6_10.coverage", "7419f25a-fd74-44bf-8d1f-973edd20e169");
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
        writeline("/home/ubuntu/results/coverage/Digester/Digester_6_10.coverage", "17808740-f323-4970-92e4-659af3e4074a");
        this.xincludeAware = xincludeAware;
    }

    /**
     * Set the public id of the current file being parse.
     *
     * @param publicId the DTD/Schema public's id.
     */
    public void setPublicId(String publicId) {
        writeline("/home/ubuntu/results/coverage/Digester/Digester_6_10.coverage", "3aefe43a-d9d7-4042-bf56-bba751c1ea8b");
        this.publicId = publicId;
    }

    /**
     * Return the public identifier of the DTD we are currently parsing under, if any.
     *
     * @return the public identifier of the DTD we are currently parsing under, if any.
     */
    public String getPublicId() {
        writeline("/home/ubuntu/results/coverage/Digester/Digester_6_10.coverage", "95b63574-64f0-427a-83fb-bbd516b34a33");
        return (this.publicId);
    }

    /**
     * Return the namespace URI that will be applied to all subsequently added <code>Rule</code> objects.
     *
     * @return the namespace URI that will be applied to all subsequently added <code>Rule</code> objects.
     */
    public String getRuleNamespaceURI() {
        writeline("/home/ubuntu/results/coverage/Digester/Digester_6_10.coverage", "0e9f6357-7081-4d3a-b3aa-e270a6f959ff");
        return (getRules().getNamespaceURI());
    }

    /**
     * Set the namespace URI that will be applied to all subsequently added <code>Rule</code> objects.
     *
     * @param ruleNamespaceURI Namespace URI that must match on all subsequently added rules, or <code>null</code> for
     *            matching regardless of the current namespace URI
     */
    public void setRuleNamespaceURI(String ruleNamespaceURI) {
        writeline("/home/ubuntu/results/coverage/Digester/Digester_6_10.coverage", "33167f1a-b1fb-4f31-b987-5328444bcc71");
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
        writeline("/home/ubuntu/results/coverage/Digester/Digester_6_10.coverage", "88c5b5b3-5d4e-4801-94eb-4990c97f4be4");
        if (parser != null) {
            writeline("/home/ubuntu/results/coverage/Digester/Digester_6_10.coverage", "74b27134-da4f-40e8-afea-7bb1f06e639f");
            return (parser);
        }
        writeline("/home/ubuntu/results/coverage/Digester/Digester_6_10.coverage", "a8aaedf5-e473-44d8-9e12-3199bdc0be51");
        try {
            writeline("/home/ubuntu/results/coverage/Digester/Digester_6_10.coverage", "2f653a14-3810-4ae9-a533-2e3372da6048");
            parser = getFactory().newSAXParser();
        } catch (Exception e) {
            writeline("/home/ubuntu/results/coverage/Digester/Digester_6_10.coverage", "03f3ee4b-0f6a-46fb-8284-e1d93131205c");
            log.error("Digester.getParser: ", e);
            writeline("/home/ubuntu/results/coverage/Digester/Digester_6_10.coverage", "a1fa266c-e4c1-442b-ba87-9adf3e5f6ac8");
            return (null);
        }
        writeline("/home/ubuntu/results/coverage/Digester/Digester_6_10.coverage", "9af24572-4a8f-47ae-bcc0-fa9a829cde02");
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
        writeline("/home/ubuntu/results/coverage/Digester/Digester_6_10.coverage", "cc2aa909-9ce2-41a9-b620-f1426daf6e50");
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
        writeline("/home/ubuntu/results/coverage/Digester/Digester_6_10.coverage", "a21fa1d1-627a-4b43-b5a9-6ae8bcc222eb");
        getParser().setProperty(property, value);
    }

    /**
     * Return the <code>Rules</code> implementation object containing our rules collection and associated matching
     * policy. If none has been established, a default implementation will be created and returned.
     *
     * @return the <code>Rules</code> implementation object.
     */
    public Rules getRules() {
        writeline("/home/ubuntu/results/coverage/Digester/Digester_6_10.coverage", "19dfadea-1bdb-4c89-b4c1-2e1460807907");
        if (this.rules == null) {
            writeline("/home/ubuntu/results/coverage/Digester/Digester_6_10.coverage", "83218f63-a2a6-4bf7-a9ae-dbe9c65d5050");
            this.rules = new RulesBase();
            writeline("/home/ubuntu/results/coverage/Digester/Digester_6_10.coverage", "7b232ceb-b27e-4e11-8d32-b8193020214f");
            this.rules.setDigester(this);
        }
        writeline("/home/ubuntu/results/coverage/Digester/Digester_6_10.coverage", "295675a4-b69b-4cc5-a9fb-94d401c4130b");
        return (this.rules);
    }

    /**
     * Set the <code>Rules</code> implementation object containing our rules collection and associated matching policy.
     *
     * @param rules New Rules implementation
     */
    public void setRules(Rules rules) {
        writeline("/home/ubuntu/results/coverage/Digester/Digester_6_10.coverage", "d22eca61-bec1-4ea0-9a90-ba0cbadd06eb");
        this.rules = rules;
        writeline("/home/ubuntu/results/coverage/Digester/Digester_6_10.coverage", "5bef761b-406d-4fc9-b1df-778e28eb45da");
        this.rules.setDigester(this);
    }

    /**
     * Return the XML Schema used when parsing.
     *
     * @return The {@link Schema} instance in use.
     * @since 2.0
     */
    public Schema getXMLSchema() {
        writeline("/home/ubuntu/results/coverage/Digester/Digester_6_10.coverage", "5719ebfb-b817-4fe4-8654-080f9c1816e2");
        return (this.schema);
    }

    /**
     * Set the XML Schema to be used when parsing.
     *
     * @param schema The {@link Schema} instance to use.
     * @since 2.0
     */
    public void setXMLSchema(Schema schema) {
        writeline("/home/ubuntu/results/coverage/Digester/Digester_6_10.coverage", "cb2721eb-5f4e-488a-8045-3bb166312b62");
        this.schema = schema;
    }

    /**
     * Return the boolean as to whether the context ClassLoader should be used.
     *
     * @return true, if the context ClassLoader should be used, false otherwise.
     */
    public boolean getUseContextClassLoader() {
        writeline("/home/ubuntu/results/coverage/Digester/Digester_6_10.coverage", "6a49f286-7822-4170-99b1-7a1d4f4fa2e7");
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
        writeline("/home/ubuntu/results/coverage/Digester/Digester_6_10.coverage", "5004e2b7-aced-48a8-a5c9-c9fbd2134840");
        useContextClassLoader = use;
    }

    /**
     * Return the validating parser flag.
     *
     * @return the validating parser flag.
     */
    public boolean getValidating() {
        writeline("/home/ubuntu/results/coverage/Digester/Digester_6_10.coverage", "61ea157a-d60e-43ec-ae30-00c7122a4aa3");
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
        writeline("/home/ubuntu/results/coverage/Digester/Digester_6_10.coverage", "40c29cea-8b32-488f-9a8c-5b796406a289");
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
        writeline("/home/ubuntu/results/coverage/Digester/Digester_6_10.coverage", "b679d2fd-3713-4bbd-921f-1ddf10c24ee2");
        if (reader == null) {
            writeline("/home/ubuntu/results/coverage/Digester/Digester_6_10.coverage", "af8f5e4d-bce4-411b-a981-fe2dd111cf7a");
            reader = getParser().getXMLReader();
        }
        writeline("/home/ubuntu/results/coverage/Digester/Digester_6_10.coverage", "a6419763-aa88-4150-9a9e-cb3952010d67");
        reader.setDTDHandler(this);
        writeline("/home/ubuntu/results/coverage/Digester/Digester_6_10.coverage", "03e988af-ed2d-4897-9272-4d0a1419533a");
        reader.setContentHandler(this);
        writeline("/home/ubuntu/results/coverage/Digester/Digester_6_10.coverage", "d4dd4aba-00af-4694-a072-272746723422");
        if (entityResolver == null) {
            writeline("/home/ubuntu/results/coverage/Digester/Digester_6_10.coverage", "0878d9b8-55d5-4a0d-a886-cc1092768192");
            reader.setEntityResolver(this);
        } else {
            writeline("/home/ubuntu/results/coverage/Digester/Digester_6_10.coverage", "9de59a8a-9164-4149-b5a9-2616179c7b2b");
            reader.setEntityResolver(entityResolver);
        }
        writeline("/home/ubuntu/results/coverage/Digester/Digester_6_10.coverage", "8a743e8e-c028-41f7-a219-29913a895e17");
        if (this.errorHandler != null) {
            writeline("/home/ubuntu/results/coverage/Digester/Digester_6_10.coverage", "5c9ec2c5-cc7b-464f-9e19-c22a79d167b2");
            reader.setErrorHandler(this.errorHandler);
        } else {
            writeline("/home/ubuntu/results/coverage/Digester/Digester_6_10.coverage", "88bf726d-4a38-4c91-afb9-25ea747429f9");
            reader.setErrorHandler(this);
        }
        writeline("/home/ubuntu/results/coverage/Digester/Digester_6_10.coverage", "0e005691-cf46-45c0-a0ad-e2cf8a5d0184");
        return reader;
    }

    /**
     * Gets the <code>Substitutor</code> used to convert attributes and body text.
     *
     * @return the <code>Substitutor</code> used to convert attributes and body text,
     *         null if not substitutions are to be performed.
     */
    public Substitutor getSubstitutor() {
        writeline("/home/ubuntu/results/coverage/Digester/Digester_6_10.coverage", "37a99220-387b-42a3-87ea-346154534b82");
        return substitutor;
    }

    /**
     * Sets the <code>Substitutor</code> to be used to convert attributes and body text.
     *
     * @param substitutor the Substitutor to be used to convert attributes and body text or null if not substitution of
     *            these values is to be performed.
     */
    public void setSubstitutor(Substitutor substitutor) {
        writeline("/home/ubuntu/results/coverage/Digester/Digester_6_10.coverage", "76fb6766-48d4-416d-9e92-8ffbb0405647");
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
        writeline("/home/ubuntu/results/coverage/Digester/Digester_6_10.coverage", "63253c9c-4104-4c53-864c-1fe3bc06342b");
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
        writeline("/home/ubuntu/results/coverage/Digester/Digester_6_10.coverage", "2db28f7f-5f3c-4760-97fa-629c7a69318e");
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
        writeline("/home/ubuntu/results/coverage/Digester/Digester_6_10.coverage", "10c2975c-976a-4104-afba-71abefdfa720");
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
        writeline("/home/ubuntu/results/coverage/Digester/Digester_6_10.coverage", "9b2ca1e7-3c4d-43dc-8eb8-ef0170cd69c1");
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
        writeline("/home/ubuntu/results/coverage/Digester/Digester_6_10.coverage", "6bc4a24f-a477-441b-9653-dc12ee5480b8");
        if (!namespaceAware) {
            writeline("/home/ubuntu/results/coverage/Digester/Digester_6_10.coverage", "37835ea4-dfda-4015-b5bb-6b847ebc8124");
            log.warn("Digester is not namespace aware");
        }
        writeline("/home/ubuntu/results/coverage/Digester/Digester_6_10.coverage", "c7835d99-74b9-45a3-a10a-b54999ef36de");
        Map<String, String> currentNamespaces = new HashMap<String, String>();
        writeline("/home/ubuntu/results/coverage/Digester/Digester_6_10.coverage", "2e142ab4-b59a-4e3e-8cb7-10fcf7f4cc5f");
        return currentNamespaces;
    }

    /**
     * Returns the executor service used to run asynchronous parse method.
     *
     * @return the executor service used to run asynchronous parse method
     * @since 3.1
     */
    public ExecutorService getExecutorService() {
        writeline("/home/ubuntu/results/coverage/Digester/Digester_6_10.coverage", "1b6b493f-4b4d-4e05-82b8-6a6469c36e44");
        return executorService;
    }

    /**
     * Sets the executor service to run asynchronous parse method.
     *
     * @param executorService the executor service to run asynchronous parse method
     * @since 3.1
     */
    public void setExecutorService(ExecutorService executorService) {
        writeline("/home/ubuntu/results/coverage/Digester/Digester_6_10.coverage", "d7458205-5be1-44ab-8759-bc4175d93e58");
        this.executorService = executorService;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public void characters(char buffer[], int start, int length) throws SAXException {
        writeline("/home/ubuntu/results/coverage/Digester/Digester_6_10.coverage", "62427da7-c79f-4f20-81ea-184bb45ddab3");
        if (customContentHandler != null) {
            writeline("/home/ubuntu/results/coverage/Digester/Digester_6_10.coverage", "fc96b421-973e-408e-998b-8effd4215cb0");
            customContentHandler.characters(buffer, start, length);
            writeline("/home/ubuntu/results/coverage/Digester/Digester_6_10.coverage", "12283f33-93c9-42d1-a2a0-3cda2bcc79b5");
            return;
        }
        writeline("/home/ubuntu/results/coverage/Digester/Digester_6_10.coverage", "a94d9f42-6e18-41fd-9e23-1122c7040034");
        if (saxLog.isDebugEnabled()) {
            writeline("/home/ubuntu/results/coverage/Digester/Digester_6_10.coverage", "65d4ddd1-60aa-448d-9951-e0ee24d71f50");
            saxLog.debug("characters(" + new String(buffer, start, length) + ")");
        }
        writeline("/home/ubuntu/results/coverage/Digester/Digester_6_10.coverage", "f3f3a955-991b-4fd1-aff9-6d4038aff1db");
        bodyText.append(buffer, start, length);
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public void endDocument() throws SAXException {
        writeline("/home/ubuntu/results/coverage/Digester/Digester_6_10.coverage", "bb7416f2-5891-4f8b-9ee2-54794a6b2dd4");
        if (saxLog.isDebugEnabled()) {
            writeline("/home/ubuntu/results/coverage/Digester/Digester_6_10.coverage", "e598a7cd-de34-4be3-be1f-52f29cec4fc8");
            if (getCount() > 1) {
                writeline("/home/ubuntu/results/coverage/Digester/Digester_6_10.coverage", "98378ff8-6c8b-42ec-a345-b3b265114b34");
                saxLog.debug("endDocument():  " + getCount() + " elements left");
            } else {
                writeline("/home/ubuntu/results/coverage/Digester/Digester_6_10.coverage", "f19e608c-0ec4-452c-be28-c326d5974dc7");
                saxLog.debug("endDocument()");
            }
        }
        writeline("/home/ubuntu/results/coverage/Digester/Digester_6_10.coverage", "5f8d828a-8917-4a7f-b23f-8dfb5cd1f74c");
        clear();
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public void endElement(String namespaceURI, String localName, String qName) throws SAXException {
        writeline("/home/ubuntu/results/coverage/Digester/Digester_6_10.coverage", "263926fa-08bc-4eb6-994b-259c4f8f5db7");
        if (customContentHandler != null) {
            writeline("/home/ubuntu/results/coverage/Digester/Digester_6_10.coverage", "2c77a3a6-50e0-499a-98d1-83140fee19a4");
            customContentHandler.endElement(namespaceURI, localName, qName);
            writeline("/home/ubuntu/results/coverage/Digester/Digester_6_10.coverage", "e6b3b8b7-8853-4d8f-a9c5-3cd43cd2a1a9");
            return;
        }
        writeline("/home/ubuntu/results/coverage/Digester/Digester_6_10.coverage", "d3a17739-524a-4548-841f-ddf25aebdcf0");
        boolean debug = log.isDebugEnabled();
        writeline("/home/ubuntu/results/coverage/Digester/Digester_6_10.coverage", "9e979335-deaf-4610-9658-b706a2929424");
        if (debug) {
            writeline("/home/ubuntu/results/coverage/Digester/Digester_6_10.coverage", "5d37c639-bf6c-4a8d-9daa-8bee1300a602");
            if (saxLog.isDebugEnabled()) {
                writeline("/home/ubuntu/results/coverage/Digester/Digester_6_10.coverage", "cd21935b-1c70-4c15-9043-58113f1ea80b");
                saxLog.debug("endElement(" + namespaceURI + "," + localName + "," + qName + ")");
            }
            writeline("/home/ubuntu/results/coverage/Digester/Digester_6_10.coverage", "dd6b0fce-456d-44b2-b456-916bedb172c7");
            log.debug("  match='" + match + "'");
            writeline("/home/ubuntu/results/coverage/Digester/Digester_6_10.coverage", "2dd7202d-abf9-481d-a46f-1cbd7f60ec5b");
            log.debug("  bodyText='" + bodyText + "'");
        }
        writeline("/home/ubuntu/results/coverage/Digester/Digester_6_10.coverage", "4e3ccc47-3c90-4267-ba0a-52d216a6e72e");
        String name = localName;
        writeline("/home/ubuntu/results/coverage/Digester/Digester_6_10.coverage", "87e57d16-e5a6-4d10-9c8d-9bbe10563953");
        if ((name == null) || (name.length() < 1)) {
            writeline("/home/ubuntu/results/coverage/Digester/Digester_6_10.coverage", "ab71027a-9ac3-4353-98b5-6ddd4fad8d0a");
            name = qName;
        }
        writeline("/home/ubuntu/results/coverage/Digester/Digester_6_10.coverage", "667363f9-f151-4259-b046-a99fd0b5cb9e");
        List<Rule> rules = matches.pop();
        writeline("/home/ubuntu/results/coverage/Digester/Digester_6_10.coverage", "cf673596-123f-4078-8a70-5c0928fb33f5");
        if ((rules != null) && (rules.size() > 0)) {
            writeline("/home/ubuntu/results/coverage/Digester/Digester_6_10.coverage", "e0da3246-aea8-4488-b4e5-92e6d37cf15e");
            String bodyText = this.bodyText.toString();
            writeline("/home/ubuntu/results/coverage/Digester/Digester_6_10.coverage", "59e24b0c-b964-48e1-a97d-20c33d968887");
            Substitutor substitutor = getSubstitutor();
            writeline("/home/ubuntu/results/coverage/Digester/Digester_6_10.coverage", "05592da2-a3b0-4d77-bf59-ea5b76ccfa00");
            if (substitutor != null) {
                writeline("/home/ubuntu/results/coverage/Digester/Digester_6_10.coverage", "0db9c44c-9939-4e48-b37a-b92733150a4f");
                bodyText = substitutor.substitute(bodyText);
            }
            writeline("/home/ubuntu/results/coverage/Digester/Digester_6_10.coverage", "c71497b0-5121-41d9-9d9c-a9530bb6d580");
            for (int i = 0; i < rules.size(); i++) {
                writeline("/home/ubuntu/results/coverage/Digester/Digester_6_10.coverage", "6a48dec5-2995-4c34-91c3-71a530143328");
                try {
                    writeline("/home/ubuntu/results/coverage/Digester/Digester_6_10.coverage", "8a3f068e-d6c2-47cc-b2fd-e0948eaae104");
                    Rule rule = rules.get(i);
                    writeline("/home/ubuntu/results/coverage/Digester/Digester_6_10.coverage", "1fff8abf-e9f8-4d14-8f16-b86c0419f9f2");
                    if (debug) {
                        writeline("/home/ubuntu/results/coverage/Digester/Digester_6_10.coverage", "431bf8c6-50ff-46df-bb54-2c57c0907f59");
                        log.debug("  Fire body() for " + rule);
                    }
                    writeline("/home/ubuntu/results/coverage/Digester/Digester_6_10.coverage", "54d1eade-13c4-4320-a158-c370d926acf1");
                    rule.body(namespaceURI, name, bodyText);
                } catch (Exception e) {
                    writeline("/home/ubuntu/results/coverage/Digester/Digester_6_10.coverage", "e0955193-8979-45b0-aa3b-6fe735410154");
                    log.error("Body event threw exception", e);
                } catch (Error e) {
                    writeline("/home/ubuntu/results/coverage/Digester/Digester_6_10.coverage", "0d79ce8a-02e3-481f-a871-1bd842e7c492");
                    log.error("Body event threw error", e);
                }
            }
        } else {
            writeline("/home/ubuntu/results/coverage/Digester/Digester_6_10.coverage", "a547c9cc-8b89-43f5-a252-eaa1f384e401");
            if (debug) {
                writeline("/home/ubuntu/results/coverage/Digester/Digester_6_10.coverage", "5e4d7295-21d1-4884-8497-f4b642ce3557");
                log.debug("  No rules found matching '" + match + "'.");
            }
        }
        writeline("/home/ubuntu/results/coverage/Digester/Digester_6_10.coverage", "2d33a37f-a102-49b4-9334-9260f8f45e29");
        bodyText = bodyTexts.pop();
        writeline("/home/ubuntu/results/coverage/Digester/Digester_6_10.coverage", "d5c61747-1410-4e0a-a5a0-27dddb04cb87");
        if (debug) {
            writeline("/home/ubuntu/results/coverage/Digester/Digester_6_10.coverage", "e13ee7a5-08d4-49b6-b5c4-a92be41bd083");
            log.debug("  Popping body text '" + bodyText.toString() + "'");
        }
        writeline("/home/ubuntu/results/coverage/Digester/Digester_6_10.coverage", "e267d994-5a5d-4bbb-bed6-5bda32ad8679");
        if (rules != null) {
            writeline("/home/ubuntu/results/coverage/Digester/Digester_6_10.coverage", "8dc173e2-6ca2-4658-8872-cf9ec7e57c74");
            for (int i = 0; i < rules.size(); i++) {
                writeline("/home/ubuntu/results/coverage/Digester/Digester_6_10.coverage", "f4eb95cb-9886-4d6f-b862-91d0ef3d3920");
                int j = (rules.size() - i) - 1;
                writeline("/home/ubuntu/results/coverage/Digester/Digester_6_10.coverage", "8ce0d017-fc90-43e7-882c-dc73ed5d338e");
                try {
                    writeline("/home/ubuntu/results/coverage/Digester/Digester_6_10.coverage", "c4980376-f264-4ce3-8e03-94e488302a29");
                    Rule rule = rules.get(j);
                    writeline("/home/ubuntu/results/coverage/Digester/Digester_6_10.coverage", "bc62d86b-3f4a-4d2f-80f1-2379e2b69edc");
                    if (debug) {
                        writeline("/home/ubuntu/results/coverage/Digester/Digester_6_10.coverage", "9db4cb94-bd0a-4768-9799-ee07403f21d8");
                        log.debug("  Fire end() for " + rule);
                    }
                    writeline("/home/ubuntu/results/coverage/Digester/Digester_6_10.coverage", "24fc0b77-bffd-4b34-adda-328ac2f12d66");
                    rule.end(namespaceURI, name);
                } catch (Exception e) {
                    writeline("/home/ubuntu/results/coverage/Digester/Digester_6_10.coverage", "6bbeca42-d2f7-4adf-bc8d-abc75fbcb60d");
                    log.error("End event threw exception", e);
                } catch (Error e) {
                    writeline("/home/ubuntu/results/coverage/Digester/Digester_6_10.coverage", "3bb74b73-1462-4df7-a40a-5e933ffdf421");
                    log.error("End event threw error", e);
                }
            }
        }
        writeline("/home/ubuntu/results/coverage/Digester/Digester_6_10.coverage", "f83584de-8f29-40cc-91a3-e596c173bbe7");
        int slash = match.lastIndexOf('/');
        writeline("/home/ubuntu/results/coverage/Digester/Digester_6_10.coverage", "57d8c5e9-ec04-41aa-a593-bfb86a4b38c5");
        if (slash >= 0) {
            writeline("/home/ubuntu/results/coverage/Digester/Digester_6_10.coverage", "594c8804-65f5-4cf1-aceb-60957155f8e0");
            match = match.substring(0, slash);
        } else {
            writeline("/home/ubuntu/results/coverage/Digester/Digester_6_10.coverage", "3a64fcf0-8c93-4bc7-a9a9-6ce6343129d3");
            match = "";
        }
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public void endPrefixMapping(String prefix) throws SAXException {
        writeline("/home/ubuntu/results/coverage/Digester/Digester_6_10.coverage", "cfc7b879-0bba-4113-b920-b46bac49929c");
        if (saxLog.isDebugEnabled()) {
            writeline("/home/ubuntu/results/coverage/Digester/Digester_6_10.coverage", "2c064703-6c34-4715-959e-bd91e0a8cc49");
            saxLog.debug("endPrefixMapping(" + prefix + ")");
        }
        writeline("/home/ubuntu/results/coverage/Digester/Digester_6_10.coverage", "cf3472b9-d494-4f19-a885-4b84902085f0");
        Stack<String> stack = namespaces.get(prefix);
        writeline("/home/ubuntu/results/coverage/Digester/Digester_6_10.coverage", "ee6b7591-709b-45b5-aaa5-e0e441ebf24d");
        if (stack == null) {
            writeline("/home/ubuntu/results/coverage/Digester/Digester_6_10.coverage", "c7f2ca99-d684-4db4-bc54-dad49645657f");
            return;
        }
        writeline("/home/ubuntu/results/coverage/Digester/Digester_6_10.coverage", "9b8b26c5-1bc9-4ec4-9eca-037c332dd601");
        try {
            writeline("/home/ubuntu/results/coverage/Digester/Digester_6_10.coverage", "e3718ce8-04a5-43ef-a6cb-9151d82f2c27");
            stack.pop();
            writeline("/home/ubuntu/results/coverage/Digester/Digester_6_10.coverage", "0ad978c8-cf96-4f2f-8cce-6fb3149577a8");
            if (stack.empty()) {
                writeline("/home/ubuntu/results/coverage/Digester/Digester_6_10.coverage", "70dfe030-c7b5-4c76-9d33-9657181b0add");
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
        writeline("/home/ubuntu/results/coverage/Digester/Digester_6_10.coverage", "48dcb36e-8a49-4df8-8b56-fa2e5782ef1f");
        if (saxLog.isDebugEnabled()) {
            writeline("/home/ubuntu/results/coverage/Digester/Digester_6_10.coverage", "092cbea2-4d8b-4b0d-82bb-3192311908fb");
            saxLog.debug("ignorableWhitespace(" + new String(buffer, start, len) + ")");
        }
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public void processingInstruction(String target, String data) throws SAXException {
        writeline("/home/ubuntu/results/coverage/Digester/Digester_6_10.coverage", "bf4fbd31-b981-4911-9a76-8944229d6be9");
        if (customContentHandler != null) {
            writeline("/home/ubuntu/results/coverage/Digester/Digester_6_10.coverage", "cfffad20-f44d-452b-a57c-b74867d002f2");
            customContentHandler.processingInstruction(target, data);
            writeline("/home/ubuntu/results/coverage/Digester/Digester_6_10.coverage", "84ab7b27-58d9-4b98-b6f0-c79e3255f0f0");
            return;
        }
        writeline("/home/ubuntu/results/coverage/Digester/Digester_6_10.coverage", "b1136ebf-2158-4430-a50c-84ca17661425");
        if (saxLog.isDebugEnabled()) {
            writeline("/home/ubuntu/results/coverage/Digester/Digester_6_10.coverage", "318ac80d-4c9e-47b4-bc8e-5b6a88c7ba8b");
            saxLog.debug("processingInstruction('" + target + "','" + data + "')");
        }
    }

    /**
     * Gets the document locator associated with our parser.
     *
     * @return the Locator supplied by the document parser
     */
    public Locator getDocumentLocator() {
        writeline("/home/ubuntu/results/coverage/Digester/Digester_6_10.coverage", "56c02b9d-a523-465d-abbf-6282447897c7");
        return locator;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public void setDocumentLocator(Locator locator) {
        writeline("/home/ubuntu/results/coverage/Digester/Digester_6_10.coverage", "ce6ecd36-6e92-4bab-88bb-921f803f5ef0");
        if (saxLog.isDebugEnabled()) {
            writeline("/home/ubuntu/results/coverage/Digester/Digester_6_10.coverage", "26440bda-ebda-4fb1-9c79-20908a60418c");
            saxLog.debug("setDocumentLocator(" + locator + ")");
        }
        writeline("/home/ubuntu/results/coverage/Digester/Digester_6_10.coverage", "4399b5a2-6c14-415a-a7bb-6f57f281ba58");
        this.locator = locator;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public void skippedEntity(String name) throws SAXException {
        writeline("/home/ubuntu/results/coverage/Digester/Digester_6_10.coverage", "c3282555-0625-44da-b482-93ba6489cce0");
        if (saxLog.isDebugEnabled()) {
            writeline("/home/ubuntu/results/coverage/Digester/Digester_6_10.coverage", "f9e53b39-da82-4332-bf5b-32418c57a30f");
            saxLog.debug("skippedEntity(" + name + ")");
        }
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public void startDocument() throws SAXException {
        writeline("/home/ubuntu/results/coverage/Digester/Digester_6_10.coverage", "a2f383ba-fd88-4125-8a82-ffd3b84abaa1");
        if (saxLog.isDebugEnabled()) {
            writeline("/home/ubuntu/results/coverage/Digester/Digester_6_10.coverage", "a11e15c3-8cab-40af-9d76-5f0ec1d190f0");
            saxLog.debug("startDocument()");
        }
        writeline("/home/ubuntu/results/coverage/Digester/Digester_6_10.coverage", "065a0e4c-ea19-45a1-859d-43003dba7ede");
        configure();
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public void startElement(String namespaceURI, String localName, String qName, Attributes list) throws SAXException {
        writeline("/home/ubuntu/results/coverage/Digester/Digester_6_10.coverage", "77348cd7-95c3-442a-875f-eb16d4d8e7e0");
        boolean debug = log.isDebugEnabled();
        writeline("/home/ubuntu/results/coverage/Digester/Digester_6_10.coverage", "2c0c2354-3cb7-4897-ba07-37efa0dae042");
        if (customContentHandler != null) {
            writeline("/home/ubuntu/results/coverage/Digester/Digester_6_10.coverage", "d33cc389-a772-4654-af65-714f33bad83f");
            customContentHandler.startElement(namespaceURI, localName, qName, list);
            writeline("/home/ubuntu/results/coverage/Digester/Digester_6_10.coverage", "5cfe2ac3-925a-4e97-81c7-e2a390cc7ae3");
            return;
        }
        writeline("/home/ubuntu/results/coverage/Digester/Digester_6_10.coverage", "0463907c-c75b-4558-b478-2c2950e568b3");
        if (saxLog.isDebugEnabled()) {
            writeline("/home/ubuntu/results/coverage/Digester/Digester_6_10.coverage", "e30d1d16-be8e-4efb-81b1-e4eadf32f0d3");
            saxLog.debug("startElement(" + namespaceURI + "," + localName + "," + qName + ")");
        }
        writeline("/home/ubuntu/results/coverage/Digester/Digester_6_10.coverage", "bd90b057-75f7-48f2-9406-d6f3045a029f");
        bodyTexts.push(bodyText);
        writeline("/home/ubuntu/results/coverage/Digester/Digester_6_10.coverage", "0157935c-71e7-4715-8ab9-8014d72ef60a");
        if (debug) {
            writeline("/home/ubuntu/results/coverage/Digester/Digester_6_10.coverage", "75c90054-094c-4d15-a2a5-b1ef24d23688");
            log.debug("  Pushing body text '" + bodyText.toString() + "'");
        }
        writeline("/home/ubuntu/results/coverage/Digester/Digester_6_10.coverage", "de468ec6-98c1-4527-9248-b1ee7fc2649d");
        bodyText = new StringBuilder();
        writeline("/home/ubuntu/results/coverage/Digester/Digester_6_10.coverage", "b4658789-c9e0-47d9-9bfb-ddc9eac11748");
        String name = localName;
        writeline("/home/ubuntu/results/coverage/Digester/Digester_6_10.coverage", "02e9bd0a-061e-4fbe-9b07-2063a0bc57c6");
        if ((name == null) || (name.length() < 1)) {
            writeline("/home/ubuntu/results/coverage/Digester/Digester_6_10.coverage", "e53ab0df-44d6-4fc9-9893-b7be622459a9");
            name = qName;
        }
        writeline("/home/ubuntu/results/coverage/Digester/Digester_6_10.coverage", "ec53baf9-0b4b-4333-bc7e-d93516fe59f3");
        StringBuilder sb = new StringBuilder(match);
        writeline("/home/ubuntu/results/coverage/Digester/Digester_6_10.coverage", "81ee01d7-52b6-4808-b58f-cb67bd2cf8e6");
        if (match.length() > 0) {
            writeline("/home/ubuntu/results/coverage/Digester/Digester_6_10.coverage", "02abd0a7-9a73-4df7-8a36-50da8ec36557");
            sb.append('/');
        }
        writeline("/home/ubuntu/results/coverage/Digester/Digester_6_10.coverage", "05cec990-520e-4d50-a4fe-59dc3fddda05");
        sb.append(name);
        writeline("/home/ubuntu/results/coverage/Digester/Digester_6_10.coverage", "a548fd69-b17a-4a41-9394-1b83de4526c4");
        match = sb.toString();
        writeline("/home/ubuntu/results/coverage/Digester/Digester_6_10.coverage", "978c6cd7-3872-4ae8-a86e-26ebe9df99fe");
        if (debug) {
            writeline("/home/ubuntu/results/coverage/Digester/Digester_6_10.coverage", "266bebc7-8e65-436b-828a-f95132043440");
            log.debug("  New match='" + match + "'");
        }
        writeline("/home/ubuntu/results/coverage/Digester/Digester_6_10.coverage", "19ab4fb2-0563-48cc-8cdd-a4899a23ee89");
        List<Rule> rules = getRules().match(namespaceURI, match, localName, list);
        writeline("/home/ubuntu/results/coverage/Digester/Digester_6_10.coverage", "89ecb105-cbff-40b5-97b6-c094dd2dd315");
        matches.push(rules);
        writeline("/home/ubuntu/results/coverage/Digester/Digester_6_10.coverage", "3177455e-fb6b-4e8a-8d40-1683d9569f72");
        if ((rules != null) && (rules.size() > 0)) {
            writeline("/home/ubuntu/results/coverage/Digester/Digester_6_10.coverage", "951da795-f838-46dc-8d32-94a0d20cabc9");
            Substitutor substitutor = getSubstitutor();
            writeline("/home/ubuntu/results/coverage/Digester/Digester_6_10.coverage", "3d7fc902-67f6-4c90-b0fd-0198c295ccae");
            if (substitutor != null) {
                writeline("/home/ubuntu/results/coverage/Digester/Digester_6_10.coverage", "ab5d7c7d-9257-468f-81cf-ca6a689d3622");
                list = substitutor.substitute(list);
            }
            writeline("/home/ubuntu/results/coverage/Digester/Digester_6_10.coverage", "5ebf5370-f2c8-4cd0-9ae9-bc2da951150c");
            for (int i = 0; i < rules.size(); i++) {
                writeline("/home/ubuntu/results/coverage/Digester/Digester_6_10.coverage", "f2d741c5-4f58-4413-92c5-10b4d97a8ada");
                try {
                    writeline("/home/ubuntu/results/coverage/Digester/Digester_6_10.coverage", "d710034a-1101-4e48-b3f0-827784fc3bc2");
                    Rule rule = rules.get(i);
                    writeline("/home/ubuntu/results/coverage/Digester/Digester_6_10.coverage", "3e6c9e29-2c49-4e35-90cd-a8bf41cc7f6e");
                    if (debug) {
                        writeline("/home/ubuntu/results/coverage/Digester/Digester_6_10.coverage", "b9a0f1e3-fbdf-4391-a253-03b505ffc04e");
                        log.debug("  Fire begin() for " + rule);
                    }
                    writeline("/home/ubuntu/results/coverage/Digester/Digester_6_10.coverage", "14f4a4f8-b505-4a17-8d9d-2db5470b892e");
                    rule.begin(namespaceURI, name, list);
                } catch (Exception e) {
                    writeline("/home/ubuntu/results/coverage/Digester/Digester_6_10.coverage", "6352f55d-223e-4cff-8455-e92fbd86e3c9");
                    log.error("Begin event threw exception", e);
                } catch (Error e) {
                    writeline("/home/ubuntu/results/coverage/Digester/Digester_6_10.coverage", "401878ec-3ac3-4c2d-9db5-ad7ad2942c54");
                    log.error("Begin event threw error", e);
                }
            }
        } else {
            writeline("/home/ubuntu/results/coverage/Digester/Digester_6_10.coverage", "6e0f9f24-4e37-4162-a066-c3acf9773b0c");
            if (debug) {
                writeline("/home/ubuntu/results/coverage/Digester/Digester_6_10.coverage", "a316ed50-0e73-469e-a52e-bbb347fae7dd");
                log.debug("  No rules found matching '" + match + "'.");
            }
        }
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public void startPrefixMapping(String prefix, String namespaceURI) throws SAXException {
        writeline("/home/ubuntu/results/coverage/Digester/Digester_6_10.coverage", "f611007f-84ad-42ff-926b-1c7d0b198899");
        if (saxLog.isDebugEnabled()) {
            writeline("/home/ubuntu/results/coverage/Digester/Digester_6_10.coverage", "e42b6a23-066e-4aeb-9a01-6bfbbb6a57af");
            saxLog.debug("startPrefixMapping(" + prefix + "," + namespaceURI + ")");
        }
        writeline("/home/ubuntu/results/coverage/Digester/Digester_6_10.coverage", "497d0203-b10a-4f89-97d4-761157178df8");
        Stack<String> stack = namespaces.get(prefix);
        writeline("/home/ubuntu/results/coverage/Digester/Digester_6_10.coverage", "06024349-e84f-4af6-ace3-66d066ec15f0");
        if (stack == null) {
            writeline("/home/ubuntu/results/coverage/Digester/Digester_6_10.coverage", "e857ae84-9353-4167-96ec-ca27ca1db4ab");
            stack = new Stack<String>();
            writeline("/home/ubuntu/results/coverage/Digester/Digester_6_10.coverage", "b0c067a9-6253-4906-94c5-4074859883e8");
            namespaces.put(prefix, stack);
        }
        writeline("/home/ubuntu/results/coverage/Digester/Digester_6_10.coverage", "e8c504e4-a3fd-40ab-a0bd-600fcb44ba6a");
        stack.push(namespaceURI);
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public void notationDecl(String name, String publicId, String systemId) {
        writeline("/home/ubuntu/results/coverage/Digester/Digester_6_10.coverage", "dfdffbd1-9498-4a66-ab39-c4339da2ee3d");
        if (saxLog.isDebugEnabled()) {
            writeline("/home/ubuntu/results/coverage/Digester/Digester_6_10.coverage", "b4183d3e-7e73-4b83-a902-9cf8d5e7ba9e");
            saxLog.debug("notationDecl(" + name + "," + publicId + "," + systemId + ")");
        }
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public void unparsedEntityDecl(String name, String publicId, String systemId, String notation) {
        writeline("/home/ubuntu/results/coverage/Digester/Digester_6_10.coverage", "a877b422-efdd-4a64-9f75-47f881136939");
        if (saxLog.isDebugEnabled()) {
            writeline("/home/ubuntu/results/coverage/Digester/Digester_6_10.coverage", "433c6ab4-0193-41b7-ae7b-176c81b4d69d");
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
        writeline("/home/ubuntu/results/coverage/Digester/Digester_6_10.coverage", "1c833723-46a2-4229-b656-cda4473e2e77");
        this.entityResolver = entityResolver;
    }

    /**
     * Return the Entity Resolver used by the SAX parser.
     *
     * @return the Entity Resolver used by the SAX parser.
     */
    public EntityResolver getEntityResolver() {
        writeline("/home/ubuntu/results/coverage/Digester/Digester_6_10.coverage", "fe012148-a489-4123-bfaa-c03c52bd280d");
        return entityResolver;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public InputSource resolveEntity(String publicId, String systemId) throws SAXException {
        writeline("/home/ubuntu/results/coverage/Digester/Digester_6_10.coverage", "56081cec-d5da-4943-b71d-f12702005694");
        if (saxLog.isDebugEnabled()) {
            writeline("/home/ubuntu/results/coverage/Digester/Digester_6_10.coverage", "8ed84e89-5638-4501-a3a0-9db2d02e488d");
            saxLog.debug("resolveEntity('" + publicId + "', '" + systemId + "')");
        }
        writeline("/home/ubuntu/results/coverage/Digester/Digester_6_10.coverage", "55ec10de-1a9e-467f-9849-831e5e188995");
        if (publicId != null) {
            writeline("/home/ubuntu/results/coverage/Digester/Digester_6_10.coverage", "dd7329c1-3178-4388-ad43-5053300d0a9c");
            this.publicId = publicId;
        }
        writeline("/home/ubuntu/results/coverage/Digester/Digester_6_10.coverage", "513084ae-74bc-45ee-9a96-e37293985371");
        URL entityURL = null;
        writeline("/home/ubuntu/results/coverage/Digester/Digester_6_10.coverage", "61594897-8a36-4b4d-ab31-b3f9dbeeeffc");
        if (publicId != null) {
            writeline("/home/ubuntu/results/coverage/Digester/Digester_6_10.coverage", "091a815e-a1c6-47c4-ae68-4e8bc1e5e180");
            entityURL = entityValidator.get(publicId);
        }
        writeline("/home/ubuntu/results/coverage/Digester/Digester_6_10.coverage", "f9ecfa39-e5e6-4e39-b920-0d8660299d1b");
        if (entityURL == null && systemId != null) {
            writeline("/home/ubuntu/results/coverage/Digester/Digester_6_10.coverage", "0289d026-9105-414b-a469-98fafaf00217");
            entityURL = entityValidator.get(systemId);
        }
        writeline("/home/ubuntu/results/coverage/Digester/Digester_6_10.coverage", "4557ba3a-6c07-4338-81f1-2e9a4057fda4");
        if (entityURL == null) {
            writeline("/home/ubuntu/results/coverage/Digester/Digester_6_10.coverage", "553110e8-dac4-40d8-a720-2b7df6985371");
            if (systemId == null) {
                writeline("/home/ubuntu/results/coverage/Digester/Digester_6_10.coverage", "23e8c84a-f877-4a79-a16a-f91a54cd7cdd");
                if (log.isDebugEnabled()) {
                    writeline("/home/ubuntu/results/coverage/Digester/Digester_6_10.coverage", "a50f45b3-1a86-463c-9990-d4ae18ceb57d");
                    log.debug(" Cannot resolve null entity, returning null InputSource");
                }
                writeline("/home/ubuntu/results/coverage/Digester/Digester_6_10.coverage", "f1d0ee77-1eb8-42eb-a889-9cd71b702210");
                return (null);
            }
            writeline("/home/ubuntu/results/coverage/Digester/Digester_6_10.coverage", "9eb3f9c5-57c7-4673-8c82-5312d4395b98");
            if (log.isDebugEnabled()) {
                writeline("/home/ubuntu/results/coverage/Digester/Digester_6_10.coverage", "49a187ab-2b43-477d-beb3-8542523c4693");
                log.debug(" Trying to resolve using system ID '" + systemId + "'");
            }
            writeline("/home/ubuntu/results/coverage/Digester/Digester_6_10.coverage", "9659215c-14d3-446e-b941-df9bb8a3a359");
            try {
                writeline("/home/ubuntu/results/coverage/Digester/Digester_6_10.coverage", "1202e0fa-40c4-4cbe-a1a6-208f5018f647");
                entityURL = new URL(systemId);
            } catch (MalformedURLException e) {
            }
        }
        writeline("/home/ubuntu/results/coverage/Digester/Digester_6_10.coverage", "2e4a2791-9f68-4bba-952c-629d4a98f152");
        if (log.isDebugEnabled()) {
            writeline("/home/ubuntu/results/coverage/Digester/Digester_6_10.coverage", "a349a816-7df0-4e0c-be12-02dca4b2f371");
            log.debug(" Resolving to alternate DTD '" + entityURL + "'");
        }
        writeline("/home/ubuntu/results/coverage/Digester/Digester_6_10.coverage", "d7ad689d-c6cd-48db-a5f3-123fe8b3702e");
        try {
            writeline("/home/ubuntu/results/coverage/Digester/Digester_6_10.coverage", "90af414b-fcf4-4ef6-bb3e-c55947787ffd");
            return createInputSourceFromURL(entityURL);
        } catch (Exception e) {
        }
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public void error(SAXParseException exception) throws SAXException {
        writeline("/home/ubuntu/results/coverage/Digester/Digester_6_10.coverage", "1aaf1177-4d0c-47b9-86ca-c4efe1f21be5");
        log.error("Parse Error at line " + exception.getLineNumber() + " column " + exception.getColumnNumber() + ": " + exception.getMessage(), exception);
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public void fatalError(SAXParseException exception) throws SAXException {
        writeline("/home/ubuntu/results/coverage/Digester/Digester_6_10.coverage", "6f2a9116-4517-4ebc-839f-8a7910425d2d");
        log.error("Parse Fatal Error at line " + exception.getLineNumber() + " column " + exception.getColumnNumber() + ": " + exception.getMessage(), exception);
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public void warning(SAXParseException exception) throws SAXException {
        writeline("/home/ubuntu/results/coverage/Digester/Digester_6_10.coverage", "aae9cc47-fb2c-4ced-a7b9-8219703d0134");
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
        writeline("/home/ubuntu/results/coverage/Digester/Digester_6_10.coverage", "c1095e3e-be68-402c-86fc-e10b8f1a2787");
        if (file == null) {
        }
        writeline("/home/ubuntu/results/coverage/Digester/Digester_6_10.coverage", "182f96c9-ddda-4c7c-baac-f0566241f1d6");
        InputSource input = new InputSource(new FileInputStream(file));
        writeline("/home/ubuntu/results/coverage/Digester/Digester_6_10.coverage", "fc1f5aae-4d57-46ba-99b6-1d3404eee9e7");
        input.setSystemId(file.toURI().toURL().toString());
        writeline("/home/ubuntu/results/coverage/Digester/Digester_6_10.coverage", "41e79d87-1198-4868-8c23-50e0765642c3");
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
        writeline("/home/ubuntu/results/coverage/Digester/Digester_6_10.coverage", "8c399321-a76f-4f31-9141-3c528c47921b");
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
        writeline("/home/ubuntu/results/coverage/Digester/Digester_6_10.coverage", "1d54a7f1-14cf-4617-852a-5d7b3d68aa1b");
        if (input == null) {
        }
        writeline("/home/ubuntu/results/coverage/Digester/Digester_6_10.coverage", "267fb62b-3dbb-463e-9093-89300dce0364");
        configure();
        writeline("/home/ubuntu/results/coverage/Digester/Digester_6_10.coverage", "cd326980-94db-435a-9a91-c9d5317876df");
        String systemId = input.getSystemId();
        writeline("/home/ubuntu/results/coverage/Digester/Digester_6_10.coverage", "abf6fdbd-d68c-4554-950d-2024125a5c9c");
        if (systemId == null) {
            writeline("/home/ubuntu/results/coverage/Digester/Digester_6_10.coverage", "5805858f-f31f-478d-889d-ac2c9e14c01a");
            systemId = "(already loaded from stream)";
        }
        writeline("/home/ubuntu/results/coverage/Digester/Digester_6_10.coverage", "5ed89f19-8b31-49df-bd58-f5e59e9f69db");
        try {
            writeline("/home/ubuntu/results/coverage/Digester/Digester_6_10.coverage", "ae6bc2a3-a1e4-43cc-abd0-8138c7be1461");
            getXMLReader().parse(input);
        } catch (IOException e) {
            writeline("/home/ubuntu/results/coverage/Digester/Digester_6_10.coverage", "e2ef4278-f52f-4b20-a46b-654dbab6018a");
            log.error(format("An error occurred while reading stream from '%s', see nested exceptions", systemId), e);
        }
        writeline("/home/ubuntu/results/coverage/Digester/Digester_6_10.coverage", "6f139a22-45e8-463f-a432-40447555d832");
        cleanup();
        writeline("/home/ubuntu/results/coverage/Digester/Digester_6_10.coverage", "30a14feb-eee5-42c3-95ea-fc0255a004cd");
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
        writeline("/home/ubuntu/results/coverage/Digester/Digester_6_10.coverage", "87e55276-2a68-4ddb-8417-79dae8aa507c");
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
        writeline("/home/ubuntu/results/coverage/Digester/Digester_6_10.coverage", "68388d3e-1a68-493d-9695-3c912988dcc2");
        if (input == null) {
        }
        writeline("/home/ubuntu/results/coverage/Digester/Digester_6_10.coverage", "72b742f2-79ce-4a78-8545-069bd47e4600");
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
        writeline("/home/ubuntu/results/coverage/Digester/Digester_6_10.coverage", "2d4e8480-8bce-4bd1-bc47-ce81df1cfcba");
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
        writeline("/home/ubuntu/results/coverage/Digester/Digester_6_10.coverage", "e1a92f33-f43f-4a4b-aa2c-25f6c51c0990");
        if (reader == null) {
        }
        writeline("/home/ubuntu/results/coverage/Digester/Digester_6_10.coverage", "3bb57318-f704-4783-9b3d-e355c5ecc820");
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
        writeline("/home/ubuntu/results/coverage/Digester/Digester_6_10.coverage", "cda01ce4-5139-435c-b851-89058253fc90");
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
        writeline("/home/ubuntu/results/coverage/Digester/Digester_6_10.coverage", "c80c5da4-df25-45c5-8c71-a3d596206615");
        if (uri == null) {
        }
        writeline("/home/ubuntu/results/coverage/Digester/Digester_6_10.coverage", "7d2447e6-a51b-47a9-8f33-4a9b830271c8");
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
        writeline("/home/ubuntu/results/coverage/Digester/Digester_6_10.coverage", "c61e35f3-7708-47f0-962b-ee6c245705be");
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
        writeline("/home/ubuntu/results/coverage/Digester/Digester_6_10.coverage", "d45d77f1-fdf6-469f-b0fd-68dfaed62338");
        if (url == null) {
        }
        writeline("/home/ubuntu/results/coverage/Digester/Digester_6_10.coverage", "5e065234-8094-427a-8691-fb8c053dd3e3");
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
        writeline("/home/ubuntu/results/coverage/Digester/Digester_6_10.coverage", "9cef05c0-b072-4781-b96c-02dae983c92a");
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
        writeline("/home/ubuntu/results/coverage/Digester/Digester_6_10.coverage", "a7b557b9-aed2-4d31-bd80-e04c5efd95ce");
        if (executorService == null) {
        }
        writeline("/home/ubuntu/results/coverage/Digester/Digester_6_10.coverage", "9a766a45-975e-42e0-b53f-ec2c3cd9f11b");
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
        writeline("/home/ubuntu/results/coverage/Digester/Digester_6_10.coverage", "d3c43751-f657-4f6f-b742-d149f8d716e4");
        if (log.isDebugEnabled()) {
            writeline("/home/ubuntu/results/coverage/Digester/Digester_6_10.coverage", "b92bcd18-85ae-4ce7-a968-1ca169c0f862");
            log.debug("register('" + publicId + "', '" + entityURL + "'");
        }
        writeline("/home/ubuntu/results/coverage/Digester/Digester_6_10.coverage", "813b7f25-ab7a-4ec8-ab0e-740cb4c0cced");
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
        writeline("/home/ubuntu/results/coverage/Digester/Digester_6_10.coverage", "bbde3464-3cc7-4cf3-b8a4-fdf1e50f21d3");
        if (log.isDebugEnabled()) {
            writeline("/home/ubuntu/results/coverage/Digester/Digester_6_10.coverage", "eaf3ae0e-f459-4bf5-952a-cf775dd882c0");
            log.debug("register('" + publicId + "', '" + entityURL + "'");
        }
        writeline("/home/ubuntu/results/coverage/Digester/Digester_6_10.coverage", "4fcc8739-575e-47e0-a9e4-b462793c34b4");
        try {
            writeline("/home/ubuntu/results/coverage/Digester/Digester_6_10.coverage", "7e8bf7e8-dcc7-4ccc-a8ed-3eeb8fbd4264");
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
        writeline("/home/ubuntu/results/coverage/Digester/Digester_6_10.coverage", "f7900dcc-327c-4807-82c9-e455557ca087");
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
        writeline("/home/ubuntu/results/coverage/Digester/Digester_6_10.coverage", "b2d2574c-fb9d-4968-a485-bfdbe46829e6");
        URLConnection connection = url.openConnection();
        writeline("/home/ubuntu/results/coverage/Digester/Digester_6_10.coverage", "1bb3dc4d-3677-4fbd-88d8-3a67b6320e81");
        connection.setUseCaches(false);
        writeline("/home/ubuntu/results/coverage/Digester/Digester_6_10.coverage", "014f0c72-ef26-409b-b037-b31e95799390");
        InputStream stream = connection.getInputStream();
        writeline("/home/ubuntu/results/coverage/Digester/Digester_6_10.coverage", "075acbf9-b2e9-4ece-b1d6-dfb5bc429f98");
        InputSource source = new InputSource(stream);
        writeline("/home/ubuntu/results/coverage/Digester/Digester_6_10.coverage", "e7f1709e-a59e-4a5a-9d65-a224f914c841");
        source.setSystemId(url.toExternalForm());
        writeline("/home/ubuntu/results/coverage/Digester/Digester_6_10.coverage", "84babbbd-3c0d-44af-b450-b0546bf35feb");
        inputSources.add(source);
        writeline("/home/ubuntu/results/coverage/Digester/Digester_6_10.coverage", "88519df4-f17b-415c-8ebb-e1285a39a512");
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
        writeline("/home/ubuntu/results/coverage/Digester/Digester_6_10.coverage", "f6a0e5e7-6c6d-4d49-a7c9-8dc154f991bc");
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
        writeline("/home/ubuntu/results/coverage/Digester/Digester_6_10.coverage", "bc49fb62-6e04-468e-8265-a076f119e034");
        rule.setDigester(this);
        writeline("/home/ubuntu/results/coverage/Digester/Digester_6_10.coverage", "8aa04bff-b67e-4d01-855f-46fb35fe7a6e");
        getRules().add(pattern, rule);
    }

    /**
     * Register a set of Rule instances defined in a RuleSet.
     *
     * @param ruleSet The RuleSet instance to configure from
     */
    public void addRuleSet(RuleSet ruleSet) {
        writeline("/home/ubuntu/results/coverage/Digester/Digester_6_10.coverage", "e56a7bc2-794e-458e-9e0d-c96673c4f4fa");
        String oldNamespaceURI = getRuleNamespaceURI();
        writeline("/home/ubuntu/results/coverage/Digester/Digester_6_10.coverage", "67afd9cc-0fd4-4f94-8b0b-488286373441");
        String newNamespaceURI = ruleSet.getNamespaceURI();
        writeline("/home/ubuntu/results/coverage/Digester/Digester_6_10.coverage", "2995f220-febf-4dd5-9268-a94c94355507");
        if (log.isDebugEnabled()) {
            writeline("/home/ubuntu/results/coverage/Digester/Digester_6_10.coverage", "51b9baa9-1f44-4225-a5fa-b1f9e31a649f");
            if (newNamespaceURI == null) {
                writeline("/home/ubuntu/results/coverage/Digester/Digester_6_10.coverage", "b35ab9a1-e352-43be-8eea-2d777cf96fa7");
                log.debug("addRuleSet() with no namespace URI");
            } else {
                writeline("/home/ubuntu/results/coverage/Digester/Digester_6_10.coverage", "14cc4c3a-a62d-4145-892b-fc7ceef1d11b");
                log.debug("addRuleSet() with namespace URI " + newNamespaceURI);
            }
        }
        writeline("/home/ubuntu/results/coverage/Digester/Digester_6_10.coverage", "32243927-91d6-4c47-9bcf-bd17dd7c997d");
        setRuleNamespaceURI(newNamespaceURI);
        writeline("/home/ubuntu/results/coverage/Digester/Digester_6_10.coverage", "ea452f47-4de2-4fc5-a951-2d811fd6bf26");
        ruleSet.addRuleInstances(this);
        writeline("/home/ubuntu/results/coverage/Digester/Digester_6_10.coverage", "bc890287-dd4e-4877-b064-05314366b284");
        setRuleNamespaceURI(oldNamespaceURI);
    }

    /**
     * Add a "bean property setter" rule for the specified parameters.
     *
     * @param pattern Element matching pattern
     * @see BeanPropertySetterRule
     */
    public void addBeanPropertySetter(String pattern) {
        writeline("/home/ubuntu/results/coverage/Digester/Digester_6_10.coverage", "07ef3480-0c42-47c0-b76c-3b50f0d1dd0a");
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
        writeline("/home/ubuntu/results/coverage/Digester/Digester_6_10.coverage", "e6c67cee-36b5-4d12-8cea-3b744551eaf6");
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
        writeline("/home/ubuntu/results/coverage/Digester/Digester_6_10.coverage", "47895c85-fd1c-4e65-84b1-9b6f93087e57");
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
        writeline("/home/ubuntu/results/coverage/Digester/Digester_6_10.coverage", "9625dbeb-e4f1-4612-8b42-0eff98432bf1");
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
        writeline("/home/ubuntu/results/coverage/Digester/Digester_6_10.coverage", "ccda21eb-960e-42e7-b2ca-afac749cb885");
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
        writeline("/home/ubuntu/results/coverage/Digester/Digester_6_10.coverage", "02c98b37-07ca-4a86-a177-a56ae16db7d8");
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
        writeline("/home/ubuntu/results/coverage/Digester/Digester_6_10.coverage", "8edeefd5-5371-4d37-964c-c5cf80c7b157");
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
        writeline("/home/ubuntu/results/coverage/Digester/Digester_6_10.coverage", "670e26ca-ec4f-4ed5-8fad-041a925e34f1");
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
        writeline("/home/ubuntu/results/coverage/Digester/Digester_6_10.coverage", "d7aedf46-0561-40c0-becc-02a189e51214");
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
        writeline("/home/ubuntu/results/coverage/Digester/Digester_6_10.coverage", "38ffae80-9564-4f9f-8960-936231fcf9e0");
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
        writeline("/home/ubuntu/results/coverage/Digester/Digester_6_10.coverage", "f2999ce8-713f-476c-971a-16e79f0af92b");
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
        writeline("/home/ubuntu/results/coverage/Digester/Digester_6_10.coverage", "e30ad474-438a-4d9c-8c10-eb2269bd5745");
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
        writeline("/home/ubuntu/results/coverage/Digester/Digester_6_10.coverage", "6980cad5-b472-4bc1-aa4b-b74868d74931");
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
        writeline("/home/ubuntu/results/coverage/Digester/Digester_6_10.coverage", "7a887ffe-50ea-4c75-8488-8021e89e6d5e");
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
        writeline("/home/ubuntu/results/coverage/Digester/Digester_6_10.coverage", "515f0545-fffa-47b9-babf-eac4a3d8a0c8");
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
        writeline("/home/ubuntu/results/coverage/Digester/Digester_6_10.coverage", "2015599d-2ebe-41e0-8e57-4051b3dafc51");
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
        writeline("/home/ubuntu/results/coverage/Digester/Digester_6_10.coverage", "e4384bdf-9bfd-4929-9a2f-199f31f3a8ef");
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
        writeline("/home/ubuntu/results/coverage/Digester/Digester_6_10.coverage", "59728b6a-62eb-44b6-ba5d-cb361369a2ba");
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
        writeline("/home/ubuntu/results/coverage/Digester/Digester_6_10.coverage", "3fa034e6-4cd3-4d37-9666-f41448898e30");
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
        writeline("/home/ubuntu/results/coverage/Digester/Digester_6_10.coverage", "1a96a5de-f39e-4717-8dcf-278fb9602045");
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
        writeline("/home/ubuntu/results/coverage/Digester/Digester_6_10.coverage", "5e77398f-3d13-440f-ab30-f2d5dad936e6");
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
        writeline("/home/ubuntu/results/coverage/Digester/Digester_6_10.coverage", "8b1deaf6-2172-485f-81bf-9f790c354056");
        creationFactory.setDigester(this);
        writeline("/home/ubuntu/results/coverage/Digester/Digester_6_10.coverage", "b32962aa-d247-4757-96cf-e804f0eafd57");
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
        writeline("/home/ubuntu/results/coverage/Digester/Digester_6_10.coverage", "88f5a0c1-615d-4123-98e8-d6e471472067");
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
        writeline("/home/ubuntu/results/coverage/Digester/Digester_6_10.coverage", "92e77458-e5ed-4668-bb27-dedf456e3075");
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
        writeline("/home/ubuntu/results/coverage/Digester/Digester_6_10.coverage", "a0b53605-c727-4f29-a2e5-fb868597d045");
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
        writeline("/home/ubuntu/results/coverage/Digester/Digester_6_10.coverage", "60ef2b83-8ba8-418d-9335-8302ff300df5");
        addRule(pattern, new ObjectCreateRule(attributeName, clazz));
    }

    /**
     * Adds an {@link SetNestedPropertiesRule}.
     *
     * @param pattern register the rule with this pattern
     * @since 1.6
     */
    public void addSetNestedProperties(String pattern) {
        writeline("/home/ubuntu/results/coverage/Digester/Digester_6_10.coverage", "e3241bf8-5fd5-4648-b239-635a16da67c7");
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
        writeline("/home/ubuntu/results/coverage/Digester/Digester_6_10.coverage", "83c83027-18d0-4669-9444-d0bfdd92acc4");
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
        writeline("/home/ubuntu/results/coverage/Digester/Digester_6_10.coverage", "eee57188-c6bf-4959-8b3a-6338c2b63bfa");
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
        writeline("/home/ubuntu/results/coverage/Digester/Digester_6_10.coverage", "4630c886-9a04-4f4e-a1ce-3b15e08e1bfa");
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
        writeline("/home/ubuntu/results/coverage/Digester/Digester_6_10.coverage", "315c45b8-b09c-4c7e-bff6-02a730e02bac");
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
        writeline("/home/ubuntu/results/coverage/Digester/Digester_6_10.coverage", "29a10ae5-d292-454b-ab27-b1aca7bd73be");
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
        writeline("/home/ubuntu/results/coverage/Digester/Digester_6_10.coverage", "2691df7e-1224-496c-8b61-144649fc55da");
        addRule(pattern, new SetRootRule(methodName, paramType));
    }

    /**
     * Add a "set properties" rule for the specified parameters.
     *
     * @param pattern Element matching pattern
     * @see SetPropertiesRule
     */
    public void addSetProperties(String pattern) {
        writeline("/home/ubuntu/results/coverage/Digester/Digester_6_10.coverage", "ed203359-e1aa-4023-b022-c6f316066f09");
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
        writeline("/home/ubuntu/results/coverage/Digester/Digester_6_10.coverage", "01485cc8-0909-4df6-9f97-9f70acc103cf");
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
        writeline("/home/ubuntu/results/coverage/Digester/Digester_6_10.coverage", "9027a1c7-bdca-4f8d-8c7b-85f23def6478");
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
        writeline("/home/ubuntu/results/coverage/Digester/Digester_6_10.coverage", "d0f2d15d-441e-448a-97ba-72afdde6564e");
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
        writeline("/home/ubuntu/results/coverage/Digester/Digester_6_10.coverage", "e9aa7dc1-6743-47a1-ba58-2211e12709a1");
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
        writeline("/home/ubuntu/results/coverage/Digester/Digester_6_10.coverage", "28291a5b-bd41-4c40-ae17-d29d10f6628d");
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
        writeline("/home/ubuntu/results/coverage/Digester/Digester_6_10.coverage", "73e57da7-58eb-43ec-b7bd-ae624e3c97d3");
        match = "";
        writeline("/home/ubuntu/results/coverage/Digester/Digester_6_10.coverage", "d8202ae5-1f72-4881-8f39-dd055a098cbb");
        bodyTexts.clear();
        writeline("/home/ubuntu/results/coverage/Digester/Digester_6_10.coverage", "ebdc2792-a31d-4ffb-ac49-32324191e109");
        params.clear();
        writeline("/home/ubuntu/results/coverage/Digester/Digester_6_10.coverage", "361a5f5b-eebd-430e-b51e-897dc63cc3d9");
        publicId = null;
        writeline("/home/ubuntu/results/coverage/Digester/Digester_6_10.coverage", "633cff27-9760-4777-b2f3-e3fabfdb96d0");
        stack.clear();
        writeline("/home/ubuntu/results/coverage/Digester/Digester_6_10.coverage", "3d7aa867-e506-4b23-9447-d3849dd469f9");
        stacksByName.clear();
        writeline("/home/ubuntu/results/coverage/Digester/Digester_6_10.coverage", "0adc0ba1-c237-4198-b623-cea020fb5415");
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
        writeline("/home/ubuntu/results/coverage/Digester/Digester_6_10.coverage", "722318b8-cba7-41c7-b22a-2ec14007766b");
        try {
            writeline("/home/ubuntu/results/coverage/Digester/Digester_6_10.coverage", "820f129b-5d27-4bda-91ca-cbcbc3d9216f");
            return this.<T>npeSafeCast(stack.peek());
        } catch (EmptyStackException e) {
            writeline("/home/ubuntu/results/coverage/Digester/Digester_6_10.coverage", "eb2a386d-b47b-494a-8aec-fd492d05b011");
            log.warn("Empty stack (returning null)");
            writeline("/home/ubuntu/results/coverage/Digester/Digester_6_10.coverage", "19040d07-f92f-4425-a621-95dcb596cc6c");
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
        writeline("/home/ubuntu/results/coverage/Digester/Digester_6_10.coverage", "790a4837-766f-4e3b-8568-001ff8af8eb8");
        int index = (stack.size() - 1) - n;
        writeline("/home/ubuntu/results/coverage/Digester/Digester_6_10.coverage", "047c93de-92e3-419d-8714-efee39691c43");
        if (index < 0) {
            writeline("/home/ubuntu/results/coverage/Digester/Digester_6_10.coverage", "2012c857-1f32-4f8b-acb8-1ff36c610f4f");
            log.warn("Empty stack (returning null)");
            writeline("/home/ubuntu/results/coverage/Digester/Digester_6_10.coverage", "43b0e661-0e4f-4360-a7d5-35552af50023");
            return (null);
        }
        writeline("/home/ubuntu/results/coverage/Digester/Digester_6_10.coverage", "dd0e4dff-e575-40c6-a648-6644761b5064");
        try {
            writeline("/home/ubuntu/results/coverage/Digester/Digester_6_10.coverage", "9e9874b9-92f4-4a6f-861b-3108d6262957");
            return this.<T>npeSafeCast(stack.get(index));
        } catch (EmptyStackException e) {
            writeline("/home/ubuntu/results/coverage/Digester/Digester_6_10.coverage", "688d8f41-99f4-4ec5-8e00-0c5707aa06db");
            log.warn("Empty stack (returning null)");
            writeline("/home/ubuntu/results/coverage/Digester/Digester_6_10.coverage", "e9bb2088-a0c4-4050-b6af-e650af7d53eb");
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
        writeline("/home/ubuntu/results/coverage/Digester/Digester_6_10.coverage", "e5b8e75c-1bc1-44fc-bdfe-153a58a32650");
        try {
            writeline("/home/ubuntu/results/coverage/Digester/Digester_6_10.coverage", "3c526905-b67a-4d33-b9ff-21422601373e");
            T popped = this.<T>npeSafeCast(stack.pop());
            writeline("/home/ubuntu/results/coverage/Digester/Digester_6_10.coverage", "45b155dc-e224-432c-8edc-d38bba6478af");
            if (stackAction != null) {
                writeline("/home/ubuntu/results/coverage/Digester/Digester_6_10.coverage", "9c637bb7-95f7-465f-b01d-204963571ba1");
                popped = stackAction.onPop(this, null, popped);
            }
            writeline("/home/ubuntu/results/coverage/Digester/Digester_6_10.coverage", "64cecb33-6915-425f-8b1c-2e0f370e1b45");
            return popped;
        } catch (EmptyStackException e) {
            writeline("/home/ubuntu/results/coverage/Digester/Digester_6_10.coverage", "f2780346-ef1b-4a18-ad22-4c3a0c282114");
            log.warn("Empty stack (returning null)");
            writeline("/home/ubuntu/results/coverage/Digester/Digester_6_10.coverage", "cd7f73a6-680a-4fa4-acba-468bfeadd64e");
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
        writeline("/home/ubuntu/results/coverage/Digester/Digester_6_10.coverage", "05a7d502-e095-42eb-b342-6712593c415c");
        if (stackAction != null) {
            writeline("/home/ubuntu/results/coverage/Digester/Digester_6_10.coverage", "129fc894-4e3a-4401-b99a-349671801682");
            object = stackAction.onPush(this, null, object);
        }
        writeline("/home/ubuntu/results/coverage/Digester/Digester_6_10.coverage", "639ce95d-99e5-444b-adc9-541451a4187e");
        if (stack.size() == 0) {
            writeline("/home/ubuntu/results/coverage/Digester/Digester_6_10.coverage", "9ea3ba04-7517-4e58-80b4-a5a28c6974fe");
            root = object;
        }
        writeline("/home/ubuntu/results/coverage/Digester/Digester_6_10.coverage", "b9e6e8a1-e15e-4f00-b9d1-2b8913f6bcff");
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
        writeline("/home/ubuntu/results/coverage/Digester/Digester_6_10.coverage", "d2b5bdea-8554-46a6-9aa6-175d29112616");
        if (stackAction != null) {
            writeline("/home/ubuntu/results/coverage/Digester/Digester_6_10.coverage", "003a1858-c56d-4c9d-9c0f-1f3b872186cd");
            value = stackAction.onPush(this, stackName, value);
        }
        writeline("/home/ubuntu/results/coverage/Digester/Digester_6_10.coverage", "657b26e4-8d60-4625-b181-81bccb1c995a");
        Stack<Object> namedStack = stacksByName.get(stackName);
        writeline("/home/ubuntu/results/coverage/Digester/Digester_6_10.coverage", "ef0a29ef-327f-44a8-8095-a8d331f0fb36");
        if (namedStack == null) {
            writeline("/home/ubuntu/results/coverage/Digester/Digester_6_10.coverage", "7d55eed5-1989-4de0-b48a-75fbd935e155");
            namedStack = new Stack<Object>();
            writeline("/home/ubuntu/results/coverage/Digester/Digester_6_10.coverage", "b5c598c0-8d84-4901-8bcc-9929a6a3c5ed");
            stacksByName.put(stackName, namedStack);
        }
        writeline("/home/ubuntu/results/coverage/Digester/Digester_6_10.coverage", "96611012-61a9-4bf2-915e-6f5cab6ca520");
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
        writeline("/home/ubuntu/results/coverage/Digester/Digester_6_10.coverage", "4a285b9f-5e87-44ac-974f-1aa52bbd3930");
        T result = null;
        writeline("/home/ubuntu/results/coverage/Digester/Digester_6_10.coverage", "c91121b5-5abc-4fae-90c6-1dae44f8d94b");
        Stack<Object> namedStack = stacksByName.get(stackName);
        writeline("/home/ubuntu/results/coverage/Digester/Digester_6_10.coverage", "c07bc072-4387-43c5-8a1c-426202f9bf4e");
        if (namedStack == null) {
            writeline("/home/ubuntu/results/coverage/Digester/Digester_6_10.coverage", "ead0b1d2-7db7-4e67-a596-a5d9ec5d1440");
            if (log.isDebugEnabled()) {
                writeline("/home/ubuntu/results/coverage/Digester/Digester_6_10.coverage", "bdf7a434-5f87-4a4d-b7ea-2004301e3582");
                log.debug("Stack '" + stackName + "' is empty");
            }
        }
        writeline("/home/ubuntu/results/coverage/Digester/Digester_6_10.coverage", "e32e70b0-420c-4d84-901a-a75e8a39cda6");
        result = this.<T>npeSafeCast(namedStack.pop());
        writeline("/home/ubuntu/results/coverage/Digester/Digester_6_10.coverage", "8e55c891-0589-427b-a076-927a03933f34");
        if (stackAction != null) {
            writeline("/home/ubuntu/results/coverage/Digester/Digester_6_10.coverage", "9208214f-5739-46bf-80d6-e11bc26be2b3");
            result = stackAction.onPop(this, stackName, result);
        }
        writeline("/home/ubuntu/results/coverage/Digester/Digester_6_10.coverage", "a97d61cd-668b-4e21-bcc4-90a59d2926ea");
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
        writeline("/home/ubuntu/results/coverage/Digester/Digester_6_10.coverage", "a5bf12f3-b0ec-40c4-8257-8812518df6c7");
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
        writeline("/home/ubuntu/results/coverage/Digester/Digester_6_10.coverage", "79f652c8-f97b-475d-98b8-0644b93724e7");
        T result = null;
        writeline("/home/ubuntu/results/coverage/Digester/Digester_6_10.coverage", "9d760e91-9b78-4bce-822a-43afe1739f36");
        Stack<Object> namedStack = stacksByName.get(stackName);
        writeline("/home/ubuntu/results/coverage/Digester/Digester_6_10.coverage", "167807b2-b6cc-47d7-a02d-e5cab19a06ab");
        if (namedStack == null) {
            writeline("/home/ubuntu/results/coverage/Digester/Digester_6_10.coverage", "4a2b7b6e-b027-4255-9684-9c478897451c");
            if (log.isDebugEnabled()) {
                writeline("/home/ubuntu/results/coverage/Digester/Digester_6_10.coverage", "8a69904b-099e-453a-a88d-c6bf58e6732b");
                log.debug("Stack '" + stackName + "' is empty");
            }
        }
        writeline("/home/ubuntu/results/coverage/Digester/Digester_6_10.coverage", "f30ce1f4-dd61-406f-9bc1-10381da140f0");
        int index = (namedStack.size() - 1) - n;
        writeline("/home/ubuntu/results/coverage/Digester/Digester_6_10.coverage", "88dec83b-38f3-4c13-87d9-f49bd583ad4e");
        if (index < 0) {
        }
        writeline("/home/ubuntu/results/coverage/Digester/Digester_6_10.coverage", "194cace9-d13e-4d82-91ea-3ff6d812dfac");
        result = this.<T>npeSafeCast(namedStack.get(index));
        writeline("/home/ubuntu/results/coverage/Digester/Digester_6_10.coverage", "430ebd60-9ed3-49fb-b60e-5306f9a21bf4");
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
        writeline("/home/ubuntu/results/coverage/Digester/Digester_6_10.coverage", "d26053a7-fe59-4ba5-8aca-11b70c43058c");
        boolean result = true;
        writeline("/home/ubuntu/results/coverage/Digester/Digester_6_10.coverage", "cea03804-f4f7-407a-81d9-2a2c4935da26");
        Stack<Object> namedStack = stacksByName.get(stackName);
        writeline("/home/ubuntu/results/coverage/Digester/Digester_6_10.coverage", "f14cbba8-7c3a-49c3-9cd0-19dcd2c686d1");
        if (namedStack != null) {
            writeline("/home/ubuntu/results/coverage/Digester/Digester_6_10.coverage", "cb21f895-48b2-4508-b07e-f0f5eec5d8c8");
            result = namedStack.isEmpty();
        }
        writeline("/home/ubuntu/results/coverage/Digester/Digester_6_10.coverage", "53a5aab2-d903-4c00-a03f-42d8fe2fd3e4");
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
        writeline("/home/ubuntu/results/coverage/Digester/Digester_6_10.coverage", "4774eb54-2e02-449e-8156-3024e540157e");
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
        writeline("/home/ubuntu/results/coverage/Digester/Digester_6_10.coverage", "25a29f6f-070b-4d52-8acd-0a3481e4fd42");
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
        writeline("/home/ubuntu/results/coverage/Digester/Digester_6_10.coverage", "36bea991-f1e9-4548-aab8-0bc99ca243c7");
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
        writeline("/home/ubuntu/results/coverage/Digester/Digester_6_10.coverage", "10656738-4316-4e70-8dc5-cb33b2ba1be5");
        if (configured) {
            writeline("/home/ubuntu/results/coverage/Digester/Digester_6_10.coverage", "a9e7ba15-c512-4870-90e0-455a780de068");
            return;
        }
        writeline("/home/ubuntu/results/coverage/Digester/Digester_6_10.coverage", "18bd1e6c-191d-4557-9eea-0536396c7360");
        initialize();
        writeline("/home/ubuntu/results/coverage/Digester/Digester_6_10.coverage", "830664a3-84c0-4319-ada1-a5685def006d");
        configured = true;
    }

    /**
     * Checks the Digester instance has been configured.
     *
     * @return true, if the Digester instance has been configured, false otherwise
     * @since 3.0
     */
    public boolean isConfigured() {
        writeline("/home/ubuntu/results/coverage/Digester/Digester_6_10.coverage", "b0123467-d398-49f2-9d2c-184327346c50");
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
        writeline("/home/ubuntu/results/coverage/Digester/Digester_6_10.coverage", "aca438e1-6dc8-4a5f-af71-8791f61055c3");
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
        writeline("/home/ubuntu/results/coverage/Digester/Digester_6_10.coverage", "88c2b51d-14eb-4ee4-b634-5c4a2b1ccc9a");
        try {
            writeline("/home/ubuntu/results/coverage/Digester/Digester_6_10.coverage", "010df307-dcd9-4e51-a92a-a70507f99d2b");
            return (params.peek());
        } catch (EmptyStackException e) {
            writeline("/home/ubuntu/results/coverage/Digester/Digester_6_10.coverage", "c25fb171-24c3-4ff9-9533-7e8af8c971a9");
            log.warn("Empty stack (returning null)");
            writeline("/home/ubuntu/results/coverage/Digester/Digester_6_10.coverage", "6e321a65-2cce-4e4e-bbbd-54eab8ade9ca");
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
        writeline("/home/ubuntu/results/coverage/Digester/Digester_6_10.coverage", "421e5978-ddc8-4b7c-9352-7c4fb74e8fb2");
        int index = (params.size() - 1) - n;
        writeline("/home/ubuntu/results/coverage/Digester/Digester_6_10.coverage", "5fecd903-9882-4008-9ef9-b2f0c119b784");
        if (index < 0) {
            writeline("/home/ubuntu/results/coverage/Digester/Digester_6_10.coverage", "73830e34-bda6-4610-8653-86447c7c1a6e");
            log.warn("Empty stack (returning null)");
            writeline("/home/ubuntu/results/coverage/Digester/Digester_6_10.coverage", "22bfe0c1-457e-48f7-91db-eb3b8eb4be26");
            return (null);
        }
        writeline("/home/ubuntu/results/coverage/Digester/Digester_6_10.coverage", "0f11a487-1d68-44bd-a6c2-1ad9682e063b");
        try {
            writeline("/home/ubuntu/results/coverage/Digester/Digester_6_10.coverage", "5e8eb05c-fa20-4eb6-ad3c-de79eca8bdf1");
            return (params.get(index));
        } catch (EmptyStackException e) {
            writeline("/home/ubuntu/results/coverage/Digester/Digester_6_10.coverage", "c7beb869-b1c3-4f4e-bfff-4a08b0293195");
            log.warn("Empty stack (returning null)");
            writeline("/home/ubuntu/results/coverage/Digester/Digester_6_10.coverage", "8afd5e3d-9a1b-4ad0-9749-22052f4a75fb");
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
        writeline("/home/ubuntu/results/coverage/Digester/Digester_6_10.coverage", "2cf92022-0c03-48db-80ef-957ba74d31dd");
        try {
            writeline("/home/ubuntu/results/coverage/Digester/Digester_6_10.coverage", "6d7f9748-4664-43a1-ac88-861d312d0573");
            if (log.isTraceEnabled()) {
                writeline("/home/ubuntu/results/coverage/Digester/Digester_6_10.coverage", "f37e835f-0f7e-47f4-812b-01228cb0a783");
                log.trace("Popping params");
            }
            writeline("/home/ubuntu/results/coverage/Digester/Digester_6_10.coverage", "98926f28-f838-4903-bc74-a472c569723d");
            return (params.pop());
        } catch (EmptyStackException e) {
            writeline("/home/ubuntu/results/coverage/Digester/Digester_6_10.coverage", "5746e3ff-761b-49f4-a8c2-40d45a16cd35");
            log.warn("Empty stack (returning null)");
            writeline("/home/ubuntu/results/coverage/Digester/Digester_6_10.coverage", "fcc101aa-0f46-4519-9657-2c9ef305f8c8");
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
        writeline("/home/ubuntu/results/coverage/Digester/Digester_6_10.coverage", "7829fff3-d168-4676-862c-6709f3bf192f");
        if (log.isTraceEnabled()) {
            writeline("/home/ubuntu/results/coverage/Digester/Digester_6_10.coverage", "31a7141e-a261-4e56-8614-6956859fc4f7");
            log.trace("Pushing params");
        }
        writeline("/home/ubuntu/results/coverage/Digester/Digester_6_10.coverage", "657878e9-53c1-450f-8eef-9f4e35abd97c");
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
        writeline("/home/ubuntu/results/coverage/Digester/Digester_6_10.coverage", "22b7c0ed-2681-49dd-a313-b3d95570597a");
        if ((e != null) && (e instanceof InvocationTargetException)) {
            writeline("/home/ubuntu/results/coverage/Digester/Digester_6_10.coverage", "87baf066-670d-4e46-ab84-a8653787cdb4");
            Throwable t = ((InvocationTargetException) e).getTargetException();
            writeline("/home/ubuntu/results/coverage/Digester/Digester_6_10.coverage", "9b0f4902-47a8-4093-9e6d-9b402da63a77");
            if ((t != null) && (t instanceof Exception)) {
                writeline("/home/ubuntu/results/coverage/Digester/Digester_6_10.coverage", "04c5c2d5-2678-4c9f-9406-aa8e56a0bc1d");
                e = (Exception) t;
            }
        }
        writeline("/home/ubuntu/results/coverage/Digester/Digester_6_10.coverage", "30aae31f-243b-43ae-bfda-fbda5a457786");
        if (locator != null) {
            writeline("/home/ubuntu/results/coverage/Digester/Digester_6_10.coverage", "9d1eb76d-715c-4865-bb78-b5dd56b7b25b");
            String error = "Error at line " + locator.getLineNumber() + " char " + locator.getColumnNumber() + ": " + message;
            writeline("/home/ubuntu/results/coverage/Digester/Digester_6_10.coverage", "3a4302f0-8d9d-48aa-b294-e76811c63430");
            if (e != null) {
                writeline("/home/ubuntu/results/coverage/Digester/Digester_6_10.coverage", "ac0619ee-7a45-4ea7-9df4-6a82b6364b85");
                return new SAXParseException(error, locator, e);
            }
            writeline("/home/ubuntu/results/coverage/Digester/Digester_6_10.coverage", "52bb09b7-ae32-495b-ae2b-4e8324124665");
            return new SAXParseException(error, locator);
        }
        writeline("/home/ubuntu/results/coverage/Digester/Digester_6_10.coverage", "d6b7c9fb-be48-4b69-b0bc-ce34474044cd");
        log.error("No Locator!");
        writeline("/home/ubuntu/results/coverage/Digester/Digester_6_10.coverage", "76ea6fd2-fdb6-4976-97a1-b4c23c54734c");
        if (e != null) {
            writeline("/home/ubuntu/results/coverage/Digester/Digester_6_10.coverage", "e8715821-64b9-4a9e-90ed-1267c0db604b");
            return new SAXException(message, e);
        }
        writeline("/home/ubuntu/results/coverage/Digester/Digester_6_10.coverage", "e47d9e2f-0759-4a2e-8ca0-eac48e8a0441");
        return new SAXException(message);
    }

    /**
     * Create a SAX exception which also understands about the location in the digester file where the exception occurs
     *
     * @param e the exception cause
     * @return the new SAX exception
     */
    public SAXException createSAXException(Exception e) {
        writeline("/home/ubuntu/results/coverage/Digester/Digester_6_10.coverage", "70fce2e0-2e99-4890-89b0-dee8061d8c3a");
        if (e instanceof InvocationTargetException) {
            writeline("/home/ubuntu/results/coverage/Digester/Digester_6_10.coverage", "4dd9e303-38d2-400e-b99b-52d78dbaaac5");
            Throwable t = ((InvocationTargetException) e).getTargetException();
            writeline("/home/ubuntu/results/coverage/Digester/Digester_6_10.coverage", "46cbba89-36fb-43e4-9429-62fb34e810e1");
            if ((t != null) && (t instanceof Exception)) {
                writeline("/home/ubuntu/results/coverage/Digester/Digester_6_10.coverage", "b3f5ce2d-ae43-462e-9672-ba657373511f");
                e = (Exception) t;
            }
        }
        writeline("/home/ubuntu/results/coverage/Digester/Digester_6_10.coverage", "4477e921-0a3d-4a02-a0e7-a1e53fd3dde1");
        return createSAXException(e.getMessage(), e);
    }

    /**
     * Create a SAX exception which also understands about the location in the digester file where the exception occurs
     *
     * @param message the custom SAX exception message
     * @return the new SAX exception
     */
    public SAXException createSAXException(String message) {
        writeline("/home/ubuntu/results/coverage/Digester/Digester_6_10.coverage", "9043b986-d902-4561-a63d-d8dcd950ade1");
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
        writeline("/home/ubuntu/results/coverage/Digester/Digester_6_10.coverage", "d64cca28-9ff4-400d-8b49-a828c96dc666");
        if (obj == null) {
            writeline("/home/ubuntu/results/coverage/Digester/Digester_6_10.coverage", "3a4d31f7-9911-443c-96ed-0c0e743b72c6");
            return null;
        }
        writeline("/home/ubuntu/results/coverage/Digester/Digester_6_10.coverage", "2dcf3e96-a068-4a7e-9be9-d0fd97d87f6e");
        @SuppressWarnings("unchecked") T result = (T) obj;
        writeline("/home/ubuntu/results/coverage/Digester/Digester_6_10.coverage", "6cd0c7b0-ca85-4a8e-8c1e-dca81df68bf7");
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
