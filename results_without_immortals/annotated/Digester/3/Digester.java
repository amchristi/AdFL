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
        writeline("/home/ubuntu/results/coverage/Digester/Digester_3_10.coverage", "e8e2e39d-9328-425e-9497-e0a564b2de2b");
        Stack<String> nsStack = namespaces.get(prefix);
        writeline("/home/ubuntu/results/coverage/Digester/Digester_3_10.coverage", "a9864fae-09dd-4909-9adf-580351e19f90");
        if (nsStack == null) {
            writeline("/home/ubuntu/results/coverage/Digester/Digester_3_10.coverage", "cc69b022-d750-4b7e-95ff-7bebcca55d7d");
            return null;
        }
        writeline("/home/ubuntu/results/coverage/Digester/Digester_3_10.coverage", "7ff785a6-0312-4b3c-9ef5-70633f92acb2");
        try {
            writeline("/home/ubuntu/results/coverage/Digester/Digester_3_10.coverage", "9156a79e-5d94-4fad-8262-f332fdf84e09");
            return (nsStack.peek());
        } catch (EmptyStackException e) {
            writeline("/home/ubuntu/results/coverage/Digester/Digester_3_10.coverage", "acdae0a0-8195-44de-b033-b64e2e6491c0");
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
        writeline("/home/ubuntu/results/coverage/Digester/Digester_3_10.coverage", "9a87b0cb-cd14-4e83-8204-63ce7aa38066");
        if (this.classLoader != null) {
            writeline("/home/ubuntu/results/coverage/Digester/Digester_3_10.coverage", "3b3eee7a-2693-4f8a-b314-1c68f0798bc7");
            return (this.classLoader);
        }
        writeline("/home/ubuntu/results/coverage/Digester/Digester_3_10.coverage", "f98e10c5-9b86-465f-8e97-49692c7b9bc7");
        if (this.useContextClassLoader) {
            writeline("/home/ubuntu/results/coverage/Digester/Digester_3_10.coverage", "7066bbc2-25dc-42c0-bd16-eb24c7435612");
            ClassLoader classLoader = Thread.currentThread().getContextClassLoader();
            writeline("/home/ubuntu/results/coverage/Digester/Digester_3_10.coverage", "79a283d4-7ffa-488d-a7d4-335fdbfde07d");
            if (classLoader != null) {
                writeline("/home/ubuntu/results/coverage/Digester/Digester_3_10.coverage", "be9c8204-6e8e-498d-8033-2491925f3de1");
                return (classLoader);
            }
        }
        writeline("/home/ubuntu/results/coverage/Digester/Digester_3_10.coverage", "e8d8b2e2-ebf9-43ef-9a8d-f441e5f8cdad");
        return (this.getClass().getClassLoader());
    }

    /**
     * Set the class loader to be used for instantiating application objects when required.
     *
     * @param classLoader The new class loader to use, or <code>null</code> to revert to the standard rules
     */
    public void setClassLoader(ClassLoader classLoader) {
        writeline("/home/ubuntu/results/coverage/Digester/Digester_3_10.coverage", "0bfb34d1-a6ae-4f77-b896-5bf7e98d3545");
        this.classLoader = classLoader;
    }

    /**
     * Return the current depth of the element stack.
     *
     * @return the current depth of the element stack.
     */
    public int getCount() {
        writeline("/home/ubuntu/results/coverage/Digester/Digester_3_10.coverage", "5fefea61-a47a-40a9-a812-1be7d497fba4");
        return (stack.size());
    }

    /**
     * Return the name of the XML element that is currently being processed.
     *
     * @return the name of the XML element that is currently being processed.
     */
    public String getCurrentElementName() {
        writeline("/home/ubuntu/results/coverage/Digester/Digester_3_10.coverage", "51c061b7-4292-4590-8038-73e20a0658e3");
        String elementName = match;
        writeline("/home/ubuntu/results/coverage/Digester/Digester_3_10.coverage", "a9ea7f65-c168-47c4-aa5c-2dd6f4d58dee");
        int lastSlash = elementName.lastIndexOf('/');
        writeline("/home/ubuntu/results/coverage/Digester/Digester_3_10.coverage", "27eb5cab-4309-4adf-bad3-2dc06c05ffae");
        if (lastSlash >= 0) {
            writeline("/home/ubuntu/results/coverage/Digester/Digester_3_10.coverage", "c2715908-63fe-467b-95a2-c0082ecec6de");
            elementName = elementName.substring(lastSlash + 1);
        }
        writeline("/home/ubuntu/results/coverage/Digester/Digester_3_10.coverage", "72491c1f-3761-4f5f-9e27-ae6267efeb74");
        return (elementName);
    }

    /**
     * Return the error handler for this Digester.
     *
     * @return the error handler for this Digester.
     */
    public ErrorHandler getErrorHandler() {
        writeline("/home/ubuntu/results/coverage/Digester/Digester_3_10.coverage", "54434a60-c0f9-4a9c-a78c-1c427c4b2086");
        return (this.errorHandler);
    }

    /**
     * Set the error handler for this Digester.
     *
     * @param errorHandler The new error handler
     */
    public void setErrorHandler(ErrorHandler errorHandler) {
        writeline("/home/ubuntu/results/coverage/Digester/Digester_3_10.coverage", "9def8c21-f4b2-4199-b37f-fa5e0ad685ff");
        this.errorHandler = errorHandler;
    }

    /**
     * Return the SAXParserFactory we will use, creating one if necessary.
     *
     * @return the SAXParserFactory we will use, creating one if necessary.
     */
    public SAXParserFactory getFactory() {
        writeline("/home/ubuntu/results/coverage/Digester/Digester_3_10.coverage", "d5fa78e2-5257-4b55-aa69-feee4b37bfa5");
        if (factory == null) {
            writeline("/home/ubuntu/results/coverage/Digester/Digester_3_10.coverage", "c305c651-16e3-4a24-9852-54c48d601697");
            factory = SAXParserFactory.newInstance();
            writeline("/home/ubuntu/results/coverage/Digester/Digester_3_10.coverage", "012bdfd1-e1ec-4cd8-b209-7cdfd622c6d6");
            factory.setNamespaceAware(namespaceAware);
            writeline("/home/ubuntu/results/coverage/Digester/Digester_3_10.coverage", "e720b6b6-8c47-4c09-996f-b8f123b32aff");
            factory.setXIncludeAware(xincludeAware);
            writeline("/home/ubuntu/results/coverage/Digester/Digester_3_10.coverage", "f40ad799-8a28-482f-917d-b425e5c9616b");
            factory.setValidating(validating);
            writeline("/home/ubuntu/results/coverage/Digester/Digester_3_10.coverage", "0fb0945f-4bca-4ef2-92d2-30d5013da59d");
            factory.setSchema(schema);
        }
        writeline("/home/ubuntu/results/coverage/Digester/Digester_3_10.coverage", "772c9922-451e-4018-9193-6a08bb76c171");
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
        writeline("/home/ubuntu/results/coverage/Digester/Digester_3_10.coverage", "6bffa6f0-a58c-4661-ae91-59c1a55ab37a");
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
        writeline("/home/ubuntu/results/coverage/Digester/Digester_3_10.coverage", "17e733f9-5127-4be5-b414-41504fba5bf0");
        getFactory().setFeature(feature, value);
    }

    /**
     * Return the current Logger associated with this instance of the Digester
     *
     * @return the current Logger associated with this instance of the Digester
     */
    public Log getLogger() {
        writeline("/home/ubuntu/results/coverage/Digester/Digester_3_10.coverage", "8eaaab22-cb77-4960-baaf-8c6dd18d8a33");
        return log;
    }

    /**
     * Set the current logger for this Digester.
     *
     * @param log the current logger for this Digester.
     */
    public void setLogger(Log log) {
        writeline("/home/ubuntu/results/coverage/Digester/Digester_3_10.coverage", "c3aca8b0-1442-4b47-9e32-67d784ec4fe1");
        this.log = log;
    }

    /**
     * Gets the logger used for logging SAX-related information. <strong>Note</strong> the output is finely grained.
     *
     * @return the logger used for logging SAX-related information
     * @since 1.6
     */
    public Log getSAXLogger() {
        writeline("/home/ubuntu/results/coverage/Digester/Digester_3_10.coverage", "9f8b13b9-99f9-4d33-819e-0e78a7ee8a52");
        return saxLog;
    }

    /**
     * Sets the logger used for logging SAX-related information. <strong>Note</strong> the output is finely grained.
     *
     * @param saxLog the logger used for logging SAX-related information, not null
     * @since 1.6
     */
    public void setSAXLogger(Log saxLog) {
        writeline("/home/ubuntu/results/coverage/Digester/Digester_3_10.coverage", "dbfc9d26-1ecd-4a9a-91ab-0f00e79b565a");
        this.saxLog = saxLog;
    }

    /**
     * Return the current rule match path
     *
     * @return the current rule match path
     */
    public String getMatch() {
        writeline("/home/ubuntu/results/coverage/Digester/Digester_3_10.coverage", "7c234a29-288a-4c7d-8821-abbf6d4446a7");
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
        writeline("/home/ubuntu/results/coverage/Digester/Digester_3_10.coverage", "ea5473ff-52ca-4c5b-86cd-a26d557fda2b");
        return matches;
    }

    /**
     * Return the "namespace aware" flag for parsers we create.
     *
     * @return the "namespace aware" flag for parsers we create.
     */
    public boolean getNamespaceAware() {
        writeline("/home/ubuntu/results/coverage/Digester/Digester_3_10.coverage", "c45a70b6-914b-498a-ab14-31a3c4dc69ca");
        return (this.namespaceAware);
    }

    /**
     * Set the "namespace aware" flag for parsers we create.
     *
     * @param namespaceAware The new "namespace aware" flag
     */
    public void setNamespaceAware(boolean namespaceAware) {
        writeline("/home/ubuntu/results/coverage/Digester/Digester_3_10.coverage", "901467cc-4989-4884-a3dc-8c3eb1b7252f");
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
        writeline("/home/ubuntu/results/coverage/Digester/Digester_3_10.coverage", "69186e74-1803-48f2-aa72-28280725be7c");
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
        writeline("/home/ubuntu/results/coverage/Digester/Digester_3_10.coverage", "8ca9635b-db50-437b-b446-ddc1e8929b6d");
        this.xincludeAware = xincludeAware;
    }

    /**
     * Set the public id of the current file being parse.
     *
     * @param publicId the DTD/Schema public's id.
     */
    public void setPublicId(String publicId) {
        writeline("/home/ubuntu/results/coverage/Digester/Digester_3_10.coverage", "7eb6e945-c666-493c-b76f-ba8f8b818d19");
        this.publicId = publicId;
    }

    /**
     * Return the public identifier of the DTD we are currently parsing under, if any.
     *
     * @return the public identifier of the DTD we are currently parsing under, if any.
     */
    public String getPublicId() {
        writeline("/home/ubuntu/results/coverage/Digester/Digester_3_10.coverage", "f7024908-2e41-4a3f-991b-f0489d945b33");
        return (this.publicId);
    }

    /**
     * Return the namespace URI that will be applied to all subsequently added <code>Rule</code> objects.
     *
     * @return the namespace URI that will be applied to all subsequently added <code>Rule</code> objects.
     */
    public String getRuleNamespaceURI() {
        writeline("/home/ubuntu/results/coverage/Digester/Digester_3_10.coverage", "d94e5a4c-3eeb-48f1-953a-c0b2aab1f633");
        return (getRules().getNamespaceURI());
    }

    /**
     * Set the namespace URI that will be applied to all subsequently added <code>Rule</code> objects.
     *
     * @param ruleNamespaceURI Namespace URI that must match on all subsequently added rules, or <code>null</code> for
     *            matching regardless of the current namespace URI
     */
    public void setRuleNamespaceURI(String ruleNamespaceURI) {
        writeline("/home/ubuntu/results/coverage/Digester/Digester_3_10.coverage", "df142385-ea90-4e4e-b7d4-561e9513d0d1");
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
        writeline("/home/ubuntu/results/coverage/Digester/Digester_3_10.coverage", "9f6ae249-94d4-480c-92eb-2a6fed97377f");
        if (parser != null) {
            writeline("/home/ubuntu/results/coverage/Digester/Digester_3_10.coverage", "8bda761d-a431-4a45-8654-d369b3a4af46");
            return (parser);
        }
        writeline("/home/ubuntu/results/coverage/Digester/Digester_3_10.coverage", "03ddae64-45a2-45ec-9997-64a45a4d31db");
        try {
            writeline("/home/ubuntu/results/coverage/Digester/Digester_3_10.coverage", "7e633372-17cb-4856-84d3-00609e32cd2f");
            parser = getFactory().newSAXParser();
        } catch (Exception e) {
            writeline("/home/ubuntu/results/coverage/Digester/Digester_3_10.coverage", "499e785f-2685-4c42-8cb8-7c5bae66e322");
            log.error("Digester.getParser: ", e);
            writeline("/home/ubuntu/results/coverage/Digester/Digester_3_10.coverage", "31b5d508-3a3b-4d32-98a2-f953a94c32c2");
            return (null);
        }
        writeline("/home/ubuntu/results/coverage/Digester/Digester_3_10.coverage", "e2200689-0d9e-4dbb-b367-cd373bb1b42e");
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
        writeline("/home/ubuntu/results/coverage/Digester/Digester_3_10.coverage", "227eeaf0-69db-4e12-8c44-de87aff2fef3");
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
        writeline("/home/ubuntu/results/coverage/Digester/Digester_3_10.coverage", "68c69305-6d66-4223-8c53-d6735d686470");
        getParser().setProperty(property, value);
    }

    /**
     * Return the <code>Rules</code> implementation object containing our rules collection and associated matching
     * policy. If none has been established, a default implementation will be created and returned.
     *
     * @return the <code>Rules</code> implementation object.
     */
    public Rules getRules() {
        writeline("/home/ubuntu/results/coverage/Digester/Digester_3_10.coverage", "738c39ed-0285-4a58-8349-fc95aca00cbd");
        if (this.rules == null) {
            writeline("/home/ubuntu/results/coverage/Digester/Digester_3_10.coverage", "d9ee7da5-4c3a-40e9-8d2e-8b9fcfdc3474");
            this.rules = new RulesBase();
            writeline("/home/ubuntu/results/coverage/Digester/Digester_3_10.coverage", "12c3cd83-f616-4b6c-839d-0968c4011008");
            this.rules.setDigester(this);
        }
        writeline("/home/ubuntu/results/coverage/Digester/Digester_3_10.coverage", "e40611a9-ffd8-4b50-9807-0ceb38a9a0c6");
        return (this.rules);
    }

    /**
     * Set the <code>Rules</code> implementation object containing our rules collection and associated matching policy.
     *
     * @param rules New Rules implementation
     */
    public void setRules(Rules rules) {
        writeline("/home/ubuntu/results/coverage/Digester/Digester_3_10.coverage", "ecfc55f5-7c68-488e-87b0-86506a3aa441");
        this.rules = rules;
        writeline("/home/ubuntu/results/coverage/Digester/Digester_3_10.coverage", "37c9eaf0-db87-4593-a5b1-8c456dcd0cc8");
        this.rules.setDigester(this);
    }

    /**
     * Return the XML Schema used when parsing.
     *
     * @return The {@link Schema} instance in use.
     * @since 2.0
     */
    public Schema getXMLSchema() {
        writeline("/home/ubuntu/results/coverage/Digester/Digester_3_10.coverage", "f716fcc2-1195-4654-8cc0-b692f07392da");
        return (this.schema);
    }

    /**
     * Set the XML Schema to be used when parsing.
     *
     * @param schema The {@link Schema} instance to use.
     * @since 2.0
     */
    public void setXMLSchema(Schema schema) {
        writeline("/home/ubuntu/results/coverage/Digester/Digester_3_10.coverage", "4d99e200-3926-4451-9d55-c72c02b39d4c");
        this.schema = schema;
    }

    /**
     * Return the boolean as to whether the context ClassLoader should be used.
     *
     * @return true, if the context ClassLoader should be used, false otherwise.
     */
    public boolean getUseContextClassLoader() {
        writeline("/home/ubuntu/results/coverage/Digester/Digester_3_10.coverage", "7462de37-034b-40e8-818e-00d56a88ca65");
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
        writeline("/home/ubuntu/results/coverage/Digester/Digester_3_10.coverage", "1fed8f51-933a-49ce-93be-2ca9137d4fb3");
        useContextClassLoader = use;
    }

    /**
     * Return the validating parser flag.
     *
     * @return the validating parser flag.
     */
    public boolean getValidating() {
        writeline("/home/ubuntu/results/coverage/Digester/Digester_3_10.coverage", "e123bab9-e3ae-47c4-9cab-87c6749f17c4");
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
        writeline("/home/ubuntu/results/coverage/Digester/Digester_3_10.coverage", "01cb2bef-2830-4a7d-8a27-3c54e1961f9a");
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
        writeline("/home/ubuntu/results/coverage/Digester/Digester_3_10.coverage", "55ff7ad2-e9ad-40cc-8564-516a0d181986");
        if (reader == null) {
            writeline("/home/ubuntu/results/coverage/Digester/Digester_3_10.coverage", "b48e9324-da7d-4c2f-a5d7-bc4e53f7976b");
            reader = getParser().getXMLReader();
        }
        writeline("/home/ubuntu/results/coverage/Digester/Digester_3_10.coverage", "ab001cd3-e345-463f-9266-2abbbc57c568");
        reader.setDTDHandler(this);
        writeline("/home/ubuntu/results/coverage/Digester/Digester_3_10.coverage", "d3ce2391-04fc-4c94-b86e-04363dfe1f37");
        reader.setContentHandler(this);
        writeline("/home/ubuntu/results/coverage/Digester/Digester_3_10.coverage", "b994f536-62a7-4aa9-8622-61cda1ac7f60");
        if (entityResolver == null) {
            writeline("/home/ubuntu/results/coverage/Digester/Digester_3_10.coverage", "951a7c55-83d5-4c95-ad3c-dc5b077d59e5");
            reader.setEntityResolver(this);
        } else {
            writeline("/home/ubuntu/results/coverage/Digester/Digester_3_10.coverage", "8c389867-ab2e-4216-a867-76dc76b83fd0");
            reader.setEntityResolver(entityResolver);
        }
        writeline("/home/ubuntu/results/coverage/Digester/Digester_3_10.coverage", "8d85857f-6128-4cf7-ba9e-fa6cf417e559");
        if (this.errorHandler != null) {
            writeline("/home/ubuntu/results/coverage/Digester/Digester_3_10.coverage", "8480c7da-27f5-4786-96f4-4f7619323c31");
            reader.setErrorHandler(this.errorHandler);
        } else {
            writeline("/home/ubuntu/results/coverage/Digester/Digester_3_10.coverage", "9af0ab8e-9b5f-4a2b-b05f-a66ed84527f2");
            reader.setErrorHandler(this);
        }
        writeline("/home/ubuntu/results/coverage/Digester/Digester_3_10.coverage", "ceb4aceb-3827-4c10-8737-18d6ecad5693");
        return reader;
    }

    /**
     * Gets the <code>Substitutor</code> used to convert attributes and body text.
     *
     * @return the <code>Substitutor</code> used to convert attributes and body text,
     *         null if not substitutions are to be performed.
     */
    public Substitutor getSubstitutor() {
        writeline("/home/ubuntu/results/coverage/Digester/Digester_3_10.coverage", "63bd57fc-590f-43dd-aea4-7b10e4aa7944");
        return substitutor;
    }

    /**
     * Sets the <code>Substitutor</code> to be used to convert attributes and body text.
     *
     * @param substitutor the Substitutor to be used to convert attributes and body text or null if not substitution of
     *            these values is to be performed.
     */
    public void setSubstitutor(Substitutor substitutor) {
        writeline("/home/ubuntu/results/coverage/Digester/Digester_3_10.coverage", "e134d1bd-ba30-4159-95e5-9e646458b9fa");
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
        writeline("/home/ubuntu/results/coverage/Digester/Digester_3_10.coverage", "426cbfc6-4d88-48d9-858f-83b6c98b0fd2");
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
        writeline("/home/ubuntu/results/coverage/Digester/Digester_3_10.coverage", "6d75d3b3-f747-4280-99c1-201bf29d2483");
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
        writeline("/home/ubuntu/results/coverage/Digester/Digester_3_10.coverage", "e5dda10f-a390-4cf0-bd77-d8af7bea5a7f");
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
        writeline("/home/ubuntu/results/coverage/Digester/Digester_3_10.coverage", "123929bd-ae68-4e20-8054-ef7de81326fe");
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
        writeline("/home/ubuntu/results/coverage/Digester/Digester_3_10.coverage", "d67e0b6a-0af1-4685-b087-c0fc070c7f06");
        if (!namespaceAware) {
            writeline("/home/ubuntu/results/coverage/Digester/Digester_3_10.coverage", "28bf3cde-20e5-47fd-b2c3-096b6333826a");
            log.warn("Digester is not namespace aware");
        }
        writeline("/home/ubuntu/results/coverage/Digester/Digester_3_10.coverage", "4ea535ff-9175-485c-bd27-5bbb6c361d4a");
        Map<String, String> currentNamespaces = new HashMap<String, String>();
        writeline("/home/ubuntu/results/coverage/Digester/Digester_3_10.coverage", "4ca3605d-e065-4c22-a12d-52fab27662d1");
        for (Map.Entry<String, Stack<String>> nsEntry : namespaces.entrySet()) {
            writeline("/home/ubuntu/results/coverage/Digester/Digester_3_10.coverage", "f4672c41-b71e-45f2-afc7-a8fb67524b43");
            try {
                writeline("/home/ubuntu/results/coverage/Digester/Digester_3_10.coverage", "d59b4a7b-e4cf-4955-83c1-cd4374222329");
                currentNamespaces.put(nsEntry.getKey(), nsEntry.getValue().peek());
            } catch (RuntimeException e) {
                writeline("/home/ubuntu/results/coverage/Digester/Digester_3_10.coverage", "eb043364-3f3d-4a00-b2b1-a69b43a15419");
                log.error(e.getMessage(), e);
                writeline("/home/ubuntu/results/coverage/Digester/Digester_3_10.coverage", "deca4cd6-db21-4628-adc0-2aba463515ef");
                throw e;
            }
        }
        writeline("/home/ubuntu/results/coverage/Digester/Digester_3_10.coverage", "0eba1a83-23e0-42b7-aef4-4a5fe880b112");
        return currentNamespaces;
    }

    /**
     * Returns the executor service used to run asynchronous parse method.
     *
     * @return the executor service used to run asynchronous parse method
     * @since 3.1
     */
    public ExecutorService getExecutorService() {
        writeline("/home/ubuntu/results/coverage/Digester/Digester_3_10.coverage", "8de31097-3d51-404f-8467-5720efad96b4");
        return executorService;
    }

    /**
     * Sets the executor service to run asynchronous parse method.
     *
     * @param executorService the executor service to run asynchronous parse method
     * @since 3.1
     */
    public void setExecutorService(ExecutorService executorService) {
        writeline("/home/ubuntu/results/coverage/Digester/Digester_3_10.coverage", "f777a299-ea35-4f26-9882-76d3f55c1bc6");
        this.executorService = executorService;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public void characters(char buffer[], int start, int length) throws SAXException {
        writeline("/home/ubuntu/results/coverage/Digester/Digester_3_10.coverage", "7cab11a3-8ea9-47c5-a7d6-7e481f7c5af0");
        if (customContentHandler != null) {
            writeline("/home/ubuntu/results/coverage/Digester/Digester_3_10.coverage", "74df4105-7e86-46e2-9f91-e36a53a71920");
            customContentHandler.characters(buffer, start, length);
            writeline("/home/ubuntu/results/coverage/Digester/Digester_3_10.coverage", "0d4e9cf0-0b6f-406f-97e5-ab2d923645dc");
            return;
        }
        writeline("/home/ubuntu/results/coverage/Digester/Digester_3_10.coverage", "c172a16d-56a8-4598-9b30-abbf0707c37a");
        if (saxLog.isDebugEnabled()) {
            writeline("/home/ubuntu/results/coverage/Digester/Digester_3_10.coverage", "bef05448-0502-4e14-8233-afaf3595a4c7");
            saxLog.debug("characters(" + new String(buffer, start, length) + ")");
        }
        writeline("/home/ubuntu/results/coverage/Digester/Digester_3_10.coverage", "dd7ae261-ca24-49cd-ace9-4328bcd92c85");
        bodyText.append(buffer, start, length);
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public void endDocument() throws SAXException {
        writeline("/home/ubuntu/results/coverage/Digester/Digester_3_10.coverage", "eeb7e5dc-15f0-4dbb-a40e-affe5e6d7a79");
        if (saxLog.isDebugEnabled()) {
            writeline("/home/ubuntu/results/coverage/Digester/Digester_3_10.coverage", "a34a739a-350b-463c-9c4e-11af9fd7c38f");
            if (getCount() > 1) {
                writeline("/home/ubuntu/results/coverage/Digester/Digester_3_10.coverage", "0ee42001-e8d4-4ff0-bf06-727d1507fab2");
                saxLog.debug("endDocument():  " + getCount() + " elements left");
            } else {
                writeline("/home/ubuntu/results/coverage/Digester/Digester_3_10.coverage", "1d100b65-4e67-40f3-8c10-c3aa17f4ed51");
                saxLog.debug("endDocument()");
            }
        }
        writeline("/home/ubuntu/results/coverage/Digester/Digester_3_10.coverage", "0df9ee9a-e5e5-4b60-8174-355fdf93e2aa");
        for (Rule rule : getRules().rules()) {
            writeline("/home/ubuntu/results/coverage/Digester/Digester_3_10.coverage", "95e2557d-4b29-42c8-ab2c-d2d261a97a0f");
            try {
                writeline("/home/ubuntu/results/coverage/Digester/Digester_3_10.coverage", "69946fb7-cb69-4bdf-8daa-165987fa5c74");
                rule.finish();
            } catch (Exception e) {
                writeline("/home/ubuntu/results/coverage/Digester/Digester_3_10.coverage", "34d6d73d-3107-46fa-9e7a-9d39080a67a7");
                log.error("Finish event threw exception", e);
                writeline("/home/ubuntu/results/coverage/Digester/Digester_3_10.coverage", "f3042245-5d5d-45a0-b1a9-31ef40373feb");
                throw createSAXException(e);
            } catch (Error e) {
                writeline("/home/ubuntu/results/coverage/Digester/Digester_3_10.coverage", "dbc67e26-0065-45ac-8379-fbfe2252603a");
                log.error("Finish event threw error", e);
                writeline("/home/ubuntu/results/coverage/Digester/Digester_3_10.coverage", "7387c74c-0fae-4d21-a634-662b7e71e68a");
                throw e;
            }
        }
        writeline("/home/ubuntu/results/coverage/Digester/Digester_3_10.coverage", "0328ad12-1daf-4b0c-93fb-b8c19596f5d9");
        clear();
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public void endElement(String namespaceURI, String localName, String qName) throws SAXException {
        writeline("/home/ubuntu/results/coverage/Digester/Digester_3_10.coverage", "62ed8454-a5e1-43c1-9b6d-2ad949ff510c");
        if (customContentHandler != null) {
            writeline("/home/ubuntu/results/coverage/Digester/Digester_3_10.coverage", "19b4975b-3c17-4a12-b02f-38141ece6058");
            customContentHandler.endElement(namespaceURI, localName, qName);
            writeline("/home/ubuntu/results/coverage/Digester/Digester_3_10.coverage", "37dfcb60-1a5a-4cba-a0cc-ad47be16fd1a");
            return;
        }
        writeline("/home/ubuntu/results/coverage/Digester/Digester_3_10.coverage", "9d164f72-eaa2-4aa9-94f9-bf982fa6a375");
        boolean debug = log.isDebugEnabled();
        writeline("/home/ubuntu/results/coverage/Digester/Digester_3_10.coverage", "da099154-981d-430e-86d3-c2cc5a4ffead");
        if (debug) {
            writeline("/home/ubuntu/results/coverage/Digester/Digester_3_10.coverage", "e072a547-24fa-4201-9f80-aa1fd83ca62b");
            if (saxLog.isDebugEnabled()) {
                writeline("/home/ubuntu/results/coverage/Digester/Digester_3_10.coverage", "617bc408-5c4e-4def-a2d7-26592d5b0ccd");
                saxLog.debug("endElement(" + namespaceURI + "," + localName + "," + qName + ")");
            }
            writeline("/home/ubuntu/results/coverage/Digester/Digester_3_10.coverage", "de663477-5ca4-4cc0-8b50-51e7639a0975");
            log.debug("  match='" + match + "'");
            writeline("/home/ubuntu/results/coverage/Digester/Digester_3_10.coverage", "4891b17a-a9ac-415c-9d29-86f6c416c410");
            log.debug("  bodyText='" + bodyText + "'");
        }
        writeline("/home/ubuntu/results/coverage/Digester/Digester_3_10.coverage", "418157f5-7084-4f66-9d49-5006160fdc57");
        String name = localName;
        writeline("/home/ubuntu/results/coverage/Digester/Digester_3_10.coverage", "627fdb84-4ec7-4548-b144-c1f0b3dfb399");
        if ((name == null) || (name.length() < 1)) {
            writeline("/home/ubuntu/results/coverage/Digester/Digester_3_10.coverage", "4e1c6a35-45cc-4b47-b056-732a597b8d4c");
            name = qName;
        }
        writeline("/home/ubuntu/results/coverage/Digester/Digester_3_10.coverage", "9330cc5d-7efc-4c3d-bd72-3ca2d10ee52a");
        List<Rule> rules = matches.pop();
        writeline("/home/ubuntu/results/coverage/Digester/Digester_3_10.coverage", "a7c18eb0-95f1-470c-97ee-ab2c6e0b40b2");
        if ((rules != null) && (rules.size() > 0)) {
            writeline("/home/ubuntu/results/coverage/Digester/Digester_3_10.coverage", "25f996ac-0129-488b-90dd-557ee57ac320");
            String bodyText = this.bodyText.toString();
            writeline("/home/ubuntu/results/coverage/Digester/Digester_3_10.coverage", "347b15b3-ca43-477a-9a64-684a236916da");
            Substitutor substitutor = getSubstitutor();
            writeline("/home/ubuntu/results/coverage/Digester/Digester_3_10.coverage", "e8a404c9-6fce-490d-ba16-923745ceaee3");
            if (substitutor != null) {
                writeline("/home/ubuntu/results/coverage/Digester/Digester_3_10.coverage", "440f1d72-89b6-450e-a296-64ce5e9c08fc");
                bodyText = substitutor.substitute(bodyText);
            }
            writeline("/home/ubuntu/results/coverage/Digester/Digester_3_10.coverage", "a3d48ed6-0c3e-49f9-8ca7-dc82f18728a5");
            for (int i = 0; i < rules.size(); i++) {
                writeline("/home/ubuntu/results/coverage/Digester/Digester_3_10.coverage", "227c92b6-d4cd-4d79-a8ae-f526e30f42b2");
                try {
                    writeline("/home/ubuntu/results/coverage/Digester/Digester_3_10.coverage", "4d67ff9d-ad47-4837-a2cf-876d924adae7");
                    Rule rule = rules.get(i);
                    writeline("/home/ubuntu/results/coverage/Digester/Digester_3_10.coverage", "3b07a6e8-baee-4784-b9bc-8cafc0caa6cd");
                    if (debug) {
                        writeline("/home/ubuntu/results/coverage/Digester/Digester_3_10.coverage", "f99a1a58-b4fb-4f45-a463-a66ec858385a");
                        log.debug("  Fire body() for " + rule);
                    }
                    writeline("/home/ubuntu/results/coverage/Digester/Digester_3_10.coverage", "c62fe207-7fe6-4b05-8f68-4af4c98ab60f");
                    rule.body(namespaceURI, name, bodyText);
                } catch (Exception e) {
                    writeline("/home/ubuntu/results/coverage/Digester/Digester_3_10.coverage", "eb3f9932-215d-4266-ba02-09a658067b4d");
                    log.error("Body event threw exception", e);
                    writeline("/home/ubuntu/results/coverage/Digester/Digester_3_10.coverage", "e1ef19ca-012c-44ef-8478-6f49395e1246");
                    throw createSAXException(e);
                } catch (Error e) {
                    writeline("/home/ubuntu/results/coverage/Digester/Digester_3_10.coverage", "36403da3-bb0c-4baf-8734-4e95e0fe116c");
                    log.error("Body event threw error", e);
                    writeline("/home/ubuntu/results/coverage/Digester/Digester_3_10.coverage", "03b25411-01b2-4216-93a5-4de91de25891");
                    throw e;
                }
            }
        } else {
            writeline("/home/ubuntu/results/coverage/Digester/Digester_3_10.coverage", "664a0bff-d518-4e83-a045-644a64e15789");
            if (debug) {
                writeline("/home/ubuntu/results/coverage/Digester/Digester_3_10.coverage", "6ad8762b-2569-49a8-b488-e0edd1018564");
                log.debug("  No rules found matching '" + match + "'.");
            }
        }
        writeline("/home/ubuntu/results/coverage/Digester/Digester_3_10.coverage", "52dbdd17-d0e9-4d39-992a-24e111f16476");
        bodyText = bodyTexts.pop();
        writeline("/home/ubuntu/results/coverage/Digester/Digester_3_10.coverage", "96583820-cc88-4327-9f8d-2540da0c98f1");
        if (debug) {
            writeline("/home/ubuntu/results/coverage/Digester/Digester_3_10.coverage", "487dc7c8-9d6e-4ed0-be66-72a097ed2f0e");
            log.debug("  Popping body text '" + bodyText.toString() + "'");
        }
        writeline("/home/ubuntu/results/coverage/Digester/Digester_3_10.coverage", "200cab46-e37f-4bc6-ad9e-3f25fae02e5b");
        if (rules != null) {
            writeline("/home/ubuntu/results/coverage/Digester/Digester_3_10.coverage", "75573189-cd1a-46a6-9d41-8f1378a3cedb");
            for (int i = 0; i < rules.size(); i++) {
                writeline("/home/ubuntu/results/coverage/Digester/Digester_3_10.coverage", "7f92a424-a421-4f57-86d2-5678e3ef4252");
                int j = (rules.size() - i) - 1;
                writeline("/home/ubuntu/results/coverage/Digester/Digester_3_10.coverage", "b6552878-577a-476c-a4f6-6220592eb4a8");
                try {
                    writeline("/home/ubuntu/results/coverage/Digester/Digester_3_10.coverage", "f80d452c-841b-4c03-9a65-838de15f22d3");
                    Rule rule = rules.get(j);
                    writeline("/home/ubuntu/results/coverage/Digester/Digester_3_10.coverage", "e489ba25-4034-4601-af16-7fba0db774d4");
                    if (debug) {
                        writeline("/home/ubuntu/results/coverage/Digester/Digester_3_10.coverage", "568e23aa-8c17-466e-8329-0c12da34c9b7");
                        log.debug("  Fire end() for " + rule);
                    }
                    writeline("/home/ubuntu/results/coverage/Digester/Digester_3_10.coverage", "955f2c37-7249-4114-b9f7-7602c57cc9b1");
                    rule.end(namespaceURI, name);
                } catch (Exception e) {
                    writeline("/home/ubuntu/results/coverage/Digester/Digester_3_10.coverage", "0c411d94-0b45-40f9-aae0-b6bcd77beb9a");
                    log.error("End event threw exception", e);
                    writeline("/home/ubuntu/results/coverage/Digester/Digester_3_10.coverage", "35e51eb8-9040-4d80-b841-4d00957df202");
                    throw createSAXException(e);
                } catch (Error e) {
                    writeline("/home/ubuntu/results/coverage/Digester/Digester_3_10.coverage", "9169939b-cf12-4cf0-8d34-b1e985ca73e8");
                    log.error("End event threw error", e);
                    writeline("/home/ubuntu/results/coverage/Digester/Digester_3_10.coverage", "f58abd58-6f24-4911-aa74-b3ae00a03ddb");
                    throw e;
                }
            }
        }
        writeline("/home/ubuntu/results/coverage/Digester/Digester_3_10.coverage", "13dc56f3-aca9-4331-93b3-c9c0eceaf0b9");
        int slash = match.lastIndexOf('/');
        writeline("/home/ubuntu/results/coverage/Digester/Digester_3_10.coverage", "9e3e360c-30de-4ab1-aa57-40422a0e9852");
        if (slash >= 0) {
            writeline("/home/ubuntu/results/coverage/Digester/Digester_3_10.coverage", "5a741da8-80f3-409f-8aba-c66ca140a7d9");
            match = match.substring(0, slash);
        } else {
            writeline("/home/ubuntu/results/coverage/Digester/Digester_3_10.coverage", "a05672c9-8260-4289-9ec0-b4a0f98219b5");
            match = "";
        }
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public void endPrefixMapping(String prefix) throws SAXException {
        writeline("/home/ubuntu/results/coverage/Digester/Digester_3_10.coverage", "77f4363f-7e93-4c2f-8ff0-ea030d8baf00");
        if (saxLog.isDebugEnabled()) {
            writeline("/home/ubuntu/results/coverage/Digester/Digester_3_10.coverage", "c89fd57a-0e38-4ab4-a2e4-41b2c9483be7");
            saxLog.debug("endPrefixMapping(" + prefix + ")");
        }
        writeline("/home/ubuntu/results/coverage/Digester/Digester_3_10.coverage", "7ad04220-d07d-4ec2-8b62-d952686f627b");
        Stack<String> stack = namespaces.get(prefix);
        writeline("/home/ubuntu/results/coverage/Digester/Digester_3_10.coverage", "20607ff0-2658-4649-b1de-9d62fea9959b");
        if (stack == null) {
            writeline("/home/ubuntu/results/coverage/Digester/Digester_3_10.coverage", "a27a504b-561b-484c-a0ec-8d2623ee1e69");
            return;
        }
        writeline("/home/ubuntu/results/coverage/Digester/Digester_3_10.coverage", "5019b326-2019-48b7-b44b-9e24d3c7999b");
        try {
            writeline("/home/ubuntu/results/coverage/Digester/Digester_3_10.coverage", "ad6cfc25-2094-4cb6-bb0b-e68d6d29573d");
            stack.pop();
            writeline("/home/ubuntu/results/coverage/Digester/Digester_3_10.coverage", "63e475af-e495-4254-a7b8-02f91b78c730");
            if (stack.empty()) {
                writeline("/home/ubuntu/results/coverage/Digester/Digester_3_10.coverage", "c8f052e5-1af4-49e5-84b3-667954cb45cb");
                namespaces.remove(prefix);
            }
        } catch (EmptyStackException e) {
            writeline("/home/ubuntu/results/coverage/Digester/Digester_3_10.coverage", "d3b8b0e4-2146-46e0-976b-7a1d2d4ddd6d");
            throw createSAXException("endPrefixMapping popped too many times");
        }
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public void ignorableWhitespace(char buffer[], int start, int len) throws SAXException {
        writeline("/home/ubuntu/results/coverage/Digester/Digester_3_10.coverage", "f73f7d07-4d9d-4d82-af2d-beef9092f850");
        if (saxLog.isDebugEnabled()) {
            writeline("/home/ubuntu/results/coverage/Digester/Digester_3_10.coverage", "2a9d0eac-141f-4da8-a2ed-c6721d4161e2");
            saxLog.debug("ignorableWhitespace(" + new String(buffer, start, len) + ")");
        }
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public void processingInstruction(String target, String data) throws SAXException {
        writeline("/home/ubuntu/results/coverage/Digester/Digester_3_10.coverage", "fac2e9c6-c8b9-4d90-8077-6ff33c97a85b");
        if (customContentHandler != null) {
            writeline("/home/ubuntu/results/coverage/Digester/Digester_3_10.coverage", "8a97da06-891c-4b0d-9c52-027e51e74062");
            customContentHandler.processingInstruction(target, data);
            writeline("/home/ubuntu/results/coverage/Digester/Digester_3_10.coverage", "00098bbc-c474-4625-b9b1-798096444f53");
            return;
        }
        writeline("/home/ubuntu/results/coverage/Digester/Digester_3_10.coverage", "bbf9211b-b5fa-42de-8cec-72249c2a3d1d");
        if (saxLog.isDebugEnabled()) {
            writeline("/home/ubuntu/results/coverage/Digester/Digester_3_10.coverage", "91216b4c-5936-4f6d-90be-be2041ba2d9b");
            saxLog.debug("processingInstruction('" + target + "','" + data + "')");
        }
    }

    /**
     * Gets the document locator associated with our parser.
     *
     * @return the Locator supplied by the document parser
     */
    public Locator getDocumentLocator() {
        writeline("/home/ubuntu/results/coverage/Digester/Digester_3_10.coverage", "29ec41bd-890d-43bf-91d8-a5ebac82c583");
        return locator;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public void setDocumentLocator(Locator locator) {
        writeline("/home/ubuntu/results/coverage/Digester/Digester_3_10.coverage", "0d50f1bf-04b5-4ab7-9ca0-5d64eca2a42f");
        if (saxLog.isDebugEnabled()) {
            writeline("/home/ubuntu/results/coverage/Digester/Digester_3_10.coverage", "fe09ffe9-07c7-40fd-a453-735511d07794");
            saxLog.debug("setDocumentLocator(" + locator + ")");
        }
        writeline("/home/ubuntu/results/coverage/Digester/Digester_3_10.coverage", "3f6065ea-2a05-471e-8986-286e699e06ac");
        this.locator = locator;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public void skippedEntity(String name) throws SAXException {
        writeline("/home/ubuntu/results/coverage/Digester/Digester_3_10.coverage", "679ec716-90c7-411e-8647-031911f0c65e");
        if (saxLog.isDebugEnabled()) {
            writeline("/home/ubuntu/results/coverage/Digester/Digester_3_10.coverage", "70411e53-7ced-48bc-a34c-42932dce9ffd");
            saxLog.debug("skippedEntity(" + name + ")");
        }
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public void startDocument() throws SAXException {
        writeline("/home/ubuntu/results/coverage/Digester/Digester_3_10.coverage", "cdfe3b81-9a6f-4453-94e6-eeacf775e668");
        if (saxLog.isDebugEnabled()) {
            writeline("/home/ubuntu/results/coverage/Digester/Digester_3_10.coverage", "390d9bc8-f408-4fe4-9554-91d76c4fa993");
            saxLog.debug("startDocument()");
        }
        writeline("/home/ubuntu/results/coverage/Digester/Digester_3_10.coverage", "f04bf07e-ff9a-4573-be80-ed313a1e0e24");
        configure();
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public void startElement(String namespaceURI, String localName, String qName, Attributes list) throws SAXException {
        writeline("/home/ubuntu/results/coverage/Digester/Digester_3_10.coverage", "d4093c85-6509-4964-ac86-123b23febd33");
        boolean debug = log.isDebugEnabled();
        writeline("/home/ubuntu/results/coverage/Digester/Digester_3_10.coverage", "2bed9c8b-36eb-48d3-b463-20e5b579c9b0");
        if (customContentHandler != null) {
            writeline("/home/ubuntu/results/coverage/Digester/Digester_3_10.coverage", "c791b231-6f85-4bfb-944b-e54c51a4fa80");
            customContentHandler.startElement(namespaceURI, localName, qName, list);
            writeline("/home/ubuntu/results/coverage/Digester/Digester_3_10.coverage", "bd804b9b-e791-422e-ae33-0adbf7b2a4c7");
            return;
        }
        writeline("/home/ubuntu/results/coverage/Digester/Digester_3_10.coverage", "1b9183d1-ea41-4c5a-b81e-99529058d61e");
        if (saxLog.isDebugEnabled()) {
            writeline("/home/ubuntu/results/coverage/Digester/Digester_3_10.coverage", "c59ed77a-e94c-4bc0-a54a-8cd9c1a4d931");
            saxLog.debug("startElement(" + namespaceURI + "," + localName + "," + qName + ")");
        }
        writeline("/home/ubuntu/results/coverage/Digester/Digester_3_10.coverage", "4f0b2c83-4496-41f2-a863-f8b694b926ad");
        bodyTexts.push(bodyText);
        writeline("/home/ubuntu/results/coverage/Digester/Digester_3_10.coverage", "17ae6bb7-cac8-433a-b115-30d980ee3af3");
        if (debug) {
            writeline("/home/ubuntu/results/coverage/Digester/Digester_3_10.coverage", "3f4c7409-4a98-4521-9260-c114ea37150a");
            log.debug("  Pushing body text '" + bodyText.toString() + "'");
        }
        writeline("/home/ubuntu/results/coverage/Digester/Digester_3_10.coverage", "6ae1e76a-c1c7-4ecf-b025-4e53b8a7a8b2");
        bodyText = new StringBuilder();
        writeline("/home/ubuntu/results/coverage/Digester/Digester_3_10.coverage", "546fa7f5-6fca-4136-8e43-14c3e52ffaa4");
        String name = localName;
        writeline("/home/ubuntu/results/coverage/Digester/Digester_3_10.coverage", "7a581730-d7ca-4729-9f09-2671da14aa36");
        if ((name == null) || (name.length() < 1)) {
            writeline("/home/ubuntu/results/coverage/Digester/Digester_3_10.coverage", "771bd47b-97c0-46ea-b5bb-b3f5eaf08aa1");
            name = qName;
        }
        writeline("/home/ubuntu/results/coverage/Digester/Digester_3_10.coverage", "b7a40762-09d7-40c2-a4ec-f31885edeabe");
        StringBuilder sb = new StringBuilder(match);
        writeline("/home/ubuntu/results/coverage/Digester/Digester_3_10.coverage", "3afb7de6-03fd-431a-bc5c-2266b4020e0d");
        if (match.length() > 0) {
            writeline("/home/ubuntu/results/coverage/Digester/Digester_3_10.coverage", "5b8cb579-2126-4842-ad7b-038d5e636699");
            sb.append('/');
        }
        writeline("/home/ubuntu/results/coverage/Digester/Digester_3_10.coverage", "022c667d-3490-40ac-bd68-6cef171b196d");
        sb.append(name);
        writeline("/home/ubuntu/results/coverage/Digester/Digester_3_10.coverage", "91dd341c-d15b-4830-8ed8-886a66b6cdb1");
        match = sb.toString();
        writeline("/home/ubuntu/results/coverage/Digester/Digester_3_10.coverage", "04e8b963-301b-4d3f-86c6-59883ad0d9e8");
        if (debug) {
            writeline("/home/ubuntu/results/coverage/Digester/Digester_3_10.coverage", "d24f399f-c3d7-4628-abc7-3968f45f4370");
            log.debug("  New match='" + match + "'");
        }
        writeline("/home/ubuntu/results/coverage/Digester/Digester_3_10.coverage", "438b8320-ac0d-403b-8c9b-491187c64a0a");
        List<Rule> rules = getRules().match(namespaceURI, match, localName, list);
        writeline("/home/ubuntu/results/coverage/Digester/Digester_3_10.coverage", "8de3702d-d98e-4b49-a7a8-dfcf81a7ff64");
        matches.push(rules);
        writeline("/home/ubuntu/results/coverage/Digester/Digester_3_10.coverage", "c4bfbd15-7d70-4808-90a8-314dadfbab8e");
        if ((rules != null) && (rules.size() > 0)) {
            writeline("/home/ubuntu/results/coverage/Digester/Digester_3_10.coverage", "e19140c3-26aa-490c-b89b-78497bef5b6e");
            Substitutor substitutor = getSubstitutor();
            writeline("/home/ubuntu/results/coverage/Digester/Digester_3_10.coverage", "a344828d-ea9e-4427-b810-b874710ca51f");
            if (substitutor != null) {
                writeline("/home/ubuntu/results/coverage/Digester/Digester_3_10.coverage", "abf4e69c-d40f-478f-b849-c8e2a4432cad");
                list = substitutor.substitute(list);
            }
            writeline("/home/ubuntu/results/coverage/Digester/Digester_3_10.coverage", "9a829bb3-679f-4257-924e-c22f7812b1eb");
            for (int i = 0; i < rules.size(); i++) {
                writeline("/home/ubuntu/results/coverage/Digester/Digester_3_10.coverage", "6a153510-1efb-444d-9012-e977c223abf5");
                try {
                    writeline("/home/ubuntu/results/coverage/Digester/Digester_3_10.coverage", "7858a55a-416a-4f51-9edb-34affe83c3c0");
                    Rule rule = rules.get(i);
                    writeline("/home/ubuntu/results/coverage/Digester/Digester_3_10.coverage", "8563b93f-9865-4333-9bc5-07e69eacdc0a");
                    if (debug) {
                        writeline("/home/ubuntu/results/coverage/Digester/Digester_3_10.coverage", "d4d9cc51-b056-4028-b673-9d53ba924210");
                        log.debug("  Fire begin() for " + rule);
                    }
                    writeline("/home/ubuntu/results/coverage/Digester/Digester_3_10.coverage", "504060ac-7fa8-4581-8e6a-1ae829bffc3c");
                    rule.begin(namespaceURI, name, list);
                } catch (Exception e) {
                    writeline("/home/ubuntu/results/coverage/Digester/Digester_3_10.coverage", "048ca60c-fe97-41a7-b94a-59d15828a39d");
                    log.error("Begin event threw exception", e);
                    writeline("/home/ubuntu/results/coverage/Digester/Digester_3_10.coverage", "a95f60b1-675d-4758-8f96-8bf0dd77d582");
                    throw createSAXException(e);
                } catch (Error e) {
                    writeline("/home/ubuntu/results/coverage/Digester/Digester_3_10.coverage", "c1fe5b04-b584-4a7d-b406-150a337853e4");
                    log.error("Begin event threw error", e);
                    writeline("/home/ubuntu/results/coverage/Digester/Digester_3_10.coverage", "bf065dcd-5912-4429-8119-dcb228e98b26");
                    throw e;
                }
            }
        } else {
            writeline("/home/ubuntu/results/coverage/Digester/Digester_3_10.coverage", "2e65ad33-9650-4d30-86fa-6fa88905a7d3");
            if (debug) {
                writeline("/home/ubuntu/results/coverage/Digester/Digester_3_10.coverage", "0cfa6731-6ff7-4d94-bbec-10bdf00a4b85");
                log.debug("  No rules found matching '" + match + "'.");
            }
        }
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public void startPrefixMapping(String prefix, String namespaceURI) throws SAXException {
        writeline("/home/ubuntu/results/coverage/Digester/Digester_3_10.coverage", "8f44ba2c-81c5-40a7-b052-d271c5c44480");
        if (saxLog.isDebugEnabled()) {
            writeline("/home/ubuntu/results/coverage/Digester/Digester_3_10.coverage", "ff1a45b5-9a59-446a-ae0f-6e2db0a5a5fd");
            saxLog.debug("startPrefixMapping(" + prefix + "," + namespaceURI + ")");
        }
        writeline("/home/ubuntu/results/coverage/Digester/Digester_3_10.coverage", "78d2238c-fc72-4fb9-85ab-5281f3838b93");
        Stack<String> stack = namespaces.get(prefix);
        writeline("/home/ubuntu/results/coverage/Digester/Digester_3_10.coverage", "f76299ee-4d38-43e4-afa4-0c114ce7052c");
        if (stack == null) {
            writeline("/home/ubuntu/results/coverage/Digester/Digester_3_10.coverage", "6ca16a00-0d8b-42ea-b045-98be58a86ebd");
            stack = new Stack<String>();
            writeline("/home/ubuntu/results/coverage/Digester/Digester_3_10.coverage", "a1896855-5e88-44b1-8015-85e55fc3b5c5");
            namespaces.put(prefix, stack);
        }
        writeline("/home/ubuntu/results/coverage/Digester/Digester_3_10.coverage", "0888212f-292b-440c-9f1f-9093bd7744ef");
        stack.push(namespaceURI);
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public void notationDecl(String name, String publicId, String systemId) {
        writeline("/home/ubuntu/results/coverage/Digester/Digester_3_10.coverage", "248b5954-3879-4e8e-9e9b-95711d9477fd");
        if (saxLog.isDebugEnabled()) {
            writeline("/home/ubuntu/results/coverage/Digester/Digester_3_10.coverage", "ab42cd34-7a2e-4a15-91ae-936ecd8c9203");
            saxLog.debug("notationDecl(" + name + "," + publicId + "," + systemId + ")");
        }
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public void unparsedEntityDecl(String name, String publicId, String systemId, String notation) {
        writeline("/home/ubuntu/results/coverage/Digester/Digester_3_10.coverage", "ab2466ff-9d09-42e5-bf22-bf2e17632a35");
        if (saxLog.isDebugEnabled()) {
            writeline("/home/ubuntu/results/coverage/Digester/Digester_3_10.coverage", "cbc9e982-3ae4-43e3-94f1-43e0bc213711");
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
        writeline("/home/ubuntu/results/coverage/Digester/Digester_3_10.coverage", "5c4681c7-fc2a-40bf-9406-73d08379d952");
        this.entityResolver = entityResolver;
    }

    /**
     * Return the Entity Resolver used by the SAX parser.
     *
     * @return the Entity Resolver used by the SAX parser.
     */
    public EntityResolver getEntityResolver() {
        writeline("/home/ubuntu/results/coverage/Digester/Digester_3_10.coverage", "f804842c-f5b0-46a0-a482-1da174e3dff9");
        return entityResolver;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public InputSource resolveEntity(String publicId, String systemId) throws SAXException {
        writeline("/home/ubuntu/results/coverage/Digester/Digester_3_10.coverage", "1c6b6f96-8e2d-4420-a2d6-48d515961180");
        if (saxLog.isDebugEnabled()) {
            writeline("/home/ubuntu/results/coverage/Digester/Digester_3_10.coverage", "5efc199f-b56c-4926-83cc-3f69674f07cd");
            saxLog.debug("resolveEntity('" + publicId + "', '" + systemId + "')");
        }
        writeline("/home/ubuntu/results/coverage/Digester/Digester_3_10.coverage", "8e87aab4-81f3-4ae7-8695-2b14f74168fb");
        if (publicId != null) {
            writeline("/home/ubuntu/results/coverage/Digester/Digester_3_10.coverage", "7078672e-5976-41d7-b6ff-3bcefdd28fc7");
            this.publicId = publicId;
        }
        writeline("/home/ubuntu/results/coverage/Digester/Digester_3_10.coverage", "e7d10e56-b0bc-4f1d-9a46-95a0a9c29e81");
        URL entityURL = null;
        writeline("/home/ubuntu/results/coverage/Digester/Digester_3_10.coverage", "c53eaf6a-c499-4d4e-8a73-a15311d7c68f");
        if (publicId != null) {
            writeline("/home/ubuntu/results/coverage/Digester/Digester_3_10.coverage", "3f3efdff-6eea-487d-aac7-baea62d43f13");
            entityURL = entityValidator.get(publicId);
        }
        writeline("/home/ubuntu/results/coverage/Digester/Digester_3_10.coverage", "42c4558a-aa8b-4437-a4a8-4e897da82281");
        if (entityURL == null && systemId != null) {
            writeline("/home/ubuntu/results/coverage/Digester/Digester_3_10.coverage", "24ad24b9-22a9-41af-b0fe-fb78c945dfff");
            entityURL = entityValidator.get(systemId);
        }
        writeline("/home/ubuntu/results/coverage/Digester/Digester_3_10.coverage", "f64035cf-104a-480d-9ccb-406f37a51431");
        if (entityURL == null) {
            writeline("/home/ubuntu/results/coverage/Digester/Digester_3_10.coverage", "e415d9cd-396d-43bc-b375-ca019c2f7125");
            if (systemId == null) {
                writeline("/home/ubuntu/results/coverage/Digester/Digester_3_10.coverage", "abf637e5-3c58-4170-8864-21ce15c67982");
                if (log.isDebugEnabled()) {
                    writeline("/home/ubuntu/results/coverage/Digester/Digester_3_10.coverage", "69bd9941-e3d8-4c06-8966-ff80d15e9bab");
                    log.debug(" Cannot resolve null entity, returning null InputSource");
                }
                writeline("/home/ubuntu/results/coverage/Digester/Digester_3_10.coverage", "aca870f6-1b2a-49ec-984c-8c13a5015725");
                return (null);
            }
            writeline("/home/ubuntu/results/coverage/Digester/Digester_3_10.coverage", "3167ed1c-64ed-49b7-a33d-5c9316ca11b1");
            if (log.isDebugEnabled()) {
                writeline("/home/ubuntu/results/coverage/Digester/Digester_3_10.coverage", "222bf29a-ef91-4043-b123-4f3e9f3fa6bd");
                log.debug(" Trying to resolve using system ID '" + systemId + "'");
            }
            writeline("/home/ubuntu/results/coverage/Digester/Digester_3_10.coverage", "83e87d4a-8c76-483e-a9b4-09439bbbf16b");
            try {
                writeline("/home/ubuntu/results/coverage/Digester/Digester_3_10.coverage", "322cfaba-d558-4c63-9ce5-8d755da43b5f");
                entityURL = new URL(systemId);
            } catch (MalformedURLException e) {
                writeline("/home/ubuntu/results/coverage/Digester/Digester_3_10.coverage", "00edfcb3-5c38-4a4a-944f-549333f60782");
                throw new IllegalArgumentException("Malformed URL '" + systemId + "' : " + e.getMessage());
            }
        }
        writeline("/home/ubuntu/results/coverage/Digester/Digester_3_10.coverage", "52c74520-23ba-4e27-baad-bf4c47a831ee");
        if (log.isDebugEnabled()) {
            writeline("/home/ubuntu/results/coverage/Digester/Digester_3_10.coverage", "d7c44fb2-f515-4562-ab6c-1adcdf5b4a3a");
            log.debug(" Resolving to alternate DTD '" + entityURL + "'");
        }
        writeline("/home/ubuntu/results/coverage/Digester/Digester_3_10.coverage", "68d8189d-f1cb-40a8-8df4-d125d82640e7");
        try {
            writeline("/home/ubuntu/results/coverage/Digester/Digester_3_10.coverage", "c14efbc7-6851-420d-bcb6-676acb34cdbf");
            return createInputSourceFromURL(entityURL);
        } catch (Exception e) {
            writeline("/home/ubuntu/results/coverage/Digester/Digester_3_10.coverage", "ac37463b-848e-481d-a6ae-e255e485c0d5");
            throw createSAXException(e);
        }
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public void error(SAXParseException exception) throws SAXException {
        writeline("/home/ubuntu/results/coverage/Digester/Digester_3_10.coverage", "63cc9220-a86e-4f27-857f-6cda6adb3101");
        log.error("Parse Error at line " + exception.getLineNumber() + " column " + exception.getColumnNumber() + ": " + exception.getMessage(), exception);
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public void fatalError(SAXParseException exception) throws SAXException {
        writeline("/home/ubuntu/results/coverage/Digester/Digester_3_10.coverage", "ef126ff2-42c3-4a3b-8d14-8a5a32ffd64a");
        log.error("Parse Fatal Error at line " + exception.getLineNumber() + " column " + exception.getColumnNumber() + ": " + exception.getMessage(), exception);
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public void warning(SAXParseException exception) throws SAXException {
        writeline("/home/ubuntu/results/coverage/Digester/Digester_3_10.coverage", "8f67f482-ea37-43d1-829d-ee1ffa52e6ff");
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
        writeline("/home/ubuntu/results/coverage/Digester/Digester_3_10.coverage", "9f855590-ea51-4d44-94da-a96bff3bf0e4");
        if (file == null) {
            writeline("/home/ubuntu/results/coverage/Digester/Digester_3_10.coverage", "25bbafdd-a462-43a8-91cd-2dd72fbf263f");
            throw new IllegalArgumentException("File to parse is null");
        }
        writeline("/home/ubuntu/results/coverage/Digester/Digester_3_10.coverage", "e56a0364-48fa-48ee-8384-fc31a380b44f");
        InputSource input = new InputSource(new FileInputStream(file));
        writeline("/home/ubuntu/results/coverage/Digester/Digester_3_10.coverage", "bf6df2c5-3959-47ad-8f0f-26d7ac69bb6c");
        input.setSystemId(file.toURI().toURL().toString());
        writeline("/home/ubuntu/results/coverage/Digester/Digester_3_10.coverage", "9f8e217e-c8bf-4a3a-9ad8-70c225cd2e5c");
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
        writeline("/home/ubuntu/results/coverage/Digester/Digester_3_10.coverage", "3a8f31e0-3fe6-4530-b07b-610fb86e711b");
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
        writeline("/home/ubuntu/results/coverage/Digester/Digester_3_10.coverage", "77a9119b-1658-49be-93df-fb2f043aebfa");
        if (input == null) {
            writeline("/home/ubuntu/results/coverage/Digester/Digester_3_10.coverage", "6591e581-8f70-49d7-a0da-54755d280941");
            throw new IllegalArgumentException("InputSource to parse is null");
        }
        writeline("/home/ubuntu/results/coverage/Digester/Digester_3_10.coverage", "1851f76b-71fe-4673-a210-e10c7fcccc82");
        configure();
        writeline("/home/ubuntu/results/coverage/Digester/Digester_3_10.coverage", "830355d3-1d49-460e-8848-9479d8b4fcdc");
        String systemId = input.getSystemId();
        writeline("/home/ubuntu/results/coverage/Digester/Digester_3_10.coverage", "869f0fa4-41d6-4a94-b8da-f61527030d6d");
        if (systemId == null) {
            writeline("/home/ubuntu/results/coverage/Digester/Digester_3_10.coverage", "bafc855a-2c78-457f-9782-fb64306528b4");
            systemId = "(already loaded from stream)";
        }
        writeline("/home/ubuntu/results/coverage/Digester/Digester_3_10.coverage", "0d3b8316-79a6-484a-9fd5-92f7749a4d8a");
        try {
            writeline("/home/ubuntu/results/coverage/Digester/Digester_3_10.coverage", "30f4c56c-c9b0-4dd0-b6d8-8ea0db3d5aa4");
            getXMLReader().parse(input);
        } catch (IOException e) {
            writeline("/home/ubuntu/results/coverage/Digester/Digester_3_10.coverage", "01d70cb1-0eb9-47cc-849f-384224a49499");
            log.error(format("An error occurred while reading stream from '%s', see nested exceptions", systemId), e);
            writeline("/home/ubuntu/results/coverage/Digester/Digester_3_10.coverage", "684d518b-6997-4316-b786-52b6be4c4cd2");
            throw e;
        }
        writeline("/home/ubuntu/results/coverage/Digester/Digester_3_10.coverage", "63157937-7369-4f41-bb80-859daf0572a8");
        cleanup();
        writeline("/home/ubuntu/results/coverage/Digester/Digester_3_10.coverage", "683a4f18-c991-4aa9-be7d-7ea181eea330");
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
        writeline("/home/ubuntu/results/coverage/Digester/Digester_3_10.coverage", "9334c6b3-24ce-471b-98c8-a0e90747af32");
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
        writeline("/home/ubuntu/results/coverage/Digester/Digester_3_10.coverage", "f1c69188-0de6-416b-b7eb-8a894215931f");
        if (input == null) {
            writeline("/home/ubuntu/results/coverage/Digester/Digester_3_10.coverage", "2ba3758d-8822-46f9-b70f-1ff8b07a07ce");
            throw new IllegalArgumentException("InputStream to parse is null");
        }
        writeline("/home/ubuntu/results/coverage/Digester/Digester_3_10.coverage", "4faff597-04a4-4a13-b387-2df636b8a55b");
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
        writeline("/home/ubuntu/results/coverage/Digester/Digester_3_10.coverage", "17cff49c-21c0-411e-acae-5f0c976fb436");
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
        writeline("/home/ubuntu/results/coverage/Digester/Digester_3_10.coverage", "0d9fd6ab-52ba-42a9-a448-f0e2dd65c78b");
        if (reader == null) {
            writeline("/home/ubuntu/results/coverage/Digester/Digester_3_10.coverage", "4940acd8-1104-4d3c-bc1d-5741b13f763b");
            throw new IllegalArgumentException("Reader to parse is null");
        }
        writeline("/home/ubuntu/results/coverage/Digester/Digester_3_10.coverage", "15940aed-99d9-4efc-acd1-1e8b2d10a752");
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
        writeline("/home/ubuntu/results/coverage/Digester/Digester_3_10.coverage", "fe668811-644a-49f7-b3cb-214b0965f46c");
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
        writeline("/home/ubuntu/results/coverage/Digester/Digester_3_10.coverage", "0a76986f-a138-4b4a-8d17-5d1c0f3f8e3b");
        if (uri == null) {
            writeline("/home/ubuntu/results/coverage/Digester/Digester_3_10.coverage", "143b4487-072f-4273-9669-bc18daa8f8e4");
            throw new IllegalArgumentException("String URI to parse is null");
        }
        writeline("/home/ubuntu/results/coverage/Digester/Digester_3_10.coverage", "3bb19594-1fe2-48b9-91e7-4cf694ed3a82");
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
        writeline("/home/ubuntu/results/coverage/Digester/Digester_3_10.coverage", "130699db-9feb-44be-a813-9b395a11e5a5");
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
        writeline("/home/ubuntu/results/coverage/Digester/Digester_3_10.coverage", "500dece8-358a-4586-a303-b4b05f95126e");
        if (url == null) {
            writeline("/home/ubuntu/results/coverage/Digester/Digester_3_10.coverage", "6af945d9-de1b-4405-b042-6565194f4641");
            throw new IllegalArgumentException("URL to parse is null");
        }
        writeline("/home/ubuntu/results/coverage/Digester/Digester_3_10.coverage", "a06d0a78-7997-4c45-91a4-880b64a37de9");
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
        writeline("/home/ubuntu/results/coverage/Digester/Digester_3_10.coverage", "aa2280ad-6313-4fd6-86af-3af10b6c70c9");
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
        writeline("/home/ubuntu/results/coverage/Digester/Digester_3_10.coverage", "674a3146-c107-47b8-a522-beedb7b2c8bb");
        if (executorService == null) {
            writeline("/home/ubuntu/results/coverage/Digester/Digester_3_10.coverage", "6481edb9-6bfa-4b43-8300-4347405287e3");
            throw new IllegalStateException("ExecutorService not set");
        }
        writeline("/home/ubuntu/results/coverage/Digester/Digester_3_10.coverage", "420ffb50-8f45-4593-95f6-d54ab24d93f5");
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
        writeline("/home/ubuntu/results/coverage/Digester/Digester_3_10.coverage", "737df84b-af41-4893-a12c-7191ed81edf3");
        if (log.isDebugEnabled()) {
            writeline("/home/ubuntu/results/coverage/Digester/Digester_3_10.coverage", "417ab57b-f32b-47bc-b0f9-3c6c6a84ce2f");
            log.debug("register('" + publicId + "', '" + entityURL + "'");
        }
        writeline("/home/ubuntu/results/coverage/Digester/Digester_3_10.coverage", "3755b2d3-fa14-415a-b854-07932a253ff8");
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
        writeline("/home/ubuntu/results/coverage/Digester/Digester_3_10.coverage", "46195f41-0c25-4696-b079-3ae1376a1e93");
        if (log.isDebugEnabled()) {
            writeline("/home/ubuntu/results/coverage/Digester/Digester_3_10.coverage", "f207b46c-3e97-47f8-8957-bb3379cf8957");
            log.debug("register('" + publicId + "', '" + entityURL + "'");
        }
        writeline("/home/ubuntu/results/coverage/Digester/Digester_3_10.coverage", "b4a2f1c3-6fc5-4380-be65-8af460083902");
        try {
            writeline("/home/ubuntu/results/coverage/Digester/Digester_3_10.coverage", "45ee6992-e317-4c0d-925a-a0cfe6210ed0");
            entityValidator.put(publicId, new URL(entityURL));
        } catch (MalformedURLException e) {
            writeline("/home/ubuntu/results/coverage/Digester/Digester_3_10.coverage", "47ebad09-9f63-4965-9a22-b1cd448887ca");
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
        writeline("/home/ubuntu/results/coverage/Digester/Digester_3_10.coverage", "84e411ac-4fbd-4702-bc5c-30aefb62a973");
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
        writeline("/home/ubuntu/results/coverage/Digester/Digester_3_10.coverage", "7df39917-9a1f-462e-8df2-0fd7486789dc");
        URLConnection connection = url.openConnection();
        writeline("/home/ubuntu/results/coverage/Digester/Digester_3_10.coverage", "027a5aeb-69bb-426b-af14-3fa5962fc496");
        connection.setUseCaches(false);
        writeline("/home/ubuntu/results/coverage/Digester/Digester_3_10.coverage", "4ef4a3e3-068f-4789-a640-5c269f9d07a4");
        InputStream stream = connection.getInputStream();
        writeline("/home/ubuntu/results/coverage/Digester/Digester_3_10.coverage", "d2572c1c-0846-4545-a836-d9c9e22024f4");
        InputSource source = new InputSource(stream);
        writeline("/home/ubuntu/results/coverage/Digester/Digester_3_10.coverage", "a56ae326-6551-4ea8-95a7-8cbc46a36726");
        source.setSystemId(url.toExternalForm());
        writeline("/home/ubuntu/results/coverage/Digester/Digester_3_10.coverage", "0130fd27-9084-48ce-9be4-ff213d0ec073");
        inputSources.add(source);
        writeline("/home/ubuntu/results/coverage/Digester/Digester_3_10.coverage", "ca429b0c-a773-4a33-952a-16d1f6f58624");
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
        writeline("/home/ubuntu/results/coverage/Digester/Digester_3_10.coverage", "2a6d2afb-0fd5-4708-92e3-5a6efe31a191");
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
        writeline("/home/ubuntu/results/coverage/Digester/Digester_3_10.coverage", "4ab0f8f3-cd7a-4a84-9d13-e7e4cd808424");
        rule.setDigester(this);
        writeline("/home/ubuntu/results/coverage/Digester/Digester_3_10.coverage", "14af2d6e-7b3a-4001-a45d-d35d6cc8c33e");
        getRules().add(pattern, rule);
    }

    /**
     * Register a set of Rule instances defined in a RuleSet.
     *
     * @param ruleSet The RuleSet instance to configure from
     */
    public void addRuleSet(RuleSet ruleSet) {
        writeline("/home/ubuntu/results/coverage/Digester/Digester_3_10.coverage", "b41b8f13-68c2-435f-a845-72f0c7249f77");
        String oldNamespaceURI = getRuleNamespaceURI();
        writeline("/home/ubuntu/results/coverage/Digester/Digester_3_10.coverage", "75e23384-69df-48c3-94ae-7090e09be7d3");
        String newNamespaceURI = ruleSet.getNamespaceURI();
        writeline("/home/ubuntu/results/coverage/Digester/Digester_3_10.coverage", "b3386042-e9ed-4ac5-8391-72134c5c10bf");
        if (log.isDebugEnabled()) {
            writeline("/home/ubuntu/results/coverage/Digester/Digester_3_10.coverage", "440ae9f7-32aa-4700-8ac2-cee0f4302ae0");
            if (newNamespaceURI == null) {
                writeline("/home/ubuntu/results/coverage/Digester/Digester_3_10.coverage", "225611d0-9211-448b-8f3a-a60b87045b18");
                log.debug("addRuleSet() with no namespace URI");
            } else {
                writeline("/home/ubuntu/results/coverage/Digester/Digester_3_10.coverage", "f20a72f6-5606-4126-8e49-314171a670f2");
                log.debug("addRuleSet() with namespace URI " + newNamespaceURI);
            }
        }
        writeline("/home/ubuntu/results/coverage/Digester/Digester_3_10.coverage", "616a7436-11da-4e84-8d79-5d5e09496085");
        setRuleNamespaceURI(newNamespaceURI);
        writeline("/home/ubuntu/results/coverage/Digester/Digester_3_10.coverage", "fd9261a0-aaaa-4908-a429-5848bbcf6f2c");
        ruleSet.addRuleInstances(this);
        writeline("/home/ubuntu/results/coverage/Digester/Digester_3_10.coverage", "556cd813-37be-4d8d-b4f3-07769eb71cc4");
        setRuleNamespaceURI(oldNamespaceURI);
    }

    /**
     * Add a "bean property setter" rule for the specified parameters.
     *
     * @param pattern Element matching pattern
     * @see BeanPropertySetterRule
     */
    public void addBeanPropertySetter(String pattern) {
        writeline("/home/ubuntu/results/coverage/Digester/Digester_3_10.coverage", "90304726-4797-4b20-9876-733b1acba724");
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
        writeline("/home/ubuntu/results/coverage/Digester/Digester_3_10.coverage", "ea331321-bfc5-4d18-8a76-9e1a906d08d2");
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
        writeline("/home/ubuntu/results/coverage/Digester/Digester_3_10.coverage", "cdee4ab7-e98c-4abf-82c6-c25782af0bf5");
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
        writeline("/home/ubuntu/results/coverage/Digester/Digester_3_10.coverage", "26f8cd1b-5af1-47ee-bf4f-ec470082e54a");
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
        writeline("/home/ubuntu/results/coverage/Digester/Digester_3_10.coverage", "a6a940cb-619b-4c78-a0e1-3fa7b9bda048");
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
        writeline("/home/ubuntu/results/coverage/Digester/Digester_3_10.coverage", "7729e3ab-182c-4e5b-8781-a0981eafabd7");
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
        writeline("/home/ubuntu/results/coverage/Digester/Digester_3_10.coverage", "15f676a6-3cf0-4420-b262-e562ea881b0e");
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
        writeline("/home/ubuntu/results/coverage/Digester/Digester_3_10.coverage", "a6c2813c-b201-4f76-acae-daf98989922a");
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
        writeline("/home/ubuntu/results/coverage/Digester/Digester_3_10.coverage", "d556ea41-210f-4f5a-968c-e05ac62f106d");
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
        writeline("/home/ubuntu/results/coverage/Digester/Digester_3_10.coverage", "84924585-5fc0-4e80-af0d-c995370f33f4");
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
        writeline("/home/ubuntu/results/coverage/Digester/Digester_3_10.coverage", "0a5e5dbf-c77d-4825-9258-ed7afd2b5756");
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
        writeline("/home/ubuntu/results/coverage/Digester/Digester_3_10.coverage", "254eea84-1542-4365-b247-e61feef64898");
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
        writeline("/home/ubuntu/results/coverage/Digester/Digester_3_10.coverage", "93707a58-b256-4931-a0b9-4fd4b4be6d27");
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
        writeline("/home/ubuntu/results/coverage/Digester/Digester_3_10.coverage", "6735cd03-d21d-4b70-873a-d5131e4228ce");
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
        writeline("/home/ubuntu/results/coverage/Digester/Digester_3_10.coverage", "c27c1c08-d57d-4351-bb8a-65b34641af9b");
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
        writeline("/home/ubuntu/results/coverage/Digester/Digester_3_10.coverage", "10e70a54-ceaa-49c9-9614-fbcd3664ccdb");
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
        writeline("/home/ubuntu/results/coverage/Digester/Digester_3_10.coverage", "b6e45744-ae37-4660-b622-a95634aad6bc");
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
        writeline("/home/ubuntu/results/coverage/Digester/Digester_3_10.coverage", "b36bdd98-a74e-4297-8f8b-e36d1f10c16a");
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
        writeline("/home/ubuntu/results/coverage/Digester/Digester_3_10.coverage", "e4399536-114f-418f-b7fe-a3d3babd8e97");
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
        writeline("/home/ubuntu/results/coverage/Digester/Digester_3_10.coverage", "1016bf43-fa6e-4809-8249-f7bbb131acd6");
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
        writeline("/home/ubuntu/results/coverage/Digester/Digester_3_10.coverage", "dff77537-9174-483a-871e-04ea4219a80a");
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
        writeline("/home/ubuntu/results/coverage/Digester/Digester_3_10.coverage", "1f0ae7be-0cf8-4fde-bc40-ef6216597113");
        creationFactory.setDigester(this);
        writeline("/home/ubuntu/results/coverage/Digester/Digester_3_10.coverage", "df16793a-c4a0-4dfc-b742-0f464f3cfb0f");
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
        writeline("/home/ubuntu/results/coverage/Digester/Digester_3_10.coverage", "765960b7-7c07-4a7d-af05-f3ee54af9eba");
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
        writeline("/home/ubuntu/results/coverage/Digester/Digester_3_10.coverage", "556ac0e9-ca29-439f-ba68-f889dadd4d17");
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
        writeline("/home/ubuntu/results/coverage/Digester/Digester_3_10.coverage", "deff4a03-056e-48eb-8e9c-03ca3bf3ff11");
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
        writeline("/home/ubuntu/results/coverage/Digester/Digester_3_10.coverage", "d9fbf381-64e4-4d82-87d0-7fce0a40bc3f");
        addRule(pattern, new ObjectCreateRule(attributeName, clazz));
    }

    /**
     * Adds an {@link SetNestedPropertiesRule}.
     *
     * @param pattern register the rule with this pattern
     * @since 1.6
     */
    public void addSetNestedProperties(String pattern) {
        writeline("/home/ubuntu/results/coverage/Digester/Digester_3_10.coverage", "538cec9f-7625-4380-a571-e7d957aa6afb");
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
        writeline("/home/ubuntu/results/coverage/Digester/Digester_3_10.coverage", "4211ea81-a02c-46de-a48f-ddfb644c9db3");
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
        writeline("/home/ubuntu/results/coverage/Digester/Digester_3_10.coverage", "bf98a6b9-4536-4bf5-9a4e-37992b36102d");
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
        writeline("/home/ubuntu/results/coverage/Digester/Digester_3_10.coverage", "effc1e18-c195-4741-ae4c-5ecce7c97bb6");
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
        writeline("/home/ubuntu/results/coverage/Digester/Digester_3_10.coverage", "5cb34366-205c-4458-a334-f3f42a7f93db");
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
        writeline("/home/ubuntu/results/coverage/Digester/Digester_3_10.coverage", "1b349183-8518-4eca-8990-071535e9e0e0");
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
        writeline("/home/ubuntu/results/coverage/Digester/Digester_3_10.coverage", "60324212-dfcf-449b-9144-85ed88baa5bc");
        addRule(pattern, new SetRootRule(methodName, paramType));
    }

    /**
     * Add a "set properties" rule for the specified parameters.
     *
     * @param pattern Element matching pattern
     * @see SetPropertiesRule
     */
    public void addSetProperties(String pattern) {
        writeline("/home/ubuntu/results/coverage/Digester/Digester_3_10.coverage", "e18a74f2-609c-4030-95ea-d79419dd411b");
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
        writeline("/home/ubuntu/results/coverage/Digester/Digester_3_10.coverage", "9697fe38-ee04-4f3e-97d1-ace51be43540");
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
        writeline("/home/ubuntu/results/coverage/Digester/Digester_3_10.coverage", "50a01e0c-5968-4481-a79e-2da77f5c1190");
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
        writeline("/home/ubuntu/results/coverage/Digester/Digester_3_10.coverage", "091a63de-d775-4bb1-a90f-f58eae4e110d");
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
        writeline("/home/ubuntu/results/coverage/Digester/Digester_3_10.coverage", "ddd830fe-e419-4f9a-a34e-acb417619ad0");
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
        writeline("/home/ubuntu/results/coverage/Digester/Digester_3_10.coverage", "ba1558c6-13b1-4139-802e-4b5e1cbf9fe1");
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
        writeline("/home/ubuntu/results/coverage/Digester/Digester_3_10.coverage", "66c718d8-4146-4347-93ad-77d0e9a6ffda");
        match = "";
        writeline("/home/ubuntu/results/coverage/Digester/Digester_3_10.coverage", "cccbbe6b-cc07-4031-81ce-8fb778ae0019");
        bodyTexts.clear();
        writeline("/home/ubuntu/results/coverage/Digester/Digester_3_10.coverage", "436f3e32-0ff0-444d-ab5a-b5af78d2b5c3");
        params.clear();
        writeline("/home/ubuntu/results/coverage/Digester/Digester_3_10.coverage", "52aa1201-3f60-44ee-bc20-b21541081fa5");
        publicId = null;
        writeline("/home/ubuntu/results/coverage/Digester/Digester_3_10.coverage", "5b9ed95d-56f2-4e1c-ad14-254b2e9d014a");
        stack.clear();
        writeline("/home/ubuntu/results/coverage/Digester/Digester_3_10.coverage", "f21c92bc-f34e-4c04-aa7f-965a5befacde");
        stacksByName.clear();
        writeline("/home/ubuntu/results/coverage/Digester/Digester_3_10.coverage", "e9a8e3ca-3798-4c22-a209-d64e4088b17a");
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
        writeline("/home/ubuntu/results/coverage/Digester/Digester_3_10.coverage", "4b72a16c-fc06-4a04-9ce3-e4388aeda187");
        try {
            writeline("/home/ubuntu/results/coverage/Digester/Digester_3_10.coverage", "b325e60e-b4ca-4ace-800d-2f3c2950ac97");
            return this.<T>npeSafeCast(stack.peek());
        } catch (EmptyStackException e) {
            writeline("/home/ubuntu/results/coverage/Digester/Digester_3_10.coverage", "49893526-c6f6-4ba2-88f7-dac3d6dd6b76");
            log.warn("Empty stack (returning null)");
            writeline("/home/ubuntu/results/coverage/Digester/Digester_3_10.coverage", "e1138b92-b515-473d-ad34-89ce5b2cd367");
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
        writeline("/home/ubuntu/results/coverage/Digester/Digester_3_10.coverage", "ccabae0b-8365-4995-929e-687d19c56c0f");
        int index = (stack.size() - 1) - n;
        writeline("/home/ubuntu/results/coverage/Digester/Digester_3_10.coverage", "1dd712a4-9c57-4905-9587-c79df580dc9d");
        if (index < 0) {
            writeline("/home/ubuntu/results/coverage/Digester/Digester_3_10.coverage", "6ead03c5-a11d-48ab-b779-9869b4542c79");
            log.warn("Empty stack (returning null)");
            writeline("/home/ubuntu/results/coverage/Digester/Digester_3_10.coverage", "31320793-3edd-4e79-8475-09e7a8696f20");
            return (null);
        }
        writeline("/home/ubuntu/results/coverage/Digester/Digester_3_10.coverage", "4fb1693e-fb54-417c-8c2d-da0bbd32d85c");
        try {
            writeline("/home/ubuntu/results/coverage/Digester/Digester_3_10.coverage", "df1c1002-6bad-46e7-95d9-895267ca493f");
            return this.<T>npeSafeCast(stack.get(index));
        } catch (EmptyStackException e) {
            writeline("/home/ubuntu/results/coverage/Digester/Digester_3_10.coverage", "0cf10cd0-3f33-4b3e-a163-3b5758e23f26");
            log.warn("Empty stack (returning null)");
            writeline("/home/ubuntu/results/coverage/Digester/Digester_3_10.coverage", "bd55235e-9cad-4eae-beff-9aabfdf512bd");
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
        writeline("/home/ubuntu/results/coverage/Digester/Digester_3_10.coverage", "a32f0acd-29de-42df-b996-a9a6fbb03218");
        try {
            writeline("/home/ubuntu/results/coverage/Digester/Digester_3_10.coverage", "425082b9-9c54-4ad6-94cd-80cf3727c502");
            T popped = this.<T>npeSafeCast(stack.pop());
            writeline("/home/ubuntu/results/coverage/Digester/Digester_3_10.coverage", "7419da68-02f5-4395-b393-a00c3502e13a");
            if (stackAction != null) {
                writeline("/home/ubuntu/results/coverage/Digester/Digester_3_10.coverage", "cdb769bf-f8bb-4076-8022-a163122dfad8");
                popped = stackAction.onPop(this, null, popped);
            }
            writeline("/home/ubuntu/results/coverage/Digester/Digester_3_10.coverage", "388ec4a0-785e-40ca-8f2c-6ee936e80020");
            return popped;
        } catch (EmptyStackException e) {
            writeline("/home/ubuntu/results/coverage/Digester/Digester_3_10.coverage", "4022c855-7a93-4695-8fe3-d041406b7235");
            log.warn("Empty stack (returning null)");
            writeline("/home/ubuntu/results/coverage/Digester/Digester_3_10.coverage", "bdc91ee7-fad6-4876-a0ef-2e09f9198dbc");
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
        writeline("/home/ubuntu/results/coverage/Digester/Digester_3_10.coverage", "8983dc56-fcc6-49c3-8119-aa4f82d9ff1d");
        if (stackAction != null) {
            writeline("/home/ubuntu/results/coverage/Digester/Digester_3_10.coverage", "ee72cdab-aab8-47ab-90b1-1d90c218fc08");
            object = stackAction.onPush(this, null, object);
        }
        writeline("/home/ubuntu/results/coverage/Digester/Digester_3_10.coverage", "69e9ad16-63ad-4c0a-9828-e0722b9f01ec");
        if (stack.size() == 0) {
            writeline("/home/ubuntu/results/coverage/Digester/Digester_3_10.coverage", "cbdf46b6-59fd-4191-b608-10e96e024d23");
            root = object;
        }
        writeline("/home/ubuntu/results/coverage/Digester/Digester_3_10.coverage", "85f8b4f6-3184-40bd-844b-7dc3acf50a94");
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
        writeline("/home/ubuntu/results/coverage/Digester/Digester_3_10.coverage", "a7f97f2e-6d3f-4d26-b66e-5e4a089ba06c");
        if (stackAction != null) {
            writeline("/home/ubuntu/results/coverage/Digester/Digester_3_10.coverage", "5918886d-eb17-42be-93b3-6e9ea5a853f0");
            value = stackAction.onPush(this, stackName, value);
        }
        writeline("/home/ubuntu/results/coverage/Digester/Digester_3_10.coverage", "46d8e1a7-7161-4001-b272-a55e43b50362");
        Stack<Object> namedStack = stacksByName.get(stackName);
        writeline("/home/ubuntu/results/coverage/Digester/Digester_3_10.coverage", "855ea169-33c4-4193-a0ac-89b2ab8fa654");
        if (namedStack == null) {
            writeline("/home/ubuntu/results/coverage/Digester/Digester_3_10.coverage", "077ba1d9-cf61-44b7-beb6-a0430ddaba8a");
            namedStack = new Stack<Object>();
            writeline("/home/ubuntu/results/coverage/Digester/Digester_3_10.coverage", "8c6710da-af54-42d1-a8a1-bfa61f86e66f");
            stacksByName.put(stackName, namedStack);
        }
        writeline("/home/ubuntu/results/coverage/Digester/Digester_3_10.coverage", "a86022d5-060b-4bfd-9924-74c4d071fc22");
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
        writeline("/home/ubuntu/results/coverage/Digester/Digester_3_10.coverage", "3535065c-a03f-4550-a4fd-3f71f8011968");
        T result = null;
        writeline("/home/ubuntu/results/coverage/Digester/Digester_3_10.coverage", "46ac1fd6-fe6a-4dbe-88fd-804bab752536");
        Stack<Object> namedStack = stacksByName.get(stackName);
        writeline("/home/ubuntu/results/coverage/Digester/Digester_3_10.coverage", "1ca45f2b-4ad3-402b-84bf-da9420632234");
        if (namedStack == null) {
            writeline("/home/ubuntu/results/coverage/Digester/Digester_3_10.coverage", "041f7855-7b86-4efd-bf17-6ab8ccfe744d");
            if (log.isDebugEnabled()) {
                writeline("/home/ubuntu/results/coverage/Digester/Digester_3_10.coverage", "af669bc4-3624-409c-8cc3-f6255543f7a8");
                log.debug("Stack '" + stackName + "' is empty");
            }
            writeline("/home/ubuntu/results/coverage/Digester/Digester_3_10.coverage", "e5cd666d-f847-4be3-af8a-c6ec1a4255c4");
            throw new EmptyStackException();
        }
        writeline("/home/ubuntu/results/coverage/Digester/Digester_3_10.coverage", "43e26aa2-22de-4fec-8177-3fd48386d6af");
        result = this.<T>npeSafeCast(namedStack.pop());
        writeline("/home/ubuntu/results/coverage/Digester/Digester_3_10.coverage", "31ae8e3e-a127-4b30-bf80-4fb51924911b");
        if (stackAction != null) {
            writeline("/home/ubuntu/results/coverage/Digester/Digester_3_10.coverage", "aab871d7-837e-4b74-ba76-a846be3f41b0");
            result = stackAction.onPop(this, stackName, result);
        }
        writeline("/home/ubuntu/results/coverage/Digester/Digester_3_10.coverage", "0e4c2082-b7bc-42ef-8e95-6454e2867e35");
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
        writeline("/home/ubuntu/results/coverage/Digester/Digester_3_10.coverage", "e0b0c602-57ca-4106-b0d9-7ba6ef26cd76");
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
        writeline("/home/ubuntu/results/coverage/Digester/Digester_3_10.coverage", "7086bf42-ae64-4855-a8a3-d645acd2a4ec");
        T result = null;
        writeline("/home/ubuntu/results/coverage/Digester/Digester_3_10.coverage", "87f0ca78-5165-4693-bf9f-304e225d0379");
        Stack<Object> namedStack = stacksByName.get(stackName);
        writeline("/home/ubuntu/results/coverage/Digester/Digester_3_10.coverage", "19e769ff-1332-49bf-9616-f8b0191ff671");
        if (namedStack == null) {
            writeline("/home/ubuntu/results/coverage/Digester/Digester_3_10.coverage", "4ba84bd5-2a9d-48f6-a8b3-e28a28a70e9b");
            if (log.isDebugEnabled()) {
                writeline("/home/ubuntu/results/coverage/Digester/Digester_3_10.coverage", "0ec581fa-07f9-4fa5-adc1-d5530a72b5c7");
                log.debug("Stack '" + stackName + "' is empty");
            }
            writeline("/home/ubuntu/results/coverage/Digester/Digester_3_10.coverage", "38a4d0a7-23b8-4cf7-9123-061904d5ac22");
            throw new EmptyStackException();
        }
        writeline("/home/ubuntu/results/coverage/Digester/Digester_3_10.coverage", "ba8a759e-04dd-467a-b5cc-d40aa98b69c8");
        int index = (namedStack.size() - 1) - n;
        writeline("/home/ubuntu/results/coverage/Digester/Digester_3_10.coverage", "7dc2fb4a-19b3-40d3-b2b9-6a4bc9f2b656");
        if (index < 0) {
            writeline("/home/ubuntu/results/coverage/Digester/Digester_3_10.coverage", "0b14f22d-5a24-4400-a416-ca2295e0570c");
            throw new EmptyStackException();
        }
        writeline("/home/ubuntu/results/coverage/Digester/Digester_3_10.coverage", "5454cb78-aceb-41eb-b3d7-7b9a03454572");
        result = this.<T>npeSafeCast(namedStack.get(index));
        writeline("/home/ubuntu/results/coverage/Digester/Digester_3_10.coverage", "b615070e-4b5c-467b-becb-4f1e5a8bcdd1");
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
        writeline("/home/ubuntu/results/coverage/Digester/Digester_3_10.coverage", "26880c2b-5c8d-4a94-b5ec-d66e4b64797f");
        boolean result = true;
        writeline("/home/ubuntu/results/coverage/Digester/Digester_3_10.coverage", "bf922850-fb30-4d6a-83a5-5aaea88dc846");
        Stack<Object> namedStack = stacksByName.get(stackName);
        writeline("/home/ubuntu/results/coverage/Digester/Digester_3_10.coverage", "ae0e561a-8cb9-4e8c-ab31-027992c7c250");
        if (namedStack != null) {
            writeline("/home/ubuntu/results/coverage/Digester/Digester_3_10.coverage", "9ee9f62b-04ce-4d27-9e67-c31e07a8f4e3");
            result = namedStack.isEmpty();
        }
        writeline("/home/ubuntu/results/coverage/Digester/Digester_3_10.coverage", "ae2f91a6-5568-4c46-b796-40d688c8157e");
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
        writeline("/home/ubuntu/results/coverage/Digester/Digester_3_10.coverage", "4261e567-da8f-4a2f-a1e7-a8431253383c");
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
        writeline("/home/ubuntu/results/coverage/Digester/Digester_3_10.coverage", "6dad59d0-d3b9-4127-8cfb-fbe7f0b235f5");
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
        writeline("/home/ubuntu/results/coverage/Digester/Digester_3_10.coverage", "46314fbd-02e9-428e-ad24-acc4c46cb8d1");
        for (InputSource source : inputSources) {
            writeline("/home/ubuntu/results/coverage/Digester/Digester_3_10.coverage", "26bfb7ea-962c-4bc9-bc16-988d2b0d8479");
            try {
                writeline("/home/ubuntu/results/coverage/Digester/Digester_3_10.coverage", "f31e1bc7-969c-45ad-bf42-939a13e2d24c");
                source.getByteStream().close();
            } catch (IOException e) {
                writeline("/home/ubuntu/results/coverage/Digester/Digester_3_10.coverage", "da0f3a47-d6dc-4d06-b87d-76fae1ee0cc7");
                if (log.isWarnEnabled()) {
                    writeline("/home/ubuntu/results/coverage/Digester/Digester_3_10.coverage", "a75557b6-47a2-49b2-a49a-afc264839f54");
                    log.warn(format("An error occurred while closing resource %s (%s)", source.getPublicId(), source.getSystemId()), e);
                }
            }
        }
        writeline("/home/ubuntu/results/coverage/Digester/Digester_3_10.coverage", "9a6a3ca2-83e3-4e2a-8831-02bf1139f875");
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
        writeline("/home/ubuntu/results/coverage/Digester/Digester_3_10.coverage", "a9dfa830-04e2-4a91-a12b-b53fe5f989b4");
        if (configured) {
            writeline("/home/ubuntu/results/coverage/Digester/Digester_3_10.coverage", "a127dafb-3ab9-4ece-9445-9575f2cf1149");
            return;
        }
        writeline("/home/ubuntu/results/coverage/Digester/Digester_3_10.coverage", "0630cade-22d9-42ed-884c-19432a625691");
        initialize();
        writeline("/home/ubuntu/results/coverage/Digester/Digester_3_10.coverage", "47662f6a-7184-4f46-893c-69f1debdd7c5");
        configured = true;
    }

    /**
     * Checks the Digester instance has been configured.
     *
     * @return true, if the Digester instance has been configured, false otherwise
     * @since 3.0
     */
    public boolean isConfigured() {
        writeline("/home/ubuntu/results/coverage/Digester/Digester_3_10.coverage", "c84b1169-17a0-42d9-8ed4-bbeade6aa6f7");
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
        writeline("/home/ubuntu/results/coverage/Digester/Digester_3_10.coverage", "d9962177-ab8d-4857-8e07-266570f0b5f8");
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
        writeline("/home/ubuntu/results/coverage/Digester/Digester_3_10.coverage", "6c8a20b4-47bd-4d10-9038-cc1314af9a6d");
        try {
            writeline("/home/ubuntu/results/coverage/Digester/Digester_3_10.coverage", "db8eb8a0-0b33-4046-9741-ad03e9a58cc3");
            return (params.peek());
        } catch (EmptyStackException e) {
            writeline("/home/ubuntu/results/coverage/Digester/Digester_3_10.coverage", "d4cc333f-c394-4d11-bfa9-ad96c3c01388");
            log.warn("Empty stack (returning null)");
            writeline("/home/ubuntu/results/coverage/Digester/Digester_3_10.coverage", "266721bb-465b-4a5a-83f3-05c7174f4bf4");
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
        writeline("/home/ubuntu/results/coverage/Digester/Digester_3_10.coverage", "12893415-93ee-4e71-b533-283a0ec1851b");
        int index = (params.size() - 1) - n;
        writeline("/home/ubuntu/results/coverage/Digester/Digester_3_10.coverage", "ea5ed2cd-aa8a-4f19-91f6-c79f25b23919");
        if (index < 0) {
            writeline("/home/ubuntu/results/coverage/Digester/Digester_3_10.coverage", "920dc756-c2a7-49b7-be23-d2620247c363");
            log.warn("Empty stack (returning null)");
            writeline("/home/ubuntu/results/coverage/Digester/Digester_3_10.coverage", "5dca52b8-9219-4a23-8d19-7ba2c36a0f07");
            return (null);
        }
        writeline("/home/ubuntu/results/coverage/Digester/Digester_3_10.coverage", "9576861a-3b35-4b49-82d7-70766850431b");
        try {
            writeline("/home/ubuntu/results/coverage/Digester/Digester_3_10.coverage", "136981e2-38c8-4084-b106-d62e5d49923c");
            return (params.get(index));
        } catch (EmptyStackException e) {
            writeline("/home/ubuntu/results/coverage/Digester/Digester_3_10.coverage", "b98b2638-543b-4c6f-b060-3235193d6cb6");
            log.warn("Empty stack (returning null)");
            writeline("/home/ubuntu/results/coverage/Digester/Digester_3_10.coverage", "db2763c9-aeb3-4d95-9ec9-ec0381ced9da");
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
        writeline("/home/ubuntu/results/coverage/Digester/Digester_3_10.coverage", "90796145-eec6-463d-987d-dd42cac665ca");
        try {
            writeline("/home/ubuntu/results/coverage/Digester/Digester_3_10.coverage", "0606d51c-d3a2-4ef8-a6a9-f4790b085059");
            if (log.isTraceEnabled()) {
                writeline("/home/ubuntu/results/coverage/Digester/Digester_3_10.coverage", "743cd4da-49fa-4b5c-89c3-ba4afd10d4ac");
                log.trace("Popping params");
            }
            writeline("/home/ubuntu/results/coverage/Digester/Digester_3_10.coverage", "7b752b68-7404-44e0-ba6e-2fa3163e1fb9");
            return (params.pop());
        } catch (EmptyStackException e) {
            writeline("/home/ubuntu/results/coverage/Digester/Digester_3_10.coverage", "d9beffb3-1ed3-4114-a018-f1fe19b04bde");
            log.warn("Empty stack (returning null)");
            writeline("/home/ubuntu/results/coverage/Digester/Digester_3_10.coverage", "bc9f377c-5001-48c7-912e-336c0fa3a004");
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
        writeline("/home/ubuntu/results/coverage/Digester/Digester_3_10.coverage", "e580fae1-d65e-474c-ab04-825d117a2b02");
        if (log.isTraceEnabled()) {
            writeline("/home/ubuntu/results/coverage/Digester/Digester_3_10.coverage", "41f4417a-8912-43ac-9a76-ed3eb6451083");
            log.trace("Pushing params");
        }
        writeline("/home/ubuntu/results/coverage/Digester/Digester_3_10.coverage", "36935428-8f0a-486a-9eb8-ccccfc89ac16");
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
        writeline("/home/ubuntu/results/coverage/Digester/Digester_3_10.coverage", "c614f514-a269-456c-8159-dfe0089dd83f");
        if ((e != null) && (e instanceof InvocationTargetException)) {
            writeline("/home/ubuntu/results/coverage/Digester/Digester_3_10.coverage", "48ad9cd0-52b1-47eb-841c-c7bcce218fe8");
            Throwable t = ((InvocationTargetException) e).getTargetException();
            writeline("/home/ubuntu/results/coverage/Digester/Digester_3_10.coverage", "64dd59ea-b0d7-4de2-b201-6a16a19e2be0");
            if ((t != null) && (t instanceof Exception)) {
                writeline("/home/ubuntu/results/coverage/Digester/Digester_3_10.coverage", "4154f752-7026-432d-9a13-ac6735256d19");
                e = (Exception) t;
            }
        }
        writeline("/home/ubuntu/results/coverage/Digester/Digester_3_10.coverage", "0632e461-2201-4d2a-b8d1-eb7db8535d1d");
        if (locator != null) {
            writeline("/home/ubuntu/results/coverage/Digester/Digester_3_10.coverage", "82d533db-1872-42a3-9c70-9143b9288bf3");
            String error = "Error at line " + locator.getLineNumber() + " char " + locator.getColumnNumber() + ": " + message;
            writeline("/home/ubuntu/results/coverage/Digester/Digester_3_10.coverage", "a1e048f5-53a7-4c4a-b686-e292c33dcacc");
            if (e != null) {
                writeline("/home/ubuntu/results/coverage/Digester/Digester_3_10.coverage", "378d7cd0-a1f5-4259-9f1f-0ce7e3356633");
                return new SAXParseException(error, locator, e);
            }
            writeline("/home/ubuntu/results/coverage/Digester/Digester_3_10.coverage", "dfd95a7c-fb0a-4d85-86a3-aef31c0ce030");
            return new SAXParseException(error, locator);
        }
        writeline("/home/ubuntu/results/coverage/Digester/Digester_3_10.coverage", "070202fe-a669-4b74-88f5-24490ae01ba1");
        log.error("No Locator!");
        writeline("/home/ubuntu/results/coverage/Digester/Digester_3_10.coverage", "5d698712-6559-4e2f-ba7f-bfd08e83697c");
        if (e != null) {
            writeline("/home/ubuntu/results/coverage/Digester/Digester_3_10.coverage", "9198306f-ef61-4d08-8509-d18a64e961fb");
            return new SAXException(message, e);
        }
        writeline("/home/ubuntu/results/coverage/Digester/Digester_3_10.coverage", "03564730-c379-46a6-91be-14ac5d273e42");
        return new SAXException(message);
    }

    /**
     * Create a SAX exception which also understands about the location in the digester file where the exception occurs
     *
     * @param e the exception cause
     * @return the new SAX exception
     */
    public SAXException createSAXException(Exception e) {
        writeline("/home/ubuntu/results/coverage/Digester/Digester_3_10.coverage", "497a5d35-9a4b-4334-adfe-40ea5545c87e");
        if (e instanceof InvocationTargetException) {
            writeline("/home/ubuntu/results/coverage/Digester/Digester_3_10.coverage", "cef7775f-7ea1-48fe-9741-55387f8620d6");
            Throwable t = ((InvocationTargetException) e).getTargetException();
            writeline("/home/ubuntu/results/coverage/Digester/Digester_3_10.coverage", "be66bc4c-527b-4b68-b16c-3bc18a1c6643");
            if ((t != null) && (t instanceof Exception)) {
                writeline("/home/ubuntu/results/coverage/Digester/Digester_3_10.coverage", "9e932bc3-5ff8-49b6-9039-28a8f302cd7d");
                e = (Exception) t;
            }
        }
        writeline("/home/ubuntu/results/coverage/Digester/Digester_3_10.coverage", "76f8da28-d58c-4235-acf0-ce8c718e0b19");
        return createSAXException(e.getMessage(), e);
    }

    /**
     * Create a SAX exception which also understands about the location in the digester file where the exception occurs
     *
     * @param message the custom SAX exception message
     * @return the new SAX exception
     */
    public SAXException createSAXException(String message) {
        writeline("/home/ubuntu/results/coverage/Digester/Digester_3_10.coverage", "188b47c9-475e-440c-8108-00d406e79252");
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
        writeline("/home/ubuntu/results/coverage/Digester/Digester_3_10.coverage", "b5d9e1e3-e57c-45ff-8366-b8922eb50fbc");
        if (obj == null) {
            writeline("/home/ubuntu/results/coverage/Digester/Digester_3_10.coverage", "62a1a034-5fe0-4460-bbfd-f090dbb7be97");
            return null;
        }
        writeline("/home/ubuntu/results/coverage/Digester/Digester_3_10.coverage", "ad59c7f7-dfef-4f25-a430-6521788a2e3a");
        @SuppressWarnings("unchecked") T result = (T) obj;
        writeline("/home/ubuntu/results/coverage/Digester/Digester_3_10.coverage", "e9242486-2b71-4807-8f13-a472aa43c231");
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
