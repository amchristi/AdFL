package org.apache.commons.digester3;

import javax.xml.parsers.DocumentBuilder;
import javax.xml.parsers.DocumentBuilderFactory;
import javax.xml.parsers.ParserConfigurationException;
import org.w3c.dom.Attr;
import org.w3c.dom.DOMException;
import org.w3c.dom.Document;
import org.w3c.dom.Element;
import org.w3c.dom.Node;
import org.xml.sax.Attributes;
import org.xml.sax.ContentHandler;
import org.xml.sax.SAXException;
import org.xml.sax.helpers.DefaultHandler;
import java.io.*;

/**
 * A rule implementation that creates a DOM {@link org.w3c.dom.Node Node} containing the XML at the element that matched
 * the rule. Two concrete types of nodes can be created by this rule:
 * <ul>
 * <li>the default is to create an {@link org.w3c.dom.Element Element} node. The created element will correspond to the
 * element that matched the rule, containing all XML content underneath that element.</li>
 * <li>alternatively, this rule can create nodes of type {@link org.w3c.dom.DocumentFragment DocumentFragment}, which
 * will contain only the XML content under the element the rule was trigged on.</li>
 * </ul>
 * The created node will be normalized, meaning it will not contain text nodes that only contain white space characters.
 * <p>
 * The created <code>Node</code> will be pushed on Digester's object stack when done. To use it in the context of
 * another DOM {@link org.w3c.dom.Document Document}, it must be imported first, using the Document method
 * {@link org.w3c.dom.Document#importNode(org.w3c.dom.Node, boolean) importNode()}.
 * </p>
 * <p>
 * <strong>Important Note:</strong> This is implemented by replacing the SAX {@link org.xml.sax.ContentHandler
 * ContentHandler} in the parser used by Digester, and resetting it when the matched element is closed. As a side
 * effect, rules that would match XML nodes under the element that matches a <code>NodeCreateRule</code> will never be
 * triggered by Digester, which usually is the behavior one would expect.
 * </p>
 * <p>
 * <strong>Note</strong> that the current implementation does not set the namespace prefixes in the exported nodes. The
 * (usually more important) namespace URIs are set, of course.
 * </p>
 * 
 * @since Digester 1.4
 */
public class NodeCreateRule extends Rule {

    /**
     * The SAX content handler that does all the actual work of assembling the DOM node tree from the SAX events.
     */
    private class NodeBuilder extends DefaultHandler {

        /**
         * Constructor.
         * <p>
         * Stores the content handler currently used by Digester so it can be reset when done, and initializes the DOM
         * objects needed to build the node.
         * </p>
         * 
         * @param doc the document to use to create nodes
         * @param root the root node
         * @throws ParserConfigurationException if the DocumentBuilderFactory could not be instantiated
         * @throws SAXException if the XMLReader could not be instantiated by Digester (should not happen)
         */
        public NodeBuilder(Document doc, Node root) throws ParserConfigurationException, SAXException {
            this.doc = doc;
            this.root = root;
            this.top = root;
            oldContentHandler = getDigester().getCustomContentHandler();
        }

        /**
         * The content handler used by Digester before it was set to this content handler.
         */
        protected ContentHandler oldContentHandler = null;

        /**
         * Depth of the current node, relative to the element where the content handler was put into action.
         */
        protected int depth = 0;

        /**
         * A DOM Document used to create the various Node instances.
         */
        protected Document doc = null;

        /**
         * The DOM node that will be pushed on Digester's stack.
         */
        protected Node root = null;

        /**
         * The current top DOM mode.
         */
        protected Node top = null;

        /**
         * The text content of the current top DOM node.
         */
        protected StringBuilder topText = new StringBuilder();

        /**
         * Appends a {@link org.w3c.dom.Text Text} node to the current node if the content reported by the parser is not
         * purely whitespace.
         */
        private void addTextIfPresent() throws SAXException {
            if (topText.length() > 0) {
                String str = topText.toString();
                topText.setLength(0);
                if (str.trim().length() > 0) {
                    try {
                        top.appendChild(doc.createTextNode(str));
                    } catch (DOMException e) {
                        throw new SAXException(e.getMessage());
                    }
                }
            }
        }

        /**
         * Handle notification about text embedded within the current node.
         * <p>
         * An xml parser calls this when text is found. We need to ensure that this text gets attached to the new Node
         * we are creating - except in the case where the only text in the node is whitespace.
         * <p>
         * There is a catch, however. According to the sax specification, a parser does not need to pass all of the text
         * content of a node in one go; it can make multiple calls passing part of the data on each call. In particular,
         * when the body of an element includes xml entity-references, at least some parsers make a separate call to
         * this method to pass just the entity content.
         * <p>
         * In this method, we therefore just append the provided text to a "current text" buffer. When the element end
         * is found, or a child element is found then we can check whether we have all-whitespace. See method
         * addTextIfPresent.
         * 
         * @param ch the characters from the XML document
         * @param start the start position in the array
         * @param length the number of characters to read from the array
         * @throws SAXException if the DOM implementation throws an exception
         */
        @Override
        public void characters(char[] ch, int start, int length) throws SAXException {
            topText.append(ch, start, length);
        }

        /**
         * Checks whether control needs to be returned to Digester.
         * 
         * @param namespaceURI the namespace URI
         * @param localName the local name
         * @param qName the qualified (prefixed) name
         * @throws SAXException if the DOM implementation throws an exception
         */
        @Override
        public void endElement(String namespaceURI, String localName, String qName) throws SAXException {
            addTextIfPresent();
            try {
                if (depth == 0) {
                    getDigester().setCustomContentHandler(oldContentHandler);
                    getDigester().push(root);
                    getDigester().endElement(namespaceURI, localName, qName);
                }
                top = top.getParentNode();
                depth--;
            } catch (DOMException e) {
                throw new SAXException(e.getMessage());
            }
        }

        /**
         * Adds a new {@link org.w3c.dom.ProcessingInstruction ProcessingInstruction} to the current node.
         * 
         * @param target the processing instruction target
         * @param data the processing instruction data, or null if none was supplied
         * @throws SAXException if the DOM implementation throws an exception
         */
        @Override
        public void processingInstruction(String target, String data) throws SAXException {
            try {
                top.appendChild(doc.createProcessingInstruction(target, data));
            } catch (DOMException e) {
                throw new SAXException(e.getMessage());
            }
        }

        /**
         * Adds a new child {@link org.w3c.dom.Element Element} to the current node.
         * 
         * @param namespaceURI the namespace URI
         * @param localName the local name
         * @param qName the qualified (prefixed) name
         * @param atts the list of attributes
         * @throws SAXException if the DOM implementation throws an exception
         */
        @Override
        public void startElement(String namespaceURI, String localName, String qName, Attributes atts) throws SAXException {
            addTextIfPresent();
            try {
                Node previousTop = top;
                if ((localName == null) || (localName.length() == 0)) {
                    top = doc.createElement(qName);
                } else {
                    top = doc.createElementNS(namespaceURI, localName);
                }
                for (int i = 0; i < atts.getLength(); i++) {
                    Attr attr = null;
                    if ((atts.getLocalName(i) == null) || (atts.getLocalName(i).length() == 0)) {
                        attr = doc.createAttribute(atts.getQName(i));
                        attr.setNodeValue(atts.getValue(i));
                        ((Element) top).setAttributeNode(attr);
                    } else {
                        attr = doc.createAttributeNS(atts.getURI(i), atts.getLocalName(i));
                        attr.setNodeValue(atts.getValue(i));
                        ((Element) top).setAttributeNodeNS(attr);
                    }
                }
                previousTop.appendChild(top);
                depth++;
            } catch (DOMException e) {
                throw new SAXException(e.getMessage());
            }
        }
    }

    /**
     * Default constructor. Creates an instance of this rule that will create a DOM {@link org.w3c.dom.Element Element}.
     *
     * @throws ParserConfigurationException if a DocumentBuilder cannot be created which satisfies the
     *         configuration requested.
     * @see DocumentBuilderFactory#newDocumentBuilder()
     */
    public NodeCreateRule() throws ParserConfigurationException {
        this(Node.ELEMENT_NODE);
    }

    /**
     * Constructor. Creates an instance of this rule that will create a DOM {@link org.w3c.dom.Element Element}, but
     * lets you specify the JAXP <code>DocumentBuilder</code> that should be used when constructing the node tree.
     * 
     * @param documentBuilder the JAXP <code>DocumentBuilder</code> to use
     */
    public NodeCreateRule(DocumentBuilder documentBuilder) {
        this(Node.ELEMENT_NODE, documentBuilder);
    }

    /**
     * Constructor. Creates an instance of this rule that will create either a DOM {@link org.w3c.dom.Element Element}
     * or a DOM {@link org.w3c.dom.DocumentFragment DocumentFragment}, depending on the value of the
     * <code>nodeType</code> parameter.
     * 
     * @param nodeType the type of node to create, which can be either {@link org.w3c.dom.Node#ELEMENT_NODE
     *            Node.ELEMENT_NODE} or {@link org.w3c.dom.Node#DOCUMENT_FRAGMENT_NODE Node.DOCUMENT_FRAGMENT_NODE}
     * @throws ParserConfigurationException if a DocumentBuilder cannot be created which satisfies the
     *         configuration requested.
     * @see DocumentBuilderFactory#newDocumentBuilder()
     */
    public NodeCreateRule(int nodeType) throws ParserConfigurationException {
        this(nodeType, DocumentBuilderFactory.newInstance().newDocumentBuilder());
    }

    /**
     * Constructor. Creates an instance of this rule that will create either a DOM {@link org.w3c.dom.Element Element}
     * or a DOM {@link org.w3c.dom.DocumentFragment DocumentFragment}, depending on the value of the
     * <code>nodeType</code> parameter. This constructor lets you specify the JAXP <code>DocumentBuilder</code> that
     * should be used when constructing the node tree.
     * 
     * @param nodeType the type of node to create, which can be either {@link org.w3c.dom.Node#ELEMENT_NODE
     *            Node.ELEMENT_NODE} or {@link org.w3c.dom.Node#DOCUMENT_FRAGMENT_NODE Node.DOCUMENT_FRAGMENT_NODE}
     * @param documentBuilder the JAXP <code>DocumentBuilder</code> to use
     */
    public NodeCreateRule(int nodeType, DocumentBuilder documentBuilder) {
        if (!((nodeType == Node.DOCUMENT_FRAGMENT_NODE) || (nodeType == Node.ELEMENT_NODE))) {
            throw new IllegalArgumentException("Can only create nodes of type DocumentFragment and Element");
        }
        this.nodeType = nodeType;
        this.documentBuilder = documentBuilder;
    }

    /**
     * The JAXP <code>DocumentBuilder</code> to use.
     */
    private DocumentBuilder documentBuilder = null;

    /**
     * The type of the node that should be created. Must be one of the constants defined in {@link org.w3c.dom.Node
     * Node}, but currently only {@link org.w3c.dom.Node#ELEMENT_NODE Node.ELEMENT_NODE} and
     * {@link org.w3c.dom.Node#DOCUMENT_FRAGMENT_NODE Node.DOCUMENT_FRAGMENT_NODE} are allowed values.
     */
    private int nodeType = Node.ELEMENT_NODE;

    /**
     * When this method fires, the digester is told to forward all SAX ContentHandler events to the builder object,
     * resulting in a DOM being built instead of normal digester rule-handling occurring. When the end of the current
     * xml element is encountered, the original content handler is restored (expected to be NULL, allowing normal
     * Digester operations to continue).
     * 
     * @param namespaceURI the namespace URI of the matching element, or an empty string if the parser is not namespace
     *            aware or the element has no namespace
     * @param name the local name if the parser is namespace aware, or just the element name otherwise
     * @param attributes The attribute list of this element
     * @throws Exception indicates a JAXP configuration problem
     */
    @Override
    public void begin(String namespaceURI, String name, Attributes attributes) throws Exception {
        writeline("/home/ubuntu/results/coverage/NodeCreateRule/NodeCreateRule_4_10.coverage", "14253efb-edda-4a48-9f5c-17eb3c626270");
        Document doc = documentBuilder.newDocument();
        writeline("/home/ubuntu/results/coverage/NodeCreateRule/NodeCreateRule_4_10.coverage", "f85bb28d-2f8e-460c-80c2-732e52ebb125");
        NodeBuilder builder = null;
        writeline("/home/ubuntu/results/coverage/NodeCreateRule/NodeCreateRule_4_10.coverage", "98280739-f9c8-40cc-b9a2-09ed8af72acd");
        if (nodeType == Node.ELEMENT_NODE) {
            writeline("/home/ubuntu/results/coverage/NodeCreateRule/NodeCreateRule_4_10.coverage", "4efeb7da-09c1-4c09-ac48-e97c8f10a4ef");
            Element element = null;
            writeline("/home/ubuntu/results/coverage/NodeCreateRule/NodeCreateRule_4_10.coverage", "ed295cdc-a884-4440-afad-09b46bb76e63");
            if (getDigester().getNamespaceAware()) {
                writeline("/home/ubuntu/results/coverage/NodeCreateRule/NodeCreateRule_4_10.coverage", "a0d688e6-40d3-47db-8471-8a4722bc1d43");
                element = doc.createElementNS(namespaceURI, name);
                writeline("/home/ubuntu/results/coverage/NodeCreateRule/NodeCreateRule_4_10.coverage", "231b7b9a-b51d-4072-b2e6-58c07f87edba");
                for (int i = 0; i < attributes.getLength(); i++) {
                    writeline("/home/ubuntu/results/coverage/NodeCreateRule/NodeCreateRule_4_10.coverage", "c36bbd83-3c17-43d8-a6f2-84cf7d5ca26e");
                    element.setAttributeNS(attributes.getURI(i), attributes.getQName(i), attributes.getValue(i));
                }
            } else {
                writeline("/home/ubuntu/results/coverage/NodeCreateRule/NodeCreateRule_4_10.coverage", "7ae0fccf-aeb0-4206-93ef-9486db0ec95c");
                element = doc.createElement(name);
                writeline("/home/ubuntu/results/coverage/NodeCreateRule/NodeCreateRule_4_10.coverage", "314896e4-c36e-4c1a-ba9b-191cf1e24845");
                for (int i = 0; i < attributes.getLength(); i++) {
                    writeline("/home/ubuntu/results/coverage/NodeCreateRule/NodeCreateRule_4_10.coverage", "fe85b330-d3c8-40c4-9c09-9d0a70f8d858");
                    element.setAttribute(attributes.getQName(i), attributes.getValue(i));
                }
            }
            writeline("/home/ubuntu/results/coverage/NodeCreateRule/NodeCreateRule_4_10.coverage", "f7bac6f6-1636-4c5b-bffc-b97d834d7918");
            builder = new NodeBuilder(doc, element);
        } else {
            writeline("/home/ubuntu/results/coverage/NodeCreateRule/NodeCreateRule_4_10.coverage", "0fadc02e-1e8e-414c-b139-c2d509d2e2d7");
            builder = new NodeBuilder(doc, doc.createDocumentFragment());
        }
        writeline("/home/ubuntu/results/coverage/NodeCreateRule/NodeCreateRule_4_10.coverage", "214f25fd-393a-41d2-91d1-7a6f9f6f2449");
        getDigester().setCustomContentHandler(builder);
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public void end(String namespace, String name) throws Exception {
        writeline("/home/ubuntu/results/coverage/NodeCreateRule/NodeCreateRule_4_10.coverage", "9e6d3bba-6fba-45e4-9795-95494dc46800");
        getDigester().pop();
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
