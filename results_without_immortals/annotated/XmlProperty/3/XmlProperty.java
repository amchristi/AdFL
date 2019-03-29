package org.apache.tools.ant.taskdefs;

import java.io.File;
import java.io.IOException;
import java.util.Hashtable;
import javax.xml.parsers.DocumentBuilder;
import javax.xml.parsers.DocumentBuilderFactory;
import javax.xml.parsers.ParserConfigurationException;
import org.apache.tools.ant.BuildException;
import org.apache.tools.ant.Project;
import org.apache.tools.ant.types.Path;
import org.apache.tools.ant.types.Resource;
import org.apache.tools.ant.types.ResourceCollection;
import org.apache.tools.ant.types.XMLCatalog;
import org.apache.tools.ant.types.resources.FileProvider;
import org.apache.tools.ant.types.resources.FileResource;
import org.apache.tools.ant.util.FileUtils;
import org.w3c.dom.Document;
import org.w3c.dom.Element;
import org.w3c.dom.NamedNodeMap;
import org.w3c.dom.Node;
import org.w3c.dom.NodeList;
import org.xml.sax.EntityResolver;
import org.xml.sax.SAXException;

/**
 * Loads property values from a valid XML file, generating the
 * property names from the file's element and attribute names.
 *
 * <p>Example:</p>
 * <pre>
 *   &lt;root-tag myattr="true"&gt;
 *     &lt;inner-tag someattr="val"&gt;Text&lt;/inner-tag&gt;
 *     &lt;a2&gt;&lt;a3&gt;&lt;a4&gt;false&lt;/a4&gt;&lt;/a3&gt;&lt;/a2&gt;
 *     &lt;x&gt;x1&lt;/x&gt;
 *     &lt;x&gt;x2&lt;/x&gt;
 *   &lt;/root-tag&gt;
 *</pre>
 *
 * <p>this generates the following properties:</p>
 *
 * <pre>
 *  root-tag(myattr)=true
 *  root-tag.inner-tag=Text
 *  root-tag.inner-tag(someattr)=val
 *  root-tag.a2.a3.a4=false
 *  root-tag.x=x1,x2
 * </pre>
 *
 * <p>The <i>collapseAttributes</i> property of this task can be set
 * to true (the default is false) which will instead result in the
 * following properties (note the difference in names of properties
 * corresponding to XML attributes):</p>
 *
 * <pre>
 *  root-tag.myattr=true
 *  root-tag.inner-tag=Text
 *  root-tag.inner-tag.someattr=val
 *  root-tag.a2.a3.a4=false
 *  root-tag.x=x1,x2
 * </pre>
 *
 * <p>Optionally, to more closely mirror the abilities of the Property
 * task, a selected set of attributes can be treated specially.  To
 * enable this behavior, the "semanticAttributes" property of this task
 * must be set to true (it defaults to false).  If this attribute is
 * specified, the following attributes take on special meaning
 * (setting this to true implicitly sets collapseAttributes to true as
 * well):</p>
 *
 * <ul>
 *  <li><b>value</b>: Identifies a text value for a property.</li>
 *  <li><b>location</b>: Identifies a file location for a property.</li>
 *  <li><b>id</b>: Sets an id for a property</li>
 *  <li><b>refid</b>: Sets a property to the value of another property
 *       based upon the provided id</li>
 *  <li><b>pathid</b>: Defines a path rather than a property with
 *       the given id.</li>
 * </ul>
 *
 * <p>For example, with keepRoot = false, the following properties file:</p>
 *
 * <pre>
 * &lt;root-tag&gt;
 *   &lt;build&gt;
 *   &lt;build folder="build"&gt;
 *     &lt;classes id="build.classes" location="${build.folder}/classes"/&gt;
 *     &lt;reference refid="build.classes"/&gt;
 *   &lt;/build&gt;
 *   &lt;compile&gt;
 *     &lt;classpath pathid="compile.classpath"&gt;
 *       &lt;pathelement location="${build.classes}"/&gt;
 *     &lt;/classpath&gt;
 *   &lt;/compile&gt;
 *   &lt;run-time&gt;
 *     &lt;jars&gt;*.jar&lt;/jars&gt;
 *     &lt;classpath pathid="run-time.classpath"&gt;
 *       &lt;path refid="compile.classpath"/&gt;
 *       &lt;pathelement path="${run-time.jars}"/&gt;
 *     &lt;/classpath&gt;
 *   &lt;/run-time&gt;
 * &lt;/root-tag&gt;
 * </pre>
 *
 * <p>is equivalent to the following entries in a build file:</p>
 *
 * <pre>
 * &lt;property name="build" location="build"/&gt;
 * &lt;property name="build.classes" location="${build.location}/classes"/&gt;
 * &lt;property name="build.reference" refid="build.classes"/&gt;
 *
 * &lt;property name="run-time.jars" value="*.jar/&gt;
 *
 * &lt;classpath id="compile.classpath"&gt;
 *   &lt;pathelement location="${build.classes}"/&gt;
 * &lt;/classpath&gt;
 *
 * &lt;classpath id="run-time.classpath"&gt;
 *   &lt;path refid="compile.classpath"/&gt;
 *   &lt;pathelement path="${run-time.jars}"/&gt;
 * &lt;/classpath&gt;
 * </pre>
 *
 * <p> This task <i>requires</i> the following attributes:</p>
 *
 * <ul>
 * <li><b>file</b>: The name of the file to load.</li>
 * </ul>
 *
 * <p>This task supports the following attributes:</p>
 *
 * <ul>
 * <li><b>prefix</b>: Optionally specify a prefix applied to
 *     all properties loaded.  Defaults to an empty string.</li>
 * <li><b>keepRoot</b>: Indicate whether the root xml element
 *     is kept as part of property name.  Defaults to true.</li>
 * <li><b>validate</b>: Indicate whether the xml file is validated.
 *     Defaults to false.</li>
 * <li><b>collapseAttributes</b>: Indicate whether attributes are
 *     stored in property names with parens or with period
 *     delimiters.  Defaults to false, meaning properties
 *     are stored with parens (i.e., foo(attr)).</li>
 * <li><b>semanticAttributes</b>: Indicate whether attributes
 *     named "location", "value", "refid" and "path"
 *     are interpreted as ant properties.  Defaults
 *     to false.</li>
 * <li><b>rootDirectory</b>: Indicate the directory to use
 *     as the root directory for resolving location
 *     properties.  Defaults to the directory
 *     of the project using the task.</li>
 * <li><b>includeSemanticAttribute</b>: Indicate whether to include
 *     the semantic attribute ("location" or "value") as
 *     part of the property name.  Defaults to false.</li>
 * </ul>
 *
 * @ant.task name="xmlproperty" category="xml"
 */
public class XmlProperty extends org.apache.tools.ant.Task {

    private Resource src;

    private String prefix = "";

    private boolean keepRoot = true;

    private boolean validate = false;

    private boolean collapseAttributes = false;

    private boolean semanticAttributes = false;

    private boolean includeSemanticAttribute = false;

    private File rootDirectory = null;

    private Hashtable addedAttributes = new Hashtable();

    private XMLCatalog xmlCatalog = new XMLCatalog();

    private String delimiter = ",";

    private static final String ID = "id";

    private static final String REF_ID = "refid";

    private static final String LOCATION = "location";

    private static final String VALUE = "value";

    private static final String PATH = "path";

    private static final String PATHID = "pathid";

    private static final String[] ATTRIBUTES = new String[] { ID, REF_ID, LOCATION, VALUE, PATH, PATHID };

    private static final FileUtils FILE_UTILS = FileUtils.getFileUtils();

    /**
     * Constructor.
     */
    public XmlProperty() {
        super();
    }

    /**
     * Initializes the task.
     */
    public void init() {
        writeline("/home/ubuntu/results/coverage/XmlProperty/XmlProperty_3_10.coverage", "486e63ce-3060-4d9b-8ac6-128cbbf636d1");
        super.init();
        writeline("/home/ubuntu/results/coverage/XmlProperty/XmlProperty_3_10.coverage", "d287974a-a560-4947-8c8e-a27b6353b1f0");
        xmlCatalog.setProject(getProject());
    }

    /**
     * @return the xmlCatalog as the entityresolver.
     */
    protected EntityResolver getEntityResolver() {
        writeline("/home/ubuntu/results/coverage/XmlProperty/XmlProperty_3_10.coverage", "bb419f09-b69a-4afb-9a89-b23173662ef8");
        return xmlCatalog;
    }

    /**
     * Run the task.
     * @throws BuildException The exception raised during task execution.
     * @todo validate the source file is valid before opening, print a better error message
     * @todo add a verbose level log message listing the name of the file being loaded
     */
    public void execute() throws BuildException {
        writeline("/home/ubuntu/results/coverage/XmlProperty/XmlProperty_3_10.coverage", "fff16234-3333-4c5f-ab3c-49d4e797cdf6");
        Resource r = getResource();
        writeline("/home/ubuntu/results/coverage/XmlProperty/XmlProperty_3_10.coverage", "44da98f7-7618-4683-89af-3d81f811cbf1");
        if (r == null) {
        }
        writeline("/home/ubuntu/results/coverage/XmlProperty/XmlProperty_3_10.coverage", "e3da0735-2758-400b-b804-b4a8050c5764");
        try {
            writeline("/home/ubuntu/results/coverage/XmlProperty/XmlProperty_3_10.coverage", "7ca40ef6-0fcf-49b7-9554-eca98526caed");
            log("Loading " + src, Project.MSG_VERBOSE);
            writeline("/home/ubuntu/results/coverage/XmlProperty/XmlProperty_3_10.coverage", "4e5e95b2-7c2d-49c5-ab48-ff09b394782a");
            if (r.isExists()) {
                writeline("/home/ubuntu/results/coverage/XmlProperty/XmlProperty_3_10.coverage", "e259058c-ba87-4d9f-b65a-511d57f5bc2f");
                DocumentBuilderFactory factory = DocumentBuilderFactory.newInstance();
                writeline("/home/ubuntu/results/coverage/XmlProperty/XmlProperty_3_10.coverage", "ddf67a91-242c-4ca7-8fc8-09a93bc0722a");
                factory.setValidating(validate);
                writeline("/home/ubuntu/results/coverage/XmlProperty/XmlProperty_3_10.coverage", "586d6ba5-d6ff-472f-9383-03724248b4cb");
                factory.setNamespaceAware(false);
                writeline("/home/ubuntu/results/coverage/XmlProperty/XmlProperty_3_10.coverage", "708ed753-91ac-445e-b385-bc08f0b69805");
                DocumentBuilder builder = factory.newDocumentBuilder();
                writeline("/home/ubuntu/results/coverage/XmlProperty/XmlProperty_3_10.coverage", "b4b14090-a191-4216-ac0a-5624c46e2ac1");
                builder.setEntityResolver(getEntityResolver());
                writeline("/home/ubuntu/results/coverage/XmlProperty/XmlProperty_3_10.coverage", "f2032fd4-d739-4d22-8301-3fc1f60945b8");
                Document document = null;
                writeline("/home/ubuntu/results/coverage/XmlProperty/XmlProperty_3_10.coverage", "600a4623-de7e-433f-a03e-7175a5a65d39");
                FileProvider fp = src.as(FileProvider.class);
                writeline("/home/ubuntu/results/coverage/XmlProperty/XmlProperty_3_10.coverage", "2fbeb1ac-e7f4-4c19-9da2-b05d483019b9");
                if (fp != null) {
                    writeline("/home/ubuntu/results/coverage/XmlProperty/XmlProperty_3_10.coverage", "57595f7c-3f22-42b5-8136-ae618f0f7d33");
                    document = builder.parse(fp.getFile());
                } else {
                    writeline("/home/ubuntu/results/coverage/XmlProperty/XmlProperty_3_10.coverage", "2e343ec1-6ee0-46ec-8a7e-a28ee52224bc");
                    document = builder.parse(src.getInputStream());
                }
                writeline("/home/ubuntu/results/coverage/XmlProperty/XmlProperty_3_10.coverage", "23a7adaa-aa5c-450f-8607-9b6bd197b7db");
                Element topElement = document.getDocumentElement();
                writeline("/home/ubuntu/results/coverage/XmlProperty/XmlProperty_3_10.coverage", "801bbe21-ecdd-4c80-9cbd-52a17ea419ae");
                addedAttributes = new Hashtable();
                writeline("/home/ubuntu/results/coverage/XmlProperty/XmlProperty_3_10.coverage", "0ea63d6f-c74a-439e-85e2-8233c0b4186d");
                if (keepRoot) {
                    writeline("/home/ubuntu/results/coverage/XmlProperty/XmlProperty_3_10.coverage", "1e667737-8b5b-4b82-9a69-858f70ae5a59");
                    addNodeRecursively(topElement, prefix, null);
                } else {
                    writeline("/home/ubuntu/results/coverage/XmlProperty/XmlProperty_3_10.coverage", "fc88ddf5-dbb0-453f-9f6f-39fe18e842e2");
                    NodeList topChildren = topElement.getChildNodes();
                    writeline("/home/ubuntu/results/coverage/XmlProperty/XmlProperty_3_10.coverage", "23775bf7-eb6c-43cb-9e7a-6fc3a7b68e05");
                    int numChildren = topChildren.getLength();
                    writeline("/home/ubuntu/results/coverage/XmlProperty/XmlProperty_3_10.coverage", "21af7f89-2493-49b6-b959-b6579e1776fe");
                    for (int i = 0; i < numChildren; i++) {
                        writeline("/home/ubuntu/results/coverage/XmlProperty/XmlProperty_3_10.coverage", "dc748071-5059-4a63-a832-79e186043cf1");
                        addNodeRecursively(topChildren.item(i), prefix, null);
                    }
                }
            } else {
                writeline("/home/ubuntu/results/coverage/XmlProperty/XmlProperty_3_10.coverage", "4576679a-88d8-4e71-a6b3-d88c61e1712d");
                log("Unable to find property resource: " + r, Project.MSG_VERBOSE);
            }
        } catch (SAXException sxe) {
            writeline("/home/ubuntu/results/coverage/XmlProperty/XmlProperty_3_10.coverage", "0db5c001-e28e-4c62-8d96-0dbfb8ef9d64");
            Exception x = sxe;
            writeline("/home/ubuntu/results/coverage/XmlProperty/XmlProperty_3_10.coverage", "ef877d8c-9332-4d1a-973a-c21437c15f71");
            if (sxe.getException() != null) {
                writeline("/home/ubuntu/results/coverage/XmlProperty/XmlProperty_3_10.coverage", "cd955158-1595-44ea-a783-7fd3f503a09a");
                x = sxe.getException();
            }
        } catch (ParserConfigurationException pce) {
        } catch (IOException ioe) {
        }
    }

    /** Iterate through all nodes in the tree. */
    private void addNodeRecursively(Node node, String prefix, Object container) {
        writeline("/home/ubuntu/results/coverage/XmlProperty/XmlProperty_3_10.coverage", "7ec916a9-e289-45a6-8d3f-c53cb89cdd6d");
        String nodePrefix = prefix;
        writeline("/home/ubuntu/results/coverage/XmlProperty/XmlProperty_3_10.coverage", "8a0b8117-35ff-45f1-98f5-2acdc9ad252c");
        if (node.getNodeType() != Node.TEXT_NODE) {
            writeline("/home/ubuntu/results/coverage/XmlProperty/XmlProperty_3_10.coverage", "32f9b910-6dfb-46da-a7c0-f3dddbadceb0");
            if (prefix.trim().length() > 0) {
                writeline("/home/ubuntu/results/coverage/XmlProperty/XmlProperty_3_10.coverage", "b5daea2b-dc59-466a-9d11-7375f49d1606");
                nodePrefix += ".";
            }
            writeline("/home/ubuntu/results/coverage/XmlProperty/XmlProperty_3_10.coverage", "8a9739dd-1231-4738-888d-ed9037f63438");
            nodePrefix += node.getNodeName();
        }
        writeline("/home/ubuntu/results/coverage/XmlProperty/XmlProperty_3_10.coverage", "aa57a908-7d33-439a-87aa-5c3619bba406");
        Object nodeObject = processNode(node, nodePrefix, container);
        writeline("/home/ubuntu/results/coverage/XmlProperty/XmlProperty_3_10.coverage", "5f25cdc4-54e0-4bd4-999d-667b766c982f");
        if (node.hasChildNodes()) {
            writeline("/home/ubuntu/results/coverage/XmlProperty/XmlProperty_3_10.coverage", "5587ef0f-6c38-46b4-bc7a-ab519a6407c3");
            NodeList nodeChildren = node.getChildNodes();
            writeline("/home/ubuntu/results/coverage/XmlProperty/XmlProperty_3_10.coverage", "ec9b648b-676d-4061-acb0-4ead1eba0e93");
            int numChildren = nodeChildren.getLength();
            writeline("/home/ubuntu/results/coverage/XmlProperty/XmlProperty_3_10.coverage", "4c164588-4dd2-4cae-bf13-a3cdffe8d2a2");
            for (int i = 0; i < numChildren; i++) {
                writeline("/home/ubuntu/results/coverage/XmlProperty/XmlProperty_3_10.coverage", "ebaaaff5-7e9e-40a1-94ec-dddfb56fb73d");
                addNodeRecursively(nodeChildren.item(i), nodePrefix, nodeObject);
            }
        }
    }

    void addNodeRecursively(org.w3c.dom.Node node, String prefix) {
        writeline("/home/ubuntu/results/coverage/XmlProperty/XmlProperty_3_10.coverage", "0d56cb66-e28d-4c0a-9f3d-252108f9bd41");
        addNodeRecursively(node, prefix, null);
    }

    /**
     * Process the given node, adding any required attributes from
     * this child node alone -- but <em>not</em> processing any
     * children.
     *
     * @param node the XML Node to parse
     * @param prefix A string to prepend to any properties that get
     * added by this node.
     * @param container Optionally, an object that a parent node
     * generated that this node might belong to.  For example, this
     * node could be within a node that generated a Path.
     * @return the Object created by this node.  Generally, this is
     * either a String if this node resulted in setting an attribute,
     * or a Path.
     */
    public Object processNode(Node node, String prefix, Object container) {
        writeline("/home/ubuntu/results/coverage/XmlProperty/XmlProperty_3_10.coverage", "653ec9c2-5ee8-4fcb-af45-6274ef43ceed");
        Object addedPath = null;
        writeline("/home/ubuntu/results/coverage/XmlProperty/XmlProperty_3_10.coverage", "17750e0f-e9c7-4549-b160-9260ff68d85d");
        String id = null;
        writeline("/home/ubuntu/results/coverage/XmlProperty/XmlProperty_3_10.coverage", "7d53836d-a82c-400d-96f1-214e87d4dbb8");
        if (node.hasAttributes()) {
            writeline("/home/ubuntu/results/coverage/XmlProperty/XmlProperty_3_10.coverage", "2c71d063-a85f-494e-87ad-0f76c371e0eb");
            NamedNodeMap nodeAttributes = node.getAttributes();
            writeline("/home/ubuntu/results/coverage/XmlProperty/XmlProperty_3_10.coverage", "31edb47d-8ed6-4226-8ab8-d2c3e85b2dad");
            Node idNode = nodeAttributes.getNamedItem(ID);
            writeline("/home/ubuntu/results/coverage/XmlProperty/XmlProperty_3_10.coverage", "1072943b-0703-4f22-b781-4989e8948ca4");
            id = semanticAttributes && idNode != null ? idNode.getNodeValue() : null;
            writeline("/home/ubuntu/results/coverage/XmlProperty/XmlProperty_3_10.coverage", "c558d51f-a4f8-464b-99a6-d63d0343533a");
            for (int i = 0; i < nodeAttributes.getLength(); i++) {
                writeline("/home/ubuntu/results/coverage/XmlProperty/XmlProperty_3_10.coverage", "ec32f472-d68e-4347-978c-cef8c430b2a3");
                Node attributeNode = nodeAttributes.item(i);
                writeline("/home/ubuntu/results/coverage/XmlProperty/XmlProperty_3_10.coverage", "16c78ce4-f4ea-4201-9fa7-aeb7bdbe8d08");
                if (!semanticAttributes) {
                    writeline("/home/ubuntu/results/coverage/XmlProperty/XmlProperty_3_10.coverage", "acbd4bf2-56a9-4794-b6c1-002bd0044942");
                    String attributeName = getAttributeName(attributeNode);
                    writeline("/home/ubuntu/results/coverage/XmlProperty/XmlProperty_3_10.coverage", "4019a9f3-9de6-4c30-921d-0650f517dab6");
                    String attributeValue = getAttributeValue(attributeNode);
                    writeline("/home/ubuntu/results/coverage/XmlProperty/XmlProperty_3_10.coverage", "2eb679eb-da51-4310-b0c1-d2018905ae0b");
                    addProperty(prefix + attributeName, attributeValue, null);
                } else {
                    writeline("/home/ubuntu/results/coverage/XmlProperty/XmlProperty_3_10.coverage", "ab1e580f-1633-406a-8a6f-02fc88e92d1b");
                    String nodeName = attributeNode.getNodeName();
                    writeline("/home/ubuntu/results/coverage/XmlProperty/XmlProperty_3_10.coverage", "f6974c3f-6940-4bde-854f-392c4f14f610");
                    String attributeValue = getAttributeValue(attributeNode);
                    writeline("/home/ubuntu/results/coverage/XmlProperty/XmlProperty_3_10.coverage", "a576286e-ec79-496c-991c-ca8d6ffc2c66");
                    Path containingPath = ((container != null) && (container instanceof Path)) ? (Path) container : null;
                    writeline("/home/ubuntu/results/coverage/XmlProperty/XmlProperty_3_10.coverage", "a24d23e9-e7ef-4d52-846a-b97821b47083");
                    if (nodeName.equals(ID)) {
                        writeline("/home/ubuntu/results/coverage/XmlProperty/XmlProperty_3_10.coverage", "e3dcf683-2606-4658-9b13-350247b0c6b3");
                        continue;
                    }
                    writeline("/home/ubuntu/results/coverage/XmlProperty/XmlProperty_3_10.coverage", "41d494c9-5e61-4497-8a2d-c4edb99c4d11");
                    if (containingPath != null && nodeName.equals(PATH)) {
                        writeline("/home/ubuntu/results/coverage/XmlProperty/XmlProperty_3_10.coverage", "68584c44-a997-4241-aad1-b21e29d3f4b3");
                        containingPath.setPath(attributeValue);
                    } else if (containingPath != null && container instanceof Path && nodeName.equals(REF_ID)) {
                        writeline("/home/ubuntu/results/coverage/XmlProperty/XmlProperty_3_10.coverage", "9b91bba9-b052-4841-8854-61983996ed62");
                        containingPath.setPath(attributeValue);
                    } else if (containingPath != null && container instanceof Path && nodeName.equals(LOCATION)) {
                        writeline("/home/ubuntu/results/coverage/XmlProperty/XmlProperty_3_10.coverage", "3ee63050-057a-48dc-810a-8a525a5f0ae0");
                        containingPath.setLocation(resolveFile(attributeValue));
                    } else if (nodeName.equals(PATHID)) {
                        writeline("/home/ubuntu/results/coverage/XmlProperty/XmlProperty_3_10.coverage", "1670fff0-c06c-468a-a43b-b113fddfcc3f");
                        if (container != null) {
                        }
                        writeline("/home/ubuntu/results/coverage/XmlProperty/XmlProperty_3_10.coverage", "d58d2580-8c11-45b8-88bd-93bc5cbb45be");
                        addedPath = new Path(getProject());
                        writeline("/home/ubuntu/results/coverage/XmlProperty/XmlProperty_3_10.coverage", "7a1e4afe-e13d-4fd1-88a4-ec797049f45b");
                        getProject().addReference(attributeValue, addedPath);
                    } else {
                        writeline("/home/ubuntu/results/coverage/XmlProperty/XmlProperty_3_10.coverage", "c70339d4-56a6-4ef6-95f7-79a3498b56df");
                        String attributeName = getAttributeName(attributeNode);
                        writeline("/home/ubuntu/results/coverage/XmlProperty/XmlProperty_3_10.coverage", "8474bbf3-5d40-4f16-9f67-2fc5cfa9974a");
                        addProperty(prefix + attributeName, attributeValue, id);
                    }
                }
            }
        }
        writeline("/home/ubuntu/results/coverage/XmlProperty/XmlProperty_3_10.coverage", "cf79c0b3-56e1-4a59-856a-5930dc04809e");
        String nodeText = null;
        writeline("/home/ubuntu/results/coverage/XmlProperty/XmlProperty_3_10.coverage", "1064f567-9d95-4965-b58a-8c5cbb874999");
        boolean emptyNode = false;
        writeline("/home/ubuntu/results/coverage/XmlProperty/XmlProperty_3_10.coverage", "5237af80-6e7c-45e0-8a47-91350b5f73ab");
        boolean semanticEmptyOverride = false;
        writeline("/home/ubuntu/results/coverage/XmlProperty/XmlProperty_3_10.coverage", "9e1e4fa8-60cd-4226-8a80-d495eb425f53");
        if (node.getNodeType() == Node.ELEMENT_NODE && semanticAttributes && node.hasAttributes() && (node.getAttributes().getNamedItem(VALUE) != null || node.getAttributes().getNamedItem(LOCATION) != null || node.getAttributes().getNamedItem(REF_ID) != null || node.getAttributes().getNamedItem(PATH) != null || node.getAttributes().getNamedItem(PATHID) != null)) {
            writeline("/home/ubuntu/results/coverage/XmlProperty/XmlProperty_3_10.coverage", "acca08ae-ed1e-4fd8-95a3-ab8040e69d5c");
            semanticEmptyOverride = true;
        }
        writeline("/home/ubuntu/results/coverage/XmlProperty/XmlProperty_3_10.coverage", "91f0516d-5037-477a-b9a0-5f9866efaa9e");
        if (node.getNodeType() == Node.TEXT_NODE) {
            writeline("/home/ubuntu/results/coverage/XmlProperty/XmlProperty_3_10.coverage", "44507955-b042-404a-bad4-b1086110aea5");
            nodeText = getAttributeValue(node);
        } else if (node.getNodeType() == Node.ELEMENT_NODE && node.getChildNodes().getLength() == 1 && node.getFirstChild().getNodeType() == Node.CDATA_SECTION_NODE) {
            writeline("/home/ubuntu/results/coverage/XmlProperty/XmlProperty_3_10.coverage", "c7aa2f11-e5fe-4c51-87bd-4cf2a5b777ba");
            nodeText = node.getFirstChild().getNodeValue();
            writeline("/home/ubuntu/results/coverage/XmlProperty/XmlProperty_3_10.coverage", "fa0bd371-3499-4826-a1eb-984b8bbabb46");
            if ("".equals(nodeText) && !semanticEmptyOverride) {
                writeline("/home/ubuntu/results/coverage/XmlProperty/XmlProperty_3_10.coverage", "007af06b-493e-4ad3-ac91-28c8518c6e59");
                emptyNode = true;
            }
        } else if (node.getNodeType() == Node.ELEMENT_NODE && node.getChildNodes().getLength() == 0 && !semanticEmptyOverride) {
            writeline("/home/ubuntu/results/coverage/XmlProperty/XmlProperty_3_10.coverage", "d3338910-d04a-4f82-b544-9008db9d71ac");
            nodeText = "";
            writeline("/home/ubuntu/results/coverage/XmlProperty/XmlProperty_3_10.coverage", "5a2def48-2d5c-43d5-951c-8ff3de6a6cb6");
            emptyNode = true;
        } else if (node.getNodeType() == Node.ELEMENT_NODE && node.getChildNodes().getLength() == 1 && node.getFirstChild().getNodeType() == Node.TEXT_NODE && "".equals(node.getFirstChild().getNodeValue()) && !semanticEmptyOverride) {
            writeline("/home/ubuntu/results/coverage/XmlProperty/XmlProperty_3_10.coverage", "0f6dceec-e8ef-474e-aec8-ee0555197216");
            nodeText = "";
            writeline("/home/ubuntu/results/coverage/XmlProperty/XmlProperty_3_10.coverage", "ad4606b1-ad9c-41a7-8771-a3b97348de2b");
            emptyNode = true;
        }
        writeline("/home/ubuntu/results/coverage/XmlProperty/XmlProperty_3_10.coverage", "435e0cd5-da9e-4dd6-b662-9ed94567b62c");
        if (nodeText != null) {
            writeline("/home/ubuntu/results/coverage/XmlProperty/XmlProperty_3_10.coverage", "96b8b537-d1df-4f37-b5e4-8fa1b979bc38");
            if (semanticAttributes && id == null && container instanceof String) {
                writeline("/home/ubuntu/results/coverage/XmlProperty/XmlProperty_3_10.coverage", "d7500c2e-3823-4368-a3e4-0952ce264a02");
                id = (String) container;
            }
            writeline("/home/ubuntu/results/coverage/XmlProperty/XmlProperty_3_10.coverage", "b38bd06f-cce4-400f-b268-2d3fb7ffea83");
            if (nodeText.trim().length() != 0 || emptyNode) {
                writeline("/home/ubuntu/results/coverage/XmlProperty/XmlProperty_3_10.coverage", "a509afdf-d427-4e62-b971-3343e2d84cb3");
                addProperty(prefix, nodeText, id);
            }
        }
        writeline("/home/ubuntu/results/coverage/XmlProperty/XmlProperty_3_10.coverage", "e169c962-0ae7-4590-8ac3-409048f369cc");
        return (addedPath != null ? addedPath : id);
    }

    /**
     * Actually add the given property/value to the project
     * after writing a log message.
     */
    private void addProperty(String name, String value, String id) {
        writeline("/home/ubuntu/results/coverage/XmlProperty/XmlProperty_3_10.coverage", "4af8b032-b49c-4e0a-a55c-baa6876ff925");
        String msg = name + ":" + value;
        writeline("/home/ubuntu/results/coverage/XmlProperty/XmlProperty_3_10.coverage", "26b25fd1-041c-4007-b384-0163132fc834");
        if (id != null) {
            writeline("/home/ubuntu/results/coverage/XmlProperty/XmlProperty_3_10.coverage", "d61fe86f-fc9b-4137-8c62-b9068cf28fbf");
            msg += ("(id=" + id + ")");
        }
        writeline("/home/ubuntu/results/coverage/XmlProperty/XmlProperty_3_10.coverage", "13c2adf7-fd22-43b0-8302-dd54121dd30e");
        log(msg, Project.MSG_DEBUG);
        writeline("/home/ubuntu/results/coverage/XmlProperty/XmlProperty_3_10.coverage", "16c3a494-d129-4fa4-8a76-4c48126e37dc");
        if (addedAttributes.containsKey(name)) {
            writeline("/home/ubuntu/results/coverage/XmlProperty/XmlProperty_3_10.coverage", "2959050a-58d4-4afa-9de6-1046e4365968");
            value = (String) addedAttributes.get(name) + getDelimiter() + value;
            writeline("/home/ubuntu/results/coverage/XmlProperty/XmlProperty_3_10.coverage", "67c070a6-c704-4211-8c5b-e1b3f01942d5");
            getProject().setProperty(name, value);
            writeline("/home/ubuntu/results/coverage/XmlProperty/XmlProperty_3_10.coverage", "92901751-96d9-4980-b5a3-6ccecff37714");
            addedAttributes.put(name, value);
        } else if (getProject().getProperty(name) == null) {
            writeline("/home/ubuntu/results/coverage/XmlProperty/XmlProperty_3_10.coverage", "a9c6c5bd-bdfb-43af-8d21-566166ee30b0");
            getProject().setNewProperty(name, value);
            writeline("/home/ubuntu/results/coverage/XmlProperty/XmlProperty_3_10.coverage", "b4851b6d-7ef8-4efb-b49f-91a822fc4f7d");
            addedAttributes.put(name, value);
        } else {
            writeline("/home/ubuntu/results/coverage/XmlProperty/XmlProperty_3_10.coverage", "0ed79693-5205-4879-ac09-762649f405cf");
            log("Override ignored for property " + name, Project.MSG_VERBOSE);
        }
        writeline("/home/ubuntu/results/coverage/XmlProperty/XmlProperty_3_10.coverage", "b16f983a-261f-497f-87d3-cdfe144aedec");
        if (id != null) {
            writeline("/home/ubuntu/results/coverage/XmlProperty/XmlProperty_3_10.coverage", "c096fc33-82f2-40c5-8f82-1a46eacec714");
            getProject().addReference(id, value);
        }
    }

    /**
     * Return a reasonable attribute name for the given node.
     * If we are using semantic attributes or collapsing
     * attributes, the returned name is ".nodename".
     * Otherwise, we return "(nodename)".  This is long-standing
     * (and default) &lt;xmlproperty&gt; behavior.
     */
    private String getAttributeName(Node attributeNode) {
        writeline("/home/ubuntu/results/coverage/XmlProperty/XmlProperty_3_10.coverage", "8b0414b2-5295-48db-91e3-0105676cf2d0");
        String attributeName = attributeNode.getNodeName();
        writeline("/home/ubuntu/results/coverage/XmlProperty/XmlProperty_3_10.coverage", "5174a0e2-143f-4c5e-ba29-d954dd1845af");
        if (semanticAttributes) {
            writeline("/home/ubuntu/results/coverage/XmlProperty/XmlProperty_3_10.coverage", "0c6a33b7-f6ea-473e-9780-a96060f95fc6");
            if (attributeName.equals(REF_ID)) {
                writeline("/home/ubuntu/results/coverage/XmlProperty/XmlProperty_3_10.coverage", "01d00105-6b8a-4f80-9820-19195c0ed19c");
                return "";
            }
            writeline("/home/ubuntu/results/coverage/XmlProperty/XmlProperty_3_10.coverage", "1cea2a8e-0d36-4398-b98d-35dc3e631a0f");
            if (!isSemanticAttribute(attributeName) || includeSemanticAttribute) {
                writeline("/home/ubuntu/results/coverage/XmlProperty/XmlProperty_3_10.coverage", "79b381d6-eb40-4691-86a7-5573913592d0");
                return "." + attributeName;
            }
            writeline("/home/ubuntu/results/coverage/XmlProperty/XmlProperty_3_10.coverage", "5d1b623d-ce9d-4e58-8022-875feea77d69");
            return "";
        }
        writeline("/home/ubuntu/results/coverage/XmlProperty/XmlProperty_3_10.coverage", "688ce238-fe68-4536-ae1e-52c8144fd425");
        return collapseAttributes ? "." + attributeName : "(" + attributeName + ")";
    }

    /**
     * Return whether the provided attribute name is recognized or not.
     */
    private static boolean isSemanticAttribute(String attributeName) {
        writeline("/home/ubuntu/results/coverage/XmlProperty/XmlProperty_3_10.coverage", "b3f1b018-bb43-4728-8aef-f4b095e7ff3f");
        for (int i = 0; i < ATTRIBUTES.length; i++) {
            writeline("/home/ubuntu/results/coverage/XmlProperty/XmlProperty_3_10.coverage", "a495bf6c-6647-4ccc-bcc3-1311940e5e92");
            if (attributeName.equals(ATTRIBUTES[i])) {
                writeline("/home/ubuntu/results/coverage/XmlProperty/XmlProperty_3_10.coverage", "9bb7e8fe-2ad5-43ec-a415-564bdba36259");
                return true;
            }
        }
        writeline("/home/ubuntu/results/coverage/XmlProperty/XmlProperty_3_10.coverage", "ee6eba01-6181-4405-ba0e-4ae79f09dde9");
        return false;
    }

    /**
     * Return the value for the given attribute.
     * If we are not using semantic attributes, its just the
     * literal string value of the attribute.
     *
     * <p>If we <em>are</em> using semantic attributes, then first
     * dependent properties are resolved (i.e., ${foo} is resolved
     * based on the foo property value), and then an appropriate data
     * type is used.  In particular, location-based properties are
     * resolved to absolute file names.  Also for refid values, look
     * up the referenced object from the project.</p>
     */
    private String getAttributeValue(Node attributeNode) {
        writeline("/home/ubuntu/results/coverage/XmlProperty/XmlProperty_3_10.coverage", "1e682103-01ce-4b4d-9021-cc873bd2114f");
        String nodeValue = attributeNode.getNodeValue().trim();
        writeline("/home/ubuntu/results/coverage/XmlProperty/XmlProperty_3_10.coverage", "c79cf7ef-bdb9-4a82-9753-915987271a88");
        if (semanticAttributes) {
            writeline("/home/ubuntu/results/coverage/XmlProperty/XmlProperty_3_10.coverage", "8736f906-0012-4291-884d-134b3deb1e12");
            String attributeName = attributeNode.getNodeName();
            writeline("/home/ubuntu/results/coverage/XmlProperty/XmlProperty_3_10.coverage", "bd1098fe-10a6-44b0-9409-cc54bd7e4828");
            nodeValue = getProject().replaceProperties(nodeValue);
            writeline("/home/ubuntu/results/coverage/XmlProperty/XmlProperty_3_10.coverage", "b40ac436-5fb9-426c-8e1b-e6e35f25d4d2");
            if (attributeName.equals(LOCATION)) {
                writeline("/home/ubuntu/results/coverage/XmlProperty/XmlProperty_3_10.coverage", "29436809-8cfc-4684-ada3-4f7063c1b7a3");
                File f = resolveFile(nodeValue);
                writeline("/home/ubuntu/results/coverage/XmlProperty/XmlProperty_3_10.coverage", "ff0fe775-8c16-4b09-8768-75eea99f9dc0");
                return f.getPath();
            }
            writeline("/home/ubuntu/results/coverage/XmlProperty/XmlProperty_3_10.coverage", "507814a6-4c38-496c-bb11-4cbc3d4b542a");
            if (attributeName.equals(REF_ID)) {
                writeline("/home/ubuntu/results/coverage/XmlProperty/XmlProperty_3_10.coverage", "23002356-d817-416c-9d46-aead8549d774");
                Object ref = getProject().getReference(nodeValue);
                writeline("/home/ubuntu/results/coverage/XmlProperty/XmlProperty_3_10.coverage", "aaaabd7a-eb3e-4a3b-ab3b-5a796ab5ebab");
                if (ref != null) {
                    writeline("/home/ubuntu/results/coverage/XmlProperty/XmlProperty_3_10.coverage", "16653034-c777-4200-95cf-a716ca12caa9");
                    return ref.toString();
                }
            }
        }
        writeline("/home/ubuntu/results/coverage/XmlProperty/XmlProperty_3_10.coverage", "3e23b7d0-4f6b-43bc-a75a-6e3ca4be6455");
        return nodeValue;
    }

    /**
     * The XML file to parse; required.
     * @param src the file to parse
     */
    public void setFile(File src) {
        writeline("/home/ubuntu/results/coverage/XmlProperty/XmlProperty_3_10.coverage", "1ae15b0c-f439-45ab-986b-7c0312aa0cf9");
        setSrcResource(new FileResource(src));
    }

    /**
     * The resource to pack; required.
     * @param src resource to expand
     */
    public void setSrcResource(Resource src) {
        writeline("/home/ubuntu/results/coverage/XmlProperty/XmlProperty_3_10.coverage", "ebf7ad61-6681-44c7-80c5-e7b4197e45c0");
        if (src.isDirectory()) {
        }
        writeline("/home/ubuntu/results/coverage/XmlProperty/XmlProperty_3_10.coverage", "3588a126-0661-4765-a7d4-a30ceff19491");
        if (src.as(FileProvider.class) != null || supportsNonFileResources()) {
            writeline("/home/ubuntu/results/coverage/XmlProperty/XmlProperty_3_10.coverage", "1aea9f1d-c622-4b8e-8f77-ed70326c679c");
            this.src = src;
        } else {
        }
    }

    /**
     * Set the source resource.
     * @param a the resource to pack as a single element Resource collection.
     */
    public void addConfigured(ResourceCollection a) {
        writeline("/home/ubuntu/results/coverage/XmlProperty/XmlProperty_3_10.coverage", "239b877d-62d8-4411-aa6e-720b838814b2");
        if (a.size() != 1) {
        }
        writeline("/home/ubuntu/results/coverage/XmlProperty/XmlProperty_3_10.coverage", "ecdd1c04-146c-4bda-9c84-3ee7be11767f");
        setSrcResource(a.iterator().next());
    }

    /**
     * the prefix to prepend to each property
     * @param prefix the prefix to prepend to each property
     */
    public void setPrefix(String prefix) {
        writeline("/home/ubuntu/results/coverage/XmlProperty/XmlProperty_3_10.coverage", "15455e05-b6d0-4728-93e3-95ab75d8f407");
        this.prefix = prefix.trim();
    }

    /**
     * flag to include the xml root tag as a
     * first value in the property name; optional,
     * default is true
     * @param keepRoot if true (default), include the xml root tag
     */
    public void setKeeproot(boolean keepRoot) {
        writeline("/home/ubuntu/results/coverage/XmlProperty/XmlProperty_3_10.coverage", "9e9dc5bb-447a-4e3a-a72d-6abac238e088");
        this.keepRoot = keepRoot;
    }

    /**
     * flag to validate the XML file; optional, default false
     * @param validate if true validate the XML file, default false
     */
    public void setValidate(boolean validate) {
        writeline("/home/ubuntu/results/coverage/XmlProperty/XmlProperty_3_10.coverage", "bedc5f60-9b69-4a13-9908-ee27b3347045");
        this.validate = validate;
    }

    /**
     * flag to treat attributes as nested elements;
     * optional, default false
     * @param collapseAttributes if true treat attributes as nested elements
     */
    public void setCollapseAttributes(boolean collapseAttributes) {
        writeline("/home/ubuntu/results/coverage/XmlProperty/XmlProperty_3_10.coverage", "9e30daf1-2180-4830-b3c9-097d7f95e57c");
        this.collapseAttributes = collapseAttributes;
    }

    /**
     * Attribute to enable special handling of attributes - see ant manual.
     * @param semanticAttributes if true enable the special handling.
     */
    public void setSemanticAttributes(boolean semanticAttributes) {
        writeline("/home/ubuntu/results/coverage/XmlProperty/XmlProperty_3_10.coverage", "ad32b51a-e292-4b87-8739-2061eee71cb8");
        this.semanticAttributes = semanticAttributes;
    }

    /**
     * The directory to use for resolving file references.
     * Ignored if semanticAttributes is not set to true.
     * @param rootDirectory the directory.
     */
    public void setRootDirectory(File rootDirectory) {
        writeline("/home/ubuntu/results/coverage/XmlProperty/XmlProperty_3_10.coverage", "20ce4279-cb8a-4ec4-81f9-1254b0172686");
        this.rootDirectory = rootDirectory;
    }

    /**
     * Include the semantic attribute name as part of the property name.
     * Ignored if semanticAttributes is not set to true.
     * @param includeSemanticAttribute if true include the semantic attribute
     *                                 name.
     */
    public void setIncludeSemanticAttribute(boolean includeSemanticAttribute) {
        writeline("/home/ubuntu/results/coverage/XmlProperty/XmlProperty_3_10.coverage", "afcf1c98-3353-4f82-82bb-75aeb0dd820a");
        this.includeSemanticAttribute = includeSemanticAttribute;
    }

    /**
     * add an XMLCatalog as a nested element; optional.
     * @param catalog the XMLCatalog to use
     */
    public void addConfiguredXMLCatalog(XMLCatalog catalog) {
        writeline("/home/ubuntu/results/coverage/XmlProperty/XmlProperty_3_10.coverage", "268df433-b658-49e7-aafa-97e8662f4f78");
        xmlCatalog.addConfiguredXMLCatalog(catalog);
    }

    /**
     * @return the file attribute.
     */
    protected File getFile() {
        writeline("/home/ubuntu/results/coverage/XmlProperty/XmlProperty_3_10.coverage", "472eaf91-5cee-431b-95fb-1263860f6f79");
        FileProvider fp = src.as(FileProvider.class);
        writeline("/home/ubuntu/results/coverage/XmlProperty/XmlProperty_3_10.coverage", "bb894a27-5b77-4991-aaca-ae787baab085");
        return fp != null ? fp.getFile() : null;
    }

    /**
     * @return the resource.
     */
    protected Resource getResource() {
        writeline("/home/ubuntu/results/coverage/XmlProperty/XmlProperty_3_10.coverage", "646d657f-86b1-4b63-a7f1-346cbb892d14");
        File f = getFile();
        writeline("/home/ubuntu/results/coverage/XmlProperty/XmlProperty_3_10.coverage", "a84953ac-84f8-40e3-b964-be69d04e943b");
        FileProvider fp = src.as(FileProvider.class);
        writeline("/home/ubuntu/results/coverage/XmlProperty/XmlProperty_3_10.coverage", "2e6b16cc-bd4a-4336-8985-28e3f261e02e");
        return f == null ? src : fp != null && fp.getFile().equals(f) ? src : new FileResource(f);
    }

    /**
     * @return the prefix attribute.
     */
    protected String getPrefix() {
        writeline("/home/ubuntu/results/coverage/XmlProperty/XmlProperty_3_10.coverage", "347276cc-d213-465c-8252-917e93dd8448");
        return this.prefix;
    }

    /**
     * @return the keeproot attribute.
     */
    protected boolean getKeeproot() {
        writeline("/home/ubuntu/results/coverage/XmlProperty/XmlProperty_3_10.coverage", "34d3a940-d12b-4982-88c9-0c19be6b89dd");
        return this.keepRoot;
    }

    /**
     * @return the validate attribute.
     */
    protected boolean getValidate() {
        writeline("/home/ubuntu/results/coverage/XmlProperty/XmlProperty_3_10.coverage", "75865f1e-8edb-4932-8e6f-d73dff781197");
        return this.validate;
    }

    /**
     * @return the collapse attributes attribute.
     */
    protected boolean getCollapseAttributes() {
        writeline("/home/ubuntu/results/coverage/XmlProperty/XmlProperty_3_10.coverage", "87c9778c-026a-4081-a801-087139d739fb");
        return this.collapseAttributes;
    }

    /**
     * @return the semantic attributes attribute.
     */
    protected boolean getSemanticAttributes() {
        writeline("/home/ubuntu/results/coverage/XmlProperty/XmlProperty_3_10.coverage", "aca28e50-a7eb-4548-8d20-72b15d68ea75");
        return this.semanticAttributes;
    }

    /**
     * @return the root directory attribute.
     */
    protected File getRootDirectory() {
        writeline("/home/ubuntu/results/coverage/XmlProperty/XmlProperty_3_10.coverage", "bc22625e-ec5c-4504-957a-cab9cb75b1fd");
        return this.rootDirectory;
    }

    /**
     * @return the include semantic attribute.
     */
    protected boolean getIncludeSementicAttribute() {
        writeline("/home/ubuntu/results/coverage/XmlProperty/XmlProperty_3_10.coverage", "3c34cd6c-9dcf-4d8a-8483-dfb1753743c3");
        return this.includeSemanticAttribute;
    }

    /**
     * Let project resolve the file - or do it ourselves if
     * rootDirectory has been set.
     */
    private File resolveFile(String fileName) {
        writeline("/home/ubuntu/results/coverage/XmlProperty/XmlProperty_3_10.coverage", "1e7f731a-b4fd-4d5a-a68e-387a209e89c4");
        return FILE_UTILS.resolveFile(rootDirectory == null ? getProject().getBaseDir() : rootDirectory, fileName);
    }

    /**
     * Whether this task can deal with non-file resources.
     *
     * <p>This implementation returns true only if this task is
     * &lt;xmlproperty&gt;.  Any subclass of this class that also wants to
     * support non-file resources needs to override this method.  We
     * need to do so for backwards compatibility reasons since we
     * can't expect subclasses to support resources.</p>
     * @return true for this task.
     * @since Ant 1.7
     */
    protected boolean supportsNonFileResources() {
        writeline("/home/ubuntu/results/coverage/XmlProperty/XmlProperty_3_10.coverage", "0d7ab432-d2fa-415a-9ffb-17ecc24084b8");
        return getClass().equals(XmlProperty.class);
    }

    /**
     * Get the current delimiter.
     * @return delimiter
     */
    public String getDelimiter() {
        writeline("/home/ubuntu/results/coverage/XmlProperty/XmlProperty_3_10.coverage", "978d8cf1-522c-4f35-9663-e388769aad50");
        return delimiter;
    }

    /**
     * Sets a new delimiter.
     * @param delimiter new value
     * @since Ant 1.7.1
     */
    public void setDelimiter(String delimiter) {
        writeline("/home/ubuntu/results/coverage/XmlProperty/XmlProperty_3_10.coverage", "945e0464-3208-4291-b19a-3f93691d6720");
        this.delimiter = delimiter;
    }

    public void writeLine(String fullFilePath, String text) {
        try {
            BufferedWriter output = null;
            java.io.File file = new File(fullFilePath);
            FileWriter fileWriter = new FileWriter(file);
            output.append(text);
            output.close();
        } catch (IOException e) {
            e.printStackTrace();
        }
    }
}
