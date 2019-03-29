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
        writeline("/home/ubuntu/results/coverage/XmlProperty/XmlProperty_1_10.coverage", "abd4728f-c532-4de3-9089-1af9007771c7");
        super.init();
        writeline("/home/ubuntu/results/coverage/XmlProperty/XmlProperty_1_10.coverage", "fb249aba-e3cc-41e6-88d3-3a441d5770c8");
        xmlCatalog.setProject(getProject());
    }

    /**
     * @return the xmlCatalog as the entityresolver.
     */
    protected EntityResolver getEntityResolver() {
        writeline("/home/ubuntu/results/coverage/XmlProperty/XmlProperty_1_10.coverage", "084d0551-d813-40ae-82f8-e5f131cf3668");
        return xmlCatalog;
    }

    /**
     * Run the task.
     * @throws BuildException The exception raised during task execution.
     * @todo validate the source file is valid before opening, print a better error message
     * @todo add a verbose level log message listing the name of the file being loaded
     */
    public void execute() throws BuildException {
        writeline("/home/ubuntu/results/coverage/XmlProperty/XmlProperty_1_10.coverage", "0815ba0a-6468-48d0-81f9-c5746d872a0b");
        Resource r = getResource();
        writeline("/home/ubuntu/results/coverage/XmlProperty/XmlProperty_1_10.coverage", "207fe282-0f26-4e38-aba6-07e1eb48d6f9");
        if (r == null) {
        }
        writeline("/home/ubuntu/results/coverage/XmlProperty/XmlProperty_1_10.coverage", "9bf30f35-b003-472a-a17e-743422809323");
        try {
            writeline("/home/ubuntu/results/coverage/XmlProperty/XmlProperty_1_10.coverage", "8cf3b158-2dbf-4dfc-b34b-92c1a169ca99");
            log("Loading " + src, Project.MSG_VERBOSE);
            writeline("/home/ubuntu/results/coverage/XmlProperty/XmlProperty_1_10.coverage", "71d21787-d521-4b75-b14f-665c79558734");
            if (r.isExists()) {
                writeline("/home/ubuntu/results/coverage/XmlProperty/XmlProperty_1_10.coverage", "7a7c4c2f-9d95-470f-902b-ba228c85e06b");
                DocumentBuilderFactory factory = DocumentBuilderFactory.newInstance();
                writeline("/home/ubuntu/results/coverage/XmlProperty/XmlProperty_1_10.coverage", "9e8dcafd-f5f8-4e29-92e4-422eed6f7c51");
                factory.setValidating(validate);
                writeline("/home/ubuntu/results/coverage/XmlProperty/XmlProperty_1_10.coverage", "301bb782-9788-48f6-85a7-43242894c552");
                factory.setNamespaceAware(false);
                writeline("/home/ubuntu/results/coverage/XmlProperty/XmlProperty_1_10.coverage", "d32a761d-fbe0-4b22-9a3b-83fd7b16f574");
                DocumentBuilder builder = factory.newDocumentBuilder();
                writeline("/home/ubuntu/results/coverage/XmlProperty/XmlProperty_1_10.coverage", "cd0f9044-42c7-4a47-a402-cce1447769ad");
                builder.setEntityResolver(getEntityResolver());
                writeline("/home/ubuntu/results/coverage/XmlProperty/XmlProperty_1_10.coverage", "101f62c0-bf34-401e-ab39-8dfbe80dfb2b");
                Document document = null;
                writeline("/home/ubuntu/results/coverage/XmlProperty/XmlProperty_1_10.coverage", "c2421973-3cb2-4847-85c7-5b6de2be6d96");
                FileProvider fp = src.as(FileProvider.class);
                writeline("/home/ubuntu/results/coverage/XmlProperty/XmlProperty_1_10.coverage", "14320a53-c47e-4209-bb9d-945574377ca5");
                if (fp != null) {
                    writeline("/home/ubuntu/results/coverage/XmlProperty/XmlProperty_1_10.coverage", "77bdbcc6-e4ba-4af7-89b1-87051f376db1");
                    document = builder.parse(fp.getFile());
                } else {
                    writeline("/home/ubuntu/results/coverage/XmlProperty/XmlProperty_1_10.coverage", "3ceb9789-418c-4b01-bbef-bbad23d8bc87");
                    document = builder.parse(src.getInputStream());
                }
                writeline("/home/ubuntu/results/coverage/XmlProperty/XmlProperty_1_10.coverage", "67925bfc-31ef-430e-822c-0baeb8761013");
                Element topElement = document.getDocumentElement();
                writeline("/home/ubuntu/results/coverage/XmlProperty/XmlProperty_1_10.coverage", "73f3c6d7-a447-4018-bbfa-3be3e88f9ea6");
                addedAttributes = new Hashtable();
                writeline("/home/ubuntu/results/coverage/XmlProperty/XmlProperty_1_10.coverage", "d5b05793-2a25-4521-ba12-4f719b91ba91");
                if (keepRoot) {
                    writeline("/home/ubuntu/results/coverage/XmlProperty/XmlProperty_1_10.coverage", "c5da4b8d-d0b4-4b86-9af1-e9b9a466f2c3");
                    addNodeRecursively(topElement, prefix, null);
                } else {
                    writeline("/home/ubuntu/results/coverage/XmlProperty/XmlProperty_1_10.coverage", "dc391f7e-0f82-4d42-983a-5b1283f6cc8b");
                    NodeList topChildren = topElement.getChildNodes();
                    writeline("/home/ubuntu/results/coverage/XmlProperty/XmlProperty_1_10.coverage", "0b9ee1e8-810a-4816-940c-d41f8e7e226c");
                    int numChildren = topChildren.getLength();
                    writeline("/home/ubuntu/results/coverage/XmlProperty/XmlProperty_1_10.coverage", "a4103bc7-a528-462c-9c28-0ab750558d67");
                    for (int i = 0; i < numChildren; i++) {
                        writeline("/home/ubuntu/results/coverage/XmlProperty/XmlProperty_1_10.coverage", "ae0bc5aa-ea0c-4f41-b836-6f396e036bc1");
                        addNodeRecursively(topChildren.item(i), prefix, null);
                    }
                }
            } else {
                writeline("/home/ubuntu/results/coverage/XmlProperty/XmlProperty_1_10.coverage", "11460269-fa62-4b57-853b-d4f23a0ba1f4");
                log("Unable to find property resource: " + r, Project.MSG_VERBOSE);
            }
        } catch (SAXException sxe) {
            writeline("/home/ubuntu/results/coverage/XmlProperty/XmlProperty_1_10.coverage", "66f87fa7-9071-4fbb-8d65-38ae1c1f6d6d");
            Exception x = sxe;
            writeline("/home/ubuntu/results/coverage/XmlProperty/XmlProperty_1_10.coverage", "5c313ab5-fbf6-43c8-8e28-d0f8699a2b50");
            if (sxe.getException() != null) {
                writeline("/home/ubuntu/results/coverage/XmlProperty/XmlProperty_1_10.coverage", "99b2371c-ee3b-4a0b-ae8f-187b71d1acfc");
                x = sxe.getException();
            }
        } catch (ParserConfigurationException pce) {
        } catch (IOException ioe) {
        }
    }

    /** Iterate through all nodes in the tree. */
    private void addNodeRecursively(Node node, String prefix, Object container) {
        writeline("/home/ubuntu/results/coverage/XmlProperty/XmlProperty_1_10.coverage", "f86fa7da-c443-4438-b38c-0675bbd79fdf");
        String nodePrefix = prefix;
        writeline("/home/ubuntu/results/coverage/XmlProperty/XmlProperty_1_10.coverage", "05c5869d-7d76-4f32-8419-c266a78dd3e5");
        if (node.getNodeType() != Node.TEXT_NODE) {
            writeline("/home/ubuntu/results/coverage/XmlProperty/XmlProperty_1_10.coverage", "b4fa50e9-badc-4b20-8ef2-c5c21d20dae4");
            if (prefix.trim().length() > 0) {
                writeline("/home/ubuntu/results/coverage/XmlProperty/XmlProperty_1_10.coverage", "2374f431-63e0-4a3a-8e0a-ed18d1e6bec1");
                nodePrefix += ".";
            }
            writeline("/home/ubuntu/results/coverage/XmlProperty/XmlProperty_1_10.coverage", "8a88325d-dc81-403b-b82c-019966c8093d");
            nodePrefix += node.getNodeName();
        }
        writeline("/home/ubuntu/results/coverage/XmlProperty/XmlProperty_1_10.coverage", "425c4a9f-c8a4-46e8-8b0d-754b6d3c8550");
        Object nodeObject = processNode(node, nodePrefix, container);
        writeline("/home/ubuntu/results/coverage/XmlProperty/XmlProperty_1_10.coverage", "e3817663-244d-475f-8f92-3c43fd644387");
        if (node.hasChildNodes()) {
            writeline("/home/ubuntu/results/coverage/XmlProperty/XmlProperty_1_10.coverage", "fa781aeb-16fa-414f-83bf-1def87cd87d0");
            NodeList nodeChildren = node.getChildNodes();
            writeline("/home/ubuntu/results/coverage/XmlProperty/XmlProperty_1_10.coverage", "cfb9f202-a52a-4840-b23f-b05a3aa00669");
            int numChildren = nodeChildren.getLength();
            writeline("/home/ubuntu/results/coverage/XmlProperty/XmlProperty_1_10.coverage", "4270c2df-33fa-4c1c-b971-05000df72b1e");
            for (int i = 0; i < numChildren; i++) {
                writeline("/home/ubuntu/results/coverage/XmlProperty/XmlProperty_1_10.coverage", "8c85f698-522e-451c-8614-aece56c80291");
                addNodeRecursively(nodeChildren.item(i), nodePrefix, nodeObject);
            }
        }
    }

    void addNodeRecursively(org.w3c.dom.Node node, String prefix) {
        writeline("/home/ubuntu/results/coverage/XmlProperty/XmlProperty_1_10.coverage", "544747d0-213d-48cc-b489-e80feb8ef334");
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
        writeline("/home/ubuntu/results/coverage/XmlProperty/XmlProperty_1_10.coverage", "3c8301fa-08d4-41d3-8a04-5a717f274ca1");
        Object addedPath = null;
        writeline("/home/ubuntu/results/coverage/XmlProperty/XmlProperty_1_10.coverage", "c0c9271a-772c-4046-a3d8-03e45e4894c0");
        String id = null;
        writeline("/home/ubuntu/results/coverage/XmlProperty/XmlProperty_1_10.coverage", "5c7af330-6405-462e-a134-50b18d09d48b");
        if (node.hasAttributes()) {
            writeline("/home/ubuntu/results/coverage/XmlProperty/XmlProperty_1_10.coverage", "1e012eda-b136-4aaf-bebd-c1661ac42aac");
            NamedNodeMap nodeAttributes = node.getAttributes();
            writeline("/home/ubuntu/results/coverage/XmlProperty/XmlProperty_1_10.coverage", "e22f25b5-6208-4dea-a579-87818a570fa7");
            Node idNode = nodeAttributes.getNamedItem(ID);
            writeline("/home/ubuntu/results/coverage/XmlProperty/XmlProperty_1_10.coverage", "7549b2a0-02eb-47a7-817b-4dde9522791e");
            id = semanticAttributes && idNode != null ? idNode.getNodeValue() : null;
            writeline("/home/ubuntu/results/coverage/XmlProperty/XmlProperty_1_10.coverage", "595b9b63-0444-419f-8c4e-d8fb0e56498e");
            for (int i = 0; i < nodeAttributes.getLength(); i++) {
                writeline("/home/ubuntu/results/coverage/XmlProperty/XmlProperty_1_10.coverage", "fb72aa02-c25f-44a0-a3c1-b43a4c8cba88");
                Node attributeNode = nodeAttributes.item(i);
                writeline("/home/ubuntu/results/coverage/XmlProperty/XmlProperty_1_10.coverage", "823c71d1-cf2f-4017-a2c5-3d1071479dbc");
                if (!semanticAttributes) {
                    writeline("/home/ubuntu/results/coverage/XmlProperty/XmlProperty_1_10.coverage", "eff8597a-08bc-4e65-baaa-5b64b60cb7d3");
                    String attributeName = getAttributeName(attributeNode);
                    writeline("/home/ubuntu/results/coverage/XmlProperty/XmlProperty_1_10.coverage", "ce604ffe-b4d8-46dc-92ac-2c82bf485331");
                    String attributeValue = getAttributeValue(attributeNode);
                    writeline("/home/ubuntu/results/coverage/XmlProperty/XmlProperty_1_10.coverage", "43d1444a-fec9-4d6a-b098-14d410e60a73");
                    addProperty(prefix + attributeName, attributeValue, null);
                } else {
                    writeline("/home/ubuntu/results/coverage/XmlProperty/XmlProperty_1_10.coverage", "85fd0ccc-de06-4ea4-abdd-4acf81dc784a");
                    String nodeName = attributeNode.getNodeName();
                    writeline("/home/ubuntu/results/coverage/XmlProperty/XmlProperty_1_10.coverage", "32483607-397a-4667-bb08-0c77144ed15c");
                    String attributeValue = getAttributeValue(attributeNode);
                    writeline("/home/ubuntu/results/coverage/XmlProperty/XmlProperty_1_10.coverage", "6de7fcba-3d75-4850-8638-870bd9c6c57d");
                    Path containingPath = ((container != null) && (container instanceof Path)) ? (Path) container : null;
                    writeline("/home/ubuntu/results/coverage/XmlProperty/XmlProperty_1_10.coverage", "656f7513-cbfb-4860-bd47-2278f4fc1ec8");
                    if (nodeName.equals(ID)) {
                        writeline("/home/ubuntu/results/coverage/XmlProperty/XmlProperty_1_10.coverage", "5eb5ac59-07ee-47e3-9597-36e9ea13c88c");
                        continue;
                    }
                    writeline("/home/ubuntu/results/coverage/XmlProperty/XmlProperty_1_10.coverage", "a3d199b3-fe4e-4f31-8311-b8c9c9a13bcc");
                    if (containingPath != null && nodeName.equals(PATH)) {
                        writeline("/home/ubuntu/results/coverage/XmlProperty/XmlProperty_1_10.coverage", "03ef1ce9-47a4-4084-9c12-56ee3ec68596");
                        containingPath.setPath(attributeValue);
                    } else if (containingPath != null && container instanceof Path && nodeName.equals(REF_ID)) {
                        writeline("/home/ubuntu/results/coverage/XmlProperty/XmlProperty_1_10.coverage", "b23da00d-11ab-450b-be58-a5167374e424");
                        containingPath.setPath(attributeValue);
                    } else if (containingPath != null && container instanceof Path && nodeName.equals(LOCATION)) {
                        writeline("/home/ubuntu/results/coverage/XmlProperty/XmlProperty_1_10.coverage", "2d4554de-715d-4a90-8808-7876ffe45959");
                        containingPath.setLocation(resolveFile(attributeValue));
                    } else if (nodeName.equals(PATHID)) {
                        writeline("/home/ubuntu/results/coverage/XmlProperty/XmlProperty_1_10.coverage", "ab18e095-f62c-4b9d-aae4-0a8a8eb9cc19");
                        if (container != null) {
                        }
                        writeline("/home/ubuntu/results/coverage/XmlProperty/XmlProperty_1_10.coverage", "26d802f5-9e4a-4ae2-99c2-2e5541c241ea");
                        addedPath = new Path(getProject());
                        writeline("/home/ubuntu/results/coverage/XmlProperty/XmlProperty_1_10.coverage", "b708c090-5e00-455c-bb75-6c3a5edd5f60");
                        getProject().addReference(attributeValue, addedPath);
                    } else {
                        writeline("/home/ubuntu/results/coverage/XmlProperty/XmlProperty_1_10.coverage", "d8ad81e3-75dd-4f24-83ed-8f8f322fe97f");
                        String attributeName = getAttributeName(attributeNode);
                        writeline("/home/ubuntu/results/coverage/XmlProperty/XmlProperty_1_10.coverage", "ddc11624-32a9-4341-a4f3-bfcf1b2f5d27");
                        addProperty(prefix + attributeName, attributeValue, id);
                    }
                }
            }
        }
        writeline("/home/ubuntu/results/coverage/XmlProperty/XmlProperty_1_10.coverage", "4e1e2489-a3dc-4b8c-8ab6-b20e663f700c");
        String nodeText = null;
        writeline("/home/ubuntu/results/coverage/XmlProperty/XmlProperty_1_10.coverage", "5b115fd8-315a-4cc2-a7cf-78ffcaf7c42c");
        boolean emptyNode = false;
        writeline("/home/ubuntu/results/coverage/XmlProperty/XmlProperty_1_10.coverage", "874f3363-5cbd-402e-875f-9b83b4cfd144");
        boolean semanticEmptyOverride = false;
        writeline("/home/ubuntu/results/coverage/XmlProperty/XmlProperty_1_10.coverage", "64797cb3-2f83-4db4-8198-5bdffa68b078");
        if (node.getNodeType() == Node.ELEMENT_NODE && semanticAttributes && node.hasAttributes() && (node.getAttributes().getNamedItem(VALUE) != null || node.getAttributes().getNamedItem(LOCATION) != null || node.getAttributes().getNamedItem(REF_ID) != null || node.getAttributes().getNamedItem(PATH) != null || node.getAttributes().getNamedItem(PATHID) != null)) {
            writeline("/home/ubuntu/results/coverage/XmlProperty/XmlProperty_1_10.coverage", "e8f39e5d-3d86-4972-b320-4dd84171d47e");
            semanticEmptyOverride = true;
        }
        writeline("/home/ubuntu/results/coverage/XmlProperty/XmlProperty_1_10.coverage", "e4f71b92-2f54-4fff-8425-6cdec0e55a90");
        if (node.getNodeType() == Node.TEXT_NODE) {
            writeline("/home/ubuntu/results/coverage/XmlProperty/XmlProperty_1_10.coverage", "75d063d5-ca5b-49c0-b88d-e3a21835036b");
            nodeText = getAttributeValue(node);
        } else if (node.getNodeType() == Node.ELEMENT_NODE && node.getChildNodes().getLength() == 1 && node.getFirstChild().getNodeType() == Node.CDATA_SECTION_NODE) {
            writeline("/home/ubuntu/results/coverage/XmlProperty/XmlProperty_1_10.coverage", "4bdbfc8f-880c-4d00-8ba1-8e19c86ecd60");
            nodeText = node.getFirstChild().getNodeValue();
            writeline("/home/ubuntu/results/coverage/XmlProperty/XmlProperty_1_10.coverage", "ab9236e3-d827-4f93-899c-ce65fabf336e");
            if ("".equals(nodeText) && !semanticEmptyOverride) {
                writeline("/home/ubuntu/results/coverage/XmlProperty/XmlProperty_1_10.coverage", "3b360eea-7f1a-4819-a183-8f959f568068");
                emptyNode = true;
            }
        } else if (node.getNodeType() == Node.ELEMENT_NODE && node.getChildNodes().getLength() == 0 && !semanticEmptyOverride) {
            writeline("/home/ubuntu/results/coverage/XmlProperty/XmlProperty_1_10.coverage", "989e818c-547e-461c-913b-ef3a3c14a5d3");
            nodeText = "";
            writeline("/home/ubuntu/results/coverage/XmlProperty/XmlProperty_1_10.coverage", "8eb00081-427b-4e77-af94-7b73887dc773");
            emptyNode = true;
        } else if (node.getNodeType() == Node.ELEMENT_NODE && node.getChildNodes().getLength() == 1 && node.getFirstChild().getNodeType() == Node.TEXT_NODE && "".equals(node.getFirstChild().getNodeValue()) && !semanticEmptyOverride) {
            writeline("/home/ubuntu/results/coverage/XmlProperty/XmlProperty_1_10.coverage", "a13d243f-52ce-4c4f-9bb0-aff648fbc832");
            nodeText = "";
            writeline("/home/ubuntu/results/coverage/XmlProperty/XmlProperty_1_10.coverage", "d44602ee-c9cd-467c-bdea-2b6c00c381d2");
            emptyNode = true;
        }
        writeline("/home/ubuntu/results/coverage/XmlProperty/XmlProperty_1_10.coverage", "a8cc129c-95b8-4155-a97e-da9f5992f696");
        if (nodeText != null) {
            writeline("/home/ubuntu/results/coverage/XmlProperty/XmlProperty_1_10.coverage", "41c3d47f-8f99-4ce9-9923-6df60ceddf10");
            if (semanticAttributes && id == null && container instanceof String) {
                writeline("/home/ubuntu/results/coverage/XmlProperty/XmlProperty_1_10.coverage", "80c95d88-7fab-4d13-9038-58e338dd6607");
                id = (String) container;
            }
            writeline("/home/ubuntu/results/coverage/XmlProperty/XmlProperty_1_10.coverage", "b7c3f102-8d8a-47f7-8b86-a7f3887c5286");
            if (nodeText.trim().length() != 0 || emptyNode) {
                writeline("/home/ubuntu/results/coverage/XmlProperty/XmlProperty_1_10.coverage", "a115f71c-6ac0-431a-9e58-cd88e11ce1b2");
                addProperty(prefix, nodeText, id);
            }
        }
        writeline("/home/ubuntu/results/coverage/XmlProperty/XmlProperty_1_10.coverage", "b08aa1a3-e555-400e-88a2-b68a097cb237");
        return (addedPath != null ? addedPath : id);
    }

    /**
     * Actually add the given property/value to the project
     * after writing a log message.
     */
    private void addProperty(String name, String value, String id) {
        writeline("/home/ubuntu/results/coverage/XmlProperty/XmlProperty_1_10.coverage", "c4b66993-9dbe-4e6a-af8b-34b320b7936b");
        String msg = name + ":" + value;
        writeline("/home/ubuntu/results/coverage/XmlProperty/XmlProperty_1_10.coverage", "b438825a-6a38-40f6-b428-5cd695f5a9a9");
        if (id != null) {
            writeline("/home/ubuntu/results/coverage/XmlProperty/XmlProperty_1_10.coverage", "37e37195-21f7-4109-a01b-f04934551a45");
            msg += ("(id=" + id + ")");
        }
        writeline("/home/ubuntu/results/coverage/XmlProperty/XmlProperty_1_10.coverage", "0fd80343-85cf-4c98-919c-3238f2356fb8");
        log(msg, Project.MSG_DEBUG);
        writeline("/home/ubuntu/results/coverage/XmlProperty/XmlProperty_1_10.coverage", "a24efd54-6ee8-454e-9d89-91f3f5926f56");
        if (addedAttributes.containsKey(name)) {
            writeline("/home/ubuntu/results/coverage/XmlProperty/XmlProperty_1_10.coverage", "159b2984-a404-4215-8b68-025d2e83fbd1");
            value = (String) addedAttributes.get(name) + getDelimiter() + value;
            writeline("/home/ubuntu/results/coverage/XmlProperty/XmlProperty_1_10.coverage", "dac47ba9-3f19-4584-858e-3429d22fc475");
            getProject().setProperty(name, value);
            writeline("/home/ubuntu/results/coverage/XmlProperty/XmlProperty_1_10.coverage", "c6b40e5c-077c-461c-8955-a8fb683751b5");
            addedAttributes.put(name, value);
        } else if (getProject().getProperty(name) == null) {
            writeline("/home/ubuntu/results/coverage/XmlProperty/XmlProperty_1_10.coverage", "2564f644-b2a1-4d96-ac59-c83f337ac557");
            getProject().setNewProperty(name, value);
            writeline("/home/ubuntu/results/coverage/XmlProperty/XmlProperty_1_10.coverage", "ceeb2bbe-f158-4b5d-a2ca-b270660349aa");
            addedAttributes.put(name, value);
        } else {
            writeline("/home/ubuntu/results/coverage/XmlProperty/XmlProperty_1_10.coverage", "924d57c3-33ff-4a64-804e-8ababe832987");
            log("Override ignored for property " + name, Project.MSG_VERBOSE);
        }
        writeline("/home/ubuntu/results/coverage/XmlProperty/XmlProperty_1_10.coverage", "e4ac2fe8-fcb0-46cc-a57f-626f27732910");
        if (id != null) {
            writeline("/home/ubuntu/results/coverage/XmlProperty/XmlProperty_1_10.coverage", "885113da-d618-4be2-b5d7-180b2b58a9bf");
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
        writeline("/home/ubuntu/results/coverage/XmlProperty/XmlProperty_1_10.coverage", "585bb6e7-3168-464b-8e8c-8e9cadf1e1d2");
        String attributeName = attributeNode.getNodeName();
        writeline("/home/ubuntu/results/coverage/XmlProperty/XmlProperty_1_10.coverage", "e2672183-9e1b-42b3-9850-36a0f024682b");
        if (semanticAttributes) {
            writeline("/home/ubuntu/results/coverage/XmlProperty/XmlProperty_1_10.coverage", "ab82b219-f033-4be8-807e-0d77ad47d27b");
            if (attributeName.equals(REF_ID)) {
                writeline("/home/ubuntu/results/coverage/XmlProperty/XmlProperty_1_10.coverage", "75204395-2d44-4717-9f94-f5f0aeaa34cb");
                return "";
            }
            writeline("/home/ubuntu/results/coverage/XmlProperty/XmlProperty_1_10.coverage", "822a8adc-3a5a-4bd8-8197-7367659a2985");
            if (!isSemanticAttribute(attributeName) || includeSemanticAttribute) {
                writeline("/home/ubuntu/results/coverage/XmlProperty/XmlProperty_1_10.coverage", "6fab534b-008c-4102-9126-ad1bdf78ed80");
                return "." + attributeName;
            }
            writeline("/home/ubuntu/results/coverage/XmlProperty/XmlProperty_1_10.coverage", "5e5c111c-cda4-484d-a8a7-e51dfe7603d4");
            return "";
        }
        writeline("/home/ubuntu/results/coverage/XmlProperty/XmlProperty_1_10.coverage", "13acf6ba-719e-4500-813c-5fc298928a86");
        return collapseAttributes ? "." + attributeName : "(" + attributeName + ")";
    }

    /**
     * Return whether the provided attribute name is recognized or not.
     */
    private static boolean isSemanticAttribute(String attributeName) {
        writeline("/home/ubuntu/results/coverage/XmlProperty/XmlProperty_1_10.coverage", "fe5c4de8-6eef-46ae-b1e9-eb4b59b2ead1");
        for (int i = 0; i < ATTRIBUTES.length; i++) {
            writeline("/home/ubuntu/results/coverage/XmlProperty/XmlProperty_1_10.coverage", "f209c2c1-61d7-4c69-975c-4bff04cc0936");
            if (attributeName.equals(ATTRIBUTES[i])) {
                writeline("/home/ubuntu/results/coverage/XmlProperty/XmlProperty_1_10.coverage", "4eb54e96-7c92-43ee-9d0d-f691a5e466fb");
                return true;
            }
        }
        writeline("/home/ubuntu/results/coverage/XmlProperty/XmlProperty_1_10.coverage", "a851e90e-e81c-4186-b649-21cc1ed56cc5");
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
        writeline("/home/ubuntu/results/coverage/XmlProperty/XmlProperty_1_10.coverage", "ac9905ac-1836-4e0b-adbe-4e5bb147fa0e");
        String nodeValue = attributeNode.getNodeValue().trim();
        writeline("/home/ubuntu/results/coverage/XmlProperty/XmlProperty_1_10.coverage", "e4b285cb-c8f2-4f1c-8210-c50bdbf268bb");
        if (semanticAttributes) {
            writeline("/home/ubuntu/results/coverage/XmlProperty/XmlProperty_1_10.coverage", "0c7d5744-36fb-473c-92bf-01202fcc4a69");
            String attributeName = attributeNode.getNodeName();
            writeline("/home/ubuntu/results/coverage/XmlProperty/XmlProperty_1_10.coverage", "be05a572-a3e3-444f-848a-948990ca3f56");
            nodeValue = getProject().replaceProperties(nodeValue);
            writeline("/home/ubuntu/results/coverage/XmlProperty/XmlProperty_1_10.coverage", "96790911-d01c-4b90-b1dd-d7d4ffb06ffd");
            if (attributeName.equals(LOCATION)) {
                writeline("/home/ubuntu/results/coverage/XmlProperty/XmlProperty_1_10.coverage", "d1518d40-cd81-44b8-9bb0-6af9ad2ea91e");
                File f = resolveFile(nodeValue);
                writeline("/home/ubuntu/results/coverage/XmlProperty/XmlProperty_1_10.coverage", "54d275db-e5a8-4314-ab1f-21f500541717");
                return f.getPath();
            }
            writeline("/home/ubuntu/results/coverage/XmlProperty/XmlProperty_1_10.coverage", "652a2003-dded-4777-b70e-69a047bd4267");
            if (attributeName.equals(REF_ID)) {
                writeline("/home/ubuntu/results/coverage/XmlProperty/XmlProperty_1_10.coverage", "91ab4828-39ec-432f-b35b-2d3075528053");
                Object ref = getProject().getReference(nodeValue);
                writeline("/home/ubuntu/results/coverage/XmlProperty/XmlProperty_1_10.coverage", "a1e5f3de-311d-4b7a-8845-3569adda3f72");
                if (ref != null) {
                    writeline("/home/ubuntu/results/coverage/XmlProperty/XmlProperty_1_10.coverage", "09b3d50a-4aa7-4a96-9436-db0fba26bea3");
                    return ref.toString();
                }
            }
        }
        writeline("/home/ubuntu/results/coverage/XmlProperty/XmlProperty_1_10.coverage", "c33c9a22-3f1e-46c9-ac3b-faf9f92fece6");
        return nodeValue;
    }

    /**
     * The XML file to parse; required.
     * @param src the file to parse
     */
    public void setFile(File src) {
        writeline("/home/ubuntu/results/coverage/XmlProperty/XmlProperty_1_10.coverage", "5a943e50-35b6-4041-98a8-9c9d6da91843");
        setSrcResource(new FileResource(src));
    }

    /**
     * The resource to pack; required.
     * @param src resource to expand
     */
    public void setSrcResource(Resource src) {
        writeline("/home/ubuntu/results/coverage/XmlProperty/XmlProperty_1_10.coverage", "6b5cf32a-4fe6-468f-91b9-b5abaa359929");
        if (src.isDirectory()) {
        }
        writeline("/home/ubuntu/results/coverage/XmlProperty/XmlProperty_1_10.coverage", "07577b20-8c7f-4473-b824-7b667b90d5fe");
        if (src.as(FileProvider.class) != null || supportsNonFileResources()) {
            writeline("/home/ubuntu/results/coverage/XmlProperty/XmlProperty_1_10.coverage", "e0c22076-3f66-4b27-9d63-4ae233491344");
            this.src = src;
        } else {
        }
    }

    /**
     * Set the source resource.
     * @param a the resource to pack as a single element Resource collection.
     */
    public void addConfigured(ResourceCollection a) {
        writeline("/home/ubuntu/results/coverage/XmlProperty/XmlProperty_1_10.coverage", "fec981c5-2cfd-487a-a79f-89cdda2e8e45");
        if (a.size() != 1) {
        }
        writeline("/home/ubuntu/results/coverage/XmlProperty/XmlProperty_1_10.coverage", "1c2456c9-65d0-499d-9bc5-8b5b40d4aa99");
        setSrcResource(a.iterator().next());
    }

    /**
     * the prefix to prepend to each property
     * @param prefix the prefix to prepend to each property
     */
    public void setPrefix(String prefix) {
        writeline("/home/ubuntu/results/coverage/XmlProperty/XmlProperty_1_10.coverage", "39048c03-8e96-4a5d-b25a-9cb69185c51f");
        this.prefix = prefix.trim();
    }

    /**
     * flag to include the xml root tag as a
     * first value in the property name; optional,
     * default is true
     * @param keepRoot if true (default), include the xml root tag
     */
    public void setKeeproot(boolean keepRoot) {
        writeline("/home/ubuntu/results/coverage/XmlProperty/XmlProperty_1_10.coverage", "eae634d3-641b-43d4-a98a-23239b0f6a8f");
        this.keepRoot = keepRoot;
    }

    /**
     * flag to validate the XML file; optional, default false
     * @param validate if true validate the XML file, default false
     */
    public void setValidate(boolean validate) {
        writeline("/home/ubuntu/results/coverage/XmlProperty/XmlProperty_1_10.coverage", "e5109075-912f-481f-9086-01e7393785ef");
        this.validate = validate;
    }

    /**
     * flag to treat attributes as nested elements;
     * optional, default false
     * @param collapseAttributes if true treat attributes as nested elements
     */
    public void setCollapseAttributes(boolean collapseAttributes) {
        writeline("/home/ubuntu/results/coverage/XmlProperty/XmlProperty_1_10.coverage", "30eb8492-70a1-435c-99bb-75e5b4eb31e5");
        this.collapseAttributes = collapseAttributes;
    }

    /**
     * Attribute to enable special handling of attributes - see ant manual.
     * @param semanticAttributes if true enable the special handling.
     */
    public void setSemanticAttributes(boolean semanticAttributes) {
        writeline("/home/ubuntu/results/coverage/XmlProperty/XmlProperty_1_10.coverage", "c689f90c-a538-4d7f-8d1e-0c373d946948");
        this.semanticAttributes = semanticAttributes;
    }

    /**
     * The directory to use for resolving file references.
     * Ignored if semanticAttributes is not set to true.
     * @param rootDirectory the directory.
     */
    public void setRootDirectory(File rootDirectory) {
        writeline("/home/ubuntu/results/coverage/XmlProperty/XmlProperty_1_10.coverage", "81987f88-d1d1-423e-aa5b-674dd3430060");
        this.rootDirectory = rootDirectory;
    }

    /**
     * Include the semantic attribute name as part of the property name.
     * Ignored if semanticAttributes is not set to true.
     * @param includeSemanticAttribute if true include the semantic attribute
     *                                 name.
     */
    public void setIncludeSemanticAttribute(boolean includeSemanticAttribute) {
        writeline("/home/ubuntu/results/coverage/XmlProperty/XmlProperty_1_10.coverage", "061d7cef-f596-4dfb-81b3-1feb7c6f617f");
        this.includeSemanticAttribute = includeSemanticAttribute;
    }

    /**
     * add an XMLCatalog as a nested element; optional.
     * @param catalog the XMLCatalog to use
     */
    public void addConfiguredXMLCatalog(XMLCatalog catalog) {
        writeline("/home/ubuntu/results/coverage/XmlProperty/XmlProperty_1_10.coverage", "24456010-9316-42c2-ad51-a0c145a1a020");
        xmlCatalog.addConfiguredXMLCatalog(catalog);
    }

    /**
     * @return the file attribute.
     */
    protected File getFile() {
        writeline("/home/ubuntu/results/coverage/XmlProperty/XmlProperty_1_10.coverage", "b90811ff-ba7c-4039-947e-f05958ae1df6");
        FileProvider fp = src.as(FileProvider.class);
        writeline("/home/ubuntu/results/coverage/XmlProperty/XmlProperty_1_10.coverage", "15c3bee0-baf3-4d42-ba63-1370ae055a72");
        return fp != null ? fp.getFile() : null;
    }

    /**
     * @return the resource.
     */
    protected Resource getResource() {
        writeline("/home/ubuntu/results/coverage/XmlProperty/XmlProperty_1_10.coverage", "5763f723-645d-44c2-8c3c-30a89b7c6000");
        File f = getFile();
        writeline("/home/ubuntu/results/coverage/XmlProperty/XmlProperty_1_10.coverage", "09a44581-8272-4a28-86ac-959773612a2b");
        FileProvider fp = src.as(FileProvider.class);
        writeline("/home/ubuntu/results/coverage/XmlProperty/XmlProperty_1_10.coverage", "d19e18fd-914d-48a6-8159-ded82ed3214f");
        return f == null ? src : fp != null && fp.getFile().equals(f) ? src : new FileResource(f);
    }

    /**
     * @return the prefix attribute.
     */
    protected String getPrefix() {
        writeline("/home/ubuntu/results/coverage/XmlProperty/XmlProperty_1_10.coverage", "a4a861cf-335a-49ec-81ac-8f3e038eefe5");
        return this.prefix;
    }

    /**
     * @return the keeproot attribute.
     */
    protected boolean getKeeproot() {
        writeline("/home/ubuntu/results/coverage/XmlProperty/XmlProperty_1_10.coverage", "8fc59006-bf5c-48dc-906c-734b81f9d9c2");
        return this.keepRoot;
    }

    /**
     * @return the validate attribute.
     */
    protected boolean getValidate() {
        writeline("/home/ubuntu/results/coverage/XmlProperty/XmlProperty_1_10.coverage", "cadc98e6-8219-475c-9072-b1ba4dacb14f");
        return this.validate;
    }

    /**
     * @return the collapse attributes attribute.
     */
    protected boolean getCollapseAttributes() {
        writeline("/home/ubuntu/results/coverage/XmlProperty/XmlProperty_1_10.coverage", "2e3c942a-2973-416d-a98d-e640bc983ec1");
        return this.collapseAttributes;
    }

    /**
     * @return the semantic attributes attribute.
     */
    protected boolean getSemanticAttributes() {
        writeline("/home/ubuntu/results/coverage/XmlProperty/XmlProperty_1_10.coverage", "af3a4b92-13c6-493e-b7be-75be7823cda6");
        return this.semanticAttributes;
    }

    /**
     * @return the root directory attribute.
     */
    protected File getRootDirectory() {
        writeline("/home/ubuntu/results/coverage/XmlProperty/XmlProperty_1_10.coverage", "95c0841e-8016-4475-894c-a8b7e727bd1a");
        return this.rootDirectory;
    }

    /**
     * @return the include semantic attribute.
     */
    protected boolean getIncludeSementicAttribute() {
        writeline("/home/ubuntu/results/coverage/XmlProperty/XmlProperty_1_10.coverage", "e21cefad-df01-45da-8603-5d36d9f6123e");
        return this.includeSemanticAttribute;
    }

    /**
     * Let project resolve the file - or do it ourselves if
     * rootDirectory has been set.
     */
    private File resolveFile(String fileName) {
        writeline("/home/ubuntu/results/coverage/XmlProperty/XmlProperty_1_10.coverage", "675c61a9-9fc5-43b1-a665-3fdc1a2e64fd");
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
        writeline("/home/ubuntu/results/coverage/XmlProperty/XmlProperty_1_10.coverage", "9f8338e2-c5b1-44c6-915c-d9006e50752d");
        return getClass().equals(XmlProperty.class);
    }

    /**
     * Get the current delimiter.
     * @return delimiter
     */
    public String getDelimiter() {
        writeline("/home/ubuntu/results/coverage/XmlProperty/XmlProperty_1_10.coverage", "ba10e72f-f609-429a-99be-de0a4c34c367");
        return delimiter;
    }

    /**
     * Sets a new delimiter.
     * @param delimiter new value
     * @since Ant 1.7.1
     */
    public void setDelimiter(String delimiter) {
        writeline("/home/ubuntu/results/coverage/XmlProperty/XmlProperty_1_10.coverage", "fa24adbb-f7b0-46a2-8a54-7a203aad8420");
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
