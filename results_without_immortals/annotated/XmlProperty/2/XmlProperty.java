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
        writeline("/home/ubuntu/results/coverage/XmlProperty/XmlProperty_2_10.coverage", "d6e33bd0-e617-4b91-9bc0-2850b9cd4b36");
        super.init();
        writeline("/home/ubuntu/results/coverage/XmlProperty/XmlProperty_2_10.coverage", "c080b206-287f-4ebc-b2b0-41e2e4d93e3d");
        xmlCatalog.setProject(getProject());
    }

    /**
     * @return the xmlCatalog as the entityresolver.
     */
    protected EntityResolver getEntityResolver() {
        writeline("/home/ubuntu/results/coverage/XmlProperty/XmlProperty_2_10.coverage", "97744ec1-2b1f-4a19-8153-519523f4329c");
        return xmlCatalog;
    }

    /**
     * Run the task.
     * @throws BuildException The exception raised during task execution.
     * @todo validate the source file is valid before opening, print a better error message
     * @todo add a verbose level log message listing the name of the file being loaded
     */
    public void execute() throws BuildException {
        writeline("/home/ubuntu/results/coverage/XmlProperty/XmlProperty_2_10.coverage", "02af8c72-59be-4608-a76d-03267eed3b03");
        Resource r = getResource();
        writeline("/home/ubuntu/results/coverage/XmlProperty/XmlProperty_2_10.coverage", "9229fbe8-3bae-4c1d-a01a-303967560499");
        if (r == null) {
        }
        writeline("/home/ubuntu/results/coverage/XmlProperty/XmlProperty_2_10.coverage", "24156179-9dbe-4de8-abeb-359a41d4d4db");
        try {
            writeline("/home/ubuntu/results/coverage/XmlProperty/XmlProperty_2_10.coverage", "9d40af02-6474-4e83-a9ec-ae60f1c206cb");
            log("Loading " + src, Project.MSG_VERBOSE);
            writeline("/home/ubuntu/results/coverage/XmlProperty/XmlProperty_2_10.coverage", "e9b2f72b-41ce-4b4b-8e71-65d6ef0a3442");
            if (r.isExists()) {
                writeline("/home/ubuntu/results/coverage/XmlProperty/XmlProperty_2_10.coverage", "91f412e7-797c-4271-a7af-e0ad8c731e04");
                DocumentBuilderFactory factory = DocumentBuilderFactory.newInstance();
                writeline("/home/ubuntu/results/coverage/XmlProperty/XmlProperty_2_10.coverage", "a3e3099e-5718-4349-aa70-8b5c7b01b632");
                factory.setValidating(validate);
                writeline("/home/ubuntu/results/coverage/XmlProperty/XmlProperty_2_10.coverage", "94b04e47-441c-431e-a4b8-bbdb761ff9c6");
                factory.setNamespaceAware(false);
                writeline("/home/ubuntu/results/coverage/XmlProperty/XmlProperty_2_10.coverage", "d90c5b2a-cfb4-4b78-873c-2087d12feb2a");
                DocumentBuilder builder = factory.newDocumentBuilder();
                writeline("/home/ubuntu/results/coverage/XmlProperty/XmlProperty_2_10.coverage", "cb7e1a29-d93a-47c7-85dd-894434a9fb5a");
                builder.setEntityResolver(getEntityResolver());
                writeline("/home/ubuntu/results/coverage/XmlProperty/XmlProperty_2_10.coverage", "495fde72-32ad-4698-a338-371190ecc05e");
                Document document = null;
                writeline("/home/ubuntu/results/coverage/XmlProperty/XmlProperty_2_10.coverage", "fd257c50-c086-434e-a45d-34820cb02d7d");
                FileProvider fp = src.as(FileProvider.class);
                writeline("/home/ubuntu/results/coverage/XmlProperty/XmlProperty_2_10.coverage", "6fcde3c8-68a0-41b1-9272-b6387ac87daa");
                if (fp != null) {
                    writeline("/home/ubuntu/results/coverage/XmlProperty/XmlProperty_2_10.coverage", "4b99f965-aeaa-4637-b873-f26c425444e9");
                    document = builder.parse(fp.getFile());
                } else {
                    writeline("/home/ubuntu/results/coverage/XmlProperty/XmlProperty_2_10.coverage", "3ae0ada7-73ef-4bbe-87b2-73cac20e6ac7");
                    document = builder.parse(src.getInputStream());
                }
                writeline("/home/ubuntu/results/coverage/XmlProperty/XmlProperty_2_10.coverage", "d7aa9441-fad4-470e-a99c-87c3fa315441");
                Element topElement = document.getDocumentElement();
                writeline("/home/ubuntu/results/coverage/XmlProperty/XmlProperty_2_10.coverage", "a67a36b6-49ac-417d-ad08-9e52d339f0da");
                addedAttributes = new Hashtable();
                writeline("/home/ubuntu/results/coverage/XmlProperty/XmlProperty_2_10.coverage", "c2d98763-210f-41f7-84f0-1c281be5e263");
                if (keepRoot) {
                    writeline("/home/ubuntu/results/coverage/XmlProperty/XmlProperty_2_10.coverage", "f5e17f2b-1f78-404c-872b-2bf24ca540ec");
                    addNodeRecursively(topElement, prefix, null);
                } else {
                    writeline("/home/ubuntu/results/coverage/XmlProperty/XmlProperty_2_10.coverage", "93810213-8724-4efc-a117-74e11984d8db");
                    NodeList topChildren = topElement.getChildNodes();
                    writeline("/home/ubuntu/results/coverage/XmlProperty/XmlProperty_2_10.coverage", "5023b748-66e4-43a7-af3c-8e3b57265735");
                    int numChildren = topChildren.getLength();
                    writeline("/home/ubuntu/results/coverage/XmlProperty/XmlProperty_2_10.coverage", "2f1a63a8-e058-4403-b58c-a38e9075d1a7");
                    for (int i = 0; i < numChildren; i++) {
                        writeline("/home/ubuntu/results/coverage/XmlProperty/XmlProperty_2_10.coverage", "c34c3836-1df8-4ece-b732-d88ab52ac092");
                        addNodeRecursively(topChildren.item(i), prefix, null);
                    }
                }
            } else {
                writeline("/home/ubuntu/results/coverage/XmlProperty/XmlProperty_2_10.coverage", "5f319fde-a9bb-4e50-9cb3-de5244f454ff");
                log("Unable to find property resource: " + r, Project.MSG_VERBOSE);
            }
        } catch (SAXException sxe) {
            writeline("/home/ubuntu/results/coverage/XmlProperty/XmlProperty_2_10.coverage", "16e23ad1-afc3-45ab-95e9-2ce741685745");
            Exception x = sxe;
            writeline("/home/ubuntu/results/coverage/XmlProperty/XmlProperty_2_10.coverage", "2c9fbbf9-43ab-4476-9682-63cd991184e2");
            if (sxe.getException() != null) {
                writeline("/home/ubuntu/results/coverage/XmlProperty/XmlProperty_2_10.coverage", "0685676e-e907-4979-843b-982b58736ede");
                x = sxe.getException();
            }
        } catch (ParserConfigurationException pce) {
        } catch (IOException ioe) {
        }
    }

    /** Iterate through all nodes in the tree. */
    private void addNodeRecursively(Node node, String prefix, Object container) {
        writeline("/home/ubuntu/results/coverage/XmlProperty/XmlProperty_2_10.coverage", "424efcde-7f43-414d-8900-7e13e7c15ee5");
        String nodePrefix = prefix;
        writeline("/home/ubuntu/results/coverage/XmlProperty/XmlProperty_2_10.coverage", "97820ad6-f8a2-4f26-ab22-432b1e5e8dae");
        if (node.getNodeType() != Node.TEXT_NODE) {
            writeline("/home/ubuntu/results/coverage/XmlProperty/XmlProperty_2_10.coverage", "607fba6b-59b7-47ee-8430-f1bc567e2a0a");
            if (prefix.trim().length() > 0) {
                writeline("/home/ubuntu/results/coverage/XmlProperty/XmlProperty_2_10.coverage", "37e42b66-4166-4634-ad9e-1fb3b283308d");
                nodePrefix += ".";
            }
            writeline("/home/ubuntu/results/coverage/XmlProperty/XmlProperty_2_10.coverage", "b7531234-c18c-4735-9a06-84ea8f7ca7e1");
            nodePrefix += node.getNodeName();
        }
        writeline("/home/ubuntu/results/coverage/XmlProperty/XmlProperty_2_10.coverage", "4a5d4285-9cbe-4fce-ade0-31d58048c067");
        Object nodeObject = processNode(node, nodePrefix, container);
        writeline("/home/ubuntu/results/coverage/XmlProperty/XmlProperty_2_10.coverage", "e7da2e09-aabf-49e7-a629-e83d5c2a5d19");
        if (node.hasChildNodes()) {
            writeline("/home/ubuntu/results/coverage/XmlProperty/XmlProperty_2_10.coverage", "f91549fd-bd1a-4ed1-a230-56edf03e09f2");
            NodeList nodeChildren = node.getChildNodes();
            writeline("/home/ubuntu/results/coverage/XmlProperty/XmlProperty_2_10.coverage", "4b3a28b3-39f9-4042-bb39-2f22515e539d");
            int numChildren = nodeChildren.getLength();
            writeline("/home/ubuntu/results/coverage/XmlProperty/XmlProperty_2_10.coverage", "2e357ecf-6291-4335-a02f-44399477de43");
            for (int i = 0; i < numChildren; i++) {
                writeline("/home/ubuntu/results/coverage/XmlProperty/XmlProperty_2_10.coverage", "1a0924a8-0b55-466f-917d-53a6a7b577be");
                addNodeRecursively(nodeChildren.item(i), nodePrefix, nodeObject);
            }
        }
    }

    void addNodeRecursively(org.w3c.dom.Node node, String prefix) {
        writeline("/home/ubuntu/results/coverage/XmlProperty/XmlProperty_2_10.coverage", "4c02ca12-93d0-41d3-84a2-050cfb6bd933");
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
        writeline("/home/ubuntu/results/coverage/XmlProperty/XmlProperty_2_10.coverage", "716d2f27-3ff6-4e0d-97f1-081037f20343");
        Object addedPath = null;
        writeline("/home/ubuntu/results/coverage/XmlProperty/XmlProperty_2_10.coverage", "4758dc24-fe70-4a94-9328-ac39382a01bf");
        String id = null;
        writeline("/home/ubuntu/results/coverage/XmlProperty/XmlProperty_2_10.coverage", "a1169544-c792-4699-933c-645287acdcd3");
        if (node.hasAttributes()) {
            writeline("/home/ubuntu/results/coverage/XmlProperty/XmlProperty_2_10.coverage", "b4cf44b3-c843-47ad-ba2d-26a257b0e80c");
            NamedNodeMap nodeAttributes = node.getAttributes();
            writeline("/home/ubuntu/results/coverage/XmlProperty/XmlProperty_2_10.coverage", "260e60a8-d1f8-42f8-bf8d-0e63dafe3c36");
            Node idNode = nodeAttributes.getNamedItem(ID);
            writeline("/home/ubuntu/results/coverage/XmlProperty/XmlProperty_2_10.coverage", "caa96b6e-cc4c-4822-9f46-2466896dc705");
            id = semanticAttributes && idNode != null ? idNode.getNodeValue() : null;
            writeline("/home/ubuntu/results/coverage/XmlProperty/XmlProperty_2_10.coverage", "981389d9-b316-4727-ada3-787b34421c61");
            for (int i = 0; i < nodeAttributes.getLength(); i++) {
                writeline("/home/ubuntu/results/coverage/XmlProperty/XmlProperty_2_10.coverage", "f5a10e56-31f8-4643-ac5b-ef9e1959a8d1");
                Node attributeNode = nodeAttributes.item(i);
                writeline("/home/ubuntu/results/coverage/XmlProperty/XmlProperty_2_10.coverage", "1d39b6ac-4124-4707-b3d3-3e1690ac0b93");
                if (!semanticAttributes) {
                    writeline("/home/ubuntu/results/coverage/XmlProperty/XmlProperty_2_10.coverage", "919116ab-d80a-4b88-a9b0-8a398a80cefb");
                    String attributeName = getAttributeName(attributeNode);
                    writeline("/home/ubuntu/results/coverage/XmlProperty/XmlProperty_2_10.coverage", "ec38fcd0-9ae9-4d36-9d97-e0564c0bb303");
                    String attributeValue = getAttributeValue(attributeNode);
                    writeline("/home/ubuntu/results/coverage/XmlProperty/XmlProperty_2_10.coverage", "733bf573-dd5f-4eea-a524-317fba8edc2d");
                    addProperty(prefix + attributeName, attributeValue, null);
                } else {
                    writeline("/home/ubuntu/results/coverage/XmlProperty/XmlProperty_2_10.coverage", "518dc563-9ca8-40d6-96b6-6cd516a91857");
                    String nodeName = attributeNode.getNodeName();
                    writeline("/home/ubuntu/results/coverage/XmlProperty/XmlProperty_2_10.coverage", "68978303-9681-4325-99a3-aeba947fc7dc");
                    String attributeValue = getAttributeValue(attributeNode);
                    writeline("/home/ubuntu/results/coverage/XmlProperty/XmlProperty_2_10.coverage", "a8e11d37-18b8-4af0-b1a3-8b7e78fcc7bb");
                    Path containingPath = ((container != null) && (container instanceof Path)) ? (Path) container : null;
                    writeline("/home/ubuntu/results/coverage/XmlProperty/XmlProperty_2_10.coverage", "923ee6f5-9eef-4bfd-bd49-b476820c2957");
                    if (nodeName.equals(ID)) {
                        writeline("/home/ubuntu/results/coverage/XmlProperty/XmlProperty_2_10.coverage", "752529d9-1e7c-481d-93ef-f1c9c4dc720e");
                        continue;
                    }
                    writeline("/home/ubuntu/results/coverage/XmlProperty/XmlProperty_2_10.coverage", "bb653224-75a1-41e1-9ae7-2636fcbe05af");
                    if (containingPath != null && nodeName.equals(PATH)) {
                        writeline("/home/ubuntu/results/coverage/XmlProperty/XmlProperty_2_10.coverage", "3a7b3b9a-c3e1-4a0b-b047-c94acb7a039e");
                        containingPath.setPath(attributeValue);
                    } else if (containingPath != null && container instanceof Path && nodeName.equals(REF_ID)) {
                        writeline("/home/ubuntu/results/coverage/XmlProperty/XmlProperty_2_10.coverage", "2815a552-5311-4eb6-8130-85407c34ae06");
                        containingPath.setPath(attributeValue);
                    } else if (containingPath != null && container instanceof Path && nodeName.equals(LOCATION)) {
                        writeline("/home/ubuntu/results/coverage/XmlProperty/XmlProperty_2_10.coverage", "ac598374-f638-4488-bc6d-69493fcf16ec");
                        containingPath.setLocation(resolveFile(attributeValue));
                    } else if (nodeName.equals(PATHID)) {
                        writeline("/home/ubuntu/results/coverage/XmlProperty/XmlProperty_2_10.coverage", "0aaa949f-7ff3-4f0a-9d4a-4be690f75a70");
                        if (container != null) {
                        }
                        writeline("/home/ubuntu/results/coverage/XmlProperty/XmlProperty_2_10.coverage", "19da4484-e164-491e-a98e-a8114c570552");
                        addedPath = new Path(getProject());
                        writeline("/home/ubuntu/results/coverage/XmlProperty/XmlProperty_2_10.coverage", "ce7543f3-f49e-4076-9ed4-9b1c59d81a71");
                        getProject().addReference(attributeValue, addedPath);
                    } else {
                        writeline("/home/ubuntu/results/coverage/XmlProperty/XmlProperty_2_10.coverage", "d50b5b59-30fb-459d-ad6e-39c7c53b77aa");
                        String attributeName = getAttributeName(attributeNode);
                        writeline("/home/ubuntu/results/coverage/XmlProperty/XmlProperty_2_10.coverage", "3ba86a5a-c293-4ca8-bc29-46f04d952024");
                        addProperty(prefix + attributeName, attributeValue, id);
                    }
                }
            }
        }
        writeline("/home/ubuntu/results/coverage/XmlProperty/XmlProperty_2_10.coverage", "f93edc6b-3b69-4899-b9a3-f8411226f3ef");
        String nodeText = null;
        writeline("/home/ubuntu/results/coverage/XmlProperty/XmlProperty_2_10.coverage", "9dcd3f01-6946-4a08-8d82-07021dfe6e30");
        boolean emptyNode = false;
        writeline("/home/ubuntu/results/coverage/XmlProperty/XmlProperty_2_10.coverage", "4cac3df5-1fe7-4b84-a358-dfd05fb6e9ae");
        boolean semanticEmptyOverride = false;
        writeline("/home/ubuntu/results/coverage/XmlProperty/XmlProperty_2_10.coverage", "31ad1edc-4fc5-44df-bf90-5feecfc1fb6e");
        if (node.getNodeType() == Node.ELEMENT_NODE && semanticAttributes && node.hasAttributes() && (node.getAttributes().getNamedItem(VALUE) != null || node.getAttributes().getNamedItem(LOCATION) != null || node.getAttributes().getNamedItem(REF_ID) != null || node.getAttributes().getNamedItem(PATH) != null || node.getAttributes().getNamedItem(PATHID) != null)) {
            writeline("/home/ubuntu/results/coverage/XmlProperty/XmlProperty_2_10.coverage", "d947d8a7-71a9-4b36-82a3-975e5f6d90c2");
            semanticEmptyOverride = true;
        }
        writeline("/home/ubuntu/results/coverage/XmlProperty/XmlProperty_2_10.coverage", "221a4b62-e03d-4527-87f8-1c492f8a0239");
        if (node.getNodeType() == Node.TEXT_NODE) {
            writeline("/home/ubuntu/results/coverage/XmlProperty/XmlProperty_2_10.coverage", "8c6e5ea5-edde-4316-a0b2-a58cfe76af47");
            nodeText = getAttributeValue(node);
        } else if (node.getNodeType() == Node.ELEMENT_NODE && node.getChildNodes().getLength() == 1 && node.getFirstChild().getNodeType() == Node.CDATA_SECTION_NODE) {
            writeline("/home/ubuntu/results/coverage/XmlProperty/XmlProperty_2_10.coverage", "210aa882-2ae7-4d24-afd9-c432dd9990f3");
            nodeText = node.getFirstChild().getNodeValue();
            writeline("/home/ubuntu/results/coverage/XmlProperty/XmlProperty_2_10.coverage", "d832c280-775f-4000-b24a-856cd516db1e");
            if ("".equals(nodeText) && !semanticEmptyOverride) {
                writeline("/home/ubuntu/results/coverage/XmlProperty/XmlProperty_2_10.coverage", "c16f3b3c-7d0c-4183-944b-1dd945952648");
                emptyNode = true;
            }
        } else if (node.getNodeType() == Node.ELEMENT_NODE && node.getChildNodes().getLength() == 0 && !semanticEmptyOverride) {
            writeline("/home/ubuntu/results/coverage/XmlProperty/XmlProperty_2_10.coverage", "12ef5e2f-8796-4652-bc57-5964b494db06");
            nodeText = "";
            writeline("/home/ubuntu/results/coverage/XmlProperty/XmlProperty_2_10.coverage", "04df41d7-cfb3-4f3a-a970-226e805ab044");
            emptyNode = true;
        } else if (node.getNodeType() == Node.ELEMENT_NODE && node.getChildNodes().getLength() == 1 && node.getFirstChild().getNodeType() == Node.TEXT_NODE && "".equals(node.getFirstChild().getNodeValue()) && !semanticEmptyOverride) {
            writeline("/home/ubuntu/results/coverage/XmlProperty/XmlProperty_2_10.coverage", "275d8134-e766-461f-be8f-f2bfc7056b12");
            nodeText = "";
            writeline("/home/ubuntu/results/coverage/XmlProperty/XmlProperty_2_10.coverage", "73786f37-2d9e-44a4-be24-e71ff30fb99a");
            emptyNode = true;
        }
        writeline("/home/ubuntu/results/coverage/XmlProperty/XmlProperty_2_10.coverage", "72bc406a-dc8e-4b3f-a9e8-9267f9fca090");
        if (nodeText != null) {
            writeline("/home/ubuntu/results/coverage/XmlProperty/XmlProperty_2_10.coverage", "58cced4f-4e01-4583-9e59-c29c9cff9d84");
            if (semanticAttributes && id == null && container instanceof String) {
                writeline("/home/ubuntu/results/coverage/XmlProperty/XmlProperty_2_10.coverage", "62be4737-8a56-4b0e-a775-3eb8773f73c7");
                id = (String) container;
            }
            writeline("/home/ubuntu/results/coverage/XmlProperty/XmlProperty_2_10.coverage", "250bd9b9-1629-4e94-ac7e-207ca98ca5c3");
            if (nodeText.trim().length() != 0 || emptyNode) {
                writeline("/home/ubuntu/results/coverage/XmlProperty/XmlProperty_2_10.coverage", "ab1065ac-82b8-4ab7-aa0a-e42a33c12c9b");
                addProperty(prefix, nodeText, id);
            }
        }
        writeline("/home/ubuntu/results/coverage/XmlProperty/XmlProperty_2_10.coverage", "72cab62a-d3cf-4d10-9c6f-10469f2e893b");
        return (addedPath != null ? addedPath : id);
    }

    /**
     * Actually add the given property/value to the project
     * after writing a log message.
     */
    private void addProperty(String name, String value, String id) {
        writeline("/home/ubuntu/results/coverage/XmlProperty/XmlProperty_2_10.coverage", "546cbc15-688d-4e1e-8885-dcd9db2df9ed");
        String msg = name + ":" + value;
        writeline("/home/ubuntu/results/coverage/XmlProperty/XmlProperty_2_10.coverage", "f099ee7f-a4d8-47fc-9b87-ae0a7577678c");
        if (id != null) {
            writeline("/home/ubuntu/results/coverage/XmlProperty/XmlProperty_2_10.coverage", "b6b7c077-04aa-4247-9d6f-b2e3167a0857");
            msg += ("(id=" + id + ")");
        }
        writeline("/home/ubuntu/results/coverage/XmlProperty/XmlProperty_2_10.coverage", "1eaba404-7972-4900-9e24-5ad980e2ef54");
        log(msg, Project.MSG_DEBUG);
        writeline("/home/ubuntu/results/coverage/XmlProperty/XmlProperty_2_10.coverage", "53233cfd-f53b-4e77-bea5-e3e967f0e3a2");
        if (addedAttributes.containsKey(name)) {
            writeline("/home/ubuntu/results/coverage/XmlProperty/XmlProperty_2_10.coverage", "e332a66d-a59b-4a1f-be75-90351803e5ca");
            value = (String) addedAttributes.get(name) + getDelimiter() + value;
            writeline("/home/ubuntu/results/coverage/XmlProperty/XmlProperty_2_10.coverage", "8923437b-1170-4633-815d-cafdc5717790");
            getProject().setProperty(name, value);
            writeline("/home/ubuntu/results/coverage/XmlProperty/XmlProperty_2_10.coverage", "1a4cac57-c089-44ba-b758-a2dc5e723620");
            addedAttributes.put(name, value);
        } else if (getProject().getProperty(name) == null) {
            writeline("/home/ubuntu/results/coverage/XmlProperty/XmlProperty_2_10.coverage", "bc8ef041-a5f5-499a-9e87-4d94ecb8e764");
            getProject().setNewProperty(name, value);
            writeline("/home/ubuntu/results/coverage/XmlProperty/XmlProperty_2_10.coverage", "503990df-d123-44ae-8b89-2a2d87047608");
            addedAttributes.put(name, value);
        } else {
            writeline("/home/ubuntu/results/coverage/XmlProperty/XmlProperty_2_10.coverage", "41d6da01-0fff-4749-a6a0-e090088042bd");
            log("Override ignored for property " + name, Project.MSG_VERBOSE);
        }
        writeline("/home/ubuntu/results/coverage/XmlProperty/XmlProperty_2_10.coverage", "ba115d8a-f8ce-4478-a3fd-92295af1eb69");
        if (id != null) {
            writeline("/home/ubuntu/results/coverage/XmlProperty/XmlProperty_2_10.coverage", "f9290d2c-958a-468c-8135-fbd3adbfe787");
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
        writeline("/home/ubuntu/results/coverage/XmlProperty/XmlProperty_2_10.coverage", "d32af81f-d44a-4171-a690-8ec1ccf823f1");
        String attributeName = attributeNode.getNodeName();
        writeline("/home/ubuntu/results/coverage/XmlProperty/XmlProperty_2_10.coverage", "c542fd0b-4f64-4ddf-a057-44a7daee176b");
        if (semanticAttributes) {
            writeline("/home/ubuntu/results/coverage/XmlProperty/XmlProperty_2_10.coverage", "6c0653df-e45a-40d3-ac3b-95eb19d0adf1");
            if (attributeName.equals(REF_ID)) {
                writeline("/home/ubuntu/results/coverage/XmlProperty/XmlProperty_2_10.coverage", "4f9e515a-f706-454e-8cc8-19d240339b48");
                return "";
            }
            writeline("/home/ubuntu/results/coverage/XmlProperty/XmlProperty_2_10.coverage", "7842c23c-36ea-4555-a82d-89cc29235cd8");
            if (!isSemanticAttribute(attributeName) || includeSemanticAttribute) {
                writeline("/home/ubuntu/results/coverage/XmlProperty/XmlProperty_2_10.coverage", "3b9c267b-0c4d-439f-b081-3e2994a6972e");
                return "." + attributeName;
            }
            writeline("/home/ubuntu/results/coverage/XmlProperty/XmlProperty_2_10.coverage", "205f99b1-be2d-4f7d-8da4-669687f68612");
            return "";
        }
        writeline("/home/ubuntu/results/coverage/XmlProperty/XmlProperty_2_10.coverage", "85f9e79b-bb26-411d-9ec8-5f2086275616");
        return collapseAttributes ? "." + attributeName : "(" + attributeName + ")";
    }

    /**
     * Return whether the provided attribute name is recognized or not.
     */
    private static boolean isSemanticAttribute(String attributeName) {
        writeline("/home/ubuntu/results/coverage/XmlProperty/XmlProperty_2_10.coverage", "191b3723-bbf9-40a9-89b4-28db0ffb2a3c");
        for (int i = 0; i < ATTRIBUTES.length; i++) {
            writeline("/home/ubuntu/results/coverage/XmlProperty/XmlProperty_2_10.coverage", "da1400b2-fea9-4586-8bd8-cb0813c49522");
            if (attributeName.equals(ATTRIBUTES[i])) {
                writeline("/home/ubuntu/results/coverage/XmlProperty/XmlProperty_2_10.coverage", "0b6ca621-33a8-430b-85da-41e94377b64d");
                return true;
            }
        }
        writeline("/home/ubuntu/results/coverage/XmlProperty/XmlProperty_2_10.coverage", "961f39c1-3e4d-43db-bbde-bac770667b4b");
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
        writeline("/home/ubuntu/results/coverage/XmlProperty/XmlProperty_2_10.coverage", "ed941b69-1aab-494d-aa05-be8b8aeb5b1a");
        String nodeValue = attributeNode.getNodeValue().trim();
        writeline("/home/ubuntu/results/coverage/XmlProperty/XmlProperty_2_10.coverage", "e1cb41d7-0268-48df-bb50-879afb739f2f");
        if (semanticAttributes) {
            writeline("/home/ubuntu/results/coverage/XmlProperty/XmlProperty_2_10.coverage", "c968db3c-cfda-4e78-adae-c87bea3350e7");
            String attributeName = attributeNode.getNodeName();
            writeline("/home/ubuntu/results/coverage/XmlProperty/XmlProperty_2_10.coverage", "8c911cd5-8348-4f36-869c-690978447bd6");
            nodeValue = getProject().replaceProperties(nodeValue);
            writeline("/home/ubuntu/results/coverage/XmlProperty/XmlProperty_2_10.coverage", "d1cf26ff-c17b-4c3e-b08c-0ce5cabf779c");
            if (attributeName.equals(LOCATION)) {
                writeline("/home/ubuntu/results/coverage/XmlProperty/XmlProperty_2_10.coverage", "5a6e8753-8ea9-4a53-b9c4-3d97e49b9ce1");
                File f = resolveFile(nodeValue);
                writeline("/home/ubuntu/results/coverage/XmlProperty/XmlProperty_2_10.coverage", "1dd0d2ff-ded3-4d90-95f4-c4f782133322");
                return f.getPath();
            }
            writeline("/home/ubuntu/results/coverage/XmlProperty/XmlProperty_2_10.coverage", "83fba91a-855c-4bbe-8983-31a2a350b2ea");
            if (attributeName.equals(REF_ID)) {
                writeline("/home/ubuntu/results/coverage/XmlProperty/XmlProperty_2_10.coverage", "88803064-efad-4ec9-afe4-b7bbbfe09a3b");
                Object ref = getProject().getReference(nodeValue);
                writeline("/home/ubuntu/results/coverage/XmlProperty/XmlProperty_2_10.coverage", "ff87b713-934e-4843-aef8-7f37073b870f");
                if (ref != null) {
                    writeline("/home/ubuntu/results/coverage/XmlProperty/XmlProperty_2_10.coverage", "85f2cef6-f880-4f3d-a02c-239234c714e4");
                    return ref.toString();
                }
            }
        }
        writeline("/home/ubuntu/results/coverage/XmlProperty/XmlProperty_2_10.coverage", "f8684a2b-1b07-47dd-84a5-80d41a06c5d3");
        return nodeValue;
    }

    /**
     * The XML file to parse; required.
     * @param src the file to parse
     */
    public void setFile(File src) {
        writeline("/home/ubuntu/results/coverage/XmlProperty/XmlProperty_2_10.coverage", "fd04d4d3-72ce-4e29-85a7-36ca066a9ba1");
        setSrcResource(new FileResource(src));
    }

    /**
     * The resource to pack; required.
     * @param src resource to expand
     */
    public void setSrcResource(Resource src) {
        writeline("/home/ubuntu/results/coverage/XmlProperty/XmlProperty_2_10.coverage", "a495905e-cffb-461e-b606-c4b748622ee1");
        if (src.isDirectory()) {
        }
        writeline("/home/ubuntu/results/coverage/XmlProperty/XmlProperty_2_10.coverage", "300e0c55-0af1-4ae7-aefb-db8b62cc8b0d");
        if (src.as(FileProvider.class) != null || supportsNonFileResources()) {
            writeline("/home/ubuntu/results/coverage/XmlProperty/XmlProperty_2_10.coverage", "f07fb2fe-290b-4138-a2d1-203759fadb43");
            this.src = src;
        } else {
        }
    }

    /**
     * Set the source resource.
     * @param a the resource to pack as a single element Resource collection.
     */
    public void addConfigured(ResourceCollection a) {
        writeline("/home/ubuntu/results/coverage/XmlProperty/XmlProperty_2_10.coverage", "8c5404da-43db-4221-872c-db7eb5647663");
        if (a.size() != 1) {
        }
        writeline("/home/ubuntu/results/coverage/XmlProperty/XmlProperty_2_10.coverage", "b966ae6f-ae4a-4e00-a554-e19a97195b3f");
        setSrcResource(a.iterator().next());
    }

    /**
     * the prefix to prepend to each property
     * @param prefix the prefix to prepend to each property
     */
    public void setPrefix(String prefix) {
        writeline("/home/ubuntu/results/coverage/XmlProperty/XmlProperty_2_10.coverage", "4b61669f-c041-4db2-a33c-a115fb5b71d5");
        this.prefix = prefix.trim();
    }

    /**
     * flag to include the xml root tag as a
     * first value in the property name; optional,
     * default is true
     * @param keepRoot if true (default), include the xml root tag
     */
    public void setKeeproot(boolean keepRoot) {
        writeline("/home/ubuntu/results/coverage/XmlProperty/XmlProperty_2_10.coverage", "2dc71971-5d87-46fa-a908-7b741f04d8d1");
        this.keepRoot = keepRoot;
    }

    /**
     * flag to validate the XML file; optional, default false
     * @param validate if true validate the XML file, default false
     */
    public void setValidate(boolean validate) {
        writeline("/home/ubuntu/results/coverage/XmlProperty/XmlProperty_2_10.coverage", "1ddc28fe-99c6-4c6f-ac9e-af6320a7d00f");
        this.validate = validate;
    }

    /**
     * flag to treat attributes as nested elements;
     * optional, default false
     * @param collapseAttributes if true treat attributes as nested elements
     */
    public void setCollapseAttributes(boolean collapseAttributes) {
        writeline("/home/ubuntu/results/coverage/XmlProperty/XmlProperty_2_10.coverage", "ec9c2850-0337-442d-8feb-cc69c5d43294");
        this.collapseAttributes = collapseAttributes;
    }

    /**
     * Attribute to enable special handling of attributes - see ant manual.
     * @param semanticAttributes if true enable the special handling.
     */
    public void setSemanticAttributes(boolean semanticAttributes) {
        writeline("/home/ubuntu/results/coverage/XmlProperty/XmlProperty_2_10.coverage", "dbb10b7e-2bcb-4567-8a1f-55db176099f4");
        this.semanticAttributes = semanticAttributes;
    }

    /**
     * The directory to use for resolving file references.
     * Ignored if semanticAttributes is not set to true.
     * @param rootDirectory the directory.
     */
    public void setRootDirectory(File rootDirectory) {
        writeline("/home/ubuntu/results/coverage/XmlProperty/XmlProperty_2_10.coverage", "5030b8e1-8b22-4a0a-8daa-896f43a33489");
        this.rootDirectory = rootDirectory;
    }

    /**
     * Include the semantic attribute name as part of the property name.
     * Ignored if semanticAttributes is not set to true.
     * @param includeSemanticAttribute if true include the semantic attribute
     *                                 name.
     */
    public void setIncludeSemanticAttribute(boolean includeSemanticAttribute) {
        writeline("/home/ubuntu/results/coverage/XmlProperty/XmlProperty_2_10.coverage", "e72574c4-6233-40e2-bc5d-4cdadc1af0e6");
        this.includeSemanticAttribute = includeSemanticAttribute;
    }

    /**
     * add an XMLCatalog as a nested element; optional.
     * @param catalog the XMLCatalog to use
     */
    public void addConfiguredXMLCatalog(XMLCatalog catalog) {
        writeline("/home/ubuntu/results/coverage/XmlProperty/XmlProperty_2_10.coverage", "cb38d8c1-0237-4ae8-9236-e63252793f43");
        xmlCatalog.addConfiguredXMLCatalog(catalog);
    }

    /**
     * @return the file attribute.
     */
    protected File getFile() {
        writeline("/home/ubuntu/results/coverage/XmlProperty/XmlProperty_2_10.coverage", "6179d8f8-0a07-4d5a-b71e-fe52fb5b6c49");
        FileProvider fp = src.as(FileProvider.class);
        writeline("/home/ubuntu/results/coverage/XmlProperty/XmlProperty_2_10.coverage", "25fdbd69-ed07-4cb7-a588-3e8f2243fd76");
        return fp != null ? fp.getFile() : null;
    }

    /**
     * @return the resource.
     */
    protected Resource getResource() {
        writeline("/home/ubuntu/results/coverage/XmlProperty/XmlProperty_2_10.coverage", "77628d44-e999-46ac-a874-5c5d536d7a80");
        File f = getFile();
        writeline("/home/ubuntu/results/coverage/XmlProperty/XmlProperty_2_10.coverage", "858f2a84-a16f-4a7d-ac35-49b0e59ec5d6");
        FileProvider fp = src.as(FileProvider.class);
        writeline("/home/ubuntu/results/coverage/XmlProperty/XmlProperty_2_10.coverage", "394d3d12-0d37-45a0-b476-3cbc20b87083");
        return f == null ? src : fp != null && fp.getFile().equals(f) ? src : new FileResource(f);
    }

    /**
     * @return the prefix attribute.
     */
    protected String getPrefix() {
        writeline("/home/ubuntu/results/coverage/XmlProperty/XmlProperty_2_10.coverage", "876779d3-f2da-4c4b-89fd-819e9c083103");
        return this.prefix;
    }

    /**
     * @return the keeproot attribute.
     */
    protected boolean getKeeproot() {
        writeline("/home/ubuntu/results/coverage/XmlProperty/XmlProperty_2_10.coverage", "9e85659c-02c5-4b5e-b149-5f1f0ab5296c");
        return this.keepRoot;
    }

    /**
     * @return the validate attribute.
     */
    protected boolean getValidate() {
        writeline("/home/ubuntu/results/coverage/XmlProperty/XmlProperty_2_10.coverage", "b600e804-817c-4401-bb58-78e54cd82fbf");
        return this.validate;
    }

    /**
     * @return the collapse attributes attribute.
     */
    protected boolean getCollapseAttributes() {
        writeline("/home/ubuntu/results/coverage/XmlProperty/XmlProperty_2_10.coverage", "3cbf9e04-37f2-4531-b679-ac76f043be01");
        return this.collapseAttributes;
    }

    /**
     * @return the semantic attributes attribute.
     */
    protected boolean getSemanticAttributes() {
        writeline("/home/ubuntu/results/coverage/XmlProperty/XmlProperty_2_10.coverage", "c463d197-58f0-46c4-a88b-8e834bacd08f");
        return this.semanticAttributes;
    }

    /**
     * @return the root directory attribute.
     */
    protected File getRootDirectory() {
        writeline("/home/ubuntu/results/coverage/XmlProperty/XmlProperty_2_10.coverage", "c673f47f-9fd9-4789-a9eb-3aa01ecb28ff");
        return this.rootDirectory;
    }

    /**
     * @return the include semantic attribute.
     */
    protected boolean getIncludeSementicAttribute() {
        writeline("/home/ubuntu/results/coverage/XmlProperty/XmlProperty_2_10.coverage", "d8613f13-3045-4c5f-9472-492544595539");
        return this.includeSemanticAttribute;
    }

    /**
     * Let project resolve the file - or do it ourselves if
     * rootDirectory has been set.
     */
    private File resolveFile(String fileName) {
        writeline("/home/ubuntu/results/coverage/XmlProperty/XmlProperty_2_10.coverage", "ed087795-e070-4883-854c-c129ce1b7d2e");
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
        writeline("/home/ubuntu/results/coverage/XmlProperty/XmlProperty_2_10.coverage", "a50e086e-c94c-4e05-87a9-2bbf63c9c3eb");
        return getClass().equals(XmlProperty.class);
    }

    /**
     * Get the current delimiter.
     * @return delimiter
     */
    public String getDelimiter() {
        writeline("/home/ubuntu/results/coverage/XmlProperty/XmlProperty_2_10.coverage", "1e306cd2-067a-4d43-8294-92fc2ff7d63c");
        return delimiter;
    }

    /**
     * Sets a new delimiter.
     * @param delimiter new value
     * @since Ant 1.7.1
     */
    public void setDelimiter(String delimiter) {
        writeline("/home/ubuntu/results/coverage/XmlProperty/XmlProperty_2_10.coverage", "a5cd27d0-e3d8-45f9-8079-369237f20566");
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
