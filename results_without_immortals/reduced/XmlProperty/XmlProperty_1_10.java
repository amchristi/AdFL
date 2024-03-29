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
        xmlCatalog.setProject(getProject());
    }

    /**
     * @return the xmlCatalog as the entityresolver.
     */
    protected EntityResolver getEntityResolver() {
        return xmlCatalog;
    }

    /**
     * Run the task.
     * @throws BuildException The exception raised during task execution.
     * @todo validate the source file is valid before opening, print a better error message
     * @todo add a verbose level log message listing the name of the file being loaded
     */
    public void execute() throws BuildException {
        Resource r = getResource();
        if (r == null) {
        }
        try {
            if (r.isExists()) {
                DocumentBuilderFactory factory = DocumentBuilderFactory.newInstance();
                DocumentBuilder builder = factory.newDocumentBuilder();
                Document document = null;
                FileProvider fp = src.as(FileProvider.class);
                if (fp != null) {
                    document = builder.parse(fp.getFile());
                } else {
                    document = builder.parse(src.getInputStream());
                }
                Element topElement = document.getDocumentElement();
                if (keepRoot) {
                    addNodeRecursively(topElement, prefix, null);
                } else {
                    NodeList topChildren = topElement.getChildNodes();
                    int numChildren = topChildren.getLength();
                    for (int i = 0; i < numChildren; i++) {
                        addNodeRecursively(topChildren.item(i), prefix, null);
                    }
                }
            } else {
            }
        } catch (SAXException sxe) {
            Exception x = sxe;
        } catch (ParserConfigurationException pce) {
        } catch (IOException ioe) {
        }
    }

    /** Iterate through all nodes in the tree. */
    private void addNodeRecursively(Node node, String prefix, Object container) {
        String nodePrefix = prefix;
        if (node.getNodeType() != Node.TEXT_NODE) {
            if (prefix.trim().length() > 0) {
                nodePrefix += ".";
            }
            nodePrefix += node.getNodeName();
        }
        Object nodeObject = processNode(node, nodePrefix, container);
        if (node.hasChildNodes()) {
            NodeList nodeChildren = node.getChildNodes();
            int numChildren = nodeChildren.getLength();
            for (int i = 0; i < numChildren; i++) {
                addNodeRecursively(nodeChildren.item(i), nodePrefix, nodeObject);
            }
        }
    }

    void addNodeRecursively(org.w3c.dom.Node node, String prefix) {
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
        Object addedPath = null;
        String id = null;
        if (node.hasAttributes()) {
            NamedNodeMap nodeAttributes = node.getAttributes();
            Node idNode = nodeAttributes.getNamedItem(ID);
            id = semanticAttributes && idNode != null ? idNode.getNodeValue() : null;
            for (int i = 0; i < nodeAttributes.getLength(); i++) {
                Node attributeNode = nodeAttributes.item(i);
                if (!semanticAttributes) {
                    String attributeName = getAttributeName(attributeNode);
                    String attributeValue = getAttributeValue(attributeNode);
                    addProperty(prefix + attributeName, attributeValue, null);
                } else {
                    String nodeName = attributeNode.getNodeName();
                    String attributeValue = getAttributeValue(attributeNode);
                    Path containingPath = ((container != null) && (container instanceof Path)) ? (Path) container : null;
                    if (nodeName.equals(ID)) {
                        continue;
                    }
                    if (containingPath != null && nodeName.equals(PATH)) {
                    } else if (containingPath != null && container instanceof Path && nodeName.equals(REF_ID)) {
                    } else if (containingPath != null && container instanceof Path && nodeName.equals(LOCATION)) {
                    } else if (nodeName.equals(PATHID)) {
                        addedPath = new Path(getProject());
                        getProject().addReference(attributeValue, addedPath);
                    } else {
                        String attributeName = getAttributeName(attributeNode);
                        addProperty(prefix + attributeName, attributeValue, id);
                    }
                }
            }
        }
        String nodeText = null;
        boolean emptyNode = false;
        boolean semanticEmptyOverride = false;
        if (node.getNodeType() == Node.TEXT_NODE) {
            nodeText = getAttributeValue(node);
        } else if (node.getNodeType() == Node.ELEMENT_NODE && node.getChildNodes().getLength() == 1 && node.getFirstChild().getNodeType() == Node.CDATA_SECTION_NODE) {
            nodeText = node.getFirstChild().getNodeValue();
        } else if (node.getNodeType() == Node.ELEMENT_NODE && node.getChildNodes().getLength() == 0 && !semanticEmptyOverride) {
        } else if (node.getNodeType() == Node.ELEMENT_NODE && node.getChildNodes().getLength() == 1 && node.getFirstChild().getNodeType() == Node.TEXT_NODE && "".equals(node.getFirstChild().getNodeValue()) && !semanticEmptyOverride) {
        }
        if (nodeText != null) {
            if (nodeText.trim().length() != 0 || emptyNode) {
                addProperty(prefix, nodeText, id);
            }
        }
        return (addedPath != null ? addedPath : id);
    }

    /**
     * Actually add the given property/value to the project
     * after writing a log message.
     */
    private void addProperty(String name, String value, String id) {
        if (addedAttributes.containsKey(name)) {
            value = (String) addedAttributes.get(name) + getDelimiter() + value;
            getProject().setProperty(name, value);
            addedAttributes.put(name, value);
        } else if (getProject().getProperty(name) == null) {
            getProject().setNewProperty(name, value);
            addedAttributes.put(name, value);
        } else {
        }
        if (id != null) {
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
        String attributeName = attributeNode.getNodeName();
        if (semanticAttributes) {
            if (!isSemanticAttribute(attributeName) || includeSemanticAttribute) {
                return "." + attributeName;
            }
            return "";
        }
        return collapseAttributes ? "." + attributeName : "(" + attributeName + ")";
    }

    /**
     * Return whether the provided attribute name is recognized or not.
     */
    private static boolean isSemanticAttribute(String attributeName) {
        for (int i = 0; i < ATTRIBUTES.length; i++) {
            if (attributeName.equals(ATTRIBUTES[i])) {
                return true;
            }
        }
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
        String nodeValue = attributeNode.getNodeValue().trim();
        if (semanticAttributes) {
            String attributeName = attributeNode.getNodeName();
            nodeValue = getProject().replaceProperties(nodeValue);
            if (attributeName.equals(LOCATION)) {
                File f = resolveFile(nodeValue);
                return f.getPath();
            }
            if (attributeName.equals(REF_ID)) {
                Object ref = getProject().getReference(nodeValue);
                if (ref != null) {
                    return ref.toString();
                }
            }
        }
        return nodeValue;
    }

    /**
     * The XML file to parse; required.
     * @param src the file to parse
     */
    public void setFile(File src) {
        setSrcResource(new FileResource(src));
    }

    /**
     * The resource to pack; required.
     * @param src resource to expand
     */
    public void setSrcResource(Resource src) {
        if (src.as(FileProvider.class) != null || supportsNonFileResources()) {
            this.src = src;
        } else {
        }
    }

    /**
     * Set the source resource.
     * @param a the resource to pack as a single element Resource collection.
     */
    public void addConfigured(ResourceCollection a) {
        setSrcResource(a.iterator().next());
    }

    /**
     * the prefix to prepend to each property
     * @param prefix the prefix to prepend to each property
     */
    public void setPrefix(String prefix) {
    }

    /**
     * flag to include the xml root tag as a
     * first value in the property name; optional,
     * default is true
     * @param keepRoot if true (default), include the xml root tag
     */
    public void setKeeproot(boolean keepRoot) {
        this.keepRoot = keepRoot;
    }

    /**
     * flag to validate the XML file; optional, default false
     * @param validate if true validate the XML file, default false
     */
    public void setValidate(boolean validate) {
    }

    /**
     * flag to treat attributes as nested elements;
     * optional, default false
     * @param collapseAttributes if true treat attributes as nested elements
     */
    public void setCollapseAttributes(boolean collapseAttributes) {
        this.collapseAttributes = collapseAttributes;
    }

    /**
     * Attribute to enable special handling of attributes - see ant manual.
     * @param semanticAttributes if true enable the special handling.
     */
    public void setSemanticAttributes(boolean semanticAttributes) {
        this.semanticAttributes = semanticAttributes;
    }

    /**
     * The directory to use for resolving file references.
     * Ignored if semanticAttributes is not set to true.
     * @param rootDirectory the directory.
     */
    public void setRootDirectory(File rootDirectory) {
    }

    /**
     * Include the semantic attribute name as part of the property name.
     * Ignored if semanticAttributes is not set to true.
     * @param includeSemanticAttribute if true include the semantic attribute
     *                                 name.
     */
    public void setIncludeSemanticAttribute(boolean includeSemanticAttribute) {
        this.includeSemanticAttribute = includeSemanticAttribute;
    }

    /**
     * add an XMLCatalog as a nested element; optional.
     * @param catalog the XMLCatalog to use
     */
    public void addConfiguredXMLCatalog(XMLCatalog catalog) {
    }

    /**
     * @return the file attribute.
     */
    protected File getFile() {
        FileProvider fp = src.as(FileProvider.class);
        return fp != null ? fp.getFile() : null;
    }

    /**
     * @return the resource.
     */
    protected Resource getResource() {
        File f = getFile();
        FileProvider fp = src.as(FileProvider.class);
        return f == null ? src : fp != null && fp.getFile().equals(f) ? src : new FileResource(f);
    }

    /**
     * @return the prefix attribute.
     */
    protected String getPrefix() {
        return this.prefix;
    }

    /**
     * @return the keeproot attribute.
     */
    protected boolean getKeeproot() {
        return this.keepRoot;
    }

    /**
     * @return the validate attribute.
     */
    protected boolean getValidate() {
        return this.validate;
    }

    /**
     * @return the collapse attributes attribute.
     */
    protected boolean getCollapseAttributes() {
        return this.collapseAttributes;
    }

    /**
     * @return the semantic attributes attribute.
     */
    protected boolean getSemanticAttributes() {
        return this.semanticAttributes;
    }

    /**
     * @return the root directory attribute.
     */
    protected File getRootDirectory() {
        return this.rootDirectory;
    }

    /**
     * @return the include semantic attribute.
     */
    protected boolean getIncludeSementicAttribute() {
        return this.includeSemanticAttribute;
    }

    /**
     * Let project resolve the file - or do it ourselves if
     * rootDirectory has been set.
     */
    private File resolveFile(String fileName) {
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
        return getClass().equals(XmlProperty.class);
    }

    /**
     * Get the current delimiter.
     * @return delimiter
     */
    public String getDelimiter() {
        return delimiter;
    }

    /**
     * Sets a new delimiter.
     * @param delimiter new value
     * @since Ant 1.7.1
     */
    public void setDelimiter(String delimiter) {
    }
}
