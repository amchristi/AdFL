package org.apache.tools.ant.taskdefs;

import java.io.File;
import java.io.InputStream;
import org.apache.tools.ant.AntClassLoader;
import org.apache.tools.ant.BuildException;
import org.apache.tools.ant.Project;
import org.apache.tools.ant.PropertyHelper;
import org.apache.tools.ant.Task;
import org.apache.tools.ant.taskdefs.condition.Condition;
import org.apache.tools.ant.types.EnumeratedAttribute;
import org.apache.tools.ant.types.Path;
import org.apache.tools.ant.types.Reference;
import org.apache.tools.ant.util.FileUtils;
import org.apache.tools.ant.util.StringUtils;

/**
 * Will set the given property if the requested resource is available at
 * runtime. This task may also be used as a condition by the condition task.
 *
 * @since Ant 1.1
 *
 * @ant.task category="control"
 */
public class Available extends Task implements Condition {

    private static final FileUtils FILE_UTILS = FileUtils.getFileUtils();

    private String property;

    private String classname;

    private String filename;

    private File file;

    private Path filepath;

    private String resource;

    private FileDir type;

    private Path classpath;

    private AntClassLoader loader;

    private Object value = "true";

    private boolean isTask = false;

    private boolean ignoreSystemclasses = false;

    private boolean searchParents = false;

    /**
     * Set the searchParents attribute.
     * This controls the behaviour of the the "file" type.
     * If true, the path, parent path and grandparent path are
     * searched for the file. If false, only the path is searched.
     * The default value is false.
     * @param searchParents the value to set.
     */
    public void setSearchParents(boolean searchParents) {
        this.searchParents = searchParents;
    }

    /**
     * Set the classpath to be used when searching for classes and resources.
     *
     * @param classpath an Ant Path object containing the search path.
     */
    public void setClasspath(Path classpath) {
        createClasspath().append(classpath);
    }

    /**
     * Classpath to be used when searching for classes and resources.
     *
     * @return an empty Path instance to be configured by Ant.
     */
    public Path createClasspath() {
        if (this.classpath == null) {
            this.classpath = new Path(getProject());
        }
        return this.classpath.createPath();
    }

    /**
     * Set the classpath by reference.
     *
     * @param r a Reference to a Path instance to be used as the classpath
     *          value.
     */
    public void setClasspathRef(Reference r) {
    }

    /**
     * Set the path to use when looking for a file.
     *
     * @param filepath a Path instance containing the search path for files.
     */
    public void setFilepath(Path filepath) {
        createFilepath().append(filepath);
    }

    /**
     * Path to search for file resources.
     *
     * @return a new Path instance which Ant will configure with a file search
     *         path.
     */
    public Path createFilepath() {
        if (this.filepath == null) {
            this.filepath = new Path(getProject());
        }
        return this.filepath.createPath();
    }

    /**
     * Set the name of the property which will be set if the particular resource
     * is available.
     *
     * @param property the name of the property to set.
     */
    public void setProperty(String property) {
        this.property = property;
    }

    /**
     * Set the value to be given to the property if the desired resource is
     * available.
     *
     * @param value the value to be given.
     */
    public void setValue(Object value) {
    }

    /**
     * Set the value to be given to the property if the desired resource is
     * available.
     *
     * @param value the value to be given.
     */
    public void setValue(String value) {
        setValue((Object) value);
    }

    /**
     * Set a classname of a class which must be available to set the given
     * property.
     *
     * @param classname the name of the class required.
     */
    public void setClassname(String classname) {
        if (!"".equals(classname)) {
            this.classname = classname;
        }
    }

    /**
     * Set the file which must be present in the file system to set the given
     * property.
     *
     * @param file the name of the file which is required.
     */
    public void setFile(File file) {
        this.file = file;
        this.filename = FILE_UTILS.removeLeadingPath(getProject().getBaseDir(), file);
    }

    /**
     * Set the name of a Java resource which is required to set the property.
     *
     * @param resource the name of a resource which is required to be available.
     */
    public void setResource(String resource) {
        this.resource = resource;
    }

    /**
     * @deprecated since 1.5.x.
     *             setType(String) is deprecated and is replaced with
     *             setType(Available.FileDir) to make Ant's Introspection
     *             mechanism do the work and also to encapsulate operations on
     *             the type in its own class.
     * @param type the type of resource
     */
    public void setType(String type) {
    }

    /**
     * Set what type of file is required - either directory or file.
     *
     * @param type an instance of the FileDir enumeratedAttribute indicating
     *             whether the file required is to be a directory or a plain
     *             file.
     */
    public void setType(FileDir type) {
        this.type = type;
    }

    /**
     * Set whether the search for classes should ignore the runtime classes and
     * just use the given classpath.
     *
     * @param ignore true if system classes are to be ignored.
     */
    public void setIgnoresystemclasses(boolean ignore) {
        this.ignoreSystemclasses = ignore;
    }

    /**
     * Entry point when operating as a task.
     *
     * @exception BuildException if the task is not configured correctly.
     */
    public void execute() throws BuildException {
        if (property == null) {
            throw new BuildException("property attribute is required", getLocation());
        }
        try {
            if (eval()) {
                PropertyHelper ph = PropertyHelper.getPropertyHelper(getProject());
                Object oldvalue = ph.getProperty(property);
                if (null != oldvalue && !oldvalue.equals(value)) {
                    log("DEPRECATED - <available> used to override an existing" + " property." + StringUtils.LINE_SEP + "  Build file should not reuse the same property" + " name for different values.", Project.MSG_WARN);
                }
                ph.setProperty(property, value, true);
            }
        } finally {
            isTask = false;
        }
    }

    /**
     * Evaluate the availability of a resource.
     *
     * @return boolean is the resource is available.
     * @exception BuildException if the condition is not configured correctly
     */
    public boolean eval() throws BuildException {
        try {
            if (classname == null && file == null && resource == null) {
                throw new BuildException("At least one of (classname|file|" + "resource) is required", getLocation());
            }
            if (type != null) {
                if (file == null) {
                    throw new BuildException("The type attribute is only valid " + "when specifying the file " + "attribute.", getLocation());
                }
            }
            if (classpath != null) {
                classpath.setProject(getProject());
                this.loader = getProject().createClassLoader(classpath);
            }
            String appendix = "";
            if (isTask) {
                appendix = " to set property " + property;
            } else {
                setTaskName("available");
            }
            if ((classname != null) && !checkClass(classname)) {
                log("Unable to load class " + classname + appendix, Project.MSG_VERBOSE);
                return false;
            }
            if ((file != null) && !checkFile()) {
                StringBuffer buf = new StringBuffer("Unable to find ");
                if (type != null) {
                    buf.append(type).append(' ');
                }
                buf.append(filename).append(appendix);
                log(buf.toString(), Project.MSG_VERBOSE);
                return false;
            }
            if ((resource != null) && !checkResource(resource)) {
                log("Unable to load resource " + resource + appendix, Project.MSG_VERBOSE);
                return false;
            }
        } finally {
            if (loader != null) {
                loader.cleanup();
                loader = null;
            }
            if (!isTask) {
                setTaskName(null);
            }
        }
        return true;
    }

    /**
     * Search for file/directory either relative to project's
     * basedir or in the path given as filepath.
     *
     * <p>filepath can be a list of directory and/or file names (gen'd
     * via <fileset>)</p>
     *
     * <p>look for:</p><ul>
     *   <li>full-pathname specified == path in list</li>
     *   <li>full-pathname specified == parent dir of path in list</li>
     *   <li>simple name specified   == path in list</li>
     *   <li>simple name specified   == path in list + name</li>
     *   <li>simple name specified   == parent dir + name</li>
     *   <li>simple name specified   == parent of parent dir + name</li>
     * </ul>
     */
    private boolean checkFile() {
        if (filepath == null) {
            return checkFile(file, filename);
        } else {
            String[] paths = filepath.list();
            for (int i = 0; i < paths.length; ++i) {
                log("Searching " + paths[i], Project.MSG_VERBOSE);
                File path = new File(paths[i]);
                if (path.exists() && (filename.equals(paths[i]) || filename.equals(path.getName()))) {
                    return false;
                }
                File parent = path.getParentFile();
                if (parent != null && parent.exists() && filename.equals(parent.getAbsolutePath())) {
                    return false;
                }
                if (path.exists() && path.isDirectory()) {
                    if (checkFile(new File(path, filename), filename + " in " + path)) {
                        return true;
                    }
                }
                while (searchParents && parent != null && parent.exists()) {
                    if (checkFile(new File(parent, filename), filename + " in " + parent)) {
                        return true;
                    }
                    parent = parent.getParentFile();
                }
            }
        }
        return false;
    }

    /**
     * Check if a given file exists and matches the required type.
     */
    private boolean checkFile(File f, String text) {
        if (type != null) {
            if (type.isDir()) {
                if (f.isDirectory()) {
                    log("Found directory: " + text, Project.MSG_VERBOSE);
                }
                return f.isDirectory();
            } else if (type.isFile()) {
                if (f.isFile()) {
                    log("Found file: " + text, Project.MSG_VERBOSE);
                }
                return f.isFile();
            }
        }
        if (f.exists()) {
            log("Found: " + text, Project.MSG_VERBOSE);
        }
        return f.exists();
    }

    /**
     * Check if a given resource can be loaded.
     */
    private boolean checkResource(String resource) {
        InputStream is = null;
        try {
            if (loader != null) {
                is = loader.getResourceAsStream(resource);
            } else {
                ClassLoader cL = this.getClass().getClassLoader();
                if (cL != null) {
                    is = cL.getResourceAsStream(resource);
                } else {
                    is = ClassLoader.getSystemResourceAsStream(resource);
                }
            }
            return is != null;
        } finally {
            FileUtils.close(is);
        }
    }

    /**
     * Check if a given class can be loaded.
     */
    private boolean checkClass(String classname) {
        try {
            if (ignoreSystemclasses) {
                loader = getProject().createClassLoader(classpath);
                loader.setParentFirst(false);
                loader.addJavaLibraries();
                try {
                    loader.findClass(classname);
                } catch (SecurityException se) {
                    return true;
                }
            } else if (loader != null) {
                loader.loadClass(classname);
            } else {
                ClassLoader l = this.getClass().getClassLoader();
                if (l != null) {
                    Class.forName(classname, true, l);
                } else {
                    Class.forName(classname);
                }
            }
            return true;
        } catch (ClassNotFoundException e) {
            log("class \"" + classname + "\" was not found", Project.MSG_DEBUG);
            return false;
        } catch (NoClassDefFoundError e) {
            log("Could not load dependent class \"" + e.getMessage() + "\" for class \"" + classname + "\"", Project.MSG_DEBUG);
            return false;
        }
    }
}
