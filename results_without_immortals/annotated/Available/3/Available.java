/*
 *  Licensed to the Apache Software Foundation (ASF) under one or more
 *  contributor license agreements.  See the NOTICE file distributed with
 *  this work for additional information regarding copyright ownership.
 *  The ASF licenses this file to You under the Apache License, Version 2.0
 *  (the "License"); you may not use this file except in compliance with
 *  the License.  You may obtain a copy of the License at
 *
 *      http://www.apache.org/licenses/LICENSE-2.0
 *
 *  Unless required by applicable law or agreed to in writing, software
 *  distributed under the License is distributed on an "AS IS" BASIS,
 *  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *  See the License for the specific language governing permissions and
 *  limitations under the License.
 *
 */
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
import java.io.*;

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
        writeline("/home/ubuntu/results/coverage/Available/Available_3_10.coverage", "2228fe34-11cf-4d2a-8f2d-0722f2063477");
        this.searchParents = searchParents;
    }

    /**
     * Set the classpath to be used when searching for classes and resources.
     *
     * @param classpath an Ant Path object containing the search path.
     */
    public void setClasspath(Path classpath) {
        writeline("/home/ubuntu/results/coverage/Available/Available_3_10.coverage", "4c6486d3-69c7-40ba-b2b3-8edcfa94edad");
        createClasspath().append(classpath);
    }

    /**
     * Classpath to be used when searching for classes and resources.
     *
     * @return an empty Path instance to be configured by Ant.
     */
    public Path createClasspath() {
        writeline("/home/ubuntu/results/coverage/Available/Available_3_10.coverage", "c8eebacd-9aec-4fa0-8a45-7f274a7766bb");
        if (this.classpath == null) {
            writeline("/home/ubuntu/results/coverage/Available/Available_3_10.coverage", "8976d62a-ef40-42fa-bfba-31191c477600");
            this.classpath = new Path(getProject());
        }
        writeline("/home/ubuntu/results/coverage/Available/Available_3_10.coverage", "16f82cd0-6e9c-4387-9650-10163f9d9591");
        return this.classpath.createPath();
    }

    /**
     * Set the classpath by reference.
     *
     * @param r a Reference to a Path instance to be used as the classpath
     * value.
     */
    public void setClasspathRef(Reference r) {
        writeline("/home/ubuntu/results/coverage/Available/Available_3_10.coverage", "589e77c5-2107-4dc9-ae97-52e06bdaa1b0");
        createClasspath().setRefid(r);
    }

    /**
     * Set the path to use when looking for a file.
     *
     * @param filepath a Path instance containing the search path for files.
     */
    public void setFilepath(Path filepath) {
        writeline("/home/ubuntu/results/coverage/Available/Available_3_10.coverage", "69f1cc7c-f32a-4fd4-8478-d1584be4985b");
        createFilepath().append(filepath);
    }

    /**
     * Path to search for file resources.
     *
     * @return a new Path instance which Ant will configure with a file search
     * path.
     */
    public Path createFilepath() {
        writeline("/home/ubuntu/results/coverage/Available/Available_3_10.coverage", "13eab9dc-a276-4261-a78d-2fc2a980bf23");
        if (this.filepath == null) {
            writeline("/home/ubuntu/results/coverage/Available/Available_3_10.coverage", "78264c88-f8a1-4e34-ad07-c3ee736b649e");
            this.filepath = new Path(getProject());
        }
        writeline("/home/ubuntu/results/coverage/Available/Available_3_10.coverage", "a3c33681-68be-401a-bfde-4bc6e12ce77d");
        return this.filepath.createPath();
    }

    /**
     * Set the name of the property which will be set if the particular resource
     * is available.
     *
     * @param property the name of the property to set.
     */
    public void setProperty(String property) {
        writeline("/home/ubuntu/results/coverage/Available/Available_3_10.coverage", "c776eb8c-d5d9-4af1-b987-769e5fab4fd7");
        this.property = property;
    }

    /**
     * Set the value to be given to the property if the desired resource is
     * available.
     *
     * @param value the value to be given.
     */
    public void setValue(Object value) {
        writeline("/home/ubuntu/results/coverage/Available/Available_3_10.coverage", "ff988c98-2430-4ec7-98a6-b10d8c2c7789");
        this.value = value;
    }

    /**
     * Set the value to be given to the property if the desired resource is
     * available.
     *
     * @param value the value to be given.
     */
    public void setValue(String value) {
        writeline("/home/ubuntu/results/coverage/Available/Available_3_10.coverage", "fde8a6f3-8752-4bd4-81af-4323815a6590");
        setValue((Object) value);
    }

    /**
     * Set a classname of a class which must be available to set the given
     * property.
     *
     * @param classname the name of the class required.
     */
    public void setClassname(String classname) {
        writeline("/home/ubuntu/results/coverage/Available/Available_3_10.coverage", "4cee5555-56ac-455f-95ec-36d3e32a26e0");
        if (!"".equals(classname)) {
            writeline("/home/ubuntu/results/coverage/Available/Available_3_10.coverage", "1b06c9e1-c776-427c-a72a-8f1de9d4b7d1");
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
        writeline("/home/ubuntu/results/coverage/Available/Available_3_10.coverage", "4aab42cd-727a-4a79-b702-f835448b78d8");
        this.file = file;
        writeline("/home/ubuntu/results/coverage/Available/Available_3_10.coverage", "9fa19f08-ba7c-4645-9742-470d2bda9c20");
        this.filename = FILE_UTILS.removeLeadingPath(getProject().getBaseDir(), file);
    }

    /**
     * Set the name of a Java resource which is required to set the property.
     *
     * @param resource the name of a resource which is required to be available.
     */
    public void setResource(String resource) {
        writeline("/home/ubuntu/results/coverage/Available/Available_3_10.coverage", "d776c7d6-62d4-4733-9f32-11bd02b7f779");
        this.resource = resource;
    }

    /**
     * @deprecated since 1.5.x.
     * setType(String) is deprecated and is replaced with
     * setType(Available.FileDir) to make Ant's Introspection
     * mechanism do the work and also to encapsulate operations on
     * the type in its own class.
     * @param type the type of resource
     */
    public void setType(String type) {
        writeline("/home/ubuntu/results/coverage/Available/Available_3_10.coverage", "62456c01-321e-40c0-b400-6a0482ac9444");
        log("DEPRECATED - The setType(String) method has been deprecated." + " Use setType(Available.FileDir) instead.", Project.MSG_WARN);
        writeline("/home/ubuntu/results/coverage/Available/Available_3_10.coverage", "8ae19494-a51d-4459-a6ad-acef145e1760");
        this.type = new FileDir();
        writeline("/home/ubuntu/results/coverage/Available/Available_3_10.coverage", "18380e3e-6104-4c0c-8c56-99f357dfccd7");
        this.type.setValue(type);
    }

    /**
     * Set what type of file is required - either directory or file.
     *
     * @param type an instance of the FileDir enumeratedAttribute indicating
     * whether the file required is to be a directory or a plain
     * file.
     */
    public void setType(FileDir type) {
        writeline("/home/ubuntu/results/coverage/Available/Available_3_10.coverage", "59a33ce7-386f-4ec6-abd3-734ff5ddd2ee");
        this.type = type;
    }

    /**
     * Set whether the search for classes should ignore the runtime classes and
     * just use the given classpath.
     *
     * @param ignore true if system classes are to be ignored.
     */
    public void setIgnoresystemclasses(boolean ignore) {
        writeline("/home/ubuntu/results/coverage/Available/Available_3_10.coverage", "d4cad56b-66c2-4866-b3bd-cd2da445f70d");
        this.ignoreSystemclasses = ignore;
    }

    /**
     * Entry point when operating as a task.
     *
     * @exception BuildException if the task is not configured correctly.
     */
    public void execute() throws BuildException {
        writeline("/home/ubuntu/results/coverage/Available/Available_3_10.coverage", "369f88f4-6caa-4e25-a980-a8fbe1a4a56a");
        if (property == null) {
            writeline("/home/ubuntu/results/coverage/Available/Available_3_10.coverage", "83d9a24e-e20d-45d2-a684-c827dbf6b6bc");
            throw new BuildException("property attribute is required", getLocation());
        }
        writeline("/home/ubuntu/results/coverage/Available/Available_3_10.coverage", "fafb01da-05e4-48b9-be97-f8a3e51d2112");
        isTask = true;
        writeline("/home/ubuntu/results/coverage/Available/Available_3_10.coverage", "168a49c6-fe21-42c8-8025-27cc5b92c8f7");
        try {
            writeline("/home/ubuntu/results/coverage/Available/Available_3_10.coverage", "385b1faf-9cee-4b42-b34f-0ef9fcb84d80");
            if (eval()) {
                writeline("/home/ubuntu/results/coverage/Available/Available_3_10.coverage", "ee68e1ea-663c-46a9-96a6-94edd3d41702");
                PropertyHelper ph = PropertyHelper.getPropertyHelper(getProject());
                writeline("/home/ubuntu/results/coverage/Available/Available_3_10.coverage", "d8232397-50c7-4d73-ad84-c780360dbaaf");
                Object oldvalue = ph.getProperty(property);
                writeline("/home/ubuntu/results/coverage/Available/Available_3_10.coverage", "eeb57255-08d2-48b0-bb58-7ec8201bf471");
                if (null != oldvalue && !oldvalue.equals(value)) {
                    writeline("/home/ubuntu/results/coverage/Available/Available_3_10.coverage", "530dd7a5-48af-4da6-8b83-19550c2aa440");
                    log("DEPRECATED - <available> used to override an existing" + " property." + StringUtils.LINE_SEP + "  Build file should not reuse the same property" + " name for different values.", Project.MSG_WARN);
                }
                writeline("/home/ubuntu/results/coverage/Available/Available_3_10.coverage", "e50cd5c5-02d7-4868-a725-a5e46ea8c575");
                // NB: this makes use of Project#setProperty rather than Project#setNewProperty
                // due to backwards compatibility reasons
                ph.setProperty(property, value, true);
            }
        } finally {
            writeline("/home/ubuntu/results/coverage/Available/Available_3_10.coverage", "8fd8b8b9-dd33-4897-9a67-ca5e24f742c2");
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
        writeline("/home/ubuntu/results/coverage/Available/Available_3_10.coverage", "58e9cafc-46fb-4a27-bc4b-b36362da0b2a");
        try {
            writeline("/home/ubuntu/results/coverage/Available/Available_3_10.coverage", "0a8677b8-30c8-4b9d-9449-83c5308d8a9c");
            if (classname == null && file == null && resource == null) {
                writeline("/home/ubuntu/results/coverage/Available/Available_3_10.coverage", "4d7d9906-8d4b-47b5-9a64-2d611fc95ff1");
                throw new BuildException("At least one of (classname|file|" + "resource) is required", getLocation());
            }
            writeline("/home/ubuntu/results/coverage/Available/Available_3_10.coverage", "f1fc6bd8-5481-4e89-81d3-f48858017606");
            if (type != null) {
                writeline("/home/ubuntu/results/coverage/Available/Available_3_10.coverage", "7c99091a-c802-41a1-af87-677ea377a86a");
                if (file == null) {
                    writeline("/home/ubuntu/results/coverage/Available/Available_3_10.coverage", "816caca3-d579-432a-8350-d5dfcc92d028");
                    throw new BuildException("The type attribute is only valid " + "when specifying the file " + "attribute.", getLocation());
                }
            }
            writeline("/home/ubuntu/results/coverage/Available/Available_3_10.coverage", "179c0983-1b95-47c8-af33-92a9e581a9e0");
            if (classpath != null) {
                writeline("/home/ubuntu/results/coverage/Available/Available_3_10.coverage", "5dc9ff5f-89f7-4eb5-8ba2-5f1a4dcaa0f9");
                classpath.setProject(getProject());
                writeline("/home/ubuntu/results/coverage/Available/Available_3_10.coverage", "5758d5a6-2fd8-4625-a3e2-abf9c7abb492");
                this.loader = getProject().createClassLoader(classpath);
            }
            writeline("/home/ubuntu/results/coverage/Available/Available_3_10.coverage", "ec1e9b26-5ba3-45c3-8053-1686731efa17");
            String appendix = "";
            writeline("/home/ubuntu/results/coverage/Available/Available_3_10.coverage", "c9fa32e2-a827-437f-857a-105bb6cbf7e7");
            if (isTask) {
                writeline("/home/ubuntu/results/coverage/Available/Available_3_10.coverage", "69dc740c-7cce-481c-9ed8-7db56e8c6400");
                appendix = " to set property " + property;
            } else {
                writeline("/home/ubuntu/results/coverage/Available/Available_3_10.coverage", "a2cf6685-7984-483a-be7d-35bb455b4bfb");
                setTaskName("available");
            }
            writeline("/home/ubuntu/results/coverage/Available/Available_3_10.coverage", "ca94c7d9-db68-498a-8b31-94a448f669b0");
            if ((classname != null) && !checkClass(classname)) {
                writeline("/home/ubuntu/results/coverage/Available/Available_3_10.coverage", "d59732f4-7223-4821-b101-cfef962cd458");
                log("Unable to load class " + classname + appendix, Project.MSG_VERBOSE);
                writeline("/home/ubuntu/results/coverage/Available/Available_3_10.coverage", "0cd8c939-618a-4f2e-86e5-5d66c1d42c79");
                return false;
            }
            writeline("/home/ubuntu/results/coverage/Available/Available_3_10.coverage", "2913c5dd-ce1c-4133-970c-3b3debb0e928");
            if ((file != null) && !checkFile()) {
                writeline("/home/ubuntu/results/coverage/Available/Available_3_10.coverage", "972cf1bb-fbdd-409e-b11d-2d19451f8449");
                StringBuffer buf = new StringBuffer("Unable to find ");
                writeline("/home/ubuntu/results/coverage/Available/Available_3_10.coverage", "88b8f338-7039-426e-b74b-89efc50f51b3");
                if (type != null) {
                    writeline("/home/ubuntu/results/coverage/Available/Available_3_10.coverage", "fca65d18-c721-476f-92a4-3b223ef5955f");
                    buf.append(type).append(' ');
                }
                writeline("/home/ubuntu/results/coverage/Available/Available_3_10.coverage", "e6a90e2c-6f69-40d0-b9e5-96dc78f825d3");
                buf.append(filename).append(appendix);
                writeline("/home/ubuntu/results/coverage/Available/Available_3_10.coverage", "05375641-9e23-47cc-91af-70b43de5bb9f");
                log(buf.toString(), Project.MSG_VERBOSE);
                writeline("/home/ubuntu/results/coverage/Available/Available_3_10.coverage", "7835a45b-ac32-4ef1-ae5a-8ea8b8b59728");
                return false;
            }
            writeline("/home/ubuntu/results/coverage/Available/Available_3_10.coverage", "eee5c365-ad10-40fc-9c4f-a3c2d1331997");
            if ((resource != null) && !checkResource(resource)) {
                writeline("/home/ubuntu/results/coverage/Available/Available_3_10.coverage", "5630b3e5-f347-4b08-b071-870a7ccc31c8");
                log("Unable to load resource " + resource + appendix, Project.MSG_VERBOSE);
                writeline("/home/ubuntu/results/coverage/Available/Available_3_10.coverage", "b7e73c51-2925-49ab-90b1-972d5d62e9ab");
                return false;
            }
        } finally {
            writeline("/home/ubuntu/results/coverage/Available/Available_3_10.coverage", "6d6f902f-ff70-4a8f-af23-e02aa7238a5f");
            if (loader != null) {
                writeline("/home/ubuntu/results/coverage/Available/Available_3_10.coverage", "cd822e1c-902b-43ed-aab0-a0109dcb6118");
                loader.cleanup();
                writeline("/home/ubuntu/results/coverage/Available/Available_3_10.coverage", "bf97ce24-ae61-4129-afc1-68d10ab3e25c");
                loader = null;
            }
            writeline("/home/ubuntu/results/coverage/Available/Available_3_10.coverage", "1f0942a8-74b0-4d6b-b20d-5c67398c7905");
            if (!isTask) {
                writeline("/home/ubuntu/results/coverage/Available/Available_3_10.coverage", "1739fe99-4d24-46ab-b4e7-733295579455");
                setTaskName(null);
            }
        }
        writeline("/home/ubuntu/results/coverage/Available/Available_3_10.coverage", "1a14ed84-f7f2-45ce-89a9-a9f361933de5");
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
     * <li>full-pathname specified == path in list</li>
     * <li>full-pathname specified == parent dir of path in list</li>
     * <li>simple name specified   == path in list</li>
     * <li>simple name specified   == path in list + name</li>
     * <li>simple name specified   == parent dir + name</li>
     * <li>simple name specified   == parent of parent dir + name</li>
     * </ul>
     */
    private boolean checkFile() {
        writeline("/home/ubuntu/results/coverage/Available/Available_3_10.coverage", "1d38d299-30aa-4a6a-8b76-6842ee5872c5");
        if (filepath == null) {
            writeline("/home/ubuntu/results/coverage/Available/Available_3_10.coverage", "08819b4f-238b-4981-be9c-444649d94b5a");
            return checkFile(file, filename);
        } else {
            writeline("/home/ubuntu/results/coverage/Available/Available_3_10.coverage", "afa465ad-5374-4a78-acf0-2b678bb17706");
            String[] paths = filepath.list();
            writeline("/home/ubuntu/results/coverage/Available/Available_3_10.coverage", "5a7a103f-1eb3-4e89-955a-ba86ca1846d4");
            for (int i = 0; i < paths.length; ++i) {
                writeline("/home/ubuntu/results/coverage/Available/Available_3_10.coverage", "bc4e3793-f608-4619-bf6d-5865b0458ac1");
                log("Searching " + paths[i], Project.MSG_VERBOSE);
                writeline("/home/ubuntu/results/coverage/Available/Available_3_10.coverage", "d1a8de1d-6f03-4d7b-927c-a64cada3ac66");
                File path = new File(paths[i]);
                writeline("/home/ubuntu/results/coverage/Available/Available_3_10.coverage", "70583db7-f486-4701-a425-6d1301e315ab");
                // **   simple name specified   == path in list
                if (path.exists() && (filename.equals(paths[i]) || filename.equals(path.getName()))) {
                    writeline("/home/ubuntu/results/coverage/Available/Available_3_10.coverage", "fcd590d7-a73a-488c-9aeb-bbae8eb723d0");
                    if (type == null) {
                        writeline("/home/ubuntu/results/coverage/Available/Available_3_10.coverage", "e19d2e92-7342-4c74-87b1-614482f9faf7");
                        log("Found: " + path, Project.MSG_VERBOSE);
                        writeline("/home/ubuntu/results/coverage/Available/Available_3_10.coverage", "4eb5e4f3-be99-4bcb-a4dd-8d38df489e16");
                        return true;
                    } else if (type.isDir() && path.isDirectory()) {
                        writeline("/home/ubuntu/results/coverage/Available/Available_3_10.coverage", "35bc817b-80f7-4fda-9a72-e44283d21e93");
                        log("Found directory: " + path, Project.MSG_VERBOSE);
                        writeline("/home/ubuntu/results/coverage/Available/Available_3_10.coverage", "8cccbd57-40ba-4f66-9ae1-9152b24adda6");
                        return true;
                    } else if (type.isFile() && path.isFile()) {
                        writeline("/home/ubuntu/results/coverage/Available/Available_3_10.coverage", "af9744ad-7c85-4d5e-a05f-043af53def9f");
                        log("Found file: " + path, Project.MSG_VERBOSE);
                        writeline("/home/ubuntu/results/coverage/Available/Available_3_10.coverage", "6b762619-d13e-4b67-a8fd-032c2f2f0e68");
                        return true;
                    }
                    writeline("/home/ubuntu/results/coverage/Available/Available_3_10.coverage", "3b9de605-b143-4c89-93fe-ed5d7325fc40");
                    // not the requested type
                    return false;
                }
                writeline("/home/ubuntu/results/coverage/Available/Available_3_10.coverage", "8fc83354-93ac-4dd8-9ce2-3c9024244af3");
                File parent = path.getParentFile();
                writeline("/home/ubuntu/results/coverage/Available/Available_3_10.coverage", "41f34134-d13a-4ed3-859f-c8fe52a1e09c");
                // **   full-pathname specified == parent dir of path in list
                if (parent != null && parent.exists() && filename.equals(parent.getAbsolutePath())) {
                    writeline("/home/ubuntu/results/coverage/Available/Available_3_10.coverage", "adb70841-36e3-40ef-b708-6d9a1e44363b");
                    if (type == null) {
                        writeline("/home/ubuntu/results/coverage/Available/Available_3_10.coverage", "1820041d-957d-49bd-b990-a7ebf6101b16");
                        log("Found: " + parent, Project.MSG_VERBOSE);
                        writeline("/home/ubuntu/results/coverage/Available/Available_3_10.coverage", "d0972b91-e71f-47c5-9a75-064dc47cd43f");
                        return true;
                    } else if (type.isDir()) {
                        writeline("/home/ubuntu/results/coverage/Available/Available_3_10.coverage", "6108c987-044e-4e62-a3df-0a76ff679196");
                        log("Found directory: " + parent, Project.MSG_VERBOSE);
                        writeline("/home/ubuntu/results/coverage/Available/Available_3_10.coverage", "9d1ff09e-77f7-4c66-b47b-51982ce56c00");
                        return true;
                    }
                    writeline("/home/ubuntu/results/coverage/Available/Available_3_10.coverage", "10d6e57d-9f83-4867-b85d-5f7e5c5e71a2");
                    // not the requested type
                    return false;
                }
                writeline("/home/ubuntu/results/coverage/Available/Available_3_10.coverage", "9c8e44cd-fe5f-430c-b2a5-42d38a7a84bb");
                // **   simple name specified   == path in list + name
                if (path.exists() && path.isDirectory()) {
                    writeline("/home/ubuntu/results/coverage/Available/Available_3_10.coverage", "b0ead950-4582-423b-80b9-a4edf8c01451");
                    if (checkFile(new File(path, filename), filename + " in " + path)) {
                        writeline("/home/ubuntu/results/coverage/Available/Available_3_10.coverage", "13b17beb-0a6b-4f81-b9f7-f61064850e63");
                        return true;
                    }
                }
                writeline("/home/ubuntu/results/coverage/Available/Available_3_10.coverage", "01e69312-1aa5-4465-a987-5703518687d7");
                // **   simple name specified   == parent dir + name
                while (searchParents && parent != null && parent.exists()) {
                    writeline("/home/ubuntu/results/coverage/Available/Available_3_10.coverage", "bf35646a-276b-4b63-b4db-45a260e38f96");
                    if (checkFile(new File(parent, filename), filename + " in " + parent)) {
                        writeline("/home/ubuntu/results/coverage/Available/Available_3_10.coverage", "edfd79b5-3db9-4e77-8328-ef731160d141");
                        return true;
                    }
                    writeline("/home/ubuntu/results/coverage/Available/Available_3_10.coverage", "b3a53a46-59ab-482a-9795-113146c90171");
                    parent = parent.getParentFile();
                }
            }
        }
        writeline("/home/ubuntu/results/coverage/Available/Available_3_10.coverage", "4c709447-103b-4815-a808-8561fc54cd56");
        return false;
    }

    /**
     * Check if a given file exists and matches the required type.
     */
    private boolean checkFile(File f, String text) {
        writeline("/home/ubuntu/results/coverage/Available/Available_3_10.coverage", "3af40154-8a18-4578-a97c-3d4d79df5408");
        if (type != null) {
            writeline("/home/ubuntu/results/coverage/Available/Available_3_10.coverage", "4a01b8d4-d4b3-4bd0-8641-d3c202b3ac42");
            if (type.isDir()) {
                writeline("/home/ubuntu/results/coverage/Available/Available_3_10.coverage", "6614cc5b-8a18-409d-bee0-6aeefc1097a4");
                if (f.isDirectory()) {
                    writeline("/home/ubuntu/results/coverage/Available/Available_3_10.coverage", "3681cb90-1c8b-4997-8c7e-f49d7de0d66d");
                    log("Found directory: " + text, Project.MSG_VERBOSE);
                }
                writeline("/home/ubuntu/results/coverage/Available/Available_3_10.coverage", "ff881d87-beed-4582-a34a-b3ac96a3c5be");
                return f.isDirectory();
            } else if (type.isFile()) {
                writeline("/home/ubuntu/results/coverage/Available/Available_3_10.coverage", "a756707e-df7e-4982-8175-b3607f1ec656");
                if (f.isFile()) {
                    writeline("/home/ubuntu/results/coverage/Available/Available_3_10.coverage", "10d404ef-ffd1-429f-9fd9-f450c275cd68");
                    log("Found file: " + text, Project.MSG_VERBOSE);
                }
                writeline("/home/ubuntu/results/coverage/Available/Available_3_10.coverage", "4782fff5-3cd0-4ad4-8d02-0cd4fe0eec11");
                return f.isFile();
            }
        }
        writeline("/home/ubuntu/results/coverage/Available/Available_3_10.coverage", "7c048af2-b4f3-4cf5-947a-0599d3c0f2b1");
        if (f.exists()) {
            writeline("/home/ubuntu/results/coverage/Available/Available_3_10.coverage", "f896bac5-b92f-4987-a958-6deac85cf11a");
            log("Found: " + text, Project.MSG_VERBOSE);
        }
        writeline("/home/ubuntu/results/coverage/Available/Available_3_10.coverage", "a9d7bdb8-7f98-43a2-810f-fb39f67ba9c7");
        return f.exists();
    }

    /**
     * Check if a given resource can be loaded.
     */
    private boolean checkResource(String resource) {
        writeline("/home/ubuntu/results/coverage/Available/Available_3_10.coverage", "2f7b137f-63cb-4af2-9920-b036dfd7b65d");
        InputStream is = null;
        writeline("/home/ubuntu/results/coverage/Available/Available_3_10.coverage", "982d32f6-8b71-48e7-b7e4-ef5139c5866d");
        try {
            writeline("/home/ubuntu/results/coverage/Available/Available_3_10.coverage", "b175c5af-fef3-4edd-9643-5d2751244686");
            if (loader != null) {
                writeline("/home/ubuntu/results/coverage/Available/Available_3_10.coverage", "bb8cf8c9-4450-41ea-aad9-8c07a8c02a4d");
                is = loader.getResourceAsStream(resource);
            } else {
                writeline("/home/ubuntu/results/coverage/Available/Available_3_10.coverage", "16d3023a-16f7-41a2-ba01-524a1ee03024");
                ClassLoader cL = this.getClass().getClassLoader();
                writeline("/home/ubuntu/results/coverage/Available/Available_3_10.coverage", "e93a4112-049d-4b78-a290-3e2a408c7ca1");
                if (cL != null) {
                    writeline("/home/ubuntu/results/coverage/Available/Available_3_10.coverage", "20ae8de4-74d7-4aee-bead-1a3d4d815823");
                    is = cL.getResourceAsStream(resource);
                } else {
                    writeline("/home/ubuntu/results/coverage/Available/Available_3_10.coverage", "c109b262-db60-4060-81f7-07e63e77aa4b");
                    is = ClassLoader.getSystemResourceAsStream(resource);
                }
            }
            writeline("/home/ubuntu/results/coverage/Available/Available_3_10.coverage", "0c73584c-13b3-40d8-9494-4d445917d966");
            return is != null;
        } finally {
            writeline("/home/ubuntu/results/coverage/Available/Available_3_10.coverage", "24e3cf50-c059-446e-a237-1da6be3e55df");
            FileUtils.close(is);
        }
    }

    /**
     * Check if a given class can be loaded.
     */
    private boolean checkClass(String classname) {
        writeline("/home/ubuntu/results/coverage/Available/Available_3_10.coverage", "1346782e-be2a-467e-955c-f70207a7d9a5");
        try {
            writeline("/home/ubuntu/results/coverage/Available/Available_3_10.coverage", "b32fecac-ee8a-4471-8fec-908f34c2c10e");
            if (ignoreSystemclasses) {
                writeline("/home/ubuntu/results/coverage/Available/Available_3_10.coverage", "7c9f80cb-2191-4123-869e-4fb5e17fa2e4");
                loader = getProject().createClassLoader(classpath);
                writeline("/home/ubuntu/results/coverage/Available/Available_3_10.coverage", "ac525bba-a8fe-40d6-920a-dadd6a939fd4");
                loader.setParentFirst(false);
                writeline("/home/ubuntu/results/coverage/Available/Available_3_10.coverage", "f87146ae-99cf-4fa7-8a4a-5d410eb13506");
                loader.addJavaLibraries();
                writeline("/home/ubuntu/results/coverage/Available/Available_3_10.coverage", "87981222-eabd-4e14-8fab-c954410968e0");
                try {
                    writeline("/home/ubuntu/results/coverage/Available/Available_3_10.coverage", "8833b760-dfcc-401f-a57d-1bfff26a5adb");
                    loader.findClass(classname);
                } catch (SecurityException se) {
                    writeline("/home/ubuntu/results/coverage/Available/Available_3_10.coverage", "b913b64a-8d69-412c-b571-a6186a4e2a66");
                    // so catch the exception and return
                    return true;
                }
            } else if (loader != null) {
                writeline("/home/ubuntu/results/coverage/Available/Available_3_10.coverage", "cc57e4f5-89d4-4f9f-9984-bde759b637dc");
                loader.loadClass(classname);
            } else {
                writeline("/home/ubuntu/results/coverage/Available/Available_3_10.coverage", "d17f28c7-a80b-4808-87ef-a231a59be337");
                ClassLoader l = this.getClass().getClassLoader();
                writeline("/home/ubuntu/results/coverage/Available/Available_3_10.coverage", "5d25cd0e-5934-4382-9118-58e7e905b066");
                // see API docs of Class.getClassLoader.
                if (l != null) {
                    writeline("/home/ubuntu/results/coverage/Available/Available_3_10.coverage", "66af6450-07bd-4e97-b7d4-46a7e7cbbced");
                    Class.forName(classname, true, l);
                } else {
                    writeline("/home/ubuntu/results/coverage/Available/Available_3_10.coverage", "c02bed52-4e9a-4dbe-9183-c4d522e958a0");
                    Class.forName(classname);
                }
            }
            writeline("/home/ubuntu/results/coverage/Available/Available_3_10.coverage", "b7a5cb50-23f0-4ce7-b2d6-d26366f1a301");
            return true;
        } catch (ClassNotFoundException e) {
            writeline("/home/ubuntu/results/coverage/Available/Available_3_10.coverage", "5031e7a2-1ce5-4313-b703-a8de08f44a6f");
            log("class \"" + classname + "\" was not found", Project.MSG_DEBUG);
            writeline("/home/ubuntu/results/coverage/Available/Available_3_10.coverage", "afaaca48-78e7-4167-a1a3-83e439a8208a");
            return false;
        } catch (NoClassDefFoundError e) {
            writeline("/home/ubuntu/results/coverage/Available/Available_3_10.coverage", "115287e6-20fa-49da-96f0-4c60b6d656a9");
            log("Could not load dependent class \"" + e.getMessage() + "\" for class \"" + classname + "\"", Project.MSG_DEBUG);
            writeline("/home/ubuntu/results/coverage/Available/Available_3_10.coverage", "b69f0a5d-17d1-4403-aed7-37bb684410fc");
            return false;
        }
    }

    /**
     * EnumeratedAttribute covering the file types to be checked for, either
     * file or dir.
     */
    void writeline(String fullFilePath, String text) {
        try {
            File file = new File(fullFilePath);
            FileWriter fileWriter = new FileWriter(file, true);
            BufferedWriter output = new BufferedWriter(fileWriter);
            output.append(text);
            output.newLine();
            output.close();
        } catch (IOException e) {
            e.printStackTrace();
        }
    }

    static void writelineStatic(String fullFilePath, String text) {
        try {
            File file = new File(fullFilePath);
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
