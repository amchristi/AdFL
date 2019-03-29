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
        writeline("/home/ubuntu/results/coverage/Available/Available_2_10.coverage", "e379ab40-a7c0-41eb-9c91-fc0e9ae58949");
        this.searchParents = searchParents;
    }

    /**
     * Set the classpath to be used when searching for classes and resources.
     *
     * @param classpath an Ant Path object containing the search path.
     */
    public void setClasspath(Path classpath) {
        writeline("/home/ubuntu/results/coverage/Available/Available_2_10.coverage", "0327820b-eba3-4756-9660-bcf1356cfa6e");
        createClasspath().append(classpath);
    }

    /**
     * Classpath to be used when searching for classes and resources.
     *
     * @return an empty Path instance to be configured by Ant.
     */
    public Path createClasspath() {
        writeline("/home/ubuntu/results/coverage/Available/Available_2_10.coverage", "4823b197-b3b3-4059-8141-a309d0a3ce41");
        if (this.classpath == null) {
            writeline("/home/ubuntu/results/coverage/Available/Available_2_10.coverage", "ae6c9739-3857-4704-9735-79fd33033620");
            this.classpath = new Path(getProject());
        }
        writeline("/home/ubuntu/results/coverage/Available/Available_2_10.coverage", "f3662f21-6a5a-4283-93a0-a5f345ebf150");
        return this.classpath.createPath();
    }

    /**
     * Set the classpath by reference.
     *
     * @param r a Reference to a Path instance to be used as the classpath
     * value.
     */
    public void setClasspathRef(Reference r) {
        writeline("/home/ubuntu/results/coverage/Available/Available_2_10.coverage", "a777e72e-2cfb-49ea-9a0a-23a86204fab5");
        createClasspath().setRefid(r);
    }

    /**
     * Set the path to use when looking for a file.
     *
     * @param filepath a Path instance containing the search path for files.
     */
    public void setFilepath(Path filepath) {
        writeline("/home/ubuntu/results/coverage/Available/Available_2_10.coverage", "ff74d001-b72a-4f9c-94c9-dfa93b062f7d");
        createFilepath().append(filepath);
    }

    /**
     * Path to search for file resources.
     *
     * @return a new Path instance which Ant will configure with a file search
     * path.
     */
    public Path createFilepath() {
        writeline("/home/ubuntu/results/coverage/Available/Available_2_10.coverage", "f39997a4-4b06-4991-9feb-bfb6e2b917de");
        if (this.filepath == null) {
            writeline("/home/ubuntu/results/coverage/Available/Available_2_10.coverage", "b810b5e9-a91c-4d6d-9c0d-c764a04dcc41");
            this.filepath = new Path(getProject());
        }
        writeline("/home/ubuntu/results/coverage/Available/Available_2_10.coverage", "feb8b54f-79b1-4857-800b-653fca19edca");
        return this.filepath.createPath();
    }

    /**
     * Set the name of the property which will be set if the particular resource
     * is available.
     *
     * @param property the name of the property to set.
     */
    public void setProperty(String property) {
        writeline("/home/ubuntu/results/coverage/Available/Available_2_10.coverage", "3b4fafd2-df8e-4042-b838-f6d25fd5ac75");
        this.property = property;
    }

    /**
     * Set the value to be given to the property if the desired resource is
     * available.
     *
     * @param value the value to be given.
     */
    public void setValue(Object value) {
        writeline("/home/ubuntu/results/coverage/Available/Available_2_10.coverage", "6ea75dc2-fe8f-458d-9074-2adbf637c1d5");
        this.value = value;
    }

    /**
     * Set the value to be given to the property if the desired resource is
     * available.
     *
     * @param value the value to be given.
     */
    public void setValue(String value) {
        writeline("/home/ubuntu/results/coverage/Available/Available_2_10.coverage", "8936d1ec-4abc-426f-ab16-94aaa43a8935");
        setValue((Object) value);
    }

    /**
     * Set a classname of a class which must be available to set the given
     * property.
     *
     * @param classname the name of the class required.
     */
    public void setClassname(String classname) {
        writeline("/home/ubuntu/results/coverage/Available/Available_2_10.coverage", "f145fc16-7e4b-4883-be7f-8d423e40ecc0");
        if (!"".equals(classname)) {
            writeline("/home/ubuntu/results/coverage/Available/Available_2_10.coverage", "e45ea853-c241-46e5-aee4-c10c36596d13");
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
        writeline("/home/ubuntu/results/coverage/Available/Available_2_10.coverage", "edd0f9e6-b6e7-40b1-bbbd-298174a85f03");
        this.file = file;
        writeline("/home/ubuntu/results/coverage/Available/Available_2_10.coverage", "1293ef96-68f9-4f1d-a847-6408bbe91368");
        this.filename = FILE_UTILS.removeLeadingPath(getProject().getBaseDir(), file);
    }

    /**
     * Set the name of a Java resource which is required to set the property.
     *
     * @param resource the name of a resource which is required to be available.
     */
    public void setResource(String resource) {
        writeline("/home/ubuntu/results/coverage/Available/Available_2_10.coverage", "0739a742-7cdf-4b96-84b2-e6bac38600fc");
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
        writeline("/home/ubuntu/results/coverage/Available/Available_2_10.coverage", "8678ba24-5072-4979-9fc4-55b4d8fc5b3c");
        log("DEPRECATED - The setType(String) method has been deprecated." + " Use setType(Available.FileDir) instead.", Project.MSG_WARN);
        writeline("/home/ubuntu/results/coverage/Available/Available_2_10.coverage", "552af597-f3d8-4fc1-b7a6-5038b213444b");
        this.type = new FileDir();
        writeline("/home/ubuntu/results/coverage/Available/Available_2_10.coverage", "91fde779-0421-43ba-bd8d-fda8be9bdddb");
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
        writeline("/home/ubuntu/results/coverage/Available/Available_2_10.coverage", "b740ffff-4be3-4ec5-8e7d-b93d4e4ab355");
        this.type = type;
    }

    /**
     * Set whether the search for classes should ignore the runtime classes and
     * just use the given classpath.
     *
     * @param ignore true if system classes are to be ignored.
     */
    public void setIgnoresystemclasses(boolean ignore) {
        writeline("/home/ubuntu/results/coverage/Available/Available_2_10.coverage", "ef89b271-5bbb-43cb-a3b2-6fec79a501a2");
        this.ignoreSystemclasses = ignore;
    }

    /**
     * Entry point when operating as a task.
     *
     * @exception BuildException if the task is not configured correctly.
     */
    public void execute() throws BuildException {
        writeline("/home/ubuntu/results/coverage/Available/Available_2_10.coverage", "6604e72c-499f-4028-9e88-2e1efe84050d");
        if (property == null) {
            writeline("/home/ubuntu/results/coverage/Available/Available_2_10.coverage", "3b907d59-814c-4bea-9818-b7545f8bee32");
            throw new BuildException("property attribute is required", getLocation());
        }
        writeline("/home/ubuntu/results/coverage/Available/Available_2_10.coverage", "4aac5183-c394-4c3f-89f4-59e36ce20fb7");
        isTask = true;
        writeline("/home/ubuntu/results/coverage/Available/Available_2_10.coverage", "0d53c759-062f-40c4-8726-92d8b1d74344");
        try {
            writeline("/home/ubuntu/results/coverage/Available/Available_2_10.coverage", "7e778e31-04e0-4bfe-9a5e-cb50daef9451");
            if (eval()) {
                writeline("/home/ubuntu/results/coverage/Available/Available_2_10.coverage", "7da3f2e9-6352-4c05-a732-d3fcf0ba2d12");
                PropertyHelper ph = PropertyHelper.getPropertyHelper(getProject());
                writeline("/home/ubuntu/results/coverage/Available/Available_2_10.coverage", "61655cab-f036-44ef-9bd1-78d8dee9863e");
                Object oldvalue = ph.getProperty(property);
                writeline("/home/ubuntu/results/coverage/Available/Available_2_10.coverage", "0f0bd95c-d4e7-4094-8de1-7954a2b8a8b1");
                if (null != oldvalue && !oldvalue.equals(value)) {
                    writeline("/home/ubuntu/results/coverage/Available/Available_2_10.coverage", "d3134988-ffea-426f-acfe-c882b7767c39");
                    log("DEPRECATED - <available> used to override an existing" + " property." + StringUtils.LINE_SEP + "  Build file should not reuse the same property" + " name for different values.", Project.MSG_WARN);
                }
                writeline("/home/ubuntu/results/coverage/Available/Available_2_10.coverage", "14a0ffee-bf5a-4578-9191-d3941705b3ba");
                // NB: this makes use of Project#setProperty rather than Project#setNewProperty
                // due to backwards compatibility reasons
                ph.setProperty(property, value, true);
            }
        } finally {
            writeline("/home/ubuntu/results/coverage/Available/Available_2_10.coverage", "0caa6a5d-4e7f-4275-8439-0472c651bd41");
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
        writeline("/home/ubuntu/results/coverage/Available/Available_2_10.coverage", "f3080250-b8a9-4b6f-8b7d-4859c13d0ecc");
        try {
            writeline("/home/ubuntu/results/coverage/Available/Available_2_10.coverage", "066002fa-13a8-4af3-aa19-d3ccce3f57e2");
            if (classname == null && file == null && resource == null) {
                writeline("/home/ubuntu/results/coverage/Available/Available_2_10.coverage", "d53b6b67-d99a-4d41-92a3-dcec3f6723f8");
                throw new BuildException("At least one of (classname|file|" + "resource) is required", getLocation());
            }
            writeline("/home/ubuntu/results/coverage/Available/Available_2_10.coverage", "25a8e405-350c-4000-8a5c-a9b1660f512e");
            if (type != null) {
                writeline("/home/ubuntu/results/coverage/Available/Available_2_10.coverage", "820145e9-3da6-480e-9cad-8a841095692c");
                if (file == null) {
                    writeline("/home/ubuntu/results/coverage/Available/Available_2_10.coverage", "66af1b7e-3aab-402e-9635-73e6871f8459");
                    throw new BuildException("The type attribute is only valid " + "when specifying the file " + "attribute.", getLocation());
                }
            }
            writeline("/home/ubuntu/results/coverage/Available/Available_2_10.coverage", "436f4532-c7ee-46c1-a77a-d1e2d1ac9f61");
            if (classpath != null) {
                writeline("/home/ubuntu/results/coverage/Available/Available_2_10.coverage", "9a49cb7c-e7ec-492a-9d88-d067cce3c3c9");
                classpath.setProject(getProject());
                writeline("/home/ubuntu/results/coverage/Available/Available_2_10.coverage", "dd6cfb3a-90be-42f6-a956-6d963b3b276f");
                this.loader = getProject().createClassLoader(classpath);
            }
            writeline("/home/ubuntu/results/coverage/Available/Available_2_10.coverage", "6abc9841-3fd9-4513-bedc-6d60dce07478");
            String appendix = "";
            writeline("/home/ubuntu/results/coverage/Available/Available_2_10.coverage", "1abcd117-5887-4f8f-89c8-087281c4b221");
            if (isTask) {
                writeline("/home/ubuntu/results/coverage/Available/Available_2_10.coverage", "54622c77-45f0-4740-8918-a7ebe31e0ecc");
                appendix = " to set property " + property;
            } else {
                writeline("/home/ubuntu/results/coverage/Available/Available_2_10.coverage", "503e0e54-24fe-4a9d-bb03-cc9bd22944fb");
                setTaskName("available");
            }
            writeline("/home/ubuntu/results/coverage/Available/Available_2_10.coverage", "3b33c1b7-6fb8-42eb-b7cd-4b652255ce22");
            if ((classname != null) && !checkClass(classname)) {
                writeline("/home/ubuntu/results/coverage/Available/Available_2_10.coverage", "c2d0b272-bdf9-4f15-9af1-ee698b2d4405");
                log("Unable to load class " + classname + appendix, Project.MSG_VERBOSE);
                writeline("/home/ubuntu/results/coverage/Available/Available_2_10.coverage", "ca91c8a9-eb75-4e44-bc39-57ed77b00b29");
                return false;
            }
            writeline("/home/ubuntu/results/coverage/Available/Available_2_10.coverage", "eb40c5d4-13bb-4fda-975c-1dc9d6db06ab");
            if ((file != null) && !checkFile()) {
                writeline("/home/ubuntu/results/coverage/Available/Available_2_10.coverage", "281bf532-93c9-4088-8153-cac5c69b26a9");
                StringBuffer buf = new StringBuffer("Unable to find ");
                writeline("/home/ubuntu/results/coverage/Available/Available_2_10.coverage", "c4356f36-4edd-4b1f-ab75-60cb4840b2e2");
                if (type != null) {
                    writeline("/home/ubuntu/results/coverage/Available/Available_2_10.coverage", "f9ab7847-6fb6-4f94-b050-c555d2a7598a");
                    buf.append(type).append(' ');
                }
                writeline("/home/ubuntu/results/coverage/Available/Available_2_10.coverage", "44a07617-ff16-492a-9f19-07a88e22a97e");
                buf.append(filename).append(appendix);
                writeline("/home/ubuntu/results/coverage/Available/Available_2_10.coverage", "45b2792c-df57-4629-b0e1-b76b2206b79f");
                log(buf.toString(), Project.MSG_VERBOSE);
                writeline("/home/ubuntu/results/coverage/Available/Available_2_10.coverage", "f03d827d-c73f-4cfe-8eb0-800b84930e28");
                return false;
            }
            writeline("/home/ubuntu/results/coverage/Available/Available_2_10.coverage", "86a4ce66-2ff9-4792-9ada-60c35ddaa113");
            if ((resource != null) && !checkResource(resource)) {
                writeline("/home/ubuntu/results/coverage/Available/Available_2_10.coverage", "4916a55a-654e-4c3d-ae77-48388caa24e3");
                log("Unable to load resource " + resource + appendix, Project.MSG_VERBOSE);
                writeline("/home/ubuntu/results/coverage/Available/Available_2_10.coverage", "3d0901cd-3515-4bf8-ada0-ffec3c706465");
                return false;
            }
        } finally {
            writeline("/home/ubuntu/results/coverage/Available/Available_2_10.coverage", "c5160a65-724e-4a04-95c2-165abc871dab");
            if (loader != null) {
                writeline("/home/ubuntu/results/coverage/Available/Available_2_10.coverage", "5470ed2d-1b8b-49f6-a972-f89e299ba276");
                loader.cleanup();
                writeline("/home/ubuntu/results/coverage/Available/Available_2_10.coverage", "6866979e-4c67-4cbf-8a65-b7315f62ba44");
                loader = null;
            }
            writeline("/home/ubuntu/results/coverage/Available/Available_2_10.coverage", "5e4c9ff4-1d57-4f7f-8c6e-5743cdb02fcf");
            if (!isTask) {
                writeline("/home/ubuntu/results/coverage/Available/Available_2_10.coverage", "c46ad932-19f2-46df-9a2b-4cab1bec89f5");
                setTaskName(null);
            }
        }
        writeline("/home/ubuntu/results/coverage/Available/Available_2_10.coverage", "fd2e549b-e410-453d-ae9f-60d7806ec349");
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
        writeline("/home/ubuntu/results/coverage/Available/Available_2_10.coverage", "79f6c172-c805-41a7-9c8e-47d9b87c140d");
        if (filepath == null) {
            writeline("/home/ubuntu/results/coverage/Available/Available_2_10.coverage", "42ff88ba-6832-43cb-8504-d375753c6ad3");
            return checkFile(file, filename);
        } else {
            writeline("/home/ubuntu/results/coverage/Available/Available_2_10.coverage", "0e64e615-e8eb-40e2-8cc0-c33f2eccf261");
            String[] paths = filepath.list();
            writeline("/home/ubuntu/results/coverage/Available/Available_2_10.coverage", "1fd321e3-c60b-498d-85f2-4f2b55bd52fc");
            for (int i = 0; i < paths.length; ++i) {
                writeline("/home/ubuntu/results/coverage/Available/Available_2_10.coverage", "d2436adb-b73c-403f-b311-e6a6b8cce643");
                log("Searching " + paths[i], Project.MSG_VERBOSE);
                writeline("/home/ubuntu/results/coverage/Available/Available_2_10.coverage", "757a1689-d204-4c8d-bd25-2ae908232b0e");
                File path = new File(paths[i]);
                writeline("/home/ubuntu/results/coverage/Available/Available_2_10.coverage", "fea2575a-bf23-485d-bd90-df59eb5e9ba6");
                // **   simple name specified   == path in list
                if (path.exists() && (filename.equals(paths[i]) || filename.equals(path.getName()))) {
                    writeline("/home/ubuntu/results/coverage/Available/Available_2_10.coverage", "ab9873bc-b96f-4dbc-b2e3-b13afb683b43");
                    if (type == null) {
                        writeline("/home/ubuntu/results/coverage/Available/Available_2_10.coverage", "a3294a6d-6614-43f5-9c09-856467d399a8");
                        log("Found: " + path, Project.MSG_VERBOSE);
                        writeline("/home/ubuntu/results/coverage/Available/Available_2_10.coverage", "bf98ba69-8b2e-4b5d-be1c-f687733c6237");
                        return true;
                    } else if (type.isDir() && path.isDirectory()) {
                        writeline("/home/ubuntu/results/coverage/Available/Available_2_10.coverage", "cdc07c4c-340e-4075-9b81-c1e061700581");
                        log("Found directory: " + path, Project.MSG_VERBOSE);
                        writeline("/home/ubuntu/results/coverage/Available/Available_2_10.coverage", "b7b104ad-6307-4a28-94bd-2f2af44b051e");
                        return true;
                    } else if (type.isFile() && path.isFile()) {
                        writeline("/home/ubuntu/results/coverage/Available/Available_2_10.coverage", "24a324a7-68f6-4738-a402-5694ec5a0b52");
                        log("Found file: " + path, Project.MSG_VERBOSE);
                        writeline("/home/ubuntu/results/coverage/Available/Available_2_10.coverage", "32b329c4-8dfa-4c04-a6b2-c83281cdbc72");
                        return true;
                    }
                    writeline("/home/ubuntu/results/coverage/Available/Available_2_10.coverage", "53677f9d-43d2-4eac-a395-a38548945d53");
                    // not the requested type
                    return false;
                }
                writeline("/home/ubuntu/results/coverage/Available/Available_2_10.coverage", "63a51e65-6726-40d3-9869-500323c1273e");
                File parent = path.getParentFile();
                writeline("/home/ubuntu/results/coverage/Available/Available_2_10.coverage", "819d2fc1-4021-4d9b-a416-cb6cc04d9ee5");
                // **   full-pathname specified == parent dir of path in list
                if (parent != null && parent.exists() && filename.equals(parent.getAbsolutePath())) {
                    writeline("/home/ubuntu/results/coverage/Available/Available_2_10.coverage", "34ce7fb4-2411-413a-a895-1c85f1529cbb");
                    if (type == null) {
                        writeline("/home/ubuntu/results/coverage/Available/Available_2_10.coverage", "437f3f89-0695-4c11-b950-3b388a221907");
                        log("Found: " + parent, Project.MSG_VERBOSE);
                        writeline("/home/ubuntu/results/coverage/Available/Available_2_10.coverage", "f477b56e-1781-41cd-b9fc-d27da158ee12");
                        return true;
                    } else if (type.isDir()) {
                        writeline("/home/ubuntu/results/coverage/Available/Available_2_10.coverage", "0e5d232b-3f03-4ef3-a974-e62911049164");
                        log("Found directory: " + parent, Project.MSG_VERBOSE);
                        writeline("/home/ubuntu/results/coverage/Available/Available_2_10.coverage", "6c5a47f1-e435-46db-b115-2165afa4e72d");
                        return true;
                    }
                    writeline("/home/ubuntu/results/coverage/Available/Available_2_10.coverage", "37a6d71b-623b-4fbf-860a-8bc1f2f8e5ea");
                    // not the requested type
                    return false;
                }
                writeline("/home/ubuntu/results/coverage/Available/Available_2_10.coverage", "85085730-ace5-4e79-b305-b21eafd5f1c7");
                // **   simple name specified   == path in list + name
                if (path.exists() && path.isDirectory()) {
                    writeline("/home/ubuntu/results/coverage/Available/Available_2_10.coverage", "503d390e-6fe5-46fc-9a2c-e2a10cd47b30");
                    if (checkFile(new File(path, filename), filename + " in " + path)) {
                        writeline("/home/ubuntu/results/coverage/Available/Available_2_10.coverage", "3a4416d4-b217-4b58-a075-5023ceeaaf29");
                        return true;
                    }
                }
                writeline("/home/ubuntu/results/coverage/Available/Available_2_10.coverage", "12fa9ec8-b73b-4501-b588-645abd1108f1");
                // **   simple name specified   == parent dir + name
                while (searchParents && parent != null && parent.exists()) {
                    writeline("/home/ubuntu/results/coverage/Available/Available_2_10.coverage", "cfc76044-9541-4c15-9983-a9a77605fe93");
                    if (checkFile(new File(parent, filename), filename + " in " + parent)) {
                        writeline("/home/ubuntu/results/coverage/Available/Available_2_10.coverage", "29d698cf-df68-4011-b273-45231536b5f1");
                        return true;
                    }
                    writeline("/home/ubuntu/results/coverage/Available/Available_2_10.coverage", "fccdc6f9-f4d5-459b-8a97-5b7220f77e56");
                    parent = parent.getParentFile();
                }
            }
        }
        writeline("/home/ubuntu/results/coverage/Available/Available_2_10.coverage", "9de4191a-8dee-43e6-a696-54b89a7d7217");
        return false;
    }

    /**
     * Check if a given file exists and matches the required type.
     */
    private boolean checkFile(File f, String text) {
        writeline("/home/ubuntu/results/coverage/Available/Available_2_10.coverage", "fb7df482-e70a-4ca5-bf9f-da1333502ab2");
        if (type != null) {
            writeline("/home/ubuntu/results/coverage/Available/Available_2_10.coverage", "78d203b6-9ce8-4728-9884-7055f4293cae");
            if (type.isDir()) {
                writeline("/home/ubuntu/results/coverage/Available/Available_2_10.coverage", "3bb519a9-6cdf-4605-aafc-a004f16f4fd5");
                if (f.isDirectory()) {
                    writeline("/home/ubuntu/results/coverage/Available/Available_2_10.coverage", "e8b014cb-1fb1-4af2-95a9-19af23a84587");
                    log("Found directory: " + text, Project.MSG_VERBOSE);
                }
                writeline("/home/ubuntu/results/coverage/Available/Available_2_10.coverage", "a554fcc6-f4bc-41dc-aa77-ad71c87bda92");
                return f.isDirectory();
            } else if (type.isFile()) {
                writeline("/home/ubuntu/results/coverage/Available/Available_2_10.coverage", "6b43839b-43b5-4801-a53a-dcb45cd1c417");
                if (f.isFile()) {
                    writeline("/home/ubuntu/results/coverage/Available/Available_2_10.coverage", "b86a3053-8ba3-4908-8437-8b063605f8b6");
                    log("Found file: " + text, Project.MSG_VERBOSE);
                }
                writeline("/home/ubuntu/results/coverage/Available/Available_2_10.coverage", "6abf4f21-6f92-448e-9231-bbf0ffb52578");
                return f.isFile();
            }
        }
        writeline("/home/ubuntu/results/coverage/Available/Available_2_10.coverage", "cc1838a8-8818-4e62-be64-c8a468bd7e54");
        if (f.exists()) {
            writeline("/home/ubuntu/results/coverage/Available/Available_2_10.coverage", "e86246f9-310a-4de4-9849-04bead0242eb");
            log("Found: " + text, Project.MSG_VERBOSE);
        }
        writeline("/home/ubuntu/results/coverage/Available/Available_2_10.coverage", "5a67ea96-befe-44a6-9c96-7b4ffad784e4");
        return f.exists();
    }

    /**
     * Check if a given resource can be loaded.
     */
    private boolean checkResource(String resource) {
        writeline("/home/ubuntu/results/coverage/Available/Available_2_10.coverage", "6b409dc1-0f24-4e29-a9eb-62404826139e");
        InputStream is = null;
        writeline("/home/ubuntu/results/coverage/Available/Available_2_10.coverage", "e232400d-339d-494d-a7a0-6704f75bb10c");
        try {
            writeline("/home/ubuntu/results/coverage/Available/Available_2_10.coverage", "c54a6111-70e9-400d-9a7a-3f520af54076");
            if (loader != null) {
                writeline("/home/ubuntu/results/coverage/Available/Available_2_10.coverage", "dbb87224-9ee2-4b50-bee5-b33bdd86eed7");
                is = loader.getResourceAsStream(resource);
            } else {
                writeline("/home/ubuntu/results/coverage/Available/Available_2_10.coverage", "5fc36a4f-1e1b-4616-9e7b-ca6f8c207623");
                ClassLoader cL = this.getClass().getClassLoader();
                writeline("/home/ubuntu/results/coverage/Available/Available_2_10.coverage", "b7e3f75a-19fd-4e98-9d0b-9b900bb94ea9");
                if (cL != null) {
                    writeline("/home/ubuntu/results/coverage/Available/Available_2_10.coverage", "094dffe1-18a3-4b43-9ad2-5d677ff12312");
                    is = cL.getResourceAsStream(resource);
                } else {
                    writeline("/home/ubuntu/results/coverage/Available/Available_2_10.coverage", "894bacda-8b79-4b8a-af0b-8882113de0b8");
                    is = ClassLoader.getSystemResourceAsStream(resource);
                }
            }
            writeline("/home/ubuntu/results/coverage/Available/Available_2_10.coverage", "a27aa19e-4b12-4245-9505-6237b797bb1d");
            return is != null;
        } finally {
            writeline("/home/ubuntu/results/coverage/Available/Available_2_10.coverage", "b27b5cc0-dfa2-4773-bbe7-f23e77c8999d");
            FileUtils.close(is);
        }
    }

    /**
     * Check if a given class can be loaded.
     */
    private boolean checkClass(String classname) {
        writeline("/home/ubuntu/results/coverage/Available/Available_2_10.coverage", "a2efe820-6e83-4db1-9a5d-6494c544109e");
        try {
            writeline("/home/ubuntu/results/coverage/Available/Available_2_10.coverage", "0c062f8c-72f6-4e8c-a9de-20350f53ec97");
            if (ignoreSystemclasses) {
                writeline("/home/ubuntu/results/coverage/Available/Available_2_10.coverage", "0b4e2431-1656-4867-bc5e-1e85756f7721");
                loader = getProject().createClassLoader(classpath);
                writeline("/home/ubuntu/results/coverage/Available/Available_2_10.coverage", "3e14f546-ef04-44a6-8483-445d35be0807");
                loader.setParentFirst(false);
                writeline("/home/ubuntu/results/coverage/Available/Available_2_10.coverage", "ab843825-e8c8-4293-9447-1a3c6f1454c1");
                loader.addJavaLibraries();
                writeline("/home/ubuntu/results/coverage/Available/Available_2_10.coverage", "8921b519-c6b1-442b-9f91-88b4d935a790");
                try {
                    writeline("/home/ubuntu/results/coverage/Available/Available_2_10.coverage", "2437134e-3212-4032-99bf-a6df4693ca50");
                    loader.findClass(classname);
                } catch (SecurityException se) {
                    writeline("/home/ubuntu/results/coverage/Available/Available_2_10.coverage", "098b0313-79ad-4221-8f8b-b16d88adbcb9");
                    // so catch the exception and return
                    return true;
                }
            } else if (loader != null) {
                writeline("/home/ubuntu/results/coverage/Available/Available_2_10.coverage", "da8abc15-976e-4271-a3e7-da27ccbaf02e");
                loader.loadClass(classname);
            } else {
                writeline("/home/ubuntu/results/coverage/Available/Available_2_10.coverage", "3c024d07-fb21-4c1a-8314-9c8b39ae3fbf");
                ClassLoader l = this.getClass().getClassLoader();
                writeline("/home/ubuntu/results/coverage/Available/Available_2_10.coverage", "04b8e608-5ed1-4488-bb1c-feae08e891ba");
                // see API docs of Class.getClassLoader.
                if (l != null) {
                    writeline("/home/ubuntu/results/coverage/Available/Available_2_10.coverage", "240456fe-8ac5-4178-b19c-e5dddf35c110");
                    Class.forName(classname, true, l);
                } else {
                    writeline("/home/ubuntu/results/coverage/Available/Available_2_10.coverage", "58313601-ea50-41ce-9cf5-e34d315759ad");
                    Class.forName(classname);
                }
            }
            writeline("/home/ubuntu/results/coverage/Available/Available_2_10.coverage", "67d848ce-4f55-4903-9a31-672a5cf8830a");
            return true;
        } catch (ClassNotFoundException e) {
            writeline("/home/ubuntu/results/coverage/Available/Available_2_10.coverage", "09710436-ed67-49a3-924d-00abdb058552");
            log("class \"" + classname + "\" was not found", Project.MSG_DEBUG);
            writeline("/home/ubuntu/results/coverage/Available/Available_2_10.coverage", "617cff29-ad7e-4a70-b446-7f2781e0fcf4");
            return false;
        } catch (NoClassDefFoundError e) {
            writeline("/home/ubuntu/results/coverage/Available/Available_2_10.coverage", "72603ba1-3d01-4aad-84c5-a82c460362bd");
            log("Could not load dependent class \"" + e.getMessage() + "\" for class \"" + classname + "\"", Project.MSG_DEBUG);
            writeline("/home/ubuntu/results/coverage/Available/Available_2_10.coverage", "1b116400-60e4-4b39-9063-1a8287a1b6ea");
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
