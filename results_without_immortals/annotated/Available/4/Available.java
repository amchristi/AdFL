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
        writeline("/home/ubuntu/results/coverage/Available/Available_4_10.coverage", "4396a18b-a818-4ccc-9986-2b91de5b7123");
        this.searchParents = searchParents;
    }

    /**
     * Set the classpath to be used when searching for classes and resources.
     *
     * @param classpath an Ant Path object containing the search path.
     */
    public void setClasspath(Path classpath) {
        writeline("/home/ubuntu/results/coverage/Available/Available_4_10.coverage", "a2e92bc7-94ce-422c-827b-128b9309892d");
        createClasspath().append(classpath);
    }

    /**
     * Classpath to be used when searching for classes and resources.
     *
     * @return an empty Path instance to be configured by Ant.
     */
    public Path createClasspath() {
        writeline("/home/ubuntu/results/coverage/Available/Available_4_10.coverage", "62dca44c-379c-4bd4-83e5-4b96bbc35a3f");
        if (this.classpath == null) {
            writeline("/home/ubuntu/results/coverage/Available/Available_4_10.coverage", "599f51c6-d5f4-4dc3-9abf-b2fce7afb1ab");
            this.classpath = new Path(getProject());
        }
        writeline("/home/ubuntu/results/coverage/Available/Available_4_10.coverage", "c0839cc6-28b3-48b9-9713-74cd3656b966");
        return this.classpath.createPath();
    }

    /**
     * Set the classpath by reference.
     *
     * @param r a Reference to a Path instance to be used as the classpath
     * value.
     */
    public void setClasspathRef(Reference r) {
        writeline("/home/ubuntu/results/coverage/Available/Available_4_10.coverage", "7ce6cbff-4156-45fc-922e-02c26a11b15a");
        createClasspath().setRefid(r);
    }

    /**
     * Set the path to use when looking for a file.
     *
     * @param filepath a Path instance containing the search path for files.
     */
    public void setFilepath(Path filepath) {
        writeline("/home/ubuntu/results/coverage/Available/Available_4_10.coverage", "daf8b35a-eef4-4afd-a397-8a0c72e23a4c");
        createFilepath().append(filepath);
    }

    /**
     * Path to search for file resources.
     *
     * @return a new Path instance which Ant will configure with a file search
     * path.
     */
    public Path createFilepath() {
        writeline("/home/ubuntu/results/coverage/Available/Available_4_10.coverage", "0e146cd3-96e8-4e55-95b1-6e23afd4f711");
        if (this.filepath == null) {
            writeline("/home/ubuntu/results/coverage/Available/Available_4_10.coverage", "bffe5c5f-258d-4335-8bab-97f96a23a1df");
            this.filepath = new Path(getProject());
        }
        writeline("/home/ubuntu/results/coverage/Available/Available_4_10.coverage", "26bcca7a-6215-4543-99ec-0f3659e326e4");
        return this.filepath.createPath();
    }

    /**
     * Set the name of the property which will be set if the particular resource
     * is available.
     *
     * @param property the name of the property to set.
     */
    public void setProperty(String property) {
        writeline("/home/ubuntu/results/coverage/Available/Available_4_10.coverage", "d09f5fec-69b4-4390-9570-e516a61b6cc4");
        this.property = property;
    }

    /**
     * Set the value to be given to the property if the desired resource is
     * available.
     *
     * @param value the value to be given.
     */
    public void setValue(Object value) {
        writeline("/home/ubuntu/results/coverage/Available/Available_4_10.coverage", "978b2422-1ad6-4aff-8b96-a40fe3cfa5a5");
        this.value = value;
    }

    /**
     * Set the value to be given to the property if the desired resource is
     * available.
     *
     * @param value the value to be given.
     */
    public void setValue(String value) {
        writeline("/home/ubuntu/results/coverage/Available/Available_4_10.coverage", "704af4a0-a5af-45ea-ad1a-f4a703f63668");
        setValue((Object) value);
    }

    /**
     * Set a classname of a class which must be available to set the given
     * property.
     *
     * @param classname the name of the class required.
     */
    public void setClassname(String classname) {
        writeline("/home/ubuntu/results/coverage/Available/Available_4_10.coverage", "85ecddee-6f9e-462e-8281-c2a71f4cb473");
        if (!"".equals(classname)) {
            writeline("/home/ubuntu/results/coverage/Available/Available_4_10.coverage", "1d90c04f-0df2-473e-82ac-5d44747f42e5");
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
        writeline("/home/ubuntu/results/coverage/Available/Available_4_10.coverage", "1c787461-2a22-4dbf-9111-80d85536afe7");
        this.file = file;
        writeline("/home/ubuntu/results/coverage/Available/Available_4_10.coverage", "5f8d9f80-9d69-42fa-9fea-95619c98f70e");
        this.filename = FILE_UTILS.removeLeadingPath(getProject().getBaseDir(), file);
    }

    /**
     * Set the name of a Java resource which is required to set the property.
     *
     * @param resource the name of a resource which is required to be available.
     */
    public void setResource(String resource) {
        writeline("/home/ubuntu/results/coverage/Available/Available_4_10.coverage", "cbf59c09-b3de-4e45-94c4-e6fb5052887d");
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
        writeline("/home/ubuntu/results/coverage/Available/Available_4_10.coverage", "411348e6-c951-4d6a-a0bd-7055e470624e");
        log("DEPRECATED - The setType(String) method has been deprecated." + " Use setType(Available.FileDir) instead.", Project.MSG_WARN);
        writeline("/home/ubuntu/results/coverage/Available/Available_4_10.coverage", "47d681f4-a29c-45df-9122-dfabbce69957");
        this.type = new FileDir();
        writeline("/home/ubuntu/results/coverage/Available/Available_4_10.coverage", "f5262a7c-7658-416f-891c-0bf2e18c3a31");
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
        writeline("/home/ubuntu/results/coverage/Available/Available_4_10.coverage", "bba37292-e656-4da8-a86e-8a57816041cd");
        this.type = type;
    }

    /**
     * Set whether the search for classes should ignore the runtime classes and
     * just use the given classpath.
     *
     * @param ignore true if system classes are to be ignored.
     */
    public void setIgnoresystemclasses(boolean ignore) {
        writeline("/home/ubuntu/results/coverage/Available/Available_4_10.coverage", "efe03670-2439-4b24-bd9b-6669f6f9a788");
        this.ignoreSystemclasses = ignore;
    }

    /**
     * Entry point when operating as a task.
     *
     * @exception BuildException if the task is not configured correctly.
     */
    public void execute() throws BuildException {
        writeline("/home/ubuntu/results/coverage/Available/Available_4_10.coverage", "7e5036c5-aa8a-46d8-ba48-4df62aa89e0b");
        if (property == null) {
            writeline("/home/ubuntu/results/coverage/Available/Available_4_10.coverage", "11ca9ff2-8099-42f3-90da-c5ff2b466234");
            throw new BuildException("property attribute is required", getLocation());
        }
        writeline("/home/ubuntu/results/coverage/Available/Available_4_10.coverage", "918fcd1d-e0ac-49e0-8545-f63a118fd04d");
        isTask = true;
        writeline("/home/ubuntu/results/coverage/Available/Available_4_10.coverage", "9bd9525c-4a71-4ecb-ab9f-e8f50e65ee9b");
        try {
            writeline("/home/ubuntu/results/coverage/Available/Available_4_10.coverage", "d8503454-23a6-43cf-add2-87840f290467");
            if (eval()) {
                writeline("/home/ubuntu/results/coverage/Available/Available_4_10.coverage", "fa3c7a79-0907-42e7-992c-7f7a7f9226ba");
                PropertyHelper ph = PropertyHelper.getPropertyHelper(getProject());
                writeline("/home/ubuntu/results/coverage/Available/Available_4_10.coverage", "2e819b6b-b5e4-47df-98f2-5d332d89ebee");
                Object oldvalue = ph.getProperty(property);
                writeline("/home/ubuntu/results/coverage/Available/Available_4_10.coverage", "dbbfb35f-bd3d-47c8-9e3f-f7bebbb78b59");
                if (null != oldvalue && !oldvalue.equals(value)) {
                    writeline("/home/ubuntu/results/coverage/Available/Available_4_10.coverage", "7a75a04b-880c-4686-9d0b-743c24f00813");
                    log("DEPRECATED - <available> used to override an existing" + " property." + StringUtils.LINE_SEP + "  Build file should not reuse the same property" + " name for different values.", Project.MSG_WARN);
                }
                writeline("/home/ubuntu/results/coverage/Available/Available_4_10.coverage", "d0db1bed-4872-46da-b5e2-ec4af713c983");
                // NB: this makes use of Project#setProperty rather than Project#setNewProperty
                // due to backwards compatibility reasons
                ph.setProperty(property, value, true);
            }
        } finally {
            writeline("/home/ubuntu/results/coverage/Available/Available_4_10.coverage", "e266433e-7972-4f75-9bd0-9cca97ec413c");
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
        writeline("/home/ubuntu/results/coverage/Available/Available_4_10.coverage", "91fa430a-fbad-4a30-b99b-1a57c87d8df7");
        try {
            writeline("/home/ubuntu/results/coverage/Available/Available_4_10.coverage", "b6fcfb41-0fa8-4067-a44a-9af10b2efd50");
            if (classname == null && file == null && resource == null) {
                writeline("/home/ubuntu/results/coverage/Available/Available_4_10.coverage", "d8716c3f-6996-4f02-961e-47a19ac54847");
                throw new BuildException("At least one of (classname|file|" + "resource) is required", getLocation());
            }
            writeline("/home/ubuntu/results/coverage/Available/Available_4_10.coverage", "e191cc7c-a02e-412f-b5b6-d6442b048004");
            if (type != null) {
                writeline("/home/ubuntu/results/coverage/Available/Available_4_10.coverage", "53c6fba8-aaae-49b7-839a-d4591300dcfb");
                if (file == null) {
                    writeline("/home/ubuntu/results/coverage/Available/Available_4_10.coverage", "899f2e15-b17e-4fe8-9bb9-e41560098a7e");
                    throw new BuildException("The type attribute is only valid " + "when specifying the file " + "attribute.", getLocation());
                }
            }
            writeline("/home/ubuntu/results/coverage/Available/Available_4_10.coverage", "ec54bc54-101e-4a67-bc27-c9a27ee47911");
            if (classpath != null) {
                writeline("/home/ubuntu/results/coverage/Available/Available_4_10.coverage", "f7a6668f-e51f-4195-bdd9-f8b7bf6944d9");
                classpath.setProject(getProject());
                writeline("/home/ubuntu/results/coverage/Available/Available_4_10.coverage", "048e2216-4d40-4de0-8112-04fac692dba4");
                this.loader = getProject().createClassLoader(classpath);
            }
            writeline("/home/ubuntu/results/coverage/Available/Available_4_10.coverage", "e2651010-0dab-41f2-98b9-ead0804f9d28");
            String appendix = "";
            writeline("/home/ubuntu/results/coverage/Available/Available_4_10.coverage", "5ef19a6e-027f-480b-abbf-0ea9f9deb56d");
            if (isTask) {
                writeline("/home/ubuntu/results/coverage/Available/Available_4_10.coverage", "4e97fcaa-b480-4297-828a-58b4f53caf42");
                appendix = " to set property " + property;
            } else {
                writeline("/home/ubuntu/results/coverage/Available/Available_4_10.coverage", "ec566c22-b375-4e18-aab9-e710a6fda75d");
                setTaskName("available");
            }
            writeline("/home/ubuntu/results/coverage/Available/Available_4_10.coverage", "9a3fc98a-e95b-4e4b-b762-059aa720657e");
            if ((classname != null) && !checkClass(classname)) {
                writeline("/home/ubuntu/results/coverage/Available/Available_4_10.coverage", "9c9c704b-508c-42eb-a04e-2be78a5ccc4a");
                log("Unable to load class " + classname + appendix, Project.MSG_VERBOSE);
                writeline("/home/ubuntu/results/coverage/Available/Available_4_10.coverage", "d02900f2-59df-48d8-83e1-17e36b0c3ac0");
                return false;
            }
            writeline("/home/ubuntu/results/coverage/Available/Available_4_10.coverage", "b2d2b039-a459-486c-b1a9-3cf1987818f1");
            if ((file != null) && !checkFile()) {
                writeline("/home/ubuntu/results/coverage/Available/Available_4_10.coverage", "8ba8228a-74d1-4514-a878-2f1f1e2268f6");
                StringBuffer buf = new StringBuffer("Unable to find ");
                writeline("/home/ubuntu/results/coverage/Available/Available_4_10.coverage", "5b9c537b-73c2-4c62-bd10-b8ff656d71e1");
                if (type != null) {
                    writeline("/home/ubuntu/results/coverage/Available/Available_4_10.coverage", "d3cf616b-5051-4581-a8cb-2d7dcad8f3f6");
                    buf.append(type).append(' ');
                }
                writeline("/home/ubuntu/results/coverage/Available/Available_4_10.coverage", "8919efb8-ed3c-45dd-af82-d69553531e91");
                buf.append(filename).append(appendix);
                writeline("/home/ubuntu/results/coverage/Available/Available_4_10.coverage", "0c63dc8e-6ac5-468d-820e-5477fcde24f6");
                log(buf.toString(), Project.MSG_VERBOSE);
                writeline("/home/ubuntu/results/coverage/Available/Available_4_10.coverage", "8b9badd4-d63f-4237-a8b9-51dfd9775ec0");
                return false;
            }
            writeline("/home/ubuntu/results/coverage/Available/Available_4_10.coverage", "014d7177-6f0a-410c-982b-e53093beb7f6");
            if ((resource != null) && !checkResource(resource)) {
                writeline("/home/ubuntu/results/coverage/Available/Available_4_10.coverage", "eae0c7cb-24b1-4ab9-b771-665a779cd4a6");
                log("Unable to load resource " + resource + appendix, Project.MSG_VERBOSE);
                writeline("/home/ubuntu/results/coverage/Available/Available_4_10.coverage", "0c76e35d-5aed-4657-aaae-f3b9be214d15");
                return false;
            }
        } finally {
            writeline("/home/ubuntu/results/coverage/Available/Available_4_10.coverage", "c5bf6f27-1a4d-4407-8227-5262d189a21c");
            if (loader != null) {
                writeline("/home/ubuntu/results/coverage/Available/Available_4_10.coverage", "ac1fbe90-639d-4eaa-a438-ab35995ddf3b");
                loader.cleanup();
                writeline("/home/ubuntu/results/coverage/Available/Available_4_10.coverage", "6b5a47a3-7c79-448c-a4c0-c085c30e71b6");
                loader = null;
            }
            writeline("/home/ubuntu/results/coverage/Available/Available_4_10.coverage", "bda67d9e-ec05-4982-aef6-f72408404f32");
            if (!isTask) {
                writeline("/home/ubuntu/results/coverage/Available/Available_4_10.coverage", "e27d3ab5-8afa-4016-9e48-a655e620a945");
                setTaskName(null);
            }
        }
        writeline("/home/ubuntu/results/coverage/Available/Available_4_10.coverage", "aaad506b-6d7c-45e9-b660-564f33115269");
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
        writeline("/home/ubuntu/results/coverage/Available/Available_4_10.coverage", "609b9b0a-9fbc-4216-a6de-8e81b7979556");
        if (filepath == null) {
            writeline("/home/ubuntu/results/coverage/Available/Available_4_10.coverage", "96d6e94b-6af5-43b5-9a42-034fe5305d7e");
            return checkFile(file, filename);
        } else {
            writeline("/home/ubuntu/results/coverage/Available/Available_4_10.coverage", "27c81e29-c778-41fb-8ef7-c3ce7e685502");
            String[] paths = filepath.list();
            writeline("/home/ubuntu/results/coverage/Available/Available_4_10.coverage", "4f6a124f-ebba-4868-95ab-942b4e34c7d5");
            for (int i = 0; i < paths.length; ++i) {
                writeline("/home/ubuntu/results/coverage/Available/Available_4_10.coverage", "5ba673b4-0e5d-4dab-9c42-6b7459d36e6d");
                log("Searching " + paths[i], Project.MSG_VERBOSE);
                writeline("/home/ubuntu/results/coverage/Available/Available_4_10.coverage", "8dd83bac-7943-4ac3-b07b-693c11653356");
                File path = new File(paths[i]);
                writeline("/home/ubuntu/results/coverage/Available/Available_4_10.coverage", "8ab1d2af-0d73-498f-b588-960bdb24f795");
                // **   simple name specified   == path in list
                if (path.exists() && (filename.equals(paths[i]) || filename.equals(path.getName()))) {
                    writeline("/home/ubuntu/results/coverage/Available/Available_4_10.coverage", "11104c5a-c801-417b-9a93-79ec6bc7377d");
                    if (type == null) {
                        writeline("/home/ubuntu/results/coverage/Available/Available_4_10.coverage", "009df7fb-16fc-440d-8b5e-8c91356e431a");
                        log("Found: " + path, Project.MSG_VERBOSE);
                        writeline("/home/ubuntu/results/coverage/Available/Available_4_10.coverage", "2b49f100-b97c-4a60-8719-1d1e36ff59e3");
                        return true;
                    } else if (type.isDir() && path.isDirectory()) {
                        writeline("/home/ubuntu/results/coverage/Available/Available_4_10.coverage", "3308cd58-746d-4bac-84c7-a74e85ebca9d");
                        log("Found directory: " + path, Project.MSG_VERBOSE);
                        writeline("/home/ubuntu/results/coverage/Available/Available_4_10.coverage", "6cc4be3b-751e-4253-8296-7b2237402936");
                        return true;
                    } else if (type.isFile() && path.isFile()) {
                        writeline("/home/ubuntu/results/coverage/Available/Available_4_10.coverage", "b2b31b32-8e63-449f-9780-512aff78c0de");
                        log("Found file: " + path, Project.MSG_VERBOSE);
                        writeline("/home/ubuntu/results/coverage/Available/Available_4_10.coverage", "3a2f3e76-7b9b-4d75-abbb-7f6a332aaa24");
                        return true;
                    }
                    writeline("/home/ubuntu/results/coverage/Available/Available_4_10.coverage", "66f5099a-d0bb-4f8a-91fc-bf05959d1c03");
                    // not the requested type
                    return false;
                }
                writeline("/home/ubuntu/results/coverage/Available/Available_4_10.coverage", "90f90947-b079-4f6d-b760-87d67fa3b925");
                File parent = path.getParentFile();
                writeline("/home/ubuntu/results/coverage/Available/Available_4_10.coverage", "f6d6e1eb-843f-4772-a337-b8deb796b118");
                // **   full-pathname specified == parent dir of path in list
                if (parent != null && parent.exists() && filename.equals(parent.getAbsolutePath())) {
                    writeline("/home/ubuntu/results/coverage/Available/Available_4_10.coverage", "b4940f05-e239-483a-932b-7f4c6a7e4009");
                    if (type == null) {
                        writeline("/home/ubuntu/results/coverage/Available/Available_4_10.coverage", "5041de34-ea00-4130-adcd-88bfbf113f03");
                        log("Found: " + parent, Project.MSG_VERBOSE);
                        writeline("/home/ubuntu/results/coverage/Available/Available_4_10.coverage", "56fa8707-815c-437e-a824-bb2ce54ffe08");
                        return true;
                    } else if (type.isDir()) {
                        writeline("/home/ubuntu/results/coverage/Available/Available_4_10.coverage", "8b34be09-75d8-48af-b607-7557b98672e7");
                        log("Found directory: " + parent, Project.MSG_VERBOSE);
                        writeline("/home/ubuntu/results/coverage/Available/Available_4_10.coverage", "fb50b4b9-7b4f-4c8d-abe1-12ad922ad860");
                        return true;
                    }
                    writeline("/home/ubuntu/results/coverage/Available/Available_4_10.coverage", "3d5efe93-ec91-4738-9f60-cf740b93702e");
                    // not the requested type
                    return false;
                }
                writeline("/home/ubuntu/results/coverage/Available/Available_4_10.coverage", "db5ac040-5ed3-4500-b86b-b2e5136d094a");
                // **   simple name specified   == path in list + name
                if (path.exists() && path.isDirectory()) {
                    writeline("/home/ubuntu/results/coverage/Available/Available_4_10.coverage", "c09ca4e8-d853-4829-927d-a36ae6266c55");
                    if (checkFile(new File(path, filename), filename + " in " + path)) {
                        writeline("/home/ubuntu/results/coverage/Available/Available_4_10.coverage", "49a5be96-86c8-4dc6-ac96-dcb46f32d7ab");
                        return true;
                    }
                }
                writeline("/home/ubuntu/results/coverage/Available/Available_4_10.coverage", "1d39e19f-53b4-4e54-af3b-c32dc0cfaac1");
                // **   simple name specified   == parent dir + name
                while (searchParents && parent != null && parent.exists()) {
                    writeline("/home/ubuntu/results/coverage/Available/Available_4_10.coverage", "75089585-eff6-4abb-8f50-653bd3a263f3");
                    if (checkFile(new File(parent, filename), filename + " in " + parent)) {
                        writeline("/home/ubuntu/results/coverage/Available/Available_4_10.coverage", "666d7d64-8ee8-442d-85ef-63a5ea1deb27");
                        return true;
                    }
                    writeline("/home/ubuntu/results/coverage/Available/Available_4_10.coverage", "2a6cc924-e8ed-4786-8485-50a283c14fc4");
                    parent = parent.getParentFile();
                }
            }
        }
        writeline("/home/ubuntu/results/coverage/Available/Available_4_10.coverage", "8d268947-0479-4747-9629-02a21aa54951");
        return false;
    }

    /**
     * Check if a given file exists and matches the required type.
     */
    private boolean checkFile(File f, String text) {
        writeline("/home/ubuntu/results/coverage/Available/Available_4_10.coverage", "7cf042c8-6ebe-4535-8680-42a361fa4c05");
        if (type != null) {
            writeline("/home/ubuntu/results/coverage/Available/Available_4_10.coverage", "b4d33dda-88a8-4318-82d8-33581e73c927");
            if (type.isDir()) {
                writeline("/home/ubuntu/results/coverage/Available/Available_4_10.coverage", "11189db9-4533-44ce-ab09-03a6e2f5c68d");
                if (f.isDirectory()) {
                    writeline("/home/ubuntu/results/coverage/Available/Available_4_10.coverage", "acc52204-189c-4eff-a292-13261157150a");
                    log("Found directory: " + text, Project.MSG_VERBOSE);
                }
                writeline("/home/ubuntu/results/coverage/Available/Available_4_10.coverage", "ca632215-7ee7-4496-bffc-b224060b4579");
                return f.isDirectory();
            } else if (type.isFile()) {
                writeline("/home/ubuntu/results/coverage/Available/Available_4_10.coverage", "195b0be0-9b1e-46af-a41c-edc3ea7df559");
                if (f.isFile()) {
                    writeline("/home/ubuntu/results/coverage/Available/Available_4_10.coverage", "7c8ae9c1-598c-4ae1-aeda-1f633c9dc70d");
                    log("Found file: " + text, Project.MSG_VERBOSE);
                }
                writeline("/home/ubuntu/results/coverage/Available/Available_4_10.coverage", "278032cb-658a-4807-8d56-8c55df0c44a8");
                return f.isFile();
            }
        }
        writeline("/home/ubuntu/results/coverage/Available/Available_4_10.coverage", "f82b24d8-c93a-408c-98f0-202b0895d4e8");
        if (f.exists()) {
            writeline("/home/ubuntu/results/coverage/Available/Available_4_10.coverage", "5bb09b6a-8ef1-4e70-bcf2-5c75bbd9a742");
            log("Found: " + text, Project.MSG_VERBOSE);
        }
        writeline("/home/ubuntu/results/coverage/Available/Available_4_10.coverage", "7be1ac44-ed63-4676-b2e0-971855376a2a");
        return f.exists();
    }

    /**
     * Check if a given resource can be loaded.
     */
    private boolean checkResource(String resource) {
        writeline("/home/ubuntu/results/coverage/Available/Available_4_10.coverage", "9a36adfc-87f3-4e69-bff4-9e8a0d63c0dc");
        InputStream is = null;
        writeline("/home/ubuntu/results/coverage/Available/Available_4_10.coverage", "db9c38ee-311e-4df8-a37e-e5b63a0d6c54");
        try {
            writeline("/home/ubuntu/results/coverage/Available/Available_4_10.coverage", "f3e2653b-41b0-4ba2-aa19-6d7c3afbf14a");
            if (loader != null) {
                writeline("/home/ubuntu/results/coverage/Available/Available_4_10.coverage", "e034ce5b-5e01-458b-915d-5639b582174c");
                is = loader.getResourceAsStream(resource);
            } else {
                writeline("/home/ubuntu/results/coverage/Available/Available_4_10.coverage", "43c43565-7155-4119-b0c8-d83c1a660962");
                ClassLoader cL = this.getClass().getClassLoader();
                writeline("/home/ubuntu/results/coverage/Available/Available_4_10.coverage", "e236240f-af40-4320-828a-d33bbc763421");
                if (cL != null) {
                    writeline("/home/ubuntu/results/coverage/Available/Available_4_10.coverage", "cbcfc470-e1f5-45d9-ab2c-f021e4aaeeba");
                    is = cL.getResourceAsStream(resource);
                } else {
                    writeline("/home/ubuntu/results/coverage/Available/Available_4_10.coverage", "55158e40-890a-429b-9aa2-33821a1a04e5");
                    is = ClassLoader.getSystemResourceAsStream(resource);
                }
            }
            writeline("/home/ubuntu/results/coverage/Available/Available_4_10.coverage", "2e1e4c18-f28a-4999-9d34-df6eea0c49eb");
            return is != null;
        } finally {
            writeline("/home/ubuntu/results/coverage/Available/Available_4_10.coverage", "2e0b5e52-18c3-4ccd-b6d8-c36b81ca8b5e");
            FileUtils.close(is);
        }
    }

    /**
     * Check if a given class can be loaded.
     */
    private boolean checkClass(String classname) {
        writeline("/home/ubuntu/results/coverage/Available/Available_4_10.coverage", "7da12755-859f-45fe-910f-439e1afe8397");
        try {
            writeline("/home/ubuntu/results/coverage/Available/Available_4_10.coverage", "9a1b9388-9f04-4398-bb98-6880a4349c35");
            if (ignoreSystemclasses) {
                writeline("/home/ubuntu/results/coverage/Available/Available_4_10.coverage", "88b4005e-59c6-475a-9e8d-66a74fc2009a");
                loader = getProject().createClassLoader(classpath);
                writeline("/home/ubuntu/results/coverage/Available/Available_4_10.coverage", "abcefb0c-675b-46bc-9432-96e018f00ef1");
                loader.setParentFirst(false);
                writeline("/home/ubuntu/results/coverage/Available/Available_4_10.coverage", "708af085-3c25-4ab6-bb42-3276f310df9a");
                loader.addJavaLibraries();
                writeline("/home/ubuntu/results/coverage/Available/Available_4_10.coverage", "1c10b79e-b1ce-498e-b54d-109a6e7d48d5");
                try {
                    writeline("/home/ubuntu/results/coverage/Available/Available_4_10.coverage", "786456d0-c314-4235-9292-216020d07291");
                    loader.findClass(classname);
                } catch (SecurityException se) {
                    writeline("/home/ubuntu/results/coverage/Available/Available_4_10.coverage", "2ec3cfad-91b2-4aa1-a1cb-7661f7ea1eb2");
                    // so catch the exception and return
                    return true;
                }
            } else if (loader != null) {
                writeline("/home/ubuntu/results/coverage/Available/Available_4_10.coverage", "d6e43aad-21b8-43a8-a5c7-d70b1b100217");
                loader.loadClass(classname);
            } else {
                writeline("/home/ubuntu/results/coverage/Available/Available_4_10.coverage", "f1eacd07-5906-487f-9146-ea460e04dd36");
                ClassLoader l = this.getClass().getClassLoader();
                writeline("/home/ubuntu/results/coverage/Available/Available_4_10.coverage", "06849991-1d81-4b1f-a379-b5314b6b12f3");
                // see API docs of Class.getClassLoader.
                if (l != null) {
                    writeline("/home/ubuntu/results/coverage/Available/Available_4_10.coverage", "bbafe932-87c7-433f-a6ae-18476b758717");
                    Class.forName(classname, true, l);
                } else {
                    writeline("/home/ubuntu/results/coverage/Available/Available_4_10.coverage", "6040c4fd-36fd-46c1-ad05-11de813a65ac");
                    Class.forName(classname);
                }
            }
            writeline("/home/ubuntu/results/coverage/Available/Available_4_10.coverage", "88029734-2c4c-42e2-b928-930c67aebcd0");
            return true;
        } catch (ClassNotFoundException e) {
            writeline("/home/ubuntu/results/coverage/Available/Available_4_10.coverage", "48812726-aa07-448c-bbe4-05c7bf518e1e");
            log("class \"" + classname + "\" was not found", Project.MSG_DEBUG);
            writeline("/home/ubuntu/results/coverage/Available/Available_4_10.coverage", "0392d384-0ab5-4f01-b30e-630082b9f30f");
            return false;
        } catch (NoClassDefFoundError e) {
            writeline("/home/ubuntu/results/coverage/Available/Available_4_10.coverage", "139b96ce-f998-4acb-abab-fec94706260b");
            log("Could not load dependent class \"" + e.getMessage() + "\" for class \"" + classname + "\"", Project.MSG_DEBUG);
            writeline("/home/ubuntu/results/coverage/Available/Available_4_10.coverage", "688a7fcf-1eba-451a-9813-fb8ad145a5a9");
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
