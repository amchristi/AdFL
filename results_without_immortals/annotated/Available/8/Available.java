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
        writeline("/home/ubuntu/results/coverage/Available/Available_8_10.coverage", "89b3f9f2-8dab-4932-9662-df81cc135f37");
        this.searchParents = searchParents;
    }

    /**
     * Set the classpath to be used when searching for classes and resources.
     *
     * @param classpath an Ant Path object containing the search path.
     */
    public void setClasspath(Path classpath) {
        writeline("/home/ubuntu/results/coverage/Available/Available_8_10.coverage", "87fcd1fd-a0a5-41ac-aa67-899173a00f62");
        createClasspath().append(classpath);
    }

    /**
     * Classpath to be used when searching for classes and resources.
     *
     * @return an empty Path instance to be configured by Ant.
     */
    public Path createClasspath() {
        writeline("/home/ubuntu/results/coverage/Available/Available_8_10.coverage", "1a8e26c7-4404-4edb-ab74-642cbbebcd5d");
        if (this.classpath == null) {
            writeline("/home/ubuntu/results/coverage/Available/Available_8_10.coverage", "70f53d05-d41f-422d-9959-48309e203e88");
            this.classpath = new Path(getProject());
        }
        writeline("/home/ubuntu/results/coverage/Available/Available_8_10.coverage", "96f6b7bc-c2cd-4da2-b5d3-4c78862a54c2");
        return this.classpath.createPath();
    }

    /**
     * Set the classpath by reference.
     *
     * @param r a Reference to a Path instance to be used as the classpath
     * value.
     */
    public void setClasspathRef(Reference r) {
        writeline("/home/ubuntu/results/coverage/Available/Available_8_10.coverage", "41407705-21b9-45a4-aae5-52f302f7f4a1");
        createClasspath().setRefid(r);
    }

    /**
     * Set the path to use when looking for a file.
     *
     * @param filepath a Path instance containing the search path for files.
     */
    public void setFilepath(Path filepath) {
        writeline("/home/ubuntu/results/coverage/Available/Available_8_10.coverage", "769769aa-5a19-42cb-867a-893a6e189337");
        createFilepath().append(filepath);
    }

    /**
     * Path to search for file resources.
     *
     * @return a new Path instance which Ant will configure with a file search
     * path.
     */
    public Path createFilepath() {
        writeline("/home/ubuntu/results/coverage/Available/Available_8_10.coverage", "edcb7072-d8ed-4cce-88bb-3fc79f9873ee");
        if (this.filepath == null) {
            writeline("/home/ubuntu/results/coverage/Available/Available_8_10.coverage", "476ba149-04bc-4152-85d8-eed58e99eebd");
            this.filepath = new Path(getProject());
        }
        writeline("/home/ubuntu/results/coverage/Available/Available_8_10.coverage", "2a509431-5ff1-4411-8713-178533ab178b");
        return this.filepath.createPath();
    }

    /**
     * Set the name of the property which will be set if the particular resource
     * is available.
     *
     * @param property the name of the property to set.
     */
    public void setProperty(String property) {
        writeline("/home/ubuntu/results/coverage/Available/Available_8_10.coverage", "577369eb-3b78-478c-b271-c14bf9e0119d");
        this.property = property;
    }

    /**
     * Set the value to be given to the property if the desired resource is
     * available.
     *
     * @param value the value to be given.
     */
    public void setValue(Object value) {
        writeline("/home/ubuntu/results/coverage/Available/Available_8_10.coverage", "291773d3-e28f-49e1-9640-c606c03aa821");
        this.value = value;
    }

    /**
     * Set the value to be given to the property if the desired resource is
     * available.
     *
     * @param value the value to be given.
     */
    public void setValue(String value) {
        writeline("/home/ubuntu/results/coverage/Available/Available_8_10.coverage", "4d470829-219b-4026-93e1-8d38cc7ae0aa");
        setValue((Object) value);
    }

    /**
     * Set a classname of a class which must be available to set the given
     * property.
     *
     * @param classname the name of the class required.
     */
    public void setClassname(String classname) {
        writeline("/home/ubuntu/results/coverage/Available/Available_8_10.coverage", "f49bab33-dead-4dbd-988e-d3bc07e6fbf7");
        if (!"".equals(classname)) {
            writeline("/home/ubuntu/results/coverage/Available/Available_8_10.coverage", "dbe3f728-f1c9-47e5-b02a-1dd47781aeb8");
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
        writeline("/home/ubuntu/results/coverage/Available/Available_8_10.coverage", "818ce436-be85-4e5b-8078-6a8b364a6699");
        this.file = file;
        writeline("/home/ubuntu/results/coverage/Available/Available_8_10.coverage", "2fca6a3b-f7f3-4046-9f2d-83dc505820bf");
        this.filename = FILE_UTILS.removeLeadingPath(getProject().getBaseDir(), file);
    }

    /**
     * Set the name of a Java resource which is required to set the property.
     *
     * @param resource the name of a resource which is required to be available.
     */
    public void setResource(String resource) {
        writeline("/home/ubuntu/results/coverage/Available/Available_8_10.coverage", "f1bfa518-8cff-4c44-9b73-45fc7a407ddb");
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
        writeline("/home/ubuntu/results/coverage/Available/Available_8_10.coverage", "ca6bcf94-574c-451d-815e-a04919efbe65");
        log("DEPRECATED - The setType(String) method has been deprecated." + " Use setType(Available.FileDir) instead.", Project.MSG_WARN);
        writeline("/home/ubuntu/results/coverage/Available/Available_8_10.coverage", "c1f7b742-7d15-41c8-b864-ec7f3a819ff5");
        this.type = new FileDir();
        writeline("/home/ubuntu/results/coverage/Available/Available_8_10.coverage", "b3ecc60b-770a-4308-a081-605a60e9530c");
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
        writeline("/home/ubuntu/results/coverage/Available/Available_8_10.coverage", "6051c413-67c5-456d-855d-3f83e08a5e58");
        this.type = type;
    }

    /**
     * Set whether the search for classes should ignore the runtime classes and
     * just use the given classpath.
     *
     * @param ignore true if system classes are to be ignored.
     */
    public void setIgnoresystemclasses(boolean ignore) {
        writeline("/home/ubuntu/results/coverage/Available/Available_8_10.coverage", "6303dea0-7802-410d-8987-bd549f9a9f5a");
        this.ignoreSystemclasses = ignore;
    }

    /**
     * Entry point when operating as a task.
     *
     * @exception BuildException if the task is not configured correctly.
     */
    public void execute() throws BuildException {
        writeline("/home/ubuntu/results/coverage/Available/Available_8_10.coverage", "3a17fd61-be10-450e-b080-be91717ad798");
        if (property == null) {
            writeline("/home/ubuntu/results/coverage/Available/Available_8_10.coverage", "31cd9316-f984-4158-a2d9-9b2467c5430a");
            throw new BuildException("property attribute is required", getLocation());
        }
        writeline("/home/ubuntu/results/coverage/Available/Available_8_10.coverage", "fe7db29b-841b-490d-b750-1bd5b5aa5080");
        isTask = true;
        writeline("/home/ubuntu/results/coverage/Available/Available_8_10.coverage", "03fc7caf-a855-4416-97e1-c5d6ac6ddde2");
        try {
            writeline("/home/ubuntu/results/coverage/Available/Available_8_10.coverage", "75e59b61-0e2f-4841-8710-cd632adbbf94");
            if (eval()) {
                writeline("/home/ubuntu/results/coverage/Available/Available_8_10.coverage", "9fbca66d-e90d-4583-b761-9b23a3fa3e95");
                PropertyHelper ph = PropertyHelper.getPropertyHelper(getProject());
                writeline("/home/ubuntu/results/coverage/Available/Available_8_10.coverage", "6afd25cc-cbc1-437a-a8be-7929ebebc9e7");
                Object oldvalue = ph.getProperty(property);
                writeline("/home/ubuntu/results/coverage/Available/Available_8_10.coverage", "4400238a-be12-45db-9431-d24c0082c7bf");
                if (null != oldvalue && !oldvalue.equals(value)) {
                    writeline("/home/ubuntu/results/coverage/Available/Available_8_10.coverage", "d9070d87-6a46-4172-98b9-77e11f7f45f3");
                    log("DEPRECATED - <available> used to override an existing" + " property." + StringUtils.LINE_SEP + "  Build file should not reuse the same property" + " name for different values.", Project.MSG_WARN);
                }
                writeline("/home/ubuntu/results/coverage/Available/Available_8_10.coverage", "503a365d-6b4b-48a4-a153-10a86d3b6c14");
                // NB: this makes use of Project#setProperty rather than Project#setNewProperty
                // due to backwards compatibility reasons
                ph.setProperty(property, value, true);
            }
        } finally {
            writeline("/home/ubuntu/results/coverage/Available/Available_8_10.coverage", "ff58f561-ebf0-42b2-99f8-68fbb938298d");
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
        writeline("/home/ubuntu/results/coverage/Available/Available_8_10.coverage", "2859bd89-1de8-4869-8ddd-ce81c1a5c9b4");
        try {
            writeline("/home/ubuntu/results/coverage/Available/Available_8_10.coverage", "f6ca4b73-18b1-4231-b305-13aa9386ea7b");
            if (classname == null && file == null && resource == null) {
                writeline("/home/ubuntu/results/coverage/Available/Available_8_10.coverage", "13e4eb23-cef9-49e5-b33e-bd65f9413d71");
                throw new BuildException("At least one of (classname|file|" + "resource) is required", getLocation());
            }
            writeline("/home/ubuntu/results/coverage/Available/Available_8_10.coverage", "030e53f6-815b-41f5-b3aa-85faf5ad07ac");
            if (type != null) {
                writeline("/home/ubuntu/results/coverage/Available/Available_8_10.coverage", "19b166cf-65e7-4867-9dd5-1ae951591d50");
                if (file == null) {
                    writeline("/home/ubuntu/results/coverage/Available/Available_8_10.coverage", "8c659bfb-4bb4-4e5a-b1d1-473ac99924ff");
                    throw new BuildException("The type attribute is only valid " + "when specifying the file " + "attribute.", getLocation());
                }
            }
            writeline("/home/ubuntu/results/coverage/Available/Available_8_10.coverage", "1271420d-0881-4a2e-a671-b62e554e5662");
            if (classpath != null) {
                writeline("/home/ubuntu/results/coverage/Available/Available_8_10.coverage", "4f299d13-b203-4e30-950f-86641ff97ad1");
                classpath.setProject(getProject());
                writeline("/home/ubuntu/results/coverage/Available/Available_8_10.coverage", "bede486a-10b7-4434-a34f-900c5247e0df");
                this.loader = getProject().createClassLoader(classpath);
            }
            writeline("/home/ubuntu/results/coverage/Available/Available_8_10.coverage", "48f674c3-a2a8-49c3-9e5d-61c9c058e0af");
            String appendix = "";
            writeline("/home/ubuntu/results/coverage/Available/Available_8_10.coverage", "9df5d721-61d0-427b-87ae-6e9f56d7cbe8");
            if (isTask) {
                writeline("/home/ubuntu/results/coverage/Available/Available_8_10.coverage", "a106ed0f-d6cf-4805-a2f1-9fd062f2705f");
                appendix = " to set property " + property;
            } else {
                writeline("/home/ubuntu/results/coverage/Available/Available_8_10.coverage", "a5fead65-282e-41e9-9807-a307157faef9");
                setTaskName("available");
            }
            writeline("/home/ubuntu/results/coverage/Available/Available_8_10.coverage", "668906c3-a9b0-444f-bced-15d1e3a01ba6");
            if ((classname != null) && !checkClass(classname)) {
                writeline("/home/ubuntu/results/coverage/Available/Available_8_10.coverage", "c1812bee-0b64-4080-b19c-ea96f97f7ac8");
                log("Unable to load class " + classname + appendix, Project.MSG_VERBOSE);
                writeline("/home/ubuntu/results/coverage/Available/Available_8_10.coverage", "eefee9b5-6ca4-4f00-aac1-827a1ca613a0");
                return false;
            }
            writeline("/home/ubuntu/results/coverage/Available/Available_8_10.coverage", "eeeb4dcb-daed-4741-9e09-62419f94531c");
            if ((file != null) && !checkFile()) {
                writeline("/home/ubuntu/results/coverage/Available/Available_8_10.coverage", "faafac2c-b83c-45de-b36b-b87d6179cfde");
                StringBuffer buf = new StringBuffer("Unable to find ");
                writeline("/home/ubuntu/results/coverage/Available/Available_8_10.coverage", "9cb4e199-9fa8-4ef5-9a6c-398e0d3f7938");
                if (type != null) {
                    writeline("/home/ubuntu/results/coverage/Available/Available_8_10.coverage", "6c7f9ec8-0dcc-494a-9689-efd64e15d531");
                    buf.append(type).append(' ');
                }
                writeline("/home/ubuntu/results/coverage/Available/Available_8_10.coverage", "9650d6d3-132f-437e-89d7-4852643549ec");
                buf.append(filename).append(appendix);
                writeline("/home/ubuntu/results/coverage/Available/Available_8_10.coverage", "b27cc889-8ffd-4c2c-937b-3e859723813f");
                log(buf.toString(), Project.MSG_VERBOSE);
                writeline("/home/ubuntu/results/coverage/Available/Available_8_10.coverage", "495c84de-eb5f-472f-ad5e-e4e2fbd477bb");
                return false;
            }
            writeline("/home/ubuntu/results/coverage/Available/Available_8_10.coverage", "2cad5f6a-9513-4350-89ef-2e78e1ce8786");
            if ((resource != null) && !checkResource(resource)) {
                writeline("/home/ubuntu/results/coverage/Available/Available_8_10.coverage", "125a47d6-af7f-4d21-b82e-44025cd1902b");
                log("Unable to load resource " + resource + appendix, Project.MSG_VERBOSE);
                writeline("/home/ubuntu/results/coverage/Available/Available_8_10.coverage", "6cf30d50-ecbd-41f8-b380-09f090065b14");
                return false;
            }
        } finally {
            writeline("/home/ubuntu/results/coverage/Available/Available_8_10.coverage", "6a09eee0-6144-490e-ac84-d5252ddaf308");
            if (loader != null) {
                writeline("/home/ubuntu/results/coverage/Available/Available_8_10.coverage", "e20e16d8-8d5f-4129-ad0c-9cf0c908d512");
                loader.cleanup();
                writeline("/home/ubuntu/results/coverage/Available/Available_8_10.coverage", "06b32279-62a9-420b-a7cc-05aa421cbe2f");
                loader = null;
            }
            writeline("/home/ubuntu/results/coverage/Available/Available_8_10.coverage", "9201960c-f4c0-47ee-9b12-2a83c3b4a085");
            if (!isTask) {
                writeline("/home/ubuntu/results/coverage/Available/Available_8_10.coverage", "36eca4df-980c-403e-8a96-de899d283eab");
                setTaskName(null);
            }
        }
        writeline("/home/ubuntu/results/coverage/Available/Available_8_10.coverage", "3915bba7-fe14-4f5c-8419-71a0973f2d6a");
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
        writeline("/home/ubuntu/results/coverage/Available/Available_8_10.coverage", "a34d294b-b90a-440b-9fda-210298bc170e");
        if (filepath == null) {
            writeline("/home/ubuntu/results/coverage/Available/Available_8_10.coverage", "dc87c513-6006-4939-9343-f036e3c5b84f");
            return checkFile(file, filename);
        } else {
            writeline("/home/ubuntu/results/coverage/Available/Available_8_10.coverage", "024af047-c9a3-4365-a71a-f9ac48590790");
            String[] paths = filepath.list();
            writeline("/home/ubuntu/results/coverage/Available/Available_8_10.coverage", "bfe3e34b-d926-4845-b702-99974afb94d0");
            for (int i = 0; i < paths.length; ++i) {
                writeline("/home/ubuntu/results/coverage/Available/Available_8_10.coverage", "b8def4f3-acc1-4733-9100-1713103ef53d");
                log("Searching " + paths[i], Project.MSG_VERBOSE);
                writeline("/home/ubuntu/results/coverage/Available/Available_8_10.coverage", "4d77f223-ba5b-4f9a-9bfd-3f49b762dce0");
                File path = new File(paths[i]);
                writeline("/home/ubuntu/results/coverage/Available/Available_8_10.coverage", "e3758e1c-f88e-4b1b-869b-840a5eadbe88");
                // **   simple name specified   == path in list
                if (path.exists() && (filename.equals(paths[i]) || filename.equals(path.getName()))) {
                    writeline("/home/ubuntu/results/coverage/Available/Available_8_10.coverage", "10c08f5a-ef5f-4f5d-86aa-46448288e4b1");
                    if (type == null) {
                        writeline("/home/ubuntu/results/coverage/Available/Available_8_10.coverage", "d98cafbb-9495-445d-afc0-92524abf31fd");
                        log("Found: " + path, Project.MSG_VERBOSE);
                        writeline("/home/ubuntu/results/coverage/Available/Available_8_10.coverage", "e361dbaf-f672-4b38-9187-e473b4c96628");
                        return true;
                    } else if (type.isDir() && path.isDirectory()) {
                        writeline("/home/ubuntu/results/coverage/Available/Available_8_10.coverage", "34f749fe-afdb-441f-9971-5330c414dce0");
                        log("Found directory: " + path, Project.MSG_VERBOSE);
                        writeline("/home/ubuntu/results/coverage/Available/Available_8_10.coverage", "a55dd141-2940-4686-9e66-ecd8df8a8c8b");
                        return true;
                    } else if (type.isFile() && path.isFile()) {
                        writeline("/home/ubuntu/results/coverage/Available/Available_8_10.coverage", "73f4c722-5419-4225-a222-746a552e1e40");
                        log("Found file: " + path, Project.MSG_VERBOSE);
                        writeline("/home/ubuntu/results/coverage/Available/Available_8_10.coverage", "0a94054a-5e39-4eb3-8af9-387fedb8496b");
                        return true;
                    }
                    writeline("/home/ubuntu/results/coverage/Available/Available_8_10.coverage", "0183db07-bd95-4734-8629-34a6b47c7e85");
                    // not the requested type
                    return false;
                }
                writeline("/home/ubuntu/results/coverage/Available/Available_8_10.coverage", "99f904cf-2d05-4910-882e-dca9579e91be");
                File parent = path.getParentFile();
                writeline("/home/ubuntu/results/coverage/Available/Available_8_10.coverage", "cc42cf46-6270-4478-afb7-0196f6b9b742");
                // **   full-pathname specified == parent dir of path in list
                if (parent != null && parent.exists() && filename.equals(parent.getAbsolutePath())) {
                    writeline("/home/ubuntu/results/coverage/Available/Available_8_10.coverage", "20762b31-b234-4e1f-b259-4f453327a53d");
                    if (type == null) {
                        writeline("/home/ubuntu/results/coverage/Available/Available_8_10.coverage", "c8669541-53ab-4dd7-9a91-22f34f680d02");
                        log("Found: " + parent, Project.MSG_VERBOSE);
                        writeline("/home/ubuntu/results/coverage/Available/Available_8_10.coverage", "b44055ec-b4a2-419f-8252-9460459b87d5");
                        return true;
                    } else if (type.isDir()) {
                        writeline("/home/ubuntu/results/coverage/Available/Available_8_10.coverage", "13eb33e7-4760-485e-b15d-432dd922415a");
                        log("Found directory: " + parent, Project.MSG_VERBOSE);
                        writeline("/home/ubuntu/results/coverage/Available/Available_8_10.coverage", "2b0b87a6-15df-49bc-b390-6cddb86366ca");
                        return true;
                    }
                    writeline("/home/ubuntu/results/coverage/Available/Available_8_10.coverage", "27321c07-7e1e-40b5-8a87-e06cc5c4b2ab");
                    // not the requested type
                    return false;
                }
                writeline("/home/ubuntu/results/coverage/Available/Available_8_10.coverage", "af05a624-5070-4e07-a564-0d1d70fb7d18");
                // **   simple name specified   == path in list + name
                if (path.exists() && path.isDirectory()) {
                    writeline("/home/ubuntu/results/coverage/Available/Available_8_10.coverage", "1037d26b-457a-4a2e-b9cd-d9cfa1d89369");
                    if (checkFile(new File(path, filename), filename + " in " + path)) {
                        writeline("/home/ubuntu/results/coverage/Available/Available_8_10.coverage", "dcffe8fc-5254-4121-bad8-3993cc90f868");
                        return true;
                    }
                }
                writeline("/home/ubuntu/results/coverage/Available/Available_8_10.coverage", "fb5242dd-8fb3-4101-9c25-ff0dc0a97c77");
                // **   simple name specified   == parent dir + name
                while (searchParents && parent != null && parent.exists()) {
                    writeline("/home/ubuntu/results/coverage/Available/Available_8_10.coverage", "6f958099-cda5-4b0f-b78c-425780168c00");
                    if (checkFile(new File(parent, filename), filename + " in " + parent)) {
                        writeline("/home/ubuntu/results/coverage/Available/Available_8_10.coverage", "9384b48f-4745-46ec-a588-763e3af1a587");
                        return true;
                    }
                    writeline("/home/ubuntu/results/coverage/Available/Available_8_10.coverage", "f515b1fe-2673-4656-ae12-1eaa58707aed");
                    parent = parent.getParentFile();
                }
            }
        }
        writeline("/home/ubuntu/results/coverage/Available/Available_8_10.coverage", "a357ec8e-99a3-4066-873a-ffd9bff0c845");
        return false;
    }

    /**
     * Check if a given file exists and matches the required type.
     */
    private boolean checkFile(File f, String text) {
        writeline("/home/ubuntu/results/coverage/Available/Available_8_10.coverage", "007bf7ef-5655-4cf4-9dd8-7b96f521e170");
        if (type != null) {
            writeline("/home/ubuntu/results/coverage/Available/Available_8_10.coverage", "13e97e5b-0d56-43ed-8280-aa359e34ff3e");
            if (type.isDir()) {
                writeline("/home/ubuntu/results/coverage/Available/Available_8_10.coverage", "2d47074a-11ce-4a06-9d86-abcf8c7f436a");
                if (f.isDirectory()) {
                    writeline("/home/ubuntu/results/coverage/Available/Available_8_10.coverage", "8e475e19-c5aa-44c0-ac5c-2df7606ad838");
                    log("Found directory: " + text, Project.MSG_VERBOSE);
                }
                writeline("/home/ubuntu/results/coverage/Available/Available_8_10.coverage", "1440cc8e-0c6f-4294-9cf0-d05bf989548a");
                return f.isDirectory();
            } else if (type.isFile()) {
                writeline("/home/ubuntu/results/coverage/Available/Available_8_10.coverage", "1d86ba5e-dab7-4a89-8f9c-fe2a5d41d405");
                if (f.isFile()) {
                    writeline("/home/ubuntu/results/coverage/Available/Available_8_10.coverage", "2a964c6b-8593-460b-a949-1160936b1ce0");
                    log("Found file: " + text, Project.MSG_VERBOSE);
                }
                writeline("/home/ubuntu/results/coverage/Available/Available_8_10.coverage", "3033682c-8a26-40e4-a7a1-2b9fd6c1be59");
                return f.isFile();
            }
        }
        writeline("/home/ubuntu/results/coverage/Available/Available_8_10.coverage", "03b2d7ca-53d7-4f00-83a8-b6db2dfed67e");
        if (f.exists()) {
            writeline("/home/ubuntu/results/coverage/Available/Available_8_10.coverage", "b8c9723b-0dea-473e-b161-4796adedd9b5");
            log("Found: " + text, Project.MSG_VERBOSE);
        }
        writeline("/home/ubuntu/results/coverage/Available/Available_8_10.coverage", "1217d8ce-c509-4ddd-b168-21db7697c994");
        return f.exists();
    }

    /**
     * Check if a given resource can be loaded.
     */
    private boolean checkResource(String resource) {
        writeline("/home/ubuntu/results/coverage/Available/Available_8_10.coverage", "6dea93cc-f4f4-4994-91bd-e23423a314ac");
        InputStream is = null;
        writeline("/home/ubuntu/results/coverage/Available/Available_8_10.coverage", "24429a99-3392-4ab4-bb96-9d30dcdb3de2");
        try {
            writeline("/home/ubuntu/results/coverage/Available/Available_8_10.coverage", "97643645-9407-49ee-9092-fb7606b6aa92");
            if (loader != null) {
                writeline("/home/ubuntu/results/coverage/Available/Available_8_10.coverage", "6b720b93-4469-43e3-bf33-bd5ceacc8873");
                is = loader.getResourceAsStream(resource);
            } else {
                writeline("/home/ubuntu/results/coverage/Available/Available_8_10.coverage", "afbbfa68-8519-4da9-8963-d9c54f399100");
                ClassLoader cL = this.getClass().getClassLoader();
                writeline("/home/ubuntu/results/coverage/Available/Available_8_10.coverage", "c5e543e2-c04c-4ed7-a3e0-c693efe336a9");
                if (cL != null) {
                    writeline("/home/ubuntu/results/coverage/Available/Available_8_10.coverage", "4b01e42f-3a8c-410b-8032-717542a946e4");
                    is = cL.getResourceAsStream(resource);
                } else {
                    writeline("/home/ubuntu/results/coverage/Available/Available_8_10.coverage", "4e5010ca-fa11-4fe7-b33b-49d7a0f2ec8a");
                    is = ClassLoader.getSystemResourceAsStream(resource);
                }
            }
            writeline("/home/ubuntu/results/coverage/Available/Available_8_10.coverage", "7638dd29-5da8-4654-b5f6-aafd74b11b1f");
            return is != null;
        } finally {
            writeline("/home/ubuntu/results/coverage/Available/Available_8_10.coverage", "adf4d187-3f6d-42bf-95b5-7d4a614c14f1");
            FileUtils.close(is);
        }
    }

    /**
     * Check if a given class can be loaded.
     */
    private boolean checkClass(String classname) {
        writeline("/home/ubuntu/results/coverage/Available/Available_8_10.coverage", "2d4b7580-6532-4947-8ec3-25501d5088dd");
        try {
            writeline("/home/ubuntu/results/coverage/Available/Available_8_10.coverage", "0793aac5-bdb5-4374-8e81-9eb785efb136");
            if (ignoreSystemclasses) {
                writeline("/home/ubuntu/results/coverage/Available/Available_8_10.coverage", "b972391d-0a3c-44c1-9fa0-243fb0bb134b");
                loader = getProject().createClassLoader(classpath);
                writeline("/home/ubuntu/results/coverage/Available/Available_8_10.coverage", "5111dcbb-41fc-4189-bef9-6629eb4ee37e");
                loader.setParentFirst(false);
                writeline("/home/ubuntu/results/coverage/Available/Available_8_10.coverage", "a1204180-9e96-4a71-8d08-c571b7c96549");
                loader.addJavaLibraries();
                writeline("/home/ubuntu/results/coverage/Available/Available_8_10.coverage", "4aa1267a-6861-4e4f-bc00-f38b67c9a9ed");
                try {
                    writeline("/home/ubuntu/results/coverage/Available/Available_8_10.coverage", "09ef7d95-3c0b-4bac-9a6f-4a62368259d4");
                    loader.findClass(classname);
                } catch (SecurityException se) {
                    writeline("/home/ubuntu/results/coverage/Available/Available_8_10.coverage", "47bbf409-5dcd-4a37-a452-d0fe74f9c074");
                    // so catch the exception and return
                    return true;
                }
            } else if (loader != null) {
                writeline("/home/ubuntu/results/coverage/Available/Available_8_10.coverage", "1ea82c8b-e876-49cd-9c66-1a008911acbd");
                loader.loadClass(classname);
            } else {
                writeline("/home/ubuntu/results/coverage/Available/Available_8_10.coverage", "caa36ebb-29f6-49d9-8fef-e416c7752aa0");
                ClassLoader l = this.getClass().getClassLoader();
                writeline("/home/ubuntu/results/coverage/Available/Available_8_10.coverage", "9ff0c556-f190-4be2-96cc-599d3e077b1e");
                // see API docs of Class.getClassLoader.
                if (l != null) {
                    writeline("/home/ubuntu/results/coverage/Available/Available_8_10.coverage", "b31c6f82-e44c-4571-bd2b-cf25d906dd0c");
                    Class.forName(classname, true, l);
                } else {
                    writeline("/home/ubuntu/results/coverage/Available/Available_8_10.coverage", "ee8d48d4-c92f-4ab8-9448-bdfd9842f1f9");
                    Class.forName(classname);
                }
            }
            writeline("/home/ubuntu/results/coverage/Available/Available_8_10.coverage", "5cb0b3e9-e37c-466f-8fd7-924978da8f5f");
            return true;
        } catch (ClassNotFoundException e) {
            writeline("/home/ubuntu/results/coverage/Available/Available_8_10.coverage", "9189c4fb-eda4-4446-ab9f-40b516270406");
            log("class \"" + classname + "\" was not found", Project.MSG_DEBUG);
            writeline("/home/ubuntu/results/coverage/Available/Available_8_10.coverage", "4be4e754-1aea-4ca6-8bf0-60fbaf1a679d");
            return false;
        } catch (NoClassDefFoundError e) {
            writeline("/home/ubuntu/results/coverage/Available/Available_8_10.coverage", "2bee73c5-82f7-487a-8fea-a0968f252137");
            log("Could not load dependent class \"" + e.getMessage() + "\" for class \"" + classname + "\"", Project.MSG_DEBUG);
            writeline("/home/ubuntu/results/coverage/Available/Available_8_10.coverage", "510e7249-d313-4a55-afba-a4973323e97f");
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
