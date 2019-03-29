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
        writeline("/home/ubuntu/results/coverage/Available/Available_5_10.coverage", "5e1d23c8-7b75-4524-8052-166ef6ff6fe2");
        this.searchParents = searchParents;
    }

    /**
     * Set the classpath to be used when searching for classes and resources.
     *
     * @param classpath an Ant Path object containing the search path.
     */
    public void setClasspath(Path classpath) {
        writeline("/home/ubuntu/results/coverage/Available/Available_5_10.coverage", "2a930e73-0b76-4283-be99-7385dc4d573c");
        createClasspath().append(classpath);
    }

    /**
     * Classpath to be used when searching for classes and resources.
     *
     * @return an empty Path instance to be configured by Ant.
     */
    public Path createClasspath() {
        writeline("/home/ubuntu/results/coverage/Available/Available_5_10.coverage", "0ed8c4c9-7be4-496b-a1ec-6d8541fcd550");
        if (this.classpath == null) {
            writeline("/home/ubuntu/results/coverage/Available/Available_5_10.coverage", "b52f1788-912b-4cb4-bece-565c29f87bf9");
            this.classpath = new Path(getProject());
        }
        writeline("/home/ubuntu/results/coverage/Available/Available_5_10.coverage", "6bbc320c-71df-4d81-895d-99f6156faa28");
        return this.classpath.createPath();
    }

    /**
     * Set the classpath by reference.
     *
     * @param r a Reference to a Path instance to be used as the classpath
     * value.
     */
    public void setClasspathRef(Reference r) {
        writeline("/home/ubuntu/results/coverage/Available/Available_5_10.coverage", "c7ce43c7-a41b-48f7-b3df-5b044776ffee");
        createClasspath().setRefid(r);
    }

    /**
     * Set the path to use when looking for a file.
     *
     * @param filepath a Path instance containing the search path for files.
     */
    public void setFilepath(Path filepath) {
        writeline("/home/ubuntu/results/coverage/Available/Available_5_10.coverage", "0570bd2f-b992-40bf-a89e-5236ac8748d6");
        createFilepath().append(filepath);
    }

    /**
     * Path to search for file resources.
     *
     * @return a new Path instance which Ant will configure with a file search
     * path.
     */
    public Path createFilepath() {
        writeline("/home/ubuntu/results/coverage/Available/Available_5_10.coverage", "574430bd-62be-43f2-ac9e-75ae64564d08");
        if (this.filepath == null) {
            writeline("/home/ubuntu/results/coverage/Available/Available_5_10.coverage", "2ed27411-6f7a-4d58-b6b8-9d1b10116e4b");
            this.filepath = new Path(getProject());
        }
        writeline("/home/ubuntu/results/coverage/Available/Available_5_10.coverage", "b9c58fc0-a7b5-4b84-ba0e-cb0cc5116c9a");
        return this.filepath.createPath();
    }

    /**
     * Set the name of the property which will be set if the particular resource
     * is available.
     *
     * @param property the name of the property to set.
     */
    public void setProperty(String property) {
        writeline("/home/ubuntu/results/coverage/Available/Available_5_10.coverage", "91a14217-51e9-487b-b5a1-610a2955f3c9");
        this.property = property;
    }

    /**
     * Set the value to be given to the property if the desired resource is
     * available.
     *
     * @param value the value to be given.
     */
    public void setValue(Object value) {
        writeline("/home/ubuntu/results/coverage/Available/Available_5_10.coverage", "60531888-1f98-4b6f-a1f9-814749ee36f5");
        this.value = value;
    }

    /**
     * Set the value to be given to the property if the desired resource is
     * available.
     *
     * @param value the value to be given.
     */
    public void setValue(String value) {
        writeline("/home/ubuntu/results/coverage/Available/Available_5_10.coverage", "e00e201a-fc14-44c2-8d97-51adc23dc210");
        setValue((Object) value);
    }

    /**
     * Set a classname of a class which must be available to set the given
     * property.
     *
     * @param classname the name of the class required.
     */
    public void setClassname(String classname) {
        writeline("/home/ubuntu/results/coverage/Available/Available_5_10.coverage", "caa828cb-7e8c-43bc-bf0d-7e7a94866c05");
        if (!"".equals(classname)) {
            writeline("/home/ubuntu/results/coverage/Available/Available_5_10.coverage", "c676d265-57db-456a-9b94-46e4eab634d2");
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
        writeline("/home/ubuntu/results/coverage/Available/Available_5_10.coverage", "988919a5-b172-435c-ad10-536da4ad0e79");
        this.file = file;
        writeline("/home/ubuntu/results/coverage/Available/Available_5_10.coverage", "2f51429e-037c-4621-9b55-826c13c540a1");
        this.filename = FILE_UTILS.removeLeadingPath(getProject().getBaseDir(), file);
    }

    /**
     * Set the name of a Java resource which is required to set the property.
     *
     * @param resource the name of a resource which is required to be available.
     */
    public void setResource(String resource) {
        writeline("/home/ubuntu/results/coverage/Available/Available_5_10.coverage", "7738168f-7c4b-4980-b29d-2d328e767947");
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
        writeline("/home/ubuntu/results/coverage/Available/Available_5_10.coverage", "4b373151-dd77-40e0-b9b6-7c1a2733e819");
        log("DEPRECATED - The setType(String) method has been deprecated." + " Use setType(Available.FileDir) instead.", Project.MSG_WARN);
        writeline("/home/ubuntu/results/coverage/Available/Available_5_10.coverage", "b7ab40f3-e8ad-4274-a490-05a1641fa629");
        this.type = new FileDir();
        writeline("/home/ubuntu/results/coverage/Available/Available_5_10.coverage", "c9ae0186-74f0-45a7-9e35-dec543078431");
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
        writeline("/home/ubuntu/results/coverage/Available/Available_5_10.coverage", "5f35ddcb-3d33-4a23-9ae7-f926c6a4e7ec");
        this.type = type;
    }

    /**
     * Set whether the search for classes should ignore the runtime classes and
     * just use the given classpath.
     *
     * @param ignore true if system classes are to be ignored.
     */
    public void setIgnoresystemclasses(boolean ignore) {
        writeline("/home/ubuntu/results/coverage/Available/Available_5_10.coverage", "cf34d4a0-2ccf-475a-8e45-19a98c8b9734");
        this.ignoreSystemclasses = ignore;
    }

    /**
     * Entry point when operating as a task.
     *
     * @exception BuildException if the task is not configured correctly.
     */
    public void execute() throws BuildException {
        writeline("/home/ubuntu/results/coverage/Available/Available_5_10.coverage", "604a86ce-8b20-4be1-80f3-ea32bce0621b");
        if (property == null) {
            writeline("/home/ubuntu/results/coverage/Available/Available_5_10.coverage", "70766f62-129e-4b96-b138-3568aacff3b4");
            throw new BuildException("property attribute is required", getLocation());
        }
        writeline("/home/ubuntu/results/coverage/Available/Available_5_10.coverage", "79fd96f7-df3b-4c21-b1bc-c34d4e8c1c70");
        isTask = true;
        writeline("/home/ubuntu/results/coverage/Available/Available_5_10.coverage", "aaf0cac4-a1d4-4ec3-8227-56d502c71b1d");
        try {
            writeline("/home/ubuntu/results/coverage/Available/Available_5_10.coverage", "76c3861c-9c0d-425a-ae4e-fed292de9ef9");
            if (eval()) {
                writeline("/home/ubuntu/results/coverage/Available/Available_5_10.coverage", "16fbb42d-c023-4f5a-b237-10c1bfd6b6eb");
                PropertyHelper ph = PropertyHelper.getPropertyHelper(getProject());
                writeline("/home/ubuntu/results/coverage/Available/Available_5_10.coverage", "9c6481fc-89db-4885-849b-496e7c05f12e");
                Object oldvalue = ph.getProperty(property);
                writeline("/home/ubuntu/results/coverage/Available/Available_5_10.coverage", "5f0b6547-5764-4979-b249-87b1c92e43d4");
                if (null != oldvalue && !oldvalue.equals(value)) {
                    writeline("/home/ubuntu/results/coverage/Available/Available_5_10.coverage", "db2a36d7-9125-4d75-b605-2a8b936fcbc5");
                    log("DEPRECATED - <available> used to override an existing" + " property." + StringUtils.LINE_SEP + "  Build file should not reuse the same property" + " name for different values.", Project.MSG_WARN);
                }
                writeline("/home/ubuntu/results/coverage/Available/Available_5_10.coverage", "97a31ce3-d99e-4c07-848a-ae51497fb2d5");
                // NB: this makes use of Project#setProperty rather than Project#setNewProperty
                // due to backwards compatibility reasons
                ph.setProperty(property, value, true);
            }
        } finally {
            writeline("/home/ubuntu/results/coverage/Available/Available_5_10.coverage", "bb355708-2eca-4bc3-84ea-f2632bb32c1c");
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
        writeline("/home/ubuntu/results/coverage/Available/Available_5_10.coverage", "7607fdb2-54b1-4515-8ee4-404de5a3ad4b");
        try {
            writeline("/home/ubuntu/results/coverage/Available/Available_5_10.coverage", "4ec708ca-b831-4a7b-9bc1-80e3a52e4f56");
            if (classname == null && file == null && resource == null) {
                writeline("/home/ubuntu/results/coverage/Available/Available_5_10.coverage", "8424926b-dff1-4014-bbf9-48818a2c7af1");
                throw new BuildException("At least one of (classname|file|" + "resource) is required", getLocation());
            }
            writeline("/home/ubuntu/results/coverage/Available/Available_5_10.coverage", "53f4b2d5-aadf-46e4-8bba-6eb52d153db9");
            if (type != null) {
                writeline("/home/ubuntu/results/coverage/Available/Available_5_10.coverage", "cf3dff62-2a78-4244-b32e-201bfa7f0fe7");
                if (file == null) {
                    writeline("/home/ubuntu/results/coverage/Available/Available_5_10.coverage", "2eab924a-38d7-4400-81ca-b046740ed8bf");
                    throw new BuildException("The type attribute is only valid " + "when specifying the file " + "attribute.", getLocation());
                }
            }
            writeline("/home/ubuntu/results/coverage/Available/Available_5_10.coverage", "bd212592-2df6-417b-8df2-08adeac80a0b");
            if (classpath != null) {
                writeline("/home/ubuntu/results/coverage/Available/Available_5_10.coverage", "86e7a7aa-1b51-4cd0-acd3-465ad007af84");
                classpath.setProject(getProject());
                writeline("/home/ubuntu/results/coverage/Available/Available_5_10.coverage", "89e03bab-b032-4b03-9685-df3e279c38dc");
                this.loader = getProject().createClassLoader(classpath);
            }
            writeline("/home/ubuntu/results/coverage/Available/Available_5_10.coverage", "76473675-59e4-4c42-8a5c-08372ed610b7");
            String appendix = "";
            writeline("/home/ubuntu/results/coverage/Available/Available_5_10.coverage", "f4b466b9-bf6d-44bc-ae31-17be2dc65abf");
            if (isTask) {
                writeline("/home/ubuntu/results/coverage/Available/Available_5_10.coverage", "7e6ee61e-138e-41e9-a5c9-963a4815f413");
                appendix = " to set property " + property;
            } else {
                writeline("/home/ubuntu/results/coverage/Available/Available_5_10.coverage", "dd6304ab-6353-457c-9046-40b4d84ae0b6");
                setTaskName("available");
            }
            writeline("/home/ubuntu/results/coverage/Available/Available_5_10.coverage", "07f3e718-76f8-48e6-98d3-a42c6ea0c4db");
            if ((classname != null) && !checkClass(classname)) {
                writeline("/home/ubuntu/results/coverage/Available/Available_5_10.coverage", "4a0a1cee-facf-4f78-9a6c-0efbea01ffd3");
                log("Unable to load class " + classname + appendix, Project.MSG_VERBOSE);
                writeline("/home/ubuntu/results/coverage/Available/Available_5_10.coverage", "646cd6d9-3d5b-4dfb-a66e-c8550691898c");
                return false;
            }
            writeline("/home/ubuntu/results/coverage/Available/Available_5_10.coverage", "239f1b50-ff28-4685-ae7d-c0d61d8d1f28");
            if ((file != null) && !checkFile()) {
                writeline("/home/ubuntu/results/coverage/Available/Available_5_10.coverage", "7ab8ef72-1c7b-449b-a45c-a41ae48fefd2");
                StringBuffer buf = new StringBuffer("Unable to find ");
                writeline("/home/ubuntu/results/coverage/Available/Available_5_10.coverage", "b2276903-3323-4072-8141-caf939a24cb9");
                if (type != null) {
                    writeline("/home/ubuntu/results/coverage/Available/Available_5_10.coverage", "040ac016-f555-4364-b1c1-7ffe63c79a5b");
                    buf.append(type).append(' ');
                }
                writeline("/home/ubuntu/results/coverage/Available/Available_5_10.coverage", "bbc85c6c-9fe0-4215-987d-b293debf779c");
                buf.append(filename).append(appendix);
                writeline("/home/ubuntu/results/coverage/Available/Available_5_10.coverage", "9f59d115-e4f7-433f-9b30-0aea9bf251b6");
                log(buf.toString(), Project.MSG_VERBOSE);
                writeline("/home/ubuntu/results/coverage/Available/Available_5_10.coverage", "0386d56f-d0b4-46dd-ad4f-fb3733d5c5ff");
                return false;
            }
            writeline("/home/ubuntu/results/coverage/Available/Available_5_10.coverage", "b353d53e-890b-490c-8619-c230cd6ecacb");
            if ((resource != null) && !checkResource(resource)) {
                writeline("/home/ubuntu/results/coverage/Available/Available_5_10.coverage", "22de6e8f-4f77-4063-943d-4e6f3dd3db08");
                log("Unable to load resource " + resource + appendix, Project.MSG_VERBOSE);
                writeline("/home/ubuntu/results/coverage/Available/Available_5_10.coverage", "e34fe860-8809-4bf0-86c5-f907aaa03196");
                return false;
            }
        } finally {
            writeline("/home/ubuntu/results/coverage/Available/Available_5_10.coverage", "416ddf16-fa76-4762-bf3c-0ab1f57f6373");
            if (loader != null) {
                writeline("/home/ubuntu/results/coverage/Available/Available_5_10.coverage", "39e69c11-b2ed-42b3-8a7d-054c91a5ddc9");
                loader.cleanup();
                writeline("/home/ubuntu/results/coverage/Available/Available_5_10.coverage", "b59301a9-a590-48fd-a5ae-5fa9a1968333");
                loader = null;
            }
            writeline("/home/ubuntu/results/coverage/Available/Available_5_10.coverage", "e463e32a-520f-4fb2-83f2-84e58522a155");
            if (!isTask) {
                writeline("/home/ubuntu/results/coverage/Available/Available_5_10.coverage", "914f394b-1a95-4b6c-93b7-55446d7602bc");
                setTaskName(null);
            }
        }
        writeline("/home/ubuntu/results/coverage/Available/Available_5_10.coverage", "37d2c76f-3975-4572-9997-438b20ec2af1");
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
        writeline("/home/ubuntu/results/coverage/Available/Available_5_10.coverage", "f208031e-2a4d-4e24-8f39-4bff920810e9");
        if (filepath == null) {
            writeline("/home/ubuntu/results/coverage/Available/Available_5_10.coverage", "c167457a-2099-4288-99c1-1c957ff6c797");
            return checkFile(file, filename);
        } else {
            writeline("/home/ubuntu/results/coverage/Available/Available_5_10.coverage", "9924f351-278a-497e-9f80-273808e4c07f");
            String[] paths = filepath.list();
            writeline("/home/ubuntu/results/coverage/Available/Available_5_10.coverage", "8d5a3a51-69e3-4b50-8387-9ab41cd2a8aa");
            for (int i = 0; i < paths.length; ++i) {
                writeline("/home/ubuntu/results/coverage/Available/Available_5_10.coverage", "fa7a117d-a228-4104-9795-0c2f3a8b5b39");
                log("Searching " + paths[i], Project.MSG_VERBOSE);
                writeline("/home/ubuntu/results/coverage/Available/Available_5_10.coverage", "35e81d65-ce7c-4ea1-8cbe-2df81fbff15b");
                File path = new File(paths[i]);
                writeline("/home/ubuntu/results/coverage/Available/Available_5_10.coverage", "f65dc3e0-2e7a-4069-aa7a-697b2f7f9834");
                // **   simple name specified   == path in list
                if (path.exists() && (filename.equals(paths[i]) || filename.equals(path.getName()))) {
                    writeline("/home/ubuntu/results/coverage/Available/Available_5_10.coverage", "68ef66c9-d9fa-4e5c-8954-adb1482f2691");
                    if (type == null) {
                        writeline("/home/ubuntu/results/coverage/Available/Available_5_10.coverage", "994e4159-0c12-498d-8c44-7f6647a8b874");
                        log("Found: " + path, Project.MSG_VERBOSE);
                        writeline("/home/ubuntu/results/coverage/Available/Available_5_10.coverage", "18736db2-0453-4db2-8dfd-af848a19cc74");
                        return true;
                    } else if (type.isDir() && path.isDirectory()) {
                        writeline("/home/ubuntu/results/coverage/Available/Available_5_10.coverage", "c7a4b1e6-fd32-440c-bda0-c6ead068b312");
                        log("Found directory: " + path, Project.MSG_VERBOSE);
                        writeline("/home/ubuntu/results/coverage/Available/Available_5_10.coverage", "dc4a9a46-6bfe-4a80-a72b-04749dc3152e");
                        return true;
                    } else if (type.isFile() && path.isFile()) {
                        writeline("/home/ubuntu/results/coverage/Available/Available_5_10.coverage", "cc5a5a60-331b-4b47-b6bc-f0353be3c3e8");
                        log("Found file: " + path, Project.MSG_VERBOSE);
                        writeline("/home/ubuntu/results/coverage/Available/Available_5_10.coverage", "819e8dbb-ffad-4867-96c5-9b6f5d8ca0b5");
                        return true;
                    }
                    writeline("/home/ubuntu/results/coverage/Available/Available_5_10.coverage", "280fa50d-4326-4cf2-a8b6-28ec69a9fe41");
                    // not the requested type
                    return false;
                }
                writeline("/home/ubuntu/results/coverage/Available/Available_5_10.coverage", "79a7c487-fb07-48e5-a69d-80b2429ee079");
                File parent = path.getParentFile();
                writeline("/home/ubuntu/results/coverage/Available/Available_5_10.coverage", "1c2f4fe9-b774-49a8-8f77-bdf47c07a865");
                // **   full-pathname specified == parent dir of path in list
                if (parent != null && parent.exists() && filename.equals(parent.getAbsolutePath())) {
                    writeline("/home/ubuntu/results/coverage/Available/Available_5_10.coverage", "40962c72-c6c2-4b04-a11b-1e67eea0563d");
                    if (type == null) {
                        writeline("/home/ubuntu/results/coverage/Available/Available_5_10.coverage", "63a58886-4bb6-4e74-ae0d-130dacc82dd5");
                        log("Found: " + parent, Project.MSG_VERBOSE);
                        writeline("/home/ubuntu/results/coverage/Available/Available_5_10.coverage", "02c3eee9-2fee-4261-9e18-792fe223b60e");
                        return true;
                    } else if (type.isDir()) {
                        writeline("/home/ubuntu/results/coverage/Available/Available_5_10.coverage", "df0d3a6f-b7a1-4d28-9809-b018f281527c");
                        log("Found directory: " + parent, Project.MSG_VERBOSE);
                        writeline("/home/ubuntu/results/coverage/Available/Available_5_10.coverage", "09218a9a-6895-4bc7-900b-f742f17e2e87");
                        return true;
                    }
                    writeline("/home/ubuntu/results/coverage/Available/Available_5_10.coverage", "922cfd4c-c46c-4586-9c55-d3f96a7a157b");
                    // not the requested type
                    return false;
                }
                writeline("/home/ubuntu/results/coverage/Available/Available_5_10.coverage", "9daee2b9-05ea-4f80-bd20-af0af48e6436");
                // **   simple name specified   == path in list + name
                if (path.exists() && path.isDirectory()) {
                    writeline("/home/ubuntu/results/coverage/Available/Available_5_10.coverage", "1fb053bb-356d-4e2a-a0e7-f6ea8b7c90b9");
                    if (checkFile(new File(path, filename), filename + " in " + path)) {
                        writeline("/home/ubuntu/results/coverage/Available/Available_5_10.coverage", "ed3f3aa7-0d44-4712-ac85-19be1aeffe2c");
                        return true;
                    }
                }
                writeline("/home/ubuntu/results/coverage/Available/Available_5_10.coverage", "5e3f5f50-22df-4b92-a35c-2e009f563ea9");
                // **   simple name specified   == parent dir + name
                while (searchParents && parent != null && parent.exists()) {
                    writeline("/home/ubuntu/results/coverage/Available/Available_5_10.coverage", "7ea58d22-29d2-446e-a5cb-22422e738578");
                    if (checkFile(new File(parent, filename), filename + " in " + parent)) {
                        writeline("/home/ubuntu/results/coverage/Available/Available_5_10.coverage", "95d4fa55-54b8-4228-8d6a-abde371a36d5");
                        return true;
                    }
                    writeline("/home/ubuntu/results/coverage/Available/Available_5_10.coverage", "68d4d01d-a105-4887-9f7c-22ce4dc9dd58");
                    parent = parent.getParentFile();
                }
            }
        }
        writeline("/home/ubuntu/results/coverage/Available/Available_5_10.coverage", "547e19aa-175a-40e3-8800-fc5091b32221");
        return false;
    }

    /**
     * Check if a given file exists and matches the required type.
     */
    private boolean checkFile(File f, String text) {
        writeline("/home/ubuntu/results/coverage/Available/Available_5_10.coverage", "a2e885c1-c8d1-4002-a4c7-96e8144fed8a");
        if (type != null) {
            writeline("/home/ubuntu/results/coverage/Available/Available_5_10.coverage", "0f6f5280-1f2d-4f7b-89e8-ad43ec9f54e8");
            if (type.isDir()) {
                writeline("/home/ubuntu/results/coverage/Available/Available_5_10.coverage", "76a0d247-5b1e-4cd2-a477-12b88ca92246");
                if (f.isDirectory()) {
                    writeline("/home/ubuntu/results/coverage/Available/Available_5_10.coverage", "10e945d8-86bd-4e51-8ba8-4eebeacba528");
                    log("Found directory: " + text, Project.MSG_VERBOSE);
                }
                writeline("/home/ubuntu/results/coverage/Available/Available_5_10.coverage", "f225b912-9bcc-4077-81ae-9a4f2c2ec5a8");
                return f.isDirectory();
            } else if (type.isFile()) {
                writeline("/home/ubuntu/results/coverage/Available/Available_5_10.coverage", "96e251fe-b531-4c87-b701-7fd5d0ddb79b");
                if (f.isFile()) {
                    writeline("/home/ubuntu/results/coverage/Available/Available_5_10.coverage", "1f9bf780-7198-4944-ac66-cbedf750a203");
                    log("Found file: " + text, Project.MSG_VERBOSE);
                }
                writeline("/home/ubuntu/results/coverage/Available/Available_5_10.coverage", "e65fecdf-299f-4392-8ffd-08906e5db2df");
                return f.isFile();
            }
        }
        writeline("/home/ubuntu/results/coverage/Available/Available_5_10.coverage", "3f163305-7a6f-471f-a418-235adb067122");
        if (f.exists()) {
            writeline("/home/ubuntu/results/coverage/Available/Available_5_10.coverage", "064ff379-8e13-44e6-b712-5db24e129d76");
            log("Found: " + text, Project.MSG_VERBOSE);
        }
        writeline("/home/ubuntu/results/coverage/Available/Available_5_10.coverage", "09f47a92-1fa0-411c-b32f-c141e2a89f6f");
        return f.exists();
    }

    /**
     * Check if a given resource can be loaded.
     */
    private boolean checkResource(String resource) {
        writeline("/home/ubuntu/results/coverage/Available/Available_5_10.coverage", "269a1471-5edc-44ec-b980-66724728187a");
        InputStream is = null;
        writeline("/home/ubuntu/results/coverage/Available/Available_5_10.coverage", "750ea72e-a42c-4981-b7ae-4dc7510d363d");
        try {
            writeline("/home/ubuntu/results/coverage/Available/Available_5_10.coverage", "36c8ee06-f0d6-4175-816c-0719ec0a615a");
            if (loader != null) {
                writeline("/home/ubuntu/results/coverage/Available/Available_5_10.coverage", "37a866c9-8ffe-4c40-9ec8-94d38b162368");
                is = loader.getResourceAsStream(resource);
            } else {
                writeline("/home/ubuntu/results/coverage/Available/Available_5_10.coverage", "429aa5b9-23b2-4447-b1a8-d86359fb3690");
                ClassLoader cL = this.getClass().getClassLoader();
                writeline("/home/ubuntu/results/coverage/Available/Available_5_10.coverage", "dc5f2d2e-f52e-4848-84b1-19fc16b1a5e5");
                if (cL != null) {
                    writeline("/home/ubuntu/results/coverage/Available/Available_5_10.coverage", "212733b3-5441-43cf-9ca5-bddedeb45330");
                    is = cL.getResourceAsStream(resource);
                } else {
                    writeline("/home/ubuntu/results/coverage/Available/Available_5_10.coverage", "f33b9341-0971-461e-b7a3-bfdc2a4229d1");
                    is = ClassLoader.getSystemResourceAsStream(resource);
                }
            }
            writeline("/home/ubuntu/results/coverage/Available/Available_5_10.coverage", "9392d795-8447-480a-b943-64662a0f52d2");
            return is != null;
        } finally {
            writeline("/home/ubuntu/results/coverage/Available/Available_5_10.coverage", "1509b1e3-25dd-4ec4-9231-12a24b5fe774");
            FileUtils.close(is);
        }
    }

    /**
     * Check if a given class can be loaded.
     */
    private boolean checkClass(String classname) {
        writeline("/home/ubuntu/results/coverage/Available/Available_5_10.coverage", "876b5ec6-bf41-407f-a630-000461a73475");
        try {
            writeline("/home/ubuntu/results/coverage/Available/Available_5_10.coverage", "ec371e97-04da-4f3a-8ae4-5b801c744d30");
            if (ignoreSystemclasses) {
                writeline("/home/ubuntu/results/coverage/Available/Available_5_10.coverage", "bb9f9f51-fade-4820-8e37-93a4b99444c2");
                loader = getProject().createClassLoader(classpath);
                writeline("/home/ubuntu/results/coverage/Available/Available_5_10.coverage", "e5dc5b9b-66b1-479c-a840-052b66cdf54e");
                loader.setParentFirst(false);
                writeline("/home/ubuntu/results/coverage/Available/Available_5_10.coverage", "f7f82d34-2297-435a-86a2-49fcd1182efc");
                loader.addJavaLibraries();
                writeline("/home/ubuntu/results/coverage/Available/Available_5_10.coverage", "733c58e5-ae4a-415f-b1a4-6158f572ed88");
                try {
                    writeline("/home/ubuntu/results/coverage/Available/Available_5_10.coverage", "ac08b572-d8ca-490a-a671-e054529e1611");
                    loader.findClass(classname);
                } catch (SecurityException se) {
                    writeline("/home/ubuntu/results/coverage/Available/Available_5_10.coverage", "af755eb9-3599-4112-828e-b455ae670589");
                    // so catch the exception and return
                    return true;
                }
            } else if (loader != null) {
                writeline("/home/ubuntu/results/coverage/Available/Available_5_10.coverage", "dc5a19fd-c384-4572-8e61-e7433eee1fbc");
                loader.loadClass(classname);
            } else {
                writeline("/home/ubuntu/results/coverage/Available/Available_5_10.coverage", "2bfa02a2-d9a8-42b6-9cef-a54be0deedd8");
                ClassLoader l = this.getClass().getClassLoader();
                writeline("/home/ubuntu/results/coverage/Available/Available_5_10.coverage", "21b09df6-da7c-4872-b3bb-2fe118016933");
                // see API docs of Class.getClassLoader.
                if (l != null) {
                    writeline("/home/ubuntu/results/coverage/Available/Available_5_10.coverage", "4f5a82b0-93fe-4dd5-8e78-449c9c844558");
                    Class.forName(classname, true, l);
                } else {
                    writeline("/home/ubuntu/results/coverage/Available/Available_5_10.coverage", "c192512a-c327-48a5-b975-f3bb45217d45");
                    Class.forName(classname);
                }
            }
            writeline("/home/ubuntu/results/coverage/Available/Available_5_10.coverage", "076d40f2-2105-4a36-ae17-88e206e8047e");
            return true;
        } catch (ClassNotFoundException e) {
            writeline("/home/ubuntu/results/coverage/Available/Available_5_10.coverage", "754d4a9b-b35e-4e02-bad7-5cb75b598429");
            log("class \"" + classname + "\" was not found", Project.MSG_DEBUG);
            writeline("/home/ubuntu/results/coverage/Available/Available_5_10.coverage", "711a027d-cbbf-4ce0-b0f5-9939f97d2337");
            return false;
        } catch (NoClassDefFoundError e) {
            writeline("/home/ubuntu/results/coverage/Available/Available_5_10.coverage", "4204feba-fbc9-4d5e-acf8-5fd512df5b11");
            log("Could not load dependent class \"" + e.getMessage() + "\" for class \"" + classname + "\"", Project.MSG_DEBUG);
            writeline("/home/ubuntu/results/coverage/Available/Available_5_10.coverage", "1470befd-70f9-4704-a22b-085b1589fdb5");
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
