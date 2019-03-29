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
        writeline("/home/ubuntu/results/coverage/Available/Available_1_10.coverage", "26bf3316-b972-44f3-bb06-69feb59f607a");
        this.searchParents = searchParents;
    }

    /**
     * Set the classpath to be used when searching for classes and resources.
     *
     * @param classpath an Ant Path object containing the search path.
     */
    public void setClasspath(Path classpath) {
        writeline("/home/ubuntu/results/coverage/Available/Available_1_10.coverage", "015895e3-fe1b-430e-b759-2bbbedb57aae");
        createClasspath().append(classpath);
    }

    /**
     * Classpath to be used when searching for classes and resources.
     *
     * @return an empty Path instance to be configured by Ant.
     */
    public Path createClasspath() {
        writeline("/home/ubuntu/results/coverage/Available/Available_1_10.coverage", "2fa0d9b5-ea82-4458-bac6-9b4425b71e2c");
        if (this.classpath == null) {
            writeline("/home/ubuntu/results/coverage/Available/Available_1_10.coverage", "3a9aca6c-e37e-414c-8032-ab191840f5d5");
            this.classpath = new Path(getProject());
        }
        writeline("/home/ubuntu/results/coverage/Available/Available_1_10.coverage", "a0364bed-8d85-4037-b9bd-010747d667a1");
        return this.classpath.createPath();
    }

    /**
     * Set the classpath by reference.
     *
     * @param r a Reference to a Path instance to be used as the classpath
     * value.
     */
    public void setClasspathRef(Reference r) {
        writeline("/home/ubuntu/results/coverage/Available/Available_1_10.coverage", "45b2180e-778e-4c01-9d24-434990b27eeb");
        createClasspath().setRefid(r);
    }

    /**
     * Set the path to use when looking for a file.
     *
     * @param filepath a Path instance containing the search path for files.
     */
    public void setFilepath(Path filepath) {
        writeline("/home/ubuntu/results/coverage/Available/Available_1_10.coverage", "f4a98027-3ea5-4361-b0fa-cf75241b3a71");
        createFilepath().append(filepath);
    }

    /**
     * Path to search for file resources.
     *
     * @return a new Path instance which Ant will configure with a file search
     * path.
     */
    public Path createFilepath() {
        writeline("/home/ubuntu/results/coverage/Available/Available_1_10.coverage", "c084142a-17d2-439d-9b3e-2f7d35337004");
        if (this.filepath == null) {
            writeline("/home/ubuntu/results/coverage/Available/Available_1_10.coverage", "ada6d451-3fe6-43ca-9be5-2a41b73a0c79");
            this.filepath = new Path(getProject());
        }
        writeline("/home/ubuntu/results/coverage/Available/Available_1_10.coverage", "a82ed440-72f3-46a4-9774-c3773bffddde");
        return this.filepath.createPath();
    }

    /**
     * Set the name of the property which will be set if the particular resource
     * is available.
     *
     * @param property the name of the property to set.
     */
    public void setProperty(String property) {
        writeline("/home/ubuntu/results/coverage/Available/Available_1_10.coverage", "7e565eea-8a1d-4012-a958-3f8ecf012b2d");
        this.property = property;
    }

    /**
     * Set the value to be given to the property if the desired resource is
     * available.
     *
     * @param value the value to be given.
     */
    public void setValue(Object value) {
        writeline("/home/ubuntu/results/coverage/Available/Available_1_10.coverage", "60f9f8cb-f465-4986-844e-6cdf6652db01");
        this.value = value;
    }

    /**
     * Set the value to be given to the property if the desired resource is
     * available.
     *
     * @param value the value to be given.
     */
    public void setValue(String value) {
        writeline("/home/ubuntu/results/coverage/Available/Available_1_10.coverage", "f620dd0f-ffc7-4cc6-b2c2-388bcc4f993a");
        setValue((Object) value);
    }

    /**
     * Set a classname of a class which must be available to set the given
     * property.
     *
     * @param classname the name of the class required.
     */
    public void setClassname(String classname) {
        writeline("/home/ubuntu/results/coverage/Available/Available_1_10.coverage", "00fb4171-640b-4d1e-b505-3277c8939fa2");
        if (!"".equals(classname)) {
            writeline("/home/ubuntu/results/coverage/Available/Available_1_10.coverage", "63503839-90fc-43fc-acd2-89a286ff415d");
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
        writeline("/home/ubuntu/results/coverage/Available/Available_1_10.coverage", "3084693e-a430-4d34-a93d-c5be3306874b");
        this.file = file;
        writeline("/home/ubuntu/results/coverage/Available/Available_1_10.coverage", "c9bb0d02-55c3-4e9e-acd7-c3a1a985ba2c");
        this.filename = FILE_UTILS.removeLeadingPath(getProject().getBaseDir(), file);
    }

    /**
     * Set the name of a Java resource which is required to set the property.
     *
     * @param resource the name of a resource which is required to be available.
     */
    public void setResource(String resource) {
        writeline("/home/ubuntu/results/coverage/Available/Available_1_10.coverage", "230a5991-e73b-40ca-b5e2-32a366899a91");
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
        writeline("/home/ubuntu/results/coverage/Available/Available_1_10.coverage", "a065fe83-c31d-4845-a58e-d0e1f126171d");
        log("DEPRECATED - The setType(String) method has been deprecated." + " Use setType(Available.FileDir) instead.", Project.MSG_WARN);
        writeline("/home/ubuntu/results/coverage/Available/Available_1_10.coverage", "dce81d45-2106-422b-bf90-b2a8e9e539c4");
        this.type = new FileDir();
        writeline("/home/ubuntu/results/coverage/Available/Available_1_10.coverage", "5c32ebb5-e8ba-4751-9ed7-b51dec0689cf");
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
        writeline("/home/ubuntu/results/coverage/Available/Available_1_10.coverage", "8467204b-855b-4de5-8e29-b97b1d4175f5");
        this.type = type;
    }

    /**
     * Set whether the search for classes should ignore the runtime classes and
     * just use the given classpath.
     *
     * @param ignore true if system classes are to be ignored.
     */
    public void setIgnoresystemclasses(boolean ignore) {
        writeline("/home/ubuntu/results/coverage/Available/Available_1_10.coverage", "9b4b01a8-6bb7-4dbe-abe7-1a5f841e0b84");
        this.ignoreSystemclasses = ignore;
    }

    /**
     * Entry point when operating as a task.
     *
     * @exception BuildException if the task is not configured correctly.
     */
    public void execute() throws BuildException {
        writeline("/home/ubuntu/results/coverage/Available/Available_1_10.coverage", "638e4f5b-4a60-4c54-bef3-c27c11e7ed6c");
        if (property == null) {
            writeline("/home/ubuntu/results/coverage/Available/Available_1_10.coverage", "7a37614c-5b2e-4a93-be81-bcb6b89b4e33");
            throw new BuildException("property attribute is required", getLocation());
        }
        writeline("/home/ubuntu/results/coverage/Available/Available_1_10.coverage", "2f9ff0dd-11a0-4aae-9bd3-b96974e2c31d");
        isTask = true;
        writeline("/home/ubuntu/results/coverage/Available/Available_1_10.coverage", "63cf63e5-c9dc-4b06-bf23-6ac2d5a5beeb");
        try {
            writeline("/home/ubuntu/results/coverage/Available/Available_1_10.coverage", "4f14b111-aa2c-4d19-912d-0adce53f241f");
            if (eval()) {
                writeline("/home/ubuntu/results/coverage/Available/Available_1_10.coverage", "0057987e-26c4-44fa-9911-ccb9a6eefec9");
                PropertyHelper ph = PropertyHelper.getPropertyHelper(getProject());
                writeline("/home/ubuntu/results/coverage/Available/Available_1_10.coverage", "fa709434-48f9-4902-a963-7c84df983b95");
                Object oldvalue = ph.getProperty(property);
                writeline("/home/ubuntu/results/coverage/Available/Available_1_10.coverage", "eb46a6fa-b165-4e58-ae4a-997987ee3a08");
                if (null != oldvalue && !oldvalue.equals(value)) {
                    writeline("/home/ubuntu/results/coverage/Available/Available_1_10.coverage", "0bfc9ff7-26ed-4d23-955d-602a0406d2ab");
                    log("DEPRECATED - <available> used to override an existing" + " property." + StringUtils.LINE_SEP + "  Build file should not reuse the same property" + " name for different values.", Project.MSG_WARN);
                }
                writeline("/home/ubuntu/results/coverage/Available/Available_1_10.coverage", "11f2574c-8f2d-4e24-a8d0-6bfa79b915ae");
                // NB: this makes use of Project#setProperty rather than Project#setNewProperty
                // due to backwards compatibility reasons
                ph.setProperty(property, value, true);
            }
        } finally {
            writeline("/home/ubuntu/results/coverage/Available/Available_1_10.coverage", "6f96e7fb-2902-4628-9a58-8781e0f43af7");
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
        writeline("/home/ubuntu/results/coverage/Available/Available_1_10.coverage", "c798d8f9-501c-4814-808c-61022db9e385");
        try {
            writeline("/home/ubuntu/results/coverage/Available/Available_1_10.coverage", "2374737e-f600-4e6d-87f8-7a16f32d5f66");
            if (classname == null && file == null && resource == null) {
                writeline("/home/ubuntu/results/coverage/Available/Available_1_10.coverage", "89c77a0e-fe50-407d-ae8d-a385d3ffbd33");
                throw new BuildException("At least one of (classname|file|" + "resource) is required", getLocation());
            }
            writeline("/home/ubuntu/results/coverage/Available/Available_1_10.coverage", "77339c68-7d18-471c-a27d-4e13bcedaa1f");
            if (type != null) {
                writeline("/home/ubuntu/results/coverage/Available/Available_1_10.coverage", "f7123adf-1e34-4f92-8418-00124f51112a");
                if (file == null) {
                    writeline("/home/ubuntu/results/coverage/Available/Available_1_10.coverage", "9bb07fed-e415-4c24-855e-63682e23b754");
                    throw new BuildException("The type attribute is only valid " + "when specifying the file " + "attribute.", getLocation());
                }
            }
            writeline("/home/ubuntu/results/coverage/Available/Available_1_10.coverage", "5f1507cd-fe4a-4258-b287-1dff3545094a");
            if (classpath != null) {
                writeline("/home/ubuntu/results/coverage/Available/Available_1_10.coverage", "472794c0-d18c-4c45-a512-dfa5f2bc54ae");
                classpath.setProject(getProject());
                writeline("/home/ubuntu/results/coverage/Available/Available_1_10.coverage", "2effd9b3-7dfa-4e32-ac7c-691ab64d67d5");
                this.loader = getProject().createClassLoader(classpath);
            }
            writeline("/home/ubuntu/results/coverage/Available/Available_1_10.coverage", "b3c7d4f3-6536-46bf-9fba-9f2b7cbf21fe");
            String appendix = "";
            writeline("/home/ubuntu/results/coverage/Available/Available_1_10.coverage", "18b8e6c3-841e-49b6-8798-10d66a005bb4");
            if (isTask) {
                writeline("/home/ubuntu/results/coverage/Available/Available_1_10.coverage", "f474e88c-74f7-40da-96e4-ad0076c33c00");
                appendix = " to set property " + property;
            } else {
                writeline("/home/ubuntu/results/coverage/Available/Available_1_10.coverage", "a46b0547-4921-408a-86da-be577ae59cd9");
                setTaskName("available");
            }
            writeline("/home/ubuntu/results/coverage/Available/Available_1_10.coverage", "66f181d5-1c26-434d-ac5d-a07cb868de50");
            if ((classname != null) && !checkClass(classname)) {
                writeline("/home/ubuntu/results/coverage/Available/Available_1_10.coverage", "aeb95529-9081-46f6-93e3-efe00001ffc1");
                log("Unable to load class " + classname + appendix, Project.MSG_VERBOSE);
                writeline("/home/ubuntu/results/coverage/Available/Available_1_10.coverage", "62d014ec-e68d-4974-b9a8-2b56e7a49291");
                return false;
            }
            writeline("/home/ubuntu/results/coverage/Available/Available_1_10.coverage", "2df76630-d84b-4909-89ef-cd75710a71b9");
            if ((file != null) && !checkFile()) {
                writeline("/home/ubuntu/results/coverage/Available/Available_1_10.coverage", "4247e754-ac79-433a-b013-61eeb0e8cac7");
                StringBuffer buf = new StringBuffer("Unable to find ");
                writeline("/home/ubuntu/results/coverage/Available/Available_1_10.coverage", "68b00ad7-765c-4ecc-a684-6f83cdd75e54");
                if (type != null) {
                    writeline("/home/ubuntu/results/coverage/Available/Available_1_10.coverage", "75420993-bc74-4c66-a0c4-98d7d5903be3");
                    buf.append(type).append(' ');
                }
                writeline("/home/ubuntu/results/coverage/Available/Available_1_10.coverage", "6340133f-f966-413c-9e7f-139cee97b320");
                buf.append(filename).append(appendix);
                writeline("/home/ubuntu/results/coverage/Available/Available_1_10.coverage", "fc3d2117-c205-40fd-930d-f1cf854997d1");
                log(buf.toString(), Project.MSG_VERBOSE);
                writeline("/home/ubuntu/results/coverage/Available/Available_1_10.coverage", "674b673c-154d-4f3a-9ed6-b15d4aee596c");
                return false;
            }
            writeline("/home/ubuntu/results/coverage/Available/Available_1_10.coverage", "3a93b182-2d5a-49bf-9016-d18b60db7b7f");
            if ((resource != null) && !checkResource(resource)) {
                writeline("/home/ubuntu/results/coverage/Available/Available_1_10.coverage", "3e41e813-e126-49bd-a771-d2ae7559db9f");
                log("Unable to load resource " + resource + appendix, Project.MSG_VERBOSE);
                writeline("/home/ubuntu/results/coverage/Available/Available_1_10.coverage", "53917f5a-bd0e-4f57-a50d-310563c8900b");
                return false;
            }
        } finally {
            writeline("/home/ubuntu/results/coverage/Available/Available_1_10.coverage", "4dbbe2ec-ddfb-4e05-b7fb-aa83be43c5af");
            if (loader != null) {
                writeline("/home/ubuntu/results/coverage/Available/Available_1_10.coverage", "2bc67973-551d-441d-a683-1a246a2a3b7e");
                loader.cleanup();
                writeline("/home/ubuntu/results/coverage/Available/Available_1_10.coverage", "d53716b6-abe5-48af-9714-7f0193ce8901");
                loader = null;
            }
            writeline("/home/ubuntu/results/coverage/Available/Available_1_10.coverage", "d705e3d7-006d-4c3b-a713-bed8b498f171");
            if (!isTask) {
                writeline("/home/ubuntu/results/coverage/Available/Available_1_10.coverage", "786888e2-5a8d-4786-93c8-44c546c26420");
                setTaskName(null);
            }
        }
        writeline("/home/ubuntu/results/coverage/Available/Available_1_10.coverage", "f1d714ec-6ebc-4e0c-8f6a-257278c401dc");
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
        writeline("/home/ubuntu/results/coverage/Available/Available_1_10.coverage", "e29576eb-5f0a-4d08-9c9b-177af6cdd2ad");
        if (filepath == null) {
            writeline("/home/ubuntu/results/coverage/Available/Available_1_10.coverage", "3d5a5144-7f26-411c-856f-b9ca75dc2c3e");
            return checkFile(file, filename);
        } else {
            writeline("/home/ubuntu/results/coverage/Available/Available_1_10.coverage", "b3c40c0d-eae4-499e-b687-0ed4bb15dfea");
            String[] paths = filepath.list();
            writeline("/home/ubuntu/results/coverage/Available/Available_1_10.coverage", "2bd50c9b-4a66-4b16-9dbd-016cab70b1f3");
            for (int i = 0; i < paths.length; ++i) {
                writeline("/home/ubuntu/results/coverage/Available/Available_1_10.coverage", "e0e363ac-8993-4731-a00f-52b8659a21ef");
                log("Searching " + paths[i], Project.MSG_VERBOSE);
                writeline("/home/ubuntu/results/coverage/Available/Available_1_10.coverage", "a34a3c06-98a8-440c-ae6b-52e16eedd42f");
                File path = new File(paths[i]);
                writeline("/home/ubuntu/results/coverage/Available/Available_1_10.coverage", "b81bd41d-a146-4086-9186-81b2f3e2c06b");
                // **   simple name specified   == path in list
                if (path.exists() && (filename.equals(paths[i]) || filename.equals(path.getName()))) {
                    writeline("/home/ubuntu/results/coverage/Available/Available_1_10.coverage", "2175974d-d298-4ee4-ad89-170c86f88bb1");
                    if (type == null) {
                        writeline("/home/ubuntu/results/coverage/Available/Available_1_10.coverage", "f4dd607f-b6d6-4676-8c86-0b6e5b876cc4");
                        log("Found: " + path, Project.MSG_VERBOSE);
                        writeline("/home/ubuntu/results/coverage/Available/Available_1_10.coverage", "1257ae53-5e6b-4a74-bf59-63ecf6197c43");
                        return true;
                    } else if (type.isDir() && path.isDirectory()) {
                        writeline("/home/ubuntu/results/coverage/Available/Available_1_10.coverage", "7801b9a0-e9d4-47b1-b1a6-704814f7df94");
                        log("Found directory: " + path, Project.MSG_VERBOSE);
                        writeline("/home/ubuntu/results/coverage/Available/Available_1_10.coverage", "42bba75d-f9d2-4771-b0af-cf78bf4aae57");
                        return true;
                    } else if (type.isFile() && path.isFile()) {
                        writeline("/home/ubuntu/results/coverage/Available/Available_1_10.coverage", "f7ede1c6-2773-437f-92a6-903165aa11ba");
                        log("Found file: " + path, Project.MSG_VERBOSE);
                        writeline("/home/ubuntu/results/coverage/Available/Available_1_10.coverage", "ccc866fd-9991-44cf-a5e8-3032e3cf99c0");
                        return true;
                    }
                    writeline("/home/ubuntu/results/coverage/Available/Available_1_10.coverage", "fe8e6b3a-2ba5-4bbf-885a-d38d05de5477");
                    // not the requested type
                    return false;
                }
                writeline("/home/ubuntu/results/coverage/Available/Available_1_10.coverage", "53489586-7403-400f-b95a-dc54f26265f9");
                File parent = path.getParentFile();
                writeline("/home/ubuntu/results/coverage/Available/Available_1_10.coverage", "9b1edd22-9d1a-48e5-8e87-8dbd68474825");
                // **   full-pathname specified == parent dir of path in list
                if (parent != null && parent.exists() && filename.equals(parent.getAbsolutePath())) {
                    writeline("/home/ubuntu/results/coverage/Available/Available_1_10.coverage", "2d652786-56db-4442-b04d-a38b6e8720ae");
                    if (type == null) {
                        writeline("/home/ubuntu/results/coverage/Available/Available_1_10.coverage", "17a809db-3399-4ef7-9ef7-22cd45cb73ea");
                        log("Found: " + parent, Project.MSG_VERBOSE);
                        writeline("/home/ubuntu/results/coverage/Available/Available_1_10.coverage", "16d881aa-7206-41e8-a975-0bc02f6ae926");
                        return true;
                    } else if (type.isDir()) {
                        writeline("/home/ubuntu/results/coverage/Available/Available_1_10.coverage", "67e9e907-602c-4dad-a1c0-eda30ecf4731");
                        log("Found directory: " + parent, Project.MSG_VERBOSE);
                        writeline("/home/ubuntu/results/coverage/Available/Available_1_10.coverage", "f34004ba-be39-440e-867a-171e9909480e");
                        return true;
                    }
                    writeline("/home/ubuntu/results/coverage/Available/Available_1_10.coverage", "2f1a9fbd-d441-4b2a-becb-a4bfde472b9c");
                    // not the requested type
                    return false;
                }
                writeline("/home/ubuntu/results/coverage/Available/Available_1_10.coverage", "a6175fb6-4dc7-4bb4-bf68-c70b7d1fd3a5");
                // **   simple name specified   == path in list + name
                if (path.exists() && path.isDirectory()) {
                    writeline("/home/ubuntu/results/coverage/Available/Available_1_10.coverage", "a903bdcd-5861-45da-9038-c6e116a4505d");
                    if (checkFile(new File(path, filename), filename + " in " + path)) {
                        writeline("/home/ubuntu/results/coverage/Available/Available_1_10.coverage", "96eb09af-36e4-4876-be44-785f7c564256");
                        return true;
                    }
                }
                writeline("/home/ubuntu/results/coverage/Available/Available_1_10.coverage", "d320723b-d17c-4e6f-ac7b-92c77c3d1651");
                // **   simple name specified   == parent dir + name
                while (searchParents && parent != null && parent.exists()) {
                    writeline("/home/ubuntu/results/coverage/Available/Available_1_10.coverage", "9dc926ac-3590-4190-90a4-b783016bb919");
                    if (checkFile(new File(parent, filename), filename + " in " + parent)) {
                        writeline("/home/ubuntu/results/coverage/Available/Available_1_10.coverage", "7160eb2f-5591-4f6c-aee7-730baad263f4");
                        return true;
                    }
                    writeline("/home/ubuntu/results/coverage/Available/Available_1_10.coverage", "8c30d68e-1cdf-4a97-83fe-79b035ff3741");
                    parent = parent.getParentFile();
                }
            }
        }
        writeline("/home/ubuntu/results/coverage/Available/Available_1_10.coverage", "652acdff-9393-4b3c-a892-36e605d58845");
        return false;
    }

    /**
     * Check if a given file exists and matches the required type.
     */
    private boolean checkFile(File f, String text) {
        writeline("/home/ubuntu/results/coverage/Available/Available_1_10.coverage", "eb1de53c-1a00-4c51-81e9-08c279a1fed1");
        if (type != null) {
            writeline("/home/ubuntu/results/coverage/Available/Available_1_10.coverage", "19c20141-cdba-43fa-93e7-35acb220c8bb");
            if (type.isDir()) {
                writeline("/home/ubuntu/results/coverage/Available/Available_1_10.coverage", "81eccfa6-b80d-4e3e-823c-1c48ff3ba722");
                if (f.isDirectory()) {
                    writeline("/home/ubuntu/results/coverage/Available/Available_1_10.coverage", "5bbf6bea-f5b6-4620-9dab-642e32f6360b");
                    log("Found directory: " + text, Project.MSG_VERBOSE);
                }
                writeline("/home/ubuntu/results/coverage/Available/Available_1_10.coverage", "e83a80b7-025e-40ac-9b4c-b2b1d4793a8f");
                return f.isDirectory();
            } else if (type.isFile()) {
                writeline("/home/ubuntu/results/coverage/Available/Available_1_10.coverage", "05c56740-1e9c-4e9e-9682-1ce5a824f0a5");
                if (f.isFile()) {
                    writeline("/home/ubuntu/results/coverage/Available/Available_1_10.coverage", "72c4659c-1cca-457c-88fb-7f6cc31b6428");
                    log("Found file: " + text, Project.MSG_VERBOSE);
                }
                writeline("/home/ubuntu/results/coverage/Available/Available_1_10.coverage", "58337bcd-6bf2-4126-900c-846f198e63e3");
                return f.isFile();
            }
        }
        writeline("/home/ubuntu/results/coverage/Available/Available_1_10.coverage", "4727064d-bb80-4d96-9ab5-8bffcb2dc32a");
        if (f.exists()) {
            writeline("/home/ubuntu/results/coverage/Available/Available_1_10.coverage", "e24e68bb-d299-448f-a747-fadfe334f8ac");
            log("Found: " + text, Project.MSG_VERBOSE);
        }
        writeline("/home/ubuntu/results/coverage/Available/Available_1_10.coverage", "1b36284f-c3c9-47ec-8dd1-6cc0b3a306bf");
        return f.exists();
    }

    /**
     * Check if a given resource can be loaded.
     */
    private boolean checkResource(String resource) {
        writeline("/home/ubuntu/results/coverage/Available/Available_1_10.coverage", "f10cb9b0-e4ac-4607-a12f-5402edeeedc7");
        InputStream is = null;
        writeline("/home/ubuntu/results/coverage/Available/Available_1_10.coverage", "77458632-4dbb-4199-b309-21458909e9bf");
        try {
            writeline("/home/ubuntu/results/coverage/Available/Available_1_10.coverage", "3d60a80e-be0f-4742-879e-ade56120b14b");
            if (loader != null) {
                writeline("/home/ubuntu/results/coverage/Available/Available_1_10.coverage", "43b8eebd-0f60-428d-9d24-d5fe646ba24d");
                is = loader.getResourceAsStream(resource);
            } else {
                writeline("/home/ubuntu/results/coverage/Available/Available_1_10.coverage", "cb059a90-2cf6-4719-bc52-ce0b76b1f25f");
                ClassLoader cL = this.getClass().getClassLoader();
                writeline("/home/ubuntu/results/coverage/Available/Available_1_10.coverage", "7ee041e3-5199-47fb-a3ea-84d8ed2c1d35");
                if (cL != null) {
                    writeline("/home/ubuntu/results/coverage/Available/Available_1_10.coverage", "fda9e9cc-1939-416c-a1ea-7ae001038379");
                    is = cL.getResourceAsStream(resource);
                } else {
                    writeline("/home/ubuntu/results/coverage/Available/Available_1_10.coverage", "6bb6dbac-ac80-462b-9c9f-2456d9e89744");
                    is = ClassLoader.getSystemResourceAsStream(resource);
                }
            }
            writeline("/home/ubuntu/results/coverage/Available/Available_1_10.coverage", "f720e2cc-e163-4ab0-a878-92210e01ee60");
            return is != null;
        } finally {
            writeline("/home/ubuntu/results/coverage/Available/Available_1_10.coverage", "0da315c3-e574-4221-891f-a686580720c8");
            FileUtils.close(is);
        }
    }

    /**
     * Check if a given class can be loaded.
     */
    private boolean checkClass(String classname) {
        writeline("/home/ubuntu/results/coverage/Available/Available_1_10.coverage", "646de794-8175-4995-b117-6b618194ed81");
        try {
            writeline("/home/ubuntu/results/coverage/Available/Available_1_10.coverage", "1450ebf0-2409-4293-94c6-065c4764e53b");
            if (ignoreSystemclasses) {
                writeline("/home/ubuntu/results/coverage/Available/Available_1_10.coverage", "b63b8694-302a-4c5b-bb07-527ceeb209fc");
                loader = getProject().createClassLoader(classpath);
                writeline("/home/ubuntu/results/coverage/Available/Available_1_10.coverage", "150a9e6b-edd2-43e1-9265-2012cb22b947");
                loader.setParentFirst(false);
                writeline("/home/ubuntu/results/coverage/Available/Available_1_10.coverage", "42e926aa-623b-4637-990b-d440bb58b624");
                loader.addJavaLibraries();
                writeline("/home/ubuntu/results/coverage/Available/Available_1_10.coverage", "45690672-e1fe-4e04-9665-6e26cb508c92");
                try {
                    writeline("/home/ubuntu/results/coverage/Available/Available_1_10.coverage", "dd1adf16-c71c-4963-b3ae-11d5fc71b0ac");
                    loader.findClass(classname);
                } catch (SecurityException se) {
                    writeline("/home/ubuntu/results/coverage/Available/Available_1_10.coverage", "ecedcc31-1425-471f-ad82-67198d7276f6");
                    // so catch the exception and return
                    return true;
                }
            } else if (loader != null) {
                writeline("/home/ubuntu/results/coverage/Available/Available_1_10.coverage", "c3fa4785-7487-45a7-8d0e-4eda16c16e45");
                loader.loadClass(classname);
            } else {
                writeline("/home/ubuntu/results/coverage/Available/Available_1_10.coverage", "5b7f5f87-1b60-4c9c-a82d-2a79faee210a");
                ClassLoader l = this.getClass().getClassLoader();
                writeline("/home/ubuntu/results/coverage/Available/Available_1_10.coverage", "da460dc0-f7df-4590-8c43-9639e7e01c22");
                // see API docs of Class.getClassLoader.
                if (l != null) {
                    writeline("/home/ubuntu/results/coverage/Available/Available_1_10.coverage", "fd6d7ca8-f2bf-4af1-94a2-db17d54fbadd");
                    Class.forName(classname, true, l);
                } else {
                    writeline("/home/ubuntu/results/coverage/Available/Available_1_10.coverage", "1adabd0f-51c6-4886-9780-2af627358e8a");
                    Class.forName(classname);
                }
            }
            writeline("/home/ubuntu/results/coverage/Available/Available_1_10.coverage", "c32a6492-d76e-43f9-9b51-06ccbea41f59");
            return true;
        } catch (ClassNotFoundException e) {
            writeline("/home/ubuntu/results/coverage/Available/Available_1_10.coverage", "c1e9faaa-a68f-435f-bae2-2e5837e940df");
            log("class \"" + classname + "\" was not found", Project.MSG_DEBUG);
            writeline("/home/ubuntu/results/coverage/Available/Available_1_10.coverage", "9eb4c780-e41b-41d9-a36e-5cd876ed08bf");
            return false;
        } catch (NoClassDefFoundError e) {
            writeline("/home/ubuntu/results/coverage/Available/Available_1_10.coverage", "86c629fe-baa2-4958-914a-462f6ecbea41");
            log("Could not load dependent class \"" + e.getMessage() + "\" for class \"" + classname + "\"", Project.MSG_DEBUG);
            writeline("/home/ubuntu/results/coverage/Available/Available_1_10.coverage", "f8876f6c-2c45-4188-928d-f92c17ea5e12");
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
