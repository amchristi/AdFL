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
        writeline("/home/ubuntu/results/coverage/Available/Available_7_10.coverage", "2d2820ba-076c-4806-9d9b-83e6b189a9b4");
        this.searchParents = searchParents;
    }

    /**
     * Set the classpath to be used when searching for classes and resources.
     *
     * @param classpath an Ant Path object containing the search path.
     */
    public void setClasspath(Path classpath) {
        writeline("/home/ubuntu/results/coverage/Available/Available_7_10.coverage", "94055d22-5458-4184-ad49-1d1273fcdd18");
        createClasspath().append(classpath);
    }

    /**
     * Classpath to be used when searching for classes and resources.
     *
     * @return an empty Path instance to be configured by Ant.
     */
    public Path createClasspath() {
        writeline("/home/ubuntu/results/coverage/Available/Available_7_10.coverage", "261f5e2a-7f80-47df-8250-cc9bbb996e27");
        if (this.classpath == null) {
            writeline("/home/ubuntu/results/coverage/Available/Available_7_10.coverage", "1c358499-e04e-44d8-b863-986f89da8fd8");
            this.classpath = new Path(getProject());
        }
        writeline("/home/ubuntu/results/coverage/Available/Available_7_10.coverage", "84dbe6e1-a6d3-407b-a090-1e8dedf8ef07");
        return this.classpath.createPath();
    }

    /**
     * Set the classpath by reference.
     *
     * @param r a Reference to a Path instance to be used as the classpath
     * value.
     */
    public void setClasspathRef(Reference r) {
        writeline("/home/ubuntu/results/coverage/Available/Available_7_10.coverage", "2293918f-2348-43aa-836d-652fef092194");
        createClasspath().setRefid(r);
    }

    /**
     * Set the path to use when looking for a file.
     *
     * @param filepath a Path instance containing the search path for files.
     */
    public void setFilepath(Path filepath) {
        writeline("/home/ubuntu/results/coverage/Available/Available_7_10.coverage", "f6ae1441-fef6-41c3-b503-f422a661641f");
        createFilepath().append(filepath);
    }

    /**
     * Path to search for file resources.
     *
     * @return a new Path instance which Ant will configure with a file search
     * path.
     */
    public Path createFilepath() {
        writeline("/home/ubuntu/results/coverage/Available/Available_7_10.coverage", "af6a6238-4b15-4c08-af07-d903b1c1e0db");
        if (this.filepath == null) {
            writeline("/home/ubuntu/results/coverage/Available/Available_7_10.coverage", "5317240c-3352-41a5-ab84-06fd20e57fb1");
            this.filepath = new Path(getProject());
        }
        writeline("/home/ubuntu/results/coverage/Available/Available_7_10.coverage", "6f2ef52b-dcfa-4a3e-8cce-32fcde02b882");
        return this.filepath.createPath();
    }

    /**
     * Set the name of the property which will be set if the particular resource
     * is available.
     *
     * @param property the name of the property to set.
     */
    public void setProperty(String property) {
        writeline("/home/ubuntu/results/coverage/Available/Available_7_10.coverage", "02a4a912-e5ab-47de-b128-99759701d7d2");
        this.property = property;
    }

    /**
     * Set the value to be given to the property if the desired resource is
     * available.
     *
     * @param value the value to be given.
     */
    public void setValue(Object value) {
        writeline("/home/ubuntu/results/coverage/Available/Available_7_10.coverage", "b18ca55f-ee63-40d2-9445-ca0047ca12c0");
        this.value = value;
    }

    /**
     * Set the value to be given to the property if the desired resource is
     * available.
     *
     * @param value the value to be given.
     */
    public void setValue(String value) {
        writeline("/home/ubuntu/results/coverage/Available/Available_7_10.coverage", "debdcb64-a7d9-4500-b380-c1d05b032105");
        setValue((Object) value);
    }

    /**
     * Set a classname of a class which must be available to set the given
     * property.
     *
     * @param classname the name of the class required.
     */
    public void setClassname(String classname) {
        writeline("/home/ubuntu/results/coverage/Available/Available_7_10.coverage", "0afef7e8-7898-4993-900e-29f83fe863ae");
        if (!"".equals(classname)) {
            writeline("/home/ubuntu/results/coverage/Available/Available_7_10.coverage", "d28abd93-a204-4fc0-86db-8e71c96f3b05");
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
        writeline("/home/ubuntu/results/coverage/Available/Available_7_10.coverage", "64edcf6b-e5fa-425c-9b38-fe7332a81104");
        this.file = file;
        writeline("/home/ubuntu/results/coverage/Available/Available_7_10.coverage", "1835797c-88b6-4058-ba79-5d8dcfa5a3e8");
        this.filename = FILE_UTILS.removeLeadingPath(getProject().getBaseDir(), file);
    }

    /**
     * Set the name of a Java resource which is required to set the property.
     *
     * @param resource the name of a resource which is required to be available.
     */
    public void setResource(String resource) {
        writeline("/home/ubuntu/results/coverage/Available/Available_7_10.coverage", "bbe9cba2-2c9f-4e21-8144-4dea630c5421");
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
        writeline("/home/ubuntu/results/coverage/Available/Available_7_10.coverage", "e12e8ad4-41f7-4f4f-b07f-d6158f890949");
        log("DEPRECATED - The setType(String) method has been deprecated." + " Use setType(Available.FileDir) instead.", Project.MSG_WARN);
        writeline("/home/ubuntu/results/coverage/Available/Available_7_10.coverage", "7436817c-10fd-4b79-a07c-6ec07f18ef0b");
        this.type = new FileDir();
        writeline("/home/ubuntu/results/coverage/Available/Available_7_10.coverage", "4c099122-d556-443e-8b58-db7e491da0f2");
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
        writeline("/home/ubuntu/results/coverage/Available/Available_7_10.coverage", "ca0ffa2c-f5c8-43af-820a-4cd1a906847c");
        this.type = type;
    }

    /**
     * Set whether the search for classes should ignore the runtime classes and
     * just use the given classpath.
     *
     * @param ignore true if system classes are to be ignored.
     */
    public void setIgnoresystemclasses(boolean ignore) {
        writeline("/home/ubuntu/results/coverage/Available/Available_7_10.coverage", "3b17f839-6012-4410-b000-f18923176293");
        this.ignoreSystemclasses = ignore;
    }

    /**
     * Entry point when operating as a task.
     *
     * @exception BuildException if the task is not configured correctly.
     */
    public void execute() throws BuildException {
        writeline("/home/ubuntu/results/coverage/Available/Available_7_10.coverage", "11c58f19-e103-4041-b167-61ae655e4ff9");
        if (property == null) {
            writeline("/home/ubuntu/results/coverage/Available/Available_7_10.coverage", "db19b3df-4e8c-4b2c-8443-ea0de80d9e85");
            throw new BuildException("property attribute is required", getLocation());
        }
        writeline("/home/ubuntu/results/coverage/Available/Available_7_10.coverage", "3911ae49-5c00-4160-80d1-07e9fba0ca5b");
        isTask = true;
        writeline("/home/ubuntu/results/coverage/Available/Available_7_10.coverage", "98852623-f659-4aa5-bdfe-d705cd9ff8e7");
        try {
            writeline("/home/ubuntu/results/coverage/Available/Available_7_10.coverage", "a4bc89c3-2d44-48ba-95ef-edf335a59b56");
            if (eval()) {
                writeline("/home/ubuntu/results/coverage/Available/Available_7_10.coverage", "1d97a527-4c8b-4373-816f-b53bfabfc958");
                PropertyHelper ph = PropertyHelper.getPropertyHelper(getProject());
                writeline("/home/ubuntu/results/coverage/Available/Available_7_10.coverage", "055fdeeb-ad7e-43df-bf0b-75da93d39d94");
                Object oldvalue = ph.getProperty(property);
                writeline("/home/ubuntu/results/coverage/Available/Available_7_10.coverage", "19c2310b-af32-4f62-b818-6bad4667719a");
                if (null != oldvalue && !oldvalue.equals(value)) {
                    writeline("/home/ubuntu/results/coverage/Available/Available_7_10.coverage", "b5f1de30-1b4e-41da-b26e-46b2c4c0bac4");
                    log("DEPRECATED - <available> used to override an existing" + " property." + StringUtils.LINE_SEP + "  Build file should not reuse the same property" + " name for different values.", Project.MSG_WARN);
                }
                writeline("/home/ubuntu/results/coverage/Available/Available_7_10.coverage", "2cbcca9a-4fbe-46e6-be85-c00a1be74029");
                // NB: this makes use of Project#setProperty rather than Project#setNewProperty
                // due to backwards compatibility reasons
                ph.setProperty(property, value, true);
            }
        } finally {
            writeline("/home/ubuntu/results/coverage/Available/Available_7_10.coverage", "74bb0915-5f8a-4ec8-b27a-3d1e47d7add6");
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
        writeline("/home/ubuntu/results/coverage/Available/Available_7_10.coverage", "01d1f2e8-59e8-43f4-80f6-ffb3b20557a4");
        try {
            writeline("/home/ubuntu/results/coverage/Available/Available_7_10.coverage", "27e23d6d-58f7-419e-8060-20d12786a7f4");
            if (classname == null && file == null && resource == null) {
                writeline("/home/ubuntu/results/coverage/Available/Available_7_10.coverage", "bc485277-05c2-43fc-800f-f60092bb9459");
                throw new BuildException("At least one of (classname|file|" + "resource) is required", getLocation());
            }
            writeline("/home/ubuntu/results/coverage/Available/Available_7_10.coverage", "f52aaae0-942f-40cd-b4be-40f04a021a72");
            if (type != null) {
                writeline("/home/ubuntu/results/coverage/Available/Available_7_10.coverage", "94c4303b-aa25-47f5-ab95-20d485a2a61a");
                if (file == null) {
                    writeline("/home/ubuntu/results/coverage/Available/Available_7_10.coverage", "bfb7c713-cbc2-473c-96dd-7a7718fef50b");
                    throw new BuildException("The type attribute is only valid " + "when specifying the file " + "attribute.", getLocation());
                }
            }
            writeline("/home/ubuntu/results/coverage/Available/Available_7_10.coverage", "9ac768ce-280d-44e7-aaeb-03cfd1955a22");
            if (classpath != null) {
                writeline("/home/ubuntu/results/coverage/Available/Available_7_10.coverage", "89cf2534-1e8f-4827-b16f-ff7d9c0e0181");
                classpath.setProject(getProject());
                writeline("/home/ubuntu/results/coverage/Available/Available_7_10.coverage", "08ae64a0-f100-42b9-aa0b-d89587c32c92");
                this.loader = getProject().createClassLoader(classpath);
            }
            writeline("/home/ubuntu/results/coverage/Available/Available_7_10.coverage", "e5f2f6d1-576c-4f75-9232-91c80910179d");
            String appendix = "";
            writeline("/home/ubuntu/results/coverage/Available/Available_7_10.coverage", "fedd7b63-810a-4a4b-98da-5d9fc80ff161");
            if (isTask) {
                writeline("/home/ubuntu/results/coverage/Available/Available_7_10.coverage", "dc45301a-2077-4410-a47e-4c3dba87833a");
                appendix = " to set property " + property;
            } else {
                writeline("/home/ubuntu/results/coverage/Available/Available_7_10.coverage", "8536ebc4-ad03-4d5f-89cc-9009d6449ed0");
                setTaskName("available");
            }
            writeline("/home/ubuntu/results/coverage/Available/Available_7_10.coverage", "6681c66f-bca6-46dd-962b-1a2c156020bc");
            if ((classname != null) && !checkClass(classname)) {
                writeline("/home/ubuntu/results/coverage/Available/Available_7_10.coverage", "f9e0bfac-dda8-4eed-8e52-7e1b278ce66b");
                log("Unable to load class " + classname + appendix, Project.MSG_VERBOSE);
                writeline("/home/ubuntu/results/coverage/Available/Available_7_10.coverage", "899ee170-b739-4c25-b813-4b75e4fa4d9c");
                return false;
            }
            writeline("/home/ubuntu/results/coverage/Available/Available_7_10.coverage", "b5e5b7bc-d04b-4bf9-9265-2ac7779720a1");
            if ((file != null) && !checkFile()) {
                writeline("/home/ubuntu/results/coverage/Available/Available_7_10.coverage", "e58838b2-62c6-4f38-8882-dff6bf912533");
                StringBuffer buf = new StringBuffer("Unable to find ");
                writeline("/home/ubuntu/results/coverage/Available/Available_7_10.coverage", "6b60ca4b-0be1-439b-826e-a3091e954ad6");
                if (type != null) {
                    writeline("/home/ubuntu/results/coverage/Available/Available_7_10.coverage", "bd31588e-8093-43dd-aa6a-de629cf5c06c");
                    buf.append(type).append(' ');
                }
                writeline("/home/ubuntu/results/coverage/Available/Available_7_10.coverage", "6780ed8b-33cd-4a55-ac68-62ee1499caab");
                buf.append(filename).append(appendix);
                writeline("/home/ubuntu/results/coverage/Available/Available_7_10.coverage", "699dd72c-a6a7-4ab2-b1bd-caf3be7595da");
                log(buf.toString(), Project.MSG_VERBOSE);
                writeline("/home/ubuntu/results/coverage/Available/Available_7_10.coverage", "8073dec2-795c-4d9b-9fcc-0ba05de92753");
                return false;
            }
            writeline("/home/ubuntu/results/coverage/Available/Available_7_10.coverage", "90219872-ee0c-4d08-af48-6d7e00ec775b");
            if ((resource != null) && !checkResource(resource)) {
                writeline("/home/ubuntu/results/coverage/Available/Available_7_10.coverage", "6b9daa55-149b-40fe-ae57-7001268fef00");
                log("Unable to load resource " + resource + appendix, Project.MSG_VERBOSE);
                writeline("/home/ubuntu/results/coverage/Available/Available_7_10.coverage", "728f826b-2d68-4fee-82b1-7521d465eac0");
                return false;
            }
        } finally {
            writeline("/home/ubuntu/results/coverage/Available/Available_7_10.coverage", "45db4057-e92a-4941-aab0-8ae7cf9fded4");
            if (loader != null) {
                writeline("/home/ubuntu/results/coverage/Available/Available_7_10.coverage", "dd2241bd-6f2f-4cfd-8941-a871995047d1");
                loader.cleanup();
                writeline("/home/ubuntu/results/coverage/Available/Available_7_10.coverage", "813271a3-329b-47bb-b841-8450309977c5");
                loader = null;
            }
            writeline("/home/ubuntu/results/coverage/Available/Available_7_10.coverage", "b37a16fd-9a5d-4389-b324-da0e50d1ee3b");
            if (!isTask) {
                writeline("/home/ubuntu/results/coverage/Available/Available_7_10.coverage", "8fc52784-19fd-4155-97c3-46f6c3fcad0c");
                setTaskName(null);
            }
        }
        writeline("/home/ubuntu/results/coverage/Available/Available_7_10.coverage", "2e284b1a-1685-498c-91c4-452f38b225a3");
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
        writeline("/home/ubuntu/results/coverage/Available/Available_7_10.coverage", "7e2b02de-c9de-46c0-b665-502cdb0f048b");
        if (filepath == null) {
            writeline("/home/ubuntu/results/coverage/Available/Available_7_10.coverage", "1fce2e21-0d63-44f9-8bc2-a73a9fc6d53a");
            return checkFile(file, filename);
        } else {
            writeline("/home/ubuntu/results/coverage/Available/Available_7_10.coverage", "5563f3c0-60a1-46ea-ad4e-86a93090b97d");
            String[] paths = filepath.list();
            writeline("/home/ubuntu/results/coverage/Available/Available_7_10.coverage", "93842168-4658-4ead-9e1b-877ebee6426e");
            for (int i = 0; i < paths.length; ++i) {
                writeline("/home/ubuntu/results/coverage/Available/Available_7_10.coverage", "1f661bd7-d534-4944-8d5c-58af392cfe09");
                log("Searching " + paths[i], Project.MSG_VERBOSE);
                writeline("/home/ubuntu/results/coverage/Available/Available_7_10.coverage", "8d5c4adc-288f-4c1e-912f-345d036f9ddc");
                File path = new File(paths[i]);
                writeline("/home/ubuntu/results/coverage/Available/Available_7_10.coverage", "3f602567-b0ec-4470-8618-d69c22855209");
                // **   simple name specified   == path in list
                if (path.exists() && (filename.equals(paths[i]) || filename.equals(path.getName()))) {
                    writeline("/home/ubuntu/results/coverage/Available/Available_7_10.coverage", "5fe99231-e043-4d0a-8187-ea3302c0a1b7");
                    if (type == null) {
                        writeline("/home/ubuntu/results/coverage/Available/Available_7_10.coverage", "9cef8bda-2caf-4c01-81bc-3ea40bd1ad72");
                        log("Found: " + path, Project.MSG_VERBOSE);
                        writeline("/home/ubuntu/results/coverage/Available/Available_7_10.coverage", "10398f4e-08fe-41f6-84e9-f1214bf575bc");
                        return true;
                    } else if (type.isDir() && path.isDirectory()) {
                        writeline("/home/ubuntu/results/coverage/Available/Available_7_10.coverage", "c8f101af-f5a3-47c3-9386-4455044c26fe");
                        log("Found directory: " + path, Project.MSG_VERBOSE);
                        writeline("/home/ubuntu/results/coverage/Available/Available_7_10.coverage", "4458afde-c253-41fb-980d-5057294e688b");
                        return true;
                    } else if (type.isFile() && path.isFile()) {
                        writeline("/home/ubuntu/results/coverage/Available/Available_7_10.coverage", "b8c7104e-8516-419c-83ae-6dea6b2faf67");
                        log("Found file: " + path, Project.MSG_VERBOSE);
                        writeline("/home/ubuntu/results/coverage/Available/Available_7_10.coverage", "143fb0a7-6eaa-4d82-8854-235af1584231");
                        return true;
                    }
                    writeline("/home/ubuntu/results/coverage/Available/Available_7_10.coverage", "7fccb45d-24a3-4e99-aa0d-89831bfa37d3");
                    // not the requested type
                    return false;
                }
                writeline("/home/ubuntu/results/coverage/Available/Available_7_10.coverage", "9ad9d1d3-fa1b-493a-ac8d-ad15139aede8");
                File parent = path.getParentFile();
                writeline("/home/ubuntu/results/coverage/Available/Available_7_10.coverage", "e0aada4e-5512-4c7e-b6ff-06605da0041c");
                // **   full-pathname specified == parent dir of path in list
                if (parent != null && parent.exists() && filename.equals(parent.getAbsolutePath())) {
                    writeline("/home/ubuntu/results/coverage/Available/Available_7_10.coverage", "e31fb817-7d50-4424-a4ec-1de617501f93");
                    if (type == null) {
                        writeline("/home/ubuntu/results/coverage/Available/Available_7_10.coverage", "edbf145c-e9af-4af2-bbd3-b1dda8ee8299");
                        log("Found: " + parent, Project.MSG_VERBOSE);
                        writeline("/home/ubuntu/results/coverage/Available/Available_7_10.coverage", "44eefe6a-2c89-450f-abce-f75d44bcea5b");
                        return true;
                    } else if (type.isDir()) {
                        writeline("/home/ubuntu/results/coverage/Available/Available_7_10.coverage", "f1dbd505-d9f7-420b-94d9-bf7987d52dd6");
                        log("Found directory: " + parent, Project.MSG_VERBOSE);
                        writeline("/home/ubuntu/results/coverage/Available/Available_7_10.coverage", "e0472587-24f2-4d8c-be03-a9c6981f427c");
                        return true;
                    }
                    writeline("/home/ubuntu/results/coverage/Available/Available_7_10.coverage", "3e6868a8-1d9e-469e-8ec4-46595c5f9452");
                    // not the requested type
                    return false;
                }
                writeline("/home/ubuntu/results/coverage/Available/Available_7_10.coverage", "ae12df03-0d48-4237-ab96-10810f1a68cf");
                // **   simple name specified   == path in list + name
                if (path.exists() && path.isDirectory()) {
                    writeline("/home/ubuntu/results/coverage/Available/Available_7_10.coverage", "1bd69f8f-f405-4e1c-8a74-562b889772b5");
                    if (checkFile(new File(path, filename), filename + " in " + path)) {
                        writeline("/home/ubuntu/results/coverage/Available/Available_7_10.coverage", "b2b4b420-b4e5-42ec-8c23-ca6dd33e49d8");
                        return true;
                    }
                }
                writeline("/home/ubuntu/results/coverage/Available/Available_7_10.coverage", "616d094a-f3aa-46a2-962f-b0acd7e474af");
                // **   simple name specified   == parent dir + name
                while (searchParents && parent != null && parent.exists()) {
                    writeline("/home/ubuntu/results/coverage/Available/Available_7_10.coverage", "34309503-b290-4509-a533-672688a0da61");
                    if (checkFile(new File(parent, filename), filename + " in " + parent)) {
                        writeline("/home/ubuntu/results/coverage/Available/Available_7_10.coverage", "58e34e71-3357-4d3b-aa10-bc8376783bb7");
                        return true;
                    }
                    writeline("/home/ubuntu/results/coverage/Available/Available_7_10.coverage", "2a53ef69-bed0-4ef3-ac74-c7be6e9fd4dd");
                    parent = parent.getParentFile();
                }
            }
        }
        writeline("/home/ubuntu/results/coverage/Available/Available_7_10.coverage", "e2422bcb-7004-4c11-a631-a8e3918fbe8b");
        return false;
    }

    /**
     * Check if a given file exists and matches the required type.
     */
    private boolean checkFile(File f, String text) {
        writeline("/home/ubuntu/results/coverage/Available/Available_7_10.coverage", "bf84de92-13b2-4b14-861f-8255ca51c29f");
        if (type != null) {
            writeline("/home/ubuntu/results/coverage/Available/Available_7_10.coverage", "d0d506ef-78e0-4c86-bebc-816d8516f737");
            if (type.isDir()) {
                writeline("/home/ubuntu/results/coverage/Available/Available_7_10.coverage", "cf9f3432-ad0b-464b-816f-e4c8197428fc");
                if (f.isDirectory()) {
                    writeline("/home/ubuntu/results/coverage/Available/Available_7_10.coverage", "1b79d282-7d09-472c-a4b1-bbcc608703ed");
                    log("Found directory: " + text, Project.MSG_VERBOSE);
                }
                writeline("/home/ubuntu/results/coverage/Available/Available_7_10.coverage", "9c8b1063-167f-4457-adab-84a36270d677");
                return f.isDirectory();
            } else if (type.isFile()) {
                writeline("/home/ubuntu/results/coverage/Available/Available_7_10.coverage", "de87247e-da0a-4c3e-80eb-01b870249914");
                if (f.isFile()) {
                    writeline("/home/ubuntu/results/coverage/Available/Available_7_10.coverage", "013227e7-1b33-4183-92fa-586dc48509d5");
                    log("Found file: " + text, Project.MSG_VERBOSE);
                }
                writeline("/home/ubuntu/results/coverage/Available/Available_7_10.coverage", "f1ea5e19-70b0-4cbe-aebf-c8a78fdbbc91");
                return f.isFile();
            }
        }
        writeline("/home/ubuntu/results/coverage/Available/Available_7_10.coverage", "691362b1-c59a-457e-b2b5-f9e6dc69a2fc");
        if (f.exists()) {
            writeline("/home/ubuntu/results/coverage/Available/Available_7_10.coverage", "6d32b56f-3c54-4a5b-af53-4750f9df4857");
            log("Found: " + text, Project.MSG_VERBOSE);
        }
        writeline("/home/ubuntu/results/coverage/Available/Available_7_10.coverage", "57a98e58-00d4-48cb-9f65-3a71340b5b94");
        return f.exists();
    }

    /**
     * Check if a given resource can be loaded.
     */
    private boolean checkResource(String resource) {
        writeline("/home/ubuntu/results/coverage/Available/Available_7_10.coverage", "3cd1662d-4331-495e-a8a4-ce8a970affd3");
        InputStream is = null;
        writeline("/home/ubuntu/results/coverage/Available/Available_7_10.coverage", "11a10928-dbe7-4c17-8cec-84e91d4dc53c");
        try {
            writeline("/home/ubuntu/results/coverage/Available/Available_7_10.coverage", "7789b639-2f8c-488d-86f8-86ab07e485b7");
            if (loader != null) {
                writeline("/home/ubuntu/results/coverage/Available/Available_7_10.coverage", "b37b50d8-1333-4fdd-97bc-5e4762003b6d");
                is = loader.getResourceAsStream(resource);
            } else {
                writeline("/home/ubuntu/results/coverage/Available/Available_7_10.coverage", "13eb3d75-ffe3-4f89-b05a-cbe88413f684");
                ClassLoader cL = this.getClass().getClassLoader();
                writeline("/home/ubuntu/results/coverage/Available/Available_7_10.coverage", "6cdbf22f-423e-4743-bf2d-00f23c099742");
                if (cL != null) {
                    writeline("/home/ubuntu/results/coverage/Available/Available_7_10.coverage", "d5767290-6cc7-42d3-ad97-22a34c6bafdf");
                    is = cL.getResourceAsStream(resource);
                } else {
                    writeline("/home/ubuntu/results/coverage/Available/Available_7_10.coverage", "81de55c9-071c-4730-ac63-3008c6fc2759");
                    is = ClassLoader.getSystemResourceAsStream(resource);
                }
            }
            writeline("/home/ubuntu/results/coverage/Available/Available_7_10.coverage", "1fdfdd4e-2862-49cb-90d3-18d38509b4e3");
            return is != null;
        } finally {
            writeline("/home/ubuntu/results/coverage/Available/Available_7_10.coverage", "6d59636b-6f3a-4a2f-b37c-0293262090ad");
            FileUtils.close(is);
        }
    }

    /**
     * Check if a given class can be loaded.
     */
    private boolean checkClass(String classname) {
        writeline("/home/ubuntu/results/coverage/Available/Available_7_10.coverage", "e69a40ef-90d4-4a74-8c5e-07cb952f5002");
        try {
            writeline("/home/ubuntu/results/coverage/Available/Available_7_10.coverage", "7fb24717-3002-4a9f-b632-b24ec3b18c69");
            if (ignoreSystemclasses) {
                writeline("/home/ubuntu/results/coverage/Available/Available_7_10.coverage", "e5635058-5c94-4369-a934-9a79fa716b9a");
                loader = getProject().createClassLoader(classpath);
                writeline("/home/ubuntu/results/coverage/Available/Available_7_10.coverage", "493d7ea2-baad-443e-aeff-8ec6a7f281a9");
                loader.setParentFirst(false);
                writeline("/home/ubuntu/results/coverage/Available/Available_7_10.coverage", "d8a59a4e-7507-4e06-82ba-6eb2a4c69913");
                loader.addJavaLibraries();
                writeline("/home/ubuntu/results/coverage/Available/Available_7_10.coverage", "749fa73c-8b60-4786-ae54-9bfe06b66697");
                try {
                    writeline("/home/ubuntu/results/coverage/Available/Available_7_10.coverage", "83d1aef6-c146-40e2-b7ce-9a32b40da3a3");
                    loader.findClass(classname);
                } catch (SecurityException se) {
                    writeline("/home/ubuntu/results/coverage/Available/Available_7_10.coverage", "9bf9158d-1fac-468f-b882-7c89dbd8cacb");
                    // so catch the exception and return
                    return true;
                }
            } else if (loader != null) {
                writeline("/home/ubuntu/results/coverage/Available/Available_7_10.coverage", "be2fd8d7-ffc2-4313-86c0-db2b8364d47b");
                loader.loadClass(classname);
            } else {
                writeline("/home/ubuntu/results/coverage/Available/Available_7_10.coverage", "d16c8683-8981-4f3a-8852-f200acb7eb48");
                ClassLoader l = this.getClass().getClassLoader();
                writeline("/home/ubuntu/results/coverage/Available/Available_7_10.coverage", "0bc461e6-e991-4878-8fd6-8ba004d6ef7a");
                // see API docs of Class.getClassLoader.
                if (l != null) {
                    writeline("/home/ubuntu/results/coverage/Available/Available_7_10.coverage", "fcae2c56-5526-45b6-88c8-8205a8692e8b");
                    Class.forName(classname, true, l);
                } else {
                    writeline("/home/ubuntu/results/coverage/Available/Available_7_10.coverage", "a485e45c-e787-407a-af0d-1c3156b23235");
                    Class.forName(classname);
                }
            }
            writeline("/home/ubuntu/results/coverage/Available/Available_7_10.coverage", "6c513e82-3e25-417f-8d5c-46ec71b97a90");
            return true;
        } catch (ClassNotFoundException e) {
            writeline("/home/ubuntu/results/coverage/Available/Available_7_10.coverage", "7a76fd80-fd5f-4fc4-b893-f5da3dabe7da");
            log("class \"" + classname + "\" was not found", Project.MSG_DEBUG);
            writeline("/home/ubuntu/results/coverage/Available/Available_7_10.coverage", "38579105-01d0-4e1a-9704-c5339c23a7f8");
            return false;
        } catch (NoClassDefFoundError e) {
            writeline("/home/ubuntu/results/coverage/Available/Available_7_10.coverage", "6cab7f76-3c98-47a7-8fdb-c0a24b9f4409");
            log("Could not load dependent class \"" + e.getMessage() + "\" for class \"" + classname + "\"", Project.MSG_DEBUG);
            writeline("/home/ubuntu/results/coverage/Available/Available_7_10.coverage", "f88c23a5-adfe-4243-8fbe-31ad5ff630a7");
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
