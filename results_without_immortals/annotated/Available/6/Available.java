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
        writeline("/home/ubuntu/results/coverage/Available/Available_6_10.coverage", "d1852f03-7b8f-4edb-92d3-266f6650877f");
        this.searchParents = searchParents;
    }

    /**
     * Set the classpath to be used when searching for classes and resources.
     *
     * @param classpath an Ant Path object containing the search path.
     */
    public void setClasspath(Path classpath) {
        writeline("/home/ubuntu/results/coverage/Available/Available_6_10.coverage", "d7c741e4-a66b-48ce-a8f0-da9d98e1305b");
        createClasspath().append(classpath);
    }

    /**
     * Classpath to be used when searching for classes and resources.
     *
     * @return an empty Path instance to be configured by Ant.
     */
    public Path createClasspath() {
        writeline("/home/ubuntu/results/coverage/Available/Available_6_10.coverage", "ab81dc5f-d4da-4a4a-9ec3-c53515b0b14f");
        if (this.classpath == null) {
            writeline("/home/ubuntu/results/coverage/Available/Available_6_10.coverage", "f8b524b1-2f38-4e8d-b9a0-607a4c382a9d");
            this.classpath = new Path(getProject());
        }
        writeline("/home/ubuntu/results/coverage/Available/Available_6_10.coverage", "d13ec72a-fb69-4f8c-9bc5-af0da57079e5");
        return this.classpath.createPath();
    }

    /**
     * Set the classpath by reference.
     *
     * @param r a Reference to a Path instance to be used as the classpath
     * value.
     */
    public void setClasspathRef(Reference r) {
        writeline("/home/ubuntu/results/coverage/Available/Available_6_10.coverage", "f58394e3-9d87-4d12-b021-fbbc339bb9f0");
        createClasspath().setRefid(r);
    }

    /**
     * Set the path to use when looking for a file.
     *
     * @param filepath a Path instance containing the search path for files.
     */
    public void setFilepath(Path filepath) {
        writeline("/home/ubuntu/results/coverage/Available/Available_6_10.coverage", "9726feaa-87c9-4c3c-ae42-8d326c73ecfe");
        createFilepath().append(filepath);
    }

    /**
     * Path to search for file resources.
     *
     * @return a new Path instance which Ant will configure with a file search
     * path.
     */
    public Path createFilepath() {
        writeline("/home/ubuntu/results/coverage/Available/Available_6_10.coverage", "554af645-d612-46c4-9fe6-d675d6734930");
        if (this.filepath == null) {
            writeline("/home/ubuntu/results/coverage/Available/Available_6_10.coverage", "8fc73e48-6104-4612-ad10-b67f646a4caf");
            this.filepath = new Path(getProject());
        }
        writeline("/home/ubuntu/results/coverage/Available/Available_6_10.coverage", "7237fdb6-4303-4494-afc5-030910c2d3fd");
        return this.filepath.createPath();
    }

    /**
     * Set the name of the property which will be set if the particular resource
     * is available.
     *
     * @param property the name of the property to set.
     */
    public void setProperty(String property) {
        writeline("/home/ubuntu/results/coverage/Available/Available_6_10.coverage", "c18307f2-5225-4383-a5ab-c29b0cc7a0f0");
        this.property = property;
    }

    /**
     * Set the value to be given to the property if the desired resource is
     * available.
     *
     * @param value the value to be given.
     */
    public void setValue(Object value) {
        writeline("/home/ubuntu/results/coverage/Available/Available_6_10.coverage", "8718f8fa-7772-4ad7-9791-e1d84293ceed");
        this.value = value;
    }

    /**
     * Set the value to be given to the property if the desired resource is
     * available.
     *
     * @param value the value to be given.
     */
    public void setValue(String value) {
        writeline("/home/ubuntu/results/coverage/Available/Available_6_10.coverage", "0a5b8446-7b53-4b69-95c2-550e8679d340");
        setValue((Object) value);
    }

    /**
     * Set a classname of a class which must be available to set the given
     * property.
     *
     * @param classname the name of the class required.
     */
    public void setClassname(String classname) {
        writeline("/home/ubuntu/results/coverage/Available/Available_6_10.coverage", "2e6ac5af-fff2-4727-a375-468016d50c14");
        if (!"".equals(classname)) {
            writeline("/home/ubuntu/results/coverage/Available/Available_6_10.coverage", "119d0418-fffb-4ea1-a6a0-63aa98f31d3f");
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
        writeline("/home/ubuntu/results/coverage/Available/Available_6_10.coverage", "25e5b4ec-a498-4479-a933-6270618c2c90");
        this.file = file;
        writeline("/home/ubuntu/results/coverage/Available/Available_6_10.coverage", "3b77693a-fc7a-4a37-81ec-afeaed0143f4");
        this.filename = FILE_UTILS.removeLeadingPath(getProject().getBaseDir(), file);
    }

    /**
     * Set the name of a Java resource which is required to set the property.
     *
     * @param resource the name of a resource which is required to be available.
     */
    public void setResource(String resource) {
        writeline("/home/ubuntu/results/coverage/Available/Available_6_10.coverage", "d5c00d97-5878-4bcb-b346-1d335cbfe0ea");
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
        writeline("/home/ubuntu/results/coverage/Available/Available_6_10.coverage", "03c40779-a3c6-4ce5-ae05-aca7f9655454");
        log("DEPRECATED - The setType(String) method has been deprecated." + " Use setType(Available.FileDir) instead.", Project.MSG_WARN);
        writeline("/home/ubuntu/results/coverage/Available/Available_6_10.coverage", "fe9aff52-2ece-41cd-972d-5689a5cc9d68");
        this.type = new FileDir();
        writeline("/home/ubuntu/results/coverage/Available/Available_6_10.coverage", "97521309-809c-4cc7-a858-2f0907d1fa85");
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
        writeline("/home/ubuntu/results/coverage/Available/Available_6_10.coverage", "73f3094b-a7d5-4168-afa8-7ca60d74132e");
        this.type = type;
    }

    /**
     * Set whether the search for classes should ignore the runtime classes and
     * just use the given classpath.
     *
     * @param ignore true if system classes are to be ignored.
     */
    public void setIgnoresystemclasses(boolean ignore) {
        writeline("/home/ubuntu/results/coverage/Available/Available_6_10.coverage", "77ffcd7d-d26a-4775-8153-9dd4a60fa715");
        this.ignoreSystemclasses = ignore;
    }

    /**
     * Entry point when operating as a task.
     *
     * @exception BuildException if the task is not configured correctly.
     */
    public void execute() throws BuildException {
        writeline("/home/ubuntu/results/coverage/Available/Available_6_10.coverage", "06dc15d3-e67f-45fd-bd3e-91926cbafbb0");
        if (property == null) {
            writeline("/home/ubuntu/results/coverage/Available/Available_6_10.coverage", "a0190143-bb62-454d-9a0c-42058b9f361b");
            throw new BuildException("property attribute is required", getLocation());
        }
        writeline("/home/ubuntu/results/coverage/Available/Available_6_10.coverage", "8d745198-4378-47b6-a3b7-227b0764abb1");
        isTask = true;
        writeline("/home/ubuntu/results/coverage/Available/Available_6_10.coverage", "e52d35db-9efe-4eef-9ba2-edd1a38778de");
        try {
            writeline("/home/ubuntu/results/coverage/Available/Available_6_10.coverage", "90c80dd6-5c7a-4528-83fe-698dbeef3f7f");
            if (eval()) {
                writeline("/home/ubuntu/results/coverage/Available/Available_6_10.coverage", "b6d8fd08-88bd-4b2c-a9e4-439d3a02d503");
                PropertyHelper ph = PropertyHelper.getPropertyHelper(getProject());
                writeline("/home/ubuntu/results/coverage/Available/Available_6_10.coverage", "95e43c9e-24fd-4d4e-987f-9f7422f1ede1");
                Object oldvalue = ph.getProperty(property);
                writeline("/home/ubuntu/results/coverage/Available/Available_6_10.coverage", "44148a53-537c-4d84-ba74-337abaf9e7e6");
                if (null != oldvalue && !oldvalue.equals(value)) {
                    writeline("/home/ubuntu/results/coverage/Available/Available_6_10.coverage", "37474f9a-e66e-4ea8-babc-d7e27e073cd3");
                    log("DEPRECATED - <available> used to override an existing" + " property." + StringUtils.LINE_SEP + "  Build file should not reuse the same property" + " name for different values.", Project.MSG_WARN);
                }
                writeline("/home/ubuntu/results/coverage/Available/Available_6_10.coverage", "70029265-06d1-49a6-8e99-c1c6bf14c525");
                // NB: this makes use of Project#setProperty rather than Project#setNewProperty
                // due to backwards compatibility reasons
                ph.setProperty(property, value, true);
            }
        } finally {
            writeline("/home/ubuntu/results/coverage/Available/Available_6_10.coverage", "d8019f4c-4211-4429-89c8-e18ed78004e0");
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
        writeline("/home/ubuntu/results/coverage/Available/Available_6_10.coverage", "cc6d3314-9f18-42ec-8a7e-7d9cacf16d3b");
        try {
            writeline("/home/ubuntu/results/coverage/Available/Available_6_10.coverage", "4e7d1e1a-3c6d-4ad5-ba22-47c7e49f1093");
            if (classname == null && file == null && resource == null) {
                writeline("/home/ubuntu/results/coverage/Available/Available_6_10.coverage", "1ca54ef2-ba61-41d8-80fd-2773d9c87b37");
                throw new BuildException("At least one of (classname|file|" + "resource) is required", getLocation());
            }
            writeline("/home/ubuntu/results/coverage/Available/Available_6_10.coverage", "8ec6a743-f690-4083-bb4a-58051287b35f");
            if (type != null) {
                writeline("/home/ubuntu/results/coverage/Available/Available_6_10.coverage", "6de8ad7d-777b-45af-b85d-d2f8413aee28");
                if (file == null) {
                    writeline("/home/ubuntu/results/coverage/Available/Available_6_10.coverage", "02276201-f6ba-42cf-a3b2-7f22fbf067cb");
                    throw new BuildException("The type attribute is only valid " + "when specifying the file " + "attribute.", getLocation());
                }
            }
            writeline("/home/ubuntu/results/coverage/Available/Available_6_10.coverage", "1c644c8b-8424-45fa-8e3e-da6c316d361b");
            if (classpath != null) {
                writeline("/home/ubuntu/results/coverage/Available/Available_6_10.coverage", "7665b234-62da-4010-9f62-db32dc32b81d");
                classpath.setProject(getProject());
                writeline("/home/ubuntu/results/coverage/Available/Available_6_10.coverage", "dd9cf886-5b85-4a3a-9090-31241aa3c6fd");
                this.loader = getProject().createClassLoader(classpath);
            }
            writeline("/home/ubuntu/results/coverage/Available/Available_6_10.coverage", "61621925-82cb-4d7e-bba6-63730cade07b");
            String appendix = "";
            writeline("/home/ubuntu/results/coverage/Available/Available_6_10.coverage", "c89edf17-76ed-4d69-8325-369bea516b96");
            if (isTask) {
                writeline("/home/ubuntu/results/coverage/Available/Available_6_10.coverage", "8825d2c7-62d5-44a2-83de-074c3dabbad9");
                appendix = " to set property " + property;
            } else {
                writeline("/home/ubuntu/results/coverage/Available/Available_6_10.coverage", "fea74e70-fa35-4544-b4fe-4c94c5574e3b");
                setTaskName("available");
            }
            writeline("/home/ubuntu/results/coverage/Available/Available_6_10.coverage", "d5ec063c-4946-4595-85f0-c8c6c8595f78");
            if ((classname != null) && !checkClass(classname)) {
                writeline("/home/ubuntu/results/coverage/Available/Available_6_10.coverage", "a071ef44-6784-4eb0-87b4-3f2a90a5df02");
                log("Unable to load class " + classname + appendix, Project.MSG_VERBOSE);
                writeline("/home/ubuntu/results/coverage/Available/Available_6_10.coverage", "5fe39e6b-a1bc-4463-a9dc-8b9ecc7a6086");
                return false;
            }
            writeline("/home/ubuntu/results/coverage/Available/Available_6_10.coverage", "4ccd96c2-c520-4496-a930-0650c95b9340");
            if ((file != null) && !checkFile()) {
                writeline("/home/ubuntu/results/coverage/Available/Available_6_10.coverage", "4c7d61f0-3602-4140-aa46-5cc6a6e351e3");
                StringBuffer buf = new StringBuffer("Unable to find ");
                writeline("/home/ubuntu/results/coverage/Available/Available_6_10.coverage", "dfa8ba94-c072-4da0-9cf4-ff4263dff3dc");
                if (type != null) {
                    writeline("/home/ubuntu/results/coverage/Available/Available_6_10.coverage", "a2508ac4-5e38-4fc4-8a1e-9cf3f88d6f80");
                    buf.append(type).append(' ');
                }
                writeline("/home/ubuntu/results/coverage/Available/Available_6_10.coverage", "3c00db75-5df2-4bdc-9b52-d15d2d4dc441");
                buf.append(filename).append(appendix);
                writeline("/home/ubuntu/results/coverage/Available/Available_6_10.coverage", "89add0b7-0236-4fda-9ef5-aaf9ce3578f3");
                log(buf.toString(), Project.MSG_VERBOSE);
                writeline("/home/ubuntu/results/coverage/Available/Available_6_10.coverage", "675a8b24-336f-4168-a3ef-82a770847e7c");
                return false;
            }
            writeline("/home/ubuntu/results/coverage/Available/Available_6_10.coverage", "27886a17-f397-427c-b499-df5134e4546c");
            if ((resource != null) && !checkResource(resource)) {
                writeline("/home/ubuntu/results/coverage/Available/Available_6_10.coverage", "c0dde0e5-82c4-4414-872f-459eba1e6d60");
                log("Unable to load resource " + resource + appendix, Project.MSG_VERBOSE);
                writeline("/home/ubuntu/results/coverage/Available/Available_6_10.coverage", "3b0ed23c-bd0f-4994-8465-3f2f0178b1fa");
                return false;
            }
        } finally {
            writeline("/home/ubuntu/results/coverage/Available/Available_6_10.coverage", "650f9b27-9d48-4136-ac7a-bc810ffe11f4");
            if (loader != null) {
                writeline("/home/ubuntu/results/coverage/Available/Available_6_10.coverage", "d14fbeeb-785c-4655-9d0b-41878d9651c2");
                loader.cleanup();
                writeline("/home/ubuntu/results/coverage/Available/Available_6_10.coverage", "6d8640e9-6161-4e74-a968-b63c2a090303");
                loader = null;
            }
            writeline("/home/ubuntu/results/coverage/Available/Available_6_10.coverage", "d05f8584-7f8f-4af7-8f41-54b2b09fdccd");
            if (!isTask) {
                writeline("/home/ubuntu/results/coverage/Available/Available_6_10.coverage", "f4ce46c9-2531-4fc6-b18b-dd9a80cdb063");
                setTaskName(null);
            }
        }
        writeline("/home/ubuntu/results/coverage/Available/Available_6_10.coverage", "ae9a5405-17b9-4e04-b116-51c632fb947e");
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
        writeline("/home/ubuntu/results/coverage/Available/Available_6_10.coverage", "7792aee8-795a-4a74-891d-2b83fd71a309");
        if (filepath == null) {
            writeline("/home/ubuntu/results/coverage/Available/Available_6_10.coverage", "0b02a88b-76a0-44fc-86b6-ce210a348072");
            return checkFile(file, filename);
        } else {
            writeline("/home/ubuntu/results/coverage/Available/Available_6_10.coverage", "4348b03a-a914-49e7-840b-2966de70735e");
            String[] paths = filepath.list();
            writeline("/home/ubuntu/results/coverage/Available/Available_6_10.coverage", "d45fedb3-cd44-4a21-8658-37d4932e420b");
            for (int i = 0; i < paths.length; ++i) {
                writeline("/home/ubuntu/results/coverage/Available/Available_6_10.coverage", "95306148-9d4c-48d3-a5ac-845673b21e50");
                log("Searching " + paths[i], Project.MSG_VERBOSE);
                writeline("/home/ubuntu/results/coverage/Available/Available_6_10.coverage", "bd69fd4d-6431-47a9-8153-46ee40b2b601");
                File path = new File(paths[i]);
                writeline("/home/ubuntu/results/coverage/Available/Available_6_10.coverage", "1605df57-063c-436e-84d4-ef71c6346e1a");
                // **   simple name specified   == path in list
                if (path.exists() && (filename.equals(paths[i]) || filename.equals(path.getName()))) {
                    writeline("/home/ubuntu/results/coverage/Available/Available_6_10.coverage", "7f3322f8-bc9f-43b8-a32f-15442f081657");
                    if (type == null) {
                        writeline("/home/ubuntu/results/coverage/Available/Available_6_10.coverage", "a2c2f59d-0901-42d2-9611-a8c9e6da2bec");
                        log("Found: " + path, Project.MSG_VERBOSE);
                        writeline("/home/ubuntu/results/coverage/Available/Available_6_10.coverage", "404236dc-1b7d-4463-a99f-41a6e99b3edb");
                        return true;
                    } else if (type.isDir() && path.isDirectory()) {
                        writeline("/home/ubuntu/results/coverage/Available/Available_6_10.coverage", "80638d39-a635-4b58-b1a3-1f18a40ef4cb");
                        log("Found directory: " + path, Project.MSG_VERBOSE);
                        writeline("/home/ubuntu/results/coverage/Available/Available_6_10.coverage", "2ea25116-94bb-4cb3-b586-0598899073fc");
                        return true;
                    } else if (type.isFile() && path.isFile()) {
                        writeline("/home/ubuntu/results/coverage/Available/Available_6_10.coverage", "7dc9dfe8-9079-4d98-9923-e88b4a7378fc");
                        log("Found file: " + path, Project.MSG_VERBOSE);
                        writeline("/home/ubuntu/results/coverage/Available/Available_6_10.coverage", "a15c4b57-1b8d-4296-ab53-1cd890d5572c");
                        return true;
                    }
                    writeline("/home/ubuntu/results/coverage/Available/Available_6_10.coverage", "0dfd85a1-23ad-459d-8986-caeeb807b1fd");
                    // not the requested type
                    return false;
                }
                writeline("/home/ubuntu/results/coverage/Available/Available_6_10.coverage", "27c649bd-0d47-4b5f-8bcd-5723e89d12c7");
                File parent = path.getParentFile();
                writeline("/home/ubuntu/results/coverage/Available/Available_6_10.coverage", "9006ede3-d406-47b8-91f6-3231d7c5e5f9");
                // **   full-pathname specified == parent dir of path in list
                if (parent != null && parent.exists() && filename.equals(parent.getAbsolutePath())) {
                    writeline("/home/ubuntu/results/coverage/Available/Available_6_10.coverage", "07434607-8754-4510-84d1-8b5ae85a7322");
                    if (type == null) {
                        writeline("/home/ubuntu/results/coverage/Available/Available_6_10.coverage", "77c679fc-6250-4786-b8c6-d3dd5dafa403");
                        log("Found: " + parent, Project.MSG_VERBOSE);
                        writeline("/home/ubuntu/results/coverage/Available/Available_6_10.coverage", "bd88cc47-70f9-4210-a0ca-331824dabc06");
                        return true;
                    } else if (type.isDir()) {
                        writeline("/home/ubuntu/results/coverage/Available/Available_6_10.coverage", "9664aac6-b0de-4156-98db-e1dd41640956");
                        log("Found directory: " + parent, Project.MSG_VERBOSE);
                        writeline("/home/ubuntu/results/coverage/Available/Available_6_10.coverage", "eb737aab-8ebc-467e-afb9-3d10e69bcfef");
                        return true;
                    }
                    writeline("/home/ubuntu/results/coverage/Available/Available_6_10.coverage", "0c55d333-c8a4-439a-9699-c402a6f26c47");
                    // not the requested type
                    return false;
                }
                writeline("/home/ubuntu/results/coverage/Available/Available_6_10.coverage", "21d0912e-c2e9-4782-9fb0-b855e6986693");
                // **   simple name specified   == path in list + name
                if (path.exists() && path.isDirectory()) {
                    writeline("/home/ubuntu/results/coverage/Available/Available_6_10.coverage", "c0d87848-0c74-4424-8a20-34904462a4e2");
                    if (checkFile(new File(path, filename), filename + " in " + path)) {
                        writeline("/home/ubuntu/results/coverage/Available/Available_6_10.coverage", "1ee8730b-6c11-4832-9bdf-11fe34233187");
                        return true;
                    }
                }
                writeline("/home/ubuntu/results/coverage/Available/Available_6_10.coverage", "1afd5499-94b4-4589-89a2-7ff398895f7f");
                // **   simple name specified   == parent dir + name
                while (searchParents && parent != null && parent.exists()) {
                    writeline("/home/ubuntu/results/coverage/Available/Available_6_10.coverage", "60953e56-584a-4546-ada6-55109ab21227");
                    if (checkFile(new File(parent, filename), filename + " in " + parent)) {
                        writeline("/home/ubuntu/results/coverage/Available/Available_6_10.coverage", "13f60a72-ee6c-48d9-9332-a3e3c3caf30d");
                        return true;
                    }
                    writeline("/home/ubuntu/results/coverage/Available/Available_6_10.coverage", "e0e94f91-8141-41b1-a98f-fdc9439ea7a6");
                    parent = parent.getParentFile();
                }
            }
        }
        writeline("/home/ubuntu/results/coverage/Available/Available_6_10.coverage", "42d2dd77-fee6-4b1f-9f31-5a0826237793");
        return false;
    }

    /**
     * Check if a given file exists and matches the required type.
     */
    private boolean checkFile(File f, String text) {
        writeline("/home/ubuntu/results/coverage/Available/Available_6_10.coverage", "925c7031-3f7b-4360-ac46-754fe8a10658");
        if (type != null) {
            writeline("/home/ubuntu/results/coverage/Available/Available_6_10.coverage", "d7b7af76-41c0-44f1-9ba1-993f5bc4290a");
            if (type.isDir()) {
                writeline("/home/ubuntu/results/coverage/Available/Available_6_10.coverage", "97defc36-09a1-494a-beac-7d81878fe22c");
                if (f.isDirectory()) {
                    writeline("/home/ubuntu/results/coverage/Available/Available_6_10.coverage", "f9f9b042-c540-419d-9231-e622359a7f40");
                    log("Found directory: " + text, Project.MSG_VERBOSE);
                }
                writeline("/home/ubuntu/results/coverage/Available/Available_6_10.coverage", "c34a1186-4479-4e28-9e40-39d62c5badb6");
                return f.isDirectory();
            } else if (type.isFile()) {
                writeline("/home/ubuntu/results/coverage/Available/Available_6_10.coverage", "569bc2c2-9673-45c5-8959-1fed2e58fb11");
                if (f.isFile()) {
                    writeline("/home/ubuntu/results/coverage/Available/Available_6_10.coverage", "d5259e16-6436-4f74-8c4e-db0af074b61c");
                    log("Found file: " + text, Project.MSG_VERBOSE);
                }
                writeline("/home/ubuntu/results/coverage/Available/Available_6_10.coverage", "af8116f9-c74a-4b88-b200-651e90703224");
                return f.isFile();
            }
        }
        writeline("/home/ubuntu/results/coverage/Available/Available_6_10.coverage", "e0a134c3-74fa-4c4b-95dc-92aa16459b68");
        if (f.exists()) {
            writeline("/home/ubuntu/results/coverage/Available/Available_6_10.coverage", "9049fd36-8542-4de4-81a0-b10749bb24d8");
            log("Found: " + text, Project.MSG_VERBOSE);
        }
        writeline("/home/ubuntu/results/coverage/Available/Available_6_10.coverage", "d7b2ee0d-e95a-4096-bc44-a748e8409946");
        return f.exists();
    }

    /**
     * Check if a given resource can be loaded.
     */
    private boolean checkResource(String resource) {
        writeline("/home/ubuntu/results/coverage/Available/Available_6_10.coverage", "9db15612-c70d-4cc4-9658-589dfbf6955c");
        InputStream is = null;
        writeline("/home/ubuntu/results/coverage/Available/Available_6_10.coverage", "8ebd4502-dffe-41bb-b9a1-84a8940b7d1f");
        try {
            writeline("/home/ubuntu/results/coverage/Available/Available_6_10.coverage", "786b4f47-e87b-46a6-b74d-3d8099f3cb6c");
            if (loader != null) {
                writeline("/home/ubuntu/results/coverage/Available/Available_6_10.coverage", "6b20c916-915f-4e20-ae41-47f1ba5f2b26");
                is = loader.getResourceAsStream(resource);
            } else {
                writeline("/home/ubuntu/results/coverage/Available/Available_6_10.coverage", "dc462b37-337c-407a-afaf-e5441c0d5b8a");
                ClassLoader cL = this.getClass().getClassLoader();
                writeline("/home/ubuntu/results/coverage/Available/Available_6_10.coverage", "be2047a3-919d-497f-bd05-2ae7fa88c71b");
                if (cL != null) {
                    writeline("/home/ubuntu/results/coverage/Available/Available_6_10.coverage", "2eee133a-308a-4361-a100-44a591bf2dce");
                    is = cL.getResourceAsStream(resource);
                } else {
                    writeline("/home/ubuntu/results/coverage/Available/Available_6_10.coverage", "191dd4af-50a4-47f6-8ffd-205efa82a57b");
                    is = ClassLoader.getSystemResourceAsStream(resource);
                }
            }
            writeline("/home/ubuntu/results/coverage/Available/Available_6_10.coverage", "0a88e3e8-c4ef-42fb-9098-1dda381be952");
            return is != null;
        } finally {
            writeline("/home/ubuntu/results/coverage/Available/Available_6_10.coverage", "6dc579cc-9d4d-4936-ad63-828af1c50766");
            FileUtils.close(is);
        }
    }

    /**
     * Check if a given class can be loaded.
     */
    private boolean checkClass(String classname) {
        writeline("/home/ubuntu/results/coverage/Available/Available_6_10.coverage", "d9356858-fc91-4a12-97b2-3c9aef788ce4");
        try {
            writeline("/home/ubuntu/results/coverage/Available/Available_6_10.coverage", "e9020680-ed42-4db8-83b4-895b391ee125");
            if (ignoreSystemclasses) {
                writeline("/home/ubuntu/results/coverage/Available/Available_6_10.coverage", "df647aec-363b-4d50-8334-13cfd0a1469d");
                loader = getProject().createClassLoader(classpath);
                writeline("/home/ubuntu/results/coverage/Available/Available_6_10.coverage", "13296684-6823-4e0b-8864-0070e9e581e1");
                loader.setParentFirst(false);
                writeline("/home/ubuntu/results/coverage/Available/Available_6_10.coverage", "869176ad-daf8-4ce0-8b77-afc5156423c1");
                loader.addJavaLibraries();
                writeline("/home/ubuntu/results/coverage/Available/Available_6_10.coverage", "4d522aae-411f-431a-a9ff-8cdb2bce46dd");
                try {
                    writeline("/home/ubuntu/results/coverage/Available/Available_6_10.coverage", "9cebb246-e986-453c-9f95-055d79400548");
                    loader.findClass(classname);
                } catch (SecurityException se) {
                    writeline("/home/ubuntu/results/coverage/Available/Available_6_10.coverage", "20e7539b-2326-454b-8ed4-19541775bcdb");
                    // so catch the exception and return
                    return true;
                }
            } else if (loader != null) {
                writeline("/home/ubuntu/results/coverage/Available/Available_6_10.coverage", "cc56cdc3-a29a-4564-b8f8-e342f279d80f");
                loader.loadClass(classname);
            } else {
                writeline("/home/ubuntu/results/coverage/Available/Available_6_10.coverage", "ed8b80f4-4875-4ff3-9ad9-c7309f2977a6");
                ClassLoader l = this.getClass().getClassLoader();
                writeline("/home/ubuntu/results/coverage/Available/Available_6_10.coverage", "67a78498-a7f2-4299-b19e-5f4ea32c5a1d");
                // see API docs of Class.getClassLoader.
                if (l != null) {
                    writeline("/home/ubuntu/results/coverage/Available/Available_6_10.coverage", "92895ddf-d872-4077-8746-ae4bfa095c7c");
                    Class.forName(classname, true, l);
                } else {
                    writeline("/home/ubuntu/results/coverage/Available/Available_6_10.coverage", "db99ff2a-6c49-4c86-9092-63559a2cb006");
                    Class.forName(classname);
                }
            }
            writeline("/home/ubuntu/results/coverage/Available/Available_6_10.coverage", "64a8cc00-8e3e-4594-a6a5-0b184a53157c");
            return true;
        } catch (ClassNotFoundException e) {
            writeline("/home/ubuntu/results/coverage/Available/Available_6_10.coverage", "4fb6f08e-44cd-4eee-8fd4-4b6db5066022");
            log("class \"" + classname + "\" was not found", Project.MSG_DEBUG);
            writeline("/home/ubuntu/results/coverage/Available/Available_6_10.coverage", "c88671cd-1855-4a6d-94ae-b6fc60a05c59");
            return false;
        } catch (NoClassDefFoundError e) {
            writeline("/home/ubuntu/results/coverage/Available/Available_6_10.coverage", "5d950f17-a68c-464e-8a04-739ede7b1127");
            log("Could not load dependent class \"" + e.getMessage() + "\" for class \"" + classname + "\"", Project.MSG_DEBUG);
            writeline("/home/ubuntu/results/coverage/Available/Available_6_10.coverage", "f4570c75-9d8f-4b47-8ee4-2233fd05e548");
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
