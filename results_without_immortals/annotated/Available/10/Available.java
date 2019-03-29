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
        writeline("/home/ubuntu/results/coverage/Available/Available_10_10.coverage", "86f5e486-c6fd-45de-bec3-697123eeb7c5");
        this.searchParents = searchParents;
    }

    /**
     * Set the classpath to be used when searching for classes and resources.
     *
     * @param classpath an Ant Path object containing the search path.
     */
    public void setClasspath(Path classpath) {
        writeline("/home/ubuntu/results/coverage/Available/Available_10_10.coverage", "80a37c15-e5c7-4bb6-aea2-f15df48d48b6");
        createClasspath().append(classpath);
    }

    /**
     * Classpath to be used when searching for classes and resources.
     *
     * @return an empty Path instance to be configured by Ant.
     */
    public Path createClasspath() {
        writeline("/home/ubuntu/results/coverage/Available/Available_10_10.coverage", "351d75ce-7514-49a6-b5ef-3fe6678fa417");
        if (this.classpath == null) {
            writeline("/home/ubuntu/results/coverage/Available/Available_10_10.coverage", "8cdf6367-54c1-4eb5-b9fc-d4f97312c6bb");
            this.classpath = new Path(getProject());
        }
        writeline("/home/ubuntu/results/coverage/Available/Available_10_10.coverage", "3611b53c-eeb2-4086-a75c-cb0bf1d4a36d");
        return this.classpath.createPath();
    }

    /**
     * Set the classpath by reference.
     *
     * @param r a Reference to a Path instance to be used as the classpath
     * value.
     */
    public void setClasspathRef(Reference r) {
        writeline("/home/ubuntu/results/coverage/Available/Available_10_10.coverage", "65f3f9f6-290d-448a-834e-8fecfd5b18d9");
        createClasspath().setRefid(r);
    }

    /**
     * Set the path to use when looking for a file.
     *
     * @param filepath a Path instance containing the search path for files.
     */
    public void setFilepath(Path filepath) {
        writeline("/home/ubuntu/results/coverage/Available/Available_10_10.coverage", "dfb64ce1-40b2-4408-9d22-f2fbadc28848");
        createFilepath().append(filepath);
    }

    /**
     * Path to search for file resources.
     *
     * @return a new Path instance which Ant will configure with a file search
     * path.
     */
    public Path createFilepath() {
        writeline("/home/ubuntu/results/coverage/Available/Available_10_10.coverage", "d0e2fbad-06d5-4175-be37-369837bb8388");
        if (this.filepath == null) {
            writeline("/home/ubuntu/results/coverage/Available/Available_10_10.coverage", "0660577d-a111-4206-9498-5852d1a4e616");
            this.filepath = new Path(getProject());
        }
        writeline("/home/ubuntu/results/coverage/Available/Available_10_10.coverage", "7fe4f7c2-53b3-4568-ac3d-9e62f691bf17");
        return this.filepath.createPath();
    }

    /**
     * Set the name of the property which will be set if the particular resource
     * is available.
     *
     * @param property the name of the property to set.
     */
    public void setProperty(String property) {
        writeline("/home/ubuntu/results/coverage/Available/Available_10_10.coverage", "06f71045-1e38-4b18-bda0-f952cbdfaf0c");
        this.property = property;
    }

    /**
     * Set the value to be given to the property if the desired resource is
     * available.
     *
     * @param value the value to be given.
     */
    public void setValue(Object value) {
        writeline("/home/ubuntu/results/coverage/Available/Available_10_10.coverage", "34219d12-900f-4517-b5f8-18c9b42b4742");
        this.value = value;
    }

    /**
     * Set the value to be given to the property if the desired resource is
     * available.
     *
     * @param value the value to be given.
     */
    public void setValue(String value) {
        writeline("/home/ubuntu/results/coverage/Available/Available_10_10.coverage", "639f7ce1-28e2-491d-bc81-ad3912ac83d1");
        setValue((Object) value);
    }

    /**
     * Set a classname of a class which must be available to set the given
     * property.
     *
     * @param classname the name of the class required.
     */
    public void setClassname(String classname) {
        writeline("/home/ubuntu/results/coverage/Available/Available_10_10.coverage", "06e04a6c-4037-494e-a577-0c908686c037");
        if (!"".equals(classname)) {
            writeline("/home/ubuntu/results/coverage/Available/Available_10_10.coverage", "2729244d-f26d-4ed9-8362-d6a0c8679471");
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
        writeline("/home/ubuntu/results/coverage/Available/Available_10_10.coverage", "dbc9b720-dec0-4c4e-85f7-0fe573058579");
        this.file = file;
        writeline("/home/ubuntu/results/coverage/Available/Available_10_10.coverage", "6c0811b2-6f7e-4b60-80f5-8be456b7afaf");
        this.filename = FILE_UTILS.removeLeadingPath(getProject().getBaseDir(), file);
    }

    /**
     * Set the name of a Java resource which is required to set the property.
     *
     * @param resource the name of a resource which is required to be available.
     */
    public void setResource(String resource) {
        writeline("/home/ubuntu/results/coverage/Available/Available_10_10.coverage", "4c276927-91b4-407d-ba5e-c96c687de702");
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
        writeline("/home/ubuntu/results/coverage/Available/Available_10_10.coverage", "d719e474-8ec7-4867-9d97-f0cd92d56946");
        log("DEPRECATED - The setType(String) method has been deprecated." + " Use setType(Available.FileDir) instead.", Project.MSG_WARN);
        writeline("/home/ubuntu/results/coverage/Available/Available_10_10.coverage", "4cb6ff61-79c0-43d2-a473-86152703d429");
        this.type = new FileDir();
        writeline("/home/ubuntu/results/coverage/Available/Available_10_10.coverage", "c95ed1f3-eda0-400f-83db-b2e692b5e46d");
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
        writeline("/home/ubuntu/results/coverage/Available/Available_10_10.coverage", "7712a7e6-c5d6-4a07-a19f-74de7c6a59af");
        this.type = type;
    }

    /**
     * Set whether the search for classes should ignore the runtime classes and
     * just use the given classpath.
     *
     * @param ignore true if system classes are to be ignored.
     */
    public void setIgnoresystemclasses(boolean ignore) {
        writeline("/home/ubuntu/results/coverage/Available/Available_10_10.coverage", "b796fdf0-4fee-4e58-a5ff-dd9b5c30e49c");
        this.ignoreSystemclasses = ignore;
    }

    /**
     * Entry point when operating as a task.
     *
     * @exception BuildException if the task is not configured correctly.
     */
    public void execute() throws BuildException {
        writeline("/home/ubuntu/results/coverage/Available/Available_10_10.coverage", "04f3d32c-e98f-4624-a9e3-15406ea17488");
        if (property == null) {
            writeline("/home/ubuntu/results/coverage/Available/Available_10_10.coverage", "681e2221-d172-4428-a6d7-036e97d35292");
            throw new BuildException("property attribute is required", getLocation());
        }
        writeline("/home/ubuntu/results/coverage/Available/Available_10_10.coverage", "d42c977f-d762-40f4-bcc2-d3fb4dab42df");
        isTask = true;
        writeline("/home/ubuntu/results/coverage/Available/Available_10_10.coverage", "b2dc017b-da7e-48ee-b489-18159eea34c1");
        try {
            writeline("/home/ubuntu/results/coverage/Available/Available_10_10.coverage", "b5f377d3-0f1d-4bf8-b5a3-c006c6b97d67");
            if (eval()) {
                writeline("/home/ubuntu/results/coverage/Available/Available_10_10.coverage", "61631733-18da-4c96-a63a-3daf2cd2a71c");
                PropertyHelper ph = PropertyHelper.getPropertyHelper(getProject());
                writeline("/home/ubuntu/results/coverage/Available/Available_10_10.coverage", "c9c0f2a6-d918-4853-8c42-6f55f000b871");
                Object oldvalue = ph.getProperty(property);
                writeline("/home/ubuntu/results/coverage/Available/Available_10_10.coverage", "efa10a5f-6ea9-4ad4-95f9-28a1bf0d33bd");
                if (null != oldvalue && !oldvalue.equals(value)) {
                    writeline("/home/ubuntu/results/coverage/Available/Available_10_10.coverage", "cd9ae230-0ba1-49e8-aec2-2d4a497e671b");
                    log("DEPRECATED - <available> used to override an existing" + " property." + StringUtils.LINE_SEP + "  Build file should not reuse the same property" + " name for different values.", Project.MSG_WARN);
                }
                writeline("/home/ubuntu/results/coverage/Available/Available_10_10.coverage", "3aab2c28-b10c-42b5-9356-21d4dc7751d6");
                // NB: this makes use of Project#setProperty rather than Project#setNewProperty
                // due to backwards compatibility reasons
                ph.setProperty(property, value, true);
            }
        } finally {
            writeline("/home/ubuntu/results/coverage/Available/Available_10_10.coverage", "8a7cb5af-c51c-4cd0-b5c8-057e11ca98f2");
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
        writeline("/home/ubuntu/results/coverage/Available/Available_10_10.coverage", "8af2f0c5-8fc4-4995-a972-dfc358931d00");
        try {
            writeline("/home/ubuntu/results/coverage/Available/Available_10_10.coverage", "50bcc871-e3d9-40c6-801d-3f8f35448575");
            if (classname == null && file == null && resource == null) {
                writeline("/home/ubuntu/results/coverage/Available/Available_10_10.coverage", "5e1fd397-233e-4c84-9a38-fec1a483a8a5");
                throw new BuildException("At least one of (classname|file|" + "resource) is required", getLocation());
            }
            writeline("/home/ubuntu/results/coverage/Available/Available_10_10.coverage", "55e079b6-08a3-4a0f-8b96-6d01e76fc24c");
            if (type != null) {
                writeline("/home/ubuntu/results/coverage/Available/Available_10_10.coverage", "d4d35268-d5b4-47af-9a52-52ef0ef68a5b");
                if (file == null) {
                    writeline("/home/ubuntu/results/coverage/Available/Available_10_10.coverage", "74fb95e2-93b7-454e-b46e-6f06d2e71ed9");
                    throw new BuildException("The type attribute is only valid " + "when specifying the file " + "attribute.", getLocation());
                }
            }
            writeline("/home/ubuntu/results/coverage/Available/Available_10_10.coverage", "2011685d-4cfe-4aea-bce8-543d683ad08c");
            if (classpath != null) {
                writeline("/home/ubuntu/results/coverage/Available/Available_10_10.coverage", "8556d4c9-27f2-4ddd-9f32-309b2991c56a");
                classpath.setProject(getProject());
                writeline("/home/ubuntu/results/coverage/Available/Available_10_10.coverage", "05b15334-b3fc-4970-a223-522d406322fc");
                this.loader = getProject().createClassLoader(classpath);
            }
            writeline("/home/ubuntu/results/coverage/Available/Available_10_10.coverage", "da807b41-ff12-4bc2-8bb9-64158ad2e03c");
            String appendix = "";
            writeline("/home/ubuntu/results/coverage/Available/Available_10_10.coverage", "b09be133-0817-45fe-b511-7716b0385e11");
            if (isTask) {
                writeline("/home/ubuntu/results/coverage/Available/Available_10_10.coverage", "6d273212-1d02-4be1-b0d2-690da85c4a99");
                appendix = " to set property " + property;
            } else {
                writeline("/home/ubuntu/results/coverage/Available/Available_10_10.coverage", "bf96ee23-3c15-4b20-9dfe-bba651f4c36f");
                setTaskName("available");
            }
            writeline("/home/ubuntu/results/coverage/Available/Available_10_10.coverage", "4cba3a70-3356-4e9d-ac09-72018960a00b");
            if ((classname != null) && !checkClass(classname)) {
                writeline("/home/ubuntu/results/coverage/Available/Available_10_10.coverage", "a2f897f1-0142-4ad6-94e6-e7700df48105");
                log("Unable to load class " + classname + appendix, Project.MSG_VERBOSE);
                writeline("/home/ubuntu/results/coverage/Available/Available_10_10.coverage", "cf871be1-5f09-4146-a978-25c2f855f496");
                return false;
            }
            writeline("/home/ubuntu/results/coverage/Available/Available_10_10.coverage", "e6a51a03-1dca-4ac3-96ab-37bf0a9b043e");
            if ((file != null) && !checkFile()) {
                writeline("/home/ubuntu/results/coverage/Available/Available_10_10.coverage", "197ca7a3-7adc-49d5-9ac6-fee89c1b89eb");
                StringBuffer buf = new StringBuffer("Unable to find ");
                writeline("/home/ubuntu/results/coverage/Available/Available_10_10.coverage", "3de29fd7-b274-4855-87ce-d8d82f5866d3");
                if (type != null) {
                    writeline("/home/ubuntu/results/coverage/Available/Available_10_10.coverage", "8f32dbbc-145b-42dc-920d-7f73d1f7b72f");
                    buf.append(type).append(' ');
                }
                writeline("/home/ubuntu/results/coverage/Available/Available_10_10.coverage", "9d97d29b-083c-49d0-aea1-8daaba9de68c");
                buf.append(filename).append(appendix);
                writeline("/home/ubuntu/results/coverage/Available/Available_10_10.coverage", "2ac2aa09-d465-479c-90ce-9f0bd7e46f08");
                log(buf.toString(), Project.MSG_VERBOSE);
                writeline("/home/ubuntu/results/coverage/Available/Available_10_10.coverage", "85b4eecd-bef7-46fd-9620-05af51e3f7f0");
                return false;
            }
            writeline("/home/ubuntu/results/coverage/Available/Available_10_10.coverage", "7db583d2-5755-4441-9b4c-8f736aefabca");
            if ((resource != null) && !checkResource(resource)) {
                writeline("/home/ubuntu/results/coverage/Available/Available_10_10.coverage", "4713a30d-69cd-45e5-8529-3dbbbbb7780a");
                log("Unable to load resource " + resource + appendix, Project.MSG_VERBOSE);
                writeline("/home/ubuntu/results/coverage/Available/Available_10_10.coverage", "d7c9dcfe-e105-4ccc-b9be-3b341289dfa9");
                return false;
            }
        } finally {
            writeline("/home/ubuntu/results/coverage/Available/Available_10_10.coverage", "c5b8a5fd-28cc-4965-be14-9b677cb00450");
            if (loader != null) {
                writeline("/home/ubuntu/results/coverage/Available/Available_10_10.coverage", "3ba5dad3-4723-4af7-b662-42d804226128");
                loader.cleanup();
                writeline("/home/ubuntu/results/coverage/Available/Available_10_10.coverage", "fe6f4e6d-e8ae-4d21-8507-1fa1c534710e");
                loader = null;
            }
            writeline("/home/ubuntu/results/coverage/Available/Available_10_10.coverage", "06881950-c0da-4f1f-83da-a2b4cff9f66a");
            if (!isTask) {
                writeline("/home/ubuntu/results/coverage/Available/Available_10_10.coverage", "43b47267-c705-4809-920f-58a8e514bc81");
                setTaskName(null);
            }
        }
        writeline("/home/ubuntu/results/coverage/Available/Available_10_10.coverage", "a7ef3a99-a01e-457e-9575-2e5ce9159847");
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
        writeline("/home/ubuntu/results/coverage/Available/Available_10_10.coverage", "8646fbf5-909e-4c68-93e1-0d3422e1342f");
        if (filepath == null) {
            writeline("/home/ubuntu/results/coverage/Available/Available_10_10.coverage", "ab894b4d-96ec-476d-bf5c-7aacdc65fe0a");
            return checkFile(file, filename);
        } else {
            writeline("/home/ubuntu/results/coverage/Available/Available_10_10.coverage", "7205a893-9181-471b-91d2-0968de48f138");
            String[] paths = filepath.list();
            writeline("/home/ubuntu/results/coverage/Available/Available_10_10.coverage", "0cf2e1a4-ef32-4dfa-ad73-05fbc3b3fa5e");
            for (int i = 0; i < paths.length; ++i) {
                writeline("/home/ubuntu/results/coverage/Available/Available_10_10.coverage", "b13fa7b5-b3ac-4ead-b408-1a2781f71276");
                log("Searching " + paths[i], Project.MSG_VERBOSE);
                writeline("/home/ubuntu/results/coverage/Available/Available_10_10.coverage", "4349cfaf-3771-4efa-8746-e4b541033144");
                File path = new File(paths[i]);
                writeline("/home/ubuntu/results/coverage/Available/Available_10_10.coverage", "a31d406f-6695-417c-a7dd-d9e9580a7c96");
                // **   simple name specified   == path in list
                if (path.exists() && (filename.equals(paths[i]) || filename.equals(path.getName()))) {
                    writeline("/home/ubuntu/results/coverage/Available/Available_10_10.coverage", "c4ec1dbe-44bd-4e3a-a7fc-3476e98420dc");
                    if (type == null) {
                        writeline("/home/ubuntu/results/coverage/Available/Available_10_10.coverage", "51faa09a-db8b-48f4-bf19-33d665bd6377");
                        log("Found: " + path, Project.MSG_VERBOSE);
                        writeline("/home/ubuntu/results/coverage/Available/Available_10_10.coverage", "5e10a273-39ec-4459-8822-3034837b5aa8");
                        return true;
                    } else if (type.isDir() && path.isDirectory()) {
                        writeline("/home/ubuntu/results/coverage/Available/Available_10_10.coverage", "da007482-c9b9-4fc4-8499-9b85811befbd");
                        log("Found directory: " + path, Project.MSG_VERBOSE);
                        writeline("/home/ubuntu/results/coverage/Available/Available_10_10.coverage", "590729ef-9c19-44b9-a929-4a0799bab606");
                        return true;
                    } else if (type.isFile() && path.isFile()) {
                        writeline("/home/ubuntu/results/coverage/Available/Available_10_10.coverage", "ece6d8b6-04db-4ec7-88f9-777ed73aa240");
                        log("Found file: " + path, Project.MSG_VERBOSE);
                        writeline("/home/ubuntu/results/coverage/Available/Available_10_10.coverage", "3b4c1420-6941-4441-a32a-0e99f4ceddde");
                        return true;
                    }
                    writeline("/home/ubuntu/results/coverage/Available/Available_10_10.coverage", "9ff9e215-30e5-4bc1-a1aa-8acff2de4ed0");
                    // not the requested type
                    return false;
                }
                writeline("/home/ubuntu/results/coverage/Available/Available_10_10.coverage", "334a71f9-3449-40a4-add7-102ecf5fa62f");
                File parent = path.getParentFile();
                writeline("/home/ubuntu/results/coverage/Available/Available_10_10.coverage", "729860e6-3ae6-4945-bce5-20168aac3d33");
                // **   full-pathname specified == parent dir of path in list
                if (parent != null && parent.exists() && filename.equals(parent.getAbsolutePath())) {
                    writeline("/home/ubuntu/results/coverage/Available/Available_10_10.coverage", "d2d8312b-4b73-4037-aaf0-d3abff0ff541");
                    if (type == null) {
                        writeline("/home/ubuntu/results/coverage/Available/Available_10_10.coverage", "2bdba62c-9d8f-4f03-b819-2397dc168a9a");
                        log("Found: " + parent, Project.MSG_VERBOSE);
                        writeline("/home/ubuntu/results/coverage/Available/Available_10_10.coverage", "7e5158b3-d3b3-4ecb-90cb-d4a49de9b2dd");
                        return true;
                    } else if (type.isDir()) {
                        writeline("/home/ubuntu/results/coverage/Available/Available_10_10.coverage", "29909190-4be5-4978-b4b7-3325f8cff32f");
                        log("Found directory: " + parent, Project.MSG_VERBOSE);
                        writeline("/home/ubuntu/results/coverage/Available/Available_10_10.coverage", "17560610-5473-4b02-956e-e9aacbd18d73");
                        return true;
                    }
                    writeline("/home/ubuntu/results/coverage/Available/Available_10_10.coverage", "94495553-5495-40c8-b1b6-f2cb7c5658cd");
                    // not the requested type
                    return false;
                }
                writeline("/home/ubuntu/results/coverage/Available/Available_10_10.coverage", "8b1db206-ab58-4807-8569-d77acd14ea9d");
                // **   simple name specified   == path in list + name
                if (path.exists() && path.isDirectory()) {
                    writeline("/home/ubuntu/results/coverage/Available/Available_10_10.coverage", "e9ceceba-e7ec-41f7-9253-9d9e70caa642");
                    if (checkFile(new File(path, filename), filename + " in " + path)) {
                        writeline("/home/ubuntu/results/coverage/Available/Available_10_10.coverage", "f911534a-5dda-41db-9e78-64d106d2551b");
                        return true;
                    }
                }
                writeline("/home/ubuntu/results/coverage/Available/Available_10_10.coverage", "1c441cf7-97a7-4802-bbd6-ee36c448bf61");
                // **   simple name specified   == parent dir + name
                while (searchParents && parent != null && parent.exists()) {
                    writeline("/home/ubuntu/results/coverage/Available/Available_10_10.coverage", "ea82e3e3-9e13-4172-835d-98729fe19c6f");
                    if (checkFile(new File(parent, filename), filename + " in " + parent)) {
                        writeline("/home/ubuntu/results/coverage/Available/Available_10_10.coverage", "3bdd91de-eacf-40f0-982e-febfda8aef8d");
                        return true;
                    }
                    writeline("/home/ubuntu/results/coverage/Available/Available_10_10.coverage", "59c86ff8-06ac-42f9-bdfe-999a45a90a17");
                    parent = parent.getParentFile();
                }
            }
        }
        writeline("/home/ubuntu/results/coverage/Available/Available_10_10.coverage", "9157ce79-e606-4bcd-8267-c129fd9948e3");
        return false;
    }

    /**
     * Check if a given file exists and matches the required type.
     */
    private boolean checkFile(File f, String text) {
        writeline("/home/ubuntu/results/coverage/Available/Available_10_10.coverage", "b38c34ae-113e-45f7-9c7c-39a6186bbc73");
        if (type != null) {
            writeline("/home/ubuntu/results/coverage/Available/Available_10_10.coverage", "77a2a857-6f96-4c14-a01b-bbc1adf3b719");
            if (type.isDir()) {
                writeline("/home/ubuntu/results/coverage/Available/Available_10_10.coverage", "6cb5f6e3-a6cd-48dd-ae42-5a747165db5c");
                if (f.isDirectory()) {
                    writeline("/home/ubuntu/results/coverage/Available/Available_10_10.coverage", "436ebe91-5cbb-48c9-8447-94677c982240");
                    log("Found directory: " + text, Project.MSG_VERBOSE);
                }
                writeline("/home/ubuntu/results/coverage/Available/Available_10_10.coverage", "93b5501f-2cba-47f0-9aff-2755416e8899");
                return f.isDirectory();
            } else if (type.isFile()) {
                writeline("/home/ubuntu/results/coverage/Available/Available_10_10.coverage", "eb5a1719-3727-48f9-9ae4-9da90eb03190");
                if (f.isFile()) {
                    writeline("/home/ubuntu/results/coverage/Available/Available_10_10.coverage", "397d94ad-d0f7-4f33-9c85-393c8b3e6571");
                    log("Found file: " + text, Project.MSG_VERBOSE);
                }
                writeline("/home/ubuntu/results/coverage/Available/Available_10_10.coverage", "84d770ac-8ecb-450c-ab03-373915d094c1");
                return f.isFile();
            }
        }
        writeline("/home/ubuntu/results/coverage/Available/Available_10_10.coverage", "7be6103c-3dca-4536-9aee-876b8b6a1521");
        if (f.exists()) {
            writeline("/home/ubuntu/results/coverage/Available/Available_10_10.coverage", "2cd61a4a-8912-4dbf-afd1-274627d64eba");
            log("Found: " + text, Project.MSG_VERBOSE);
        }
        writeline("/home/ubuntu/results/coverage/Available/Available_10_10.coverage", "a4b74c21-b8cf-488e-b1c7-db6cbc4e7dc7");
        return f.exists();
    }

    /**
     * Check if a given resource can be loaded.
     */
    private boolean checkResource(String resource) {
        writeline("/home/ubuntu/results/coverage/Available/Available_10_10.coverage", "8b82d501-334c-4501-bb81-f258d87db5ef");
        InputStream is = null;
        writeline("/home/ubuntu/results/coverage/Available/Available_10_10.coverage", "f08296f3-79b8-45e5-8b90-cde0acd54b5c");
        try {
            writeline("/home/ubuntu/results/coverage/Available/Available_10_10.coverage", "ab087b4c-7107-4ca3-827d-91fd3a7fa6ce");
            if (loader != null) {
                writeline("/home/ubuntu/results/coverage/Available/Available_10_10.coverage", "f36026fe-6257-4dcb-a641-98e610018d62");
                is = loader.getResourceAsStream(resource);
            } else {
                writeline("/home/ubuntu/results/coverage/Available/Available_10_10.coverage", "ca57abbd-83be-42f4-9ee5-40d59f54c2c4");
                ClassLoader cL = this.getClass().getClassLoader();
                writeline("/home/ubuntu/results/coverage/Available/Available_10_10.coverage", "8694bf00-be39-4356-8134-55f638b82f3f");
                if (cL != null) {
                    writeline("/home/ubuntu/results/coverage/Available/Available_10_10.coverage", "f46cdbd5-ce8d-48dd-bf4f-afe4d9b678e7");
                    is = cL.getResourceAsStream(resource);
                } else {
                    writeline("/home/ubuntu/results/coverage/Available/Available_10_10.coverage", "9df4dbc6-a571-4613-aec4-01b455b82ab7");
                    is = ClassLoader.getSystemResourceAsStream(resource);
                }
            }
            writeline("/home/ubuntu/results/coverage/Available/Available_10_10.coverage", "df57fdce-3089-41ef-bf49-69b9fb73f3cd");
            return is != null;
        } finally {
            writeline("/home/ubuntu/results/coverage/Available/Available_10_10.coverage", "1bae6755-93f7-4002-8cba-099b4a6e575e");
            FileUtils.close(is);
        }
    }

    /**
     * Check if a given class can be loaded.
     */
    private boolean checkClass(String classname) {
        writeline("/home/ubuntu/results/coverage/Available/Available_10_10.coverage", "7744e270-7c40-46e4-b5e1-13b96401027b");
        try {
            writeline("/home/ubuntu/results/coverage/Available/Available_10_10.coverage", "a6f842f2-0907-40c9-b349-e72f303eed33");
            if (ignoreSystemclasses) {
                writeline("/home/ubuntu/results/coverage/Available/Available_10_10.coverage", "937fed7c-a27e-4ba3-9dc9-2e91cfe3181a");
                loader = getProject().createClassLoader(classpath);
                writeline("/home/ubuntu/results/coverage/Available/Available_10_10.coverage", "b06fc4c1-a52e-4cec-af5e-e6d84a7d0f05");
                loader.setParentFirst(false);
                writeline("/home/ubuntu/results/coverage/Available/Available_10_10.coverage", "3868d0c3-2f2d-49e7-8fa0-d544368f96c9");
                loader.addJavaLibraries();
                writeline("/home/ubuntu/results/coverage/Available/Available_10_10.coverage", "533b0ec1-e9f0-49a3-a8a4-8c44ce332bfb");
                try {
                    writeline("/home/ubuntu/results/coverage/Available/Available_10_10.coverage", "02b8bc77-a81e-40d9-917c-045790d638c3");
                    loader.findClass(classname);
                } catch (SecurityException se) {
                    writeline("/home/ubuntu/results/coverage/Available/Available_10_10.coverage", "ab5799ba-673a-4004-8671-e2a613d48cb2");
                    // so catch the exception and return
                    return true;
                }
            } else if (loader != null) {
                writeline("/home/ubuntu/results/coverage/Available/Available_10_10.coverage", "0ac4670b-52af-415b-8bd0-de20d77ed6ac");
                loader.loadClass(classname);
            } else {
                writeline("/home/ubuntu/results/coverage/Available/Available_10_10.coverage", "6f35de8a-c5e0-4d5b-9993-a3cc59da27aa");
                ClassLoader l = this.getClass().getClassLoader();
                writeline("/home/ubuntu/results/coverage/Available/Available_10_10.coverage", "7f3a24e3-9472-4cf8-b80d-93d16fa4f456");
                // see API docs of Class.getClassLoader.
                if (l != null) {
                    writeline("/home/ubuntu/results/coverage/Available/Available_10_10.coverage", "0a4f73fe-08a9-4690-9951-241c0ce430eb");
                    Class.forName(classname, true, l);
                } else {
                    writeline("/home/ubuntu/results/coverage/Available/Available_10_10.coverage", "7af93425-142f-4f85-8a0b-6b7fe6628348");
                    Class.forName(classname);
                }
            }
            writeline("/home/ubuntu/results/coverage/Available/Available_10_10.coverage", "68cd52f4-cbea-481f-b8fb-6e15b0fd3eba");
            return true;
        } catch (ClassNotFoundException e) {
            writeline("/home/ubuntu/results/coverage/Available/Available_10_10.coverage", "91218867-b097-4f5f-8df6-729b0edf3e35");
            log("class \"" + classname + "\" was not found", Project.MSG_DEBUG);
            writeline("/home/ubuntu/results/coverage/Available/Available_10_10.coverage", "d3b4cf6b-e05c-4fe0-83b2-3eca5e2e7a91");
            return false;
        } catch (NoClassDefFoundError e) {
            writeline("/home/ubuntu/results/coverage/Available/Available_10_10.coverage", "b657465b-d2b0-49e5-b880-4f605d1bd83d");
            log("Could not load dependent class \"" + e.getMessage() + "\" for class \"" + classname + "\"", Project.MSG_DEBUG);
            writeline("/home/ubuntu/results/coverage/Available/Available_10_10.coverage", "bbf2db0b-78b9-44d1-93bd-5f11838ae307");
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
