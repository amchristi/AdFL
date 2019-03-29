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
        writeline("/home/ubuntu/results/coverage/Available/Available_9_10.coverage", "a320879a-ca65-424b-8de8-7d34972153f9");
        this.searchParents = searchParents;
    }

    /**
     * Set the classpath to be used when searching for classes and resources.
     *
     * @param classpath an Ant Path object containing the search path.
     */
    public void setClasspath(Path classpath) {
        writeline("/home/ubuntu/results/coverage/Available/Available_9_10.coverage", "24ce5f9d-6e62-4627-858f-f1570e7f3e0d");
        createClasspath().append(classpath);
    }

    /**
     * Classpath to be used when searching for classes and resources.
     *
     * @return an empty Path instance to be configured by Ant.
     */
    public Path createClasspath() {
        writeline("/home/ubuntu/results/coverage/Available/Available_9_10.coverage", "a7556ca6-1a54-4e95-8de2-d1f6586b5076");
        if (this.classpath == null) {
            writeline("/home/ubuntu/results/coverage/Available/Available_9_10.coverage", "4c17e4cc-e8a8-40d4-9b7c-480e9bb75b58");
            this.classpath = new Path(getProject());
        }
        writeline("/home/ubuntu/results/coverage/Available/Available_9_10.coverage", "5841f086-db88-4faf-9957-a4d650719b0b");
        return this.classpath.createPath();
    }

    /**
     * Set the classpath by reference.
     *
     * @param r a Reference to a Path instance to be used as the classpath
     * value.
     */
    public void setClasspathRef(Reference r) {
        writeline("/home/ubuntu/results/coverage/Available/Available_9_10.coverage", "7438efdb-5694-4fb0-a298-76e15a4e97ed");
        createClasspath().setRefid(r);
    }

    /**
     * Set the path to use when looking for a file.
     *
     * @param filepath a Path instance containing the search path for files.
     */
    public void setFilepath(Path filepath) {
        writeline("/home/ubuntu/results/coverage/Available/Available_9_10.coverage", "a9ff8f24-d119-4ee7-aca6-aa4f1581be3a");
        createFilepath().append(filepath);
    }

    /**
     * Path to search for file resources.
     *
     * @return a new Path instance which Ant will configure with a file search
     * path.
     */
    public Path createFilepath() {
        writeline("/home/ubuntu/results/coverage/Available/Available_9_10.coverage", "d759c81f-94bb-42f6-ae41-f057821871fd");
        if (this.filepath == null) {
            writeline("/home/ubuntu/results/coverage/Available/Available_9_10.coverage", "6d47539e-0e6d-4c34-b6a7-1ffb15787a1b");
            this.filepath = new Path(getProject());
        }
        writeline("/home/ubuntu/results/coverage/Available/Available_9_10.coverage", "089a3e68-62b2-45da-9b0d-dc6efec55b75");
        return this.filepath.createPath();
    }

    /**
     * Set the name of the property which will be set if the particular resource
     * is available.
     *
     * @param property the name of the property to set.
     */
    public void setProperty(String property) {
        writeline("/home/ubuntu/results/coverage/Available/Available_9_10.coverage", "f5c75559-6978-422e-9475-6d5e6210faa5");
        this.property = property;
    }

    /**
     * Set the value to be given to the property if the desired resource is
     * available.
     *
     * @param value the value to be given.
     */
    public void setValue(Object value) {
        writeline("/home/ubuntu/results/coverage/Available/Available_9_10.coverage", "353c6389-304b-4660-9279-5d5e16d775b5");
        this.value = value;
    }

    /**
     * Set the value to be given to the property if the desired resource is
     * available.
     *
     * @param value the value to be given.
     */
    public void setValue(String value) {
        writeline("/home/ubuntu/results/coverage/Available/Available_9_10.coverage", "fffc73a0-e73d-444a-877e-ef9d01679d45");
        setValue((Object) value);
    }

    /**
     * Set a classname of a class which must be available to set the given
     * property.
     *
     * @param classname the name of the class required.
     */
    public void setClassname(String classname) {
        writeline("/home/ubuntu/results/coverage/Available/Available_9_10.coverage", "f3b39bb3-10ad-43ec-85a5-8b24eb38fa1f");
        if (!"".equals(classname)) {
            writeline("/home/ubuntu/results/coverage/Available/Available_9_10.coverage", "20303d9d-2da2-4f06-93cc-d183250c90cb");
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
        writeline("/home/ubuntu/results/coverage/Available/Available_9_10.coverage", "6a867c80-a8ae-497d-807b-fda8e5033a15");
        this.file = file;
        writeline("/home/ubuntu/results/coverage/Available/Available_9_10.coverage", "17ac219e-efa7-437b-a76c-4fa366a1a94f");
        this.filename = FILE_UTILS.removeLeadingPath(getProject().getBaseDir(), file);
    }

    /**
     * Set the name of a Java resource which is required to set the property.
     *
     * @param resource the name of a resource which is required to be available.
     */
    public void setResource(String resource) {
        writeline("/home/ubuntu/results/coverage/Available/Available_9_10.coverage", "62002d51-9056-4a0f-aaff-783ceac22639");
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
        writeline("/home/ubuntu/results/coverage/Available/Available_9_10.coverage", "96687e34-6b36-459f-9b0d-e81b331e2a0a");
        log("DEPRECATED - The setType(String) method has been deprecated." + " Use setType(Available.FileDir) instead.", Project.MSG_WARN);
        writeline("/home/ubuntu/results/coverage/Available/Available_9_10.coverage", "a0a80f34-c13d-4123-b563-ee0f8e6ec409");
        this.type = new FileDir();
        writeline("/home/ubuntu/results/coverage/Available/Available_9_10.coverage", "664bcbad-a9d7-47ac-8e06-e91ab7774685");
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
        writeline("/home/ubuntu/results/coverage/Available/Available_9_10.coverage", "b2420e10-5bd2-423e-843a-e41cc5010cc4");
        this.type = type;
    }

    /**
     * Set whether the search for classes should ignore the runtime classes and
     * just use the given classpath.
     *
     * @param ignore true if system classes are to be ignored.
     */
    public void setIgnoresystemclasses(boolean ignore) {
        writeline("/home/ubuntu/results/coverage/Available/Available_9_10.coverage", "cb7828a8-4b5f-4c6b-b3c8-87ef1497a411");
        this.ignoreSystemclasses = ignore;
    }

    /**
     * Entry point when operating as a task.
     *
     * @exception BuildException if the task is not configured correctly.
     */
    public void execute() throws BuildException {
        writeline("/home/ubuntu/results/coverage/Available/Available_9_10.coverage", "307281d3-806e-4afe-84eb-704ed2deb571");
        if (property == null) {
            writeline("/home/ubuntu/results/coverage/Available/Available_9_10.coverage", "795aebd2-b59c-4406-93ad-69fc5a101776");
            throw new BuildException("property attribute is required", getLocation());
        }
        writeline("/home/ubuntu/results/coverage/Available/Available_9_10.coverage", "6c55f960-9e57-4b47-96d0-51850076fbff");
        isTask = true;
        writeline("/home/ubuntu/results/coverage/Available/Available_9_10.coverage", "9605624c-f9f2-4cc5-b98b-c6f7f688b685");
        try {
            writeline("/home/ubuntu/results/coverage/Available/Available_9_10.coverage", "4139073b-a5bb-4ad0-ae8c-764cc0a2eddd");
            if (eval()) {
                writeline("/home/ubuntu/results/coverage/Available/Available_9_10.coverage", "6cba8f1b-51ab-48f5-aee8-42a9c99e2eb2");
                PropertyHelper ph = PropertyHelper.getPropertyHelper(getProject());
                writeline("/home/ubuntu/results/coverage/Available/Available_9_10.coverage", "4355fc45-1261-41c0-9a0d-1c823f908d04");
                Object oldvalue = ph.getProperty(property);
                writeline("/home/ubuntu/results/coverage/Available/Available_9_10.coverage", "cd5246b0-523c-4b6b-a09e-2c671e07b31e");
                if (null != oldvalue && !oldvalue.equals(value)) {
                    writeline("/home/ubuntu/results/coverage/Available/Available_9_10.coverage", "4d1b4a2d-89a7-4525-9c74-cad52f729b8e");
                    log("DEPRECATED - <available> used to override an existing" + " property." + StringUtils.LINE_SEP + "  Build file should not reuse the same property" + " name for different values.", Project.MSG_WARN);
                }
                writeline("/home/ubuntu/results/coverage/Available/Available_9_10.coverage", "8467695c-3ae0-40c6-b3b7-36025329758f");
                // NB: this makes use of Project#setProperty rather than Project#setNewProperty
                // due to backwards compatibility reasons
                ph.setProperty(property, value, true);
            }
        } finally {
            writeline("/home/ubuntu/results/coverage/Available/Available_9_10.coverage", "909439a4-f63c-444e-a5a3-7ae1b50ae84b");
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
        writeline("/home/ubuntu/results/coverage/Available/Available_9_10.coverage", "f32473c7-cfd4-4876-b933-22c37f873b9d");
        try {
            writeline("/home/ubuntu/results/coverage/Available/Available_9_10.coverage", "2716af96-528d-43d7-a22e-82f15f83d40f");
            if (classname == null && file == null && resource == null) {
                writeline("/home/ubuntu/results/coverage/Available/Available_9_10.coverage", "057b17bf-5f73-4ea3-8a78-2b3f3fbd5b80");
                throw new BuildException("At least one of (classname|file|" + "resource) is required", getLocation());
            }
            writeline("/home/ubuntu/results/coverage/Available/Available_9_10.coverage", "b25179bb-c956-4a6a-ac6f-34e6a55499fb");
            if (type != null) {
                writeline("/home/ubuntu/results/coverage/Available/Available_9_10.coverage", "f457e270-745c-4d88-b91f-63dfca29b364");
                if (file == null) {
                    writeline("/home/ubuntu/results/coverage/Available/Available_9_10.coverage", "ee19edc8-126b-4ceb-afc4-387d780d4d18");
                    throw new BuildException("The type attribute is only valid " + "when specifying the file " + "attribute.", getLocation());
                }
            }
            writeline("/home/ubuntu/results/coverage/Available/Available_9_10.coverage", "0d69d6d1-3f13-4192-9fd1-1ad187158703");
            if (classpath != null) {
                writeline("/home/ubuntu/results/coverage/Available/Available_9_10.coverage", "9285758d-87d6-40cb-be75-87d0926bc1cb");
                classpath.setProject(getProject());
                writeline("/home/ubuntu/results/coverage/Available/Available_9_10.coverage", "7c492a84-4d34-48a5-b0c7-12a8ec9e673c");
                this.loader = getProject().createClassLoader(classpath);
            }
            writeline("/home/ubuntu/results/coverage/Available/Available_9_10.coverage", "17b81dd0-7360-4c51-bc39-cf13a57b1210");
            String appendix = "";
            writeline("/home/ubuntu/results/coverage/Available/Available_9_10.coverage", "7d5faa83-bd05-4487-a6af-5adf3aff5642");
            if (isTask) {
                writeline("/home/ubuntu/results/coverage/Available/Available_9_10.coverage", "0634a7c4-000c-42d5-8841-2bb9788362d3");
                appendix = " to set property " + property;
            } else {
                writeline("/home/ubuntu/results/coverage/Available/Available_9_10.coverage", "c6d6ac81-8b10-47cf-a278-42815ba3643c");
                setTaskName("available");
            }
            writeline("/home/ubuntu/results/coverage/Available/Available_9_10.coverage", "2c583ed9-cd0d-4fae-9601-fc46cfb27780");
            if ((classname != null) && !checkClass(classname)) {
                writeline("/home/ubuntu/results/coverage/Available/Available_9_10.coverage", "a11ad8f0-93aa-4d8c-8ff1-f032ea17964e");
                log("Unable to load class " + classname + appendix, Project.MSG_VERBOSE);
                writeline("/home/ubuntu/results/coverage/Available/Available_9_10.coverage", "b083f823-fddf-460e-8cc7-09e470c085a5");
                return false;
            }
            writeline("/home/ubuntu/results/coverage/Available/Available_9_10.coverage", "80d1da61-e5a5-41bd-af88-290a396a18fb");
            if ((file != null) && !checkFile()) {
                writeline("/home/ubuntu/results/coverage/Available/Available_9_10.coverage", "3f7adebd-7df8-4ecc-99a2-64d74c7f81f9");
                StringBuffer buf = new StringBuffer("Unable to find ");
                writeline("/home/ubuntu/results/coverage/Available/Available_9_10.coverage", "e21d6b2a-cb45-4439-9eee-766391a914eb");
                if (type != null) {
                    writeline("/home/ubuntu/results/coverage/Available/Available_9_10.coverage", "ac5d3e1a-1ef1-4570-bf7b-8914bdfb6163");
                    buf.append(type).append(' ');
                }
                writeline("/home/ubuntu/results/coverage/Available/Available_9_10.coverage", "14e92d04-fb69-42a3-923a-1ed83734ce57");
                buf.append(filename).append(appendix);
                writeline("/home/ubuntu/results/coverage/Available/Available_9_10.coverage", "a2c56f43-ee8a-40b2-a1bd-9f8efb6a6976");
                log(buf.toString(), Project.MSG_VERBOSE);
                writeline("/home/ubuntu/results/coverage/Available/Available_9_10.coverage", "63984bc1-60fc-4111-af1f-37f18b3dc98a");
                return false;
            }
            writeline("/home/ubuntu/results/coverage/Available/Available_9_10.coverage", "729cac64-c3bb-4af6-95ea-01dc64ee991b");
            if ((resource != null) && !checkResource(resource)) {
                writeline("/home/ubuntu/results/coverage/Available/Available_9_10.coverage", "75e78d63-9fbb-4642-b74c-86ae3bd6e1eb");
                log("Unable to load resource " + resource + appendix, Project.MSG_VERBOSE);
                writeline("/home/ubuntu/results/coverage/Available/Available_9_10.coverage", "f2582abc-3d33-4fe3-9cdd-fa97e264d90b");
                return false;
            }
        } finally {
            writeline("/home/ubuntu/results/coverage/Available/Available_9_10.coverage", "d913af0a-9212-4d48-9528-6a4831f1aee5");
            if (loader != null) {
                writeline("/home/ubuntu/results/coverage/Available/Available_9_10.coverage", "4f9d944d-d49d-4227-98c0-54230b306652");
                loader.cleanup();
                writeline("/home/ubuntu/results/coverage/Available/Available_9_10.coverage", "a3d64e09-c447-4300-a666-902e540662c7");
                loader = null;
            }
            writeline("/home/ubuntu/results/coverage/Available/Available_9_10.coverage", "8b48d465-b3e5-4e48-91cf-cc148f8e2fe9");
            if (!isTask) {
                writeline("/home/ubuntu/results/coverage/Available/Available_9_10.coverage", "b409b46b-8534-4363-bfe1-ad1c3fb99269");
                setTaskName(null);
            }
        }
        writeline("/home/ubuntu/results/coverage/Available/Available_9_10.coverage", "49f61f44-17e8-4fb0-9396-5559310d62b3");
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
        writeline("/home/ubuntu/results/coverage/Available/Available_9_10.coverage", "0694df17-28c5-4a06-9557-acfdeb45a089");
        if (filepath == null) {
            writeline("/home/ubuntu/results/coverage/Available/Available_9_10.coverage", "3e459e7a-fe5c-4809-ad49-0181be37b19d");
            return checkFile(file, filename);
        } else {
            writeline("/home/ubuntu/results/coverage/Available/Available_9_10.coverage", "020ac28f-1fb8-43fb-a4d3-ae6caa620e74");
            String[] paths = filepath.list();
            writeline("/home/ubuntu/results/coverage/Available/Available_9_10.coverage", "2e96bd26-a84b-476c-a74c-9335616adb52");
            for (int i = 0; i < paths.length; ++i) {
                writeline("/home/ubuntu/results/coverage/Available/Available_9_10.coverage", "a8d687a0-48ec-4779-8914-25d28c0bf38a");
                log("Searching " + paths[i], Project.MSG_VERBOSE);
                writeline("/home/ubuntu/results/coverage/Available/Available_9_10.coverage", "c7be85a7-8f95-4b71-b83c-5ea77d8a17f7");
                File path = new File(paths[i]);
                writeline("/home/ubuntu/results/coverage/Available/Available_9_10.coverage", "81b15751-f2ba-46ef-a269-da7cf99ad44f");
                // **   simple name specified   == path in list
                if (path.exists() && (filename.equals(paths[i]) || filename.equals(path.getName()))) {
                    writeline("/home/ubuntu/results/coverage/Available/Available_9_10.coverage", "9f2c12f7-4047-46c7-8bc3-8fd89ba30baa");
                    if (type == null) {
                        writeline("/home/ubuntu/results/coverage/Available/Available_9_10.coverage", "e56776fe-890c-4a3f-817a-80d38ca89bbd");
                        log("Found: " + path, Project.MSG_VERBOSE);
                        writeline("/home/ubuntu/results/coverage/Available/Available_9_10.coverage", "2ad44d2d-7d08-469a-8400-13ac541faed4");
                        return true;
                    } else if (type.isDir() && path.isDirectory()) {
                        writeline("/home/ubuntu/results/coverage/Available/Available_9_10.coverage", "8d3dde55-1540-4bd5-a353-145188ff01c2");
                        log("Found directory: " + path, Project.MSG_VERBOSE);
                        writeline("/home/ubuntu/results/coverage/Available/Available_9_10.coverage", "142115e5-3473-496e-a74a-266ae39c8406");
                        return true;
                    } else if (type.isFile() && path.isFile()) {
                        writeline("/home/ubuntu/results/coverage/Available/Available_9_10.coverage", "2a0e1b26-af7d-4777-96fd-bb0efdb19194");
                        log("Found file: " + path, Project.MSG_VERBOSE);
                        writeline("/home/ubuntu/results/coverage/Available/Available_9_10.coverage", "1bfecd06-2e36-4abf-bd53-d34b3b6a6f42");
                        return true;
                    }
                    writeline("/home/ubuntu/results/coverage/Available/Available_9_10.coverage", "90bc587a-7d16-481a-8139-bbf4c4ea6bea");
                    // not the requested type
                    return false;
                }
                writeline("/home/ubuntu/results/coverage/Available/Available_9_10.coverage", "b3ca20ad-6166-4b2b-b769-ba4d0d6bd319");
                File parent = path.getParentFile();
                writeline("/home/ubuntu/results/coverage/Available/Available_9_10.coverage", "3ddefd28-db4e-47b4-87ed-821e5bf94270");
                // **   full-pathname specified == parent dir of path in list
                if (parent != null && parent.exists() && filename.equals(parent.getAbsolutePath())) {
                    writeline("/home/ubuntu/results/coverage/Available/Available_9_10.coverage", "ec867187-8944-46c2-9a5e-69f2e20475a0");
                    if (type == null) {
                        writeline("/home/ubuntu/results/coverage/Available/Available_9_10.coverage", "5bb9129d-1fc2-4ed2-b216-8243877ec9a1");
                        log("Found: " + parent, Project.MSG_VERBOSE);
                        writeline("/home/ubuntu/results/coverage/Available/Available_9_10.coverage", "93e0d7a8-29ef-4317-8d7d-debef4348851");
                        return true;
                    } else if (type.isDir()) {
                        writeline("/home/ubuntu/results/coverage/Available/Available_9_10.coverage", "44dfbdea-25d3-4818-b88d-1c87886aed62");
                        log("Found directory: " + parent, Project.MSG_VERBOSE);
                        writeline("/home/ubuntu/results/coverage/Available/Available_9_10.coverage", "e4c36712-dc6b-4b6c-9506-22a5adb62a6c");
                        return true;
                    }
                    writeline("/home/ubuntu/results/coverage/Available/Available_9_10.coverage", "acbbb182-b2ee-4e95-8e2a-3e6b21909813");
                    // not the requested type
                    return false;
                }
                writeline("/home/ubuntu/results/coverage/Available/Available_9_10.coverage", "6780d0fe-27d2-4233-b7e4-b25401c5308b");
                // **   simple name specified   == path in list + name
                if (path.exists() && path.isDirectory()) {
                    writeline("/home/ubuntu/results/coverage/Available/Available_9_10.coverage", "9088ae1e-c753-41e6-9bef-98d2b253d917");
                    if (checkFile(new File(path, filename), filename + " in " + path)) {
                        writeline("/home/ubuntu/results/coverage/Available/Available_9_10.coverage", "b98784a5-1b61-4396-94e9-85d234628d02");
                        return true;
                    }
                }
                writeline("/home/ubuntu/results/coverage/Available/Available_9_10.coverage", "1b4fc918-247e-4488-87bb-61f36c308bbd");
                // **   simple name specified   == parent dir + name
                while (searchParents && parent != null && parent.exists()) {
                    writeline("/home/ubuntu/results/coverage/Available/Available_9_10.coverage", "fb4ff890-b529-44af-9580-80b0ac357f02");
                    if (checkFile(new File(parent, filename), filename + " in " + parent)) {
                        writeline("/home/ubuntu/results/coverage/Available/Available_9_10.coverage", "65679992-ca07-4731-a2c0-727adceee06d");
                        return true;
                    }
                    writeline("/home/ubuntu/results/coverage/Available/Available_9_10.coverage", "aea31b2d-6314-4552-9182-6bdde5068b22");
                    parent = parent.getParentFile();
                }
            }
        }
        writeline("/home/ubuntu/results/coverage/Available/Available_9_10.coverage", "2b676253-386d-4611-a134-321783dcfc90");
        return false;
    }

    /**
     * Check if a given file exists and matches the required type.
     */
    private boolean checkFile(File f, String text) {
        writeline("/home/ubuntu/results/coverage/Available/Available_9_10.coverage", "cf807710-ded6-43a8-a3e3-c92fe143babe");
        if (type != null) {
            writeline("/home/ubuntu/results/coverage/Available/Available_9_10.coverage", "35a6d823-a36a-4a73-b5f6-2c30e60dbba4");
            if (type.isDir()) {
                writeline("/home/ubuntu/results/coverage/Available/Available_9_10.coverage", "187870d6-0afc-46b5-838a-c225b02636d7");
                if (f.isDirectory()) {
                    writeline("/home/ubuntu/results/coverage/Available/Available_9_10.coverage", "bb3162ad-7b1e-4fb4-9f8d-8daacc6d6333");
                    log("Found directory: " + text, Project.MSG_VERBOSE);
                }
                writeline("/home/ubuntu/results/coverage/Available/Available_9_10.coverage", "4eec25f2-26bc-442c-9f4d-efab24ae13c2");
                return f.isDirectory();
            } else if (type.isFile()) {
                writeline("/home/ubuntu/results/coverage/Available/Available_9_10.coverage", "57f97269-9a0a-43b6-bc03-c64ddf2c603e");
                if (f.isFile()) {
                    writeline("/home/ubuntu/results/coverage/Available/Available_9_10.coverage", "3de03250-ea25-4f0f-8a2d-77ed17208a8a");
                    log("Found file: " + text, Project.MSG_VERBOSE);
                }
                writeline("/home/ubuntu/results/coverage/Available/Available_9_10.coverage", "851cec95-6948-4b17-9deb-26e6ed80e3c6");
                return f.isFile();
            }
        }
        writeline("/home/ubuntu/results/coverage/Available/Available_9_10.coverage", "bd155867-31ac-465e-8367-42f2aae72aa9");
        if (f.exists()) {
            writeline("/home/ubuntu/results/coverage/Available/Available_9_10.coverage", "c4933adc-f0f3-461c-af57-721d167df44e");
            log("Found: " + text, Project.MSG_VERBOSE);
        }
        writeline("/home/ubuntu/results/coverage/Available/Available_9_10.coverage", "3cce8547-c402-4cc5-8164-df8e0733ce42");
        return f.exists();
    }

    /**
     * Check if a given resource can be loaded.
     */
    private boolean checkResource(String resource) {
        writeline("/home/ubuntu/results/coverage/Available/Available_9_10.coverage", "04e3e847-4110-46a2-bbde-723acb906022");
        InputStream is = null;
        writeline("/home/ubuntu/results/coverage/Available/Available_9_10.coverage", "b16e2b8f-5da7-43f8-a988-8db35c0f0239");
        try {
            writeline("/home/ubuntu/results/coverage/Available/Available_9_10.coverage", "2a511dd2-e8a8-448d-8385-01ab344c1254");
            if (loader != null) {
                writeline("/home/ubuntu/results/coverage/Available/Available_9_10.coverage", "0a10182a-9fbd-4c1b-b258-55f488c8354b");
                is = loader.getResourceAsStream(resource);
            } else {
                writeline("/home/ubuntu/results/coverage/Available/Available_9_10.coverage", "0daafde0-e031-4747-aebd-0d0100205348");
                ClassLoader cL = this.getClass().getClassLoader();
                writeline("/home/ubuntu/results/coverage/Available/Available_9_10.coverage", "b892bce2-aed3-4c60-aec5-fa80e9aebe7e");
                if (cL != null) {
                    writeline("/home/ubuntu/results/coverage/Available/Available_9_10.coverage", "d71ef749-6c83-4b73-819c-da7e6d959596");
                    is = cL.getResourceAsStream(resource);
                } else {
                    writeline("/home/ubuntu/results/coverage/Available/Available_9_10.coverage", "9aedb056-8796-4ec2-a8f1-37018f3f30a1");
                    is = ClassLoader.getSystemResourceAsStream(resource);
                }
            }
            writeline("/home/ubuntu/results/coverage/Available/Available_9_10.coverage", "ba877e0e-9200-4c22-b107-662b3b4436b4");
            return is != null;
        } finally {
            writeline("/home/ubuntu/results/coverage/Available/Available_9_10.coverage", "f63a3785-1243-411c-a0ab-e801afdc9ef3");
            FileUtils.close(is);
        }
    }

    /**
     * Check if a given class can be loaded.
     */
    private boolean checkClass(String classname) {
        writeline("/home/ubuntu/results/coverage/Available/Available_9_10.coverage", "c77f4e14-b75f-43f2-b44c-384f22480adf");
        try {
            writeline("/home/ubuntu/results/coverage/Available/Available_9_10.coverage", "5e5daf57-48f7-4ca3-9a34-f290e138da29");
            if (ignoreSystemclasses) {
                writeline("/home/ubuntu/results/coverage/Available/Available_9_10.coverage", "28410aa5-c714-4a46-b4cf-fc41db76bf5b");
                loader = getProject().createClassLoader(classpath);
                writeline("/home/ubuntu/results/coverage/Available/Available_9_10.coverage", "7c53855e-b2a6-4d29-9944-5842655b7820");
                loader.setParentFirst(false);
                writeline("/home/ubuntu/results/coverage/Available/Available_9_10.coverage", "9ee534ee-1057-4bae-a576-e6ba354c5e33");
                loader.addJavaLibraries();
                writeline("/home/ubuntu/results/coverage/Available/Available_9_10.coverage", "0f5980e9-649c-48c1-b5ca-7e33ecbfa1eb");
                try {
                    writeline("/home/ubuntu/results/coverage/Available/Available_9_10.coverage", "73b1d5df-8933-47ab-9a6b-95287c5f8b0a");
                    loader.findClass(classname);
                } catch (SecurityException se) {
                    writeline("/home/ubuntu/results/coverage/Available/Available_9_10.coverage", "d66d2c19-71b0-4608-8ef2-ea9bd94acfea");
                    // so catch the exception and return
                    return true;
                }
            } else if (loader != null) {
                writeline("/home/ubuntu/results/coverage/Available/Available_9_10.coverage", "64190494-f8d6-4cc1-a085-967b41ec5028");
                loader.loadClass(classname);
            } else {
                writeline("/home/ubuntu/results/coverage/Available/Available_9_10.coverage", "4fcf4d77-85a4-44fb-b8bf-f2ca8eda5c71");
                ClassLoader l = this.getClass().getClassLoader();
                writeline("/home/ubuntu/results/coverage/Available/Available_9_10.coverage", "a8b56bf8-681e-4650-937f-89bc000e63f8");
                // see API docs of Class.getClassLoader.
                if (l != null) {
                    writeline("/home/ubuntu/results/coverage/Available/Available_9_10.coverage", "14931c20-72fd-4720-a129-a06329fad292");
                    Class.forName(classname, true, l);
                } else {
                    writeline("/home/ubuntu/results/coverage/Available/Available_9_10.coverage", "c6727f78-865f-44b0-b23e-cc06b9431dcb");
                    Class.forName(classname);
                }
            }
            writeline("/home/ubuntu/results/coverage/Available/Available_9_10.coverage", "33fccb37-ebd8-47bd-957c-e30c4a08f5ea");
            return true;
        } catch (ClassNotFoundException e) {
            writeline("/home/ubuntu/results/coverage/Available/Available_9_10.coverage", "594de456-3623-4321-9bbc-77f77467445b");
            log("class \"" + classname + "\" was not found", Project.MSG_DEBUG);
            writeline("/home/ubuntu/results/coverage/Available/Available_9_10.coverage", "610ec69b-d6cb-4d2c-9dd0-dc224e31db9a");
            return false;
        } catch (NoClassDefFoundError e) {
            writeline("/home/ubuntu/results/coverage/Available/Available_9_10.coverage", "d255ff3e-faf0-4e76-800f-cac0a23d29e3");
            log("Could not load dependent class \"" + e.getMessage() + "\" for class \"" + classname + "\"", Project.MSG_DEBUG);
            writeline("/home/ubuntu/results/coverage/Available/Available_9_10.coverage", "10127892-3373-426b-a048-e537248f2aa4");
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
