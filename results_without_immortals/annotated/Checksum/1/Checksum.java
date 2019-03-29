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

import java.io.BufferedReader;
import java.io.File;
import java.io.FileInputStream;
import java.io.FileOutputStream;
import java.io.FileReader;
import java.io.IOException;
import java.security.DigestInputStream;
import java.security.MessageDigest;
import java.security.NoSuchAlgorithmException;
import java.security.NoSuchProviderException;
import java.text.MessageFormat;
import java.text.ParseException;
import java.util.Arrays;
import java.util.Comparator;
import java.util.HashMap;
import java.util.Hashtable;
import java.util.Map;
import org.apache.tools.ant.BuildException;
import org.apache.tools.ant.Project;
import org.apache.tools.ant.taskdefs.condition.Condition;
import org.apache.tools.ant.types.EnumeratedAttribute;
import org.apache.tools.ant.types.FileSet;
import org.apache.tools.ant.types.Resource;
import org.apache.tools.ant.types.ResourceCollection;
import org.apache.tools.ant.types.resources.FileProvider;
import org.apache.tools.ant.types.resources.Restrict;
import org.apache.tools.ant.types.resources.Union;
import org.apache.tools.ant.types.resources.selectors.Type;
import org.apache.tools.ant.util.FileUtils;
import org.apache.tools.ant.util.StringUtils;
import java.io.*;

/**
 * Used to create or verify file checksums.
 *
 * @since Ant 1.5
 *
 * @ant.task category="control"
 */
public class Checksum extends MatchingTask implements Condition {

    private static final int NIBBLE = 4;

    private static final int WORD = 16;

    private static final int BUFFER_SIZE = 8 * 1024;

    private static final int BYTE_MASK = 0xFF;

    private static class FileUnion extends Restrict {

        private Union u;

        FileUnion() {
            u = new Union();
            super.add(u);
            super.add(Type.FILE);
        }

        public void add(ResourceCollection rc) {
            u.add(rc);
        }
    }

    /**
     * File for which checksum is to be calculated.
     */
    private File file = null;

    /**
     * Root directory in which the checksum files will be written.
     * If not specified, the checksum files will be written
     * in the same directory as each file.
     */
    private File todir;

    /**
     * MessageDigest algorithm to be used.
     */
    private String algorithm = "MD5";

    /**
     * MessageDigest Algorithm provider
     */
    private String provider = null;

    /**
     * File Extension that is be to used to create or identify
     * destination file
     */
    private String fileext;

    /**
     * Holds generated checksum and gets set as a Project Property.
     */
    private String property;

    /**
     * Holds checksums for all files (both calculated and cached on disk).
     * Key:   java.util.File (source file)
     * Value: java.lang.String (digest)
     */
    private Map<File, byte[]> allDigests = new HashMap<File, byte[]>();

    /**
     * Holds relative file names for all files (always with a forward slash).
     * This is used to calculate the total hash.
     * Key:   java.util.File (source file)
     * Value: java.lang.String (relative file name)
     */
    private Map<File, String> relativeFilePaths = new HashMap<File, String>();

    /**
     * Property where totalChecksum gets set.
     */
    private String totalproperty;

    /**
     * Whether or not to create a new file.
     * Defaults to <code>false</code>.
     */
    private boolean forceOverwrite;

    /**
     * Contains the result of a checksum verification. ("true" or "false")
     */
    private String verifyProperty;

    /**
     * Resource Collection.
     */
    private FileUnion resources = null;

    /**
     * Stores SourceFile, DestFile pairs and SourceFile, Property String pairs.
     */
    private Hashtable<File, Object> includeFileMap = new Hashtable<File, Object>();

    /**
     * Message Digest instance
     */
    private MessageDigest messageDigest;

    /**
     * is this task being used as a nested condition element?
     */
    private boolean isCondition;

    /**
     * Size of the read buffer to use.
     */
    private int readBufferSize = BUFFER_SIZE;

    /**
     * Formater for the checksum file.
     */
    private MessageFormat format = FormatElement.getDefault().getFormat();

    /**
     * Sets the file for which the checksum is to be calculated.
     * @param file a <code>File</code> value
     */
    public void setFile(File file) {
        writeline("/home/ubuntu/results/coverage/Checksum/Checksum_1_10.coverage", "51c38f50-ceee-417f-a88e-f9bd03896416");
        this.file = file;
    }

    /**
     * Sets the root directory where checksum files will be
     * written/read
     * @param todir the directory to write to
     * @since Ant 1.6
     */
    public void setTodir(File todir) {
        writeline("/home/ubuntu/results/coverage/Checksum/Checksum_1_10.coverage", "de1944e6-c158-4977-ba8c-7a6770d9333e");
        this.todir = todir;
    }

    /**
     * Specifies the algorithm to be used to compute the checksum.
     * Defaults to "MD5". Other popular algorithms like "SHA" may be used as well.
     * @param algorithm a <code>String</code> value
     */
    public void setAlgorithm(String algorithm) {
        writeline("/home/ubuntu/results/coverage/Checksum/Checksum_1_10.coverage", "7cfb79ff-450e-49e3-b46d-1606330728de");
        this.algorithm = algorithm;
    }

    /**
     * Sets the MessageDigest algorithm provider to be used
     * to calculate the checksum.
     * @param provider a <code>String</code> value
     */
    public void setProvider(String provider) {
        writeline("/home/ubuntu/results/coverage/Checksum/Checksum_1_10.coverage", "b506d7e5-a3f0-4c04-b1c7-cba391a48bd4");
        this.provider = provider;
    }

    /**
     * Sets the file extension that is be to used to
     * create or identify destination file.
     * @param fileext a <code>String</code> value
     */
    public void setFileext(String fileext) {
        writeline("/home/ubuntu/results/coverage/Checksum/Checksum_1_10.coverage", "78278616-a878-4764-878c-8188367224d6");
        this.fileext = fileext;
    }

    /**
     * Sets the property to hold the generated checksum.
     * @param property a <code>String</code> value
     */
    public void setProperty(String property) {
        writeline("/home/ubuntu/results/coverage/Checksum/Checksum_1_10.coverage", "e15d73b4-9b37-452c-afcf-5fba3a8ea07f");
        this.property = property;
    }

    /**
     * Sets the property to hold the generated total checksum
     * for all files.
     * @param totalproperty a <code>String</code> value
     *
     * @since Ant 1.6
     */
    public void setTotalproperty(String totalproperty) {
        writeline("/home/ubuntu/results/coverage/Checksum/Checksum_1_10.coverage", "ec83ca06-6f5e-47ab-84ef-ea0dd953e7c9");
        this.totalproperty = totalproperty;
    }

    /**
     * Sets the verify property.  This project property holds
     * the result of a checksum verification - "true" or "false"
     * @param verifyProperty a <code>String</code> value
     */
    public void setVerifyproperty(String verifyProperty) {
        writeline("/home/ubuntu/results/coverage/Checksum/Checksum_1_10.coverage", "9d0cbd19-7d62-48e6-84ed-b79229fd4c88");
        this.verifyProperty = verifyProperty;
    }

    /**
     * Whether or not to overwrite existing file irrespective of
     * whether it is newer than
     * the source file.  Defaults to false.
     * @param forceOverwrite a <code>boolean</code> value
     */
    public void setForceOverwrite(boolean forceOverwrite) {
        writeline("/home/ubuntu/results/coverage/Checksum/Checksum_1_10.coverage", "49b4b944-8482-4fa9-a1fc-1ffc5565fe54");
        this.forceOverwrite = forceOverwrite;
    }

    /**
     * The size of the read buffer to use.
     * @param size an <code>int</code> value
     */
    public void setReadBufferSize(int size) {
        writeline("/home/ubuntu/results/coverage/Checksum/Checksum_1_10.coverage", "50df06c6-fb2e-4f8e-a41a-35f1c3513807");
        this.readBufferSize = size;
    }

    /**
     * Select the in/output pattern via a well know format name.
     * @param e an <code>enumerated</code> value
     *
     * @since 1.7.0
     */
    public void setFormat(FormatElement e) {
        writeline("/home/ubuntu/results/coverage/Checksum/Checksum_1_10.coverage", "79cf05cd-a178-4e58-872f-9b283bbe7459");
        format = e.getFormat();
    }

    /**
     * Specify the pattern to use as a MessageFormat pattern.
     *
     * <p>{0} gets replaced by the checksum, {1} by the filename.</p>
     * @param p a <code>String</code> value
     *
     * @since 1.7.0
     */
    public void setPattern(String p) {
        writeline("/home/ubuntu/results/coverage/Checksum/Checksum_1_10.coverage", "0f0a8a9f-5ef7-4488-8d58-57ac83bcefd7");
        format = new MessageFormat(p);
    }

    /**
     * Files to generate checksums for.
     * @param set a fileset of files to generate checksums for.
     */
    public void addFileset(FileSet set) {
        writeline("/home/ubuntu/results/coverage/Checksum/Checksum_1_10.coverage", "6ffa35fa-0131-432f-a084-21d446696e44");
        add(set);
    }

    /**
     * Add a resource collection.
     * @param rc the ResourceCollection to add.
     */
    public void add(ResourceCollection rc) {
        writeline("/home/ubuntu/results/coverage/Checksum/Checksum_1_10.coverage", "bc2e1f80-9faf-48bd-b5e9-927d83caa9c7");
        if (rc == null) {
            writeline("/home/ubuntu/results/coverage/Checksum/Checksum_1_10.coverage", "e5aba4b2-7050-448f-9b3a-73e44119d575");
            return;
        }
        writeline("/home/ubuntu/results/coverage/Checksum/Checksum_1_10.coverage", "52684f80-ae70-4649-9e10-ad27682d39bd");
        resources = (resources == null) ? new FileUnion() : resources;
        writeline("/home/ubuntu/results/coverage/Checksum/Checksum_1_10.coverage", "38aff649-00b6-4292-b773-783b33e5efe8");
        resources.add(rc);
    }

    /**
     * Calculate the checksum(s).
     * @throws BuildException on error
     */
    public void execute() throws BuildException {
        writeline("/home/ubuntu/results/coverage/Checksum/Checksum_1_10.coverage", "53587165-5093-4b26-9ebc-0be61769244f");
        isCondition = false;
        writeline("/home/ubuntu/results/coverage/Checksum/Checksum_1_10.coverage", "7d05f7d0-2e50-4bab-9973-494b507d5b6d");
        boolean value = validateAndExecute();
        writeline("/home/ubuntu/results/coverage/Checksum/Checksum_1_10.coverage", "547b0207-f86b-43f0-8b03-cc5f215412cb");
        if (verifyProperty != null) {
            writeline("/home/ubuntu/results/coverage/Checksum/Checksum_1_10.coverage", "285629f8-b7e4-4525-a665-ce19a527fdf3");
            getProject().setNewProperty(verifyProperty, (value ? Boolean.TRUE.toString() : Boolean.FALSE.toString()));
        }
    }

    /**
     * Calculate the checksum(s)
     *
     * @return Returns true if the checksum verification test passed,
     * false otherwise.
     * @throws BuildException on error
     */
    public boolean eval() throws BuildException {
        writeline("/home/ubuntu/results/coverage/Checksum/Checksum_1_10.coverage", "21cb3897-3709-4c57-b18d-17e1ceaa51a1");
        isCondition = true;
        writeline("/home/ubuntu/results/coverage/Checksum/Checksum_1_10.coverage", "3bdd18e3-23be-4395-9fc8-7f85df793b70");
        return validateAndExecute();
    }

    /**
     * Validate attributes and get down to business.
     */
    private boolean validateAndExecute() throws BuildException {
        writeline("/home/ubuntu/results/coverage/Checksum/Checksum_1_10.coverage", "f6aa3940-d314-4120-bbe4-4b36408fc912");
        String savedFileExt = fileext;
        writeline("/home/ubuntu/results/coverage/Checksum/Checksum_1_10.coverage", "665d47e8-798b-4746-a464-8ecc0321cada");
        if (file == null && (resources == null || resources.size() == 0)) {
            writeline("/home/ubuntu/results/coverage/Checksum/Checksum_1_10.coverage", "c069c986-b645-4049-895c-01cabc20f6ac");
            throw new BuildException("Specify at least one source - a file or a resource collection.");
        }
        writeline("/home/ubuntu/results/coverage/Checksum/Checksum_1_10.coverage", "490cb764-620a-4fb2-9a08-cf9b0e20adc7");
        if (!(resources == null || resources.isFilesystemOnly())) {
            writeline("/home/ubuntu/results/coverage/Checksum/Checksum_1_10.coverage", "a47ddb04-7e8b-44bb-abf4-a370bc31342b");
            throw new BuildException("Can only calculate checksums for file-based resources.");
        }
        writeline("/home/ubuntu/results/coverage/Checksum/Checksum_1_10.coverage", "70185495-e7ff-417d-815d-b8889306176e");
        if (file != null && file.exists() && file.isDirectory()) {
            writeline("/home/ubuntu/results/coverage/Checksum/Checksum_1_10.coverage", "dec46256-38f7-46b3-8675-bc0d39ed37c0");
            throw new BuildException("Checksum cannot be generated for directories");
        }
        writeline("/home/ubuntu/results/coverage/Checksum/Checksum_1_10.coverage", "1a2a73e0-a104-4645-b21b-f9234228423b");
        if (file != null && totalproperty != null) {
            writeline("/home/ubuntu/results/coverage/Checksum/Checksum_1_10.coverage", "c986d265-a2d9-49e0-b34e-a43cb8195648");
            throw new BuildException("File and Totalproperty cannot co-exist.");
        }
        writeline("/home/ubuntu/results/coverage/Checksum/Checksum_1_10.coverage", "dbcb05fe-858e-4ba8-9083-b20d0e3d1e3c");
        if (property != null && fileext != null) {
            writeline("/home/ubuntu/results/coverage/Checksum/Checksum_1_10.coverage", "5d976d5f-bf18-47ba-96f7-4ea415dc0f87");
            throw new BuildException("Property and FileExt cannot co-exist.");
        }
        writeline("/home/ubuntu/results/coverage/Checksum/Checksum_1_10.coverage", "6d5fb04c-fc7c-42ba-a83f-8786bdf9c498");
        if (property != null) {
            writeline("/home/ubuntu/results/coverage/Checksum/Checksum_1_10.coverage", "33fb6293-6ad2-4a1c-ac4c-b2c4f91f4d60");
            if (forceOverwrite) {
                writeline("/home/ubuntu/results/coverage/Checksum/Checksum_1_10.coverage", "df3d6f12-40c2-4915-84e5-758aad41af3f");
                throw new BuildException("ForceOverwrite cannot be used when Property is specified");
            }
            writeline("/home/ubuntu/results/coverage/Checksum/Checksum_1_10.coverage", "dfaaf33b-5e62-4e1e-acb6-965865002966");
            int ct = 0;
            writeline("/home/ubuntu/results/coverage/Checksum/Checksum_1_10.coverage", "b97b9259-2313-4e4e-86a7-e810b65956d3");
            if (resources != null) {
                writeline("/home/ubuntu/results/coverage/Checksum/Checksum_1_10.coverage", "380ac915-92bf-4337-9c18-f6629e4e1c25");
                ct += resources.size();
            }
            writeline("/home/ubuntu/results/coverage/Checksum/Checksum_1_10.coverage", "f2e4a58d-8e46-41ea-bcda-5b79e2a74660");
            if (file != null) {
                writeline("/home/ubuntu/results/coverage/Checksum/Checksum_1_10.coverage", "011e2e0f-7b3b-438e-a3dd-7fecbad12bdf");
                ct++;
            }
            writeline("/home/ubuntu/results/coverage/Checksum/Checksum_1_10.coverage", "e58710fd-587c-4fce-87f7-ae0c03f92bc0");
            if (ct > 1) {
                writeline("/home/ubuntu/results/coverage/Checksum/Checksum_1_10.coverage", "27cefc95-f48b-4950-bf6a-3cb4556a886d");
                throw new BuildException("Multiple files cannot be used when Property is specified");
            }
        }
        writeline("/home/ubuntu/results/coverage/Checksum/Checksum_1_10.coverage", "7bd3c412-e0d0-4305-aece-66a3d4d84a93");
        if (verifyProperty != null) {
            writeline("/home/ubuntu/results/coverage/Checksum/Checksum_1_10.coverage", "91b94c73-04db-4fdd-8c8c-fb8fb85ec305");
            isCondition = true;
        }
        writeline("/home/ubuntu/results/coverage/Checksum/Checksum_1_10.coverage", "c3136fc6-e993-4190-911e-208dc2603814");
        if (verifyProperty != null && forceOverwrite) {
            writeline("/home/ubuntu/results/coverage/Checksum/Checksum_1_10.coverage", "4ea4418c-f2ed-4b4b-858e-d3cb53884fb2");
            throw new BuildException("VerifyProperty and ForceOverwrite cannot co-exist.");
        }
        writeline("/home/ubuntu/results/coverage/Checksum/Checksum_1_10.coverage", "70096d71-6591-4cf2-8d34-5372dfccb004");
        if (isCondition && forceOverwrite) {
            writeline("/home/ubuntu/results/coverage/Checksum/Checksum_1_10.coverage", "b8aa57af-2b05-4e78-9fda-d43e617ff6fe");
            throw new BuildException("ForceOverwrite cannot be used when conditions are being used.");
        }
        writeline("/home/ubuntu/results/coverage/Checksum/Checksum_1_10.coverage", "f29a2fe7-ced8-45c8-94d1-f78734f3c59b");
        messageDigest = null;
        writeline("/home/ubuntu/results/coverage/Checksum/Checksum_1_10.coverage", "2364c423-fc2e-4680-9331-ea297c384dc3");
        if (provider != null) {
            writeline("/home/ubuntu/results/coverage/Checksum/Checksum_1_10.coverage", "fc51f6ed-2398-4449-8c83-c4571fa31b46");
            try {
                writeline("/home/ubuntu/results/coverage/Checksum/Checksum_1_10.coverage", "d2b47a4e-0db2-40c5-a6fa-5d9a04671702");
                messageDigest = MessageDigest.getInstance(algorithm, provider);
            } catch (NoSuchAlgorithmException noalgo) {
                writeline("/home/ubuntu/results/coverage/Checksum/Checksum_1_10.coverage", "45c513f4-4950-4b8c-953d-9b30d93d42c6");
                throw new BuildException(noalgo, getLocation());
            } catch (NoSuchProviderException noprovider) {
                writeline("/home/ubuntu/results/coverage/Checksum/Checksum_1_10.coverage", "41bc9c54-7189-4d04-ad6b-1e90705987b3");
                throw new BuildException(noprovider, getLocation());
            }
        } else {
            writeline("/home/ubuntu/results/coverage/Checksum/Checksum_1_10.coverage", "87639415-0e3f-4eed-a7fc-c0b3c6df3ef6");
            try {
                writeline("/home/ubuntu/results/coverage/Checksum/Checksum_1_10.coverage", "f312520c-94eb-435d-8542-54179e67f6c1");
                messageDigest = MessageDigest.getInstance(algorithm);
            } catch (NoSuchAlgorithmException noalgo) {
                writeline("/home/ubuntu/results/coverage/Checksum/Checksum_1_10.coverage", "9333c814-2d44-43eb-bd75-232dd1a20a8d");
                throw new BuildException(noalgo, getLocation());
            }
        }
        writeline("/home/ubuntu/results/coverage/Checksum/Checksum_1_10.coverage", "547049ea-3a6d-400c-a805-1099a06d5a3c");
        if (messageDigest == null) {
            writeline("/home/ubuntu/results/coverage/Checksum/Checksum_1_10.coverage", "07b47460-a37f-4a84-9f54-6dba3830b44d");
            throw new BuildException("Unable to create Message Digest", getLocation());
        }
        writeline("/home/ubuntu/results/coverage/Checksum/Checksum_1_10.coverage", "fcd88970-ac77-4cac-b56f-1b970b25ff3e");
        if (fileext == null) {
            writeline("/home/ubuntu/results/coverage/Checksum/Checksum_1_10.coverage", "180c66ad-a9cc-4acb-8e7b-5270cce5a39a");
            fileext = "." + algorithm;
        } else if (fileext.trim().length() == 0) {
            writeline("/home/ubuntu/results/coverage/Checksum/Checksum_1_10.coverage", "13ac5c00-2a49-4bf6-8830-f31d1db86129");
            throw new BuildException("File extension when specified must not be an empty string");
        }
        writeline("/home/ubuntu/results/coverage/Checksum/Checksum_1_10.coverage", "3620bcb5-7076-47fe-b978-3761a6ddda56");
        try {
            writeline("/home/ubuntu/results/coverage/Checksum/Checksum_1_10.coverage", "4b538872-5d95-4318-ae4f-bcbb77c03985");
            if (resources != null) {
                writeline("/home/ubuntu/results/coverage/Checksum/Checksum_1_10.coverage", "2d12cb90-9fdc-4638-9f78-eceee33b22f2");
                for (Resource r : resources) {
                    writeline("/home/ubuntu/results/coverage/Checksum/Checksum_1_10.coverage", "a0f303ff-ff34-4c63-bf24-63e911cacca2");
                    File src = r.as(FileProvider.class).getFile();
                    writeline("/home/ubuntu/results/coverage/Checksum/Checksum_1_10.coverage", "3c389d55-d6bf-4420-92de-a7476dcc4a05");
                    if (totalproperty != null || todir != null) {
                        writeline("/home/ubuntu/results/coverage/Checksum/Checksum_1_10.coverage", "bc3c7ce2-287e-4869-af7c-32ee68ef7acc");
                        // Use '/' to calculate digest based on file name.
                        // This is required in order to get the same result
                        // on different platforms.
                        relativeFilePaths.put(src, r.getName().replace(File.separatorChar, '/'));
                    }
                    writeline("/home/ubuntu/results/coverage/Checksum/Checksum_1_10.coverage", "fa48e5af-630c-440b-9cec-cd96ed21de00");
                    addToIncludeFileMap(src);
                }
            }
            writeline("/home/ubuntu/results/coverage/Checksum/Checksum_1_10.coverage", "3e62008f-c03e-40ea-b96f-eee1bda43659");
            if (file != null) {
                writeline("/home/ubuntu/results/coverage/Checksum/Checksum_1_10.coverage", "7f698346-af7a-4934-8514-6d34c6730dad");
                if (totalproperty != null || todir != null) {
                    writeline("/home/ubuntu/results/coverage/Checksum/Checksum_1_10.coverage", "7b553423-3c84-4c32-a340-17d65db9e375");
                    relativeFilePaths.put(file, file.getName().replace(File.separatorChar, '/'));
                }
                writeline("/home/ubuntu/results/coverage/Checksum/Checksum_1_10.coverage", "a3d7cf1b-2f1c-43dc-9e28-654bdf3d70ea");
                addToIncludeFileMap(file);
            }
            writeline("/home/ubuntu/results/coverage/Checksum/Checksum_1_10.coverage", "e3327e1c-bdcb-490c-85c0-d7639a0fdc98");
            return generateChecksums();
        } finally {
            writeline("/home/ubuntu/results/coverage/Checksum/Checksum_1_10.coverage", "8e7f651b-edef-4139-8ba4-99584bb527df");
            fileext = savedFileExt;
            writeline("/home/ubuntu/results/coverage/Checksum/Checksum_1_10.coverage", "490de2e5-08ec-4d57-9fa9-580f3c5edbbf");
            includeFileMap.clear();
        }
    }

    /**
     * Add key-value pair to the hashtable upon which
     * to later operate upon.
     */
    private void addToIncludeFileMap(File file) throws BuildException {
        writeline("/home/ubuntu/results/coverage/Checksum/Checksum_1_10.coverage", "09653404-b91d-41be-a9c9-84ac47998ba5");
        if (file.exists()) {
            writeline("/home/ubuntu/results/coverage/Checksum/Checksum_1_10.coverage", "a1d36419-1dc2-48d8-8834-ff4f7f766372");
            if (property == null) {
                writeline("/home/ubuntu/results/coverage/Checksum/Checksum_1_10.coverage", "e80740f2-674e-4890-a49b-2e195ff2cead");
                File checksumFile = getChecksumFile(file);
                writeline("/home/ubuntu/results/coverage/Checksum/Checksum_1_10.coverage", "1ae56d48-88b6-486c-a3e2-a18b14832cf1");
                if (forceOverwrite || isCondition || (file.lastModified() > checksumFile.lastModified())) {
                    writeline("/home/ubuntu/results/coverage/Checksum/Checksum_1_10.coverage", "180c2da4-4e91-4a32-936d-41d24249cd47");
                    includeFileMap.put(file, checksumFile);
                } else {
                    writeline("/home/ubuntu/results/coverage/Checksum/Checksum_1_10.coverage", "8fd6474a-5547-4569-a4a6-51c3428cfda9");
                    log(file + " omitted as " + checksumFile + " is up to date.", Project.MSG_VERBOSE);
                    writeline("/home/ubuntu/results/coverage/Checksum/Checksum_1_10.coverage", "59db20ae-d929-43d4-851b-60bcb589695c");
                    if (totalproperty != null) {
                        writeline("/home/ubuntu/results/coverage/Checksum/Checksum_1_10.coverage", "bc52fbce-08b2-427c-9e7b-fb1b1ce61f6c");
                        // Read the checksum from disk.
                        String checksum = readChecksum(checksumFile);
                        writeline("/home/ubuntu/results/coverage/Checksum/Checksum_1_10.coverage", "de6eff11-b316-4af8-b2a4-a76d74d4826c");
                        byte[] digest = decodeHex(checksum.toCharArray());
                        writeline("/home/ubuntu/results/coverage/Checksum/Checksum_1_10.coverage", "be65e245-eef7-4cf3-91a7-85a33f242dc8");
                        allDigests.put(file, digest);
                    }
                }
            } else {
                writeline("/home/ubuntu/results/coverage/Checksum/Checksum_1_10.coverage", "3e45b604-1376-42d4-b889-50463a0a8d86");
                includeFileMap.put(file, property);
            }
        } else {
            writeline("/home/ubuntu/results/coverage/Checksum/Checksum_1_10.coverage", "b04279e5-ed9e-4036-827b-e2757448632d");
            String message = "Could not find file " + file.getAbsolutePath() + " to generate checksum for.";
            writeline("/home/ubuntu/results/coverage/Checksum/Checksum_1_10.coverage", "868f37b9-8fc0-4c87-94b4-f09590b93ca4");
            log(message);
            writeline("/home/ubuntu/results/coverage/Checksum/Checksum_1_10.coverage", "329ad3c7-4174-4643-8d3f-af4a3af7b1cd");
            throw new BuildException(message, getLocation());
        }
    }

    private File getChecksumFile(File file) {
        writeline("/home/ubuntu/results/coverage/Checksum/Checksum_1_10.coverage", "f9fe08d9-429a-4ca2-98d2-35e7a3551221");
        File directory;
        writeline("/home/ubuntu/results/coverage/Checksum/Checksum_1_10.coverage", "880d2314-c0de-41d4-b4af-3b7ac84023c8");
        if (todir != null) {
            writeline("/home/ubuntu/results/coverage/Checksum/Checksum_1_10.coverage", "cc16072a-d3c4-43c1-ae37-91de58d45d23");
            // A separate directory was explicitly declared
            String path = getRelativeFilePath(file);
            writeline("/home/ubuntu/results/coverage/Checksum/Checksum_1_10.coverage", "faaf64a7-1ba5-4b5f-bf5e-78e9bd7fd029");
            directory = new File(todir, path).getParentFile();
            writeline("/home/ubuntu/results/coverage/Checksum/Checksum_1_10.coverage", "98632fa9-60c8-4452-893e-ba6b7b9694c1");
            // Create the directory, as it might not exist.
            directory.mkdirs();
        } else {
            writeline("/home/ubuntu/results/coverage/Checksum/Checksum_1_10.coverage", "eb6d6736-a0fe-4e0d-8b4c-d7b15076157b");
            // Just use the same directory as the file itself.
            // This directory will exist
            directory = file.getParentFile();
        }
        writeline("/home/ubuntu/results/coverage/Checksum/Checksum_1_10.coverage", "16febd68-e466-42fc-8ba5-51181f5ae936");
        File checksumFile = new File(directory, file.getName() + fileext);
        writeline("/home/ubuntu/results/coverage/Checksum/Checksum_1_10.coverage", "3d7886b4-2849-4dd4-af33-01f86bbdd0db");
        return checksumFile;
    }

    /**
     * Generate checksum(s) using the message digest created earlier.
     */
    private boolean generateChecksums() throws BuildException {
        writeline("/home/ubuntu/results/coverage/Checksum/Checksum_1_10.coverage", "a3c261ef-506d-49fc-9807-468128ae4a28");
        boolean checksumMatches = true;
        writeline("/home/ubuntu/results/coverage/Checksum/Checksum_1_10.coverage", "6e6cdfe2-87b3-4c01-a862-e5997a93b4f3");
        FileInputStream fis = null;
        writeline("/home/ubuntu/results/coverage/Checksum/Checksum_1_10.coverage", "6b1ec968-f621-471c-9c95-905205b19bcc");
        FileOutputStream fos = null;
        writeline("/home/ubuntu/results/coverage/Checksum/Checksum_1_10.coverage", "d2213e47-09c8-4c07-a7c5-a63b738cf392");
        byte[] buf = new byte[readBufferSize];
        writeline("/home/ubuntu/results/coverage/Checksum/Checksum_1_10.coverage", "bf8ddf88-b7c3-4380-a024-42b9479139df");
        try {
            writeline("/home/ubuntu/results/coverage/Checksum/Checksum_1_10.coverage", "69dda7fd-c950-4c8e-a9e6-16c37b145b83");
            for (Map.Entry<File, Object> e : includeFileMap.entrySet()) {
                writeline("/home/ubuntu/results/coverage/Checksum/Checksum_1_10.coverage", "664a0241-4a26-4b56-974d-392a608a4b2c");
                messageDigest.reset();
                writeline("/home/ubuntu/results/coverage/Checksum/Checksum_1_10.coverage", "4dbb9dbb-8dbf-4262-9b74-5371368ad331");
                File src = e.getKey();
                writeline("/home/ubuntu/results/coverage/Checksum/Checksum_1_10.coverage", "77beef0d-3753-4aa3-8d65-7ead0d8ccb1d");
                if (!isCondition) {
                    writeline("/home/ubuntu/results/coverage/Checksum/Checksum_1_10.coverage", "e9ce1ef1-1a3b-4082-9942-4e52a97d5138");
                    log("Calculating " + algorithm + " checksum for " + src, Project.MSG_VERBOSE);
                }
                writeline("/home/ubuntu/results/coverage/Checksum/Checksum_1_10.coverage", "d7d3ca99-351b-4edd-8699-e35945daf221");
                fis = new FileInputStream(src);
                writeline("/home/ubuntu/results/coverage/Checksum/Checksum_1_10.coverage", "a88dae78-ff65-4b5a-b657-3f3b7b7aa8df");
                DigestInputStream dis = new DigestInputStream(fis, messageDigest);
                writeline("/home/ubuntu/results/coverage/Checksum/Checksum_1_10.coverage", "20a9de98-5f4b-4aa9-8b76-4454843b443e");
                while (dis.read(buf, 0, readBufferSize) != -1) {
                // Empty statement
                }
                writeline("/home/ubuntu/results/coverage/Checksum/Checksum_1_10.coverage", "8f90536b-3461-406d-b375-e4db3b4810c9");
                dis.close();
                writeline("/home/ubuntu/results/coverage/Checksum/Checksum_1_10.coverage", "7c647c33-a060-4aad-b8e9-854c11d13688");
                fis.close();
                writeline("/home/ubuntu/results/coverage/Checksum/Checksum_1_10.coverage", "9c1bdb04-eacc-4a5f-8145-38d289f53793");
                fis = null;
                writeline("/home/ubuntu/results/coverage/Checksum/Checksum_1_10.coverage", "83616bb6-3914-47e5-bee0-1692a14c543e");
                byte[] fileDigest = messageDigest.digest();
                writeline("/home/ubuntu/results/coverage/Checksum/Checksum_1_10.coverage", "9af030b5-2414-49e7-b8ef-36f3d2406dce");
                if (totalproperty != null) {
                    writeline("/home/ubuntu/results/coverage/Checksum/Checksum_1_10.coverage", "19b0d821-151a-440d-8962-133a97c782a4");
                    allDigests.put(src, fileDigest);
                }
                writeline("/home/ubuntu/results/coverage/Checksum/Checksum_1_10.coverage", "c8597508-51f7-4530-adea-87d7b1c0655c");
                String checksum = createDigestString(fileDigest);
                writeline("/home/ubuntu/results/coverage/Checksum/Checksum_1_10.coverage", "712e1b9e-0510-49a5-824f-290cc5a12fd1");
                // can either be a property name string or a file
                Object destination = e.getValue();
                writeline("/home/ubuntu/results/coverage/Checksum/Checksum_1_10.coverage", "b2c96020-8552-416f-aae3-7fc51d0f5248");
                if (destination instanceof java.lang.String) {
                    writeline("/home/ubuntu/results/coverage/Checksum/Checksum_1_10.coverage", "136e8328-d17e-4b9f-ae28-fe4561cffe45");
                    String prop = (String) destination;
                    writeline("/home/ubuntu/results/coverage/Checksum/Checksum_1_10.coverage", "5aa636f2-f595-42db-8fb1-b59f03cc1673");
                    if (isCondition) {
                        writeline("/home/ubuntu/results/coverage/Checksum/Checksum_1_10.coverage", "233c3bb0-4a2e-47a9-9e4a-9f05bcf22283");
                        checksumMatches = checksumMatches && checksum.equals(property);
                    } else {
                        writeline("/home/ubuntu/results/coverage/Checksum/Checksum_1_10.coverage", "8c6f5179-7300-4cb0-8cde-eb7ab0229f83");
                        getProject().setNewProperty(prop, checksum);
                    }
                } else if (destination instanceof java.io.File) {
                    writeline("/home/ubuntu/results/coverage/Checksum/Checksum_1_10.coverage", "a277800e-0e2f-4732-8910-de4b54b95bf4");
                    if (isCondition) {
                        writeline("/home/ubuntu/results/coverage/Checksum/Checksum_1_10.coverage", "c0b8c411-9867-4a9c-8307-b309cd68aff3");
                        File existingFile = (File) destination;
                        writeline("/home/ubuntu/results/coverage/Checksum/Checksum_1_10.coverage", "f878bb67-4270-42bf-8042-85644d359c40");
                        if (existingFile.exists()) {
                            writeline("/home/ubuntu/results/coverage/Checksum/Checksum_1_10.coverage", "603db11e-163f-4dd1-b935-8aa94f43b95d");
                            try {
                                writeline("/home/ubuntu/results/coverage/Checksum/Checksum_1_10.coverage", "ac0eb995-ee2e-4073-b7d6-b8b52b02ea72");
                                String suppliedChecksum = readChecksum(existingFile);
                                writeline("/home/ubuntu/results/coverage/Checksum/Checksum_1_10.coverage", "c2519c6c-dbc1-4188-b812-ac8b0f5a3182");
                                checksumMatches = checksumMatches && checksum.equals(suppliedChecksum);
                            } catch (BuildException be) {
                                writeline("/home/ubuntu/results/coverage/Checksum/Checksum_1_10.coverage", "246abc35-3108-4063-8a58-ca436f7c9d0e");
                                // file is on wrong format, swallow
                                checksumMatches = false;
                            }
                        } else {
                            writeline("/home/ubuntu/results/coverage/Checksum/Checksum_1_10.coverage", "a34fb349-e7bd-49ae-aa33-d205d0b96037");
                            checksumMatches = false;
                        }
                    } else {
                        writeline("/home/ubuntu/results/coverage/Checksum/Checksum_1_10.coverage", "d2324c3a-7d3d-4db2-b815-3e01e784afa6");
                        File dest = (File) destination;
                        writeline("/home/ubuntu/results/coverage/Checksum/Checksum_1_10.coverage", "fd1cef53-e8f0-4ed6-b92e-74fdb018da61");
                        fos = new FileOutputStream(dest);
                        writeline("/home/ubuntu/results/coverage/Checksum/Checksum_1_10.coverage", "43b24130-5a4e-4b37-8a2a-de8811e44e4f");
                        fos.write(format.format(new Object[] { checksum, src.getName(), FileUtils.getRelativePath(dest.getParentFile(), src), FileUtils.getRelativePath(getProject().getBaseDir(), src), src.getAbsolutePath() }).getBytes());
                        writeline("/home/ubuntu/results/coverage/Checksum/Checksum_1_10.coverage", "a06b58ea-7c14-440f-a59f-6b0d7c9fc0f5");
                        fos.write(StringUtils.LINE_SEP.getBytes());
                        writeline("/home/ubuntu/results/coverage/Checksum/Checksum_1_10.coverage", "eb2ffa79-0d73-4ed2-bd3a-380c43d1fff0");
                        fos.close();
                        writeline("/home/ubuntu/results/coverage/Checksum/Checksum_1_10.coverage", "2d6ed792-9585-4576-8f28-18a322a10ec1");
                        fos = null;
                    }
                }
            }
            writeline("/home/ubuntu/results/coverage/Checksum/Checksum_1_10.coverage", "d03f4ea1-851a-41c0-b960-468af278715e");
            if (totalproperty != null) {
                writeline("/home/ubuntu/results/coverage/Checksum/Checksum_1_10.coverage", "46c01d38-02e4-463c-a3d2-f1fe942408b2");
                // Calculate the total checksum
                // Convert the keys (source files) into a sorted array.
                File[] keyArray = allDigests.keySet().toArray(new File[allDigests.size()]);
                writeline("/home/ubuntu/results/coverage/Checksum/Checksum_1_10.coverage", "fabb982c-763c-491e-a9bd-73e19ee4aa4d");
                // File is Comparable, but sort-order is platform
                // dependent (case-insensitive on Windows)
                Arrays.sort(keyArray, new Comparator<File>() {

                    public int compare(File f1, File f2) {
                        return f1 == null ? (f2 == null ? 0 : -1) : (f2 == null ? 1 : getRelativeFilePath(f1).compareTo(getRelativeFilePath(f2)));
                    }
                });
                writeline("/home/ubuntu/results/coverage/Checksum/Checksum_1_10.coverage", "f71a6c4b-2b81-4863-8097-b754c3ae509c");
                // Loop over the checksums and generate a total hash.
                messageDigest.reset();
                writeline("/home/ubuntu/results/coverage/Checksum/Checksum_1_10.coverage", "039f8f91-adc8-4988-b2b8-ba3848b204f7");
                for (File src : keyArray) {
                    writeline("/home/ubuntu/results/coverage/Checksum/Checksum_1_10.coverage", "a7e4400e-17c7-4199-925e-b568cbc12f72");
                    // Add the digest for the file content
                    byte[] digest = allDigests.get(src);
                    writeline("/home/ubuntu/results/coverage/Checksum/Checksum_1_10.coverage", "e81cd617-30fd-4ccf-882b-9ebd7d4022c6");
                    messageDigest.update(digest);
                    writeline("/home/ubuntu/results/coverage/Checksum/Checksum_1_10.coverage", "7baa0b22-a714-4ea0-8513-26bd0de11482");
                    // Add the file path
                    String fileName = getRelativeFilePath(src);
                    writeline("/home/ubuntu/results/coverage/Checksum/Checksum_1_10.coverage", "ac76ff1f-fd4d-4b8c-917c-dbac10264648");
                    messageDigest.update(fileName.getBytes());
                }
                writeline("/home/ubuntu/results/coverage/Checksum/Checksum_1_10.coverage", "9a24c3e2-2d20-474e-a317-5cba86ee43c8");
                String totalChecksum = createDigestString(messageDigest.digest());
                writeline("/home/ubuntu/results/coverage/Checksum/Checksum_1_10.coverage", "cdc50f21-f273-4344-bad9-3de1a5da3361");
                getProject().setNewProperty(totalproperty, totalChecksum);
            }
        } catch (Exception e) {
            writeline("/home/ubuntu/results/coverage/Checksum/Checksum_1_10.coverage", "0f427cfd-cf4e-41ce-b8e4-21480f272d34");
            throw new BuildException(e, getLocation());
        } finally {
            writeline("/home/ubuntu/results/coverage/Checksum/Checksum_1_10.coverage", "7d187d1a-118c-4645-806b-329a76d61b37");
            FileUtils.close(fis);
            writeline("/home/ubuntu/results/coverage/Checksum/Checksum_1_10.coverage", "aa4ec40b-a066-42c8-b18d-cf06a3fc4fa2");
            FileUtils.close(fos);
        }
        writeline("/home/ubuntu/results/coverage/Checksum/Checksum_1_10.coverage", "58b60815-8a71-483b-8a27-468c9409cfbd");
        return checksumMatches;
    }

    private String createDigestString(byte[] fileDigest) {
        writeline("/home/ubuntu/results/coverage/Checksum/Checksum_1_10.coverage", "1af22057-b7ac-435b-b664-97fbafde451c");
        StringBuffer checksumSb = new StringBuffer();
        writeline("/home/ubuntu/results/coverage/Checksum/Checksum_1_10.coverage", "5e87c8c6-c07e-4a5b-82fc-bae536e4059c");
        for (int i = 0; i < fileDigest.length; i++) {
            writeline("/home/ubuntu/results/coverage/Checksum/Checksum_1_10.coverage", "26d3c75b-6cc4-4082-930e-a3085464f517");
            String hexStr = Integer.toHexString(BYTE_MASK & fileDigest[i]);
            writeline("/home/ubuntu/results/coverage/Checksum/Checksum_1_10.coverage", "e726b806-5be1-4303-9661-251a98cfb862");
            if (hexStr.length() < 2) {
                writeline("/home/ubuntu/results/coverage/Checksum/Checksum_1_10.coverage", "f020ea75-0e03-45c1-990f-0c051a6f1957");
                checksumSb.append("0");
            }
            writeline("/home/ubuntu/results/coverage/Checksum/Checksum_1_10.coverage", "4795acd3-cfab-4352-a48e-0e4a9bb0f98c");
            checksumSb.append(hexStr);
        }
        writeline("/home/ubuntu/results/coverage/Checksum/Checksum_1_10.coverage", "604d0722-b464-4397-b751-4efa87761359");
        return checksumSb.toString();
    }

    /**
     * Converts an array of characters representing hexadecimal values into an
     * array of bytes of those same values. The returned array will be half the
     * length of the passed array, as it takes two characters to represent any
     * given byte. An exception is thrown if the passed char array has an odd
     * number of elements.
     *
     * NOTE: This code is copied from jakarta-commons codec.
     * @param data an array of characters representing hexadecimal values
     * @return the converted array of bytes
     * @throws BuildException on error
     */
    public static byte[] decodeHex(char[] data) throws BuildException {
        writelineStatic("/home/ubuntu/results/coverage/Checksum/Checksum_1_10.coverage", "cca963b5-2b33-487f-ba1a-3d21b63ceca7");
        int l = data.length;
        writelineStatic("/home/ubuntu/results/coverage/Checksum/Checksum_1_10.coverage", "23e5d6ac-89ab-4f70-a0f4-619730ec26e3");
        if ((l & 0x01) != 0) {
            writelineStatic("/home/ubuntu/results/coverage/Checksum/Checksum_1_10.coverage", "affa7d6c-b7ed-4c7d-9451-dc30c3d05808");
            throw new BuildException("odd number of characters.");
        }
        writelineStatic("/home/ubuntu/results/coverage/Checksum/Checksum_1_10.coverage", "324028b1-73cc-4f27-88a8-08826cba98ab");
        byte[] out = new byte[l >> 1];
        writelineStatic("/home/ubuntu/results/coverage/Checksum/Checksum_1_10.coverage", "7e2b6c22-d9c8-46c5-a940-3c83de4eef33");
        // two characters form the hex value.
        for (int i = 0, j = 0; j < l; i++) {
            writelineStatic("/home/ubuntu/results/coverage/Checksum/Checksum_1_10.coverage", "0cc18d76-7bfd-478b-a7e8-6e00128ae667");
            int f = Character.digit(data[j++], WORD) << NIBBLE;
            writelineStatic("/home/ubuntu/results/coverage/Checksum/Checksum_1_10.coverage", "f474b0a6-7870-4645-ba16-f06938a9543b");
            f = f | Character.digit(data[j++], WORD);
            writelineStatic("/home/ubuntu/results/coverage/Checksum/Checksum_1_10.coverage", "89c1483a-df01-4623-a431-7308ea082eb8");
            out[i] = (byte) (f & BYTE_MASK);
        }
        writelineStatic("/home/ubuntu/results/coverage/Checksum/Checksum_1_10.coverage", "5c40393f-049c-4cba-bba3-2c150ab831bb");
        return out;
    }

    /**
     * reads the checksum from a file using the specified format.
     *
     * @since 1.7
     */
    private String readChecksum(File f) {
        writeline("/home/ubuntu/results/coverage/Checksum/Checksum_1_10.coverage", "56b5c870-fe9f-46ce-bb73-fa1a6ac8fbe8");
        BufferedReader diskChecksumReader = null;
        writeline("/home/ubuntu/results/coverage/Checksum/Checksum_1_10.coverage", "dc4ddceb-21cd-4a4b-bc3a-f40e26817c23");
        try {
            writeline("/home/ubuntu/results/coverage/Checksum/Checksum_1_10.coverage", "7335d07c-caa2-4699-9a7f-ab9d9998157c");
            diskChecksumReader = new BufferedReader(new FileReader(f));
            writeline("/home/ubuntu/results/coverage/Checksum/Checksum_1_10.coverage", "bca15aae-48fb-4a78-8794-384412be42dd");
            Object[] result = format.parse(diskChecksumReader.readLine());
            writeline("/home/ubuntu/results/coverage/Checksum/Checksum_1_10.coverage", "38e007e6-1bfc-4187-a1b2-6300f4b61227");
            if (result == null || result.length == 0 || result[0] == null) {
                writeline("/home/ubuntu/results/coverage/Checksum/Checksum_1_10.coverage", "22eae652-e482-40ab-b6d2-461d21e62cc0");
                throw new BuildException("failed to find a checksum");
            }
            writeline("/home/ubuntu/results/coverage/Checksum/Checksum_1_10.coverage", "ea64ad11-f69e-4457-9720-8aec2d429a7f");
            return (String) result[0];
        } catch (IOException e) {
            writeline("/home/ubuntu/results/coverage/Checksum/Checksum_1_10.coverage", "d186a8aa-3f36-4d65-9a87-8a684d2398ad");
            throw new BuildException("Couldn't read checksum file " + f, e);
        } catch (ParseException e) {
            writeline("/home/ubuntu/results/coverage/Checksum/Checksum_1_10.coverage", "be72945c-fd83-4411-afc3-992fe33dda9d");
            throw new BuildException("Couldn't read checksum file " + f, e);
        } finally {
            writeline("/home/ubuntu/results/coverage/Checksum/Checksum_1_10.coverage", "9d55e04a-6890-4d6c-aab0-0c9a0e875ede");
            FileUtils.close(diskChecksumReader);
        }
    }

    /**
     * @since Ant 1.8.2
     */
    private String getRelativeFilePath(File f) {
        writeline("/home/ubuntu/results/coverage/Checksum/Checksum_1_10.coverage", "eae46010-2a60-402e-ae3b-de3b719118e5");
        String path = (String) relativeFilePaths.get(f);
        writeline("/home/ubuntu/results/coverage/Checksum/Checksum_1_10.coverage", "1b888021-f58e-4744-aa94-e69c74a9ed70");
        if (path == null) {
            writeline("/home/ubuntu/results/coverage/Checksum/Checksum_1_10.coverage", "405a689b-f183-4926-b129-2328b049cb7d");
            // bug 37386. this should not occur, but it has, once.
            throw new BuildException("Internal error: " + "relativeFilePaths could not match file " + f + "\n" + "please file a bug report on this");
        }
        writeline("/home/ubuntu/results/coverage/Checksum/Checksum_1_10.coverage", "2811575c-d8fe-42a9-b6cb-710777147ed4");
        return path;
    }

    /**
     * Helper class for the format attribute.
     *
     * @since 1.7
     */
    public static class FormatElement extends EnumeratedAttribute {

        private static HashMap<String, MessageFormat> formatMap = new HashMap<String, MessageFormat>();

        private static final String CHECKSUM = "CHECKSUM";

        private static final String MD5SUM = "MD5SUM";

        private static final String SVF = "SVF";

        static {
            formatMap.put(CHECKSUM, new MessageFormat("{0}"));
            formatMap.put(MD5SUM, new MessageFormat("{0} *{1}"));
            formatMap.put(SVF, new MessageFormat("MD5 ({1}) = {0}"));
        }

        /**
         * Constructor for FormatElement
         */
        public FormatElement() {
            super();
        }

        /**
         * Get the default value - CHECKSUM.
         * @return the defaul value.
         */
        public static FormatElement getDefault() {
            FormatElement e = new FormatElement();
            e.setValue(CHECKSUM);
            return e;
        }

        /**
         * Convert this enumerated type to a <code>MessageFormat</code>.
         * @return a <code>MessageFormat</code> object.
         */
        public MessageFormat getFormat() {
            return (MessageFormat) formatMap.get(getValue());
        }

        /**
         * Get the valid values.
         * @return an array of values.
         */
        public String[] getValues() {
            return new String[] { CHECKSUM, MD5SUM, SVF };
        }
    }

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
