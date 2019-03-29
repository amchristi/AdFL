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
        writeline("/home/ubuntu/results/coverage/Checksum/Checksum_7_10.coverage", "30f511f2-6c3b-4995-91b4-3486b1b4ada9");
        this.file = file;
    }

    /**
     * Sets the root directory where checksum files will be
     * written/read
     * @param todir the directory to write to
     * @since Ant 1.6
     */
    public void setTodir(File todir) {
        writeline("/home/ubuntu/results/coverage/Checksum/Checksum_7_10.coverage", "7939321d-1dbf-4214-a6ea-4ac422323672");
        this.todir = todir;
    }

    /**
     * Specifies the algorithm to be used to compute the checksum.
     * Defaults to "MD5". Other popular algorithms like "SHA" may be used as well.
     * @param algorithm a <code>String</code> value
     */
    public void setAlgorithm(String algorithm) {
        writeline("/home/ubuntu/results/coverage/Checksum/Checksum_7_10.coverage", "267beb5f-cc70-4a02-9625-f7b5197c7db4");
        this.algorithm = algorithm;
    }

    /**
     * Sets the MessageDigest algorithm provider to be used
     * to calculate the checksum.
     * @param provider a <code>String</code> value
     */
    public void setProvider(String provider) {
        writeline("/home/ubuntu/results/coverage/Checksum/Checksum_7_10.coverage", "11d49248-abff-4578-9bbf-2712698f11fe");
        this.provider = provider;
    }

    /**
     * Sets the file extension that is be to used to
     * create or identify destination file.
     * @param fileext a <code>String</code> value
     */
    public void setFileext(String fileext) {
        writeline("/home/ubuntu/results/coverage/Checksum/Checksum_7_10.coverage", "dcbf2d5e-dd38-4d8b-bc69-3435768aa576");
        this.fileext = fileext;
    }

    /**
     * Sets the property to hold the generated checksum.
     * @param property a <code>String</code> value
     */
    public void setProperty(String property) {
        writeline("/home/ubuntu/results/coverage/Checksum/Checksum_7_10.coverage", "9151e3b4-f463-43d6-b7b5-eb721d2ab329");
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
        writeline("/home/ubuntu/results/coverage/Checksum/Checksum_7_10.coverage", "cd4b3ecc-8779-4da6-9d42-3370e09537fc");
        this.totalproperty = totalproperty;
    }

    /**
     * Sets the verify property.  This project property holds
     * the result of a checksum verification - "true" or "false"
     * @param verifyProperty a <code>String</code> value
     */
    public void setVerifyproperty(String verifyProperty) {
        writeline("/home/ubuntu/results/coverage/Checksum/Checksum_7_10.coverage", "c94517a4-9ac9-4cf6-bb3f-af1ee4352788");
        this.verifyProperty = verifyProperty;
    }

    /**
     * Whether or not to overwrite existing file irrespective of
     * whether it is newer than
     * the source file.  Defaults to false.
     * @param forceOverwrite a <code>boolean</code> value
     */
    public void setForceOverwrite(boolean forceOverwrite) {
        writeline("/home/ubuntu/results/coverage/Checksum/Checksum_7_10.coverage", "a1d6a15b-65df-462c-af5b-a5aab5a642e1");
        this.forceOverwrite = forceOverwrite;
    }

    /**
     * The size of the read buffer to use.
     * @param size an <code>int</code> value
     */
    public void setReadBufferSize(int size) {
        writeline("/home/ubuntu/results/coverage/Checksum/Checksum_7_10.coverage", "40ad09aa-7f04-42ed-8f17-049821dbd0ee");
        this.readBufferSize = size;
    }

    /**
     * Select the in/output pattern via a well know format name.
     * @param e an <code>enumerated</code> value
     *
     * @since 1.7.0
     */
    public void setFormat(FormatElement e) {
        writeline("/home/ubuntu/results/coverage/Checksum/Checksum_7_10.coverage", "42b95998-642c-494d-925c-b43364f79b57");
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
        writeline("/home/ubuntu/results/coverage/Checksum/Checksum_7_10.coverage", "d4a30efd-bc62-4f6d-b873-8add4f740041");
        format = new MessageFormat(p);
    }

    /**
     * Files to generate checksums for.
     * @param set a fileset of files to generate checksums for.
     */
    public void addFileset(FileSet set) {
        writeline("/home/ubuntu/results/coverage/Checksum/Checksum_7_10.coverage", "a648c498-109d-48b2-a340-87926637c2ac");
        add(set);
    }

    /**
     * Add a resource collection.
     * @param rc the ResourceCollection to add.
     */
    public void add(ResourceCollection rc) {
        writeline("/home/ubuntu/results/coverage/Checksum/Checksum_7_10.coverage", "d01c2d1c-6f88-4fb5-8cbe-48b03db9065c");
        if (rc == null) {
            writeline("/home/ubuntu/results/coverage/Checksum/Checksum_7_10.coverage", "5bfd3ba3-3f2f-4264-984b-970ecf7b52e2");
            return;
        }
        writeline("/home/ubuntu/results/coverage/Checksum/Checksum_7_10.coverage", "47b9bbfd-d1c9-4b43-97fd-7d93f79cd4a2");
        resources = (resources == null) ? new FileUnion() : resources;
        writeline("/home/ubuntu/results/coverage/Checksum/Checksum_7_10.coverage", "80beef95-91ad-4d45-b3b3-8a0ee9a7fb26");
        resources.add(rc);
    }

    /**
     * Calculate the checksum(s).
     * @throws BuildException on error
     */
    public void execute() throws BuildException {
        writeline("/home/ubuntu/results/coverage/Checksum/Checksum_7_10.coverage", "58fe36cc-4150-43da-ae71-6f821028cc8a");
        isCondition = false;
        writeline("/home/ubuntu/results/coverage/Checksum/Checksum_7_10.coverage", "9189629b-0d0c-461e-8a87-b39b978e5349");
        boolean value = validateAndExecute();
        writeline("/home/ubuntu/results/coverage/Checksum/Checksum_7_10.coverage", "3b052be2-05e8-4e23-bb50-2d3d63e5f31b");
        if (verifyProperty != null) {
            writeline("/home/ubuntu/results/coverage/Checksum/Checksum_7_10.coverage", "e3301de8-141d-4f8f-ba2d-baa0feee25b3");
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
        writeline("/home/ubuntu/results/coverage/Checksum/Checksum_7_10.coverage", "0ae96c25-acde-4bda-a727-3eb28c7447f3");
        isCondition = true;
        writeline("/home/ubuntu/results/coverage/Checksum/Checksum_7_10.coverage", "794e819f-1da2-4409-88a3-f61f12b1f3f1");
        return validateAndExecute();
    }

    /**
     * Validate attributes and get down to business.
     */
    private boolean validateAndExecute() throws BuildException {
        writeline("/home/ubuntu/results/coverage/Checksum/Checksum_7_10.coverage", "960e57bf-738b-48d7-902c-119384e45df5");
        String savedFileExt = fileext;
        writeline("/home/ubuntu/results/coverage/Checksum/Checksum_7_10.coverage", "10912337-bc73-4ff7-8850-cbc4622fd61c");
        if (file == null && (resources == null || resources.size() == 0)) {
            writeline("/home/ubuntu/results/coverage/Checksum/Checksum_7_10.coverage", "9a0099ab-ed31-47a1-912f-6e0391245673");
            throw new BuildException("Specify at least one source - a file or a resource collection.");
        }
        writeline("/home/ubuntu/results/coverage/Checksum/Checksum_7_10.coverage", "1c2771d4-0ce5-42c7-86ba-ea441c02fda9");
        if (!(resources == null || resources.isFilesystemOnly())) {
            writeline("/home/ubuntu/results/coverage/Checksum/Checksum_7_10.coverage", "7713680d-fe5d-4b54-afcd-f893861cfb4f");
            throw new BuildException("Can only calculate checksums for file-based resources.");
        }
        writeline("/home/ubuntu/results/coverage/Checksum/Checksum_7_10.coverage", "37c3dedb-f889-48f8-9e3f-9daa31abe93d");
        if (file != null && file.exists() && file.isDirectory()) {
            writeline("/home/ubuntu/results/coverage/Checksum/Checksum_7_10.coverage", "3e3330a3-f1c1-4428-a6dc-8ba8416b1ff9");
            throw new BuildException("Checksum cannot be generated for directories");
        }
        writeline("/home/ubuntu/results/coverage/Checksum/Checksum_7_10.coverage", "9ca0ad61-5b52-4c80-8372-db31b4f9aceb");
        if (file != null && totalproperty != null) {
            writeline("/home/ubuntu/results/coverage/Checksum/Checksum_7_10.coverage", "419c9452-c60c-492e-a07c-76194f4ab0d8");
            throw new BuildException("File and Totalproperty cannot co-exist.");
        }
        writeline("/home/ubuntu/results/coverage/Checksum/Checksum_7_10.coverage", "1664841a-323e-4e5c-8e5d-0041fd5e7d9f");
        if (property != null && fileext != null) {
            writeline("/home/ubuntu/results/coverage/Checksum/Checksum_7_10.coverage", "3b4d6b10-862c-43de-8c83-c36c5a2c87c3");
            throw new BuildException("Property and FileExt cannot co-exist.");
        }
        writeline("/home/ubuntu/results/coverage/Checksum/Checksum_7_10.coverage", "ee9de051-544d-4870-8ab6-a867ac0da0a8");
        if (property != null) {
            writeline("/home/ubuntu/results/coverage/Checksum/Checksum_7_10.coverage", "5b10b4ae-093d-44d5-9067-5d7804814b35");
            if (forceOverwrite) {
                writeline("/home/ubuntu/results/coverage/Checksum/Checksum_7_10.coverage", "0cadaff1-e1d2-460f-b2af-60056839dc6b");
                throw new BuildException("ForceOverwrite cannot be used when Property is specified");
            }
            writeline("/home/ubuntu/results/coverage/Checksum/Checksum_7_10.coverage", "043a5b0f-fb57-4143-a9d5-82776a604ca4");
            int ct = 0;
            writeline("/home/ubuntu/results/coverage/Checksum/Checksum_7_10.coverage", "eb3efa7d-2a50-4770-be1b-1f737a265501");
            if (resources != null) {
                writeline("/home/ubuntu/results/coverage/Checksum/Checksum_7_10.coverage", "d7d2d0d0-5472-40a5-a887-b6002426775c");
                ct += resources.size();
            }
            writeline("/home/ubuntu/results/coverage/Checksum/Checksum_7_10.coverage", "ee8364be-4edd-46a5-9633-d011a584b6e0");
            if (file != null) {
                writeline("/home/ubuntu/results/coverage/Checksum/Checksum_7_10.coverage", "f31de856-6c89-4cd4-8969-79e59969f45a");
                ct++;
            }
            writeline("/home/ubuntu/results/coverage/Checksum/Checksum_7_10.coverage", "c3b41cc0-f4b6-418f-abc6-1edb80458b06");
            if (ct > 1) {
                writeline("/home/ubuntu/results/coverage/Checksum/Checksum_7_10.coverage", "b0344a59-203e-4d7a-8c54-962f6374ef5b");
                throw new BuildException("Multiple files cannot be used when Property is specified");
            }
        }
        writeline("/home/ubuntu/results/coverage/Checksum/Checksum_7_10.coverage", "1c664d58-d0fb-44d6-950a-28319445c39e");
        if (verifyProperty != null) {
            writeline("/home/ubuntu/results/coverage/Checksum/Checksum_7_10.coverage", "0c3db27c-a233-4bfa-bec7-8a72f2bd3eeb");
            isCondition = true;
        }
        writeline("/home/ubuntu/results/coverage/Checksum/Checksum_7_10.coverage", "661d378f-0e7a-4a82-b3a2-dfa12aaa79c4");
        if (verifyProperty != null && forceOverwrite) {
            writeline("/home/ubuntu/results/coverage/Checksum/Checksum_7_10.coverage", "c458cb87-e688-478d-ae36-c4b8bad4d06d");
            throw new BuildException("VerifyProperty and ForceOverwrite cannot co-exist.");
        }
        writeline("/home/ubuntu/results/coverage/Checksum/Checksum_7_10.coverage", "3bd30f2e-bf56-4605-ac0c-f717c49d4e1d");
        if (isCondition && forceOverwrite) {
            writeline("/home/ubuntu/results/coverage/Checksum/Checksum_7_10.coverage", "eacfe341-2550-4920-96ac-b7e2031d365d");
            throw new BuildException("ForceOverwrite cannot be used when conditions are being used.");
        }
        writeline("/home/ubuntu/results/coverage/Checksum/Checksum_7_10.coverage", "513ad201-c516-46ba-a42e-f409fe04dbef");
        messageDigest = null;
        writeline("/home/ubuntu/results/coverage/Checksum/Checksum_7_10.coverage", "816e84ab-9592-4735-b15a-02730d2dba4d");
        if (provider != null) {
            writeline("/home/ubuntu/results/coverage/Checksum/Checksum_7_10.coverage", "34702224-e6ec-486a-b323-d92c99402b75");
            try {
                writeline("/home/ubuntu/results/coverage/Checksum/Checksum_7_10.coverage", "0b918389-92fd-49fb-9ab5-1de8b4ef6716");
                messageDigest = MessageDigest.getInstance(algorithm, provider);
            } catch (NoSuchAlgorithmException noalgo) {
                writeline("/home/ubuntu/results/coverage/Checksum/Checksum_7_10.coverage", "33e39266-47bb-4b1e-9289-cf76d4c88642");
                throw new BuildException(noalgo, getLocation());
            } catch (NoSuchProviderException noprovider) {
                writeline("/home/ubuntu/results/coverage/Checksum/Checksum_7_10.coverage", "aa727dca-0b2f-4b3f-97c5-425f5bf27e23");
                throw new BuildException(noprovider, getLocation());
            }
        } else {
            writeline("/home/ubuntu/results/coverage/Checksum/Checksum_7_10.coverage", "4107e7db-2260-4c08-8678-a296682e0baf");
            try {
                writeline("/home/ubuntu/results/coverage/Checksum/Checksum_7_10.coverage", "1193f15c-d09e-428f-80d9-5d5558671619");
                messageDigest = MessageDigest.getInstance(algorithm);
            } catch (NoSuchAlgorithmException noalgo) {
                writeline("/home/ubuntu/results/coverage/Checksum/Checksum_7_10.coverage", "253b03a3-4f37-4c8b-976b-3d49b49ea7b4");
                throw new BuildException(noalgo, getLocation());
            }
        }
        writeline("/home/ubuntu/results/coverage/Checksum/Checksum_7_10.coverage", "3d98a34b-a3d8-41b2-af84-cb7f84ed2da9");
        if (messageDigest == null) {
            writeline("/home/ubuntu/results/coverage/Checksum/Checksum_7_10.coverage", "cd858432-084e-4dce-9d13-6dc2b25fe292");
            throw new BuildException("Unable to create Message Digest", getLocation());
        }
        writeline("/home/ubuntu/results/coverage/Checksum/Checksum_7_10.coverage", "0f55dcbf-e811-44d8-9fb5-03b04e9aa450");
        if (fileext == null) {
            writeline("/home/ubuntu/results/coverage/Checksum/Checksum_7_10.coverage", "f74c094a-0111-4bf7-968e-bffe3f17a86b");
            fileext = "." + algorithm;
        } else if (fileext.trim().length() == 0) {
            writeline("/home/ubuntu/results/coverage/Checksum/Checksum_7_10.coverage", "57d724a8-a7df-4dc3-a760-2ecff17b2896");
            throw new BuildException("File extension when specified must not be an empty string");
        }
        writeline("/home/ubuntu/results/coverage/Checksum/Checksum_7_10.coverage", "215a41c4-bd25-4523-8447-067652b801ac");
        try {
            writeline("/home/ubuntu/results/coverage/Checksum/Checksum_7_10.coverage", "709c50d4-50e2-4240-8012-4495cae123c7");
            if (resources != null) {
                writeline("/home/ubuntu/results/coverage/Checksum/Checksum_7_10.coverage", "3f8482a7-d8ca-4a4e-8d9a-876c3361e07a");
                for (Resource r : resources) {
                    writeline("/home/ubuntu/results/coverage/Checksum/Checksum_7_10.coverage", "b553e5b0-3875-48fa-8704-2765dee11f36");
                    File src = r.as(FileProvider.class).getFile();
                    writeline("/home/ubuntu/results/coverage/Checksum/Checksum_7_10.coverage", "28613bc8-2625-4b94-b720-6c28bef447cc");
                    if (totalproperty != null || todir != null) {
                        writeline("/home/ubuntu/results/coverage/Checksum/Checksum_7_10.coverage", "ee279832-6208-4c77-a2c2-ee72f99fb388");
                        // Use '/' to calculate digest based on file name.
                        // This is required in order to get the same result
                        // on different platforms.
                        relativeFilePaths.put(src, r.getName().replace(File.separatorChar, '/'));
                    }
                    writeline("/home/ubuntu/results/coverage/Checksum/Checksum_7_10.coverage", "9b44c49f-446b-4786-af0d-941c28a19f45");
                    addToIncludeFileMap(src);
                }
            }
            writeline("/home/ubuntu/results/coverage/Checksum/Checksum_7_10.coverage", "45f73efb-7a9f-4a92-8563-b2ae1d6603f9");
            if (file != null) {
                writeline("/home/ubuntu/results/coverage/Checksum/Checksum_7_10.coverage", "37513952-fcaf-42de-991c-3fd03af8297c");
                if (totalproperty != null || todir != null) {
                    writeline("/home/ubuntu/results/coverage/Checksum/Checksum_7_10.coverage", "47f6d546-6eb3-4f21-9f9f-c600cf2a6086");
                    relativeFilePaths.put(file, file.getName().replace(File.separatorChar, '/'));
                }
                writeline("/home/ubuntu/results/coverage/Checksum/Checksum_7_10.coverage", "2a6431b0-bd09-49bd-a69d-2111d5d5df3e");
                addToIncludeFileMap(file);
            }
            writeline("/home/ubuntu/results/coverage/Checksum/Checksum_7_10.coverage", "9c89de3c-f6ca-4fbb-8a7d-c54fa1f5a663");
            return generateChecksums();
        } finally {
            writeline("/home/ubuntu/results/coverage/Checksum/Checksum_7_10.coverage", "aabf8e4f-44af-4580-8e75-b356fa875f10");
            fileext = savedFileExt;
            writeline("/home/ubuntu/results/coverage/Checksum/Checksum_7_10.coverage", "6fe0ad33-5257-4709-b4e8-c6e989e00aba");
            includeFileMap.clear();
        }
    }

    /**
     * Add key-value pair to the hashtable upon which
     * to later operate upon.
     */
    private void addToIncludeFileMap(File file) throws BuildException {
        writeline("/home/ubuntu/results/coverage/Checksum/Checksum_7_10.coverage", "b34cbcb7-f06c-453a-909b-800763ac0f73");
        if (file.exists()) {
            writeline("/home/ubuntu/results/coverage/Checksum/Checksum_7_10.coverage", "967d488b-3e9a-4cfb-8748-25980142e34d");
            if (property == null) {
                writeline("/home/ubuntu/results/coverage/Checksum/Checksum_7_10.coverage", "87b3d5ad-7667-4009-bafc-740d68c8a82e");
                File checksumFile = getChecksumFile(file);
                writeline("/home/ubuntu/results/coverage/Checksum/Checksum_7_10.coverage", "5ccad49c-e27f-4662-b4aa-51e70d16b4fc");
                if (forceOverwrite || isCondition || (file.lastModified() > checksumFile.lastModified())) {
                    writeline("/home/ubuntu/results/coverage/Checksum/Checksum_7_10.coverage", "1b922f55-5dfd-4651-8859-d161f2e51417");
                    includeFileMap.put(file, checksumFile);
                } else {
                    writeline("/home/ubuntu/results/coverage/Checksum/Checksum_7_10.coverage", "89de6593-ef7a-4b86-aa23-5af15156a302");
                    log(file + " omitted as " + checksumFile + " is up to date.", Project.MSG_VERBOSE);
                    writeline("/home/ubuntu/results/coverage/Checksum/Checksum_7_10.coverage", "e10bcede-3f77-4c88-b027-ae9a4f052786");
                    if (totalproperty != null) {
                        writeline("/home/ubuntu/results/coverage/Checksum/Checksum_7_10.coverage", "f1aafe60-be23-437a-a9fc-071fd46b9752");
                        // Read the checksum from disk.
                        String checksum = readChecksum(checksumFile);
                        writeline("/home/ubuntu/results/coverage/Checksum/Checksum_7_10.coverage", "2eca01ee-f059-4e78-b21b-1aea5bb98ac3");
                        byte[] digest = decodeHex(checksum.toCharArray());
                        writeline("/home/ubuntu/results/coverage/Checksum/Checksum_7_10.coverage", "c452c0e1-1471-4820-9310-8e2355e644e0");
                        allDigests.put(file, digest);
                    }
                }
            } else {
                writeline("/home/ubuntu/results/coverage/Checksum/Checksum_7_10.coverage", "78829d72-3f07-40a0-8808-c2ca283e47a4");
                includeFileMap.put(file, property);
            }
        } else {
            writeline("/home/ubuntu/results/coverage/Checksum/Checksum_7_10.coverage", "0ee7ce89-83a5-4bc8-9c22-4e42b974222d");
            String message = "Could not find file " + file.getAbsolutePath() + " to generate checksum for.";
            writeline("/home/ubuntu/results/coverage/Checksum/Checksum_7_10.coverage", "9809b756-21f2-42bd-a19a-d69b952227f7");
            log(message);
            writeline("/home/ubuntu/results/coverage/Checksum/Checksum_7_10.coverage", "ed34e9bc-30d4-4636-9ffe-bb4761ac2c3a");
            throw new BuildException(message, getLocation());
        }
    }

    private File getChecksumFile(File file) {
        writeline("/home/ubuntu/results/coverage/Checksum/Checksum_7_10.coverage", "f80651f9-b1bf-4bf4-8b78-f7fb9a3febdf");
        File directory;
        writeline("/home/ubuntu/results/coverage/Checksum/Checksum_7_10.coverage", "be519d49-3dc9-4126-9573-b9926e70e3a5");
        if (todir != null) {
            writeline("/home/ubuntu/results/coverage/Checksum/Checksum_7_10.coverage", "aa74bff6-dedf-45d9-9e85-8aa3edac69aa");
            // A separate directory was explicitly declared
            String path = getRelativeFilePath(file);
            writeline("/home/ubuntu/results/coverage/Checksum/Checksum_7_10.coverage", "ff5f08fd-b044-4a94-b54b-fda97707817a");
            directory = new File(todir, path).getParentFile();
            writeline("/home/ubuntu/results/coverage/Checksum/Checksum_7_10.coverage", "67b968f6-35a6-4be9-926c-1741047cde1c");
            // Create the directory, as it might not exist.
            directory.mkdirs();
        } else {
            writeline("/home/ubuntu/results/coverage/Checksum/Checksum_7_10.coverage", "670b9f94-8b2e-4bbf-bf76-62c083e13693");
            // Just use the same directory as the file itself.
            // This directory will exist
            directory = file.getParentFile();
        }
        writeline("/home/ubuntu/results/coverage/Checksum/Checksum_7_10.coverage", "5ebbea17-0aba-457e-ab02-7935f22b21d0");
        File checksumFile = new File(directory, file.getName() + fileext);
        writeline("/home/ubuntu/results/coverage/Checksum/Checksum_7_10.coverage", "126491bd-22f5-4416-91e6-384d6f69d4a0");
        return checksumFile;
    }

    /**
     * Generate checksum(s) using the message digest created earlier.
     */
    private boolean generateChecksums() throws BuildException {
        writeline("/home/ubuntu/results/coverage/Checksum/Checksum_7_10.coverage", "6d742730-fd6f-42df-b410-757904f73699");
        boolean checksumMatches = true;
        writeline("/home/ubuntu/results/coverage/Checksum/Checksum_7_10.coverage", "2f50462a-3f42-4964-9d15-9df194379589");
        FileInputStream fis = null;
        writeline("/home/ubuntu/results/coverage/Checksum/Checksum_7_10.coverage", "762bd381-7340-40ca-8741-2f7e38d915be");
        FileOutputStream fos = null;
        writeline("/home/ubuntu/results/coverage/Checksum/Checksum_7_10.coverage", "49f57a85-9e6b-49a5-86db-1adb193b5bff");
        byte[] buf = new byte[readBufferSize];
        writeline("/home/ubuntu/results/coverage/Checksum/Checksum_7_10.coverage", "40e7b6c6-6397-48f1-aaad-3aa2d29c45ed");
        try {
            writeline("/home/ubuntu/results/coverage/Checksum/Checksum_7_10.coverage", "7cf9bc6e-8d1a-4264-bab6-f98d71cafd9f");
            for (Map.Entry<File, Object> e : includeFileMap.entrySet()) {
                writeline("/home/ubuntu/results/coverage/Checksum/Checksum_7_10.coverage", "e3ac13a0-2696-482b-8e2f-9360f3fe31a3");
                messageDigest.reset();
                writeline("/home/ubuntu/results/coverage/Checksum/Checksum_7_10.coverage", "4158ba1b-c2e4-4481-a970-427210a19bd5");
                File src = e.getKey();
                writeline("/home/ubuntu/results/coverage/Checksum/Checksum_7_10.coverage", "76b225f4-28de-4bd3-bb9e-2267baddd5f9");
                if (!isCondition) {
                    writeline("/home/ubuntu/results/coverage/Checksum/Checksum_7_10.coverage", "49933298-6c62-4d00-b37c-816e64e7ddc7");
                    log("Calculating " + algorithm + " checksum for " + src, Project.MSG_VERBOSE);
                }
                writeline("/home/ubuntu/results/coverage/Checksum/Checksum_7_10.coverage", "99d70b6e-3878-40c1-ac16-42ad50bb62c6");
                fis = new FileInputStream(src);
                writeline("/home/ubuntu/results/coverage/Checksum/Checksum_7_10.coverage", "f98d5850-52e5-4226-9a1d-979b4263b450");
                DigestInputStream dis = new DigestInputStream(fis, messageDigest);
                writeline("/home/ubuntu/results/coverage/Checksum/Checksum_7_10.coverage", "00c7438d-87b1-4f3f-b437-357b875a6484");
                while (dis.read(buf, 0, readBufferSize) != -1) {
                // Empty statement
                }
                writeline("/home/ubuntu/results/coverage/Checksum/Checksum_7_10.coverage", "b561a9e7-0e94-4852-b2f0-fc13b97a8c9d");
                dis.close();
                writeline("/home/ubuntu/results/coverage/Checksum/Checksum_7_10.coverage", "6f347c2b-c676-4ccb-9164-107f6e6fbe70");
                fis.close();
                writeline("/home/ubuntu/results/coverage/Checksum/Checksum_7_10.coverage", "3cf71063-d571-428f-ae75-536742f9063c");
                fis = null;
                writeline("/home/ubuntu/results/coverage/Checksum/Checksum_7_10.coverage", "b7b4c8bd-41bc-4cd9-83c8-afecfe596727");
                byte[] fileDigest = messageDigest.digest();
                writeline("/home/ubuntu/results/coverage/Checksum/Checksum_7_10.coverage", "3a5aee03-ca4c-4d17-88a7-f47c4988933a");
                if (totalproperty != null) {
                    writeline("/home/ubuntu/results/coverage/Checksum/Checksum_7_10.coverage", "b75bb9f2-f024-4e7e-b287-6e409a9c359a");
                    allDigests.put(src, fileDigest);
                }
                writeline("/home/ubuntu/results/coverage/Checksum/Checksum_7_10.coverage", "b35e943c-e5c0-446d-b578-3dbcd3ff1402");
                String checksum = createDigestString(fileDigest);
                writeline("/home/ubuntu/results/coverage/Checksum/Checksum_7_10.coverage", "f8965cbb-6426-4b5b-8fa0-63d843618c0e");
                // can either be a property name string or a file
                Object destination = e.getValue();
                writeline("/home/ubuntu/results/coverage/Checksum/Checksum_7_10.coverage", "b62683b2-6f94-48b8-84c0-1ba84fcfeaf6");
                if (destination instanceof java.lang.String) {
                    writeline("/home/ubuntu/results/coverage/Checksum/Checksum_7_10.coverage", "573031cb-62b6-4eee-99c6-690cd5568ba1");
                    String prop = (String) destination;
                    writeline("/home/ubuntu/results/coverage/Checksum/Checksum_7_10.coverage", "f4e79690-9264-4965-bd42-e429ecd38c3f");
                    if (isCondition) {
                        writeline("/home/ubuntu/results/coverage/Checksum/Checksum_7_10.coverage", "730e23ee-dd92-4677-be66-5b87d0a56c30");
                        checksumMatches = checksumMatches && checksum.equals(property);
                    } else {
                        writeline("/home/ubuntu/results/coverage/Checksum/Checksum_7_10.coverage", "16817783-a657-42b5-8458-713aecdbaca5");
                        getProject().setNewProperty(prop, checksum);
                    }
                } else if (destination instanceof java.io.File) {
                    writeline("/home/ubuntu/results/coverage/Checksum/Checksum_7_10.coverage", "7350f315-2309-45d9-8f45-3ddf028d30fe");
                    if (isCondition) {
                        writeline("/home/ubuntu/results/coverage/Checksum/Checksum_7_10.coverage", "c4ddc6a0-d841-44d6-9427-b26b9fb33aa7");
                        File existingFile = (File) destination;
                        writeline("/home/ubuntu/results/coverage/Checksum/Checksum_7_10.coverage", "8692797a-5cec-4226-a3f0-ca199f5bb408");
                        if (existingFile.exists()) {
                            writeline("/home/ubuntu/results/coverage/Checksum/Checksum_7_10.coverage", "220e47e8-1911-45d9-9e22-d7bd5276d1a9");
                            try {
                                writeline("/home/ubuntu/results/coverage/Checksum/Checksum_7_10.coverage", "77a92e3c-2b87-406d-973d-4e3c239f3d3e");
                                String suppliedChecksum = readChecksum(existingFile);
                                writeline("/home/ubuntu/results/coverage/Checksum/Checksum_7_10.coverage", "709c1427-1b7e-466f-bb97-fec0656ed05c");
                                checksumMatches = checksumMatches && checksum.equals(suppliedChecksum);
                            } catch (BuildException be) {
                                writeline("/home/ubuntu/results/coverage/Checksum/Checksum_7_10.coverage", "472ea40d-3725-471c-b829-138ee177a47d");
                                // file is on wrong format, swallow
                                checksumMatches = false;
                            }
                        } else {
                            writeline("/home/ubuntu/results/coverage/Checksum/Checksum_7_10.coverage", "d681c622-7aed-4241-a340-e1bd78d56cb6");
                            checksumMatches = false;
                        }
                    } else {
                        writeline("/home/ubuntu/results/coverage/Checksum/Checksum_7_10.coverage", "9fa830b7-7b54-48c1-badd-f3e5aed428bc");
                        File dest = (File) destination;
                        writeline("/home/ubuntu/results/coverage/Checksum/Checksum_7_10.coverage", "d8a0bfa8-c343-41ee-8797-3cd60bbceb44");
                        fos = new FileOutputStream(dest);
                        writeline("/home/ubuntu/results/coverage/Checksum/Checksum_7_10.coverage", "874cd835-0a60-4030-9c93-0d88fcfae829");
                        fos.write(format.format(new Object[] { checksum, src.getName(), FileUtils.getRelativePath(dest.getParentFile(), src), FileUtils.getRelativePath(getProject().getBaseDir(), src), src.getAbsolutePath() }).getBytes());
                        writeline("/home/ubuntu/results/coverage/Checksum/Checksum_7_10.coverage", "e30ec0f2-d7cd-4e21-929c-e8f5e7da2943");
                        fos.write(StringUtils.LINE_SEP.getBytes());
                        writeline("/home/ubuntu/results/coverage/Checksum/Checksum_7_10.coverage", "3a179e9d-42c0-412d-96d3-55acc2a6206b");
                        fos.close();
                        writeline("/home/ubuntu/results/coverage/Checksum/Checksum_7_10.coverage", "0fa5fb79-ae67-4701-a9c2-20982abc2025");
                        fos = null;
                    }
                }
            }
            writeline("/home/ubuntu/results/coverage/Checksum/Checksum_7_10.coverage", "7a23916a-35c5-44a9-beb1-0e65705b3010");
            if (totalproperty != null) {
                writeline("/home/ubuntu/results/coverage/Checksum/Checksum_7_10.coverage", "50336421-6560-400e-bc31-cf48a8f16f98");
                // Calculate the total checksum
                // Convert the keys (source files) into a sorted array.
                File[] keyArray = allDigests.keySet().toArray(new File[allDigests.size()]);
                writeline("/home/ubuntu/results/coverage/Checksum/Checksum_7_10.coverage", "6ee548ba-61c5-4a8e-a763-893e9b33ca11");
                // File is Comparable, but sort-order is platform
                // dependent (case-insensitive on Windows)
                Arrays.sort(keyArray, new Comparator<File>() {

                    public int compare(File f1, File f2) {
                        return f1 == null ? (f2 == null ? 0 : -1) : (f2 == null ? 1 : getRelativeFilePath(f1).compareTo(getRelativeFilePath(f2)));
                    }
                });
                writeline("/home/ubuntu/results/coverage/Checksum/Checksum_7_10.coverage", "0b97030e-380a-4f15-a8ae-8d891e7a0ea6");
                // Loop over the checksums and generate a total hash.
                messageDigest.reset();
                writeline("/home/ubuntu/results/coverage/Checksum/Checksum_7_10.coverage", "a5c0e34d-2be0-4332-923d-6ea92269014e");
                for (File src : keyArray) {
                    writeline("/home/ubuntu/results/coverage/Checksum/Checksum_7_10.coverage", "95f5ca49-c621-4ce6-8bbf-68df9b7d1037");
                    // Add the digest for the file content
                    byte[] digest = allDigests.get(src);
                    writeline("/home/ubuntu/results/coverage/Checksum/Checksum_7_10.coverage", "fc1b7546-903a-469d-ab73-9dce7f23f9d9");
                    messageDigest.update(digest);
                    writeline("/home/ubuntu/results/coverage/Checksum/Checksum_7_10.coverage", "f56d9db5-fdc2-49e2-873a-112108382578");
                    // Add the file path
                    String fileName = getRelativeFilePath(src);
                    writeline("/home/ubuntu/results/coverage/Checksum/Checksum_7_10.coverage", "b15d4db4-9a74-46a7-8b95-2eb5e13df5c0");
                    messageDigest.update(fileName.getBytes());
                }
                writeline("/home/ubuntu/results/coverage/Checksum/Checksum_7_10.coverage", "5ebbbbd9-4dcb-442b-bfb2-7bceec304f8c");
                String totalChecksum = createDigestString(messageDigest.digest());
                writeline("/home/ubuntu/results/coverage/Checksum/Checksum_7_10.coverage", "7ae55fe4-87bb-4140-8a5d-896a75fd6022");
                getProject().setNewProperty(totalproperty, totalChecksum);
            }
        } catch (Exception e) {
            writeline("/home/ubuntu/results/coverage/Checksum/Checksum_7_10.coverage", "01f285bd-a4a6-408e-8f2b-e197695fa830");
            throw new BuildException(e, getLocation());
        } finally {
            writeline("/home/ubuntu/results/coverage/Checksum/Checksum_7_10.coverage", "9ce0e40f-64c2-405e-905b-bdaede881604");
            FileUtils.close(fis);
            writeline("/home/ubuntu/results/coverage/Checksum/Checksum_7_10.coverage", "25256dd9-bda5-4da0-8b77-882a73ebe9e3");
            FileUtils.close(fos);
        }
        writeline("/home/ubuntu/results/coverage/Checksum/Checksum_7_10.coverage", "dc9dab62-6c60-4494-a4ad-1a1ad2243376");
        return checksumMatches;
    }

    private String createDigestString(byte[] fileDigest) {
        writeline("/home/ubuntu/results/coverage/Checksum/Checksum_7_10.coverage", "12896599-d56b-4cdd-98d8-e0a2acde75f1");
        StringBuffer checksumSb = new StringBuffer();
        writeline("/home/ubuntu/results/coverage/Checksum/Checksum_7_10.coverage", "69cc2563-5e87-42ac-8fdd-5a0e4a664502");
        for (int i = 0; i < fileDigest.length; i++) {
            writeline("/home/ubuntu/results/coverage/Checksum/Checksum_7_10.coverage", "09866564-e9b7-46a1-b804-d33d179954eb");
            String hexStr = Integer.toHexString(BYTE_MASK & fileDigest[i]);
            writeline("/home/ubuntu/results/coverage/Checksum/Checksum_7_10.coverage", "d44b2d63-f74c-4eab-a4f8-e348086f1afd");
            if (hexStr.length() < 2) {
                writeline("/home/ubuntu/results/coverage/Checksum/Checksum_7_10.coverage", "0a3c229b-ef7f-46d0-a715-8fd495ace79d");
                checksumSb.append("0");
            }
            writeline("/home/ubuntu/results/coverage/Checksum/Checksum_7_10.coverage", "7857525a-b675-40dd-b79e-063c07666d7b");
            checksumSb.append(hexStr);
        }
        writeline("/home/ubuntu/results/coverage/Checksum/Checksum_7_10.coverage", "b6313d42-69a7-4cc5-bbb6-218e7124ae4c");
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
        writelineStatic("/home/ubuntu/results/coverage/Checksum/Checksum_7_10.coverage", "4cad1e52-855b-46bd-9faa-2b7f8cc4511f");
        int l = data.length;
        writelineStatic("/home/ubuntu/results/coverage/Checksum/Checksum_7_10.coverage", "a01d4bba-c527-44ff-a70e-4b7145450356");
        if ((l & 0x01) != 0) {
            writelineStatic("/home/ubuntu/results/coverage/Checksum/Checksum_7_10.coverage", "baab464e-d238-4660-9904-047d04616a07");
            throw new BuildException("odd number of characters.");
        }
        writelineStatic("/home/ubuntu/results/coverage/Checksum/Checksum_7_10.coverage", "05838cdf-4c8b-4f4a-a619-d3b4fd7eb75f");
        byte[] out = new byte[l >> 1];
        writelineStatic("/home/ubuntu/results/coverage/Checksum/Checksum_7_10.coverage", "3a7759bd-e853-4898-a06c-1953071c1f22");
        // two characters form the hex value.
        for (int i = 0, j = 0; j < l; i++) {
            writelineStatic("/home/ubuntu/results/coverage/Checksum/Checksum_7_10.coverage", "fa2877da-ef6f-4f5f-bc75-e82ff194a9ae");
            int f = Character.digit(data[j++], WORD) << NIBBLE;
            writelineStatic("/home/ubuntu/results/coverage/Checksum/Checksum_7_10.coverage", "1130be94-a3ff-4ec1-bb77-e65463a592c7");
            f = f | Character.digit(data[j++], WORD);
            writelineStatic("/home/ubuntu/results/coverage/Checksum/Checksum_7_10.coverage", "b6feb301-f87b-4496-a277-6fe48df3209f");
            out[i] = (byte) (f & BYTE_MASK);
        }
        writelineStatic("/home/ubuntu/results/coverage/Checksum/Checksum_7_10.coverage", "0722ff6d-7aa2-49ec-9fcc-e76b0e5abaeb");
        return out;
    }

    /**
     * reads the checksum from a file using the specified format.
     *
     * @since 1.7
     */
    private String readChecksum(File f) {
        writeline("/home/ubuntu/results/coverage/Checksum/Checksum_7_10.coverage", "c087fd8f-eefe-4d74-ad9c-b9ecc4781fba");
        BufferedReader diskChecksumReader = null;
        writeline("/home/ubuntu/results/coverage/Checksum/Checksum_7_10.coverage", "7675bf84-b931-422c-93aa-de6955716a3d");
        try {
            writeline("/home/ubuntu/results/coverage/Checksum/Checksum_7_10.coverage", "2cc202ac-4d68-4d76-9b5b-1a6871c3065c");
            diskChecksumReader = new BufferedReader(new FileReader(f));
            writeline("/home/ubuntu/results/coverage/Checksum/Checksum_7_10.coverage", "f775a8bd-d546-473b-9368-3c78f9d53721");
            Object[] result = format.parse(diskChecksumReader.readLine());
            writeline("/home/ubuntu/results/coverage/Checksum/Checksum_7_10.coverage", "ccde0c18-b8b4-4cdc-a7d8-b908a580490d");
            if (result == null || result.length == 0 || result[0] == null) {
                writeline("/home/ubuntu/results/coverage/Checksum/Checksum_7_10.coverage", "7de1c769-5994-47d7-a316-671029c43596");
                throw new BuildException("failed to find a checksum");
            }
            writeline("/home/ubuntu/results/coverage/Checksum/Checksum_7_10.coverage", "048d1922-1b43-438a-9e36-e7eb4dc0b3b4");
            return (String) result[0];
        } catch (IOException e) {
            writeline("/home/ubuntu/results/coverage/Checksum/Checksum_7_10.coverage", "d223362b-85fa-4b24-b05f-34fc7f1c8467");
            throw new BuildException("Couldn't read checksum file " + f, e);
        } catch (ParseException e) {
            writeline("/home/ubuntu/results/coverage/Checksum/Checksum_7_10.coverage", "26792718-6e3b-4786-9ae5-b9c594d6fb79");
            throw new BuildException("Couldn't read checksum file " + f, e);
        } finally {
            writeline("/home/ubuntu/results/coverage/Checksum/Checksum_7_10.coverage", "fa381384-c204-44a8-aca5-e5db1a33521f");
            FileUtils.close(diskChecksumReader);
        }
    }

    /**
     * @since Ant 1.8.2
     */
    private String getRelativeFilePath(File f) {
        writeline("/home/ubuntu/results/coverage/Checksum/Checksum_7_10.coverage", "d48b71e0-e44e-4faf-8334-1edde049676d");
        String path = (String) relativeFilePaths.get(f);
        writeline("/home/ubuntu/results/coverage/Checksum/Checksum_7_10.coverage", "b757ddda-2a7e-4cef-8d7b-fc4b6cc5ffcc");
        if (path == null) {
            writeline("/home/ubuntu/results/coverage/Checksum/Checksum_7_10.coverage", "aecb9c87-4dba-4f87-a73e-bf02ea25d844");
            // bug 37386. this should not occur, but it has, once.
            throw new BuildException("Internal error: " + "relativeFilePaths could not match file " + f + "\n" + "please file a bug report on this");
        }
        writeline("/home/ubuntu/results/coverage/Checksum/Checksum_7_10.coverage", "1e0a938f-6e54-4b97-b745-88a5ac88e6b8");
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
