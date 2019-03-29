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
        writeline("/home/ubuntu/results/coverage/Checksum/Checksum_6_10.coverage", "1c6eb7a8-3605-4503-9716-2d3db9fe2e78");
        this.file = file;
    }

    /**
     * Sets the root directory where checksum files will be
     * written/read
     * @param todir the directory to write to
     * @since Ant 1.6
     */
    public void setTodir(File todir) {
        writeline("/home/ubuntu/results/coverage/Checksum/Checksum_6_10.coverage", "93340dbe-afc0-42e1-960e-e21cb0bfbc1e");
        this.todir = todir;
    }

    /**
     * Specifies the algorithm to be used to compute the checksum.
     * Defaults to "MD5". Other popular algorithms like "SHA" may be used as well.
     * @param algorithm a <code>String</code> value
     */
    public void setAlgorithm(String algorithm) {
        writeline("/home/ubuntu/results/coverage/Checksum/Checksum_6_10.coverage", "045fcaaf-5164-46a9-a879-a3680c3509f5");
        this.algorithm = algorithm;
    }

    /**
     * Sets the MessageDigest algorithm provider to be used
     * to calculate the checksum.
     * @param provider a <code>String</code> value
     */
    public void setProvider(String provider) {
        writeline("/home/ubuntu/results/coverage/Checksum/Checksum_6_10.coverage", "0c665eac-4a06-4a2c-8b23-60557613e077");
        this.provider = provider;
    }

    /**
     * Sets the file extension that is be to used to
     * create or identify destination file.
     * @param fileext a <code>String</code> value
     */
    public void setFileext(String fileext) {
        writeline("/home/ubuntu/results/coverage/Checksum/Checksum_6_10.coverage", "f3b2072f-c85c-4891-8c2b-cc389a69442c");
        this.fileext = fileext;
    }

    /**
     * Sets the property to hold the generated checksum.
     * @param property a <code>String</code> value
     */
    public void setProperty(String property) {
        writeline("/home/ubuntu/results/coverage/Checksum/Checksum_6_10.coverage", "a6393897-ad5b-41ef-9c63-ffadcabdea32");
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
        writeline("/home/ubuntu/results/coverage/Checksum/Checksum_6_10.coverage", "47a5c543-2aa0-4a9b-bf24-543a687bf980");
        this.totalproperty = totalproperty;
    }

    /**
     * Sets the verify property.  This project property holds
     * the result of a checksum verification - "true" or "false"
     * @param verifyProperty a <code>String</code> value
     */
    public void setVerifyproperty(String verifyProperty) {
        writeline("/home/ubuntu/results/coverage/Checksum/Checksum_6_10.coverage", "58eacb0d-f46d-4532-bf25-db96ab3492c1");
        this.verifyProperty = verifyProperty;
    }

    /**
     * Whether or not to overwrite existing file irrespective of
     * whether it is newer than
     * the source file.  Defaults to false.
     * @param forceOverwrite a <code>boolean</code> value
     */
    public void setForceOverwrite(boolean forceOverwrite) {
        writeline("/home/ubuntu/results/coverage/Checksum/Checksum_6_10.coverage", "f9ef5b89-569e-4081-894b-03f2be4217e8");
        this.forceOverwrite = forceOverwrite;
    }

    /**
     * The size of the read buffer to use.
     * @param size an <code>int</code> value
     */
    public void setReadBufferSize(int size) {
        writeline("/home/ubuntu/results/coverage/Checksum/Checksum_6_10.coverage", "a6ad5a66-437c-4e5b-a0b3-11aecd100cd3");
        this.readBufferSize = size;
    }

    /**
     * Select the in/output pattern via a well know format name.
     * @param e an <code>enumerated</code> value
     *
     * @since 1.7.0
     */
    public void setFormat(FormatElement e) {
        writeline("/home/ubuntu/results/coverage/Checksum/Checksum_6_10.coverage", "700a18d1-5961-470b-96c6-c968255613da");
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
        writeline("/home/ubuntu/results/coverage/Checksum/Checksum_6_10.coverage", "18a20d33-01cb-4929-ae01-b754e35e3ff4");
        format = new MessageFormat(p);
    }

    /**
     * Files to generate checksums for.
     * @param set a fileset of files to generate checksums for.
     */
    public void addFileset(FileSet set) {
        writeline("/home/ubuntu/results/coverage/Checksum/Checksum_6_10.coverage", "2c87c578-1ccb-4f61-befe-cc6b52e61e4f");
        add(set);
    }

    /**
     * Add a resource collection.
     * @param rc the ResourceCollection to add.
     */
    public void add(ResourceCollection rc) {
        writeline("/home/ubuntu/results/coverage/Checksum/Checksum_6_10.coverage", "1323db07-9c49-4ad6-bf82-718cde2df840");
        if (rc == null) {
            writeline("/home/ubuntu/results/coverage/Checksum/Checksum_6_10.coverage", "c0b74bb3-0cb3-4826-b218-3e0a66909176");
            return;
        }
        writeline("/home/ubuntu/results/coverage/Checksum/Checksum_6_10.coverage", "ad36492b-c7d8-4057-910c-2c7a4274b4c7");
        resources = (resources == null) ? new FileUnion() : resources;
        writeline("/home/ubuntu/results/coverage/Checksum/Checksum_6_10.coverage", "fa4e7549-439d-451b-9140-6ed6060fd270");
        resources.add(rc);
    }

    /**
     * Calculate the checksum(s).
     * @throws BuildException on error
     */
    public void execute() throws BuildException {
        writeline("/home/ubuntu/results/coverage/Checksum/Checksum_6_10.coverage", "36977579-998c-4190-986d-ee7e2b9d2f38");
        isCondition = false;
        writeline("/home/ubuntu/results/coverage/Checksum/Checksum_6_10.coverage", "0e68effe-6166-4397-b705-ed175fcc973e");
        boolean value = validateAndExecute();
        writeline("/home/ubuntu/results/coverage/Checksum/Checksum_6_10.coverage", "e626b06c-f248-4f44-8720-4f997f4f5e3f");
        if (verifyProperty != null) {
            writeline("/home/ubuntu/results/coverage/Checksum/Checksum_6_10.coverage", "228fcfaa-7e5a-4df7-90ca-e031ac5aef02");
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
        writeline("/home/ubuntu/results/coverage/Checksum/Checksum_6_10.coverage", "d9d4a392-ce0c-4138-9ae9-8ec0b79333f9");
        isCondition = true;
        writeline("/home/ubuntu/results/coverage/Checksum/Checksum_6_10.coverage", "14550a95-9a83-4eda-a6fd-a3f4d1b74513");
        return validateAndExecute();
    }

    /**
     * Validate attributes and get down to business.
     */
    private boolean validateAndExecute() throws BuildException {
        writeline("/home/ubuntu/results/coverage/Checksum/Checksum_6_10.coverage", "1b994304-dc94-43c0-bb45-c4d1e6c1ace9");
        String savedFileExt = fileext;
        writeline("/home/ubuntu/results/coverage/Checksum/Checksum_6_10.coverage", "603f5f14-931b-41df-b8f7-929afbfc749c");
        if (file == null && (resources == null || resources.size() == 0)) {
            writeline("/home/ubuntu/results/coverage/Checksum/Checksum_6_10.coverage", "f3305ecc-781a-490c-a0c5-14630493bfc0");
            throw new BuildException("Specify at least one source - a file or a resource collection.");
        }
        writeline("/home/ubuntu/results/coverage/Checksum/Checksum_6_10.coverage", "298ca0c9-73c1-4f27-9b98-33b07ac37d37");
        if (!(resources == null || resources.isFilesystemOnly())) {
            writeline("/home/ubuntu/results/coverage/Checksum/Checksum_6_10.coverage", "1eed6abc-c79e-4688-a376-d6f0eb7ddc76");
            throw new BuildException("Can only calculate checksums for file-based resources.");
        }
        writeline("/home/ubuntu/results/coverage/Checksum/Checksum_6_10.coverage", "6f7e1d72-38b3-44fe-84ce-d3548076bea3");
        if (file != null && file.exists() && file.isDirectory()) {
            writeline("/home/ubuntu/results/coverage/Checksum/Checksum_6_10.coverage", "7c07cbcd-3066-49ff-a99e-38b942304a98");
            throw new BuildException("Checksum cannot be generated for directories");
        }
        writeline("/home/ubuntu/results/coverage/Checksum/Checksum_6_10.coverage", "c0a0d671-9e44-4a56-8ce5-2955c1260521");
        if (file != null && totalproperty != null) {
            writeline("/home/ubuntu/results/coverage/Checksum/Checksum_6_10.coverage", "b8b3c466-2f4f-4957-9014-468079d74945");
            throw new BuildException("File and Totalproperty cannot co-exist.");
        }
        writeline("/home/ubuntu/results/coverage/Checksum/Checksum_6_10.coverage", "9f5624a0-41d9-4231-a3a3-d159c4311360");
        if (property != null && fileext != null) {
            writeline("/home/ubuntu/results/coverage/Checksum/Checksum_6_10.coverage", "291d3577-a789-4c39-8732-2ca987892212");
            throw new BuildException("Property and FileExt cannot co-exist.");
        }
        writeline("/home/ubuntu/results/coverage/Checksum/Checksum_6_10.coverage", "35d69a3f-a0e5-4f79-b4f8-b118053e681a");
        if (property != null) {
            writeline("/home/ubuntu/results/coverage/Checksum/Checksum_6_10.coverage", "136e61bb-5b18-4383-9026-591d0944f9a7");
            if (forceOverwrite) {
                writeline("/home/ubuntu/results/coverage/Checksum/Checksum_6_10.coverage", "611b59ac-a986-4e0e-8970-ccc3f9bb20a0");
                throw new BuildException("ForceOverwrite cannot be used when Property is specified");
            }
            writeline("/home/ubuntu/results/coverage/Checksum/Checksum_6_10.coverage", "3c6b7d44-5c8c-49c1-8bb4-274d9c6631c8");
            int ct = 0;
            writeline("/home/ubuntu/results/coverage/Checksum/Checksum_6_10.coverage", "96681a00-eccb-4645-af4d-b1a9593cf3ef");
            if (resources != null) {
                writeline("/home/ubuntu/results/coverage/Checksum/Checksum_6_10.coverage", "c5078da2-bc38-4934-b8c3-eb0f08c6275a");
                ct += resources.size();
            }
            writeline("/home/ubuntu/results/coverage/Checksum/Checksum_6_10.coverage", "3f367c52-be11-4366-b06b-1aaa74d43b07");
            if (file != null) {
                writeline("/home/ubuntu/results/coverage/Checksum/Checksum_6_10.coverage", "65fdba9e-f25c-463c-981c-954fd9a1910a");
                ct++;
            }
            writeline("/home/ubuntu/results/coverage/Checksum/Checksum_6_10.coverage", "05277ed4-42bb-48b2-8898-72b88322bde2");
            if (ct > 1) {
                writeline("/home/ubuntu/results/coverage/Checksum/Checksum_6_10.coverage", "e8a72323-9116-4353-85e2-4fbb815e2c54");
                throw new BuildException("Multiple files cannot be used when Property is specified");
            }
        }
        writeline("/home/ubuntu/results/coverage/Checksum/Checksum_6_10.coverage", "085a2565-2c14-428f-960c-590d62caded4");
        if (verifyProperty != null) {
            writeline("/home/ubuntu/results/coverage/Checksum/Checksum_6_10.coverage", "2f3c6b81-2869-452f-b64f-ddafe185d77b");
            isCondition = true;
        }
        writeline("/home/ubuntu/results/coverage/Checksum/Checksum_6_10.coverage", "2edd5d03-ab86-4f93-a90a-487029ebe4db");
        if (verifyProperty != null && forceOverwrite) {
            writeline("/home/ubuntu/results/coverage/Checksum/Checksum_6_10.coverage", "28261650-5211-4b61-bf0f-817063bdd72d");
            throw new BuildException("VerifyProperty and ForceOverwrite cannot co-exist.");
        }
        writeline("/home/ubuntu/results/coverage/Checksum/Checksum_6_10.coverage", "abb547bc-70c2-4eb7-aa21-c2bd8d9352bd");
        if (isCondition && forceOverwrite) {
            writeline("/home/ubuntu/results/coverage/Checksum/Checksum_6_10.coverage", "fda5fd0c-08a5-45f7-8069-edae4c1bbdf6");
            throw new BuildException("ForceOverwrite cannot be used when conditions are being used.");
        }
        writeline("/home/ubuntu/results/coverage/Checksum/Checksum_6_10.coverage", "28cfbee8-a0b7-4b86-b861-48db64572fbf");
        messageDigest = null;
        writeline("/home/ubuntu/results/coverage/Checksum/Checksum_6_10.coverage", "c6eb4155-afed-4b8a-a8c4-80a22d1a3299");
        if (provider != null) {
            writeline("/home/ubuntu/results/coverage/Checksum/Checksum_6_10.coverage", "415e0a69-09a0-401e-8028-b089c27e73ae");
            try {
                writeline("/home/ubuntu/results/coverage/Checksum/Checksum_6_10.coverage", "b700c98a-cef9-4489-b053-9148fe8a07e8");
                messageDigest = MessageDigest.getInstance(algorithm, provider);
            } catch (NoSuchAlgorithmException noalgo) {
                writeline("/home/ubuntu/results/coverage/Checksum/Checksum_6_10.coverage", "51dd1622-d266-41e3-8bb2-08b9abf03db2");
                throw new BuildException(noalgo, getLocation());
            } catch (NoSuchProviderException noprovider) {
                writeline("/home/ubuntu/results/coverage/Checksum/Checksum_6_10.coverage", "7cb14ebe-44b7-413e-a884-412c6fd9d0b0");
                throw new BuildException(noprovider, getLocation());
            }
        } else {
            writeline("/home/ubuntu/results/coverage/Checksum/Checksum_6_10.coverage", "1baa82c4-e07b-4f4f-a212-cfe620634bc5");
            try {
                writeline("/home/ubuntu/results/coverage/Checksum/Checksum_6_10.coverage", "9f464fd0-c5de-4518-b03d-601b86ed0236");
                messageDigest = MessageDigest.getInstance(algorithm);
            } catch (NoSuchAlgorithmException noalgo) {
                writeline("/home/ubuntu/results/coverage/Checksum/Checksum_6_10.coverage", "954db4fe-ba78-4371-95d1-a56775d6cef8");
                throw new BuildException(noalgo, getLocation());
            }
        }
        writeline("/home/ubuntu/results/coverage/Checksum/Checksum_6_10.coverage", "a8fae380-7a8d-4eb6-9a55-27640ddbd5e5");
        if (messageDigest == null) {
            writeline("/home/ubuntu/results/coverage/Checksum/Checksum_6_10.coverage", "fe25cee1-00a7-40dd-b34e-e3d0e17d794e");
            throw new BuildException("Unable to create Message Digest", getLocation());
        }
        writeline("/home/ubuntu/results/coverage/Checksum/Checksum_6_10.coverage", "6c85a7a6-ac96-48af-8e87-71a7327b7cc4");
        if (fileext == null) {
            writeline("/home/ubuntu/results/coverage/Checksum/Checksum_6_10.coverage", "a2dc087f-6bbf-4bee-b085-ec477a7ca687");
            fileext = "." + algorithm;
        } else if (fileext.trim().length() == 0) {
            writeline("/home/ubuntu/results/coverage/Checksum/Checksum_6_10.coverage", "6729c4bd-e8ee-4e03-9f7e-f5f81a63edbe");
            throw new BuildException("File extension when specified must not be an empty string");
        }
        writeline("/home/ubuntu/results/coverage/Checksum/Checksum_6_10.coverage", "57ef2f4a-e3b3-4536-a735-c02512528507");
        try {
            writeline("/home/ubuntu/results/coverage/Checksum/Checksum_6_10.coverage", "07b30df3-0a79-438b-b34c-d19ee7771514");
            if (resources != null) {
                writeline("/home/ubuntu/results/coverage/Checksum/Checksum_6_10.coverage", "e260d127-f7fc-4ca8-983a-96686693faca");
                for (Resource r : resources) {
                    writeline("/home/ubuntu/results/coverage/Checksum/Checksum_6_10.coverage", "7844ed39-b773-46fe-b562-cd7d0a6a0c48");
                    File src = r.as(FileProvider.class).getFile();
                    writeline("/home/ubuntu/results/coverage/Checksum/Checksum_6_10.coverage", "f3f96382-ab58-4d30-8147-98c06392fb06");
                    if (totalproperty != null || todir != null) {
                        writeline("/home/ubuntu/results/coverage/Checksum/Checksum_6_10.coverage", "fec36843-b937-4ec7-ac07-7c9e34cffead");
                        // Use '/' to calculate digest based on file name.
                        // This is required in order to get the same result
                        // on different platforms.
                        relativeFilePaths.put(src, r.getName().replace(File.separatorChar, '/'));
                    }
                    writeline("/home/ubuntu/results/coverage/Checksum/Checksum_6_10.coverage", "1f57d024-c01c-4cf3-9a80-c83d281ef5f1");
                    addToIncludeFileMap(src);
                }
            }
            writeline("/home/ubuntu/results/coverage/Checksum/Checksum_6_10.coverage", "013559f8-bc8a-4919-9451-5887e71e4f6e");
            if (file != null) {
                writeline("/home/ubuntu/results/coverage/Checksum/Checksum_6_10.coverage", "710410c3-ff9d-4368-89f0-1ef7894ed252");
                if (totalproperty != null || todir != null) {
                    writeline("/home/ubuntu/results/coverage/Checksum/Checksum_6_10.coverage", "d00893a3-1218-46f1-b005-02934fde2699");
                    relativeFilePaths.put(file, file.getName().replace(File.separatorChar, '/'));
                }
                writeline("/home/ubuntu/results/coverage/Checksum/Checksum_6_10.coverage", "ba3805bd-6fd2-4b52-8a2f-b8644a25f2af");
                addToIncludeFileMap(file);
            }
            writeline("/home/ubuntu/results/coverage/Checksum/Checksum_6_10.coverage", "a60a1745-9a4d-424e-9bc7-db361f685424");
            return generateChecksums();
        } finally {
            writeline("/home/ubuntu/results/coverage/Checksum/Checksum_6_10.coverage", "df895a0e-745c-4edb-bc46-74d49bb8e15a");
            fileext = savedFileExt;
            writeline("/home/ubuntu/results/coverage/Checksum/Checksum_6_10.coverage", "ce8c3aaf-7b10-46d8-bf80-491a0caa72f4");
            includeFileMap.clear();
        }
    }

    /**
     * Add key-value pair to the hashtable upon which
     * to later operate upon.
     */
    private void addToIncludeFileMap(File file) throws BuildException {
        writeline("/home/ubuntu/results/coverage/Checksum/Checksum_6_10.coverage", "7ec913cc-371e-4da9-9548-0e31f121fb25");
        if (file.exists()) {
            writeline("/home/ubuntu/results/coverage/Checksum/Checksum_6_10.coverage", "2a90af70-834c-41ee-b4c5-0266991ab34f");
            if (property == null) {
                writeline("/home/ubuntu/results/coverage/Checksum/Checksum_6_10.coverage", "2c73ab62-9ba3-4c2c-af8f-fb7d2c9eee56");
                File checksumFile = getChecksumFile(file);
                writeline("/home/ubuntu/results/coverage/Checksum/Checksum_6_10.coverage", "f934ded0-c8fb-49be-ac97-ac881e4a6e65");
                if (forceOverwrite || isCondition || (file.lastModified() > checksumFile.lastModified())) {
                    writeline("/home/ubuntu/results/coverage/Checksum/Checksum_6_10.coverage", "b6bd7134-1bc6-4f29-bc57-cdbf3c4577b0");
                    includeFileMap.put(file, checksumFile);
                } else {
                    writeline("/home/ubuntu/results/coverage/Checksum/Checksum_6_10.coverage", "215fc520-63bd-470e-ac42-e4e49d826254");
                    log(file + " omitted as " + checksumFile + " is up to date.", Project.MSG_VERBOSE);
                    writeline("/home/ubuntu/results/coverage/Checksum/Checksum_6_10.coverage", "7d223958-74c6-47c9-a4f3-bed4c1c7bbad");
                    if (totalproperty != null) {
                        writeline("/home/ubuntu/results/coverage/Checksum/Checksum_6_10.coverage", "0996bb0c-75d9-4a0d-af27-ed6cfc466ded");
                        // Read the checksum from disk.
                        String checksum = readChecksum(checksumFile);
                        writeline("/home/ubuntu/results/coverage/Checksum/Checksum_6_10.coverage", "b3717b57-93c2-4397-bf86-70f6dfc8951b");
                        byte[] digest = decodeHex(checksum.toCharArray());
                        writeline("/home/ubuntu/results/coverage/Checksum/Checksum_6_10.coverage", "63032ef1-5fd5-4216-8d46-d3eabf886825");
                        allDigests.put(file, digest);
                    }
                }
            } else {
                writeline("/home/ubuntu/results/coverage/Checksum/Checksum_6_10.coverage", "f86461b0-e359-4548-b6bc-38e335f3e623");
                includeFileMap.put(file, property);
            }
        } else {
            writeline("/home/ubuntu/results/coverage/Checksum/Checksum_6_10.coverage", "034f3826-4b4c-4b67-9f6a-38c11ab717df");
            String message = "Could not find file " + file.getAbsolutePath() + " to generate checksum for.";
            writeline("/home/ubuntu/results/coverage/Checksum/Checksum_6_10.coverage", "6789e770-9f3d-467f-8932-be5d1292f111");
            log(message);
            writeline("/home/ubuntu/results/coverage/Checksum/Checksum_6_10.coverage", "f4cde1c7-c396-4e42-b5db-001e1b7fb4b9");
            throw new BuildException(message, getLocation());
        }
    }

    private File getChecksumFile(File file) {
        writeline("/home/ubuntu/results/coverage/Checksum/Checksum_6_10.coverage", "7f7f1e27-561b-4078-8ea4-10407712efb5");
        File directory;
        writeline("/home/ubuntu/results/coverage/Checksum/Checksum_6_10.coverage", "49e63d57-83bc-4fe5-9db1-210e29ff7bb9");
        if (todir != null) {
            writeline("/home/ubuntu/results/coverage/Checksum/Checksum_6_10.coverage", "464e956b-9d30-4d28-a509-b1b0243a03ac");
            // A separate directory was explicitly declared
            String path = getRelativeFilePath(file);
            writeline("/home/ubuntu/results/coverage/Checksum/Checksum_6_10.coverage", "60c46d96-589e-4f4c-9e1b-a916c31b518a");
            directory = new File(todir, path).getParentFile();
            writeline("/home/ubuntu/results/coverage/Checksum/Checksum_6_10.coverage", "dedb4652-c68f-4c52-b049-927fcd719e4c");
            // Create the directory, as it might not exist.
            directory.mkdirs();
        } else {
            writeline("/home/ubuntu/results/coverage/Checksum/Checksum_6_10.coverage", "a98cfe9b-3267-46e3-a442-8835085630d3");
            // Just use the same directory as the file itself.
            // This directory will exist
            directory = file.getParentFile();
        }
        writeline("/home/ubuntu/results/coverage/Checksum/Checksum_6_10.coverage", "af090087-1944-4235-b738-f1c987dea6e1");
        File checksumFile = new File(directory, file.getName() + fileext);
        writeline("/home/ubuntu/results/coverage/Checksum/Checksum_6_10.coverage", "426444a7-12f0-44db-aa02-e52a9a10b3a8");
        return checksumFile;
    }

    /**
     * Generate checksum(s) using the message digest created earlier.
     */
    private boolean generateChecksums() throws BuildException {
        writeline("/home/ubuntu/results/coverage/Checksum/Checksum_6_10.coverage", "55485273-05ac-461d-b5e0-20305032182f");
        boolean checksumMatches = true;
        writeline("/home/ubuntu/results/coverage/Checksum/Checksum_6_10.coverage", "1d55879b-b913-4918-a2d0-778db37ae0e3");
        FileInputStream fis = null;
        writeline("/home/ubuntu/results/coverage/Checksum/Checksum_6_10.coverage", "fb82ca27-b343-4f77-b1ee-3c35cef9d081");
        FileOutputStream fos = null;
        writeline("/home/ubuntu/results/coverage/Checksum/Checksum_6_10.coverage", "2e2a057a-f282-4100-aef6-b754cf442e70");
        byte[] buf = new byte[readBufferSize];
        writeline("/home/ubuntu/results/coverage/Checksum/Checksum_6_10.coverage", "8c870ae5-f26c-4afe-a430-ba6bdb7a8666");
        try {
            writeline("/home/ubuntu/results/coverage/Checksum/Checksum_6_10.coverage", "48ecc7a4-5eb4-44be-ac99-cd5b283e2c9b");
            for (Map.Entry<File, Object> e : includeFileMap.entrySet()) {
                writeline("/home/ubuntu/results/coverage/Checksum/Checksum_6_10.coverage", "c8d6c2c9-c110-4628-97e9-f5d2df2c0e91");
                messageDigest.reset();
                writeline("/home/ubuntu/results/coverage/Checksum/Checksum_6_10.coverage", "c639f4e0-69c9-4c02-84d7-c731bf4d70b9");
                File src = e.getKey();
                writeline("/home/ubuntu/results/coverage/Checksum/Checksum_6_10.coverage", "c4ded3d9-daeb-4091-ab39-961c1eb2ed48");
                if (!isCondition) {
                    writeline("/home/ubuntu/results/coverage/Checksum/Checksum_6_10.coverage", "39e09cef-94b7-4086-ab4c-59ff038f63c8");
                    log("Calculating " + algorithm + " checksum for " + src, Project.MSG_VERBOSE);
                }
                writeline("/home/ubuntu/results/coverage/Checksum/Checksum_6_10.coverage", "bc009b93-1152-4166-8357-0717ffcd16ff");
                fis = new FileInputStream(src);
                writeline("/home/ubuntu/results/coverage/Checksum/Checksum_6_10.coverage", "98ff21fc-b07a-4f0f-bd89-1142e4e21bc7");
                DigestInputStream dis = new DigestInputStream(fis, messageDigest);
                writeline("/home/ubuntu/results/coverage/Checksum/Checksum_6_10.coverage", "b2e1f91c-0cf6-45a8-9fdf-9f2520e0a921");
                while (dis.read(buf, 0, readBufferSize) != -1) {
                // Empty statement
                }
                writeline("/home/ubuntu/results/coverage/Checksum/Checksum_6_10.coverage", "5d8b7df5-6941-47ad-b255-f30bfeca953a");
                dis.close();
                writeline("/home/ubuntu/results/coverage/Checksum/Checksum_6_10.coverage", "9dae14b5-e228-4b17-b539-a36803bbd77f");
                fis.close();
                writeline("/home/ubuntu/results/coverage/Checksum/Checksum_6_10.coverage", "80527651-c5db-4d86-b82a-b1d0f52ab509");
                fis = null;
                writeline("/home/ubuntu/results/coverage/Checksum/Checksum_6_10.coverage", "7155fde3-5bd9-4f76-84aa-bcdfb96feb75");
                byte[] fileDigest = messageDigest.digest();
                writeline("/home/ubuntu/results/coverage/Checksum/Checksum_6_10.coverage", "54a5d9ba-c9ec-4ee0-ab94-628add367f69");
                if (totalproperty != null) {
                    writeline("/home/ubuntu/results/coverage/Checksum/Checksum_6_10.coverage", "99793681-7d81-4ef4-868d-8edfaf82437c");
                    allDigests.put(src, fileDigest);
                }
                writeline("/home/ubuntu/results/coverage/Checksum/Checksum_6_10.coverage", "e32d35b9-a017-49c8-8ecb-202ac5ef0d0a");
                String checksum = createDigestString(fileDigest);
                writeline("/home/ubuntu/results/coverage/Checksum/Checksum_6_10.coverage", "74bccfe7-a503-4cd8-9e20-d7cdfd8d024b");
                // can either be a property name string or a file
                Object destination = e.getValue();
                writeline("/home/ubuntu/results/coverage/Checksum/Checksum_6_10.coverage", "e668bd9c-0c37-4612-826b-c37b71a03a9a");
                if (destination instanceof java.lang.String) {
                    writeline("/home/ubuntu/results/coverage/Checksum/Checksum_6_10.coverage", "378e89a6-fb94-42c6-8c21-db07a2e26486");
                    String prop = (String) destination;
                    writeline("/home/ubuntu/results/coverage/Checksum/Checksum_6_10.coverage", "1bb9aa32-c0ea-4bb5-8404-332e8c62cb6a");
                    if (isCondition) {
                        writeline("/home/ubuntu/results/coverage/Checksum/Checksum_6_10.coverage", "4241d4f4-2d3a-4b57-9a9b-7894c63da16b");
                        checksumMatches = checksumMatches && checksum.equals(property);
                    } else {
                        writeline("/home/ubuntu/results/coverage/Checksum/Checksum_6_10.coverage", "efbd7b9c-ae79-4bf4-85b5-006539f1a113");
                        getProject().setNewProperty(prop, checksum);
                    }
                } else if (destination instanceof java.io.File) {
                    writeline("/home/ubuntu/results/coverage/Checksum/Checksum_6_10.coverage", "418b631d-d6ae-4547-92a5-100853521a1e");
                    if (isCondition) {
                        writeline("/home/ubuntu/results/coverage/Checksum/Checksum_6_10.coverage", "d7824dcf-178c-4977-b52f-7c75af5139a7");
                        File existingFile = (File) destination;
                        writeline("/home/ubuntu/results/coverage/Checksum/Checksum_6_10.coverage", "09252947-4f70-4834-9332-91bd6d3ee3c1");
                        if (existingFile.exists()) {
                            writeline("/home/ubuntu/results/coverage/Checksum/Checksum_6_10.coverage", "e0b0f66e-4248-4b8f-852d-f5f1add082b5");
                            try {
                                writeline("/home/ubuntu/results/coverage/Checksum/Checksum_6_10.coverage", "b3c49898-b9c2-4268-a360-687a05d05d20");
                                String suppliedChecksum = readChecksum(existingFile);
                                writeline("/home/ubuntu/results/coverage/Checksum/Checksum_6_10.coverage", "40110854-2352-4ef7-b471-c0de47d0897d");
                                checksumMatches = checksumMatches && checksum.equals(suppliedChecksum);
                            } catch (BuildException be) {
                                writeline("/home/ubuntu/results/coverage/Checksum/Checksum_6_10.coverage", "1feb50be-b04b-46fc-a9b6-bddef8eda848");
                                // file is on wrong format, swallow
                                checksumMatches = false;
                            }
                        } else {
                            writeline("/home/ubuntu/results/coverage/Checksum/Checksum_6_10.coverage", "a91888e7-c513-46fd-b866-2ab15381ac8d");
                            checksumMatches = false;
                        }
                    } else {
                        writeline("/home/ubuntu/results/coverage/Checksum/Checksum_6_10.coverage", "7c81a8fb-d84b-4e31-b144-ccc61b4eef8f");
                        File dest = (File) destination;
                        writeline("/home/ubuntu/results/coverage/Checksum/Checksum_6_10.coverage", "1c29c9a6-d64b-4bc3-91ac-2398a495cdf9");
                        fos = new FileOutputStream(dest);
                        writeline("/home/ubuntu/results/coverage/Checksum/Checksum_6_10.coverage", "ba54d075-1fea-402d-95dc-5dcc90dff821");
                        fos.write(format.format(new Object[] { checksum, src.getName(), FileUtils.getRelativePath(dest.getParentFile(), src), FileUtils.getRelativePath(getProject().getBaseDir(), src), src.getAbsolutePath() }).getBytes());
                        writeline("/home/ubuntu/results/coverage/Checksum/Checksum_6_10.coverage", "aa5d3f77-9467-4f8c-b361-3cd1e671aa49");
                        fos.write(StringUtils.LINE_SEP.getBytes());
                        writeline("/home/ubuntu/results/coverage/Checksum/Checksum_6_10.coverage", "1a867342-60bf-4a03-9570-560e99ae8bb0");
                        fos.close();
                        writeline("/home/ubuntu/results/coverage/Checksum/Checksum_6_10.coverage", "a38f1114-716d-4cf5-aaa8-f3c6ae23ae6a");
                        fos = null;
                    }
                }
            }
            writeline("/home/ubuntu/results/coverage/Checksum/Checksum_6_10.coverage", "38a94adf-a771-497b-9b54-06afc9ca1372");
            if (totalproperty != null) {
                writeline("/home/ubuntu/results/coverage/Checksum/Checksum_6_10.coverage", "8ab7edc9-ffb5-4989-a010-8a0b17a347f5");
                // Calculate the total checksum
                // Convert the keys (source files) into a sorted array.
                File[] keyArray = allDigests.keySet().toArray(new File[allDigests.size()]);
                writeline("/home/ubuntu/results/coverage/Checksum/Checksum_6_10.coverage", "600bf8a3-4672-48ab-9bb6-04949863148a");
                // File is Comparable, but sort-order is platform
                // dependent (case-insensitive on Windows)
                Arrays.sort(keyArray, new Comparator<File>() {

                    public int compare(File f1, File f2) {
                        return f1 == null ? (f2 == null ? 0 : -1) : (f2 == null ? 1 : getRelativeFilePath(f1).compareTo(getRelativeFilePath(f2)));
                    }
                });
                writeline("/home/ubuntu/results/coverage/Checksum/Checksum_6_10.coverage", "e4717bc6-34a0-41c8-bbef-2116e9527414");
                // Loop over the checksums and generate a total hash.
                messageDigest.reset();
                writeline("/home/ubuntu/results/coverage/Checksum/Checksum_6_10.coverage", "e7626e96-5cdb-4774-84fc-6c8dcff81663");
                for (File src : keyArray) {
                    writeline("/home/ubuntu/results/coverage/Checksum/Checksum_6_10.coverage", "3090b85a-ed0a-431c-8b75-5639e1d5807e");
                    // Add the digest for the file content
                    byte[] digest = allDigests.get(src);
                    writeline("/home/ubuntu/results/coverage/Checksum/Checksum_6_10.coverage", "2c5508d7-f2f3-4b6c-bdd1-6482f1d62c17");
                    messageDigest.update(digest);
                    writeline("/home/ubuntu/results/coverage/Checksum/Checksum_6_10.coverage", "2a9d80ad-9af4-4b74-b7eb-8ec5801f3d20");
                    // Add the file path
                    String fileName = getRelativeFilePath(src);
                    writeline("/home/ubuntu/results/coverage/Checksum/Checksum_6_10.coverage", "abd5dde2-b525-44d1-ac8b-f5ba0c6426d4");
                    messageDigest.update(fileName.getBytes());
                }
                writeline("/home/ubuntu/results/coverage/Checksum/Checksum_6_10.coverage", "66b9c803-9b65-4528-8169-043df5fe02eb");
                String totalChecksum = createDigestString(messageDigest.digest());
                writeline("/home/ubuntu/results/coverage/Checksum/Checksum_6_10.coverage", "794a865e-b326-4153-b90b-bb7b35dcebc4");
                getProject().setNewProperty(totalproperty, totalChecksum);
            }
        } catch (Exception e) {
            writeline("/home/ubuntu/results/coverage/Checksum/Checksum_6_10.coverage", "88371266-7e24-4cb3-b2f7-cc4ed4e7537f");
            throw new BuildException(e, getLocation());
        } finally {
            writeline("/home/ubuntu/results/coverage/Checksum/Checksum_6_10.coverage", "6a79962c-39e1-4ee6-91ab-8b9db567e684");
            FileUtils.close(fis);
            writeline("/home/ubuntu/results/coverage/Checksum/Checksum_6_10.coverage", "679c17e8-c349-484c-a3d3-453f714fbc5d");
            FileUtils.close(fos);
        }
        writeline("/home/ubuntu/results/coverage/Checksum/Checksum_6_10.coverage", "0fa46dc8-cd92-49c6-a14f-745847400c8a");
        return checksumMatches;
    }

    private String createDigestString(byte[] fileDigest) {
        writeline("/home/ubuntu/results/coverage/Checksum/Checksum_6_10.coverage", "d523bc33-91b1-476c-b992-1d6bbe3cad9f");
        StringBuffer checksumSb = new StringBuffer();
        writeline("/home/ubuntu/results/coverage/Checksum/Checksum_6_10.coverage", "3da8a5d2-dc49-498d-88df-4cc89cd55c70");
        for (int i = 0; i < fileDigest.length; i++) {
            writeline("/home/ubuntu/results/coverage/Checksum/Checksum_6_10.coverage", "0c1002d5-b9a1-4727-928d-001ecd46af0e");
            String hexStr = Integer.toHexString(BYTE_MASK & fileDigest[i]);
            writeline("/home/ubuntu/results/coverage/Checksum/Checksum_6_10.coverage", "b936e2fc-ca80-4dc0-b7cd-0544d165d475");
            if (hexStr.length() < 2) {
                writeline("/home/ubuntu/results/coverage/Checksum/Checksum_6_10.coverage", "3d9d42c6-e3ed-4e65-81ba-75d50223f631");
                checksumSb.append("0");
            }
            writeline("/home/ubuntu/results/coverage/Checksum/Checksum_6_10.coverage", "b10ba917-07cf-4bc0-8d9e-3fbf3016e700");
            checksumSb.append(hexStr);
        }
        writeline("/home/ubuntu/results/coverage/Checksum/Checksum_6_10.coverage", "e1e6eec1-63e3-4938-aa8e-9c33bb783400");
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
        writelineStatic("/home/ubuntu/results/coverage/Checksum/Checksum_6_10.coverage", "70250607-7161-48c6-b7a0-62fd5bec3080");
        int l = data.length;
        writelineStatic("/home/ubuntu/results/coverage/Checksum/Checksum_6_10.coverage", "c6f38815-2b24-4712-912c-27b25cfc38ae");
        if ((l & 0x01) != 0) {
            writelineStatic("/home/ubuntu/results/coverage/Checksum/Checksum_6_10.coverage", "91e1e558-59df-4da9-91dd-a535dd882dd7");
            throw new BuildException("odd number of characters.");
        }
        writelineStatic("/home/ubuntu/results/coverage/Checksum/Checksum_6_10.coverage", "0fbce225-2ac3-499f-9bf1-91965965cb37");
        byte[] out = new byte[l >> 1];
        writelineStatic("/home/ubuntu/results/coverage/Checksum/Checksum_6_10.coverage", "830cfa6e-f4d6-4259-90b1-01128c0a3b27");
        // two characters form the hex value.
        for (int i = 0, j = 0; j < l; i++) {
            writelineStatic("/home/ubuntu/results/coverage/Checksum/Checksum_6_10.coverage", "4e0c02dd-b319-4a42-85f1-7105d1c8a1eb");
            int f = Character.digit(data[j++], WORD) << NIBBLE;
            writelineStatic("/home/ubuntu/results/coverage/Checksum/Checksum_6_10.coverage", "dc72000b-f6cc-4849-971e-804228684575");
            f = f | Character.digit(data[j++], WORD);
            writelineStatic("/home/ubuntu/results/coverage/Checksum/Checksum_6_10.coverage", "b2caf15c-ff13-4cae-909f-fdcaee462136");
            out[i] = (byte) (f & BYTE_MASK);
        }
        writelineStatic("/home/ubuntu/results/coverage/Checksum/Checksum_6_10.coverage", "499b4927-8a95-496f-b9be-a51cf8069922");
        return out;
    }

    /**
     * reads the checksum from a file using the specified format.
     *
     * @since 1.7
     */
    private String readChecksum(File f) {
        writeline("/home/ubuntu/results/coverage/Checksum/Checksum_6_10.coverage", "e733efbd-561b-4669-bd80-1318ccb2ff4a");
        BufferedReader diskChecksumReader = null;
        writeline("/home/ubuntu/results/coverage/Checksum/Checksum_6_10.coverage", "889b4abe-80aa-410c-9aff-b1925301227e");
        try {
            writeline("/home/ubuntu/results/coverage/Checksum/Checksum_6_10.coverage", "678462c8-9883-4766-bdba-ca091789c3ea");
            diskChecksumReader = new BufferedReader(new FileReader(f));
            writeline("/home/ubuntu/results/coverage/Checksum/Checksum_6_10.coverage", "35ee54e3-dea7-4546-bc45-83371d3da037");
            Object[] result = format.parse(diskChecksumReader.readLine());
            writeline("/home/ubuntu/results/coverage/Checksum/Checksum_6_10.coverage", "343f4222-a16b-4905-80b4-c79e450c56f9");
            if (result == null || result.length == 0 || result[0] == null) {
                writeline("/home/ubuntu/results/coverage/Checksum/Checksum_6_10.coverage", "a605db34-e77c-4596-98f6-1e97a9902866");
                throw new BuildException("failed to find a checksum");
            }
            writeline("/home/ubuntu/results/coverage/Checksum/Checksum_6_10.coverage", "8a8565bf-9ab1-4f71-a85b-ade329418974");
            return (String) result[0];
        } catch (IOException e) {
            writeline("/home/ubuntu/results/coverage/Checksum/Checksum_6_10.coverage", "5c20b46c-40c2-45ef-a19c-9af54700824c");
            throw new BuildException("Couldn't read checksum file " + f, e);
        } catch (ParseException e) {
            writeline("/home/ubuntu/results/coverage/Checksum/Checksum_6_10.coverage", "90112a6b-ad33-4e27-a408-837445492d23");
            throw new BuildException("Couldn't read checksum file " + f, e);
        } finally {
            writeline("/home/ubuntu/results/coverage/Checksum/Checksum_6_10.coverage", "dc670b04-f1e5-4254-876c-4d3f439b5fbc");
            FileUtils.close(diskChecksumReader);
        }
    }

    /**
     * @since Ant 1.8.2
     */
    private String getRelativeFilePath(File f) {
        writeline("/home/ubuntu/results/coverage/Checksum/Checksum_6_10.coverage", "a20cbea4-4a78-4601-b542-5eb1206a4d2b");
        String path = (String) relativeFilePaths.get(f);
        writeline("/home/ubuntu/results/coverage/Checksum/Checksum_6_10.coverage", "0c872940-4304-4313-a8e3-c89fb71acec8");
        if (path == null) {
            writeline("/home/ubuntu/results/coverage/Checksum/Checksum_6_10.coverage", "7dabdfb7-8f94-4ade-ba4c-290506ba5432");
            // bug 37386. this should not occur, but it has, once.
            throw new BuildException("Internal error: " + "relativeFilePaths could not match file " + f + "\n" + "please file a bug report on this");
        }
        writeline("/home/ubuntu/results/coverage/Checksum/Checksum_6_10.coverage", "b97fcb2d-316b-4c12-9ec3-f8bfb15be908");
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
