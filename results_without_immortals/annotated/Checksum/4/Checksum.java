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
        writeline("/home/ubuntu/results/coverage/Checksum/Checksum_4_10.coverage", "7dea5733-7311-4ead-baf2-80769fce977d");
        this.file = file;
    }

    /**
     * Sets the root directory where checksum files will be
     * written/read
     * @param todir the directory to write to
     * @since Ant 1.6
     */
    public void setTodir(File todir) {
        writeline("/home/ubuntu/results/coverage/Checksum/Checksum_4_10.coverage", "0be66913-7d10-400e-9259-3d764e8602dc");
        this.todir = todir;
    }

    /**
     * Specifies the algorithm to be used to compute the checksum.
     * Defaults to "MD5". Other popular algorithms like "SHA" may be used as well.
     * @param algorithm a <code>String</code> value
     */
    public void setAlgorithm(String algorithm) {
        writeline("/home/ubuntu/results/coverage/Checksum/Checksum_4_10.coverage", "4922cca4-c281-4295-b2a1-8bc86e59f65f");
        this.algorithm = algorithm;
    }

    /**
     * Sets the MessageDigest algorithm provider to be used
     * to calculate the checksum.
     * @param provider a <code>String</code> value
     */
    public void setProvider(String provider) {
        writeline("/home/ubuntu/results/coverage/Checksum/Checksum_4_10.coverage", "cdb16469-6daf-47fe-a89d-3535caf42e70");
        this.provider = provider;
    }

    /**
     * Sets the file extension that is be to used to
     * create or identify destination file.
     * @param fileext a <code>String</code> value
     */
    public void setFileext(String fileext) {
        writeline("/home/ubuntu/results/coverage/Checksum/Checksum_4_10.coverage", "69298cd7-6bd9-49b3-a4c8-30d32b6a08b3");
        this.fileext = fileext;
    }

    /**
     * Sets the property to hold the generated checksum.
     * @param property a <code>String</code> value
     */
    public void setProperty(String property) {
        writeline("/home/ubuntu/results/coverage/Checksum/Checksum_4_10.coverage", "35d2d69a-8e11-4d26-9a30-b2b463d147fb");
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
        writeline("/home/ubuntu/results/coverage/Checksum/Checksum_4_10.coverage", "f02bb201-2859-4756-b3c9-7f3ba4f8ce15");
        this.totalproperty = totalproperty;
    }

    /**
     * Sets the verify property.  This project property holds
     * the result of a checksum verification - "true" or "false"
     * @param verifyProperty a <code>String</code> value
     */
    public void setVerifyproperty(String verifyProperty) {
        writeline("/home/ubuntu/results/coverage/Checksum/Checksum_4_10.coverage", "b81d4e25-31a3-4b09-8aae-0ce0d17ede09");
        this.verifyProperty = verifyProperty;
    }

    /**
     * Whether or not to overwrite existing file irrespective of
     * whether it is newer than
     * the source file.  Defaults to false.
     * @param forceOverwrite a <code>boolean</code> value
     */
    public void setForceOverwrite(boolean forceOverwrite) {
        writeline("/home/ubuntu/results/coverage/Checksum/Checksum_4_10.coverage", "2a993fc4-970f-429d-88db-75a695f15d0c");
        this.forceOverwrite = forceOverwrite;
    }

    /**
     * The size of the read buffer to use.
     * @param size an <code>int</code> value
     */
    public void setReadBufferSize(int size) {
        writeline("/home/ubuntu/results/coverage/Checksum/Checksum_4_10.coverage", "aa5d4c0c-de08-4726-90a4-c7f31e036585");
        this.readBufferSize = size;
    }

    /**
     * Select the in/output pattern via a well know format name.
     * @param e an <code>enumerated</code> value
     *
     * @since 1.7.0
     */
    public void setFormat(FormatElement e) {
        writeline("/home/ubuntu/results/coverage/Checksum/Checksum_4_10.coverage", "1a0e361d-926c-4115-a3d1-9e1907673c7c");
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
        writeline("/home/ubuntu/results/coverage/Checksum/Checksum_4_10.coverage", "b2a4e2e7-473e-424c-8a80-f023e6ae9cdb");
        format = new MessageFormat(p);
    }

    /**
     * Files to generate checksums for.
     * @param set a fileset of files to generate checksums for.
     */
    public void addFileset(FileSet set) {
        writeline("/home/ubuntu/results/coverage/Checksum/Checksum_4_10.coverage", "95760905-6ac6-4a6d-89fc-8b23c14fc6c1");
        add(set);
    }

    /**
     * Add a resource collection.
     * @param rc the ResourceCollection to add.
     */
    public void add(ResourceCollection rc) {
        writeline("/home/ubuntu/results/coverage/Checksum/Checksum_4_10.coverage", "18721959-339f-4c08-8a6e-a86e7f6db38c");
        if (rc == null) {
            writeline("/home/ubuntu/results/coverage/Checksum/Checksum_4_10.coverage", "e0213e9b-40ec-4106-b7ea-f2b336652631");
            return;
        }
        writeline("/home/ubuntu/results/coverage/Checksum/Checksum_4_10.coverage", "b31973e6-0c7e-444e-ae30-f7362ea6132e");
        resources = (resources == null) ? new FileUnion() : resources;
        writeline("/home/ubuntu/results/coverage/Checksum/Checksum_4_10.coverage", "f19039cb-fa15-4204-a6a0-b3b8074502ed");
        resources.add(rc);
    }

    /**
     * Calculate the checksum(s).
     * @throws BuildException on error
     */
    public void execute() throws BuildException {
        writeline("/home/ubuntu/results/coverage/Checksum/Checksum_4_10.coverage", "16fb3fd3-e0c5-4d6c-82bd-68e613305685");
        isCondition = false;
        writeline("/home/ubuntu/results/coverage/Checksum/Checksum_4_10.coverage", "8b6e22dc-4616-411e-acd6-03ed2e1a0bd0");
        boolean value = validateAndExecute();
        writeline("/home/ubuntu/results/coverage/Checksum/Checksum_4_10.coverage", "bd993d30-38a4-446f-ae6c-896a545aacdd");
        if (verifyProperty != null) {
            writeline("/home/ubuntu/results/coverage/Checksum/Checksum_4_10.coverage", "10b0d8d7-909a-4476-8378-ec01d1523f4c");
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
        writeline("/home/ubuntu/results/coverage/Checksum/Checksum_4_10.coverage", "c05eed54-6aec-4960-ba9a-df5df59e352c");
        isCondition = true;
        writeline("/home/ubuntu/results/coverage/Checksum/Checksum_4_10.coverage", "f91fc08f-1472-4b59-bf6f-4646eb01eec9");
        return validateAndExecute();
    }

    /**
     * Validate attributes and get down to business.
     */
    private boolean validateAndExecute() throws BuildException {
        writeline("/home/ubuntu/results/coverage/Checksum/Checksum_4_10.coverage", "f4cef18a-01be-4c4f-b2ef-c60da2125987");
        String savedFileExt = fileext;
        writeline("/home/ubuntu/results/coverage/Checksum/Checksum_4_10.coverage", "46b385b8-bc75-4efb-bb64-f4284a5d38bc");
        if (file == null && (resources == null || resources.size() == 0)) {
            writeline("/home/ubuntu/results/coverage/Checksum/Checksum_4_10.coverage", "9b24ef04-6dc0-4e9c-a6e1-ed03239f55be");
            throw new BuildException("Specify at least one source - a file or a resource collection.");
        }
        writeline("/home/ubuntu/results/coverage/Checksum/Checksum_4_10.coverage", "ee7272d0-d95e-44c7-b11c-e1f587403043");
        if (!(resources == null || resources.isFilesystemOnly())) {
            writeline("/home/ubuntu/results/coverage/Checksum/Checksum_4_10.coverage", "5eed3822-394b-4638-9544-d642a29a2b1d");
            throw new BuildException("Can only calculate checksums for file-based resources.");
        }
        writeline("/home/ubuntu/results/coverage/Checksum/Checksum_4_10.coverage", "8ce3bc9d-1dd8-4e11-88a7-70f9dad1d298");
        if (file != null && file.exists() && file.isDirectory()) {
            writeline("/home/ubuntu/results/coverage/Checksum/Checksum_4_10.coverage", "67f731cc-899e-4be7-9611-6bf235b2faff");
            throw new BuildException("Checksum cannot be generated for directories");
        }
        writeline("/home/ubuntu/results/coverage/Checksum/Checksum_4_10.coverage", "3f7d21dc-920b-490c-ac46-9f0094026820");
        if (file != null && totalproperty != null) {
            writeline("/home/ubuntu/results/coverage/Checksum/Checksum_4_10.coverage", "8db2d4fb-a01f-47a9-9c1a-2f1cd6402b27");
            throw new BuildException("File and Totalproperty cannot co-exist.");
        }
        writeline("/home/ubuntu/results/coverage/Checksum/Checksum_4_10.coverage", "a1b1611f-2bf0-4f4d-a7af-4dd6c3fdfa74");
        if (property != null && fileext != null) {
            writeline("/home/ubuntu/results/coverage/Checksum/Checksum_4_10.coverage", "acd71cd0-efcf-4b18-92d4-e2300acf4aee");
            throw new BuildException("Property and FileExt cannot co-exist.");
        }
        writeline("/home/ubuntu/results/coverage/Checksum/Checksum_4_10.coverage", "0d980aac-3dd4-420c-9ab1-2615c4c4071d");
        if (property != null) {
            writeline("/home/ubuntu/results/coverage/Checksum/Checksum_4_10.coverage", "e452798f-dbc0-47d5-96e1-485e91a24a9f");
            if (forceOverwrite) {
                writeline("/home/ubuntu/results/coverage/Checksum/Checksum_4_10.coverage", "bb6ad30c-e2c0-463e-9312-e6d707ae8b1a");
                throw new BuildException("ForceOverwrite cannot be used when Property is specified");
            }
            writeline("/home/ubuntu/results/coverage/Checksum/Checksum_4_10.coverage", "2625094d-a688-4ca1-be6f-7a8fd673c689");
            int ct = 0;
            writeline("/home/ubuntu/results/coverage/Checksum/Checksum_4_10.coverage", "1da0b26b-51f8-4f94-bc6c-2c7a9ac8a873");
            if (resources != null) {
                writeline("/home/ubuntu/results/coverage/Checksum/Checksum_4_10.coverage", "c5b5a125-fdb7-4ee5-9b5c-812e98db074e");
                ct += resources.size();
            }
            writeline("/home/ubuntu/results/coverage/Checksum/Checksum_4_10.coverage", "ad7e59ba-70c9-45e1-8b87-eb6032c5b025");
            if (file != null) {
                writeline("/home/ubuntu/results/coverage/Checksum/Checksum_4_10.coverage", "c5f4305e-cbe8-4ad0-a526-666d99d3777f");
                ct++;
            }
            writeline("/home/ubuntu/results/coverage/Checksum/Checksum_4_10.coverage", "12847163-7a12-4b80-a235-3ae8a80e3924");
            if (ct > 1) {
                writeline("/home/ubuntu/results/coverage/Checksum/Checksum_4_10.coverage", "94b56e40-9a25-42a9-9739-d01a88f14c18");
                throw new BuildException("Multiple files cannot be used when Property is specified");
            }
        }
        writeline("/home/ubuntu/results/coverage/Checksum/Checksum_4_10.coverage", "3828c14f-5673-4521-a909-3359e5618e86");
        if (verifyProperty != null) {
            writeline("/home/ubuntu/results/coverage/Checksum/Checksum_4_10.coverage", "96d06970-7163-4bf3-914f-60722bc86d92");
            isCondition = true;
        }
        writeline("/home/ubuntu/results/coverage/Checksum/Checksum_4_10.coverage", "75bf29a2-8e60-4564-984a-95e604303de7");
        if (verifyProperty != null && forceOverwrite) {
            writeline("/home/ubuntu/results/coverage/Checksum/Checksum_4_10.coverage", "eb05b0d5-6ac0-445c-8e07-fc0e425fcd35");
            throw new BuildException("VerifyProperty and ForceOverwrite cannot co-exist.");
        }
        writeline("/home/ubuntu/results/coverage/Checksum/Checksum_4_10.coverage", "90d3f133-cd59-4574-8824-41f10d52d9ad");
        if (isCondition && forceOverwrite) {
            writeline("/home/ubuntu/results/coverage/Checksum/Checksum_4_10.coverage", "b4fa5001-868f-4ea5-9df7-47dbbbc9eba7");
            throw new BuildException("ForceOverwrite cannot be used when conditions are being used.");
        }
        writeline("/home/ubuntu/results/coverage/Checksum/Checksum_4_10.coverage", "88e54338-810b-4d92-a25a-019a4b723a8d");
        messageDigest = null;
        writeline("/home/ubuntu/results/coverage/Checksum/Checksum_4_10.coverage", "f84d4f75-9506-4e4c-b47e-8f03e4409c80");
        if (provider != null) {
            writeline("/home/ubuntu/results/coverage/Checksum/Checksum_4_10.coverage", "4e9800bb-0122-4ef4-9991-9f658f821009");
            try {
                writeline("/home/ubuntu/results/coverage/Checksum/Checksum_4_10.coverage", "822394d1-6922-4082-ae14-26e1679d9494");
                messageDigest = MessageDigest.getInstance(algorithm, provider);
            } catch (NoSuchAlgorithmException noalgo) {
                writeline("/home/ubuntu/results/coverage/Checksum/Checksum_4_10.coverage", "42eca37e-bafe-4ea2-92f0-5768f96dc88b");
                throw new BuildException(noalgo, getLocation());
            } catch (NoSuchProviderException noprovider) {
                writeline("/home/ubuntu/results/coverage/Checksum/Checksum_4_10.coverage", "6629105e-54ad-4e20-84c7-df9a46723791");
                throw new BuildException(noprovider, getLocation());
            }
        } else {
            writeline("/home/ubuntu/results/coverage/Checksum/Checksum_4_10.coverage", "c258029d-3dd8-4332-9f1b-d028af068464");
            try {
                writeline("/home/ubuntu/results/coverage/Checksum/Checksum_4_10.coverage", "62f39472-1c95-42c4-9b2d-6e81e9135f8a");
                messageDigest = MessageDigest.getInstance(algorithm);
            } catch (NoSuchAlgorithmException noalgo) {
                writeline("/home/ubuntu/results/coverage/Checksum/Checksum_4_10.coverage", "5de940ce-baf5-4b22-84ff-a6efd6ef02e7");
                throw new BuildException(noalgo, getLocation());
            }
        }
        writeline("/home/ubuntu/results/coverage/Checksum/Checksum_4_10.coverage", "44a86bfd-3c0f-4ca5-a733-6628df203839");
        if (messageDigest == null) {
            writeline("/home/ubuntu/results/coverage/Checksum/Checksum_4_10.coverage", "d382816f-9d51-40aa-8650-9d6a0098610f");
            throw new BuildException("Unable to create Message Digest", getLocation());
        }
        writeline("/home/ubuntu/results/coverage/Checksum/Checksum_4_10.coverage", "8dce5cd6-e7fe-4a0a-bca3-3607a4915318");
        if (fileext == null) {
            writeline("/home/ubuntu/results/coverage/Checksum/Checksum_4_10.coverage", "a821d94c-6cd2-4426-ad88-e6d09588fffc");
            fileext = "." + algorithm;
        } else if (fileext.trim().length() == 0) {
            writeline("/home/ubuntu/results/coverage/Checksum/Checksum_4_10.coverage", "634e204f-d0fa-47c3-a382-785ff8d969b9");
            throw new BuildException("File extension when specified must not be an empty string");
        }
        writeline("/home/ubuntu/results/coverage/Checksum/Checksum_4_10.coverage", "ca0483f4-ad88-450c-8970-9d11e95f3d74");
        try {
            writeline("/home/ubuntu/results/coverage/Checksum/Checksum_4_10.coverage", "efeeef4f-74fa-4e59-8af8-5127d2493f84");
            if (resources != null) {
                writeline("/home/ubuntu/results/coverage/Checksum/Checksum_4_10.coverage", "b3e7d670-9d34-48e6-ae1b-24aad53b1948");
                for (Resource r : resources) {
                    writeline("/home/ubuntu/results/coverage/Checksum/Checksum_4_10.coverage", "59104fcb-f8ec-4d21-b279-1cd72cb75519");
                    File src = r.as(FileProvider.class).getFile();
                    writeline("/home/ubuntu/results/coverage/Checksum/Checksum_4_10.coverage", "3dfde102-b171-4e41-86b0-8651a4060835");
                    if (totalproperty != null || todir != null) {
                        writeline("/home/ubuntu/results/coverage/Checksum/Checksum_4_10.coverage", "50dabc9e-fdd1-4745-b510-da08e212c9d8");
                        // Use '/' to calculate digest based on file name.
                        // This is required in order to get the same result
                        // on different platforms.
                        relativeFilePaths.put(src, r.getName().replace(File.separatorChar, '/'));
                    }
                    writeline("/home/ubuntu/results/coverage/Checksum/Checksum_4_10.coverage", "0906ab3b-1e55-43f3-8a0d-b763304b09ca");
                    addToIncludeFileMap(src);
                }
            }
            writeline("/home/ubuntu/results/coverage/Checksum/Checksum_4_10.coverage", "1ac58107-fda3-4f76-9a35-8756413560d0");
            if (file != null) {
                writeline("/home/ubuntu/results/coverage/Checksum/Checksum_4_10.coverage", "51a3c507-5dac-4c10-95e8-a273995f9007");
                if (totalproperty != null || todir != null) {
                    writeline("/home/ubuntu/results/coverage/Checksum/Checksum_4_10.coverage", "faf062ef-4774-4ce0-b293-2b54bd7110b9");
                    relativeFilePaths.put(file, file.getName().replace(File.separatorChar, '/'));
                }
                writeline("/home/ubuntu/results/coverage/Checksum/Checksum_4_10.coverage", "f49be3ea-0e15-4d35-844b-b304e5824929");
                addToIncludeFileMap(file);
            }
            writeline("/home/ubuntu/results/coverage/Checksum/Checksum_4_10.coverage", "c2c9d75f-caa3-4c91-b422-b15b9154cf4e");
            return generateChecksums();
        } finally {
            writeline("/home/ubuntu/results/coverage/Checksum/Checksum_4_10.coverage", "0332efa1-4884-4cda-9778-8637f7eda7db");
            fileext = savedFileExt;
            writeline("/home/ubuntu/results/coverage/Checksum/Checksum_4_10.coverage", "6406af4b-e6d1-47cf-98a2-50d70cf690b9");
            includeFileMap.clear();
        }
    }

    /**
     * Add key-value pair to the hashtable upon which
     * to later operate upon.
     */
    private void addToIncludeFileMap(File file) throws BuildException {
        writeline("/home/ubuntu/results/coverage/Checksum/Checksum_4_10.coverage", "9f4cdcfc-e62b-4ea8-887c-5fe88275b439");
        if (file.exists()) {
            writeline("/home/ubuntu/results/coverage/Checksum/Checksum_4_10.coverage", "fab2aa78-301b-4662-a7cc-591b27938a0f");
            if (property == null) {
                writeline("/home/ubuntu/results/coverage/Checksum/Checksum_4_10.coverage", "2c67125f-e751-4d1d-bea9-95cf048cf669");
                File checksumFile = getChecksumFile(file);
                writeline("/home/ubuntu/results/coverage/Checksum/Checksum_4_10.coverage", "36dd6f4e-83f2-49fd-b9bf-ee51a9e3ae53");
                if (forceOverwrite || isCondition || (file.lastModified() > checksumFile.lastModified())) {
                    writeline("/home/ubuntu/results/coverage/Checksum/Checksum_4_10.coverage", "8cd44813-0671-44d5-865b-2fe4d4be0623");
                    includeFileMap.put(file, checksumFile);
                } else {
                    writeline("/home/ubuntu/results/coverage/Checksum/Checksum_4_10.coverage", "24402af5-a559-481f-a721-5bfa929ec186");
                    log(file + " omitted as " + checksumFile + " is up to date.", Project.MSG_VERBOSE);
                    writeline("/home/ubuntu/results/coverage/Checksum/Checksum_4_10.coverage", "2983da64-e697-4c2a-b827-11d71c68061e");
                    if (totalproperty != null) {
                        writeline("/home/ubuntu/results/coverage/Checksum/Checksum_4_10.coverage", "c1e32ab7-677e-45e9-be6a-3bd1e6aded6e");
                        // Read the checksum from disk.
                        String checksum = readChecksum(checksumFile);
                        writeline("/home/ubuntu/results/coverage/Checksum/Checksum_4_10.coverage", "25215d73-f1be-4550-b7f0-cc57a33fa159");
                        byte[] digest = decodeHex(checksum.toCharArray());
                        writeline("/home/ubuntu/results/coverage/Checksum/Checksum_4_10.coverage", "7b9e0257-0dea-49a1-a261-9b8e64d09544");
                        allDigests.put(file, digest);
                    }
                }
            } else {
                writeline("/home/ubuntu/results/coverage/Checksum/Checksum_4_10.coverage", "41b2cae7-4411-487a-a503-ad8a6979070b");
                includeFileMap.put(file, property);
            }
        } else {
            writeline("/home/ubuntu/results/coverage/Checksum/Checksum_4_10.coverage", "98f02736-f0ca-43e8-a996-fab593daca6c");
            String message = "Could not find file " + file.getAbsolutePath() + " to generate checksum for.";
            writeline("/home/ubuntu/results/coverage/Checksum/Checksum_4_10.coverage", "c2e4798d-56d6-4bb6-a1c0-37c7179acdec");
            log(message);
            writeline("/home/ubuntu/results/coverage/Checksum/Checksum_4_10.coverage", "1c7fc732-cb1d-4831-8cb2-c105aec0f12b");
            throw new BuildException(message, getLocation());
        }
    }

    private File getChecksumFile(File file) {
        writeline("/home/ubuntu/results/coverage/Checksum/Checksum_4_10.coverage", "7cfa2982-b940-418a-9d43-8cc0587728ea");
        File directory;
        writeline("/home/ubuntu/results/coverage/Checksum/Checksum_4_10.coverage", "a2f60f38-2ca8-4e4f-a74a-ccc05047e570");
        if (todir != null) {
            writeline("/home/ubuntu/results/coverage/Checksum/Checksum_4_10.coverage", "544cf622-09e1-4456-98fb-381f73881cfc");
            // A separate directory was explicitly declared
            String path = getRelativeFilePath(file);
            writeline("/home/ubuntu/results/coverage/Checksum/Checksum_4_10.coverage", "732474ea-b109-410f-b76f-f7c58be78553");
            directory = new File(todir, path).getParentFile();
            writeline("/home/ubuntu/results/coverage/Checksum/Checksum_4_10.coverage", "98ad3d31-d132-4892-ba48-9f43113d9aad");
            // Create the directory, as it might not exist.
            directory.mkdirs();
        } else {
            writeline("/home/ubuntu/results/coverage/Checksum/Checksum_4_10.coverage", "d9abb6aa-4b4e-4327-9f2c-bb1b9c7cd654");
            // Just use the same directory as the file itself.
            // This directory will exist
            directory = file.getParentFile();
        }
        writeline("/home/ubuntu/results/coverage/Checksum/Checksum_4_10.coverage", "04eb15c6-0dce-4e4e-8249-ea5b417e4b55");
        File checksumFile = new File(directory, file.getName() + fileext);
        writeline("/home/ubuntu/results/coverage/Checksum/Checksum_4_10.coverage", "9550f5cb-3942-48e9-8276-ff777c00925f");
        return checksumFile;
    }

    /**
     * Generate checksum(s) using the message digest created earlier.
     */
    private boolean generateChecksums() throws BuildException {
        writeline("/home/ubuntu/results/coverage/Checksum/Checksum_4_10.coverage", "92f78ffe-8853-46ae-b7cd-efae60d920a3");
        boolean checksumMatches = true;
        writeline("/home/ubuntu/results/coverage/Checksum/Checksum_4_10.coverage", "32218a89-836a-45d3-961a-e109b05b9edf");
        FileInputStream fis = null;
        writeline("/home/ubuntu/results/coverage/Checksum/Checksum_4_10.coverage", "ab3676e0-aebc-4d6b-b2a7-665499cd7f49");
        FileOutputStream fos = null;
        writeline("/home/ubuntu/results/coverage/Checksum/Checksum_4_10.coverage", "653d2b1e-4eec-4034-910e-13cf0b544568");
        byte[] buf = new byte[readBufferSize];
        writeline("/home/ubuntu/results/coverage/Checksum/Checksum_4_10.coverage", "831db6ef-5558-41bd-9da3-9e63976a5b14");
        try {
            writeline("/home/ubuntu/results/coverage/Checksum/Checksum_4_10.coverage", "f2c65390-d473-431e-88b1-f887eda4da2d");
            for (Map.Entry<File, Object> e : includeFileMap.entrySet()) {
                writeline("/home/ubuntu/results/coverage/Checksum/Checksum_4_10.coverage", "449cc284-da0b-4c3b-94ad-19c6220b1430");
                messageDigest.reset();
                writeline("/home/ubuntu/results/coverage/Checksum/Checksum_4_10.coverage", "4542eeef-7d7c-4734-84d2-cde05a2d835b");
                File src = e.getKey();
                writeline("/home/ubuntu/results/coverage/Checksum/Checksum_4_10.coverage", "17b172ff-0f52-421f-bc7b-6314a579b26f");
                if (!isCondition) {
                    writeline("/home/ubuntu/results/coverage/Checksum/Checksum_4_10.coverage", "2bf563be-a1e4-4a02-a515-c593452fe2e4");
                    log("Calculating " + algorithm + " checksum for " + src, Project.MSG_VERBOSE);
                }
                writeline("/home/ubuntu/results/coverage/Checksum/Checksum_4_10.coverage", "7e3a03b0-de97-4be9-9677-d2a2087d8516");
                fis = new FileInputStream(src);
                writeline("/home/ubuntu/results/coverage/Checksum/Checksum_4_10.coverage", "9e4a30a7-fe11-458b-a6a1-a5a55c0059f4");
                DigestInputStream dis = new DigestInputStream(fis, messageDigest);
                writeline("/home/ubuntu/results/coverage/Checksum/Checksum_4_10.coverage", "810968dd-479c-43a3-a857-4a777881859b");
                while (dis.read(buf, 0, readBufferSize) != -1) {
                // Empty statement
                }
                writeline("/home/ubuntu/results/coverage/Checksum/Checksum_4_10.coverage", "603e512d-dbf1-4364-a7e4-926711b8eb94");
                dis.close();
                writeline("/home/ubuntu/results/coverage/Checksum/Checksum_4_10.coverage", "a9c8dd11-2a7b-4f19-ba7e-6c17d9114503");
                fis.close();
                writeline("/home/ubuntu/results/coverage/Checksum/Checksum_4_10.coverage", "6f578f08-4191-4422-941c-c1bc3d3740cb");
                fis = null;
                writeline("/home/ubuntu/results/coverage/Checksum/Checksum_4_10.coverage", "64ff90e3-7368-40d7-a2fe-6e9c37eca8d0");
                byte[] fileDigest = messageDigest.digest();
                writeline("/home/ubuntu/results/coverage/Checksum/Checksum_4_10.coverage", "3d41679d-8471-40f4-bba6-2f716cdea0f0");
                if (totalproperty != null) {
                    writeline("/home/ubuntu/results/coverage/Checksum/Checksum_4_10.coverage", "08f4cff0-cc51-49b5-8c17-5eafc1f62344");
                    allDigests.put(src, fileDigest);
                }
                writeline("/home/ubuntu/results/coverage/Checksum/Checksum_4_10.coverage", "e9575bd4-2049-4a4f-a879-b4460bb102ae");
                String checksum = createDigestString(fileDigest);
                writeline("/home/ubuntu/results/coverage/Checksum/Checksum_4_10.coverage", "ffe8c472-3596-400b-947c-f2de6c80ba5f");
                // can either be a property name string or a file
                Object destination = e.getValue();
                writeline("/home/ubuntu/results/coverage/Checksum/Checksum_4_10.coverage", "6ba71cc1-b9e4-435d-aa03-44ab0165f379");
                if (destination instanceof java.lang.String) {
                    writeline("/home/ubuntu/results/coverage/Checksum/Checksum_4_10.coverage", "e1493c7d-1ee4-492a-aad3-5957d3522602");
                    String prop = (String) destination;
                    writeline("/home/ubuntu/results/coverage/Checksum/Checksum_4_10.coverage", "7ab0f53e-ebb9-42d5-a190-95c4d7885564");
                    if (isCondition) {
                        writeline("/home/ubuntu/results/coverage/Checksum/Checksum_4_10.coverage", "43cc2d12-1367-4dd8-90ce-c19235393b05");
                        checksumMatches = checksumMatches && checksum.equals(property);
                    } else {
                        writeline("/home/ubuntu/results/coverage/Checksum/Checksum_4_10.coverage", "d578fb6a-aa59-42f1-bf4c-fbf6aa161c9f");
                        getProject().setNewProperty(prop, checksum);
                    }
                } else if (destination instanceof java.io.File) {
                    writeline("/home/ubuntu/results/coverage/Checksum/Checksum_4_10.coverage", "037826eb-cb56-4835-9a9a-45d87e77fd6e");
                    if (isCondition) {
                        writeline("/home/ubuntu/results/coverage/Checksum/Checksum_4_10.coverage", "1b805161-780d-4d3d-b949-35f95445b336");
                        File existingFile = (File) destination;
                        writeline("/home/ubuntu/results/coverage/Checksum/Checksum_4_10.coverage", "4e2ffd70-1f24-42a4-a995-1ce9004afd19");
                        if (existingFile.exists()) {
                            writeline("/home/ubuntu/results/coverage/Checksum/Checksum_4_10.coverage", "33ea64e5-8901-4fce-ad87-6c07e6a05caf");
                            try {
                                writeline("/home/ubuntu/results/coverage/Checksum/Checksum_4_10.coverage", "dd9ba011-35a6-4827-a4d3-83fd5751d367");
                                String suppliedChecksum = readChecksum(existingFile);
                                writeline("/home/ubuntu/results/coverage/Checksum/Checksum_4_10.coverage", "62c2848b-eebf-4039-b5bb-588049b6d3f8");
                                checksumMatches = checksumMatches && checksum.equals(suppliedChecksum);
                            } catch (BuildException be) {
                                writeline("/home/ubuntu/results/coverage/Checksum/Checksum_4_10.coverage", "018f05c0-616b-469c-ab4c-892da6a44faa");
                                // file is on wrong format, swallow
                                checksumMatches = false;
                            }
                        } else {
                            writeline("/home/ubuntu/results/coverage/Checksum/Checksum_4_10.coverage", "348ba368-41dc-4c7c-8ec0-0b5abbbd6364");
                            checksumMatches = false;
                        }
                    } else {
                        writeline("/home/ubuntu/results/coverage/Checksum/Checksum_4_10.coverage", "d5616075-176d-490a-9000-b3f430d763d3");
                        File dest = (File) destination;
                        writeline("/home/ubuntu/results/coverage/Checksum/Checksum_4_10.coverage", "0203a409-ae60-41aa-a387-d156e56f1b16");
                        fos = new FileOutputStream(dest);
                        writeline("/home/ubuntu/results/coverage/Checksum/Checksum_4_10.coverage", "0e4948a1-dcc5-4c4b-84f1-8297d0a50201");
                        fos.write(format.format(new Object[] { checksum, src.getName(), FileUtils.getRelativePath(dest.getParentFile(), src), FileUtils.getRelativePath(getProject().getBaseDir(), src), src.getAbsolutePath() }).getBytes());
                        writeline("/home/ubuntu/results/coverage/Checksum/Checksum_4_10.coverage", "9542ceb7-7960-40b5-b3f4-0211db695938");
                        fos.write(StringUtils.LINE_SEP.getBytes());
                        writeline("/home/ubuntu/results/coverage/Checksum/Checksum_4_10.coverage", "ef867910-54c2-40b5-b0dd-46e1aa7be18a");
                        fos.close();
                        writeline("/home/ubuntu/results/coverage/Checksum/Checksum_4_10.coverage", "8a8e37bb-fe9c-4c21-91ca-c55fe673d384");
                        fos = null;
                    }
                }
            }
            writeline("/home/ubuntu/results/coverage/Checksum/Checksum_4_10.coverage", "c9cd15f3-e7e7-42f8-af42-a8f8853a4b69");
            if (totalproperty != null) {
                writeline("/home/ubuntu/results/coverage/Checksum/Checksum_4_10.coverage", "a8dbcfac-81c6-4ded-8cb6-2a2f6417600c");
                // Calculate the total checksum
                // Convert the keys (source files) into a sorted array.
                File[] keyArray = allDigests.keySet().toArray(new File[allDigests.size()]);
                writeline("/home/ubuntu/results/coverage/Checksum/Checksum_4_10.coverage", "c9154540-aeec-409f-bf39-d4e70446fb88");
                // File is Comparable, but sort-order is platform
                // dependent (case-insensitive on Windows)
                Arrays.sort(keyArray, new Comparator<File>() {

                    public int compare(File f1, File f2) {
                        return f1 == null ? (f2 == null ? 0 : -1) : (f2 == null ? 1 : getRelativeFilePath(f1).compareTo(getRelativeFilePath(f2)));
                    }
                });
                writeline("/home/ubuntu/results/coverage/Checksum/Checksum_4_10.coverage", "3d98c075-ce9a-4893-8c7a-61947b265920");
                // Loop over the checksums and generate a total hash.
                messageDigest.reset();
                writeline("/home/ubuntu/results/coverage/Checksum/Checksum_4_10.coverage", "4872bd95-99b6-4a5e-90e5-1e9ac0973f5f");
                for (File src : keyArray) {
                    writeline("/home/ubuntu/results/coverage/Checksum/Checksum_4_10.coverage", "d15a56a9-2c63-46fd-b5ea-42b0f746ff22");
                    // Add the digest for the file content
                    byte[] digest = allDigests.get(src);
                    writeline("/home/ubuntu/results/coverage/Checksum/Checksum_4_10.coverage", "cf878beb-6020-4110-a74d-56e53ec00791");
                    messageDigest.update(digest);
                    writeline("/home/ubuntu/results/coverage/Checksum/Checksum_4_10.coverage", "bc326159-3178-4fa5-8d88-24648308d217");
                    // Add the file path
                    String fileName = getRelativeFilePath(src);
                    writeline("/home/ubuntu/results/coverage/Checksum/Checksum_4_10.coverage", "fb1a221a-8a6d-46f0-a7e8-b3d74f6125fa");
                    messageDigest.update(fileName.getBytes());
                }
                writeline("/home/ubuntu/results/coverage/Checksum/Checksum_4_10.coverage", "07e1bca2-d27a-42c5-8632-10f00dbc7939");
                String totalChecksum = createDigestString(messageDigest.digest());
                writeline("/home/ubuntu/results/coverage/Checksum/Checksum_4_10.coverage", "c3ce425a-bbcd-4257-98db-166107522ca0");
                getProject().setNewProperty(totalproperty, totalChecksum);
            }
        } catch (Exception e) {
            writeline("/home/ubuntu/results/coverage/Checksum/Checksum_4_10.coverage", "32281579-4e06-41be-af4f-585e3eb91ef7");
            throw new BuildException(e, getLocation());
        } finally {
            writeline("/home/ubuntu/results/coverage/Checksum/Checksum_4_10.coverage", "425e4a2d-79de-4a52-814b-4277549b8d6e");
            FileUtils.close(fis);
            writeline("/home/ubuntu/results/coverage/Checksum/Checksum_4_10.coverage", "313d9b58-75d9-4b41-9575-4555c46d4f55");
            FileUtils.close(fos);
        }
        writeline("/home/ubuntu/results/coverage/Checksum/Checksum_4_10.coverage", "b38faa95-532c-4a15-93b1-b231312cfc0c");
        return checksumMatches;
    }

    private String createDigestString(byte[] fileDigest) {
        writeline("/home/ubuntu/results/coverage/Checksum/Checksum_4_10.coverage", "f7b6ac18-a572-42af-929f-87fc7915447e");
        StringBuffer checksumSb = new StringBuffer();
        writeline("/home/ubuntu/results/coverage/Checksum/Checksum_4_10.coverage", "ef4b7130-5601-43c1-bd2e-96df1754b1eb");
        for (int i = 0; i < fileDigest.length; i++) {
            writeline("/home/ubuntu/results/coverage/Checksum/Checksum_4_10.coverage", "717dc198-d896-41b6-84e5-e221469f8230");
            String hexStr = Integer.toHexString(BYTE_MASK & fileDigest[i]);
            writeline("/home/ubuntu/results/coverage/Checksum/Checksum_4_10.coverage", "c26eebff-f3a4-430e-bdfb-72afb66a5f36");
            if (hexStr.length() < 2) {
                writeline("/home/ubuntu/results/coverage/Checksum/Checksum_4_10.coverage", "7c08a4a7-e1f3-407d-aea0-6b373d582bd4");
                checksumSb.append("0");
            }
            writeline("/home/ubuntu/results/coverage/Checksum/Checksum_4_10.coverage", "531d907b-6947-433f-ad09-841a37759768");
            checksumSb.append(hexStr);
        }
        writeline("/home/ubuntu/results/coverage/Checksum/Checksum_4_10.coverage", "474be7d3-3f58-4f8a-ba52-7d1e0538b259");
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
        writelineStatic("/home/ubuntu/results/coverage/Checksum/Checksum_4_10.coverage", "677e9621-0713-497e-a7f3-2b5c63f9f481");
        int l = data.length;
        writelineStatic("/home/ubuntu/results/coverage/Checksum/Checksum_4_10.coverage", "29dbc4ae-7e46-4b3f-b45a-29a12f663b71");
        if ((l & 0x01) != 0) {
            writelineStatic("/home/ubuntu/results/coverage/Checksum/Checksum_4_10.coverage", "2be2a061-8658-49ba-90cc-c3d2016d42e7");
            throw new BuildException("odd number of characters.");
        }
        writelineStatic("/home/ubuntu/results/coverage/Checksum/Checksum_4_10.coverage", "69f34712-7f8f-4bd3-a577-2717d0aaf420");
        byte[] out = new byte[l >> 1];
        writelineStatic("/home/ubuntu/results/coverage/Checksum/Checksum_4_10.coverage", "69f21802-5186-4947-92e5-4a66d54fe1bc");
        // two characters form the hex value.
        for (int i = 0, j = 0; j < l; i++) {
            writelineStatic("/home/ubuntu/results/coverage/Checksum/Checksum_4_10.coverage", "b4ef3160-efef-427c-8114-463547f9e4e7");
            int f = Character.digit(data[j++], WORD) << NIBBLE;
            writelineStatic("/home/ubuntu/results/coverage/Checksum/Checksum_4_10.coverage", "a1360ae2-ba2b-486c-8df9-249aa8f082ec");
            f = f | Character.digit(data[j++], WORD);
            writelineStatic("/home/ubuntu/results/coverage/Checksum/Checksum_4_10.coverage", "1475dd07-7f61-460d-9def-5fd50ffeb0be");
            out[i] = (byte) (f & BYTE_MASK);
        }
        writelineStatic("/home/ubuntu/results/coverage/Checksum/Checksum_4_10.coverage", "a2b73889-ff70-49ff-a325-fc98a5325920");
        return out;
    }

    /**
     * reads the checksum from a file using the specified format.
     *
     * @since 1.7
     */
    private String readChecksum(File f) {
        writeline("/home/ubuntu/results/coverage/Checksum/Checksum_4_10.coverage", "8c7f4da7-0e7d-4d0d-b7c9-31981ca78f3e");
        BufferedReader diskChecksumReader = null;
        writeline("/home/ubuntu/results/coverage/Checksum/Checksum_4_10.coverage", "6e8389c3-9d2f-4f15-94ae-0cbd91e5f076");
        try {
            writeline("/home/ubuntu/results/coverage/Checksum/Checksum_4_10.coverage", "d3ed54df-69cc-4828-bf00-0ba730ff9a43");
            diskChecksumReader = new BufferedReader(new FileReader(f));
            writeline("/home/ubuntu/results/coverage/Checksum/Checksum_4_10.coverage", "db86cb3b-065d-4777-961e-7381f2c5ac04");
            Object[] result = format.parse(diskChecksumReader.readLine());
            writeline("/home/ubuntu/results/coverage/Checksum/Checksum_4_10.coverage", "b220e0dd-5411-4781-846a-fbe8d4df9a9a");
            if (result == null || result.length == 0 || result[0] == null) {
                writeline("/home/ubuntu/results/coverage/Checksum/Checksum_4_10.coverage", "929e116b-9a7a-4e43-89fd-7c6b76129829");
                throw new BuildException("failed to find a checksum");
            }
            writeline("/home/ubuntu/results/coverage/Checksum/Checksum_4_10.coverage", "abc1a627-b2b5-4ccd-951b-80efe8a8f684");
            return (String) result[0];
        } catch (IOException e) {
            writeline("/home/ubuntu/results/coverage/Checksum/Checksum_4_10.coverage", "3c6febf8-51b0-484b-881e-4358c1dc5768");
            throw new BuildException("Couldn't read checksum file " + f, e);
        } catch (ParseException e) {
            writeline("/home/ubuntu/results/coverage/Checksum/Checksum_4_10.coverage", "ba06e1bb-6d25-4419-ab5e-d8d6c8a65d81");
            throw new BuildException("Couldn't read checksum file " + f, e);
        } finally {
            writeline("/home/ubuntu/results/coverage/Checksum/Checksum_4_10.coverage", "2c3d3464-d4d6-45d1-acee-e3af2d1779b5");
            FileUtils.close(diskChecksumReader);
        }
    }

    /**
     * @since Ant 1.8.2
     */
    private String getRelativeFilePath(File f) {
        writeline("/home/ubuntu/results/coverage/Checksum/Checksum_4_10.coverage", "cca93f61-0d23-4cb8-bc20-a43637056420");
        String path = (String) relativeFilePaths.get(f);
        writeline("/home/ubuntu/results/coverage/Checksum/Checksum_4_10.coverage", "301cd768-b668-4423-aa17-1b3074d61dad");
        if (path == null) {
            writeline("/home/ubuntu/results/coverage/Checksum/Checksum_4_10.coverage", "75543ca3-b5ee-47e0-b64b-4735c2ed0274");
            // bug 37386. this should not occur, but it has, once.
            throw new BuildException("Internal error: " + "relativeFilePaths could not match file " + f + "\n" + "please file a bug report on this");
        }
        writeline("/home/ubuntu/results/coverage/Checksum/Checksum_4_10.coverage", "c245d49a-c15b-4644-be3a-488e34568589");
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
