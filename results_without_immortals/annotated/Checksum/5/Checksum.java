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
        writeline("/home/ubuntu/results/coverage/Checksum/Checksum_5_10.coverage", "f1e35aa6-ff50-4fea-a5b6-44f442f22baf");
        this.file = file;
    }

    /**
     * Sets the root directory where checksum files will be
     * written/read
     * @param todir the directory to write to
     * @since Ant 1.6
     */
    public void setTodir(File todir) {
        writeline("/home/ubuntu/results/coverage/Checksum/Checksum_5_10.coverage", "acae7244-70c6-4a4d-84ac-2f368ecbe964");
        this.todir = todir;
    }

    /**
     * Specifies the algorithm to be used to compute the checksum.
     * Defaults to "MD5". Other popular algorithms like "SHA" may be used as well.
     * @param algorithm a <code>String</code> value
     */
    public void setAlgorithm(String algorithm) {
        writeline("/home/ubuntu/results/coverage/Checksum/Checksum_5_10.coverage", "b8c769d8-3ad1-479f-881b-eb816b641728");
        this.algorithm = algorithm;
    }

    /**
     * Sets the MessageDigest algorithm provider to be used
     * to calculate the checksum.
     * @param provider a <code>String</code> value
     */
    public void setProvider(String provider) {
        writeline("/home/ubuntu/results/coverage/Checksum/Checksum_5_10.coverage", "e61a61b3-0a83-4a70-8537-b8db56cbba1e");
        this.provider = provider;
    }

    /**
     * Sets the file extension that is be to used to
     * create or identify destination file.
     * @param fileext a <code>String</code> value
     */
    public void setFileext(String fileext) {
        writeline("/home/ubuntu/results/coverage/Checksum/Checksum_5_10.coverage", "a4fa8380-5091-47f5-8185-cc6bc14b0b65");
        this.fileext = fileext;
    }

    /**
     * Sets the property to hold the generated checksum.
     * @param property a <code>String</code> value
     */
    public void setProperty(String property) {
        writeline("/home/ubuntu/results/coverage/Checksum/Checksum_5_10.coverage", "4097bac5-bc9b-438a-a135-614c2c8e76bb");
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
        writeline("/home/ubuntu/results/coverage/Checksum/Checksum_5_10.coverage", "b1d88b0c-f981-4f30-b632-a7042c36b414");
        this.totalproperty = totalproperty;
    }

    /**
     * Sets the verify property.  This project property holds
     * the result of a checksum verification - "true" or "false"
     * @param verifyProperty a <code>String</code> value
     */
    public void setVerifyproperty(String verifyProperty) {
        writeline("/home/ubuntu/results/coverage/Checksum/Checksum_5_10.coverage", "27ee7884-c736-4da6-bc85-3be8463b3ec9");
        this.verifyProperty = verifyProperty;
    }

    /**
     * Whether or not to overwrite existing file irrespective of
     * whether it is newer than
     * the source file.  Defaults to false.
     * @param forceOverwrite a <code>boolean</code> value
     */
    public void setForceOverwrite(boolean forceOverwrite) {
        writeline("/home/ubuntu/results/coverage/Checksum/Checksum_5_10.coverage", "397923c5-ce88-442c-8a34-d2fa1e1ce9e9");
        this.forceOverwrite = forceOverwrite;
    }

    /**
     * The size of the read buffer to use.
     * @param size an <code>int</code> value
     */
    public void setReadBufferSize(int size) {
        writeline("/home/ubuntu/results/coverage/Checksum/Checksum_5_10.coverage", "4056192c-b71a-4919-9606-b0a1874a5559");
        this.readBufferSize = size;
    }

    /**
     * Select the in/output pattern via a well know format name.
     * @param e an <code>enumerated</code> value
     *
     * @since 1.7.0
     */
    public void setFormat(FormatElement e) {
        writeline("/home/ubuntu/results/coverage/Checksum/Checksum_5_10.coverage", "7818698c-15b5-4a5b-a1b8-b383fa131bc3");
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
        writeline("/home/ubuntu/results/coverage/Checksum/Checksum_5_10.coverage", "8f047781-1149-4c08-8116-0c429f07d826");
        format = new MessageFormat(p);
    }

    /**
     * Files to generate checksums for.
     * @param set a fileset of files to generate checksums for.
     */
    public void addFileset(FileSet set) {
        writeline("/home/ubuntu/results/coverage/Checksum/Checksum_5_10.coverage", "36697fee-bb9b-4df6-8c58-a05f2daaebb8");
        add(set);
    }

    /**
     * Add a resource collection.
     * @param rc the ResourceCollection to add.
     */
    public void add(ResourceCollection rc) {
        writeline("/home/ubuntu/results/coverage/Checksum/Checksum_5_10.coverage", "76ebf7d3-dbff-41af-940b-b3a7e5b2b706");
        if (rc == null) {
            writeline("/home/ubuntu/results/coverage/Checksum/Checksum_5_10.coverage", "b88aac98-ab7a-471f-955f-b39af6eaf54b");
            return;
        }
        writeline("/home/ubuntu/results/coverage/Checksum/Checksum_5_10.coverage", "f664a4eb-7564-4487-9f38-5049ef28b725");
        resources = (resources == null) ? new FileUnion() : resources;
        writeline("/home/ubuntu/results/coverage/Checksum/Checksum_5_10.coverage", "4117bb30-a722-4d15-b92c-5b2b0d46c864");
        resources.add(rc);
    }

    /**
     * Calculate the checksum(s).
     * @throws BuildException on error
     */
    public void execute() throws BuildException {
        writeline("/home/ubuntu/results/coverage/Checksum/Checksum_5_10.coverage", "c3429212-e65e-4b36-9815-e5bbcd0805b7");
        isCondition = false;
        writeline("/home/ubuntu/results/coverage/Checksum/Checksum_5_10.coverage", "b39d7d44-e2da-46c9-9856-e0a1d76f2e02");
        boolean value = validateAndExecute();
        writeline("/home/ubuntu/results/coverage/Checksum/Checksum_5_10.coverage", "d1c49565-70cf-4404-9ab3-97a5d7d224f6");
        if (verifyProperty != null) {
            writeline("/home/ubuntu/results/coverage/Checksum/Checksum_5_10.coverage", "d00cd415-0fed-4266-86fa-8d26833fc4b6");
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
        writeline("/home/ubuntu/results/coverage/Checksum/Checksum_5_10.coverage", "9a1b14fa-6a52-4dc2-bf7a-6f2864399255");
        isCondition = true;
        writeline("/home/ubuntu/results/coverage/Checksum/Checksum_5_10.coverage", "39a9c876-0599-46d1-9a9e-7ff1f1bd2ab1");
        return validateAndExecute();
    }

    /**
     * Validate attributes and get down to business.
     */
    private boolean validateAndExecute() throws BuildException {
        writeline("/home/ubuntu/results/coverage/Checksum/Checksum_5_10.coverage", "0c46ca20-d64f-4161-a19a-15ec19e0bbd0");
        String savedFileExt = fileext;
        writeline("/home/ubuntu/results/coverage/Checksum/Checksum_5_10.coverage", "14a81b0e-463e-40c6-927a-5b52aa973157");
        if (file == null && (resources == null || resources.size() == 0)) {
            writeline("/home/ubuntu/results/coverage/Checksum/Checksum_5_10.coverage", "ebcb9db9-05cf-4584-9f7a-02b4d2ad2000");
            throw new BuildException("Specify at least one source - a file or a resource collection.");
        }
        writeline("/home/ubuntu/results/coverage/Checksum/Checksum_5_10.coverage", "74d60c1f-0814-4469-b8af-de5ff7397437");
        if (!(resources == null || resources.isFilesystemOnly())) {
            writeline("/home/ubuntu/results/coverage/Checksum/Checksum_5_10.coverage", "1cefd7fa-c097-43f7-9701-c1fd009ff192");
            throw new BuildException("Can only calculate checksums for file-based resources.");
        }
        writeline("/home/ubuntu/results/coverage/Checksum/Checksum_5_10.coverage", "6a2ed7f4-6efa-40a1-8328-5eb88cae33d9");
        if (file != null && file.exists() && file.isDirectory()) {
            writeline("/home/ubuntu/results/coverage/Checksum/Checksum_5_10.coverage", "5509da45-1f65-4cff-95fc-358921fe6697");
            throw new BuildException("Checksum cannot be generated for directories");
        }
        writeline("/home/ubuntu/results/coverage/Checksum/Checksum_5_10.coverage", "804a7e69-605e-4265-90c8-5c431895a1e4");
        if (file != null && totalproperty != null) {
            writeline("/home/ubuntu/results/coverage/Checksum/Checksum_5_10.coverage", "a152ac62-ad9b-475a-9d94-da2f87a6778e");
            throw new BuildException("File and Totalproperty cannot co-exist.");
        }
        writeline("/home/ubuntu/results/coverage/Checksum/Checksum_5_10.coverage", "93474c02-ad34-4dfd-a066-b3588a5f3b1d");
        if (property != null && fileext != null) {
            writeline("/home/ubuntu/results/coverage/Checksum/Checksum_5_10.coverage", "1251131a-a31c-476d-9344-c93854833549");
            throw new BuildException("Property and FileExt cannot co-exist.");
        }
        writeline("/home/ubuntu/results/coverage/Checksum/Checksum_5_10.coverage", "06fde1ca-47a1-411f-8f56-2c5c162e3e23");
        if (property != null) {
            writeline("/home/ubuntu/results/coverage/Checksum/Checksum_5_10.coverage", "f91a13f8-0d0c-45b5-8d84-c47fb03f1c1b");
            if (forceOverwrite) {
                writeline("/home/ubuntu/results/coverage/Checksum/Checksum_5_10.coverage", "3ea649fe-015a-4aca-9932-235af6e0cf82");
                throw new BuildException("ForceOverwrite cannot be used when Property is specified");
            }
            writeline("/home/ubuntu/results/coverage/Checksum/Checksum_5_10.coverage", "3cc459fa-8fe6-4150-a4ee-a38b0dc6a51e");
            int ct = 0;
            writeline("/home/ubuntu/results/coverage/Checksum/Checksum_5_10.coverage", "9744f6e6-4650-4f93-a40a-4883f55262e0");
            if (resources != null) {
                writeline("/home/ubuntu/results/coverage/Checksum/Checksum_5_10.coverage", "cb36bb09-17df-4c0e-b4a6-588026901341");
                ct += resources.size();
            }
            writeline("/home/ubuntu/results/coverage/Checksum/Checksum_5_10.coverage", "199aaea1-8e1b-4e09-888d-2a3e300cc8f5");
            if (file != null) {
                writeline("/home/ubuntu/results/coverage/Checksum/Checksum_5_10.coverage", "322a8a5b-b1ed-4192-a9a0-a2b19342df68");
                ct++;
            }
            writeline("/home/ubuntu/results/coverage/Checksum/Checksum_5_10.coverage", "08415a40-6030-4ba6-ba33-306f989501f6");
            if (ct > 1) {
                writeline("/home/ubuntu/results/coverage/Checksum/Checksum_5_10.coverage", "17baf6c5-21f0-47ca-a49f-53233be13afd");
                throw new BuildException("Multiple files cannot be used when Property is specified");
            }
        }
        writeline("/home/ubuntu/results/coverage/Checksum/Checksum_5_10.coverage", "c9400764-7a3a-4970-8dee-f4a832e49a59");
        if (verifyProperty != null) {
            writeline("/home/ubuntu/results/coverage/Checksum/Checksum_5_10.coverage", "a314a013-a6e7-47f0-8adf-c4d1b3481e48");
            isCondition = true;
        }
        writeline("/home/ubuntu/results/coverage/Checksum/Checksum_5_10.coverage", "0d5d084d-b9d4-4068-a963-e9bf1f5a6a28");
        if (verifyProperty != null && forceOverwrite) {
            writeline("/home/ubuntu/results/coverage/Checksum/Checksum_5_10.coverage", "1e6e8f3c-102f-4662-872b-42b0afc34f15");
            throw new BuildException("VerifyProperty and ForceOverwrite cannot co-exist.");
        }
        writeline("/home/ubuntu/results/coverage/Checksum/Checksum_5_10.coverage", "6b6d1bfe-0c0a-4771-9ab8-98efd47067da");
        if (isCondition && forceOverwrite) {
            writeline("/home/ubuntu/results/coverage/Checksum/Checksum_5_10.coverage", "f65b7851-1c4a-4632-b42c-23f977493a57");
            throw new BuildException("ForceOverwrite cannot be used when conditions are being used.");
        }
        writeline("/home/ubuntu/results/coverage/Checksum/Checksum_5_10.coverage", "f1ba5a7b-d6b8-47e3-b7cb-aeac5f951752");
        messageDigest = null;
        writeline("/home/ubuntu/results/coverage/Checksum/Checksum_5_10.coverage", "6290b41b-6936-407f-875a-cbfdf0774bf4");
        if (provider != null) {
            writeline("/home/ubuntu/results/coverage/Checksum/Checksum_5_10.coverage", "891649d5-504f-46f7-8c58-25f2fa7a1265");
            try {
                writeline("/home/ubuntu/results/coverage/Checksum/Checksum_5_10.coverage", "51bfc48f-978a-4acc-a3c3-fb672db3aa0f");
                messageDigest = MessageDigest.getInstance(algorithm, provider);
            } catch (NoSuchAlgorithmException noalgo) {
                writeline("/home/ubuntu/results/coverage/Checksum/Checksum_5_10.coverage", "586cd845-026c-4531-8537-2f76d7666566");
                throw new BuildException(noalgo, getLocation());
            } catch (NoSuchProviderException noprovider) {
                writeline("/home/ubuntu/results/coverage/Checksum/Checksum_5_10.coverage", "cfbc266d-5e40-4457-820c-49f0e4d50279");
                throw new BuildException(noprovider, getLocation());
            }
        } else {
            writeline("/home/ubuntu/results/coverage/Checksum/Checksum_5_10.coverage", "9370a6c4-f623-4c2d-9dd0-310f78aec2f9");
            try {
                writeline("/home/ubuntu/results/coverage/Checksum/Checksum_5_10.coverage", "dd91202e-9b31-4324-82bc-9c7875413732");
                messageDigest = MessageDigest.getInstance(algorithm);
            } catch (NoSuchAlgorithmException noalgo) {
                writeline("/home/ubuntu/results/coverage/Checksum/Checksum_5_10.coverage", "033ba832-a0e5-4d58-81ab-8f2b1e1a8888");
                throw new BuildException(noalgo, getLocation());
            }
        }
        writeline("/home/ubuntu/results/coverage/Checksum/Checksum_5_10.coverage", "958f0e71-0a91-4577-8af6-ab5297f20e28");
        if (messageDigest == null) {
            writeline("/home/ubuntu/results/coverage/Checksum/Checksum_5_10.coverage", "72f3698a-5bc7-4ddc-b3fc-7a800e71f108");
            throw new BuildException("Unable to create Message Digest", getLocation());
        }
        writeline("/home/ubuntu/results/coverage/Checksum/Checksum_5_10.coverage", "7d3bb5b3-7a52-413e-b4c6-0ebb9cd1cd5b");
        if (fileext == null) {
            writeline("/home/ubuntu/results/coverage/Checksum/Checksum_5_10.coverage", "6e780b3b-0245-4a1d-adaf-2bf27c207810");
            fileext = "." + algorithm;
        } else if (fileext.trim().length() == 0) {
            writeline("/home/ubuntu/results/coverage/Checksum/Checksum_5_10.coverage", "ad432792-2323-4b72-a9a6-c8153cf9c0d4");
            throw new BuildException("File extension when specified must not be an empty string");
        }
        writeline("/home/ubuntu/results/coverage/Checksum/Checksum_5_10.coverage", "4bcbf513-a2f4-4f6c-965a-24e716c2f93b");
        try {
            writeline("/home/ubuntu/results/coverage/Checksum/Checksum_5_10.coverage", "b7ae802c-3f6a-43a0-ab90-bf12f4e0887e");
            if (resources != null) {
                writeline("/home/ubuntu/results/coverage/Checksum/Checksum_5_10.coverage", "257cb462-f916-4de8-b11c-9cf9ec67774a");
                for (Resource r : resources) {
                    writeline("/home/ubuntu/results/coverage/Checksum/Checksum_5_10.coverage", "ebb86f0a-72a2-4384-9919-275f43045ed9");
                    File src = r.as(FileProvider.class).getFile();
                    writeline("/home/ubuntu/results/coverage/Checksum/Checksum_5_10.coverage", "f097296d-28c6-4cd5-ae34-3201fa337910");
                    if (totalproperty != null || todir != null) {
                        writeline("/home/ubuntu/results/coverage/Checksum/Checksum_5_10.coverage", "bba1c2cc-d61c-468a-b9e5-20d26ab1a826");
                        // Use '/' to calculate digest based on file name.
                        // This is required in order to get the same result
                        // on different platforms.
                        relativeFilePaths.put(src, r.getName().replace(File.separatorChar, '/'));
                    }
                    writeline("/home/ubuntu/results/coverage/Checksum/Checksum_5_10.coverage", "5c06eef3-ca8a-4ec3-a3d2-a45a6fea8bb9");
                    addToIncludeFileMap(src);
                }
            }
            writeline("/home/ubuntu/results/coverage/Checksum/Checksum_5_10.coverage", "a73b3e8f-f82e-4967-b284-17ed4b065fce");
            if (file != null) {
                writeline("/home/ubuntu/results/coverage/Checksum/Checksum_5_10.coverage", "8af0d475-9e8e-4985-8ccd-aa84227811c7");
                if (totalproperty != null || todir != null) {
                    writeline("/home/ubuntu/results/coverage/Checksum/Checksum_5_10.coverage", "584b0c02-7be3-4382-a979-bb487ff711c2");
                    relativeFilePaths.put(file, file.getName().replace(File.separatorChar, '/'));
                }
                writeline("/home/ubuntu/results/coverage/Checksum/Checksum_5_10.coverage", "50042ea7-dd57-43bf-b119-9e3baff89e49");
                addToIncludeFileMap(file);
            }
            writeline("/home/ubuntu/results/coverage/Checksum/Checksum_5_10.coverage", "8bb4e0e6-7acb-4c0a-a80b-4761f5a152e2");
            return generateChecksums();
        } finally {
            writeline("/home/ubuntu/results/coverage/Checksum/Checksum_5_10.coverage", "01e0c370-23f8-4cc1-be09-7f06e97c5a7c");
            fileext = savedFileExt;
            writeline("/home/ubuntu/results/coverage/Checksum/Checksum_5_10.coverage", "23bbd1c3-3d96-4af0-a548-5b67232adf3c");
            includeFileMap.clear();
        }
    }

    /**
     * Add key-value pair to the hashtable upon which
     * to later operate upon.
     */
    private void addToIncludeFileMap(File file) throws BuildException {
        writeline("/home/ubuntu/results/coverage/Checksum/Checksum_5_10.coverage", "8aeee4a9-e475-4741-bef2-186241c3e5ba");
        if (file.exists()) {
            writeline("/home/ubuntu/results/coverage/Checksum/Checksum_5_10.coverage", "6d58f0b8-e7a5-415e-af78-fd74163b52f3");
            if (property == null) {
                writeline("/home/ubuntu/results/coverage/Checksum/Checksum_5_10.coverage", "fd50f36f-edf5-4098-bd7d-614ee8d0fba8");
                File checksumFile = getChecksumFile(file);
                writeline("/home/ubuntu/results/coverage/Checksum/Checksum_5_10.coverage", "629ca4e6-23fe-4026-abe3-b20401315fae");
                if (forceOverwrite || isCondition || (file.lastModified() > checksumFile.lastModified())) {
                    writeline("/home/ubuntu/results/coverage/Checksum/Checksum_5_10.coverage", "aeafecf4-a3bc-480c-824f-7160b00fff68");
                    includeFileMap.put(file, checksumFile);
                } else {
                    writeline("/home/ubuntu/results/coverage/Checksum/Checksum_5_10.coverage", "4c049b53-2d63-44dd-b7ad-b18cfa7c9a16");
                    log(file + " omitted as " + checksumFile + " is up to date.", Project.MSG_VERBOSE);
                    writeline("/home/ubuntu/results/coverage/Checksum/Checksum_5_10.coverage", "cf3a0bc6-8029-4a3a-9127-a5536609346f");
                    if (totalproperty != null) {
                        writeline("/home/ubuntu/results/coverage/Checksum/Checksum_5_10.coverage", "e02d601f-f6c5-4373-961d-cf178a943449");
                        // Read the checksum from disk.
                        String checksum = readChecksum(checksumFile);
                        writeline("/home/ubuntu/results/coverage/Checksum/Checksum_5_10.coverage", "3b8c6845-5474-4f5a-8d6a-ce6953cec4d0");
                        byte[] digest = decodeHex(checksum.toCharArray());
                        writeline("/home/ubuntu/results/coverage/Checksum/Checksum_5_10.coverage", "d388c142-1e31-4ecd-b86a-678ffa59168c");
                        allDigests.put(file, digest);
                    }
                }
            } else {
                writeline("/home/ubuntu/results/coverage/Checksum/Checksum_5_10.coverage", "de407b07-cec3-414d-ba0d-afdd9407ea6b");
                includeFileMap.put(file, property);
            }
        } else {
            writeline("/home/ubuntu/results/coverage/Checksum/Checksum_5_10.coverage", "21b049ed-e3e6-4b4a-a949-f786b4a8d529");
            String message = "Could not find file " + file.getAbsolutePath() + " to generate checksum for.";
            writeline("/home/ubuntu/results/coverage/Checksum/Checksum_5_10.coverage", "d22c1871-d7d4-49b5-b30a-7efe6c822170");
            log(message);
            writeline("/home/ubuntu/results/coverage/Checksum/Checksum_5_10.coverage", "ea0f1cfd-02f3-47f9-bbf7-1779a3fe0ee3");
            throw new BuildException(message, getLocation());
        }
    }

    private File getChecksumFile(File file) {
        writeline("/home/ubuntu/results/coverage/Checksum/Checksum_5_10.coverage", "57b3f6df-fbbb-4a7b-8740-725a44a279b0");
        File directory;
        writeline("/home/ubuntu/results/coverage/Checksum/Checksum_5_10.coverage", "ef47f828-8dcf-4ff2-8806-c7491db34518");
        if (todir != null) {
            writeline("/home/ubuntu/results/coverage/Checksum/Checksum_5_10.coverage", "c8839450-390e-4d80-8b9c-a262bf5bcbab");
            // A separate directory was explicitly declared
            String path = getRelativeFilePath(file);
            writeline("/home/ubuntu/results/coverage/Checksum/Checksum_5_10.coverage", "79119132-77e0-45d3-9727-03644fff40ef");
            directory = new File(todir, path).getParentFile();
            writeline("/home/ubuntu/results/coverage/Checksum/Checksum_5_10.coverage", "d57438d0-ad35-4203-945c-6a519700469a");
            // Create the directory, as it might not exist.
            directory.mkdirs();
        } else {
            writeline("/home/ubuntu/results/coverage/Checksum/Checksum_5_10.coverage", "077c3641-8880-4cbb-a28f-1178ea1c5637");
            // Just use the same directory as the file itself.
            // This directory will exist
            directory = file.getParentFile();
        }
        writeline("/home/ubuntu/results/coverage/Checksum/Checksum_5_10.coverage", "567965a4-57ee-4801-bd6b-392f97539f16");
        File checksumFile = new File(directory, file.getName() + fileext);
        writeline("/home/ubuntu/results/coverage/Checksum/Checksum_5_10.coverage", "59c1487c-04ac-4714-8501-a00f1625f74a");
        return checksumFile;
    }

    /**
     * Generate checksum(s) using the message digest created earlier.
     */
    private boolean generateChecksums() throws BuildException {
        writeline("/home/ubuntu/results/coverage/Checksum/Checksum_5_10.coverage", "32a03c16-6776-4de5-add0-6c41b97a9dc3");
        boolean checksumMatches = true;
        writeline("/home/ubuntu/results/coverage/Checksum/Checksum_5_10.coverage", "2b7e5220-d11b-4704-bcdb-85a3f131c75b");
        FileInputStream fis = null;
        writeline("/home/ubuntu/results/coverage/Checksum/Checksum_5_10.coverage", "5a0e914e-bc40-4bf7-917a-41953c62c833");
        FileOutputStream fos = null;
        writeline("/home/ubuntu/results/coverage/Checksum/Checksum_5_10.coverage", "2d2220df-ca15-44c1-ad01-4086b510c551");
        byte[] buf = new byte[readBufferSize];
        writeline("/home/ubuntu/results/coverage/Checksum/Checksum_5_10.coverage", "4e6bbb37-d91d-4b40-b0d5-41115e168eeb");
        try {
            writeline("/home/ubuntu/results/coverage/Checksum/Checksum_5_10.coverage", "3648b535-c166-4864-bdf7-e9b5b0e30e4f");
            for (Map.Entry<File, Object> e : includeFileMap.entrySet()) {
                writeline("/home/ubuntu/results/coverage/Checksum/Checksum_5_10.coverage", "6282c89a-7e98-4e56-9fa6-45ac0b1b1fe7");
                messageDigest.reset();
                writeline("/home/ubuntu/results/coverage/Checksum/Checksum_5_10.coverage", "ba9b3610-c28e-411b-8b80-149ae2378ab1");
                File src = e.getKey();
                writeline("/home/ubuntu/results/coverage/Checksum/Checksum_5_10.coverage", "cdbbba62-cd10-46a2-b329-6ad2a6df728c");
                if (!isCondition) {
                    writeline("/home/ubuntu/results/coverage/Checksum/Checksum_5_10.coverage", "aef5e96b-23f0-499d-8e6c-761c4dd71725");
                    log("Calculating " + algorithm + " checksum for " + src, Project.MSG_VERBOSE);
                }
                writeline("/home/ubuntu/results/coverage/Checksum/Checksum_5_10.coverage", "71bad682-a4d2-49ae-a137-274a34a2ebdd");
                fis = new FileInputStream(src);
                writeline("/home/ubuntu/results/coverage/Checksum/Checksum_5_10.coverage", "e670ad70-220c-44bc-bcf7-0406db179116");
                DigestInputStream dis = new DigestInputStream(fis, messageDigest);
                writeline("/home/ubuntu/results/coverage/Checksum/Checksum_5_10.coverage", "dccdf927-02f8-4dea-be5c-11312f9eb1b5");
                while (dis.read(buf, 0, readBufferSize) != -1) {
                // Empty statement
                }
                writeline("/home/ubuntu/results/coverage/Checksum/Checksum_5_10.coverage", "00371091-584f-4eef-9210-7ff13cfb23a3");
                dis.close();
                writeline("/home/ubuntu/results/coverage/Checksum/Checksum_5_10.coverage", "bc239a9f-a794-403d-b283-249d15249865");
                fis.close();
                writeline("/home/ubuntu/results/coverage/Checksum/Checksum_5_10.coverage", "8869999c-17ef-4f84-b325-2c70e021bdf7");
                fis = null;
                writeline("/home/ubuntu/results/coverage/Checksum/Checksum_5_10.coverage", "01aff0cc-c6a3-4bfa-ac61-26e7f45717b9");
                byte[] fileDigest = messageDigest.digest();
                writeline("/home/ubuntu/results/coverage/Checksum/Checksum_5_10.coverage", "250281a7-9fac-45ad-900c-87dac16776c8");
                if (totalproperty != null) {
                    writeline("/home/ubuntu/results/coverage/Checksum/Checksum_5_10.coverage", "4b860cb9-0541-4d66-9ce0-27a66821fd8f");
                    allDigests.put(src, fileDigest);
                }
                writeline("/home/ubuntu/results/coverage/Checksum/Checksum_5_10.coverage", "9f7ce7ad-76d5-4374-8e49-25e79ae703a8");
                String checksum = createDigestString(fileDigest);
                writeline("/home/ubuntu/results/coverage/Checksum/Checksum_5_10.coverage", "51290213-67c0-4851-a0de-03f00d748090");
                // can either be a property name string or a file
                Object destination = e.getValue();
                writeline("/home/ubuntu/results/coverage/Checksum/Checksum_5_10.coverage", "3485e885-b7a6-499e-8acc-fb7b7468899a");
                if (destination instanceof java.lang.String) {
                    writeline("/home/ubuntu/results/coverage/Checksum/Checksum_5_10.coverage", "b5556c11-71e4-4153-8e13-162ef222edb4");
                    String prop = (String) destination;
                    writeline("/home/ubuntu/results/coverage/Checksum/Checksum_5_10.coverage", "ef23c376-69e1-4eb4-9e65-3fbc72ce3712");
                    if (isCondition) {
                        writeline("/home/ubuntu/results/coverage/Checksum/Checksum_5_10.coverage", "9b062975-2d55-42e1-8a0e-aead277e14a7");
                        checksumMatches = checksumMatches && checksum.equals(property);
                    } else {
                        writeline("/home/ubuntu/results/coverage/Checksum/Checksum_5_10.coverage", "50b64d37-7758-439a-a4ef-bf0308b2d6ae");
                        getProject().setNewProperty(prop, checksum);
                    }
                } else if (destination instanceof java.io.File) {
                    writeline("/home/ubuntu/results/coverage/Checksum/Checksum_5_10.coverage", "9b6bcabc-6414-4e25-ac82-2622b4f42922");
                    if (isCondition) {
                        writeline("/home/ubuntu/results/coverage/Checksum/Checksum_5_10.coverage", "30edea4d-ece1-43b5-b6ea-8bc86c324416");
                        File existingFile = (File) destination;
                        writeline("/home/ubuntu/results/coverage/Checksum/Checksum_5_10.coverage", "90adcf34-659a-462e-a567-29e92cb5a87b");
                        if (existingFile.exists()) {
                            writeline("/home/ubuntu/results/coverage/Checksum/Checksum_5_10.coverage", "7ae5b70f-f882-4262-b7ab-da2c52cb28f2");
                            try {
                                writeline("/home/ubuntu/results/coverage/Checksum/Checksum_5_10.coverage", "7e6ee047-fc4d-43e5-a04e-35aaa79f1cfd");
                                String suppliedChecksum = readChecksum(existingFile);
                                writeline("/home/ubuntu/results/coverage/Checksum/Checksum_5_10.coverage", "595308ca-cd48-4249-ada4-1211471de1f2");
                                checksumMatches = checksumMatches && checksum.equals(suppliedChecksum);
                            } catch (BuildException be) {
                                writeline("/home/ubuntu/results/coverage/Checksum/Checksum_5_10.coverage", "cd7d35f7-1cc4-461e-93a8-f6566dd7584a");
                                // file is on wrong format, swallow
                                checksumMatches = false;
                            }
                        } else {
                            writeline("/home/ubuntu/results/coverage/Checksum/Checksum_5_10.coverage", "1b6cabc2-1b1a-4ef8-931c-2237bcfa1531");
                            checksumMatches = false;
                        }
                    } else {
                        writeline("/home/ubuntu/results/coverage/Checksum/Checksum_5_10.coverage", "be937b0c-b75f-4c8f-bab0-e9ca010b83a5");
                        File dest = (File) destination;
                        writeline("/home/ubuntu/results/coverage/Checksum/Checksum_5_10.coverage", "1359d2dd-fb46-4a3d-bd36-b34fb6502970");
                        fos = new FileOutputStream(dest);
                        writeline("/home/ubuntu/results/coverage/Checksum/Checksum_5_10.coverage", "683fb525-2d8a-472f-a682-a65b2b0dfeb0");
                        fos.write(format.format(new Object[] { checksum, src.getName(), FileUtils.getRelativePath(dest.getParentFile(), src), FileUtils.getRelativePath(getProject().getBaseDir(), src), src.getAbsolutePath() }).getBytes());
                        writeline("/home/ubuntu/results/coverage/Checksum/Checksum_5_10.coverage", "fd072163-1c7c-49d6-be6f-3780876ca84b");
                        fos.write(StringUtils.LINE_SEP.getBytes());
                        writeline("/home/ubuntu/results/coverage/Checksum/Checksum_5_10.coverage", "32fdffa7-cda2-47a6-a57a-ad5464573994");
                        fos.close();
                        writeline("/home/ubuntu/results/coverage/Checksum/Checksum_5_10.coverage", "1c14859f-3330-4beb-94d0-1da89a20000c");
                        fos = null;
                    }
                }
            }
            writeline("/home/ubuntu/results/coverage/Checksum/Checksum_5_10.coverage", "359d8264-5a04-4ffe-b6c0-4289447911d9");
            if (totalproperty != null) {
                writeline("/home/ubuntu/results/coverage/Checksum/Checksum_5_10.coverage", "01b70403-1d5a-4705-a4a1-a9e9191ea086");
                // Calculate the total checksum
                // Convert the keys (source files) into a sorted array.
                File[] keyArray = allDigests.keySet().toArray(new File[allDigests.size()]);
                writeline("/home/ubuntu/results/coverage/Checksum/Checksum_5_10.coverage", "cab7372a-450e-4cfa-a4f2-415a51d23397");
                // File is Comparable, but sort-order is platform
                // dependent (case-insensitive on Windows)
                Arrays.sort(keyArray, new Comparator<File>() {

                    public int compare(File f1, File f2) {
                        return f1 == null ? (f2 == null ? 0 : -1) : (f2 == null ? 1 : getRelativeFilePath(f1).compareTo(getRelativeFilePath(f2)));
                    }
                });
                writeline("/home/ubuntu/results/coverage/Checksum/Checksum_5_10.coverage", "46ded0de-6a3c-4797-8ec4-e2df308531dc");
                // Loop over the checksums and generate a total hash.
                messageDigest.reset();
                writeline("/home/ubuntu/results/coverage/Checksum/Checksum_5_10.coverage", "7d77c196-8c94-465a-835b-a2afb36206ae");
                for (File src : keyArray) {
                    writeline("/home/ubuntu/results/coverage/Checksum/Checksum_5_10.coverage", "8dc0a073-f9c7-4ec3-8076-e9be939129ec");
                    // Add the digest for the file content
                    byte[] digest = allDigests.get(src);
                    writeline("/home/ubuntu/results/coverage/Checksum/Checksum_5_10.coverage", "bad03579-f2b0-4bb6-a2c6-c8ab0582feeb");
                    messageDigest.update(digest);
                    writeline("/home/ubuntu/results/coverage/Checksum/Checksum_5_10.coverage", "910bda7b-1d5d-4919-9ecd-c52de580b3a7");
                    // Add the file path
                    String fileName = getRelativeFilePath(src);
                    writeline("/home/ubuntu/results/coverage/Checksum/Checksum_5_10.coverage", "120294ec-f13b-4c67-ab64-1dd11847d876");
                    messageDigest.update(fileName.getBytes());
                }
                writeline("/home/ubuntu/results/coverage/Checksum/Checksum_5_10.coverage", "1b22bbee-90d7-4ac1-9766-2bb37b29c167");
                String totalChecksum = createDigestString(messageDigest.digest());
                writeline("/home/ubuntu/results/coverage/Checksum/Checksum_5_10.coverage", "bd874580-9ef7-4877-8a0a-3156e8af1d76");
                getProject().setNewProperty(totalproperty, totalChecksum);
            }
        } catch (Exception e) {
            writeline("/home/ubuntu/results/coverage/Checksum/Checksum_5_10.coverage", "d9d8dd2a-f9fe-4225-a10e-a42aa4e25cab");
            throw new BuildException(e, getLocation());
        } finally {
            writeline("/home/ubuntu/results/coverage/Checksum/Checksum_5_10.coverage", "adbdfff3-ba45-4177-b7b5-c4d0f2c82c62");
            FileUtils.close(fis);
            writeline("/home/ubuntu/results/coverage/Checksum/Checksum_5_10.coverage", "f7c838b7-88e3-4f34-949a-4a76fda33756");
            FileUtils.close(fos);
        }
        writeline("/home/ubuntu/results/coverage/Checksum/Checksum_5_10.coverage", "23f61348-817a-4d96-af8a-7cefb216a642");
        return checksumMatches;
    }

    private String createDigestString(byte[] fileDigest) {
        writeline("/home/ubuntu/results/coverage/Checksum/Checksum_5_10.coverage", "9a0e92ef-8d83-40db-a10d-6ec22a4fd130");
        StringBuffer checksumSb = new StringBuffer();
        writeline("/home/ubuntu/results/coverage/Checksum/Checksum_5_10.coverage", "2e3dd0b1-22cd-4ea5-bcb9-cfdb93d6011f");
        for (int i = 0; i < fileDigest.length; i++) {
            writeline("/home/ubuntu/results/coverage/Checksum/Checksum_5_10.coverage", "21b647ce-bf57-4d78-bc28-3a3e07849b91");
            String hexStr = Integer.toHexString(BYTE_MASK & fileDigest[i]);
            writeline("/home/ubuntu/results/coverage/Checksum/Checksum_5_10.coverage", "992fd582-825c-4651-8f64-ad3090306868");
            if (hexStr.length() < 2) {
                writeline("/home/ubuntu/results/coverage/Checksum/Checksum_5_10.coverage", "ef977413-df88-41dc-9ce3-803814050961");
                checksumSb.append("0");
            }
            writeline("/home/ubuntu/results/coverage/Checksum/Checksum_5_10.coverage", "07be6f2e-4845-42c7-bb64-3a63816fab67");
            checksumSb.append(hexStr);
        }
        writeline("/home/ubuntu/results/coverage/Checksum/Checksum_5_10.coverage", "85c3a492-97c3-4b9b-87a4-d43ab77c882d");
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
        writelineStatic("/home/ubuntu/results/coverage/Checksum/Checksum_5_10.coverage", "0ae6907c-7eda-423f-9ca1-eb850068b5b1");
        int l = data.length;
        writelineStatic("/home/ubuntu/results/coverage/Checksum/Checksum_5_10.coverage", "54712632-5fcc-4b69-81e9-6b7b4185655a");
        if ((l & 0x01) != 0) {
            writelineStatic("/home/ubuntu/results/coverage/Checksum/Checksum_5_10.coverage", "31b06951-be04-4df9-8a57-a5b841f2c363");
            throw new BuildException("odd number of characters.");
        }
        writelineStatic("/home/ubuntu/results/coverage/Checksum/Checksum_5_10.coverage", "9c1abf34-7342-45b6-a185-2f4a42c0783a");
        byte[] out = new byte[l >> 1];
        writelineStatic("/home/ubuntu/results/coverage/Checksum/Checksum_5_10.coverage", "a42b5be4-ff34-421c-ad6c-541e85986f9b");
        // two characters form the hex value.
        for (int i = 0, j = 0; j < l; i++) {
            writelineStatic("/home/ubuntu/results/coverage/Checksum/Checksum_5_10.coverage", "96c4df5a-db98-4127-b558-31a93add9897");
            int f = Character.digit(data[j++], WORD) << NIBBLE;
            writelineStatic("/home/ubuntu/results/coverage/Checksum/Checksum_5_10.coverage", "c64063aa-00be-4286-96c4-4d15f8eb71ec");
            f = f | Character.digit(data[j++], WORD);
            writelineStatic("/home/ubuntu/results/coverage/Checksum/Checksum_5_10.coverage", "59c3271f-9abf-4c52-8515-253d844f4caa");
            out[i] = (byte) (f & BYTE_MASK);
        }
        writelineStatic("/home/ubuntu/results/coverage/Checksum/Checksum_5_10.coverage", "3e580fc6-92ca-4481-88ff-c761984f4f67");
        return out;
    }

    /**
     * reads the checksum from a file using the specified format.
     *
     * @since 1.7
     */
    private String readChecksum(File f) {
        writeline("/home/ubuntu/results/coverage/Checksum/Checksum_5_10.coverage", "0da73e97-55e6-4924-9b39-db38b58b446f");
        BufferedReader diskChecksumReader = null;
        writeline("/home/ubuntu/results/coverage/Checksum/Checksum_5_10.coverage", "86bf1def-3377-401a-ac2a-060747356bd5");
        try {
            writeline("/home/ubuntu/results/coverage/Checksum/Checksum_5_10.coverage", "9fd446e9-aec7-44d1-ace0-eb258962140d");
            diskChecksumReader = new BufferedReader(new FileReader(f));
            writeline("/home/ubuntu/results/coverage/Checksum/Checksum_5_10.coverage", "f7099999-3c51-4373-a859-6af27f0e0937");
            Object[] result = format.parse(diskChecksumReader.readLine());
            writeline("/home/ubuntu/results/coverage/Checksum/Checksum_5_10.coverage", "04760279-2017-45cc-96f8-a10633105517");
            if (result == null || result.length == 0 || result[0] == null) {
                writeline("/home/ubuntu/results/coverage/Checksum/Checksum_5_10.coverage", "b81f5572-23d5-466d-b057-259913e09e39");
                throw new BuildException("failed to find a checksum");
            }
            writeline("/home/ubuntu/results/coverage/Checksum/Checksum_5_10.coverage", "0d118951-c5cf-4a7b-886f-1f39e0ffc368");
            return (String) result[0];
        } catch (IOException e) {
            writeline("/home/ubuntu/results/coverage/Checksum/Checksum_5_10.coverage", "6fb2c4b3-41c6-4682-828c-b50ec7b41846");
            throw new BuildException("Couldn't read checksum file " + f, e);
        } catch (ParseException e) {
            writeline("/home/ubuntu/results/coverage/Checksum/Checksum_5_10.coverage", "16ae89be-7577-478a-84ad-44cd78fbb3b5");
            throw new BuildException("Couldn't read checksum file " + f, e);
        } finally {
            writeline("/home/ubuntu/results/coverage/Checksum/Checksum_5_10.coverage", "ae48ef87-d2b3-4a26-9efc-fa948f2dcc0a");
            FileUtils.close(diskChecksumReader);
        }
    }

    /**
     * @since Ant 1.8.2
     */
    private String getRelativeFilePath(File f) {
        writeline("/home/ubuntu/results/coverage/Checksum/Checksum_5_10.coverage", "d868d6d2-2e17-4641-b2f4-c87a16572e14");
        String path = (String) relativeFilePaths.get(f);
        writeline("/home/ubuntu/results/coverage/Checksum/Checksum_5_10.coverage", "0a4d4b03-39db-4616-b91b-f3dfede28c1e");
        if (path == null) {
            writeline("/home/ubuntu/results/coverage/Checksum/Checksum_5_10.coverage", "d010cf74-2ac5-4d71-b973-a741ae71129b");
            // bug 37386. this should not occur, but it has, once.
            throw new BuildException("Internal error: " + "relativeFilePaths could not match file " + f + "\n" + "please file a bug report on this");
        }
        writeline("/home/ubuntu/results/coverage/Checksum/Checksum_5_10.coverage", "f53040bb-c9c2-46e8-8b4e-74c51e83f1ed");
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
