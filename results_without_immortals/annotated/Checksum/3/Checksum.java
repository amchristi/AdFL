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
        writeline("/home/ubuntu/results/coverage/Checksum/Checksum_3_10.coverage", "f1a5dc11-cfaa-4d53-95f2-5cde5d57f61b");
        this.file = file;
    }

    /**
     * Sets the root directory where checksum files will be
     * written/read
     * @param todir the directory to write to
     * @since Ant 1.6
     */
    public void setTodir(File todir) {
        writeline("/home/ubuntu/results/coverage/Checksum/Checksum_3_10.coverage", "f813c285-854d-449e-a2c4-c0bd95a05b8f");
        this.todir = todir;
    }

    /**
     * Specifies the algorithm to be used to compute the checksum.
     * Defaults to "MD5". Other popular algorithms like "SHA" may be used as well.
     * @param algorithm a <code>String</code> value
     */
    public void setAlgorithm(String algorithm) {
        writeline("/home/ubuntu/results/coverage/Checksum/Checksum_3_10.coverage", "f3c02ffe-db7b-4809-9fc2-d37f54bd5b2e");
        this.algorithm = algorithm;
    }

    /**
     * Sets the MessageDigest algorithm provider to be used
     * to calculate the checksum.
     * @param provider a <code>String</code> value
     */
    public void setProvider(String provider) {
        writeline("/home/ubuntu/results/coverage/Checksum/Checksum_3_10.coverage", "eff65799-e321-400b-b7aa-77cc0be95af7");
        this.provider = provider;
    }

    /**
     * Sets the file extension that is be to used to
     * create or identify destination file.
     * @param fileext a <code>String</code> value
     */
    public void setFileext(String fileext) {
        writeline("/home/ubuntu/results/coverage/Checksum/Checksum_3_10.coverage", "7005b7d8-2242-4bda-a584-e449488c0861");
        this.fileext = fileext;
    }

    /**
     * Sets the property to hold the generated checksum.
     * @param property a <code>String</code> value
     */
    public void setProperty(String property) {
        writeline("/home/ubuntu/results/coverage/Checksum/Checksum_3_10.coverage", "d9f67858-0dfb-44be-9269-e3a7d7928db1");
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
        writeline("/home/ubuntu/results/coverage/Checksum/Checksum_3_10.coverage", "614a45c4-b21b-4988-8347-7def9c571b52");
        this.totalproperty = totalproperty;
    }

    /**
     * Sets the verify property.  This project property holds
     * the result of a checksum verification - "true" or "false"
     * @param verifyProperty a <code>String</code> value
     */
    public void setVerifyproperty(String verifyProperty) {
        writeline("/home/ubuntu/results/coverage/Checksum/Checksum_3_10.coverage", "ffb1231c-f2a9-4116-9324-3cada0653058");
        this.verifyProperty = verifyProperty;
    }

    /**
     * Whether or not to overwrite existing file irrespective of
     * whether it is newer than
     * the source file.  Defaults to false.
     * @param forceOverwrite a <code>boolean</code> value
     */
    public void setForceOverwrite(boolean forceOverwrite) {
        writeline("/home/ubuntu/results/coverage/Checksum/Checksum_3_10.coverage", "889d745b-3fff-4316-a12b-0aeac63e8c88");
        this.forceOverwrite = forceOverwrite;
    }

    /**
     * The size of the read buffer to use.
     * @param size an <code>int</code> value
     */
    public void setReadBufferSize(int size) {
        writeline("/home/ubuntu/results/coverage/Checksum/Checksum_3_10.coverage", "cc5faeb9-57cc-41f3-a9e4-af012b53d0ec");
        this.readBufferSize = size;
    }

    /**
     * Select the in/output pattern via a well know format name.
     * @param e an <code>enumerated</code> value
     *
     * @since 1.7.0
     */
    public void setFormat(FormatElement e) {
        writeline("/home/ubuntu/results/coverage/Checksum/Checksum_3_10.coverage", "8b8a3b7a-4652-440a-b386-31b5dca29e00");
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
        writeline("/home/ubuntu/results/coverage/Checksum/Checksum_3_10.coverage", "abde752f-6831-413c-8360-7868d1c21586");
        format = new MessageFormat(p);
    }

    /**
     * Files to generate checksums for.
     * @param set a fileset of files to generate checksums for.
     */
    public void addFileset(FileSet set) {
        writeline("/home/ubuntu/results/coverage/Checksum/Checksum_3_10.coverage", "e868c7b6-b136-4372-aa0a-f9cf2c7761b1");
        add(set);
    }

    /**
     * Add a resource collection.
     * @param rc the ResourceCollection to add.
     */
    public void add(ResourceCollection rc) {
        writeline("/home/ubuntu/results/coverage/Checksum/Checksum_3_10.coverage", "72df456a-57b8-452b-91c8-927747541996");
        if (rc == null) {
            writeline("/home/ubuntu/results/coverage/Checksum/Checksum_3_10.coverage", "38b87ce1-4a20-41ca-843f-2c40d0028096");
            return;
        }
        writeline("/home/ubuntu/results/coverage/Checksum/Checksum_3_10.coverage", "203fdc37-6723-4d29-861b-11e259f1ab97");
        resources = (resources == null) ? new FileUnion() : resources;
        writeline("/home/ubuntu/results/coverage/Checksum/Checksum_3_10.coverage", "4e267931-4536-4ff4-afe9-ef03cd7878a2");
        resources.add(rc);
    }

    /**
     * Calculate the checksum(s).
     * @throws BuildException on error
     */
    public void execute() throws BuildException {
        writeline("/home/ubuntu/results/coverage/Checksum/Checksum_3_10.coverage", "feff3230-7b07-4d7d-b523-daddc192b9cd");
        isCondition = false;
        writeline("/home/ubuntu/results/coverage/Checksum/Checksum_3_10.coverage", "557a495a-423c-480a-a23e-20cd070de925");
        boolean value = validateAndExecute();
        writeline("/home/ubuntu/results/coverage/Checksum/Checksum_3_10.coverage", "b4d49ab7-24c2-4821-b3b0-be964212c38b");
        if (verifyProperty != null) {
            writeline("/home/ubuntu/results/coverage/Checksum/Checksum_3_10.coverage", "fec1990c-3766-4fa4-849b-f98f4f3b6c28");
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
        writeline("/home/ubuntu/results/coverage/Checksum/Checksum_3_10.coverage", "390da3df-fe07-46a4-a15c-e28f7d724cf7");
        isCondition = true;
        writeline("/home/ubuntu/results/coverage/Checksum/Checksum_3_10.coverage", "b12b705d-3cdc-4af3-a949-4f284c5d1c1a");
        return validateAndExecute();
    }

    /**
     * Validate attributes and get down to business.
     */
    private boolean validateAndExecute() throws BuildException {
        writeline("/home/ubuntu/results/coverage/Checksum/Checksum_3_10.coverage", "c5f28b96-df9d-4512-ab97-c5f7db60427b");
        String savedFileExt = fileext;
        writeline("/home/ubuntu/results/coverage/Checksum/Checksum_3_10.coverage", "5d24479e-1799-457d-96fe-c49228e5248a");
        if (file == null && (resources == null || resources.size() == 0)) {
            writeline("/home/ubuntu/results/coverage/Checksum/Checksum_3_10.coverage", "d3c40a7d-5ab1-4260-b439-05d2b8800e56");
            throw new BuildException("Specify at least one source - a file or a resource collection.");
        }
        writeline("/home/ubuntu/results/coverage/Checksum/Checksum_3_10.coverage", "739abb41-171c-4612-9d70-0e2e98bc787c");
        if (!(resources == null || resources.isFilesystemOnly())) {
            writeline("/home/ubuntu/results/coverage/Checksum/Checksum_3_10.coverage", "9c2a1fa2-cbe1-4146-8113-6ada20359b15");
            throw new BuildException("Can only calculate checksums for file-based resources.");
        }
        writeline("/home/ubuntu/results/coverage/Checksum/Checksum_3_10.coverage", "c2a655dd-62d9-4319-a104-c6a752d6af64");
        if (file != null && file.exists() && file.isDirectory()) {
            writeline("/home/ubuntu/results/coverage/Checksum/Checksum_3_10.coverage", "689b9ba0-5898-403d-8e63-5257988d789e");
            throw new BuildException("Checksum cannot be generated for directories");
        }
        writeline("/home/ubuntu/results/coverage/Checksum/Checksum_3_10.coverage", "dd13d349-d292-476d-9ed4-f5f7672594f2");
        if (file != null && totalproperty != null) {
            writeline("/home/ubuntu/results/coverage/Checksum/Checksum_3_10.coverage", "61b2f9b6-6cd7-4c6c-b705-bd1ca2d0c11b");
            throw new BuildException("File and Totalproperty cannot co-exist.");
        }
        writeline("/home/ubuntu/results/coverage/Checksum/Checksum_3_10.coverage", "6e37bebd-d44d-47e0-b917-6141e01dfcac");
        if (property != null && fileext != null) {
            writeline("/home/ubuntu/results/coverage/Checksum/Checksum_3_10.coverage", "fe5f8069-7fc0-4b7f-847c-dff9ec74d08b");
            throw new BuildException("Property and FileExt cannot co-exist.");
        }
        writeline("/home/ubuntu/results/coverage/Checksum/Checksum_3_10.coverage", "77759840-b936-4859-9178-195e01f1b66a");
        if (property != null) {
            writeline("/home/ubuntu/results/coverage/Checksum/Checksum_3_10.coverage", "00ea3bac-1b20-49e9-adf2-6a047c7a6eac");
            if (forceOverwrite) {
                writeline("/home/ubuntu/results/coverage/Checksum/Checksum_3_10.coverage", "12504b9d-860d-4490-ad12-e2a48b95d76f");
                throw new BuildException("ForceOverwrite cannot be used when Property is specified");
            }
            writeline("/home/ubuntu/results/coverage/Checksum/Checksum_3_10.coverage", "f59d5a75-45bb-4dda-95bd-56912637bb1b");
            int ct = 0;
            writeline("/home/ubuntu/results/coverage/Checksum/Checksum_3_10.coverage", "fb6bf65e-2790-44ea-9401-28f145b1152e");
            if (resources != null) {
                writeline("/home/ubuntu/results/coverage/Checksum/Checksum_3_10.coverage", "e85220e7-c4d9-45f6-bbf7-13da5d5edeb2");
                ct += resources.size();
            }
            writeline("/home/ubuntu/results/coverage/Checksum/Checksum_3_10.coverage", "4b80c673-0328-45c9-9e6d-41b8fd0a8542");
            if (file != null) {
                writeline("/home/ubuntu/results/coverage/Checksum/Checksum_3_10.coverage", "e229333a-712b-4ea3-8a8b-a7c1cb291ba5");
                ct++;
            }
            writeline("/home/ubuntu/results/coverage/Checksum/Checksum_3_10.coverage", "34f33a9b-0b25-4a58-a9c1-24f9105dcda9");
            if (ct > 1) {
                writeline("/home/ubuntu/results/coverage/Checksum/Checksum_3_10.coverage", "ddc3e3e2-b34a-49e6-97d7-7dae170935f9");
                throw new BuildException("Multiple files cannot be used when Property is specified");
            }
        }
        writeline("/home/ubuntu/results/coverage/Checksum/Checksum_3_10.coverage", "ca566d8e-1b08-49d2-9804-1ba228233226");
        if (verifyProperty != null) {
            writeline("/home/ubuntu/results/coverage/Checksum/Checksum_3_10.coverage", "5f2346e6-4ac4-4ddf-bc44-ee77c80657e6");
            isCondition = true;
        }
        writeline("/home/ubuntu/results/coverage/Checksum/Checksum_3_10.coverage", "a26ae8c8-ad8b-4dea-a3be-44b37b6d3605");
        if (verifyProperty != null && forceOverwrite) {
            writeline("/home/ubuntu/results/coverage/Checksum/Checksum_3_10.coverage", "b87a3088-600e-493f-9330-937ff768dc95");
            throw new BuildException("VerifyProperty and ForceOverwrite cannot co-exist.");
        }
        writeline("/home/ubuntu/results/coverage/Checksum/Checksum_3_10.coverage", "e64fc97c-cc04-4b40-894a-eb086a04681e");
        if (isCondition && forceOverwrite) {
            writeline("/home/ubuntu/results/coverage/Checksum/Checksum_3_10.coverage", "fd8b3c2d-11b8-475c-87ac-68aa75f198fa");
            throw new BuildException("ForceOverwrite cannot be used when conditions are being used.");
        }
        writeline("/home/ubuntu/results/coverage/Checksum/Checksum_3_10.coverage", "5d2be085-f3dc-4306-b473-b4dbafd89ecd");
        messageDigest = null;
        writeline("/home/ubuntu/results/coverage/Checksum/Checksum_3_10.coverage", "10a89e85-16fb-426a-be3d-01e7b67ca36f");
        if (provider != null) {
            writeline("/home/ubuntu/results/coverage/Checksum/Checksum_3_10.coverage", "6d2708e9-1d44-45d9-b543-b153c5bb25b7");
            try {
                writeline("/home/ubuntu/results/coverage/Checksum/Checksum_3_10.coverage", "fb211f8e-cde3-4e07-a34d-1fcdabd40626");
                messageDigest = MessageDigest.getInstance(algorithm, provider);
            } catch (NoSuchAlgorithmException noalgo) {
                writeline("/home/ubuntu/results/coverage/Checksum/Checksum_3_10.coverage", "3bf6cad8-24c8-4d9a-858b-b394d0744312");
                throw new BuildException(noalgo, getLocation());
            } catch (NoSuchProviderException noprovider) {
                writeline("/home/ubuntu/results/coverage/Checksum/Checksum_3_10.coverage", "352a5401-1693-48fa-b12f-73dee50af04b");
                throw new BuildException(noprovider, getLocation());
            }
        } else {
            writeline("/home/ubuntu/results/coverage/Checksum/Checksum_3_10.coverage", "b2b4ec0b-060b-4305-bcec-f330a9f01a59");
            try {
                writeline("/home/ubuntu/results/coverage/Checksum/Checksum_3_10.coverage", "a9562a20-76fc-4b3a-99df-a8a9fc501f38");
                messageDigest = MessageDigest.getInstance(algorithm);
            } catch (NoSuchAlgorithmException noalgo) {
                writeline("/home/ubuntu/results/coverage/Checksum/Checksum_3_10.coverage", "22c557b4-5263-427c-9fe5-4a071e5ad65e");
                throw new BuildException(noalgo, getLocation());
            }
        }
        writeline("/home/ubuntu/results/coverage/Checksum/Checksum_3_10.coverage", "55f9d408-c5ae-465e-8c20-f1ff6645894f");
        if (messageDigest == null) {
            writeline("/home/ubuntu/results/coverage/Checksum/Checksum_3_10.coverage", "05e959f2-0987-4d33-8868-fc6807fcffff");
            throw new BuildException("Unable to create Message Digest", getLocation());
        }
        writeline("/home/ubuntu/results/coverage/Checksum/Checksum_3_10.coverage", "c7654da4-b051-4419-9df9-b31efe1ee3b5");
        if (fileext == null) {
            writeline("/home/ubuntu/results/coverage/Checksum/Checksum_3_10.coverage", "621cfda2-dd1f-4f94-8501-a092175348b5");
            fileext = "." + algorithm;
        } else if (fileext.trim().length() == 0) {
            writeline("/home/ubuntu/results/coverage/Checksum/Checksum_3_10.coverage", "f5121f6c-cb90-4c96-bb6c-662393bf56b0");
            throw new BuildException("File extension when specified must not be an empty string");
        }
        writeline("/home/ubuntu/results/coverage/Checksum/Checksum_3_10.coverage", "c6987c61-2a27-4a52-a743-0f6739006ed0");
        try {
            writeline("/home/ubuntu/results/coverage/Checksum/Checksum_3_10.coverage", "61de0a65-89bc-4469-bc12-a14a20d09fac");
            if (resources != null) {
                writeline("/home/ubuntu/results/coverage/Checksum/Checksum_3_10.coverage", "7a337f94-0f0f-44a5-b363-53d92e2009b3");
                for (Resource r : resources) {
                    writeline("/home/ubuntu/results/coverage/Checksum/Checksum_3_10.coverage", "4dfd2841-6dd8-4a41-9004-d41a05a2d6ac");
                    File src = r.as(FileProvider.class).getFile();
                    writeline("/home/ubuntu/results/coverage/Checksum/Checksum_3_10.coverage", "4b9c2f11-e9bc-453c-97f2-e64ed37a2555");
                    if (totalproperty != null || todir != null) {
                        writeline("/home/ubuntu/results/coverage/Checksum/Checksum_3_10.coverage", "e6d19bbd-6a9b-400f-b836-a019b8fac43f");
                        // Use '/' to calculate digest based on file name.
                        // This is required in order to get the same result
                        // on different platforms.
                        relativeFilePaths.put(src, r.getName().replace(File.separatorChar, '/'));
                    }
                    writeline("/home/ubuntu/results/coverage/Checksum/Checksum_3_10.coverage", "d1dd7a88-d20e-46a7-88f0-bfaf98478ada");
                    addToIncludeFileMap(src);
                }
            }
            writeline("/home/ubuntu/results/coverage/Checksum/Checksum_3_10.coverage", "0d56f477-164f-4da3-a9bd-990084459c98");
            if (file != null) {
                writeline("/home/ubuntu/results/coverage/Checksum/Checksum_3_10.coverage", "2f591533-70cf-4fe7-aff3-a155fdf859f5");
                if (totalproperty != null || todir != null) {
                    writeline("/home/ubuntu/results/coverage/Checksum/Checksum_3_10.coverage", "36264d27-f1e1-4d95-a155-cc9cb0154f99");
                    relativeFilePaths.put(file, file.getName().replace(File.separatorChar, '/'));
                }
                writeline("/home/ubuntu/results/coverage/Checksum/Checksum_3_10.coverage", "6483b5c0-bbeb-4f66-97cf-4964a630648d");
                addToIncludeFileMap(file);
            }
            writeline("/home/ubuntu/results/coverage/Checksum/Checksum_3_10.coverage", "d4f3444b-ac2b-4d19-945c-ef2e035edffa");
            return generateChecksums();
        } finally {
            writeline("/home/ubuntu/results/coverage/Checksum/Checksum_3_10.coverage", "a9b41032-801a-4ce7-968d-20c90533380f");
            fileext = savedFileExt;
            writeline("/home/ubuntu/results/coverage/Checksum/Checksum_3_10.coverage", "cc5f6908-07ff-4d9b-a869-c417d2cdaa75");
            includeFileMap.clear();
        }
    }

    /**
     * Add key-value pair to the hashtable upon which
     * to later operate upon.
     */
    private void addToIncludeFileMap(File file) throws BuildException {
        writeline("/home/ubuntu/results/coverage/Checksum/Checksum_3_10.coverage", "af0182a9-6563-42f2-a767-4d1052b3236b");
        if (file.exists()) {
            writeline("/home/ubuntu/results/coverage/Checksum/Checksum_3_10.coverage", "7708b772-870e-4369-b1db-9d2aa69c1e85");
            if (property == null) {
                writeline("/home/ubuntu/results/coverage/Checksum/Checksum_3_10.coverage", "9563c5cc-e67a-4ed4-8392-77e46c20d088");
                File checksumFile = getChecksumFile(file);
                writeline("/home/ubuntu/results/coverage/Checksum/Checksum_3_10.coverage", "79cc3a25-85f8-4cf7-91a0-112cc4ec2ae4");
                if (forceOverwrite || isCondition || (file.lastModified() > checksumFile.lastModified())) {
                    writeline("/home/ubuntu/results/coverage/Checksum/Checksum_3_10.coverage", "342221f9-c831-43f9-889a-34ab18214535");
                    includeFileMap.put(file, checksumFile);
                } else {
                    writeline("/home/ubuntu/results/coverage/Checksum/Checksum_3_10.coverage", "830ab65d-2868-41ba-b728-02697d90c644");
                    log(file + " omitted as " + checksumFile + " is up to date.", Project.MSG_VERBOSE);
                    writeline("/home/ubuntu/results/coverage/Checksum/Checksum_3_10.coverage", "980ccbd7-3342-48ba-a7e0-aea5d5e0b0a3");
                    if (totalproperty != null) {
                        writeline("/home/ubuntu/results/coverage/Checksum/Checksum_3_10.coverage", "2178ac0e-cb49-4c7f-806a-b108cd1b82a0");
                        // Read the checksum from disk.
                        String checksum = readChecksum(checksumFile);
                        writeline("/home/ubuntu/results/coverage/Checksum/Checksum_3_10.coverage", "b3a4eb5f-11fb-4490-bba0-1d3a745926e0");
                        byte[] digest = decodeHex(checksum.toCharArray());
                        writeline("/home/ubuntu/results/coverage/Checksum/Checksum_3_10.coverage", "8cb30abe-5609-4524-9d7c-cb9da79707c6");
                        allDigests.put(file, digest);
                    }
                }
            } else {
                writeline("/home/ubuntu/results/coverage/Checksum/Checksum_3_10.coverage", "fed9b07d-fd23-4227-9556-cab71838db3f");
                includeFileMap.put(file, property);
            }
        } else {
            writeline("/home/ubuntu/results/coverage/Checksum/Checksum_3_10.coverage", "1c3f9661-a676-4246-afd4-d8ecc5d9dc05");
            String message = "Could not find file " + file.getAbsolutePath() + " to generate checksum for.";
            writeline("/home/ubuntu/results/coverage/Checksum/Checksum_3_10.coverage", "c9e2b0e8-db4b-4de4-8ab7-41a5b6fa16ad");
            log(message);
            writeline("/home/ubuntu/results/coverage/Checksum/Checksum_3_10.coverage", "5786a01e-745c-4db2-aed5-569e769f2783");
            throw new BuildException(message, getLocation());
        }
    }

    private File getChecksumFile(File file) {
        writeline("/home/ubuntu/results/coverage/Checksum/Checksum_3_10.coverage", "5d2df50e-f35d-4f15-9132-5b55a819b4da");
        File directory;
        writeline("/home/ubuntu/results/coverage/Checksum/Checksum_3_10.coverage", "3fbc4385-3314-4331-9f84-c4553cd2ef6a");
        if (todir != null) {
            writeline("/home/ubuntu/results/coverage/Checksum/Checksum_3_10.coverage", "d4f2eb5a-ba46-4afa-a0c7-92fa9fbe2e67");
            // A separate directory was explicitly declared
            String path = getRelativeFilePath(file);
            writeline("/home/ubuntu/results/coverage/Checksum/Checksum_3_10.coverage", "f6b43934-e863-4f92-9998-43c301c0a31b");
            directory = new File(todir, path).getParentFile();
            writeline("/home/ubuntu/results/coverage/Checksum/Checksum_3_10.coverage", "8c942347-73a2-441c-b368-43556507c343");
            // Create the directory, as it might not exist.
            directory.mkdirs();
        } else {
            writeline("/home/ubuntu/results/coverage/Checksum/Checksum_3_10.coverage", "4999c304-4c0d-466d-ad14-6b8b6dbba0a8");
            // Just use the same directory as the file itself.
            // This directory will exist
            directory = file.getParentFile();
        }
        writeline("/home/ubuntu/results/coverage/Checksum/Checksum_3_10.coverage", "e3a1a265-269e-4a51-8915-b11ce85b284b");
        File checksumFile = new File(directory, file.getName() + fileext);
        writeline("/home/ubuntu/results/coverage/Checksum/Checksum_3_10.coverage", "cca46576-0934-42f5-9112-9c87b60ba389");
        return checksumFile;
    }

    /**
     * Generate checksum(s) using the message digest created earlier.
     */
    private boolean generateChecksums() throws BuildException {
        writeline("/home/ubuntu/results/coverage/Checksum/Checksum_3_10.coverage", "8454b247-e354-4582-be83-8639793cc1a4");
        boolean checksumMatches = true;
        writeline("/home/ubuntu/results/coverage/Checksum/Checksum_3_10.coverage", "69923f00-7c2b-4bbc-80fe-fee425fcf23a");
        FileInputStream fis = null;
        writeline("/home/ubuntu/results/coverage/Checksum/Checksum_3_10.coverage", "a2b8d8a6-a4af-47a9-9a77-dd51275c9d09");
        FileOutputStream fos = null;
        writeline("/home/ubuntu/results/coverage/Checksum/Checksum_3_10.coverage", "f85baab4-b1a5-4745-beea-8c21ac2b8c49");
        byte[] buf = new byte[readBufferSize];
        writeline("/home/ubuntu/results/coverage/Checksum/Checksum_3_10.coverage", "518490fd-0ccc-4b53-883d-49511aeebebe");
        try {
            writeline("/home/ubuntu/results/coverage/Checksum/Checksum_3_10.coverage", "548e9e97-51df-4988-81f6-85ad2329cb64");
            for (Map.Entry<File, Object> e : includeFileMap.entrySet()) {
                writeline("/home/ubuntu/results/coverage/Checksum/Checksum_3_10.coverage", "cfd17692-b92a-48b8-bd1c-1ddb6b5779e4");
                messageDigest.reset();
                writeline("/home/ubuntu/results/coverage/Checksum/Checksum_3_10.coverage", "6d4d1ea2-54d1-4eff-a82e-f0bb1f83420a");
                File src = e.getKey();
                writeline("/home/ubuntu/results/coverage/Checksum/Checksum_3_10.coverage", "16f46b0b-d881-467e-8b54-488125847cd5");
                if (!isCondition) {
                    writeline("/home/ubuntu/results/coverage/Checksum/Checksum_3_10.coverage", "eea26ae2-a5da-428c-ae94-ef3e613fa300");
                    log("Calculating " + algorithm + " checksum for " + src, Project.MSG_VERBOSE);
                }
                writeline("/home/ubuntu/results/coverage/Checksum/Checksum_3_10.coverage", "ba37ba1f-86bd-4b79-a016-d0a986edd93b");
                fis = new FileInputStream(src);
                writeline("/home/ubuntu/results/coverage/Checksum/Checksum_3_10.coverage", "e250f0f4-8551-416f-8728-ed6464c57dbe");
                DigestInputStream dis = new DigestInputStream(fis, messageDigest);
                writeline("/home/ubuntu/results/coverage/Checksum/Checksum_3_10.coverage", "eb5c51ea-f856-4c1d-a666-09ffe58ff1eb");
                while (dis.read(buf, 0, readBufferSize) != -1) {
                // Empty statement
                }
                writeline("/home/ubuntu/results/coverage/Checksum/Checksum_3_10.coverage", "995c6433-5a16-4a2b-88df-44e26a4eb6bb");
                dis.close();
                writeline("/home/ubuntu/results/coverage/Checksum/Checksum_3_10.coverage", "4402d43c-39bb-4cc0-b315-04af1664b5b0");
                fis.close();
                writeline("/home/ubuntu/results/coverage/Checksum/Checksum_3_10.coverage", "2854e8fd-07db-4848-8502-b6fb12b893e5");
                fis = null;
                writeline("/home/ubuntu/results/coverage/Checksum/Checksum_3_10.coverage", "33387d7b-7697-429b-a6d1-c6e8cbb2ad5f");
                byte[] fileDigest = messageDigest.digest();
                writeline("/home/ubuntu/results/coverage/Checksum/Checksum_3_10.coverage", "1f8d40f8-d4f3-44a2-8238-90da05b16cf9");
                if (totalproperty != null) {
                    writeline("/home/ubuntu/results/coverage/Checksum/Checksum_3_10.coverage", "a7aaa4e1-6819-42d4-96cd-726f43c02350");
                    allDigests.put(src, fileDigest);
                }
                writeline("/home/ubuntu/results/coverage/Checksum/Checksum_3_10.coverage", "c9933f47-3f51-4598-a82d-3bc6f562d5b0");
                String checksum = createDigestString(fileDigest);
                writeline("/home/ubuntu/results/coverage/Checksum/Checksum_3_10.coverage", "13e5e8f8-eb60-42d5-b22b-731d6263e8d5");
                // can either be a property name string or a file
                Object destination = e.getValue();
                writeline("/home/ubuntu/results/coverage/Checksum/Checksum_3_10.coverage", "ce686825-190a-4eed-80ce-acffbd574bd4");
                if (destination instanceof java.lang.String) {
                    writeline("/home/ubuntu/results/coverage/Checksum/Checksum_3_10.coverage", "469f6744-d7bd-4442-9108-b57e8518a3e7");
                    String prop = (String) destination;
                    writeline("/home/ubuntu/results/coverage/Checksum/Checksum_3_10.coverage", "ecc155b7-526b-46e0-8b7b-cabcde976b16");
                    if (isCondition) {
                        writeline("/home/ubuntu/results/coverage/Checksum/Checksum_3_10.coverage", "f4e95a83-4676-4d56-abfd-61f02c826f27");
                        checksumMatches = checksumMatches && checksum.equals(property);
                    } else {
                        writeline("/home/ubuntu/results/coverage/Checksum/Checksum_3_10.coverage", "bcd206d8-a106-4817-8d1c-af2b5b037412");
                        getProject().setNewProperty(prop, checksum);
                    }
                } else if (destination instanceof java.io.File) {
                    writeline("/home/ubuntu/results/coverage/Checksum/Checksum_3_10.coverage", "495c5fe4-04df-4dc6-9274-a33366518c85");
                    if (isCondition) {
                        writeline("/home/ubuntu/results/coverage/Checksum/Checksum_3_10.coverage", "b5fe724f-d9ef-4adf-ab76-084b372f4a5e");
                        File existingFile = (File) destination;
                        writeline("/home/ubuntu/results/coverage/Checksum/Checksum_3_10.coverage", "baf533c6-d2d4-478b-9645-d6e1027f32c1");
                        if (existingFile.exists()) {
                            writeline("/home/ubuntu/results/coverage/Checksum/Checksum_3_10.coverage", "ccbd6294-b2f0-4839-8048-8d6947310713");
                            try {
                                writeline("/home/ubuntu/results/coverage/Checksum/Checksum_3_10.coverage", "1609f575-7855-46a4-85b7-a7dfb5f0dde6");
                                String suppliedChecksum = readChecksum(existingFile);
                                writeline("/home/ubuntu/results/coverage/Checksum/Checksum_3_10.coverage", "589a67c1-3c0b-4a95-b8bb-004c9893deb1");
                                checksumMatches = checksumMatches && checksum.equals(suppliedChecksum);
                            } catch (BuildException be) {
                                writeline("/home/ubuntu/results/coverage/Checksum/Checksum_3_10.coverage", "4361d6cc-d896-4677-be69-46186d086d54");
                                // file is on wrong format, swallow
                                checksumMatches = false;
                            }
                        } else {
                            writeline("/home/ubuntu/results/coverage/Checksum/Checksum_3_10.coverage", "1ecf9586-d949-4f3c-a32f-cec218eb4586");
                            checksumMatches = false;
                        }
                    } else {
                        writeline("/home/ubuntu/results/coverage/Checksum/Checksum_3_10.coverage", "a3eb3547-c573-4bca-b4aa-df0c624233c8");
                        File dest = (File) destination;
                        writeline("/home/ubuntu/results/coverage/Checksum/Checksum_3_10.coverage", "19fb1b20-7d05-4ef5-a678-dbff1e1f24d5");
                        fos = new FileOutputStream(dest);
                        writeline("/home/ubuntu/results/coverage/Checksum/Checksum_3_10.coverage", "66d9fc51-5a73-4e73-b1db-b244cd06ba9d");
                        fos.write(format.format(new Object[] { checksum, src.getName(), FileUtils.getRelativePath(dest.getParentFile(), src), FileUtils.getRelativePath(getProject().getBaseDir(), src), src.getAbsolutePath() }).getBytes());
                        writeline("/home/ubuntu/results/coverage/Checksum/Checksum_3_10.coverage", "84ac19b1-00da-4531-841e-98efa85ca928");
                        fos.write(StringUtils.LINE_SEP.getBytes());
                        writeline("/home/ubuntu/results/coverage/Checksum/Checksum_3_10.coverage", "1f62d021-5296-4690-8d9a-5c3c788e5b71");
                        fos.close();
                        writeline("/home/ubuntu/results/coverage/Checksum/Checksum_3_10.coverage", "206158fe-7b9f-48dd-93c1-595faaeb3b7b");
                        fos = null;
                    }
                }
            }
            writeline("/home/ubuntu/results/coverage/Checksum/Checksum_3_10.coverage", "e04b4021-6f14-4dbc-9bdd-842852fcb198");
            if (totalproperty != null) {
                writeline("/home/ubuntu/results/coverage/Checksum/Checksum_3_10.coverage", "380234c3-87d6-408e-8dee-7bc4029962cf");
                // Calculate the total checksum
                // Convert the keys (source files) into a sorted array.
                File[] keyArray = allDigests.keySet().toArray(new File[allDigests.size()]);
                writeline("/home/ubuntu/results/coverage/Checksum/Checksum_3_10.coverage", "5f4c05e9-d688-4ec8-bc8f-ec4b06a29339");
                // File is Comparable, but sort-order is platform
                // dependent (case-insensitive on Windows)
                Arrays.sort(keyArray, new Comparator<File>() {

                    public int compare(File f1, File f2) {
                        return f1 == null ? (f2 == null ? 0 : -1) : (f2 == null ? 1 : getRelativeFilePath(f1).compareTo(getRelativeFilePath(f2)));
                    }
                });
                writeline("/home/ubuntu/results/coverage/Checksum/Checksum_3_10.coverage", "5a21695c-5904-498e-abaa-099709fa0329");
                // Loop over the checksums and generate a total hash.
                messageDigest.reset();
                writeline("/home/ubuntu/results/coverage/Checksum/Checksum_3_10.coverage", "e8a12841-29a2-47b0-9e93-4282b0375710");
                for (File src : keyArray) {
                    writeline("/home/ubuntu/results/coverage/Checksum/Checksum_3_10.coverage", "3ca1c33d-5bbd-4f7b-b21a-b2ada6ba5cb2");
                    // Add the digest for the file content
                    byte[] digest = allDigests.get(src);
                    writeline("/home/ubuntu/results/coverage/Checksum/Checksum_3_10.coverage", "54463356-8f23-4fcc-b6fe-3ec83d8a6897");
                    messageDigest.update(digest);
                    writeline("/home/ubuntu/results/coverage/Checksum/Checksum_3_10.coverage", "96b5f206-a6ea-47d0-8d07-3ab4ee04b466");
                    // Add the file path
                    String fileName = getRelativeFilePath(src);
                    writeline("/home/ubuntu/results/coverage/Checksum/Checksum_3_10.coverage", "0f3982cb-d1d5-4e4d-a458-c68c7058d1a6");
                    messageDigest.update(fileName.getBytes());
                }
                writeline("/home/ubuntu/results/coverage/Checksum/Checksum_3_10.coverage", "7070692b-5b3f-46e1-8e06-12cf317f3b57");
                String totalChecksum = createDigestString(messageDigest.digest());
                writeline("/home/ubuntu/results/coverage/Checksum/Checksum_3_10.coverage", "067a895c-d5e8-474b-b740-a781ecc55b1c");
                getProject().setNewProperty(totalproperty, totalChecksum);
            }
        } catch (Exception e) {
            writeline("/home/ubuntu/results/coverage/Checksum/Checksum_3_10.coverage", "2b401cb4-2fac-4666-a0a4-f2cd2cbf5985");
            throw new BuildException(e, getLocation());
        } finally {
            writeline("/home/ubuntu/results/coverage/Checksum/Checksum_3_10.coverage", "0adac44f-adf7-4f38-9c2e-8c0e6ca3a69c");
            FileUtils.close(fis);
            writeline("/home/ubuntu/results/coverage/Checksum/Checksum_3_10.coverage", "b98f1dd3-05a1-419a-973e-7268f79f21bf");
            FileUtils.close(fos);
        }
        writeline("/home/ubuntu/results/coverage/Checksum/Checksum_3_10.coverage", "c44b812f-22f5-481b-8933-8f8b82f62ef6");
        return checksumMatches;
    }

    private String createDigestString(byte[] fileDigest) {
        writeline("/home/ubuntu/results/coverage/Checksum/Checksum_3_10.coverage", "0bd305d6-3845-4356-a45f-2a5fd52116b5");
        StringBuffer checksumSb = new StringBuffer();
        writeline("/home/ubuntu/results/coverage/Checksum/Checksum_3_10.coverage", "c2084db3-7098-40cc-aa4e-1555455846ca");
        for (int i = 0; i < fileDigest.length; i++) {
            writeline("/home/ubuntu/results/coverage/Checksum/Checksum_3_10.coverage", "3944a623-29c0-4e4a-8f16-04c72e119f02");
            String hexStr = Integer.toHexString(BYTE_MASK & fileDigest[i]);
            writeline("/home/ubuntu/results/coverage/Checksum/Checksum_3_10.coverage", "f932c3b1-25e6-49ec-ae31-e52b418295ff");
            if (hexStr.length() < 2) {
                writeline("/home/ubuntu/results/coverage/Checksum/Checksum_3_10.coverage", "b4174bab-bc91-46c8-b75e-fce4c24cf60c");
                checksumSb.append("0");
            }
            writeline("/home/ubuntu/results/coverage/Checksum/Checksum_3_10.coverage", "3fb6f7cd-2f43-48f0-8213-497726fc949b");
            checksumSb.append(hexStr);
        }
        writeline("/home/ubuntu/results/coverage/Checksum/Checksum_3_10.coverage", "334fa541-3758-44d0-a1c7-1d72c4a7b90d");
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
        writelineStatic("/home/ubuntu/results/coverage/Checksum/Checksum_3_10.coverage", "e7ecbce6-ee6f-4c68-9ca1-9dd3dc8ab4e6");
        int l = data.length;
        writelineStatic("/home/ubuntu/results/coverage/Checksum/Checksum_3_10.coverage", "0164cddc-a450-46c7-ba94-0819bbea2fec");
        if ((l & 0x01) != 0) {
            writelineStatic("/home/ubuntu/results/coverage/Checksum/Checksum_3_10.coverage", "3c70b7ce-aa05-4a44-a4f4-bd43a411110b");
            throw new BuildException("odd number of characters.");
        }
        writelineStatic("/home/ubuntu/results/coverage/Checksum/Checksum_3_10.coverage", "f370ea37-f683-4466-816c-4914b214d5f5");
        byte[] out = new byte[l >> 1];
        writelineStatic("/home/ubuntu/results/coverage/Checksum/Checksum_3_10.coverage", "d112a698-a571-42e8-a229-fe4dc3a32626");
        // two characters form the hex value.
        for (int i = 0, j = 0; j < l; i++) {
            writelineStatic("/home/ubuntu/results/coverage/Checksum/Checksum_3_10.coverage", "1cd4b968-8aac-40f0-8690-8e4719a517a0");
            int f = Character.digit(data[j++], WORD) << NIBBLE;
            writelineStatic("/home/ubuntu/results/coverage/Checksum/Checksum_3_10.coverage", "cad50ff4-265f-4169-9d6e-78b62b8a80e9");
            f = f | Character.digit(data[j++], WORD);
            writelineStatic("/home/ubuntu/results/coverage/Checksum/Checksum_3_10.coverage", "40b92a44-748f-46f3-834d-af003833454b");
            out[i] = (byte) (f & BYTE_MASK);
        }
        writelineStatic("/home/ubuntu/results/coverage/Checksum/Checksum_3_10.coverage", "a396f878-62bd-4d68-b5ff-0fbad26d2438");
        return out;
    }

    /**
     * reads the checksum from a file using the specified format.
     *
     * @since 1.7
     */
    private String readChecksum(File f) {
        writeline("/home/ubuntu/results/coverage/Checksum/Checksum_3_10.coverage", "8c3cfa3e-7f17-4a95-a1f6-67fd800dc739");
        BufferedReader diskChecksumReader = null;
        writeline("/home/ubuntu/results/coverage/Checksum/Checksum_3_10.coverage", "0a95b0b0-a5b4-49af-aa24-10a348e7c837");
        try {
            writeline("/home/ubuntu/results/coverage/Checksum/Checksum_3_10.coverage", "95777602-fd52-4ada-9219-8a868ede1afb");
            diskChecksumReader = new BufferedReader(new FileReader(f));
            writeline("/home/ubuntu/results/coverage/Checksum/Checksum_3_10.coverage", "e959d785-b764-4d07-a506-5d5133a87702");
            Object[] result = format.parse(diskChecksumReader.readLine());
            writeline("/home/ubuntu/results/coverage/Checksum/Checksum_3_10.coverage", "568d28ed-b37d-4f58-a13b-a1b5e670a8f2");
            if (result == null || result.length == 0 || result[0] == null) {
                writeline("/home/ubuntu/results/coverage/Checksum/Checksum_3_10.coverage", "cbbe4550-99ae-4908-925b-f701bc850874");
                throw new BuildException("failed to find a checksum");
            }
            writeline("/home/ubuntu/results/coverage/Checksum/Checksum_3_10.coverage", "389f0a7c-5923-4a73-869e-366d0d823888");
            return (String) result[0];
        } catch (IOException e) {
            writeline("/home/ubuntu/results/coverage/Checksum/Checksum_3_10.coverage", "6b9497d2-9938-40c9-9d06-b47c3d974153");
            throw new BuildException("Couldn't read checksum file " + f, e);
        } catch (ParseException e) {
            writeline("/home/ubuntu/results/coverage/Checksum/Checksum_3_10.coverage", "1a1153be-1d0f-492e-86dc-a344418dc9f4");
            throw new BuildException("Couldn't read checksum file " + f, e);
        } finally {
            writeline("/home/ubuntu/results/coverage/Checksum/Checksum_3_10.coverage", "ffdb28a2-1014-44f8-aa46-5177937c6ae5");
            FileUtils.close(diskChecksumReader);
        }
    }

    /**
     * @since Ant 1.8.2
     */
    private String getRelativeFilePath(File f) {
        writeline("/home/ubuntu/results/coverage/Checksum/Checksum_3_10.coverage", "04324dfe-2c5b-45c8-ab40-75eb23d273f8");
        String path = (String) relativeFilePaths.get(f);
        writeline("/home/ubuntu/results/coverage/Checksum/Checksum_3_10.coverage", "f9a2a4e2-b9bf-4b84-b4c4-543f92ee8a82");
        if (path == null) {
            writeline("/home/ubuntu/results/coverage/Checksum/Checksum_3_10.coverage", "23164920-8d00-4132-8010-e9b99d823b00");
            // bug 37386. this should not occur, but it has, once.
            throw new BuildException("Internal error: " + "relativeFilePaths could not match file " + f + "\n" + "please file a bug report on this");
        }
        writeline("/home/ubuntu/results/coverage/Checksum/Checksum_3_10.coverage", "27bcb000-284c-40a8-b7fb-8ac07115095f");
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
