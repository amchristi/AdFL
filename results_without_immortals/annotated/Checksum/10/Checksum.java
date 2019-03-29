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
        writeline("/home/ubuntu/results/coverage/Checksum/Checksum_10_10.coverage", "d62b3079-9414-4de9-9657-78090e57cbba");
        this.file = file;
    }

    /**
     * Sets the root directory where checksum files will be
     * written/read
     * @param todir the directory to write to
     * @since Ant 1.6
     */
    public void setTodir(File todir) {
        writeline("/home/ubuntu/results/coverage/Checksum/Checksum_10_10.coverage", "75798cc3-8e63-422b-8350-e8b3eb14b203");
        this.todir = todir;
    }

    /**
     * Specifies the algorithm to be used to compute the checksum.
     * Defaults to "MD5". Other popular algorithms like "SHA" may be used as well.
     * @param algorithm a <code>String</code> value
     */
    public void setAlgorithm(String algorithm) {
        writeline("/home/ubuntu/results/coverage/Checksum/Checksum_10_10.coverage", "dec79e3d-86c5-48fe-ad9b-256e377a9a8d");
        this.algorithm = algorithm;
    }

    /**
     * Sets the MessageDigest algorithm provider to be used
     * to calculate the checksum.
     * @param provider a <code>String</code> value
     */
    public void setProvider(String provider) {
        writeline("/home/ubuntu/results/coverage/Checksum/Checksum_10_10.coverage", "9ea89243-7a5a-4a02-b42c-ae5aee98ebdf");
        this.provider = provider;
    }

    /**
     * Sets the file extension that is be to used to
     * create or identify destination file.
     * @param fileext a <code>String</code> value
     */
    public void setFileext(String fileext) {
        writeline("/home/ubuntu/results/coverage/Checksum/Checksum_10_10.coverage", "6674d17e-30f1-41da-a1de-d4ec58005e0b");
        this.fileext = fileext;
    }

    /**
     * Sets the property to hold the generated checksum.
     * @param property a <code>String</code> value
     */
    public void setProperty(String property) {
        writeline("/home/ubuntu/results/coverage/Checksum/Checksum_10_10.coverage", "b98262ef-4720-43f5-9b3f-b01e159ef630");
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
        writeline("/home/ubuntu/results/coverage/Checksum/Checksum_10_10.coverage", "2db3a83b-f91b-4b84-857f-e8a9c5da8a87");
        this.totalproperty = totalproperty;
    }

    /**
     * Sets the verify property.  This project property holds
     * the result of a checksum verification - "true" or "false"
     * @param verifyProperty a <code>String</code> value
     */
    public void setVerifyproperty(String verifyProperty) {
        writeline("/home/ubuntu/results/coverage/Checksum/Checksum_10_10.coverage", "8b36c995-1ff2-400b-a1fb-d0573ddaa177");
        this.verifyProperty = verifyProperty;
    }

    /**
     * Whether or not to overwrite existing file irrespective of
     * whether it is newer than
     * the source file.  Defaults to false.
     * @param forceOverwrite a <code>boolean</code> value
     */
    public void setForceOverwrite(boolean forceOverwrite) {
        writeline("/home/ubuntu/results/coverage/Checksum/Checksum_10_10.coverage", "bae08ee4-e521-4e71-b8bd-de44c0561035");
        this.forceOverwrite = forceOverwrite;
    }

    /**
     * The size of the read buffer to use.
     * @param size an <code>int</code> value
     */
    public void setReadBufferSize(int size) {
        writeline("/home/ubuntu/results/coverage/Checksum/Checksum_10_10.coverage", "2c1a7aa7-f9b7-4d75-8794-f5f63cce9de1");
        this.readBufferSize = size;
    }

    /**
     * Select the in/output pattern via a well know format name.
     * @param e an <code>enumerated</code> value
     *
     * @since 1.7.0
     */
    public void setFormat(FormatElement e) {
        writeline("/home/ubuntu/results/coverage/Checksum/Checksum_10_10.coverage", "fba89d67-a5f0-4c09-af8c-032eb98e19a8");
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
        writeline("/home/ubuntu/results/coverage/Checksum/Checksum_10_10.coverage", "a368c795-697f-45fc-9016-9176e88f2079");
        format = new MessageFormat(p);
    }

    /**
     * Files to generate checksums for.
     * @param set a fileset of files to generate checksums for.
     */
    public void addFileset(FileSet set) {
        writeline("/home/ubuntu/results/coverage/Checksum/Checksum_10_10.coverage", "8e46120c-ec28-42ae-ad4b-62165043d2bb");
        add(set);
    }

    /**
     * Add a resource collection.
     * @param rc the ResourceCollection to add.
     */
    public void add(ResourceCollection rc) {
        writeline("/home/ubuntu/results/coverage/Checksum/Checksum_10_10.coverage", "11dcff85-fecd-42c0-9bb6-bb52619e04d0");
        if (rc == null) {
            writeline("/home/ubuntu/results/coverage/Checksum/Checksum_10_10.coverage", "b0e1ffbe-4389-4fb4-88ec-c5976c7b694b");
            return;
        }
        writeline("/home/ubuntu/results/coverage/Checksum/Checksum_10_10.coverage", "e7404139-9f57-4e01-bd6e-cb6bdab03f9d");
        resources = (resources == null) ? new FileUnion() : resources;
        writeline("/home/ubuntu/results/coverage/Checksum/Checksum_10_10.coverage", "9f050afd-5317-409c-a9f3-ecdadc8d82a7");
        resources.add(rc);
    }

    /**
     * Calculate the checksum(s).
     * @throws BuildException on error
     */
    public void execute() throws BuildException {
        writeline("/home/ubuntu/results/coverage/Checksum/Checksum_10_10.coverage", "a0e15d02-9a26-44d4-906b-c0bbd0f20ccf");
        isCondition = false;
        writeline("/home/ubuntu/results/coverage/Checksum/Checksum_10_10.coverage", "00a558c3-db4f-4440-b6db-fc77b116c7d6");
        boolean value = validateAndExecute();
        writeline("/home/ubuntu/results/coverage/Checksum/Checksum_10_10.coverage", "82ebf4ac-ba6e-484a-8e5a-aa8b2767c8d3");
        if (verifyProperty != null) {
            writeline("/home/ubuntu/results/coverage/Checksum/Checksum_10_10.coverage", "7c6edf67-7969-4cf6-9c5e-be402a2eee9d");
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
        writeline("/home/ubuntu/results/coverage/Checksum/Checksum_10_10.coverage", "0c9f076e-f9c3-41e2-b27e-95456b002e4c");
        isCondition = true;
        writeline("/home/ubuntu/results/coverage/Checksum/Checksum_10_10.coverage", "692a1dc5-eddd-432b-bf07-c3fe6407e476");
        return validateAndExecute();
    }

    /**
     * Validate attributes and get down to business.
     */
    private boolean validateAndExecute() throws BuildException {
        writeline("/home/ubuntu/results/coverage/Checksum/Checksum_10_10.coverage", "6a523862-5e8a-4508-9595-db3bb3f749c8");
        String savedFileExt = fileext;
        writeline("/home/ubuntu/results/coverage/Checksum/Checksum_10_10.coverage", "63dbd44a-c1bd-4ebb-856c-0366e2f1b383");
        if (file == null && (resources == null || resources.size() == 0)) {
            writeline("/home/ubuntu/results/coverage/Checksum/Checksum_10_10.coverage", "018140d6-eb5b-48f4-80e4-e3782e0a43c7");
            throw new BuildException("Specify at least one source - a file or a resource collection.");
        }
        writeline("/home/ubuntu/results/coverage/Checksum/Checksum_10_10.coverage", "d7545465-53a4-445a-a430-e087d8042d70");
        if (!(resources == null || resources.isFilesystemOnly())) {
            writeline("/home/ubuntu/results/coverage/Checksum/Checksum_10_10.coverage", "336474da-89eb-4fb5-8f6b-82758b45f2c6");
            throw new BuildException("Can only calculate checksums for file-based resources.");
        }
        writeline("/home/ubuntu/results/coverage/Checksum/Checksum_10_10.coverage", "12002b88-f884-43e0-8992-9643dbe4cf01");
        if (file != null && file.exists() && file.isDirectory()) {
            writeline("/home/ubuntu/results/coverage/Checksum/Checksum_10_10.coverage", "0f8f5f22-6ece-4fa3-b8fa-720195762023");
            throw new BuildException("Checksum cannot be generated for directories");
        }
        writeline("/home/ubuntu/results/coverage/Checksum/Checksum_10_10.coverage", "1e7d7b53-3b9b-40c2-890d-3215249dbc9b");
        if (file != null && totalproperty != null) {
            writeline("/home/ubuntu/results/coverage/Checksum/Checksum_10_10.coverage", "6e614bab-7955-451c-a75d-b69832b17216");
            throw new BuildException("File and Totalproperty cannot co-exist.");
        }
        writeline("/home/ubuntu/results/coverage/Checksum/Checksum_10_10.coverage", "22cc7968-dd9a-4fe1-bb87-33f38c4351d3");
        if (property != null && fileext != null) {
            writeline("/home/ubuntu/results/coverage/Checksum/Checksum_10_10.coverage", "f810e80a-f2f2-4bdc-9710-7061321adefa");
            throw new BuildException("Property and FileExt cannot co-exist.");
        }
        writeline("/home/ubuntu/results/coverage/Checksum/Checksum_10_10.coverage", "249803a6-0425-4f32-94df-25eef52222d0");
        if (property != null) {
            writeline("/home/ubuntu/results/coverage/Checksum/Checksum_10_10.coverage", "dff5afd9-fc1f-4d72-9848-106f7618031a");
            if (forceOverwrite) {
                writeline("/home/ubuntu/results/coverage/Checksum/Checksum_10_10.coverage", "33a668c2-5f7f-4b20-a9cb-801234d59f9f");
                throw new BuildException("ForceOverwrite cannot be used when Property is specified");
            }
            writeline("/home/ubuntu/results/coverage/Checksum/Checksum_10_10.coverage", "e5c319cd-a7b7-41b2-9e6b-c0d31295de76");
            int ct = 0;
            writeline("/home/ubuntu/results/coverage/Checksum/Checksum_10_10.coverage", "6c1d0f15-f1b1-458b-847c-4990216c20dc");
            if (resources != null) {
                writeline("/home/ubuntu/results/coverage/Checksum/Checksum_10_10.coverage", "cfbab571-9f75-4a7f-8121-9fab7b63b6db");
                ct += resources.size();
            }
            writeline("/home/ubuntu/results/coverage/Checksum/Checksum_10_10.coverage", "2880cc7b-ce19-415e-bc33-352be4dfc42a");
            if (file != null) {
                writeline("/home/ubuntu/results/coverage/Checksum/Checksum_10_10.coverage", "a73db2a2-b0a7-468f-b8b2-27c85112c958");
                ct++;
            }
            writeline("/home/ubuntu/results/coverage/Checksum/Checksum_10_10.coverage", "acb8459c-6e64-4be6-8692-2e609eb0c107");
            if (ct > 1) {
                writeline("/home/ubuntu/results/coverage/Checksum/Checksum_10_10.coverage", "c577193a-427f-4284-b429-7e9884f76667");
                throw new BuildException("Multiple files cannot be used when Property is specified");
            }
        }
        writeline("/home/ubuntu/results/coverage/Checksum/Checksum_10_10.coverage", "a1db20a0-856e-4bce-9ed3-4c76e3b2ff0b");
        if (verifyProperty != null) {
            writeline("/home/ubuntu/results/coverage/Checksum/Checksum_10_10.coverage", "55f59711-f8c4-43d8-bd9c-a54902e65ae1");
            isCondition = true;
        }
        writeline("/home/ubuntu/results/coverage/Checksum/Checksum_10_10.coverage", "9bd4d098-6267-447b-b848-8cd1c0a94e2f");
        if (verifyProperty != null && forceOverwrite) {
            writeline("/home/ubuntu/results/coverage/Checksum/Checksum_10_10.coverage", "c6ca0cd1-be63-413b-9b8c-d5f8458c4835");
            throw new BuildException("VerifyProperty and ForceOverwrite cannot co-exist.");
        }
        writeline("/home/ubuntu/results/coverage/Checksum/Checksum_10_10.coverage", "b266f535-4b07-4b26-84f7-4e883d1920b7");
        if (isCondition && forceOverwrite) {
            writeline("/home/ubuntu/results/coverage/Checksum/Checksum_10_10.coverage", "5608f0bd-31f8-4991-a297-ad422dc4e463");
            throw new BuildException("ForceOverwrite cannot be used when conditions are being used.");
        }
        writeline("/home/ubuntu/results/coverage/Checksum/Checksum_10_10.coverage", "b8d38e31-cf51-4dff-802b-01fc850860aa");
        messageDigest = null;
        writeline("/home/ubuntu/results/coverage/Checksum/Checksum_10_10.coverage", "7bbb2e4a-6c91-41d9-8b84-6b7e8f3a2ab5");
        if (provider != null) {
            writeline("/home/ubuntu/results/coverage/Checksum/Checksum_10_10.coverage", "1a050347-f610-474b-8c04-4c357084166f");
            try {
                writeline("/home/ubuntu/results/coverage/Checksum/Checksum_10_10.coverage", "73177dbb-cdec-4b9b-8050-1b81e6461798");
                messageDigest = MessageDigest.getInstance(algorithm, provider);
            } catch (NoSuchAlgorithmException noalgo) {
                writeline("/home/ubuntu/results/coverage/Checksum/Checksum_10_10.coverage", "2ccc77f3-1bc1-4963-bc56-4af3c0ac1b7b");
                throw new BuildException(noalgo, getLocation());
            } catch (NoSuchProviderException noprovider) {
                writeline("/home/ubuntu/results/coverage/Checksum/Checksum_10_10.coverage", "76853f54-2d36-4b4b-8875-2c702a1a4a00");
                throw new BuildException(noprovider, getLocation());
            }
        } else {
            writeline("/home/ubuntu/results/coverage/Checksum/Checksum_10_10.coverage", "c30fb7c0-7573-46f8-b754-93e8197718b7");
            try {
                writeline("/home/ubuntu/results/coverage/Checksum/Checksum_10_10.coverage", "de2c8960-197b-4796-aed1-41c11f3357d2");
                messageDigest = MessageDigest.getInstance(algorithm);
            } catch (NoSuchAlgorithmException noalgo) {
                writeline("/home/ubuntu/results/coverage/Checksum/Checksum_10_10.coverage", "b151189a-dd47-49aa-b793-01e107f68403");
                throw new BuildException(noalgo, getLocation());
            }
        }
        writeline("/home/ubuntu/results/coverage/Checksum/Checksum_10_10.coverage", "f5bbbe9a-3427-4881-a055-8c72dd97a9f5");
        if (messageDigest == null) {
            writeline("/home/ubuntu/results/coverage/Checksum/Checksum_10_10.coverage", "a2f3520f-2279-41db-9b59-057b5156c697");
            throw new BuildException("Unable to create Message Digest", getLocation());
        }
        writeline("/home/ubuntu/results/coverage/Checksum/Checksum_10_10.coverage", "8618a7c2-f5fa-4896-9fc8-d84df2afbb83");
        if (fileext == null) {
            writeline("/home/ubuntu/results/coverage/Checksum/Checksum_10_10.coverage", "19c04105-630f-4c22-87ae-ad8b9396f525");
            fileext = "." + algorithm;
        } else if (fileext.trim().length() == 0) {
            writeline("/home/ubuntu/results/coverage/Checksum/Checksum_10_10.coverage", "8a07bbe6-a847-49f2-8e38-0b0b6eb8b56f");
            throw new BuildException("File extension when specified must not be an empty string");
        }
        writeline("/home/ubuntu/results/coverage/Checksum/Checksum_10_10.coverage", "c0932b62-0476-4f88-b4ce-c2c95fc68922");
        try {
            writeline("/home/ubuntu/results/coverage/Checksum/Checksum_10_10.coverage", "247027c0-223f-4198-ab29-1e9f777ff6b5");
            if (resources != null) {
                writeline("/home/ubuntu/results/coverage/Checksum/Checksum_10_10.coverage", "08c1f646-26c5-48b1-ae70-44197c1feae9");
                for (Resource r : resources) {
                    writeline("/home/ubuntu/results/coverage/Checksum/Checksum_10_10.coverage", "99e779a7-d47a-4f7c-908e-ff0ea1823730");
                    File src = r.as(FileProvider.class).getFile();
                    writeline("/home/ubuntu/results/coverage/Checksum/Checksum_10_10.coverage", "d27a54b8-4849-489c-af4a-202fb42fd3fe");
                    if (totalproperty != null || todir != null) {
                        writeline("/home/ubuntu/results/coverage/Checksum/Checksum_10_10.coverage", "bb6aed7f-182b-4337-8d9c-2e1b7fb42887");
                        // Use '/' to calculate digest based on file name.
                        // This is required in order to get the same result
                        // on different platforms.
                        relativeFilePaths.put(src, r.getName().replace(File.separatorChar, '/'));
                    }
                    writeline("/home/ubuntu/results/coverage/Checksum/Checksum_10_10.coverage", "2c94001d-9b7d-4d2a-8e03-fd7cc729211a");
                    addToIncludeFileMap(src);
                }
            }
            writeline("/home/ubuntu/results/coverage/Checksum/Checksum_10_10.coverage", "f2a4be1e-692d-4b72-90fe-53ce4e172e62");
            if (file != null) {
                writeline("/home/ubuntu/results/coverage/Checksum/Checksum_10_10.coverage", "aca61b73-508d-4ef3-9b91-0b4401ad3f76");
                if (totalproperty != null || todir != null) {
                    writeline("/home/ubuntu/results/coverage/Checksum/Checksum_10_10.coverage", "62794a7c-6dd7-4aed-8583-1a9cfb726a3c");
                    relativeFilePaths.put(file, file.getName().replace(File.separatorChar, '/'));
                }
                writeline("/home/ubuntu/results/coverage/Checksum/Checksum_10_10.coverage", "c0c6ba94-b849-43c1-baa6-b9570947776b");
                addToIncludeFileMap(file);
            }
            writeline("/home/ubuntu/results/coverage/Checksum/Checksum_10_10.coverage", "82b1eccc-cd99-4edd-ad5d-20cb70818aa7");
            return generateChecksums();
        } finally {
            writeline("/home/ubuntu/results/coverage/Checksum/Checksum_10_10.coverage", "fb02c07a-b0eb-484c-b002-306a14a7e4b7");
            fileext = savedFileExt;
            writeline("/home/ubuntu/results/coverage/Checksum/Checksum_10_10.coverage", "a36bde6d-2de5-4c83-ab07-d743ff9a120a");
            includeFileMap.clear();
        }
    }

    /**
     * Add key-value pair to the hashtable upon which
     * to later operate upon.
     */
    private void addToIncludeFileMap(File file) throws BuildException {
        writeline("/home/ubuntu/results/coverage/Checksum/Checksum_10_10.coverage", "f6f89f0b-3f79-4d40-8335-b26b803e1e8a");
        if (file.exists()) {
            writeline("/home/ubuntu/results/coverage/Checksum/Checksum_10_10.coverage", "1e1c8946-4eea-4484-a832-f8b9fc6e598b");
            if (property == null) {
                writeline("/home/ubuntu/results/coverage/Checksum/Checksum_10_10.coverage", "ac78a6de-5a8e-4e0f-88f0-a6d745c62730");
                File checksumFile = getChecksumFile(file);
                writeline("/home/ubuntu/results/coverage/Checksum/Checksum_10_10.coverage", "0f1b9654-a5cf-48a2-88b5-aa39c7b812e9");
                if (forceOverwrite || isCondition || (file.lastModified() > checksumFile.lastModified())) {
                    writeline("/home/ubuntu/results/coverage/Checksum/Checksum_10_10.coverage", "7eac18c0-5883-41af-873f-d001925b0421");
                    includeFileMap.put(file, checksumFile);
                } else {
                    writeline("/home/ubuntu/results/coverage/Checksum/Checksum_10_10.coverage", "50a7c06e-3536-484e-b744-c7a927e12470");
                    log(file + " omitted as " + checksumFile + " is up to date.", Project.MSG_VERBOSE);
                    writeline("/home/ubuntu/results/coverage/Checksum/Checksum_10_10.coverage", "b9e4cc83-6878-4266-94a7-36d67b01bc13");
                    if (totalproperty != null) {
                        writeline("/home/ubuntu/results/coverage/Checksum/Checksum_10_10.coverage", "2811a3e4-81bc-46c4-90ec-bbb5c8d945da");
                        // Read the checksum from disk.
                        String checksum = readChecksum(checksumFile);
                        writeline("/home/ubuntu/results/coverage/Checksum/Checksum_10_10.coverage", "611d2d40-6b12-4775-8004-6848e1e9687e");
                        byte[] digest = decodeHex(checksum.toCharArray());
                        writeline("/home/ubuntu/results/coverage/Checksum/Checksum_10_10.coverage", "67ab911e-e831-4446-a06e-1ec5a9bec2c0");
                        allDigests.put(file, digest);
                    }
                }
            } else {
                writeline("/home/ubuntu/results/coverage/Checksum/Checksum_10_10.coverage", "14c75de2-aa04-4bbc-9777-ab64720bce65");
                includeFileMap.put(file, property);
            }
        } else {
            writeline("/home/ubuntu/results/coverage/Checksum/Checksum_10_10.coverage", "979fce78-9bcf-4031-bc21-3e6924fcd729");
            String message = "Could not find file " + file.getAbsolutePath() + " to generate checksum for.";
            writeline("/home/ubuntu/results/coverage/Checksum/Checksum_10_10.coverage", "330f109e-7788-4d36-87e1-b0d2d5b09fa2");
            log(message);
            writeline("/home/ubuntu/results/coverage/Checksum/Checksum_10_10.coverage", "7285a772-939f-4411-9ed6-d6d183bfc998");
            throw new BuildException(message, getLocation());
        }
    }

    private File getChecksumFile(File file) {
        writeline("/home/ubuntu/results/coverage/Checksum/Checksum_10_10.coverage", "55d96e4c-3d0a-4ba7-8009-0600edc3deb1");
        File directory;
        writeline("/home/ubuntu/results/coverage/Checksum/Checksum_10_10.coverage", "928785f6-b138-457f-b6f2-ea6411ce2111");
        if (todir != null) {
            writeline("/home/ubuntu/results/coverage/Checksum/Checksum_10_10.coverage", "55f38717-a5cf-4690-9fb5-c09b9bbc3204");
            // A separate directory was explicitly declared
            String path = getRelativeFilePath(file);
            writeline("/home/ubuntu/results/coverage/Checksum/Checksum_10_10.coverage", "f4c22106-3ec4-439b-b308-652b303d4832");
            directory = new File(todir, path).getParentFile();
            writeline("/home/ubuntu/results/coverage/Checksum/Checksum_10_10.coverage", "ba76a97c-f3c7-46fe-86ad-3b062de65b42");
            // Create the directory, as it might not exist.
            directory.mkdirs();
        } else {
            writeline("/home/ubuntu/results/coverage/Checksum/Checksum_10_10.coverage", "eb85f294-cae7-4c87-b799-90b890acd402");
            // Just use the same directory as the file itself.
            // This directory will exist
            directory = file.getParentFile();
        }
        writeline("/home/ubuntu/results/coverage/Checksum/Checksum_10_10.coverage", "22c84fc1-f904-46c6-8597-7fbf901e6227");
        File checksumFile = new File(directory, file.getName() + fileext);
        writeline("/home/ubuntu/results/coverage/Checksum/Checksum_10_10.coverage", "4d65f8ad-c989-4b62-b15b-89c28262d0fe");
        return checksumFile;
    }

    /**
     * Generate checksum(s) using the message digest created earlier.
     */
    private boolean generateChecksums() throws BuildException {
        writeline("/home/ubuntu/results/coverage/Checksum/Checksum_10_10.coverage", "058497f2-4f96-4757-8106-370675b687f2");
        boolean checksumMatches = true;
        writeline("/home/ubuntu/results/coverage/Checksum/Checksum_10_10.coverage", "59d38334-30e7-4a6c-932b-0a42c22d4e66");
        FileInputStream fis = null;
        writeline("/home/ubuntu/results/coverage/Checksum/Checksum_10_10.coverage", "d2f9be21-fba5-4d2f-b5f2-e4d0dfa4ac27");
        FileOutputStream fos = null;
        writeline("/home/ubuntu/results/coverage/Checksum/Checksum_10_10.coverage", "f146fad7-6a3f-4f71-a4b9-1969a341d4aa");
        byte[] buf = new byte[readBufferSize];
        writeline("/home/ubuntu/results/coverage/Checksum/Checksum_10_10.coverage", "ad4fe7d5-af1f-49cb-adae-b26c637e1d9c");
        try {
            writeline("/home/ubuntu/results/coverage/Checksum/Checksum_10_10.coverage", "046fad40-9ab9-4016-af20-3eab77c05045");
            for (Map.Entry<File, Object> e : includeFileMap.entrySet()) {
                writeline("/home/ubuntu/results/coverage/Checksum/Checksum_10_10.coverage", "69296bc7-0b79-4727-b8fd-92fceb579848");
                messageDigest.reset();
                writeline("/home/ubuntu/results/coverage/Checksum/Checksum_10_10.coverage", "6b1c0b5c-e6f2-4562-84f6-e7dcb393cf3f");
                File src = e.getKey();
                writeline("/home/ubuntu/results/coverage/Checksum/Checksum_10_10.coverage", "f2fffcea-3deb-42fd-bfec-39d12539a889");
                if (!isCondition) {
                    writeline("/home/ubuntu/results/coverage/Checksum/Checksum_10_10.coverage", "b7f975d4-fac0-463c-a4ea-35796b53fb3c");
                    log("Calculating " + algorithm + " checksum for " + src, Project.MSG_VERBOSE);
                }
                writeline("/home/ubuntu/results/coverage/Checksum/Checksum_10_10.coverage", "dc755242-16d4-414e-b5f4-a7712538da53");
                fis = new FileInputStream(src);
                writeline("/home/ubuntu/results/coverage/Checksum/Checksum_10_10.coverage", "cf5ed4cf-f1b0-4676-b9c5-3ca3b2069af2");
                DigestInputStream dis = new DigestInputStream(fis, messageDigest);
                writeline("/home/ubuntu/results/coverage/Checksum/Checksum_10_10.coverage", "34102008-c608-474e-8b85-1c90a10a47fb");
                while (dis.read(buf, 0, readBufferSize) != -1) {
                // Empty statement
                }
                writeline("/home/ubuntu/results/coverage/Checksum/Checksum_10_10.coverage", "574ba97a-a5aa-44ca-b881-2c1dfe4ca050");
                dis.close();
                writeline("/home/ubuntu/results/coverage/Checksum/Checksum_10_10.coverage", "00357669-4dc7-43f6-b420-e82c4b2a89e5");
                fis.close();
                writeline("/home/ubuntu/results/coverage/Checksum/Checksum_10_10.coverage", "85218665-61da-4e83-bacc-a7bf7c333b1b");
                fis = null;
                writeline("/home/ubuntu/results/coverage/Checksum/Checksum_10_10.coverage", "2ea0819e-0581-4148-9765-7920ea560556");
                byte[] fileDigest = messageDigest.digest();
                writeline("/home/ubuntu/results/coverage/Checksum/Checksum_10_10.coverage", "f0042346-bc84-4365-b70a-3a8954501936");
                if (totalproperty != null) {
                    writeline("/home/ubuntu/results/coverage/Checksum/Checksum_10_10.coverage", "ae959e8a-d392-445f-aea7-788b1dde02e6");
                    allDigests.put(src, fileDigest);
                }
                writeline("/home/ubuntu/results/coverage/Checksum/Checksum_10_10.coverage", "40bec596-42c4-45f2-b8d6-b188c67650e8");
                String checksum = createDigestString(fileDigest);
                writeline("/home/ubuntu/results/coverage/Checksum/Checksum_10_10.coverage", "47825de1-7034-4b91-b005-c0b754251c10");
                // can either be a property name string or a file
                Object destination = e.getValue();
                writeline("/home/ubuntu/results/coverage/Checksum/Checksum_10_10.coverage", "768f7363-798a-4acc-b7f4-df090dfd4aff");
                if (destination instanceof java.lang.String) {
                    writeline("/home/ubuntu/results/coverage/Checksum/Checksum_10_10.coverage", "a1b2d470-b27b-4232-a7a2-6e3862cf725a");
                    String prop = (String) destination;
                    writeline("/home/ubuntu/results/coverage/Checksum/Checksum_10_10.coverage", "070b726e-bee5-4a9e-97ea-7629ce53c174");
                    if (isCondition) {
                        writeline("/home/ubuntu/results/coverage/Checksum/Checksum_10_10.coverage", "5c66db71-0e75-45f4-a481-45c3bae8e661");
                        checksumMatches = checksumMatches && checksum.equals(property);
                    } else {
                        writeline("/home/ubuntu/results/coverage/Checksum/Checksum_10_10.coverage", "759add4e-3891-4205-9a4d-442eaab87eb5");
                        getProject().setNewProperty(prop, checksum);
                    }
                } else if (destination instanceof java.io.File) {
                    writeline("/home/ubuntu/results/coverage/Checksum/Checksum_10_10.coverage", "61b8e762-ee52-48b5-92eb-1b2c85f80a31");
                    if (isCondition) {
                        writeline("/home/ubuntu/results/coverage/Checksum/Checksum_10_10.coverage", "4ec48f13-b778-4ea6-8d0b-de7ee62fdce0");
                        File existingFile = (File) destination;
                        writeline("/home/ubuntu/results/coverage/Checksum/Checksum_10_10.coverage", "0e82579c-6fad-470b-abe7-cffabd02590b");
                        if (existingFile.exists()) {
                            writeline("/home/ubuntu/results/coverage/Checksum/Checksum_10_10.coverage", "744605cf-a93a-4f1f-a3ae-08d3728f236f");
                            try {
                                writeline("/home/ubuntu/results/coverage/Checksum/Checksum_10_10.coverage", "b58f5de5-26c9-442e-82d1-8f1025541aea");
                                String suppliedChecksum = readChecksum(existingFile);
                                writeline("/home/ubuntu/results/coverage/Checksum/Checksum_10_10.coverage", "d626d70b-e2ff-47d9-b61b-5bbf7959cf62");
                                checksumMatches = checksumMatches && checksum.equals(suppliedChecksum);
                            } catch (BuildException be) {
                                writeline("/home/ubuntu/results/coverage/Checksum/Checksum_10_10.coverage", "7f007fc4-5320-4fac-90cb-3bb9799407bf");
                                // file is on wrong format, swallow
                                checksumMatches = false;
                            }
                        } else {
                            writeline("/home/ubuntu/results/coverage/Checksum/Checksum_10_10.coverage", "6428b2fc-1667-4287-bc95-c93d78f6c287");
                            checksumMatches = false;
                        }
                    } else {
                        writeline("/home/ubuntu/results/coverage/Checksum/Checksum_10_10.coverage", "8d280454-5e66-4747-a9b3-7c6fd6ed1675");
                        File dest = (File) destination;
                        writeline("/home/ubuntu/results/coverage/Checksum/Checksum_10_10.coverage", "840fde70-0da7-4384-ae66-beff0f738aeb");
                        fos = new FileOutputStream(dest);
                        writeline("/home/ubuntu/results/coverage/Checksum/Checksum_10_10.coverage", "49933a49-dc04-4707-a470-c4d2a8326b2e");
                        fos.write(format.format(new Object[] { checksum, src.getName(), FileUtils.getRelativePath(dest.getParentFile(), src), FileUtils.getRelativePath(getProject().getBaseDir(), src), src.getAbsolutePath() }).getBytes());
                        writeline("/home/ubuntu/results/coverage/Checksum/Checksum_10_10.coverage", "d5e87a50-86eb-47ab-b14d-573c3877c3a4");
                        fos.write(StringUtils.LINE_SEP.getBytes());
                        writeline("/home/ubuntu/results/coverage/Checksum/Checksum_10_10.coverage", "acc99a99-49b2-4364-a92e-58358e8d01e6");
                        fos.close();
                        writeline("/home/ubuntu/results/coverage/Checksum/Checksum_10_10.coverage", "6775691a-2b7d-4ef3-84d9-86d4ce165f49");
                        fos = null;
                    }
                }
            }
            writeline("/home/ubuntu/results/coverage/Checksum/Checksum_10_10.coverage", "6153bb04-5136-4bb8-95c2-e98f3b8163b5");
            if (totalproperty != null) {
                writeline("/home/ubuntu/results/coverage/Checksum/Checksum_10_10.coverage", "94273151-e4a8-411d-81c5-c3c810692f16");
                // Calculate the total checksum
                // Convert the keys (source files) into a sorted array.
                File[] keyArray = allDigests.keySet().toArray(new File[allDigests.size()]);
                writeline("/home/ubuntu/results/coverage/Checksum/Checksum_10_10.coverage", "b0646042-eec3-4bc6-8f89-309d509caeb6");
                // File is Comparable, but sort-order is platform
                // dependent (case-insensitive on Windows)
                Arrays.sort(keyArray, new Comparator<File>() {

                    public int compare(File f1, File f2) {
                        return f1 == null ? (f2 == null ? 0 : -1) : (f2 == null ? 1 : getRelativeFilePath(f1).compareTo(getRelativeFilePath(f2)));
                    }
                });
                writeline("/home/ubuntu/results/coverage/Checksum/Checksum_10_10.coverage", "ddf94388-fbcc-4026-8b39-0e7142685cc1");
                // Loop over the checksums and generate a total hash.
                messageDigest.reset();
                writeline("/home/ubuntu/results/coverage/Checksum/Checksum_10_10.coverage", "3d6ba19e-fd9f-4fd3-9843-cebcff52525c");
                for (File src : keyArray) {
                    writeline("/home/ubuntu/results/coverage/Checksum/Checksum_10_10.coverage", "d96833bc-2a79-4fe8-a168-bd89444026fe");
                    // Add the digest for the file content
                    byte[] digest = allDigests.get(src);
                    writeline("/home/ubuntu/results/coverage/Checksum/Checksum_10_10.coverage", "ca11cd05-c1eb-45df-b8f4-d699dd5b97bd");
                    messageDigest.update(digest);
                    writeline("/home/ubuntu/results/coverage/Checksum/Checksum_10_10.coverage", "f62bd059-f1e5-4ca2-a1e9-d56c86c91ce2");
                    // Add the file path
                    String fileName = getRelativeFilePath(src);
                    writeline("/home/ubuntu/results/coverage/Checksum/Checksum_10_10.coverage", "553e6c12-8e93-4d6f-913a-54a1e20a1ebb");
                    messageDigest.update(fileName.getBytes());
                }
                writeline("/home/ubuntu/results/coverage/Checksum/Checksum_10_10.coverage", "6137f846-89f7-40ee-9f80-171bc6d506a6");
                String totalChecksum = createDigestString(messageDigest.digest());
                writeline("/home/ubuntu/results/coverage/Checksum/Checksum_10_10.coverage", "314d0fe7-49f5-48d1-a899-142d3cf1758b");
                getProject().setNewProperty(totalproperty, totalChecksum);
            }
        } catch (Exception e) {
            writeline("/home/ubuntu/results/coverage/Checksum/Checksum_10_10.coverage", "1b3d78a6-4809-4110-a7a7-493d3a76e969");
            throw new BuildException(e, getLocation());
        } finally {
            writeline("/home/ubuntu/results/coverage/Checksum/Checksum_10_10.coverage", "42af2e5c-c926-44c0-97b4-f9b6277d2245");
            FileUtils.close(fis);
            writeline("/home/ubuntu/results/coverage/Checksum/Checksum_10_10.coverage", "8c8f8a79-4b41-4e72-9dae-3e188349bb00");
            FileUtils.close(fos);
        }
        writeline("/home/ubuntu/results/coverage/Checksum/Checksum_10_10.coverage", "03898ccd-c83d-4636-8b44-16707d90f9c4");
        return checksumMatches;
    }

    private String createDigestString(byte[] fileDigest) {
        writeline("/home/ubuntu/results/coverage/Checksum/Checksum_10_10.coverage", "2357f402-91f8-4c59-8e0c-eace527a223c");
        StringBuffer checksumSb = new StringBuffer();
        writeline("/home/ubuntu/results/coverage/Checksum/Checksum_10_10.coverage", "715dc827-fc79-466f-9464-f5eb218592bb");
        for (int i = 0; i < fileDigest.length; i++) {
            writeline("/home/ubuntu/results/coverage/Checksum/Checksum_10_10.coverage", "234115f1-40f6-4108-ae54-5d05c9bbdf6e");
            String hexStr = Integer.toHexString(BYTE_MASK & fileDigest[i]);
            writeline("/home/ubuntu/results/coverage/Checksum/Checksum_10_10.coverage", "42c651c4-bdb2-434e-8d96-f53e6d5cd3e7");
            if (hexStr.length() < 2) {
                writeline("/home/ubuntu/results/coverage/Checksum/Checksum_10_10.coverage", "7f4f454a-3aca-4925-9668-86410d99e3a2");
                checksumSb.append("0");
            }
            writeline("/home/ubuntu/results/coverage/Checksum/Checksum_10_10.coverage", "0a011b65-b191-4241-b391-6f6c3cf62980");
            checksumSb.append(hexStr);
        }
        writeline("/home/ubuntu/results/coverage/Checksum/Checksum_10_10.coverage", "f2e5cd7e-5674-4aaf-b0e8-be57b4d82ecf");
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
        writelineStatic("/home/ubuntu/results/coverage/Checksum/Checksum_10_10.coverage", "1d4ea732-665a-4a47-8882-bf71999971ec");
        int l = data.length;
        writelineStatic("/home/ubuntu/results/coverage/Checksum/Checksum_10_10.coverage", "25959d73-4ac2-4a1d-b554-4a69413ea6cf");
        if ((l & 0x01) != 0) {
            writelineStatic("/home/ubuntu/results/coverage/Checksum/Checksum_10_10.coverage", "d7b4d17f-e0bd-46f7-b59e-1f0639f7b005");
            throw new BuildException("odd number of characters.");
        }
        writelineStatic("/home/ubuntu/results/coverage/Checksum/Checksum_10_10.coverage", "f37b387c-fa71-4550-a671-8cf00b28f1c8");
        byte[] out = new byte[l >> 1];
        writelineStatic("/home/ubuntu/results/coverage/Checksum/Checksum_10_10.coverage", "e5801bad-bca3-4892-a416-8dff08f42bc1");
        // two characters form the hex value.
        for (int i = 0, j = 0; j < l; i++) {
            writelineStatic("/home/ubuntu/results/coverage/Checksum/Checksum_10_10.coverage", "1ce51667-3c3f-4346-bf47-81c137679c8e");
            int f = Character.digit(data[j++], WORD) << NIBBLE;
            writelineStatic("/home/ubuntu/results/coverage/Checksum/Checksum_10_10.coverage", "ac99b2b8-9345-46aa-9535-ec5d0c1f1783");
            f = f | Character.digit(data[j++], WORD);
            writelineStatic("/home/ubuntu/results/coverage/Checksum/Checksum_10_10.coverage", "d0bd5df0-4cc3-4a59-9255-a70ce5e36c89");
            out[i] = (byte) (f & BYTE_MASK);
        }
        writelineStatic("/home/ubuntu/results/coverage/Checksum/Checksum_10_10.coverage", "1a80e55c-ff79-4a7b-8525-425ef15b4285");
        return out;
    }

    /**
     * reads the checksum from a file using the specified format.
     *
     * @since 1.7
     */
    private String readChecksum(File f) {
        writeline("/home/ubuntu/results/coverage/Checksum/Checksum_10_10.coverage", "11c12f2a-8d32-4e4e-903e-1a415c6bc8be");
        BufferedReader diskChecksumReader = null;
        writeline("/home/ubuntu/results/coverage/Checksum/Checksum_10_10.coverage", "2ad31cfc-5b40-416c-bb86-224175e0d1a4");
        try {
            writeline("/home/ubuntu/results/coverage/Checksum/Checksum_10_10.coverage", "48f9a3b6-6d2a-4f94-b516-38e558f3d8c7");
            diskChecksumReader = new BufferedReader(new FileReader(f));
            writeline("/home/ubuntu/results/coverage/Checksum/Checksum_10_10.coverage", "f0fac31e-5ca1-4398-bdde-3a8810a1702e");
            Object[] result = format.parse(diskChecksumReader.readLine());
            writeline("/home/ubuntu/results/coverage/Checksum/Checksum_10_10.coverage", "b0049de2-8ce6-4b20-89ed-a647d55914f0");
            if (result == null || result.length == 0 || result[0] == null) {
                writeline("/home/ubuntu/results/coverage/Checksum/Checksum_10_10.coverage", "abf10306-b6d6-4391-b86d-175261bdae76");
                throw new BuildException("failed to find a checksum");
            }
            writeline("/home/ubuntu/results/coverage/Checksum/Checksum_10_10.coverage", "d2747961-34e6-42d4-b33e-f918917191e0");
            return (String) result[0];
        } catch (IOException e) {
            writeline("/home/ubuntu/results/coverage/Checksum/Checksum_10_10.coverage", "808700b3-99ab-49f8-999a-c6bede1fd5e2");
            throw new BuildException("Couldn't read checksum file " + f, e);
        } catch (ParseException e) {
            writeline("/home/ubuntu/results/coverage/Checksum/Checksum_10_10.coverage", "3ce7c23e-e251-42b2-af25-1c7976794e7d");
            throw new BuildException("Couldn't read checksum file " + f, e);
        } finally {
            writeline("/home/ubuntu/results/coverage/Checksum/Checksum_10_10.coverage", "5c9c3fd3-de1c-4e98-a7cd-773bf290ef7d");
            FileUtils.close(diskChecksumReader);
        }
    }

    /**
     * @since Ant 1.8.2
     */
    private String getRelativeFilePath(File f) {
        writeline("/home/ubuntu/results/coverage/Checksum/Checksum_10_10.coverage", "4851acd0-042d-4bdb-a824-773df6a4d369");
        String path = (String) relativeFilePaths.get(f);
        writeline("/home/ubuntu/results/coverage/Checksum/Checksum_10_10.coverage", "df8f3c19-8134-45f7-972c-e977e4167445");
        if (path == null) {
            writeline("/home/ubuntu/results/coverage/Checksum/Checksum_10_10.coverage", "b1f47efe-e83a-4743-9364-9b58c957e14d");
            // bug 37386. this should not occur, but it has, once.
            throw new BuildException("Internal error: " + "relativeFilePaths could not match file " + f + "\n" + "please file a bug report on this");
        }
        writeline("/home/ubuntu/results/coverage/Checksum/Checksum_10_10.coverage", "49826d55-a080-42ec-9ec4-7efb8735c882");
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
