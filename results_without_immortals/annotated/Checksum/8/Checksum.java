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
        writeline("/home/ubuntu/results/coverage/Checksum/Checksum_8_10.coverage", "f99df735-ed56-4bc0-ad6c-36d26ac3725d");
        this.file = file;
    }

    /**
     * Sets the root directory where checksum files will be
     * written/read
     * @param todir the directory to write to
     * @since Ant 1.6
     */
    public void setTodir(File todir) {
        writeline("/home/ubuntu/results/coverage/Checksum/Checksum_8_10.coverage", "6286862a-44b3-46b5-a347-a54560f162b5");
        this.todir = todir;
    }

    /**
     * Specifies the algorithm to be used to compute the checksum.
     * Defaults to "MD5". Other popular algorithms like "SHA" may be used as well.
     * @param algorithm a <code>String</code> value
     */
    public void setAlgorithm(String algorithm) {
        writeline("/home/ubuntu/results/coverage/Checksum/Checksum_8_10.coverage", "891eb0bd-5d84-4710-9bda-e05d841a05d3");
        this.algorithm = algorithm;
    }

    /**
     * Sets the MessageDigest algorithm provider to be used
     * to calculate the checksum.
     * @param provider a <code>String</code> value
     */
    public void setProvider(String provider) {
        writeline("/home/ubuntu/results/coverage/Checksum/Checksum_8_10.coverage", "f44b81a5-cd4f-4402-a3d7-871b69c8ca18");
        this.provider = provider;
    }

    /**
     * Sets the file extension that is be to used to
     * create or identify destination file.
     * @param fileext a <code>String</code> value
     */
    public void setFileext(String fileext) {
        writeline("/home/ubuntu/results/coverage/Checksum/Checksum_8_10.coverage", "614ff8f8-392d-4ad5-b9ed-f6ca72141588");
        this.fileext = fileext;
    }

    /**
     * Sets the property to hold the generated checksum.
     * @param property a <code>String</code> value
     */
    public void setProperty(String property) {
        writeline("/home/ubuntu/results/coverage/Checksum/Checksum_8_10.coverage", "4639c255-9a13-4c5a-89a5-0cd9c45719ed");
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
        writeline("/home/ubuntu/results/coverage/Checksum/Checksum_8_10.coverage", "00e5c7e6-84b7-4f4e-af2d-e520e50ec20d");
        this.totalproperty = totalproperty;
    }

    /**
     * Sets the verify property.  This project property holds
     * the result of a checksum verification - "true" or "false"
     * @param verifyProperty a <code>String</code> value
     */
    public void setVerifyproperty(String verifyProperty) {
        writeline("/home/ubuntu/results/coverage/Checksum/Checksum_8_10.coverage", "d54d7a91-1349-40c5-99b4-6566b4db922c");
        this.verifyProperty = verifyProperty;
    }

    /**
     * Whether or not to overwrite existing file irrespective of
     * whether it is newer than
     * the source file.  Defaults to false.
     * @param forceOverwrite a <code>boolean</code> value
     */
    public void setForceOverwrite(boolean forceOverwrite) {
        writeline("/home/ubuntu/results/coverage/Checksum/Checksum_8_10.coverage", "7a3ec25f-8b8d-42ce-b69c-38a61730398c");
        this.forceOverwrite = forceOverwrite;
    }

    /**
     * The size of the read buffer to use.
     * @param size an <code>int</code> value
     */
    public void setReadBufferSize(int size) {
        writeline("/home/ubuntu/results/coverage/Checksum/Checksum_8_10.coverage", "11a24cb8-6e03-4164-84cf-5d08ebd5a5b1");
        this.readBufferSize = size;
    }

    /**
     * Select the in/output pattern via a well know format name.
     * @param e an <code>enumerated</code> value
     *
     * @since 1.7.0
     */
    public void setFormat(FormatElement e) {
        writeline("/home/ubuntu/results/coverage/Checksum/Checksum_8_10.coverage", "67cf6dac-9d9a-4961-81a5-80435482fd25");
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
        writeline("/home/ubuntu/results/coverage/Checksum/Checksum_8_10.coverage", "21102d82-8e9a-4b9b-b9ff-ffe444d4378c");
        format = new MessageFormat(p);
    }

    /**
     * Files to generate checksums for.
     * @param set a fileset of files to generate checksums for.
     */
    public void addFileset(FileSet set) {
        writeline("/home/ubuntu/results/coverage/Checksum/Checksum_8_10.coverage", "c0869b8a-a8e3-49c3-8752-6f5e395b0f25");
        add(set);
    }

    /**
     * Add a resource collection.
     * @param rc the ResourceCollection to add.
     */
    public void add(ResourceCollection rc) {
        writeline("/home/ubuntu/results/coverage/Checksum/Checksum_8_10.coverage", "33dc502d-197b-4118-ac93-14c93af97af8");
        if (rc == null) {
            writeline("/home/ubuntu/results/coverage/Checksum/Checksum_8_10.coverage", "650c7b7c-1d3d-4481-9508-53af923a3162");
            return;
        }
        writeline("/home/ubuntu/results/coverage/Checksum/Checksum_8_10.coverage", "4d9d408e-8066-4530-bac7-e66fad33fcc9");
        resources = (resources == null) ? new FileUnion() : resources;
        writeline("/home/ubuntu/results/coverage/Checksum/Checksum_8_10.coverage", "4eba119f-324e-405f-b4bb-51bee4da4433");
        resources.add(rc);
    }

    /**
     * Calculate the checksum(s).
     * @throws BuildException on error
     */
    public void execute() throws BuildException {
        writeline("/home/ubuntu/results/coverage/Checksum/Checksum_8_10.coverage", "fe0b1d26-bfeb-413a-9419-6e96488c1ffc");
        isCondition = false;
        writeline("/home/ubuntu/results/coverage/Checksum/Checksum_8_10.coverage", "b2ccd2ea-c1ec-496d-b509-92423b0e7720");
        boolean value = validateAndExecute();
        writeline("/home/ubuntu/results/coverage/Checksum/Checksum_8_10.coverage", "21fcbe2e-e22d-419c-b886-a430e96a662e");
        if (verifyProperty != null) {
            writeline("/home/ubuntu/results/coverage/Checksum/Checksum_8_10.coverage", "5ce5f464-ccf3-4070-9b90-9db531967e9f");
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
        writeline("/home/ubuntu/results/coverage/Checksum/Checksum_8_10.coverage", "7ce8027f-7119-42f6-842a-9d5566aabc88");
        isCondition = true;
        writeline("/home/ubuntu/results/coverage/Checksum/Checksum_8_10.coverage", "271dab2c-a326-4c6f-9d6b-0c1f1f1429ab");
        return validateAndExecute();
    }

    /**
     * Validate attributes and get down to business.
     */
    private boolean validateAndExecute() throws BuildException {
        writeline("/home/ubuntu/results/coverage/Checksum/Checksum_8_10.coverage", "a232b080-ebb3-4c87-bda4-bb334be957c4");
        String savedFileExt = fileext;
        writeline("/home/ubuntu/results/coverage/Checksum/Checksum_8_10.coverage", "e6a4c3f6-f244-48ae-a6cd-a1bb789e0f1c");
        if (file == null && (resources == null || resources.size() == 0)) {
            writeline("/home/ubuntu/results/coverage/Checksum/Checksum_8_10.coverage", "b44efe8f-1973-4333-8e68-42af96591d5f");
            throw new BuildException("Specify at least one source - a file or a resource collection.");
        }
        writeline("/home/ubuntu/results/coverage/Checksum/Checksum_8_10.coverage", "0af41b3b-00c8-40d7-9664-639c14bf7536");
        if (!(resources == null || resources.isFilesystemOnly())) {
            writeline("/home/ubuntu/results/coverage/Checksum/Checksum_8_10.coverage", "8c4db6b5-862d-43b7-ad64-d98fdcc172a2");
            throw new BuildException("Can only calculate checksums for file-based resources.");
        }
        writeline("/home/ubuntu/results/coverage/Checksum/Checksum_8_10.coverage", "2215c231-7836-4366-90ef-94baf3ddf740");
        if (file != null && file.exists() && file.isDirectory()) {
            writeline("/home/ubuntu/results/coverage/Checksum/Checksum_8_10.coverage", "4e96a18b-34de-422e-b7d5-31bcc404f8b3");
            throw new BuildException("Checksum cannot be generated for directories");
        }
        writeline("/home/ubuntu/results/coverage/Checksum/Checksum_8_10.coverage", "d0ceecab-47fd-49fc-8b20-151d6bf9c075");
        if (file != null && totalproperty != null) {
            writeline("/home/ubuntu/results/coverage/Checksum/Checksum_8_10.coverage", "7a7dffd3-811a-4d72-942f-d0383678a55b");
            throw new BuildException("File and Totalproperty cannot co-exist.");
        }
        writeline("/home/ubuntu/results/coverage/Checksum/Checksum_8_10.coverage", "3aae83a4-5dcf-43a4-b25c-79de0f4ace4a");
        if (property != null && fileext != null) {
            writeline("/home/ubuntu/results/coverage/Checksum/Checksum_8_10.coverage", "2658391e-d9ef-4a27-aacf-f90b438fad69");
            throw new BuildException("Property and FileExt cannot co-exist.");
        }
        writeline("/home/ubuntu/results/coverage/Checksum/Checksum_8_10.coverage", "19dcaca8-4c9e-4a40-ab4b-5eeb1b0770ca");
        if (property != null) {
            writeline("/home/ubuntu/results/coverage/Checksum/Checksum_8_10.coverage", "003f2ebc-8c67-403e-8ddf-4af7c77797f7");
            if (forceOverwrite) {
                writeline("/home/ubuntu/results/coverage/Checksum/Checksum_8_10.coverage", "6d33e098-1b18-4492-924e-d4111951ed4d");
                throw new BuildException("ForceOverwrite cannot be used when Property is specified");
            }
            writeline("/home/ubuntu/results/coverage/Checksum/Checksum_8_10.coverage", "d110e344-4cdb-4c6f-a4d0-2277ec3ee21f");
            int ct = 0;
            writeline("/home/ubuntu/results/coverage/Checksum/Checksum_8_10.coverage", "6b6ea141-6b7d-4161-afec-6eb4c9cbc60f");
            if (resources != null) {
                writeline("/home/ubuntu/results/coverage/Checksum/Checksum_8_10.coverage", "b5a076d1-0959-49b3-bf2e-4208255045b8");
                ct += resources.size();
            }
            writeline("/home/ubuntu/results/coverage/Checksum/Checksum_8_10.coverage", "42f2a63f-363c-477b-b76c-98886d64f371");
            if (file != null) {
                writeline("/home/ubuntu/results/coverage/Checksum/Checksum_8_10.coverage", "77d8033c-8f90-48cb-9868-51c8c4d4222e");
                ct++;
            }
            writeline("/home/ubuntu/results/coverage/Checksum/Checksum_8_10.coverage", "335c9c4b-cec7-4702-9485-6d59a461946c");
            if (ct > 1) {
                writeline("/home/ubuntu/results/coverage/Checksum/Checksum_8_10.coverage", "ce4bbdb9-9687-4584-860b-b7eccb3e8a26");
                throw new BuildException("Multiple files cannot be used when Property is specified");
            }
        }
        writeline("/home/ubuntu/results/coverage/Checksum/Checksum_8_10.coverage", "ae132323-53d3-435c-adb4-f302ce35f819");
        if (verifyProperty != null) {
            writeline("/home/ubuntu/results/coverage/Checksum/Checksum_8_10.coverage", "d271b4fb-a7a1-4c53-8982-c58f88ebfab3");
            isCondition = true;
        }
        writeline("/home/ubuntu/results/coverage/Checksum/Checksum_8_10.coverage", "a14afb5e-6964-4dd4-b22d-8f62226606cd");
        if (verifyProperty != null && forceOverwrite) {
            writeline("/home/ubuntu/results/coverage/Checksum/Checksum_8_10.coverage", "917ef10a-61f7-4a23-ba48-dba898b6e5f3");
            throw new BuildException("VerifyProperty and ForceOverwrite cannot co-exist.");
        }
        writeline("/home/ubuntu/results/coverage/Checksum/Checksum_8_10.coverage", "f4a1b023-daf9-42dd-93f0-5ffbde2013ad");
        if (isCondition && forceOverwrite) {
            writeline("/home/ubuntu/results/coverage/Checksum/Checksum_8_10.coverage", "e2b8047f-6217-4060-a298-14bf269e4fb1");
            throw new BuildException("ForceOverwrite cannot be used when conditions are being used.");
        }
        writeline("/home/ubuntu/results/coverage/Checksum/Checksum_8_10.coverage", "07f1deb2-c04e-4ccb-b1d1-774875b78c1a");
        messageDigest = null;
        writeline("/home/ubuntu/results/coverage/Checksum/Checksum_8_10.coverage", "5de06f24-3e16-411e-8152-844233b9c1ad");
        if (provider != null) {
            writeline("/home/ubuntu/results/coverage/Checksum/Checksum_8_10.coverage", "907299bd-e27e-427d-90ce-6b2a33351cf4");
            try {
                writeline("/home/ubuntu/results/coverage/Checksum/Checksum_8_10.coverage", "32355c6a-da39-40f4-b040-e073a0b1316d");
                messageDigest = MessageDigest.getInstance(algorithm, provider);
            } catch (NoSuchAlgorithmException noalgo) {
                writeline("/home/ubuntu/results/coverage/Checksum/Checksum_8_10.coverage", "718caefb-34b0-4e1a-970b-24c313202a4d");
                throw new BuildException(noalgo, getLocation());
            } catch (NoSuchProviderException noprovider) {
                writeline("/home/ubuntu/results/coverage/Checksum/Checksum_8_10.coverage", "e645abba-67df-49b5-9451-6aedb41cc2c8");
                throw new BuildException(noprovider, getLocation());
            }
        } else {
            writeline("/home/ubuntu/results/coverage/Checksum/Checksum_8_10.coverage", "98cdc345-a40b-455c-acc1-b47d01e783f1");
            try {
                writeline("/home/ubuntu/results/coverage/Checksum/Checksum_8_10.coverage", "16683468-0150-40af-8836-35556905c9fe");
                messageDigest = MessageDigest.getInstance(algorithm);
            } catch (NoSuchAlgorithmException noalgo) {
                writeline("/home/ubuntu/results/coverage/Checksum/Checksum_8_10.coverage", "91e1118b-f5a8-4fc6-890a-8f91e122f600");
                throw new BuildException(noalgo, getLocation());
            }
        }
        writeline("/home/ubuntu/results/coverage/Checksum/Checksum_8_10.coverage", "2010a75e-be56-48a8-8617-67cf6d464374");
        if (messageDigest == null) {
            writeline("/home/ubuntu/results/coverage/Checksum/Checksum_8_10.coverage", "d6c0ecd1-f555-46f4-a2ba-6354e4b31568");
            throw new BuildException("Unable to create Message Digest", getLocation());
        }
        writeline("/home/ubuntu/results/coverage/Checksum/Checksum_8_10.coverage", "7250668e-39b8-4203-b2ee-a7d0ee5b9062");
        if (fileext == null) {
            writeline("/home/ubuntu/results/coverage/Checksum/Checksum_8_10.coverage", "b4d32a06-be8a-481f-a28d-792bbafb807c");
            fileext = "." + algorithm;
        } else if (fileext.trim().length() == 0) {
            writeline("/home/ubuntu/results/coverage/Checksum/Checksum_8_10.coverage", "82822104-f283-4244-914c-5f1be86fc601");
            throw new BuildException("File extension when specified must not be an empty string");
        }
        writeline("/home/ubuntu/results/coverage/Checksum/Checksum_8_10.coverage", "e4324245-f938-4ad7-b043-72695f48bd77");
        try {
            writeline("/home/ubuntu/results/coverage/Checksum/Checksum_8_10.coverage", "5ad8e1f1-a1a4-4fbf-9111-f94b7427ed25");
            if (resources != null) {
                writeline("/home/ubuntu/results/coverage/Checksum/Checksum_8_10.coverage", "01c3de2f-fa42-4df5-afd6-36fd8a721f29");
                for (Resource r : resources) {
                    writeline("/home/ubuntu/results/coverage/Checksum/Checksum_8_10.coverage", "37cf0043-fa0b-4254-9a57-2ee2f428d882");
                    File src = r.as(FileProvider.class).getFile();
                    writeline("/home/ubuntu/results/coverage/Checksum/Checksum_8_10.coverage", "b33f9e82-a21f-4520-93cf-06b2270c5c9a");
                    if (totalproperty != null || todir != null) {
                        writeline("/home/ubuntu/results/coverage/Checksum/Checksum_8_10.coverage", "1380ecf6-09c1-4669-970f-e9c65669865f");
                        // Use '/' to calculate digest based on file name.
                        // This is required in order to get the same result
                        // on different platforms.
                        relativeFilePaths.put(src, r.getName().replace(File.separatorChar, '/'));
                    }
                    writeline("/home/ubuntu/results/coverage/Checksum/Checksum_8_10.coverage", "a3e1f492-9db1-4193-8064-8b3ae6b79e94");
                    addToIncludeFileMap(src);
                }
            }
            writeline("/home/ubuntu/results/coverage/Checksum/Checksum_8_10.coverage", "2457355c-7c64-4e83-a720-f8d56d7faa2b");
            if (file != null) {
                writeline("/home/ubuntu/results/coverage/Checksum/Checksum_8_10.coverage", "f3461ee4-408f-4cd0-b7ea-bdc2d17bb301");
                if (totalproperty != null || todir != null) {
                    writeline("/home/ubuntu/results/coverage/Checksum/Checksum_8_10.coverage", "632af619-3c73-4799-a7a1-ee67444f0add");
                    relativeFilePaths.put(file, file.getName().replace(File.separatorChar, '/'));
                }
                writeline("/home/ubuntu/results/coverage/Checksum/Checksum_8_10.coverage", "a66f31a9-c2cf-4b30-bae7-7a30264cae90");
                addToIncludeFileMap(file);
            }
            writeline("/home/ubuntu/results/coverage/Checksum/Checksum_8_10.coverage", "d81d07f3-6899-4930-b5f7-d4782a98ebbf");
            return generateChecksums();
        } finally {
            writeline("/home/ubuntu/results/coverage/Checksum/Checksum_8_10.coverage", "099eacf6-74c8-4ecf-8d89-14ea6fd2ff3d");
            fileext = savedFileExt;
            writeline("/home/ubuntu/results/coverage/Checksum/Checksum_8_10.coverage", "732e6264-1cf4-45b5-8b49-73ee8ac9178b");
            includeFileMap.clear();
        }
    }

    /**
     * Add key-value pair to the hashtable upon which
     * to later operate upon.
     */
    private void addToIncludeFileMap(File file) throws BuildException {
        writeline("/home/ubuntu/results/coverage/Checksum/Checksum_8_10.coverage", "7c5bc3bb-39fa-499b-982f-2ab45a40bd8a");
        if (file.exists()) {
            writeline("/home/ubuntu/results/coverage/Checksum/Checksum_8_10.coverage", "a0cb2926-d49b-46f1-8c51-dfef9390e4bd");
            if (property == null) {
                writeline("/home/ubuntu/results/coverage/Checksum/Checksum_8_10.coverage", "6b484343-dfd0-4c70-90e5-b0687a4628fb");
                File checksumFile = getChecksumFile(file);
                writeline("/home/ubuntu/results/coverage/Checksum/Checksum_8_10.coverage", "210fd5e5-6ce3-4edb-ab5e-bb9c9f5e6b76");
                if (forceOverwrite || isCondition || (file.lastModified() > checksumFile.lastModified())) {
                    writeline("/home/ubuntu/results/coverage/Checksum/Checksum_8_10.coverage", "f4cc5bd3-20eb-445f-a21b-49fe9afe5377");
                    includeFileMap.put(file, checksumFile);
                } else {
                    writeline("/home/ubuntu/results/coverage/Checksum/Checksum_8_10.coverage", "082120c9-e9c2-4314-a1fc-cef136079a2c");
                    log(file + " omitted as " + checksumFile + " is up to date.", Project.MSG_VERBOSE);
                    writeline("/home/ubuntu/results/coverage/Checksum/Checksum_8_10.coverage", "f4447028-1556-4705-81ac-9bc07a8b86e3");
                    if (totalproperty != null) {
                        writeline("/home/ubuntu/results/coverage/Checksum/Checksum_8_10.coverage", "ad12a5f0-db07-4fc6-b37f-4a882f6a8352");
                        // Read the checksum from disk.
                        String checksum = readChecksum(checksumFile);
                        writeline("/home/ubuntu/results/coverage/Checksum/Checksum_8_10.coverage", "43d9afd6-b5d1-4267-84f3-46300df94a60");
                        byte[] digest = decodeHex(checksum.toCharArray());
                        writeline("/home/ubuntu/results/coverage/Checksum/Checksum_8_10.coverage", "d37ab833-7131-4b92-9313-e41744bf047b");
                        allDigests.put(file, digest);
                    }
                }
            } else {
                writeline("/home/ubuntu/results/coverage/Checksum/Checksum_8_10.coverage", "71878dd5-92c8-4c96-b295-495c32124ff4");
                includeFileMap.put(file, property);
            }
        } else {
            writeline("/home/ubuntu/results/coverage/Checksum/Checksum_8_10.coverage", "9c6dd75c-de76-4008-bfd7-0e253bc35da2");
            String message = "Could not find file " + file.getAbsolutePath() + " to generate checksum for.";
            writeline("/home/ubuntu/results/coverage/Checksum/Checksum_8_10.coverage", "bd7d7b3f-ecd0-4e3d-a39e-2e7ea25d9bd7");
            log(message);
            writeline("/home/ubuntu/results/coverage/Checksum/Checksum_8_10.coverage", "156c126e-c5ac-4b36-88f1-e2167415f4c2");
            throw new BuildException(message, getLocation());
        }
    }

    private File getChecksumFile(File file) {
        writeline("/home/ubuntu/results/coverage/Checksum/Checksum_8_10.coverage", "af86b1e5-8a8b-4bed-9ff1-68c11219f1f5");
        File directory;
        writeline("/home/ubuntu/results/coverage/Checksum/Checksum_8_10.coverage", "b6b7b4c8-ec3e-4183-b795-dc8e0bdce901");
        if (todir != null) {
            writeline("/home/ubuntu/results/coverage/Checksum/Checksum_8_10.coverage", "b76985b2-a69b-4b3c-9593-1591c504534b");
            // A separate directory was explicitly declared
            String path = getRelativeFilePath(file);
            writeline("/home/ubuntu/results/coverage/Checksum/Checksum_8_10.coverage", "f3e4055c-bb48-47bd-8054-8a7d876a5908");
            directory = new File(todir, path).getParentFile();
            writeline("/home/ubuntu/results/coverage/Checksum/Checksum_8_10.coverage", "a9ecfdd9-4ce3-4a6a-9851-163c2c99eaec");
            // Create the directory, as it might not exist.
            directory.mkdirs();
        } else {
            writeline("/home/ubuntu/results/coverage/Checksum/Checksum_8_10.coverage", "c67b2480-d5c6-4146-b9e0-49ccac14137b");
            // Just use the same directory as the file itself.
            // This directory will exist
            directory = file.getParentFile();
        }
        writeline("/home/ubuntu/results/coverage/Checksum/Checksum_8_10.coverage", "376a5faf-753e-4db0-8653-2623a1df553a");
        File checksumFile = new File(directory, file.getName() + fileext);
        writeline("/home/ubuntu/results/coverage/Checksum/Checksum_8_10.coverage", "87e2a0a5-ada2-45cc-8305-23002aa7d737");
        return checksumFile;
    }

    /**
     * Generate checksum(s) using the message digest created earlier.
     */
    private boolean generateChecksums() throws BuildException {
        writeline("/home/ubuntu/results/coverage/Checksum/Checksum_8_10.coverage", "d5207a7e-22e5-405d-8239-2f5fa1480edd");
        boolean checksumMatches = true;
        writeline("/home/ubuntu/results/coverage/Checksum/Checksum_8_10.coverage", "f1c7a074-ba4f-4489-b3f1-fe31e1a29f85");
        FileInputStream fis = null;
        writeline("/home/ubuntu/results/coverage/Checksum/Checksum_8_10.coverage", "326daf71-0764-4b72-937d-4f731abf8f31");
        FileOutputStream fos = null;
        writeline("/home/ubuntu/results/coverage/Checksum/Checksum_8_10.coverage", "84dc9811-cdc1-42f9-823e-04c578a6c33c");
        byte[] buf = new byte[readBufferSize];
        writeline("/home/ubuntu/results/coverage/Checksum/Checksum_8_10.coverage", "c8867218-1f3d-4c5c-8848-a16abf4475b4");
        try {
            writeline("/home/ubuntu/results/coverage/Checksum/Checksum_8_10.coverage", "3c8cf627-7968-40cd-8e9c-8b344a5f67fc");
            for (Map.Entry<File, Object> e : includeFileMap.entrySet()) {
                writeline("/home/ubuntu/results/coverage/Checksum/Checksum_8_10.coverage", "085d79bc-4192-48a0-bb72-fb68e38c63ad");
                messageDigest.reset();
                writeline("/home/ubuntu/results/coverage/Checksum/Checksum_8_10.coverage", "c7dd3b46-00a9-4d7f-b732-89e006d83d53");
                File src = e.getKey();
                writeline("/home/ubuntu/results/coverage/Checksum/Checksum_8_10.coverage", "e4af2f1c-c1c7-4ccc-afac-0a2351174007");
                if (!isCondition) {
                    writeline("/home/ubuntu/results/coverage/Checksum/Checksum_8_10.coverage", "6d81d37e-0f3c-487c-b27e-7067fb75cfa9");
                    log("Calculating " + algorithm + " checksum for " + src, Project.MSG_VERBOSE);
                }
                writeline("/home/ubuntu/results/coverage/Checksum/Checksum_8_10.coverage", "62646833-f16e-4b98-a059-783bcb574003");
                fis = new FileInputStream(src);
                writeline("/home/ubuntu/results/coverage/Checksum/Checksum_8_10.coverage", "32209362-6d2b-4dc3-8cd2-9f9e8bef24fd");
                DigestInputStream dis = new DigestInputStream(fis, messageDigest);
                writeline("/home/ubuntu/results/coverage/Checksum/Checksum_8_10.coverage", "d707e5e4-fb2c-4ec8-8d4d-74df7549993f");
                while (dis.read(buf, 0, readBufferSize) != -1) {
                // Empty statement
                }
                writeline("/home/ubuntu/results/coverage/Checksum/Checksum_8_10.coverage", "e910889b-45df-4c3b-ac97-f8d6f8f65152");
                dis.close();
                writeline("/home/ubuntu/results/coverage/Checksum/Checksum_8_10.coverage", "b3d686aa-bd29-4adf-9c27-068c8055a6b3");
                fis.close();
                writeline("/home/ubuntu/results/coverage/Checksum/Checksum_8_10.coverage", "33784c35-92de-4618-a75b-383dcc274553");
                fis = null;
                writeline("/home/ubuntu/results/coverage/Checksum/Checksum_8_10.coverage", "9add78b6-f9a4-4081-9db6-622ce5a17b63");
                byte[] fileDigest = messageDigest.digest();
                writeline("/home/ubuntu/results/coverage/Checksum/Checksum_8_10.coverage", "fecc4751-7b06-476d-8c38-e0193301f728");
                if (totalproperty != null) {
                    writeline("/home/ubuntu/results/coverage/Checksum/Checksum_8_10.coverage", "ea0d405c-e06d-4fd9-a3fa-bd2c26370a13");
                    allDigests.put(src, fileDigest);
                }
                writeline("/home/ubuntu/results/coverage/Checksum/Checksum_8_10.coverage", "c28b6460-5b65-49d1-8717-821578ffb8a7");
                String checksum = createDigestString(fileDigest);
                writeline("/home/ubuntu/results/coverage/Checksum/Checksum_8_10.coverage", "e8796f3c-badf-4f55-8de2-7beef5d6f55f");
                // can either be a property name string or a file
                Object destination = e.getValue();
                writeline("/home/ubuntu/results/coverage/Checksum/Checksum_8_10.coverage", "7ee55f71-0de7-44d9-b5f4-7bd962ddd715");
                if (destination instanceof java.lang.String) {
                    writeline("/home/ubuntu/results/coverage/Checksum/Checksum_8_10.coverage", "89e9f0ef-f8f9-48c1-8c10-302df107d99a");
                    String prop = (String) destination;
                    writeline("/home/ubuntu/results/coverage/Checksum/Checksum_8_10.coverage", "eb23d132-04e0-48c1-9501-0d4bf9cd525a");
                    if (isCondition) {
                        writeline("/home/ubuntu/results/coverage/Checksum/Checksum_8_10.coverage", "fa824cbd-1b22-43c5-abff-3beb59b1eb61");
                        checksumMatches = checksumMatches && checksum.equals(property);
                    } else {
                        writeline("/home/ubuntu/results/coverage/Checksum/Checksum_8_10.coverage", "2638f3f1-c2e8-401c-a658-e2134b2ae932");
                        getProject().setNewProperty(prop, checksum);
                    }
                } else if (destination instanceof java.io.File) {
                    writeline("/home/ubuntu/results/coverage/Checksum/Checksum_8_10.coverage", "c8a667d5-208a-4086-932d-a6c37e754d6f");
                    if (isCondition) {
                        writeline("/home/ubuntu/results/coverage/Checksum/Checksum_8_10.coverage", "659d4669-85ab-4943-ae14-438fe9f0de83");
                        File existingFile = (File) destination;
                        writeline("/home/ubuntu/results/coverage/Checksum/Checksum_8_10.coverage", "e68028be-8199-40e9-bf9b-2bf389c6fbd8");
                        if (existingFile.exists()) {
                            writeline("/home/ubuntu/results/coverage/Checksum/Checksum_8_10.coverage", "abad9f5c-c701-4036-ab92-b6463fedf194");
                            try {
                                writeline("/home/ubuntu/results/coverage/Checksum/Checksum_8_10.coverage", "96bbbaed-4b6a-4db0-b081-50dce19bc9a1");
                                String suppliedChecksum = readChecksum(existingFile);
                                writeline("/home/ubuntu/results/coverage/Checksum/Checksum_8_10.coverage", "055ee3ea-4b9d-479b-9c11-342704e4e73f");
                                checksumMatches = checksumMatches && checksum.equals(suppliedChecksum);
                            } catch (BuildException be) {
                                writeline("/home/ubuntu/results/coverage/Checksum/Checksum_8_10.coverage", "8a49c463-c07a-4094-ae3b-69c438afa8d8");
                                // file is on wrong format, swallow
                                checksumMatches = false;
                            }
                        } else {
                            writeline("/home/ubuntu/results/coverage/Checksum/Checksum_8_10.coverage", "f2f3972c-3a43-4ee7-b34e-dc7b8b4c7c64");
                            checksumMatches = false;
                        }
                    } else {
                        writeline("/home/ubuntu/results/coverage/Checksum/Checksum_8_10.coverage", "b5a4a95c-d5db-4c50-83ee-07d9f309ff72");
                        File dest = (File) destination;
                        writeline("/home/ubuntu/results/coverage/Checksum/Checksum_8_10.coverage", "32a336f9-0057-4214-930b-5767cefe86aa");
                        fos = new FileOutputStream(dest);
                        writeline("/home/ubuntu/results/coverage/Checksum/Checksum_8_10.coverage", "20f64bca-49dc-440d-ba85-f915b5980066");
                        fos.write(format.format(new Object[] { checksum, src.getName(), FileUtils.getRelativePath(dest.getParentFile(), src), FileUtils.getRelativePath(getProject().getBaseDir(), src), src.getAbsolutePath() }).getBytes());
                        writeline("/home/ubuntu/results/coverage/Checksum/Checksum_8_10.coverage", "726336a0-4500-4bb6-b05f-8c2ddedbc52b");
                        fos.write(StringUtils.LINE_SEP.getBytes());
                        writeline("/home/ubuntu/results/coverage/Checksum/Checksum_8_10.coverage", "3ac12017-f14e-4518-9db7-28465871c1b0");
                        fos.close();
                        writeline("/home/ubuntu/results/coverage/Checksum/Checksum_8_10.coverage", "b167367d-6ca0-43e3-b250-26cc40e87f9e");
                        fos = null;
                    }
                }
            }
            writeline("/home/ubuntu/results/coverage/Checksum/Checksum_8_10.coverage", "a749d32e-8964-4e58-a168-aaa657d7c9d2");
            if (totalproperty != null) {
                writeline("/home/ubuntu/results/coverage/Checksum/Checksum_8_10.coverage", "3264cd24-daef-452d-b204-093e4f982fbe");
                // Calculate the total checksum
                // Convert the keys (source files) into a sorted array.
                File[] keyArray = allDigests.keySet().toArray(new File[allDigests.size()]);
                writeline("/home/ubuntu/results/coverage/Checksum/Checksum_8_10.coverage", "a5d2f3c3-e4c7-447a-a755-210e98fccefe");
                // File is Comparable, but sort-order is platform
                // dependent (case-insensitive on Windows)
                Arrays.sort(keyArray, new Comparator<File>() {

                    public int compare(File f1, File f2) {
                        return f1 == null ? (f2 == null ? 0 : -1) : (f2 == null ? 1 : getRelativeFilePath(f1).compareTo(getRelativeFilePath(f2)));
                    }
                });
                writeline("/home/ubuntu/results/coverage/Checksum/Checksum_8_10.coverage", "f12ee77e-375a-4669-b8d7-a99cc2e19ec6");
                // Loop over the checksums and generate a total hash.
                messageDigest.reset();
                writeline("/home/ubuntu/results/coverage/Checksum/Checksum_8_10.coverage", "11a063e4-c785-46cc-890e-a8591fa9b566");
                for (File src : keyArray) {
                    writeline("/home/ubuntu/results/coverage/Checksum/Checksum_8_10.coverage", "6ee2f07b-14a2-49d6-8618-9fab9883b250");
                    // Add the digest for the file content
                    byte[] digest = allDigests.get(src);
                    writeline("/home/ubuntu/results/coverage/Checksum/Checksum_8_10.coverage", "7fb90ede-1409-4c4f-adfd-f85b6cb48172");
                    messageDigest.update(digest);
                    writeline("/home/ubuntu/results/coverage/Checksum/Checksum_8_10.coverage", "015ae7d4-e2d4-4f3b-a985-99c91f49361f");
                    // Add the file path
                    String fileName = getRelativeFilePath(src);
                    writeline("/home/ubuntu/results/coverage/Checksum/Checksum_8_10.coverage", "935ec310-94b3-4ab0-ac06-3c68446addf6");
                    messageDigest.update(fileName.getBytes());
                }
                writeline("/home/ubuntu/results/coverage/Checksum/Checksum_8_10.coverage", "d753b62e-4977-44d0-9777-7eb489eaf243");
                String totalChecksum = createDigestString(messageDigest.digest());
                writeline("/home/ubuntu/results/coverage/Checksum/Checksum_8_10.coverage", "f60094e2-e89e-403f-a833-5767cad3308d");
                getProject().setNewProperty(totalproperty, totalChecksum);
            }
        } catch (Exception e) {
            writeline("/home/ubuntu/results/coverage/Checksum/Checksum_8_10.coverage", "1419a42a-46f1-4def-9081-9b81ede0adb1");
            throw new BuildException(e, getLocation());
        } finally {
            writeline("/home/ubuntu/results/coverage/Checksum/Checksum_8_10.coverage", "05df5b5c-cd05-47d8-ba42-d44377cfc261");
            FileUtils.close(fis);
            writeline("/home/ubuntu/results/coverage/Checksum/Checksum_8_10.coverage", "8cbf7553-9682-4cf1-b5f1-44aa35cc2f13");
            FileUtils.close(fos);
        }
        writeline("/home/ubuntu/results/coverage/Checksum/Checksum_8_10.coverage", "f599a9d2-55fa-4447-9972-5ef736048464");
        return checksumMatches;
    }

    private String createDigestString(byte[] fileDigest) {
        writeline("/home/ubuntu/results/coverage/Checksum/Checksum_8_10.coverage", "a3f10360-c62e-4039-95fb-d78d5ccd4341");
        StringBuffer checksumSb = new StringBuffer();
        writeline("/home/ubuntu/results/coverage/Checksum/Checksum_8_10.coverage", "442bf217-d921-4f3d-a80f-96b8de33263a");
        for (int i = 0; i < fileDigest.length; i++) {
            writeline("/home/ubuntu/results/coverage/Checksum/Checksum_8_10.coverage", "83ac351e-69af-460c-9003-aa6884c6d589");
            String hexStr = Integer.toHexString(BYTE_MASK & fileDigest[i]);
            writeline("/home/ubuntu/results/coverage/Checksum/Checksum_8_10.coverage", "392d0c84-ca88-4b54-9013-28d10213f434");
            if (hexStr.length() < 2) {
                writeline("/home/ubuntu/results/coverage/Checksum/Checksum_8_10.coverage", "3902a047-51e6-45c9-a50d-bcb731788b30");
                checksumSb.append("0");
            }
            writeline("/home/ubuntu/results/coverage/Checksum/Checksum_8_10.coverage", "9dae147e-c614-478a-a7f6-70c2270d33f2");
            checksumSb.append(hexStr);
        }
        writeline("/home/ubuntu/results/coverage/Checksum/Checksum_8_10.coverage", "2672741b-d637-4139-93f0-b9a8f6ff8ab0");
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
        writelineStatic("/home/ubuntu/results/coverage/Checksum/Checksum_8_10.coverage", "999d0f52-b765-445f-8886-aa5277ca416c");
        int l = data.length;
        writelineStatic("/home/ubuntu/results/coverage/Checksum/Checksum_8_10.coverage", "a32779ff-7273-4950-a6ae-df3a309f4499");
        if ((l & 0x01) != 0) {
            writelineStatic("/home/ubuntu/results/coverage/Checksum/Checksum_8_10.coverage", "5d104827-c065-4225-b47d-ca7cc18cdc47");
            throw new BuildException("odd number of characters.");
        }
        writelineStatic("/home/ubuntu/results/coverage/Checksum/Checksum_8_10.coverage", "67701f28-920c-4192-807e-20869a2d8f60");
        byte[] out = new byte[l >> 1];
        writelineStatic("/home/ubuntu/results/coverage/Checksum/Checksum_8_10.coverage", "c21c516d-ed8b-461e-bf24-b2b52f414352");
        // two characters form the hex value.
        for (int i = 0, j = 0; j < l; i++) {
            writelineStatic("/home/ubuntu/results/coverage/Checksum/Checksum_8_10.coverage", "22fda32d-ea71-4227-87eb-56bbd4c021d1");
            int f = Character.digit(data[j++], WORD) << NIBBLE;
            writelineStatic("/home/ubuntu/results/coverage/Checksum/Checksum_8_10.coverage", "57d80d6b-dba4-40ed-b7f4-920c6062c92e");
            f = f | Character.digit(data[j++], WORD);
            writelineStatic("/home/ubuntu/results/coverage/Checksum/Checksum_8_10.coverage", "687b7e88-c5ad-4082-9355-6088e30a9191");
            out[i] = (byte) (f & BYTE_MASK);
        }
        writelineStatic("/home/ubuntu/results/coverage/Checksum/Checksum_8_10.coverage", "338bf3bb-7fc7-4df3-b786-a94fdfe15297");
        return out;
    }

    /**
     * reads the checksum from a file using the specified format.
     *
     * @since 1.7
     */
    private String readChecksum(File f) {
        writeline("/home/ubuntu/results/coverage/Checksum/Checksum_8_10.coverage", "b6831147-9b75-42b6-99f8-7aa647b97358");
        BufferedReader diskChecksumReader = null;
        writeline("/home/ubuntu/results/coverage/Checksum/Checksum_8_10.coverage", "b248702c-ba78-4ca2-b7d7-8e7a586bd258");
        try {
            writeline("/home/ubuntu/results/coverage/Checksum/Checksum_8_10.coverage", "3787f61a-5c5f-451e-ba1e-e28f813a8873");
            diskChecksumReader = new BufferedReader(new FileReader(f));
            writeline("/home/ubuntu/results/coverage/Checksum/Checksum_8_10.coverage", "1317ae92-ede1-4057-8d82-5ac89eb13006");
            Object[] result = format.parse(diskChecksumReader.readLine());
            writeline("/home/ubuntu/results/coverage/Checksum/Checksum_8_10.coverage", "46287765-df84-40a5-8f54-740a1ddb0b8a");
            if (result == null || result.length == 0 || result[0] == null) {
                writeline("/home/ubuntu/results/coverage/Checksum/Checksum_8_10.coverage", "289354ee-88cd-469e-9389-5dd82c8b48bc");
                throw new BuildException("failed to find a checksum");
            }
            writeline("/home/ubuntu/results/coverage/Checksum/Checksum_8_10.coverage", "2e17c745-31bc-45fd-8e17-3cd92699a566");
            return (String) result[0];
        } catch (IOException e) {
            writeline("/home/ubuntu/results/coverage/Checksum/Checksum_8_10.coverage", "f29ec411-0759-4888-868f-252b2b381b23");
            throw new BuildException("Couldn't read checksum file " + f, e);
        } catch (ParseException e) {
            writeline("/home/ubuntu/results/coverage/Checksum/Checksum_8_10.coverage", "bd7b2bb5-e838-4257-86fa-02b2715e88ba");
            throw new BuildException("Couldn't read checksum file " + f, e);
        } finally {
            writeline("/home/ubuntu/results/coverage/Checksum/Checksum_8_10.coverage", "bf7de045-7e1a-4189-915d-a0434051fa06");
            FileUtils.close(diskChecksumReader);
        }
    }

    /**
     * @since Ant 1.8.2
     */
    private String getRelativeFilePath(File f) {
        writeline("/home/ubuntu/results/coverage/Checksum/Checksum_8_10.coverage", "92ba9ba4-d850-400c-bad8-34ce5f32630d");
        String path = (String) relativeFilePaths.get(f);
        writeline("/home/ubuntu/results/coverage/Checksum/Checksum_8_10.coverage", "9d9feac3-fc36-4202-8f8b-85ff4833f4ef");
        if (path == null) {
            writeline("/home/ubuntu/results/coverage/Checksum/Checksum_8_10.coverage", "8d876e04-8c21-4a22-a3cb-9afbddf352e5");
            // bug 37386. this should not occur, but it has, once.
            throw new BuildException("Internal error: " + "relativeFilePaths could not match file " + f + "\n" + "please file a bug report on this");
        }
        writeline("/home/ubuntu/results/coverage/Checksum/Checksum_8_10.coverage", "954c5b67-f44a-4f92-a901-23321c59e0b1");
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
