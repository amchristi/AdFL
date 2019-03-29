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
        writeline("/home/ubuntu/results/coverage/Checksum/Checksum_9_10.coverage", "63567fbd-96aa-4348-ab3f-6be91be66c9b");
        this.file = file;
    }

    /**
     * Sets the root directory where checksum files will be
     * written/read
     * @param todir the directory to write to
     * @since Ant 1.6
     */
    public void setTodir(File todir) {
        writeline("/home/ubuntu/results/coverage/Checksum/Checksum_9_10.coverage", "2833acfd-a2df-4113-b231-cb6eed77c4e6");
        this.todir = todir;
    }

    /**
     * Specifies the algorithm to be used to compute the checksum.
     * Defaults to "MD5". Other popular algorithms like "SHA" may be used as well.
     * @param algorithm a <code>String</code> value
     */
    public void setAlgorithm(String algorithm) {
        writeline("/home/ubuntu/results/coverage/Checksum/Checksum_9_10.coverage", "ec980646-fc63-4d5a-8ba5-f04ff6f97531");
        this.algorithm = algorithm;
    }

    /**
     * Sets the MessageDigest algorithm provider to be used
     * to calculate the checksum.
     * @param provider a <code>String</code> value
     */
    public void setProvider(String provider) {
        writeline("/home/ubuntu/results/coverage/Checksum/Checksum_9_10.coverage", "4b74019b-6109-42d1-aea5-3d1447a00312");
        this.provider = provider;
    }

    /**
     * Sets the file extension that is be to used to
     * create or identify destination file.
     * @param fileext a <code>String</code> value
     */
    public void setFileext(String fileext) {
        writeline("/home/ubuntu/results/coverage/Checksum/Checksum_9_10.coverage", "9e76f9bf-df20-4fe5-bdd9-266c0ae89337");
        this.fileext = fileext;
    }

    /**
     * Sets the property to hold the generated checksum.
     * @param property a <code>String</code> value
     */
    public void setProperty(String property) {
        writeline("/home/ubuntu/results/coverage/Checksum/Checksum_9_10.coverage", "46fffd3c-1527-41a6-9b06-910ebc327494");
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
        writeline("/home/ubuntu/results/coverage/Checksum/Checksum_9_10.coverage", "a3bf68e4-07b7-4347-8fdd-2716e295bad2");
        this.totalproperty = totalproperty;
    }

    /**
     * Sets the verify property.  This project property holds
     * the result of a checksum verification - "true" or "false"
     * @param verifyProperty a <code>String</code> value
     */
    public void setVerifyproperty(String verifyProperty) {
        writeline("/home/ubuntu/results/coverage/Checksum/Checksum_9_10.coverage", "121f6910-3f16-461f-9d0e-717e750a738c");
        this.verifyProperty = verifyProperty;
    }

    /**
     * Whether or not to overwrite existing file irrespective of
     * whether it is newer than
     * the source file.  Defaults to false.
     * @param forceOverwrite a <code>boolean</code> value
     */
    public void setForceOverwrite(boolean forceOverwrite) {
        writeline("/home/ubuntu/results/coverage/Checksum/Checksum_9_10.coverage", "97cc8426-6167-41e7-95f8-4666eb2dbac4");
        this.forceOverwrite = forceOverwrite;
    }

    /**
     * The size of the read buffer to use.
     * @param size an <code>int</code> value
     */
    public void setReadBufferSize(int size) {
        writeline("/home/ubuntu/results/coverage/Checksum/Checksum_9_10.coverage", "5131ddc3-8c6c-4057-a906-478e84a5a685");
        this.readBufferSize = size;
    }

    /**
     * Select the in/output pattern via a well know format name.
     * @param e an <code>enumerated</code> value
     *
     * @since 1.7.0
     */
    public void setFormat(FormatElement e) {
        writeline("/home/ubuntu/results/coverage/Checksum/Checksum_9_10.coverage", "04059a70-d46a-4b84-8daf-57e21b3c131f");
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
        writeline("/home/ubuntu/results/coverage/Checksum/Checksum_9_10.coverage", "63fb4fe8-37ba-43aa-9a7a-7258c43ca9dc");
        format = new MessageFormat(p);
    }

    /**
     * Files to generate checksums for.
     * @param set a fileset of files to generate checksums for.
     */
    public void addFileset(FileSet set) {
        writeline("/home/ubuntu/results/coverage/Checksum/Checksum_9_10.coverage", "88c6fda4-d3e4-4e20-acbd-fafad4615ac5");
        add(set);
    }

    /**
     * Add a resource collection.
     * @param rc the ResourceCollection to add.
     */
    public void add(ResourceCollection rc) {
        writeline("/home/ubuntu/results/coverage/Checksum/Checksum_9_10.coverage", "03dfe30c-89a5-4cb5-9f88-92fea17069cb");
        if (rc == null) {
            writeline("/home/ubuntu/results/coverage/Checksum/Checksum_9_10.coverage", "ee59810a-9fa7-46b2-9689-6f014e31f096");
            return;
        }
        writeline("/home/ubuntu/results/coverage/Checksum/Checksum_9_10.coverage", "771bdc5e-9b5e-4e26-b9de-eb6fa6e3c757");
        resources = (resources == null) ? new FileUnion() : resources;
        writeline("/home/ubuntu/results/coverage/Checksum/Checksum_9_10.coverage", "66ea2087-133a-4d58-a077-9eb2e683aae4");
        resources.add(rc);
    }

    /**
     * Calculate the checksum(s).
     * @throws BuildException on error
     */
    public void execute() throws BuildException {
        writeline("/home/ubuntu/results/coverage/Checksum/Checksum_9_10.coverage", "d62be848-bba1-4e86-be00-5e042c80f3e7");
        isCondition = false;
        writeline("/home/ubuntu/results/coverage/Checksum/Checksum_9_10.coverage", "b95c1d33-f355-41d4-aac7-d63d458a49f6");
        boolean value = validateAndExecute();
        writeline("/home/ubuntu/results/coverage/Checksum/Checksum_9_10.coverage", "c3553c01-c4a4-4361-81b9-8863c8c56dc1");
        if (verifyProperty != null) {
            writeline("/home/ubuntu/results/coverage/Checksum/Checksum_9_10.coverage", "fe279a0e-aeeb-443c-94bf-c55ce75b8049");
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
        writeline("/home/ubuntu/results/coverage/Checksum/Checksum_9_10.coverage", "8dc27eff-1caf-439a-bdbd-38853467d12a");
        isCondition = true;
        writeline("/home/ubuntu/results/coverage/Checksum/Checksum_9_10.coverage", "eb746726-5747-460e-96e3-f238eda54bd3");
        return validateAndExecute();
    }

    /**
     * Validate attributes and get down to business.
     */
    private boolean validateAndExecute() throws BuildException {
        writeline("/home/ubuntu/results/coverage/Checksum/Checksum_9_10.coverage", "0b1fcea1-afcf-4d0d-bf47-cb53a27ef078");
        String savedFileExt = fileext;
        writeline("/home/ubuntu/results/coverage/Checksum/Checksum_9_10.coverage", "47224dfe-72a6-4244-b815-de949b967dc9");
        if (file == null && (resources == null || resources.size() == 0)) {
            writeline("/home/ubuntu/results/coverage/Checksum/Checksum_9_10.coverage", "c71cd35b-1d65-402c-9743-04f06e8e3fc9");
            throw new BuildException("Specify at least one source - a file or a resource collection.");
        }
        writeline("/home/ubuntu/results/coverage/Checksum/Checksum_9_10.coverage", "dbe3e24c-4f01-4940-acd5-2a2c6c57f44a");
        if (!(resources == null || resources.isFilesystemOnly())) {
            writeline("/home/ubuntu/results/coverage/Checksum/Checksum_9_10.coverage", "a99ecd14-4e8c-4134-bf49-71c287ce2e80");
            throw new BuildException("Can only calculate checksums for file-based resources.");
        }
        writeline("/home/ubuntu/results/coverage/Checksum/Checksum_9_10.coverage", "f68d3d16-960a-4fa1-b9ed-9e236d118b17");
        if (file != null && file.exists() && file.isDirectory()) {
            writeline("/home/ubuntu/results/coverage/Checksum/Checksum_9_10.coverage", "6412275e-0e43-455c-90b6-80ad04bc2c50");
            throw new BuildException("Checksum cannot be generated for directories");
        }
        writeline("/home/ubuntu/results/coverage/Checksum/Checksum_9_10.coverage", "6956512e-9d18-4742-9d4c-6a2004e04708");
        if (file != null && totalproperty != null) {
            writeline("/home/ubuntu/results/coverage/Checksum/Checksum_9_10.coverage", "a2bab03b-a69d-4b9a-80cf-3ef88852a43e");
            throw new BuildException("File and Totalproperty cannot co-exist.");
        }
        writeline("/home/ubuntu/results/coverage/Checksum/Checksum_9_10.coverage", "5746ed52-662c-496e-a1ca-6a824c4b1c3a");
        if (property != null && fileext != null) {
            writeline("/home/ubuntu/results/coverage/Checksum/Checksum_9_10.coverage", "294740b0-dfc0-492a-ad49-efac0b9e0256");
            throw new BuildException("Property and FileExt cannot co-exist.");
        }
        writeline("/home/ubuntu/results/coverage/Checksum/Checksum_9_10.coverage", "29e1d51c-90dc-411e-8f41-63b5a4afa0b3");
        if (property != null) {
            writeline("/home/ubuntu/results/coverage/Checksum/Checksum_9_10.coverage", "0a59212b-f026-43cc-ad9e-8b3e5b7dbfa6");
            if (forceOverwrite) {
                writeline("/home/ubuntu/results/coverage/Checksum/Checksum_9_10.coverage", "ee10b33c-ea6b-4a57-9e5c-776595240a7a");
                throw new BuildException("ForceOverwrite cannot be used when Property is specified");
            }
            writeline("/home/ubuntu/results/coverage/Checksum/Checksum_9_10.coverage", "209b201c-97d1-488d-8979-70e54b139a9e");
            int ct = 0;
            writeline("/home/ubuntu/results/coverage/Checksum/Checksum_9_10.coverage", "12c08330-1633-42be-a3ee-656e07856b12");
            if (resources != null) {
                writeline("/home/ubuntu/results/coverage/Checksum/Checksum_9_10.coverage", "0cad1ed4-8fa1-4326-9032-f792f72668eb");
                ct += resources.size();
            }
            writeline("/home/ubuntu/results/coverage/Checksum/Checksum_9_10.coverage", "951e85f8-93bc-4378-91ec-9c7875a3b96d");
            if (file != null) {
                writeline("/home/ubuntu/results/coverage/Checksum/Checksum_9_10.coverage", "eec4bce3-0dde-435d-8618-d83c4d7061b6");
                ct++;
            }
            writeline("/home/ubuntu/results/coverage/Checksum/Checksum_9_10.coverage", "d35433dc-96a3-4e67-afba-9dd136f9514a");
            if (ct > 1) {
                writeline("/home/ubuntu/results/coverage/Checksum/Checksum_9_10.coverage", "69dbb62a-8631-4058-9155-09f6595d254b");
                throw new BuildException("Multiple files cannot be used when Property is specified");
            }
        }
        writeline("/home/ubuntu/results/coverage/Checksum/Checksum_9_10.coverage", "47ddcec4-09f5-45e6-9593-c19879b5ba4e");
        if (verifyProperty != null) {
            writeline("/home/ubuntu/results/coverage/Checksum/Checksum_9_10.coverage", "e4e3f566-a1a0-4513-9873-7e0a07e1b851");
            isCondition = true;
        }
        writeline("/home/ubuntu/results/coverage/Checksum/Checksum_9_10.coverage", "f7f9c91c-5693-4934-8a6a-25797fc4c998");
        if (verifyProperty != null && forceOverwrite) {
            writeline("/home/ubuntu/results/coverage/Checksum/Checksum_9_10.coverage", "c891b265-7468-4ec9-af1c-399f8d5cab1e");
            throw new BuildException("VerifyProperty and ForceOverwrite cannot co-exist.");
        }
        writeline("/home/ubuntu/results/coverage/Checksum/Checksum_9_10.coverage", "e5e6d382-c5ea-42b5-8705-685df0bed261");
        if (isCondition && forceOverwrite) {
            writeline("/home/ubuntu/results/coverage/Checksum/Checksum_9_10.coverage", "8bcb29b3-ae26-4b81-8872-7cad8ce84d01");
            throw new BuildException("ForceOverwrite cannot be used when conditions are being used.");
        }
        writeline("/home/ubuntu/results/coverage/Checksum/Checksum_9_10.coverage", "b24c9181-3017-42b3-af10-c5bf6b1e24fe");
        messageDigest = null;
        writeline("/home/ubuntu/results/coverage/Checksum/Checksum_9_10.coverage", "b794ad4d-6227-445b-9e7f-f64fe28a6784");
        if (provider != null) {
            writeline("/home/ubuntu/results/coverage/Checksum/Checksum_9_10.coverage", "58a8c58f-204b-418d-a2ec-300fe206f7ec");
            try {
                writeline("/home/ubuntu/results/coverage/Checksum/Checksum_9_10.coverage", "3ed3263f-4991-4ad7-8385-5f2de8532d70");
                messageDigest = MessageDigest.getInstance(algorithm, provider);
            } catch (NoSuchAlgorithmException noalgo) {
                writeline("/home/ubuntu/results/coverage/Checksum/Checksum_9_10.coverage", "749cb153-ef9b-4cb1-8f73-63c63cf754ea");
                throw new BuildException(noalgo, getLocation());
            } catch (NoSuchProviderException noprovider) {
                writeline("/home/ubuntu/results/coverage/Checksum/Checksum_9_10.coverage", "75d29a46-2c6e-4ea4-9a8e-e2b7fa04feb0");
                throw new BuildException(noprovider, getLocation());
            }
        } else {
            writeline("/home/ubuntu/results/coverage/Checksum/Checksum_9_10.coverage", "9d2656ba-c520-413e-ad19-2d2fb836331e");
            try {
                writeline("/home/ubuntu/results/coverage/Checksum/Checksum_9_10.coverage", "1ad29032-17f9-439c-8bf8-1459f9e37671");
                messageDigest = MessageDigest.getInstance(algorithm);
            } catch (NoSuchAlgorithmException noalgo) {
                writeline("/home/ubuntu/results/coverage/Checksum/Checksum_9_10.coverage", "67bce87a-2c7e-4812-98b8-40bbf1cd44e2");
                throw new BuildException(noalgo, getLocation());
            }
        }
        writeline("/home/ubuntu/results/coverage/Checksum/Checksum_9_10.coverage", "601a998e-c8b1-4e24-b89b-69a5ebcf97c6");
        if (messageDigest == null) {
            writeline("/home/ubuntu/results/coverage/Checksum/Checksum_9_10.coverage", "a25c0ba1-bf66-4e20-bd89-90d7a2278027");
            throw new BuildException("Unable to create Message Digest", getLocation());
        }
        writeline("/home/ubuntu/results/coverage/Checksum/Checksum_9_10.coverage", "b4dc279c-19d0-4b05-955f-4136f23451fe");
        if (fileext == null) {
            writeline("/home/ubuntu/results/coverage/Checksum/Checksum_9_10.coverage", "52730913-14c7-4bd4-9051-91143ba05e7c");
            fileext = "." + algorithm;
        } else if (fileext.trim().length() == 0) {
            writeline("/home/ubuntu/results/coverage/Checksum/Checksum_9_10.coverage", "8dc66a11-a352-4d11-a6e8-6c56ec17c8bc");
            throw new BuildException("File extension when specified must not be an empty string");
        }
        writeline("/home/ubuntu/results/coverage/Checksum/Checksum_9_10.coverage", "29edace4-9843-4da1-91a7-1064f377ba3c");
        try {
            writeline("/home/ubuntu/results/coverage/Checksum/Checksum_9_10.coverage", "bd57a9f6-2305-4700-ac89-205235793a4e");
            if (resources != null) {
                writeline("/home/ubuntu/results/coverage/Checksum/Checksum_9_10.coverage", "16b9991c-5725-4180-bbb2-0932113a210c");
                for (Resource r : resources) {
                    writeline("/home/ubuntu/results/coverage/Checksum/Checksum_9_10.coverage", "84ea0264-3bbd-46a7-946b-df7a1e957345");
                    File src = r.as(FileProvider.class).getFile();
                    writeline("/home/ubuntu/results/coverage/Checksum/Checksum_9_10.coverage", "8b8cbd97-5384-418e-b446-309d23b4cce5");
                    if (totalproperty != null || todir != null) {
                        writeline("/home/ubuntu/results/coverage/Checksum/Checksum_9_10.coverage", "685d2590-065e-4310-8079-781c97abaedd");
                        // Use '/' to calculate digest based on file name.
                        // This is required in order to get the same result
                        // on different platforms.
                        relativeFilePaths.put(src, r.getName().replace(File.separatorChar, '/'));
                    }
                    writeline("/home/ubuntu/results/coverage/Checksum/Checksum_9_10.coverage", "3393dc59-a7e5-4de1-b1b0-c22d1db67e63");
                    addToIncludeFileMap(src);
                }
            }
            writeline("/home/ubuntu/results/coverage/Checksum/Checksum_9_10.coverage", "81e58380-5d9d-4b2b-b1a0-d54d20f0c098");
            if (file != null) {
                writeline("/home/ubuntu/results/coverage/Checksum/Checksum_9_10.coverage", "ed0df181-4fbc-4afb-a287-7972866e4156");
                if (totalproperty != null || todir != null) {
                    writeline("/home/ubuntu/results/coverage/Checksum/Checksum_9_10.coverage", "a989f034-8749-4cb8-8238-16e8e37e541a");
                    relativeFilePaths.put(file, file.getName().replace(File.separatorChar, '/'));
                }
                writeline("/home/ubuntu/results/coverage/Checksum/Checksum_9_10.coverage", "d2e7062c-4cad-4213-b9ec-d6f92ff75b3a");
                addToIncludeFileMap(file);
            }
            writeline("/home/ubuntu/results/coverage/Checksum/Checksum_9_10.coverage", "342f8921-5459-4ecf-b153-26f0b4184e3b");
            return generateChecksums();
        } finally {
            writeline("/home/ubuntu/results/coverage/Checksum/Checksum_9_10.coverage", "60c6b52e-708d-4ed0-8577-3f936e1f3687");
            fileext = savedFileExt;
            writeline("/home/ubuntu/results/coverage/Checksum/Checksum_9_10.coverage", "8df06ade-93ff-4ca2-910a-100807156d6a");
            includeFileMap.clear();
        }
    }

    /**
     * Add key-value pair to the hashtable upon which
     * to later operate upon.
     */
    private void addToIncludeFileMap(File file) throws BuildException {
        writeline("/home/ubuntu/results/coverage/Checksum/Checksum_9_10.coverage", "05df6916-2250-4db3-b017-7bf05525ba27");
        if (file.exists()) {
            writeline("/home/ubuntu/results/coverage/Checksum/Checksum_9_10.coverage", "2cd11bf7-2b3e-4c25-873f-c63ef0cacc79");
            if (property == null) {
                writeline("/home/ubuntu/results/coverage/Checksum/Checksum_9_10.coverage", "674374b2-f427-4fbf-a2c6-99dc0a09b0b9");
                File checksumFile = getChecksumFile(file);
                writeline("/home/ubuntu/results/coverage/Checksum/Checksum_9_10.coverage", "ab252566-9565-4bc1-af05-db2a40270122");
                if (forceOverwrite || isCondition || (file.lastModified() > checksumFile.lastModified())) {
                    writeline("/home/ubuntu/results/coverage/Checksum/Checksum_9_10.coverage", "b7e36cba-d948-4884-a2e0-b0f2f8c1fdc7");
                    includeFileMap.put(file, checksumFile);
                } else {
                    writeline("/home/ubuntu/results/coverage/Checksum/Checksum_9_10.coverage", "14102f6a-0be7-4a41-9f93-9e5f4119562b");
                    log(file + " omitted as " + checksumFile + " is up to date.", Project.MSG_VERBOSE);
                    writeline("/home/ubuntu/results/coverage/Checksum/Checksum_9_10.coverage", "1154b330-d398-4daf-ba5b-f05098e550f3");
                    if (totalproperty != null) {
                        writeline("/home/ubuntu/results/coverage/Checksum/Checksum_9_10.coverage", "e925128a-43c8-4a50-a6fc-7a343adb2749");
                        // Read the checksum from disk.
                        String checksum = readChecksum(checksumFile);
                        writeline("/home/ubuntu/results/coverage/Checksum/Checksum_9_10.coverage", "ea063bb1-fbd0-4562-b473-5004f356ae02");
                        byte[] digest = decodeHex(checksum.toCharArray());
                        writeline("/home/ubuntu/results/coverage/Checksum/Checksum_9_10.coverage", "1b9832a1-a535-48d8-b4a5-7da990b8f7a8");
                        allDigests.put(file, digest);
                    }
                }
            } else {
                writeline("/home/ubuntu/results/coverage/Checksum/Checksum_9_10.coverage", "2455dc32-7c41-47bb-9390-40d1b6235dc9");
                includeFileMap.put(file, property);
            }
        } else {
            writeline("/home/ubuntu/results/coverage/Checksum/Checksum_9_10.coverage", "e2e6b28f-8c61-4bec-a637-fc1888d3842d");
            String message = "Could not find file " + file.getAbsolutePath() + " to generate checksum for.";
            writeline("/home/ubuntu/results/coverage/Checksum/Checksum_9_10.coverage", "e130a2ec-3e4e-4914-98bd-1d63a8ffb7ac");
            log(message);
            writeline("/home/ubuntu/results/coverage/Checksum/Checksum_9_10.coverage", "bc51b2e0-094c-4269-8877-6b6d91c25404");
            throw new BuildException(message, getLocation());
        }
    }

    private File getChecksumFile(File file) {
        writeline("/home/ubuntu/results/coverage/Checksum/Checksum_9_10.coverage", "907b6d0a-3633-4659-9d7b-d5a165d8c068");
        File directory;
        writeline("/home/ubuntu/results/coverage/Checksum/Checksum_9_10.coverage", "997b9558-8fa2-492e-a9cd-73053fc2beb4");
        if (todir != null) {
            writeline("/home/ubuntu/results/coverage/Checksum/Checksum_9_10.coverage", "73f33b8b-2aa7-4fd1-9444-b6e45c6f0672");
            // A separate directory was explicitly declared
            String path = getRelativeFilePath(file);
            writeline("/home/ubuntu/results/coverage/Checksum/Checksum_9_10.coverage", "6d416ebb-1c39-49f5-90e5-64b21d5a9694");
            directory = new File(todir, path).getParentFile();
            writeline("/home/ubuntu/results/coverage/Checksum/Checksum_9_10.coverage", "57520fe0-771d-48e6-8397-907465ef8004");
            // Create the directory, as it might not exist.
            directory.mkdirs();
        } else {
            writeline("/home/ubuntu/results/coverage/Checksum/Checksum_9_10.coverage", "e1bd66ed-9dac-4e51-852d-8a6b18b5e3a7");
            // Just use the same directory as the file itself.
            // This directory will exist
            directory = file.getParentFile();
        }
        writeline("/home/ubuntu/results/coverage/Checksum/Checksum_9_10.coverage", "5edf8452-a1e9-47a9-b5b7-4670b27764d8");
        File checksumFile = new File(directory, file.getName() + fileext);
        writeline("/home/ubuntu/results/coverage/Checksum/Checksum_9_10.coverage", "b5cf2802-9c3f-45d6-84e1-55256852d573");
        return checksumFile;
    }

    /**
     * Generate checksum(s) using the message digest created earlier.
     */
    private boolean generateChecksums() throws BuildException {
        writeline("/home/ubuntu/results/coverage/Checksum/Checksum_9_10.coverage", "a99fcca9-0027-4459-896c-4e9942073200");
        boolean checksumMatches = true;
        writeline("/home/ubuntu/results/coverage/Checksum/Checksum_9_10.coverage", "548c8ad7-4a73-438e-9926-5b6649b74dc0");
        FileInputStream fis = null;
        writeline("/home/ubuntu/results/coverage/Checksum/Checksum_9_10.coverage", "7d48197e-1082-4e40-81ba-dd52874d8c18");
        FileOutputStream fos = null;
        writeline("/home/ubuntu/results/coverage/Checksum/Checksum_9_10.coverage", "70cef185-252b-471e-b0ee-0d6740e14cef");
        byte[] buf = new byte[readBufferSize];
        writeline("/home/ubuntu/results/coverage/Checksum/Checksum_9_10.coverage", "6b0204de-e4dd-4ef1-be3b-e12d655b174c");
        try {
            writeline("/home/ubuntu/results/coverage/Checksum/Checksum_9_10.coverage", "29c70bba-a21a-4819-9212-e31f23697428");
            for (Map.Entry<File, Object> e : includeFileMap.entrySet()) {
                writeline("/home/ubuntu/results/coverage/Checksum/Checksum_9_10.coverage", "6bac9a5d-5fb7-49ae-93f7-0e0e4550be4a");
                messageDigest.reset();
                writeline("/home/ubuntu/results/coverage/Checksum/Checksum_9_10.coverage", "c3afbee7-3acb-4340-95f4-904e23a52356");
                File src = e.getKey();
                writeline("/home/ubuntu/results/coverage/Checksum/Checksum_9_10.coverage", "9f08528f-5075-4286-b27d-972eb5a08bb8");
                if (!isCondition) {
                    writeline("/home/ubuntu/results/coverage/Checksum/Checksum_9_10.coverage", "6a8d4309-186a-43a4-8b17-1db652f4e87a");
                    log("Calculating " + algorithm + " checksum for " + src, Project.MSG_VERBOSE);
                }
                writeline("/home/ubuntu/results/coverage/Checksum/Checksum_9_10.coverage", "87c18c73-d1c5-4a7a-9b29-e02028016959");
                fis = new FileInputStream(src);
                writeline("/home/ubuntu/results/coverage/Checksum/Checksum_9_10.coverage", "1c9bc23e-4002-482c-9e05-04b2dff7101e");
                DigestInputStream dis = new DigestInputStream(fis, messageDigest);
                writeline("/home/ubuntu/results/coverage/Checksum/Checksum_9_10.coverage", "ec2a3067-4e00-4970-a963-1522e8d76fa4");
                while (dis.read(buf, 0, readBufferSize) != -1) {
                // Empty statement
                }
                writeline("/home/ubuntu/results/coverage/Checksum/Checksum_9_10.coverage", "58abfcf1-7a6d-4c8a-8de6-579327934d87");
                dis.close();
                writeline("/home/ubuntu/results/coverage/Checksum/Checksum_9_10.coverage", "653900ed-44d6-4f2d-9fab-7f4b8ae178f6");
                fis.close();
                writeline("/home/ubuntu/results/coverage/Checksum/Checksum_9_10.coverage", "5939a4be-b685-45d8-b083-5cd97eeb7a51");
                fis = null;
                writeline("/home/ubuntu/results/coverage/Checksum/Checksum_9_10.coverage", "0e271efb-e605-4bda-9368-c4ae177d1aeb");
                byte[] fileDigest = messageDigest.digest();
                writeline("/home/ubuntu/results/coverage/Checksum/Checksum_9_10.coverage", "2bd7d969-7563-4d5d-a40f-2e9c1bdd5558");
                if (totalproperty != null) {
                    writeline("/home/ubuntu/results/coverage/Checksum/Checksum_9_10.coverage", "968a8158-b917-4c6f-b75d-943ab32b3814");
                    allDigests.put(src, fileDigest);
                }
                writeline("/home/ubuntu/results/coverage/Checksum/Checksum_9_10.coverage", "1f23122c-7e03-41f0-aeb2-3f51ec582b50");
                String checksum = createDigestString(fileDigest);
                writeline("/home/ubuntu/results/coverage/Checksum/Checksum_9_10.coverage", "d80af892-1ca6-4771-aae0-cc5118c566b1");
                // can either be a property name string or a file
                Object destination = e.getValue();
                writeline("/home/ubuntu/results/coverage/Checksum/Checksum_9_10.coverage", "6aebb614-bf70-4cec-b1b2-f325c1bd9861");
                if (destination instanceof java.lang.String) {
                    writeline("/home/ubuntu/results/coverage/Checksum/Checksum_9_10.coverage", "a8bdfb2f-690b-412b-b7a9-569544d1140b");
                    String prop = (String) destination;
                    writeline("/home/ubuntu/results/coverage/Checksum/Checksum_9_10.coverage", "dbe40e62-fb7b-4607-a569-5c454f3a5dc7");
                    if (isCondition) {
                        writeline("/home/ubuntu/results/coverage/Checksum/Checksum_9_10.coverage", "6d7d732b-2007-46aa-8ff0-055edb6ea227");
                        checksumMatches = checksumMatches && checksum.equals(property);
                    } else {
                        writeline("/home/ubuntu/results/coverage/Checksum/Checksum_9_10.coverage", "77a04088-68bd-462b-b626-eb05c0378a7f");
                        getProject().setNewProperty(prop, checksum);
                    }
                } else if (destination instanceof java.io.File) {
                    writeline("/home/ubuntu/results/coverage/Checksum/Checksum_9_10.coverage", "a2092ce3-0f74-4cac-9587-0e337da758eb");
                    if (isCondition) {
                        writeline("/home/ubuntu/results/coverage/Checksum/Checksum_9_10.coverage", "4cb16263-1384-439d-a9d3-1249a070200e");
                        File existingFile = (File) destination;
                        writeline("/home/ubuntu/results/coverage/Checksum/Checksum_9_10.coverage", "9b40b3e7-1822-4efd-ac01-e1976a0b8344");
                        if (existingFile.exists()) {
                            writeline("/home/ubuntu/results/coverage/Checksum/Checksum_9_10.coverage", "00fcc7eb-bf4e-465b-9172-12dc4169e0e7");
                            try {
                                writeline("/home/ubuntu/results/coverage/Checksum/Checksum_9_10.coverage", "5c533072-3d03-47d4-842b-f8fbd8c233fa");
                                String suppliedChecksum = readChecksum(existingFile);
                                writeline("/home/ubuntu/results/coverage/Checksum/Checksum_9_10.coverage", "0a04ab49-39ee-4fbf-b755-2b94862a0cc4");
                                checksumMatches = checksumMatches && checksum.equals(suppliedChecksum);
                            } catch (BuildException be) {
                                writeline("/home/ubuntu/results/coverage/Checksum/Checksum_9_10.coverage", "d98a2a73-7362-4e33-afc6-3d30bf88acf9");
                                // file is on wrong format, swallow
                                checksumMatches = false;
                            }
                        } else {
                            writeline("/home/ubuntu/results/coverage/Checksum/Checksum_9_10.coverage", "c098b005-de71-432f-bcb2-76040529ed2b");
                            checksumMatches = false;
                        }
                    } else {
                        writeline("/home/ubuntu/results/coverage/Checksum/Checksum_9_10.coverage", "e8d2f339-8249-474d-9db0-229bc5a81758");
                        File dest = (File) destination;
                        writeline("/home/ubuntu/results/coverage/Checksum/Checksum_9_10.coverage", "a26f7162-ea43-4039-b4de-96fe56243106");
                        fos = new FileOutputStream(dest);
                        writeline("/home/ubuntu/results/coverage/Checksum/Checksum_9_10.coverage", "63d17bd5-4201-47b5-80ef-d926eecd1c74");
                        fos.write(format.format(new Object[] { checksum, src.getName(), FileUtils.getRelativePath(dest.getParentFile(), src), FileUtils.getRelativePath(getProject().getBaseDir(), src), src.getAbsolutePath() }).getBytes());
                        writeline("/home/ubuntu/results/coverage/Checksum/Checksum_9_10.coverage", "8f5a1e5f-8e00-44bb-9f56-2ce17bcce092");
                        fos.write(StringUtils.LINE_SEP.getBytes());
                        writeline("/home/ubuntu/results/coverage/Checksum/Checksum_9_10.coverage", "d2a653f9-3f77-4f5c-a11d-3a77261d53d6");
                        fos.close();
                        writeline("/home/ubuntu/results/coverage/Checksum/Checksum_9_10.coverage", "4e584275-2236-4085-a00e-c2bf21b611fb");
                        fos = null;
                    }
                }
            }
            writeline("/home/ubuntu/results/coverage/Checksum/Checksum_9_10.coverage", "a317d302-a1cb-4b34-8575-e6316be9df93");
            if (totalproperty != null) {
                writeline("/home/ubuntu/results/coverage/Checksum/Checksum_9_10.coverage", "bc345f48-e81d-4ef4-b0e3-a34520db7698");
                // Calculate the total checksum
                // Convert the keys (source files) into a sorted array.
                File[] keyArray = allDigests.keySet().toArray(new File[allDigests.size()]);
                writeline("/home/ubuntu/results/coverage/Checksum/Checksum_9_10.coverage", "c67a8a45-67d7-43b4-9cc4-10d8589df6f1");
                // File is Comparable, but sort-order is platform
                // dependent (case-insensitive on Windows)
                Arrays.sort(keyArray, new Comparator<File>() {

                    public int compare(File f1, File f2) {
                        return f1 == null ? (f2 == null ? 0 : -1) : (f2 == null ? 1 : getRelativeFilePath(f1).compareTo(getRelativeFilePath(f2)));
                    }
                });
                writeline("/home/ubuntu/results/coverage/Checksum/Checksum_9_10.coverage", "8105ecf2-5c25-4685-83b3-b7464e801b0b");
                // Loop over the checksums and generate a total hash.
                messageDigest.reset();
                writeline("/home/ubuntu/results/coverage/Checksum/Checksum_9_10.coverage", "ccac26b9-2cc9-4067-b728-ed9676024410");
                for (File src : keyArray) {
                    writeline("/home/ubuntu/results/coverage/Checksum/Checksum_9_10.coverage", "28c38e30-79b7-456c-bd13-e1641bb47548");
                    // Add the digest for the file content
                    byte[] digest = allDigests.get(src);
                    writeline("/home/ubuntu/results/coverage/Checksum/Checksum_9_10.coverage", "f93eaac7-6ceb-4a6c-812a-0fbf4e348e28");
                    messageDigest.update(digest);
                    writeline("/home/ubuntu/results/coverage/Checksum/Checksum_9_10.coverage", "705a76f7-af6f-46ca-b426-368194b4baf9");
                    // Add the file path
                    String fileName = getRelativeFilePath(src);
                    writeline("/home/ubuntu/results/coverage/Checksum/Checksum_9_10.coverage", "766ad53e-0294-4fd6-9ece-82138e729e3e");
                    messageDigest.update(fileName.getBytes());
                }
                writeline("/home/ubuntu/results/coverage/Checksum/Checksum_9_10.coverage", "472957f0-e0de-472b-80f4-2d13200538cc");
                String totalChecksum = createDigestString(messageDigest.digest());
                writeline("/home/ubuntu/results/coverage/Checksum/Checksum_9_10.coverage", "f0ad4a33-6763-4d05-b625-6886d1133b7b");
                getProject().setNewProperty(totalproperty, totalChecksum);
            }
        } catch (Exception e) {
            writeline("/home/ubuntu/results/coverage/Checksum/Checksum_9_10.coverage", "ffd0732a-0c57-46e1-b50b-43658373d3ac");
            throw new BuildException(e, getLocation());
        } finally {
            writeline("/home/ubuntu/results/coverage/Checksum/Checksum_9_10.coverage", "51c5b7b8-905a-4859-8436-f4b2ccc3b241");
            FileUtils.close(fis);
            writeline("/home/ubuntu/results/coverage/Checksum/Checksum_9_10.coverage", "6ada5000-3a1b-41ed-882b-f7ee9b428f3b");
            FileUtils.close(fos);
        }
        writeline("/home/ubuntu/results/coverage/Checksum/Checksum_9_10.coverage", "9b8dc6e3-e7b6-424f-816f-4c8a304d8309");
        return checksumMatches;
    }

    private String createDigestString(byte[] fileDigest) {
        writeline("/home/ubuntu/results/coverage/Checksum/Checksum_9_10.coverage", "fa624096-414a-4c35-ac48-c469acdab0ab");
        StringBuffer checksumSb = new StringBuffer();
        writeline("/home/ubuntu/results/coverage/Checksum/Checksum_9_10.coverage", "74708977-1a69-4e75-9251-710a1609482f");
        for (int i = 0; i < fileDigest.length; i++) {
            writeline("/home/ubuntu/results/coverage/Checksum/Checksum_9_10.coverage", "17295315-2042-48d7-aa2d-bc977fd30ab3");
            String hexStr = Integer.toHexString(BYTE_MASK & fileDigest[i]);
            writeline("/home/ubuntu/results/coverage/Checksum/Checksum_9_10.coverage", "1bb109a4-a084-4c25-bf70-7d6ab97dbacf");
            if (hexStr.length() < 2) {
                writeline("/home/ubuntu/results/coverage/Checksum/Checksum_9_10.coverage", "58012345-d7b6-410a-8664-849403c7a1c2");
                checksumSb.append("0");
            }
            writeline("/home/ubuntu/results/coverage/Checksum/Checksum_9_10.coverage", "d1440d48-bbb0-4c1b-8ff8-dd273abc5327");
            checksumSb.append(hexStr);
        }
        writeline("/home/ubuntu/results/coverage/Checksum/Checksum_9_10.coverage", "b3e2d9be-9071-471c-8f27-be06f30e957c");
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
        writelineStatic("/home/ubuntu/results/coverage/Checksum/Checksum_9_10.coverage", "1720eafc-8bce-4535-8b59-a73dbf365724");
        int l = data.length;
        writelineStatic("/home/ubuntu/results/coverage/Checksum/Checksum_9_10.coverage", "b5193aa8-f5ab-4a4e-bd4a-c8a26a4153cf");
        if ((l & 0x01) != 0) {
            writelineStatic("/home/ubuntu/results/coverage/Checksum/Checksum_9_10.coverage", "6b4e912c-920d-4343-82d9-33c61af146b3");
            throw new BuildException("odd number of characters.");
        }
        writelineStatic("/home/ubuntu/results/coverage/Checksum/Checksum_9_10.coverage", "fff18ddb-f9d5-442e-88c8-988e9e1f2c2e");
        byte[] out = new byte[l >> 1];
        writelineStatic("/home/ubuntu/results/coverage/Checksum/Checksum_9_10.coverage", "f54a6d2c-1308-4afc-b239-160157d0d307");
        // two characters form the hex value.
        for (int i = 0, j = 0; j < l; i++) {
            writelineStatic("/home/ubuntu/results/coverage/Checksum/Checksum_9_10.coverage", "1ed4e02f-630d-4ef3-93a0-75e308f7ed4f");
            int f = Character.digit(data[j++], WORD) << NIBBLE;
            writelineStatic("/home/ubuntu/results/coverage/Checksum/Checksum_9_10.coverage", "58cd8184-24fb-487f-a7fe-3efc8364298b");
            f = f | Character.digit(data[j++], WORD);
            writelineStatic("/home/ubuntu/results/coverage/Checksum/Checksum_9_10.coverage", "182377f5-e2cf-4500-91fb-83ea0710050c");
            out[i] = (byte) (f & BYTE_MASK);
        }
        writelineStatic("/home/ubuntu/results/coverage/Checksum/Checksum_9_10.coverage", "3a2fd45b-4cd3-422e-b480-bf9a0dbb1dc5");
        return out;
    }

    /**
     * reads the checksum from a file using the specified format.
     *
     * @since 1.7
     */
    private String readChecksum(File f) {
        writeline("/home/ubuntu/results/coverage/Checksum/Checksum_9_10.coverage", "b62b2ce9-6686-4f0b-9cd2-b5871a5f678a");
        BufferedReader diskChecksumReader = null;
        writeline("/home/ubuntu/results/coverage/Checksum/Checksum_9_10.coverage", "cb70447b-17f8-4957-83b1-4706693f5acf");
        try {
            writeline("/home/ubuntu/results/coverage/Checksum/Checksum_9_10.coverage", "ad109cb0-133a-44c5-8bc5-4b28b3f7501b");
            diskChecksumReader = new BufferedReader(new FileReader(f));
            writeline("/home/ubuntu/results/coverage/Checksum/Checksum_9_10.coverage", "648b7ab5-ee49-4d66-bdf2-6ae0794677b8");
            Object[] result = format.parse(diskChecksumReader.readLine());
            writeline("/home/ubuntu/results/coverage/Checksum/Checksum_9_10.coverage", "5cf8b1cc-2dc2-4ca5-bb67-4c780dc5b325");
            if (result == null || result.length == 0 || result[0] == null) {
                writeline("/home/ubuntu/results/coverage/Checksum/Checksum_9_10.coverage", "e5301f0a-fd4c-45e1-be7c-eec3c1c4ff48");
                throw new BuildException("failed to find a checksum");
            }
            writeline("/home/ubuntu/results/coverage/Checksum/Checksum_9_10.coverage", "c2ec06d0-5187-4a67-a2db-3fc69061f221");
            return (String) result[0];
        } catch (IOException e) {
            writeline("/home/ubuntu/results/coverage/Checksum/Checksum_9_10.coverage", "e6393ec2-b638-4327-8a1b-c13d6b85aa69");
            throw new BuildException("Couldn't read checksum file " + f, e);
        } catch (ParseException e) {
            writeline("/home/ubuntu/results/coverage/Checksum/Checksum_9_10.coverage", "eb28e0af-3114-422e-81d0-a8b109407899");
            throw new BuildException("Couldn't read checksum file " + f, e);
        } finally {
            writeline("/home/ubuntu/results/coverage/Checksum/Checksum_9_10.coverage", "3430fb6c-fd05-433c-bd32-df2a332b24c2");
            FileUtils.close(diskChecksumReader);
        }
    }

    /**
     * @since Ant 1.8.2
     */
    private String getRelativeFilePath(File f) {
        writeline("/home/ubuntu/results/coverage/Checksum/Checksum_9_10.coverage", "055349bb-662d-419c-9b8e-b3763cc71066");
        String path = (String) relativeFilePaths.get(f);
        writeline("/home/ubuntu/results/coverage/Checksum/Checksum_9_10.coverage", "010c6d34-a19c-44e8-b33f-d0c2a56d8851");
        if (path == null) {
            writeline("/home/ubuntu/results/coverage/Checksum/Checksum_9_10.coverage", "f1f09016-386e-41ab-b496-3e8d015f580c");
            // bug 37386. this should not occur, but it has, once.
            throw new BuildException("Internal error: " + "relativeFilePaths could not match file " + f + "\n" + "please file a bug report on this");
        }
        writeline("/home/ubuntu/results/coverage/Checksum/Checksum_9_10.coverage", "bc4f5a93-adf4-4c88-af86-7cc4c98d61f0");
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
