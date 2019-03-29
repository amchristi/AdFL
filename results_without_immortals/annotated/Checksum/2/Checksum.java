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
        writeline("/home/ubuntu/results/coverage/Checksum/Checksum_2_10.coverage", "8305b1e0-dc85-4f87-bf5e-1ad0fc6bfe20");
        this.file = file;
    }

    /**
     * Sets the root directory where checksum files will be
     * written/read
     * @param todir the directory to write to
     * @since Ant 1.6
     */
    public void setTodir(File todir) {
        writeline("/home/ubuntu/results/coverage/Checksum/Checksum_2_10.coverage", "894ab94c-552c-48b5-aa08-cf11e9c138b4");
        this.todir = todir;
    }

    /**
     * Specifies the algorithm to be used to compute the checksum.
     * Defaults to "MD5". Other popular algorithms like "SHA" may be used as well.
     * @param algorithm a <code>String</code> value
     */
    public void setAlgorithm(String algorithm) {
        writeline("/home/ubuntu/results/coverage/Checksum/Checksum_2_10.coverage", "434e122c-3bb2-4bdf-ade0-3cca14933fdc");
        this.algorithm = algorithm;
    }

    /**
     * Sets the MessageDigest algorithm provider to be used
     * to calculate the checksum.
     * @param provider a <code>String</code> value
     */
    public void setProvider(String provider) {
        writeline("/home/ubuntu/results/coverage/Checksum/Checksum_2_10.coverage", "8db3b0cb-7557-4b37-a0a3-4eef8f8cc197");
        this.provider = provider;
    }

    /**
     * Sets the file extension that is be to used to
     * create or identify destination file.
     * @param fileext a <code>String</code> value
     */
    public void setFileext(String fileext) {
        writeline("/home/ubuntu/results/coverage/Checksum/Checksum_2_10.coverage", "a45ca836-d603-4879-8c0f-abe0fd2d381a");
        this.fileext = fileext;
    }

    /**
     * Sets the property to hold the generated checksum.
     * @param property a <code>String</code> value
     */
    public void setProperty(String property) {
        writeline("/home/ubuntu/results/coverage/Checksum/Checksum_2_10.coverage", "d69a6f8c-3c88-4b87-922e-293422491d78");
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
        writeline("/home/ubuntu/results/coverage/Checksum/Checksum_2_10.coverage", "d3e2f7be-8c39-423c-bb5c-978309eb5fc2");
        this.totalproperty = totalproperty;
    }

    /**
     * Sets the verify property.  This project property holds
     * the result of a checksum verification - "true" or "false"
     * @param verifyProperty a <code>String</code> value
     */
    public void setVerifyproperty(String verifyProperty) {
        writeline("/home/ubuntu/results/coverage/Checksum/Checksum_2_10.coverage", "d37f22f3-de9c-4ea4-8c02-0ce44332fd09");
        this.verifyProperty = verifyProperty;
    }

    /**
     * Whether or not to overwrite existing file irrespective of
     * whether it is newer than
     * the source file.  Defaults to false.
     * @param forceOverwrite a <code>boolean</code> value
     */
    public void setForceOverwrite(boolean forceOverwrite) {
        writeline("/home/ubuntu/results/coverage/Checksum/Checksum_2_10.coverage", "15005dba-21d4-4f7a-91d1-2941ae2943db");
        this.forceOverwrite = forceOverwrite;
    }

    /**
     * The size of the read buffer to use.
     * @param size an <code>int</code> value
     */
    public void setReadBufferSize(int size) {
        writeline("/home/ubuntu/results/coverage/Checksum/Checksum_2_10.coverage", "137ab0ef-5691-4fb1-8ebc-f15151e10389");
        this.readBufferSize = size;
    }

    /**
     * Select the in/output pattern via a well know format name.
     * @param e an <code>enumerated</code> value
     *
     * @since 1.7.0
     */
    public void setFormat(FormatElement e) {
        writeline("/home/ubuntu/results/coverage/Checksum/Checksum_2_10.coverage", "78b0031c-20e6-43b9-898f-946e1d55cac7");
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
        writeline("/home/ubuntu/results/coverage/Checksum/Checksum_2_10.coverage", "821132e0-5312-4a28-a620-a64a96c61a6c");
        format = new MessageFormat(p);
    }

    /**
     * Files to generate checksums for.
     * @param set a fileset of files to generate checksums for.
     */
    public void addFileset(FileSet set) {
        writeline("/home/ubuntu/results/coverage/Checksum/Checksum_2_10.coverage", "d78cc820-3055-463d-a286-8b1a989ab9cb");
        add(set);
    }

    /**
     * Add a resource collection.
     * @param rc the ResourceCollection to add.
     */
    public void add(ResourceCollection rc) {
        writeline("/home/ubuntu/results/coverage/Checksum/Checksum_2_10.coverage", "896ad1f5-5290-496e-8a06-412919d66d27");
        if (rc == null) {
            writeline("/home/ubuntu/results/coverage/Checksum/Checksum_2_10.coverage", "7d807a00-660e-4161-9104-f968bb96798a");
            return;
        }
        writeline("/home/ubuntu/results/coverage/Checksum/Checksum_2_10.coverage", "28c985f1-4e27-47e9-b0ab-af6929fa181c");
        resources = (resources == null) ? new FileUnion() : resources;
        writeline("/home/ubuntu/results/coverage/Checksum/Checksum_2_10.coverage", "a4711dda-d0f7-422f-9e6f-035c8135d0d0");
        resources.add(rc);
    }

    /**
     * Calculate the checksum(s).
     * @throws BuildException on error
     */
    public void execute() throws BuildException {
        writeline("/home/ubuntu/results/coverage/Checksum/Checksum_2_10.coverage", "1ee66068-055f-4d2c-b720-379c0657fbf1");
        isCondition = false;
        writeline("/home/ubuntu/results/coverage/Checksum/Checksum_2_10.coverage", "79141c4e-611f-479c-8fba-5c22743becbe");
        boolean value = validateAndExecute();
        writeline("/home/ubuntu/results/coverage/Checksum/Checksum_2_10.coverage", "5010448b-4d1d-4651-8110-ea28089f4cbd");
        if (verifyProperty != null) {
            writeline("/home/ubuntu/results/coverage/Checksum/Checksum_2_10.coverage", "02584079-7044-477a-a570-38e62394617b");
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
        writeline("/home/ubuntu/results/coverage/Checksum/Checksum_2_10.coverage", "bbf03af3-8791-41a4-ae51-b191ef4e80ed");
        isCondition = true;
        writeline("/home/ubuntu/results/coverage/Checksum/Checksum_2_10.coverage", "c023471c-b08c-4cf4-a9c9-5d0ffd04aeb2");
        return validateAndExecute();
    }

    /**
     * Validate attributes and get down to business.
     */
    private boolean validateAndExecute() throws BuildException {
        writeline("/home/ubuntu/results/coverage/Checksum/Checksum_2_10.coverage", "f8aeb535-e8a4-4077-b1eb-33637d396d01");
        String savedFileExt = fileext;
        writeline("/home/ubuntu/results/coverage/Checksum/Checksum_2_10.coverage", "211e6823-ec2d-4025-9faf-8cb447eebdd6");
        if (file == null && (resources == null || resources.size() == 0)) {
            writeline("/home/ubuntu/results/coverage/Checksum/Checksum_2_10.coverage", "5d7b2131-a639-4595-b0b1-e01d65442afe");
            throw new BuildException("Specify at least one source - a file or a resource collection.");
        }
        writeline("/home/ubuntu/results/coverage/Checksum/Checksum_2_10.coverage", "42f27946-228c-4cb8-bb51-cb1d5469ae35");
        if (!(resources == null || resources.isFilesystemOnly())) {
            writeline("/home/ubuntu/results/coverage/Checksum/Checksum_2_10.coverage", "ef4d7618-692c-435d-9861-d329a96f8662");
            throw new BuildException("Can only calculate checksums for file-based resources.");
        }
        writeline("/home/ubuntu/results/coverage/Checksum/Checksum_2_10.coverage", "0ce6c818-2633-4c01-8a07-def28b1002dc");
        if (file != null && file.exists() && file.isDirectory()) {
            writeline("/home/ubuntu/results/coverage/Checksum/Checksum_2_10.coverage", "7aa34d7a-3a43-4885-b383-45ca61e73fae");
            throw new BuildException("Checksum cannot be generated for directories");
        }
        writeline("/home/ubuntu/results/coverage/Checksum/Checksum_2_10.coverage", "1e9040b1-3095-46b0-bd3d-7e4ee44422f6");
        if (file != null && totalproperty != null) {
            writeline("/home/ubuntu/results/coverage/Checksum/Checksum_2_10.coverage", "faafa96f-dacb-4b63-b4f2-7a8e843d4121");
            throw new BuildException("File and Totalproperty cannot co-exist.");
        }
        writeline("/home/ubuntu/results/coverage/Checksum/Checksum_2_10.coverage", "059865a1-c7d0-45cd-b21d-c57a0de30826");
        if (property != null && fileext != null) {
            writeline("/home/ubuntu/results/coverage/Checksum/Checksum_2_10.coverage", "b619d72f-7d7c-455b-90b7-926c36469b4a");
            throw new BuildException("Property and FileExt cannot co-exist.");
        }
        writeline("/home/ubuntu/results/coverage/Checksum/Checksum_2_10.coverage", "bfa8bb36-d444-4ee2-bc71-03394ae1ab10");
        if (property != null) {
            writeline("/home/ubuntu/results/coverage/Checksum/Checksum_2_10.coverage", "92c53771-c48d-4804-969d-f922dfaa1321");
            if (forceOverwrite) {
                writeline("/home/ubuntu/results/coverage/Checksum/Checksum_2_10.coverage", "9241126b-99ca-4a61-a9c8-01a03f2ff364");
                throw new BuildException("ForceOverwrite cannot be used when Property is specified");
            }
            writeline("/home/ubuntu/results/coverage/Checksum/Checksum_2_10.coverage", "fdfcf2f5-91e8-4f9c-99ca-4b709c2cee54");
            int ct = 0;
            writeline("/home/ubuntu/results/coverage/Checksum/Checksum_2_10.coverage", "c4625cd1-eb97-4c95-a9d0-58b693de0333");
            if (resources != null) {
                writeline("/home/ubuntu/results/coverage/Checksum/Checksum_2_10.coverage", "2c1a4bc5-ba48-49d9-8d96-da29914c43dd");
                ct += resources.size();
            }
            writeline("/home/ubuntu/results/coverage/Checksum/Checksum_2_10.coverage", "454095c2-8155-4995-a931-7c7bc94fbc67");
            if (file != null) {
                writeline("/home/ubuntu/results/coverage/Checksum/Checksum_2_10.coverage", "2dffecd1-7f9e-4e8b-9c96-251ae7c39751");
                ct++;
            }
            writeline("/home/ubuntu/results/coverage/Checksum/Checksum_2_10.coverage", "b5c48b5c-20b6-44c3-b515-69c59524f5ec");
            if (ct > 1) {
                writeline("/home/ubuntu/results/coverage/Checksum/Checksum_2_10.coverage", "5dbe713c-fe5b-4400-99ab-e9a3e4ae73b8");
                throw new BuildException("Multiple files cannot be used when Property is specified");
            }
        }
        writeline("/home/ubuntu/results/coverage/Checksum/Checksum_2_10.coverage", "42488a23-9452-4c0c-92b0-e6dac37604b7");
        if (verifyProperty != null) {
            writeline("/home/ubuntu/results/coverage/Checksum/Checksum_2_10.coverage", "9b50d618-bc58-4b5d-a7f9-8007983700e5");
            isCondition = true;
        }
        writeline("/home/ubuntu/results/coverage/Checksum/Checksum_2_10.coverage", "334cabda-35e8-4278-a658-0d58a86ec9df");
        if (verifyProperty != null && forceOverwrite) {
            writeline("/home/ubuntu/results/coverage/Checksum/Checksum_2_10.coverage", "b77bc379-6d6d-4b76-9061-19cad0c52ddc");
            throw new BuildException("VerifyProperty and ForceOverwrite cannot co-exist.");
        }
        writeline("/home/ubuntu/results/coverage/Checksum/Checksum_2_10.coverage", "a9102382-4d3c-4318-8118-9b5247f1d7ac");
        if (isCondition && forceOverwrite) {
            writeline("/home/ubuntu/results/coverage/Checksum/Checksum_2_10.coverage", "d3ec9c43-054d-4f85-96b7-0d671f849545");
            throw new BuildException("ForceOverwrite cannot be used when conditions are being used.");
        }
        writeline("/home/ubuntu/results/coverage/Checksum/Checksum_2_10.coverage", "09e48502-6574-4d2d-9aed-b26f91068f9e");
        messageDigest = null;
        writeline("/home/ubuntu/results/coverage/Checksum/Checksum_2_10.coverage", "097b708f-79b1-41c7-af94-0a2c901fb113");
        if (provider != null) {
            writeline("/home/ubuntu/results/coverage/Checksum/Checksum_2_10.coverage", "5526417d-d67d-49b4-bc2a-1281ddc8e785");
            try {
                writeline("/home/ubuntu/results/coverage/Checksum/Checksum_2_10.coverage", "2d511492-03b2-41a8-ad88-8e3ce2030a7c");
                messageDigest = MessageDigest.getInstance(algorithm, provider);
            } catch (NoSuchAlgorithmException noalgo) {
                writeline("/home/ubuntu/results/coverage/Checksum/Checksum_2_10.coverage", "c1bd9956-5c3a-406d-ac7f-0992755e5cd7");
                throw new BuildException(noalgo, getLocation());
            } catch (NoSuchProviderException noprovider) {
                writeline("/home/ubuntu/results/coverage/Checksum/Checksum_2_10.coverage", "8693fba8-f3b0-48d9-b9e3-0d7e1b954fa7");
                throw new BuildException(noprovider, getLocation());
            }
        } else {
            writeline("/home/ubuntu/results/coverage/Checksum/Checksum_2_10.coverage", "120d6847-e515-4835-b55a-227d01a9c51b");
            try {
                writeline("/home/ubuntu/results/coverage/Checksum/Checksum_2_10.coverage", "e7a0573f-2066-4e53-b7c3-b13df2847386");
                messageDigest = MessageDigest.getInstance(algorithm);
            } catch (NoSuchAlgorithmException noalgo) {
                writeline("/home/ubuntu/results/coverage/Checksum/Checksum_2_10.coverage", "a09f046f-af89-4b4d-9a05-d7cb68485d1c");
                throw new BuildException(noalgo, getLocation());
            }
        }
        writeline("/home/ubuntu/results/coverage/Checksum/Checksum_2_10.coverage", "f8bd6b3e-b9ef-4ce7-8538-66dc7a1390e4");
        if (messageDigest == null) {
            writeline("/home/ubuntu/results/coverage/Checksum/Checksum_2_10.coverage", "f384b50c-5162-45e8-8237-93f1e735b97e");
            throw new BuildException("Unable to create Message Digest", getLocation());
        }
        writeline("/home/ubuntu/results/coverage/Checksum/Checksum_2_10.coverage", "0babd248-2d30-4650-aaf5-4be0e3c34952");
        if (fileext == null) {
            writeline("/home/ubuntu/results/coverage/Checksum/Checksum_2_10.coverage", "55104d83-639a-4586-98c4-7415f70efd1d");
            fileext = "." + algorithm;
        } else if (fileext.trim().length() == 0) {
            writeline("/home/ubuntu/results/coverage/Checksum/Checksum_2_10.coverage", "f36d4cc2-5103-42b0-95ec-b33f30f13e87");
            throw new BuildException("File extension when specified must not be an empty string");
        }
        writeline("/home/ubuntu/results/coverage/Checksum/Checksum_2_10.coverage", "4b32377b-80c1-4502-a240-c9c50f803bcf");
        try {
            writeline("/home/ubuntu/results/coverage/Checksum/Checksum_2_10.coverage", "f69b529f-7480-4798-a45f-64f0ddba21b4");
            if (resources != null) {
                writeline("/home/ubuntu/results/coverage/Checksum/Checksum_2_10.coverage", "de8e5bcc-fa56-4b12-ab66-c55ec23b1af7");
                for (Resource r : resources) {
                    writeline("/home/ubuntu/results/coverage/Checksum/Checksum_2_10.coverage", "d7c97a2d-6753-4f73-baa0-64813b1e5739");
                    File src = r.as(FileProvider.class).getFile();
                    writeline("/home/ubuntu/results/coverage/Checksum/Checksum_2_10.coverage", "144f4306-609e-49f9-8484-6b8b0c2868fc");
                    if (totalproperty != null || todir != null) {
                        writeline("/home/ubuntu/results/coverage/Checksum/Checksum_2_10.coverage", "37727a37-d88e-4e3c-96d7-c97259bb2767");
                        // Use '/' to calculate digest based on file name.
                        // This is required in order to get the same result
                        // on different platforms.
                        relativeFilePaths.put(src, r.getName().replace(File.separatorChar, '/'));
                    }
                    writeline("/home/ubuntu/results/coverage/Checksum/Checksum_2_10.coverage", "c7cdc9a4-0a95-407b-80e6-1144bf30e7cf");
                    addToIncludeFileMap(src);
                }
            }
            writeline("/home/ubuntu/results/coverage/Checksum/Checksum_2_10.coverage", "930c34f2-03b7-40d4-8c7d-41a41a25a98c");
            if (file != null) {
                writeline("/home/ubuntu/results/coverage/Checksum/Checksum_2_10.coverage", "6f760c4e-5d55-4f05-b315-fb48b88c3db6");
                if (totalproperty != null || todir != null) {
                    writeline("/home/ubuntu/results/coverage/Checksum/Checksum_2_10.coverage", "b4822559-38fa-43fb-b3c0-b7cbda1189ea");
                    relativeFilePaths.put(file, file.getName().replace(File.separatorChar, '/'));
                }
                writeline("/home/ubuntu/results/coverage/Checksum/Checksum_2_10.coverage", "20833945-594e-4fc6-989c-dd81213919ce");
                addToIncludeFileMap(file);
            }
            writeline("/home/ubuntu/results/coverage/Checksum/Checksum_2_10.coverage", "975ea749-b40a-4a16-81da-49363c0bcb70");
            return generateChecksums();
        } finally {
            writeline("/home/ubuntu/results/coverage/Checksum/Checksum_2_10.coverage", "3cbeadce-2a38-42d9-9b8c-703e6a47a70e");
            fileext = savedFileExt;
            writeline("/home/ubuntu/results/coverage/Checksum/Checksum_2_10.coverage", "8707af69-fcef-4a63-9f56-d7e41554d19b");
            includeFileMap.clear();
        }
    }

    /**
     * Add key-value pair to the hashtable upon which
     * to later operate upon.
     */
    private void addToIncludeFileMap(File file) throws BuildException {
        writeline("/home/ubuntu/results/coverage/Checksum/Checksum_2_10.coverage", "09b66a0b-39dd-4d76-8faa-c75022f3e6ad");
        if (file.exists()) {
            writeline("/home/ubuntu/results/coverage/Checksum/Checksum_2_10.coverage", "e27df615-48e1-4340-9281-c35349d8f152");
            if (property == null) {
                writeline("/home/ubuntu/results/coverage/Checksum/Checksum_2_10.coverage", "fd59e442-4009-48b0-b87c-6359fa41bf83");
                File checksumFile = getChecksumFile(file);
                writeline("/home/ubuntu/results/coverage/Checksum/Checksum_2_10.coverage", "c7fd802a-d242-40b3-beef-676de54a4229");
                if (forceOverwrite || isCondition || (file.lastModified() > checksumFile.lastModified())) {
                    writeline("/home/ubuntu/results/coverage/Checksum/Checksum_2_10.coverage", "5268a5b5-089e-420a-84ef-045430f117ef");
                    includeFileMap.put(file, checksumFile);
                } else {
                    writeline("/home/ubuntu/results/coverage/Checksum/Checksum_2_10.coverage", "d7217b8c-a0bc-4eb4-8f82-5c54b34d5b8b");
                    log(file + " omitted as " + checksumFile + " is up to date.", Project.MSG_VERBOSE);
                    writeline("/home/ubuntu/results/coverage/Checksum/Checksum_2_10.coverage", "d56a05f6-9add-4eb0-a867-66f4d17f5765");
                    if (totalproperty != null) {
                        writeline("/home/ubuntu/results/coverage/Checksum/Checksum_2_10.coverage", "d808d773-1e85-4e0c-a097-aa3f803b1d2b");
                        // Read the checksum from disk.
                        String checksum = readChecksum(checksumFile);
                        writeline("/home/ubuntu/results/coverage/Checksum/Checksum_2_10.coverage", "90c958ce-4f42-4f3e-a581-ecd1d62fd657");
                        byte[] digest = decodeHex(checksum.toCharArray());
                        writeline("/home/ubuntu/results/coverage/Checksum/Checksum_2_10.coverage", "d5a908cd-0cb3-45bc-9c81-e281fe40e974");
                        allDigests.put(file, digest);
                    }
                }
            } else {
                writeline("/home/ubuntu/results/coverage/Checksum/Checksum_2_10.coverage", "9e5479bb-6caf-474e-a15b-8e7498e587ff");
                includeFileMap.put(file, property);
            }
        } else {
            writeline("/home/ubuntu/results/coverage/Checksum/Checksum_2_10.coverage", "5a08f678-ce20-41b7-be67-8929f1928ee7");
            String message = "Could not find file " + file.getAbsolutePath() + " to generate checksum for.";
            writeline("/home/ubuntu/results/coverage/Checksum/Checksum_2_10.coverage", "04e03503-a133-4f00-9304-b4346c6d7a13");
            log(message);
            writeline("/home/ubuntu/results/coverage/Checksum/Checksum_2_10.coverage", "b25b3efa-4d42-4e0c-b336-8e123fbd2925");
            throw new BuildException(message, getLocation());
        }
    }

    private File getChecksumFile(File file) {
        writeline("/home/ubuntu/results/coverage/Checksum/Checksum_2_10.coverage", "2e5dc4f3-4065-4bc5-b854-ea2d7edeca79");
        File directory;
        writeline("/home/ubuntu/results/coverage/Checksum/Checksum_2_10.coverage", "37c5f3d9-9d5f-4fac-b040-2d74fc76374e");
        if (todir != null) {
            writeline("/home/ubuntu/results/coverage/Checksum/Checksum_2_10.coverage", "20ef8ce0-84c0-4494-bfa2-6b23e0c5319b");
            // A separate directory was explicitly declared
            String path = getRelativeFilePath(file);
            writeline("/home/ubuntu/results/coverage/Checksum/Checksum_2_10.coverage", "81dd19fd-b8be-46f0-bdef-15aab4d7b956");
            directory = new File(todir, path).getParentFile();
            writeline("/home/ubuntu/results/coverage/Checksum/Checksum_2_10.coverage", "2c699030-f81f-4e32-9ef7-6c606736780e");
            // Create the directory, as it might not exist.
            directory.mkdirs();
        } else {
            writeline("/home/ubuntu/results/coverage/Checksum/Checksum_2_10.coverage", "1b78a78c-43f9-4e34-8054-01213485aee0");
            // Just use the same directory as the file itself.
            // This directory will exist
            directory = file.getParentFile();
        }
        writeline("/home/ubuntu/results/coverage/Checksum/Checksum_2_10.coverage", "a3f744ed-282c-4cf5-860b-948b479f6c99");
        File checksumFile = new File(directory, file.getName() + fileext);
        writeline("/home/ubuntu/results/coverage/Checksum/Checksum_2_10.coverage", "3dce197e-eeb4-4703-af52-a9427ae43208");
        return checksumFile;
    }

    /**
     * Generate checksum(s) using the message digest created earlier.
     */
    private boolean generateChecksums() throws BuildException {
        writeline("/home/ubuntu/results/coverage/Checksum/Checksum_2_10.coverage", "180c05e9-919f-4e16-b726-5e880781e591");
        boolean checksumMatches = true;
        writeline("/home/ubuntu/results/coverage/Checksum/Checksum_2_10.coverage", "98b89a37-9418-494b-888c-5658dde48387");
        FileInputStream fis = null;
        writeline("/home/ubuntu/results/coverage/Checksum/Checksum_2_10.coverage", "0e6dc5b3-0bde-4860-81f8-4e5aa2d9169e");
        FileOutputStream fos = null;
        writeline("/home/ubuntu/results/coverage/Checksum/Checksum_2_10.coverage", "eebebc37-b93a-4ad3-a0e2-b8ca104213cc");
        byte[] buf = new byte[readBufferSize];
        writeline("/home/ubuntu/results/coverage/Checksum/Checksum_2_10.coverage", "43b1ee5f-6eab-4780-b7d7-c9e7e842e539");
        try {
            writeline("/home/ubuntu/results/coverage/Checksum/Checksum_2_10.coverage", "1b403a9e-3ee9-4c5e-8ccd-30e02f31f170");
            for (Map.Entry<File, Object> e : includeFileMap.entrySet()) {
                writeline("/home/ubuntu/results/coverage/Checksum/Checksum_2_10.coverage", "a51b634d-d84b-4f0e-933f-349a893b5143");
                messageDigest.reset();
                writeline("/home/ubuntu/results/coverage/Checksum/Checksum_2_10.coverage", "e16ab218-6f36-4504-ab75-0c8f6c0a82ca");
                File src = e.getKey();
                writeline("/home/ubuntu/results/coverage/Checksum/Checksum_2_10.coverage", "822cbace-fad0-4d46-b497-1477d958b202");
                if (!isCondition) {
                    writeline("/home/ubuntu/results/coverage/Checksum/Checksum_2_10.coverage", "16b82427-941d-4ee6-9f46-a2b355a8f914");
                    log("Calculating " + algorithm + " checksum for " + src, Project.MSG_VERBOSE);
                }
                writeline("/home/ubuntu/results/coverage/Checksum/Checksum_2_10.coverage", "b194f40c-77fd-4cfd-96cf-0d25bd4b921e");
                fis = new FileInputStream(src);
                writeline("/home/ubuntu/results/coverage/Checksum/Checksum_2_10.coverage", "ed8e4a95-e57e-42b8-9530-2f57332a2913");
                DigestInputStream dis = new DigestInputStream(fis, messageDigest);
                writeline("/home/ubuntu/results/coverage/Checksum/Checksum_2_10.coverage", "5be1a498-2ba1-471d-b1b1-d0aa65f971b8");
                while (dis.read(buf, 0, readBufferSize) != -1) {
                // Empty statement
                }
                writeline("/home/ubuntu/results/coverage/Checksum/Checksum_2_10.coverage", "1b0d69d1-4a98-4e0d-9054-2fe2d767f2e0");
                dis.close();
                writeline("/home/ubuntu/results/coverage/Checksum/Checksum_2_10.coverage", "ed0e4387-0a19-4592-9983-09e934370ee0");
                fis.close();
                writeline("/home/ubuntu/results/coverage/Checksum/Checksum_2_10.coverage", "c21c1286-98ef-4af0-a752-8d91218ed5ce");
                fis = null;
                writeline("/home/ubuntu/results/coverage/Checksum/Checksum_2_10.coverage", "f05950c7-488f-4998-a816-a7cfd4cfc699");
                byte[] fileDigest = messageDigest.digest();
                writeline("/home/ubuntu/results/coverage/Checksum/Checksum_2_10.coverage", "6a27cfe5-a953-4312-8048-c7b8668ad6cb");
                if (totalproperty != null) {
                    writeline("/home/ubuntu/results/coverage/Checksum/Checksum_2_10.coverage", "39190e43-13e1-44fb-8d8d-0ba479df7d37");
                    allDigests.put(src, fileDigest);
                }
                writeline("/home/ubuntu/results/coverage/Checksum/Checksum_2_10.coverage", "752479ca-fadf-4141-b316-fd0fa04ec6d2");
                String checksum = createDigestString(fileDigest);
                writeline("/home/ubuntu/results/coverage/Checksum/Checksum_2_10.coverage", "5cbbde5b-8d83-4a4b-a39a-544d20dfe11b");
                // can either be a property name string or a file
                Object destination = e.getValue();
                writeline("/home/ubuntu/results/coverage/Checksum/Checksum_2_10.coverage", "8fb0d093-fc37-4fd8-9078-9fdb79fe9cb3");
                if (destination instanceof java.lang.String) {
                    writeline("/home/ubuntu/results/coverage/Checksum/Checksum_2_10.coverage", "15e6bb8e-c9d8-4cc3-b41a-ec7694b2c2fd");
                    String prop = (String) destination;
                    writeline("/home/ubuntu/results/coverage/Checksum/Checksum_2_10.coverage", "a319a069-efa2-4733-9203-9de2ea1cad69");
                    if (isCondition) {
                        writeline("/home/ubuntu/results/coverage/Checksum/Checksum_2_10.coverage", "c7710d79-e2b4-4946-beb8-eede4a204854");
                        checksumMatches = checksumMatches && checksum.equals(property);
                    } else {
                        writeline("/home/ubuntu/results/coverage/Checksum/Checksum_2_10.coverage", "ad8d2b61-5f84-4c72-96f5-e24f8a12932f");
                        getProject().setNewProperty(prop, checksum);
                    }
                } else if (destination instanceof java.io.File) {
                    writeline("/home/ubuntu/results/coverage/Checksum/Checksum_2_10.coverage", "eaf03138-74d2-4f0d-b45a-4ec7659f4c43");
                    if (isCondition) {
                        writeline("/home/ubuntu/results/coverage/Checksum/Checksum_2_10.coverage", "a69f1146-bc56-46ef-ab04-b588f8af792b");
                        File existingFile = (File) destination;
                        writeline("/home/ubuntu/results/coverage/Checksum/Checksum_2_10.coverage", "4bfaa452-8f0f-4ba8-a11e-4c795209a878");
                        if (existingFile.exists()) {
                            writeline("/home/ubuntu/results/coverage/Checksum/Checksum_2_10.coverage", "daefceb9-f109-4ddc-8e66-0bb8e52ff866");
                            try {
                                writeline("/home/ubuntu/results/coverage/Checksum/Checksum_2_10.coverage", "b43aa919-ffdb-4add-a7c1-d63ed17e2c59");
                                String suppliedChecksum = readChecksum(existingFile);
                                writeline("/home/ubuntu/results/coverage/Checksum/Checksum_2_10.coverage", "f5c0c29d-aaf7-426e-b6fa-5cdb01a9e7ad");
                                checksumMatches = checksumMatches && checksum.equals(suppliedChecksum);
                            } catch (BuildException be) {
                                writeline("/home/ubuntu/results/coverage/Checksum/Checksum_2_10.coverage", "1cf280a6-bca7-4fb8-b035-e86e9709f103");
                                // file is on wrong format, swallow
                                checksumMatches = false;
                            }
                        } else {
                            writeline("/home/ubuntu/results/coverage/Checksum/Checksum_2_10.coverage", "036131cf-ae2a-4938-8e17-4ea12899ed75");
                            checksumMatches = false;
                        }
                    } else {
                        writeline("/home/ubuntu/results/coverage/Checksum/Checksum_2_10.coverage", "39f6395d-3615-4673-a48c-48591d5dae23");
                        File dest = (File) destination;
                        writeline("/home/ubuntu/results/coverage/Checksum/Checksum_2_10.coverage", "50662feb-8a92-4b06-a636-0ae0198401d1");
                        fos = new FileOutputStream(dest);
                        writeline("/home/ubuntu/results/coverage/Checksum/Checksum_2_10.coverage", "0862a0a3-0a64-4073-b92c-0defa77cd8d3");
                        fos.write(format.format(new Object[] { checksum, src.getName(), FileUtils.getRelativePath(dest.getParentFile(), src), FileUtils.getRelativePath(getProject().getBaseDir(), src), src.getAbsolutePath() }).getBytes());
                        writeline("/home/ubuntu/results/coverage/Checksum/Checksum_2_10.coverage", "551ba006-9834-4b0c-a4b7-f9064e187917");
                        fos.write(StringUtils.LINE_SEP.getBytes());
                        writeline("/home/ubuntu/results/coverage/Checksum/Checksum_2_10.coverage", "090e7d9a-d359-4940-8531-5a1c6c788bc8");
                        fos.close();
                        writeline("/home/ubuntu/results/coverage/Checksum/Checksum_2_10.coverage", "987bd3ba-1a46-4c34-bdfb-5e586b830e1b");
                        fos = null;
                    }
                }
            }
            writeline("/home/ubuntu/results/coverage/Checksum/Checksum_2_10.coverage", "c66169de-5401-40b7-8b4d-4c974d69f3fb");
            if (totalproperty != null) {
                writeline("/home/ubuntu/results/coverage/Checksum/Checksum_2_10.coverage", "bae7e6b2-3d04-4046-880b-ad19c21f2913");
                // Calculate the total checksum
                // Convert the keys (source files) into a sorted array.
                File[] keyArray = allDigests.keySet().toArray(new File[allDigests.size()]);
                writeline("/home/ubuntu/results/coverage/Checksum/Checksum_2_10.coverage", "d72e5d60-3ce0-4699-a03a-95514623abf7");
                // File is Comparable, but sort-order is platform
                // dependent (case-insensitive on Windows)
                Arrays.sort(keyArray, new Comparator<File>() {

                    public int compare(File f1, File f2) {
                        return f1 == null ? (f2 == null ? 0 : -1) : (f2 == null ? 1 : getRelativeFilePath(f1).compareTo(getRelativeFilePath(f2)));
                    }
                });
                writeline("/home/ubuntu/results/coverage/Checksum/Checksum_2_10.coverage", "1711250b-b0f8-45bc-8026-7c9dbb866433");
                // Loop over the checksums and generate a total hash.
                messageDigest.reset();
                writeline("/home/ubuntu/results/coverage/Checksum/Checksum_2_10.coverage", "fd25e905-b4a4-4ae2-a339-8e9a6375cf21");
                for (File src : keyArray) {
                    writeline("/home/ubuntu/results/coverage/Checksum/Checksum_2_10.coverage", "bfa7976e-bb8b-4e2b-9e33-5b1bf4d2e3a7");
                    // Add the digest for the file content
                    byte[] digest = allDigests.get(src);
                    writeline("/home/ubuntu/results/coverage/Checksum/Checksum_2_10.coverage", "24a0b24e-5355-4733-aff4-c90ef7b544db");
                    messageDigest.update(digest);
                    writeline("/home/ubuntu/results/coverage/Checksum/Checksum_2_10.coverage", "dc6de95f-d852-4f6b-a184-5da60de18b00");
                    // Add the file path
                    String fileName = getRelativeFilePath(src);
                    writeline("/home/ubuntu/results/coverage/Checksum/Checksum_2_10.coverage", "edfd8731-d202-4ee7-9f54-f0a6f9f26377");
                    messageDigest.update(fileName.getBytes());
                }
                writeline("/home/ubuntu/results/coverage/Checksum/Checksum_2_10.coverage", "c6c2e5b5-335e-49c1-a756-0a250ad5a9ae");
                String totalChecksum = createDigestString(messageDigest.digest());
                writeline("/home/ubuntu/results/coverage/Checksum/Checksum_2_10.coverage", "5f9e3d71-eda9-4be4-8aa7-9960b9b93f37");
                getProject().setNewProperty(totalproperty, totalChecksum);
            }
        } catch (Exception e) {
            writeline("/home/ubuntu/results/coverage/Checksum/Checksum_2_10.coverage", "708e4362-8fc0-4422-92f2-c3f31d1372e0");
            throw new BuildException(e, getLocation());
        } finally {
            writeline("/home/ubuntu/results/coverage/Checksum/Checksum_2_10.coverage", "252d19b5-fd64-4f39-bca4-b80490d85d49");
            FileUtils.close(fis);
            writeline("/home/ubuntu/results/coverage/Checksum/Checksum_2_10.coverage", "edbfbcce-ba7c-492f-ab7c-ddc0ccceef41");
            FileUtils.close(fos);
        }
        writeline("/home/ubuntu/results/coverage/Checksum/Checksum_2_10.coverage", "1d7c6c15-0732-40fc-9fde-9ac92d9c680c");
        return checksumMatches;
    }

    private String createDigestString(byte[] fileDigest) {
        writeline("/home/ubuntu/results/coverage/Checksum/Checksum_2_10.coverage", "33e42bc6-a9b3-4f47-99e6-e19156b2d314");
        StringBuffer checksumSb = new StringBuffer();
        writeline("/home/ubuntu/results/coverage/Checksum/Checksum_2_10.coverage", "4d52a3e0-cf10-4a37-bd61-c07915e5fdb8");
        for (int i = 0; i < fileDigest.length; i++) {
            writeline("/home/ubuntu/results/coverage/Checksum/Checksum_2_10.coverage", "d21a2821-ed80-4a46-ae3d-0680c12edffc");
            String hexStr = Integer.toHexString(BYTE_MASK & fileDigest[i]);
            writeline("/home/ubuntu/results/coverage/Checksum/Checksum_2_10.coverage", "5159705f-b71f-40d9-bd53-493f4c4f7345");
            if (hexStr.length() < 2) {
                writeline("/home/ubuntu/results/coverage/Checksum/Checksum_2_10.coverage", "f655f09a-9724-4f74-9801-06d8ec8e179d");
                checksumSb.append("0");
            }
            writeline("/home/ubuntu/results/coverage/Checksum/Checksum_2_10.coverage", "44452ee0-aa21-4207-9231-d0a48f406eb9");
            checksumSb.append(hexStr);
        }
        writeline("/home/ubuntu/results/coverage/Checksum/Checksum_2_10.coverage", "a9da7063-030c-475f-b40b-6f73091a3e51");
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
        writelineStatic("/home/ubuntu/results/coverage/Checksum/Checksum_2_10.coverage", "1e42209b-1fa5-4019-a55b-f7c01d1f0689");
        int l = data.length;
        writelineStatic("/home/ubuntu/results/coverage/Checksum/Checksum_2_10.coverage", "a7e110a3-887f-44ea-80bc-a2606a42068f");
        if ((l & 0x01) != 0) {
            writelineStatic("/home/ubuntu/results/coverage/Checksum/Checksum_2_10.coverage", "2265d651-3bcf-453a-8066-56b303d83365");
            throw new BuildException("odd number of characters.");
        }
        writelineStatic("/home/ubuntu/results/coverage/Checksum/Checksum_2_10.coverage", "5d39f875-c8be-4e08-9c9c-eac609afe606");
        byte[] out = new byte[l >> 1];
        writelineStatic("/home/ubuntu/results/coverage/Checksum/Checksum_2_10.coverage", "cca83531-8bc6-495f-ab45-997a53024b7f");
        // two characters form the hex value.
        for (int i = 0, j = 0; j < l; i++) {
            writelineStatic("/home/ubuntu/results/coverage/Checksum/Checksum_2_10.coverage", "eb926939-4d14-4ec5-ad38-3281b6febc77");
            int f = Character.digit(data[j++], WORD) << NIBBLE;
            writelineStatic("/home/ubuntu/results/coverage/Checksum/Checksum_2_10.coverage", "21f4f4c9-0eb0-4e68-b642-3d3336ce41e1");
            f = f | Character.digit(data[j++], WORD);
            writelineStatic("/home/ubuntu/results/coverage/Checksum/Checksum_2_10.coverage", "448c8029-05de-494e-aa36-cc3d574a7040");
            out[i] = (byte) (f & BYTE_MASK);
        }
        writelineStatic("/home/ubuntu/results/coverage/Checksum/Checksum_2_10.coverage", "d7d61f4d-637f-43d1-ac9e-dd210d1d989d");
        return out;
    }

    /**
     * reads the checksum from a file using the specified format.
     *
     * @since 1.7
     */
    private String readChecksum(File f) {
        writeline("/home/ubuntu/results/coverage/Checksum/Checksum_2_10.coverage", "276878a6-c6f5-4944-8e67-754b6fd82046");
        BufferedReader diskChecksumReader = null;
        writeline("/home/ubuntu/results/coverage/Checksum/Checksum_2_10.coverage", "16f51f58-055b-4c05-8da2-ecd5a7b30fa2");
        try {
            writeline("/home/ubuntu/results/coverage/Checksum/Checksum_2_10.coverage", "397e6705-9a06-4ce7-83f2-4a3318685360");
            diskChecksumReader = new BufferedReader(new FileReader(f));
            writeline("/home/ubuntu/results/coverage/Checksum/Checksum_2_10.coverage", "6eca3871-7acb-48e5-a2e1-527431203ef0");
            Object[] result = format.parse(diskChecksumReader.readLine());
            writeline("/home/ubuntu/results/coverage/Checksum/Checksum_2_10.coverage", "30af6551-59f3-4c9d-835c-14a3af6ddba0");
            if (result == null || result.length == 0 || result[0] == null) {
                writeline("/home/ubuntu/results/coverage/Checksum/Checksum_2_10.coverage", "4d26f145-19f6-4ffb-a837-b24f7d4a9d6b");
                throw new BuildException("failed to find a checksum");
            }
            writeline("/home/ubuntu/results/coverage/Checksum/Checksum_2_10.coverage", "c47abc7b-57a8-49f6-832a-7108bdecd22e");
            return (String) result[0];
        } catch (IOException e) {
            writeline("/home/ubuntu/results/coverage/Checksum/Checksum_2_10.coverage", "f8d2d4d1-4452-496e-b27b-251086d7dc10");
            throw new BuildException("Couldn't read checksum file " + f, e);
        } catch (ParseException e) {
            writeline("/home/ubuntu/results/coverage/Checksum/Checksum_2_10.coverage", "74159590-1a1d-4ad4-95ba-aaaf3048ea0a");
            throw new BuildException("Couldn't read checksum file " + f, e);
        } finally {
            writeline("/home/ubuntu/results/coverage/Checksum/Checksum_2_10.coverage", "52799d7a-e9c2-4e6a-88c3-2d6a5fb96089");
            FileUtils.close(diskChecksumReader);
        }
    }

    /**
     * @since Ant 1.8.2
     */
    private String getRelativeFilePath(File f) {
        writeline("/home/ubuntu/results/coverage/Checksum/Checksum_2_10.coverage", "5bee8204-0473-49c7-b39f-d719c4023d61");
        String path = (String) relativeFilePaths.get(f);
        writeline("/home/ubuntu/results/coverage/Checksum/Checksum_2_10.coverage", "03d78e35-1c4a-49ee-8942-4ffa9762d231");
        if (path == null) {
            writeline("/home/ubuntu/results/coverage/Checksum/Checksum_2_10.coverage", "29ae66a5-e4e9-4464-be7f-317159e6bc7b");
            // bug 37386. this should not occur, but it has, once.
            throw new BuildException("Internal error: " + "relativeFilePaths could not match file " + f + "\n" + "please file a bug report on this");
        }
        writeline("/home/ubuntu/results/coverage/Checksum/Checksum_2_10.coverage", "5d61d144-58fb-4f9f-98b8-ed4cc62dc165");
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
