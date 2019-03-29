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
import java.io.FileReader;
import java.io.IOException;
import java.io.InputStreamReader;
import java.io.Reader;
import java.util.Enumeration;
import java.util.NoSuchElementException;
import java.util.Vector;
import org.apache.tools.ant.BuildException;
import org.apache.tools.ant.DirectoryScanner;
import org.apache.tools.ant.Project;
import org.apache.tools.ant.filters.ChainableReader;
import org.apache.tools.ant.filters.FixCrLfFilter;
import org.apache.tools.ant.types.EnumeratedAttribute;
import org.apache.tools.ant.types.FilterChain;
import org.apache.tools.ant.util.FileUtils;
import java.io.*;

public class FixCRLF extends MatchingTask implements ChainableReader {

    private static final String FIXCRLF_ERROR = "<fixcrlf> error: ";

    /**
     * error string for using srcdir and file
     */
    public static final String ERROR_FILE_AND_SRCDIR = FIXCRLF_ERROR + "srcdir and file are mutually exclusive";

    private static final FileUtils FILE_UTILS = FileUtils.getFileUtils();

    private boolean preserveLastModified = false;

    private File srcDir;

    private File destDir = null;

    private File file;

    private FixCrLfFilter filter = new FixCrLfFilter();

    private Vector<FilterChain> fcv = null;

    /**
     * Encoding to assume for the files
     */
    private String encoding = null;

    /**
     * Encoding to use for output files
     */
    private String outputEncoding = null;

    /**
     * Chain this task as a reader.
     * @param rdr Reader to chain.
     * @return a Reader.
     * @since Ant 1.7?
     */
    public final Reader chain(final Reader rdr) {
        writeline("/home/ubuntu/results/coverage/FixCRLF/FixCRLF_8_10.coverage", "e614575a-bbfe-47e4-ae9a-3bc859222988");
        return filter.chain(rdr);
    }

    /**
     * Set the source dir to find the source text files.
     * @param srcDir the source directory.
     */
    public void setSrcdir(File srcDir) {
        writeline("/home/ubuntu/results/coverage/FixCRLF/FixCRLF_8_10.coverage", "85039a46-88fa-4bb6-9bc1-7d78b69372d6");
        this.srcDir = srcDir;
    }

    /**
     * Set the destination where the fixed files should be placed.
     * Default is to replace the original file.
     * @param destDir the destination directory.
     */
    public void setDestdir(File destDir) {
        writeline("/home/ubuntu/results/coverage/FixCRLF/FixCRLF_8_10.coverage", "5ab329cf-95be-446a-8567-65ae22460d3f");
        this.destDir = destDir;
    }

    /**
     * Set to true if modifying Java source files.
     * @param javafiles whether modifying Java files.
     */
    public void setJavafiles(boolean javafiles) {
        writeline("/home/ubuntu/results/coverage/FixCRLF/FixCRLF_8_10.coverage", "d993512b-acec-4464-ab7a-71aafbbf3336");
        filter.setJavafiles(javafiles);
    }

    /**
     * Set a single file to convert.
     * @since Ant 1.6.3
     * @param file the file to convert.
     */
    public void setFile(File file) {
        writeline("/home/ubuntu/results/coverage/FixCRLF/FixCRLF_8_10.coverage", "c1761f5e-af7b-4e24-a90f-0cfa8fc908b1");
        this.file = file;
    }

    /**
     * Specify how EndOfLine characters are to be handled.
     *
     * @param attr valid values:
     * <ul>
     * <li>asis: leave line endings alone
     * <li>cr: convert line endings to CR
     * <li>lf: convert line endings to LF
     * <li>crlf: convert line endings to CRLF
     * </ul>
     */
    public void setEol(CrLf attr) {
        writeline("/home/ubuntu/results/coverage/FixCRLF/FixCRLF_8_10.coverage", "bf03d0b0-ea6b-46d6-8537-2cb464d227f8");
        filter.setEol(FixCrLfFilter.CrLf.newInstance(attr.getValue()));
    }

    /**
     * Specify how carriage return (CR) characters are to be handled.
     *
     * @param attr valid values:
     * <ul>
     * <li>add: ensure that there is a CR before every LF
     * <li>asis: leave CR characters alone
     * <li>remove: remove all CR characters
     * </ul>
     *
     * @deprecated since 1.4.x.
     * Use {@link #setEol setEol} instead.
     */
    public void setCr(AddAsisRemove attr) {
        writeline("/home/ubuntu/results/coverage/FixCRLF/FixCRLF_8_10.coverage", "4b48a6aa-c2fa-43c4-8e2c-69e2692d223e");
        log("DEPRECATED: The cr attribute has been deprecated,", Project.MSG_WARN);
        writeline("/home/ubuntu/results/coverage/FixCRLF/FixCRLF_8_10.coverage", "2b683e8a-4d49-4375-9018-934a981ffa69");
        log("Please use the eol attribute instead", Project.MSG_WARN);
        writeline("/home/ubuntu/results/coverage/FixCRLF/FixCRLF_8_10.coverage", "f89b5951-8d54-4d4c-9e19-39389a6da413");
        String option = attr.getValue();
        writeline("/home/ubuntu/results/coverage/FixCRLF/FixCRLF_8_10.coverage", "999beee0-49de-4c89-8b68-3edbdb339c4a");
        CrLf c = new CrLf();
        writeline("/home/ubuntu/results/coverage/FixCRLF/FixCRLF_8_10.coverage", "88299316-4aa0-462a-b853-cd548717fb03");
        if (option.equals("remove")) {
            writeline("/home/ubuntu/results/coverage/FixCRLF/FixCRLF_8_10.coverage", "daf5c021-bdd8-41e8-b6f9-96fc0ec9d328");
            c.setValue("lf");
        } else if (option.equals("asis")) {
            writeline("/home/ubuntu/results/coverage/FixCRLF/FixCRLF_8_10.coverage", "33cd0944-7f2a-423f-a8f8-aa020842e315");
            c.setValue("asis");
        } else {
            writeline("/home/ubuntu/results/coverage/FixCRLF/FixCRLF_8_10.coverage", "f279482d-78c7-4152-b0f6-71e3cd678f86");
            // must be "add"
            c.setValue("crlf");
        }
        writeline("/home/ubuntu/results/coverage/FixCRLF/FixCRLF_8_10.coverage", "f6e1a01e-ec74-494c-8e63-bfdb9f710e4e");
        setEol(c);
    }

    /**
     * Specify how tab characters are to be handled.
     *
     * @param attr valid values:
     * <ul>
     * <li>add: convert sequences of spaces which span a tab stop to tabs
     * <li>asis: leave tab and space characters alone
     * <li>remove: convert tabs to spaces
     * </ul>
     */
    public void setTab(AddAsisRemove attr) {
        writeline("/home/ubuntu/results/coverage/FixCRLF/FixCRLF_8_10.coverage", "714bbdda-2235-40fc-98aa-644402e14001");
        filter.setTab(FixCrLfFilter.AddAsisRemove.newInstance(attr.getValue()));
    }

    /**
     * Specify tab length in characters.
     *
     * @param tlength specify the length of tab in spaces.
     * @throws BuildException on error.
     */
    public void setTablength(int tlength) throws BuildException {
        writeline("/home/ubuntu/results/coverage/FixCRLF/FixCRLF_8_10.coverage", "32de49db-9513-4cc8-8333-2a50522ead40");
        try {
            writeline("/home/ubuntu/results/coverage/FixCRLF/FixCRLF_8_10.coverage", "8a10ab05-f838-4bd2-8277-6f7f84ee3156");
            filter.setTablength(tlength);
        } catch (IOException e) {
            writeline("/home/ubuntu/results/coverage/FixCRLF/FixCRLF_8_10.coverage", "4731269e-78b2-4015-9a16-c1f844abd1e8");
            // a BuildException
            throw new BuildException(e.getMessage(), e);
        }
    }

    /**
     * Specify how DOS EOF (control-z) characters are to be handled.
     *
     * @param attr valid values:
     * <ul>
     * <li>add: ensure that there is an eof at the end of the file
     * <li>asis: leave eof characters alone
     * <li>remove: remove any eof character found at the end
     * </ul>
     */
    public void setEof(AddAsisRemove attr) {
        writeline("/home/ubuntu/results/coverage/FixCRLF/FixCRLF_8_10.coverage", "e28f6424-855f-4d85-b269-365e8d366479");
        filter.setEof(FixCrLfFilter.AddAsisRemove.newInstance(attr.getValue()));
    }

    /**
     * Specifies the encoding Ant expects the files to be
     * in--defaults to the platforms default encoding.
     * @param encoding String encoding name.
     */
    public void setEncoding(String encoding) {
        writeline("/home/ubuntu/results/coverage/FixCRLF/FixCRLF_8_10.coverage", "5436cc25-4871-4684-af2b-11ad023b9e7f");
        this.encoding = encoding;
    }

    /**
     * Specifies the encoding that the files are
     * to be written in--same as input encoding by default.
     * @param outputEncoding String outputEncoding name.
     */
    public void setOutputEncoding(String outputEncoding) {
        writeline("/home/ubuntu/results/coverage/FixCRLF/FixCRLF_8_10.coverage", "f573b8ae-bf0f-4287-af29-212fb5e9490b");
        this.outputEncoding = outputEncoding;
    }

    /**
     * Specify whether a missing EOL will be added
     * to the final line of a file.
     * @param fixlast whether to fix the last line.
     */
    public void setFixlast(boolean fixlast) {
        writeline("/home/ubuntu/results/coverage/FixCRLF/FixCRLF_8_10.coverage", "41464874-05c7-4920-95c4-a22864c9e263");
        filter.setFixlast(fixlast);
    }

    /**
     * Set whether to preserve the last modified time as the original files.
     * @param preserve true if timestamps should be preserved.
     * @since Ant 1.6.3
     */
    public void setPreserveLastModified(boolean preserve) {
        writeline("/home/ubuntu/results/coverage/FixCRLF/FixCRLF_8_10.coverage", "c5134579-3716-476c-9a30-9d8c3f497b06");
        preserveLastModified = preserve;
    }

    /**
     * Executes the task.
     * @throws BuildException on error.
     */
    public void execute() throws BuildException {
        writeline("/home/ubuntu/results/coverage/FixCRLF/FixCRLF_8_10.coverage", "306a0796-c72b-4a72-9f41-12db82fbb4ef");
        // first off, make sure that we've got a srcdir and destdir
        validate();
        writeline("/home/ubuntu/results/coverage/FixCRLF/FixCRLF_8_10.coverage", "94cc5fcc-be35-4eaf-af9d-0a54986287fd");
        // log options used
        String enc = encoding == null ? "default" : encoding;
        writeline("/home/ubuntu/results/coverage/FixCRLF/FixCRLF_8_10.coverage", "c1b93edb-6316-4aa1-864a-29a482686093");
        log("options:" + " eol=" + filter.getEol().getValue() + " tab=" + filter.getTab().getValue() + " eof=" + filter.getEof().getValue() + " tablength=" + filter.getTablength() + " encoding=" + enc + " outputencoding=" + (outputEncoding == null ? enc : outputEncoding), Project.MSG_VERBOSE);
        writeline("/home/ubuntu/results/coverage/FixCRLF/FixCRLF_8_10.coverage", "00dc4445-b229-4403-bdc1-f167cac98069");
        DirectoryScanner ds = super.getDirectoryScanner(srcDir);
        writeline("/home/ubuntu/results/coverage/FixCRLF/FixCRLF_8_10.coverage", "0500c328-0277-46f1-b866-f217f1c0488b");
        String[] files = ds.getIncludedFiles();
        writeline("/home/ubuntu/results/coverage/FixCRLF/FixCRLF_8_10.coverage", "0bf39c39-f86a-4a75-b46d-f89b9d3422cf");
        for (int i = 0; i < files.length; i++) {
            writeline("/home/ubuntu/results/coverage/FixCRLF/FixCRLF_8_10.coverage", "6cc39184-2f33-43c2-8ef4-5112e1ee9b7a");
            processFile(files[i]);
        }
    }

    private void validate() throws BuildException {
        writeline("/home/ubuntu/results/coverage/FixCRLF/FixCRLF_8_10.coverage", "3b3b1414-988a-432a-9102-2d2617f69e98");
        if (file != null) {
            writeline("/home/ubuntu/results/coverage/FixCRLF/FixCRLF_8_10.coverage", "8e12b177-554b-4659-9d47-6ae9e820c87a");
            if (srcDir != null) {
                writeline("/home/ubuntu/results/coverage/FixCRLF/FixCRLF_8_10.coverage", "d5abc825-8383-4a94-88fd-66bb5b13327c");
                throw new BuildException(ERROR_FILE_AND_SRCDIR);
            }
            writeline("/home/ubuntu/results/coverage/FixCRLF/FixCRLF_8_10.coverage", "e4fdb2b8-aff2-42c5-a7ba-ef6440f59161");
            // patch file into the fileset
            fileset.setFile(file);
            writeline("/home/ubuntu/results/coverage/FixCRLF/FixCRLF_8_10.coverage", "2993cc90-7987-4cba-96c1-2645d8b51678");
            // set our parent dir
            srcDir = file.getParentFile();
        }
        writeline("/home/ubuntu/results/coverage/FixCRLF/FixCRLF_8_10.coverage", "87772802-bcb6-4a63-a2e3-d15a2d119aab");
        if (srcDir == null) {
            writeline("/home/ubuntu/results/coverage/FixCRLF/FixCRLF_8_10.coverage", "1d187ba3-9c49-484e-8334-b15d4ec5aa3e");
            throw new BuildException(FIXCRLF_ERROR + "srcdir attribute must be set!");
        }
        writeline("/home/ubuntu/results/coverage/FixCRLF/FixCRLF_8_10.coverage", "f0a424bb-712d-4bd1-9916-468ec1d5daef");
        if (!srcDir.exists()) {
            writeline("/home/ubuntu/results/coverage/FixCRLF/FixCRLF_8_10.coverage", "fd33facb-bf58-4f83-ac4f-cf63db272f96");
            throw new BuildException(FIXCRLF_ERROR + "srcdir does not exist: '" + srcDir + "'");
        }
        writeline("/home/ubuntu/results/coverage/FixCRLF/FixCRLF_8_10.coverage", "b970b8d2-725c-44b4-8745-1119861f212c");
        if (!srcDir.isDirectory()) {
            writeline("/home/ubuntu/results/coverage/FixCRLF/FixCRLF_8_10.coverage", "9063d2ea-5ec1-4d17-bb8c-8753abdb364c");
            throw new BuildException(FIXCRLF_ERROR + "srcdir is not a directory: '" + srcDir + "'");
        }
        writeline("/home/ubuntu/results/coverage/FixCRLF/FixCRLF_8_10.coverage", "4bf84e55-5abb-4027-9960-aeba64fc874d");
        if (destDir != null) {
            writeline("/home/ubuntu/results/coverage/FixCRLF/FixCRLF_8_10.coverage", "1fae70e5-1e7f-4b71-a1f8-0199d24657a6");
            if (!destDir.exists()) {
                writeline("/home/ubuntu/results/coverage/FixCRLF/FixCRLF_8_10.coverage", "24814419-fa6c-4a20-a961-74c49572747b");
                throw new BuildException(FIXCRLF_ERROR + "destdir does not exist: '" + destDir + "'");
            }
            writeline("/home/ubuntu/results/coverage/FixCRLF/FixCRLF_8_10.coverage", "8dceb238-064b-4818-a8fc-b3306d31ecfd");
            if (!destDir.isDirectory()) {
                writeline("/home/ubuntu/results/coverage/FixCRLF/FixCRLF_8_10.coverage", "30eb94ac-7984-4ce1-b385-b1fa3cb74bb4");
                throw new BuildException(FIXCRLF_ERROR + "destdir is not a directory: '" + destDir + "'");
            }
        }
    }

    private void processFile(String file) throws BuildException {
        writeline("/home/ubuntu/results/coverage/FixCRLF/FixCRLF_8_10.coverage", "26e130ee-da67-4192-954e-56e1517b1139");
        File srcFile = new File(srcDir, file);
        writeline("/home/ubuntu/results/coverage/FixCRLF/FixCRLF_8_10.coverage", "2a1844e2-6cfc-471f-8250-88002b5d1f67");
        long lastModified = srcFile.lastModified();
        writeline("/home/ubuntu/results/coverage/FixCRLF/FixCRLF_8_10.coverage", "66ed9926-103f-47f0-acb8-4260f99edd0c");
        File destD = destDir == null ? srcDir : destDir;
        writeline("/home/ubuntu/results/coverage/FixCRLF/FixCRLF_8_10.coverage", "021d6cf7-d729-4a57-88bf-50de688dbd77");
        if (fcv == null) {
            writeline("/home/ubuntu/results/coverage/FixCRLF/FixCRLF_8_10.coverage", "fd2362b4-4c2d-49a3-ac25-0c9a976d8a09");
            FilterChain fc = new FilterChain();
            writeline("/home/ubuntu/results/coverage/FixCRLF/FixCRLF_8_10.coverage", "44184f20-19ef-4a5f-848d-ae67a925f8fc");
            fc.add(filter);
            writeline("/home/ubuntu/results/coverage/FixCRLF/FixCRLF_8_10.coverage", "41926610-87a8-4d77-99ac-0817c86085ea");
            fcv = new Vector<FilterChain>(1);
            writeline("/home/ubuntu/results/coverage/FixCRLF/FixCRLF_8_10.coverage", "99f627f6-a029-4e0b-8ad4-cf920776d81b");
            fcv.add(fc);
        }
        writeline("/home/ubuntu/results/coverage/FixCRLF/FixCRLF_8_10.coverage", "a92413ef-552a-4d36-9733-0aab85067e92");
        File tmpFile = FILE_UTILS.createTempFile("fixcrlf", "", null, true, true);
        writeline("/home/ubuntu/results/coverage/FixCRLF/FixCRLF_8_10.coverage", "169fcdfc-d74e-4434-84ff-082cf8313b2a");
        try {
            writeline("/home/ubuntu/results/coverage/FixCRLF/FixCRLF_8_10.coverage", "ec8649ca-5a3d-42f2-8b57-cbf75d10a018");
            FILE_UTILS.copyFile(srcFile, tmpFile, null, fcv, true, false, encoding, outputEncoding == null ? encoding : outputEncoding, getProject());
            writeline("/home/ubuntu/results/coverage/FixCRLF/FixCRLF_8_10.coverage", "1b64bcf6-7ac2-46b3-82ac-9e66d250995a");
            File destFile = new File(destD, file);
            writeline("/home/ubuntu/results/coverage/FixCRLF/FixCRLF_8_10.coverage", "22d0789b-644c-485a-9d25-525e10dd6b8c");
            boolean destIsWrong = true;
            writeline("/home/ubuntu/results/coverage/FixCRLF/FixCRLF_8_10.coverage", "5eb1acf9-e180-4d47-a993-a68033b08bb5");
            if (destFile.exists()) {
                writeline("/home/ubuntu/results/coverage/FixCRLF/FixCRLF_8_10.coverage", "0ce0c09b-e11c-4f4e-a8a4-bd9af2925c75");
                // Compare the destination with the temp file
                log("destFile " + destFile + " exists", Project.MSG_DEBUG);
                writeline("/home/ubuntu/results/coverage/FixCRLF/FixCRLF_8_10.coverage", "d4b9035f-abc3-46f5-852c-c4a79baf112a");
                destIsWrong = !FILE_UTILS.contentEquals(destFile, tmpFile);
                writeline("/home/ubuntu/results/coverage/FixCRLF/FixCRLF_8_10.coverage", "8c1e98ff-13c3-4850-8c4d-2df9b53a114e");
                log(destFile + (destIsWrong ? " is being written" : " is not written, as the contents are identical"), Project.MSG_DEBUG);
            }
            writeline("/home/ubuntu/results/coverage/FixCRLF/FixCRLF_8_10.coverage", "65c18865-52ea-4195-833d-9e44a23a9d9d");
            if (destIsWrong) {
                writeline("/home/ubuntu/results/coverage/FixCRLF/FixCRLF_8_10.coverage", "d8e6b601-d451-4675-997a-25e2b4f04eaf");
                FILE_UTILS.rename(tmpFile, destFile);
                writeline("/home/ubuntu/results/coverage/FixCRLF/FixCRLF_8_10.coverage", "2164b4f2-86db-482f-8fc4-bdd04e5ccdc7");
                if (preserveLastModified) {
                    writeline("/home/ubuntu/results/coverage/FixCRLF/FixCRLF_8_10.coverage", "e76a1325-6eeb-4dfc-a305-47bae694cdd5");
                    log("preserved lastModified for " + destFile, Project.MSG_DEBUG);
                    writeline("/home/ubuntu/results/coverage/FixCRLF/FixCRLF_8_10.coverage", "7ec35e54-4f43-4916-a0c7-5b8d334f72ef");
                    FILE_UTILS.setFileLastModified(destFile, lastModified);
                }
            }
        } catch (IOException e) {
            writeline("/home/ubuntu/results/coverage/FixCRLF/FixCRLF_8_10.coverage", "19c2511b-bc54-490a-8841-c16ef1e690bb");
            throw new BuildException("error running fixcrlf on file " + srcFile, e);
        } finally {
            writeline("/home/ubuntu/results/coverage/FixCRLF/FixCRLF_8_10.coverage", "57cf6518-31e3-4cd6-be4f-43e48b9968f1");
            if (tmpFile != null && tmpFile.exists()) {
                writeline("/home/ubuntu/results/coverage/FixCRLF/FixCRLF_8_10.coverage", "13081b01-4d10-4e8f-92b8-26cf66901c68");
                FILE_UTILS.tryHardToDelete(tmpFile);
            }
        }
    }

    /**
     * Deprecated, the functionality has been moved to filters.FixCrLfFilter.
     * @deprecated since 1.7.0.
     */
    protected class OneLiner implements Enumeration<Object> {

        private static final int UNDEF = -1;

        private static final int NOTJAVA = 0;

        private static final int LOOKING = 1;

        private static final int INBUFLEN = 8192;

        private static final int LINEBUFLEN = 200;

        private static final char CTRLZ = '\u001A';

        private int state = filter.getJavafiles() ? LOOKING : NOTJAVA;

        private StringBuffer eolStr = new StringBuffer(LINEBUFLEN);

        private StringBuffer eofStr = new StringBuffer();

        private BufferedReader reader;

        private StringBuffer line = new StringBuffer();

        private boolean reachedEof = false;

        private File srcFile;

        /**
         * Constructor.
         * @param srcFile the file to read.
         * @throws BuildException if there is an error.
         */
        public OneLiner(File srcFile) throws BuildException {
            this.srcFile = srcFile;
            try {
                reader = new BufferedReader(((encoding == null) ? new FileReader(srcFile) : new InputStreamReader(new FileInputStream(srcFile), encoding)), INBUFLEN);
                nextLine();
            } catch (IOException e) {
                throw new BuildException(srcFile + ": " + e.getMessage(), e, getLocation());
            }
        }

        /**
         * Move to the next line.
         * @throws BuildException if there is an error.
         */
        protected void nextLine() throws BuildException {
            int ch = -1;
            int eolcount = 0;
            eolStr = new StringBuffer();
            line = new StringBuffer();
            try {
                ch = reader.read();
                while (ch != -1 && ch != '\r' && ch != '\n') {
                    line.append((char) ch);
                    ch = reader.read();
                }
                if (ch == -1 && line.length() == 0) {
                    // Eof has been reached
                    reachedEof = true;
                    return;
                }
                switch((char) ch) {
                    case '\r':
                        // Check for \r, \r\n and \r\r\n
                        // Regard \r\r not followed by \n as two lines
                        ++eolcount;
                        eolStr.append('\r');
                        reader.mark(2);
                        ch = reader.read();
                        switch(ch) {
                            case '\r':
                                ch = reader.read();
                                if ((char) (ch) == '\n') {
                                    eolcount += 2;
                                    eolStr.append("\r\n");
                                } else {
                                    reader.reset();
                                }
                                break;
                            case '\n':
                                ++eolcount;
                                eolStr.append('\n');
                                break;
                            case -1:
                                // of the stream
                                break;
                            default:
                                reader.reset();
                                break;
                        }
                        // end of switch ((char)(ch = reader.read()))
                        break;
                    case '\n':
                        ++eolcount;
                        eolStr.append('\n');
                        break;
                    default:
                }
                // are CTRL-Zs, set eofStr
                if (eolcount == 0) {
                    int i = line.length();
                    while (--i >= 0 && line.charAt(i) == CTRLZ) {
                    // keep searching for the first ^Z
                    }
                    if (i < line.length() - 1) {
                        // Trailing characters are ^Zs
                        // Construct new line and eofStr
                        eofStr.append(line.toString().substring(i + 1));
                        if (i < 0) {
                            line.setLength(0);
                            reachedEof = true;
                        } else {
                            line.setLength(i + 1);
                        }
                    }
                }
            // end of if (eolcount == 0)
            } catch (IOException e) {
                throw new BuildException(srcFile + ": " + e.getMessage(), e, getLocation());
            }
        }

        /**
         * get the eof string.
         * @return the eof string.
         */
        public String getEofStr() {
            return eofStr.substring(0);
        }

        /**
         * get the state.
         * @return the state.
         */
        public int getState() {
            return state;
        }

        /**
         * Set the state.
         * @param state the value to use.
         */
        public void setState(int state) {
            this.state = state;
        }

        /**
         * @return true if there is more elements.
         */
        public boolean hasMoreElements() {
            return !reachedEof;
        }

        /**
         * get the next element.
         * @return the next element.
         * @throws NoSuchElementException if there is no more.
         */
        public Object nextElement() throws NoSuchElementException {
            if (!hasMoreElements()) {
                throw new NoSuchElementException("OneLiner");
            }
            BufferLine tmpLine = new BufferLine(line.toString(), eolStr.substring(0));
            nextLine();
            return tmpLine;
        }

        /**
         * Close the reader.
         * @throws IOException if there is an error.
         */
        public void close() throws IOException {
            if (reader != null) {
                reader.close();
            }
        }

        class BufferLine {

            private int next = 0;

            private int column = 0;

            private int lookahead = UNDEF;

            private String line;

            private String eolStr;

            public BufferLine(String line, String eolStr) throws BuildException {
                next = 0;
                column = 0;
                this.line = line;
                this.eolStr = eolStr;
            }

            public int getNext() {
                return next;
            }

            public void setNext(int next) {
                this.next = next;
            }

            public int getLookahead() {
                return lookahead;
            }

            public void setLookahead(int lookahead) {
                this.lookahead = lookahead;
            }

            public char getChar(int i) {
                return line.charAt(i);
            }

            public char getNextChar() {
                return getChar(next);
            }

            public char getNextCharInc() {
                return getChar(next++);
            }

            public int getColumn() {
                return column;
            }

            public void setColumn(int col) {
                column = col;
            }

            public int incColumn() {
                return column++;
            }

            public int length() {
                return line.length();
            }

            public int getEolLength() {
                return eolStr.length();
            }

            public String getLineString() {
                return line;
            }

            public String getEol() {
                return eolStr;
            }

            public String substring(int begin) {
                return line.substring(begin);
            }

            public String substring(int begin, int end) {
                return line.substring(begin, end);
            }

            public void setState(int state) {
                OneLiner.this.setState(state);
            }

            public int getState() {
                return OneLiner.this.getState();
            }
        }
    }

    /**
     * Enumerated attribute with the values "asis", "add" and "remove".
     */
    public static class AddAsisRemove extends EnumeratedAttribute {

        /**
         * {@inheritDoc}.
         */
        public String[] getValues() {
            return new String[] { "add", "asis", "remove" };
        }
    }

    /**
     * Enumerated attribute with the values "asis", "cr", "lf" and "crlf".
     */
    public static class CrLf extends EnumeratedAttribute {

        /**
         * @see EnumeratedAttribute#getValues
         */
        /**
         * {@inheritDoc}.
         */
        public String[] getValues() {
            return new String[] { "asis", "cr", "lf", "crlf", "mac", "unix", "dos" };
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
