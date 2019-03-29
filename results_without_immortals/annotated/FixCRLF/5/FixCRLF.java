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
        writeline("/home/ubuntu/results/coverage/FixCRLF/FixCRLF_5_10.coverage", "51b2add1-a85e-431a-ad63-997f0d1a06e2");
        return filter.chain(rdr);
    }

    /**
     * Set the source dir to find the source text files.
     * @param srcDir the source directory.
     */
    public void setSrcdir(File srcDir) {
        writeline("/home/ubuntu/results/coverage/FixCRLF/FixCRLF_5_10.coverage", "6015a828-e613-4a38-826e-661fa34e1359");
        this.srcDir = srcDir;
    }

    /**
     * Set the destination where the fixed files should be placed.
     * Default is to replace the original file.
     * @param destDir the destination directory.
     */
    public void setDestdir(File destDir) {
        writeline("/home/ubuntu/results/coverage/FixCRLF/FixCRLF_5_10.coverage", "66e49576-82a1-4767-be7c-bab36093794d");
        this.destDir = destDir;
    }

    /**
     * Set to true if modifying Java source files.
     * @param javafiles whether modifying Java files.
     */
    public void setJavafiles(boolean javafiles) {
        writeline("/home/ubuntu/results/coverage/FixCRLF/FixCRLF_5_10.coverage", "96a40407-9119-47a0-b530-9c1d5757eb0e");
        filter.setJavafiles(javafiles);
    }

    /**
     * Set a single file to convert.
     * @since Ant 1.6.3
     * @param file the file to convert.
     */
    public void setFile(File file) {
        writeline("/home/ubuntu/results/coverage/FixCRLF/FixCRLF_5_10.coverage", "716e9c91-0b2d-43fc-b3e5-cd0d3c218c2a");
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
        writeline("/home/ubuntu/results/coverage/FixCRLF/FixCRLF_5_10.coverage", "fa94de96-5ab9-4725-a658-1a5739d9e278");
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
        writeline("/home/ubuntu/results/coverage/FixCRLF/FixCRLF_5_10.coverage", "d708b2d2-c5cb-498e-a0d6-aca3675d5f8a");
        log("DEPRECATED: The cr attribute has been deprecated,", Project.MSG_WARN);
        writeline("/home/ubuntu/results/coverage/FixCRLF/FixCRLF_5_10.coverage", "c3171ec4-1b3d-493c-bb0e-1939e720f6ca");
        log("Please use the eol attribute instead", Project.MSG_WARN);
        writeline("/home/ubuntu/results/coverage/FixCRLF/FixCRLF_5_10.coverage", "d24d0dfd-838c-47cf-ba29-08edb2315c1f");
        String option = attr.getValue();
        writeline("/home/ubuntu/results/coverage/FixCRLF/FixCRLF_5_10.coverage", "2928293a-8685-405a-8d71-99ac55ee3d34");
        CrLf c = new CrLf();
        writeline("/home/ubuntu/results/coverage/FixCRLF/FixCRLF_5_10.coverage", "a16f6bbb-716e-4509-a555-c278af671436");
        if (option.equals("remove")) {
            writeline("/home/ubuntu/results/coverage/FixCRLF/FixCRLF_5_10.coverage", "cec6ea95-d21e-433a-9974-8be3776e3550");
            c.setValue("lf");
        } else if (option.equals("asis")) {
            writeline("/home/ubuntu/results/coverage/FixCRLF/FixCRLF_5_10.coverage", "81918d08-1229-4eef-b9f0-11f77035cd61");
            c.setValue("asis");
        } else {
            writeline("/home/ubuntu/results/coverage/FixCRLF/FixCRLF_5_10.coverage", "34034894-b5b4-4a62-9b00-d72fc7f36207");
            // must be "add"
            c.setValue("crlf");
        }
        writeline("/home/ubuntu/results/coverage/FixCRLF/FixCRLF_5_10.coverage", "e9f4cdbb-d14f-4b88-b5fc-3f7b45b2ac03");
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
        writeline("/home/ubuntu/results/coverage/FixCRLF/FixCRLF_5_10.coverage", "0b863e1c-e5d9-4db7-b55d-836b982643bc");
        filter.setTab(FixCrLfFilter.AddAsisRemove.newInstance(attr.getValue()));
    }

    /**
     * Specify tab length in characters.
     *
     * @param tlength specify the length of tab in spaces.
     * @throws BuildException on error.
     */
    public void setTablength(int tlength) throws BuildException {
        writeline("/home/ubuntu/results/coverage/FixCRLF/FixCRLF_5_10.coverage", "a118937a-6439-4e4a-9d13-6fa11a6a6653");
        try {
            writeline("/home/ubuntu/results/coverage/FixCRLF/FixCRLF_5_10.coverage", "b6a110c5-9dc1-4042-b551-390482353ac1");
            filter.setTablength(tlength);
        } catch (IOException e) {
            writeline("/home/ubuntu/results/coverage/FixCRLF/FixCRLF_5_10.coverage", "ebe1d57a-1d97-4346-8c4e-f606aac7b7bd");
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
        writeline("/home/ubuntu/results/coverage/FixCRLF/FixCRLF_5_10.coverage", "6829a9fd-d195-4eaa-81df-de589202c9df");
        filter.setEof(FixCrLfFilter.AddAsisRemove.newInstance(attr.getValue()));
    }

    /**
     * Specifies the encoding Ant expects the files to be
     * in--defaults to the platforms default encoding.
     * @param encoding String encoding name.
     */
    public void setEncoding(String encoding) {
        writeline("/home/ubuntu/results/coverage/FixCRLF/FixCRLF_5_10.coverage", "d8a09296-cf72-4679-ab92-4caae7e83cee");
        this.encoding = encoding;
    }

    /**
     * Specifies the encoding that the files are
     * to be written in--same as input encoding by default.
     * @param outputEncoding String outputEncoding name.
     */
    public void setOutputEncoding(String outputEncoding) {
        writeline("/home/ubuntu/results/coverage/FixCRLF/FixCRLF_5_10.coverage", "a5d70b05-a925-4137-9684-c495f8fa28aa");
        this.outputEncoding = outputEncoding;
    }

    /**
     * Specify whether a missing EOL will be added
     * to the final line of a file.
     * @param fixlast whether to fix the last line.
     */
    public void setFixlast(boolean fixlast) {
        writeline("/home/ubuntu/results/coverage/FixCRLF/FixCRLF_5_10.coverage", "daa6be00-0d6a-4ed6-9542-d1cb77aef63f");
        filter.setFixlast(fixlast);
    }

    /**
     * Set whether to preserve the last modified time as the original files.
     * @param preserve true if timestamps should be preserved.
     * @since Ant 1.6.3
     */
    public void setPreserveLastModified(boolean preserve) {
        writeline("/home/ubuntu/results/coverage/FixCRLF/FixCRLF_5_10.coverage", "a2d5c16a-a62a-4240-bf9c-6b25f63753d6");
        preserveLastModified = preserve;
    }

    /**
     * Executes the task.
     * @throws BuildException on error.
     */
    public void execute() throws BuildException {
        writeline("/home/ubuntu/results/coverage/FixCRLF/FixCRLF_5_10.coverage", "676805ee-8663-4cde-9615-b9c85a3c5cd4");
        // first off, make sure that we've got a srcdir and destdir
        validate();
        writeline("/home/ubuntu/results/coverage/FixCRLF/FixCRLF_5_10.coverage", "ccc72abb-c12f-4f14-9be3-6d2ae1f4a216");
        // log options used
        String enc = encoding == null ? "default" : encoding;
        writeline("/home/ubuntu/results/coverage/FixCRLF/FixCRLF_5_10.coverage", "0c14111c-1eb1-4c0a-8a3a-b140f22665f3");
        log("options:" + " eol=" + filter.getEol().getValue() + " tab=" + filter.getTab().getValue() + " eof=" + filter.getEof().getValue() + " tablength=" + filter.getTablength() + " encoding=" + enc + " outputencoding=" + (outputEncoding == null ? enc : outputEncoding), Project.MSG_VERBOSE);
        writeline("/home/ubuntu/results/coverage/FixCRLF/FixCRLF_5_10.coverage", "b0eb0f84-75a7-4abc-ab43-6cd076a1d749");
        DirectoryScanner ds = super.getDirectoryScanner(srcDir);
        writeline("/home/ubuntu/results/coverage/FixCRLF/FixCRLF_5_10.coverage", "099112da-0835-49ba-85d2-a44549e0b4a0");
        String[] files = ds.getIncludedFiles();
        writeline("/home/ubuntu/results/coverage/FixCRLF/FixCRLF_5_10.coverage", "4c356898-4ca8-4d2e-8acb-d68f568426a2");
        for (int i = 0; i < files.length; i++) {
            writeline("/home/ubuntu/results/coverage/FixCRLF/FixCRLF_5_10.coverage", "d5f5ee00-c4ea-444a-b5fa-d976dcffd0fc");
            processFile(files[i]);
        }
    }

    private void validate() throws BuildException {
        writeline("/home/ubuntu/results/coverage/FixCRLF/FixCRLF_5_10.coverage", "26426683-326d-4f5f-9355-e3385f8c160f");
        if (file != null) {
            writeline("/home/ubuntu/results/coverage/FixCRLF/FixCRLF_5_10.coverage", "420be573-7001-4a3f-9b28-b15db09f88ff");
            if (srcDir != null) {
                writeline("/home/ubuntu/results/coverage/FixCRLF/FixCRLF_5_10.coverage", "203d5c90-3439-4767-aa8a-079c4b08b5c9");
                throw new BuildException(ERROR_FILE_AND_SRCDIR);
            }
            writeline("/home/ubuntu/results/coverage/FixCRLF/FixCRLF_5_10.coverage", "0cafae36-b7a0-451f-a859-cecc143f16eb");
            // patch file into the fileset
            fileset.setFile(file);
            writeline("/home/ubuntu/results/coverage/FixCRLF/FixCRLF_5_10.coverage", "0cbb735e-a880-4b1b-b51f-d00b693b9350");
            // set our parent dir
            srcDir = file.getParentFile();
        }
        writeline("/home/ubuntu/results/coverage/FixCRLF/FixCRLF_5_10.coverage", "4aa9819f-14d5-49dd-96c3-5969f392ecbb");
        if (srcDir == null) {
            writeline("/home/ubuntu/results/coverage/FixCRLF/FixCRLF_5_10.coverage", "38998284-2c45-4969-9edc-c64f5cd00b44");
            throw new BuildException(FIXCRLF_ERROR + "srcdir attribute must be set!");
        }
        writeline("/home/ubuntu/results/coverage/FixCRLF/FixCRLF_5_10.coverage", "50116a27-0257-4d2e-a5d6-6850212976e2");
        if (!srcDir.exists()) {
            writeline("/home/ubuntu/results/coverage/FixCRLF/FixCRLF_5_10.coverage", "8a4ea268-09b7-4aa2-ac91-d8a4c1c13330");
            throw new BuildException(FIXCRLF_ERROR + "srcdir does not exist: '" + srcDir + "'");
        }
        writeline("/home/ubuntu/results/coverage/FixCRLF/FixCRLF_5_10.coverage", "ce1ccc67-b4f3-4da2-8e89-9883e436fc98");
        if (!srcDir.isDirectory()) {
            writeline("/home/ubuntu/results/coverage/FixCRLF/FixCRLF_5_10.coverage", "8cb4c07d-24e7-4993-b271-6c75f35f14f9");
            throw new BuildException(FIXCRLF_ERROR + "srcdir is not a directory: '" + srcDir + "'");
        }
        writeline("/home/ubuntu/results/coverage/FixCRLF/FixCRLF_5_10.coverage", "cc52d170-f2f6-4878-a80c-3c4d686b6622");
        if (destDir != null) {
            writeline("/home/ubuntu/results/coverage/FixCRLF/FixCRLF_5_10.coverage", "56217c85-7eab-4ffc-9184-04736192e40b");
            if (!destDir.exists()) {
                writeline("/home/ubuntu/results/coverage/FixCRLF/FixCRLF_5_10.coverage", "4d5f910a-8661-4b79-9ac7-1dbbca183609");
                throw new BuildException(FIXCRLF_ERROR + "destdir does not exist: '" + destDir + "'");
            }
            writeline("/home/ubuntu/results/coverage/FixCRLF/FixCRLF_5_10.coverage", "6815f663-f3ff-4dfd-b8aa-ff4c225da9c9");
            if (!destDir.isDirectory()) {
                writeline("/home/ubuntu/results/coverage/FixCRLF/FixCRLF_5_10.coverage", "fe3efad6-aa79-4792-aa73-7ac1d21624ad");
                throw new BuildException(FIXCRLF_ERROR + "destdir is not a directory: '" + destDir + "'");
            }
        }
    }

    private void processFile(String file) throws BuildException {
        writeline("/home/ubuntu/results/coverage/FixCRLF/FixCRLF_5_10.coverage", "eea33e51-a0bb-4a0c-8c4f-df1e19ec1900");
        File srcFile = new File(srcDir, file);
        writeline("/home/ubuntu/results/coverage/FixCRLF/FixCRLF_5_10.coverage", "5c09cc33-b89c-4084-b23c-060dd0a80b45");
        long lastModified = srcFile.lastModified();
        writeline("/home/ubuntu/results/coverage/FixCRLF/FixCRLF_5_10.coverage", "a02448d5-3b50-40ec-831f-daf153a83084");
        File destD = destDir == null ? srcDir : destDir;
        writeline("/home/ubuntu/results/coverage/FixCRLF/FixCRLF_5_10.coverage", "04058ca1-bae4-4481-bedc-fb7caeb563e7");
        if (fcv == null) {
            writeline("/home/ubuntu/results/coverage/FixCRLF/FixCRLF_5_10.coverage", "7c173057-497c-460f-8b40-d2f340ea2fd9");
            FilterChain fc = new FilterChain();
            writeline("/home/ubuntu/results/coverage/FixCRLF/FixCRLF_5_10.coverage", "5175e0f6-a4d1-47a3-9313-8061f3013199");
            fc.add(filter);
            writeline("/home/ubuntu/results/coverage/FixCRLF/FixCRLF_5_10.coverage", "45b496c2-316b-4869-8bc6-5f4c5e2939d8");
            fcv = new Vector<FilterChain>(1);
            writeline("/home/ubuntu/results/coverage/FixCRLF/FixCRLF_5_10.coverage", "db2ee0c4-1957-40b6-8ea8-39992715faba");
            fcv.add(fc);
        }
        writeline("/home/ubuntu/results/coverage/FixCRLF/FixCRLF_5_10.coverage", "a30d1e66-69f5-4ec9-a75e-cf55eeaaa93c");
        File tmpFile = FILE_UTILS.createTempFile("fixcrlf", "", null, true, true);
        writeline("/home/ubuntu/results/coverage/FixCRLF/FixCRLF_5_10.coverage", "47e32e3e-886a-4d8d-8d3b-49ffd70d37ca");
        try {
            writeline("/home/ubuntu/results/coverage/FixCRLF/FixCRLF_5_10.coverage", "eed52c1c-33a5-462d-a43b-f8e5f7ba09c9");
            FILE_UTILS.copyFile(srcFile, tmpFile, null, fcv, true, false, encoding, outputEncoding == null ? encoding : outputEncoding, getProject());
            writeline("/home/ubuntu/results/coverage/FixCRLF/FixCRLF_5_10.coverage", "4ea08b4b-be04-47bc-9433-384032f26d1d");
            File destFile = new File(destD, file);
            writeline("/home/ubuntu/results/coverage/FixCRLF/FixCRLF_5_10.coverage", "6dda2c7c-cb3c-4afe-92f4-e81c269df871");
            boolean destIsWrong = true;
            writeline("/home/ubuntu/results/coverage/FixCRLF/FixCRLF_5_10.coverage", "56b5600a-2e40-4b0c-a9f3-c1573c4b16e9");
            if (destFile.exists()) {
                writeline("/home/ubuntu/results/coverage/FixCRLF/FixCRLF_5_10.coverage", "5ffd2955-7cb5-4265-8284-1a6f5d78c4e3");
                // Compare the destination with the temp file
                log("destFile " + destFile + " exists", Project.MSG_DEBUG);
                writeline("/home/ubuntu/results/coverage/FixCRLF/FixCRLF_5_10.coverage", "a2e664b6-dc60-48ea-849f-55a354fdc1e9");
                destIsWrong = !FILE_UTILS.contentEquals(destFile, tmpFile);
                writeline("/home/ubuntu/results/coverage/FixCRLF/FixCRLF_5_10.coverage", "b63a15f2-2681-4f08-a8e7-b62157ab2f23");
                log(destFile + (destIsWrong ? " is being written" : " is not written, as the contents are identical"), Project.MSG_DEBUG);
            }
            writeline("/home/ubuntu/results/coverage/FixCRLF/FixCRLF_5_10.coverage", "871c6bc5-aa71-4cc5-be4c-5e3f2b2be82b");
            if (destIsWrong) {
                writeline("/home/ubuntu/results/coverage/FixCRLF/FixCRLF_5_10.coverage", "7e019b22-d728-4d09-a0e6-2a6a3aba8dd3");
                FILE_UTILS.rename(tmpFile, destFile);
                writeline("/home/ubuntu/results/coverage/FixCRLF/FixCRLF_5_10.coverage", "2a11786d-713b-401b-b16b-d96d6ec103b2");
                if (preserveLastModified) {
                    writeline("/home/ubuntu/results/coverage/FixCRLF/FixCRLF_5_10.coverage", "d0c44984-d796-4ce4-b3d8-1da148c639c8");
                    log("preserved lastModified for " + destFile, Project.MSG_DEBUG);
                    writeline("/home/ubuntu/results/coverage/FixCRLF/FixCRLF_5_10.coverage", "3c886916-32f1-4975-adc6-131567dbac25");
                    FILE_UTILS.setFileLastModified(destFile, lastModified);
                }
            }
        } catch (IOException e) {
            writeline("/home/ubuntu/results/coverage/FixCRLF/FixCRLF_5_10.coverage", "4b80069a-cbe2-4f7d-b32f-75887f563b98");
            throw new BuildException("error running fixcrlf on file " + srcFile, e);
        } finally {
            writeline("/home/ubuntu/results/coverage/FixCRLF/FixCRLF_5_10.coverage", "32f0e171-7b34-42f2-a019-447baadb8302");
            if (tmpFile != null && tmpFile.exists()) {
                writeline("/home/ubuntu/results/coverage/FixCRLF/FixCRLF_5_10.coverage", "1331d523-4c27-437c-9971-7a929ca779b9");
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
