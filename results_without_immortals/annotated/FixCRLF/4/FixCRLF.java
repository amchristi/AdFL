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
        writeline("/home/ubuntu/results/coverage/FixCRLF/FixCRLF_4_10.coverage", "3b1adf99-52b0-4397-9a3a-26d0b0e9174f");
        return filter.chain(rdr);
    }

    /**
     * Set the source dir to find the source text files.
     * @param srcDir the source directory.
     */
    public void setSrcdir(File srcDir) {
        writeline("/home/ubuntu/results/coverage/FixCRLF/FixCRLF_4_10.coverage", "8293b111-4d9d-4394-ba87-cdb5169394d2");
        this.srcDir = srcDir;
    }

    /**
     * Set the destination where the fixed files should be placed.
     * Default is to replace the original file.
     * @param destDir the destination directory.
     */
    public void setDestdir(File destDir) {
        writeline("/home/ubuntu/results/coverage/FixCRLF/FixCRLF_4_10.coverage", "925d2f07-5c7a-46f5-8e31-f7b2420c650a");
        this.destDir = destDir;
    }

    /**
     * Set to true if modifying Java source files.
     * @param javafiles whether modifying Java files.
     */
    public void setJavafiles(boolean javafiles) {
        writeline("/home/ubuntu/results/coverage/FixCRLF/FixCRLF_4_10.coverage", "188f6c08-ebcb-4202-9d3e-49d8a8099745");
        filter.setJavafiles(javafiles);
    }

    /**
     * Set a single file to convert.
     * @since Ant 1.6.3
     * @param file the file to convert.
     */
    public void setFile(File file) {
        writeline("/home/ubuntu/results/coverage/FixCRLF/FixCRLF_4_10.coverage", "7569c8e9-c1ec-4c39-84b5-93110fd97864");
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
        writeline("/home/ubuntu/results/coverage/FixCRLF/FixCRLF_4_10.coverage", "6d89e550-2066-48f3-be16-15e5597e793a");
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
        writeline("/home/ubuntu/results/coverage/FixCRLF/FixCRLF_4_10.coverage", "000c32ad-f2f4-44a8-a4e4-c9ebbe3f8b55");
        log("DEPRECATED: The cr attribute has been deprecated,", Project.MSG_WARN);
        writeline("/home/ubuntu/results/coverage/FixCRLF/FixCRLF_4_10.coverage", "91625b54-3563-411c-948e-08bb899694d5");
        log("Please use the eol attribute instead", Project.MSG_WARN);
        writeline("/home/ubuntu/results/coverage/FixCRLF/FixCRLF_4_10.coverage", "21777b2c-9abc-44da-80c8-88f63edc512b");
        String option = attr.getValue();
        writeline("/home/ubuntu/results/coverage/FixCRLF/FixCRLF_4_10.coverage", "f2c94885-51d5-4a04-96c9-f7ff6ce7c21a");
        CrLf c = new CrLf();
        writeline("/home/ubuntu/results/coverage/FixCRLF/FixCRLF_4_10.coverage", "a7bac988-572c-4fd3-9be1-4170eb33df73");
        if (option.equals("remove")) {
            writeline("/home/ubuntu/results/coverage/FixCRLF/FixCRLF_4_10.coverage", "fb18a4d5-1ab2-41ff-a485-8cc7def1ddb4");
            c.setValue("lf");
        } else if (option.equals("asis")) {
            writeline("/home/ubuntu/results/coverage/FixCRLF/FixCRLF_4_10.coverage", "d4cd634c-997a-4c79-bbec-1ce7ca35b527");
            c.setValue("asis");
        } else {
            writeline("/home/ubuntu/results/coverage/FixCRLF/FixCRLF_4_10.coverage", "1b7b5ea4-062d-4155-80be-2a9af51e2085");
            // must be "add"
            c.setValue("crlf");
        }
        writeline("/home/ubuntu/results/coverage/FixCRLF/FixCRLF_4_10.coverage", "349951ec-a694-46f0-82f9-6ea113736f1d");
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
        writeline("/home/ubuntu/results/coverage/FixCRLF/FixCRLF_4_10.coverage", "baa57dd0-bf8f-4df4-92d0-2dc108f57d4e");
        filter.setTab(FixCrLfFilter.AddAsisRemove.newInstance(attr.getValue()));
    }

    /**
     * Specify tab length in characters.
     *
     * @param tlength specify the length of tab in spaces.
     * @throws BuildException on error.
     */
    public void setTablength(int tlength) throws BuildException {
        writeline("/home/ubuntu/results/coverage/FixCRLF/FixCRLF_4_10.coverage", "b3761889-998e-49c5-b95a-bb1d9aa475ce");
        try {
            writeline("/home/ubuntu/results/coverage/FixCRLF/FixCRLF_4_10.coverage", "0812cb84-81c7-4523-8849-689cfc75418c");
            filter.setTablength(tlength);
        } catch (IOException e) {
            writeline("/home/ubuntu/results/coverage/FixCRLF/FixCRLF_4_10.coverage", "9a8a81fc-b41b-4d3f-84cb-6967ff85d3a3");
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
        writeline("/home/ubuntu/results/coverage/FixCRLF/FixCRLF_4_10.coverage", "f948457a-6f5d-42f3-b223-cbff10721662");
        filter.setEof(FixCrLfFilter.AddAsisRemove.newInstance(attr.getValue()));
    }

    /**
     * Specifies the encoding Ant expects the files to be
     * in--defaults to the platforms default encoding.
     * @param encoding String encoding name.
     */
    public void setEncoding(String encoding) {
        writeline("/home/ubuntu/results/coverage/FixCRLF/FixCRLF_4_10.coverage", "6e2b21a2-fa7c-48c5-b0d9-7043d55d5d64");
        this.encoding = encoding;
    }

    /**
     * Specifies the encoding that the files are
     * to be written in--same as input encoding by default.
     * @param outputEncoding String outputEncoding name.
     */
    public void setOutputEncoding(String outputEncoding) {
        writeline("/home/ubuntu/results/coverage/FixCRLF/FixCRLF_4_10.coverage", "3736b03a-e131-45e7-9838-25114f6ca619");
        this.outputEncoding = outputEncoding;
    }

    /**
     * Specify whether a missing EOL will be added
     * to the final line of a file.
     * @param fixlast whether to fix the last line.
     */
    public void setFixlast(boolean fixlast) {
        writeline("/home/ubuntu/results/coverage/FixCRLF/FixCRLF_4_10.coverage", "d475970f-466b-4261-a31d-7d8fced53b43");
        filter.setFixlast(fixlast);
    }

    /**
     * Set whether to preserve the last modified time as the original files.
     * @param preserve true if timestamps should be preserved.
     * @since Ant 1.6.3
     */
    public void setPreserveLastModified(boolean preserve) {
        writeline("/home/ubuntu/results/coverage/FixCRLF/FixCRLF_4_10.coverage", "fd7174b2-a960-45a6-85d1-1b04645fbd77");
        preserveLastModified = preserve;
    }

    /**
     * Executes the task.
     * @throws BuildException on error.
     */
    public void execute() throws BuildException {
        writeline("/home/ubuntu/results/coverage/FixCRLF/FixCRLF_4_10.coverage", "309f4d45-3276-4091-941d-dc2f72a67b72");
        // first off, make sure that we've got a srcdir and destdir
        validate();
        writeline("/home/ubuntu/results/coverage/FixCRLF/FixCRLF_4_10.coverage", "9e42d9d9-83b8-4137-b186-6b93e26cda37");
        // log options used
        String enc = encoding == null ? "default" : encoding;
        writeline("/home/ubuntu/results/coverage/FixCRLF/FixCRLF_4_10.coverage", "374e529f-79cd-47b4-acf3-609abb477cba");
        log("options:" + " eol=" + filter.getEol().getValue() + " tab=" + filter.getTab().getValue() + " eof=" + filter.getEof().getValue() + " tablength=" + filter.getTablength() + " encoding=" + enc + " outputencoding=" + (outputEncoding == null ? enc : outputEncoding), Project.MSG_VERBOSE);
        writeline("/home/ubuntu/results/coverage/FixCRLF/FixCRLF_4_10.coverage", "a5bc4b58-11fd-4f66-b9d7-f9ef3df475f3");
        DirectoryScanner ds = super.getDirectoryScanner(srcDir);
        writeline("/home/ubuntu/results/coverage/FixCRLF/FixCRLF_4_10.coverage", "90134e13-2a65-481e-977a-2c4766231d4a");
        String[] files = ds.getIncludedFiles();
        writeline("/home/ubuntu/results/coverage/FixCRLF/FixCRLF_4_10.coverage", "51bcda08-617e-4975-9f98-ca600ddfa940");
        for (int i = 0; i < files.length; i++) {
            writeline("/home/ubuntu/results/coverage/FixCRLF/FixCRLF_4_10.coverage", "c1cbd68f-f49f-4120-9d0b-e0d9e73fa8df");
            processFile(files[i]);
        }
    }

    private void validate() throws BuildException {
        writeline("/home/ubuntu/results/coverage/FixCRLF/FixCRLF_4_10.coverage", "f6f8719b-3bfd-439f-b469-ed9a1e586e47");
        if (file != null) {
            writeline("/home/ubuntu/results/coverage/FixCRLF/FixCRLF_4_10.coverage", "77729b56-5bf4-421d-81f5-6eec7427528c");
            if (srcDir != null) {
                writeline("/home/ubuntu/results/coverage/FixCRLF/FixCRLF_4_10.coverage", "44d89f38-6806-4a0b-b3b7-52918a5a14f0");
                throw new BuildException(ERROR_FILE_AND_SRCDIR);
            }
            writeline("/home/ubuntu/results/coverage/FixCRLF/FixCRLF_4_10.coverage", "8c129f76-fd37-4916-832e-8ab3acc46e02");
            // patch file into the fileset
            fileset.setFile(file);
            writeline("/home/ubuntu/results/coverage/FixCRLF/FixCRLF_4_10.coverage", "ee9b4c48-237f-4dd1-a716-06a7b96c4298");
            // set our parent dir
            srcDir = file.getParentFile();
        }
        writeline("/home/ubuntu/results/coverage/FixCRLF/FixCRLF_4_10.coverage", "8be977a4-c2d3-4d37-8024-c40cc7d6da5f");
        if (srcDir == null) {
            writeline("/home/ubuntu/results/coverage/FixCRLF/FixCRLF_4_10.coverage", "e410ec02-fd6a-4910-b973-ddaa2f7b40db");
            throw new BuildException(FIXCRLF_ERROR + "srcdir attribute must be set!");
        }
        writeline("/home/ubuntu/results/coverage/FixCRLF/FixCRLF_4_10.coverage", "11c2b340-c847-4cd4-b81c-d3a59da7d190");
        if (!srcDir.exists()) {
            writeline("/home/ubuntu/results/coverage/FixCRLF/FixCRLF_4_10.coverage", "265058c2-f5f8-469a-ac6e-89fe36ed6755");
            throw new BuildException(FIXCRLF_ERROR + "srcdir does not exist: '" + srcDir + "'");
        }
        writeline("/home/ubuntu/results/coverage/FixCRLF/FixCRLF_4_10.coverage", "5f770dc1-76d9-4ec0-b25f-68ddeddc26c5");
        if (!srcDir.isDirectory()) {
            writeline("/home/ubuntu/results/coverage/FixCRLF/FixCRLF_4_10.coverage", "e37df528-60d2-4eb3-9ac1-b87afb9f1276");
            throw new BuildException(FIXCRLF_ERROR + "srcdir is not a directory: '" + srcDir + "'");
        }
        writeline("/home/ubuntu/results/coverage/FixCRLF/FixCRLF_4_10.coverage", "564bcfd9-9477-49a0-aafe-1b903fd0d9ba");
        if (destDir != null) {
            writeline("/home/ubuntu/results/coverage/FixCRLF/FixCRLF_4_10.coverage", "9ef684b4-3683-4771-8797-626c79daca63");
            if (!destDir.exists()) {
                writeline("/home/ubuntu/results/coverage/FixCRLF/FixCRLF_4_10.coverage", "6c9d771e-d67c-4e36-a135-0960709aaf02");
                throw new BuildException(FIXCRLF_ERROR + "destdir does not exist: '" + destDir + "'");
            }
            writeline("/home/ubuntu/results/coverage/FixCRLF/FixCRLF_4_10.coverage", "c6c04580-2d54-4eae-bc0b-c0555b06d0ba");
            if (!destDir.isDirectory()) {
                writeline("/home/ubuntu/results/coverage/FixCRLF/FixCRLF_4_10.coverage", "90eac2ae-98c7-4234-9ebd-8c1cc30e2d63");
                throw new BuildException(FIXCRLF_ERROR + "destdir is not a directory: '" + destDir + "'");
            }
        }
    }

    private void processFile(String file) throws BuildException {
        writeline("/home/ubuntu/results/coverage/FixCRLF/FixCRLF_4_10.coverage", "cada8fdc-6bb1-45f8-a55c-f560fddf242c");
        File srcFile = new File(srcDir, file);
        writeline("/home/ubuntu/results/coverage/FixCRLF/FixCRLF_4_10.coverage", "cfcad3c5-110a-4031-a3f7-cb1c91a6a49f");
        long lastModified = srcFile.lastModified();
        writeline("/home/ubuntu/results/coverage/FixCRLF/FixCRLF_4_10.coverage", "f7f78591-205b-4daf-8bda-576ed55072d3");
        File destD = destDir == null ? srcDir : destDir;
        writeline("/home/ubuntu/results/coverage/FixCRLF/FixCRLF_4_10.coverage", "e6913b15-0df8-4523-a9c0-73894bb3f23c");
        if (fcv == null) {
            writeline("/home/ubuntu/results/coverage/FixCRLF/FixCRLF_4_10.coverage", "8a81c3b9-6e2c-4d33-b71c-794ea4b80974");
            FilterChain fc = new FilterChain();
            writeline("/home/ubuntu/results/coverage/FixCRLF/FixCRLF_4_10.coverage", "0484ecee-c5df-45dc-8a50-f333ea285aeb");
            fc.add(filter);
            writeline("/home/ubuntu/results/coverage/FixCRLF/FixCRLF_4_10.coverage", "0a1cb034-0bb7-48c4-8a38-6e82d40cc5e5");
            fcv = new Vector<FilterChain>(1);
            writeline("/home/ubuntu/results/coverage/FixCRLF/FixCRLF_4_10.coverage", "ba30b5bd-5421-4cb9-94ec-c3f6cde938c7");
            fcv.add(fc);
        }
        writeline("/home/ubuntu/results/coverage/FixCRLF/FixCRLF_4_10.coverage", "a4a5cac5-9b59-4280-bf59-92f5ae4fb7e9");
        File tmpFile = FILE_UTILS.createTempFile("fixcrlf", "", null, true, true);
        writeline("/home/ubuntu/results/coverage/FixCRLF/FixCRLF_4_10.coverage", "fac3d9f4-b5de-4f15-b396-7ee836e92270");
        try {
            writeline("/home/ubuntu/results/coverage/FixCRLF/FixCRLF_4_10.coverage", "ac521122-6557-4c58-8907-dd3ce5c2f2f9");
            FILE_UTILS.copyFile(srcFile, tmpFile, null, fcv, true, false, encoding, outputEncoding == null ? encoding : outputEncoding, getProject());
            writeline("/home/ubuntu/results/coverage/FixCRLF/FixCRLF_4_10.coverage", "600f9ed1-a267-432f-aa5f-46a9eac05394");
            File destFile = new File(destD, file);
            writeline("/home/ubuntu/results/coverage/FixCRLF/FixCRLF_4_10.coverage", "b4c61d49-16f9-403e-967b-a8883eb0f1f5");
            boolean destIsWrong = true;
            writeline("/home/ubuntu/results/coverage/FixCRLF/FixCRLF_4_10.coverage", "59814496-9012-4447-b350-066b0c459bd1");
            if (destFile.exists()) {
                writeline("/home/ubuntu/results/coverage/FixCRLF/FixCRLF_4_10.coverage", "cad7378d-f7ef-406d-8d65-38a26d31273b");
                // Compare the destination with the temp file
                log("destFile " + destFile + " exists", Project.MSG_DEBUG);
                writeline("/home/ubuntu/results/coverage/FixCRLF/FixCRLF_4_10.coverage", "5df9a77d-c579-4562-9153-c96be8a30f1c");
                destIsWrong = !FILE_UTILS.contentEquals(destFile, tmpFile);
                writeline("/home/ubuntu/results/coverage/FixCRLF/FixCRLF_4_10.coverage", "c082aa9b-3dab-4bf3-8bb3-8b3d32b51df7");
                log(destFile + (destIsWrong ? " is being written" : " is not written, as the contents are identical"), Project.MSG_DEBUG);
            }
            writeline("/home/ubuntu/results/coverage/FixCRLF/FixCRLF_4_10.coverage", "bf5efa0e-3a00-4835-9347-d56fa953a729");
            if (destIsWrong) {
                writeline("/home/ubuntu/results/coverage/FixCRLF/FixCRLF_4_10.coverage", "02f11bb5-e1ec-4320-b687-131e5b0b3f60");
                FILE_UTILS.rename(tmpFile, destFile);
                writeline("/home/ubuntu/results/coverage/FixCRLF/FixCRLF_4_10.coverage", "75b30591-8c0b-478e-9375-571fbf7ba744");
                if (preserveLastModified) {
                    writeline("/home/ubuntu/results/coverage/FixCRLF/FixCRLF_4_10.coverage", "fc4d360c-8735-412c-a956-0786bc530a89");
                    log("preserved lastModified for " + destFile, Project.MSG_DEBUG);
                    writeline("/home/ubuntu/results/coverage/FixCRLF/FixCRLF_4_10.coverage", "8e76e6e0-18f1-43ed-9409-b28dffaecf4f");
                    FILE_UTILS.setFileLastModified(destFile, lastModified);
                }
            }
        } catch (IOException e) {
            writeline("/home/ubuntu/results/coverage/FixCRLF/FixCRLF_4_10.coverage", "950c8d80-aa81-4bc5-82f0-6cf07f22f9ea");
            throw new BuildException("error running fixcrlf on file " + srcFile, e);
        } finally {
            writeline("/home/ubuntu/results/coverage/FixCRLF/FixCRLF_4_10.coverage", "b2260a8c-e513-4e49-a421-7ad6f4d88d1f");
            if (tmpFile != null && tmpFile.exists()) {
                writeline("/home/ubuntu/results/coverage/FixCRLF/FixCRLF_4_10.coverage", "33dc400c-2cb8-4d7a-8906-bc4d02d7f368");
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
