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
        writeline("/home/ubuntu/results/coverage/FixCRLF/FixCRLF_7_10.coverage", "1a7a730c-6981-4818-9400-28ddcd1032f4");
        return filter.chain(rdr);
    }

    /**
     * Set the source dir to find the source text files.
     * @param srcDir the source directory.
     */
    public void setSrcdir(File srcDir) {
        writeline("/home/ubuntu/results/coverage/FixCRLF/FixCRLF_7_10.coverage", "cbd9bd24-f0dd-4800-b093-e7e18b52d961");
        this.srcDir = srcDir;
    }

    /**
     * Set the destination where the fixed files should be placed.
     * Default is to replace the original file.
     * @param destDir the destination directory.
     */
    public void setDestdir(File destDir) {
        writeline("/home/ubuntu/results/coverage/FixCRLF/FixCRLF_7_10.coverage", "96164266-78f9-46c2-9ea2-aec6feefba34");
        this.destDir = destDir;
    }

    /**
     * Set to true if modifying Java source files.
     * @param javafiles whether modifying Java files.
     */
    public void setJavafiles(boolean javafiles) {
        writeline("/home/ubuntu/results/coverage/FixCRLF/FixCRLF_7_10.coverage", "cfcd9bd8-abae-44d4-87b5-e6e5eb3131de");
        filter.setJavafiles(javafiles);
    }

    /**
     * Set a single file to convert.
     * @since Ant 1.6.3
     * @param file the file to convert.
     */
    public void setFile(File file) {
        writeline("/home/ubuntu/results/coverage/FixCRLF/FixCRLF_7_10.coverage", "0329ea70-4803-495d-b9f5-9147800522c9");
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
        writeline("/home/ubuntu/results/coverage/FixCRLF/FixCRLF_7_10.coverage", "01fb1b57-2569-4ba0-90e9-c4b246b79c4f");
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
        writeline("/home/ubuntu/results/coverage/FixCRLF/FixCRLF_7_10.coverage", "048ff383-a8aa-4f5c-a05c-4451014959c0");
        log("DEPRECATED: The cr attribute has been deprecated,", Project.MSG_WARN);
        writeline("/home/ubuntu/results/coverage/FixCRLF/FixCRLF_7_10.coverage", "c760b8c3-4eae-4258-b0ae-c3e92292f881");
        log("Please use the eol attribute instead", Project.MSG_WARN);
        writeline("/home/ubuntu/results/coverage/FixCRLF/FixCRLF_7_10.coverage", "2bff66c5-a2df-412c-985e-e4a4e465e9d3");
        String option = attr.getValue();
        writeline("/home/ubuntu/results/coverage/FixCRLF/FixCRLF_7_10.coverage", "aa4e1623-9789-4699-9e72-316dfdbbd610");
        CrLf c = new CrLf();
        writeline("/home/ubuntu/results/coverage/FixCRLF/FixCRLF_7_10.coverage", "eada081a-1d75-40e5-9c64-857c1782d053");
        if (option.equals("remove")) {
            writeline("/home/ubuntu/results/coverage/FixCRLF/FixCRLF_7_10.coverage", "db966034-34e7-44e8-a217-baed0f25e948");
            c.setValue("lf");
        } else if (option.equals("asis")) {
            writeline("/home/ubuntu/results/coverage/FixCRLF/FixCRLF_7_10.coverage", "9743bcb6-59c5-46bf-9406-cf08a8ac61a2");
            c.setValue("asis");
        } else {
            writeline("/home/ubuntu/results/coverage/FixCRLF/FixCRLF_7_10.coverage", "43a5d1c8-23b3-4c99-8340-46e437b3e7c4");
            // must be "add"
            c.setValue("crlf");
        }
        writeline("/home/ubuntu/results/coverage/FixCRLF/FixCRLF_7_10.coverage", "d4215875-8661-4909-ab55-ce3afef03789");
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
        writeline("/home/ubuntu/results/coverage/FixCRLF/FixCRLF_7_10.coverage", "ae273a63-70dc-4d02-acbc-ecec108dd5ad");
        filter.setTab(FixCrLfFilter.AddAsisRemove.newInstance(attr.getValue()));
    }

    /**
     * Specify tab length in characters.
     *
     * @param tlength specify the length of tab in spaces.
     * @throws BuildException on error.
     */
    public void setTablength(int tlength) throws BuildException {
        writeline("/home/ubuntu/results/coverage/FixCRLF/FixCRLF_7_10.coverage", "dd6c8b97-1105-4a44-85b7-0bec5e2c27a6");
        try {
            writeline("/home/ubuntu/results/coverage/FixCRLF/FixCRLF_7_10.coverage", "7307af22-5a20-44a2-ae05-71b944cf1642");
            filter.setTablength(tlength);
        } catch (IOException e) {
            writeline("/home/ubuntu/results/coverage/FixCRLF/FixCRLF_7_10.coverage", "f5855a8d-68f9-4088-9b3b-3b851c3e71b4");
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
        writeline("/home/ubuntu/results/coverage/FixCRLF/FixCRLF_7_10.coverage", "5a52cfc0-9d1b-4403-8167-105e8e922825");
        filter.setEof(FixCrLfFilter.AddAsisRemove.newInstance(attr.getValue()));
    }

    /**
     * Specifies the encoding Ant expects the files to be
     * in--defaults to the platforms default encoding.
     * @param encoding String encoding name.
     */
    public void setEncoding(String encoding) {
        writeline("/home/ubuntu/results/coverage/FixCRLF/FixCRLF_7_10.coverage", "d9df6005-62c3-44b5-94f5-1f51d1bb3618");
        this.encoding = encoding;
    }

    /**
     * Specifies the encoding that the files are
     * to be written in--same as input encoding by default.
     * @param outputEncoding String outputEncoding name.
     */
    public void setOutputEncoding(String outputEncoding) {
        writeline("/home/ubuntu/results/coverage/FixCRLF/FixCRLF_7_10.coverage", "d34149d4-e10c-40f9-b0e7-8087c92602d2");
        this.outputEncoding = outputEncoding;
    }

    /**
     * Specify whether a missing EOL will be added
     * to the final line of a file.
     * @param fixlast whether to fix the last line.
     */
    public void setFixlast(boolean fixlast) {
        writeline("/home/ubuntu/results/coverage/FixCRLF/FixCRLF_7_10.coverage", "5b91d898-1e10-4a48-a8ab-d21075c69eb0");
        filter.setFixlast(fixlast);
    }

    /**
     * Set whether to preserve the last modified time as the original files.
     * @param preserve true if timestamps should be preserved.
     * @since Ant 1.6.3
     */
    public void setPreserveLastModified(boolean preserve) {
        writeline("/home/ubuntu/results/coverage/FixCRLF/FixCRLF_7_10.coverage", "5bb94228-526d-49d5-a985-95ce71760d7a");
        preserveLastModified = preserve;
    }

    /**
     * Executes the task.
     * @throws BuildException on error.
     */
    public void execute() throws BuildException {
        writeline("/home/ubuntu/results/coverage/FixCRLF/FixCRLF_7_10.coverage", "67ab34e0-8416-4f3b-8905-255ac1e95d36");
        // first off, make sure that we've got a srcdir and destdir
        validate();
        writeline("/home/ubuntu/results/coverage/FixCRLF/FixCRLF_7_10.coverage", "1b740503-0a90-46ab-acc5-2f645c6070d3");
        // log options used
        String enc = encoding == null ? "default" : encoding;
        writeline("/home/ubuntu/results/coverage/FixCRLF/FixCRLF_7_10.coverage", "2bc70271-a7fb-4800-86f2-9826496d556c");
        log("options:" + " eol=" + filter.getEol().getValue() + " tab=" + filter.getTab().getValue() + " eof=" + filter.getEof().getValue() + " tablength=" + filter.getTablength() + " encoding=" + enc + " outputencoding=" + (outputEncoding == null ? enc : outputEncoding), Project.MSG_VERBOSE);
        writeline("/home/ubuntu/results/coverage/FixCRLF/FixCRLF_7_10.coverage", "42dbddf3-f959-44cd-a2ae-8a18acc298fd");
        DirectoryScanner ds = super.getDirectoryScanner(srcDir);
        writeline("/home/ubuntu/results/coverage/FixCRLF/FixCRLF_7_10.coverage", "4e23bed3-2a5e-42d1-8c1d-e577f84b4e15");
        String[] files = ds.getIncludedFiles();
        writeline("/home/ubuntu/results/coverage/FixCRLF/FixCRLF_7_10.coverage", "bae88921-6a91-4087-887a-5040506ba8a8");
        for (int i = 0; i < files.length; i++) {
            writeline("/home/ubuntu/results/coverage/FixCRLF/FixCRLF_7_10.coverage", "6169e2a4-8dc0-451b-b3d7-fb5b343cb8d9");
            processFile(files[i]);
        }
    }

    private void validate() throws BuildException {
        writeline("/home/ubuntu/results/coverage/FixCRLF/FixCRLF_7_10.coverage", "10d7ccdd-6a9b-4077-99bd-51aad708494d");
        if (file != null) {
            writeline("/home/ubuntu/results/coverage/FixCRLF/FixCRLF_7_10.coverage", "c9197f99-ccec-44cf-ab97-a290a5634dfd");
            if (srcDir != null) {
                writeline("/home/ubuntu/results/coverage/FixCRLF/FixCRLF_7_10.coverage", "59a98c06-1d86-4a74-8206-71e04ec4716c");
                throw new BuildException(ERROR_FILE_AND_SRCDIR);
            }
            writeline("/home/ubuntu/results/coverage/FixCRLF/FixCRLF_7_10.coverage", "96bb1d46-db96-4d1f-a75c-833bfafb7d55");
            // patch file into the fileset
            fileset.setFile(file);
            writeline("/home/ubuntu/results/coverage/FixCRLF/FixCRLF_7_10.coverage", "3d4c19e3-e8c1-46a7-806e-a86a932ee148");
            // set our parent dir
            srcDir = file.getParentFile();
        }
        writeline("/home/ubuntu/results/coverage/FixCRLF/FixCRLF_7_10.coverage", "ef9d518c-e2e2-44f8-96b8-19e49b07ca12");
        if (srcDir == null) {
            writeline("/home/ubuntu/results/coverage/FixCRLF/FixCRLF_7_10.coverage", "7bf30379-d3ef-43c6-99ee-9679fba587f5");
            throw new BuildException(FIXCRLF_ERROR + "srcdir attribute must be set!");
        }
        writeline("/home/ubuntu/results/coverage/FixCRLF/FixCRLF_7_10.coverage", "d4650854-4d43-457b-9c6f-65186f989880");
        if (!srcDir.exists()) {
            writeline("/home/ubuntu/results/coverage/FixCRLF/FixCRLF_7_10.coverage", "0b52f1ad-f082-468f-bfb5-7fe2e81b8164");
            throw new BuildException(FIXCRLF_ERROR + "srcdir does not exist: '" + srcDir + "'");
        }
        writeline("/home/ubuntu/results/coverage/FixCRLF/FixCRLF_7_10.coverage", "db93db96-cfa7-4f79-bae4-41f3749b4e04");
        if (!srcDir.isDirectory()) {
            writeline("/home/ubuntu/results/coverage/FixCRLF/FixCRLF_7_10.coverage", "bd85148e-d052-472d-850b-05e04194c615");
            throw new BuildException(FIXCRLF_ERROR + "srcdir is not a directory: '" + srcDir + "'");
        }
        writeline("/home/ubuntu/results/coverage/FixCRLF/FixCRLF_7_10.coverage", "a4f42bf8-e7a4-46e4-8ab1-e822301285a8");
        if (destDir != null) {
            writeline("/home/ubuntu/results/coverage/FixCRLF/FixCRLF_7_10.coverage", "97b6d7e2-aa40-4695-b8fa-8964cd6d496b");
            if (!destDir.exists()) {
                writeline("/home/ubuntu/results/coverage/FixCRLF/FixCRLF_7_10.coverage", "87e9342e-0168-4eb4-ad85-0276d00f79ed");
                throw new BuildException(FIXCRLF_ERROR + "destdir does not exist: '" + destDir + "'");
            }
            writeline("/home/ubuntu/results/coverage/FixCRLF/FixCRLF_7_10.coverage", "deab3956-2356-448b-958e-7b6393b089c3");
            if (!destDir.isDirectory()) {
                writeline("/home/ubuntu/results/coverage/FixCRLF/FixCRLF_7_10.coverage", "4a7a65fb-38ad-42ee-b272-d91481f15325");
                throw new BuildException(FIXCRLF_ERROR + "destdir is not a directory: '" + destDir + "'");
            }
        }
    }

    private void processFile(String file) throws BuildException {
        writeline("/home/ubuntu/results/coverage/FixCRLF/FixCRLF_7_10.coverage", "c80bc01b-b625-4756-82e6-141073dae324");
        File srcFile = new File(srcDir, file);
        writeline("/home/ubuntu/results/coverage/FixCRLF/FixCRLF_7_10.coverage", "a5ec5b42-9c26-4661-9720-1603b2b42885");
        long lastModified = srcFile.lastModified();
        writeline("/home/ubuntu/results/coverage/FixCRLF/FixCRLF_7_10.coverage", "8d08a9ab-3fad-4542-9da3-6c01bacef0aa");
        File destD = destDir == null ? srcDir : destDir;
        writeline("/home/ubuntu/results/coverage/FixCRLF/FixCRLF_7_10.coverage", "5d50b2c4-71d2-4150-a0a2-3eceffe7941f");
        if (fcv == null) {
            writeline("/home/ubuntu/results/coverage/FixCRLF/FixCRLF_7_10.coverage", "2ee01e28-87cb-42e7-9053-4ac31102b844");
            FilterChain fc = new FilterChain();
            writeline("/home/ubuntu/results/coverage/FixCRLF/FixCRLF_7_10.coverage", "6ec55987-b03a-4178-bfa3-778ba59e412a");
            fc.add(filter);
            writeline("/home/ubuntu/results/coverage/FixCRLF/FixCRLF_7_10.coverage", "f92ed40c-15a6-4fd1-904d-71fc9e953e88");
            fcv = new Vector<FilterChain>(1);
            writeline("/home/ubuntu/results/coverage/FixCRLF/FixCRLF_7_10.coverage", "97ef17ed-c609-427d-b199-c3fe26371388");
            fcv.add(fc);
        }
        writeline("/home/ubuntu/results/coverage/FixCRLF/FixCRLF_7_10.coverage", "0684fdf4-158a-4415-871c-0c6c7d3f17a2");
        File tmpFile = FILE_UTILS.createTempFile("fixcrlf", "", null, true, true);
        writeline("/home/ubuntu/results/coverage/FixCRLF/FixCRLF_7_10.coverage", "2941462c-5fdc-49be-801d-43bad9a862a1");
        try {
            writeline("/home/ubuntu/results/coverage/FixCRLF/FixCRLF_7_10.coverage", "983122f3-ba54-4169-8068-b3663e7a4fe9");
            FILE_UTILS.copyFile(srcFile, tmpFile, null, fcv, true, false, encoding, outputEncoding == null ? encoding : outputEncoding, getProject());
            writeline("/home/ubuntu/results/coverage/FixCRLF/FixCRLF_7_10.coverage", "01e039a4-1642-42d4-93c9-af8e86c0e5a0");
            File destFile = new File(destD, file);
            writeline("/home/ubuntu/results/coverage/FixCRLF/FixCRLF_7_10.coverage", "28394b47-eec7-4ac7-ae62-b6897b7c42dc");
            boolean destIsWrong = true;
            writeline("/home/ubuntu/results/coverage/FixCRLF/FixCRLF_7_10.coverage", "f403071c-7bb2-4a02-90b0-75742742e14a");
            if (destFile.exists()) {
                writeline("/home/ubuntu/results/coverage/FixCRLF/FixCRLF_7_10.coverage", "38643f2f-1a6a-4039-b05d-f443398dfe3e");
                // Compare the destination with the temp file
                log("destFile " + destFile + " exists", Project.MSG_DEBUG);
                writeline("/home/ubuntu/results/coverage/FixCRLF/FixCRLF_7_10.coverage", "17f1c8c5-5bd7-4637-a749-38125d0398fd");
                destIsWrong = !FILE_UTILS.contentEquals(destFile, tmpFile);
                writeline("/home/ubuntu/results/coverage/FixCRLF/FixCRLF_7_10.coverage", "92c401c2-4028-4988-92b8-d2b1777caf5d");
                log(destFile + (destIsWrong ? " is being written" : " is not written, as the contents are identical"), Project.MSG_DEBUG);
            }
            writeline("/home/ubuntu/results/coverage/FixCRLF/FixCRLF_7_10.coverage", "1c8c4b7f-0f3a-4997-9a46-c5087081fef7");
            if (destIsWrong) {
                writeline("/home/ubuntu/results/coverage/FixCRLF/FixCRLF_7_10.coverage", "d157b7ac-978d-4a91-a768-7a8a97edfd6e");
                FILE_UTILS.rename(tmpFile, destFile);
                writeline("/home/ubuntu/results/coverage/FixCRLF/FixCRLF_7_10.coverage", "38cd6338-eb6a-40c8-aca2-e95ff1b0710e");
                if (preserveLastModified) {
                    writeline("/home/ubuntu/results/coverage/FixCRLF/FixCRLF_7_10.coverage", "09b4ed50-0933-4aa1-96a7-3353d6ad4c28");
                    log("preserved lastModified for " + destFile, Project.MSG_DEBUG);
                    writeline("/home/ubuntu/results/coverage/FixCRLF/FixCRLF_7_10.coverage", "4f3816d4-92fa-4995-8941-ac2306005a8a");
                    FILE_UTILS.setFileLastModified(destFile, lastModified);
                }
            }
        } catch (IOException e) {
            writeline("/home/ubuntu/results/coverage/FixCRLF/FixCRLF_7_10.coverage", "06f706e9-a6eb-4891-b810-776aeafe2bb1");
            throw new BuildException("error running fixcrlf on file " + srcFile, e);
        } finally {
            writeline("/home/ubuntu/results/coverage/FixCRLF/FixCRLF_7_10.coverage", "171712f2-321e-4894-8b7b-3f5425ef8d02");
            if (tmpFile != null && tmpFile.exists()) {
                writeline("/home/ubuntu/results/coverage/FixCRLF/FixCRLF_7_10.coverage", "1d480170-2485-4c44-aa52-07133a2cc593");
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
