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
        writeline("/home/ubuntu/results/coverage/FixCRLF/FixCRLF_2_10.coverage", "5230fd9f-731b-4602-837d-79596b7be5b4");
        return filter.chain(rdr);
    }

    /**
     * Set the source dir to find the source text files.
     * @param srcDir the source directory.
     */
    public void setSrcdir(File srcDir) {
        writeline("/home/ubuntu/results/coverage/FixCRLF/FixCRLF_2_10.coverage", "6479c647-e8df-49d8-a463-2e1c612552aa");
        this.srcDir = srcDir;
    }

    /**
     * Set the destination where the fixed files should be placed.
     * Default is to replace the original file.
     * @param destDir the destination directory.
     */
    public void setDestdir(File destDir) {
        writeline("/home/ubuntu/results/coverage/FixCRLF/FixCRLF_2_10.coverage", "6e156239-9732-41ed-8d91-698136cfeee2");
        this.destDir = destDir;
    }

    /**
     * Set to true if modifying Java source files.
     * @param javafiles whether modifying Java files.
     */
    public void setJavafiles(boolean javafiles) {
        writeline("/home/ubuntu/results/coverage/FixCRLF/FixCRLF_2_10.coverage", "1a1a0607-30b4-486f-935a-3c4fe05a6286");
        filter.setJavafiles(javafiles);
    }

    /**
     * Set a single file to convert.
     * @since Ant 1.6.3
     * @param file the file to convert.
     */
    public void setFile(File file) {
        writeline("/home/ubuntu/results/coverage/FixCRLF/FixCRLF_2_10.coverage", "d82e4ce3-0e08-4ff7-9da9-27efd18ba89f");
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
        writeline("/home/ubuntu/results/coverage/FixCRLF/FixCRLF_2_10.coverage", "82cb190e-1a7e-49c3-8f02-8efaf68e736c");
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
        writeline("/home/ubuntu/results/coverage/FixCRLF/FixCRLF_2_10.coverage", "bf1be294-da39-4fb6-984a-f028ab65ec3e");
        log("DEPRECATED: The cr attribute has been deprecated,", Project.MSG_WARN);
        writeline("/home/ubuntu/results/coverage/FixCRLF/FixCRLF_2_10.coverage", "647921df-90d8-4627-9c78-45c03fbcfe26");
        log("Please use the eol attribute instead", Project.MSG_WARN);
        writeline("/home/ubuntu/results/coverage/FixCRLF/FixCRLF_2_10.coverage", "bc816d65-7ca3-4868-8367-257b0ea00062");
        String option = attr.getValue();
        writeline("/home/ubuntu/results/coverage/FixCRLF/FixCRLF_2_10.coverage", "7cb600c7-1bdf-4651-b9f0-c5edde4910ca");
        CrLf c = new CrLf();
        writeline("/home/ubuntu/results/coverage/FixCRLF/FixCRLF_2_10.coverage", "0c3ad503-fe9e-4146-959c-4046cd1ea7a1");
        if (option.equals("remove")) {
            writeline("/home/ubuntu/results/coverage/FixCRLF/FixCRLF_2_10.coverage", "d379e906-7208-446d-a4c7-96d73230beda");
            c.setValue("lf");
        } else if (option.equals("asis")) {
            writeline("/home/ubuntu/results/coverage/FixCRLF/FixCRLF_2_10.coverage", "afce52e1-9238-45e7-82da-9f5a08631cdc");
            c.setValue("asis");
        } else {
            writeline("/home/ubuntu/results/coverage/FixCRLF/FixCRLF_2_10.coverage", "c158b087-2b0f-4a91-a21f-27e40d3a6700");
            // must be "add"
            c.setValue("crlf");
        }
        writeline("/home/ubuntu/results/coverage/FixCRLF/FixCRLF_2_10.coverage", "eeaef8cf-822e-487d-b184-f57fbd9f3293");
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
        writeline("/home/ubuntu/results/coverage/FixCRLF/FixCRLF_2_10.coverage", "d49f24b5-bb56-4cae-9a28-d6ca01b9ad69");
        filter.setTab(FixCrLfFilter.AddAsisRemove.newInstance(attr.getValue()));
    }

    /**
     * Specify tab length in characters.
     *
     * @param tlength specify the length of tab in spaces.
     * @throws BuildException on error.
     */
    public void setTablength(int tlength) throws BuildException {
        writeline("/home/ubuntu/results/coverage/FixCRLF/FixCRLF_2_10.coverage", "14566b9a-9f4e-45b2-9ad1-75914903682f");
        try {
            writeline("/home/ubuntu/results/coverage/FixCRLF/FixCRLF_2_10.coverage", "13489bd5-78b5-4adc-9175-0177ccef4b19");
            filter.setTablength(tlength);
        } catch (IOException e) {
            writeline("/home/ubuntu/results/coverage/FixCRLF/FixCRLF_2_10.coverage", "37f431d7-8fab-40c2-ab05-06da5e1ba429");
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
        writeline("/home/ubuntu/results/coverage/FixCRLF/FixCRLF_2_10.coverage", "41f45f60-c9f3-46c0-ad51-5022743652ef");
        filter.setEof(FixCrLfFilter.AddAsisRemove.newInstance(attr.getValue()));
    }

    /**
     * Specifies the encoding Ant expects the files to be
     * in--defaults to the platforms default encoding.
     * @param encoding String encoding name.
     */
    public void setEncoding(String encoding) {
        writeline("/home/ubuntu/results/coverage/FixCRLF/FixCRLF_2_10.coverage", "2c746098-9421-403b-a540-4e79e74dec15");
        this.encoding = encoding;
    }

    /**
     * Specifies the encoding that the files are
     * to be written in--same as input encoding by default.
     * @param outputEncoding String outputEncoding name.
     */
    public void setOutputEncoding(String outputEncoding) {
        writeline("/home/ubuntu/results/coverage/FixCRLF/FixCRLF_2_10.coverage", "c7713a60-435c-4897-a18e-36c6394723ec");
        this.outputEncoding = outputEncoding;
    }

    /**
     * Specify whether a missing EOL will be added
     * to the final line of a file.
     * @param fixlast whether to fix the last line.
     */
    public void setFixlast(boolean fixlast) {
        writeline("/home/ubuntu/results/coverage/FixCRLF/FixCRLF_2_10.coverage", "8087fbda-efa0-470f-925e-d4221ccc8893");
        filter.setFixlast(fixlast);
    }

    /**
     * Set whether to preserve the last modified time as the original files.
     * @param preserve true if timestamps should be preserved.
     * @since Ant 1.6.3
     */
    public void setPreserveLastModified(boolean preserve) {
        writeline("/home/ubuntu/results/coverage/FixCRLF/FixCRLF_2_10.coverage", "8832292c-36fe-4a71-b894-3bd846c12cdc");
        preserveLastModified = preserve;
    }

    /**
     * Executes the task.
     * @throws BuildException on error.
     */
    public void execute() throws BuildException {
        writeline("/home/ubuntu/results/coverage/FixCRLF/FixCRLF_2_10.coverage", "306de5e0-6be2-4413-ad11-6f7871b62f9a");
        // first off, make sure that we've got a srcdir and destdir
        validate();
        writeline("/home/ubuntu/results/coverage/FixCRLF/FixCRLF_2_10.coverage", "3987fa90-4b78-4308-ba3b-3e9f03c27406");
        // log options used
        String enc = encoding == null ? "default" : encoding;
        writeline("/home/ubuntu/results/coverage/FixCRLF/FixCRLF_2_10.coverage", "3cc822da-414e-402f-b014-67e95c18a40a");
        log("options:" + " eol=" + filter.getEol().getValue() + " tab=" + filter.getTab().getValue() + " eof=" + filter.getEof().getValue() + " tablength=" + filter.getTablength() + " encoding=" + enc + " outputencoding=" + (outputEncoding == null ? enc : outputEncoding), Project.MSG_VERBOSE);
        writeline("/home/ubuntu/results/coverage/FixCRLF/FixCRLF_2_10.coverage", "c8d6137d-f044-4510-83e4-1c6a7fe8ab02");
        DirectoryScanner ds = super.getDirectoryScanner(srcDir);
        writeline("/home/ubuntu/results/coverage/FixCRLF/FixCRLF_2_10.coverage", "c5e951c4-fac5-49d2-a327-40c4400e8ba8");
        String[] files = ds.getIncludedFiles();
        writeline("/home/ubuntu/results/coverage/FixCRLF/FixCRLF_2_10.coverage", "085d6d95-7d6e-4fe5-88d9-9ba70b9d7821");
        for (int i = 0; i < files.length; i++) {
            writeline("/home/ubuntu/results/coverage/FixCRLF/FixCRLF_2_10.coverage", "8fc26caf-975e-4c9c-abdd-294ee0877c11");
            processFile(files[i]);
        }
    }

    private void validate() throws BuildException {
        writeline("/home/ubuntu/results/coverage/FixCRLF/FixCRLF_2_10.coverage", "66f8d1f4-a348-4fe1-905b-47f09681587d");
        if (file != null) {
            writeline("/home/ubuntu/results/coverage/FixCRLF/FixCRLF_2_10.coverage", "580af175-d651-49e5-8e05-e8512774c47e");
            if (srcDir != null) {
                writeline("/home/ubuntu/results/coverage/FixCRLF/FixCRLF_2_10.coverage", "03fd43f6-7b59-485f-ba48-968a896d9a2c");
                throw new BuildException(ERROR_FILE_AND_SRCDIR);
            }
            writeline("/home/ubuntu/results/coverage/FixCRLF/FixCRLF_2_10.coverage", "75535ae9-b79f-4692-9a30-e631e206390c");
            // patch file into the fileset
            fileset.setFile(file);
            writeline("/home/ubuntu/results/coverage/FixCRLF/FixCRLF_2_10.coverage", "af7ba79e-649f-42a6-b614-4c34437eedff");
            // set our parent dir
            srcDir = file.getParentFile();
        }
        writeline("/home/ubuntu/results/coverage/FixCRLF/FixCRLF_2_10.coverage", "ae8dfbcf-93fb-4658-9243-78ecfe3d60a8");
        if (srcDir == null) {
            writeline("/home/ubuntu/results/coverage/FixCRLF/FixCRLF_2_10.coverage", "3014fba9-d595-4483-b14e-5d5eb37d0604");
            throw new BuildException(FIXCRLF_ERROR + "srcdir attribute must be set!");
        }
        writeline("/home/ubuntu/results/coverage/FixCRLF/FixCRLF_2_10.coverage", "3752779e-6561-4018-a7d0-caf412c85517");
        if (!srcDir.exists()) {
            writeline("/home/ubuntu/results/coverage/FixCRLF/FixCRLF_2_10.coverage", "c1069a75-bcbd-4cfc-a36c-5eddb63f78fa");
            throw new BuildException(FIXCRLF_ERROR + "srcdir does not exist: '" + srcDir + "'");
        }
        writeline("/home/ubuntu/results/coverage/FixCRLF/FixCRLF_2_10.coverage", "a474f3f4-d6b9-4504-a510-5aa5e926de01");
        if (!srcDir.isDirectory()) {
            writeline("/home/ubuntu/results/coverage/FixCRLF/FixCRLF_2_10.coverage", "f21f2a97-590b-40f8-ad85-6600e370be97");
            throw new BuildException(FIXCRLF_ERROR + "srcdir is not a directory: '" + srcDir + "'");
        }
        writeline("/home/ubuntu/results/coverage/FixCRLF/FixCRLF_2_10.coverage", "ee48da76-db8c-425b-b24b-7085c3110dc8");
        if (destDir != null) {
            writeline("/home/ubuntu/results/coverage/FixCRLF/FixCRLF_2_10.coverage", "b6d6edad-2bdd-49f2-8186-f4f77966fa72");
            if (!destDir.exists()) {
                writeline("/home/ubuntu/results/coverage/FixCRLF/FixCRLF_2_10.coverage", "6b78669a-115c-4ac3-8cd1-88596d68bddd");
                throw new BuildException(FIXCRLF_ERROR + "destdir does not exist: '" + destDir + "'");
            }
            writeline("/home/ubuntu/results/coverage/FixCRLF/FixCRLF_2_10.coverage", "344f569a-20a3-44d7-85ac-2e7c1da19a7a");
            if (!destDir.isDirectory()) {
                writeline("/home/ubuntu/results/coverage/FixCRLF/FixCRLF_2_10.coverage", "fc3fc4f4-f2e2-4ac6-9af7-bc3c3632115a");
                throw new BuildException(FIXCRLF_ERROR + "destdir is not a directory: '" + destDir + "'");
            }
        }
    }

    private void processFile(String file) throws BuildException {
        writeline("/home/ubuntu/results/coverage/FixCRLF/FixCRLF_2_10.coverage", "06800003-cf6c-4664-b60c-d19776d7ab12");
        File srcFile = new File(srcDir, file);
        writeline("/home/ubuntu/results/coverage/FixCRLF/FixCRLF_2_10.coverage", "19b17a9c-91ec-455e-8036-0600531693fe");
        long lastModified = srcFile.lastModified();
        writeline("/home/ubuntu/results/coverage/FixCRLF/FixCRLF_2_10.coverage", "1c5dfd23-3ce3-42e6-8e5c-bf29a05d6100");
        File destD = destDir == null ? srcDir : destDir;
        writeline("/home/ubuntu/results/coverage/FixCRLF/FixCRLF_2_10.coverage", "c4e9104a-149a-45ba-9eeb-9f71849d5a26");
        if (fcv == null) {
            writeline("/home/ubuntu/results/coverage/FixCRLF/FixCRLF_2_10.coverage", "36e33af5-b49f-4556-bc7a-9e3ae9813407");
            FilterChain fc = new FilterChain();
            writeline("/home/ubuntu/results/coverage/FixCRLF/FixCRLF_2_10.coverage", "d07ef1b9-d65c-4577-94cd-849c199c5c95");
            fc.add(filter);
            writeline("/home/ubuntu/results/coverage/FixCRLF/FixCRLF_2_10.coverage", "cf14ef56-49a7-49e2-b668-8072699d4203");
            fcv = new Vector<FilterChain>(1);
            writeline("/home/ubuntu/results/coverage/FixCRLF/FixCRLF_2_10.coverage", "9caa7c09-fb1c-4aae-bc2a-f39245fff678");
            fcv.add(fc);
        }
        writeline("/home/ubuntu/results/coverage/FixCRLF/FixCRLF_2_10.coverage", "47d726ba-d42e-4217-91a8-383561b686bf");
        File tmpFile = FILE_UTILS.createTempFile("fixcrlf", "", null, true, true);
        writeline("/home/ubuntu/results/coverage/FixCRLF/FixCRLF_2_10.coverage", "067306f0-2c42-4ec2-97f8-382b1d7e65dd");
        try {
            writeline("/home/ubuntu/results/coverage/FixCRLF/FixCRLF_2_10.coverage", "3200111b-de5a-4e0d-8019-3d50e8b08346");
            FILE_UTILS.copyFile(srcFile, tmpFile, null, fcv, true, false, encoding, outputEncoding == null ? encoding : outputEncoding, getProject());
            writeline("/home/ubuntu/results/coverage/FixCRLF/FixCRLF_2_10.coverage", "f45122ba-4dbf-4139-b7f7-65b22883e3a6");
            File destFile = new File(destD, file);
            writeline("/home/ubuntu/results/coverage/FixCRLF/FixCRLF_2_10.coverage", "96fce85d-8a93-4629-b80c-4b58b868739e");
            boolean destIsWrong = true;
            writeline("/home/ubuntu/results/coverage/FixCRLF/FixCRLF_2_10.coverage", "1b8d481b-c8a7-46e2-9f37-5de64f29ab8d");
            if (destFile.exists()) {
                writeline("/home/ubuntu/results/coverage/FixCRLF/FixCRLF_2_10.coverage", "d9c3d987-0128-4606-b2a7-e31b6e6d5b34");
                // Compare the destination with the temp file
                log("destFile " + destFile + " exists", Project.MSG_DEBUG);
                writeline("/home/ubuntu/results/coverage/FixCRLF/FixCRLF_2_10.coverage", "652a5be2-4b87-4e82-8b1c-79aab6347778");
                destIsWrong = !FILE_UTILS.contentEquals(destFile, tmpFile);
                writeline("/home/ubuntu/results/coverage/FixCRLF/FixCRLF_2_10.coverage", "e5174d85-a625-4386-b86e-81b3073e1f8e");
                log(destFile + (destIsWrong ? " is being written" : " is not written, as the contents are identical"), Project.MSG_DEBUG);
            }
            writeline("/home/ubuntu/results/coverage/FixCRLF/FixCRLF_2_10.coverage", "259c2b2a-3acf-4f7e-9dd9-0be5fe302375");
            if (destIsWrong) {
                writeline("/home/ubuntu/results/coverage/FixCRLF/FixCRLF_2_10.coverage", "764bc6cf-1cf6-4cfa-8502-598c8c2ac73b");
                FILE_UTILS.rename(tmpFile, destFile);
                writeline("/home/ubuntu/results/coverage/FixCRLF/FixCRLF_2_10.coverage", "84535c05-7cf5-496c-8356-e63f007726a7");
                if (preserveLastModified) {
                    writeline("/home/ubuntu/results/coverage/FixCRLF/FixCRLF_2_10.coverage", "998e4341-41bd-452d-9f46-810bb1cd94e0");
                    log("preserved lastModified for " + destFile, Project.MSG_DEBUG);
                    writeline("/home/ubuntu/results/coverage/FixCRLF/FixCRLF_2_10.coverage", "db70cd32-4191-4705-886b-ac1ea6865068");
                    FILE_UTILS.setFileLastModified(destFile, lastModified);
                }
            }
        } catch (IOException e) {
            writeline("/home/ubuntu/results/coverage/FixCRLF/FixCRLF_2_10.coverage", "df4fb2df-ced3-4439-a4b3-d4154254b171");
            throw new BuildException("error running fixcrlf on file " + srcFile, e);
        } finally {
            writeline("/home/ubuntu/results/coverage/FixCRLF/FixCRLF_2_10.coverage", "b55f2b52-181a-45d4-b534-3c096345538d");
            if (tmpFile != null && tmpFile.exists()) {
                writeline("/home/ubuntu/results/coverage/FixCRLF/FixCRLF_2_10.coverage", "ddb0c33c-be74-45cc-80d7-cf07ec804947");
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
