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

import java.io.File;
import java.io.IOException;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Hashtable;
import java.util.List;
import java.util.Map;
import java.util.Vector;
import org.apache.tools.ant.BuildException;
import org.apache.tools.ant.DirectoryScanner;
import org.apache.tools.ant.Project;
import org.apache.tools.ant.Task;
import org.apache.tools.ant.types.FileSet;
import org.apache.tools.ant.types.FilterChain;
import org.apache.tools.ant.types.FilterSet;
import org.apache.tools.ant.types.FilterSetCollection;
import org.apache.tools.ant.types.Mapper;
import org.apache.tools.ant.types.Resource;
import org.apache.tools.ant.types.ResourceCollection;
import org.apache.tools.ant.types.ResourceFactory;
import org.apache.tools.ant.types.resources.FileProvider;
import org.apache.tools.ant.types.resources.FileResource;
import org.apache.tools.ant.util.FileNameMapper;
import org.apache.tools.ant.util.FileUtils;
import org.apache.tools.ant.util.FlatFileNameMapper;
import org.apache.tools.ant.util.IdentityMapper;
import org.apache.tools.ant.util.LinkedHashtable;
import org.apache.tools.ant.util.ResourceUtils;
import org.apache.tools.ant.util.SourceFileScanner;
import java.io.*;

/**
 * <p>Copies a file or directory to a new file
 * or directory.  Files are only copied if the source file is newer
 * than the destination file, or when the destination file does not
 * exist.  It is possible to explicitly overwrite existing files.</p>
 *
 * <p>This implementation is based on Arnout Kuiper's initial design
 * document, the following mailing list discussions, and the
 * copyfile/copydir tasks.</p>
 *
 * @since Ant 1.2
 *
 * @ant.task category="filesystem"
 */
public class Copy extends Task {

    private static final String MSG_WHEN_COPYING_EMPTY_RC_TO_FILE = "Cannot perform operation from directory to file.";

    static final File NULL_FILE_PLACEHOLDER = new File("/NULL_FILE");

    static final String LINE_SEPARATOR = System.getProperty("line.separator");

    // CheckStyle:VisibilityModifier OFF - bc
    // the source file
    protected File file = null;

    // the destination file
    protected File destFile = null;

    // the destination directory
    protected File destDir = null;

    protected Vector<ResourceCollection> rcs = new Vector<ResourceCollection>();

    // here to provide API backwards compatibility
    protected Vector<ResourceCollection> filesets = rcs;

    private boolean enableMultipleMappings = false;

    protected boolean filtering = false;

    protected boolean preserveLastModified = false;

    protected boolean forceOverwrite = false;

    protected boolean flatten = false;

    protected int verbosity = Project.MSG_VERBOSE;

    protected boolean includeEmpty = true;

    protected boolean failonerror = true;

    protected Hashtable<String, String[]> fileCopyMap = new LinkedHashtable<String, String[]>();

    protected Hashtable<String, String[]> dirCopyMap = new LinkedHashtable<String, String[]>();

    protected Hashtable<File, File> completeDirMap = new LinkedHashtable<File, File>();

    protected Mapper mapperElement = null;

    protected FileUtils fileUtils;

    // CheckStyle:VisibilityModifier ON
    private final Vector<FilterChain> filterChains = new Vector<FilterChain>();

    private final Vector<FilterSet> filterSets = new Vector<FilterSet>();

    private String inputEncoding = null;

    private String outputEncoding = null;

    private long granularity = 0;

    private boolean force = false;

    private boolean quiet = false;

    // used to store the single non-file resource to copy when the
    // tofile attribute has been used
    private Resource singleResource = null;

    /**
     * Copy task constructor.
     */
    public Copy() {
        fileUtils = FileUtils.getFileUtils();
        granularity = fileUtils.getFileTimestampGranularity();
    }

    /**
     * Get the FileUtils for this task.
     * @return the fileutils object.
     */
    protected FileUtils getFileUtils() {
        writeline("/home/ubuntu/results/coverage/Copy/Copy_6_10.coverage", "f9fed52d-dea4-45d1-bafb-e689906261a3");
        return fileUtils;
    }

    /**
     * Set a single source file to copy.
     * @param file the file to copy.
     */
    public void setFile(final File file) {
        writeline("/home/ubuntu/results/coverage/Copy/Copy_6_10.coverage", "18a3c1e1-95de-4ab8-8717-e9ab4f94745d");
        this.file = file;
    }

    /**
     * Set the destination file.
     * @param destFile the file to copy to.
     */
    public void setTofile(final File destFile) {
        writeline("/home/ubuntu/results/coverage/Copy/Copy_6_10.coverage", "7736eab3-0dcc-41ec-9c89-12252b47c929");
        this.destFile = destFile;
    }

    /**
     * Set the destination directory.
     * @param destDir the destination directory.
     */
    public void setTodir(final File destDir) {
        writeline("/home/ubuntu/results/coverage/Copy/Copy_6_10.coverage", "9d1f04f8-b625-4b2e-80ae-d0e170d31536");
        this.destDir = destDir;
    }

    /**
     * Add a FilterChain.
     * @return a filter chain object.
     */
    public FilterChain createFilterChain() {
        writeline("/home/ubuntu/results/coverage/Copy/Copy_6_10.coverage", "ab3df2ee-bae2-4bac-b1d4-776fcfefe51a");
        final FilterChain filterChain = new FilterChain();
        writeline("/home/ubuntu/results/coverage/Copy/Copy_6_10.coverage", "4d93c5c3-84a0-4f1c-a5c3-4bb0fdc94df9");
        filterChains.addElement(filterChain);
        writeline("/home/ubuntu/results/coverage/Copy/Copy_6_10.coverage", "6eccee9e-aa63-4586-a75c-d6d0347fbca4");
        return filterChain;
    }

    /**
     * Add a filterset.
     * @return a filter set object.
     */
    public FilterSet createFilterSet() {
        writeline("/home/ubuntu/results/coverage/Copy/Copy_6_10.coverage", "b34c8140-2d35-46b4-8456-50e5680f0df2");
        final FilterSet filterSet = new FilterSet();
        writeline("/home/ubuntu/results/coverage/Copy/Copy_6_10.coverage", "0037f991-461c-405d-ba80-4b6cc62b68d5");
        filterSets.addElement(filterSet);
        writeline("/home/ubuntu/results/coverage/Copy/Copy_6_10.coverage", "1dc044f4-1e2f-4837-95bc-5b287a1bc3ee");
        return filterSet;
    }

    /**
     * Give the copied files the same last modified time as the original files.
     * @param preserve a boolean string.
     * @deprecated since 1.5.x.
     * setPreserveLastModified(String) has been deprecated and
     * replaced with setPreserveLastModified(boolean) to
     * consistently let the Introspection mechanism work.
     */
    @Deprecated
    public void setPreserveLastModified(final String preserve) {
        writeline("/home/ubuntu/results/coverage/Copy/Copy_6_10.coverage", "aa3a0478-6cb9-46fd-895c-6572b96c7ad4");
        setPreserveLastModified(Project.toBoolean(preserve));
    }

    /**
     * Give the copied files the same last modified time as the original files.
     * @param preserve if true preserve the modified time; default is false.
     */
    public void setPreserveLastModified(final boolean preserve) {
        writeline("/home/ubuntu/results/coverage/Copy/Copy_6_10.coverage", "2e0d3f1d-64b8-4b8d-8659-68b7e49ed588");
        preserveLastModified = preserve;
    }

    /**
     * Get whether to give the copied files the same last modified time as
     * the original files.
     * @return the whether destination files will inherit the modification
     * times of the corresponding source files.
     * @since 1.32, Ant 1.5
     */
    public boolean getPreserveLastModified() {
        writeline("/home/ubuntu/results/coverage/Copy/Copy_6_10.coverage", "b42d20ff-df8a-47c5-bf3f-d97c96a7c6a4");
        return preserveLastModified;
    }

    /**
     * Get the filtersets being applied to this operation.
     *
     * @return a vector of FilterSet objects.
     */
    protected Vector<FilterSet> getFilterSets() {
        writeline("/home/ubuntu/results/coverage/Copy/Copy_6_10.coverage", "86802cc6-6e31-4e9f-9dc7-49386a91a876");
        return filterSets;
    }

    /**
     * Get the filterchains being applied to this operation.
     *
     * @return a vector of FilterChain objects.
     */
    protected Vector<FilterChain> getFilterChains() {
        writeline("/home/ubuntu/results/coverage/Copy/Copy_6_10.coverage", "0d67bf55-7e5e-4507-aa77-6aa7afd48e04");
        return filterChains;
    }

    /**
     * Set filtering mode.
     * @param filtering if true enable filtering; default is false.
     */
    public void setFiltering(final boolean filtering) {
        writeline("/home/ubuntu/results/coverage/Copy/Copy_6_10.coverage", "9fc45fcd-00c7-458f-a1c9-dcceef9eb08c");
        this.filtering = filtering;
    }

    /**
     * Set overwrite mode regarding existing destination file(s).
     * @param overwrite if true force overwriting of destination file(s)
     * even if the destination file(s) are younger than
     * the corresponding source file. Default is false.
     */
    public void setOverwrite(final boolean overwrite) {
        writeline("/home/ubuntu/results/coverage/Copy/Copy_6_10.coverage", "351d15f1-d38b-46e8-adae-6edf763e9b85");
        this.forceOverwrite = overwrite;
    }

    /**
     * Whether read-only destinations will be overwritten.
     *
     * <p>Defaults to false</p>
     *
     * @since Ant 1.8.2
     */
    public void setForce(final boolean f) {
        writeline("/home/ubuntu/results/coverage/Copy/Copy_6_10.coverage", "71a7d110-7e70-400d-a303-ce46025e50ce");
        force = f;
    }

    /**
     * Whether read-only destinations will be overwritten.
     *
     * @since Ant 1.8.2
     */
    public boolean getForce() {
        writeline("/home/ubuntu/results/coverage/Copy/Copy_6_10.coverage", "29606669-b203-403b-be4b-ed7546750cf7");
        return force;
    }

    /**
     * Set whether files copied from directory trees will be "flattened"
     * into a single directory.  If there are multiple files with
     * the same name in the source directory tree, only the first
     * file will be copied into the "flattened" directory, unless
     * the forceoverwrite attribute is true.
     * @param flatten if true flatten the destination directory. Default
     * is false.
     */
    public void setFlatten(final boolean flatten) {
        writeline("/home/ubuntu/results/coverage/Copy/Copy_6_10.coverage", "fb920c84-d05c-4eaa-9881-34582f042777");
        this.flatten = flatten;
    }

    /**
     * Set verbose mode. Used to force listing of all names of copied files.
     * @param verbose whether to output the names of copied files.
     * Default is false.
     */
    public void setVerbose(final boolean verbose) {
        writeline("/home/ubuntu/results/coverage/Copy/Copy_6_10.coverage", "79154c6a-1c7a-42d1-a418-00c40324e7be");
        this.verbosity = verbose ? Project.MSG_INFO : Project.MSG_VERBOSE;
    }

    /**
     * Set whether to copy empty directories.
     * @param includeEmpty if true copy empty directories. Default is true.
     */
    public void setIncludeEmptyDirs(final boolean includeEmpty) {
        writeline("/home/ubuntu/results/coverage/Copy/Copy_6_10.coverage", "76d3a841-c6b6-4076-b8dd-f9050dceb9f0");
        this.includeEmpty = includeEmpty;
    }

    /**
     * Set quiet mode. Used to hide messages when a file or directory to be
     * copied does not exist.
     *
     * @param quiet
     * whether or not to display error messages when a file or
     * directory does not exist. Default is false.
     */
    public void setQuiet(final boolean quiet) {
        writeline("/home/ubuntu/results/coverage/Copy/Copy_6_10.coverage", "8346fc75-73c6-4b51-b122-553052b70519");
        this.quiet = quiet;
    }

    /**
     * Set method of handling mappers that return multiple
     * mappings for a given source path.
     * @param enableMultipleMappings If true the task will
     * copy to all the mappings for a given source path, if
     * false, only the first file or directory is
     * processed.
     * By default, this setting is false to provide backward
     * compatibility with earlier releases.
     * @since Ant 1.6
     */
    public void setEnableMultipleMappings(final boolean enableMultipleMappings) {
        writeline("/home/ubuntu/results/coverage/Copy/Copy_6_10.coverage", "e2fd054a-5db1-43b2-84d2-41c94c2e85ef");
        this.enableMultipleMappings = enableMultipleMappings;
    }

    /**
     * Get whether multiple mapping is enabled.
     * @return true if multiple mapping is enabled; false otherwise.
     */
    public boolean isEnableMultipleMapping() {
        writeline("/home/ubuntu/results/coverage/Copy/Copy_6_10.coverage", "b1b53b31-1062-49bc-951d-e7ab34553c63");
        return enableMultipleMappings;
    }

    /**
     * Set whether to fail when errors are encountered. If false, note errors
     * to the output but keep going. Default is true.
     * @param failonerror true or false.
     */
    public void setFailOnError(final boolean failonerror) {
        writeline("/home/ubuntu/results/coverage/Copy/Copy_6_10.coverage", "f7ff2e21-fd52-4ecb-88db-b3206f48331b");
        this.failonerror = failonerror;
    }

    /**
     * Add a set of files to copy.
     * @param set a set of files to copy.
     */
    public void addFileset(final FileSet set) {
        writeline("/home/ubuntu/results/coverage/Copy/Copy_6_10.coverage", "666085bd-2ad2-4a89-82f1-4a000c9b4c5d");
        add(set);
    }

    /**
     * Add a collection of files to copy.
     * @param res a resource collection to copy.
     * @since Ant 1.7
     */
    public void add(final ResourceCollection res) {
        writeline("/home/ubuntu/results/coverage/Copy/Copy_6_10.coverage", "78188062-8cba-4d0c-a7b9-689c513179c2");
        rcs.add(res);
    }

    /**
     * Define the mapper to map source to destination files.
     * @return a mapper to be configured.
     * @exception BuildException if more than one mapper is defined.
     */
    public Mapper createMapper() throws BuildException {
        writeline("/home/ubuntu/results/coverage/Copy/Copy_6_10.coverage", "d73bb3e0-9a5b-4ee3-be28-24adac335c82");
        if (mapperElement != null) {
            writeline("/home/ubuntu/results/coverage/Copy/Copy_6_10.coverage", "f92acc36-e5e4-4411-a967-edfed1c51d57");
            throw new BuildException("Cannot define more than one mapper", getLocation());
        }
        writeline("/home/ubuntu/results/coverage/Copy/Copy_6_10.coverage", "e8d94e96-a12f-4125-9c2b-6fdad452e52a");
        mapperElement = new Mapper(getProject());
        writeline("/home/ubuntu/results/coverage/Copy/Copy_6_10.coverage", "79f58a8b-36ee-4da2-9bc5-7d57cb18e8a5");
        return mapperElement;
    }

    /**
     * Add a nested filenamemapper.
     * @param fileNameMapper the mapper to add.
     * @since Ant 1.6.3
     */
    public void add(final FileNameMapper fileNameMapper) {
        writeline("/home/ubuntu/results/coverage/Copy/Copy_6_10.coverage", "eee85ace-5fa8-4212-a2ed-9500da71ffaa");
        createMapper().add(fileNameMapper);
    }

    /**
     * Set the character encoding.
     * @param encoding the character encoding.
     * @since 1.32, Ant 1.5
     */
    public void setEncoding(final String encoding) {
        writeline("/home/ubuntu/results/coverage/Copy/Copy_6_10.coverage", "2b423d0a-34a9-4afd-b44a-0c872fe1b0e2");
        this.inputEncoding = encoding;
        writeline("/home/ubuntu/results/coverage/Copy/Copy_6_10.coverage", "4dc44be5-e8f8-4e0c-b16b-67cf72629769");
        if (outputEncoding == null) {
            writeline("/home/ubuntu/results/coverage/Copy/Copy_6_10.coverage", "01b247e6-dd3a-454e-8324-c1a2f6de880c");
            outputEncoding = encoding;
        }
    }

    /**
     * Get the character encoding to be used.
     * @return the character encoding, <code>null</code> if not set.
     *
     * @since 1.32, Ant 1.5
     */
    public String getEncoding() {
        writeline("/home/ubuntu/results/coverage/Copy/Copy_6_10.coverage", "325d7dd3-428d-48b3-92f0-0cb7231f7fc9");
        return inputEncoding;
    }

    /**
     * Set the character encoding for output files.
     * @param encoding the output character encoding.
     * @since Ant 1.6
     */
    public void setOutputEncoding(final String encoding) {
        writeline("/home/ubuntu/results/coverage/Copy/Copy_6_10.coverage", "1e5051f7-de80-4eee-b343-de40ebbcc092");
        this.outputEncoding = encoding;
    }

    /**
     * Get the character encoding for output files.
     * @return the character encoding for output files,
     * <code>null</code> if not set.
     *
     * @since Ant 1.6
     */
    public String getOutputEncoding() {
        writeline("/home/ubuntu/results/coverage/Copy/Copy_6_10.coverage", "8b849769-236f-4197-bc15-7fe95963bd56");
        return outputEncoding;
    }

    /**
     * Set the number of milliseconds leeway to give before deciding a
     * target is out of date.
     *
     * <p>Default is 1 second, or 2 seconds on DOS systems.</p>
     * @param granularity the granularity used to decide if a target is out of
     * date.
     * @since Ant 1.6.2
     */
    public void setGranularity(final long granularity) {
        writeline("/home/ubuntu/results/coverage/Copy/Copy_6_10.coverage", "60ac7c85-f34b-4dbc-9d67-bcc6976f09ca");
        this.granularity = granularity;
    }

    /**
     * Perform the copy operation.
     * @exception BuildException if an error occurs.
     */
    @Override
    public void execute() throws BuildException {
        writeline("/home/ubuntu/results/coverage/Copy/Copy_6_10.coverage", "66e27a60-c558-4650-854a-243680947e2c");
        // may be altered in validateAttributes
        final File savedFile = file;
        writeline("/home/ubuntu/results/coverage/Copy/Copy_6_10.coverage", "3f2da815-624e-465f-a447-f01a1c871c11");
        final File savedDestFile = destFile;
        writeline("/home/ubuntu/results/coverage/Copy/Copy_6_10.coverage", "35cae18f-6266-4a3f-86fd-2826a2bc2319");
        final File savedDestDir = destDir;
        writeline("/home/ubuntu/results/coverage/Copy/Copy_6_10.coverage", "cdbf551c-0bcf-42d2-a552-28356b1f5635");
        ResourceCollection savedRc = null;
        writeline("/home/ubuntu/results/coverage/Copy/Copy_6_10.coverage", "3ad0b808-d73a-4754-a083-2ed6e4e5c6e4");
        if (file == null && destFile != null && rcs.size() == 1) {
            writeline("/home/ubuntu/results/coverage/Copy/Copy_6_10.coverage", "36fa5a15-1971-4e32-b670-7887fd2a8667");
            // will be removed in validateAttributes
            savedRc = rcs.elementAt(0);
        }
        writeline("/home/ubuntu/results/coverage/Copy/Copy_6_10.coverage", "e7defaaf-0260-4a56-b556-9cd47252cd35");
        try {
            writeline("/home/ubuntu/results/coverage/Copy/Copy_6_10.coverage", "c4f76b0f-ed93-4d92-97bd-424f4ec8958c");
            // make sure we don't have an illegal set of options
            try {
                writeline("/home/ubuntu/results/coverage/Copy/Copy_6_10.coverage", "997883b2-3cef-4f24-89cb-7e7e1b8a7344");
                validateAttributes();
            } catch (final BuildException e) {
                writeline("/home/ubuntu/results/coverage/Copy/Copy_6_10.coverage", "abb1ee12-376d-43bb-a885-b3ba71aa82e3");
                if (failonerror || !getMessage(e).equals(MSG_WHEN_COPYING_EMPTY_RC_TO_FILE)) {
                    writeline("/home/ubuntu/results/coverage/Copy/Copy_6_10.coverage", "e249df6d-31c8-4b66-827b-58c00aea49de");
                    throw e;
                } else {
                    writeline("/home/ubuntu/results/coverage/Copy/Copy_6_10.coverage", "c2d610a9-25f0-4855-8f69-97477d3def6f");
                    log("Warning: " + getMessage(e), Project.MSG_ERR);
                    writeline("/home/ubuntu/results/coverage/Copy/Copy_6_10.coverage", "5b744343-50cd-442c-88a5-0e37514fcb07");
                    return;
                }
            }
            writeline("/home/ubuntu/results/coverage/Copy/Copy_6_10.coverage", "4900cf7c-cc42-4857-8859-7fa98e42a5b9");
            // deal with the single file
            copySingleFile();
            writeline("/home/ubuntu/results/coverage/Copy/Copy_6_10.coverage", "4971e355-2609-4044-97a9-a24f2f494f7c");
            // deal with the ResourceCollections
            /* for historical and performance reasons we have to do
               things in a rather complex way.

               (1) Move is optimized to move directories if a fileset
               has been included completely, therefore FileSets need a
               special treatment.  This is also required to support
               the failOnError semantice (skip filesets with broken
               basedir but handle the remaining collections).

               (2) We carry around a few protected methods that work
               on basedirs and arrays of names.  To optimize stuff, all
               resources with the same basedir get collected in
               separate lists and then each list is handled in one go.
            */
            final HashMap<File, List<String>> filesByBasedir = new HashMap<File, List<String>>();
            writeline("/home/ubuntu/results/coverage/Copy/Copy_6_10.coverage", "2a21fcff-ce71-4b03-a335-07345f6ab053");
            final HashMap<File, List<String>> dirsByBasedir = new HashMap<File, List<String>>();
            writeline("/home/ubuntu/results/coverage/Copy/Copy_6_10.coverage", "61e6dd99-10a3-47d6-b4e5-63d14a36d1cd");
            final HashSet<File> baseDirs = new HashSet<File>();
            writeline("/home/ubuntu/results/coverage/Copy/Copy_6_10.coverage", "7f77c58e-4f49-471c-9288-3e52219d6398");
            final ArrayList<Resource> nonFileResources = new ArrayList<Resource>();
            writeline("/home/ubuntu/results/coverage/Copy/Copy_6_10.coverage", "cf0fcbda-2160-48d5-ad3b-4a2924db4bec");
            final int size = rcs.size();
            writeline("/home/ubuntu/results/coverage/Copy/Copy_6_10.coverage", "c2d66826-1d90-4027-bcad-a906265e91e2");
            for (int i = 0; i < size; i++) {
                writeline("/home/ubuntu/results/coverage/Copy/Copy_6_10.coverage", "40a8239d-79d7-421e-86de-fb868af817b3");
                final ResourceCollection rc = rcs.elementAt(i);
                writeline("/home/ubuntu/results/coverage/Copy/Copy_6_10.coverage", "44a30f0d-0b39-4cb7-85b2-3653742b1877");
                // Step (1) - beware of the ZipFileSet
                if (rc instanceof FileSet && rc.isFilesystemOnly()) {
                    writeline("/home/ubuntu/results/coverage/Copy/Copy_6_10.coverage", "b29b57b0-135d-43da-aaa4-424cabdb8084");
                    final FileSet fs = (FileSet) rc;
                    writeline("/home/ubuntu/results/coverage/Copy/Copy_6_10.coverage", "aee664fb-a4a7-435b-8387-e831f77f6464");
                    DirectoryScanner ds = null;
                    writeline("/home/ubuntu/results/coverage/Copy/Copy_6_10.coverage", "46a119f4-0840-4c85-8e81-758c33070be3");
                    try {
                        writeline("/home/ubuntu/results/coverage/Copy/Copy_6_10.coverage", "f3f89663-a82a-4c14-92da-c30da4c81330");
                        ds = fs.getDirectoryScanner(getProject());
                    } catch (final BuildException e) {
                        writeline("/home/ubuntu/results/coverage/Copy/Copy_6_10.coverage", "1414a5af-70d1-41bd-8c6b-3cdfb1a82c0e");
                        if (failonerror || !getMessage(e).endsWith(DirectoryScanner.DOES_NOT_EXIST_POSTFIX)) {
                            writeline("/home/ubuntu/results/coverage/Copy/Copy_6_10.coverage", "e5d6a228-f651-4bcd-82ab-ce0107fea085");
                            throw e;
                        } else {
                            writeline("/home/ubuntu/results/coverage/Copy/Copy_6_10.coverage", "9b32da88-81d8-4776-995d-0ea9f2d12c86");
                            if (!quiet) {
                                writeline("/home/ubuntu/results/coverage/Copy/Copy_6_10.coverage", "aba7f733-e25b-4efa-8458-c29162dbc383");
                                log("Warning: " + getMessage(e), Project.MSG_ERR);
                            }
                            writeline("/home/ubuntu/results/coverage/Copy/Copy_6_10.coverage", "f3c624cd-82eb-402d-a2b7-209166f4a713");
                            continue;
                        }
                    }
                    writeline("/home/ubuntu/results/coverage/Copy/Copy_6_10.coverage", "89f44234-1bcc-4bbd-8c02-095931b0b461");
                    final File fromDir = fs.getDir(getProject());
                    writeline("/home/ubuntu/results/coverage/Copy/Copy_6_10.coverage", "16e45b0e-c1fc-4b8b-9d14-5867ef1c40f3");
                    final String[] srcFiles = ds.getIncludedFiles();
                    writeline("/home/ubuntu/results/coverage/Copy/Copy_6_10.coverage", "74b86c19-995b-4daa-8cbd-2a83d7e2e702");
                    final String[] srcDirs = ds.getIncludedDirectories();
                    writeline("/home/ubuntu/results/coverage/Copy/Copy_6_10.coverage", "2e17cfc8-938d-4780-93ad-3cee6674a48b");
                    if (!flatten && mapperElement == null && ds.isEverythingIncluded() && !fs.hasPatterns()) {
                        writeline("/home/ubuntu/results/coverage/Copy/Copy_6_10.coverage", "1162d2e0-0bbe-4846-93f8-7e4dab61cb3a");
                        completeDirMap.put(fromDir, destDir);
                    }
                    writeline("/home/ubuntu/results/coverage/Copy/Copy_6_10.coverage", "39ba8f69-e250-4f07-aace-c86d87457519");
                    add(fromDir, srcFiles, filesByBasedir);
                    writeline("/home/ubuntu/results/coverage/Copy/Copy_6_10.coverage", "12e7794b-13c4-4f9a-9744-ec81a50e8472");
                    add(fromDir, srcDirs, dirsByBasedir);
                    writeline("/home/ubuntu/results/coverage/Copy/Copy_6_10.coverage", "3b763520-7be1-4635-ba9b-4a8faf8783c7");
                    baseDirs.add(fromDir);
                } else {
                    writeline("/home/ubuntu/results/coverage/Copy/Copy_6_10.coverage", "0b249675-2737-4d44-a804-b0615b86d0a8");
                    if (!rc.isFilesystemOnly() && !supportsNonFileResources()) {
                        writeline("/home/ubuntu/results/coverage/Copy/Copy_6_10.coverage", "7d81062d-b67f-4d9b-b593-0912880d4493");
                        throw new BuildException("Only FileSystem resources are supported.");
                    }
                    writeline("/home/ubuntu/results/coverage/Copy/Copy_6_10.coverage", "1ad1f85a-a65d-40f1-a7bc-9cde6c3ea33f");
                    for (final Resource r : rc) {
                        writeline("/home/ubuntu/results/coverage/Copy/Copy_6_10.coverage", "707577a1-eeba-4007-b6de-f69c1033a562");
                        if (!r.isExists()) {
                            writeline("/home/ubuntu/results/coverage/Copy/Copy_6_10.coverage", "7eccccfa-cbda-46ac-afc5-41fde2e58e6f");
                            final String message = "Warning: Could not find resource " + r.toLongString() + " to copy.";
                            writeline("/home/ubuntu/results/coverage/Copy/Copy_6_10.coverage", "6ef32c29-887a-4b80-8633-8b600d0f7932");
                            if (!failonerror) {
                                writeline("/home/ubuntu/results/coverage/Copy/Copy_6_10.coverage", "4b9d32ae-bae8-4163-bae8-7aff099b6589");
                                if (!quiet) {
                                    writeline("/home/ubuntu/results/coverage/Copy/Copy_6_10.coverage", "0b126b84-566f-49a8-80a2-e7c3fa21d1f3");
                                    log(message, Project.MSG_ERR);
                                }
                            } else {
                                writeline("/home/ubuntu/results/coverage/Copy/Copy_6_10.coverage", "ac2e1f14-07cb-48ef-9e38-f1efad9cb7ef");
                                throw new BuildException(message);
                            }
                            writeline("/home/ubuntu/results/coverage/Copy/Copy_6_10.coverage", "80b8e48c-fa8b-4cde-9956-9fa0ffa93adf");
                            continue;
                        }
                        writeline("/home/ubuntu/results/coverage/Copy/Copy_6_10.coverage", "8e345b05-b236-4c83-9c30-b11fe6fe5b4b");
                        File baseDir = NULL_FILE_PLACEHOLDER;
                        writeline("/home/ubuntu/results/coverage/Copy/Copy_6_10.coverage", "9c7e50be-3b0d-4815-a5ff-be9dc4dcff5d");
                        String name = r.getName();
                        writeline("/home/ubuntu/results/coverage/Copy/Copy_6_10.coverage", "3e09115d-cdaa-491f-911a-d8bdcbc7a299");
                        final FileProvider fp = r.as(FileProvider.class);
                        writeline("/home/ubuntu/results/coverage/Copy/Copy_6_10.coverage", "ac158dc4-4452-4923-bff6-530f78f43c89");
                        if (fp != null) {
                            writeline("/home/ubuntu/results/coverage/Copy/Copy_6_10.coverage", "aa04ce23-3247-4366-a323-8e8eb8bdd276");
                            final FileResource fr = ResourceUtils.asFileResource(fp);
                            writeline("/home/ubuntu/results/coverage/Copy/Copy_6_10.coverage", "a832d66a-5504-4db1-883c-ab2649ded382");
                            baseDir = getKeyFile(fr.getBaseDir());
                            writeline("/home/ubuntu/results/coverage/Copy/Copy_6_10.coverage", "e17134a4-e62d-4a5f-bc6b-95abb8443df2");
                            if (fr.getBaseDir() == null) {
                                writeline("/home/ubuntu/results/coverage/Copy/Copy_6_10.coverage", "58ad7e1f-4fe4-4e29-8330-480faaa86df7");
                                name = fr.getFile().getAbsolutePath();
                            }
                        }
                        writeline("/home/ubuntu/results/coverage/Copy/Copy_6_10.coverage", "2b197030-c970-41f1-b66c-d3c1568c6e30");
                        // files.
                        if (r.isDirectory() || fp != null) {
                            writeline("/home/ubuntu/results/coverage/Copy/Copy_6_10.coverage", "32af61b3-67c6-4f33-b0e4-4b49af047411");
                            add(baseDir, name, r.isDirectory() ? dirsByBasedir : filesByBasedir);
                            writeline("/home/ubuntu/results/coverage/Copy/Copy_6_10.coverage", "c2520f7a-3f14-415c-b3f9-0294e2b2cb52");
                            baseDirs.add(baseDir);
                        } else {
                            writeline("/home/ubuntu/results/coverage/Copy/Copy_6_10.coverage", "1bed4227-a515-4965-9833-11bd8994dc75");
                            // a not-directory file resource
                            // needs special treatment
                            nonFileResources.add(r);
                        }
                    }
                }
            }
            writeline("/home/ubuntu/results/coverage/Copy/Copy_6_10.coverage", "5ed7da05-63a7-4260-9e4f-982f344fc7fa");
            iterateOverBaseDirs(baseDirs, dirsByBasedir, filesByBasedir);
            writeline("/home/ubuntu/results/coverage/Copy/Copy_6_10.coverage", "bd451a70-6948-4463-8d4b-fa90b4cf1907");
            // do all the copy operations now...
            try {
                writeline("/home/ubuntu/results/coverage/Copy/Copy_6_10.coverage", "44220e08-c592-4bc0-86a1-2825a66eea0d");
                doFileOperations();
            } catch (final BuildException e) {
                writeline("/home/ubuntu/results/coverage/Copy/Copy_6_10.coverage", "23c27bf0-f743-47c5-babf-12312a6eb5a8");
                if (!failonerror) {
                    writeline("/home/ubuntu/results/coverage/Copy/Copy_6_10.coverage", "5ba1b85f-20a8-489a-8cc2-0560a4c34a32");
                    if (!quiet) {
                        writeline("/home/ubuntu/results/coverage/Copy/Copy_6_10.coverage", "30b2e384-f11c-4727-a6a9-9a6729808ade");
                        log("Warning: " + getMessage(e), Project.MSG_ERR);
                    }
                } else {
                    writeline("/home/ubuntu/results/coverage/Copy/Copy_6_10.coverage", "6c6f8453-dede-4911-877b-ca6175e3e535");
                    throw e;
                }
            }
            writeline("/home/ubuntu/results/coverage/Copy/Copy_6_10.coverage", "75e32b1f-0487-4b11-bc1f-a26ab8af2363");
            if (nonFileResources.size() > 0 || singleResource != null) {
                writeline("/home/ubuntu/results/coverage/Copy/Copy_6_10.coverage", "1a6088de-da90-4c92-b543-cd2cf0e281a9");
                final Resource[] nonFiles = nonFileResources.toArray(new Resource[nonFileResources.size()]);
                writeline("/home/ubuntu/results/coverage/Copy/Copy_6_10.coverage", "a06b4235-a924-4fc3-99a6-cf8c03d9f295");
                // restrict to out-of-date resources
                final Map<Resource, String[]> map = scan(nonFiles, destDir);
                writeline("/home/ubuntu/results/coverage/Copy/Copy_6_10.coverage", "c1021fd7-755d-4cab-9177-612d6b3305b8");
                if (singleResource != null) {
                    writeline("/home/ubuntu/results/coverage/Copy/Copy_6_10.coverage", "4ded0731-f540-4e29-879c-036c7212476e");
                    map.put(singleResource, new String[] { destFile.getAbsolutePath() });
                }
                writeline("/home/ubuntu/results/coverage/Copy/Copy_6_10.coverage", "6084ea84-4a7e-49b6-9614-ba12083c71ec");
                try {
                    writeline("/home/ubuntu/results/coverage/Copy/Copy_6_10.coverage", "950aeecb-9a1d-4cb2-b87a-27e6c7a1f866");
                    doResourceOperations(map);
                } catch (final BuildException e) {
                    writeline("/home/ubuntu/results/coverage/Copy/Copy_6_10.coverage", "f6d8d024-2eca-4826-8dcf-fd3c338c2174");
                    if (!failonerror) {
                        writeline("/home/ubuntu/results/coverage/Copy/Copy_6_10.coverage", "5c564052-b217-4276-99fd-bd0aec07b1fd");
                        if (!quiet) {
                            writeline("/home/ubuntu/results/coverage/Copy/Copy_6_10.coverage", "5da21910-5baf-4d62-9a47-519a8b644782");
                            log("Warning: " + getMessage(e), Project.MSG_ERR);
                        }
                    } else {
                        writeline("/home/ubuntu/results/coverage/Copy/Copy_6_10.coverage", "f91eada6-ca0d-43d5-83ff-6adc46c69d2d");
                        throw e;
                    }
                }
            }
        } finally {
            writeline("/home/ubuntu/results/coverage/Copy/Copy_6_10.coverage", "a28d6e25-8dc0-4968-a262-025c153a9ef3");
            // clean up again, so this instance can be used a second
            // time
            singleResource = null;
            writeline("/home/ubuntu/results/coverage/Copy/Copy_6_10.coverage", "52119195-4910-4cdb-ab4f-dd2db9641892");
            file = savedFile;
            writeline("/home/ubuntu/results/coverage/Copy/Copy_6_10.coverage", "19252b7b-9505-4f35-9eda-3918be1d8f98");
            destFile = savedDestFile;
            writeline("/home/ubuntu/results/coverage/Copy/Copy_6_10.coverage", "4a621554-09a7-416a-ae28-620f1694cafe");
            destDir = savedDestDir;
            writeline("/home/ubuntu/results/coverage/Copy/Copy_6_10.coverage", "d9db91e0-a610-4833-970b-d5ec4ecfcf33");
            if (savedRc != null) {
                writeline("/home/ubuntu/results/coverage/Copy/Copy_6_10.coverage", "782d7617-63cd-469e-8930-d138edd9fa87");
                rcs.insertElementAt(savedRc, 0);
            }
            writeline("/home/ubuntu/results/coverage/Copy/Copy_6_10.coverage", "824ef821-861d-4c22-ad5f-b0c72cde52c3");
            fileCopyMap.clear();
            writeline("/home/ubuntu/results/coverage/Copy/Copy_6_10.coverage", "e37a6004-6676-4b56-9c44-f104458b3b24");
            dirCopyMap.clear();
            writeline("/home/ubuntu/results/coverage/Copy/Copy_6_10.coverage", "d84039e3-04c8-45c1-9102-7f070570f8bf");
            completeDirMap.clear();
        }
    }

    /**
     * *********************************************************************
     * *  protected and private methods
     * **********************************************************************
     */
    private void copySingleFile() {
        writeline("/home/ubuntu/results/coverage/Copy/Copy_6_10.coverage", "4ce78b68-c5b0-4bfa-bc21-4f4d9ac0c0fc");
        // deal with the single file
        if (file != null) {
            writeline("/home/ubuntu/results/coverage/Copy/Copy_6_10.coverage", "a1932388-a547-4b16-bb98-a28caf729b58");
            if (file.exists()) {
                writeline("/home/ubuntu/results/coverage/Copy/Copy_6_10.coverage", "d6729587-83a1-456d-a3e0-0d9c525c81f5");
                if (destFile == null) {
                    writeline("/home/ubuntu/results/coverage/Copy/Copy_6_10.coverage", "c56088c5-00c3-4242-9899-bd48f4cace67");
                    destFile = new File(destDir, file.getName());
                }
                writeline("/home/ubuntu/results/coverage/Copy/Copy_6_10.coverage", "9d89d953-1d7c-49e4-84c5-4f4bc8b668a8");
                if (forceOverwrite || !destFile.exists() || (file.lastModified() - granularity > destFile.lastModified())) {
                    writeline("/home/ubuntu/results/coverage/Copy/Copy_6_10.coverage", "dcbf1c2d-6a83-42ff-beca-deea49633f04");
                    fileCopyMap.put(file.getAbsolutePath(), new String[] { destFile.getAbsolutePath() });
                } else {
                    writeline("/home/ubuntu/results/coverage/Copy/Copy_6_10.coverage", "10f8fb2c-9b51-43b3-86a0-c6e5b02bb808");
                    log(file + " omitted as " + destFile + " is up to date.", Project.MSG_VERBOSE);
                }
            } else {
                writeline("/home/ubuntu/results/coverage/Copy/Copy_6_10.coverage", "932e0742-f443-4d39-ae21-d8a5925d6772");
                final String message = "Warning: Could not find file " + file.getAbsolutePath() + " to copy.";
                writeline("/home/ubuntu/results/coverage/Copy/Copy_6_10.coverage", "33240edb-a536-406e-93db-7b6a585ac68d");
                if (!failonerror) {
                    writeline("/home/ubuntu/results/coverage/Copy/Copy_6_10.coverage", "d9e710c1-b4d4-42a5-9ff9-8a3b31d937ee");
                    if (!quiet) {
                        writeline("/home/ubuntu/results/coverage/Copy/Copy_6_10.coverage", "302b2a34-b254-4a4f-aadf-4a0e50f9b15b");
                        log(message, Project.MSG_ERR);
                    }
                } else {
                    writeline("/home/ubuntu/results/coverage/Copy/Copy_6_10.coverage", "ac2b7428-6828-43fd-94d4-a253676123c7");
                    throw new BuildException(message);
                }
            }
        }
    }

    private void iterateOverBaseDirs(final HashSet<File> baseDirs, final HashMap<File, List<String>> dirsByBasedir, final HashMap<File, List<String>> filesByBasedir) {
        writeline("/home/ubuntu/results/coverage/Copy/Copy_6_10.coverage", "1b062efb-fb76-4528-96db-727de97d71ab");
        for (final File f : baseDirs) {
            writeline("/home/ubuntu/results/coverage/Copy/Copy_6_10.coverage", "3772f116-281b-458a-bfaf-8ea99eae2a3e");
            final List<String> files = filesByBasedir.get(f);
            writeline("/home/ubuntu/results/coverage/Copy/Copy_6_10.coverage", "bfacdf8b-0172-44ed-8140-32dfb91f632f");
            final List<String> dirs = dirsByBasedir.get(f);
            writeline("/home/ubuntu/results/coverage/Copy/Copy_6_10.coverage", "854f496e-f096-4511-a0b6-00419e1a71a6");
            String[] srcFiles = new String[0];
            writeline("/home/ubuntu/results/coverage/Copy/Copy_6_10.coverage", "8019610c-7573-4c61-a2e5-399d3c372756");
            if (files != null) {
                writeline("/home/ubuntu/results/coverage/Copy/Copy_6_10.coverage", "f3d9ca55-012d-4a2b-9dc6-acb03c7e249e");
                srcFiles = files.toArray(srcFiles);
            }
            writeline("/home/ubuntu/results/coverage/Copy/Copy_6_10.coverage", "e30865aa-85f3-454d-a1dc-e363710b75e4");
            String[] srcDirs = new String[0];
            writeline("/home/ubuntu/results/coverage/Copy/Copy_6_10.coverage", "ecb9454f-e81f-4cdb-94e5-6f9ad3fdb7c7");
            if (dirs != null) {
                writeline("/home/ubuntu/results/coverage/Copy/Copy_6_10.coverage", "386d1b60-17ab-4640-b328-d084ee51a655");
                srcDirs = dirs.toArray(srcDirs);
            }
            writeline("/home/ubuntu/results/coverage/Copy/Copy_6_10.coverage", "50724c24-2ce7-4e32-ad49-bc12f93d62f8");
            scan(f == NULL_FILE_PLACEHOLDER ? null : f, destDir, srcFiles, srcDirs);
        }
    }

    /**
     * Ensure we have a consistent and legal set of attributes, and set
     * any internal flags necessary based on different combinations
     * of attributes.
     * @exception BuildException if an error occurs.
     */
    protected void validateAttributes() throws BuildException {
        writeline("/home/ubuntu/results/coverage/Copy/Copy_6_10.coverage", "d947576e-ef8d-4231-b70e-1b65888e610a");
        if (file == null && rcs.size() == 0) {
            writeline("/home/ubuntu/results/coverage/Copy/Copy_6_10.coverage", "0c4cddb9-5cc5-4eed-a944-7fe47a25095f");
            throw new BuildException("Specify at least one source--a file or a resource collection.");
        }
        writeline("/home/ubuntu/results/coverage/Copy/Copy_6_10.coverage", "d37b9674-65f1-436e-939c-17d8b0964c5a");
        if (destFile != null && destDir != null) {
            writeline("/home/ubuntu/results/coverage/Copy/Copy_6_10.coverage", "678de152-652f-4f70-a670-a5fc69ef07ce");
            throw new BuildException("Only one of tofile and todir may be set.");
        }
        writeline("/home/ubuntu/results/coverage/Copy/Copy_6_10.coverage", "31185b43-1482-4610-bf9c-69c55cf23a45");
        if (destFile == null && destDir == null) {
            writeline("/home/ubuntu/results/coverage/Copy/Copy_6_10.coverage", "4714f1f2-6350-4ca4-8eb7-2cc2dd3b6b8e");
            throw new BuildException("One of tofile or todir must be set.");
        }
        writeline("/home/ubuntu/results/coverage/Copy/Copy_6_10.coverage", "60d19675-1596-49ab-9d2a-d6b6f9d80479");
        if (file != null && file.isDirectory()) {
            writeline("/home/ubuntu/results/coverage/Copy/Copy_6_10.coverage", "9a911f1e-51fe-4b17-a87b-314cb322607f");
            throw new BuildException("Use a resource collection to copy directories.");
        }
        writeline("/home/ubuntu/results/coverage/Copy/Copy_6_10.coverage", "ff70d354-0199-460b-bc47-8685f9725685");
        if (destFile != null && rcs.size() > 0) {
            writeline("/home/ubuntu/results/coverage/Copy/Copy_6_10.coverage", "8b28bc34-8048-4a43-8522-c5d9589a93dd");
            if (rcs.size() > 1) {
                writeline("/home/ubuntu/results/coverage/Copy/Copy_6_10.coverage", "9e6a7395-ba64-422c-80da-9a6717e04dad");
                throw new BuildException("Cannot concatenate multiple files into a single file.");
            } else {
                writeline("/home/ubuntu/results/coverage/Copy/Copy_6_10.coverage", "f408105c-059a-4b9d-87c6-2cc2079bb853");
                final ResourceCollection rc = rcs.elementAt(0);
                writeline("/home/ubuntu/results/coverage/Copy/Copy_6_10.coverage", "3121a2c9-8502-4f10-bb58-7b6701057907");
                if (!rc.isFilesystemOnly() && !supportsNonFileResources()) {
                    writeline("/home/ubuntu/results/coverage/Copy/Copy_6_10.coverage", "a9388e1a-ccaa-40ab-a722-ab45204c3d7c");
                    throw new BuildException("Only FileSystem resources are" + " supported.");
                }
                writeline("/home/ubuntu/results/coverage/Copy/Copy_6_10.coverage", "f726fee7-10f4-4626-8d0f-0524603c4002");
                if (rc.size() == 0) {
                    writeline("/home/ubuntu/results/coverage/Copy/Copy_6_10.coverage", "4c6b8074-b1bd-48e0-8b7a-7a12af65ce56");
                    throw new BuildException(MSG_WHEN_COPYING_EMPTY_RC_TO_FILE);
                } else if (rc.size() == 1) {
                    writeline("/home/ubuntu/results/coverage/Copy/Copy_6_10.coverage", "110fc720-8249-4a61-96bb-5fb5400cca3b");
                    final Resource res = rc.iterator().next();
                    writeline("/home/ubuntu/results/coverage/Copy/Copy_6_10.coverage", "fe125946-a10d-4262-b839-0ec5fea0aac4");
                    final FileProvider r = res.as(FileProvider.class);
                    writeline("/home/ubuntu/results/coverage/Copy/Copy_6_10.coverage", "6884bd1e-b50c-4534-88fe-494546efba2a");
                    if (file == null) {
                        writeline("/home/ubuntu/results/coverage/Copy/Copy_6_10.coverage", "024419af-963d-4a4a-992d-5dff7b5ebf96");
                        if (r != null) {
                            writeline("/home/ubuntu/results/coverage/Copy/Copy_6_10.coverage", "581a3be7-bf15-4b29-80b1-96fac974af60");
                            file = r.getFile();
                        } else {
                            writeline("/home/ubuntu/results/coverage/Copy/Copy_6_10.coverage", "53e94e72-66ca-4875-ac00-5137ef7bec22");
                            singleResource = res;
                        }
                        writeline("/home/ubuntu/results/coverage/Copy/Copy_6_10.coverage", "4eb3c4e0-7442-42e8-af20-4b2208988d3d");
                        rcs.removeElementAt(0);
                    } else {
                        writeline("/home/ubuntu/results/coverage/Copy/Copy_6_10.coverage", "9f1d60a4-2e5a-4544-a1e4-ff119186c5d9");
                        throw new BuildException("Cannot concatenate multiple files into a single file.");
                    }
                } else {
                    writeline("/home/ubuntu/results/coverage/Copy/Copy_6_10.coverage", "4e216d7f-19ec-4b7f-bf0b-180a4717a236");
                    throw new BuildException("Cannot concatenate multiple files into a single file.");
                }
            }
        }
        writeline("/home/ubuntu/results/coverage/Copy/Copy_6_10.coverage", "77d3736d-03d6-448a-bf7e-f4e9b122f392");
        if (destFile != null) {
            writeline("/home/ubuntu/results/coverage/Copy/Copy_6_10.coverage", "ac7a08e6-9cbc-4694-9730-eebb4d622cbc");
            destDir = destFile.getParentFile();
        }
    }

    /**
     * Compares source files to destination files to see if they should be
     * copied.
     *
     * @param fromDir  The source directory.
     * @param toDir    The destination directory.
     * @param files    A list of files to copy.
     * @param dirs     A list of directories to copy.
     */
    protected void scan(final File fromDir, final File toDir, final String[] files, final String[] dirs) {
        writeline("/home/ubuntu/results/coverage/Copy/Copy_6_10.coverage", "e475e477-004f-4751-a7b0-48a986dfb783");
        final FileNameMapper mapper = getMapper();
        writeline("/home/ubuntu/results/coverage/Copy/Copy_6_10.coverage", "0de841cd-2ac1-4c20-912b-faf534256644");
        buildMap(fromDir, toDir, files, mapper, fileCopyMap);
        writeline("/home/ubuntu/results/coverage/Copy/Copy_6_10.coverage", "54d0e5c0-2c20-47e6-bac2-8d2fb23b11cc");
        if (includeEmpty) {
            writeline("/home/ubuntu/results/coverage/Copy/Copy_6_10.coverage", "4c86c98b-fa0e-47ca-9a87-3c5376cb26c6");
            buildMap(fromDir, toDir, dirs, mapper, dirCopyMap);
        }
    }

    /**
     * Compares source resources to destination files to see if they
     * should be copied.
     *
     * @param fromResources  The source resources.
     * @param toDir          The destination directory.
     *
     * @return a Map with the out-of-date resources as keys and an
     * array of target file names as values.
     *
     * @since Ant 1.7
     */
    protected Map<Resource, String[]> scan(final Resource[] fromResources, final File toDir) {
        writeline("/home/ubuntu/results/coverage/Copy/Copy_6_10.coverage", "88190578-764c-45b8-ab67-4ddb996a19bc");
        return buildMap(fromResources, toDir, getMapper());
    }

    /**
     * Add to a map of files/directories to copy.
     *
     * @param fromDir the source directory.
     * @param toDir   the destination directory.
     * @param names   a list of filenames.
     * @param mapper  a <code>FileNameMapper</code> value.
     * @param map     a map of source file to array of destination files.
     */
    protected void buildMap(final File fromDir, final File toDir, final String[] names, final FileNameMapper mapper, final Hashtable<String, String[]> map) {
        writeline("/home/ubuntu/results/coverage/Copy/Copy_6_10.coverage", "3cc01157-1780-4333-87a4-d9e6d06b4858");
        String[] toCopy = null;
        writeline("/home/ubuntu/results/coverage/Copy/Copy_6_10.coverage", "2b29cfc9-818c-48f9-a7b6-ed1a3b1f6295");
        if (forceOverwrite) {
            writeline("/home/ubuntu/results/coverage/Copy/Copy_6_10.coverage", "bab7b6f6-190f-453e-a4bd-458ab20751ab");
            final Vector<String> v = new Vector<String>();
            writeline("/home/ubuntu/results/coverage/Copy/Copy_6_10.coverage", "54079ba2-1edc-4a7e-b766-3649507d46c8");
            for (int i = 0; i < names.length; i++) {
                writeline("/home/ubuntu/results/coverage/Copy/Copy_6_10.coverage", "668fc8be-eda6-46c5-a328-ae6d03deb9c0");
                if (mapper.mapFileName(names[i]) != null) {
                    writeline("/home/ubuntu/results/coverage/Copy/Copy_6_10.coverage", "95aa017c-36d9-4a3d-a3ee-f13d273c2cb8");
                    v.addElement(names[i]);
                }
            }
            writeline("/home/ubuntu/results/coverage/Copy/Copy_6_10.coverage", "f8ec9ce8-96cb-471b-8a98-94e006deccc4");
            toCopy = new String[v.size()];
            writeline("/home/ubuntu/results/coverage/Copy/Copy_6_10.coverage", "c3c77c7f-1399-425f-bccc-3303f6491bee");
            v.copyInto(toCopy);
        } else {
            writeline("/home/ubuntu/results/coverage/Copy/Copy_6_10.coverage", "94b3d2f5-f30b-48ad-8610-8c1d5a50dd91");
            final SourceFileScanner ds = new SourceFileScanner(this);
            writeline("/home/ubuntu/results/coverage/Copy/Copy_6_10.coverage", "8128e7ca-18eb-4bf0-84d0-a38be0d11345");
            toCopy = ds.restrict(names, fromDir, toDir, mapper, granularity);
        }
        writeline("/home/ubuntu/results/coverage/Copy/Copy_6_10.coverage", "c9d1c181-602e-47b8-ac78-6f0a3fa6c98e");
        for (int i = 0; i < toCopy.length; i++) {
            writeline("/home/ubuntu/results/coverage/Copy/Copy_6_10.coverage", "f15b9a88-d4c6-4f1b-b51f-8b099b541cc3");
            final File src = new File(fromDir, toCopy[i]);
            writeline("/home/ubuntu/results/coverage/Copy/Copy_6_10.coverage", "ba14da12-083f-45c3-acc6-d31867e64046");
            final String[] mappedFiles = mapper.mapFileName(toCopy[i]);
            writeline("/home/ubuntu/results/coverage/Copy/Copy_6_10.coverage", "681dabba-8f77-4f8f-a679-705bd6421d7a");
            if (!enableMultipleMappings) {
                writeline("/home/ubuntu/results/coverage/Copy/Copy_6_10.coverage", "a63dde2a-1ccd-4787-ad9c-a19019b69065");
                map.put(src.getAbsolutePath(), new String[] { new File(toDir, mappedFiles[0]).getAbsolutePath() });
            } else {
                writeline("/home/ubuntu/results/coverage/Copy/Copy_6_10.coverage", "3647d514-75ef-4901-92b7-3d3054f0d5cb");
                // reuse the array created by the mapper
                for (int k = 0; k < mappedFiles.length; k++) {
                    writeline("/home/ubuntu/results/coverage/Copy/Copy_6_10.coverage", "e4dcedda-20c5-4df1-8e9a-a413fe268c1a");
                    mappedFiles[k] = new File(toDir, mappedFiles[k]).getAbsolutePath();
                }
                writeline("/home/ubuntu/results/coverage/Copy/Copy_6_10.coverage", "3329d5ba-d858-421f-bef0-5067c3b32802");
                map.put(src.getAbsolutePath(), mappedFiles);
            }
        }
    }

    /**
     * Create a map of resources to copy.
     *
     * @param fromResources  The source resources.
     * @param toDir   the destination directory.
     * @param mapper  a <code>FileNameMapper</code> value.
     * @return a map of source resource to array of destination files.
     * @since Ant 1.7
     */
    protected Map<Resource, String[]> buildMap(final Resource[] fromResources, final File toDir, final FileNameMapper mapper) {
        writeline("/home/ubuntu/results/coverage/Copy/Copy_6_10.coverage", "1f0ad20d-7a60-4c85-be77-6614c5cc9d95");
        final HashMap<Resource, String[]> map = new HashMap<Resource, String[]>();
        writeline("/home/ubuntu/results/coverage/Copy/Copy_6_10.coverage", "50137b31-edd6-41e2-8caa-8f739cba8abd");
        Resource[] toCopy = null;
        writeline("/home/ubuntu/results/coverage/Copy/Copy_6_10.coverage", "769fbbe9-5f45-48b5-9272-bcddfca28076");
        if (forceOverwrite) {
            writeline("/home/ubuntu/results/coverage/Copy/Copy_6_10.coverage", "1345cfc7-313e-4adb-a6d1-e46f3cded168");
            final Vector<Resource> v = new Vector<Resource>();
            writeline("/home/ubuntu/results/coverage/Copy/Copy_6_10.coverage", "93a7306f-7905-45c8-aaa7-0efa3145a38f");
            for (int i = 0; i < fromResources.length; i++) {
                writeline("/home/ubuntu/results/coverage/Copy/Copy_6_10.coverage", "58af8771-7037-405f-9d2c-8b7c4c54ed94");
                if (mapper.mapFileName(fromResources[i].getName()) != null) {
                    writeline("/home/ubuntu/results/coverage/Copy/Copy_6_10.coverage", "3127d5fe-ffbd-4257-a880-f33f4b73731f");
                    v.addElement(fromResources[i]);
                }
            }
            writeline("/home/ubuntu/results/coverage/Copy/Copy_6_10.coverage", "8c8b8442-ecb0-4043-a7d7-6127106b8af7");
            toCopy = new Resource[v.size()];
            writeline("/home/ubuntu/results/coverage/Copy/Copy_6_10.coverage", "f97ff982-90ab-4195-822b-8aaee6bdc6d1");
            v.copyInto(toCopy);
        } else {
            writeline("/home/ubuntu/results/coverage/Copy/Copy_6_10.coverage", "ac597245-3c2a-48c6-9bd6-34c8da3a88f7");
            toCopy = ResourceUtils.selectOutOfDateSources(this, fromResources, mapper, new ResourceFactory() {

                public Resource getResource(final String name) {
                    return new FileResource(toDir, name);
                }
            }, granularity);
        }
        writeline("/home/ubuntu/results/coverage/Copy/Copy_6_10.coverage", "ee45725f-ba3b-4ff7-9a53-5fa366c02cbf");
        for (int i = 0; i < toCopy.length; i++) {
            writeline("/home/ubuntu/results/coverage/Copy/Copy_6_10.coverage", "22fbf07f-10cb-4ce3-940b-0b49437cb46e");
            final String[] mappedFiles = mapper.mapFileName(toCopy[i].getName());
            writeline("/home/ubuntu/results/coverage/Copy/Copy_6_10.coverage", "2f8e0c26-bdd6-49dd-bb74-894cc5c5559c");
            for (int j = 0; j < mappedFiles.length; j++) {
                writeline("/home/ubuntu/results/coverage/Copy/Copy_6_10.coverage", "b39aaebe-d490-4fd6-a34d-37984e658a68");
                if (mappedFiles[j] == null) {
                    writeline("/home/ubuntu/results/coverage/Copy/Copy_6_10.coverage", "9034c626-a79a-4349-b9e6-24604649ddba");
                    throw new BuildException("Can't copy a resource without a" + " name if the mapper doesn't" + " provide one.");
                }
            }
            writeline("/home/ubuntu/results/coverage/Copy/Copy_6_10.coverage", "05a963c3-388e-4352-bb0f-0ed918ab0950");
            if (!enableMultipleMappings) {
                writeline("/home/ubuntu/results/coverage/Copy/Copy_6_10.coverage", "a41f7fe8-7c8d-4cff-9c36-bee869be317e");
                map.put(toCopy[i], new String[] { new File(toDir, mappedFiles[0]).getAbsolutePath() });
            } else {
                writeline("/home/ubuntu/results/coverage/Copy/Copy_6_10.coverage", "102d08ec-0c4e-4b57-a7f2-f0f3099220f5");
                // reuse the array created by the mapper
                for (int k = 0; k < mappedFiles.length; k++) {
                    writeline("/home/ubuntu/results/coverage/Copy/Copy_6_10.coverage", "88f4cf53-c7c9-4f75-8743-5311aa1a307f");
                    mappedFiles[k] = new File(toDir, mappedFiles[k]).getAbsolutePath();
                }
                writeline("/home/ubuntu/results/coverage/Copy/Copy_6_10.coverage", "fc256524-0784-4a82-920e-48b463675c80");
                map.put(toCopy[i], mappedFiles);
            }
        }
        writeline("/home/ubuntu/results/coverage/Copy/Copy_6_10.coverage", "7db9c4c9-397f-4ed8-8d1d-f32ff8ce1da0");
        return map;
    }

    /**
     * Actually does the file (and possibly empty directory) copies.
     * This is a good method for subclasses to override.
     */
    protected void doFileOperations() {
        writeline("/home/ubuntu/results/coverage/Copy/Copy_6_10.coverage", "26774449-d8fe-4bda-9d2c-236c1a7ed181");
        if (fileCopyMap.size() > 0) {
            writeline("/home/ubuntu/results/coverage/Copy/Copy_6_10.coverage", "4b5761ef-9265-45a9-86ea-c55edef08b50");
            log("Copying " + fileCopyMap.size() + " file" + (fileCopyMap.size() == 1 ? "" : "s") + " to " + destDir.getAbsolutePath());
            writeline("/home/ubuntu/results/coverage/Copy/Copy_6_10.coverage", "20caa9c4-3f86-4f35-b5ff-158f4bb8156d");
            for (final Map.Entry<String, String[]> e : fileCopyMap.entrySet()) {
                writeline("/home/ubuntu/results/coverage/Copy/Copy_6_10.coverage", "84852f38-29c9-4936-9471-9fb7078c3704");
                final String fromFile = e.getKey();
                writeline("/home/ubuntu/results/coverage/Copy/Copy_6_10.coverage", "b0ac8ae4-f3b5-48e8-b5f3-9aa34b24fffb");
                final String[] toFiles = e.getValue();
                writeline("/home/ubuntu/results/coverage/Copy/Copy_6_10.coverage", "3eb54b96-ce50-4b61-82b8-ffa1a81848bc");
                for (int i = 0; i < toFiles.length; i++) {
                    writeline("/home/ubuntu/results/coverage/Copy/Copy_6_10.coverage", "086d90bb-b33f-4dd3-bd38-42be14c1f19b");
                    final String toFile = toFiles[i];
                    writeline("/home/ubuntu/results/coverage/Copy/Copy_6_10.coverage", "9c9e8af9-1655-4417-8299-914c0418e2b1");
                    if (fromFile.equals(toFile)) {
                        writeline("/home/ubuntu/results/coverage/Copy/Copy_6_10.coverage", "16098dab-6eba-4131-bd02-7f4ff132f2cb");
                        log("Skipping self-copy of " + fromFile, verbosity);
                        writeline("/home/ubuntu/results/coverage/Copy/Copy_6_10.coverage", "6553f3bd-e901-453d-9350-20701d96e29a");
                        continue;
                    }
                    writeline("/home/ubuntu/results/coverage/Copy/Copy_6_10.coverage", "8109b794-4b3d-4161-8bb9-26483867c0ce");
                    try {
                        writeline("/home/ubuntu/results/coverage/Copy/Copy_6_10.coverage", "7156096a-e25f-4000-8835-6f53d0b9452f");
                        log("Copying " + fromFile + " to " + toFile, verbosity);
                        writeline("/home/ubuntu/results/coverage/Copy/Copy_6_10.coverage", "bcf472fd-6a2a-4f61-8de9-48f97fc38d76");
                        final FilterSetCollection executionFilters = new FilterSetCollection();
                        writeline("/home/ubuntu/results/coverage/Copy/Copy_6_10.coverage", "0ce05e82-a76d-4e25-824e-a5e5749879b9");
                        if (filtering) {
                            writeline("/home/ubuntu/results/coverage/Copy/Copy_6_10.coverage", "bb0c0514-d8ab-41a4-bcd0-13190e1ad59b");
                            executionFilters.addFilterSet(getProject().getGlobalFilterSet());
                        }
                        writeline("/home/ubuntu/results/coverage/Copy/Copy_6_10.coverage", "dc2fd787-ca08-4715-8be7-f646f2de64dd");
                        for (final FilterSet filterSet : filterSets) {
                            writeline("/home/ubuntu/results/coverage/Copy/Copy_6_10.coverage", "1b0f5ac2-93d4-4186-aba0-014152023905");
                            executionFilters.addFilterSet(filterSet);
                        }
                        writeline("/home/ubuntu/results/coverage/Copy/Copy_6_10.coverage", "07db3229-ef92-408b-92a4-146b682c4fa8");
                        fileUtils.copyFile(new File(fromFile), new File(toFile), executionFilters, filterChains, forceOverwrite, preserveLastModified, /* append: */
                        false, inputEncoding, outputEncoding, getProject(), getForce());
                    } catch (final IOException ioe) {
                        writeline("/home/ubuntu/results/coverage/Copy/Copy_6_10.coverage", "5c5cf272-6d08-4822-95a9-03887deca281");
                        String msg = "Failed to copy " + fromFile + " to " + toFile + " due to " + getDueTo(ioe);
                        writeline("/home/ubuntu/results/coverage/Copy/Copy_6_10.coverage", "e0f68844-50c8-407b-b3f0-6d226bd170ab");
                        final File targetFile = new File(toFile);
                        writeline("/home/ubuntu/results/coverage/Copy/Copy_6_10.coverage", "fbdea1f3-473e-4f38-bbb1-84b89c44c4e7");
                        if (!(ioe instanceof ResourceUtils.ReadOnlyTargetFileException) && targetFile.exists() && !targetFile.delete()) {
                            writeline("/home/ubuntu/results/coverage/Copy/Copy_6_10.coverage", "2865c9fa-06bb-4305-a5df-202fb2745449");
                            msg += " and I couldn't delete the corrupt " + toFile;
                        }
                        writeline("/home/ubuntu/results/coverage/Copy/Copy_6_10.coverage", "53116c37-a115-4250-90c5-e2c2c2652f3c");
                        if (failonerror) {
                            writeline("/home/ubuntu/results/coverage/Copy/Copy_6_10.coverage", "eeec52b8-08c0-4592-998b-47d8ac8d69cc");
                            throw new BuildException(msg, ioe, getLocation());
                        }
                        writeline("/home/ubuntu/results/coverage/Copy/Copy_6_10.coverage", "a13801b5-4149-4b28-aaf0-bcc4d4d7eacf");
                        log(msg, Project.MSG_ERR);
                    }
                }
            }
        }
        writeline("/home/ubuntu/results/coverage/Copy/Copy_6_10.coverage", "88282ec5-4f92-41f0-a99d-dfb3041e2ece");
        if (includeEmpty) {
            writeline("/home/ubuntu/results/coverage/Copy/Copy_6_10.coverage", "51bd7ec6-4009-4b9d-8ec3-a027198d619d");
            int createCount = 0;
            writeline("/home/ubuntu/results/coverage/Copy/Copy_6_10.coverage", "278f17ac-f860-46c3-aad9-95a8009eb69e");
            for (final String[] dirs : dirCopyMap.values()) {
                writeline("/home/ubuntu/results/coverage/Copy/Copy_6_10.coverage", "87a67eb0-5668-4a76-abb6-fe23df420e21");
                for (int i = 0; i < dirs.length; i++) {
                    writeline("/home/ubuntu/results/coverage/Copy/Copy_6_10.coverage", "861a9888-6031-4645-86cb-3fd8d8c06073");
                    final File d = new File(dirs[i]);
                    writeline("/home/ubuntu/results/coverage/Copy/Copy_6_10.coverage", "393e53eb-d06e-4b73-857f-580685a57e51");
                    if (!d.exists()) {
                        writeline("/home/ubuntu/results/coverage/Copy/Copy_6_10.coverage", "b0c0308a-bc85-4f3c-9237-de215fe8a5b8");
                        if (!(d.mkdirs() || d.isDirectory())) {
                            writeline("/home/ubuntu/results/coverage/Copy/Copy_6_10.coverage", "3c41892c-0ecc-4342-aa13-2522f1276399");
                            log("Unable to create directory " + d.getAbsolutePath(), Project.MSG_ERR);
                        } else {
                            writeline("/home/ubuntu/results/coverage/Copy/Copy_6_10.coverage", "e94662ab-f183-4457-8cfa-89087b424906");
                            createCount++;
                        }
                    }
                }
            }
            writeline("/home/ubuntu/results/coverage/Copy/Copy_6_10.coverage", "957f63b4-76ff-4226-a6c5-ff116d54cf94");
            if (createCount > 0) {
                writeline("/home/ubuntu/results/coverage/Copy/Copy_6_10.coverage", "997b246d-206b-447a-9f96-4184bb8b01a0");
                log("Copied " + dirCopyMap.size() + " empty director" + (dirCopyMap.size() == 1 ? "y" : "ies") + " to " + createCount + " empty director" + (createCount == 1 ? "y" : "ies") + " under " + destDir.getAbsolutePath());
            }
        }
    }

    /**
     * Actually does the resource copies.
     * This is a good method for subclasses to override.
     * @param map a map of source resource to array of destination files.
     * @since Ant 1.7
     */
    protected void doResourceOperations(final Map<Resource, String[]> map) {
        writeline("/home/ubuntu/results/coverage/Copy/Copy_6_10.coverage", "c9de2cd0-3bc6-4c17-9a32-9a63d376a221");
        if (map.size() > 0) {
            writeline("/home/ubuntu/results/coverage/Copy/Copy_6_10.coverage", "af6542d7-e638-466b-a5ae-b61af18f56b7");
            log("Copying " + map.size() + " resource" + (map.size() == 1 ? "" : "s") + " to " + destDir.getAbsolutePath());
            writeline("/home/ubuntu/results/coverage/Copy/Copy_6_10.coverage", "2c347af9-081d-44cb-8523-5f2bfb52be9c");
            for (final Map.Entry<Resource, String[]> e : map.entrySet()) {
                writeline("/home/ubuntu/results/coverage/Copy/Copy_6_10.coverage", "3f824792-16b3-4976-b102-a3a8a5a103be");
                final Resource fromResource = e.getKey();
                writeline("/home/ubuntu/results/coverage/Copy/Copy_6_10.coverage", "cc3d7f88-fade-4f6e-b3d8-275c30d7bb5d");
                for (final String toFile : e.getValue()) {
                    writeline("/home/ubuntu/results/coverage/Copy/Copy_6_10.coverage", "172cd032-20b2-42d7-9c96-65d481771cdf");
                    try {
                        writeline("/home/ubuntu/results/coverage/Copy/Copy_6_10.coverage", "d18f5a75-94e8-4d45-a6ee-f3e67a1dbdc2");
                        log("Copying " + fromResource + " to " + toFile, verbosity);
                        writeline("/home/ubuntu/results/coverage/Copy/Copy_6_10.coverage", "0c5bcd3a-7505-45c2-a554-67bee13e2a85");
                        final FilterSetCollection executionFilters = new FilterSetCollection();
                        writeline("/home/ubuntu/results/coverage/Copy/Copy_6_10.coverage", "99d11ffe-ead3-4ebd-8582-094b55d10c7b");
                        if (filtering) {
                            writeline("/home/ubuntu/results/coverage/Copy/Copy_6_10.coverage", "dc95ab0a-20ad-4ed7-af2c-d7e8ad6ef8bc");
                            executionFilters.addFilterSet(getProject().getGlobalFilterSet());
                        }
                        writeline("/home/ubuntu/results/coverage/Copy/Copy_6_10.coverage", "35c4a0e8-95dc-4f85-9d28-47e1a597aec2");
                        for (final FilterSet filterSet : filterSets) {
                            writeline("/home/ubuntu/results/coverage/Copy/Copy_6_10.coverage", "4632705a-4f16-4777-b328-45f922b03b4e");
                            executionFilters.addFilterSet(filterSet);
                        }
                        writeline("/home/ubuntu/results/coverage/Copy/Copy_6_10.coverage", "5fb16b92-653f-4b38-b35c-482084c49074");
                        ResourceUtils.copyResource(fromResource, new FileResource(destDir, toFile), executionFilters, filterChains, forceOverwrite, preserveLastModified, /* append: */
                        false, inputEncoding, outputEncoding, getProject(), getForce());
                    } catch (final IOException ioe) {
                        writeline("/home/ubuntu/results/coverage/Copy/Copy_6_10.coverage", "a046b0c7-135e-4317-9dd6-a4a07a83f90a");
                        String msg = "Failed to copy " + fromResource + " to " + toFile + " due to " + getDueTo(ioe);
                        writeline("/home/ubuntu/results/coverage/Copy/Copy_6_10.coverage", "85183e94-2931-42fa-bda9-c3e25fa170d0");
                        final File targetFile = new File(toFile);
                        writeline("/home/ubuntu/results/coverage/Copy/Copy_6_10.coverage", "9131295f-cd08-41ba-88e4-c0e59d1b65f8");
                        if (!(ioe instanceof ResourceUtils.ReadOnlyTargetFileException) && targetFile.exists() && !targetFile.delete()) {
                            writeline("/home/ubuntu/results/coverage/Copy/Copy_6_10.coverage", "c15f8d9c-773d-4eda-b7ac-06c496083b27");
                            msg += " and I couldn't delete the corrupt " + toFile;
                        }
                        writeline("/home/ubuntu/results/coverage/Copy/Copy_6_10.coverage", "fdc60895-6ca9-43c3-a258-5ede9c9af7ff");
                        if (failonerror) {
                            writeline("/home/ubuntu/results/coverage/Copy/Copy_6_10.coverage", "a5ce2b87-6c79-4db1-82a8-86a367308e0a");
                            throw new BuildException(msg, ioe, getLocation());
                        }
                        writeline("/home/ubuntu/results/coverage/Copy/Copy_6_10.coverage", "cd5a3874-ac25-4f30-af95-0ab04f3728b6");
                        log(msg, Project.MSG_ERR);
                    }
                }
            }
        }
    }

    /**
     * Whether this task can deal with non-file resources.
     *
     * <p>&lt;copy&gt; can while &lt;move&gt; can't since we don't
     * know how to remove non-file resources.</p>
     *
     * <p>This implementation returns true only if this task is
     * &lt;copy&gt;.  Any subclass of this class that also wants to
     * support non-file resources needs to override this method.  We
     * need to do so for backwards compatibility reasons since we
     * can't expect subclasses to support resources.</p>
     * @return true if this task supports non file resources.
     * @since Ant 1.7
     */
    protected boolean supportsNonFileResources() {
        writeline("/home/ubuntu/results/coverage/Copy/Copy_6_10.coverage", "ee69ea61-63d8-4c34-84e6-206a3a9806ee");
        return getClass().equals(Copy.class);
    }

    /**
     * Adds the given strings to a list contained in the given map.
     * The file is the key into the map.
     */
    private static void add(File baseDir, final String[] names, final Map<File, List<String>> m) {
        writelineStatic("/home/ubuntu/results/coverage/Copy/Copy_6_10.coverage", "ae9ac5a5-7f13-4376-9a6e-8053ac49bdcd");
        if (names != null) {
            writelineStatic("/home/ubuntu/results/coverage/Copy/Copy_6_10.coverage", "fdfbe3b6-4dbb-4dff-a66b-d82709a56ca7");
            baseDir = getKeyFile(baseDir);
            writelineStatic("/home/ubuntu/results/coverage/Copy/Copy_6_10.coverage", "d9f97f36-0a26-455f-bf39-df78daf63280");
            List<String> l = m.get(baseDir);
            writelineStatic("/home/ubuntu/results/coverage/Copy/Copy_6_10.coverage", "059bd340-ca94-49a5-932e-135c16235949");
            if (l == null) {
                writelineStatic("/home/ubuntu/results/coverage/Copy/Copy_6_10.coverage", "b66c3fc6-c92b-4b93-9651-f54682f7f42c");
                l = new ArrayList<String>(names.length);
                writelineStatic("/home/ubuntu/results/coverage/Copy/Copy_6_10.coverage", "9574e018-7d3b-4e7b-b5bd-786fe8393609");
                m.put(baseDir, l);
            }
            writelineStatic("/home/ubuntu/results/coverage/Copy/Copy_6_10.coverage", "f628194a-599d-408b-b402-33b605366874");
            l.addAll(java.util.Arrays.asList(names));
        }
    }

    /**
     * Adds the given string to a list contained in the given map.
     * The file is the key into the map.
     */
    private static void add(final File baseDir, final String name, final Map<File, List<String>> m) {
        writelineStatic("/home/ubuntu/results/coverage/Copy/Copy_6_10.coverage", "5934251b-d5ba-466c-a8e2-ab87c17012d3");
        if (name != null) {
            writelineStatic("/home/ubuntu/results/coverage/Copy/Copy_6_10.coverage", "558b857d-a71c-40f8-8864-ca51181788c9");
            add(baseDir, new String[] { name }, m);
        }
    }

    /**
     * Either returns its argument or a plaeholder if the argument is null.
     */
    private static File getKeyFile(final File f) {
        writelineStatic("/home/ubuntu/results/coverage/Copy/Copy_6_10.coverage", "df781acb-f5d0-45b4-b0bb-8a94335ed710");
        return f == null ? NULL_FILE_PLACEHOLDER : f;
    }

    /**
     * returns the mapper to use based on nested elements or the
     * flatten attribute.
     */
    private FileNameMapper getMapper() {
        writeline("/home/ubuntu/results/coverage/Copy/Copy_6_10.coverage", "fae3dd6b-8c81-4ccc-8252-ce9eded530a8");
        FileNameMapper mapper = null;
        writeline("/home/ubuntu/results/coverage/Copy/Copy_6_10.coverage", "df237e65-4676-4ebf-95eb-76ab3c82d3c2");
        if (mapperElement != null) {
            writeline("/home/ubuntu/results/coverage/Copy/Copy_6_10.coverage", "a2f1ca6c-7e00-4ed2-bf80-f1ad587c2d9b");
            mapper = mapperElement.getImplementation();
        } else if (flatten) {
            writeline("/home/ubuntu/results/coverage/Copy/Copy_6_10.coverage", "aae63227-b1e9-464e-b376-a9bfb1523593");
            mapper = new FlatFileNameMapper();
        } else {
            writeline("/home/ubuntu/results/coverage/Copy/Copy_6_10.coverage", "54834d1f-5ac2-4f6a-b4be-19534e24fdc8");
            mapper = new IdentityMapper();
        }
        writeline("/home/ubuntu/results/coverage/Copy/Copy_6_10.coverage", "e31dd72a-c257-4a69-85b3-08c42b46e45b");
        return mapper;
    }

    /**
     * Handle getMessage() for exceptions.
     * @param ex the exception to handle
     * @return ex.getMessage() if ex.getMessage() is not null
     * otherwise return ex.toString()
     */
    private String getMessage(final Exception ex) {
        writeline("/home/ubuntu/results/coverage/Copy/Copy_6_10.coverage", "a61efbd6-6fdf-4d43-affe-896ca8dcc74e");
        return ex.getMessage() == null ? ex.toString() : ex.getMessage();
    }

    /**
     * Returns a reason for failure based on
     * the exception thrown.
     * If the exception is not IOException output the class name,
     * output the message
     * if the exception is MalformedInput add a little note.
     */
    private String getDueTo(final Exception ex) {
        writeline("/home/ubuntu/results/coverage/Copy/Copy_6_10.coverage", "1f8cf68b-a6ef-4e39-bc37-92d0efb8a3bd");
        final boolean baseIOException = ex.getClass() == IOException.class;
        writeline("/home/ubuntu/results/coverage/Copy/Copy_6_10.coverage", "3b9c0c8d-2ea6-4852-a0fa-bb64571ee964");
        final StringBuffer message = new StringBuffer();
        writeline("/home/ubuntu/results/coverage/Copy/Copy_6_10.coverage", "18af0340-9dab-441e-ae0b-725eec8c9c37");
        if (!baseIOException || ex.getMessage() == null) {
            writeline("/home/ubuntu/results/coverage/Copy/Copy_6_10.coverage", "d7419d1b-69c8-4970-9df3-4c010629ce29");
            message.append(ex.getClass().getName());
        }
        writeline("/home/ubuntu/results/coverage/Copy/Copy_6_10.coverage", "40a8a8c8-65f5-4104-9c88-8a60edd92b00");
        if (ex.getMessage() != null) {
            writeline("/home/ubuntu/results/coverage/Copy/Copy_6_10.coverage", "531dc334-ee76-4c66-8e31-827ae235f0f6");
            if (!baseIOException) {
                writeline("/home/ubuntu/results/coverage/Copy/Copy_6_10.coverage", "9ee12703-cc8f-43f8-93d2-dbb10d8103e4");
                message.append(" ");
            }
            writeline("/home/ubuntu/results/coverage/Copy/Copy_6_10.coverage", "3873af92-2375-4dab-8cf8-b8ba1548da87");
            message.append(ex.getMessage());
        }
        writeline("/home/ubuntu/results/coverage/Copy/Copy_6_10.coverage", "47a43463-5fcb-4135-85eb-6067a7e8feb7");
        if (ex.getClass().getName().indexOf("MalformedInput") != -1) {
            writeline("/home/ubuntu/results/coverage/Copy/Copy_6_10.coverage", "7355ac2f-74f4-4a7d-bcee-6ba189366ee4");
            message.append(LINE_SEPARATOR);
            writeline("/home/ubuntu/results/coverage/Copy/Copy_6_10.coverage", "0f23f73b-7e4e-4285-b893-93aa48b1da8d");
            message.append("This is normally due to the input file containing invalid");
            writeline("/home/ubuntu/results/coverage/Copy/Copy_6_10.coverage", "6eff8213-3313-48b7-9b27-711552fbda61");
            message.append(LINE_SEPARATOR);
            writeline("/home/ubuntu/results/coverage/Copy/Copy_6_10.coverage", "3ce866a0-811e-4094-801b-182164f69561");
            message.append("bytes for the character encoding used : ");
            writeline("/home/ubuntu/results/coverage/Copy/Copy_6_10.coverage", "ab5bd3ba-df06-4bcf-acda-7f1a5c2739b6");
            message.append((inputEncoding == null ? fileUtils.getDefaultEncoding() : inputEncoding));
            writeline("/home/ubuntu/results/coverage/Copy/Copy_6_10.coverage", "2f2f46c4-b413-414f-a414-ded75a146a2d");
            message.append(LINE_SEPARATOR);
        }
        writeline("/home/ubuntu/results/coverage/Copy/Copy_6_10.coverage", "2e9ac6e5-5cd5-4ca2-bd2e-647747599c34");
        return message.toString();
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
