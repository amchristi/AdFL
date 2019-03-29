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
        writeline("/home/ubuntu/results/coverage/Copy/Copy_1_10.coverage", "878da953-227d-4d47-8715-7d333cb64131");
        return fileUtils;
    }

    /**
     * Set a single source file to copy.
     * @param file the file to copy.
     */
    public void setFile(final File file) {
        writeline("/home/ubuntu/results/coverage/Copy/Copy_1_10.coverage", "f6fa10f5-9669-4fe7-9579-25fec819aa37");
        this.file = file;
    }

    /**
     * Set the destination file.
     * @param destFile the file to copy to.
     */
    public void setTofile(final File destFile) {
        writeline("/home/ubuntu/results/coverage/Copy/Copy_1_10.coverage", "b11f93e2-8c74-4ed0-9a49-9e53c231aa83");
        this.destFile = destFile;
    }

    /**
     * Set the destination directory.
     * @param destDir the destination directory.
     */
    public void setTodir(final File destDir) {
        writeline("/home/ubuntu/results/coverage/Copy/Copy_1_10.coverage", "b03256be-a3e0-4b6a-b72b-d280b222a2d2");
        this.destDir = destDir;
    }

    /**
     * Add a FilterChain.
     * @return a filter chain object.
     */
    public FilterChain createFilterChain() {
        writeline("/home/ubuntu/results/coverage/Copy/Copy_1_10.coverage", "818e1997-ba79-4279-a13c-93d81fabd7e7");
        final FilterChain filterChain = new FilterChain();
        writeline("/home/ubuntu/results/coverage/Copy/Copy_1_10.coverage", "c920a89a-b36f-4da5-ba40-454c331a58fa");
        filterChains.addElement(filterChain);
        writeline("/home/ubuntu/results/coverage/Copy/Copy_1_10.coverage", "8902b2ab-7708-49c6-a076-7c31e86de0be");
        return filterChain;
    }

    /**
     * Add a filterset.
     * @return a filter set object.
     */
    public FilterSet createFilterSet() {
        writeline("/home/ubuntu/results/coverage/Copy/Copy_1_10.coverage", "488023c6-ce07-4239-8671-ba27aac5fa7a");
        final FilterSet filterSet = new FilterSet();
        writeline("/home/ubuntu/results/coverage/Copy/Copy_1_10.coverage", "08de7248-7a30-401a-a6b9-cd5f1e88e024");
        filterSets.addElement(filterSet);
        writeline("/home/ubuntu/results/coverage/Copy/Copy_1_10.coverage", "24c877c7-ccf1-4df0-89ef-ec9244fae261");
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
        writeline("/home/ubuntu/results/coverage/Copy/Copy_1_10.coverage", "ec3c9ccc-616b-4b6a-9754-f4ad163763db");
        setPreserveLastModified(Project.toBoolean(preserve));
    }

    /**
     * Give the copied files the same last modified time as the original files.
     * @param preserve if true preserve the modified time; default is false.
     */
    public void setPreserveLastModified(final boolean preserve) {
        writeline("/home/ubuntu/results/coverage/Copy/Copy_1_10.coverage", "9f664b89-41cd-48cc-9f56-58bbe6dafcef");
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
        writeline("/home/ubuntu/results/coverage/Copy/Copy_1_10.coverage", "8b3bc2cd-bffc-4c58-9b17-58107f72079f");
        return preserveLastModified;
    }

    /**
     * Get the filtersets being applied to this operation.
     *
     * @return a vector of FilterSet objects.
     */
    protected Vector<FilterSet> getFilterSets() {
        writeline("/home/ubuntu/results/coverage/Copy/Copy_1_10.coverage", "80ed56c0-6924-4a60-b24a-8a99bb21b7e0");
        return filterSets;
    }

    /**
     * Get the filterchains being applied to this operation.
     *
     * @return a vector of FilterChain objects.
     */
    protected Vector<FilterChain> getFilterChains() {
        writeline("/home/ubuntu/results/coverage/Copy/Copy_1_10.coverage", "396989d4-b315-4cbe-887e-136fbff2f29d");
        return filterChains;
    }

    /**
     * Set filtering mode.
     * @param filtering if true enable filtering; default is false.
     */
    public void setFiltering(final boolean filtering) {
        writeline("/home/ubuntu/results/coverage/Copy/Copy_1_10.coverage", "489b5585-d3b6-4838-9fc1-e081d39f3cab");
        this.filtering = filtering;
    }

    /**
     * Set overwrite mode regarding existing destination file(s).
     * @param overwrite if true force overwriting of destination file(s)
     * even if the destination file(s) are younger than
     * the corresponding source file. Default is false.
     */
    public void setOverwrite(final boolean overwrite) {
        writeline("/home/ubuntu/results/coverage/Copy/Copy_1_10.coverage", "7734b035-0fae-4532-a3e3-7ef304e33444");
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
        writeline("/home/ubuntu/results/coverage/Copy/Copy_1_10.coverage", "0b88ad4e-6847-4b28-a6d6-62a765f8e114");
        force = f;
    }

    /**
     * Whether read-only destinations will be overwritten.
     *
     * @since Ant 1.8.2
     */
    public boolean getForce() {
        writeline("/home/ubuntu/results/coverage/Copy/Copy_1_10.coverage", "765f641c-7e34-4666-9ff3-00eb77fefa5b");
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
        writeline("/home/ubuntu/results/coverage/Copy/Copy_1_10.coverage", "23cad50c-e0c2-46ea-976e-14764674d8da");
        this.flatten = flatten;
    }

    /**
     * Set verbose mode. Used to force listing of all names of copied files.
     * @param verbose whether to output the names of copied files.
     * Default is false.
     */
    public void setVerbose(final boolean verbose) {
        writeline("/home/ubuntu/results/coverage/Copy/Copy_1_10.coverage", "c3b1fbaa-0187-43a6-91f6-5a568a9cb3d4");
        this.verbosity = verbose ? Project.MSG_INFO : Project.MSG_VERBOSE;
    }

    /**
     * Set whether to copy empty directories.
     * @param includeEmpty if true copy empty directories. Default is true.
     */
    public void setIncludeEmptyDirs(final boolean includeEmpty) {
        writeline("/home/ubuntu/results/coverage/Copy/Copy_1_10.coverage", "c045f1cf-cd8a-41d4-92a6-4e4f3bff7a8c");
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
        writeline("/home/ubuntu/results/coverage/Copy/Copy_1_10.coverage", "69a974f8-b081-40d9-b42b-22b63f2afdaf");
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
        writeline("/home/ubuntu/results/coverage/Copy/Copy_1_10.coverage", "867cdabb-f8d3-4358-a8e4-17a297b07f8d");
        this.enableMultipleMappings = enableMultipleMappings;
    }

    /**
     * Get whether multiple mapping is enabled.
     * @return true if multiple mapping is enabled; false otherwise.
     */
    public boolean isEnableMultipleMapping() {
        writeline("/home/ubuntu/results/coverage/Copy/Copy_1_10.coverage", "d1aae765-1d75-4862-bf5f-6260c043c91b");
        return enableMultipleMappings;
    }

    /**
     * Set whether to fail when errors are encountered. If false, note errors
     * to the output but keep going. Default is true.
     * @param failonerror true or false.
     */
    public void setFailOnError(final boolean failonerror) {
        writeline("/home/ubuntu/results/coverage/Copy/Copy_1_10.coverage", "c6f2d6d4-9e6e-4fc6-8c38-148edb5a9da6");
        this.failonerror = failonerror;
    }

    /**
     * Add a set of files to copy.
     * @param set a set of files to copy.
     */
    public void addFileset(final FileSet set) {
        writeline("/home/ubuntu/results/coverage/Copy/Copy_1_10.coverage", "b6d3b584-3398-496c-b8b1-3f2a947447f1");
        add(set);
    }

    /**
     * Add a collection of files to copy.
     * @param res a resource collection to copy.
     * @since Ant 1.7
     */
    public void add(final ResourceCollection res) {
        writeline("/home/ubuntu/results/coverage/Copy/Copy_1_10.coverage", "b4777047-b778-427d-abc3-d76b88e2a1e3");
        rcs.add(res);
    }

    /**
     * Define the mapper to map source to destination files.
     * @return a mapper to be configured.
     * @exception BuildException if more than one mapper is defined.
     */
    public Mapper createMapper() throws BuildException {
        writeline("/home/ubuntu/results/coverage/Copy/Copy_1_10.coverage", "98133d82-5f33-4b44-911e-1c68cec35ead");
        if (mapperElement != null) {
            writeline("/home/ubuntu/results/coverage/Copy/Copy_1_10.coverage", "df6d14b4-e647-4bc3-95c3-3e8562cc4b78");
            throw new BuildException("Cannot define more than one mapper", getLocation());
        }
        writeline("/home/ubuntu/results/coverage/Copy/Copy_1_10.coverage", "459ad799-1172-4223-aa92-50d88895954d");
        mapperElement = new Mapper(getProject());
        writeline("/home/ubuntu/results/coverage/Copy/Copy_1_10.coverage", "1f7f2269-c9c7-410c-9f5c-bc1935671c50");
        return mapperElement;
    }

    /**
     * Add a nested filenamemapper.
     * @param fileNameMapper the mapper to add.
     * @since Ant 1.6.3
     */
    public void add(final FileNameMapper fileNameMapper) {
        writeline("/home/ubuntu/results/coverage/Copy/Copy_1_10.coverage", "98c422a6-7f53-4d5f-a23c-b4af30b92ba6");
        createMapper().add(fileNameMapper);
    }

    /**
     * Set the character encoding.
     * @param encoding the character encoding.
     * @since 1.32, Ant 1.5
     */
    public void setEncoding(final String encoding) {
        writeline("/home/ubuntu/results/coverage/Copy/Copy_1_10.coverage", "d61f09cd-4cce-44ad-8cfe-d85cb30767fa");
        this.inputEncoding = encoding;
        writeline("/home/ubuntu/results/coverage/Copy/Copy_1_10.coverage", "67c60e8a-afdb-48ac-8afa-bbeb642341f1");
        if (outputEncoding == null) {
            writeline("/home/ubuntu/results/coverage/Copy/Copy_1_10.coverage", "6304791f-227d-4ef1-a6af-86816da34ed6");
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
        writeline("/home/ubuntu/results/coverage/Copy/Copy_1_10.coverage", "c56116a1-3c09-45c3-9d7f-d570289bb96c");
        return inputEncoding;
    }

    /**
     * Set the character encoding for output files.
     * @param encoding the output character encoding.
     * @since Ant 1.6
     */
    public void setOutputEncoding(final String encoding) {
        writeline("/home/ubuntu/results/coverage/Copy/Copy_1_10.coverage", "5291e943-5da0-46bb-9d5e-e5b1cb54bc68");
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
        writeline("/home/ubuntu/results/coverage/Copy/Copy_1_10.coverage", "f33b39f2-3962-4ab8-9761-2d50551fd937");
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
        writeline("/home/ubuntu/results/coverage/Copy/Copy_1_10.coverage", "4ea2bdb1-4b80-4d77-8d1d-44b7a3e9792d");
        this.granularity = granularity;
    }

    /**
     * Perform the copy operation.
     * @exception BuildException if an error occurs.
     */
    @Override
    public void execute() throws BuildException {
        writeline("/home/ubuntu/results/coverage/Copy/Copy_1_10.coverage", "e98e8734-9faa-4a59-b702-d0e2c3ce1bb6");
        // may be altered in validateAttributes
        final File savedFile = file;
        writeline("/home/ubuntu/results/coverage/Copy/Copy_1_10.coverage", "486fe6d9-61fa-4ec8-b019-7b4ea4d38970");
        final File savedDestFile = destFile;
        writeline("/home/ubuntu/results/coverage/Copy/Copy_1_10.coverage", "f9730589-f045-4919-a4c0-266ba7a78428");
        final File savedDestDir = destDir;
        writeline("/home/ubuntu/results/coverage/Copy/Copy_1_10.coverage", "7e19765f-529e-48ac-a35b-6638473da89a");
        ResourceCollection savedRc = null;
        writeline("/home/ubuntu/results/coverage/Copy/Copy_1_10.coverage", "3fafb7bc-e7a8-4739-8f0a-d36e8be50352");
        if (file == null && destFile != null && rcs.size() == 1) {
            writeline("/home/ubuntu/results/coverage/Copy/Copy_1_10.coverage", "653047d5-6634-4886-ba49-f2e1166b2a9c");
            // will be removed in validateAttributes
            savedRc = rcs.elementAt(0);
        }
        writeline("/home/ubuntu/results/coverage/Copy/Copy_1_10.coverage", "e577d741-209f-47d6-90e2-b20ab372fe3f");
        try {
            writeline("/home/ubuntu/results/coverage/Copy/Copy_1_10.coverage", "51593227-997f-4bc0-8d67-a2c7a6becba1");
            // make sure we don't have an illegal set of options
            try {
                writeline("/home/ubuntu/results/coverage/Copy/Copy_1_10.coverage", "23754c07-bb80-48cd-8edf-c16ef211b50b");
                validateAttributes();
            } catch (final BuildException e) {
                writeline("/home/ubuntu/results/coverage/Copy/Copy_1_10.coverage", "d7a2ca35-9521-42d5-b573-48163c567eca");
                if (failonerror || !getMessage(e).equals(MSG_WHEN_COPYING_EMPTY_RC_TO_FILE)) {
                    writeline("/home/ubuntu/results/coverage/Copy/Copy_1_10.coverage", "73575dff-caa7-4145-8abe-f7c0d8e87bbb");
                    throw e;
                } else {
                    writeline("/home/ubuntu/results/coverage/Copy/Copy_1_10.coverage", "4b042348-4230-4a8c-95bb-b9ef8d4707fc");
                    log("Warning: " + getMessage(e), Project.MSG_ERR);
                    writeline("/home/ubuntu/results/coverage/Copy/Copy_1_10.coverage", "464be77f-b493-47fb-b8ee-f5d83dc00713");
                    return;
                }
            }
            writeline("/home/ubuntu/results/coverage/Copy/Copy_1_10.coverage", "e3f49941-719c-4ffd-b0be-bf6138c8df4a");
            // deal with the single file
            copySingleFile();
            writeline("/home/ubuntu/results/coverage/Copy/Copy_1_10.coverage", "a6509be4-12db-4db6-a0ee-90973a11c36c");
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
            writeline("/home/ubuntu/results/coverage/Copy/Copy_1_10.coverage", "9383897c-56cd-4f05-ac29-46a9ec24f3e3");
            final HashMap<File, List<String>> dirsByBasedir = new HashMap<File, List<String>>();
            writeline("/home/ubuntu/results/coverage/Copy/Copy_1_10.coverage", "894fe63a-36b8-426a-a7b5-848332175cc6");
            final HashSet<File> baseDirs = new HashSet<File>();
            writeline("/home/ubuntu/results/coverage/Copy/Copy_1_10.coverage", "86c984e5-8db1-42d8-91fc-f4c264f1dcea");
            final ArrayList<Resource> nonFileResources = new ArrayList<Resource>();
            writeline("/home/ubuntu/results/coverage/Copy/Copy_1_10.coverage", "afd73557-9666-4aef-833d-39992a60e532");
            final int size = rcs.size();
            writeline("/home/ubuntu/results/coverage/Copy/Copy_1_10.coverage", "38ceeccb-4bfc-469a-9b4e-37e0e58d9366");
            for (int i = 0; i < size; i++) {
                writeline("/home/ubuntu/results/coverage/Copy/Copy_1_10.coverage", "dd451bb8-eeea-46fe-8ddc-3ea813416e2f");
                final ResourceCollection rc = rcs.elementAt(i);
                writeline("/home/ubuntu/results/coverage/Copy/Copy_1_10.coverage", "05a2d53f-91be-466d-a57c-7f3fbb762f9b");
                // Step (1) - beware of the ZipFileSet
                if (rc instanceof FileSet && rc.isFilesystemOnly()) {
                    writeline("/home/ubuntu/results/coverage/Copy/Copy_1_10.coverage", "aa5667b8-800e-4f09-a204-0234e08fb2c2");
                    final FileSet fs = (FileSet) rc;
                    writeline("/home/ubuntu/results/coverage/Copy/Copy_1_10.coverage", "6aebcdef-93bd-4ff3-b509-731cb69ff605");
                    DirectoryScanner ds = null;
                    writeline("/home/ubuntu/results/coverage/Copy/Copy_1_10.coverage", "80bc281b-6a2a-4ea8-ab09-9a1fe85ab089");
                    try {
                        writeline("/home/ubuntu/results/coverage/Copy/Copy_1_10.coverage", "3d0cd17d-10f0-4730-ab42-062fe0577bd4");
                        ds = fs.getDirectoryScanner(getProject());
                    } catch (final BuildException e) {
                        writeline("/home/ubuntu/results/coverage/Copy/Copy_1_10.coverage", "ed5ba4b3-d9cd-43cc-8ba4-39d6f98191e8");
                        if (failonerror || !getMessage(e).endsWith(DirectoryScanner.DOES_NOT_EXIST_POSTFIX)) {
                            writeline("/home/ubuntu/results/coverage/Copy/Copy_1_10.coverage", "8c6dbc02-77b2-46e9-937a-d97f49252b0a");
                            throw e;
                        } else {
                            writeline("/home/ubuntu/results/coverage/Copy/Copy_1_10.coverage", "2f789ee4-599c-4079-aa5d-bcf4f2b44dc5");
                            if (!quiet) {
                                writeline("/home/ubuntu/results/coverage/Copy/Copy_1_10.coverage", "dae81b1a-59c5-4e23-8870-82f26f446e12");
                                log("Warning: " + getMessage(e), Project.MSG_ERR);
                            }
                            writeline("/home/ubuntu/results/coverage/Copy/Copy_1_10.coverage", "73919638-3ef6-41e9-b4ff-3e10af6d3d52");
                            continue;
                        }
                    }
                    writeline("/home/ubuntu/results/coverage/Copy/Copy_1_10.coverage", "8cbbe966-29f6-4964-9a7f-13078339c042");
                    final File fromDir = fs.getDir(getProject());
                    writeline("/home/ubuntu/results/coverage/Copy/Copy_1_10.coverage", "8fc9fb51-c6b2-49a7-9965-255069a49545");
                    final String[] srcFiles = ds.getIncludedFiles();
                    writeline("/home/ubuntu/results/coverage/Copy/Copy_1_10.coverage", "7f00405f-418d-4217-b5da-e3bd209ad999");
                    final String[] srcDirs = ds.getIncludedDirectories();
                    writeline("/home/ubuntu/results/coverage/Copy/Copy_1_10.coverage", "80c2690c-c0c0-4190-b5e9-aa01798c7aa1");
                    if (!flatten && mapperElement == null && ds.isEverythingIncluded() && !fs.hasPatterns()) {
                        writeline("/home/ubuntu/results/coverage/Copy/Copy_1_10.coverage", "d0240774-c20f-4174-b741-77cb48bbb401");
                        completeDirMap.put(fromDir, destDir);
                    }
                    writeline("/home/ubuntu/results/coverage/Copy/Copy_1_10.coverage", "3dda9b00-999b-41f4-846b-8abf034e3eac");
                    add(fromDir, srcFiles, filesByBasedir);
                    writeline("/home/ubuntu/results/coverage/Copy/Copy_1_10.coverage", "d8a66417-6569-4e2a-981f-a51b0b66036e");
                    add(fromDir, srcDirs, dirsByBasedir);
                    writeline("/home/ubuntu/results/coverage/Copy/Copy_1_10.coverage", "8861c4ce-0bc9-4d69-acc4-0fa2d95756c5");
                    baseDirs.add(fromDir);
                } else {
                    writeline("/home/ubuntu/results/coverage/Copy/Copy_1_10.coverage", "73583b2b-a64e-45fc-8bca-1df4b2538b39");
                    if (!rc.isFilesystemOnly() && !supportsNonFileResources()) {
                        writeline("/home/ubuntu/results/coverage/Copy/Copy_1_10.coverage", "eb8c16a2-a22f-42d3-be45-204c7f7b0ad0");
                        throw new BuildException("Only FileSystem resources are supported.");
                    }
                    writeline("/home/ubuntu/results/coverage/Copy/Copy_1_10.coverage", "334ffe48-1209-4c8b-ae53-eea4579259f9");
                    for (final Resource r : rc) {
                        writeline("/home/ubuntu/results/coverage/Copy/Copy_1_10.coverage", "f5319f1a-b2c1-4fa0-aa0e-bd8238890eaa");
                        if (!r.isExists()) {
                            writeline("/home/ubuntu/results/coverage/Copy/Copy_1_10.coverage", "3064e82d-6c95-4cfa-99d3-096c40bc16df");
                            final String message = "Warning: Could not find resource " + r.toLongString() + " to copy.";
                            writeline("/home/ubuntu/results/coverage/Copy/Copy_1_10.coverage", "810d1674-37ee-4156-b245-4bec461a58f3");
                            if (!failonerror) {
                                writeline("/home/ubuntu/results/coverage/Copy/Copy_1_10.coverage", "adb2971c-8cd3-4124-9d23-f69e28dfb464");
                                if (!quiet) {
                                    writeline("/home/ubuntu/results/coverage/Copy/Copy_1_10.coverage", "30aa021f-d380-4f6e-a1d4-fd58a70c37e0");
                                    log(message, Project.MSG_ERR);
                                }
                            } else {
                                writeline("/home/ubuntu/results/coverage/Copy/Copy_1_10.coverage", "f7a88dda-86f5-47c7-a254-1f0d89109ecb");
                                throw new BuildException(message);
                            }
                            writeline("/home/ubuntu/results/coverage/Copy/Copy_1_10.coverage", "fabe82cc-a2c1-4569-b5f5-e53bbfcbf9c1");
                            continue;
                        }
                        writeline("/home/ubuntu/results/coverage/Copy/Copy_1_10.coverage", "04fa0a91-166e-471a-8caa-e7c27a436411");
                        File baseDir = NULL_FILE_PLACEHOLDER;
                        writeline("/home/ubuntu/results/coverage/Copy/Copy_1_10.coverage", "f92b86ae-1a4d-4d61-9dcd-84af156d1fc3");
                        String name = r.getName();
                        writeline("/home/ubuntu/results/coverage/Copy/Copy_1_10.coverage", "da086914-aef4-4ffe-a072-5407deaf230d");
                        final FileProvider fp = r.as(FileProvider.class);
                        writeline("/home/ubuntu/results/coverage/Copy/Copy_1_10.coverage", "edf9d2dc-ac01-4b92-8f6c-06e458ead4eb");
                        if (fp != null) {
                            writeline("/home/ubuntu/results/coverage/Copy/Copy_1_10.coverage", "b97eab5e-cd34-4c59-9b9f-1812ae03873d");
                            final FileResource fr = ResourceUtils.asFileResource(fp);
                            writeline("/home/ubuntu/results/coverage/Copy/Copy_1_10.coverage", "c40b1b5b-0024-4afe-be15-8766cad2e409");
                            baseDir = getKeyFile(fr.getBaseDir());
                            writeline("/home/ubuntu/results/coverage/Copy/Copy_1_10.coverage", "28f85da8-7026-4085-9ceb-58f451eb0806");
                            if (fr.getBaseDir() == null) {
                                writeline("/home/ubuntu/results/coverage/Copy/Copy_1_10.coverage", "eb4be69d-ec34-45e3-9c93-7b2571e74be2");
                                name = fr.getFile().getAbsolutePath();
                            }
                        }
                        writeline("/home/ubuntu/results/coverage/Copy/Copy_1_10.coverage", "acbf427c-eedd-40fb-b7ce-9ff9e9a3ab8b");
                        // files.
                        if (r.isDirectory() || fp != null) {
                            writeline("/home/ubuntu/results/coverage/Copy/Copy_1_10.coverage", "1e7ccc4f-daa1-41b0-91db-54c669ca829e");
                            add(baseDir, name, r.isDirectory() ? dirsByBasedir : filesByBasedir);
                            writeline("/home/ubuntu/results/coverage/Copy/Copy_1_10.coverage", "80a75897-e633-424d-a13f-cb8fd9816061");
                            baseDirs.add(baseDir);
                        } else {
                            writeline("/home/ubuntu/results/coverage/Copy/Copy_1_10.coverage", "0708d153-a54a-4b2d-9249-de64071a65be");
                            // a not-directory file resource
                            // needs special treatment
                            nonFileResources.add(r);
                        }
                    }
                }
            }
            writeline("/home/ubuntu/results/coverage/Copy/Copy_1_10.coverage", "2895adce-89d6-49d5-b4ca-4fee29413d3f");
            iterateOverBaseDirs(baseDirs, dirsByBasedir, filesByBasedir);
            writeline("/home/ubuntu/results/coverage/Copy/Copy_1_10.coverage", "30a4462a-653f-4a03-a0e6-9bd5b3ba522b");
            // do all the copy operations now...
            try {
                writeline("/home/ubuntu/results/coverage/Copy/Copy_1_10.coverage", "eddd7a29-72d0-4aee-a133-256d194eecd1");
                doFileOperations();
            } catch (final BuildException e) {
                writeline("/home/ubuntu/results/coverage/Copy/Copy_1_10.coverage", "cf98f748-8791-44ba-b396-61d17b16e28b");
                if (!failonerror) {
                    writeline("/home/ubuntu/results/coverage/Copy/Copy_1_10.coverage", "ff6ce49c-4550-439b-a1b7-9c38e0c80807");
                    if (!quiet) {
                        writeline("/home/ubuntu/results/coverage/Copy/Copy_1_10.coverage", "8ab502c6-47e5-421e-b688-c21cb9e52b5d");
                        log("Warning: " + getMessage(e), Project.MSG_ERR);
                    }
                } else {
                    writeline("/home/ubuntu/results/coverage/Copy/Copy_1_10.coverage", "1174a0d3-f209-417c-b171-d3dd863ed838");
                    throw e;
                }
            }
            writeline("/home/ubuntu/results/coverage/Copy/Copy_1_10.coverage", "470b9094-5402-4fea-a166-30fa805fcfc3");
            if (nonFileResources.size() > 0 || singleResource != null) {
                writeline("/home/ubuntu/results/coverage/Copy/Copy_1_10.coverage", "275c1ee7-148b-4392-8a55-63dea7de2893");
                final Resource[] nonFiles = nonFileResources.toArray(new Resource[nonFileResources.size()]);
                writeline("/home/ubuntu/results/coverage/Copy/Copy_1_10.coverage", "44762e37-813c-48af-aac6-85e7412c7f27");
                // restrict to out-of-date resources
                final Map<Resource, String[]> map = scan(nonFiles, destDir);
                writeline("/home/ubuntu/results/coverage/Copy/Copy_1_10.coverage", "75b47b59-4189-46b4-ab7d-fdfb6ae848a5");
                if (singleResource != null) {
                    writeline("/home/ubuntu/results/coverage/Copy/Copy_1_10.coverage", "abd55c04-e9d0-491c-8f63-28a861598c2c");
                    map.put(singleResource, new String[] { destFile.getAbsolutePath() });
                }
                writeline("/home/ubuntu/results/coverage/Copy/Copy_1_10.coverage", "4f6f9817-7c66-4434-aa54-00b7220131f0");
                try {
                    writeline("/home/ubuntu/results/coverage/Copy/Copy_1_10.coverage", "ca46aa54-58b1-4750-9ba6-e160800d8529");
                    doResourceOperations(map);
                } catch (final BuildException e) {
                    writeline("/home/ubuntu/results/coverage/Copy/Copy_1_10.coverage", "7408dd9b-c8fb-4827-bdd9-b1b75b1f7f8e");
                    if (!failonerror) {
                        writeline("/home/ubuntu/results/coverage/Copy/Copy_1_10.coverage", "f5d9c4a5-14a1-4ec0-884f-e3ab50d71347");
                        if (!quiet) {
                            writeline("/home/ubuntu/results/coverage/Copy/Copy_1_10.coverage", "1e614e3d-d0ca-4933-95ef-f271a1b25138");
                            log("Warning: " + getMessage(e), Project.MSG_ERR);
                        }
                    } else {
                        writeline("/home/ubuntu/results/coverage/Copy/Copy_1_10.coverage", "d5e2eaf8-332d-4729-a18d-269d48d7eb0e");
                        throw e;
                    }
                }
            }
        } finally {
            writeline("/home/ubuntu/results/coverage/Copy/Copy_1_10.coverage", "156e67c0-ade8-4a02-834a-05dd40eeac1d");
            // clean up again, so this instance can be used a second
            // time
            singleResource = null;
            writeline("/home/ubuntu/results/coverage/Copy/Copy_1_10.coverage", "5e105d16-eb59-4217-9229-54eeb0614d90");
            file = savedFile;
            writeline("/home/ubuntu/results/coverage/Copy/Copy_1_10.coverage", "17bafaf3-d9d4-4d37-9cb4-08629d5a11cc");
            destFile = savedDestFile;
            writeline("/home/ubuntu/results/coverage/Copy/Copy_1_10.coverage", "4aa93c25-2226-4a2e-b0db-de24fbb6340f");
            destDir = savedDestDir;
            writeline("/home/ubuntu/results/coverage/Copy/Copy_1_10.coverage", "62b22ce8-c9c7-4fdc-a6d0-b10a436c8c91");
            if (savedRc != null) {
                writeline("/home/ubuntu/results/coverage/Copy/Copy_1_10.coverage", "e56b1685-59e5-4f43-954f-acbd01f68af2");
                rcs.insertElementAt(savedRc, 0);
            }
            writeline("/home/ubuntu/results/coverage/Copy/Copy_1_10.coverage", "4b6ad49d-03c5-437e-929a-c9b65e19e734");
            fileCopyMap.clear();
            writeline("/home/ubuntu/results/coverage/Copy/Copy_1_10.coverage", "a50b2294-9ba6-48fa-a625-1458b76d2292");
            dirCopyMap.clear();
            writeline("/home/ubuntu/results/coverage/Copy/Copy_1_10.coverage", "383dcf8f-a7c5-47e7-a052-c346782a62e6");
            completeDirMap.clear();
        }
    }

    /**
     * *********************************************************************
     * *  protected and private methods
     * **********************************************************************
     */
    private void copySingleFile() {
        writeline("/home/ubuntu/results/coverage/Copy/Copy_1_10.coverage", "2ce5042a-9154-41ff-993e-db9050730852");
        // deal with the single file
        if (file != null) {
            writeline("/home/ubuntu/results/coverage/Copy/Copy_1_10.coverage", "9336a6d4-542b-4ff3-a79c-68d5879e7416");
            if (file.exists()) {
                writeline("/home/ubuntu/results/coverage/Copy/Copy_1_10.coverage", "f34e2c6a-1c58-4298-aeaa-30267f331458");
                if (destFile == null) {
                    writeline("/home/ubuntu/results/coverage/Copy/Copy_1_10.coverage", "49216d01-43c4-4c4c-9a40-e0938facd319");
                    destFile = new File(destDir, file.getName());
                }
                writeline("/home/ubuntu/results/coverage/Copy/Copy_1_10.coverage", "88d0a421-065b-4dac-82cb-fd7422f84270");
                if (forceOverwrite || !destFile.exists() || (file.lastModified() - granularity > destFile.lastModified())) {
                    writeline("/home/ubuntu/results/coverage/Copy/Copy_1_10.coverage", "309cec2f-ea3f-4b5f-8b67-8a38ab682993");
                    fileCopyMap.put(file.getAbsolutePath(), new String[] { destFile.getAbsolutePath() });
                } else {
                    writeline("/home/ubuntu/results/coverage/Copy/Copy_1_10.coverage", "299b71a5-5714-4b12-99b7-30751fcb0900");
                    log(file + " omitted as " + destFile + " is up to date.", Project.MSG_VERBOSE);
                }
            } else {
                writeline("/home/ubuntu/results/coverage/Copy/Copy_1_10.coverage", "26cb21a3-137d-479c-ac0e-e5721026dccc");
                final String message = "Warning: Could not find file " + file.getAbsolutePath() + " to copy.";
                writeline("/home/ubuntu/results/coverage/Copy/Copy_1_10.coverage", "bc8d1451-b62a-4c9c-b890-618cf1669d2c");
                if (!failonerror) {
                    writeline("/home/ubuntu/results/coverage/Copy/Copy_1_10.coverage", "8237b5fc-88db-4fd7-aeba-2eb452f74654");
                    if (!quiet) {
                        writeline("/home/ubuntu/results/coverage/Copy/Copy_1_10.coverage", "88f59cab-5636-49d2-a1fa-9ce4b21c6d8a");
                        log(message, Project.MSG_ERR);
                    }
                } else {
                    writeline("/home/ubuntu/results/coverage/Copy/Copy_1_10.coverage", "9a99f7e8-e98e-42f6-b818-89c5b0a14758");
                    throw new BuildException(message);
                }
            }
        }
    }

    private void iterateOverBaseDirs(final HashSet<File> baseDirs, final HashMap<File, List<String>> dirsByBasedir, final HashMap<File, List<String>> filesByBasedir) {
        writeline("/home/ubuntu/results/coverage/Copy/Copy_1_10.coverage", "d648f6a8-9348-4240-bfc5-5e4b077b2c86");
        for (final File f : baseDirs) {
            writeline("/home/ubuntu/results/coverage/Copy/Copy_1_10.coverage", "4d31b8a4-dd9e-4a0e-b7d6-1ae29b3cf517");
            final List<String> files = filesByBasedir.get(f);
            writeline("/home/ubuntu/results/coverage/Copy/Copy_1_10.coverage", "e1efc9d1-00a7-4a20-8e7e-f54162a5c643");
            final List<String> dirs = dirsByBasedir.get(f);
            writeline("/home/ubuntu/results/coverage/Copy/Copy_1_10.coverage", "ffa223be-6c98-4a06-b7f1-d2273ca1eb29");
            String[] srcFiles = new String[0];
            writeline("/home/ubuntu/results/coverage/Copy/Copy_1_10.coverage", "540fd961-87b8-44cb-9621-d7b8cb6adb29");
            if (files != null) {
                writeline("/home/ubuntu/results/coverage/Copy/Copy_1_10.coverage", "f72ebcff-381b-4160-acf3-f2039e0bac5e");
                srcFiles = files.toArray(srcFiles);
            }
            writeline("/home/ubuntu/results/coverage/Copy/Copy_1_10.coverage", "6bb8d25d-a4bc-44c6-b53b-af9b036a0595");
            String[] srcDirs = new String[0];
            writeline("/home/ubuntu/results/coverage/Copy/Copy_1_10.coverage", "d688d26a-cf5e-4fa5-9fe5-55cef67f0c4c");
            if (dirs != null) {
                writeline("/home/ubuntu/results/coverage/Copy/Copy_1_10.coverage", "7f86d5c8-271b-48dd-8902-91178c86e42a");
                srcDirs = dirs.toArray(srcDirs);
            }
            writeline("/home/ubuntu/results/coverage/Copy/Copy_1_10.coverage", "fc15253e-3f15-41eb-84b1-e3b3bd7e5cad");
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
        writeline("/home/ubuntu/results/coverage/Copy/Copy_1_10.coverage", "bc69b001-4c9e-49a8-9fed-f94a9a8d2369");
        if (file == null && rcs.size() == 0) {
            writeline("/home/ubuntu/results/coverage/Copy/Copy_1_10.coverage", "8f7f1dba-6fd7-4455-b89c-0e392173e997");
            throw new BuildException("Specify at least one source--a file or a resource collection.");
        }
        writeline("/home/ubuntu/results/coverage/Copy/Copy_1_10.coverage", "9a6d3b58-30fd-4723-ba43-1ec884617c8d");
        if (destFile != null && destDir != null) {
            writeline("/home/ubuntu/results/coverage/Copy/Copy_1_10.coverage", "9bbf64f6-f79e-43b4-978e-122116332a0c");
            throw new BuildException("Only one of tofile and todir may be set.");
        }
        writeline("/home/ubuntu/results/coverage/Copy/Copy_1_10.coverage", "03e7b612-7e93-4404-b31f-a387f7136864");
        if (destFile == null && destDir == null) {
            writeline("/home/ubuntu/results/coverage/Copy/Copy_1_10.coverage", "6ce68237-ef21-463a-becf-aa602c4b6388");
            throw new BuildException("One of tofile or todir must be set.");
        }
        writeline("/home/ubuntu/results/coverage/Copy/Copy_1_10.coverage", "3baa0f1a-2c60-4f5c-992e-9a8082571446");
        if (file != null && file.isDirectory()) {
            writeline("/home/ubuntu/results/coverage/Copy/Copy_1_10.coverage", "7abc3c3b-ab48-4410-b58a-27e989f5e317");
            throw new BuildException("Use a resource collection to copy directories.");
        }
        writeline("/home/ubuntu/results/coverage/Copy/Copy_1_10.coverage", "414bc5a9-0476-443a-8eec-a3a1313d3482");
        if (destFile != null && rcs.size() > 0) {
            writeline("/home/ubuntu/results/coverage/Copy/Copy_1_10.coverage", "a842c58b-6b0a-4634-bdf6-e9ef02e49d13");
            if (rcs.size() > 1) {
                writeline("/home/ubuntu/results/coverage/Copy/Copy_1_10.coverage", "87e1db16-a717-43e4-ac06-f9983ae04e28");
                throw new BuildException("Cannot concatenate multiple files into a single file.");
            } else {
                writeline("/home/ubuntu/results/coverage/Copy/Copy_1_10.coverage", "71eafa8b-11a0-47b4-843b-279ce3d2893b");
                final ResourceCollection rc = rcs.elementAt(0);
                writeline("/home/ubuntu/results/coverage/Copy/Copy_1_10.coverage", "404282d6-3d68-4b09-90ff-07cd6da08f6c");
                if (!rc.isFilesystemOnly() && !supportsNonFileResources()) {
                    writeline("/home/ubuntu/results/coverage/Copy/Copy_1_10.coverage", "0896b007-ecee-4087-9d1e-23dbf35e0346");
                    throw new BuildException("Only FileSystem resources are" + " supported.");
                }
                writeline("/home/ubuntu/results/coverage/Copy/Copy_1_10.coverage", "1e1c63a5-90fa-4b06-b006-f7d59ce509f3");
                if (rc.size() == 0) {
                    writeline("/home/ubuntu/results/coverage/Copy/Copy_1_10.coverage", "499c391a-276d-4914-a775-b15118426b69");
                    throw new BuildException(MSG_WHEN_COPYING_EMPTY_RC_TO_FILE);
                } else if (rc.size() == 1) {
                    writeline("/home/ubuntu/results/coverage/Copy/Copy_1_10.coverage", "423f3b99-4768-4d8c-8d29-d2cc81d21cb9");
                    final Resource res = rc.iterator().next();
                    writeline("/home/ubuntu/results/coverage/Copy/Copy_1_10.coverage", "20435ee6-8028-411b-8def-63c71da0097c");
                    final FileProvider r = res.as(FileProvider.class);
                    writeline("/home/ubuntu/results/coverage/Copy/Copy_1_10.coverage", "bd5bdbb1-a0a1-40c2-ab8a-3e89c6051c3f");
                    if (file == null) {
                        writeline("/home/ubuntu/results/coverage/Copy/Copy_1_10.coverage", "675185b4-292d-4e06-9536-0bc526d31bc3");
                        if (r != null) {
                            writeline("/home/ubuntu/results/coverage/Copy/Copy_1_10.coverage", "2c05aa9e-dc8a-48e8-8bf9-c3ed369d9432");
                            file = r.getFile();
                        } else {
                            writeline("/home/ubuntu/results/coverage/Copy/Copy_1_10.coverage", "750cf292-b093-4a37-b546-35326a7ddef4");
                            singleResource = res;
                        }
                        writeline("/home/ubuntu/results/coverage/Copy/Copy_1_10.coverage", "68ef8faf-16a3-4c94-9636-819818178175");
                        rcs.removeElementAt(0);
                    } else {
                        writeline("/home/ubuntu/results/coverage/Copy/Copy_1_10.coverage", "12d0d73e-d229-4d1a-bfd0-9d6be46f6b64");
                        throw new BuildException("Cannot concatenate multiple files into a single file.");
                    }
                } else {
                    writeline("/home/ubuntu/results/coverage/Copy/Copy_1_10.coverage", "aab05a74-3393-47bb-a986-8ab0b1d266ea");
                    throw new BuildException("Cannot concatenate multiple files into a single file.");
                }
            }
        }
        writeline("/home/ubuntu/results/coverage/Copy/Copy_1_10.coverage", "31150644-02ad-404f-8c63-75f985f233b1");
        if (destFile != null) {
            writeline("/home/ubuntu/results/coverage/Copy/Copy_1_10.coverage", "4501ccb6-b510-42a2-bf68-7ca158e440aa");
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
        writeline("/home/ubuntu/results/coverage/Copy/Copy_1_10.coverage", "971aa43f-b65d-49f8-a42f-dace20f25a79");
        final FileNameMapper mapper = getMapper();
        writeline("/home/ubuntu/results/coverage/Copy/Copy_1_10.coverage", "a13c04c5-b96a-4031-9eaa-e8e5cfb820b2");
        buildMap(fromDir, toDir, files, mapper, fileCopyMap);
        writeline("/home/ubuntu/results/coverage/Copy/Copy_1_10.coverage", "cdce34c3-8908-44a6-a7bc-a7882fc5189f");
        if (includeEmpty) {
            writeline("/home/ubuntu/results/coverage/Copy/Copy_1_10.coverage", "0b0c44c5-99e0-4071-88ee-a688d8cfde72");
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
        writeline("/home/ubuntu/results/coverage/Copy/Copy_1_10.coverage", "611ac124-8862-4ab3-a9c9-df97f5101191");
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
        writeline("/home/ubuntu/results/coverage/Copy/Copy_1_10.coverage", "c74659d0-2ba1-4a03-b542-0aaa72185938");
        String[] toCopy = null;
        writeline("/home/ubuntu/results/coverage/Copy/Copy_1_10.coverage", "91c15fa1-e66e-4440-ac46-4b2021326323");
        if (forceOverwrite) {
            writeline("/home/ubuntu/results/coverage/Copy/Copy_1_10.coverage", "2b39b9b0-d049-487c-81ae-26090492dec3");
            final Vector<String> v = new Vector<String>();
            writeline("/home/ubuntu/results/coverage/Copy/Copy_1_10.coverage", "0d921d26-cdec-4aa9-a2b8-30d748c95052");
            for (int i = 0; i < names.length; i++) {
                writeline("/home/ubuntu/results/coverage/Copy/Copy_1_10.coverage", "84fbeca8-de9c-4f11-baae-4ed4f0d31c9c");
                if (mapper.mapFileName(names[i]) != null) {
                    writeline("/home/ubuntu/results/coverage/Copy/Copy_1_10.coverage", "5a68f2c9-64d8-4377-8b35-d6599aa739b6");
                    v.addElement(names[i]);
                }
            }
            writeline("/home/ubuntu/results/coverage/Copy/Copy_1_10.coverage", "0200e743-4c93-49f8-a7e8-ba0b9fd59f91");
            toCopy = new String[v.size()];
            writeline("/home/ubuntu/results/coverage/Copy/Copy_1_10.coverage", "3dc7c4b8-8fe4-4e1c-b2ed-fd34cdd55a34");
            v.copyInto(toCopy);
        } else {
            writeline("/home/ubuntu/results/coverage/Copy/Copy_1_10.coverage", "175b59ad-6d27-4516-a269-a2b250557b99");
            final SourceFileScanner ds = new SourceFileScanner(this);
            writeline("/home/ubuntu/results/coverage/Copy/Copy_1_10.coverage", "b60aeb91-2806-42f5-a5a3-c122c1b83677");
            toCopy = ds.restrict(names, fromDir, toDir, mapper, granularity);
        }
        writeline("/home/ubuntu/results/coverage/Copy/Copy_1_10.coverage", "117464e8-c493-482f-9f01-ab9abb3945ea");
        for (int i = 0; i < toCopy.length; i++) {
            writeline("/home/ubuntu/results/coverage/Copy/Copy_1_10.coverage", "6e1fad49-65a7-4e92-829a-7c67b2767ae5");
            final File src = new File(fromDir, toCopy[i]);
            writeline("/home/ubuntu/results/coverage/Copy/Copy_1_10.coverage", "bda9dfa6-7ace-4a5e-a1f9-e22febada749");
            final String[] mappedFiles = mapper.mapFileName(toCopy[i]);
            writeline("/home/ubuntu/results/coverage/Copy/Copy_1_10.coverage", "0b79021e-7566-49cb-b8cc-c63c5c0653a6");
            if (!enableMultipleMappings) {
                writeline("/home/ubuntu/results/coverage/Copy/Copy_1_10.coverage", "7da3107e-cd5a-4ccd-9f6e-cff8a481725b");
                map.put(src.getAbsolutePath(), new String[] { new File(toDir, mappedFiles[0]).getAbsolutePath() });
            } else {
                writeline("/home/ubuntu/results/coverage/Copy/Copy_1_10.coverage", "7c735374-4d20-4171-830c-86bd58725c96");
                // reuse the array created by the mapper
                for (int k = 0; k < mappedFiles.length; k++) {
                    writeline("/home/ubuntu/results/coverage/Copy/Copy_1_10.coverage", "74addb60-7766-4aa1-b538-66a5d991ff85");
                    mappedFiles[k] = new File(toDir, mappedFiles[k]).getAbsolutePath();
                }
                writeline("/home/ubuntu/results/coverage/Copy/Copy_1_10.coverage", "ce1d434d-f8b5-4eb8-bab1-7a660517ba2c");
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
        writeline("/home/ubuntu/results/coverage/Copy/Copy_1_10.coverage", "2cbe593d-e56b-49b6-8d60-a2042d629cb3");
        final HashMap<Resource, String[]> map = new HashMap<Resource, String[]>();
        writeline("/home/ubuntu/results/coverage/Copy/Copy_1_10.coverage", "2d4bdf77-d558-4311-9e6b-c29df0bd972e");
        Resource[] toCopy = null;
        writeline("/home/ubuntu/results/coverage/Copy/Copy_1_10.coverage", "305b8b30-58cf-4088-a50e-c8cbaad539c8");
        if (forceOverwrite) {
            writeline("/home/ubuntu/results/coverage/Copy/Copy_1_10.coverage", "00037526-2efd-4375-a8b3-f94e221690a1");
            final Vector<Resource> v = new Vector<Resource>();
            writeline("/home/ubuntu/results/coverage/Copy/Copy_1_10.coverage", "b1a07bc8-8d68-4f1f-b893-24bccaf10742");
            for (int i = 0; i < fromResources.length; i++) {
                writeline("/home/ubuntu/results/coverage/Copy/Copy_1_10.coverage", "07d60b6f-af5d-4c57-97e3-b29984ec3a26");
                if (mapper.mapFileName(fromResources[i].getName()) != null) {
                    writeline("/home/ubuntu/results/coverage/Copy/Copy_1_10.coverage", "f444c43c-4d6e-48bd-8f6e-17c2f2811ba2");
                    v.addElement(fromResources[i]);
                }
            }
            writeline("/home/ubuntu/results/coverage/Copy/Copy_1_10.coverage", "79c3ea39-bb81-4b93-8408-234e9ee1f21f");
            toCopy = new Resource[v.size()];
            writeline("/home/ubuntu/results/coverage/Copy/Copy_1_10.coverage", "91ae9809-5f71-4b9b-a04d-cb9754b6b273");
            v.copyInto(toCopy);
        } else {
            writeline("/home/ubuntu/results/coverage/Copy/Copy_1_10.coverage", "ec132e2b-e6c9-4954-bd2d-df2e07de62ed");
            toCopy = ResourceUtils.selectOutOfDateSources(this, fromResources, mapper, new ResourceFactory() {

                public Resource getResource(final String name) {
                    return new FileResource(toDir, name);
                }
            }, granularity);
        }
        writeline("/home/ubuntu/results/coverage/Copy/Copy_1_10.coverage", "223f251b-1809-4ec4-aa90-ef784559e089");
        for (int i = 0; i < toCopy.length; i++) {
            writeline("/home/ubuntu/results/coverage/Copy/Copy_1_10.coverage", "82dcdbab-0558-42c3-9cf9-9e012abd5019");
            final String[] mappedFiles = mapper.mapFileName(toCopy[i].getName());
            writeline("/home/ubuntu/results/coverage/Copy/Copy_1_10.coverage", "c88a8edc-a5d9-4528-a284-a486d446960b");
            for (int j = 0; j < mappedFiles.length; j++) {
                writeline("/home/ubuntu/results/coverage/Copy/Copy_1_10.coverage", "94139e09-e8c5-4e5f-9d16-aa74aa0b82b0");
                if (mappedFiles[j] == null) {
                    writeline("/home/ubuntu/results/coverage/Copy/Copy_1_10.coverage", "20de36af-79d0-4b60-a359-2b0283486d0c");
                    throw new BuildException("Can't copy a resource without a" + " name if the mapper doesn't" + " provide one.");
                }
            }
            writeline("/home/ubuntu/results/coverage/Copy/Copy_1_10.coverage", "b05e093b-637d-4f09-8024-dffdb37e9182");
            if (!enableMultipleMappings) {
                writeline("/home/ubuntu/results/coverage/Copy/Copy_1_10.coverage", "d3f82a47-f3cc-48a7-aeed-b0190b6c033f");
                map.put(toCopy[i], new String[] { new File(toDir, mappedFiles[0]).getAbsolutePath() });
            } else {
                writeline("/home/ubuntu/results/coverage/Copy/Copy_1_10.coverage", "d58094a1-3b8d-489e-b591-a5187d1477e2");
                // reuse the array created by the mapper
                for (int k = 0; k < mappedFiles.length; k++) {
                    writeline("/home/ubuntu/results/coverage/Copy/Copy_1_10.coverage", "11f74d3e-8567-4bfd-94df-e6980d2ca772");
                    mappedFiles[k] = new File(toDir, mappedFiles[k]).getAbsolutePath();
                }
                writeline("/home/ubuntu/results/coverage/Copy/Copy_1_10.coverage", "0484bee1-4063-482d-9376-c5ca2b1361db");
                map.put(toCopy[i], mappedFiles);
            }
        }
        writeline("/home/ubuntu/results/coverage/Copy/Copy_1_10.coverage", "ace240a3-a6bf-4d63-b7e3-698b4aa0839c");
        return map;
    }

    /**
     * Actually does the file (and possibly empty directory) copies.
     * This is a good method for subclasses to override.
     */
    protected void doFileOperations() {
        writeline("/home/ubuntu/results/coverage/Copy/Copy_1_10.coverage", "d8826394-ae6e-49ee-89e5-3b83d2ba8934");
        if (fileCopyMap.size() > 0) {
            writeline("/home/ubuntu/results/coverage/Copy/Copy_1_10.coverage", "d1dc9eda-e353-477b-a00a-a2ad93570da3");
            log("Copying " + fileCopyMap.size() + " file" + (fileCopyMap.size() == 1 ? "" : "s") + " to " + destDir.getAbsolutePath());
            writeline("/home/ubuntu/results/coverage/Copy/Copy_1_10.coverage", "60ac3725-0a68-475b-8148-9ad1b91b7f09");
            for (final Map.Entry<String, String[]> e : fileCopyMap.entrySet()) {
                writeline("/home/ubuntu/results/coverage/Copy/Copy_1_10.coverage", "c8826277-b753-4bf5-9d59-43704d22ed30");
                final String fromFile = e.getKey();
                writeline("/home/ubuntu/results/coverage/Copy/Copy_1_10.coverage", "37cd667a-fb7e-4f27-af17-f03529c1464c");
                final String[] toFiles = e.getValue();
                writeline("/home/ubuntu/results/coverage/Copy/Copy_1_10.coverage", "011ff0cb-42cf-4386-b105-9a0ed8235fd2");
                for (int i = 0; i < toFiles.length; i++) {
                    writeline("/home/ubuntu/results/coverage/Copy/Copy_1_10.coverage", "302390fb-ee25-4264-916f-b6e86200dec9");
                    final String toFile = toFiles[i];
                    writeline("/home/ubuntu/results/coverage/Copy/Copy_1_10.coverage", "92788d51-15c9-4496-8868-0b39be5659eb");
                    if (fromFile.equals(toFile)) {
                        writeline("/home/ubuntu/results/coverage/Copy/Copy_1_10.coverage", "8cbfc4d5-a685-4f3a-81cd-d1230d45c005");
                        log("Skipping self-copy of " + fromFile, verbosity);
                        writeline("/home/ubuntu/results/coverage/Copy/Copy_1_10.coverage", "3b3f8b25-c581-4e47-babc-461fc654b5ba");
                        continue;
                    }
                    writeline("/home/ubuntu/results/coverage/Copy/Copy_1_10.coverage", "8df34fcf-a735-4509-9353-404a1b633796");
                    try {
                        writeline("/home/ubuntu/results/coverage/Copy/Copy_1_10.coverage", "334958a3-6094-4503-8f27-896b2c838dce");
                        log("Copying " + fromFile + " to " + toFile, verbosity);
                        writeline("/home/ubuntu/results/coverage/Copy/Copy_1_10.coverage", "e0145ac9-6675-44c1-b19f-906dce47d127");
                        final FilterSetCollection executionFilters = new FilterSetCollection();
                        writeline("/home/ubuntu/results/coverage/Copy/Copy_1_10.coverage", "4d83d01a-f104-40ec-9e5e-62389bbd12b3");
                        if (filtering) {
                            writeline("/home/ubuntu/results/coverage/Copy/Copy_1_10.coverage", "54d8f0cf-d477-47f7-a4e3-31ee9fa1e22e");
                            executionFilters.addFilterSet(getProject().getGlobalFilterSet());
                        }
                        writeline("/home/ubuntu/results/coverage/Copy/Copy_1_10.coverage", "10c110cb-b18f-49d6-8b6a-fb1148a846cb");
                        for (final FilterSet filterSet : filterSets) {
                            writeline("/home/ubuntu/results/coverage/Copy/Copy_1_10.coverage", "36510026-1f79-4ba7-9279-302b1e63df3d");
                            executionFilters.addFilterSet(filterSet);
                        }
                        writeline("/home/ubuntu/results/coverage/Copy/Copy_1_10.coverage", "8fe1df6c-16c4-4440-a962-595b231ee60d");
                        fileUtils.copyFile(new File(fromFile), new File(toFile), executionFilters, filterChains, forceOverwrite, preserveLastModified, /* append: */
                        false, inputEncoding, outputEncoding, getProject(), getForce());
                    } catch (final IOException ioe) {
                        writeline("/home/ubuntu/results/coverage/Copy/Copy_1_10.coverage", "2d61bb66-85dd-4f6c-b414-1e7ea0b5339c");
                        String msg = "Failed to copy " + fromFile + " to " + toFile + " due to " + getDueTo(ioe);
                        writeline("/home/ubuntu/results/coverage/Copy/Copy_1_10.coverage", "2aef19be-3ec4-4350-b4b6-9ef5752fc834");
                        final File targetFile = new File(toFile);
                        writeline("/home/ubuntu/results/coverage/Copy/Copy_1_10.coverage", "3d402e87-d24a-4185-97ae-0e54dbed3d36");
                        if (!(ioe instanceof ResourceUtils.ReadOnlyTargetFileException) && targetFile.exists() && !targetFile.delete()) {
                            writeline("/home/ubuntu/results/coverage/Copy/Copy_1_10.coverage", "d9266884-5e40-44dd-a2dd-7b841c477423");
                            msg += " and I couldn't delete the corrupt " + toFile;
                        }
                        writeline("/home/ubuntu/results/coverage/Copy/Copy_1_10.coverage", "66943e95-f96e-4c8b-897a-3a47bb26a385");
                        if (failonerror) {
                            writeline("/home/ubuntu/results/coverage/Copy/Copy_1_10.coverage", "c3a4c155-f522-4f9f-acbb-0a8365febde1");
                            throw new BuildException(msg, ioe, getLocation());
                        }
                        writeline("/home/ubuntu/results/coverage/Copy/Copy_1_10.coverage", "ecfec175-95a8-4bc0-89f9-46ad50dfd31d");
                        log(msg, Project.MSG_ERR);
                    }
                }
            }
        }
        writeline("/home/ubuntu/results/coverage/Copy/Copy_1_10.coverage", "2b3577c1-c7d7-4890-8afc-e786f7453734");
        if (includeEmpty) {
            writeline("/home/ubuntu/results/coverage/Copy/Copy_1_10.coverage", "afb427a0-9732-414b-ad4b-94ea816fab4b");
            int createCount = 0;
            writeline("/home/ubuntu/results/coverage/Copy/Copy_1_10.coverage", "df7d867e-dec1-4611-8d4c-c5762ec85800");
            for (final String[] dirs : dirCopyMap.values()) {
                writeline("/home/ubuntu/results/coverage/Copy/Copy_1_10.coverage", "1f632972-4cdd-43ae-9b01-576f49b3a542");
                for (int i = 0; i < dirs.length; i++) {
                    writeline("/home/ubuntu/results/coverage/Copy/Copy_1_10.coverage", "4c3a5331-e7d4-45f2-9ff3-861e12455153");
                    final File d = new File(dirs[i]);
                    writeline("/home/ubuntu/results/coverage/Copy/Copy_1_10.coverage", "b4bc092f-4a4f-42bb-8011-f27aeb69ab66");
                    if (!d.exists()) {
                        writeline("/home/ubuntu/results/coverage/Copy/Copy_1_10.coverage", "b189184e-be27-4c87-810e-3cbe6dcbf245");
                        if (!(d.mkdirs() || d.isDirectory())) {
                            writeline("/home/ubuntu/results/coverage/Copy/Copy_1_10.coverage", "66ebe8f2-5a27-4b2a-99b3-fd7760a08412");
                            log("Unable to create directory " + d.getAbsolutePath(), Project.MSG_ERR);
                        } else {
                            writeline("/home/ubuntu/results/coverage/Copy/Copy_1_10.coverage", "7e3bb47c-19c1-4232-b0b6-409afe20b0ed");
                            createCount++;
                        }
                    }
                }
            }
            writeline("/home/ubuntu/results/coverage/Copy/Copy_1_10.coverage", "01a66c5c-8aab-4847-bca5-1056db8f37c7");
            if (createCount > 0) {
                writeline("/home/ubuntu/results/coverage/Copy/Copy_1_10.coverage", "dbb75cfd-9d06-4bde-ab40-1c6fada616d7");
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
        writeline("/home/ubuntu/results/coverage/Copy/Copy_1_10.coverage", "00ed385e-cb55-41a3-b43d-a784c652931e");
        if (map.size() > 0) {
            writeline("/home/ubuntu/results/coverage/Copy/Copy_1_10.coverage", "3226469c-a94f-4bc2-832c-0c4ea12ace95");
            log("Copying " + map.size() + " resource" + (map.size() == 1 ? "" : "s") + " to " + destDir.getAbsolutePath());
            writeline("/home/ubuntu/results/coverage/Copy/Copy_1_10.coverage", "1200d86a-c382-4164-8f12-29c9e8541b62");
            for (final Map.Entry<Resource, String[]> e : map.entrySet()) {
                writeline("/home/ubuntu/results/coverage/Copy/Copy_1_10.coverage", "830b974f-8717-4642-bd6c-adaf2fc0d1e1");
                final Resource fromResource = e.getKey();
                writeline("/home/ubuntu/results/coverage/Copy/Copy_1_10.coverage", "2919071d-ded1-4c81-bda2-a44106adf15e");
                for (final String toFile : e.getValue()) {
                    writeline("/home/ubuntu/results/coverage/Copy/Copy_1_10.coverage", "b5eee2d3-5cee-44d4-a485-22a013b4338e");
                    try {
                        writeline("/home/ubuntu/results/coverage/Copy/Copy_1_10.coverage", "1ef3964a-75d6-4e97-b19d-65a0f0ed3f84");
                        log("Copying " + fromResource + " to " + toFile, verbosity);
                        writeline("/home/ubuntu/results/coverage/Copy/Copy_1_10.coverage", "235a3a1b-e00b-43a8-915c-c40182f5e33b");
                        final FilterSetCollection executionFilters = new FilterSetCollection();
                        writeline("/home/ubuntu/results/coverage/Copy/Copy_1_10.coverage", "450cb423-1894-4433-b514-f47df27f4269");
                        if (filtering) {
                            writeline("/home/ubuntu/results/coverage/Copy/Copy_1_10.coverage", "9e18cace-c6d4-4223-88b0-42bdb211efc9");
                            executionFilters.addFilterSet(getProject().getGlobalFilterSet());
                        }
                        writeline("/home/ubuntu/results/coverage/Copy/Copy_1_10.coverage", "1fa42b49-ff09-4d1b-a2ce-6a4fd55fbbd1");
                        for (final FilterSet filterSet : filterSets) {
                            writeline("/home/ubuntu/results/coverage/Copy/Copy_1_10.coverage", "0d0e0690-bb87-48ea-822f-ea684e2a971f");
                            executionFilters.addFilterSet(filterSet);
                        }
                        writeline("/home/ubuntu/results/coverage/Copy/Copy_1_10.coverage", "8b659a30-4474-4e42-9754-59b13081b4b5");
                        ResourceUtils.copyResource(fromResource, new FileResource(destDir, toFile), executionFilters, filterChains, forceOverwrite, preserveLastModified, /* append: */
                        false, inputEncoding, outputEncoding, getProject(), getForce());
                    } catch (final IOException ioe) {
                        writeline("/home/ubuntu/results/coverage/Copy/Copy_1_10.coverage", "9d143e38-7d9b-4dc7-8230-4f14b44e3209");
                        String msg = "Failed to copy " + fromResource + " to " + toFile + " due to " + getDueTo(ioe);
                        writeline("/home/ubuntu/results/coverage/Copy/Copy_1_10.coverage", "c88769f0-6227-441c-8fed-9f457e18da96");
                        final File targetFile = new File(toFile);
                        writeline("/home/ubuntu/results/coverage/Copy/Copy_1_10.coverage", "3fe8e131-b6e6-475b-bcb1-9e45dde8415c");
                        if (!(ioe instanceof ResourceUtils.ReadOnlyTargetFileException) && targetFile.exists() && !targetFile.delete()) {
                            writeline("/home/ubuntu/results/coverage/Copy/Copy_1_10.coverage", "4338c099-2d63-43b0-a8bb-2041392d1253");
                            msg += " and I couldn't delete the corrupt " + toFile;
                        }
                        writeline("/home/ubuntu/results/coverage/Copy/Copy_1_10.coverage", "a602b03f-478f-4415-94aa-82d7e546bf2c");
                        if (failonerror) {
                            writeline("/home/ubuntu/results/coverage/Copy/Copy_1_10.coverage", "ae180dbf-20a2-481d-9953-73912681b976");
                            throw new BuildException(msg, ioe, getLocation());
                        }
                        writeline("/home/ubuntu/results/coverage/Copy/Copy_1_10.coverage", "3efd7b91-7efe-4c7e-a56d-c94678770235");
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
        writeline("/home/ubuntu/results/coverage/Copy/Copy_1_10.coverage", "78a89e7c-8431-40be-b81f-55546227d668");
        return getClass().equals(Copy.class);
    }

    /**
     * Adds the given strings to a list contained in the given map.
     * The file is the key into the map.
     */
    private static void add(File baseDir, final String[] names, final Map<File, List<String>> m) {
        writelineStatic("/home/ubuntu/results/coverage/Copy/Copy_1_10.coverage", "378cb9e2-be08-4b21-9031-8da03e05329b");
        if (names != null) {
            writelineStatic("/home/ubuntu/results/coverage/Copy/Copy_1_10.coverage", "fe9a9ced-8aa0-4290-a52b-0efdca9be6b9");
            baseDir = getKeyFile(baseDir);
            writelineStatic("/home/ubuntu/results/coverage/Copy/Copy_1_10.coverage", "5fa40f11-3309-49c1-95f3-af07c5b58110");
            List<String> l = m.get(baseDir);
            writelineStatic("/home/ubuntu/results/coverage/Copy/Copy_1_10.coverage", "e3634750-e004-4af5-8098-bc7c6ab45b7f");
            if (l == null) {
                writelineStatic("/home/ubuntu/results/coverage/Copy/Copy_1_10.coverage", "51407e57-33f0-4732-a770-8fd148697540");
                l = new ArrayList<String>(names.length);
                writelineStatic("/home/ubuntu/results/coverage/Copy/Copy_1_10.coverage", "11972019-886c-4b5b-9258-44e829ea7c9f");
                m.put(baseDir, l);
            }
            writelineStatic("/home/ubuntu/results/coverage/Copy/Copy_1_10.coverage", "bed683e1-f958-43ce-929d-2f73aa33a95a");
            l.addAll(java.util.Arrays.asList(names));
        }
    }

    /**
     * Adds the given string to a list contained in the given map.
     * The file is the key into the map.
     */
    private static void add(final File baseDir, final String name, final Map<File, List<String>> m) {
        writelineStatic("/home/ubuntu/results/coverage/Copy/Copy_1_10.coverage", "8afd57dd-67ba-438e-93cf-08c1b03cf658");
        if (name != null) {
            writelineStatic("/home/ubuntu/results/coverage/Copy/Copy_1_10.coverage", "00cfd536-6f3f-420d-a6ec-dc65ec02508c");
            add(baseDir, new String[] { name }, m);
        }
    }

    /**
     * Either returns its argument or a plaeholder if the argument is null.
     */
    private static File getKeyFile(final File f) {
        writelineStatic("/home/ubuntu/results/coverage/Copy/Copy_1_10.coverage", "d0b379e8-735c-4fac-adbe-fe7e38d89a4a");
        return f == null ? NULL_FILE_PLACEHOLDER : f;
    }

    /**
     * returns the mapper to use based on nested elements or the
     * flatten attribute.
     */
    private FileNameMapper getMapper() {
        writeline("/home/ubuntu/results/coverage/Copy/Copy_1_10.coverage", "f0135735-9252-4ebd-80f5-9192cb2c14aa");
        FileNameMapper mapper = null;
        writeline("/home/ubuntu/results/coverage/Copy/Copy_1_10.coverage", "91e0200a-dab8-42f9-8919-71aa07ba41c2");
        if (mapperElement != null) {
            writeline("/home/ubuntu/results/coverage/Copy/Copy_1_10.coverage", "da4be0b4-9ae4-4fad-b688-de27ebb9001c");
            mapper = mapperElement.getImplementation();
        } else if (flatten) {
            writeline("/home/ubuntu/results/coverage/Copy/Copy_1_10.coverage", "2bc56157-7348-4d50-9199-81e687cc3604");
            mapper = new FlatFileNameMapper();
        } else {
            writeline("/home/ubuntu/results/coverage/Copy/Copy_1_10.coverage", "237905d5-1e5d-46f4-a50f-f9d1991230bc");
            mapper = new IdentityMapper();
        }
        writeline("/home/ubuntu/results/coverage/Copy/Copy_1_10.coverage", "bb4476fa-8701-410b-a7c5-9c6500458681");
        return mapper;
    }

    /**
     * Handle getMessage() for exceptions.
     * @param ex the exception to handle
     * @return ex.getMessage() if ex.getMessage() is not null
     * otherwise return ex.toString()
     */
    private String getMessage(final Exception ex) {
        writeline("/home/ubuntu/results/coverage/Copy/Copy_1_10.coverage", "54c78ee6-ffb6-45be-9e7c-8c104271a7f7");
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
        writeline("/home/ubuntu/results/coverage/Copy/Copy_1_10.coverage", "96c7429c-12fc-44a9-a79b-8269f82d1cd7");
        final boolean baseIOException = ex.getClass() == IOException.class;
        writeline("/home/ubuntu/results/coverage/Copy/Copy_1_10.coverage", "8b972d13-11b1-4ba6-adf5-86668c0f8e78");
        final StringBuffer message = new StringBuffer();
        writeline("/home/ubuntu/results/coverage/Copy/Copy_1_10.coverage", "09160f72-2316-4162-bebd-51d96060fd15");
        if (!baseIOException || ex.getMessage() == null) {
            writeline("/home/ubuntu/results/coverage/Copy/Copy_1_10.coverage", "738c90ad-1cbd-43d3-ba68-fa9cb16de57c");
            message.append(ex.getClass().getName());
        }
        writeline("/home/ubuntu/results/coverage/Copy/Copy_1_10.coverage", "aab1a7e0-f153-4982-9da2-878c4d3a4887");
        if (ex.getMessage() != null) {
            writeline("/home/ubuntu/results/coverage/Copy/Copy_1_10.coverage", "6a71e20b-4631-456d-a5da-d526f6d89ad9");
            if (!baseIOException) {
                writeline("/home/ubuntu/results/coverage/Copy/Copy_1_10.coverage", "89653beb-c884-497f-b125-65e55f5ab3c6");
                message.append(" ");
            }
            writeline("/home/ubuntu/results/coverage/Copy/Copy_1_10.coverage", "c61ff73a-6784-4a6c-9cb5-7029b7c5c932");
            message.append(ex.getMessage());
        }
        writeline("/home/ubuntu/results/coverage/Copy/Copy_1_10.coverage", "c4e1261f-99f7-4f54-a3b5-13b4af865fa5");
        if (ex.getClass().getName().indexOf("MalformedInput") != -1) {
            writeline("/home/ubuntu/results/coverage/Copy/Copy_1_10.coverage", "9446fc85-750f-4a35-8433-b32de8df3a2b");
            message.append(LINE_SEPARATOR);
            writeline("/home/ubuntu/results/coverage/Copy/Copy_1_10.coverage", "930fde5f-616b-4cd6-a4f0-3f8861ba5da8");
            message.append("This is normally due to the input file containing invalid");
            writeline("/home/ubuntu/results/coverage/Copy/Copy_1_10.coverage", "1a6a652e-daf2-4676-a982-facc1b3f0b5b");
            message.append(LINE_SEPARATOR);
            writeline("/home/ubuntu/results/coverage/Copy/Copy_1_10.coverage", "e4c34332-c1c6-4320-a290-117c3a2ac3f8");
            message.append("bytes for the character encoding used : ");
            writeline("/home/ubuntu/results/coverage/Copy/Copy_1_10.coverage", "8d6bdb33-7e5e-487c-b2ed-7e8ac997f81f");
            message.append((inputEncoding == null ? fileUtils.getDefaultEncoding() : inputEncoding));
            writeline("/home/ubuntu/results/coverage/Copy/Copy_1_10.coverage", "451110de-1f69-4c78-a87d-5f193444a5ae");
            message.append(LINE_SEPARATOR);
        }
        writeline("/home/ubuntu/results/coverage/Copy/Copy_1_10.coverage", "b94d58fc-5d60-47d7-bfb4-f6c8961768f3");
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
