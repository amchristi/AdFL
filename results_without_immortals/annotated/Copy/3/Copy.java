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
        writeline("/home/ubuntu/results/coverage/Copy/Copy_3_10.coverage", "5d6a1357-dd4d-449f-bb4c-3b64d017fb49");
        return fileUtils;
    }

    /**
     * Set a single source file to copy.
     * @param file the file to copy.
     */
    public void setFile(final File file) {
        writeline("/home/ubuntu/results/coverage/Copy/Copy_3_10.coverage", "26fed1c0-cca5-4c1d-b181-8e4b557c88ba");
        this.file = file;
    }

    /**
     * Set the destination file.
     * @param destFile the file to copy to.
     */
    public void setTofile(final File destFile) {
        writeline("/home/ubuntu/results/coverage/Copy/Copy_3_10.coverage", "e0d8f688-0cf8-4615-9f07-42d277d65504");
        this.destFile = destFile;
    }

    /**
     * Set the destination directory.
     * @param destDir the destination directory.
     */
    public void setTodir(final File destDir) {
        writeline("/home/ubuntu/results/coverage/Copy/Copy_3_10.coverage", "55338a04-0fa5-450f-a446-0f1db4b0c4c3");
        this.destDir = destDir;
    }

    /**
     * Add a FilterChain.
     * @return a filter chain object.
     */
    public FilterChain createFilterChain() {
        writeline("/home/ubuntu/results/coverage/Copy/Copy_3_10.coverage", "183c11e6-f7ce-4705-bd3e-34fcc2468207");
        final FilterChain filterChain = new FilterChain();
        writeline("/home/ubuntu/results/coverage/Copy/Copy_3_10.coverage", "1c2faf54-cd9a-4774-ad83-65069779b971");
        filterChains.addElement(filterChain);
        writeline("/home/ubuntu/results/coverage/Copy/Copy_3_10.coverage", "eec54a97-562b-49f3-8eaa-a211c9bbcdd7");
        return filterChain;
    }

    /**
     * Add a filterset.
     * @return a filter set object.
     */
    public FilterSet createFilterSet() {
        writeline("/home/ubuntu/results/coverage/Copy/Copy_3_10.coverage", "7a216291-a431-4462-b34c-73c3bb1cf3c8");
        final FilterSet filterSet = new FilterSet();
        writeline("/home/ubuntu/results/coverage/Copy/Copy_3_10.coverage", "5a28e069-2c60-45ad-acaf-dc2fa414031e");
        filterSets.addElement(filterSet);
        writeline("/home/ubuntu/results/coverage/Copy/Copy_3_10.coverage", "cee04766-9c35-4844-9da6-9324a07ebc8b");
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
        writeline("/home/ubuntu/results/coverage/Copy/Copy_3_10.coverage", "f6a6cf46-e148-4acf-a763-517e06faa0e0");
        setPreserveLastModified(Project.toBoolean(preserve));
    }

    /**
     * Give the copied files the same last modified time as the original files.
     * @param preserve if true preserve the modified time; default is false.
     */
    public void setPreserveLastModified(final boolean preserve) {
        writeline("/home/ubuntu/results/coverage/Copy/Copy_3_10.coverage", "5472470b-a9d3-4fe8-9fa8-6d66c27233c3");
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
        writeline("/home/ubuntu/results/coverage/Copy/Copy_3_10.coverage", "84c6fb1b-b2c7-4b20-8bb9-0cdf7a3e8206");
        return preserveLastModified;
    }

    /**
     * Get the filtersets being applied to this operation.
     *
     * @return a vector of FilterSet objects.
     */
    protected Vector<FilterSet> getFilterSets() {
        writeline("/home/ubuntu/results/coverage/Copy/Copy_3_10.coverage", "4b2a0cbc-5fae-4b6c-83bf-180c5dcd8ce6");
        return filterSets;
    }

    /**
     * Get the filterchains being applied to this operation.
     *
     * @return a vector of FilterChain objects.
     */
    protected Vector<FilterChain> getFilterChains() {
        writeline("/home/ubuntu/results/coverage/Copy/Copy_3_10.coverage", "ede30d78-5174-4105-a448-42a32ae35362");
        return filterChains;
    }

    /**
     * Set filtering mode.
     * @param filtering if true enable filtering; default is false.
     */
    public void setFiltering(final boolean filtering) {
        writeline("/home/ubuntu/results/coverage/Copy/Copy_3_10.coverage", "49f9b2fb-2544-4f2c-9bae-565aa1675218");
        this.filtering = filtering;
    }

    /**
     * Set overwrite mode regarding existing destination file(s).
     * @param overwrite if true force overwriting of destination file(s)
     * even if the destination file(s) are younger than
     * the corresponding source file. Default is false.
     */
    public void setOverwrite(final boolean overwrite) {
        writeline("/home/ubuntu/results/coverage/Copy/Copy_3_10.coverage", "0f5688f0-b324-4bba-bbff-7541f9c3c6a7");
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
        writeline("/home/ubuntu/results/coverage/Copy/Copy_3_10.coverage", "558dc2cb-479e-40ef-9886-ee21d121fb2c");
        force = f;
    }

    /**
     * Whether read-only destinations will be overwritten.
     *
     * @since Ant 1.8.2
     */
    public boolean getForce() {
        writeline("/home/ubuntu/results/coverage/Copy/Copy_3_10.coverage", "946e4462-c2e9-45e6-ac02-f9f16234bd8f");
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
        writeline("/home/ubuntu/results/coverage/Copy/Copy_3_10.coverage", "a8e4ed9f-8b39-411f-a80d-3efd864410e8");
        this.flatten = flatten;
    }

    /**
     * Set verbose mode. Used to force listing of all names of copied files.
     * @param verbose whether to output the names of copied files.
     * Default is false.
     */
    public void setVerbose(final boolean verbose) {
        writeline("/home/ubuntu/results/coverage/Copy/Copy_3_10.coverage", "1e75e4c4-e206-452f-a5fe-18c682fd45d9");
        this.verbosity = verbose ? Project.MSG_INFO : Project.MSG_VERBOSE;
    }

    /**
     * Set whether to copy empty directories.
     * @param includeEmpty if true copy empty directories. Default is true.
     */
    public void setIncludeEmptyDirs(final boolean includeEmpty) {
        writeline("/home/ubuntu/results/coverage/Copy/Copy_3_10.coverage", "6198bc6d-7095-43a2-bf61-694d7b949e6e");
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
        writeline("/home/ubuntu/results/coverage/Copy/Copy_3_10.coverage", "cb336854-bede-45f7-ad4d-779ebc957a8b");
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
        writeline("/home/ubuntu/results/coverage/Copy/Copy_3_10.coverage", "2d4d09c9-2d4d-4250-8f71-c70ccde58a5b");
        this.enableMultipleMappings = enableMultipleMappings;
    }

    /**
     * Get whether multiple mapping is enabled.
     * @return true if multiple mapping is enabled; false otherwise.
     */
    public boolean isEnableMultipleMapping() {
        writeline("/home/ubuntu/results/coverage/Copy/Copy_3_10.coverage", "bb46dbce-13bb-4f47-b08c-b34aa7a597ba");
        return enableMultipleMappings;
    }

    /**
     * Set whether to fail when errors are encountered. If false, note errors
     * to the output but keep going. Default is true.
     * @param failonerror true or false.
     */
    public void setFailOnError(final boolean failonerror) {
        writeline("/home/ubuntu/results/coverage/Copy/Copy_3_10.coverage", "b3f15492-ece0-495b-b5b6-b4792f465ad2");
        this.failonerror = failonerror;
    }

    /**
     * Add a set of files to copy.
     * @param set a set of files to copy.
     */
    public void addFileset(final FileSet set) {
        writeline("/home/ubuntu/results/coverage/Copy/Copy_3_10.coverage", "5b3d79ff-5e5f-4f4d-a48e-ee2bcb0a08ae");
        add(set);
    }

    /**
     * Add a collection of files to copy.
     * @param res a resource collection to copy.
     * @since Ant 1.7
     */
    public void add(final ResourceCollection res) {
        writeline("/home/ubuntu/results/coverage/Copy/Copy_3_10.coverage", "d034a5db-9bb5-4e11-b116-720f77128f74");
        rcs.add(res);
    }

    /**
     * Define the mapper to map source to destination files.
     * @return a mapper to be configured.
     * @exception BuildException if more than one mapper is defined.
     */
    public Mapper createMapper() throws BuildException {
        writeline("/home/ubuntu/results/coverage/Copy/Copy_3_10.coverage", "7c8569ff-1a2b-46fd-adec-37af80680949");
        if (mapperElement != null) {
            writeline("/home/ubuntu/results/coverage/Copy/Copy_3_10.coverage", "ff8dc887-c3a2-4bd4-bf01-620a9bc61195");
            throw new BuildException("Cannot define more than one mapper", getLocation());
        }
        writeline("/home/ubuntu/results/coverage/Copy/Copy_3_10.coverage", "cc044ff3-0f02-486d-a51b-ec08d13c4597");
        mapperElement = new Mapper(getProject());
        writeline("/home/ubuntu/results/coverage/Copy/Copy_3_10.coverage", "83cbf13b-fffa-42b8-9acd-c48510451acf");
        return mapperElement;
    }

    /**
     * Add a nested filenamemapper.
     * @param fileNameMapper the mapper to add.
     * @since Ant 1.6.3
     */
    public void add(final FileNameMapper fileNameMapper) {
        writeline("/home/ubuntu/results/coverage/Copy/Copy_3_10.coverage", "9ca0b152-b2b6-4183-aa07-7be6b433f75c");
        createMapper().add(fileNameMapper);
    }

    /**
     * Set the character encoding.
     * @param encoding the character encoding.
     * @since 1.32, Ant 1.5
     */
    public void setEncoding(final String encoding) {
        writeline("/home/ubuntu/results/coverage/Copy/Copy_3_10.coverage", "efd0fcc5-cc81-45c4-83cc-944e0aba81a0");
        this.inputEncoding = encoding;
        writeline("/home/ubuntu/results/coverage/Copy/Copy_3_10.coverage", "ecb264cc-f495-4d90-8567-a449d1e30773");
        if (outputEncoding == null) {
            writeline("/home/ubuntu/results/coverage/Copy/Copy_3_10.coverage", "15bf7c49-c24e-48e1-81ec-337111d98905");
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
        writeline("/home/ubuntu/results/coverage/Copy/Copy_3_10.coverage", "35e6977e-2663-4f39-80e0-0ca4371b2bd0");
        return inputEncoding;
    }

    /**
     * Set the character encoding for output files.
     * @param encoding the output character encoding.
     * @since Ant 1.6
     */
    public void setOutputEncoding(final String encoding) {
        writeline("/home/ubuntu/results/coverage/Copy/Copy_3_10.coverage", "c52a6ecd-875d-4107-b82e-a7dbcfc0fa3c");
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
        writeline("/home/ubuntu/results/coverage/Copy/Copy_3_10.coverage", "60aa9347-45a9-444a-a62b-653704c48fc3");
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
        writeline("/home/ubuntu/results/coverage/Copy/Copy_3_10.coverage", "3e4918a4-8c2b-4203-98a3-f409c77ebac3");
        this.granularity = granularity;
    }

    /**
     * Perform the copy operation.
     * @exception BuildException if an error occurs.
     */
    @Override
    public void execute() throws BuildException {
        writeline("/home/ubuntu/results/coverage/Copy/Copy_3_10.coverage", "e7e74374-09ed-430a-a8c6-bf00798e8adc");
        // may be altered in validateAttributes
        final File savedFile = file;
        writeline("/home/ubuntu/results/coverage/Copy/Copy_3_10.coverage", "9ad84e89-8501-4de1-b5f6-490241c7ad56");
        final File savedDestFile = destFile;
        writeline("/home/ubuntu/results/coverage/Copy/Copy_3_10.coverage", "31570282-08ac-4487-8d28-0d1b34e41223");
        final File savedDestDir = destDir;
        writeline("/home/ubuntu/results/coverage/Copy/Copy_3_10.coverage", "6babfc7b-7f32-42b9-a893-496fc1e0f18d");
        ResourceCollection savedRc = null;
        writeline("/home/ubuntu/results/coverage/Copy/Copy_3_10.coverage", "4bc83a2c-398a-4639-ba3a-9e6146edd99b");
        if (file == null && destFile != null && rcs.size() == 1) {
            writeline("/home/ubuntu/results/coverage/Copy/Copy_3_10.coverage", "051b13c0-3784-4d69-9ed8-c2037fa69001");
            // will be removed in validateAttributes
            savedRc = rcs.elementAt(0);
        }
        writeline("/home/ubuntu/results/coverage/Copy/Copy_3_10.coverage", "10308d44-b076-49a9-b25e-32bc2196cd0b");
        try {
            writeline("/home/ubuntu/results/coverage/Copy/Copy_3_10.coverage", "00d16d52-e05b-42a7-bbc2-286fa84af62d");
            // make sure we don't have an illegal set of options
            try {
                writeline("/home/ubuntu/results/coverage/Copy/Copy_3_10.coverage", "719a5a1e-2383-4afb-9360-b49d5ae746a2");
                validateAttributes();
            } catch (final BuildException e) {
                writeline("/home/ubuntu/results/coverage/Copy/Copy_3_10.coverage", "2f7c7186-df0e-47a2-bd12-3cbdd2159b94");
                if (failonerror || !getMessage(e).equals(MSG_WHEN_COPYING_EMPTY_RC_TO_FILE)) {
                    writeline("/home/ubuntu/results/coverage/Copy/Copy_3_10.coverage", "acb30255-6c61-4a06-9373-88fab19ecfe1");
                    throw e;
                } else {
                    writeline("/home/ubuntu/results/coverage/Copy/Copy_3_10.coverage", "cb7f9791-eaf0-4d22-9cfd-600878cf1064");
                    log("Warning: " + getMessage(e), Project.MSG_ERR);
                    writeline("/home/ubuntu/results/coverage/Copy/Copy_3_10.coverage", "6228cdd9-e072-4104-866b-f4e4ef545a5d");
                    return;
                }
            }
            writeline("/home/ubuntu/results/coverage/Copy/Copy_3_10.coverage", "fe289701-8b40-4245-8809-681e8441c668");
            // deal with the single file
            copySingleFile();
            writeline("/home/ubuntu/results/coverage/Copy/Copy_3_10.coverage", "9481d969-c002-46ab-bfd9-1fc4864fefba");
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
            writeline("/home/ubuntu/results/coverage/Copy/Copy_3_10.coverage", "06412c8f-2057-4380-9010-cee16b79b35b");
            final HashMap<File, List<String>> dirsByBasedir = new HashMap<File, List<String>>();
            writeline("/home/ubuntu/results/coverage/Copy/Copy_3_10.coverage", "ee934631-7557-4c00-91e9-3f6615702ebf");
            final HashSet<File> baseDirs = new HashSet<File>();
            writeline("/home/ubuntu/results/coverage/Copy/Copy_3_10.coverage", "92427f9a-a3e0-43c1-9918-6fbdecf95919");
            final ArrayList<Resource> nonFileResources = new ArrayList<Resource>();
            writeline("/home/ubuntu/results/coverage/Copy/Copy_3_10.coverage", "91235125-2b61-453d-b190-e597cf86d16a");
            final int size = rcs.size();
            writeline("/home/ubuntu/results/coverage/Copy/Copy_3_10.coverage", "a91b6b51-1580-4e4b-9e16-8084c11d7655");
            for (int i = 0; i < size; i++) {
                writeline("/home/ubuntu/results/coverage/Copy/Copy_3_10.coverage", "d11bc68b-d17c-4473-bfb8-5557321d2ceb");
                final ResourceCollection rc = rcs.elementAt(i);
                writeline("/home/ubuntu/results/coverage/Copy/Copy_3_10.coverage", "b23fdcec-2b55-450c-9283-9f67401db77b");
                // Step (1) - beware of the ZipFileSet
                if (rc instanceof FileSet && rc.isFilesystemOnly()) {
                    writeline("/home/ubuntu/results/coverage/Copy/Copy_3_10.coverage", "049711b4-f3f4-43cc-8b6e-517f48b326d8");
                    final FileSet fs = (FileSet) rc;
                    writeline("/home/ubuntu/results/coverage/Copy/Copy_3_10.coverage", "aac6fbb3-6bf6-4027-9590-386bc5f8dfce");
                    DirectoryScanner ds = null;
                    writeline("/home/ubuntu/results/coverage/Copy/Copy_3_10.coverage", "38edb783-ee07-4bdb-8a78-f5201f5a2261");
                    try {
                        writeline("/home/ubuntu/results/coverage/Copy/Copy_3_10.coverage", "22c76e69-6644-473b-aa92-90c3c98d1646");
                        ds = fs.getDirectoryScanner(getProject());
                    } catch (final BuildException e) {
                        writeline("/home/ubuntu/results/coverage/Copy/Copy_3_10.coverage", "3f66a60c-d03c-43db-a469-013591206150");
                        if (failonerror || !getMessage(e).endsWith(DirectoryScanner.DOES_NOT_EXIST_POSTFIX)) {
                            writeline("/home/ubuntu/results/coverage/Copy/Copy_3_10.coverage", "af304a22-52a5-4b65-87f0-d888ad7d178f");
                            throw e;
                        } else {
                            writeline("/home/ubuntu/results/coverage/Copy/Copy_3_10.coverage", "351bc9b6-0834-4923-8fe7-0e9464fe50f2");
                            if (!quiet) {
                                writeline("/home/ubuntu/results/coverage/Copy/Copy_3_10.coverage", "2203269c-0e0c-49ee-914e-538a899b2214");
                                log("Warning: " + getMessage(e), Project.MSG_ERR);
                            }
                            writeline("/home/ubuntu/results/coverage/Copy/Copy_3_10.coverage", "334f9ef3-99a8-4c12-8128-f62d787c0168");
                            continue;
                        }
                    }
                    writeline("/home/ubuntu/results/coverage/Copy/Copy_3_10.coverage", "2350fdeb-56e0-42ed-b680-9cdfa4113249");
                    final File fromDir = fs.getDir(getProject());
                    writeline("/home/ubuntu/results/coverage/Copy/Copy_3_10.coverage", "db7ac089-f299-44ac-99e0-ae8d8b33d04e");
                    final String[] srcFiles = ds.getIncludedFiles();
                    writeline("/home/ubuntu/results/coverage/Copy/Copy_3_10.coverage", "9a321310-37b5-4bc8-8ee5-dc116e228d2a");
                    final String[] srcDirs = ds.getIncludedDirectories();
                    writeline("/home/ubuntu/results/coverage/Copy/Copy_3_10.coverage", "1285be01-d682-4213-bd3d-fe03e152aee6");
                    if (!flatten && mapperElement == null && ds.isEverythingIncluded() && !fs.hasPatterns()) {
                        writeline("/home/ubuntu/results/coverage/Copy/Copy_3_10.coverage", "e956f2a5-2f2e-4f20-9b0f-4c9b02d50782");
                        completeDirMap.put(fromDir, destDir);
                    }
                    writeline("/home/ubuntu/results/coverage/Copy/Copy_3_10.coverage", "58329b17-3e1c-49b4-8506-75fc81ed3a9c");
                    add(fromDir, srcFiles, filesByBasedir);
                    writeline("/home/ubuntu/results/coverage/Copy/Copy_3_10.coverage", "70194d13-b916-4b17-82dd-a2c59f12c915");
                    add(fromDir, srcDirs, dirsByBasedir);
                    writeline("/home/ubuntu/results/coverage/Copy/Copy_3_10.coverage", "8c2c198a-fec1-43da-836a-17f4975a622d");
                    baseDirs.add(fromDir);
                } else {
                    writeline("/home/ubuntu/results/coverage/Copy/Copy_3_10.coverage", "585ece07-9830-449c-951f-6471eab171df");
                    if (!rc.isFilesystemOnly() && !supportsNonFileResources()) {
                        writeline("/home/ubuntu/results/coverage/Copy/Copy_3_10.coverage", "a6307d53-1231-42ed-b21f-6f3c368feb56");
                        throw new BuildException("Only FileSystem resources are supported.");
                    }
                    writeline("/home/ubuntu/results/coverage/Copy/Copy_3_10.coverage", "cad96fd6-2866-4074-aa5b-f20dbb114e85");
                    for (final Resource r : rc) {
                        writeline("/home/ubuntu/results/coverage/Copy/Copy_3_10.coverage", "a60a59db-654f-48ec-9605-0fd5363ef4f1");
                        if (!r.isExists()) {
                            writeline("/home/ubuntu/results/coverage/Copy/Copy_3_10.coverage", "b48a544d-ca1c-449c-9667-fa382ce63239");
                            final String message = "Warning: Could not find resource " + r.toLongString() + " to copy.";
                            writeline("/home/ubuntu/results/coverage/Copy/Copy_3_10.coverage", "7220b89a-5f8a-416a-bada-2bbe479db478");
                            if (!failonerror) {
                                writeline("/home/ubuntu/results/coverage/Copy/Copy_3_10.coverage", "2f6987b6-fd47-4f93-aaf5-12101792ac04");
                                if (!quiet) {
                                    writeline("/home/ubuntu/results/coverage/Copy/Copy_3_10.coverage", "e66c2a36-989b-4557-af25-cb63ebe7fdee");
                                    log(message, Project.MSG_ERR);
                                }
                            } else {
                                writeline("/home/ubuntu/results/coverage/Copy/Copy_3_10.coverage", "e92ce2bd-5bfe-4fd1-b3bc-8823fba82044");
                                throw new BuildException(message);
                            }
                            writeline("/home/ubuntu/results/coverage/Copy/Copy_3_10.coverage", "8875ef06-798a-43a2-8e35-ac8acf003200");
                            continue;
                        }
                        writeline("/home/ubuntu/results/coverage/Copy/Copy_3_10.coverage", "78fa9d7b-c264-4b91-996f-8c6dfa1f92da");
                        File baseDir = NULL_FILE_PLACEHOLDER;
                        writeline("/home/ubuntu/results/coverage/Copy/Copy_3_10.coverage", "e0f2fbeb-77b7-44a8-85ab-d19752082138");
                        String name = r.getName();
                        writeline("/home/ubuntu/results/coverage/Copy/Copy_3_10.coverage", "00d9a351-40a8-4725-9f5b-eaa9389f55d6");
                        final FileProvider fp = r.as(FileProvider.class);
                        writeline("/home/ubuntu/results/coverage/Copy/Copy_3_10.coverage", "32058693-0703-408e-8a95-9393cac60104");
                        if (fp != null) {
                            writeline("/home/ubuntu/results/coverage/Copy/Copy_3_10.coverage", "b73dd330-04db-4148-a509-4bfbc65f55af");
                            final FileResource fr = ResourceUtils.asFileResource(fp);
                            writeline("/home/ubuntu/results/coverage/Copy/Copy_3_10.coverage", "24d5153b-6196-4123-b7ba-86fa75bd8bf5");
                            baseDir = getKeyFile(fr.getBaseDir());
                            writeline("/home/ubuntu/results/coverage/Copy/Copy_3_10.coverage", "f60a1580-07b3-426b-9e65-6bf2df1a1248");
                            if (fr.getBaseDir() == null) {
                                writeline("/home/ubuntu/results/coverage/Copy/Copy_3_10.coverage", "83eb9b70-1874-4b5d-aec0-5682627d90ff");
                                name = fr.getFile().getAbsolutePath();
                            }
                        }
                        writeline("/home/ubuntu/results/coverage/Copy/Copy_3_10.coverage", "56a21201-aecd-4924-b6ee-2df247549ea4");
                        // files.
                        if (r.isDirectory() || fp != null) {
                            writeline("/home/ubuntu/results/coverage/Copy/Copy_3_10.coverage", "9f6a6aa9-fdeb-4b7d-81b5-102e76c647ff");
                            add(baseDir, name, r.isDirectory() ? dirsByBasedir : filesByBasedir);
                            writeline("/home/ubuntu/results/coverage/Copy/Copy_3_10.coverage", "a11a5cd7-0fe0-4ba8-a14d-5d49f4d00348");
                            baseDirs.add(baseDir);
                        } else {
                            writeline("/home/ubuntu/results/coverage/Copy/Copy_3_10.coverage", "b99f1716-5c8d-4623-a677-e1f4e8ede330");
                            // a not-directory file resource
                            // needs special treatment
                            nonFileResources.add(r);
                        }
                    }
                }
            }
            writeline("/home/ubuntu/results/coverage/Copy/Copy_3_10.coverage", "7c0a41f2-4961-49cc-b943-1242f2cd9391");
            iterateOverBaseDirs(baseDirs, dirsByBasedir, filesByBasedir);
            writeline("/home/ubuntu/results/coverage/Copy/Copy_3_10.coverage", "d5ff63c6-3f4b-4abb-aa21-75a756e2bea0");
            // do all the copy operations now...
            try {
                writeline("/home/ubuntu/results/coverage/Copy/Copy_3_10.coverage", "d7467231-adf9-40f3-9cea-4582c6edec74");
                doFileOperations();
            } catch (final BuildException e) {
                writeline("/home/ubuntu/results/coverage/Copy/Copy_3_10.coverage", "6b5bb067-5281-418a-8530-10d5c33bf61a");
                if (!failonerror) {
                    writeline("/home/ubuntu/results/coverage/Copy/Copy_3_10.coverage", "f027ffa5-79e9-4be0-87af-f789ea4a4e52");
                    if (!quiet) {
                        writeline("/home/ubuntu/results/coverage/Copy/Copy_3_10.coverage", "9e69cce2-9000-4f61-9b0e-7990fe9181cf");
                        log("Warning: " + getMessage(e), Project.MSG_ERR);
                    }
                } else {
                    writeline("/home/ubuntu/results/coverage/Copy/Copy_3_10.coverage", "2a865417-7f2a-4503-b927-d8ab27333592");
                    throw e;
                }
            }
            writeline("/home/ubuntu/results/coverage/Copy/Copy_3_10.coverage", "015a3d49-64db-4613-940f-a9245cb08604");
            if (nonFileResources.size() > 0 || singleResource != null) {
                writeline("/home/ubuntu/results/coverage/Copy/Copy_3_10.coverage", "ef379e32-8c22-4556-a86d-2a13fdeadc76");
                final Resource[] nonFiles = nonFileResources.toArray(new Resource[nonFileResources.size()]);
                writeline("/home/ubuntu/results/coverage/Copy/Copy_3_10.coverage", "bfa1b9ef-260e-45e2-8f3a-e3a6dbe79026");
                // restrict to out-of-date resources
                final Map<Resource, String[]> map = scan(nonFiles, destDir);
                writeline("/home/ubuntu/results/coverage/Copy/Copy_3_10.coverage", "870e485a-2ba1-44a2-9431-80dc91950ac1");
                if (singleResource != null) {
                    writeline("/home/ubuntu/results/coverage/Copy/Copy_3_10.coverage", "533223cb-4a0b-4acc-9b3f-d98fc3ae4109");
                    map.put(singleResource, new String[] { destFile.getAbsolutePath() });
                }
                writeline("/home/ubuntu/results/coverage/Copy/Copy_3_10.coverage", "3837eb54-3244-4f00-8340-72c25fc1407b");
                try {
                    writeline("/home/ubuntu/results/coverage/Copy/Copy_3_10.coverage", "883c24ab-72eb-4c9f-b108-45203229b91f");
                    doResourceOperations(map);
                } catch (final BuildException e) {
                    writeline("/home/ubuntu/results/coverage/Copy/Copy_3_10.coverage", "8194f502-d1e8-4261-b631-71a2dc6660d1");
                    if (!failonerror) {
                        writeline("/home/ubuntu/results/coverage/Copy/Copy_3_10.coverage", "18435177-10d6-4472-867d-7325616ae7ae");
                        if (!quiet) {
                            writeline("/home/ubuntu/results/coverage/Copy/Copy_3_10.coverage", "9043a70e-7387-4ad0-a7bd-c151365c9b2b");
                            log("Warning: " + getMessage(e), Project.MSG_ERR);
                        }
                    } else {
                        writeline("/home/ubuntu/results/coverage/Copy/Copy_3_10.coverage", "f37538e7-639d-4c03-a0e4-6cc6df5f367f");
                        throw e;
                    }
                }
            }
        } finally {
            writeline("/home/ubuntu/results/coverage/Copy/Copy_3_10.coverage", "bf1a58ab-e139-4114-9e6e-8bff47381080");
            // clean up again, so this instance can be used a second
            // time
            singleResource = null;
            writeline("/home/ubuntu/results/coverage/Copy/Copy_3_10.coverage", "2e761c1a-79bb-47bb-9c40-a8bb6ba9d7cc");
            file = savedFile;
            writeline("/home/ubuntu/results/coverage/Copy/Copy_3_10.coverage", "d84a6997-26d6-4c51-83b5-90e1ebc9ac89");
            destFile = savedDestFile;
            writeline("/home/ubuntu/results/coverage/Copy/Copy_3_10.coverage", "5f4dc94c-b005-4f55-b6a9-9924d11408d4");
            destDir = savedDestDir;
            writeline("/home/ubuntu/results/coverage/Copy/Copy_3_10.coverage", "4e7f848f-c770-4018-8aa6-29212294e782");
            if (savedRc != null) {
                writeline("/home/ubuntu/results/coverage/Copy/Copy_3_10.coverage", "9a57f6f2-c232-43c9-a6a9-9bfda66f19da");
                rcs.insertElementAt(savedRc, 0);
            }
            writeline("/home/ubuntu/results/coverage/Copy/Copy_3_10.coverage", "06a4f695-aa80-4581-b705-7e4c19270840");
            fileCopyMap.clear();
            writeline("/home/ubuntu/results/coverage/Copy/Copy_3_10.coverage", "c48055df-e725-4fac-8e47-bcb11f6bd359");
            dirCopyMap.clear();
            writeline("/home/ubuntu/results/coverage/Copy/Copy_3_10.coverage", "4e7b2e88-9125-4ea9-8611-74f8015b5fe8");
            completeDirMap.clear();
        }
    }

    /**
     * *********************************************************************
     * *  protected and private methods
     * **********************************************************************
     */
    private void copySingleFile() {
        writeline("/home/ubuntu/results/coverage/Copy/Copy_3_10.coverage", "294d8a14-8ff2-4a3d-8b71-12764411fea6");
        // deal with the single file
        if (file != null) {
            writeline("/home/ubuntu/results/coverage/Copy/Copy_3_10.coverage", "c2364d11-2847-4475-b7a0-2a9e7f5418fc");
            if (file.exists()) {
                writeline("/home/ubuntu/results/coverage/Copy/Copy_3_10.coverage", "0e61ca6f-ff6c-4bba-919e-d815919029bd");
                if (destFile == null) {
                    writeline("/home/ubuntu/results/coverage/Copy/Copy_3_10.coverage", "c98a0174-cb54-4c90-b997-fc2456b1c0e5");
                    destFile = new File(destDir, file.getName());
                }
                writeline("/home/ubuntu/results/coverage/Copy/Copy_3_10.coverage", "fa39695e-b6cc-40e5-9588-cbf4a4cb9187");
                if (forceOverwrite || !destFile.exists() || (file.lastModified() - granularity > destFile.lastModified())) {
                    writeline("/home/ubuntu/results/coverage/Copy/Copy_3_10.coverage", "bdd2f55b-4c8a-4bfc-b136-a1c0708dc5bf");
                    fileCopyMap.put(file.getAbsolutePath(), new String[] { destFile.getAbsolutePath() });
                } else {
                    writeline("/home/ubuntu/results/coverage/Copy/Copy_3_10.coverage", "f0fccdb4-bdc1-4121-a4b0-51c752d31a5e");
                    log(file + " omitted as " + destFile + " is up to date.", Project.MSG_VERBOSE);
                }
            } else {
                writeline("/home/ubuntu/results/coverage/Copy/Copy_3_10.coverage", "4c830a44-5fa3-400b-9a60-260215420155");
                final String message = "Warning: Could not find file " + file.getAbsolutePath() + " to copy.";
                writeline("/home/ubuntu/results/coverage/Copy/Copy_3_10.coverage", "530ce508-787e-4088-96ba-1093e5e4a299");
                if (!failonerror) {
                    writeline("/home/ubuntu/results/coverage/Copy/Copy_3_10.coverage", "4b331ef3-de3e-46ae-b995-fa0eefe5f4cb");
                    if (!quiet) {
                        writeline("/home/ubuntu/results/coverage/Copy/Copy_3_10.coverage", "e95860d6-83d2-4497-9aa7-f4fed2e017c1");
                        log(message, Project.MSG_ERR);
                    }
                } else {
                    writeline("/home/ubuntu/results/coverage/Copy/Copy_3_10.coverage", "19808b6a-ab87-4745-b082-e448f1a3b930");
                    throw new BuildException(message);
                }
            }
        }
    }

    private void iterateOverBaseDirs(final HashSet<File> baseDirs, final HashMap<File, List<String>> dirsByBasedir, final HashMap<File, List<String>> filesByBasedir) {
        writeline("/home/ubuntu/results/coverage/Copy/Copy_3_10.coverage", "73ae9fcd-1287-42fb-9a2d-b75c6ec86903");
        for (final File f : baseDirs) {
            writeline("/home/ubuntu/results/coverage/Copy/Copy_3_10.coverage", "c6933556-1129-46b6-b8b1-154640a560d3");
            final List<String> files = filesByBasedir.get(f);
            writeline("/home/ubuntu/results/coverage/Copy/Copy_3_10.coverage", "766635b6-9705-4317-9269-e49b648cb5c1");
            final List<String> dirs = dirsByBasedir.get(f);
            writeline("/home/ubuntu/results/coverage/Copy/Copy_3_10.coverage", "d7c6a7bb-cab5-4e98-8e5b-18bd4e0be5ce");
            String[] srcFiles = new String[0];
            writeline("/home/ubuntu/results/coverage/Copy/Copy_3_10.coverage", "a4b5714b-0735-443f-b03f-a32974a07361");
            if (files != null) {
                writeline("/home/ubuntu/results/coverage/Copy/Copy_3_10.coverage", "53df253e-c35c-4aef-923d-faa68f5c6349");
                srcFiles = files.toArray(srcFiles);
            }
            writeline("/home/ubuntu/results/coverage/Copy/Copy_3_10.coverage", "0cd01c11-c3c9-49d7-814f-87a87e27ef18");
            String[] srcDirs = new String[0];
            writeline("/home/ubuntu/results/coverage/Copy/Copy_3_10.coverage", "cbbd7b00-45c1-4819-84d6-351cee97f037");
            if (dirs != null) {
                writeline("/home/ubuntu/results/coverage/Copy/Copy_3_10.coverage", "62673534-491b-4261-9687-6591cf095db5");
                srcDirs = dirs.toArray(srcDirs);
            }
            writeline("/home/ubuntu/results/coverage/Copy/Copy_3_10.coverage", "0be608e3-cc33-4949-9b77-02b6116b0c16");
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
        writeline("/home/ubuntu/results/coverage/Copy/Copy_3_10.coverage", "3f5603dc-74d5-4f3c-879e-c0c213a74440");
        if (file == null && rcs.size() == 0) {
            writeline("/home/ubuntu/results/coverage/Copy/Copy_3_10.coverage", "e98ceada-5836-44c8-8643-aad5e4fa06ca");
            throw new BuildException("Specify at least one source--a file or a resource collection.");
        }
        writeline("/home/ubuntu/results/coverage/Copy/Copy_3_10.coverage", "7260ee40-b08e-4389-89bd-0bdddaa5d0b0");
        if (destFile != null && destDir != null) {
            writeline("/home/ubuntu/results/coverage/Copy/Copy_3_10.coverage", "c48f5c58-ded3-42f4-8c4f-5bfcc18a53be");
            throw new BuildException("Only one of tofile and todir may be set.");
        }
        writeline("/home/ubuntu/results/coverage/Copy/Copy_3_10.coverage", "eaf85356-8556-4810-a7cc-f9290ef51cbf");
        if (destFile == null && destDir == null) {
            writeline("/home/ubuntu/results/coverage/Copy/Copy_3_10.coverage", "ff4d4fa8-e0b1-436e-8bc7-f273a2013df9");
            throw new BuildException("One of tofile or todir must be set.");
        }
        writeline("/home/ubuntu/results/coverage/Copy/Copy_3_10.coverage", "3bd1d59a-2a5c-4c45-b1ca-523af26af9ce");
        if (file != null && file.isDirectory()) {
            writeline("/home/ubuntu/results/coverage/Copy/Copy_3_10.coverage", "d9397069-86b3-4945-8850-579ffe720937");
            throw new BuildException("Use a resource collection to copy directories.");
        }
        writeline("/home/ubuntu/results/coverage/Copy/Copy_3_10.coverage", "e01837e9-9e23-48d1-957f-7c369be111e0");
        if (destFile != null && rcs.size() > 0) {
            writeline("/home/ubuntu/results/coverage/Copy/Copy_3_10.coverage", "8ddc413d-e4b3-4b18-a64d-52efea33432e");
            if (rcs.size() > 1) {
                writeline("/home/ubuntu/results/coverage/Copy/Copy_3_10.coverage", "da4fd236-a6d1-45d6-9f9b-f0871810f391");
                throw new BuildException("Cannot concatenate multiple files into a single file.");
            } else {
                writeline("/home/ubuntu/results/coverage/Copy/Copy_3_10.coverage", "c11d838a-9d1c-47de-bc0e-cc964c3e5c6b");
                final ResourceCollection rc = rcs.elementAt(0);
                writeline("/home/ubuntu/results/coverage/Copy/Copy_3_10.coverage", "fd2f9905-51dd-4477-a3af-1e75e0358b67");
                if (!rc.isFilesystemOnly() && !supportsNonFileResources()) {
                    writeline("/home/ubuntu/results/coverage/Copy/Copy_3_10.coverage", "818fec86-0a27-4882-b562-1868eaf2ba0a");
                    throw new BuildException("Only FileSystem resources are" + " supported.");
                }
                writeline("/home/ubuntu/results/coverage/Copy/Copy_3_10.coverage", "9fb202c9-6f72-413f-9e6e-673e9ea1fcce");
                if (rc.size() == 0) {
                    writeline("/home/ubuntu/results/coverage/Copy/Copy_3_10.coverage", "d54040be-2f22-4303-8d58-5068ea3df1d5");
                    throw new BuildException(MSG_WHEN_COPYING_EMPTY_RC_TO_FILE);
                } else if (rc.size() == 1) {
                    writeline("/home/ubuntu/results/coverage/Copy/Copy_3_10.coverage", "ea92fc26-b518-459e-9a21-08ec9ea1e928");
                    final Resource res = rc.iterator().next();
                    writeline("/home/ubuntu/results/coverage/Copy/Copy_3_10.coverage", "1da0ec93-5b7e-4427-b84c-110c7df295d4");
                    final FileProvider r = res.as(FileProvider.class);
                    writeline("/home/ubuntu/results/coverage/Copy/Copy_3_10.coverage", "95f0861f-0faf-4e0d-8429-c92bb8e66cea");
                    if (file == null) {
                        writeline("/home/ubuntu/results/coverage/Copy/Copy_3_10.coverage", "35747861-d302-4dcb-a00d-90fb690ec7de");
                        if (r != null) {
                            writeline("/home/ubuntu/results/coverage/Copy/Copy_3_10.coverage", "68ead2cb-eb0c-4e58-a196-7ae32dcd286f");
                            file = r.getFile();
                        } else {
                            writeline("/home/ubuntu/results/coverage/Copy/Copy_3_10.coverage", "adbaca48-3c12-4229-8b04-ce4428740c2f");
                            singleResource = res;
                        }
                        writeline("/home/ubuntu/results/coverage/Copy/Copy_3_10.coverage", "058f1edc-400c-4f33-82ba-daa641ff32e9");
                        rcs.removeElementAt(0);
                    } else {
                        writeline("/home/ubuntu/results/coverage/Copy/Copy_3_10.coverage", "bbe3d2ab-878c-46b7-b06d-32078c37aa2e");
                        throw new BuildException("Cannot concatenate multiple files into a single file.");
                    }
                } else {
                    writeline("/home/ubuntu/results/coverage/Copy/Copy_3_10.coverage", "24f8e4c8-257f-4957-ad7c-12b0b2e52a84");
                    throw new BuildException("Cannot concatenate multiple files into a single file.");
                }
            }
        }
        writeline("/home/ubuntu/results/coverage/Copy/Copy_3_10.coverage", "1c5adaa3-8baf-488d-8003-1cd2561c2639");
        if (destFile != null) {
            writeline("/home/ubuntu/results/coverage/Copy/Copy_3_10.coverage", "8917b537-0c6f-49cf-9afb-50416262b484");
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
        writeline("/home/ubuntu/results/coverage/Copy/Copy_3_10.coverage", "a1cef2b0-13c7-46f9-8e62-8e6b4c9252fe");
        final FileNameMapper mapper = getMapper();
        writeline("/home/ubuntu/results/coverage/Copy/Copy_3_10.coverage", "bbae27c2-20d7-4d52-be0d-0e89730b631a");
        buildMap(fromDir, toDir, files, mapper, fileCopyMap);
        writeline("/home/ubuntu/results/coverage/Copy/Copy_3_10.coverage", "7d2348ee-d4c8-444e-8d08-d3a86263102d");
        if (includeEmpty) {
            writeline("/home/ubuntu/results/coverage/Copy/Copy_3_10.coverage", "c5d8619d-35bd-49ea-8920-858498fcbad4");
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
        writeline("/home/ubuntu/results/coverage/Copy/Copy_3_10.coverage", "5fb98b9e-0945-447a-b74a-9cb4b509b88f");
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
        writeline("/home/ubuntu/results/coverage/Copy/Copy_3_10.coverage", "2b55a5f2-6393-419f-9140-f0263d7554e7");
        String[] toCopy = null;
        writeline("/home/ubuntu/results/coverage/Copy/Copy_3_10.coverage", "15ef7095-424d-4dcd-ad53-cf7a26a1450b");
        if (forceOverwrite) {
            writeline("/home/ubuntu/results/coverage/Copy/Copy_3_10.coverage", "b047d7cf-dc36-41fe-b554-cbae94788d86");
            final Vector<String> v = new Vector<String>();
            writeline("/home/ubuntu/results/coverage/Copy/Copy_3_10.coverage", "43a14061-8342-4fe2-bab5-d2975f0c70d6");
            for (int i = 0; i < names.length; i++) {
                writeline("/home/ubuntu/results/coverage/Copy/Copy_3_10.coverage", "39cc0331-a6a8-48bd-94b3-70425bd67048");
                if (mapper.mapFileName(names[i]) != null) {
                    writeline("/home/ubuntu/results/coverage/Copy/Copy_3_10.coverage", "84752df6-6a65-449f-ae4e-858aa9a47301");
                    v.addElement(names[i]);
                }
            }
            writeline("/home/ubuntu/results/coverage/Copy/Copy_3_10.coverage", "164c7d97-9b53-44bb-bb2a-d2271d4e73aa");
            toCopy = new String[v.size()];
            writeline("/home/ubuntu/results/coverage/Copy/Copy_3_10.coverage", "f453ffbb-ef84-4d0b-b783-7c87324b924d");
            v.copyInto(toCopy);
        } else {
            writeline("/home/ubuntu/results/coverage/Copy/Copy_3_10.coverage", "88a5ba76-2283-4084-bbde-9232f4f648ff");
            final SourceFileScanner ds = new SourceFileScanner(this);
            writeline("/home/ubuntu/results/coverage/Copy/Copy_3_10.coverage", "b38c177a-8f8e-4ffd-a185-75558d1912e8");
            toCopy = ds.restrict(names, fromDir, toDir, mapper, granularity);
        }
        writeline("/home/ubuntu/results/coverage/Copy/Copy_3_10.coverage", "59100c2c-b954-49df-8b6e-5edea63fc839");
        for (int i = 0; i < toCopy.length; i++) {
            writeline("/home/ubuntu/results/coverage/Copy/Copy_3_10.coverage", "ff928f0a-6ef3-41e5-957a-c0e7deae14e5");
            final File src = new File(fromDir, toCopy[i]);
            writeline("/home/ubuntu/results/coverage/Copy/Copy_3_10.coverage", "6bce61d5-4539-4e24-9379-3447d83d8473");
            final String[] mappedFiles = mapper.mapFileName(toCopy[i]);
            writeline("/home/ubuntu/results/coverage/Copy/Copy_3_10.coverage", "3708f624-8810-4d78-8da8-52cb71aad3ff");
            if (!enableMultipleMappings) {
                writeline("/home/ubuntu/results/coverage/Copy/Copy_3_10.coverage", "17c7a5a1-460c-404a-a82d-888a0aa6b895");
                map.put(src.getAbsolutePath(), new String[] { new File(toDir, mappedFiles[0]).getAbsolutePath() });
            } else {
                writeline("/home/ubuntu/results/coverage/Copy/Copy_3_10.coverage", "74e7697a-34a5-43f4-8054-51ff6ec26073");
                // reuse the array created by the mapper
                for (int k = 0; k < mappedFiles.length; k++) {
                    writeline("/home/ubuntu/results/coverage/Copy/Copy_3_10.coverage", "4864a78a-2ad2-4bbc-8119-562272ca55e1");
                    mappedFiles[k] = new File(toDir, mappedFiles[k]).getAbsolutePath();
                }
                writeline("/home/ubuntu/results/coverage/Copy/Copy_3_10.coverage", "085eb262-591f-4954-b1d8-739551c42128");
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
        writeline("/home/ubuntu/results/coverage/Copy/Copy_3_10.coverage", "9d8ada18-52b6-429a-a363-7d412d1c99f1");
        final HashMap<Resource, String[]> map = new HashMap<Resource, String[]>();
        writeline("/home/ubuntu/results/coverage/Copy/Copy_3_10.coverage", "0fc85291-ca0d-4dc8-a5c0-6ff90f1d12b3");
        Resource[] toCopy = null;
        writeline("/home/ubuntu/results/coverage/Copy/Copy_3_10.coverage", "f0164e1f-b924-41c9-b051-f52de17e2cf4");
        if (forceOverwrite) {
            writeline("/home/ubuntu/results/coverage/Copy/Copy_3_10.coverage", "031c5578-0e0e-4451-9e0a-80c807015ef6");
            final Vector<Resource> v = new Vector<Resource>();
            writeline("/home/ubuntu/results/coverage/Copy/Copy_3_10.coverage", "69c27cd7-9874-4648-981e-12b4651cb388");
            for (int i = 0; i < fromResources.length; i++) {
                writeline("/home/ubuntu/results/coverage/Copy/Copy_3_10.coverage", "62f0f9c6-88b6-499e-a506-219e515042a8");
                if (mapper.mapFileName(fromResources[i].getName()) != null) {
                    writeline("/home/ubuntu/results/coverage/Copy/Copy_3_10.coverage", "7908924a-1ae4-4b19-9d14-6ee1e43fb33f");
                    v.addElement(fromResources[i]);
                }
            }
            writeline("/home/ubuntu/results/coverage/Copy/Copy_3_10.coverage", "2dfe5905-5703-433b-8c61-d1cd554e2cd3");
            toCopy = new Resource[v.size()];
            writeline("/home/ubuntu/results/coverage/Copy/Copy_3_10.coverage", "2aa26269-150e-4bc9-a78e-9feacf147922");
            v.copyInto(toCopy);
        } else {
            writeline("/home/ubuntu/results/coverage/Copy/Copy_3_10.coverage", "ef351f68-90da-4e3c-bb01-22826a8a2018");
            toCopy = ResourceUtils.selectOutOfDateSources(this, fromResources, mapper, new ResourceFactory() {

                public Resource getResource(final String name) {
                    return new FileResource(toDir, name);
                }
            }, granularity);
        }
        writeline("/home/ubuntu/results/coverage/Copy/Copy_3_10.coverage", "1d5da9d9-0195-4593-a3dd-30fbe7fb4686");
        for (int i = 0; i < toCopy.length; i++) {
            writeline("/home/ubuntu/results/coverage/Copy/Copy_3_10.coverage", "d06f8cc7-b68f-4736-8110-3fc3c962d467");
            final String[] mappedFiles = mapper.mapFileName(toCopy[i].getName());
            writeline("/home/ubuntu/results/coverage/Copy/Copy_3_10.coverage", "4c1e266d-56fe-4857-b3a8-2c4d432759ec");
            for (int j = 0; j < mappedFiles.length; j++) {
                writeline("/home/ubuntu/results/coverage/Copy/Copy_3_10.coverage", "4a2445ec-8774-4eec-abe1-960940ecdaf7");
                if (mappedFiles[j] == null) {
                    writeline("/home/ubuntu/results/coverage/Copy/Copy_3_10.coverage", "04146990-1aaa-404f-99b4-1b23d37e0ab6");
                    throw new BuildException("Can't copy a resource without a" + " name if the mapper doesn't" + " provide one.");
                }
            }
            writeline("/home/ubuntu/results/coverage/Copy/Copy_3_10.coverage", "0d994c31-96c9-448e-a397-edf13564c52b");
            if (!enableMultipleMappings) {
                writeline("/home/ubuntu/results/coverage/Copy/Copy_3_10.coverage", "57995e3e-96cc-475e-96b1-69cd535ba2e7");
                map.put(toCopy[i], new String[] { new File(toDir, mappedFiles[0]).getAbsolutePath() });
            } else {
                writeline("/home/ubuntu/results/coverage/Copy/Copy_3_10.coverage", "180a7b4a-cf58-4ca5-abca-d575607bde26");
                // reuse the array created by the mapper
                for (int k = 0; k < mappedFiles.length; k++) {
                    writeline("/home/ubuntu/results/coverage/Copy/Copy_3_10.coverage", "4332540d-0832-4c7e-a2c2-ca6725c339c3");
                    mappedFiles[k] = new File(toDir, mappedFiles[k]).getAbsolutePath();
                }
                writeline("/home/ubuntu/results/coverage/Copy/Copy_3_10.coverage", "75cc4d8c-e8a5-4ab7-ac6f-1b3d6f8e5d6b");
                map.put(toCopy[i], mappedFiles);
            }
        }
        writeline("/home/ubuntu/results/coverage/Copy/Copy_3_10.coverage", "23e0e69d-4702-41ad-8fae-9f130ed031f2");
        return map;
    }

    /**
     * Actually does the file (and possibly empty directory) copies.
     * This is a good method for subclasses to override.
     */
    protected void doFileOperations() {
        writeline("/home/ubuntu/results/coverage/Copy/Copy_3_10.coverage", "5045034f-a119-4b56-9aa1-3dc35d90f90c");
        if (fileCopyMap.size() > 0) {
            writeline("/home/ubuntu/results/coverage/Copy/Copy_3_10.coverage", "5f759992-7d9d-4a4a-af92-45fc7649d1a9");
            log("Copying " + fileCopyMap.size() + " file" + (fileCopyMap.size() == 1 ? "" : "s") + " to " + destDir.getAbsolutePath());
            writeline("/home/ubuntu/results/coverage/Copy/Copy_3_10.coverage", "9d9cfc98-8083-4079-9d3d-8d3f3afab7b9");
            for (final Map.Entry<String, String[]> e : fileCopyMap.entrySet()) {
                writeline("/home/ubuntu/results/coverage/Copy/Copy_3_10.coverage", "ebd1cfbb-1564-4d37-abbb-c290422d0307");
                final String fromFile = e.getKey();
                writeline("/home/ubuntu/results/coverage/Copy/Copy_3_10.coverage", "cd7a80e3-9b10-4ce5-9e41-1507aa427236");
                final String[] toFiles = e.getValue();
                writeline("/home/ubuntu/results/coverage/Copy/Copy_3_10.coverage", "934e11f6-34bd-4bc7-8a81-59af7e2d5240");
                for (int i = 0; i < toFiles.length; i++) {
                    writeline("/home/ubuntu/results/coverage/Copy/Copy_3_10.coverage", "1b8af384-c061-4b40-bc0e-1c502df98fa1");
                    final String toFile = toFiles[i];
                    writeline("/home/ubuntu/results/coverage/Copy/Copy_3_10.coverage", "335d5029-34f3-4af2-82e7-f2821807fc20");
                    if (fromFile.equals(toFile)) {
                        writeline("/home/ubuntu/results/coverage/Copy/Copy_3_10.coverage", "9a5a46e8-710c-4862-8ccd-24a5dfa1f273");
                        log("Skipping self-copy of " + fromFile, verbosity);
                        writeline("/home/ubuntu/results/coverage/Copy/Copy_3_10.coverage", "2bda1c5f-379a-46fd-bab2-516d82ce39b7");
                        continue;
                    }
                    writeline("/home/ubuntu/results/coverage/Copy/Copy_3_10.coverage", "cc3b4efb-0798-4c32-aec1-0221e80f11f0");
                    try {
                        writeline("/home/ubuntu/results/coverage/Copy/Copy_3_10.coverage", "392ff5af-d9bc-443e-be51-5739fb3b3624");
                        log("Copying " + fromFile + " to " + toFile, verbosity);
                        writeline("/home/ubuntu/results/coverage/Copy/Copy_3_10.coverage", "2d059d6d-3c6c-4d1e-aca3-c63f110ee51c");
                        final FilterSetCollection executionFilters = new FilterSetCollection();
                        writeline("/home/ubuntu/results/coverage/Copy/Copy_3_10.coverage", "33a031c0-409c-448d-ad44-bc6818844484");
                        if (filtering) {
                            writeline("/home/ubuntu/results/coverage/Copy/Copy_3_10.coverage", "3cacb1cf-7887-433e-8ecf-0c7dba11cdca");
                            executionFilters.addFilterSet(getProject().getGlobalFilterSet());
                        }
                        writeline("/home/ubuntu/results/coverage/Copy/Copy_3_10.coverage", "a2cf43d2-d891-4de0-8b14-af7f5c94854c");
                        for (final FilterSet filterSet : filterSets) {
                            writeline("/home/ubuntu/results/coverage/Copy/Copy_3_10.coverage", "456e9533-f943-4ebb-b8ba-046ffabca153");
                            executionFilters.addFilterSet(filterSet);
                        }
                        writeline("/home/ubuntu/results/coverage/Copy/Copy_3_10.coverage", "dc8304d0-a0de-4cbc-a30f-67bfeca12c44");
                        fileUtils.copyFile(new File(fromFile), new File(toFile), executionFilters, filterChains, forceOverwrite, preserveLastModified, /* append: */
                        false, inputEncoding, outputEncoding, getProject(), getForce());
                    } catch (final IOException ioe) {
                        writeline("/home/ubuntu/results/coverage/Copy/Copy_3_10.coverage", "589d53cc-1842-41fc-a236-8582efbe8d6a");
                        String msg = "Failed to copy " + fromFile + " to " + toFile + " due to " + getDueTo(ioe);
                        writeline("/home/ubuntu/results/coverage/Copy/Copy_3_10.coverage", "98cde5b5-7226-497e-a129-c294b242b8c3");
                        final File targetFile = new File(toFile);
                        writeline("/home/ubuntu/results/coverage/Copy/Copy_3_10.coverage", "3120c41f-f417-458f-9b78-6434ec181bed");
                        if (!(ioe instanceof ResourceUtils.ReadOnlyTargetFileException) && targetFile.exists() && !targetFile.delete()) {
                            writeline("/home/ubuntu/results/coverage/Copy/Copy_3_10.coverage", "a8707115-568f-4269-8745-ffacbbbee8bb");
                            msg += " and I couldn't delete the corrupt " + toFile;
                        }
                        writeline("/home/ubuntu/results/coverage/Copy/Copy_3_10.coverage", "00aafb5a-fda2-4e55-829e-9467628bd08d");
                        if (failonerror) {
                            writeline("/home/ubuntu/results/coverage/Copy/Copy_3_10.coverage", "4b912370-610a-4d45-af85-a9b5b8a80ac6");
                            throw new BuildException(msg, ioe, getLocation());
                        }
                        writeline("/home/ubuntu/results/coverage/Copy/Copy_3_10.coverage", "2708b75f-bb8b-4c55-93ec-ec35cb77614c");
                        log(msg, Project.MSG_ERR);
                    }
                }
            }
        }
        writeline("/home/ubuntu/results/coverage/Copy/Copy_3_10.coverage", "1a13e7e9-9749-4586-996b-22634e2b86a1");
        if (includeEmpty) {
            writeline("/home/ubuntu/results/coverage/Copy/Copy_3_10.coverage", "bb5f5656-4ab6-4737-9ad5-00bc027135d5");
            int createCount = 0;
            writeline("/home/ubuntu/results/coverage/Copy/Copy_3_10.coverage", "f6114f6c-08d3-427b-bcb1-828e9348edac");
            for (final String[] dirs : dirCopyMap.values()) {
                writeline("/home/ubuntu/results/coverage/Copy/Copy_3_10.coverage", "f14274fc-c1fb-492e-99bb-1cc92541057b");
                for (int i = 0; i < dirs.length; i++) {
                    writeline("/home/ubuntu/results/coverage/Copy/Copy_3_10.coverage", "7dbcfa7f-4b54-49ad-91ab-43c83dab9589");
                    final File d = new File(dirs[i]);
                    writeline("/home/ubuntu/results/coverage/Copy/Copy_3_10.coverage", "47db680e-480a-4dc9-934b-b08c2ac8d10b");
                    if (!d.exists()) {
                        writeline("/home/ubuntu/results/coverage/Copy/Copy_3_10.coverage", "15be6e2e-c515-421b-ae17-be0f80bcf309");
                        if (!(d.mkdirs() || d.isDirectory())) {
                            writeline("/home/ubuntu/results/coverage/Copy/Copy_3_10.coverage", "4e96c74e-a392-4098-a8a4-144fe8f08c44");
                            log("Unable to create directory " + d.getAbsolutePath(), Project.MSG_ERR);
                        } else {
                            writeline("/home/ubuntu/results/coverage/Copy/Copy_3_10.coverage", "4e2bd79e-362e-45cf-a982-55931b2eb2e2");
                            createCount++;
                        }
                    }
                }
            }
            writeline("/home/ubuntu/results/coverage/Copy/Copy_3_10.coverage", "9ccf181d-6af6-4710-b2ba-72ba712afbd0");
            if (createCount > 0) {
                writeline("/home/ubuntu/results/coverage/Copy/Copy_3_10.coverage", "25f59ceb-d7e9-4217-a9b9-74f22902b59d");
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
        writeline("/home/ubuntu/results/coverage/Copy/Copy_3_10.coverage", "b9cbef31-9fbd-45bc-b46d-92e702526997");
        if (map.size() > 0) {
            writeline("/home/ubuntu/results/coverage/Copy/Copy_3_10.coverage", "c6044efc-1bf9-4d08-8e4e-5a21544669ae");
            log("Copying " + map.size() + " resource" + (map.size() == 1 ? "" : "s") + " to " + destDir.getAbsolutePath());
            writeline("/home/ubuntu/results/coverage/Copy/Copy_3_10.coverage", "14a9729e-8f70-4479-a5bd-7fc0cc8d9b2d");
            for (final Map.Entry<Resource, String[]> e : map.entrySet()) {
                writeline("/home/ubuntu/results/coverage/Copy/Copy_3_10.coverage", "068ec078-d1f1-4b01-a82d-6dde950c7a6e");
                final Resource fromResource = e.getKey();
                writeline("/home/ubuntu/results/coverage/Copy/Copy_3_10.coverage", "a15dca9d-2646-4b17-bc01-7029175c32c5");
                for (final String toFile : e.getValue()) {
                    writeline("/home/ubuntu/results/coverage/Copy/Copy_3_10.coverage", "88d75e7f-4d52-4c92-b027-7f531da812e2");
                    try {
                        writeline("/home/ubuntu/results/coverage/Copy/Copy_3_10.coverage", "fc591e75-ae95-4897-8745-53d435ceb36d");
                        log("Copying " + fromResource + " to " + toFile, verbosity);
                        writeline("/home/ubuntu/results/coverage/Copy/Copy_3_10.coverage", "734f9d9a-e05d-418e-bc01-5247d4a4bcd4");
                        final FilterSetCollection executionFilters = new FilterSetCollection();
                        writeline("/home/ubuntu/results/coverage/Copy/Copy_3_10.coverage", "e5c60cba-64ec-4433-a31e-2e92898c8109");
                        if (filtering) {
                            writeline("/home/ubuntu/results/coverage/Copy/Copy_3_10.coverage", "885dd5ca-693b-4aae-8f31-aa5f84d6406c");
                            executionFilters.addFilterSet(getProject().getGlobalFilterSet());
                        }
                        writeline("/home/ubuntu/results/coverage/Copy/Copy_3_10.coverage", "5a79731d-8c26-4347-a27e-c6657f2b750c");
                        for (final FilterSet filterSet : filterSets) {
                            writeline("/home/ubuntu/results/coverage/Copy/Copy_3_10.coverage", "fc9ab7cc-bdf9-4f25-80db-67d58f82ef97");
                            executionFilters.addFilterSet(filterSet);
                        }
                        writeline("/home/ubuntu/results/coverage/Copy/Copy_3_10.coverage", "c5682cd0-d08d-4db6-830a-843db4d559ee");
                        ResourceUtils.copyResource(fromResource, new FileResource(destDir, toFile), executionFilters, filterChains, forceOverwrite, preserveLastModified, /* append: */
                        false, inputEncoding, outputEncoding, getProject(), getForce());
                    } catch (final IOException ioe) {
                        writeline("/home/ubuntu/results/coverage/Copy/Copy_3_10.coverage", "e76a7e28-fdc3-464e-8e1a-19a60fc42db1");
                        String msg = "Failed to copy " + fromResource + " to " + toFile + " due to " + getDueTo(ioe);
                        writeline("/home/ubuntu/results/coverage/Copy/Copy_3_10.coverage", "aacee372-f563-43f7-96ba-4fd7cef065ea");
                        final File targetFile = new File(toFile);
                        writeline("/home/ubuntu/results/coverage/Copy/Copy_3_10.coverage", "52a75255-0141-46b6-97b4-fc3d72b0c5d7");
                        if (!(ioe instanceof ResourceUtils.ReadOnlyTargetFileException) && targetFile.exists() && !targetFile.delete()) {
                            writeline("/home/ubuntu/results/coverage/Copy/Copy_3_10.coverage", "8857692b-8032-4e07-baac-f0cf411878c9");
                            msg += " and I couldn't delete the corrupt " + toFile;
                        }
                        writeline("/home/ubuntu/results/coverage/Copy/Copy_3_10.coverage", "e580cbd0-2ced-4b01-9db9-9b2e4d0af247");
                        if (failonerror) {
                            writeline("/home/ubuntu/results/coverage/Copy/Copy_3_10.coverage", "a1817415-256a-4fb7-8f2e-bf4711cdd2a3");
                            throw new BuildException(msg, ioe, getLocation());
                        }
                        writeline("/home/ubuntu/results/coverage/Copy/Copy_3_10.coverage", "2240aad4-7519-4402-b39e-25ede6b18670");
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
        writeline("/home/ubuntu/results/coverage/Copy/Copy_3_10.coverage", "1538564e-5c4d-4fdd-bc07-202dcb61499c");
        return getClass().equals(Copy.class);
    }

    /**
     * Adds the given strings to a list contained in the given map.
     * The file is the key into the map.
     */
    private static void add(File baseDir, final String[] names, final Map<File, List<String>> m) {
        writelineStatic("/home/ubuntu/results/coverage/Copy/Copy_3_10.coverage", "002ca8a5-928c-4757-b856-96ef2cece8fc");
        if (names != null) {
            writelineStatic("/home/ubuntu/results/coverage/Copy/Copy_3_10.coverage", "c06ac595-803f-4534-b21c-1a746976d1aa");
            baseDir = getKeyFile(baseDir);
            writelineStatic("/home/ubuntu/results/coverage/Copy/Copy_3_10.coverage", "8a913c8d-dfbb-49e3-85a3-503dcd28c39d");
            List<String> l = m.get(baseDir);
            writelineStatic("/home/ubuntu/results/coverage/Copy/Copy_3_10.coverage", "75d76266-45d0-41b7-96eb-b7470c4b34ed");
            if (l == null) {
                writelineStatic("/home/ubuntu/results/coverage/Copy/Copy_3_10.coverage", "71cbaced-32b1-4eac-bca5-7f67f15ec537");
                l = new ArrayList<String>(names.length);
                writelineStatic("/home/ubuntu/results/coverage/Copy/Copy_3_10.coverage", "0c9fbb8f-4a4a-4b0b-9440-6fa5c0e3ad87");
                m.put(baseDir, l);
            }
            writelineStatic("/home/ubuntu/results/coverage/Copy/Copy_3_10.coverage", "112e300e-742f-4317-9454-6a61a63165a2");
            l.addAll(java.util.Arrays.asList(names));
        }
    }

    /**
     * Adds the given string to a list contained in the given map.
     * The file is the key into the map.
     */
    private static void add(final File baseDir, final String name, final Map<File, List<String>> m) {
        writelineStatic("/home/ubuntu/results/coverage/Copy/Copy_3_10.coverage", "e072a9ac-0856-4b9b-a1c4-755a9da31b4f");
        if (name != null) {
            writelineStatic("/home/ubuntu/results/coverage/Copy/Copy_3_10.coverage", "3f53f424-99c4-4b15-97ce-ccbc12393856");
            add(baseDir, new String[] { name }, m);
        }
    }

    /**
     * Either returns its argument or a plaeholder if the argument is null.
     */
    private static File getKeyFile(final File f) {
        writelineStatic("/home/ubuntu/results/coverage/Copy/Copy_3_10.coverage", "5223181e-bdad-4f36-a4ed-e5f5cce827fd");
        return f == null ? NULL_FILE_PLACEHOLDER : f;
    }

    /**
     * returns the mapper to use based on nested elements or the
     * flatten attribute.
     */
    private FileNameMapper getMapper() {
        writeline("/home/ubuntu/results/coverage/Copy/Copy_3_10.coverage", "5b6d4af0-6c15-46f3-8793-2f97e85c1710");
        FileNameMapper mapper = null;
        writeline("/home/ubuntu/results/coverage/Copy/Copy_3_10.coverage", "20324c78-85d2-45bb-b0d2-c94404ee5883");
        if (mapperElement != null) {
            writeline("/home/ubuntu/results/coverage/Copy/Copy_3_10.coverage", "8dc87eea-aaea-4dbd-947b-45e89955e601");
            mapper = mapperElement.getImplementation();
        } else if (flatten) {
            writeline("/home/ubuntu/results/coverage/Copy/Copy_3_10.coverage", "fc0ab903-b7c6-4f5d-bd5d-41aa00c00958");
            mapper = new FlatFileNameMapper();
        } else {
            writeline("/home/ubuntu/results/coverage/Copy/Copy_3_10.coverage", "bffb37df-36e2-4ef1-9278-2fc932261454");
            mapper = new IdentityMapper();
        }
        writeline("/home/ubuntu/results/coverage/Copy/Copy_3_10.coverage", "31f0f059-13cd-4823-ac83-a6cbcdd5d2ae");
        return mapper;
    }

    /**
     * Handle getMessage() for exceptions.
     * @param ex the exception to handle
     * @return ex.getMessage() if ex.getMessage() is not null
     * otherwise return ex.toString()
     */
    private String getMessage(final Exception ex) {
        writeline("/home/ubuntu/results/coverage/Copy/Copy_3_10.coverage", "604792f3-b617-41df-9009-d1daa0b4aea0");
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
        writeline("/home/ubuntu/results/coverage/Copy/Copy_3_10.coverage", "1bf71a43-e99f-4d07-851a-2177efc340e7");
        final boolean baseIOException = ex.getClass() == IOException.class;
        writeline("/home/ubuntu/results/coverage/Copy/Copy_3_10.coverage", "8a3af5f9-38c8-4c94-a01e-4b196ce1e8e0");
        final StringBuffer message = new StringBuffer();
        writeline("/home/ubuntu/results/coverage/Copy/Copy_3_10.coverage", "52d364fd-f23d-44cf-89ff-f27485fbeeda");
        if (!baseIOException || ex.getMessage() == null) {
            writeline("/home/ubuntu/results/coverage/Copy/Copy_3_10.coverage", "55dd330c-7ec3-4332-9d5c-894d4fe1e821");
            message.append(ex.getClass().getName());
        }
        writeline("/home/ubuntu/results/coverage/Copy/Copy_3_10.coverage", "c8173658-a595-40fc-9518-99910e121a89");
        if (ex.getMessage() != null) {
            writeline("/home/ubuntu/results/coverage/Copy/Copy_3_10.coverage", "0834e642-950a-4c4e-8eee-d3474ff6cf26");
            if (!baseIOException) {
                writeline("/home/ubuntu/results/coverage/Copy/Copy_3_10.coverage", "ed30f7ac-23c3-4827-98f6-12ef335c7242");
                message.append(" ");
            }
            writeline("/home/ubuntu/results/coverage/Copy/Copy_3_10.coverage", "79227dc3-d418-4e14-a487-8737dd877f48");
            message.append(ex.getMessage());
        }
        writeline("/home/ubuntu/results/coverage/Copy/Copy_3_10.coverage", "b5e91360-c7be-4bdb-ac14-2b610d8014bd");
        if (ex.getClass().getName().indexOf("MalformedInput") != -1) {
            writeline("/home/ubuntu/results/coverage/Copy/Copy_3_10.coverage", "39acccb4-4c79-4045-ab8b-b0da41bbf499");
            message.append(LINE_SEPARATOR);
            writeline("/home/ubuntu/results/coverage/Copy/Copy_3_10.coverage", "2c223098-66e0-4e1a-b900-8ba93783f925");
            message.append("This is normally due to the input file containing invalid");
            writeline("/home/ubuntu/results/coverage/Copy/Copy_3_10.coverage", "d5803e29-bc8f-42de-abc5-9d110b1535df");
            message.append(LINE_SEPARATOR);
            writeline("/home/ubuntu/results/coverage/Copy/Copy_3_10.coverage", "1454a1dc-63ff-4a00-8fb0-35d02af1c53f");
            message.append("bytes for the character encoding used : ");
            writeline("/home/ubuntu/results/coverage/Copy/Copy_3_10.coverage", "cf753839-ec09-4b2f-b786-b495fbd31a24");
            message.append((inputEncoding == null ? fileUtils.getDefaultEncoding() : inputEncoding));
            writeline("/home/ubuntu/results/coverage/Copy/Copy_3_10.coverage", "88b0abff-71fb-4d70-b76e-bb6b5e54e1c9");
            message.append(LINE_SEPARATOR);
        }
        writeline("/home/ubuntu/results/coverage/Copy/Copy_3_10.coverage", "0786d549-1d46-464c-b2f0-2f41a5fbf7d8");
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
