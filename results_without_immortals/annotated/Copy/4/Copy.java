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
        writeline("/home/ubuntu/results/coverage/Copy/Copy_4_10.coverage", "00d24d6d-c4d5-4add-a4a2-1aa7c0a1d126");
        return fileUtils;
    }

    /**
     * Set a single source file to copy.
     * @param file the file to copy.
     */
    public void setFile(final File file) {
        writeline("/home/ubuntu/results/coverage/Copy/Copy_4_10.coverage", "9fa1b214-5f3d-4af2-b4e6-cba59116442e");
        this.file = file;
    }

    /**
     * Set the destination file.
     * @param destFile the file to copy to.
     */
    public void setTofile(final File destFile) {
        writeline("/home/ubuntu/results/coverage/Copy/Copy_4_10.coverage", "fd8c365f-9519-4d75-b0d8-fd6a32b5c8fc");
        this.destFile = destFile;
    }

    /**
     * Set the destination directory.
     * @param destDir the destination directory.
     */
    public void setTodir(final File destDir) {
        writeline("/home/ubuntu/results/coverage/Copy/Copy_4_10.coverage", "46791996-3a05-4fd8-9701-8f6cc1dbd09c");
        this.destDir = destDir;
    }

    /**
     * Add a FilterChain.
     * @return a filter chain object.
     */
    public FilterChain createFilterChain() {
        writeline("/home/ubuntu/results/coverage/Copy/Copy_4_10.coverage", "fbee24b3-1ade-4356-ac8f-4b410a7dce71");
        final FilterChain filterChain = new FilterChain();
        writeline("/home/ubuntu/results/coverage/Copy/Copy_4_10.coverage", "6773fdaa-1864-4eae-8fd4-cbb4a6faa34d");
        filterChains.addElement(filterChain);
        writeline("/home/ubuntu/results/coverage/Copy/Copy_4_10.coverage", "1ac3497c-7438-4678-b43f-ba88e5bb9aeb");
        return filterChain;
    }

    /**
     * Add a filterset.
     * @return a filter set object.
     */
    public FilterSet createFilterSet() {
        writeline("/home/ubuntu/results/coverage/Copy/Copy_4_10.coverage", "907c564a-3221-4c38-9461-3f97c4b67a1d");
        final FilterSet filterSet = new FilterSet();
        writeline("/home/ubuntu/results/coverage/Copy/Copy_4_10.coverage", "eed31b0d-5c98-4f9b-b740-baa28cd9a90c");
        filterSets.addElement(filterSet);
        writeline("/home/ubuntu/results/coverage/Copy/Copy_4_10.coverage", "b14c23d2-a000-456d-9bbc-bb9836ae4449");
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
        writeline("/home/ubuntu/results/coverage/Copy/Copy_4_10.coverage", "33ff4a0d-ee62-4373-aa3a-9f33c4716d14");
        setPreserveLastModified(Project.toBoolean(preserve));
    }

    /**
     * Give the copied files the same last modified time as the original files.
     * @param preserve if true preserve the modified time; default is false.
     */
    public void setPreserveLastModified(final boolean preserve) {
        writeline("/home/ubuntu/results/coverage/Copy/Copy_4_10.coverage", "75f442de-7333-40c5-a1ae-93748261162a");
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
        writeline("/home/ubuntu/results/coverage/Copy/Copy_4_10.coverage", "947674c3-6aba-47d0-9eee-9793d27f9b36");
        return preserveLastModified;
    }

    /**
     * Get the filtersets being applied to this operation.
     *
     * @return a vector of FilterSet objects.
     */
    protected Vector<FilterSet> getFilterSets() {
        writeline("/home/ubuntu/results/coverage/Copy/Copy_4_10.coverage", "32a610b5-46d8-4fa9-afec-17b2aac1a628");
        return filterSets;
    }

    /**
     * Get the filterchains being applied to this operation.
     *
     * @return a vector of FilterChain objects.
     */
    protected Vector<FilterChain> getFilterChains() {
        writeline("/home/ubuntu/results/coverage/Copy/Copy_4_10.coverage", "13076b27-087b-4ab0-b5a6-ab1b2354932f");
        return filterChains;
    }

    /**
     * Set filtering mode.
     * @param filtering if true enable filtering; default is false.
     */
    public void setFiltering(final boolean filtering) {
        writeline("/home/ubuntu/results/coverage/Copy/Copy_4_10.coverage", "80d25739-f381-44b8-91a2-bd379681f34a");
        this.filtering = filtering;
    }

    /**
     * Set overwrite mode regarding existing destination file(s).
     * @param overwrite if true force overwriting of destination file(s)
     * even if the destination file(s) are younger than
     * the corresponding source file. Default is false.
     */
    public void setOverwrite(final boolean overwrite) {
        writeline("/home/ubuntu/results/coverage/Copy/Copy_4_10.coverage", "5052de19-e4e2-4aa8-bf8a-e99c3d855fe6");
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
        writeline("/home/ubuntu/results/coverage/Copy/Copy_4_10.coverage", "25d54112-9cbb-4f72-ba0c-7a3277de420c");
        force = f;
    }

    /**
     * Whether read-only destinations will be overwritten.
     *
     * @since Ant 1.8.2
     */
    public boolean getForce() {
        writeline("/home/ubuntu/results/coverage/Copy/Copy_4_10.coverage", "d3ec41b2-86c0-4cd9-b8cc-f64628836a8f");
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
        writeline("/home/ubuntu/results/coverage/Copy/Copy_4_10.coverage", "407b73b3-eeb7-4004-bfb4-73c0d5bdf393");
        this.flatten = flatten;
    }

    /**
     * Set verbose mode. Used to force listing of all names of copied files.
     * @param verbose whether to output the names of copied files.
     * Default is false.
     */
    public void setVerbose(final boolean verbose) {
        writeline("/home/ubuntu/results/coverage/Copy/Copy_4_10.coverage", "0dc01f8e-1f7b-4867-a507-dcc4c46ef6c1");
        this.verbosity = verbose ? Project.MSG_INFO : Project.MSG_VERBOSE;
    }

    /**
     * Set whether to copy empty directories.
     * @param includeEmpty if true copy empty directories. Default is true.
     */
    public void setIncludeEmptyDirs(final boolean includeEmpty) {
        writeline("/home/ubuntu/results/coverage/Copy/Copy_4_10.coverage", "de987553-e146-4c87-b0ec-4ce642bcdb3b");
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
        writeline("/home/ubuntu/results/coverage/Copy/Copy_4_10.coverage", "58a555a0-9dfe-433b-afa2-2ff65f8a84b5");
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
        writeline("/home/ubuntu/results/coverage/Copy/Copy_4_10.coverage", "ff46c700-cd16-429e-8ade-cd27ee20ade2");
        this.enableMultipleMappings = enableMultipleMappings;
    }

    /**
     * Get whether multiple mapping is enabled.
     * @return true if multiple mapping is enabled; false otherwise.
     */
    public boolean isEnableMultipleMapping() {
        writeline("/home/ubuntu/results/coverage/Copy/Copy_4_10.coverage", "790e5601-6995-4d83-b894-d72c1d473611");
        return enableMultipleMappings;
    }

    /**
     * Set whether to fail when errors are encountered. If false, note errors
     * to the output but keep going. Default is true.
     * @param failonerror true or false.
     */
    public void setFailOnError(final boolean failonerror) {
        writeline("/home/ubuntu/results/coverage/Copy/Copy_4_10.coverage", "3b301d9f-5c5c-455f-96d2-fa1745a755d0");
        this.failonerror = failonerror;
    }

    /**
     * Add a set of files to copy.
     * @param set a set of files to copy.
     */
    public void addFileset(final FileSet set) {
        writeline("/home/ubuntu/results/coverage/Copy/Copy_4_10.coverage", "69109184-c5e8-47f7-b179-a5ae38c42507");
        add(set);
    }

    /**
     * Add a collection of files to copy.
     * @param res a resource collection to copy.
     * @since Ant 1.7
     */
    public void add(final ResourceCollection res) {
        writeline("/home/ubuntu/results/coverage/Copy/Copy_4_10.coverage", "93486273-a119-4440-9b2c-cf0df6ca3bf6");
        rcs.add(res);
    }

    /**
     * Define the mapper to map source to destination files.
     * @return a mapper to be configured.
     * @exception BuildException if more than one mapper is defined.
     */
    public Mapper createMapper() throws BuildException {
        writeline("/home/ubuntu/results/coverage/Copy/Copy_4_10.coverage", "ec21aa81-00fb-4a57-acaf-251abea80af6");
        if (mapperElement != null) {
            writeline("/home/ubuntu/results/coverage/Copy/Copy_4_10.coverage", "f135931f-0a28-4ec7-9747-c52b52554a3c");
            throw new BuildException("Cannot define more than one mapper", getLocation());
        }
        writeline("/home/ubuntu/results/coverage/Copy/Copy_4_10.coverage", "61a8e65e-cb5e-49bf-9bfb-a4491d81b90b");
        mapperElement = new Mapper(getProject());
        writeline("/home/ubuntu/results/coverage/Copy/Copy_4_10.coverage", "b76c5eeb-a4dc-4d74-a3f3-ea38d7f61da3");
        return mapperElement;
    }

    /**
     * Add a nested filenamemapper.
     * @param fileNameMapper the mapper to add.
     * @since Ant 1.6.3
     */
    public void add(final FileNameMapper fileNameMapper) {
        writeline("/home/ubuntu/results/coverage/Copy/Copy_4_10.coverage", "fa37bb97-ce9b-4dae-8939-2536df88b229");
        createMapper().add(fileNameMapper);
    }

    /**
     * Set the character encoding.
     * @param encoding the character encoding.
     * @since 1.32, Ant 1.5
     */
    public void setEncoding(final String encoding) {
        writeline("/home/ubuntu/results/coverage/Copy/Copy_4_10.coverage", "3371488b-4a0e-42ce-8f0e-23a60cb5cf17");
        this.inputEncoding = encoding;
        writeline("/home/ubuntu/results/coverage/Copy/Copy_4_10.coverage", "3ae90d42-afbf-4570-8f30-3d2aae15878f");
        if (outputEncoding == null) {
            writeline("/home/ubuntu/results/coverage/Copy/Copy_4_10.coverage", "145beab2-4949-4587-89fc-9fe834893b9c");
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
        writeline("/home/ubuntu/results/coverage/Copy/Copy_4_10.coverage", "c3ac5ba0-0307-4851-88fa-57fe85ad4d8e");
        return inputEncoding;
    }

    /**
     * Set the character encoding for output files.
     * @param encoding the output character encoding.
     * @since Ant 1.6
     */
    public void setOutputEncoding(final String encoding) {
        writeline("/home/ubuntu/results/coverage/Copy/Copy_4_10.coverage", "d495e2e5-d806-4b58-b8b4-96f5582b337f");
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
        writeline("/home/ubuntu/results/coverage/Copy/Copy_4_10.coverage", "84cc7dfc-7f2e-4a86-9b38-acece963b2f3");
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
        writeline("/home/ubuntu/results/coverage/Copy/Copy_4_10.coverage", "58f0b147-c194-4b8d-8d44-7d3e94861ff2");
        this.granularity = granularity;
    }

    /**
     * Perform the copy operation.
     * @exception BuildException if an error occurs.
     */
    @Override
    public void execute() throws BuildException {
        writeline("/home/ubuntu/results/coverage/Copy/Copy_4_10.coverage", "a06c6cef-9feb-4e81-abb3-4116db6c522b");
        // may be altered in validateAttributes
        final File savedFile = file;
        writeline("/home/ubuntu/results/coverage/Copy/Copy_4_10.coverage", "de9a173f-1e73-4ac8-bd97-dddc300aad8c");
        final File savedDestFile = destFile;
        writeline("/home/ubuntu/results/coverage/Copy/Copy_4_10.coverage", "865792b8-9901-4863-8f9d-1c32e334d7bc");
        final File savedDestDir = destDir;
        writeline("/home/ubuntu/results/coverage/Copy/Copy_4_10.coverage", "98fff74a-9030-469a-ac56-d55db4ef65e0");
        ResourceCollection savedRc = null;
        writeline("/home/ubuntu/results/coverage/Copy/Copy_4_10.coverage", "1a953042-871a-4111-bfec-f42ad10f85e2");
        if (file == null && destFile != null && rcs.size() == 1) {
            writeline("/home/ubuntu/results/coverage/Copy/Copy_4_10.coverage", "eaa89d34-8956-4d68-867a-01a8cb1f0bc6");
            // will be removed in validateAttributes
            savedRc = rcs.elementAt(0);
        }
        writeline("/home/ubuntu/results/coverage/Copy/Copy_4_10.coverage", "be3ad4ab-7f55-4cca-8d25-e086231881f0");
        try {
            writeline("/home/ubuntu/results/coverage/Copy/Copy_4_10.coverage", "1982c97a-5687-4934-9389-d630a0dac908");
            // make sure we don't have an illegal set of options
            try {
                writeline("/home/ubuntu/results/coverage/Copy/Copy_4_10.coverage", "f2882011-b06e-499c-b713-e8cd10c41a86");
                validateAttributes();
            } catch (final BuildException e) {
                writeline("/home/ubuntu/results/coverage/Copy/Copy_4_10.coverage", "f0567877-5f2a-4562-852d-8a90dfde544d");
                if (failonerror || !getMessage(e).equals(MSG_WHEN_COPYING_EMPTY_RC_TO_FILE)) {
                    writeline("/home/ubuntu/results/coverage/Copy/Copy_4_10.coverage", "655a2a4d-2c97-4666-997e-e62bb21f0ddc");
                    throw e;
                } else {
                    writeline("/home/ubuntu/results/coverage/Copy/Copy_4_10.coverage", "16b7d3e7-2538-4f00-8d95-0f2613704566");
                    log("Warning: " + getMessage(e), Project.MSG_ERR);
                    writeline("/home/ubuntu/results/coverage/Copy/Copy_4_10.coverage", "c352f054-3df3-487c-ac75-4142db35b03d");
                    return;
                }
            }
            writeline("/home/ubuntu/results/coverage/Copy/Copy_4_10.coverage", "dc38777d-5d16-4bb6-b0e1-3a01478b704e");
            // deal with the single file
            copySingleFile();
            writeline("/home/ubuntu/results/coverage/Copy/Copy_4_10.coverage", "6ffc19f4-970e-4e91-b75c-074e9aa6adae");
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
            writeline("/home/ubuntu/results/coverage/Copy/Copy_4_10.coverage", "48911ac3-b3f6-4b6d-9922-ff559d1ecf37");
            final HashMap<File, List<String>> dirsByBasedir = new HashMap<File, List<String>>();
            writeline("/home/ubuntu/results/coverage/Copy/Copy_4_10.coverage", "534e76ab-3903-40c8-8579-cb8a794ad924");
            final HashSet<File> baseDirs = new HashSet<File>();
            writeline("/home/ubuntu/results/coverage/Copy/Copy_4_10.coverage", "ec091844-e704-4815-b468-531af9c3395c");
            final ArrayList<Resource> nonFileResources = new ArrayList<Resource>();
            writeline("/home/ubuntu/results/coverage/Copy/Copy_4_10.coverage", "32097985-9175-49db-87c8-6ad6aa8644e1");
            final int size = rcs.size();
            writeline("/home/ubuntu/results/coverage/Copy/Copy_4_10.coverage", "08d8585b-c62d-47d7-b697-0a96d1a6080d");
            for (int i = 0; i < size; i++) {
                writeline("/home/ubuntu/results/coverage/Copy/Copy_4_10.coverage", "f8dc4ee3-7444-4df4-91da-6346ba0ebdcd");
                final ResourceCollection rc = rcs.elementAt(i);
                writeline("/home/ubuntu/results/coverage/Copy/Copy_4_10.coverage", "8166656c-0a88-4cc3-96ef-a2723cac46fe");
                // Step (1) - beware of the ZipFileSet
                if (rc instanceof FileSet && rc.isFilesystemOnly()) {
                    writeline("/home/ubuntu/results/coverage/Copy/Copy_4_10.coverage", "cdc62485-72b5-4d4f-bfde-40dde2095398");
                    final FileSet fs = (FileSet) rc;
                    writeline("/home/ubuntu/results/coverage/Copy/Copy_4_10.coverage", "af7bdffd-c9c1-4f07-a1b4-039247b23813");
                    DirectoryScanner ds = null;
                    writeline("/home/ubuntu/results/coverage/Copy/Copy_4_10.coverage", "f939d420-dc5d-4385-925e-9df61dce08c1");
                    try {
                        writeline("/home/ubuntu/results/coverage/Copy/Copy_4_10.coverage", "762496b1-5311-440b-b42a-71bbb7e5834b");
                        ds = fs.getDirectoryScanner(getProject());
                    } catch (final BuildException e) {
                        writeline("/home/ubuntu/results/coverage/Copy/Copy_4_10.coverage", "9568278b-1306-4ffb-9cd3-8427c4cda42f");
                        if (failonerror || !getMessage(e).endsWith(DirectoryScanner.DOES_NOT_EXIST_POSTFIX)) {
                            writeline("/home/ubuntu/results/coverage/Copy/Copy_4_10.coverage", "88a10315-d6ec-4b89-bbcc-754c21208124");
                            throw e;
                        } else {
                            writeline("/home/ubuntu/results/coverage/Copy/Copy_4_10.coverage", "ab61d0cc-1d41-4ebe-8d2a-e77a4d22e838");
                            if (!quiet) {
                                writeline("/home/ubuntu/results/coverage/Copy/Copy_4_10.coverage", "059db3c2-101a-49f9-a41d-27a3d5a47539");
                                log("Warning: " + getMessage(e), Project.MSG_ERR);
                            }
                            writeline("/home/ubuntu/results/coverage/Copy/Copy_4_10.coverage", "a4d0fe21-d7f7-4094-b06d-decad278825e");
                            continue;
                        }
                    }
                    writeline("/home/ubuntu/results/coverage/Copy/Copy_4_10.coverage", "ea394400-3328-4917-be77-7552f7fff280");
                    final File fromDir = fs.getDir(getProject());
                    writeline("/home/ubuntu/results/coverage/Copy/Copy_4_10.coverage", "51142626-f715-4aff-a183-157f8fb22e52");
                    final String[] srcFiles = ds.getIncludedFiles();
                    writeline("/home/ubuntu/results/coverage/Copy/Copy_4_10.coverage", "a7fa6119-b6d9-4956-9e99-e36c297d5079");
                    final String[] srcDirs = ds.getIncludedDirectories();
                    writeline("/home/ubuntu/results/coverage/Copy/Copy_4_10.coverage", "e6c8d92f-88ba-4c30-b4de-c7fc1d6d83c9");
                    if (!flatten && mapperElement == null && ds.isEverythingIncluded() && !fs.hasPatterns()) {
                        writeline("/home/ubuntu/results/coverage/Copy/Copy_4_10.coverage", "17c5da18-dd32-4e5e-9dae-e73bbd37d98f");
                        completeDirMap.put(fromDir, destDir);
                    }
                    writeline("/home/ubuntu/results/coverage/Copy/Copy_4_10.coverage", "89c7458f-c981-4a35-baa6-573c0b8ddbad");
                    add(fromDir, srcFiles, filesByBasedir);
                    writeline("/home/ubuntu/results/coverage/Copy/Copy_4_10.coverage", "d98a6d1f-a7c7-4345-a9bb-ea7eea58da56");
                    add(fromDir, srcDirs, dirsByBasedir);
                    writeline("/home/ubuntu/results/coverage/Copy/Copy_4_10.coverage", "bea836e8-44f4-41c2-814a-8212506de94a");
                    baseDirs.add(fromDir);
                } else {
                    writeline("/home/ubuntu/results/coverage/Copy/Copy_4_10.coverage", "9a5a8e2e-2f6e-42a1-9093-f010c5a16a84");
                    if (!rc.isFilesystemOnly() && !supportsNonFileResources()) {
                        writeline("/home/ubuntu/results/coverage/Copy/Copy_4_10.coverage", "512dfeaf-0f18-4dba-9955-b3c2942e21fd");
                        throw new BuildException("Only FileSystem resources are supported.");
                    }
                    writeline("/home/ubuntu/results/coverage/Copy/Copy_4_10.coverage", "696339dd-7862-4aab-8dbf-11c5cd0c728d");
                    for (final Resource r : rc) {
                        writeline("/home/ubuntu/results/coverage/Copy/Copy_4_10.coverage", "78428acc-4b45-4e1d-b1f8-990d8f86ab04");
                        if (!r.isExists()) {
                            writeline("/home/ubuntu/results/coverage/Copy/Copy_4_10.coverage", "be8e5f20-2f32-4d28-b3dc-16a478d6cf9c");
                            final String message = "Warning: Could not find resource " + r.toLongString() + " to copy.";
                            writeline("/home/ubuntu/results/coverage/Copy/Copy_4_10.coverage", "4177dd01-07a9-4e46-b4e7-8f32dbe86171");
                            if (!failonerror) {
                                writeline("/home/ubuntu/results/coverage/Copy/Copy_4_10.coverage", "cbb2f47e-4921-4ee0-8c58-76bc71858c89");
                                if (!quiet) {
                                    writeline("/home/ubuntu/results/coverage/Copy/Copy_4_10.coverage", "9aa4e7c0-d1f9-4567-af8a-9c0d4c65274d");
                                    log(message, Project.MSG_ERR);
                                }
                            } else {
                                writeline("/home/ubuntu/results/coverage/Copy/Copy_4_10.coverage", "8a28c8bb-2191-4d83-81b3-34d053507fbc");
                                throw new BuildException(message);
                            }
                            writeline("/home/ubuntu/results/coverage/Copy/Copy_4_10.coverage", "bbfb30c7-4131-422f-955d-bea62ec647d6");
                            continue;
                        }
                        writeline("/home/ubuntu/results/coverage/Copy/Copy_4_10.coverage", "7917d3d0-3f34-41c6-9796-ebd5146d6f5f");
                        File baseDir = NULL_FILE_PLACEHOLDER;
                        writeline("/home/ubuntu/results/coverage/Copy/Copy_4_10.coverage", "3282e345-fadf-46e2-bdd8-094096c7749f");
                        String name = r.getName();
                        writeline("/home/ubuntu/results/coverage/Copy/Copy_4_10.coverage", "9527d8ba-1346-41ab-944b-088cd8a56a7c");
                        final FileProvider fp = r.as(FileProvider.class);
                        writeline("/home/ubuntu/results/coverage/Copy/Copy_4_10.coverage", "a25b9d6c-f07c-4bac-87de-51b939ce0742");
                        if (fp != null) {
                            writeline("/home/ubuntu/results/coverage/Copy/Copy_4_10.coverage", "0533766a-6aee-444f-aea7-348c01be2214");
                            final FileResource fr = ResourceUtils.asFileResource(fp);
                            writeline("/home/ubuntu/results/coverage/Copy/Copy_4_10.coverage", "2ff86249-01e6-4e37-813a-8020619885fb");
                            baseDir = getKeyFile(fr.getBaseDir());
                            writeline("/home/ubuntu/results/coverage/Copy/Copy_4_10.coverage", "1927e190-a0a8-45e3-b681-04497bfa52ac");
                            if (fr.getBaseDir() == null) {
                                writeline("/home/ubuntu/results/coverage/Copy/Copy_4_10.coverage", "fee0d631-78ca-41d3-b578-9f25f8477bcb");
                                name = fr.getFile().getAbsolutePath();
                            }
                        }
                        writeline("/home/ubuntu/results/coverage/Copy/Copy_4_10.coverage", "01625ae3-b965-4565-808d-87819fb4a120");
                        // files.
                        if (r.isDirectory() || fp != null) {
                            writeline("/home/ubuntu/results/coverage/Copy/Copy_4_10.coverage", "e81efcbd-d4c2-4cdc-a578-4e9f8ca5ee7a");
                            add(baseDir, name, r.isDirectory() ? dirsByBasedir : filesByBasedir);
                            writeline("/home/ubuntu/results/coverage/Copy/Copy_4_10.coverage", "ecc38d6d-f86c-4a77-88d7-2697688582ec");
                            baseDirs.add(baseDir);
                        } else {
                            writeline("/home/ubuntu/results/coverage/Copy/Copy_4_10.coverage", "057aad1b-4855-4a5f-a05d-1b6ae52c5ce2");
                            // a not-directory file resource
                            // needs special treatment
                            nonFileResources.add(r);
                        }
                    }
                }
            }
            writeline("/home/ubuntu/results/coverage/Copy/Copy_4_10.coverage", "694efae4-0efd-4d69-9c1f-353c83308a42");
            iterateOverBaseDirs(baseDirs, dirsByBasedir, filesByBasedir);
            writeline("/home/ubuntu/results/coverage/Copy/Copy_4_10.coverage", "672bce88-baf7-4d9e-a648-bf09e938c28a");
            // do all the copy operations now...
            try {
                writeline("/home/ubuntu/results/coverage/Copy/Copy_4_10.coverage", "8dd6b7df-7f38-4d27-9a83-a5027657ae62");
                doFileOperations();
            } catch (final BuildException e) {
                writeline("/home/ubuntu/results/coverage/Copy/Copy_4_10.coverage", "1eb3da95-31da-4bd1-8a9f-2a89ad187363");
                if (!failonerror) {
                    writeline("/home/ubuntu/results/coverage/Copy/Copy_4_10.coverage", "2c04ff81-133f-42f3-b381-ae912899f2ae");
                    if (!quiet) {
                        writeline("/home/ubuntu/results/coverage/Copy/Copy_4_10.coverage", "602e6be3-f910-4fd7-b958-9df433917eab");
                        log("Warning: " + getMessage(e), Project.MSG_ERR);
                    }
                } else {
                    writeline("/home/ubuntu/results/coverage/Copy/Copy_4_10.coverage", "cc41c960-7c61-4fd5-97ad-fa9eb6b60ca9");
                    throw e;
                }
            }
            writeline("/home/ubuntu/results/coverage/Copy/Copy_4_10.coverage", "e12dd1bb-f2bc-4a6f-be31-c9cf9dc18e02");
            if (nonFileResources.size() > 0 || singleResource != null) {
                writeline("/home/ubuntu/results/coverage/Copy/Copy_4_10.coverage", "923daffa-642f-4c84-b177-eee10f892704");
                final Resource[] nonFiles = nonFileResources.toArray(new Resource[nonFileResources.size()]);
                writeline("/home/ubuntu/results/coverage/Copy/Copy_4_10.coverage", "cba0db33-c023-4db4-8f12-9802075511a1");
                // restrict to out-of-date resources
                final Map<Resource, String[]> map = scan(nonFiles, destDir);
                writeline("/home/ubuntu/results/coverage/Copy/Copy_4_10.coverage", "075076cd-e261-4c27-b9b6-bfdfd7ba3a36");
                if (singleResource != null) {
                    writeline("/home/ubuntu/results/coverage/Copy/Copy_4_10.coverage", "6fe51a7d-27a7-40a5-bb1a-48ef599f3327");
                    map.put(singleResource, new String[] { destFile.getAbsolutePath() });
                }
                writeline("/home/ubuntu/results/coverage/Copy/Copy_4_10.coverage", "e12177df-f3b2-4cb7-a1c0-ea33eb10c8d4");
                try {
                    writeline("/home/ubuntu/results/coverage/Copy/Copy_4_10.coverage", "1525fbe4-b708-4aba-b74a-c3a134b41510");
                    doResourceOperations(map);
                } catch (final BuildException e) {
                    writeline("/home/ubuntu/results/coverage/Copy/Copy_4_10.coverage", "8a6808c2-e440-44f1-a8e1-ab8c1b075f75");
                    if (!failonerror) {
                        writeline("/home/ubuntu/results/coverage/Copy/Copy_4_10.coverage", "749ddbea-c5a2-4ad0-9e66-d44c7938d637");
                        if (!quiet) {
                            writeline("/home/ubuntu/results/coverage/Copy/Copy_4_10.coverage", "33920f31-c532-41b5-a5c7-83837fc7f93a");
                            log("Warning: " + getMessage(e), Project.MSG_ERR);
                        }
                    } else {
                        writeline("/home/ubuntu/results/coverage/Copy/Copy_4_10.coverage", "8645d6bc-f19b-4a1a-b116-8395db4ce7e0");
                        throw e;
                    }
                }
            }
        } finally {
            writeline("/home/ubuntu/results/coverage/Copy/Copy_4_10.coverage", "b60fe34c-78d4-4992-9928-56345bd590e9");
            // clean up again, so this instance can be used a second
            // time
            singleResource = null;
            writeline("/home/ubuntu/results/coverage/Copy/Copy_4_10.coverage", "083b4443-1c50-4e28-8a6f-1c4e0fd55bef");
            file = savedFile;
            writeline("/home/ubuntu/results/coverage/Copy/Copy_4_10.coverage", "ee07ca55-0955-4662-91ae-7e2e1deb03bb");
            destFile = savedDestFile;
            writeline("/home/ubuntu/results/coverage/Copy/Copy_4_10.coverage", "7f747c66-e0e1-4609-bc04-9161940c39a2");
            destDir = savedDestDir;
            writeline("/home/ubuntu/results/coverage/Copy/Copy_4_10.coverage", "45022f16-7484-4d22-82e9-d777e6f2be84");
            if (savedRc != null) {
                writeline("/home/ubuntu/results/coverage/Copy/Copy_4_10.coverage", "79148d6e-317c-4d1f-b118-2d0d6646c12a");
                rcs.insertElementAt(savedRc, 0);
            }
            writeline("/home/ubuntu/results/coverage/Copy/Copy_4_10.coverage", "b55c0bed-215e-4d1c-abb8-f6c46c223f4c");
            fileCopyMap.clear();
            writeline("/home/ubuntu/results/coverage/Copy/Copy_4_10.coverage", "2368bd4a-9ca9-4db6-bffa-7174862b37c1");
            dirCopyMap.clear();
            writeline("/home/ubuntu/results/coverage/Copy/Copy_4_10.coverage", "95293c60-2f7f-488e-b6ee-557a60d0f4a6");
            completeDirMap.clear();
        }
    }

    /**
     * *********************************************************************
     * *  protected and private methods
     * **********************************************************************
     */
    private void copySingleFile() {
        writeline("/home/ubuntu/results/coverage/Copy/Copy_4_10.coverage", "906e154b-6323-49cf-821e-383242361807");
        // deal with the single file
        if (file != null) {
            writeline("/home/ubuntu/results/coverage/Copy/Copy_4_10.coverage", "4fe55fdf-35fd-46df-a77f-e8e427bcc421");
            if (file.exists()) {
                writeline("/home/ubuntu/results/coverage/Copy/Copy_4_10.coverage", "a087fbdf-df2d-447b-8cba-840c2362bee6");
                if (destFile == null) {
                    writeline("/home/ubuntu/results/coverage/Copy/Copy_4_10.coverage", "bcf02038-0a52-4ba2-9d61-14cc9099afea");
                    destFile = new File(destDir, file.getName());
                }
                writeline("/home/ubuntu/results/coverage/Copy/Copy_4_10.coverage", "fcafa538-998c-4f67-9dd7-e2feb78002f1");
                if (forceOverwrite || !destFile.exists() || (file.lastModified() - granularity > destFile.lastModified())) {
                    writeline("/home/ubuntu/results/coverage/Copy/Copy_4_10.coverage", "cc18e7ad-34ed-4ef3-8b92-b6e2c55fd15b");
                    fileCopyMap.put(file.getAbsolutePath(), new String[] { destFile.getAbsolutePath() });
                } else {
                    writeline("/home/ubuntu/results/coverage/Copy/Copy_4_10.coverage", "d2092425-361d-435e-9618-c3aa371a08b7");
                    log(file + " omitted as " + destFile + " is up to date.", Project.MSG_VERBOSE);
                }
            } else {
                writeline("/home/ubuntu/results/coverage/Copy/Copy_4_10.coverage", "57cb3b9c-2f99-4806-b81c-4857e7097ee3");
                final String message = "Warning: Could not find file " + file.getAbsolutePath() + " to copy.";
                writeline("/home/ubuntu/results/coverage/Copy/Copy_4_10.coverage", "b774b2b8-96a8-46b2-96fe-cd5f2c47f675");
                if (!failonerror) {
                    writeline("/home/ubuntu/results/coverage/Copy/Copy_4_10.coverage", "1247cb58-390f-49b3-bd2c-d2a9bdf78192");
                    if (!quiet) {
                        writeline("/home/ubuntu/results/coverage/Copy/Copy_4_10.coverage", "292f8c31-3000-4637-8f0f-4a9296878394");
                        log(message, Project.MSG_ERR);
                    }
                } else {
                    writeline("/home/ubuntu/results/coverage/Copy/Copy_4_10.coverage", "de64c303-f6eb-4a24-8297-c0e5bb332f2c");
                    throw new BuildException(message);
                }
            }
        }
    }

    private void iterateOverBaseDirs(final HashSet<File> baseDirs, final HashMap<File, List<String>> dirsByBasedir, final HashMap<File, List<String>> filesByBasedir) {
        writeline("/home/ubuntu/results/coverage/Copy/Copy_4_10.coverage", "2483b1c6-f075-430b-8968-061dcb870c7b");
        for (final File f : baseDirs) {
            writeline("/home/ubuntu/results/coverage/Copy/Copy_4_10.coverage", "645e8403-bfde-4cd1-860e-aed101a09ac1");
            final List<String> files = filesByBasedir.get(f);
            writeline("/home/ubuntu/results/coverage/Copy/Copy_4_10.coverage", "6292ac4e-32bb-4931-933b-70d903a2829b");
            final List<String> dirs = dirsByBasedir.get(f);
            writeline("/home/ubuntu/results/coverage/Copy/Copy_4_10.coverage", "4d11ea38-483d-477b-bf8e-0e8006de758d");
            String[] srcFiles = new String[0];
            writeline("/home/ubuntu/results/coverage/Copy/Copy_4_10.coverage", "2dbe528a-8f7c-467b-82f0-1fc7ee62ada9");
            if (files != null) {
                writeline("/home/ubuntu/results/coverage/Copy/Copy_4_10.coverage", "a1fe3fc3-2481-4c33-8fbb-cd102c122f2b");
                srcFiles = files.toArray(srcFiles);
            }
            writeline("/home/ubuntu/results/coverage/Copy/Copy_4_10.coverage", "1f489c50-6d12-48f9-b2aa-7dca5ab84fbb");
            String[] srcDirs = new String[0];
            writeline("/home/ubuntu/results/coverage/Copy/Copy_4_10.coverage", "0892e8b6-0c8e-4805-aa1b-53ead4ea12ca");
            if (dirs != null) {
                writeline("/home/ubuntu/results/coverage/Copy/Copy_4_10.coverage", "4041fd5d-f00d-40dd-8fba-4ff40879caf7");
                srcDirs = dirs.toArray(srcDirs);
            }
            writeline("/home/ubuntu/results/coverage/Copy/Copy_4_10.coverage", "db56aa73-1da6-4cc4-b431-9f34a151c427");
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
        writeline("/home/ubuntu/results/coverage/Copy/Copy_4_10.coverage", "20817992-cc6d-4955-b9a9-a73e03b5841b");
        if (file == null && rcs.size() == 0) {
            writeline("/home/ubuntu/results/coverage/Copy/Copy_4_10.coverage", "7cc5d4df-8ede-449c-96cb-ff5517e79bec");
            throw new BuildException("Specify at least one source--a file or a resource collection.");
        }
        writeline("/home/ubuntu/results/coverage/Copy/Copy_4_10.coverage", "97b3742d-c65b-48d0-b2a6-d5c5922aef5d");
        if (destFile != null && destDir != null) {
            writeline("/home/ubuntu/results/coverage/Copy/Copy_4_10.coverage", "188a68d2-fd75-40d1-89ed-92b1401f4a99");
            throw new BuildException("Only one of tofile and todir may be set.");
        }
        writeline("/home/ubuntu/results/coverage/Copy/Copy_4_10.coverage", "9b4d0e22-aa76-4431-b5d1-1e5ec89ec93f");
        if (destFile == null && destDir == null) {
            writeline("/home/ubuntu/results/coverage/Copy/Copy_4_10.coverage", "651c5df2-3933-4d46-a5c7-da322de773aa");
            throw new BuildException("One of tofile or todir must be set.");
        }
        writeline("/home/ubuntu/results/coverage/Copy/Copy_4_10.coverage", "c2a30877-c439-4dbf-a967-0b2583d432af");
        if (file != null && file.isDirectory()) {
            writeline("/home/ubuntu/results/coverage/Copy/Copy_4_10.coverage", "94f395f6-a7e3-4465-9840-a2022021410d");
            throw new BuildException("Use a resource collection to copy directories.");
        }
        writeline("/home/ubuntu/results/coverage/Copy/Copy_4_10.coverage", "a289d98b-943a-47bc-a271-020bc5acbf4f");
        if (destFile != null && rcs.size() > 0) {
            writeline("/home/ubuntu/results/coverage/Copy/Copy_4_10.coverage", "d12ae304-53a5-4f31-8a54-68b68a6ab0ff");
            if (rcs.size() > 1) {
                writeline("/home/ubuntu/results/coverage/Copy/Copy_4_10.coverage", "7cf260c8-5743-4e4a-a882-3d94921d762e");
                throw new BuildException("Cannot concatenate multiple files into a single file.");
            } else {
                writeline("/home/ubuntu/results/coverage/Copy/Copy_4_10.coverage", "dd4e4db1-c122-4f6e-a51b-ad8dfbd95850");
                final ResourceCollection rc = rcs.elementAt(0);
                writeline("/home/ubuntu/results/coverage/Copy/Copy_4_10.coverage", "0df322c0-4766-4248-bd34-a8f18607e84c");
                if (!rc.isFilesystemOnly() && !supportsNonFileResources()) {
                    writeline("/home/ubuntu/results/coverage/Copy/Copy_4_10.coverage", "deeaba7a-e03f-4c5a-998f-23e2c3a4e811");
                    throw new BuildException("Only FileSystem resources are" + " supported.");
                }
                writeline("/home/ubuntu/results/coverage/Copy/Copy_4_10.coverage", "11ea9233-6560-4e7d-83b3-483311eb265a");
                if (rc.size() == 0) {
                    writeline("/home/ubuntu/results/coverage/Copy/Copy_4_10.coverage", "17bb5e2d-55b1-46b3-ad0d-c84839aafb44");
                    throw new BuildException(MSG_WHEN_COPYING_EMPTY_RC_TO_FILE);
                } else if (rc.size() == 1) {
                    writeline("/home/ubuntu/results/coverage/Copy/Copy_4_10.coverage", "415bc2ab-931a-494f-bff3-ad8bbc6cf4a2");
                    final Resource res = rc.iterator().next();
                    writeline("/home/ubuntu/results/coverage/Copy/Copy_4_10.coverage", "5a852c56-24b7-4f65-9be1-dc6fc23ab31a");
                    final FileProvider r = res.as(FileProvider.class);
                    writeline("/home/ubuntu/results/coverage/Copy/Copy_4_10.coverage", "8971c496-e266-41e6-ac99-11ee7544fd45");
                    if (file == null) {
                        writeline("/home/ubuntu/results/coverage/Copy/Copy_4_10.coverage", "060ed1c3-e842-4c24-bdde-3a4df6792617");
                        if (r != null) {
                            writeline("/home/ubuntu/results/coverage/Copy/Copy_4_10.coverage", "2692b851-ac19-4348-90b3-54027dec417a");
                            file = r.getFile();
                        } else {
                            writeline("/home/ubuntu/results/coverage/Copy/Copy_4_10.coverage", "fb3c3eee-a7e9-451f-9190-1064d7425b40");
                            singleResource = res;
                        }
                        writeline("/home/ubuntu/results/coverage/Copy/Copy_4_10.coverage", "645069a2-f9de-4c83-bb57-19b2a8f75d60");
                        rcs.removeElementAt(0);
                    } else {
                        writeline("/home/ubuntu/results/coverage/Copy/Copy_4_10.coverage", "4bfebc19-778f-4e33-b44d-191c3ec7d00f");
                        throw new BuildException("Cannot concatenate multiple files into a single file.");
                    }
                } else {
                    writeline("/home/ubuntu/results/coverage/Copy/Copy_4_10.coverage", "13854e0f-5110-41a5-a46c-97d8bc44b38a");
                    throw new BuildException("Cannot concatenate multiple files into a single file.");
                }
            }
        }
        writeline("/home/ubuntu/results/coverage/Copy/Copy_4_10.coverage", "86983035-e7b2-48f9-869b-986f15dd9465");
        if (destFile != null) {
            writeline("/home/ubuntu/results/coverage/Copy/Copy_4_10.coverage", "888801e0-2e42-4beb-b8f9-ff96047443a7");
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
        writeline("/home/ubuntu/results/coverage/Copy/Copy_4_10.coverage", "f1a1c8ef-1253-4290-a3ae-3392d5dd45ec");
        final FileNameMapper mapper = getMapper();
        writeline("/home/ubuntu/results/coverage/Copy/Copy_4_10.coverage", "cff43b59-78da-4f50-a3d3-3ab9002b30d8");
        buildMap(fromDir, toDir, files, mapper, fileCopyMap);
        writeline("/home/ubuntu/results/coverage/Copy/Copy_4_10.coverage", "a06cf878-8017-4e83-96f1-52280959c448");
        if (includeEmpty) {
            writeline("/home/ubuntu/results/coverage/Copy/Copy_4_10.coverage", "c7801597-fcea-4f7d-9cea-28d98cbb3363");
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
        writeline("/home/ubuntu/results/coverage/Copy/Copy_4_10.coverage", "4dec013e-7b39-4306-abab-faf68454c2b9");
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
        writeline("/home/ubuntu/results/coverage/Copy/Copy_4_10.coverage", "149224b6-0790-4e62-b9e9-d7dc82ed5ce2");
        String[] toCopy = null;
        writeline("/home/ubuntu/results/coverage/Copy/Copy_4_10.coverage", "12d7bb19-0881-44f4-9665-72fed8b32632");
        if (forceOverwrite) {
            writeline("/home/ubuntu/results/coverage/Copy/Copy_4_10.coverage", "1b2470b1-bec9-4f24-8d57-c1324f9d688e");
            final Vector<String> v = new Vector<String>();
            writeline("/home/ubuntu/results/coverage/Copy/Copy_4_10.coverage", "051b880b-8bb9-44d5-b3c1-122b6bc93b71");
            for (int i = 0; i < names.length; i++) {
                writeline("/home/ubuntu/results/coverage/Copy/Copy_4_10.coverage", "f93f4fc8-e26a-4a2b-a865-64434a449790");
                if (mapper.mapFileName(names[i]) != null) {
                    writeline("/home/ubuntu/results/coverage/Copy/Copy_4_10.coverage", "8ef11054-241a-4d9a-836a-bbc82180a0de");
                    v.addElement(names[i]);
                }
            }
            writeline("/home/ubuntu/results/coverage/Copy/Copy_4_10.coverage", "5a05226f-644f-4218-98b9-e2faaebaabd7");
            toCopy = new String[v.size()];
            writeline("/home/ubuntu/results/coverage/Copy/Copy_4_10.coverage", "91157cb8-a8df-4d69-9c01-3818cfea2311");
            v.copyInto(toCopy);
        } else {
            writeline("/home/ubuntu/results/coverage/Copy/Copy_4_10.coverage", "ddc94ded-0a75-447f-95f2-b96d1c140fa6");
            final SourceFileScanner ds = new SourceFileScanner(this);
            writeline("/home/ubuntu/results/coverage/Copy/Copy_4_10.coverage", "baa8ee5d-0672-4b9d-9be6-3c231c01efc8");
            toCopy = ds.restrict(names, fromDir, toDir, mapper, granularity);
        }
        writeline("/home/ubuntu/results/coverage/Copy/Copy_4_10.coverage", "0b896251-c31c-481a-8a54-b18ec09fabeb");
        for (int i = 0; i < toCopy.length; i++) {
            writeline("/home/ubuntu/results/coverage/Copy/Copy_4_10.coverage", "db910307-81e1-4766-a6ec-7d6fe748e0fd");
            final File src = new File(fromDir, toCopy[i]);
            writeline("/home/ubuntu/results/coverage/Copy/Copy_4_10.coverage", "aa75fc3e-1d07-4683-82af-ea8f778fa596");
            final String[] mappedFiles = mapper.mapFileName(toCopy[i]);
            writeline("/home/ubuntu/results/coverage/Copy/Copy_4_10.coverage", "e4d76901-3a6b-4de8-bb94-7eedf5517ad1");
            if (!enableMultipleMappings) {
                writeline("/home/ubuntu/results/coverage/Copy/Copy_4_10.coverage", "82e4f76c-2ae7-4b1e-b595-bbb3ee4e6538");
                map.put(src.getAbsolutePath(), new String[] { new File(toDir, mappedFiles[0]).getAbsolutePath() });
            } else {
                writeline("/home/ubuntu/results/coverage/Copy/Copy_4_10.coverage", "c4032084-b8c3-49cc-a6e5-374e532ec9cb");
                // reuse the array created by the mapper
                for (int k = 0; k < mappedFiles.length; k++) {
                    writeline("/home/ubuntu/results/coverage/Copy/Copy_4_10.coverage", "42144587-3695-433c-b7be-d1ef0dd114d4");
                    mappedFiles[k] = new File(toDir, mappedFiles[k]).getAbsolutePath();
                }
                writeline("/home/ubuntu/results/coverage/Copy/Copy_4_10.coverage", "6cc9b814-f6ed-430c-b910-20cd0e0fa3b9");
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
        writeline("/home/ubuntu/results/coverage/Copy/Copy_4_10.coverage", "eac2299a-b523-4f90-bf78-b568d56480ce");
        final HashMap<Resource, String[]> map = new HashMap<Resource, String[]>();
        writeline("/home/ubuntu/results/coverage/Copy/Copy_4_10.coverage", "9fbe2218-25ab-4aab-a8dd-b553a13a1a81");
        Resource[] toCopy = null;
        writeline("/home/ubuntu/results/coverage/Copy/Copy_4_10.coverage", "ff4ede08-7841-4a3f-b22f-30c8666b12c4");
        if (forceOverwrite) {
            writeline("/home/ubuntu/results/coverage/Copy/Copy_4_10.coverage", "2e29986d-2e1b-4f52-8d63-6a8097614c21");
            final Vector<Resource> v = new Vector<Resource>();
            writeline("/home/ubuntu/results/coverage/Copy/Copy_4_10.coverage", "9d1676d4-4e5c-4e7f-b90b-5d961314ef56");
            for (int i = 0; i < fromResources.length; i++) {
                writeline("/home/ubuntu/results/coverage/Copy/Copy_4_10.coverage", "59ff152b-ff73-4661-b48a-2e3e9070c384");
                if (mapper.mapFileName(fromResources[i].getName()) != null) {
                    writeline("/home/ubuntu/results/coverage/Copy/Copy_4_10.coverage", "02f87ba9-ed10-4d5a-bdc5-dd86cc489d70");
                    v.addElement(fromResources[i]);
                }
            }
            writeline("/home/ubuntu/results/coverage/Copy/Copy_4_10.coverage", "cb34a657-e694-4da8-af66-2ab53b6bb753");
            toCopy = new Resource[v.size()];
            writeline("/home/ubuntu/results/coverage/Copy/Copy_4_10.coverage", "62564250-1799-44d8-9f88-72c362c5cf4b");
            v.copyInto(toCopy);
        } else {
            writeline("/home/ubuntu/results/coverage/Copy/Copy_4_10.coverage", "737b453c-4952-424f-987d-0a284e64fc32");
            toCopy = ResourceUtils.selectOutOfDateSources(this, fromResources, mapper, new ResourceFactory() {

                public Resource getResource(final String name) {
                    return new FileResource(toDir, name);
                }
            }, granularity);
        }
        writeline("/home/ubuntu/results/coverage/Copy/Copy_4_10.coverage", "68e6546a-fdfc-4b94-8a8a-bca508c5d3ba");
        for (int i = 0; i < toCopy.length; i++) {
            writeline("/home/ubuntu/results/coverage/Copy/Copy_4_10.coverage", "c34ab246-7aca-4646-946c-585dd3b7b59d");
            final String[] mappedFiles = mapper.mapFileName(toCopy[i].getName());
            writeline("/home/ubuntu/results/coverage/Copy/Copy_4_10.coverage", "7551fd59-d1f6-464b-933e-4425e4b394d9");
            for (int j = 0; j < mappedFiles.length; j++) {
                writeline("/home/ubuntu/results/coverage/Copy/Copy_4_10.coverage", "468eaf12-3d86-47f3-8fc2-75db85aa8c3e");
                if (mappedFiles[j] == null) {
                    writeline("/home/ubuntu/results/coverage/Copy/Copy_4_10.coverage", "803edd59-446b-49df-8beb-488b0c2eeb59");
                    throw new BuildException("Can't copy a resource without a" + " name if the mapper doesn't" + " provide one.");
                }
            }
            writeline("/home/ubuntu/results/coverage/Copy/Copy_4_10.coverage", "ce4d64a8-a9c7-436c-8d56-e7fc9de9fcfe");
            if (!enableMultipleMappings) {
                writeline("/home/ubuntu/results/coverage/Copy/Copy_4_10.coverage", "7e1eca39-4dc5-4f9b-8d0f-2636de6faa5a");
                map.put(toCopy[i], new String[] { new File(toDir, mappedFiles[0]).getAbsolutePath() });
            } else {
                writeline("/home/ubuntu/results/coverage/Copy/Copy_4_10.coverage", "d42aa7f8-866c-4bbb-ac9d-d1dc5e59ef4f");
                // reuse the array created by the mapper
                for (int k = 0; k < mappedFiles.length; k++) {
                    writeline("/home/ubuntu/results/coverage/Copy/Copy_4_10.coverage", "b0d19600-2b0e-48d4-b951-502a915e2c60");
                    mappedFiles[k] = new File(toDir, mappedFiles[k]).getAbsolutePath();
                }
                writeline("/home/ubuntu/results/coverage/Copy/Copy_4_10.coverage", "2c3b8374-e54a-4718-9729-141fec5622b2");
                map.put(toCopy[i], mappedFiles);
            }
        }
        writeline("/home/ubuntu/results/coverage/Copy/Copy_4_10.coverage", "3cc8bac9-54c9-4cc2-8b90-fefc4603a536");
        return map;
    }

    /**
     * Actually does the file (and possibly empty directory) copies.
     * This is a good method for subclasses to override.
     */
    protected void doFileOperations() {
        writeline("/home/ubuntu/results/coverage/Copy/Copy_4_10.coverage", "74486b41-07fa-44db-bc34-44f7e452171c");
        if (fileCopyMap.size() > 0) {
            writeline("/home/ubuntu/results/coverage/Copy/Copy_4_10.coverage", "d2a10219-437b-47f8-9adc-7678f644106d");
            log("Copying " + fileCopyMap.size() + " file" + (fileCopyMap.size() == 1 ? "" : "s") + " to " + destDir.getAbsolutePath());
            writeline("/home/ubuntu/results/coverage/Copy/Copy_4_10.coverage", "fa52303b-ee3a-4d37-a693-5737d4e36afe");
            for (final Map.Entry<String, String[]> e : fileCopyMap.entrySet()) {
                writeline("/home/ubuntu/results/coverage/Copy/Copy_4_10.coverage", "3911f410-3935-4e44-9657-a5e42940104f");
                final String fromFile = e.getKey();
                writeline("/home/ubuntu/results/coverage/Copy/Copy_4_10.coverage", "faf59e82-e92f-465b-9cfd-e19469d33a68");
                final String[] toFiles = e.getValue();
                writeline("/home/ubuntu/results/coverage/Copy/Copy_4_10.coverage", "cf9ee08c-6f55-47ff-ad85-2ee2bfb1eac2");
                for (int i = 0; i < toFiles.length; i++) {
                    writeline("/home/ubuntu/results/coverage/Copy/Copy_4_10.coverage", "512c17bc-fb5c-4b1d-ba9f-81ebc967a808");
                    final String toFile = toFiles[i];
                    writeline("/home/ubuntu/results/coverage/Copy/Copy_4_10.coverage", "5a9ab999-abb0-4692-928b-87414568e630");
                    if (fromFile.equals(toFile)) {
                        writeline("/home/ubuntu/results/coverage/Copy/Copy_4_10.coverage", "52210267-4abe-454f-abe8-5c406650946a");
                        log("Skipping self-copy of " + fromFile, verbosity);
                        writeline("/home/ubuntu/results/coverage/Copy/Copy_4_10.coverage", "ea8f254e-3c87-4e5f-af43-17ea93e0489b");
                        continue;
                    }
                    writeline("/home/ubuntu/results/coverage/Copy/Copy_4_10.coverage", "9ec07b93-4221-4e99-9ca2-8582270fb2e3");
                    try {
                        writeline("/home/ubuntu/results/coverage/Copy/Copy_4_10.coverage", "b8eff8ea-ce08-4a51-92d8-15edd75dc6f1");
                        log("Copying " + fromFile + " to " + toFile, verbosity);
                        writeline("/home/ubuntu/results/coverage/Copy/Copy_4_10.coverage", "b3e6074e-7c15-4325-a651-97c6177aee1e");
                        final FilterSetCollection executionFilters = new FilterSetCollection();
                        writeline("/home/ubuntu/results/coverage/Copy/Copy_4_10.coverage", "a00470c4-767f-4ebe-b749-e8ca4ea2fe38");
                        if (filtering) {
                            writeline("/home/ubuntu/results/coverage/Copy/Copy_4_10.coverage", "a03e91cf-6afd-46f5-8ea0-658c99818bb7");
                            executionFilters.addFilterSet(getProject().getGlobalFilterSet());
                        }
                        writeline("/home/ubuntu/results/coverage/Copy/Copy_4_10.coverage", "396c12ec-b90f-45bb-b8a6-460cd8eaf7a2");
                        for (final FilterSet filterSet : filterSets) {
                            writeline("/home/ubuntu/results/coverage/Copy/Copy_4_10.coverage", "bd0d922e-28d3-417a-ae84-1569c18bf216");
                            executionFilters.addFilterSet(filterSet);
                        }
                        writeline("/home/ubuntu/results/coverage/Copy/Copy_4_10.coverage", "bd0c7d2a-bf32-4527-8893-db5ad91e09f5");
                        fileUtils.copyFile(new File(fromFile), new File(toFile), executionFilters, filterChains, forceOverwrite, preserveLastModified, /* append: */
                        false, inputEncoding, outputEncoding, getProject(), getForce());
                    } catch (final IOException ioe) {
                        writeline("/home/ubuntu/results/coverage/Copy/Copy_4_10.coverage", "5f009bc2-0efa-4a3f-97a3-9f397c39743e");
                        String msg = "Failed to copy " + fromFile + " to " + toFile + " due to " + getDueTo(ioe);
                        writeline("/home/ubuntu/results/coverage/Copy/Copy_4_10.coverage", "d2f84a6f-1e45-4907-81fa-3307b7048e40");
                        final File targetFile = new File(toFile);
                        writeline("/home/ubuntu/results/coverage/Copy/Copy_4_10.coverage", "415db3a7-bb57-46fe-b500-3cb01ec42b77");
                        if (!(ioe instanceof ResourceUtils.ReadOnlyTargetFileException) && targetFile.exists() && !targetFile.delete()) {
                            writeline("/home/ubuntu/results/coverage/Copy/Copy_4_10.coverage", "44305b0f-7f26-45a2-95ce-89f6887c9857");
                            msg += " and I couldn't delete the corrupt " + toFile;
                        }
                        writeline("/home/ubuntu/results/coverage/Copy/Copy_4_10.coverage", "54d9222a-4d2d-424b-b02d-fc028c59d9b5");
                        if (failonerror) {
                            writeline("/home/ubuntu/results/coverage/Copy/Copy_4_10.coverage", "2b60617d-5eb2-4159-bab6-902cd7ea29c8");
                            throw new BuildException(msg, ioe, getLocation());
                        }
                        writeline("/home/ubuntu/results/coverage/Copy/Copy_4_10.coverage", "fc09a8e0-f372-465f-97cf-f6576e45551e");
                        log(msg, Project.MSG_ERR);
                    }
                }
            }
        }
        writeline("/home/ubuntu/results/coverage/Copy/Copy_4_10.coverage", "1cabbad0-726d-41c7-b905-a97cd3ecb758");
        if (includeEmpty) {
            writeline("/home/ubuntu/results/coverage/Copy/Copy_4_10.coverage", "5c35e840-2be9-40f9-9a41-c52b9ec58e40");
            int createCount = 0;
            writeline("/home/ubuntu/results/coverage/Copy/Copy_4_10.coverage", "0c9ad6bd-64c3-409a-95ee-258783f9d4b1");
            for (final String[] dirs : dirCopyMap.values()) {
                writeline("/home/ubuntu/results/coverage/Copy/Copy_4_10.coverage", "a5530654-8b50-44ac-a1ea-319d8eb62c66");
                for (int i = 0; i < dirs.length; i++) {
                    writeline("/home/ubuntu/results/coverage/Copy/Copy_4_10.coverage", "05ed56b8-796d-4e53-83cd-3f14762a6b7b");
                    final File d = new File(dirs[i]);
                    writeline("/home/ubuntu/results/coverage/Copy/Copy_4_10.coverage", "e334b6e8-e013-433d-a1be-e74463bcd5fc");
                    if (!d.exists()) {
                        writeline("/home/ubuntu/results/coverage/Copy/Copy_4_10.coverage", "7c31d642-0427-4c26-8fd1-2d91051e6e60");
                        if (!(d.mkdirs() || d.isDirectory())) {
                            writeline("/home/ubuntu/results/coverage/Copy/Copy_4_10.coverage", "0d9ad0f0-9bec-4236-a015-8b4fb474f728");
                            log("Unable to create directory " + d.getAbsolutePath(), Project.MSG_ERR);
                        } else {
                            writeline("/home/ubuntu/results/coverage/Copy/Copy_4_10.coverage", "7ccd4f95-7c36-4633-869a-73da37b01d7c");
                            createCount++;
                        }
                    }
                }
            }
            writeline("/home/ubuntu/results/coverage/Copy/Copy_4_10.coverage", "f03e8b9d-cfce-4ce3-8fe5-fd39b2311f56");
            if (createCount > 0) {
                writeline("/home/ubuntu/results/coverage/Copy/Copy_4_10.coverage", "0e0bde6b-c82f-430b-8b31-0eaafe5b924b");
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
        writeline("/home/ubuntu/results/coverage/Copy/Copy_4_10.coverage", "7ac005cb-8e69-4e7a-b45a-6565e21771f6");
        if (map.size() > 0) {
            writeline("/home/ubuntu/results/coverage/Copy/Copy_4_10.coverage", "10b21648-fa29-4b2c-a6e5-bc500ca9369d");
            log("Copying " + map.size() + " resource" + (map.size() == 1 ? "" : "s") + " to " + destDir.getAbsolutePath());
            writeline("/home/ubuntu/results/coverage/Copy/Copy_4_10.coverage", "ea06a392-5a3f-4d69-a1f3-417975673e5b");
            for (final Map.Entry<Resource, String[]> e : map.entrySet()) {
                writeline("/home/ubuntu/results/coverage/Copy/Copy_4_10.coverage", "2625b9b7-f68c-475b-b4ca-90783195edc7");
                final Resource fromResource = e.getKey();
                writeline("/home/ubuntu/results/coverage/Copy/Copy_4_10.coverage", "94bc281e-b4a2-4fe4-93ea-db380c4f2136");
                for (final String toFile : e.getValue()) {
                    writeline("/home/ubuntu/results/coverage/Copy/Copy_4_10.coverage", "e6ab347c-4d0a-4047-99ca-0f0441dbc3b9");
                    try {
                        writeline("/home/ubuntu/results/coverage/Copy/Copy_4_10.coverage", "f650d546-a183-44b5-a17b-cfd9b3e8be5c");
                        log("Copying " + fromResource + " to " + toFile, verbosity);
                        writeline("/home/ubuntu/results/coverage/Copy/Copy_4_10.coverage", "fa747627-7e25-4075-a3d7-1577cded685a");
                        final FilterSetCollection executionFilters = new FilterSetCollection();
                        writeline("/home/ubuntu/results/coverage/Copy/Copy_4_10.coverage", "b920f08e-6d74-47dc-8d14-c9275be2adc2");
                        if (filtering) {
                            writeline("/home/ubuntu/results/coverage/Copy/Copy_4_10.coverage", "8bcb0774-ba65-49e8-a1e4-35a662b39c40");
                            executionFilters.addFilterSet(getProject().getGlobalFilterSet());
                        }
                        writeline("/home/ubuntu/results/coverage/Copy/Copy_4_10.coverage", "c0d2e69a-f103-4dc5-91d8-6a29de053b46");
                        for (final FilterSet filterSet : filterSets) {
                            writeline("/home/ubuntu/results/coverage/Copy/Copy_4_10.coverage", "5ae6bee7-7c0a-4c4d-9a26-8809a04ff3c3");
                            executionFilters.addFilterSet(filterSet);
                        }
                        writeline("/home/ubuntu/results/coverage/Copy/Copy_4_10.coverage", "39394ec2-331a-4f33-ab2d-dded249fc0e3");
                        ResourceUtils.copyResource(fromResource, new FileResource(destDir, toFile), executionFilters, filterChains, forceOverwrite, preserveLastModified, /* append: */
                        false, inputEncoding, outputEncoding, getProject(), getForce());
                    } catch (final IOException ioe) {
                        writeline("/home/ubuntu/results/coverage/Copy/Copy_4_10.coverage", "52d2495c-fc02-4aa3-9858-0ab4af64a596");
                        String msg = "Failed to copy " + fromResource + " to " + toFile + " due to " + getDueTo(ioe);
                        writeline("/home/ubuntu/results/coverage/Copy/Copy_4_10.coverage", "6135ad6b-86e7-4499-a4ca-c261ecd0de71");
                        final File targetFile = new File(toFile);
                        writeline("/home/ubuntu/results/coverage/Copy/Copy_4_10.coverage", "fdfdc93d-eed2-46fa-8f68-825b5391eebf");
                        if (!(ioe instanceof ResourceUtils.ReadOnlyTargetFileException) && targetFile.exists() && !targetFile.delete()) {
                            writeline("/home/ubuntu/results/coverage/Copy/Copy_4_10.coverage", "188478fe-d605-4f47-bba8-a8cfd25160b5");
                            msg += " and I couldn't delete the corrupt " + toFile;
                        }
                        writeline("/home/ubuntu/results/coverage/Copy/Copy_4_10.coverage", "411483d1-85c7-4b32-8e76-b8906a136376");
                        if (failonerror) {
                            writeline("/home/ubuntu/results/coverage/Copy/Copy_4_10.coverage", "58ea09d6-5e24-4cee-b562-c37c0b041177");
                            throw new BuildException(msg, ioe, getLocation());
                        }
                        writeline("/home/ubuntu/results/coverage/Copy/Copy_4_10.coverage", "c86e3d30-5980-41f2-bdd1-1e93768677ff");
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
        writeline("/home/ubuntu/results/coverage/Copy/Copy_4_10.coverage", "f3ef30d6-aa4a-4b68-b9e1-4aa4cd8e7702");
        return getClass().equals(Copy.class);
    }

    /**
     * Adds the given strings to a list contained in the given map.
     * The file is the key into the map.
     */
    private static void add(File baseDir, final String[] names, final Map<File, List<String>> m) {
        writelineStatic("/home/ubuntu/results/coverage/Copy/Copy_4_10.coverage", "05e1c03b-189a-43c0-9d6e-cf14840826d7");
        if (names != null) {
            writelineStatic("/home/ubuntu/results/coverage/Copy/Copy_4_10.coverage", "ee8db532-6660-4fbe-a30d-7f39d05f8e53");
            baseDir = getKeyFile(baseDir);
            writelineStatic("/home/ubuntu/results/coverage/Copy/Copy_4_10.coverage", "89868641-beb1-49c6-bc12-874fa66824ed");
            List<String> l = m.get(baseDir);
            writelineStatic("/home/ubuntu/results/coverage/Copy/Copy_4_10.coverage", "8195d2ed-0d9b-46ff-b58e-13e39fbf6716");
            if (l == null) {
                writelineStatic("/home/ubuntu/results/coverage/Copy/Copy_4_10.coverage", "e07a9522-6f1a-4cbc-868e-f9f8829a6cd7");
                l = new ArrayList<String>(names.length);
                writelineStatic("/home/ubuntu/results/coverage/Copy/Copy_4_10.coverage", "b7fde580-3c5c-45ae-82dc-22d767f3e513");
                m.put(baseDir, l);
            }
            writelineStatic("/home/ubuntu/results/coverage/Copy/Copy_4_10.coverage", "65998ffe-6867-4a91-acc4-d5a72136e351");
            l.addAll(java.util.Arrays.asList(names));
        }
    }

    /**
     * Adds the given string to a list contained in the given map.
     * The file is the key into the map.
     */
    private static void add(final File baseDir, final String name, final Map<File, List<String>> m) {
        writelineStatic("/home/ubuntu/results/coverage/Copy/Copy_4_10.coverage", "ca57b751-c78e-40ad-be32-3465b951af9d");
        if (name != null) {
            writelineStatic("/home/ubuntu/results/coverage/Copy/Copy_4_10.coverage", "b175af4a-8aae-40ff-b056-745ddc46eb6d");
            add(baseDir, new String[] { name }, m);
        }
    }

    /**
     * Either returns its argument or a plaeholder if the argument is null.
     */
    private static File getKeyFile(final File f) {
        writelineStatic("/home/ubuntu/results/coverage/Copy/Copy_4_10.coverage", "40324f2f-7765-47cf-b077-48bed0546cfd");
        return f == null ? NULL_FILE_PLACEHOLDER : f;
    }

    /**
     * returns the mapper to use based on nested elements or the
     * flatten attribute.
     */
    private FileNameMapper getMapper() {
        writeline("/home/ubuntu/results/coverage/Copy/Copy_4_10.coverage", "abb0dc00-3afb-443c-8b8c-387eb9045ea5");
        FileNameMapper mapper = null;
        writeline("/home/ubuntu/results/coverage/Copy/Copy_4_10.coverage", "7b53482d-f2a6-451b-b202-a1cc0560c68a");
        if (mapperElement != null) {
            writeline("/home/ubuntu/results/coverage/Copy/Copy_4_10.coverage", "4f380d84-5808-49b6-a6c6-a491b4818b09");
            mapper = mapperElement.getImplementation();
        } else if (flatten) {
            writeline("/home/ubuntu/results/coverage/Copy/Copy_4_10.coverage", "d5e3867d-7dc6-4252-bb1f-3de5604d46fb");
            mapper = new FlatFileNameMapper();
        } else {
            writeline("/home/ubuntu/results/coverage/Copy/Copy_4_10.coverage", "75699a78-383c-4aba-b249-5cf79db349f2");
            mapper = new IdentityMapper();
        }
        writeline("/home/ubuntu/results/coverage/Copy/Copy_4_10.coverage", "549b5c0b-e093-4247-b920-63f0958c4e84");
        return mapper;
    }

    /**
     * Handle getMessage() for exceptions.
     * @param ex the exception to handle
     * @return ex.getMessage() if ex.getMessage() is not null
     * otherwise return ex.toString()
     */
    private String getMessage(final Exception ex) {
        writeline("/home/ubuntu/results/coverage/Copy/Copy_4_10.coverage", "b136b9a4-0ae8-4a10-9cc8-33be4bc16f5a");
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
        writeline("/home/ubuntu/results/coverage/Copy/Copy_4_10.coverage", "84c54e1c-7d7a-475b-99f2-bcdfb7aad71b");
        final boolean baseIOException = ex.getClass() == IOException.class;
        writeline("/home/ubuntu/results/coverage/Copy/Copy_4_10.coverage", "bbf36313-e5ea-43c2-a1e0-e8db66b5cd26");
        final StringBuffer message = new StringBuffer();
        writeline("/home/ubuntu/results/coverage/Copy/Copy_4_10.coverage", "200a11a9-1dfb-4673-8d4d-0ec7a2e6adcf");
        if (!baseIOException || ex.getMessage() == null) {
            writeline("/home/ubuntu/results/coverage/Copy/Copy_4_10.coverage", "769f5485-d9e1-4e91-af72-827c277fe797");
            message.append(ex.getClass().getName());
        }
        writeline("/home/ubuntu/results/coverage/Copy/Copy_4_10.coverage", "69af1246-45aa-4443-ba93-96aea15c3e7f");
        if (ex.getMessage() != null) {
            writeline("/home/ubuntu/results/coverage/Copy/Copy_4_10.coverage", "40efe58f-aba7-488f-9211-80c2e8089ab0");
            if (!baseIOException) {
                writeline("/home/ubuntu/results/coverage/Copy/Copy_4_10.coverage", "5f9f4e3b-0e07-42a3-a30c-b99588283d6f");
                message.append(" ");
            }
            writeline("/home/ubuntu/results/coverage/Copy/Copy_4_10.coverage", "63eca231-58af-42a2-a4b3-f4bfb52daee4");
            message.append(ex.getMessage());
        }
        writeline("/home/ubuntu/results/coverage/Copy/Copy_4_10.coverage", "c74eb836-978d-494a-99e4-e5ff7f740df8");
        if (ex.getClass().getName().indexOf("MalformedInput") != -1) {
            writeline("/home/ubuntu/results/coverage/Copy/Copy_4_10.coverage", "49d9d9e1-9174-415e-b802-91b14980235b");
            message.append(LINE_SEPARATOR);
            writeline("/home/ubuntu/results/coverage/Copy/Copy_4_10.coverage", "54ef702a-58fc-4511-b4e7-9fd846b125b2");
            message.append("This is normally due to the input file containing invalid");
            writeline("/home/ubuntu/results/coverage/Copy/Copy_4_10.coverage", "27911731-f1e2-43c7-a1b1-3207ff686c35");
            message.append(LINE_SEPARATOR);
            writeline("/home/ubuntu/results/coverage/Copy/Copy_4_10.coverage", "1eea91dd-820b-4334-b479-c3baa33f414c");
            message.append("bytes for the character encoding used : ");
            writeline("/home/ubuntu/results/coverage/Copy/Copy_4_10.coverage", "2dc89962-2d80-4fe1-86e3-9428ec9ec751");
            message.append((inputEncoding == null ? fileUtils.getDefaultEncoding() : inputEncoding));
            writeline("/home/ubuntu/results/coverage/Copy/Copy_4_10.coverage", "5e3664f6-f2a8-4934-8a4d-c9ed49119e89");
            message.append(LINE_SEPARATOR);
        }
        writeline("/home/ubuntu/results/coverage/Copy/Copy_4_10.coverage", "38107595-0dc0-4b29-8401-4b6bd226b0af");
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
