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
        writeline("/home/ubuntu/results/coverage/Copy/Copy_7_10.coverage", "0fd4e421-14a4-4d12-8d40-611aeb38a87d");
        return fileUtils;
    }

    /**
     * Set a single source file to copy.
     * @param file the file to copy.
     */
    public void setFile(final File file) {
        writeline("/home/ubuntu/results/coverage/Copy/Copy_7_10.coverage", "7de8a79c-a965-4e6f-bf70-4d382acac09a");
        this.file = file;
    }

    /**
     * Set the destination file.
     * @param destFile the file to copy to.
     */
    public void setTofile(final File destFile) {
        writeline("/home/ubuntu/results/coverage/Copy/Copy_7_10.coverage", "28cc1e02-2f5f-4af4-8736-78fe0445b99a");
        this.destFile = destFile;
    }

    /**
     * Set the destination directory.
     * @param destDir the destination directory.
     */
    public void setTodir(final File destDir) {
        writeline("/home/ubuntu/results/coverage/Copy/Copy_7_10.coverage", "a92a57db-23e4-4063-931c-33b74f12d274");
        this.destDir = destDir;
    }

    /**
     * Add a FilterChain.
     * @return a filter chain object.
     */
    public FilterChain createFilterChain() {
        writeline("/home/ubuntu/results/coverage/Copy/Copy_7_10.coverage", "756998df-8ba4-40e0-9430-f969b2ab3715");
        final FilterChain filterChain = new FilterChain();
        writeline("/home/ubuntu/results/coverage/Copy/Copy_7_10.coverage", "a6213235-cd8f-431c-8d71-5687f2fd9abf");
        filterChains.addElement(filterChain);
        writeline("/home/ubuntu/results/coverage/Copy/Copy_7_10.coverage", "00b3085f-51e8-406b-b353-db4e3b1b3ec8");
        return filterChain;
    }

    /**
     * Add a filterset.
     * @return a filter set object.
     */
    public FilterSet createFilterSet() {
        writeline("/home/ubuntu/results/coverage/Copy/Copy_7_10.coverage", "95dd9c14-c8e2-4f34-a92d-c7302f76859b");
        final FilterSet filterSet = new FilterSet();
        writeline("/home/ubuntu/results/coverage/Copy/Copy_7_10.coverage", "1e28686d-378b-477f-9bc1-90ea135ba06b");
        filterSets.addElement(filterSet);
        writeline("/home/ubuntu/results/coverage/Copy/Copy_7_10.coverage", "73f592ea-171c-4e91-a153-539b201ec1b6");
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
        writeline("/home/ubuntu/results/coverage/Copy/Copy_7_10.coverage", "08496487-36f9-4b91-b30c-1e298e32ae30");
        setPreserveLastModified(Project.toBoolean(preserve));
    }

    /**
     * Give the copied files the same last modified time as the original files.
     * @param preserve if true preserve the modified time; default is false.
     */
    public void setPreserveLastModified(final boolean preserve) {
        writeline("/home/ubuntu/results/coverage/Copy/Copy_7_10.coverage", "e4d87d32-aaf0-4f4a-98bf-db55b2651482");
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
        writeline("/home/ubuntu/results/coverage/Copy/Copy_7_10.coverage", "5a73a635-28e8-4455-8b46-b4c36e868911");
        return preserveLastModified;
    }

    /**
     * Get the filtersets being applied to this operation.
     *
     * @return a vector of FilterSet objects.
     */
    protected Vector<FilterSet> getFilterSets() {
        writeline("/home/ubuntu/results/coverage/Copy/Copy_7_10.coverage", "3ad9dc6c-6a47-4bdf-89e4-48d4baa30fda");
        return filterSets;
    }

    /**
     * Get the filterchains being applied to this operation.
     *
     * @return a vector of FilterChain objects.
     */
    protected Vector<FilterChain> getFilterChains() {
        writeline("/home/ubuntu/results/coverage/Copy/Copy_7_10.coverage", "308192fa-ad66-4715-a805-52e9481421cd");
        return filterChains;
    }

    /**
     * Set filtering mode.
     * @param filtering if true enable filtering; default is false.
     */
    public void setFiltering(final boolean filtering) {
        writeline("/home/ubuntu/results/coverage/Copy/Copy_7_10.coverage", "86b5b2a6-91ee-46bc-8633-4707ba629e9c");
        this.filtering = filtering;
    }

    /**
     * Set overwrite mode regarding existing destination file(s).
     * @param overwrite if true force overwriting of destination file(s)
     * even if the destination file(s) are younger than
     * the corresponding source file. Default is false.
     */
    public void setOverwrite(final boolean overwrite) {
        writeline("/home/ubuntu/results/coverage/Copy/Copy_7_10.coverage", "b5d7aeec-be50-4613-85d2-689e3c2d1c71");
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
        writeline("/home/ubuntu/results/coverage/Copy/Copy_7_10.coverage", "5e1911fe-14d9-4bfe-a934-796ab9edc520");
        force = f;
    }

    /**
     * Whether read-only destinations will be overwritten.
     *
     * @since Ant 1.8.2
     */
    public boolean getForce() {
        writeline("/home/ubuntu/results/coverage/Copy/Copy_7_10.coverage", "f606ea1a-389a-4fef-a366-5ade372bf75f");
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
        writeline("/home/ubuntu/results/coverage/Copy/Copy_7_10.coverage", "680e3aa8-1bc7-42c4-a194-d88aef13e5a1");
        this.flatten = flatten;
    }

    /**
     * Set verbose mode. Used to force listing of all names of copied files.
     * @param verbose whether to output the names of copied files.
     * Default is false.
     */
    public void setVerbose(final boolean verbose) {
        writeline("/home/ubuntu/results/coverage/Copy/Copy_7_10.coverage", "208c8de1-d78d-4e4f-8f4e-90def8ce8aa9");
        this.verbosity = verbose ? Project.MSG_INFO : Project.MSG_VERBOSE;
    }

    /**
     * Set whether to copy empty directories.
     * @param includeEmpty if true copy empty directories. Default is true.
     */
    public void setIncludeEmptyDirs(final boolean includeEmpty) {
        writeline("/home/ubuntu/results/coverage/Copy/Copy_7_10.coverage", "e6a4037d-dc92-4c93-bcfe-83e112b804db");
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
        writeline("/home/ubuntu/results/coverage/Copy/Copy_7_10.coverage", "aca307ad-6fa0-406e-b131-df194ae8f3f5");
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
        writeline("/home/ubuntu/results/coverage/Copy/Copy_7_10.coverage", "c16a63fd-2f51-4ecd-8116-fc3fd9fa31f4");
        this.enableMultipleMappings = enableMultipleMappings;
    }

    /**
     * Get whether multiple mapping is enabled.
     * @return true if multiple mapping is enabled; false otherwise.
     */
    public boolean isEnableMultipleMapping() {
        writeline("/home/ubuntu/results/coverage/Copy/Copy_7_10.coverage", "13c01dfb-88b3-4dbb-adf8-414251f826ef");
        return enableMultipleMappings;
    }

    /**
     * Set whether to fail when errors are encountered. If false, note errors
     * to the output but keep going. Default is true.
     * @param failonerror true or false.
     */
    public void setFailOnError(final boolean failonerror) {
        writeline("/home/ubuntu/results/coverage/Copy/Copy_7_10.coverage", "5994d5aa-940f-4a3b-b911-9e396495bfa7");
        this.failonerror = failonerror;
    }

    /**
     * Add a set of files to copy.
     * @param set a set of files to copy.
     */
    public void addFileset(final FileSet set) {
        writeline("/home/ubuntu/results/coverage/Copy/Copy_7_10.coverage", "34f26829-c12c-46a4-8d0e-752bc69dd72e");
        add(set);
    }

    /**
     * Add a collection of files to copy.
     * @param res a resource collection to copy.
     * @since Ant 1.7
     */
    public void add(final ResourceCollection res) {
        writeline("/home/ubuntu/results/coverage/Copy/Copy_7_10.coverage", "5d6cdbae-f585-430e-baec-498e53b5d988");
        rcs.add(res);
    }

    /**
     * Define the mapper to map source to destination files.
     * @return a mapper to be configured.
     * @exception BuildException if more than one mapper is defined.
     */
    public Mapper createMapper() throws BuildException {
        writeline("/home/ubuntu/results/coverage/Copy/Copy_7_10.coverage", "74b64e0f-4f75-4c01-a508-90f1da0bb9c4");
        if (mapperElement != null) {
            writeline("/home/ubuntu/results/coverage/Copy/Copy_7_10.coverage", "61d3658f-7385-41f4-b602-0214838a6455");
            throw new BuildException("Cannot define more than one mapper", getLocation());
        }
        writeline("/home/ubuntu/results/coverage/Copy/Copy_7_10.coverage", "68c131cd-fc53-49a6-8528-9b7e6df62fec");
        mapperElement = new Mapper(getProject());
        writeline("/home/ubuntu/results/coverage/Copy/Copy_7_10.coverage", "1e74287b-127d-45c7-b610-6cdb69ecc10a");
        return mapperElement;
    }

    /**
     * Add a nested filenamemapper.
     * @param fileNameMapper the mapper to add.
     * @since Ant 1.6.3
     */
    public void add(final FileNameMapper fileNameMapper) {
        writeline("/home/ubuntu/results/coverage/Copy/Copy_7_10.coverage", "e890e930-7945-4906-954f-0264aae36092");
        createMapper().add(fileNameMapper);
    }

    /**
     * Set the character encoding.
     * @param encoding the character encoding.
     * @since 1.32, Ant 1.5
     */
    public void setEncoding(final String encoding) {
        writeline("/home/ubuntu/results/coverage/Copy/Copy_7_10.coverage", "fe5d41a3-a569-442c-8f71-6fce7b0cccb7");
        this.inputEncoding = encoding;
        writeline("/home/ubuntu/results/coverage/Copy/Copy_7_10.coverage", "ca6fac7b-ab41-4330-a309-cf01b0ff7290");
        if (outputEncoding == null) {
            writeline("/home/ubuntu/results/coverage/Copy/Copy_7_10.coverage", "f74e9e02-f81c-485d-8b78-5b2e54ef3d31");
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
        writeline("/home/ubuntu/results/coverage/Copy/Copy_7_10.coverage", "af8f6052-20c2-4fd9-a348-bc4dbe0a2fc1");
        return inputEncoding;
    }

    /**
     * Set the character encoding for output files.
     * @param encoding the output character encoding.
     * @since Ant 1.6
     */
    public void setOutputEncoding(final String encoding) {
        writeline("/home/ubuntu/results/coverage/Copy/Copy_7_10.coverage", "67c2e54d-8070-4336-9bef-8273817cf554");
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
        writeline("/home/ubuntu/results/coverage/Copy/Copy_7_10.coverage", "b7400cac-2bcc-4a3a-876c-b20ccc0f54f0");
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
        writeline("/home/ubuntu/results/coverage/Copy/Copy_7_10.coverage", "6b554898-a1e3-44e5-8c57-06db7fc33c2a");
        this.granularity = granularity;
    }

    /**
     * Perform the copy operation.
     * @exception BuildException if an error occurs.
     */
    @Override
    public void execute() throws BuildException {
        writeline("/home/ubuntu/results/coverage/Copy/Copy_7_10.coverage", "88bf861a-fddf-460c-8ef5-89b04cb3665e");
        // may be altered in validateAttributes
        final File savedFile = file;
        writeline("/home/ubuntu/results/coverage/Copy/Copy_7_10.coverage", "fc50336d-1d56-4441-8b75-37807902c167");
        final File savedDestFile = destFile;
        writeline("/home/ubuntu/results/coverage/Copy/Copy_7_10.coverage", "01078590-f9f7-482a-97f6-5cd76d7bae4c");
        final File savedDestDir = destDir;
        writeline("/home/ubuntu/results/coverage/Copy/Copy_7_10.coverage", "721eb3a6-285c-45aa-8942-4deb9826c054");
        ResourceCollection savedRc = null;
        writeline("/home/ubuntu/results/coverage/Copy/Copy_7_10.coverage", "9267db53-d319-4134-9250-fd8581cd8bc0");
        if (file == null && destFile != null && rcs.size() == 1) {
            writeline("/home/ubuntu/results/coverage/Copy/Copy_7_10.coverage", "840ca8c9-ef00-4b7e-b268-13cd90c3948d");
            // will be removed in validateAttributes
            savedRc = rcs.elementAt(0);
        }
        writeline("/home/ubuntu/results/coverage/Copy/Copy_7_10.coverage", "36fd8e3e-b849-47c7-b72d-b8c4959b1f56");
        try {
            writeline("/home/ubuntu/results/coverage/Copy/Copy_7_10.coverage", "f8dd65bc-a9a4-475f-b84a-b6e2621023b1");
            // make sure we don't have an illegal set of options
            try {
                writeline("/home/ubuntu/results/coverage/Copy/Copy_7_10.coverage", "8ebd89e2-e546-43a2-9b86-563714a21152");
                validateAttributes();
            } catch (final BuildException e) {
                writeline("/home/ubuntu/results/coverage/Copy/Copy_7_10.coverage", "cf485366-696a-442e-b04c-f568f3b45285");
                if (failonerror || !getMessage(e).equals(MSG_WHEN_COPYING_EMPTY_RC_TO_FILE)) {
                    writeline("/home/ubuntu/results/coverage/Copy/Copy_7_10.coverage", "8d811b3b-da89-4428-b90b-aa0995c98de6");
                    throw e;
                } else {
                    writeline("/home/ubuntu/results/coverage/Copy/Copy_7_10.coverage", "9a739fae-d2c2-42f9-bac8-460659ed7b88");
                    log("Warning: " + getMessage(e), Project.MSG_ERR);
                    writeline("/home/ubuntu/results/coverage/Copy/Copy_7_10.coverage", "0abc08e5-9724-45a6-bc77-dd88bd349810");
                    return;
                }
            }
            writeline("/home/ubuntu/results/coverage/Copy/Copy_7_10.coverage", "10bb1d3f-c74a-492c-a072-afc59f83ee36");
            // deal with the single file
            copySingleFile();
            writeline("/home/ubuntu/results/coverage/Copy/Copy_7_10.coverage", "c71fd3ab-3117-4e70-84cc-14aefd4005b0");
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
            writeline("/home/ubuntu/results/coverage/Copy/Copy_7_10.coverage", "c21b1528-9615-477f-809f-23c0951be76b");
            final HashMap<File, List<String>> dirsByBasedir = new HashMap<File, List<String>>();
            writeline("/home/ubuntu/results/coverage/Copy/Copy_7_10.coverage", "3041a7e9-f168-4f12-bdf1-021d536d8b90");
            final HashSet<File> baseDirs = new HashSet<File>();
            writeline("/home/ubuntu/results/coverage/Copy/Copy_7_10.coverage", "ba5e5b49-c441-458f-ae66-9590f03030f4");
            final ArrayList<Resource> nonFileResources = new ArrayList<Resource>();
            writeline("/home/ubuntu/results/coverage/Copy/Copy_7_10.coverage", "7993d694-8834-42ed-9939-3032bbe27e5d");
            final int size = rcs.size();
            writeline("/home/ubuntu/results/coverage/Copy/Copy_7_10.coverage", "6b676c9f-7f3b-4f5e-8340-2684637ce8b1");
            for (int i = 0; i < size; i++) {
                writeline("/home/ubuntu/results/coverage/Copy/Copy_7_10.coverage", "283a6d62-aa09-4b20-be18-56a801f426dd");
                final ResourceCollection rc = rcs.elementAt(i);
                writeline("/home/ubuntu/results/coverage/Copy/Copy_7_10.coverage", "f4ab0495-edf6-4b94-99cf-9da94e6887e2");
                // Step (1) - beware of the ZipFileSet
                if (rc instanceof FileSet && rc.isFilesystemOnly()) {
                    writeline("/home/ubuntu/results/coverage/Copy/Copy_7_10.coverage", "70197f59-8da4-467f-aa1a-619ce83c486a");
                    final FileSet fs = (FileSet) rc;
                    writeline("/home/ubuntu/results/coverage/Copy/Copy_7_10.coverage", "13394b5d-a473-4207-8b44-03e41c9f7c09");
                    DirectoryScanner ds = null;
                    writeline("/home/ubuntu/results/coverage/Copy/Copy_7_10.coverage", "4eaa0568-01f2-4ad6-a9fd-13bfe6432b74");
                    try {
                        writeline("/home/ubuntu/results/coverage/Copy/Copy_7_10.coverage", "37703486-cacd-4a92-91cc-1b35c1c577be");
                        ds = fs.getDirectoryScanner(getProject());
                    } catch (final BuildException e) {
                        writeline("/home/ubuntu/results/coverage/Copy/Copy_7_10.coverage", "eb587148-4c71-42e9-9582-99216643aecd");
                        if (failonerror || !getMessage(e).endsWith(DirectoryScanner.DOES_NOT_EXIST_POSTFIX)) {
                            writeline("/home/ubuntu/results/coverage/Copy/Copy_7_10.coverage", "16be8a64-fac0-42c0-b2b5-9af419fc5ac6");
                            throw e;
                        } else {
                            writeline("/home/ubuntu/results/coverage/Copy/Copy_7_10.coverage", "db065688-2034-4029-a657-668288280a06");
                            if (!quiet) {
                                writeline("/home/ubuntu/results/coverage/Copy/Copy_7_10.coverage", "1e64fed7-cf5c-48da-b8cc-cdc9964c2204");
                                log("Warning: " + getMessage(e), Project.MSG_ERR);
                            }
                            writeline("/home/ubuntu/results/coverage/Copy/Copy_7_10.coverage", "ae241846-4be3-4dd0-a686-97d4c53925ad");
                            continue;
                        }
                    }
                    writeline("/home/ubuntu/results/coverage/Copy/Copy_7_10.coverage", "0aea82a3-adb0-4d02-ab7e-e1da4a9f2e90");
                    final File fromDir = fs.getDir(getProject());
                    writeline("/home/ubuntu/results/coverage/Copy/Copy_7_10.coverage", "8c8aa099-0c3d-4d7b-bf03-19590d3a2f94");
                    final String[] srcFiles = ds.getIncludedFiles();
                    writeline("/home/ubuntu/results/coverage/Copy/Copy_7_10.coverage", "578e2f50-c77f-420c-aef0-15c03bce6e08");
                    final String[] srcDirs = ds.getIncludedDirectories();
                    writeline("/home/ubuntu/results/coverage/Copy/Copy_7_10.coverage", "6c74c30a-60c8-425b-96cf-a435d1d90313");
                    if (!flatten && mapperElement == null && ds.isEverythingIncluded() && !fs.hasPatterns()) {
                        writeline("/home/ubuntu/results/coverage/Copy/Copy_7_10.coverage", "3986e185-ec12-4018-aefa-5d153e5a6bdd");
                        completeDirMap.put(fromDir, destDir);
                    }
                    writeline("/home/ubuntu/results/coverage/Copy/Copy_7_10.coverage", "7b9a49e6-3818-4245-90a8-9d6a00bcdfbc");
                    add(fromDir, srcFiles, filesByBasedir);
                    writeline("/home/ubuntu/results/coverage/Copy/Copy_7_10.coverage", "16a3bf4e-d0c5-4100-b3fd-2a643d468f3e");
                    add(fromDir, srcDirs, dirsByBasedir);
                    writeline("/home/ubuntu/results/coverage/Copy/Copy_7_10.coverage", "ecd50e06-e5e5-41b6-b166-35da44a395f3");
                    baseDirs.add(fromDir);
                } else {
                    writeline("/home/ubuntu/results/coverage/Copy/Copy_7_10.coverage", "9db595d1-074e-48aa-b789-485e9075fde1");
                    if (!rc.isFilesystemOnly() && !supportsNonFileResources()) {
                        writeline("/home/ubuntu/results/coverage/Copy/Copy_7_10.coverage", "c7cc0cec-2efa-45c7-a74d-24658bc6769f");
                        throw new BuildException("Only FileSystem resources are supported.");
                    }
                    writeline("/home/ubuntu/results/coverage/Copy/Copy_7_10.coverage", "23d219f7-29f9-400c-b333-76eaa2cc7f0c");
                    for (final Resource r : rc) {
                        writeline("/home/ubuntu/results/coverage/Copy/Copy_7_10.coverage", "5c19b62d-20ff-49e7-908f-6227fa38d765");
                        if (!r.isExists()) {
                            writeline("/home/ubuntu/results/coverage/Copy/Copy_7_10.coverage", "98e247c4-a885-40af-bf24-a0d690b4030c");
                            final String message = "Warning: Could not find resource " + r.toLongString() + " to copy.";
                            writeline("/home/ubuntu/results/coverage/Copy/Copy_7_10.coverage", "b64218dc-f88a-4908-9458-b84ceef54dcc");
                            if (!failonerror) {
                                writeline("/home/ubuntu/results/coverage/Copy/Copy_7_10.coverage", "be1753cc-8798-47b8-8841-6f0f24259cb2");
                                if (!quiet) {
                                    writeline("/home/ubuntu/results/coverage/Copy/Copy_7_10.coverage", "da545cd2-1261-4c28-afdc-2b5cb02977e8");
                                    log(message, Project.MSG_ERR);
                                }
                            } else {
                                writeline("/home/ubuntu/results/coverage/Copy/Copy_7_10.coverage", "6c8480d5-aca5-4021-aa2a-c11f6c617a2b");
                                throw new BuildException(message);
                            }
                            writeline("/home/ubuntu/results/coverage/Copy/Copy_7_10.coverage", "7cd75451-7ed1-4bc3-b64b-6c3c4a29b9ff");
                            continue;
                        }
                        writeline("/home/ubuntu/results/coverage/Copy/Copy_7_10.coverage", "39d2c6fa-84d5-47d1-afc7-e0b9464e4067");
                        File baseDir = NULL_FILE_PLACEHOLDER;
                        writeline("/home/ubuntu/results/coverage/Copy/Copy_7_10.coverage", "7cc1139f-da39-4110-ad62-08f3478f49aa");
                        String name = r.getName();
                        writeline("/home/ubuntu/results/coverage/Copy/Copy_7_10.coverage", "a0b7845a-3972-4872-914e-bce7b1c939b6");
                        final FileProvider fp = r.as(FileProvider.class);
                        writeline("/home/ubuntu/results/coverage/Copy/Copy_7_10.coverage", "9ff8f8cd-0976-4391-9f61-c4c48356980f");
                        if (fp != null) {
                            writeline("/home/ubuntu/results/coverage/Copy/Copy_7_10.coverage", "a51aa1c8-6c96-40b6-9ffa-e8ad4441cc56");
                            final FileResource fr = ResourceUtils.asFileResource(fp);
                            writeline("/home/ubuntu/results/coverage/Copy/Copy_7_10.coverage", "92a29a10-b5d1-4a56-8529-c1bb8a16b53c");
                            baseDir = getKeyFile(fr.getBaseDir());
                            writeline("/home/ubuntu/results/coverage/Copy/Copy_7_10.coverage", "76878aba-8bd6-4c71-b874-2d7d3b490e0e");
                            if (fr.getBaseDir() == null) {
                                writeline("/home/ubuntu/results/coverage/Copy/Copy_7_10.coverage", "dd619a98-f949-49f3-9f1f-98601e2613dd");
                                name = fr.getFile().getAbsolutePath();
                            }
                        }
                        writeline("/home/ubuntu/results/coverage/Copy/Copy_7_10.coverage", "5333f358-2eba-49b7-8c9e-e2e395f2eef4");
                        // files.
                        if (r.isDirectory() || fp != null) {
                            writeline("/home/ubuntu/results/coverage/Copy/Copy_7_10.coverage", "fd2c2bbc-9ba3-449f-a5d9-ce1537c90134");
                            add(baseDir, name, r.isDirectory() ? dirsByBasedir : filesByBasedir);
                            writeline("/home/ubuntu/results/coverage/Copy/Copy_7_10.coverage", "0536f340-7743-4709-a614-6886c24d5515");
                            baseDirs.add(baseDir);
                        } else {
                            writeline("/home/ubuntu/results/coverage/Copy/Copy_7_10.coverage", "ae5fc1ab-c5a5-4842-b4c0-eca074394e21");
                            // a not-directory file resource
                            // needs special treatment
                            nonFileResources.add(r);
                        }
                    }
                }
            }
            writeline("/home/ubuntu/results/coverage/Copy/Copy_7_10.coverage", "786471fe-51a5-4172-90b5-eaef3878e1ad");
            iterateOverBaseDirs(baseDirs, dirsByBasedir, filesByBasedir);
            writeline("/home/ubuntu/results/coverage/Copy/Copy_7_10.coverage", "bf88969b-4f03-40e4-85e2-a9017ce65de4");
            // do all the copy operations now...
            try {
                writeline("/home/ubuntu/results/coverage/Copy/Copy_7_10.coverage", "2b4e00ef-0216-4300-acdf-befb671bee16");
                doFileOperations();
            } catch (final BuildException e) {
                writeline("/home/ubuntu/results/coverage/Copy/Copy_7_10.coverage", "60c079f3-bfc3-472a-a1f3-d0e474843f3b");
                if (!failonerror) {
                    writeline("/home/ubuntu/results/coverage/Copy/Copy_7_10.coverage", "d442c1af-0939-4480-b877-e7e6209cf584");
                    if (!quiet) {
                        writeline("/home/ubuntu/results/coverage/Copy/Copy_7_10.coverage", "e7b33973-96f3-48d8-af22-f185935a8656");
                        log("Warning: " + getMessage(e), Project.MSG_ERR);
                    }
                } else {
                    writeline("/home/ubuntu/results/coverage/Copy/Copy_7_10.coverage", "65e4fc84-2e3b-436b-af35-5538a59543cb");
                    throw e;
                }
            }
            writeline("/home/ubuntu/results/coverage/Copy/Copy_7_10.coverage", "c656190e-29c7-4a7e-a3af-1e1e2bf806ef");
            if (nonFileResources.size() > 0 || singleResource != null) {
                writeline("/home/ubuntu/results/coverage/Copy/Copy_7_10.coverage", "49818386-5c5c-4f89-8eea-21a4e9f53bb1");
                final Resource[] nonFiles = nonFileResources.toArray(new Resource[nonFileResources.size()]);
                writeline("/home/ubuntu/results/coverage/Copy/Copy_7_10.coverage", "babecf8a-8ea6-4d92-8420-0024e584aa79");
                // restrict to out-of-date resources
                final Map<Resource, String[]> map = scan(nonFiles, destDir);
                writeline("/home/ubuntu/results/coverage/Copy/Copy_7_10.coverage", "5ec56bf3-b952-4946-ab09-4fbcecf59054");
                if (singleResource != null) {
                    writeline("/home/ubuntu/results/coverage/Copy/Copy_7_10.coverage", "aa0f6cec-292c-46d4-bba7-d66502a1f670");
                    map.put(singleResource, new String[] { destFile.getAbsolutePath() });
                }
                writeline("/home/ubuntu/results/coverage/Copy/Copy_7_10.coverage", "c3d565f6-35c5-445c-89a7-2003367b4933");
                try {
                    writeline("/home/ubuntu/results/coverage/Copy/Copy_7_10.coverage", "ff664755-5bfd-4dda-bc8c-1708f19fb504");
                    doResourceOperations(map);
                } catch (final BuildException e) {
                    writeline("/home/ubuntu/results/coverage/Copy/Copy_7_10.coverage", "91e34513-3534-4ac5-b400-b7241a8d2275");
                    if (!failonerror) {
                        writeline("/home/ubuntu/results/coverage/Copy/Copy_7_10.coverage", "efffa07e-e55a-4255-a944-b2c4074fa4b8");
                        if (!quiet) {
                            writeline("/home/ubuntu/results/coverage/Copy/Copy_7_10.coverage", "980f4afb-9915-49db-b11f-ccb567f25380");
                            log("Warning: " + getMessage(e), Project.MSG_ERR);
                        }
                    } else {
                        writeline("/home/ubuntu/results/coverage/Copy/Copy_7_10.coverage", "76e45156-c459-41ba-833d-a89341089d87");
                        throw e;
                    }
                }
            }
        } finally {
            writeline("/home/ubuntu/results/coverage/Copy/Copy_7_10.coverage", "15b38e4f-21c6-4aaa-9156-01a8720f6ca1");
            // clean up again, so this instance can be used a second
            // time
            singleResource = null;
            writeline("/home/ubuntu/results/coverage/Copy/Copy_7_10.coverage", "bb449c51-6f9b-4d4e-ae68-a538dcb590a2");
            file = savedFile;
            writeline("/home/ubuntu/results/coverage/Copy/Copy_7_10.coverage", "3e56a55c-793c-4ee8-8f3a-d7688aaebe0c");
            destFile = savedDestFile;
            writeline("/home/ubuntu/results/coverage/Copy/Copy_7_10.coverage", "76cf9f96-6871-4a6e-91c0-8fa552737751");
            destDir = savedDestDir;
            writeline("/home/ubuntu/results/coverage/Copy/Copy_7_10.coverage", "e3d477b6-584f-4875-b9cc-0c24e31c3d7e");
            if (savedRc != null) {
                writeline("/home/ubuntu/results/coverage/Copy/Copy_7_10.coverage", "e9eefab4-1dae-4442-becc-ca6675d35231");
                rcs.insertElementAt(savedRc, 0);
            }
            writeline("/home/ubuntu/results/coverage/Copy/Copy_7_10.coverage", "91553538-afe6-43f5-8bea-50ca64308057");
            fileCopyMap.clear();
            writeline("/home/ubuntu/results/coverage/Copy/Copy_7_10.coverage", "942b4f7a-3089-413d-8fe5-8eaba299bbf9");
            dirCopyMap.clear();
            writeline("/home/ubuntu/results/coverage/Copy/Copy_7_10.coverage", "ff885b2b-205f-4a33-a884-b84001ac3538");
            completeDirMap.clear();
        }
    }

    /**
     * *********************************************************************
     * *  protected and private methods
     * **********************************************************************
     */
    private void copySingleFile() {
        writeline("/home/ubuntu/results/coverage/Copy/Copy_7_10.coverage", "60c95bf4-4586-47ae-92ce-c22ace59e857");
        // deal with the single file
        if (file != null) {
            writeline("/home/ubuntu/results/coverage/Copy/Copy_7_10.coverage", "af4a8928-087b-4620-ab52-87716d2e8ab4");
            if (file.exists()) {
                writeline("/home/ubuntu/results/coverage/Copy/Copy_7_10.coverage", "ecac770b-67e5-4d38-a64b-bc45c13a272e");
                if (destFile == null) {
                    writeline("/home/ubuntu/results/coverage/Copy/Copy_7_10.coverage", "850574dd-eef6-4c1c-89e6-04e21eab68f6");
                    destFile = new File(destDir, file.getName());
                }
                writeline("/home/ubuntu/results/coverage/Copy/Copy_7_10.coverage", "37544283-ba85-4297-8f0a-866a522e215b");
                if (forceOverwrite || !destFile.exists() || (file.lastModified() - granularity > destFile.lastModified())) {
                    writeline("/home/ubuntu/results/coverage/Copy/Copy_7_10.coverage", "9f8aef35-1d05-4a60-b6c0-0eb2279bae6a");
                    fileCopyMap.put(file.getAbsolutePath(), new String[] { destFile.getAbsolutePath() });
                } else {
                    writeline("/home/ubuntu/results/coverage/Copy/Copy_7_10.coverage", "2e9f3dfa-987c-40d4-853b-5e9414404fc7");
                    log(file + " omitted as " + destFile + " is up to date.", Project.MSG_VERBOSE);
                }
            } else {
                writeline("/home/ubuntu/results/coverage/Copy/Copy_7_10.coverage", "cde0ac26-2fb2-419a-9476-6512fa36739e");
                final String message = "Warning: Could not find file " + file.getAbsolutePath() + " to copy.";
                writeline("/home/ubuntu/results/coverage/Copy/Copy_7_10.coverage", "5b9fb2b1-d3e1-4ed7-9674-26b929226402");
                if (!failonerror) {
                    writeline("/home/ubuntu/results/coverage/Copy/Copy_7_10.coverage", "44a1fe7b-1ad8-47d0-a2d5-6e963a3a9304");
                    if (!quiet) {
                        writeline("/home/ubuntu/results/coverage/Copy/Copy_7_10.coverage", "4e1314ef-a7ab-4635-8387-20210039039f");
                        log(message, Project.MSG_ERR);
                    }
                } else {
                    writeline("/home/ubuntu/results/coverage/Copy/Copy_7_10.coverage", "767447b3-b6e7-4513-9b60-eb0ce8e64742");
                    throw new BuildException(message);
                }
            }
        }
    }

    private void iterateOverBaseDirs(final HashSet<File> baseDirs, final HashMap<File, List<String>> dirsByBasedir, final HashMap<File, List<String>> filesByBasedir) {
        writeline("/home/ubuntu/results/coverage/Copy/Copy_7_10.coverage", "ab43d2d8-49eb-4fd5-b5b2-4c313189255e");
        for (final File f : baseDirs) {
            writeline("/home/ubuntu/results/coverage/Copy/Copy_7_10.coverage", "8df4b4e9-b095-4e49-907a-9d219651892e");
            final List<String> files = filesByBasedir.get(f);
            writeline("/home/ubuntu/results/coverage/Copy/Copy_7_10.coverage", "07db3d0b-9b50-4c7e-bf1d-54c27d5b89d1");
            final List<String> dirs = dirsByBasedir.get(f);
            writeline("/home/ubuntu/results/coverage/Copy/Copy_7_10.coverage", "5ce93ab9-d840-4897-bb50-731f7ae2f1eb");
            String[] srcFiles = new String[0];
            writeline("/home/ubuntu/results/coverage/Copy/Copy_7_10.coverage", "e213479d-663e-4cff-9aeb-1138dcceae18");
            if (files != null) {
                writeline("/home/ubuntu/results/coverage/Copy/Copy_7_10.coverage", "c72dec70-2966-4a6b-94df-3b85a418306f");
                srcFiles = files.toArray(srcFiles);
            }
            writeline("/home/ubuntu/results/coverage/Copy/Copy_7_10.coverage", "eefd8d83-49db-4963-af84-81857f2a871a");
            String[] srcDirs = new String[0];
            writeline("/home/ubuntu/results/coverage/Copy/Copy_7_10.coverage", "26c7d27d-31a6-4c32-a087-b0c443d6f360");
            if (dirs != null) {
                writeline("/home/ubuntu/results/coverage/Copy/Copy_7_10.coverage", "5ededfc2-9968-43de-819d-3c90a7623794");
                srcDirs = dirs.toArray(srcDirs);
            }
            writeline("/home/ubuntu/results/coverage/Copy/Copy_7_10.coverage", "a6945ab9-22e4-42e1-9ad8-bdfda46c49e3");
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
        writeline("/home/ubuntu/results/coverage/Copy/Copy_7_10.coverage", "425dad49-a156-4082-ba59-2d6ff757dfc0");
        if (file == null && rcs.size() == 0) {
            writeline("/home/ubuntu/results/coverage/Copy/Copy_7_10.coverage", "8e1654aa-f6ee-4663-a099-b601c079def1");
            throw new BuildException("Specify at least one source--a file or a resource collection.");
        }
        writeline("/home/ubuntu/results/coverage/Copy/Copy_7_10.coverage", "3c1252b4-c9e6-4eee-9411-0d747f55b5bc");
        if (destFile != null && destDir != null) {
            writeline("/home/ubuntu/results/coverage/Copy/Copy_7_10.coverage", "fb06a108-6ddd-4aeb-91f0-31e736cd6b85");
            throw new BuildException("Only one of tofile and todir may be set.");
        }
        writeline("/home/ubuntu/results/coverage/Copy/Copy_7_10.coverage", "b2f35404-9fc2-4ec8-a759-eeea025b3a1c");
        if (destFile == null && destDir == null) {
            writeline("/home/ubuntu/results/coverage/Copy/Copy_7_10.coverage", "b6fa6ca1-c1f4-416c-a9a1-60e033073b6a");
            throw new BuildException("One of tofile or todir must be set.");
        }
        writeline("/home/ubuntu/results/coverage/Copy/Copy_7_10.coverage", "74b29562-f540-4a56-a4ec-6efbb152159a");
        if (file != null && file.isDirectory()) {
            writeline("/home/ubuntu/results/coverage/Copy/Copy_7_10.coverage", "d821dac8-ae3d-4d30-b768-9a8e585f8e57");
            throw new BuildException("Use a resource collection to copy directories.");
        }
        writeline("/home/ubuntu/results/coverage/Copy/Copy_7_10.coverage", "900b5d18-e449-4203-8331-b89caec74e63");
        if (destFile != null && rcs.size() > 0) {
            writeline("/home/ubuntu/results/coverage/Copy/Copy_7_10.coverage", "bbaab4d1-7572-4036-8871-dc091ab3c37f");
            if (rcs.size() > 1) {
                writeline("/home/ubuntu/results/coverage/Copy/Copy_7_10.coverage", "94773162-aee2-4280-81e1-06ae190fe571");
                throw new BuildException("Cannot concatenate multiple files into a single file.");
            } else {
                writeline("/home/ubuntu/results/coverage/Copy/Copy_7_10.coverage", "856acf40-9fd8-4f8f-ad00-c30f2398d214");
                final ResourceCollection rc = rcs.elementAt(0);
                writeline("/home/ubuntu/results/coverage/Copy/Copy_7_10.coverage", "67611563-2e0e-4844-91e6-d640f1021eb4");
                if (!rc.isFilesystemOnly() && !supportsNonFileResources()) {
                    writeline("/home/ubuntu/results/coverage/Copy/Copy_7_10.coverage", "daefc6a2-3025-480e-9481-f877b23c7de2");
                    throw new BuildException("Only FileSystem resources are" + " supported.");
                }
                writeline("/home/ubuntu/results/coverage/Copy/Copy_7_10.coverage", "d4be7cbd-03b8-43a0-b7e4-50b7b830e296");
                if (rc.size() == 0) {
                    writeline("/home/ubuntu/results/coverage/Copy/Copy_7_10.coverage", "d2c8b53a-3084-4358-b8a2-67b1ded2ff88");
                    throw new BuildException(MSG_WHEN_COPYING_EMPTY_RC_TO_FILE);
                } else if (rc.size() == 1) {
                    writeline("/home/ubuntu/results/coverage/Copy/Copy_7_10.coverage", "168416a8-ec3a-42ee-b5ae-2dd6c3ddf132");
                    final Resource res = rc.iterator().next();
                    writeline("/home/ubuntu/results/coverage/Copy/Copy_7_10.coverage", "939f0dfb-4a66-436f-af32-153b1c401c0f");
                    final FileProvider r = res.as(FileProvider.class);
                    writeline("/home/ubuntu/results/coverage/Copy/Copy_7_10.coverage", "f24a761f-0f57-4965-9ba2-30cecb7f048f");
                    if (file == null) {
                        writeline("/home/ubuntu/results/coverage/Copy/Copy_7_10.coverage", "90faaaf1-e2a6-483b-a9d7-3e5bf8834a33");
                        if (r != null) {
                            writeline("/home/ubuntu/results/coverage/Copy/Copy_7_10.coverage", "f48c4996-144c-4d86-ba54-62dc620cb170");
                            file = r.getFile();
                        } else {
                            writeline("/home/ubuntu/results/coverage/Copy/Copy_7_10.coverage", "11ea545e-80b6-47fa-b4bb-b7274290e014");
                            singleResource = res;
                        }
                        writeline("/home/ubuntu/results/coverage/Copy/Copy_7_10.coverage", "59643203-16f7-479c-89ef-f9b6e2fc9855");
                        rcs.removeElementAt(0);
                    } else {
                        writeline("/home/ubuntu/results/coverage/Copy/Copy_7_10.coverage", "3b70482c-45eb-4ae6-a527-6bf9ada90b4d");
                        throw new BuildException("Cannot concatenate multiple files into a single file.");
                    }
                } else {
                    writeline("/home/ubuntu/results/coverage/Copy/Copy_7_10.coverage", "1e27e064-a8cc-4c06-b86e-04dd157a03ca");
                    throw new BuildException("Cannot concatenate multiple files into a single file.");
                }
            }
        }
        writeline("/home/ubuntu/results/coverage/Copy/Copy_7_10.coverage", "6d8d9d5a-4b11-44bd-b21a-200e06dcb06f");
        if (destFile != null) {
            writeline("/home/ubuntu/results/coverage/Copy/Copy_7_10.coverage", "0f1fac0b-209e-430f-8196-386f347b5a36");
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
        writeline("/home/ubuntu/results/coverage/Copy/Copy_7_10.coverage", "eb7d82c8-7860-4ff2-9889-756073c7f9b3");
        final FileNameMapper mapper = getMapper();
        writeline("/home/ubuntu/results/coverage/Copy/Copy_7_10.coverage", "0335c123-87ac-4612-8305-fd8c7a25976a");
        buildMap(fromDir, toDir, files, mapper, fileCopyMap);
        writeline("/home/ubuntu/results/coverage/Copy/Copy_7_10.coverage", "3fabc0b0-2dc7-4001-bb3d-afb4c49afd4f");
        if (includeEmpty) {
            writeline("/home/ubuntu/results/coverage/Copy/Copy_7_10.coverage", "4556d4ac-6d7d-4cd0-bad4-ed08f1472a8e");
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
        writeline("/home/ubuntu/results/coverage/Copy/Copy_7_10.coverage", "abcd47da-d04d-4ca4-b818-5d7ea37cc4a3");
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
        writeline("/home/ubuntu/results/coverage/Copy/Copy_7_10.coverage", "accfa7d9-9b7c-41be-b674-226069a1a4d0");
        String[] toCopy = null;
        writeline("/home/ubuntu/results/coverage/Copy/Copy_7_10.coverage", "f51004dd-6110-476c-943e-4fa91a2da4a7");
        if (forceOverwrite) {
            writeline("/home/ubuntu/results/coverage/Copy/Copy_7_10.coverage", "e3c1d251-96b3-456b-96bb-5b5b1343ce03");
            final Vector<String> v = new Vector<String>();
            writeline("/home/ubuntu/results/coverage/Copy/Copy_7_10.coverage", "a74adf5b-5e04-4c29-a5e6-4244fbe67996");
            for (int i = 0; i < names.length; i++) {
                writeline("/home/ubuntu/results/coverage/Copy/Copy_7_10.coverage", "a4a13041-fe2f-457d-8655-1035a06906b0");
                if (mapper.mapFileName(names[i]) != null) {
                    writeline("/home/ubuntu/results/coverage/Copy/Copy_7_10.coverage", "6424a988-aa69-4612-a815-1bdbbb1b8e61");
                    v.addElement(names[i]);
                }
            }
            writeline("/home/ubuntu/results/coverage/Copy/Copy_7_10.coverage", "b92413cf-ebf0-4fd0-a376-c71811637dd9");
            toCopy = new String[v.size()];
            writeline("/home/ubuntu/results/coverage/Copy/Copy_7_10.coverage", "24ab3e71-1b74-42bf-acfa-0e04d01e1292");
            v.copyInto(toCopy);
        } else {
            writeline("/home/ubuntu/results/coverage/Copy/Copy_7_10.coverage", "3b302a43-40e4-4208-91b4-04d4fd31e109");
            final SourceFileScanner ds = new SourceFileScanner(this);
            writeline("/home/ubuntu/results/coverage/Copy/Copy_7_10.coverage", "d58cfa58-4462-49da-8830-a758e6853b57");
            toCopy = ds.restrict(names, fromDir, toDir, mapper, granularity);
        }
        writeline("/home/ubuntu/results/coverage/Copy/Copy_7_10.coverage", "1b08c2b2-e8f4-4d07-9d15-8dab28088b7d");
        for (int i = 0; i < toCopy.length; i++) {
            writeline("/home/ubuntu/results/coverage/Copy/Copy_7_10.coverage", "5a4354d4-3721-4df8-8a50-e2f9b4a4b1ea");
            final File src = new File(fromDir, toCopy[i]);
            writeline("/home/ubuntu/results/coverage/Copy/Copy_7_10.coverage", "85c9aaba-2b5c-41fc-9a42-ed45bf7cec21");
            final String[] mappedFiles = mapper.mapFileName(toCopy[i]);
            writeline("/home/ubuntu/results/coverage/Copy/Copy_7_10.coverage", "95c2f404-7e89-44ac-85de-fc8eb95e5397");
            if (!enableMultipleMappings) {
                writeline("/home/ubuntu/results/coverage/Copy/Copy_7_10.coverage", "22ac6c1f-de83-49e8-95a9-9e398a2a3fc8");
                map.put(src.getAbsolutePath(), new String[] { new File(toDir, mappedFiles[0]).getAbsolutePath() });
            } else {
                writeline("/home/ubuntu/results/coverage/Copy/Copy_7_10.coverage", "e0edf197-345f-4ffd-9ef5-a60b66663c87");
                // reuse the array created by the mapper
                for (int k = 0; k < mappedFiles.length; k++) {
                    writeline("/home/ubuntu/results/coverage/Copy/Copy_7_10.coverage", "daed0f5e-58d5-41bd-acdd-268de3941b3b");
                    mappedFiles[k] = new File(toDir, mappedFiles[k]).getAbsolutePath();
                }
                writeline("/home/ubuntu/results/coverage/Copy/Copy_7_10.coverage", "45ef954d-fd99-48ad-9364-aad6bf3230bb");
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
        writeline("/home/ubuntu/results/coverage/Copy/Copy_7_10.coverage", "b04748bc-7899-44de-9558-c02c45af4003");
        final HashMap<Resource, String[]> map = new HashMap<Resource, String[]>();
        writeline("/home/ubuntu/results/coverage/Copy/Copy_7_10.coverage", "8545374f-f989-45cf-8f1c-59dffdd2ce24");
        Resource[] toCopy = null;
        writeline("/home/ubuntu/results/coverage/Copy/Copy_7_10.coverage", "69d1f870-f353-4e86-a265-fce692765e6e");
        if (forceOverwrite) {
            writeline("/home/ubuntu/results/coverage/Copy/Copy_7_10.coverage", "b4712622-db69-4233-bd0d-adffd043969d");
            final Vector<Resource> v = new Vector<Resource>();
            writeline("/home/ubuntu/results/coverage/Copy/Copy_7_10.coverage", "1b7d69aa-eeea-43f3-8786-1d59ddaeb2d5");
            for (int i = 0; i < fromResources.length; i++) {
                writeline("/home/ubuntu/results/coverage/Copy/Copy_7_10.coverage", "f24026c4-10d2-494e-b866-97c538423a88");
                if (mapper.mapFileName(fromResources[i].getName()) != null) {
                    writeline("/home/ubuntu/results/coverage/Copy/Copy_7_10.coverage", "e4428505-f587-498d-a596-31accf17bd5b");
                    v.addElement(fromResources[i]);
                }
            }
            writeline("/home/ubuntu/results/coverage/Copy/Copy_7_10.coverage", "bdac414d-7cf4-4b36-8398-477d4093bd1d");
            toCopy = new Resource[v.size()];
            writeline("/home/ubuntu/results/coverage/Copy/Copy_7_10.coverage", "f6c52e03-86bb-4ae9-aed6-b2eae5d41e1e");
            v.copyInto(toCopy);
        } else {
            writeline("/home/ubuntu/results/coverage/Copy/Copy_7_10.coverage", "ae6de424-9978-4214-b0b1-0d02d4dc24f1");
            toCopy = ResourceUtils.selectOutOfDateSources(this, fromResources, mapper, new ResourceFactory() {

                public Resource getResource(final String name) {
                    return new FileResource(toDir, name);
                }
            }, granularity);
        }
        writeline("/home/ubuntu/results/coverage/Copy/Copy_7_10.coverage", "22dba300-3351-40e7-902f-52860a49d520");
        for (int i = 0; i < toCopy.length; i++) {
            writeline("/home/ubuntu/results/coverage/Copy/Copy_7_10.coverage", "11edd11a-3bca-4d8b-8afa-c1b3f0aac337");
            final String[] mappedFiles = mapper.mapFileName(toCopy[i].getName());
            writeline("/home/ubuntu/results/coverage/Copy/Copy_7_10.coverage", "e5583ecd-2ba9-47c9-842e-0b9416feaa7a");
            for (int j = 0; j < mappedFiles.length; j++) {
                writeline("/home/ubuntu/results/coverage/Copy/Copy_7_10.coverage", "0666ea26-e6fb-4d2c-be58-25256cb4936b");
                if (mappedFiles[j] == null) {
                    writeline("/home/ubuntu/results/coverage/Copy/Copy_7_10.coverage", "7e4bb5af-dc04-454e-87c0-6a9583d5b868");
                    throw new BuildException("Can't copy a resource without a" + " name if the mapper doesn't" + " provide one.");
                }
            }
            writeline("/home/ubuntu/results/coverage/Copy/Copy_7_10.coverage", "a6720f4c-f9ae-44ad-8ab9-70bb08c931c0");
            if (!enableMultipleMappings) {
                writeline("/home/ubuntu/results/coverage/Copy/Copy_7_10.coverage", "c298f5f9-4b97-4a72-af75-d852d085cf19");
                map.put(toCopy[i], new String[] { new File(toDir, mappedFiles[0]).getAbsolutePath() });
            } else {
                writeline("/home/ubuntu/results/coverage/Copy/Copy_7_10.coverage", "32253692-99e0-49d8-949e-c1642c71689e");
                // reuse the array created by the mapper
                for (int k = 0; k < mappedFiles.length; k++) {
                    writeline("/home/ubuntu/results/coverage/Copy/Copy_7_10.coverage", "fc78b102-fc37-492f-b84a-1247c81cec90");
                    mappedFiles[k] = new File(toDir, mappedFiles[k]).getAbsolutePath();
                }
                writeline("/home/ubuntu/results/coverage/Copy/Copy_7_10.coverage", "ea8fb622-534f-4423-a813-de52dae79ad1");
                map.put(toCopy[i], mappedFiles);
            }
        }
        writeline("/home/ubuntu/results/coverage/Copy/Copy_7_10.coverage", "eb74f890-b8d2-44fd-ac55-1f5bb38f8dbb");
        return map;
    }

    /**
     * Actually does the file (and possibly empty directory) copies.
     * This is a good method for subclasses to override.
     */
    protected void doFileOperations() {
        writeline("/home/ubuntu/results/coverage/Copy/Copy_7_10.coverage", "d5d9931b-70a7-416b-ad2d-6a6bdba8740e");
        if (fileCopyMap.size() > 0) {
            writeline("/home/ubuntu/results/coverage/Copy/Copy_7_10.coverage", "d3cdeddd-098a-4f2b-be65-22d9b0a3b462");
            log("Copying " + fileCopyMap.size() + " file" + (fileCopyMap.size() == 1 ? "" : "s") + " to " + destDir.getAbsolutePath());
            writeline("/home/ubuntu/results/coverage/Copy/Copy_7_10.coverage", "f9e02cad-b656-4094-a325-3408326668e3");
            for (final Map.Entry<String, String[]> e : fileCopyMap.entrySet()) {
                writeline("/home/ubuntu/results/coverage/Copy/Copy_7_10.coverage", "864f6234-edf3-4cdc-b513-27c3aac856f2");
                final String fromFile = e.getKey();
                writeline("/home/ubuntu/results/coverage/Copy/Copy_7_10.coverage", "7421cdf6-a816-4204-8eed-ffb48b8d249f");
                final String[] toFiles = e.getValue();
                writeline("/home/ubuntu/results/coverage/Copy/Copy_7_10.coverage", "607ae917-3bf8-4ba2-b910-5df05ed98535");
                for (int i = 0; i < toFiles.length; i++) {
                    writeline("/home/ubuntu/results/coverage/Copy/Copy_7_10.coverage", "ab45b47b-23fb-4ab1-8ad6-c8ea5d184271");
                    final String toFile = toFiles[i];
                    writeline("/home/ubuntu/results/coverage/Copy/Copy_7_10.coverage", "1069e758-946c-43ed-880a-49445ba881f3");
                    if (fromFile.equals(toFile)) {
                        writeline("/home/ubuntu/results/coverage/Copy/Copy_7_10.coverage", "b721669d-e45b-426b-a6d7-d32b6b801716");
                        log("Skipping self-copy of " + fromFile, verbosity);
                        writeline("/home/ubuntu/results/coverage/Copy/Copy_7_10.coverage", "0297d0e9-2170-4c8c-8c33-b2e8da37a8a7");
                        continue;
                    }
                    writeline("/home/ubuntu/results/coverage/Copy/Copy_7_10.coverage", "da706c29-a47c-4461-b1fa-f98ac913b549");
                    try {
                        writeline("/home/ubuntu/results/coverage/Copy/Copy_7_10.coverage", "f02b2171-06ee-4500-a2af-2a894d143c4d");
                        log("Copying " + fromFile + " to " + toFile, verbosity);
                        writeline("/home/ubuntu/results/coverage/Copy/Copy_7_10.coverage", "78ec82ae-9619-48a7-b810-d7eef33834f2");
                        final FilterSetCollection executionFilters = new FilterSetCollection();
                        writeline("/home/ubuntu/results/coverage/Copy/Copy_7_10.coverage", "a9103215-9fe3-4c3d-8ba9-b841393ad57c");
                        if (filtering) {
                            writeline("/home/ubuntu/results/coverage/Copy/Copy_7_10.coverage", "61a9eace-73be-4d51-959b-dd2b5387a283");
                            executionFilters.addFilterSet(getProject().getGlobalFilterSet());
                        }
                        writeline("/home/ubuntu/results/coverage/Copy/Copy_7_10.coverage", "f523cf5e-58e3-4405-bc13-94dd72c3f5d4");
                        for (final FilterSet filterSet : filterSets) {
                            writeline("/home/ubuntu/results/coverage/Copy/Copy_7_10.coverage", "409fc9a5-95a2-4cc4-baed-de650184c332");
                            executionFilters.addFilterSet(filterSet);
                        }
                        writeline("/home/ubuntu/results/coverage/Copy/Copy_7_10.coverage", "74631aff-ef05-48ff-8976-b780c22c0268");
                        fileUtils.copyFile(new File(fromFile), new File(toFile), executionFilters, filterChains, forceOverwrite, preserveLastModified, /* append: */
                        false, inputEncoding, outputEncoding, getProject(), getForce());
                    } catch (final IOException ioe) {
                        writeline("/home/ubuntu/results/coverage/Copy/Copy_7_10.coverage", "cad4e537-cc1a-4a93-8b0c-a41ff0a92f0c");
                        String msg = "Failed to copy " + fromFile + " to " + toFile + " due to " + getDueTo(ioe);
                        writeline("/home/ubuntu/results/coverage/Copy/Copy_7_10.coverage", "1ab5c32f-798e-4778-91df-ef6a5bfc6e2c");
                        final File targetFile = new File(toFile);
                        writeline("/home/ubuntu/results/coverage/Copy/Copy_7_10.coverage", "ad177457-cd31-4411-8de2-0a7474dc1331");
                        if (!(ioe instanceof ResourceUtils.ReadOnlyTargetFileException) && targetFile.exists() && !targetFile.delete()) {
                            writeline("/home/ubuntu/results/coverage/Copy/Copy_7_10.coverage", "4df6dc3e-674a-4adb-b015-2518f3d3213d");
                            msg += " and I couldn't delete the corrupt " + toFile;
                        }
                        writeline("/home/ubuntu/results/coverage/Copy/Copy_7_10.coverage", "601de604-8daa-4dec-9f1b-0d66a3ca6956");
                        if (failonerror) {
                            writeline("/home/ubuntu/results/coverage/Copy/Copy_7_10.coverage", "6facef0f-c3a0-4161-8c00-a423f806bd5e");
                            throw new BuildException(msg, ioe, getLocation());
                        }
                        writeline("/home/ubuntu/results/coverage/Copy/Copy_7_10.coverage", "1b3b87ed-e0d7-47c1-a9a9-441c5243c29e");
                        log(msg, Project.MSG_ERR);
                    }
                }
            }
        }
        writeline("/home/ubuntu/results/coverage/Copy/Copy_7_10.coverage", "19ab6612-3ff9-440d-a4b9-b7dba426cdcc");
        if (includeEmpty) {
            writeline("/home/ubuntu/results/coverage/Copy/Copy_7_10.coverage", "817f1140-676e-4ab5-ae0a-cf829d16db48");
            int createCount = 0;
            writeline("/home/ubuntu/results/coverage/Copy/Copy_7_10.coverage", "58b3f9e2-c01d-402d-b586-3e88735a06b2");
            for (final String[] dirs : dirCopyMap.values()) {
                writeline("/home/ubuntu/results/coverage/Copy/Copy_7_10.coverage", "c6244f39-e05e-4f05-8ca0-6e0dec2228b8");
                for (int i = 0; i < dirs.length; i++) {
                    writeline("/home/ubuntu/results/coverage/Copy/Copy_7_10.coverage", "910b66b3-d761-40d3-aa62-f56ca2fe1d9b");
                    final File d = new File(dirs[i]);
                    writeline("/home/ubuntu/results/coverage/Copy/Copy_7_10.coverage", "8a27d846-8581-4104-a5b9-acd22a37d5e7");
                    if (!d.exists()) {
                        writeline("/home/ubuntu/results/coverage/Copy/Copy_7_10.coverage", "3344ffa8-92d9-4c99-9bd5-6e1745333adf");
                        if (!(d.mkdirs() || d.isDirectory())) {
                            writeline("/home/ubuntu/results/coverage/Copy/Copy_7_10.coverage", "d3441fac-002c-41d3-b101-3704ad12313c");
                            log("Unable to create directory " + d.getAbsolutePath(), Project.MSG_ERR);
                        } else {
                            writeline("/home/ubuntu/results/coverage/Copy/Copy_7_10.coverage", "2829e669-eba6-4320-89be-ce4e6beedb6b");
                            createCount++;
                        }
                    }
                }
            }
            writeline("/home/ubuntu/results/coverage/Copy/Copy_7_10.coverage", "434aba60-2988-451d-986f-acb9e34e5e80");
            if (createCount > 0) {
                writeline("/home/ubuntu/results/coverage/Copy/Copy_7_10.coverage", "ddd23b8e-1cec-4bb0-83f4-4da4ffa63368");
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
        writeline("/home/ubuntu/results/coverage/Copy/Copy_7_10.coverage", "cfde14c7-652b-4c3b-8c36-2ba691336756");
        if (map.size() > 0) {
            writeline("/home/ubuntu/results/coverage/Copy/Copy_7_10.coverage", "ef26cffb-47b5-4baf-a239-868696797332");
            log("Copying " + map.size() + " resource" + (map.size() == 1 ? "" : "s") + " to " + destDir.getAbsolutePath());
            writeline("/home/ubuntu/results/coverage/Copy/Copy_7_10.coverage", "076fa884-3d1d-46c6-9c4d-e5639d3b8b8c");
            for (final Map.Entry<Resource, String[]> e : map.entrySet()) {
                writeline("/home/ubuntu/results/coverage/Copy/Copy_7_10.coverage", "3c416749-6079-45b8-8f98-5ed4fcc76c39");
                final Resource fromResource = e.getKey();
                writeline("/home/ubuntu/results/coverage/Copy/Copy_7_10.coverage", "af6a9fa4-24a7-4268-b342-4c08c0c27744");
                for (final String toFile : e.getValue()) {
                    writeline("/home/ubuntu/results/coverage/Copy/Copy_7_10.coverage", "594a00e7-4bf7-4487-a904-9b7540e359de");
                    try {
                        writeline("/home/ubuntu/results/coverage/Copy/Copy_7_10.coverage", "4f41605a-f6d7-44b2-b630-17d29719c5ea");
                        log("Copying " + fromResource + " to " + toFile, verbosity);
                        writeline("/home/ubuntu/results/coverage/Copy/Copy_7_10.coverage", "423c4ff7-4144-450d-b693-7c4d64492452");
                        final FilterSetCollection executionFilters = new FilterSetCollection();
                        writeline("/home/ubuntu/results/coverage/Copy/Copy_7_10.coverage", "dc342fff-19fc-4d53-8821-0d517eec857f");
                        if (filtering) {
                            writeline("/home/ubuntu/results/coverage/Copy/Copy_7_10.coverage", "acd8fafb-3462-4889-8014-dbabad69c0eb");
                            executionFilters.addFilterSet(getProject().getGlobalFilterSet());
                        }
                        writeline("/home/ubuntu/results/coverage/Copy/Copy_7_10.coverage", "f1cc5662-49ef-419a-bdef-0d3acc829616");
                        for (final FilterSet filterSet : filterSets) {
                            writeline("/home/ubuntu/results/coverage/Copy/Copy_7_10.coverage", "f986bb25-1a18-41f0-ad62-34e9334adc5e");
                            executionFilters.addFilterSet(filterSet);
                        }
                        writeline("/home/ubuntu/results/coverage/Copy/Copy_7_10.coverage", "8344507d-87a0-461c-8a8d-67afb8b35fdf");
                        ResourceUtils.copyResource(fromResource, new FileResource(destDir, toFile), executionFilters, filterChains, forceOverwrite, preserveLastModified, /* append: */
                        false, inputEncoding, outputEncoding, getProject(), getForce());
                    } catch (final IOException ioe) {
                        writeline("/home/ubuntu/results/coverage/Copy/Copy_7_10.coverage", "e05bbe4a-692d-4e4a-80a7-629eb38fcaa3");
                        String msg = "Failed to copy " + fromResource + " to " + toFile + " due to " + getDueTo(ioe);
                        writeline("/home/ubuntu/results/coverage/Copy/Copy_7_10.coverage", "5ccdadea-a608-4e16-9211-6e0dbdd9ae00");
                        final File targetFile = new File(toFile);
                        writeline("/home/ubuntu/results/coverage/Copy/Copy_7_10.coverage", "949aebba-ab76-4665-8f07-7dde15c22ba4");
                        if (!(ioe instanceof ResourceUtils.ReadOnlyTargetFileException) && targetFile.exists() && !targetFile.delete()) {
                            writeline("/home/ubuntu/results/coverage/Copy/Copy_7_10.coverage", "134e39a9-d052-4b71-8247-adc06e416c04");
                            msg += " and I couldn't delete the corrupt " + toFile;
                        }
                        writeline("/home/ubuntu/results/coverage/Copy/Copy_7_10.coverage", "9a492b8b-c619-45e0-bf3e-aa3f6b520ee9");
                        if (failonerror) {
                            writeline("/home/ubuntu/results/coverage/Copy/Copy_7_10.coverage", "224ba112-e286-4494-9fd8-a2cdaaddda43");
                            throw new BuildException(msg, ioe, getLocation());
                        }
                        writeline("/home/ubuntu/results/coverage/Copy/Copy_7_10.coverage", "43238c2f-6a07-4236-a149-bf98058952d8");
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
        writeline("/home/ubuntu/results/coverage/Copy/Copy_7_10.coverage", "ceff6388-3f10-445d-9a87-23775cfd55a0");
        return getClass().equals(Copy.class);
    }

    /**
     * Adds the given strings to a list contained in the given map.
     * The file is the key into the map.
     */
    private static void add(File baseDir, final String[] names, final Map<File, List<String>> m) {
        writelineStatic("/home/ubuntu/results/coverage/Copy/Copy_7_10.coverage", "7f5f68dc-6dce-49a2-bef0-5b9813abe72b");
        if (names != null) {
            writelineStatic("/home/ubuntu/results/coverage/Copy/Copy_7_10.coverage", "565c46b8-bb62-497f-a183-ceb351712c4f");
            baseDir = getKeyFile(baseDir);
            writelineStatic("/home/ubuntu/results/coverage/Copy/Copy_7_10.coverage", "37491435-4d3f-41b9-a3a3-c92f4ce65f4b");
            List<String> l = m.get(baseDir);
            writelineStatic("/home/ubuntu/results/coverage/Copy/Copy_7_10.coverage", "a274e71c-13f1-4314-a469-0a5ed37b2c74");
            if (l == null) {
                writelineStatic("/home/ubuntu/results/coverage/Copy/Copy_7_10.coverage", "f8468577-25b1-4de0-b514-8c0643085f91");
                l = new ArrayList<String>(names.length);
                writelineStatic("/home/ubuntu/results/coverage/Copy/Copy_7_10.coverage", "78a71d2c-dbc5-4964-a955-9fc9d0f3e713");
                m.put(baseDir, l);
            }
            writelineStatic("/home/ubuntu/results/coverage/Copy/Copy_7_10.coverage", "eafec2ba-7482-4282-b65d-f264376def8c");
            l.addAll(java.util.Arrays.asList(names));
        }
    }

    /**
     * Adds the given string to a list contained in the given map.
     * The file is the key into the map.
     */
    private static void add(final File baseDir, final String name, final Map<File, List<String>> m) {
        writelineStatic("/home/ubuntu/results/coverage/Copy/Copy_7_10.coverage", "2e107996-6d45-452e-b0e5-6135204433b0");
        if (name != null) {
            writelineStatic("/home/ubuntu/results/coverage/Copy/Copy_7_10.coverage", "8d4038f0-11c9-44e5-a494-7d9bdc5cb9d0");
            add(baseDir, new String[] { name }, m);
        }
    }

    /**
     * Either returns its argument or a plaeholder if the argument is null.
     */
    private static File getKeyFile(final File f) {
        writelineStatic("/home/ubuntu/results/coverage/Copy/Copy_7_10.coverage", "9b69353c-6fce-44ec-8322-946db0917620");
        return f == null ? NULL_FILE_PLACEHOLDER : f;
    }

    /**
     * returns the mapper to use based on nested elements or the
     * flatten attribute.
     */
    private FileNameMapper getMapper() {
        writeline("/home/ubuntu/results/coverage/Copy/Copy_7_10.coverage", "02325b2b-0460-4059-a9f7-f771cc5f15e0");
        FileNameMapper mapper = null;
        writeline("/home/ubuntu/results/coverage/Copy/Copy_7_10.coverage", "a9862a2e-1688-4a3a-ae63-ecb2c2d5ad57");
        if (mapperElement != null) {
            writeline("/home/ubuntu/results/coverage/Copy/Copy_7_10.coverage", "4539e3ce-f8fd-47a0-a3ee-9f8b039c71f8");
            mapper = mapperElement.getImplementation();
        } else if (flatten) {
            writeline("/home/ubuntu/results/coverage/Copy/Copy_7_10.coverage", "e31f18c4-e5c3-43ac-85d7-5c6ccf071b3b");
            mapper = new FlatFileNameMapper();
        } else {
            writeline("/home/ubuntu/results/coverage/Copy/Copy_7_10.coverage", "03cdcbf3-617f-4a20-8169-0c97476ae9ca");
            mapper = new IdentityMapper();
        }
        writeline("/home/ubuntu/results/coverage/Copy/Copy_7_10.coverage", "d68b0296-e096-49b4-befb-60c7f4669543");
        return mapper;
    }

    /**
     * Handle getMessage() for exceptions.
     * @param ex the exception to handle
     * @return ex.getMessage() if ex.getMessage() is not null
     * otherwise return ex.toString()
     */
    private String getMessage(final Exception ex) {
        writeline("/home/ubuntu/results/coverage/Copy/Copy_7_10.coverage", "768dea4a-0e0c-4217-b723-334c8c77c0d0");
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
        writeline("/home/ubuntu/results/coverage/Copy/Copy_7_10.coverage", "0be1dc96-e8e6-4c9d-98a1-ee5450215bda");
        final boolean baseIOException = ex.getClass() == IOException.class;
        writeline("/home/ubuntu/results/coverage/Copy/Copy_7_10.coverage", "3feed12e-7384-4bb4-98cd-33e1b678eaa4");
        final StringBuffer message = new StringBuffer();
        writeline("/home/ubuntu/results/coverage/Copy/Copy_7_10.coverage", "8e138882-7d76-4225-bd87-c7faaff1baa4");
        if (!baseIOException || ex.getMessage() == null) {
            writeline("/home/ubuntu/results/coverage/Copy/Copy_7_10.coverage", "8c8a13e4-d216-4a53-89a2-b0c96c781074");
            message.append(ex.getClass().getName());
        }
        writeline("/home/ubuntu/results/coverage/Copy/Copy_7_10.coverage", "716c02d3-3fbc-4d0c-a785-9baad7584d1e");
        if (ex.getMessage() != null) {
            writeline("/home/ubuntu/results/coverage/Copy/Copy_7_10.coverage", "2a9053c3-1079-4c3c-9bd4-fff8e79f742a");
            if (!baseIOException) {
                writeline("/home/ubuntu/results/coverage/Copy/Copy_7_10.coverage", "e5a8c7d8-ff60-41c9-97f5-bdcc96738b41");
                message.append(" ");
            }
            writeline("/home/ubuntu/results/coverage/Copy/Copy_7_10.coverage", "7f9592df-167e-4851-b6b1-29cca35292f9");
            message.append(ex.getMessage());
        }
        writeline("/home/ubuntu/results/coverage/Copy/Copy_7_10.coverage", "26073f12-0ef1-4d20-950a-665a1ced810c");
        if (ex.getClass().getName().indexOf("MalformedInput") != -1) {
            writeline("/home/ubuntu/results/coverage/Copy/Copy_7_10.coverage", "41af3445-9f30-41ba-abad-cdab24d1f6ef");
            message.append(LINE_SEPARATOR);
            writeline("/home/ubuntu/results/coverage/Copy/Copy_7_10.coverage", "3fcfdfe8-b610-4076-9581-48917194486d");
            message.append("This is normally due to the input file containing invalid");
            writeline("/home/ubuntu/results/coverage/Copy/Copy_7_10.coverage", "ef46442d-a9f7-4da9-8096-0138ea866a5f");
            message.append(LINE_SEPARATOR);
            writeline("/home/ubuntu/results/coverage/Copy/Copy_7_10.coverage", "f3b25c86-2594-4773-95d1-c8a841103378");
            message.append("bytes for the character encoding used : ");
            writeline("/home/ubuntu/results/coverage/Copy/Copy_7_10.coverage", "d821fb2c-b841-427d-bab3-de937cb590a2");
            message.append((inputEncoding == null ? fileUtils.getDefaultEncoding() : inputEncoding));
            writeline("/home/ubuntu/results/coverage/Copy/Copy_7_10.coverage", "d04fcb24-38ec-40e6-8e1f-4185960af3be");
            message.append(LINE_SEPARATOR);
        }
        writeline("/home/ubuntu/results/coverage/Copy/Copy_7_10.coverage", "6ab07cb4-c2d1-4928-971b-1520b15d009c");
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
