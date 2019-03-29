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
        writeline("/home/ubuntu/results/coverage/Copy/Copy_10_10.coverage", "5b9d8d83-1fab-4158-8816-4fcf053d49f5");
        return fileUtils;
    }

    /**
     * Set a single source file to copy.
     * @param file the file to copy.
     */
    public void setFile(final File file) {
        writeline("/home/ubuntu/results/coverage/Copy/Copy_10_10.coverage", "d95c64ba-066d-4d36-847f-57bc9a662c6b");
        this.file = file;
    }

    /**
     * Set the destination file.
     * @param destFile the file to copy to.
     */
    public void setTofile(final File destFile) {
        writeline("/home/ubuntu/results/coverage/Copy/Copy_10_10.coverage", "34400f92-2934-4fd6-851f-cd7c6e058edf");
        this.destFile = destFile;
    }

    /**
     * Set the destination directory.
     * @param destDir the destination directory.
     */
    public void setTodir(final File destDir) {
        writeline("/home/ubuntu/results/coverage/Copy/Copy_10_10.coverage", "d2d28dd3-3de1-49e0-8b71-c4ffa8b0865b");
        this.destDir = destDir;
    }

    /**
     * Add a FilterChain.
     * @return a filter chain object.
     */
    public FilterChain createFilterChain() {
        writeline("/home/ubuntu/results/coverage/Copy/Copy_10_10.coverage", "3e92f109-ca40-4998-b5f8-e3d3f9f533a0");
        final FilterChain filterChain = new FilterChain();
        writeline("/home/ubuntu/results/coverage/Copy/Copy_10_10.coverage", "6d811b43-d07a-4761-a32d-800c18fee917");
        filterChains.addElement(filterChain);
        writeline("/home/ubuntu/results/coverage/Copy/Copy_10_10.coverage", "6d9f40dd-ffaf-4ec2-90cb-bf01f3025b59");
        return filterChain;
    }

    /**
     * Add a filterset.
     * @return a filter set object.
     */
    public FilterSet createFilterSet() {
        writeline("/home/ubuntu/results/coverage/Copy/Copy_10_10.coverage", "d1c7e20b-ddd1-4d43-bb4b-aecd467fb3cc");
        final FilterSet filterSet = new FilterSet();
        writeline("/home/ubuntu/results/coverage/Copy/Copy_10_10.coverage", "f3e04177-48e7-470a-baf9-ab3cc34398a5");
        filterSets.addElement(filterSet);
        writeline("/home/ubuntu/results/coverage/Copy/Copy_10_10.coverage", "7471f684-7099-4e43-8bf0-4ad7075658e8");
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
        writeline("/home/ubuntu/results/coverage/Copy/Copy_10_10.coverage", "e79d1fd4-7019-48ac-83cb-a9227e73cc09");
        setPreserveLastModified(Project.toBoolean(preserve));
    }

    /**
     * Give the copied files the same last modified time as the original files.
     * @param preserve if true preserve the modified time; default is false.
     */
    public void setPreserveLastModified(final boolean preserve) {
        writeline("/home/ubuntu/results/coverage/Copy/Copy_10_10.coverage", "5340d9ef-509b-4b96-bdae-de39fd5e95cf");
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
        writeline("/home/ubuntu/results/coverage/Copy/Copy_10_10.coverage", "884ca4cd-1cca-4b6e-ae3d-eb41dbe423ed");
        return preserveLastModified;
    }

    /**
     * Get the filtersets being applied to this operation.
     *
     * @return a vector of FilterSet objects.
     */
    protected Vector<FilterSet> getFilterSets() {
        writeline("/home/ubuntu/results/coverage/Copy/Copy_10_10.coverage", "6883e31c-8c32-49ed-8e2b-b5e25ca957d1");
        return filterSets;
    }

    /**
     * Get the filterchains being applied to this operation.
     *
     * @return a vector of FilterChain objects.
     */
    protected Vector<FilterChain> getFilterChains() {
        writeline("/home/ubuntu/results/coverage/Copy/Copy_10_10.coverage", "9535cfc9-e6eb-4ba3-afef-556dc35d80fc");
        return filterChains;
    }

    /**
     * Set filtering mode.
     * @param filtering if true enable filtering; default is false.
     */
    public void setFiltering(final boolean filtering) {
        writeline("/home/ubuntu/results/coverage/Copy/Copy_10_10.coverage", "c450dc7a-4bf0-4f93-b30e-b2cadd5c2890");
        this.filtering = filtering;
    }

    /**
     * Set overwrite mode regarding existing destination file(s).
     * @param overwrite if true force overwriting of destination file(s)
     * even if the destination file(s) are younger than
     * the corresponding source file. Default is false.
     */
    public void setOverwrite(final boolean overwrite) {
        writeline("/home/ubuntu/results/coverage/Copy/Copy_10_10.coverage", "a48708bf-9b92-4cc8-8e50-846d85e7c7ad");
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
        writeline("/home/ubuntu/results/coverage/Copy/Copy_10_10.coverage", "7b69aaf8-f7a7-417b-bce1-285c57635435");
        force = f;
    }

    /**
     * Whether read-only destinations will be overwritten.
     *
     * @since Ant 1.8.2
     */
    public boolean getForce() {
        writeline("/home/ubuntu/results/coverage/Copy/Copy_10_10.coverage", "0356500e-7298-4752-bb6b-bc3cd7640cc0");
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
        writeline("/home/ubuntu/results/coverage/Copy/Copy_10_10.coverage", "d026222d-a4ef-403c-a6de-13a1c0041308");
        this.flatten = flatten;
    }

    /**
     * Set verbose mode. Used to force listing of all names of copied files.
     * @param verbose whether to output the names of copied files.
     * Default is false.
     */
    public void setVerbose(final boolean verbose) {
        writeline("/home/ubuntu/results/coverage/Copy/Copy_10_10.coverage", "3a41df99-780d-4213-beb4-6d044ee39612");
        this.verbosity = verbose ? Project.MSG_INFO : Project.MSG_VERBOSE;
    }

    /**
     * Set whether to copy empty directories.
     * @param includeEmpty if true copy empty directories. Default is true.
     */
    public void setIncludeEmptyDirs(final boolean includeEmpty) {
        writeline("/home/ubuntu/results/coverage/Copy/Copy_10_10.coverage", "e5677a6a-c74e-41af-a561-0c19fa5bbd66");
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
        writeline("/home/ubuntu/results/coverage/Copy/Copy_10_10.coverage", "ffd0577a-a1da-4131-9b1f-9d02e819a994");
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
        writeline("/home/ubuntu/results/coverage/Copy/Copy_10_10.coverage", "a273e2cf-a85d-412b-b3d9-8db61e10498b");
        this.enableMultipleMappings = enableMultipleMappings;
    }

    /**
     * Get whether multiple mapping is enabled.
     * @return true if multiple mapping is enabled; false otherwise.
     */
    public boolean isEnableMultipleMapping() {
        writeline("/home/ubuntu/results/coverage/Copy/Copy_10_10.coverage", "376d397f-6fe1-49dc-827d-9b92600fcd8e");
        return enableMultipleMappings;
    }

    /**
     * Set whether to fail when errors are encountered. If false, note errors
     * to the output but keep going. Default is true.
     * @param failonerror true or false.
     */
    public void setFailOnError(final boolean failonerror) {
        writeline("/home/ubuntu/results/coverage/Copy/Copy_10_10.coverage", "6fd78207-b61d-40a3-8a55-e5dfef057e28");
        this.failonerror = failonerror;
    }

    /**
     * Add a set of files to copy.
     * @param set a set of files to copy.
     */
    public void addFileset(final FileSet set) {
        writeline("/home/ubuntu/results/coverage/Copy/Copy_10_10.coverage", "0c413d45-22c5-49cd-a5cc-842d00ec33f7");
        add(set);
    }

    /**
     * Add a collection of files to copy.
     * @param res a resource collection to copy.
     * @since Ant 1.7
     */
    public void add(final ResourceCollection res) {
        writeline("/home/ubuntu/results/coverage/Copy/Copy_10_10.coverage", "c3932a4d-1907-4319-b98f-886c3189baf0");
        rcs.add(res);
    }

    /**
     * Define the mapper to map source to destination files.
     * @return a mapper to be configured.
     * @exception BuildException if more than one mapper is defined.
     */
    public Mapper createMapper() throws BuildException {
        writeline("/home/ubuntu/results/coverage/Copy/Copy_10_10.coverage", "f555f263-4691-4b0b-8097-40cbc8f4ad6e");
        if (mapperElement != null) {
            writeline("/home/ubuntu/results/coverage/Copy/Copy_10_10.coverage", "20554ce9-4df1-41ae-92c7-886c5cf12934");
            throw new BuildException("Cannot define more than one mapper", getLocation());
        }
        writeline("/home/ubuntu/results/coverage/Copy/Copy_10_10.coverage", "8cd0eca6-e5eb-4273-8257-aeb2aa35d486");
        mapperElement = new Mapper(getProject());
        writeline("/home/ubuntu/results/coverage/Copy/Copy_10_10.coverage", "22692b69-ab02-456b-94fb-fb3158c7a95d");
        return mapperElement;
    }

    /**
     * Add a nested filenamemapper.
     * @param fileNameMapper the mapper to add.
     * @since Ant 1.6.3
     */
    public void add(final FileNameMapper fileNameMapper) {
        writeline("/home/ubuntu/results/coverage/Copy/Copy_10_10.coverage", "3b26719e-7c7a-48a2-bf17-d1b9f36fe733");
        createMapper().add(fileNameMapper);
    }

    /**
     * Set the character encoding.
     * @param encoding the character encoding.
     * @since 1.32, Ant 1.5
     */
    public void setEncoding(final String encoding) {
        writeline("/home/ubuntu/results/coverage/Copy/Copy_10_10.coverage", "9e6f9b12-950c-4b71-a8e3-74b80efaf8cb");
        this.inputEncoding = encoding;
        writeline("/home/ubuntu/results/coverage/Copy/Copy_10_10.coverage", "6ccea1e3-5b17-485c-92f3-cd7f7cae3bf8");
        if (outputEncoding == null) {
            writeline("/home/ubuntu/results/coverage/Copy/Copy_10_10.coverage", "393e7741-7405-4543-9547-eacc1f6dc7bc");
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
        writeline("/home/ubuntu/results/coverage/Copy/Copy_10_10.coverage", "5a51f44d-8aba-44d1-b528-61bd95efa343");
        return inputEncoding;
    }

    /**
     * Set the character encoding for output files.
     * @param encoding the output character encoding.
     * @since Ant 1.6
     */
    public void setOutputEncoding(final String encoding) {
        writeline("/home/ubuntu/results/coverage/Copy/Copy_10_10.coverage", "c8066748-6748-4b02-8c21-825ba25ac8c5");
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
        writeline("/home/ubuntu/results/coverage/Copy/Copy_10_10.coverage", "252f2d1d-cacd-4db4-a958-fd125fc86234");
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
        writeline("/home/ubuntu/results/coverage/Copy/Copy_10_10.coverage", "16198e26-d06b-481a-a934-99182b13641c");
        this.granularity = granularity;
    }

    /**
     * Perform the copy operation.
     * @exception BuildException if an error occurs.
     */
    @Override
    public void execute() throws BuildException {
        writeline("/home/ubuntu/results/coverage/Copy/Copy_10_10.coverage", "840c77ec-5953-4b45-b9a4-316b007d4aad");
        // may be altered in validateAttributes
        final File savedFile = file;
        writeline("/home/ubuntu/results/coverage/Copy/Copy_10_10.coverage", "b99337d0-195b-4279-92db-0a779d0e964b");
        final File savedDestFile = destFile;
        writeline("/home/ubuntu/results/coverage/Copy/Copy_10_10.coverage", "3a31f2b6-1d21-42ac-a80e-db13cb55c738");
        final File savedDestDir = destDir;
        writeline("/home/ubuntu/results/coverage/Copy/Copy_10_10.coverage", "3f8b6689-fc8c-4c63-9e08-f5b61f395217");
        ResourceCollection savedRc = null;
        writeline("/home/ubuntu/results/coverage/Copy/Copy_10_10.coverage", "a81fcd63-2d9f-45d1-a5be-5ff5f5b35c2d");
        if (file == null && destFile != null && rcs.size() == 1) {
            writeline("/home/ubuntu/results/coverage/Copy/Copy_10_10.coverage", "2421b324-6413-4e00-8f9a-35a9f1a6a6a0");
            // will be removed in validateAttributes
            savedRc = rcs.elementAt(0);
        }
        writeline("/home/ubuntu/results/coverage/Copy/Copy_10_10.coverage", "4e541dd5-099a-442e-a92a-c43cc215aaa4");
        try {
            writeline("/home/ubuntu/results/coverage/Copy/Copy_10_10.coverage", "a5d07f53-c92e-458e-aef1-081cbd53d10e");
            // make sure we don't have an illegal set of options
            try {
                writeline("/home/ubuntu/results/coverage/Copy/Copy_10_10.coverage", "7415df5b-4f95-4f35-bdf6-eb361eda0aec");
                validateAttributes();
            } catch (final BuildException e) {
                writeline("/home/ubuntu/results/coverage/Copy/Copy_10_10.coverage", "63aed2bb-a02b-4491-b2f9-97c6a5dbcdec");
                if (failonerror || !getMessage(e).equals(MSG_WHEN_COPYING_EMPTY_RC_TO_FILE)) {
                    writeline("/home/ubuntu/results/coverage/Copy/Copy_10_10.coverage", "a5e34766-9da3-46bb-8882-4675f38e8f7b");
                    throw e;
                } else {
                    writeline("/home/ubuntu/results/coverage/Copy/Copy_10_10.coverage", "98dd84c1-0fe7-48f6-83f3-d0b4225f8c80");
                    log("Warning: " + getMessage(e), Project.MSG_ERR);
                    writeline("/home/ubuntu/results/coverage/Copy/Copy_10_10.coverage", "affb99e5-466f-415b-a53b-d857d8b2b0e7");
                    return;
                }
            }
            writeline("/home/ubuntu/results/coverage/Copy/Copy_10_10.coverage", "ac1eb1ec-330b-485b-9d25-6014e978049e");
            // deal with the single file
            copySingleFile();
            writeline("/home/ubuntu/results/coverage/Copy/Copy_10_10.coverage", "7cc21041-28c5-4e76-9b2d-df5a4c284425");
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
            writeline("/home/ubuntu/results/coverage/Copy/Copy_10_10.coverage", "d0a16743-9a73-41a6-a1c6-c99bc2ce78ed");
            final HashMap<File, List<String>> dirsByBasedir = new HashMap<File, List<String>>();
            writeline("/home/ubuntu/results/coverage/Copy/Copy_10_10.coverage", "221b8688-38ca-4906-8f52-9ab0c24c8957");
            final HashSet<File> baseDirs = new HashSet<File>();
            writeline("/home/ubuntu/results/coverage/Copy/Copy_10_10.coverage", "345ccbfd-286b-468c-a522-bb18e94a9f09");
            final ArrayList<Resource> nonFileResources = new ArrayList<Resource>();
            writeline("/home/ubuntu/results/coverage/Copy/Copy_10_10.coverage", "6c1b184a-3172-4bfd-a3e5-63c94fe1f66f");
            final int size = rcs.size();
            writeline("/home/ubuntu/results/coverage/Copy/Copy_10_10.coverage", "e6634f25-d0a8-4521-90bc-67ee4024fab2");
            for (int i = 0; i < size; i++) {
                writeline("/home/ubuntu/results/coverage/Copy/Copy_10_10.coverage", "2c338189-f4b2-409e-bdff-34264d31aacb");
                final ResourceCollection rc = rcs.elementAt(i);
                writeline("/home/ubuntu/results/coverage/Copy/Copy_10_10.coverage", "714efa23-8bf0-496e-80f0-72f4fffffaec");
                // Step (1) - beware of the ZipFileSet
                if (rc instanceof FileSet && rc.isFilesystemOnly()) {
                    writeline("/home/ubuntu/results/coverage/Copy/Copy_10_10.coverage", "5f9991bf-6752-4e68-9a24-53c91944b968");
                    final FileSet fs = (FileSet) rc;
                    writeline("/home/ubuntu/results/coverage/Copy/Copy_10_10.coverage", "b24ff803-f884-453c-88f3-cc76878849c7");
                    DirectoryScanner ds = null;
                    writeline("/home/ubuntu/results/coverage/Copy/Copy_10_10.coverage", "eb7ef969-aaf5-4189-9ffb-a7eeeb04edcf");
                    try {
                        writeline("/home/ubuntu/results/coverage/Copy/Copy_10_10.coverage", "993166f1-05dc-422f-9378-bba7e0855a2d");
                        ds = fs.getDirectoryScanner(getProject());
                    } catch (final BuildException e) {
                        writeline("/home/ubuntu/results/coverage/Copy/Copy_10_10.coverage", "0d1135eb-af62-431f-a00d-25f12565b825");
                        if (failonerror || !getMessage(e).endsWith(DirectoryScanner.DOES_NOT_EXIST_POSTFIX)) {
                            writeline("/home/ubuntu/results/coverage/Copy/Copy_10_10.coverage", "c5998c4a-71eb-468e-9e2f-bf8b43a7fd64");
                            throw e;
                        } else {
                            writeline("/home/ubuntu/results/coverage/Copy/Copy_10_10.coverage", "7f78e921-ae21-4eeb-9abb-d28c26991e68");
                            if (!quiet) {
                                writeline("/home/ubuntu/results/coverage/Copy/Copy_10_10.coverage", "df5a2114-0e77-45fb-bf26-693e304b0719");
                                log("Warning: " + getMessage(e), Project.MSG_ERR);
                            }
                            writeline("/home/ubuntu/results/coverage/Copy/Copy_10_10.coverage", "0675e36c-9015-4b4b-aaa3-9f4049e6b449");
                            continue;
                        }
                    }
                    writeline("/home/ubuntu/results/coverage/Copy/Copy_10_10.coverage", "d4d162ed-5334-4204-99a9-05e9ce0780df");
                    final File fromDir = fs.getDir(getProject());
                    writeline("/home/ubuntu/results/coverage/Copy/Copy_10_10.coverage", "150475b6-3d0f-4a30-9f5a-c9d740fc652c");
                    final String[] srcFiles = ds.getIncludedFiles();
                    writeline("/home/ubuntu/results/coverage/Copy/Copy_10_10.coverage", "b94c9c6b-d9a1-4351-804a-0a84ef909524");
                    final String[] srcDirs = ds.getIncludedDirectories();
                    writeline("/home/ubuntu/results/coverage/Copy/Copy_10_10.coverage", "8a0893b0-0e59-4f70-b874-b26aa8c24c44");
                    if (!flatten && mapperElement == null && ds.isEverythingIncluded() && !fs.hasPatterns()) {
                        writeline("/home/ubuntu/results/coverage/Copy/Copy_10_10.coverage", "9453fd56-6fb0-488a-ae0b-35e8b78dedd6");
                        completeDirMap.put(fromDir, destDir);
                    }
                    writeline("/home/ubuntu/results/coverage/Copy/Copy_10_10.coverage", "f4890031-be75-4b99-910d-b5a3855326c7");
                    add(fromDir, srcFiles, filesByBasedir);
                    writeline("/home/ubuntu/results/coverage/Copy/Copy_10_10.coverage", "70e4bdaa-e7b3-42fd-bbd4-258ab8170226");
                    add(fromDir, srcDirs, dirsByBasedir);
                    writeline("/home/ubuntu/results/coverage/Copy/Copy_10_10.coverage", "17059c7c-8453-44b6-acf9-f2d44190cf0b");
                    baseDirs.add(fromDir);
                } else {
                    writeline("/home/ubuntu/results/coverage/Copy/Copy_10_10.coverage", "106ec162-8eb8-408d-9298-be7311c3273e");
                    if (!rc.isFilesystemOnly() && !supportsNonFileResources()) {
                        writeline("/home/ubuntu/results/coverage/Copy/Copy_10_10.coverage", "abbe7d5c-23ee-4875-91d9-c2df9d6163b3");
                        throw new BuildException("Only FileSystem resources are supported.");
                    }
                    writeline("/home/ubuntu/results/coverage/Copy/Copy_10_10.coverage", "6dca860b-5846-47e4-baf2-d55b8f8574bb");
                    for (final Resource r : rc) {
                        writeline("/home/ubuntu/results/coverage/Copy/Copy_10_10.coverage", "fd955701-ae29-44ec-a19d-c98ef50f31fe");
                        if (!r.isExists()) {
                            writeline("/home/ubuntu/results/coverage/Copy/Copy_10_10.coverage", "ddc875a7-0d82-410f-bea7-b3f0b76d150a");
                            final String message = "Warning: Could not find resource " + r.toLongString() + " to copy.";
                            writeline("/home/ubuntu/results/coverage/Copy/Copy_10_10.coverage", "e8c80846-87fa-4bdc-b946-9b07aae5c82a");
                            if (!failonerror) {
                                writeline("/home/ubuntu/results/coverage/Copy/Copy_10_10.coverage", "271d6c40-f64d-4824-aa8b-dae1cb9d3f1c");
                                if (!quiet) {
                                    writeline("/home/ubuntu/results/coverage/Copy/Copy_10_10.coverage", "40650ff7-4d75-415a-a60b-e892726a6786");
                                    log(message, Project.MSG_ERR);
                                }
                            } else {
                                writeline("/home/ubuntu/results/coverage/Copy/Copy_10_10.coverage", "b86089f8-282f-4a78-96d7-95f1afe9c543");
                                throw new BuildException(message);
                            }
                            writeline("/home/ubuntu/results/coverage/Copy/Copy_10_10.coverage", "31e3855f-ad7c-4a3b-8981-d64a79181231");
                            continue;
                        }
                        writeline("/home/ubuntu/results/coverage/Copy/Copy_10_10.coverage", "454c05c7-0b72-44a5-b432-bad9b7a0b982");
                        File baseDir = NULL_FILE_PLACEHOLDER;
                        writeline("/home/ubuntu/results/coverage/Copy/Copy_10_10.coverage", "35b9457e-02a2-4f2b-8ee0-99eda895eb37");
                        String name = r.getName();
                        writeline("/home/ubuntu/results/coverage/Copy/Copy_10_10.coverage", "622318a8-7f5a-47fb-bfd2-35fcb3d4be73");
                        final FileProvider fp = r.as(FileProvider.class);
                        writeline("/home/ubuntu/results/coverage/Copy/Copy_10_10.coverage", "340870f6-faa2-45e1-831b-6d73198e6474");
                        if (fp != null) {
                            writeline("/home/ubuntu/results/coverage/Copy/Copy_10_10.coverage", "c55777ab-3293-4dcf-a4c1-6e0a187bfbce");
                            final FileResource fr = ResourceUtils.asFileResource(fp);
                            writeline("/home/ubuntu/results/coverage/Copy/Copy_10_10.coverage", "2bb773b3-1bbd-491e-9117-92e089b886cb");
                            baseDir = getKeyFile(fr.getBaseDir());
                            writeline("/home/ubuntu/results/coverage/Copy/Copy_10_10.coverage", "f92de589-16ae-427f-ba90-07b210b0479d");
                            if (fr.getBaseDir() == null) {
                                writeline("/home/ubuntu/results/coverage/Copy/Copy_10_10.coverage", "68b037fe-8217-4e81-8f4e-051cb643f1cb");
                                name = fr.getFile().getAbsolutePath();
                            }
                        }
                        writeline("/home/ubuntu/results/coverage/Copy/Copy_10_10.coverage", "fad9fdb1-03c6-45d3-8be2-73d47e1097c0");
                        // files.
                        if (r.isDirectory() || fp != null) {
                            writeline("/home/ubuntu/results/coverage/Copy/Copy_10_10.coverage", "84afc933-2518-47f7-b228-b3730181e1d6");
                            add(baseDir, name, r.isDirectory() ? dirsByBasedir : filesByBasedir);
                            writeline("/home/ubuntu/results/coverage/Copy/Copy_10_10.coverage", "c7d787c5-88a1-4004-9927-b89319647373");
                            baseDirs.add(baseDir);
                        } else {
                            writeline("/home/ubuntu/results/coverage/Copy/Copy_10_10.coverage", "9acdaccd-e5bf-431f-8b2d-a86ae8ec62ae");
                            // a not-directory file resource
                            // needs special treatment
                            nonFileResources.add(r);
                        }
                    }
                }
            }
            writeline("/home/ubuntu/results/coverage/Copy/Copy_10_10.coverage", "b9200ffe-3e95-4c94-ad6e-dc39211a1e01");
            iterateOverBaseDirs(baseDirs, dirsByBasedir, filesByBasedir);
            writeline("/home/ubuntu/results/coverage/Copy/Copy_10_10.coverage", "4f3dae23-f9a6-4622-9e0b-bc9bf99a12c3");
            // do all the copy operations now...
            try {
                writeline("/home/ubuntu/results/coverage/Copy/Copy_10_10.coverage", "85cbfaad-54b2-414d-b4a3-4b75beedc3b3");
                doFileOperations();
            } catch (final BuildException e) {
                writeline("/home/ubuntu/results/coverage/Copy/Copy_10_10.coverage", "8c5a8302-778d-46d6-987a-e68200230d4e");
                if (!failonerror) {
                    writeline("/home/ubuntu/results/coverage/Copy/Copy_10_10.coverage", "5b0cdfbc-5a2d-4ad3-b930-09822bec54e0");
                    if (!quiet) {
                        writeline("/home/ubuntu/results/coverage/Copy/Copy_10_10.coverage", "7db61f0c-6d84-43a5-96ef-5ce9c34a7193");
                        log("Warning: " + getMessage(e), Project.MSG_ERR);
                    }
                } else {
                    writeline("/home/ubuntu/results/coverage/Copy/Copy_10_10.coverage", "cfb48d68-a939-49f1-9813-6e356e322ca4");
                    throw e;
                }
            }
            writeline("/home/ubuntu/results/coverage/Copy/Copy_10_10.coverage", "9f5e1024-c5fa-4f57-b63a-c1ea42a3b073");
            if (nonFileResources.size() > 0 || singleResource != null) {
                writeline("/home/ubuntu/results/coverage/Copy/Copy_10_10.coverage", "1e921fe5-0abf-4840-9fb3-2cee346a0d5a");
                final Resource[] nonFiles = nonFileResources.toArray(new Resource[nonFileResources.size()]);
                writeline("/home/ubuntu/results/coverage/Copy/Copy_10_10.coverage", "a70e4e03-ea23-4447-870e-547c643b2809");
                // restrict to out-of-date resources
                final Map<Resource, String[]> map = scan(nonFiles, destDir);
                writeline("/home/ubuntu/results/coverage/Copy/Copy_10_10.coverage", "81ba07b7-6812-42d6-bcc2-a3e2a4f0ac67");
                if (singleResource != null) {
                    writeline("/home/ubuntu/results/coverage/Copy/Copy_10_10.coverage", "43790fa0-8f87-4ce7-a2c0-b221bd28fb30");
                    map.put(singleResource, new String[] { destFile.getAbsolutePath() });
                }
                writeline("/home/ubuntu/results/coverage/Copy/Copy_10_10.coverage", "79c5e86b-5ffa-43ca-9fe1-8f1776341efb");
                try {
                    writeline("/home/ubuntu/results/coverage/Copy/Copy_10_10.coverage", "ee25f37d-a581-4537-b571-6e6a6811175a");
                    doResourceOperations(map);
                } catch (final BuildException e) {
                    writeline("/home/ubuntu/results/coverage/Copy/Copy_10_10.coverage", "21b0f0a4-9069-4de4-b3e1-d5672b9ba9e2");
                    if (!failonerror) {
                        writeline("/home/ubuntu/results/coverage/Copy/Copy_10_10.coverage", "d85d5606-471d-48cb-9414-94d884fbb342");
                        if (!quiet) {
                            writeline("/home/ubuntu/results/coverage/Copy/Copy_10_10.coverage", "84fce1d8-f058-45b0-9286-22c4f0455963");
                            log("Warning: " + getMessage(e), Project.MSG_ERR);
                        }
                    } else {
                        writeline("/home/ubuntu/results/coverage/Copy/Copy_10_10.coverage", "cb0a0e8e-b429-4018-9864-71fb98a2653c");
                        throw e;
                    }
                }
            }
        } finally {
            writeline("/home/ubuntu/results/coverage/Copy/Copy_10_10.coverage", "8c5d526a-c654-4540-9b39-f1e78679ae60");
            // clean up again, so this instance can be used a second
            // time
            singleResource = null;
            writeline("/home/ubuntu/results/coverage/Copy/Copy_10_10.coverage", "242caa11-fde9-488e-b9c9-39855f074ab6");
            file = savedFile;
            writeline("/home/ubuntu/results/coverage/Copy/Copy_10_10.coverage", "f55c8f07-1476-4770-8e8e-3ea690c29464");
            destFile = savedDestFile;
            writeline("/home/ubuntu/results/coverage/Copy/Copy_10_10.coverage", "a517edf2-3103-4cd1-a248-b2912709a6e8");
            destDir = savedDestDir;
            writeline("/home/ubuntu/results/coverage/Copy/Copy_10_10.coverage", "c9091546-c8f1-4d81-94f8-dc3ba8e64257");
            if (savedRc != null) {
                writeline("/home/ubuntu/results/coverage/Copy/Copy_10_10.coverage", "7fe0d41b-db29-4384-a162-00e604f88cf4");
                rcs.insertElementAt(savedRc, 0);
            }
            writeline("/home/ubuntu/results/coverage/Copy/Copy_10_10.coverage", "07b32bc5-6a19-4a36-9b05-d17720d4343b");
            fileCopyMap.clear();
            writeline("/home/ubuntu/results/coverage/Copy/Copy_10_10.coverage", "f6348133-b0dd-4297-962d-08904790b0bc");
            dirCopyMap.clear();
            writeline("/home/ubuntu/results/coverage/Copy/Copy_10_10.coverage", "e3f3e839-3745-4d14-ba6b-92ebd86c065a");
            completeDirMap.clear();
        }
    }

    /**
     * *********************************************************************
     * *  protected and private methods
     * **********************************************************************
     */
    private void copySingleFile() {
        writeline("/home/ubuntu/results/coverage/Copy/Copy_10_10.coverage", "23a18662-64b3-4221-b4b5-f898486493d9");
        // deal with the single file
        if (file != null) {
            writeline("/home/ubuntu/results/coverage/Copy/Copy_10_10.coverage", "5ce7efaa-e36d-461c-8e61-a75b9d6f0f22");
            if (file.exists()) {
                writeline("/home/ubuntu/results/coverage/Copy/Copy_10_10.coverage", "2dfba1f5-5412-4779-92dc-6141c4d95e88");
                if (destFile == null) {
                    writeline("/home/ubuntu/results/coverage/Copy/Copy_10_10.coverage", "fc4a2620-e029-4034-8248-76e3cc6d7b28");
                    destFile = new File(destDir, file.getName());
                }
                writeline("/home/ubuntu/results/coverage/Copy/Copy_10_10.coverage", "5bd39608-2beb-44b3-8c35-83920a29d8b5");
                if (forceOverwrite || !destFile.exists() || (file.lastModified() - granularity > destFile.lastModified())) {
                    writeline("/home/ubuntu/results/coverage/Copy/Copy_10_10.coverage", "eb0257c8-7a65-49bf-abda-046a18c0818e");
                    fileCopyMap.put(file.getAbsolutePath(), new String[] { destFile.getAbsolutePath() });
                } else {
                    writeline("/home/ubuntu/results/coverage/Copy/Copy_10_10.coverage", "f5e85339-0499-48cc-bc30-3b5be13ddf24");
                    log(file + " omitted as " + destFile + " is up to date.", Project.MSG_VERBOSE);
                }
            } else {
                writeline("/home/ubuntu/results/coverage/Copy/Copy_10_10.coverage", "e4a2b38b-db21-4365-9ce7-46346f391bd1");
                final String message = "Warning: Could not find file " + file.getAbsolutePath() + " to copy.";
                writeline("/home/ubuntu/results/coverage/Copy/Copy_10_10.coverage", "9893062f-31d2-4fca-b27f-5fa1301de7ce");
                if (!failonerror) {
                    writeline("/home/ubuntu/results/coverage/Copy/Copy_10_10.coverage", "61fdf089-d177-4157-be80-dad2db8fd984");
                    if (!quiet) {
                        writeline("/home/ubuntu/results/coverage/Copy/Copy_10_10.coverage", "e2d43ab1-07e6-4b2d-a098-7a9ba9df1ab5");
                        log(message, Project.MSG_ERR);
                    }
                } else {
                    writeline("/home/ubuntu/results/coverage/Copy/Copy_10_10.coverage", "c01e50ad-2477-4066-b62a-22548743c9a2");
                    throw new BuildException(message);
                }
            }
        }
    }

    private void iterateOverBaseDirs(final HashSet<File> baseDirs, final HashMap<File, List<String>> dirsByBasedir, final HashMap<File, List<String>> filesByBasedir) {
        writeline("/home/ubuntu/results/coverage/Copy/Copy_10_10.coverage", "8c02b1a9-28c7-4cab-b3aa-053f8eb704a6");
        for (final File f : baseDirs) {
            writeline("/home/ubuntu/results/coverage/Copy/Copy_10_10.coverage", "2267455f-ff81-4081-9417-14e953259190");
            final List<String> files = filesByBasedir.get(f);
            writeline("/home/ubuntu/results/coverage/Copy/Copy_10_10.coverage", "0f27d7e5-7629-4218-aed4-1a8b02d88c2e");
            final List<String> dirs = dirsByBasedir.get(f);
            writeline("/home/ubuntu/results/coverage/Copy/Copy_10_10.coverage", "c99f152a-7fd9-4ca2-9fea-6b66216d9b09");
            String[] srcFiles = new String[0];
            writeline("/home/ubuntu/results/coverage/Copy/Copy_10_10.coverage", "fca53a84-f368-41f2-841c-59d7e6f9b704");
            if (files != null) {
                writeline("/home/ubuntu/results/coverage/Copy/Copy_10_10.coverage", "a7c30664-343a-497c-9d6f-ca1241fd6aab");
                srcFiles = files.toArray(srcFiles);
            }
            writeline("/home/ubuntu/results/coverage/Copy/Copy_10_10.coverage", "03442cfe-152a-4797-b4c7-89c349ad3cd1");
            String[] srcDirs = new String[0];
            writeline("/home/ubuntu/results/coverage/Copy/Copy_10_10.coverage", "020f7cca-2dc4-4309-89f2-9718b53b0cb5");
            if (dirs != null) {
                writeline("/home/ubuntu/results/coverage/Copy/Copy_10_10.coverage", "4a904e99-a7ba-4430-978b-45a69faa2465");
                srcDirs = dirs.toArray(srcDirs);
            }
            writeline("/home/ubuntu/results/coverage/Copy/Copy_10_10.coverage", "9d74ca86-57ee-4c9c-ba81-ff571d29fe1c");
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
        writeline("/home/ubuntu/results/coverage/Copy/Copy_10_10.coverage", "ac582939-b239-49c9-bc0e-d4bc7912f6b1");
        if (file == null && rcs.size() == 0) {
            writeline("/home/ubuntu/results/coverage/Copy/Copy_10_10.coverage", "c5cdf2ce-1404-4907-a015-18d45e2ea15b");
            throw new BuildException("Specify at least one source--a file or a resource collection.");
        }
        writeline("/home/ubuntu/results/coverage/Copy/Copy_10_10.coverage", "dabf0b2c-6997-4235-849a-77dd7bf138ec");
        if (destFile != null && destDir != null) {
            writeline("/home/ubuntu/results/coverage/Copy/Copy_10_10.coverage", "0cdcf570-7dab-4ea2-aeed-d57514684f60");
            throw new BuildException("Only one of tofile and todir may be set.");
        }
        writeline("/home/ubuntu/results/coverage/Copy/Copy_10_10.coverage", "10e2c2d4-84b6-4d84-90cc-748f67e1a98e");
        if (destFile == null && destDir == null) {
            writeline("/home/ubuntu/results/coverage/Copy/Copy_10_10.coverage", "57481b19-8008-447e-a9de-a4b143c4aa2d");
            throw new BuildException("One of tofile or todir must be set.");
        }
        writeline("/home/ubuntu/results/coverage/Copy/Copy_10_10.coverage", "6c1ed47c-2020-4ba0-bd74-e32d044496bc");
        if (file != null && file.isDirectory()) {
            writeline("/home/ubuntu/results/coverage/Copy/Copy_10_10.coverage", "f5774274-aa09-416d-b9f8-628a5004c9c3");
            throw new BuildException("Use a resource collection to copy directories.");
        }
        writeline("/home/ubuntu/results/coverage/Copy/Copy_10_10.coverage", "c870cfc3-2520-4a0f-96ce-eca348f69ae7");
        if (destFile != null && rcs.size() > 0) {
            writeline("/home/ubuntu/results/coverage/Copy/Copy_10_10.coverage", "aea20b55-fde7-438c-85f7-da33a9e1658e");
            if (rcs.size() > 1) {
                writeline("/home/ubuntu/results/coverage/Copy/Copy_10_10.coverage", "52fc5bc4-cbde-4312-8eb5-9b1e252ea8e6");
                throw new BuildException("Cannot concatenate multiple files into a single file.");
            } else {
                writeline("/home/ubuntu/results/coverage/Copy/Copy_10_10.coverage", "ccc6ac08-5d87-415f-b589-69a3985d4997");
                final ResourceCollection rc = rcs.elementAt(0);
                writeline("/home/ubuntu/results/coverage/Copy/Copy_10_10.coverage", "568c280a-6187-4cc4-870b-1ad02f00b813");
                if (!rc.isFilesystemOnly() && !supportsNonFileResources()) {
                    writeline("/home/ubuntu/results/coverage/Copy/Copy_10_10.coverage", "aa232d3a-c8c3-41ef-842b-80cce098102b");
                    throw new BuildException("Only FileSystem resources are" + " supported.");
                }
                writeline("/home/ubuntu/results/coverage/Copy/Copy_10_10.coverage", "85ac0ad8-9925-4571-b099-d52f83a33e64");
                if (rc.size() == 0) {
                    writeline("/home/ubuntu/results/coverage/Copy/Copy_10_10.coverage", "1fbe6181-b566-4e0e-af4f-d7423877396d");
                    throw new BuildException(MSG_WHEN_COPYING_EMPTY_RC_TO_FILE);
                } else if (rc.size() == 1) {
                    writeline("/home/ubuntu/results/coverage/Copy/Copy_10_10.coverage", "5ae5c171-68a3-4665-be4e-e8521ba43c7e");
                    final Resource res = rc.iterator().next();
                    writeline("/home/ubuntu/results/coverage/Copy/Copy_10_10.coverage", "763db1c4-3686-4c4b-8ed5-36ef79ccc210");
                    final FileProvider r = res.as(FileProvider.class);
                    writeline("/home/ubuntu/results/coverage/Copy/Copy_10_10.coverage", "196e9087-0464-4de2-ac34-76f7981d3372");
                    if (file == null) {
                        writeline("/home/ubuntu/results/coverage/Copy/Copy_10_10.coverage", "48e40bbb-1f84-4ac1-9ea1-901b43f5ae5d");
                        if (r != null) {
                            writeline("/home/ubuntu/results/coverage/Copy/Copy_10_10.coverage", "ec1d5619-6993-4702-be7e-2f0b035b4af5");
                            file = r.getFile();
                        } else {
                            writeline("/home/ubuntu/results/coverage/Copy/Copy_10_10.coverage", "c6582593-444e-48e1-9eac-063ad2ec977a");
                            singleResource = res;
                        }
                        writeline("/home/ubuntu/results/coverage/Copy/Copy_10_10.coverage", "3641224d-864a-469d-9ed8-e0848ab8198e");
                        rcs.removeElementAt(0);
                    } else {
                        writeline("/home/ubuntu/results/coverage/Copy/Copy_10_10.coverage", "e40b17d1-3776-4188-8e0e-a5c72741eefa");
                        throw new BuildException("Cannot concatenate multiple files into a single file.");
                    }
                } else {
                    writeline("/home/ubuntu/results/coverage/Copy/Copy_10_10.coverage", "3348cf19-eb1b-45d6-a341-389f56a4b77f");
                    throw new BuildException("Cannot concatenate multiple files into a single file.");
                }
            }
        }
        writeline("/home/ubuntu/results/coverage/Copy/Copy_10_10.coverage", "cde377f5-5eff-44bd-9012-9af89707fe8b");
        if (destFile != null) {
            writeline("/home/ubuntu/results/coverage/Copy/Copy_10_10.coverage", "aa37533d-05c5-47ad-bae0-ef81279c202b");
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
        writeline("/home/ubuntu/results/coverage/Copy/Copy_10_10.coverage", "7c1ede41-c094-468f-acfd-d02d2168856b");
        final FileNameMapper mapper = getMapper();
        writeline("/home/ubuntu/results/coverage/Copy/Copy_10_10.coverage", "0f4dcf9c-6a22-4ce3-abdf-7b407feb5f5f");
        buildMap(fromDir, toDir, files, mapper, fileCopyMap);
        writeline("/home/ubuntu/results/coverage/Copy/Copy_10_10.coverage", "57bbb8d8-9753-4bee-83a1-79d6541732da");
        if (includeEmpty) {
            writeline("/home/ubuntu/results/coverage/Copy/Copy_10_10.coverage", "9545893c-dfba-4a0f-8f01-217ae9f74dcc");
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
        writeline("/home/ubuntu/results/coverage/Copy/Copy_10_10.coverage", "c78c3ae4-2a3c-45d4-abdb-302bbbec0292");
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
        writeline("/home/ubuntu/results/coverage/Copy/Copy_10_10.coverage", "d0b24344-e3ed-4beb-af74-c3e19b9b7aa8");
        String[] toCopy = null;
        writeline("/home/ubuntu/results/coverage/Copy/Copy_10_10.coverage", "391e3808-6753-4e8a-bd84-bb9addb7b0f6");
        if (forceOverwrite) {
            writeline("/home/ubuntu/results/coverage/Copy/Copy_10_10.coverage", "6152228a-18db-40a7-a85a-bd4987196b8e");
            final Vector<String> v = new Vector<String>();
            writeline("/home/ubuntu/results/coverage/Copy/Copy_10_10.coverage", "7f1cb3c9-84f9-47d4-9897-852755e92ba6");
            for (int i = 0; i < names.length; i++) {
                writeline("/home/ubuntu/results/coverage/Copy/Copy_10_10.coverage", "dc3de163-cd07-46bf-a59c-749f2588b0e6");
                if (mapper.mapFileName(names[i]) != null) {
                    writeline("/home/ubuntu/results/coverage/Copy/Copy_10_10.coverage", "1f0dece5-d14e-47a2-8368-c655e6ed2a3a");
                    v.addElement(names[i]);
                }
            }
            writeline("/home/ubuntu/results/coverage/Copy/Copy_10_10.coverage", "e6bd0320-e5bf-490f-bf4a-6e09000048f7");
            toCopy = new String[v.size()];
            writeline("/home/ubuntu/results/coverage/Copy/Copy_10_10.coverage", "f2e6da0b-9ae7-4a9d-8b41-f5e1e7e808f1");
            v.copyInto(toCopy);
        } else {
            writeline("/home/ubuntu/results/coverage/Copy/Copy_10_10.coverage", "75b25406-063e-4a7b-b4c7-91ffec4ccfa7");
            final SourceFileScanner ds = new SourceFileScanner(this);
            writeline("/home/ubuntu/results/coverage/Copy/Copy_10_10.coverage", "83c8c456-e019-4864-8993-ddf81f5866be");
            toCopy = ds.restrict(names, fromDir, toDir, mapper, granularity);
        }
        writeline("/home/ubuntu/results/coverage/Copy/Copy_10_10.coverage", "92d55954-d1b2-4fe4-a1f2-37fad7b79f6c");
        for (int i = 0; i < toCopy.length; i++) {
            writeline("/home/ubuntu/results/coverage/Copy/Copy_10_10.coverage", "a8ef1f74-3831-4781-9998-b4506f2f0275");
            final File src = new File(fromDir, toCopy[i]);
            writeline("/home/ubuntu/results/coverage/Copy/Copy_10_10.coverage", "cd80b79c-0e8b-466a-99ea-ff4c2e903db9");
            final String[] mappedFiles = mapper.mapFileName(toCopy[i]);
            writeline("/home/ubuntu/results/coverage/Copy/Copy_10_10.coverage", "8622d031-090a-46db-a3b1-902e4c142c53");
            if (!enableMultipleMappings) {
                writeline("/home/ubuntu/results/coverage/Copy/Copy_10_10.coverage", "9ba93df7-2f25-4350-8a72-2be6a43c0d6a");
                map.put(src.getAbsolutePath(), new String[] { new File(toDir, mappedFiles[0]).getAbsolutePath() });
            } else {
                writeline("/home/ubuntu/results/coverage/Copy/Copy_10_10.coverage", "be87eb13-4dd0-40c5-8eee-2dc96d3ebc28");
                // reuse the array created by the mapper
                for (int k = 0; k < mappedFiles.length; k++) {
                    writeline("/home/ubuntu/results/coverage/Copy/Copy_10_10.coverage", "9a8f0ffd-4a04-4bed-930e-65aadc837c58");
                    mappedFiles[k] = new File(toDir, mappedFiles[k]).getAbsolutePath();
                }
                writeline("/home/ubuntu/results/coverage/Copy/Copy_10_10.coverage", "26fbf5bd-3a8c-40be-9b7a-2beb8cba1ed1");
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
        writeline("/home/ubuntu/results/coverage/Copy/Copy_10_10.coverage", "b73303cc-13ae-493b-b6d0-848f2283efe6");
        final HashMap<Resource, String[]> map = new HashMap<Resource, String[]>();
        writeline("/home/ubuntu/results/coverage/Copy/Copy_10_10.coverage", "305a6730-273f-49fb-94ef-b8380b98855d");
        Resource[] toCopy = null;
        writeline("/home/ubuntu/results/coverage/Copy/Copy_10_10.coverage", "41dc170a-bf92-4707-beb7-c38169ffbe09");
        if (forceOverwrite) {
            writeline("/home/ubuntu/results/coverage/Copy/Copy_10_10.coverage", "899e2c8c-884d-4f1a-80b8-199697e03f3c");
            final Vector<Resource> v = new Vector<Resource>();
            writeline("/home/ubuntu/results/coverage/Copy/Copy_10_10.coverage", "c31a3f89-278d-4de1-a5b2-fa4243f5b207");
            for (int i = 0; i < fromResources.length; i++) {
                writeline("/home/ubuntu/results/coverage/Copy/Copy_10_10.coverage", "3bf3d2ad-8aac-4cdd-b6be-0e34d7d48981");
                if (mapper.mapFileName(fromResources[i].getName()) != null) {
                    writeline("/home/ubuntu/results/coverage/Copy/Copy_10_10.coverage", "60ff08e3-804f-4ea4-9041-eb36b2d107a5");
                    v.addElement(fromResources[i]);
                }
            }
            writeline("/home/ubuntu/results/coverage/Copy/Copy_10_10.coverage", "8405d73f-da99-42f1-bcba-24bd7e6a6da5");
            toCopy = new Resource[v.size()];
            writeline("/home/ubuntu/results/coverage/Copy/Copy_10_10.coverage", "01bf4795-b458-4a07-885a-1a62b385f142");
            v.copyInto(toCopy);
        } else {
            writeline("/home/ubuntu/results/coverage/Copy/Copy_10_10.coverage", "75a6641b-8f27-4b49-805a-abd82845e390");
            toCopy = ResourceUtils.selectOutOfDateSources(this, fromResources, mapper, new ResourceFactory() {

                public Resource getResource(final String name) {
                    return new FileResource(toDir, name);
                }
            }, granularity);
        }
        writeline("/home/ubuntu/results/coverage/Copy/Copy_10_10.coverage", "0632e7ca-e379-4387-a391-d2576d2666a8");
        for (int i = 0; i < toCopy.length; i++) {
            writeline("/home/ubuntu/results/coverage/Copy/Copy_10_10.coverage", "105f93a7-9d4d-465f-a19c-71ed46a176aa");
            final String[] mappedFiles = mapper.mapFileName(toCopy[i].getName());
            writeline("/home/ubuntu/results/coverage/Copy/Copy_10_10.coverage", "b4e094c1-10cf-4fce-9e96-e9612f0d158e");
            for (int j = 0; j < mappedFiles.length; j++) {
                writeline("/home/ubuntu/results/coverage/Copy/Copy_10_10.coverage", "8bad6c61-cf00-46bc-aac3-3c1c32b30956");
                if (mappedFiles[j] == null) {
                    writeline("/home/ubuntu/results/coverage/Copy/Copy_10_10.coverage", "057cde0e-31ef-4fca-98fc-4911818d4195");
                    throw new BuildException("Can't copy a resource without a" + " name if the mapper doesn't" + " provide one.");
                }
            }
            writeline("/home/ubuntu/results/coverage/Copy/Copy_10_10.coverage", "c36b9e91-f56a-4e7a-90c2-a1821cf52ab1");
            if (!enableMultipleMappings) {
                writeline("/home/ubuntu/results/coverage/Copy/Copy_10_10.coverage", "2703afa8-80b7-414a-99dc-7f5a36cb9f67");
                map.put(toCopy[i], new String[] { new File(toDir, mappedFiles[0]).getAbsolutePath() });
            } else {
                writeline("/home/ubuntu/results/coverage/Copy/Copy_10_10.coverage", "470ba9a0-ceee-4a01-b361-79fc421b03b6");
                // reuse the array created by the mapper
                for (int k = 0; k < mappedFiles.length; k++) {
                    writeline("/home/ubuntu/results/coverage/Copy/Copy_10_10.coverage", "a2ba4f04-e55f-4c42-b4d7-dcb08d25ea71");
                    mappedFiles[k] = new File(toDir, mappedFiles[k]).getAbsolutePath();
                }
                writeline("/home/ubuntu/results/coverage/Copy/Copy_10_10.coverage", "fd6d06f3-dd38-475a-afc2-9d0ce8660d1f");
                map.put(toCopy[i], mappedFiles);
            }
        }
        writeline("/home/ubuntu/results/coverage/Copy/Copy_10_10.coverage", "dfa1503c-22f4-4d80-a81b-d2d554067f4b");
        return map;
    }

    /**
     * Actually does the file (and possibly empty directory) copies.
     * This is a good method for subclasses to override.
     */
    protected void doFileOperations() {
        writeline("/home/ubuntu/results/coverage/Copy/Copy_10_10.coverage", "c0af7756-f4a4-40f4-a349-20c2e84f2c31");
        if (fileCopyMap.size() > 0) {
            writeline("/home/ubuntu/results/coverage/Copy/Copy_10_10.coverage", "aa25aa59-e927-4525-8e92-81f577a91b1e");
            log("Copying " + fileCopyMap.size() + " file" + (fileCopyMap.size() == 1 ? "" : "s") + " to " + destDir.getAbsolutePath());
            writeline("/home/ubuntu/results/coverage/Copy/Copy_10_10.coverage", "be2f82cb-ca68-44b1-ab97-00d9fb168a04");
            for (final Map.Entry<String, String[]> e : fileCopyMap.entrySet()) {
                writeline("/home/ubuntu/results/coverage/Copy/Copy_10_10.coverage", "cebb8b7a-5796-4a69-872d-f47af969f453");
                final String fromFile = e.getKey();
                writeline("/home/ubuntu/results/coverage/Copy/Copy_10_10.coverage", "9ed9b568-5fcb-41fa-905f-37d8d844c503");
                final String[] toFiles = e.getValue();
                writeline("/home/ubuntu/results/coverage/Copy/Copy_10_10.coverage", "d5ea043e-19ef-4892-8950-fec0bc5ef9b5");
                for (int i = 0; i < toFiles.length; i++) {
                    writeline("/home/ubuntu/results/coverage/Copy/Copy_10_10.coverage", "2820fd98-10dc-490b-8b74-ab2425c17d25");
                    final String toFile = toFiles[i];
                    writeline("/home/ubuntu/results/coverage/Copy/Copy_10_10.coverage", "b67081ab-848e-4fd8-b4c9-b91fd3ae66f8");
                    if (fromFile.equals(toFile)) {
                        writeline("/home/ubuntu/results/coverage/Copy/Copy_10_10.coverage", "d1c8efd6-79aa-4cdc-9672-b02ed8b1050e");
                        log("Skipping self-copy of " + fromFile, verbosity);
                        writeline("/home/ubuntu/results/coverage/Copy/Copy_10_10.coverage", "7cbbaf3c-3835-4d84-a482-bc5f14569dae");
                        continue;
                    }
                    writeline("/home/ubuntu/results/coverage/Copy/Copy_10_10.coverage", "077c3113-b020-4022-9c4e-9dd6bcdfe6b2");
                    try {
                        writeline("/home/ubuntu/results/coverage/Copy/Copy_10_10.coverage", "418a2cbf-9783-483f-87c5-036252d5f652");
                        log("Copying " + fromFile + " to " + toFile, verbosity);
                        writeline("/home/ubuntu/results/coverage/Copy/Copy_10_10.coverage", "bb324900-724d-4b3a-908a-bec23bc75320");
                        final FilterSetCollection executionFilters = new FilterSetCollection();
                        writeline("/home/ubuntu/results/coverage/Copy/Copy_10_10.coverage", "ecef73d5-b697-4574-baeb-65b1c90107ad");
                        if (filtering) {
                            writeline("/home/ubuntu/results/coverage/Copy/Copy_10_10.coverage", "4f9baf0f-039f-470b-b49d-a88177b11e01");
                            executionFilters.addFilterSet(getProject().getGlobalFilterSet());
                        }
                        writeline("/home/ubuntu/results/coverage/Copy/Copy_10_10.coverage", "1a590653-0cf1-4b99-bc70-23adacc12130");
                        for (final FilterSet filterSet : filterSets) {
                            writeline("/home/ubuntu/results/coverage/Copy/Copy_10_10.coverage", "e5d3b823-0d57-4172-a04a-b410cebf5f08");
                            executionFilters.addFilterSet(filterSet);
                        }
                        writeline("/home/ubuntu/results/coverage/Copy/Copy_10_10.coverage", "6e12b27b-5cce-44e4-afee-38570a531749");
                        fileUtils.copyFile(new File(fromFile), new File(toFile), executionFilters, filterChains, forceOverwrite, preserveLastModified, /* append: */
                        false, inputEncoding, outputEncoding, getProject(), getForce());
                    } catch (final IOException ioe) {
                        writeline("/home/ubuntu/results/coverage/Copy/Copy_10_10.coverage", "a569c06d-e279-48ab-ba19-795a1f04d113");
                        String msg = "Failed to copy " + fromFile + " to " + toFile + " due to " + getDueTo(ioe);
                        writeline("/home/ubuntu/results/coverage/Copy/Copy_10_10.coverage", "95356f16-aa66-4ee8-9015-30035774db5d");
                        final File targetFile = new File(toFile);
                        writeline("/home/ubuntu/results/coverage/Copy/Copy_10_10.coverage", "70c79510-103b-4283-941c-355b16a8ce3b");
                        if (!(ioe instanceof ResourceUtils.ReadOnlyTargetFileException) && targetFile.exists() && !targetFile.delete()) {
                            writeline("/home/ubuntu/results/coverage/Copy/Copy_10_10.coverage", "10c7f51f-fbc5-4ba2-a865-b02537e30f66");
                            msg += " and I couldn't delete the corrupt " + toFile;
                        }
                        writeline("/home/ubuntu/results/coverage/Copy/Copy_10_10.coverage", "4a0d1e7d-ee23-48d7-8941-ba4961e4521c");
                        if (failonerror) {
                            writeline("/home/ubuntu/results/coverage/Copy/Copy_10_10.coverage", "cf89f656-0d44-44e6-9397-63676238378e");
                            throw new BuildException(msg, ioe, getLocation());
                        }
                        writeline("/home/ubuntu/results/coverage/Copy/Copy_10_10.coverage", "e46b063f-bb1c-41b9-9492-f4a5e93f8c6b");
                        log(msg, Project.MSG_ERR);
                    }
                }
            }
        }
        writeline("/home/ubuntu/results/coverage/Copy/Copy_10_10.coverage", "88fabfba-08fc-4436-8e71-239e3a803c72");
        if (includeEmpty) {
            writeline("/home/ubuntu/results/coverage/Copy/Copy_10_10.coverage", "ba5e5f35-71c7-4b59-9b9e-d452c218549e");
            int createCount = 0;
            writeline("/home/ubuntu/results/coverage/Copy/Copy_10_10.coverage", "a393d7d9-6f69-4752-96e5-bde52fbeea92");
            for (final String[] dirs : dirCopyMap.values()) {
                writeline("/home/ubuntu/results/coverage/Copy/Copy_10_10.coverage", "39115004-ed75-4192-8dd6-7ad3b14dbbb6");
                for (int i = 0; i < dirs.length; i++) {
                    writeline("/home/ubuntu/results/coverage/Copy/Copy_10_10.coverage", "4b11b2c5-a182-46a2-b755-7df4d03ec580");
                    final File d = new File(dirs[i]);
                    writeline("/home/ubuntu/results/coverage/Copy/Copy_10_10.coverage", "de6fdea8-c6e1-4fe8-bfba-dfc81cb6cbe1");
                    if (!d.exists()) {
                        writeline("/home/ubuntu/results/coverage/Copy/Copy_10_10.coverage", "1e0a61c7-2321-44b5-ac4b-2e81d569ac3c");
                        if (!(d.mkdirs() || d.isDirectory())) {
                            writeline("/home/ubuntu/results/coverage/Copy/Copy_10_10.coverage", "40ec22e3-50f0-4933-a250-a0ea0551ea1c");
                            log("Unable to create directory " + d.getAbsolutePath(), Project.MSG_ERR);
                        } else {
                            writeline("/home/ubuntu/results/coverage/Copy/Copy_10_10.coverage", "9ba8e692-c3f9-4c8f-ae8a-cf3a042324d2");
                            createCount++;
                        }
                    }
                }
            }
            writeline("/home/ubuntu/results/coverage/Copy/Copy_10_10.coverage", "841d1eef-d24e-444e-a934-be8e68fa98c8");
            if (createCount > 0) {
                writeline("/home/ubuntu/results/coverage/Copy/Copy_10_10.coverage", "338d7a4d-04e2-455c-96a5-798220297a88");
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
        writeline("/home/ubuntu/results/coverage/Copy/Copy_10_10.coverage", "2f17510d-53eb-437a-b89e-22bc5e4d4e94");
        if (map.size() > 0) {
            writeline("/home/ubuntu/results/coverage/Copy/Copy_10_10.coverage", "d990982a-f6c5-4333-a675-401a40fd02f4");
            log("Copying " + map.size() + " resource" + (map.size() == 1 ? "" : "s") + " to " + destDir.getAbsolutePath());
            writeline("/home/ubuntu/results/coverage/Copy/Copy_10_10.coverage", "10d1a0f5-06d6-475f-b448-b251fb092a3c");
            for (final Map.Entry<Resource, String[]> e : map.entrySet()) {
                writeline("/home/ubuntu/results/coverage/Copy/Copy_10_10.coverage", "31e0c0fa-a9f6-4fe9-bc80-44228277c330");
                final Resource fromResource = e.getKey();
                writeline("/home/ubuntu/results/coverage/Copy/Copy_10_10.coverage", "f5b9ac09-f27c-49ea-b967-310a05e8d04e");
                for (final String toFile : e.getValue()) {
                    writeline("/home/ubuntu/results/coverage/Copy/Copy_10_10.coverage", "ee254f2b-7609-45a5-8366-759786eda06f");
                    try {
                        writeline("/home/ubuntu/results/coverage/Copy/Copy_10_10.coverage", "f7a5a76d-7401-44ac-883f-9167898adb30");
                        log("Copying " + fromResource + " to " + toFile, verbosity);
                        writeline("/home/ubuntu/results/coverage/Copy/Copy_10_10.coverage", "564b52d9-73e7-4220-b11b-f6886af9df78");
                        final FilterSetCollection executionFilters = new FilterSetCollection();
                        writeline("/home/ubuntu/results/coverage/Copy/Copy_10_10.coverage", "3c7e084f-3acb-436b-a2a0-214ca36ba1cd");
                        if (filtering) {
                            writeline("/home/ubuntu/results/coverage/Copy/Copy_10_10.coverage", "256c8d48-b8f5-46a7-bea5-0489f204e0c9");
                            executionFilters.addFilterSet(getProject().getGlobalFilterSet());
                        }
                        writeline("/home/ubuntu/results/coverage/Copy/Copy_10_10.coverage", "4baa0381-fb11-4e96-b685-a973f94d127a");
                        for (final FilterSet filterSet : filterSets) {
                            writeline("/home/ubuntu/results/coverage/Copy/Copy_10_10.coverage", "ca92ae2c-5195-4a57-b84a-ab205da69d7e");
                            executionFilters.addFilterSet(filterSet);
                        }
                        writeline("/home/ubuntu/results/coverage/Copy/Copy_10_10.coverage", "6ffa04af-cc2b-4120-95b8-c4b475b85dfd");
                        ResourceUtils.copyResource(fromResource, new FileResource(destDir, toFile), executionFilters, filterChains, forceOverwrite, preserveLastModified, /* append: */
                        false, inputEncoding, outputEncoding, getProject(), getForce());
                    } catch (final IOException ioe) {
                        writeline("/home/ubuntu/results/coverage/Copy/Copy_10_10.coverage", "44bc0293-85df-4dc0-960a-f06ffaefc4c1");
                        String msg = "Failed to copy " + fromResource + " to " + toFile + " due to " + getDueTo(ioe);
                        writeline("/home/ubuntu/results/coverage/Copy/Copy_10_10.coverage", "d467f0ce-121a-4b7f-88cc-a24a1f47c89e");
                        final File targetFile = new File(toFile);
                        writeline("/home/ubuntu/results/coverage/Copy/Copy_10_10.coverage", "198f9650-3673-4aec-b762-0a19fb37c5e5");
                        if (!(ioe instanceof ResourceUtils.ReadOnlyTargetFileException) && targetFile.exists() && !targetFile.delete()) {
                            writeline("/home/ubuntu/results/coverage/Copy/Copy_10_10.coverage", "222a8de2-fd95-458b-a971-2e6471ac173e");
                            msg += " and I couldn't delete the corrupt " + toFile;
                        }
                        writeline("/home/ubuntu/results/coverage/Copy/Copy_10_10.coverage", "0977fc69-db3e-4f0d-9e49-94557647a784");
                        if (failonerror) {
                            writeline("/home/ubuntu/results/coverage/Copy/Copy_10_10.coverage", "a8026c65-5d2c-4626-8402-71e0b248f88f");
                            throw new BuildException(msg, ioe, getLocation());
                        }
                        writeline("/home/ubuntu/results/coverage/Copy/Copy_10_10.coverage", "292c9403-8c26-4288-a0aa-ba7565402aa9");
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
        writeline("/home/ubuntu/results/coverage/Copy/Copy_10_10.coverage", "92f189cb-0ff3-44bb-b798-5548a27e48cd");
        return getClass().equals(Copy.class);
    }

    /**
     * Adds the given strings to a list contained in the given map.
     * The file is the key into the map.
     */
    private static void add(File baseDir, final String[] names, final Map<File, List<String>> m) {
        writelineStatic("/home/ubuntu/results/coverage/Copy/Copy_10_10.coverage", "84f4ddd1-9f27-489e-8018-fa363b6af4c8");
        if (names != null) {
            writelineStatic("/home/ubuntu/results/coverage/Copy/Copy_10_10.coverage", "dd2b4c2c-abcb-4d15-96c4-7360615c2ab9");
            baseDir = getKeyFile(baseDir);
            writelineStatic("/home/ubuntu/results/coverage/Copy/Copy_10_10.coverage", "03f73807-7a3d-4b49-8a0a-6d816339bf7d");
            List<String> l = m.get(baseDir);
            writelineStatic("/home/ubuntu/results/coverage/Copy/Copy_10_10.coverage", "5918048e-cb1a-4048-8ace-68da63c0891e");
            if (l == null) {
                writelineStatic("/home/ubuntu/results/coverage/Copy/Copy_10_10.coverage", "85825ebe-31cb-4af1-b8d3-6f6fe810ddd7");
                l = new ArrayList<String>(names.length);
                writelineStatic("/home/ubuntu/results/coverage/Copy/Copy_10_10.coverage", "08097a70-3d66-4388-b54e-89bc6a8865f3");
                m.put(baseDir, l);
            }
            writelineStatic("/home/ubuntu/results/coverage/Copy/Copy_10_10.coverage", "64649d4f-702a-4bdb-ab80-b6662b142acf");
            l.addAll(java.util.Arrays.asList(names));
        }
    }

    /**
     * Adds the given string to a list contained in the given map.
     * The file is the key into the map.
     */
    private static void add(final File baseDir, final String name, final Map<File, List<String>> m) {
        writelineStatic("/home/ubuntu/results/coverage/Copy/Copy_10_10.coverage", "e6f9cce5-35f5-4538-a024-bc0a4f5ced4c");
        if (name != null) {
            writelineStatic("/home/ubuntu/results/coverage/Copy/Copy_10_10.coverage", "3729370a-ee9a-4473-ab25-08edcd2a28d8");
            add(baseDir, new String[] { name }, m);
        }
    }

    /**
     * Either returns its argument or a plaeholder if the argument is null.
     */
    private static File getKeyFile(final File f) {
        writelineStatic("/home/ubuntu/results/coverage/Copy/Copy_10_10.coverage", "9a9ab767-6282-494d-b3a1-880be96893fa");
        return f == null ? NULL_FILE_PLACEHOLDER : f;
    }

    /**
     * returns the mapper to use based on nested elements or the
     * flatten attribute.
     */
    private FileNameMapper getMapper() {
        writeline("/home/ubuntu/results/coverage/Copy/Copy_10_10.coverage", "fb1dd727-fca8-496e-9cf3-a40d9a96644e");
        FileNameMapper mapper = null;
        writeline("/home/ubuntu/results/coverage/Copy/Copy_10_10.coverage", "e04dbe29-33d2-4202-baca-39863b7eb31f");
        if (mapperElement != null) {
            writeline("/home/ubuntu/results/coverage/Copy/Copy_10_10.coverage", "c2511ba6-5db4-43d4-a7ae-4f9f4a58cf55");
            mapper = mapperElement.getImplementation();
        } else if (flatten) {
            writeline("/home/ubuntu/results/coverage/Copy/Copy_10_10.coverage", "ee84a6aa-c9a1-4e38-a03c-7168b4a5d7e7");
            mapper = new FlatFileNameMapper();
        } else {
            writeline("/home/ubuntu/results/coverage/Copy/Copy_10_10.coverage", "a2547d37-a81a-45f0-b9a2-48ad6b643f24");
            mapper = new IdentityMapper();
        }
        writeline("/home/ubuntu/results/coverage/Copy/Copy_10_10.coverage", "ee839b6d-b720-4e41-b725-e443199982fc");
        return mapper;
    }

    /**
     * Handle getMessage() for exceptions.
     * @param ex the exception to handle
     * @return ex.getMessage() if ex.getMessage() is not null
     * otherwise return ex.toString()
     */
    private String getMessage(final Exception ex) {
        writeline("/home/ubuntu/results/coverage/Copy/Copy_10_10.coverage", "7bb6b835-3d4c-4d6a-a5e5-1a850dea758d");
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
        writeline("/home/ubuntu/results/coverage/Copy/Copy_10_10.coverage", "984e9b14-c738-4e99-8ceb-2f094b365845");
        final boolean baseIOException = ex.getClass() == IOException.class;
        writeline("/home/ubuntu/results/coverage/Copy/Copy_10_10.coverage", "489c7daf-3d0b-488d-942e-a265ba261f91");
        final StringBuffer message = new StringBuffer();
        writeline("/home/ubuntu/results/coverage/Copy/Copy_10_10.coverage", "18998786-4846-4507-b299-b8a206256b4e");
        if (!baseIOException || ex.getMessage() == null) {
            writeline("/home/ubuntu/results/coverage/Copy/Copy_10_10.coverage", "5119f5d3-ffac-41e8-a3c2-21ca186057e1");
            message.append(ex.getClass().getName());
        }
        writeline("/home/ubuntu/results/coverage/Copy/Copy_10_10.coverage", "9169d15c-2b29-4ecb-a47e-0e4d4724b7b2");
        if (ex.getMessage() != null) {
            writeline("/home/ubuntu/results/coverage/Copy/Copy_10_10.coverage", "6b6c35ab-034f-410f-b19a-ec1c99d637da");
            if (!baseIOException) {
                writeline("/home/ubuntu/results/coverage/Copy/Copy_10_10.coverage", "66a4dd24-7ff2-4093-8fdc-ebcfd9f8acb0");
                message.append(" ");
            }
            writeline("/home/ubuntu/results/coverage/Copy/Copy_10_10.coverage", "390fcb77-d6cc-4db1-a1ec-83413640c88c");
            message.append(ex.getMessage());
        }
        writeline("/home/ubuntu/results/coverage/Copy/Copy_10_10.coverage", "41db7947-2052-4cc5-9146-8d1c0c815789");
        if (ex.getClass().getName().indexOf("MalformedInput") != -1) {
            writeline("/home/ubuntu/results/coverage/Copy/Copy_10_10.coverage", "8ea153fd-7861-4b12-8832-33a532c40b70");
            message.append(LINE_SEPARATOR);
            writeline("/home/ubuntu/results/coverage/Copy/Copy_10_10.coverage", "f11f3f20-64d9-4ba3-a36a-850524aa28eb");
            message.append("This is normally due to the input file containing invalid");
            writeline("/home/ubuntu/results/coverage/Copy/Copy_10_10.coverage", "60344455-f25a-4612-9642-b0cfb605a33a");
            message.append(LINE_SEPARATOR);
            writeline("/home/ubuntu/results/coverage/Copy/Copy_10_10.coverage", "57bb7acd-5bd0-4cf8-ae1b-86e71eef9382");
            message.append("bytes for the character encoding used : ");
            writeline("/home/ubuntu/results/coverage/Copy/Copy_10_10.coverage", "8b59b3b0-4eea-4441-b0c6-150d7a9f5018");
            message.append((inputEncoding == null ? fileUtils.getDefaultEncoding() : inputEncoding));
            writeline("/home/ubuntu/results/coverage/Copy/Copy_10_10.coverage", "12ac5015-864d-422c-a96d-bddb9bb4ad37");
            message.append(LINE_SEPARATOR);
        }
        writeline("/home/ubuntu/results/coverage/Copy/Copy_10_10.coverage", "43ca6c42-d871-4399-9034-b6009dfc0627");
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
