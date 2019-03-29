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
        writeline("/home/ubuntu/results/coverage/Copy/Copy_9_10.coverage", "e1e97bdc-fe10-442d-b944-2973a4e2b0d9");
        return fileUtils;
    }

    /**
     * Set a single source file to copy.
     * @param file the file to copy.
     */
    public void setFile(final File file) {
        writeline("/home/ubuntu/results/coverage/Copy/Copy_9_10.coverage", "3b08365e-022a-4177-b540-7cfb59be2301");
        this.file = file;
    }

    /**
     * Set the destination file.
     * @param destFile the file to copy to.
     */
    public void setTofile(final File destFile) {
        writeline("/home/ubuntu/results/coverage/Copy/Copy_9_10.coverage", "0a7eefda-19f1-4309-9b5b-e76c972c6776");
        this.destFile = destFile;
    }

    /**
     * Set the destination directory.
     * @param destDir the destination directory.
     */
    public void setTodir(final File destDir) {
        writeline("/home/ubuntu/results/coverage/Copy/Copy_9_10.coverage", "d2643772-1a07-4070-ad9f-5b347d3394c8");
        this.destDir = destDir;
    }

    /**
     * Add a FilterChain.
     * @return a filter chain object.
     */
    public FilterChain createFilterChain() {
        writeline("/home/ubuntu/results/coverage/Copy/Copy_9_10.coverage", "6e11f572-f95c-41bf-a448-2f7a4a6d1e39");
        final FilterChain filterChain = new FilterChain();
        writeline("/home/ubuntu/results/coverage/Copy/Copy_9_10.coverage", "bd6f9f50-a29a-49bb-94e9-cfe80061b0f2");
        filterChains.addElement(filterChain);
        writeline("/home/ubuntu/results/coverage/Copy/Copy_9_10.coverage", "0fbefb74-9175-4754-a208-6e3e7bc44d76");
        return filterChain;
    }

    /**
     * Add a filterset.
     * @return a filter set object.
     */
    public FilterSet createFilterSet() {
        writeline("/home/ubuntu/results/coverage/Copy/Copy_9_10.coverage", "dca533be-b776-4762-b591-f890061790f0");
        final FilterSet filterSet = new FilterSet();
        writeline("/home/ubuntu/results/coverage/Copy/Copy_9_10.coverage", "0b64cc23-6c4b-4098-9d3f-108c38f9d3c0");
        filterSets.addElement(filterSet);
        writeline("/home/ubuntu/results/coverage/Copy/Copy_9_10.coverage", "3b77bc9c-712c-45eb-b1c7-128c08c81ca0");
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
        writeline("/home/ubuntu/results/coverage/Copy/Copy_9_10.coverage", "1d72cf71-cb11-42af-b124-00f56ff0b73a");
        setPreserveLastModified(Project.toBoolean(preserve));
    }

    /**
     * Give the copied files the same last modified time as the original files.
     * @param preserve if true preserve the modified time; default is false.
     */
    public void setPreserveLastModified(final boolean preserve) {
        writeline("/home/ubuntu/results/coverage/Copy/Copy_9_10.coverage", "61b193f2-97dd-4631-81fc-77ea1331c076");
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
        writeline("/home/ubuntu/results/coverage/Copy/Copy_9_10.coverage", "9abaecf2-c698-4647-bcf5-203c0d1a182a");
        return preserveLastModified;
    }

    /**
     * Get the filtersets being applied to this operation.
     *
     * @return a vector of FilterSet objects.
     */
    protected Vector<FilterSet> getFilterSets() {
        writeline("/home/ubuntu/results/coverage/Copy/Copy_9_10.coverage", "793f3d73-19e5-4f4e-aeb3-504d8fe3a819");
        return filterSets;
    }

    /**
     * Get the filterchains being applied to this operation.
     *
     * @return a vector of FilterChain objects.
     */
    protected Vector<FilterChain> getFilterChains() {
        writeline("/home/ubuntu/results/coverage/Copy/Copy_9_10.coverage", "2f1b363e-3c6c-4338-8751-60c48f271c9b");
        return filterChains;
    }

    /**
     * Set filtering mode.
     * @param filtering if true enable filtering; default is false.
     */
    public void setFiltering(final boolean filtering) {
        writeline("/home/ubuntu/results/coverage/Copy/Copy_9_10.coverage", "efd83985-dd40-4812-9ae4-87fd85af80b5");
        this.filtering = filtering;
    }

    /**
     * Set overwrite mode regarding existing destination file(s).
     * @param overwrite if true force overwriting of destination file(s)
     * even if the destination file(s) are younger than
     * the corresponding source file. Default is false.
     */
    public void setOverwrite(final boolean overwrite) {
        writeline("/home/ubuntu/results/coverage/Copy/Copy_9_10.coverage", "f6e8c063-2936-4e63-a301-587ca7085efb");
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
        writeline("/home/ubuntu/results/coverage/Copy/Copy_9_10.coverage", "3c52904b-9389-4d4d-87e2-5b2663b9e69c");
        force = f;
    }

    /**
     * Whether read-only destinations will be overwritten.
     *
     * @since Ant 1.8.2
     */
    public boolean getForce() {
        writeline("/home/ubuntu/results/coverage/Copy/Copy_9_10.coverage", "ccd40dcd-1948-47a3-b452-7282f0379858");
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
        writeline("/home/ubuntu/results/coverage/Copy/Copy_9_10.coverage", "95a57678-ab7f-42ad-93c8-cde849ba6703");
        this.flatten = flatten;
    }

    /**
     * Set verbose mode. Used to force listing of all names of copied files.
     * @param verbose whether to output the names of copied files.
     * Default is false.
     */
    public void setVerbose(final boolean verbose) {
        writeline("/home/ubuntu/results/coverage/Copy/Copy_9_10.coverage", "16d9381f-e024-4ed3-8a12-166bcfa84af8");
        this.verbosity = verbose ? Project.MSG_INFO : Project.MSG_VERBOSE;
    }

    /**
     * Set whether to copy empty directories.
     * @param includeEmpty if true copy empty directories. Default is true.
     */
    public void setIncludeEmptyDirs(final boolean includeEmpty) {
        writeline("/home/ubuntu/results/coverage/Copy/Copy_9_10.coverage", "d97a168b-a5a9-473d-8cf7-c4fa5e1196ce");
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
        writeline("/home/ubuntu/results/coverage/Copy/Copy_9_10.coverage", "0db44d8d-a3fe-4290-8a25-7a08aa3f63a2");
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
        writeline("/home/ubuntu/results/coverage/Copy/Copy_9_10.coverage", "423d74e7-48e6-4629-8401-4c62f34ebd3c");
        this.enableMultipleMappings = enableMultipleMappings;
    }

    /**
     * Get whether multiple mapping is enabled.
     * @return true if multiple mapping is enabled; false otherwise.
     */
    public boolean isEnableMultipleMapping() {
        writeline("/home/ubuntu/results/coverage/Copy/Copy_9_10.coverage", "f33081b3-d20a-47d4-ba15-455c600ef35b");
        return enableMultipleMappings;
    }

    /**
     * Set whether to fail when errors are encountered. If false, note errors
     * to the output but keep going. Default is true.
     * @param failonerror true or false.
     */
    public void setFailOnError(final boolean failonerror) {
        writeline("/home/ubuntu/results/coverage/Copy/Copy_9_10.coverage", "6ef62f6b-570d-4b2c-be17-e81f2666bad6");
        this.failonerror = failonerror;
    }

    /**
     * Add a set of files to copy.
     * @param set a set of files to copy.
     */
    public void addFileset(final FileSet set) {
        writeline("/home/ubuntu/results/coverage/Copy/Copy_9_10.coverage", "cd375588-897a-424c-8c29-3fbb2251b3d9");
        add(set);
    }

    /**
     * Add a collection of files to copy.
     * @param res a resource collection to copy.
     * @since Ant 1.7
     */
    public void add(final ResourceCollection res) {
        writeline("/home/ubuntu/results/coverage/Copy/Copy_9_10.coverage", "58c36cab-3948-473f-9da3-580987d85ee1");
        rcs.add(res);
    }

    /**
     * Define the mapper to map source to destination files.
     * @return a mapper to be configured.
     * @exception BuildException if more than one mapper is defined.
     */
    public Mapper createMapper() throws BuildException {
        writeline("/home/ubuntu/results/coverage/Copy/Copy_9_10.coverage", "f4797019-0803-4311-baf7-34c09858b16e");
        if (mapperElement != null) {
            writeline("/home/ubuntu/results/coverage/Copy/Copy_9_10.coverage", "402e28ab-2cb8-4010-b097-b084bbf63e83");
            throw new BuildException("Cannot define more than one mapper", getLocation());
        }
        writeline("/home/ubuntu/results/coverage/Copy/Copy_9_10.coverage", "d7c4867a-c9a2-478a-be13-08c9eb3fe7b9");
        mapperElement = new Mapper(getProject());
        writeline("/home/ubuntu/results/coverage/Copy/Copy_9_10.coverage", "4ae45833-92aa-49ef-aad6-b9b23fb9e0e1");
        return mapperElement;
    }

    /**
     * Add a nested filenamemapper.
     * @param fileNameMapper the mapper to add.
     * @since Ant 1.6.3
     */
    public void add(final FileNameMapper fileNameMapper) {
        writeline("/home/ubuntu/results/coverage/Copy/Copy_9_10.coverage", "b937100e-4684-49f0-b465-3e6187480fa0");
        createMapper().add(fileNameMapper);
    }

    /**
     * Set the character encoding.
     * @param encoding the character encoding.
     * @since 1.32, Ant 1.5
     */
    public void setEncoding(final String encoding) {
        writeline("/home/ubuntu/results/coverage/Copy/Copy_9_10.coverage", "1ad0330d-c47a-4bcf-8e87-e7e987dab004");
        this.inputEncoding = encoding;
        writeline("/home/ubuntu/results/coverage/Copy/Copy_9_10.coverage", "0cdee881-0bba-46cd-990f-24f91827f4a9");
        if (outputEncoding == null) {
            writeline("/home/ubuntu/results/coverage/Copy/Copy_9_10.coverage", "f0991975-15cf-46b2-b23e-228904ce6ab4");
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
        writeline("/home/ubuntu/results/coverage/Copy/Copy_9_10.coverage", "cf9c7f99-8c81-4990-b7a0-6fa28d48af51");
        return inputEncoding;
    }

    /**
     * Set the character encoding for output files.
     * @param encoding the output character encoding.
     * @since Ant 1.6
     */
    public void setOutputEncoding(final String encoding) {
        writeline("/home/ubuntu/results/coverage/Copy/Copy_9_10.coverage", "bf700e16-65ba-4ff8-b25a-12584036d315");
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
        writeline("/home/ubuntu/results/coverage/Copy/Copy_9_10.coverage", "f55c6a6d-cb4e-4341-a67f-a8223b32245b");
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
        writeline("/home/ubuntu/results/coverage/Copy/Copy_9_10.coverage", "90bf97c6-bf39-43de-b739-83464dc4c865");
        this.granularity = granularity;
    }

    /**
     * Perform the copy operation.
     * @exception BuildException if an error occurs.
     */
    @Override
    public void execute() throws BuildException {
        writeline("/home/ubuntu/results/coverage/Copy/Copy_9_10.coverage", "d2aefeeb-7d1e-4b39-8cc8-d57b1612f40b");
        // may be altered in validateAttributes
        final File savedFile = file;
        writeline("/home/ubuntu/results/coverage/Copy/Copy_9_10.coverage", "68b2de18-bd5f-4a7a-aa77-4930eb501652");
        final File savedDestFile = destFile;
        writeline("/home/ubuntu/results/coverage/Copy/Copy_9_10.coverage", "01ad8f6f-b060-496a-bc56-ab36f779332d");
        final File savedDestDir = destDir;
        writeline("/home/ubuntu/results/coverage/Copy/Copy_9_10.coverage", "78a74223-7a9c-4b6a-a855-863e86f60c74");
        ResourceCollection savedRc = null;
        writeline("/home/ubuntu/results/coverage/Copy/Copy_9_10.coverage", "a72c6af4-8da6-4375-887e-1e9609e923ae");
        if (file == null && destFile != null && rcs.size() == 1) {
            writeline("/home/ubuntu/results/coverage/Copy/Copy_9_10.coverage", "fd00515f-64eb-44b8-b1b0-cbcb2e2032d8");
            // will be removed in validateAttributes
            savedRc = rcs.elementAt(0);
        }
        writeline("/home/ubuntu/results/coverage/Copy/Copy_9_10.coverage", "6ddebe96-9c3a-4de4-87ed-f12959d19874");
        try {
            writeline("/home/ubuntu/results/coverage/Copy/Copy_9_10.coverage", "741a3328-9176-4a11-8286-2b9e9f7bee44");
            // make sure we don't have an illegal set of options
            try {
                writeline("/home/ubuntu/results/coverage/Copy/Copy_9_10.coverage", "3778e950-f07c-4692-b656-06ba95db61a9");
                validateAttributes();
            } catch (final BuildException e) {
                writeline("/home/ubuntu/results/coverage/Copy/Copy_9_10.coverage", "5e7893e1-af65-45c8-adec-f51d74550261");
                if (failonerror || !getMessage(e).equals(MSG_WHEN_COPYING_EMPTY_RC_TO_FILE)) {
                    writeline("/home/ubuntu/results/coverage/Copy/Copy_9_10.coverage", "7f27c78e-dddb-49e4-b882-3cfbb24e810a");
                    throw e;
                } else {
                    writeline("/home/ubuntu/results/coverage/Copy/Copy_9_10.coverage", "f2830379-1b22-4b62-97f2-347403135b14");
                    log("Warning: " + getMessage(e), Project.MSG_ERR);
                    writeline("/home/ubuntu/results/coverage/Copy/Copy_9_10.coverage", "d721f9fd-12ac-40bf-a053-96fded5da3eb");
                    return;
                }
            }
            writeline("/home/ubuntu/results/coverage/Copy/Copy_9_10.coverage", "fc0e11ec-4774-418c-bafc-bbcae35f8c89");
            // deal with the single file
            copySingleFile();
            writeline("/home/ubuntu/results/coverage/Copy/Copy_9_10.coverage", "40b08e1e-ecfa-416d-ba41-3061a9a7a3f6");
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
            writeline("/home/ubuntu/results/coverage/Copy/Copy_9_10.coverage", "60a651f5-6f56-4c60-b16b-ade9b89f00e1");
            final HashMap<File, List<String>> dirsByBasedir = new HashMap<File, List<String>>();
            writeline("/home/ubuntu/results/coverage/Copy/Copy_9_10.coverage", "36a09e03-64dd-4e89-8598-8ffea342b1bb");
            final HashSet<File> baseDirs = new HashSet<File>();
            writeline("/home/ubuntu/results/coverage/Copy/Copy_9_10.coverage", "359de454-b27d-4773-b59a-4fddd02c4802");
            final ArrayList<Resource> nonFileResources = new ArrayList<Resource>();
            writeline("/home/ubuntu/results/coverage/Copy/Copy_9_10.coverage", "9331f732-ce3a-4071-8b5b-17339a9a9bea");
            final int size = rcs.size();
            writeline("/home/ubuntu/results/coverage/Copy/Copy_9_10.coverage", "0f966250-837f-4f55-a314-68af613f9d46");
            for (int i = 0; i < size; i++) {
                writeline("/home/ubuntu/results/coverage/Copy/Copy_9_10.coverage", "8cde7377-21e5-4c7a-ac4b-56d403e272a6");
                final ResourceCollection rc = rcs.elementAt(i);
                writeline("/home/ubuntu/results/coverage/Copy/Copy_9_10.coverage", "252ee7b3-129c-40d0-9415-7849b6085e21");
                // Step (1) - beware of the ZipFileSet
                if (rc instanceof FileSet && rc.isFilesystemOnly()) {
                    writeline("/home/ubuntu/results/coverage/Copy/Copy_9_10.coverage", "4209d91c-c4ed-4cac-9534-71e4375dfbda");
                    final FileSet fs = (FileSet) rc;
                    writeline("/home/ubuntu/results/coverage/Copy/Copy_9_10.coverage", "ccf819ac-5a73-4676-b66f-e020a23a100d");
                    DirectoryScanner ds = null;
                    writeline("/home/ubuntu/results/coverage/Copy/Copy_9_10.coverage", "28f17358-167c-4fc9-8719-fa2dcd184e75");
                    try {
                        writeline("/home/ubuntu/results/coverage/Copy/Copy_9_10.coverage", "f1173da9-e79d-4912-9883-bc85d29f900b");
                        ds = fs.getDirectoryScanner(getProject());
                    } catch (final BuildException e) {
                        writeline("/home/ubuntu/results/coverage/Copy/Copy_9_10.coverage", "215d3262-40b5-4bb6-a86a-6027795fc759");
                        if (failonerror || !getMessage(e).endsWith(DirectoryScanner.DOES_NOT_EXIST_POSTFIX)) {
                            writeline("/home/ubuntu/results/coverage/Copy/Copy_9_10.coverage", "cb04599d-aa33-4381-aa5a-d469c003be11");
                            throw e;
                        } else {
                            writeline("/home/ubuntu/results/coverage/Copy/Copy_9_10.coverage", "ca972d49-6266-468d-82bd-18d4003874ac");
                            if (!quiet) {
                                writeline("/home/ubuntu/results/coverage/Copy/Copy_9_10.coverage", "d3b481c9-27f1-4a66-a17b-9d02a14ae014");
                                log("Warning: " + getMessage(e), Project.MSG_ERR);
                            }
                            writeline("/home/ubuntu/results/coverage/Copy/Copy_9_10.coverage", "c08d9003-3206-4d84-a577-6ca2229ca3c4");
                            continue;
                        }
                    }
                    writeline("/home/ubuntu/results/coverage/Copy/Copy_9_10.coverage", "d2fef928-65de-40a0-aaee-9c7e6722bb0f");
                    final File fromDir = fs.getDir(getProject());
                    writeline("/home/ubuntu/results/coverage/Copy/Copy_9_10.coverage", "2e73f15a-5fb5-492e-add3-ed4b41685d69");
                    final String[] srcFiles = ds.getIncludedFiles();
                    writeline("/home/ubuntu/results/coverage/Copy/Copy_9_10.coverage", "8198e845-3533-4910-842a-9459f5b7b3c3");
                    final String[] srcDirs = ds.getIncludedDirectories();
                    writeline("/home/ubuntu/results/coverage/Copy/Copy_9_10.coverage", "697a4923-aa0e-4fb6-b162-1688f724df7c");
                    if (!flatten && mapperElement == null && ds.isEverythingIncluded() && !fs.hasPatterns()) {
                        writeline("/home/ubuntu/results/coverage/Copy/Copy_9_10.coverage", "46bdfce1-e8d2-4be9-be29-e94ae23e958c");
                        completeDirMap.put(fromDir, destDir);
                    }
                    writeline("/home/ubuntu/results/coverage/Copy/Copy_9_10.coverage", "418db882-3102-480b-958d-ca2debe88c1e");
                    add(fromDir, srcFiles, filesByBasedir);
                    writeline("/home/ubuntu/results/coverage/Copy/Copy_9_10.coverage", "a84e6475-2ca7-416c-b6ca-355394da5e45");
                    add(fromDir, srcDirs, dirsByBasedir);
                    writeline("/home/ubuntu/results/coverage/Copy/Copy_9_10.coverage", "d00c842f-2799-4353-a12d-093dd91f6602");
                    baseDirs.add(fromDir);
                } else {
                    writeline("/home/ubuntu/results/coverage/Copy/Copy_9_10.coverage", "fd978b7a-5070-4665-a730-0d73cc35ea05");
                    if (!rc.isFilesystemOnly() && !supportsNonFileResources()) {
                        writeline("/home/ubuntu/results/coverage/Copy/Copy_9_10.coverage", "96150bd6-fb98-4d61-b540-daf99466d5b7");
                        throw new BuildException("Only FileSystem resources are supported.");
                    }
                    writeline("/home/ubuntu/results/coverage/Copy/Copy_9_10.coverage", "daca2dee-050d-4c7c-843d-535ae59aa805");
                    for (final Resource r : rc) {
                        writeline("/home/ubuntu/results/coverage/Copy/Copy_9_10.coverage", "20969076-ccd1-4801-ad31-48b688ea4186");
                        if (!r.isExists()) {
                            writeline("/home/ubuntu/results/coverage/Copy/Copy_9_10.coverage", "e7d1ad53-b7a4-4c16-a222-dc26f2e440fa");
                            final String message = "Warning: Could not find resource " + r.toLongString() + " to copy.";
                            writeline("/home/ubuntu/results/coverage/Copy/Copy_9_10.coverage", "13972b10-5f23-4f22-b376-2e6146786ea0");
                            if (!failonerror) {
                                writeline("/home/ubuntu/results/coverage/Copy/Copy_9_10.coverage", "32cdf5f6-9f8a-4353-9e9d-2ba0887d847b");
                                if (!quiet) {
                                    writeline("/home/ubuntu/results/coverage/Copy/Copy_9_10.coverage", "a110a34b-a92e-43c8-90b4-a1bbe53033eb");
                                    log(message, Project.MSG_ERR);
                                }
                            } else {
                                writeline("/home/ubuntu/results/coverage/Copy/Copy_9_10.coverage", "f823fb4a-a7c1-4caa-bf88-daa980dcfb95");
                                throw new BuildException(message);
                            }
                            writeline("/home/ubuntu/results/coverage/Copy/Copy_9_10.coverage", "285a58a7-b605-4962-a7dd-d62e1d6a7566");
                            continue;
                        }
                        writeline("/home/ubuntu/results/coverage/Copy/Copy_9_10.coverage", "da556a40-5366-4c68-85ba-fc2fecbc87b6");
                        File baseDir = NULL_FILE_PLACEHOLDER;
                        writeline("/home/ubuntu/results/coverage/Copy/Copy_9_10.coverage", "531f2bac-1dd8-4642-a9dc-2c3438f7288e");
                        String name = r.getName();
                        writeline("/home/ubuntu/results/coverage/Copy/Copy_9_10.coverage", "6dbac5c7-3fd5-44b1-9e29-6c79c699fa1a");
                        final FileProvider fp = r.as(FileProvider.class);
                        writeline("/home/ubuntu/results/coverage/Copy/Copy_9_10.coverage", "d1096d58-b92c-4f53-b0b7-c08b68a44710");
                        if (fp != null) {
                            writeline("/home/ubuntu/results/coverage/Copy/Copy_9_10.coverage", "b34750f6-2ca5-4ab0-8332-a48b2e2b1088");
                            final FileResource fr = ResourceUtils.asFileResource(fp);
                            writeline("/home/ubuntu/results/coverage/Copy/Copy_9_10.coverage", "728a8e8a-8fea-41a8-b879-91c7cbc3a957");
                            baseDir = getKeyFile(fr.getBaseDir());
                            writeline("/home/ubuntu/results/coverage/Copy/Copy_9_10.coverage", "66abf4a8-3bcf-4fe7-b563-f238b72ce224");
                            if (fr.getBaseDir() == null) {
                                writeline("/home/ubuntu/results/coverage/Copy/Copy_9_10.coverage", "76f12c1d-0038-4ea1-820d-24c8fdf18dd8");
                                name = fr.getFile().getAbsolutePath();
                            }
                        }
                        writeline("/home/ubuntu/results/coverage/Copy/Copy_9_10.coverage", "95e4b433-62d8-491d-b1d2-1be01e7ed2bc");
                        // files.
                        if (r.isDirectory() || fp != null) {
                            writeline("/home/ubuntu/results/coverage/Copy/Copy_9_10.coverage", "f86359ea-b45f-4a57-99cb-26a1a9eff95d");
                            add(baseDir, name, r.isDirectory() ? dirsByBasedir : filesByBasedir);
                            writeline("/home/ubuntu/results/coverage/Copy/Copy_9_10.coverage", "3eb64ea6-70fc-4c94-80c7-0325a470f5b4");
                            baseDirs.add(baseDir);
                        } else {
                            writeline("/home/ubuntu/results/coverage/Copy/Copy_9_10.coverage", "ea36836b-9ec6-42e0-8735-be8c9daab439");
                            // a not-directory file resource
                            // needs special treatment
                            nonFileResources.add(r);
                        }
                    }
                }
            }
            writeline("/home/ubuntu/results/coverage/Copy/Copy_9_10.coverage", "c79f51f7-12ff-4a01-988d-656c15b1a7df");
            iterateOverBaseDirs(baseDirs, dirsByBasedir, filesByBasedir);
            writeline("/home/ubuntu/results/coverage/Copy/Copy_9_10.coverage", "7ffcb0b1-d3fd-4aed-aa08-b694d72805b4");
            // do all the copy operations now...
            try {
                writeline("/home/ubuntu/results/coverage/Copy/Copy_9_10.coverage", "eb67385b-2399-495c-b86e-b656057b01e3");
                doFileOperations();
            } catch (final BuildException e) {
                writeline("/home/ubuntu/results/coverage/Copy/Copy_9_10.coverage", "55fa9975-1650-4297-a7e8-c7388e9da568");
                if (!failonerror) {
                    writeline("/home/ubuntu/results/coverage/Copy/Copy_9_10.coverage", "b72ed773-9318-409a-a5fb-7b10279bdd74");
                    if (!quiet) {
                        writeline("/home/ubuntu/results/coverage/Copy/Copy_9_10.coverage", "96bfa82f-0713-420c-b04f-71c14e61f93a");
                        log("Warning: " + getMessage(e), Project.MSG_ERR);
                    }
                } else {
                    writeline("/home/ubuntu/results/coverage/Copy/Copy_9_10.coverage", "a6d4220a-b621-435c-80c8-60369461a6d8");
                    throw e;
                }
            }
            writeline("/home/ubuntu/results/coverage/Copy/Copy_9_10.coverage", "6db2ec20-5bcc-4a71-a94f-60f1fa1c769c");
            if (nonFileResources.size() > 0 || singleResource != null) {
                writeline("/home/ubuntu/results/coverage/Copy/Copy_9_10.coverage", "770b948a-bbe6-45b8-84ec-839db0263dbc");
                final Resource[] nonFiles = nonFileResources.toArray(new Resource[nonFileResources.size()]);
                writeline("/home/ubuntu/results/coverage/Copy/Copy_9_10.coverage", "d2e8cce9-5819-4f00-9f40-d091e5e862ee");
                // restrict to out-of-date resources
                final Map<Resource, String[]> map = scan(nonFiles, destDir);
                writeline("/home/ubuntu/results/coverage/Copy/Copy_9_10.coverage", "68a305d5-2d52-4ce8-bbe7-963f421fc12d");
                if (singleResource != null) {
                    writeline("/home/ubuntu/results/coverage/Copy/Copy_9_10.coverage", "8467ff37-a4d3-471a-b757-e1db2cf25824");
                    map.put(singleResource, new String[] { destFile.getAbsolutePath() });
                }
                writeline("/home/ubuntu/results/coverage/Copy/Copy_9_10.coverage", "ca13d470-6518-4b9f-8d9e-c8578ba6d9ba");
                try {
                    writeline("/home/ubuntu/results/coverage/Copy/Copy_9_10.coverage", "b9e080f7-44c2-42a7-a27a-3d1314a53795");
                    doResourceOperations(map);
                } catch (final BuildException e) {
                    writeline("/home/ubuntu/results/coverage/Copy/Copy_9_10.coverage", "7900ae1b-1e00-4f0c-a37d-7bef52ec2533");
                    if (!failonerror) {
                        writeline("/home/ubuntu/results/coverage/Copy/Copy_9_10.coverage", "6bf9a47b-4795-4f1d-a089-82f381c64c6b");
                        if (!quiet) {
                            writeline("/home/ubuntu/results/coverage/Copy/Copy_9_10.coverage", "b5979bef-8d3c-45a6-bd1c-93dfc20f2e2b");
                            log("Warning: " + getMessage(e), Project.MSG_ERR);
                        }
                    } else {
                        writeline("/home/ubuntu/results/coverage/Copy/Copy_9_10.coverage", "6ffce2ea-d284-4a87-b1a1-1825718a8137");
                        throw e;
                    }
                }
            }
        } finally {
            writeline("/home/ubuntu/results/coverage/Copy/Copy_9_10.coverage", "7f38082d-9a71-4f3b-9081-ebbc03a2079d");
            // clean up again, so this instance can be used a second
            // time
            singleResource = null;
            writeline("/home/ubuntu/results/coverage/Copy/Copy_9_10.coverage", "3dbfde48-182f-49fc-bbf7-49863b76636d");
            file = savedFile;
            writeline("/home/ubuntu/results/coverage/Copy/Copy_9_10.coverage", "aaa2b735-35cf-430c-93e4-abd2aa9523e7");
            destFile = savedDestFile;
            writeline("/home/ubuntu/results/coverage/Copy/Copy_9_10.coverage", "d712aace-5509-4d76-a7f9-00f1e8a9ba92");
            destDir = savedDestDir;
            writeline("/home/ubuntu/results/coverage/Copy/Copy_9_10.coverage", "06dfeb26-8c71-4694-ae86-32018b0587d5");
            if (savedRc != null) {
                writeline("/home/ubuntu/results/coverage/Copy/Copy_9_10.coverage", "d7a61a4f-4a73-4798-949b-8157513dfbfd");
                rcs.insertElementAt(savedRc, 0);
            }
            writeline("/home/ubuntu/results/coverage/Copy/Copy_9_10.coverage", "7ca5848a-c82f-4a16-a5e3-40591a9ab0fd");
            fileCopyMap.clear();
            writeline("/home/ubuntu/results/coverage/Copy/Copy_9_10.coverage", "669807be-5cc2-4b05-9456-a9c60b0b3642");
            dirCopyMap.clear();
            writeline("/home/ubuntu/results/coverage/Copy/Copy_9_10.coverage", "2458c477-1364-41f2-a422-ab44f536965a");
            completeDirMap.clear();
        }
    }

    /**
     * *********************************************************************
     * *  protected and private methods
     * **********************************************************************
     */
    private void copySingleFile() {
        writeline("/home/ubuntu/results/coverage/Copy/Copy_9_10.coverage", "82ed6c79-ba3c-4956-9095-d8e10298328e");
        // deal with the single file
        if (file != null) {
            writeline("/home/ubuntu/results/coverage/Copy/Copy_9_10.coverage", "5b7e41ab-ac90-4aa5-91cb-06af72feaaca");
            if (file.exists()) {
                writeline("/home/ubuntu/results/coverage/Copy/Copy_9_10.coverage", "21898d03-1b84-41eb-948c-b9a21b90bcef");
                if (destFile == null) {
                    writeline("/home/ubuntu/results/coverage/Copy/Copy_9_10.coverage", "50cfb9f7-bf0d-489a-9ab5-ae0106f73fe2");
                    destFile = new File(destDir, file.getName());
                }
                writeline("/home/ubuntu/results/coverage/Copy/Copy_9_10.coverage", "49e27f81-de35-4318-b04d-1f84d98e050e");
                if (forceOverwrite || !destFile.exists() || (file.lastModified() - granularity > destFile.lastModified())) {
                    writeline("/home/ubuntu/results/coverage/Copy/Copy_9_10.coverage", "de8a5481-6f1b-48e4-8249-02518ad63dad");
                    fileCopyMap.put(file.getAbsolutePath(), new String[] { destFile.getAbsolutePath() });
                } else {
                    writeline("/home/ubuntu/results/coverage/Copy/Copy_9_10.coverage", "917d2849-cf0f-4172-a65f-87619048390e");
                    log(file + " omitted as " + destFile + " is up to date.", Project.MSG_VERBOSE);
                }
            } else {
                writeline("/home/ubuntu/results/coverage/Copy/Copy_9_10.coverage", "870c804a-8a51-4ae0-97a5-9582bebccbba");
                final String message = "Warning: Could not find file " + file.getAbsolutePath() + " to copy.";
                writeline("/home/ubuntu/results/coverage/Copy/Copy_9_10.coverage", "befc19b1-0446-4119-b831-185137dbc2d1");
                if (!failonerror) {
                    writeline("/home/ubuntu/results/coverage/Copy/Copy_9_10.coverage", "667d9bba-9fa0-462f-b683-a05860a6fd36");
                    if (!quiet) {
                        writeline("/home/ubuntu/results/coverage/Copy/Copy_9_10.coverage", "c2c339c4-f9e7-4a41-a6d7-b59a891df5a1");
                        log(message, Project.MSG_ERR);
                    }
                } else {
                    writeline("/home/ubuntu/results/coverage/Copy/Copy_9_10.coverage", "7e34116b-218c-40ab-98bb-26e52b22d6ca");
                    throw new BuildException(message);
                }
            }
        }
    }

    private void iterateOverBaseDirs(final HashSet<File> baseDirs, final HashMap<File, List<String>> dirsByBasedir, final HashMap<File, List<String>> filesByBasedir) {
        writeline("/home/ubuntu/results/coverage/Copy/Copy_9_10.coverage", "95dfc3bf-938e-4c7a-864b-05453303165c");
        for (final File f : baseDirs) {
            writeline("/home/ubuntu/results/coverage/Copy/Copy_9_10.coverage", "c2a51bdd-61af-4ea9-8a71-6adcaa5f0c1e");
            final List<String> files = filesByBasedir.get(f);
            writeline("/home/ubuntu/results/coverage/Copy/Copy_9_10.coverage", "04fd9ea4-1aca-4ec3-bda5-6219e3bcb204");
            final List<String> dirs = dirsByBasedir.get(f);
            writeline("/home/ubuntu/results/coverage/Copy/Copy_9_10.coverage", "4e9e2642-a378-4fa7-88e0-15c74455c6a7");
            String[] srcFiles = new String[0];
            writeline("/home/ubuntu/results/coverage/Copy/Copy_9_10.coverage", "94e4723d-7232-45ce-8be7-81714f6e44d5");
            if (files != null) {
                writeline("/home/ubuntu/results/coverage/Copy/Copy_9_10.coverage", "9193d30f-186e-460b-bc5d-cf96c0236037");
                srcFiles = files.toArray(srcFiles);
            }
            writeline("/home/ubuntu/results/coverage/Copy/Copy_9_10.coverage", "4798ade6-f45a-4893-aa06-997ebe943ad6");
            String[] srcDirs = new String[0];
            writeline("/home/ubuntu/results/coverage/Copy/Copy_9_10.coverage", "f1f33cc2-5bce-43b6-9b10-529d700352c8");
            if (dirs != null) {
                writeline("/home/ubuntu/results/coverage/Copy/Copy_9_10.coverage", "e39259c0-33d7-4e33-8752-89fa33227420");
                srcDirs = dirs.toArray(srcDirs);
            }
            writeline("/home/ubuntu/results/coverage/Copy/Copy_9_10.coverage", "fe12dd37-b9bb-482f-8f17-f7d4bd46d02a");
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
        writeline("/home/ubuntu/results/coverage/Copy/Copy_9_10.coverage", "c8583272-ec11-4a0d-ade9-4816387e2682");
        if (file == null && rcs.size() == 0) {
            writeline("/home/ubuntu/results/coverage/Copy/Copy_9_10.coverage", "c73cfafa-c350-47ef-ab35-c020f1e174ad");
            throw new BuildException("Specify at least one source--a file or a resource collection.");
        }
        writeline("/home/ubuntu/results/coverage/Copy/Copy_9_10.coverage", "c107500c-1174-438b-a400-a73fcad707e1");
        if (destFile != null && destDir != null) {
            writeline("/home/ubuntu/results/coverage/Copy/Copy_9_10.coverage", "d7af9230-6b3d-449c-99ad-70ec1534018a");
            throw new BuildException("Only one of tofile and todir may be set.");
        }
        writeline("/home/ubuntu/results/coverage/Copy/Copy_9_10.coverage", "5a0c0145-56ab-4bd5-a828-decacd21ab1c");
        if (destFile == null && destDir == null) {
            writeline("/home/ubuntu/results/coverage/Copy/Copy_9_10.coverage", "48678b7e-016b-4a3c-b2a3-66278c8c6037");
            throw new BuildException("One of tofile or todir must be set.");
        }
        writeline("/home/ubuntu/results/coverage/Copy/Copy_9_10.coverage", "e41770d3-0cb4-4fd9-b80b-6d66fc076c02");
        if (file != null && file.isDirectory()) {
            writeline("/home/ubuntu/results/coverage/Copy/Copy_9_10.coverage", "f6048adc-fb67-4bba-ac8e-1406527c9d3e");
            throw new BuildException("Use a resource collection to copy directories.");
        }
        writeline("/home/ubuntu/results/coverage/Copy/Copy_9_10.coverage", "de36e1a9-b6cd-43a8-909a-21f226feb277");
        if (destFile != null && rcs.size() > 0) {
            writeline("/home/ubuntu/results/coverage/Copy/Copy_9_10.coverage", "f2e140d3-0ae5-47ca-87c1-ee61d9cae2fe");
            if (rcs.size() > 1) {
                writeline("/home/ubuntu/results/coverage/Copy/Copy_9_10.coverage", "92c7f770-ff4f-41aa-b4a1-82c62459ba1f");
                throw new BuildException("Cannot concatenate multiple files into a single file.");
            } else {
                writeline("/home/ubuntu/results/coverage/Copy/Copy_9_10.coverage", "7646aa0e-472c-44cd-a32e-c5d607a37657");
                final ResourceCollection rc = rcs.elementAt(0);
                writeline("/home/ubuntu/results/coverage/Copy/Copy_9_10.coverage", "ce47624f-3962-4ed9-bbdd-b84e691c3189");
                if (!rc.isFilesystemOnly() && !supportsNonFileResources()) {
                    writeline("/home/ubuntu/results/coverage/Copy/Copy_9_10.coverage", "4b6d3da9-1634-43c3-9029-8760169a9cf5");
                    throw new BuildException("Only FileSystem resources are" + " supported.");
                }
                writeline("/home/ubuntu/results/coverage/Copy/Copy_9_10.coverage", "fd77b926-ef90-42f4-81f2-b369d4d60f80");
                if (rc.size() == 0) {
                    writeline("/home/ubuntu/results/coverage/Copy/Copy_9_10.coverage", "f8a371ce-962e-4c10-a05c-003d9b6bd798");
                    throw new BuildException(MSG_WHEN_COPYING_EMPTY_RC_TO_FILE);
                } else if (rc.size() == 1) {
                    writeline("/home/ubuntu/results/coverage/Copy/Copy_9_10.coverage", "41e7ad5c-b276-403c-a7d7-07df66722e2f");
                    final Resource res = rc.iterator().next();
                    writeline("/home/ubuntu/results/coverage/Copy/Copy_9_10.coverage", "28a22405-960c-4fb3-9775-dc9791b65e70");
                    final FileProvider r = res.as(FileProvider.class);
                    writeline("/home/ubuntu/results/coverage/Copy/Copy_9_10.coverage", "02d139e2-0cac-4964-b78d-a87bcb0e8e29");
                    if (file == null) {
                        writeline("/home/ubuntu/results/coverage/Copy/Copy_9_10.coverage", "13292d6f-b2de-4a1f-b956-67533dc3b54c");
                        if (r != null) {
                            writeline("/home/ubuntu/results/coverage/Copy/Copy_9_10.coverage", "8b12424d-7626-4e2a-bc27-62b76d330bf6");
                            file = r.getFile();
                        } else {
                            writeline("/home/ubuntu/results/coverage/Copy/Copy_9_10.coverage", "db4e6b12-c6bc-4fc8-8e44-9fe712e3095f");
                            singleResource = res;
                        }
                        writeline("/home/ubuntu/results/coverage/Copy/Copy_9_10.coverage", "a307ed15-2783-43e9-911e-4d2f85154d29");
                        rcs.removeElementAt(0);
                    } else {
                        writeline("/home/ubuntu/results/coverage/Copy/Copy_9_10.coverage", "9dff7499-83d5-4bbe-b45b-e918bdb8049d");
                        throw new BuildException("Cannot concatenate multiple files into a single file.");
                    }
                } else {
                    writeline("/home/ubuntu/results/coverage/Copy/Copy_9_10.coverage", "57fa988a-3111-45fb-b917-756b5b4628c6");
                    throw new BuildException("Cannot concatenate multiple files into a single file.");
                }
            }
        }
        writeline("/home/ubuntu/results/coverage/Copy/Copy_9_10.coverage", "4380c14f-af58-4d80-8063-29854b2275db");
        if (destFile != null) {
            writeline("/home/ubuntu/results/coverage/Copy/Copy_9_10.coverage", "d82eaf95-219a-4134-9d39-32c3ed04238b");
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
        writeline("/home/ubuntu/results/coverage/Copy/Copy_9_10.coverage", "2a0bb2fb-c669-4614-9688-ecbf266b2df4");
        final FileNameMapper mapper = getMapper();
        writeline("/home/ubuntu/results/coverage/Copy/Copy_9_10.coverage", "5e774338-05ff-4adf-827e-09e42ec6ab37");
        buildMap(fromDir, toDir, files, mapper, fileCopyMap);
        writeline("/home/ubuntu/results/coverage/Copy/Copy_9_10.coverage", "ad10f41f-e32b-4501-83cb-606af528294c");
        if (includeEmpty) {
            writeline("/home/ubuntu/results/coverage/Copy/Copy_9_10.coverage", "0d712cc5-d520-429c-8c59-02ff32fb452e");
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
        writeline("/home/ubuntu/results/coverage/Copy/Copy_9_10.coverage", "bccd9657-49d0-4984-91ef-0af7854e8292");
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
        writeline("/home/ubuntu/results/coverage/Copy/Copy_9_10.coverage", "6f3e927f-26ac-4275-a453-f604efbcf99b");
        String[] toCopy = null;
        writeline("/home/ubuntu/results/coverage/Copy/Copy_9_10.coverage", "3c59034a-4b1c-45cb-aeb9-2482c3d50593");
        if (forceOverwrite) {
            writeline("/home/ubuntu/results/coverage/Copy/Copy_9_10.coverage", "bd96db05-9e3b-46ef-8531-1ab68dd95907");
            final Vector<String> v = new Vector<String>();
            writeline("/home/ubuntu/results/coverage/Copy/Copy_9_10.coverage", "62e545ce-5095-48c0-8810-a89f4cbd8c56");
            for (int i = 0; i < names.length; i++) {
                writeline("/home/ubuntu/results/coverage/Copy/Copy_9_10.coverage", "92013ac4-f115-45e3-9676-35e6a112a832");
                if (mapper.mapFileName(names[i]) != null) {
                    writeline("/home/ubuntu/results/coverage/Copy/Copy_9_10.coverage", "d8015263-85d9-40b4-854f-12c63505df02");
                    v.addElement(names[i]);
                }
            }
            writeline("/home/ubuntu/results/coverage/Copy/Copy_9_10.coverage", "3c3325ce-a38f-4c84-99ec-a2a32f6012ad");
            toCopy = new String[v.size()];
            writeline("/home/ubuntu/results/coverage/Copy/Copy_9_10.coverage", "b8e5e8f7-88f7-4ebe-9e0e-892f07379b67");
            v.copyInto(toCopy);
        } else {
            writeline("/home/ubuntu/results/coverage/Copy/Copy_9_10.coverage", "5f11e6cb-a474-4ac6-bfd7-78bdeabe63a1");
            final SourceFileScanner ds = new SourceFileScanner(this);
            writeline("/home/ubuntu/results/coverage/Copy/Copy_9_10.coverage", "eb9c04a0-329b-4967-8a1a-41e93e9310ce");
            toCopy = ds.restrict(names, fromDir, toDir, mapper, granularity);
        }
        writeline("/home/ubuntu/results/coverage/Copy/Copy_9_10.coverage", "7110fa87-9cf3-4861-9626-721cc88a12c2");
        for (int i = 0; i < toCopy.length; i++) {
            writeline("/home/ubuntu/results/coverage/Copy/Copy_9_10.coverage", "24f3ea76-968c-410f-89f3-a1df6efa2670");
            final File src = new File(fromDir, toCopy[i]);
            writeline("/home/ubuntu/results/coverage/Copy/Copy_9_10.coverage", "179c5482-a3db-440c-897e-5bdf5eabf073");
            final String[] mappedFiles = mapper.mapFileName(toCopy[i]);
            writeline("/home/ubuntu/results/coverage/Copy/Copy_9_10.coverage", "5113e819-9add-41dd-9e8c-081a5d8db726");
            if (!enableMultipleMappings) {
                writeline("/home/ubuntu/results/coverage/Copy/Copy_9_10.coverage", "e4eefcda-bb03-47cc-ad22-ecc592a3a6be");
                map.put(src.getAbsolutePath(), new String[] { new File(toDir, mappedFiles[0]).getAbsolutePath() });
            } else {
                writeline("/home/ubuntu/results/coverage/Copy/Copy_9_10.coverage", "580c81be-304e-4aff-8a2e-0514932922d4");
                // reuse the array created by the mapper
                for (int k = 0; k < mappedFiles.length; k++) {
                    writeline("/home/ubuntu/results/coverage/Copy/Copy_9_10.coverage", "57891337-8fa4-48cf-bdba-8e3d3662dc96");
                    mappedFiles[k] = new File(toDir, mappedFiles[k]).getAbsolutePath();
                }
                writeline("/home/ubuntu/results/coverage/Copy/Copy_9_10.coverage", "737f6b85-66ce-474a-93e3-984a25834fed");
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
        writeline("/home/ubuntu/results/coverage/Copy/Copy_9_10.coverage", "a8d49ef2-dc3d-417b-af12-b723709eec5f");
        final HashMap<Resource, String[]> map = new HashMap<Resource, String[]>();
        writeline("/home/ubuntu/results/coverage/Copy/Copy_9_10.coverage", "afea07d2-9a33-47b3-8d4b-603a255fda62");
        Resource[] toCopy = null;
        writeline("/home/ubuntu/results/coverage/Copy/Copy_9_10.coverage", "610b7e02-0773-46a6-ad82-be0b149844c1");
        if (forceOverwrite) {
            writeline("/home/ubuntu/results/coverage/Copy/Copy_9_10.coverage", "a16b65fd-b294-444c-940e-b9728ecd2a89");
            final Vector<Resource> v = new Vector<Resource>();
            writeline("/home/ubuntu/results/coverage/Copy/Copy_9_10.coverage", "3284b228-c198-44b8-a386-39fe593ccb7c");
            for (int i = 0; i < fromResources.length; i++) {
                writeline("/home/ubuntu/results/coverage/Copy/Copy_9_10.coverage", "5918922e-8695-4294-8ad4-c5cb023decc3");
                if (mapper.mapFileName(fromResources[i].getName()) != null) {
                    writeline("/home/ubuntu/results/coverage/Copy/Copy_9_10.coverage", "26f92e7e-79b6-46d9-95fd-1e4aa353d156");
                    v.addElement(fromResources[i]);
                }
            }
            writeline("/home/ubuntu/results/coverage/Copy/Copy_9_10.coverage", "45c71c6a-694e-4d1d-9e7f-ad8387e1bd62");
            toCopy = new Resource[v.size()];
            writeline("/home/ubuntu/results/coverage/Copy/Copy_9_10.coverage", "54d20050-34d7-453b-a4eb-44e2f881ff33");
            v.copyInto(toCopy);
        } else {
            writeline("/home/ubuntu/results/coverage/Copy/Copy_9_10.coverage", "9b800e84-dadc-4cc3-89a6-59f3a1204fcb");
            toCopy = ResourceUtils.selectOutOfDateSources(this, fromResources, mapper, new ResourceFactory() {

                public Resource getResource(final String name) {
                    return new FileResource(toDir, name);
                }
            }, granularity);
        }
        writeline("/home/ubuntu/results/coverage/Copy/Copy_9_10.coverage", "6fdc574f-869d-4062-be4f-45a2b1c6a693");
        for (int i = 0; i < toCopy.length; i++) {
            writeline("/home/ubuntu/results/coverage/Copy/Copy_9_10.coverage", "f9f97dd3-d73f-496c-867d-05a6e770695c");
            final String[] mappedFiles = mapper.mapFileName(toCopy[i].getName());
            writeline("/home/ubuntu/results/coverage/Copy/Copy_9_10.coverage", "899ef14c-7286-44b7-9119-fe173abdbd69");
            for (int j = 0; j < mappedFiles.length; j++) {
                writeline("/home/ubuntu/results/coverage/Copy/Copy_9_10.coverage", "9a46543d-64da-4eec-904c-c2dceb13d833");
                if (mappedFiles[j] == null) {
                    writeline("/home/ubuntu/results/coverage/Copy/Copy_9_10.coverage", "cde1659f-2ee6-4d39-8dc8-f4ac1c998647");
                    throw new BuildException("Can't copy a resource without a" + " name if the mapper doesn't" + " provide one.");
                }
            }
            writeline("/home/ubuntu/results/coverage/Copy/Copy_9_10.coverage", "ecfc71b2-dc2d-4c91-9a0d-d665ff4c1d41");
            if (!enableMultipleMappings) {
                writeline("/home/ubuntu/results/coverage/Copy/Copy_9_10.coverage", "f5e82947-f2ac-481c-9447-48a79be57ae3");
                map.put(toCopy[i], new String[] { new File(toDir, mappedFiles[0]).getAbsolutePath() });
            } else {
                writeline("/home/ubuntu/results/coverage/Copy/Copy_9_10.coverage", "81b48486-50f3-495a-868e-1103d4a63652");
                // reuse the array created by the mapper
                for (int k = 0; k < mappedFiles.length; k++) {
                    writeline("/home/ubuntu/results/coverage/Copy/Copy_9_10.coverage", "d3cccbdf-d985-49e5-9da2-b149c1a97260");
                    mappedFiles[k] = new File(toDir, mappedFiles[k]).getAbsolutePath();
                }
                writeline("/home/ubuntu/results/coverage/Copy/Copy_9_10.coverage", "8147e597-a9a4-414b-85d1-521838b1ecd5");
                map.put(toCopy[i], mappedFiles);
            }
        }
        writeline("/home/ubuntu/results/coverage/Copy/Copy_9_10.coverage", "01ceb214-9579-4f17-b101-6875d931dd92");
        return map;
    }

    /**
     * Actually does the file (and possibly empty directory) copies.
     * This is a good method for subclasses to override.
     */
    protected void doFileOperations() {
        writeline("/home/ubuntu/results/coverage/Copy/Copy_9_10.coverage", "b5e7ccad-e8af-402b-b0ae-25e68ebd3569");
        if (fileCopyMap.size() > 0) {
            writeline("/home/ubuntu/results/coverage/Copy/Copy_9_10.coverage", "a54cbc1d-cc35-4bb3-86e7-c71181d8e2ee");
            log("Copying " + fileCopyMap.size() + " file" + (fileCopyMap.size() == 1 ? "" : "s") + " to " + destDir.getAbsolutePath());
            writeline("/home/ubuntu/results/coverage/Copy/Copy_9_10.coverage", "68013e04-49de-4bb0-828e-63b925593704");
            for (final Map.Entry<String, String[]> e : fileCopyMap.entrySet()) {
                writeline("/home/ubuntu/results/coverage/Copy/Copy_9_10.coverage", "0a8f73f6-eae3-4680-b5d8-40c376c53df5");
                final String fromFile = e.getKey();
                writeline("/home/ubuntu/results/coverage/Copy/Copy_9_10.coverage", "019c3250-d7d1-4f59-9f0e-f3627a91e624");
                final String[] toFiles = e.getValue();
                writeline("/home/ubuntu/results/coverage/Copy/Copy_9_10.coverage", "60289a52-a523-4686-b1c4-d6d8070270bc");
                for (int i = 0; i < toFiles.length; i++) {
                    writeline("/home/ubuntu/results/coverage/Copy/Copy_9_10.coverage", "747bce9e-8ead-4dad-be9d-4b0f95622006");
                    final String toFile = toFiles[i];
                    writeline("/home/ubuntu/results/coverage/Copy/Copy_9_10.coverage", "599a1535-6e6e-4dcd-aa1f-b33e3daa3c84");
                    if (fromFile.equals(toFile)) {
                        writeline("/home/ubuntu/results/coverage/Copy/Copy_9_10.coverage", "c380f501-bd4a-43ac-a2e1-4db20a44403b");
                        log("Skipping self-copy of " + fromFile, verbosity);
                        writeline("/home/ubuntu/results/coverage/Copy/Copy_9_10.coverage", "9a4e3cc0-fde8-44f8-bbf1-edc7144f0207");
                        continue;
                    }
                    writeline("/home/ubuntu/results/coverage/Copy/Copy_9_10.coverage", "4d1f4634-e062-439a-8ad4-b9fcb9864122");
                    try {
                        writeline("/home/ubuntu/results/coverage/Copy/Copy_9_10.coverage", "7bcd94d1-5151-4766-981e-365d9c6c3aa1");
                        log("Copying " + fromFile + " to " + toFile, verbosity);
                        writeline("/home/ubuntu/results/coverage/Copy/Copy_9_10.coverage", "6b055ab9-e242-4b6e-bb8e-232c6416bc4d");
                        final FilterSetCollection executionFilters = new FilterSetCollection();
                        writeline("/home/ubuntu/results/coverage/Copy/Copy_9_10.coverage", "ab38f5e2-65ab-44a2-b050-b93d380fa426");
                        if (filtering) {
                            writeline("/home/ubuntu/results/coverage/Copy/Copy_9_10.coverage", "9e766232-0a94-4792-9e4e-00ac38cab8ff");
                            executionFilters.addFilterSet(getProject().getGlobalFilterSet());
                        }
                        writeline("/home/ubuntu/results/coverage/Copy/Copy_9_10.coverage", "21bfb86a-8715-40e4-b16b-b8adcf3ce7a7");
                        for (final FilterSet filterSet : filterSets) {
                            writeline("/home/ubuntu/results/coverage/Copy/Copy_9_10.coverage", "32a702d3-37ac-4111-a97d-77fbd2fc1ad5");
                            executionFilters.addFilterSet(filterSet);
                        }
                        writeline("/home/ubuntu/results/coverage/Copy/Copy_9_10.coverage", "69279932-4dfd-4a8e-88ba-011c29d9ec34");
                        fileUtils.copyFile(new File(fromFile), new File(toFile), executionFilters, filterChains, forceOverwrite, preserveLastModified, /* append: */
                        false, inputEncoding, outputEncoding, getProject(), getForce());
                    } catch (final IOException ioe) {
                        writeline("/home/ubuntu/results/coverage/Copy/Copy_9_10.coverage", "b89a3c90-6e41-4d70-b192-fdb1a9a48a65");
                        String msg = "Failed to copy " + fromFile + " to " + toFile + " due to " + getDueTo(ioe);
                        writeline("/home/ubuntu/results/coverage/Copy/Copy_9_10.coverage", "419b6d33-9c88-4ded-bead-56188ad3a1b0");
                        final File targetFile = new File(toFile);
                        writeline("/home/ubuntu/results/coverage/Copy/Copy_9_10.coverage", "3cdf873b-9d5f-4a78-a16f-e438ef9ee85b");
                        if (!(ioe instanceof ResourceUtils.ReadOnlyTargetFileException) && targetFile.exists() && !targetFile.delete()) {
                            writeline("/home/ubuntu/results/coverage/Copy/Copy_9_10.coverage", "2c0cbea1-242f-4a20-82b7-3b795a08438a");
                            msg += " and I couldn't delete the corrupt " + toFile;
                        }
                        writeline("/home/ubuntu/results/coverage/Copy/Copy_9_10.coverage", "c2d21452-05ba-4918-b232-74f928c79eff");
                        if (failonerror) {
                            writeline("/home/ubuntu/results/coverage/Copy/Copy_9_10.coverage", "e2b4794d-cf0d-476d-8d0b-2985cac8ab95");
                            throw new BuildException(msg, ioe, getLocation());
                        }
                        writeline("/home/ubuntu/results/coverage/Copy/Copy_9_10.coverage", "dcdda219-1000-4062-8e8c-c3bae7ec261d");
                        log(msg, Project.MSG_ERR);
                    }
                }
            }
        }
        writeline("/home/ubuntu/results/coverage/Copy/Copy_9_10.coverage", "7bd318b2-ac2f-4b15-811e-4db8c5bb49ae");
        if (includeEmpty) {
            writeline("/home/ubuntu/results/coverage/Copy/Copy_9_10.coverage", "3cf7bc10-6eb3-4d53-8e2c-32ad41212c02");
            int createCount = 0;
            writeline("/home/ubuntu/results/coverage/Copy/Copy_9_10.coverage", "76427994-ed22-439b-a7e8-22109c0fd6b7");
            for (final String[] dirs : dirCopyMap.values()) {
                writeline("/home/ubuntu/results/coverage/Copy/Copy_9_10.coverage", "511d41d9-b010-4b5b-97f7-902bdfd1cc25");
                for (int i = 0; i < dirs.length; i++) {
                    writeline("/home/ubuntu/results/coverage/Copy/Copy_9_10.coverage", "2e728e67-9c45-4f5c-bc76-208fb2caa226");
                    final File d = new File(dirs[i]);
                    writeline("/home/ubuntu/results/coverage/Copy/Copy_9_10.coverage", "1b22996d-0724-4176-892d-c89056858c85");
                    if (!d.exists()) {
                        writeline("/home/ubuntu/results/coverage/Copy/Copy_9_10.coverage", "c44dcc26-2480-4e3d-84e8-6ccc5f5012d4");
                        if (!(d.mkdirs() || d.isDirectory())) {
                            writeline("/home/ubuntu/results/coverage/Copy/Copy_9_10.coverage", "f2d03cc5-abd7-4dd1-8bfc-cb5e0552ad52");
                            log("Unable to create directory " + d.getAbsolutePath(), Project.MSG_ERR);
                        } else {
                            writeline("/home/ubuntu/results/coverage/Copy/Copy_9_10.coverage", "756d872f-e733-4c6b-8a96-1a9edaf6990c");
                            createCount++;
                        }
                    }
                }
            }
            writeline("/home/ubuntu/results/coverage/Copy/Copy_9_10.coverage", "1dd02e4e-2129-4cec-9104-fb0673073686");
            if (createCount > 0) {
                writeline("/home/ubuntu/results/coverage/Copy/Copy_9_10.coverage", "acea8c94-ebe0-46dc-8aaa-e9a2dbc8155f");
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
        writeline("/home/ubuntu/results/coverage/Copy/Copy_9_10.coverage", "b7a9d1fb-244f-4652-97d5-bfdf09430483");
        if (map.size() > 0) {
            writeline("/home/ubuntu/results/coverage/Copy/Copy_9_10.coverage", "b8b5a312-b013-4028-b7f6-d5371225a80b");
            log("Copying " + map.size() + " resource" + (map.size() == 1 ? "" : "s") + " to " + destDir.getAbsolutePath());
            writeline("/home/ubuntu/results/coverage/Copy/Copy_9_10.coverage", "5ece58e7-dccb-4abd-a931-0f6ecab3cae4");
            for (final Map.Entry<Resource, String[]> e : map.entrySet()) {
                writeline("/home/ubuntu/results/coverage/Copy/Copy_9_10.coverage", "5a2aad31-7682-45d9-a127-24925af4e9ab");
                final Resource fromResource = e.getKey();
                writeline("/home/ubuntu/results/coverage/Copy/Copy_9_10.coverage", "10b24649-9754-4880-bd19-93765c08e83a");
                for (final String toFile : e.getValue()) {
                    writeline("/home/ubuntu/results/coverage/Copy/Copy_9_10.coverage", "da2fd97e-1f48-419b-94e8-409f5b82705c");
                    try {
                        writeline("/home/ubuntu/results/coverage/Copy/Copy_9_10.coverage", "743f4bf8-2cf8-4a26-8171-476fceb636b6");
                        log("Copying " + fromResource + " to " + toFile, verbosity);
                        writeline("/home/ubuntu/results/coverage/Copy/Copy_9_10.coverage", "adfd1a79-921d-41c1-9d91-895747ba227e");
                        final FilterSetCollection executionFilters = new FilterSetCollection();
                        writeline("/home/ubuntu/results/coverage/Copy/Copy_9_10.coverage", "affc981b-da3d-45c9-ac99-6d9c31615232");
                        if (filtering) {
                            writeline("/home/ubuntu/results/coverage/Copy/Copy_9_10.coverage", "68965c4a-2e81-4056-9a96-7cb0af7632f1");
                            executionFilters.addFilterSet(getProject().getGlobalFilterSet());
                        }
                        writeline("/home/ubuntu/results/coverage/Copy/Copy_9_10.coverage", "4df63aa3-6855-4571-9c7c-71cf09ef3eb8");
                        for (final FilterSet filterSet : filterSets) {
                            writeline("/home/ubuntu/results/coverage/Copy/Copy_9_10.coverage", "74729282-a711-401d-b347-330320f62012");
                            executionFilters.addFilterSet(filterSet);
                        }
                        writeline("/home/ubuntu/results/coverage/Copy/Copy_9_10.coverage", "f40419ec-4254-42e0-bcb0-e2b1c64c5f65");
                        ResourceUtils.copyResource(fromResource, new FileResource(destDir, toFile), executionFilters, filterChains, forceOverwrite, preserveLastModified, /* append: */
                        false, inputEncoding, outputEncoding, getProject(), getForce());
                    } catch (final IOException ioe) {
                        writeline("/home/ubuntu/results/coverage/Copy/Copy_9_10.coverage", "2196562d-6ed4-46f7-aa7b-f4e44e4d8b72");
                        String msg = "Failed to copy " + fromResource + " to " + toFile + " due to " + getDueTo(ioe);
                        writeline("/home/ubuntu/results/coverage/Copy/Copy_9_10.coverage", "edcdd3ae-45ed-49f7-9921-61ef27b0a886");
                        final File targetFile = new File(toFile);
                        writeline("/home/ubuntu/results/coverage/Copy/Copy_9_10.coverage", "7d0e3916-af7d-4085-b4f2-d57f3afe8c2f");
                        if (!(ioe instanceof ResourceUtils.ReadOnlyTargetFileException) && targetFile.exists() && !targetFile.delete()) {
                            writeline("/home/ubuntu/results/coverage/Copy/Copy_9_10.coverage", "8c3f0aa4-3d3c-446f-8576-e9292cd3c215");
                            msg += " and I couldn't delete the corrupt " + toFile;
                        }
                        writeline("/home/ubuntu/results/coverage/Copy/Copy_9_10.coverage", "98db16f6-aab9-4c75-a8e4-30de31c0192c");
                        if (failonerror) {
                            writeline("/home/ubuntu/results/coverage/Copy/Copy_9_10.coverage", "53d4f981-b535-42a0-aad2-7cc053459b9c");
                            throw new BuildException(msg, ioe, getLocation());
                        }
                        writeline("/home/ubuntu/results/coverage/Copy/Copy_9_10.coverage", "6eab2345-876c-4b8a-904a-383ae26fa426");
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
        writeline("/home/ubuntu/results/coverage/Copy/Copy_9_10.coverage", "fee25ed3-39df-471a-9fd7-4906a9f4832f");
        return getClass().equals(Copy.class);
    }

    /**
     * Adds the given strings to a list contained in the given map.
     * The file is the key into the map.
     */
    private static void add(File baseDir, final String[] names, final Map<File, List<String>> m) {
        writelineStatic("/home/ubuntu/results/coverage/Copy/Copy_9_10.coverage", "068ed32d-1617-400d-b762-306cfebeeb34");
        if (names != null) {
            writelineStatic("/home/ubuntu/results/coverage/Copy/Copy_9_10.coverage", "74857b62-6a6c-496f-ae40-4ac0bcac0ca9");
            baseDir = getKeyFile(baseDir);
            writelineStatic("/home/ubuntu/results/coverage/Copy/Copy_9_10.coverage", "930b9671-b00e-4778-8d00-46d3c4adc7ed");
            List<String> l = m.get(baseDir);
            writelineStatic("/home/ubuntu/results/coverage/Copy/Copy_9_10.coverage", "1ae9178d-6c11-4bb0-86dc-f5913f100498");
            if (l == null) {
                writelineStatic("/home/ubuntu/results/coverage/Copy/Copy_9_10.coverage", "5269dd9b-b492-479e-a266-48b5d2747e2e");
                l = new ArrayList<String>(names.length);
                writelineStatic("/home/ubuntu/results/coverage/Copy/Copy_9_10.coverage", "3ffdd548-d79b-42aa-81e1-85820ac8459b");
                m.put(baseDir, l);
            }
            writelineStatic("/home/ubuntu/results/coverage/Copy/Copy_9_10.coverage", "3b4b8b3e-9846-45b7-bb86-53c4704812bb");
            l.addAll(java.util.Arrays.asList(names));
        }
    }

    /**
     * Adds the given string to a list contained in the given map.
     * The file is the key into the map.
     */
    private static void add(final File baseDir, final String name, final Map<File, List<String>> m) {
        writelineStatic("/home/ubuntu/results/coverage/Copy/Copy_9_10.coverage", "865bdba8-3d7e-4863-af6c-ba95951ee0f5");
        if (name != null) {
            writelineStatic("/home/ubuntu/results/coverage/Copy/Copy_9_10.coverage", "4a84c99f-4ae4-4ade-aadd-2a1d18847710");
            add(baseDir, new String[] { name }, m);
        }
    }

    /**
     * Either returns its argument or a plaeholder if the argument is null.
     */
    private static File getKeyFile(final File f) {
        writelineStatic("/home/ubuntu/results/coverage/Copy/Copy_9_10.coverage", "1ac6801b-da93-4708-b599-4b4a82165907");
        return f == null ? NULL_FILE_PLACEHOLDER : f;
    }

    /**
     * returns the mapper to use based on nested elements or the
     * flatten attribute.
     */
    private FileNameMapper getMapper() {
        writeline("/home/ubuntu/results/coverage/Copy/Copy_9_10.coverage", "b21bd2a9-dc89-47ec-9af6-2a9d1a3265ed");
        FileNameMapper mapper = null;
        writeline("/home/ubuntu/results/coverage/Copy/Copy_9_10.coverage", "f9ad422e-08b5-4779-8cf3-ce5adfd59bd9");
        if (mapperElement != null) {
            writeline("/home/ubuntu/results/coverage/Copy/Copy_9_10.coverage", "2ee72a63-2301-434d-bc56-d50bbe99ecdd");
            mapper = mapperElement.getImplementation();
        } else if (flatten) {
            writeline("/home/ubuntu/results/coverage/Copy/Copy_9_10.coverage", "5762e546-6d77-4041-a3ce-0dbdbd4a4610");
            mapper = new FlatFileNameMapper();
        } else {
            writeline("/home/ubuntu/results/coverage/Copy/Copy_9_10.coverage", "ba07ae73-b829-47b7-9321-68d9e9346eba");
            mapper = new IdentityMapper();
        }
        writeline("/home/ubuntu/results/coverage/Copy/Copy_9_10.coverage", "73878aaf-576b-4f79-ab9f-752e5bb99252");
        return mapper;
    }

    /**
     * Handle getMessage() for exceptions.
     * @param ex the exception to handle
     * @return ex.getMessage() if ex.getMessage() is not null
     * otherwise return ex.toString()
     */
    private String getMessage(final Exception ex) {
        writeline("/home/ubuntu/results/coverage/Copy/Copy_9_10.coverage", "72edcd2a-311b-494f-ac65-5eec4d3a7701");
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
        writeline("/home/ubuntu/results/coverage/Copy/Copy_9_10.coverage", "61a63800-e157-427d-8807-60f8534bc09c");
        final boolean baseIOException = ex.getClass() == IOException.class;
        writeline("/home/ubuntu/results/coverage/Copy/Copy_9_10.coverage", "93fc896b-fa54-4c63-a1f5-7d7c14eea089");
        final StringBuffer message = new StringBuffer();
        writeline("/home/ubuntu/results/coverage/Copy/Copy_9_10.coverage", "9b349be7-186d-4996-9b8c-a48d1bb11b42");
        if (!baseIOException || ex.getMessage() == null) {
            writeline("/home/ubuntu/results/coverage/Copy/Copy_9_10.coverage", "4d298fcc-1304-4bb6-a728-9a6187708df5");
            message.append(ex.getClass().getName());
        }
        writeline("/home/ubuntu/results/coverage/Copy/Copy_9_10.coverage", "4fefb67c-9cb1-45d5-bcbd-a5f999a79240");
        if (ex.getMessage() != null) {
            writeline("/home/ubuntu/results/coverage/Copy/Copy_9_10.coverage", "80a44a4b-a48c-4774-9834-a1b8c4abd694");
            if (!baseIOException) {
                writeline("/home/ubuntu/results/coverage/Copy/Copy_9_10.coverage", "ae00fb8b-36e1-462d-9f36-f0d96f173d0a");
                message.append(" ");
            }
            writeline("/home/ubuntu/results/coverage/Copy/Copy_9_10.coverage", "dbadcf9c-96b6-4540-b4f2-6d4588e44a74");
            message.append(ex.getMessage());
        }
        writeline("/home/ubuntu/results/coverage/Copy/Copy_9_10.coverage", "e8918fac-af72-451d-a815-8c44b31d6f58");
        if (ex.getClass().getName().indexOf("MalformedInput") != -1) {
            writeline("/home/ubuntu/results/coverage/Copy/Copy_9_10.coverage", "534ade86-2d44-4c0d-b5a5-010ced6e3cf5");
            message.append(LINE_SEPARATOR);
            writeline("/home/ubuntu/results/coverage/Copy/Copy_9_10.coverage", "8a51a1fb-91e0-492b-a0c2-36317a8a1c60");
            message.append("This is normally due to the input file containing invalid");
            writeline("/home/ubuntu/results/coverage/Copy/Copy_9_10.coverage", "74726d72-a311-45dd-b57f-724df7119447");
            message.append(LINE_SEPARATOR);
            writeline("/home/ubuntu/results/coverage/Copy/Copy_9_10.coverage", "de6158f9-1a8b-48a7-9aa9-e9ebd6e37bb7");
            message.append("bytes for the character encoding used : ");
            writeline("/home/ubuntu/results/coverage/Copy/Copy_9_10.coverage", "35387e17-6543-43d1-8b6b-dd093710e34e");
            message.append((inputEncoding == null ? fileUtils.getDefaultEncoding() : inputEncoding));
            writeline("/home/ubuntu/results/coverage/Copy/Copy_9_10.coverage", "f0e5f33c-d69f-4a45-8657-0dd18749c7ad");
            message.append(LINE_SEPARATOR);
        }
        writeline("/home/ubuntu/results/coverage/Copy/Copy_9_10.coverage", "aa4f9925-24fa-42b0-98cb-d6980fa6a89a");
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
