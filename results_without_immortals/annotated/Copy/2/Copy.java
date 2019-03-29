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
        writeline("/home/ubuntu/results/coverage/Copy/Copy_2_10.coverage", "418fd1c1-fa6a-4261-b427-edb132fc66d0");
        return fileUtils;
    }

    /**
     * Set a single source file to copy.
     * @param file the file to copy.
     */
    public void setFile(final File file) {
        writeline("/home/ubuntu/results/coverage/Copy/Copy_2_10.coverage", "dc670242-b30b-4def-ac74-467559960741");
        this.file = file;
    }

    /**
     * Set the destination file.
     * @param destFile the file to copy to.
     */
    public void setTofile(final File destFile) {
        writeline("/home/ubuntu/results/coverage/Copy/Copy_2_10.coverage", "35c7a30c-c8ce-4d57-9f8e-ea74867e1801");
        this.destFile = destFile;
    }

    /**
     * Set the destination directory.
     * @param destDir the destination directory.
     */
    public void setTodir(final File destDir) {
        writeline("/home/ubuntu/results/coverage/Copy/Copy_2_10.coverage", "1b412f1b-614f-47c8-b15a-d8af1bd69ebd");
        this.destDir = destDir;
    }

    /**
     * Add a FilterChain.
     * @return a filter chain object.
     */
    public FilterChain createFilterChain() {
        writeline("/home/ubuntu/results/coverage/Copy/Copy_2_10.coverage", "95bb99ee-4434-490c-9d00-f5a7ec76ce42");
        final FilterChain filterChain = new FilterChain();
        writeline("/home/ubuntu/results/coverage/Copy/Copy_2_10.coverage", "56111541-cacf-412e-9ed0-0f71cb911d3d");
        filterChains.addElement(filterChain);
        writeline("/home/ubuntu/results/coverage/Copy/Copy_2_10.coverage", "c1c71866-6394-487c-9d50-744d2d6f8e01");
        return filterChain;
    }

    /**
     * Add a filterset.
     * @return a filter set object.
     */
    public FilterSet createFilterSet() {
        writeline("/home/ubuntu/results/coverage/Copy/Copy_2_10.coverage", "2ed569cd-fb07-41ce-9efc-815ab7f21508");
        final FilterSet filterSet = new FilterSet();
        writeline("/home/ubuntu/results/coverage/Copy/Copy_2_10.coverage", "d7fbb47e-c5d3-4c74-8557-5af5f4f69735");
        filterSets.addElement(filterSet);
        writeline("/home/ubuntu/results/coverage/Copy/Copy_2_10.coverage", "04e99504-7b1f-4860-9c13-bf5824856672");
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
        writeline("/home/ubuntu/results/coverage/Copy/Copy_2_10.coverage", "e199897b-d64f-4a41-8939-37c0df04db9d");
        setPreserveLastModified(Project.toBoolean(preserve));
    }

    /**
     * Give the copied files the same last modified time as the original files.
     * @param preserve if true preserve the modified time; default is false.
     */
    public void setPreserveLastModified(final boolean preserve) {
        writeline("/home/ubuntu/results/coverage/Copy/Copy_2_10.coverage", "ad6b0e0b-73db-4554-8744-ebcc6a15a6e0");
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
        writeline("/home/ubuntu/results/coverage/Copy/Copy_2_10.coverage", "d110e1e6-8951-4ecf-ae8b-939c9a8c5957");
        return preserveLastModified;
    }

    /**
     * Get the filtersets being applied to this operation.
     *
     * @return a vector of FilterSet objects.
     */
    protected Vector<FilterSet> getFilterSets() {
        writeline("/home/ubuntu/results/coverage/Copy/Copy_2_10.coverage", "eaf5dc82-ac92-4571-9329-fdfb53c8a623");
        return filterSets;
    }

    /**
     * Get the filterchains being applied to this operation.
     *
     * @return a vector of FilterChain objects.
     */
    protected Vector<FilterChain> getFilterChains() {
        writeline("/home/ubuntu/results/coverage/Copy/Copy_2_10.coverage", "c9fd0269-29e3-490d-8a91-2eb011ffc7e7");
        return filterChains;
    }

    /**
     * Set filtering mode.
     * @param filtering if true enable filtering; default is false.
     */
    public void setFiltering(final boolean filtering) {
        writeline("/home/ubuntu/results/coverage/Copy/Copy_2_10.coverage", "178f3383-c05f-432f-b539-2cf2eea180ff");
        this.filtering = filtering;
    }

    /**
     * Set overwrite mode regarding existing destination file(s).
     * @param overwrite if true force overwriting of destination file(s)
     * even if the destination file(s) are younger than
     * the corresponding source file. Default is false.
     */
    public void setOverwrite(final boolean overwrite) {
        writeline("/home/ubuntu/results/coverage/Copy/Copy_2_10.coverage", "147412a0-2aab-462c-869b-7d49eeb85fff");
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
        writeline("/home/ubuntu/results/coverage/Copy/Copy_2_10.coverage", "d6808ae0-e169-47b5-b7ea-b3040471c6af");
        force = f;
    }

    /**
     * Whether read-only destinations will be overwritten.
     *
     * @since Ant 1.8.2
     */
    public boolean getForce() {
        writeline("/home/ubuntu/results/coverage/Copy/Copy_2_10.coverage", "ec78e1a8-0d01-4ad3-9a9b-bacb670d5736");
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
        writeline("/home/ubuntu/results/coverage/Copy/Copy_2_10.coverage", "a0b63e7a-f6fc-4090-a37a-1df5e11fad83");
        this.flatten = flatten;
    }

    /**
     * Set verbose mode. Used to force listing of all names of copied files.
     * @param verbose whether to output the names of copied files.
     * Default is false.
     */
    public void setVerbose(final boolean verbose) {
        writeline("/home/ubuntu/results/coverage/Copy/Copy_2_10.coverage", "b5aa5d36-9659-4a88-b908-e4aa09f72fdd");
        this.verbosity = verbose ? Project.MSG_INFO : Project.MSG_VERBOSE;
    }

    /**
     * Set whether to copy empty directories.
     * @param includeEmpty if true copy empty directories. Default is true.
     */
    public void setIncludeEmptyDirs(final boolean includeEmpty) {
        writeline("/home/ubuntu/results/coverage/Copy/Copy_2_10.coverage", "9099ffb8-2d83-4ad6-85a6-088cb451bcdf");
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
        writeline("/home/ubuntu/results/coverage/Copy/Copy_2_10.coverage", "ee392484-cf03-463f-91fe-7bf6e4d1fa36");
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
        writeline("/home/ubuntu/results/coverage/Copy/Copy_2_10.coverage", "2a815af0-ee7b-4b13-91ab-c5ae91804a28");
        this.enableMultipleMappings = enableMultipleMappings;
    }

    /**
     * Get whether multiple mapping is enabled.
     * @return true if multiple mapping is enabled; false otherwise.
     */
    public boolean isEnableMultipleMapping() {
        writeline("/home/ubuntu/results/coverage/Copy/Copy_2_10.coverage", "15aa7281-7ad1-425e-9fa4-3e627bedf6f8");
        return enableMultipleMappings;
    }

    /**
     * Set whether to fail when errors are encountered. If false, note errors
     * to the output but keep going. Default is true.
     * @param failonerror true or false.
     */
    public void setFailOnError(final boolean failonerror) {
        writeline("/home/ubuntu/results/coverage/Copy/Copy_2_10.coverage", "8333bc18-6e02-421f-9a47-3d46f32e1997");
        this.failonerror = failonerror;
    }

    /**
     * Add a set of files to copy.
     * @param set a set of files to copy.
     */
    public void addFileset(final FileSet set) {
        writeline("/home/ubuntu/results/coverage/Copy/Copy_2_10.coverage", "99b18a88-63b0-431b-bf18-83696ba78c40");
        add(set);
    }

    /**
     * Add a collection of files to copy.
     * @param res a resource collection to copy.
     * @since Ant 1.7
     */
    public void add(final ResourceCollection res) {
        writeline("/home/ubuntu/results/coverage/Copy/Copy_2_10.coverage", "64b386be-158c-42ba-99b9-33093567f1b0");
        rcs.add(res);
    }

    /**
     * Define the mapper to map source to destination files.
     * @return a mapper to be configured.
     * @exception BuildException if more than one mapper is defined.
     */
    public Mapper createMapper() throws BuildException {
        writeline("/home/ubuntu/results/coverage/Copy/Copy_2_10.coverage", "e4fe974e-fbc0-4a52-8c72-7c0fb4d2a29f");
        if (mapperElement != null) {
            writeline("/home/ubuntu/results/coverage/Copy/Copy_2_10.coverage", "3f7e970f-de94-4d58-9b4a-6287ce73d254");
            throw new BuildException("Cannot define more than one mapper", getLocation());
        }
        writeline("/home/ubuntu/results/coverage/Copy/Copy_2_10.coverage", "0e8b5840-e6ee-49be-86a9-f2b1ae5faebf");
        mapperElement = new Mapper(getProject());
        writeline("/home/ubuntu/results/coverage/Copy/Copy_2_10.coverage", "5126fc57-7498-4ebd-b479-553c35eff1f3");
        return mapperElement;
    }

    /**
     * Add a nested filenamemapper.
     * @param fileNameMapper the mapper to add.
     * @since Ant 1.6.3
     */
    public void add(final FileNameMapper fileNameMapper) {
        writeline("/home/ubuntu/results/coverage/Copy/Copy_2_10.coverage", "486a1963-16b0-429b-93b8-ccab12ca53cc");
        createMapper().add(fileNameMapper);
    }

    /**
     * Set the character encoding.
     * @param encoding the character encoding.
     * @since 1.32, Ant 1.5
     */
    public void setEncoding(final String encoding) {
        writeline("/home/ubuntu/results/coverage/Copy/Copy_2_10.coverage", "0cc14912-6f5d-49b0-9336-bedbaaddc819");
        this.inputEncoding = encoding;
        writeline("/home/ubuntu/results/coverage/Copy/Copy_2_10.coverage", "6b765d78-2ef5-4f27-b311-367c2d3c9f30");
        if (outputEncoding == null) {
            writeline("/home/ubuntu/results/coverage/Copy/Copy_2_10.coverage", "9a01b23a-28e5-4183-8433-e7714e51ffd2");
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
        writeline("/home/ubuntu/results/coverage/Copy/Copy_2_10.coverage", "a83a5148-24c2-4fb3-a407-643f362ff55b");
        return inputEncoding;
    }

    /**
     * Set the character encoding for output files.
     * @param encoding the output character encoding.
     * @since Ant 1.6
     */
    public void setOutputEncoding(final String encoding) {
        writeline("/home/ubuntu/results/coverage/Copy/Copy_2_10.coverage", "95df4b31-d453-49d8-8da8-ab24f70bc569");
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
        writeline("/home/ubuntu/results/coverage/Copy/Copy_2_10.coverage", "e5a214cc-ffe2-4711-bbf0-937d51eab803");
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
        writeline("/home/ubuntu/results/coverage/Copy/Copy_2_10.coverage", "92e860ca-9e10-46ea-a177-5cac09f98a9f");
        this.granularity = granularity;
    }

    /**
     * Perform the copy operation.
     * @exception BuildException if an error occurs.
     */
    @Override
    public void execute() throws BuildException {
        writeline("/home/ubuntu/results/coverage/Copy/Copy_2_10.coverage", "bf1c8951-c733-4497-a1e9-ed34a61633f2");
        // may be altered in validateAttributes
        final File savedFile = file;
        writeline("/home/ubuntu/results/coverage/Copy/Copy_2_10.coverage", "5eb6613a-8467-46ba-a03c-9ecff4a99f07");
        final File savedDestFile = destFile;
        writeline("/home/ubuntu/results/coverage/Copy/Copy_2_10.coverage", "3bbdeaaf-15b1-4a5c-a0fa-2292790cdd81");
        final File savedDestDir = destDir;
        writeline("/home/ubuntu/results/coverage/Copy/Copy_2_10.coverage", "0f96ebcd-d12a-495e-b07c-01f968659af2");
        ResourceCollection savedRc = null;
        writeline("/home/ubuntu/results/coverage/Copy/Copy_2_10.coverage", "9c434d5b-e8a4-45fc-a4a6-3a55afb7e88c");
        if (file == null && destFile != null && rcs.size() == 1) {
            writeline("/home/ubuntu/results/coverage/Copy/Copy_2_10.coverage", "d4cb8c21-842e-44cf-acd6-9db005f77def");
            // will be removed in validateAttributes
            savedRc = rcs.elementAt(0);
        }
        writeline("/home/ubuntu/results/coverage/Copy/Copy_2_10.coverage", "134eeba6-d3cd-402c-ba68-044368f33e68");
        try {
            writeline("/home/ubuntu/results/coverage/Copy/Copy_2_10.coverage", "7ae9e60c-8096-4f3e-ad90-2e9ef765efad");
            // make sure we don't have an illegal set of options
            try {
                writeline("/home/ubuntu/results/coverage/Copy/Copy_2_10.coverage", "9ce7322f-7dae-4456-86a4-9d209616e2a4");
                validateAttributes();
            } catch (final BuildException e) {
                writeline("/home/ubuntu/results/coverage/Copy/Copy_2_10.coverage", "6db92b0a-1a2e-463a-9a09-bd2f9b4e677b");
                if (failonerror || !getMessage(e).equals(MSG_WHEN_COPYING_EMPTY_RC_TO_FILE)) {
                    writeline("/home/ubuntu/results/coverage/Copy/Copy_2_10.coverage", "c6b29729-8ca4-4750-8afd-3175793ad888");
                    throw e;
                } else {
                    writeline("/home/ubuntu/results/coverage/Copy/Copy_2_10.coverage", "8caff015-8e15-459f-8224-49b4bb5caf8e");
                    log("Warning: " + getMessage(e), Project.MSG_ERR);
                    writeline("/home/ubuntu/results/coverage/Copy/Copy_2_10.coverage", "ba16dd09-3f4d-457b-8888-732ea2c2ee9a");
                    return;
                }
            }
            writeline("/home/ubuntu/results/coverage/Copy/Copy_2_10.coverage", "05d2ea79-1e68-402b-ab03-3ed7cfb2005f");
            // deal with the single file
            copySingleFile();
            writeline("/home/ubuntu/results/coverage/Copy/Copy_2_10.coverage", "b187ff76-26ce-4d46-80b9-d028eb618cff");
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
            writeline("/home/ubuntu/results/coverage/Copy/Copy_2_10.coverage", "3cb629d2-791d-4086-91d4-9fac73081747");
            final HashMap<File, List<String>> dirsByBasedir = new HashMap<File, List<String>>();
            writeline("/home/ubuntu/results/coverage/Copy/Copy_2_10.coverage", "f8225376-e9c5-469d-bc85-6655b9042b07");
            final HashSet<File> baseDirs = new HashSet<File>();
            writeline("/home/ubuntu/results/coverage/Copy/Copy_2_10.coverage", "a2082cb4-b607-4e95-9369-ff1e146c04b9");
            final ArrayList<Resource> nonFileResources = new ArrayList<Resource>();
            writeline("/home/ubuntu/results/coverage/Copy/Copy_2_10.coverage", "768e5cc7-84f9-4a27-9305-d86af7e9c391");
            final int size = rcs.size();
            writeline("/home/ubuntu/results/coverage/Copy/Copy_2_10.coverage", "11858b84-32b8-4d8c-a6f2-084415069179");
            for (int i = 0; i < size; i++) {
                writeline("/home/ubuntu/results/coverage/Copy/Copy_2_10.coverage", "e42456c5-39d8-4970-8f45-340b31317c83");
                final ResourceCollection rc = rcs.elementAt(i);
                writeline("/home/ubuntu/results/coverage/Copy/Copy_2_10.coverage", "15d1d4b9-26b9-4c29-89fa-30213dbb9cbc");
                // Step (1) - beware of the ZipFileSet
                if (rc instanceof FileSet && rc.isFilesystemOnly()) {
                    writeline("/home/ubuntu/results/coverage/Copy/Copy_2_10.coverage", "b4556aa3-54ec-4eb3-a7cf-f8c1a40ad592");
                    final FileSet fs = (FileSet) rc;
                    writeline("/home/ubuntu/results/coverage/Copy/Copy_2_10.coverage", "ce5d4efc-f1dc-40c8-85b6-1da2e2c863e2");
                    DirectoryScanner ds = null;
                    writeline("/home/ubuntu/results/coverage/Copy/Copy_2_10.coverage", "8cd21a43-b813-4028-85c4-93e700a3df2f");
                    try {
                        writeline("/home/ubuntu/results/coverage/Copy/Copy_2_10.coverage", "b2141d28-68c9-4d6f-a8d9-18c3e63b754e");
                        ds = fs.getDirectoryScanner(getProject());
                    } catch (final BuildException e) {
                        writeline("/home/ubuntu/results/coverage/Copy/Copy_2_10.coverage", "d46390a8-04b2-43d9-890f-034739faad31");
                        if (failonerror || !getMessage(e).endsWith(DirectoryScanner.DOES_NOT_EXIST_POSTFIX)) {
                            writeline("/home/ubuntu/results/coverage/Copy/Copy_2_10.coverage", "2c082663-adb3-45ce-b6a7-ee6ac02b2028");
                            throw e;
                        } else {
                            writeline("/home/ubuntu/results/coverage/Copy/Copy_2_10.coverage", "94f016b3-3c98-4b33-8e87-e0edd8caa628");
                            if (!quiet) {
                                writeline("/home/ubuntu/results/coverage/Copy/Copy_2_10.coverage", "d0113dd2-ef1c-4c21-9f83-b1600ce78959");
                                log("Warning: " + getMessage(e), Project.MSG_ERR);
                            }
                            writeline("/home/ubuntu/results/coverage/Copy/Copy_2_10.coverage", "344b8bda-c451-448e-a692-26e246601590");
                            continue;
                        }
                    }
                    writeline("/home/ubuntu/results/coverage/Copy/Copy_2_10.coverage", "235339be-00d7-49f9-a388-55e2998a6287");
                    final File fromDir = fs.getDir(getProject());
                    writeline("/home/ubuntu/results/coverage/Copy/Copy_2_10.coverage", "683a761c-eb8a-4c6b-a68d-520a809b93c9");
                    final String[] srcFiles = ds.getIncludedFiles();
                    writeline("/home/ubuntu/results/coverage/Copy/Copy_2_10.coverage", "ee5307ef-863f-4915-be60-113e0b5931bf");
                    final String[] srcDirs = ds.getIncludedDirectories();
                    writeline("/home/ubuntu/results/coverage/Copy/Copy_2_10.coverage", "ca17b23f-d01b-4715-88ea-b1156e8c152e");
                    if (!flatten && mapperElement == null && ds.isEverythingIncluded() && !fs.hasPatterns()) {
                        writeline("/home/ubuntu/results/coverage/Copy/Copy_2_10.coverage", "84986262-55b8-48f8-b634-785b7e912bee");
                        completeDirMap.put(fromDir, destDir);
                    }
                    writeline("/home/ubuntu/results/coverage/Copy/Copy_2_10.coverage", "6ab28a94-21ef-4cfc-b037-d31586685632");
                    add(fromDir, srcFiles, filesByBasedir);
                    writeline("/home/ubuntu/results/coverage/Copy/Copy_2_10.coverage", "0379b7b8-3549-4e17-8681-e6d9a6312fe8");
                    add(fromDir, srcDirs, dirsByBasedir);
                    writeline("/home/ubuntu/results/coverage/Copy/Copy_2_10.coverage", "e1685532-2630-4652-a9a1-d6e555c00fdf");
                    baseDirs.add(fromDir);
                } else {
                    writeline("/home/ubuntu/results/coverage/Copy/Copy_2_10.coverage", "2e4c982d-3ad1-4382-8eaa-a25216365205");
                    if (!rc.isFilesystemOnly() && !supportsNonFileResources()) {
                        writeline("/home/ubuntu/results/coverage/Copy/Copy_2_10.coverage", "5d5ee924-a39b-4cff-9206-55998bffff44");
                        throw new BuildException("Only FileSystem resources are supported.");
                    }
                    writeline("/home/ubuntu/results/coverage/Copy/Copy_2_10.coverage", "8b7a478d-1d71-41c9-9bb8-cc493dc567ff");
                    for (final Resource r : rc) {
                        writeline("/home/ubuntu/results/coverage/Copy/Copy_2_10.coverage", "7366fe96-c66d-479d-b96b-5d5e134d2e45");
                        if (!r.isExists()) {
                            writeline("/home/ubuntu/results/coverage/Copy/Copy_2_10.coverage", "40ccc090-0fa1-4c28-9de0-4b19444ae0b4");
                            final String message = "Warning: Could not find resource " + r.toLongString() + " to copy.";
                            writeline("/home/ubuntu/results/coverage/Copy/Copy_2_10.coverage", "bc020ee2-35fd-408d-9317-917dd3550917");
                            if (!failonerror) {
                                writeline("/home/ubuntu/results/coverage/Copy/Copy_2_10.coverage", "143e9d5a-3263-4d2b-9594-4c3e891d6159");
                                if (!quiet) {
                                    writeline("/home/ubuntu/results/coverage/Copy/Copy_2_10.coverage", "74ea9f02-9c98-41b0-b29f-c3c313c51124");
                                    log(message, Project.MSG_ERR);
                                }
                            } else {
                                writeline("/home/ubuntu/results/coverage/Copy/Copy_2_10.coverage", "f1248cce-0c2f-4cb3-88b0-046c3bf33643");
                                throw new BuildException(message);
                            }
                            writeline("/home/ubuntu/results/coverage/Copy/Copy_2_10.coverage", "15678b56-7a33-473f-aa7f-c0bdfea54976");
                            continue;
                        }
                        writeline("/home/ubuntu/results/coverage/Copy/Copy_2_10.coverage", "c201f7ac-fc3c-4954-81a6-467d9de67012");
                        File baseDir = NULL_FILE_PLACEHOLDER;
                        writeline("/home/ubuntu/results/coverage/Copy/Copy_2_10.coverage", "792577ae-4741-4584-a941-ba6a57a04a33");
                        String name = r.getName();
                        writeline("/home/ubuntu/results/coverage/Copy/Copy_2_10.coverage", "2eb3abaa-a754-4a3f-a8f4-d1fc23488259");
                        final FileProvider fp = r.as(FileProvider.class);
                        writeline("/home/ubuntu/results/coverage/Copy/Copy_2_10.coverage", "73add618-85fb-405e-8601-40c4544bded5");
                        if (fp != null) {
                            writeline("/home/ubuntu/results/coverage/Copy/Copy_2_10.coverage", "13447a8b-70a4-4ddc-924f-48c4a09895ba");
                            final FileResource fr = ResourceUtils.asFileResource(fp);
                            writeline("/home/ubuntu/results/coverage/Copy/Copy_2_10.coverage", "a7047573-40ca-493a-8366-9231ce55c025");
                            baseDir = getKeyFile(fr.getBaseDir());
                            writeline("/home/ubuntu/results/coverage/Copy/Copy_2_10.coverage", "15863e1c-2056-44a5-b8a3-81da72e035aa");
                            if (fr.getBaseDir() == null) {
                                writeline("/home/ubuntu/results/coverage/Copy/Copy_2_10.coverage", "ac1bd05f-93bd-42db-9e87-20c95ac2eb68");
                                name = fr.getFile().getAbsolutePath();
                            }
                        }
                        writeline("/home/ubuntu/results/coverage/Copy/Copy_2_10.coverage", "3014dd84-f8e3-483f-bf56-fba47b15c65c");
                        // files.
                        if (r.isDirectory() || fp != null) {
                            writeline("/home/ubuntu/results/coverage/Copy/Copy_2_10.coverage", "c93a9acf-753b-4c2f-9da3-b361e107ff5e");
                            add(baseDir, name, r.isDirectory() ? dirsByBasedir : filesByBasedir);
                            writeline("/home/ubuntu/results/coverage/Copy/Copy_2_10.coverage", "242584d7-9c10-40b0-ba7e-5c3377b1900e");
                            baseDirs.add(baseDir);
                        } else {
                            writeline("/home/ubuntu/results/coverage/Copy/Copy_2_10.coverage", "304675df-095b-4d8a-b6f7-0eb9b0a19fce");
                            // a not-directory file resource
                            // needs special treatment
                            nonFileResources.add(r);
                        }
                    }
                }
            }
            writeline("/home/ubuntu/results/coverage/Copy/Copy_2_10.coverage", "b5816ff7-72ec-4987-9632-58804b1ef1b0");
            iterateOverBaseDirs(baseDirs, dirsByBasedir, filesByBasedir);
            writeline("/home/ubuntu/results/coverage/Copy/Copy_2_10.coverage", "9165af5b-4069-45b4-817e-475c07ace391");
            // do all the copy operations now...
            try {
                writeline("/home/ubuntu/results/coverage/Copy/Copy_2_10.coverage", "ee1ec922-a3af-46a2-89c7-6d694f729032");
                doFileOperations();
            } catch (final BuildException e) {
                writeline("/home/ubuntu/results/coverage/Copy/Copy_2_10.coverage", "d17a5d8b-3144-4084-ae74-d471c5a694fc");
                if (!failonerror) {
                    writeline("/home/ubuntu/results/coverage/Copy/Copy_2_10.coverage", "33eec511-9bcc-4b5c-b514-0f6de6f0b23b");
                    if (!quiet) {
                        writeline("/home/ubuntu/results/coverage/Copy/Copy_2_10.coverage", "5d5cb1a2-4eac-46a6-afa6-c1c951e4e418");
                        log("Warning: " + getMessage(e), Project.MSG_ERR);
                    }
                } else {
                    writeline("/home/ubuntu/results/coverage/Copy/Copy_2_10.coverage", "7fcc94b4-8604-4322-904c-eb1eff748b83");
                    throw e;
                }
            }
            writeline("/home/ubuntu/results/coverage/Copy/Copy_2_10.coverage", "a2e7a5c7-450d-4ce5-890d-93721266c682");
            if (nonFileResources.size() > 0 || singleResource != null) {
                writeline("/home/ubuntu/results/coverage/Copy/Copy_2_10.coverage", "b9ca31bb-aea2-467d-a7bb-0e71fc648abe");
                final Resource[] nonFiles = nonFileResources.toArray(new Resource[nonFileResources.size()]);
                writeline("/home/ubuntu/results/coverage/Copy/Copy_2_10.coverage", "306af444-edec-49c4-a2b6-a3468520d81c");
                // restrict to out-of-date resources
                final Map<Resource, String[]> map = scan(nonFiles, destDir);
                writeline("/home/ubuntu/results/coverage/Copy/Copy_2_10.coverage", "2604c87b-3f93-4565-9bf0-d4e770d760f3");
                if (singleResource != null) {
                    writeline("/home/ubuntu/results/coverage/Copy/Copy_2_10.coverage", "07407d4c-5687-439d-adc9-c9e13c6ab684");
                    map.put(singleResource, new String[] { destFile.getAbsolutePath() });
                }
                writeline("/home/ubuntu/results/coverage/Copy/Copy_2_10.coverage", "dcab7af5-ae6b-4ef4-89a2-f7bdb9d24603");
                try {
                    writeline("/home/ubuntu/results/coverage/Copy/Copy_2_10.coverage", "5e482357-e7c6-41d0-b1a0-9bf6be1c2ab8");
                    doResourceOperations(map);
                } catch (final BuildException e) {
                    writeline("/home/ubuntu/results/coverage/Copy/Copy_2_10.coverage", "d0144b76-fcde-4342-b5a1-4ff4b6cb770e");
                    if (!failonerror) {
                        writeline("/home/ubuntu/results/coverage/Copy/Copy_2_10.coverage", "6791dad1-9e42-472f-a04e-edc9ab8b6365");
                        if (!quiet) {
                            writeline("/home/ubuntu/results/coverage/Copy/Copy_2_10.coverage", "604d5a7f-49dd-4dd8-a70c-08f5c632ab92");
                            log("Warning: " + getMessage(e), Project.MSG_ERR);
                        }
                    } else {
                        writeline("/home/ubuntu/results/coverage/Copy/Copy_2_10.coverage", "4a19ba65-67d4-462c-84db-2692ede48a81");
                        throw e;
                    }
                }
            }
        } finally {
            writeline("/home/ubuntu/results/coverage/Copy/Copy_2_10.coverage", "bf5a76f8-9337-49b1-9f2a-753aeb983d9f");
            // clean up again, so this instance can be used a second
            // time
            singleResource = null;
            writeline("/home/ubuntu/results/coverage/Copy/Copy_2_10.coverage", "a052a095-0cca-44de-bf8f-1605a2c835c8");
            file = savedFile;
            writeline("/home/ubuntu/results/coverage/Copy/Copy_2_10.coverage", "1cc63629-ce0e-49cc-a2ba-f66006b3cfd2");
            destFile = savedDestFile;
            writeline("/home/ubuntu/results/coverage/Copy/Copy_2_10.coverage", "cfa6b64a-e92e-46a2-b249-b5798ac96efa");
            destDir = savedDestDir;
            writeline("/home/ubuntu/results/coverage/Copy/Copy_2_10.coverage", "2cecb377-60e2-4e88-9f14-4a3334aa4e29");
            if (savedRc != null) {
                writeline("/home/ubuntu/results/coverage/Copy/Copy_2_10.coverage", "0580ddf9-5567-41c1-9f6e-6411c086a038");
                rcs.insertElementAt(savedRc, 0);
            }
            writeline("/home/ubuntu/results/coverage/Copy/Copy_2_10.coverage", "260b1095-99bd-4f0a-8580-92bca69106c9");
            fileCopyMap.clear();
            writeline("/home/ubuntu/results/coverage/Copy/Copy_2_10.coverage", "3628080d-c39f-4045-9105-05e114d32743");
            dirCopyMap.clear();
            writeline("/home/ubuntu/results/coverage/Copy/Copy_2_10.coverage", "e67c70cd-f6a2-4503-9e80-81cbb0dc9128");
            completeDirMap.clear();
        }
    }

    /**
     * *********************************************************************
     * *  protected and private methods
     * **********************************************************************
     */
    private void copySingleFile() {
        writeline("/home/ubuntu/results/coverage/Copy/Copy_2_10.coverage", "4ebe31d3-a8da-4e96-bbdf-f3620715075a");
        // deal with the single file
        if (file != null) {
            writeline("/home/ubuntu/results/coverage/Copy/Copy_2_10.coverage", "74ed8b77-dfac-4c1b-bdd3-3598f99bc902");
            if (file.exists()) {
                writeline("/home/ubuntu/results/coverage/Copy/Copy_2_10.coverage", "24475fc7-47a6-4d2e-821d-e7c1718e40a9");
                if (destFile == null) {
                    writeline("/home/ubuntu/results/coverage/Copy/Copy_2_10.coverage", "27cdcfcf-7f43-4047-b29e-e31850f3aa4c");
                    destFile = new File(destDir, file.getName());
                }
                writeline("/home/ubuntu/results/coverage/Copy/Copy_2_10.coverage", "a73b98d4-9198-4a33-812b-3d459b74d96d");
                if (forceOverwrite || !destFile.exists() || (file.lastModified() - granularity > destFile.lastModified())) {
                    writeline("/home/ubuntu/results/coverage/Copy/Copy_2_10.coverage", "784e8f10-06d5-48a0-8d0c-3d08142f4ee9");
                    fileCopyMap.put(file.getAbsolutePath(), new String[] { destFile.getAbsolutePath() });
                } else {
                    writeline("/home/ubuntu/results/coverage/Copy/Copy_2_10.coverage", "068a1838-2c0a-4172-9b1a-472ebe4c58e4");
                    log(file + " omitted as " + destFile + " is up to date.", Project.MSG_VERBOSE);
                }
            } else {
                writeline("/home/ubuntu/results/coverage/Copy/Copy_2_10.coverage", "5d33a1e8-f988-4748-a52b-f33c2a2be358");
                final String message = "Warning: Could not find file " + file.getAbsolutePath() + " to copy.";
                writeline("/home/ubuntu/results/coverage/Copy/Copy_2_10.coverage", "4d222e56-a5dd-4d5a-9bdc-1de9293f1766");
                if (!failonerror) {
                    writeline("/home/ubuntu/results/coverage/Copy/Copy_2_10.coverage", "acb19faf-6fa3-410b-8f8f-40f8c1a8b3cd");
                    if (!quiet) {
                        writeline("/home/ubuntu/results/coverage/Copy/Copy_2_10.coverage", "c8de28b6-1926-4db9-9520-064ede3aa95c");
                        log(message, Project.MSG_ERR);
                    }
                } else {
                    writeline("/home/ubuntu/results/coverage/Copy/Copy_2_10.coverage", "cb72812d-a361-43aa-a0e8-26225bfa1ea9");
                    throw new BuildException(message);
                }
            }
        }
    }

    private void iterateOverBaseDirs(final HashSet<File> baseDirs, final HashMap<File, List<String>> dirsByBasedir, final HashMap<File, List<String>> filesByBasedir) {
        writeline("/home/ubuntu/results/coverage/Copy/Copy_2_10.coverage", "a9ad28ad-79bd-45a0-bf4e-2841a1b90d79");
        for (final File f : baseDirs) {
            writeline("/home/ubuntu/results/coverage/Copy/Copy_2_10.coverage", "adf31fb7-99af-4155-9c80-50b87236970b");
            final List<String> files = filesByBasedir.get(f);
            writeline("/home/ubuntu/results/coverage/Copy/Copy_2_10.coverage", "f2826edf-6355-4f1f-8729-b0299dec7dcf");
            final List<String> dirs = dirsByBasedir.get(f);
            writeline("/home/ubuntu/results/coverage/Copy/Copy_2_10.coverage", "48c67e1a-9e7d-4a53-b485-d85a48e7226c");
            String[] srcFiles = new String[0];
            writeline("/home/ubuntu/results/coverage/Copy/Copy_2_10.coverage", "785cab19-4674-4fed-be5a-dfcaa6e36552");
            if (files != null) {
                writeline("/home/ubuntu/results/coverage/Copy/Copy_2_10.coverage", "2474eb71-d3e5-406d-8da9-d4014ad6d055");
                srcFiles = files.toArray(srcFiles);
            }
            writeline("/home/ubuntu/results/coverage/Copy/Copy_2_10.coverage", "4c011c02-3a7f-4323-80d6-8fc540e98392");
            String[] srcDirs = new String[0];
            writeline("/home/ubuntu/results/coverage/Copy/Copy_2_10.coverage", "ddd68d66-a47e-4721-a420-fbacc202312c");
            if (dirs != null) {
                writeline("/home/ubuntu/results/coverage/Copy/Copy_2_10.coverage", "e90da94e-d399-46d5-8e0d-eda2ca2cd0ed");
                srcDirs = dirs.toArray(srcDirs);
            }
            writeline("/home/ubuntu/results/coverage/Copy/Copy_2_10.coverage", "e1a77833-8457-453e-9261-7f401d8f68b7");
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
        writeline("/home/ubuntu/results/coverage/Copy/Copy_2_10.coverage", "5106f9f7-7f0e-4a97-a5e7-881ae9f3d5b7");
        if (file == null && rcs.size() == 0) {
            writeline("/home/ubuntu/results/coverage/Copy/Copy_2_10.coverage", "3c138c8e-ad36-4c36-9847-0b2165608df5");
            throw new BuildException("Specify at least one source--a file or a resource collection.");
        }
        writeline("/home/ubuntu/results/coverage/Copy/Copy_2_10.coverage", "48ee3bd7-d768-4175-8df1-eb1a44c13db5");
        if (destFile != null && destDir != null) {
            writeline("/home/ubuntu/results/coverage/Copy/Copy_2_10.coverage", "73c01a71-3f39-4da6-b40d-4bed2b56673a");
            throw new BuildException("Only one of tofile and todir may be set.");
        }
        writeline("/home/ubuntu/results/coverage/Copy/Copy_2_10.coverage", "bed57e7c-10fa-41b4-b481-cd1fe6751a55");
        if (destFile == null && destDir == null) {
            writeline("/home/ubuntu/results/coverage/Copy/Copy_2_10.coverage", "54334e50-b4c3-4517-a64c-b34efc845fec");
            throw new BuildException("One of tofile or todir must be set.");
        }
        writeline("/home/ubuntu/results/coverage/Copy/Copy_2_10.coverage", "5c73a045-e8b0-4afe-9753-12308d3b4cbc");
        if (file != null && file.isDirectory()) {
            writeline("/home/ubuntu/results/coverage/Copy/Copy_2_10.coverage", "a55a7b66-fe1c-44bc-9f2d-6876695670ac");
            throw new BuildException("Use a resource collection to copy directories.");
        }
        writeline("/home/ubuntu/results/coverage/Copy/Copy_2_10.coverage", "9f2d4020-bf0b-4a5f-8edd-6ca378773f2d");
        if (destFile != null && rcs.size() > 0) {
            writeline("/home/ubuntu/results/coverage/Copy/Copy_2_10.coverage", "c505adec-338f-4b09-ac40-31390fd811ae");
            if (rcs.size() > 1) {
                writeline("/home/ubuntu/results/coverage/Copy/Copy_2_10.coverage", "b1e9a6e7-1213-4e88-adeb-67e36ac7a41c");
                throw new BuildException("Cannot concatenate multiple files into a single file.");
            } else {
                writeline("/home/ubuntu/results/coverage/Copy/Copy_2_10.coverage", "6504131e-b59e-4db0-9df2-65d3e73e50d3");
                final ResourceCollection rc = rcs.elementAt(0);
                writeline("/home/ubuntu/results/coverage/Copy/Copy_2_10.coverage", "f34b4d4e-66a1-4023-8244-cbefc651b701");
                if (!rc.isFilesystemOnly() && !supportsNonFileResources()) {
                    writeline("/home/ubuntu/results/coverage/Copy/Copy_2_10.coverage", "257d2929-dcad-487d-a0ed-1c4ceb687ed3");
                    throw new BuildException("Only FileSystem resources are" + " supported.");
                }
                writeline("/home/ubuntu/results/coverage/Copy/Copy_2_10.coverage", "32005340-37b6-452d-9cba-3f494fd98765");
                if (rc.size() == 0) {
                    writeline("/home/ubuntu/results/coverage/Copy/Copy_2_10.coverage", "325ca898-7855-4062-8f05-b3d26d3c760e");
                    throw new BuildException(MSG_WHEN_COPYING_EMPTY_RC_TO_FILE);
                } else if (rc.size() == 1) {
                    writeline("/home/ubuntu/results/coverage/Copy/Copy_2_10.coverage", "77b676d5-d09f-484e-af71-4c9ee77feb57");
                    final Resource res = rc.iterator().next();
                    writeline("/home/ubuntu/results/coverage/Copy/Copy_2_10.coverage", "61260f62-ba10-4020-9f10-49058efa5bc8");
                    final FileProvider r = res.as(FileProvider.class);
                    writeline("/home/ubuntu/results/coverage/Copy/Copy_2_10.coverage", "acb84431-db9b-4785-9a9f-ae831a061938");
                    if (file == null) {
                        writeline("/home/ubuntu/results/coverage/Copy/Copy_2_10.coverage", "e767406c-9273-44c7-af3b-cbb47a240221");
                        if (r != null) {
                            writeline("/home/ubuntu/results/coverage/Copy/Copy_2_10.coverage", "94a253ce-099b-4256-8735-619f61533de7");
                            file = r.getFile();
                        } else {
                            writeline("/home/ubuntu/results/coverage/Copy/Copy_2_10.coverage", "0c3a0044-e5a3-4991-94a9-1a2e42bcefc9");
                            singleResource = res;
                        }
                        writeline("/home/ubuntu/results/coverage/Copy/Copy_2_10.coverage", "a8094fe5-4dce-435e-adb6-28c6cdfd0628");
                        rcs.removeElementAt(0);
                    } else {
                        writeline("/home/ubuntu/results/coverage/Copy/Copy_2_10.coverage", "b592f220-1279-4116-a427-1ba85ca7b5a1");
                        throw new BuildException("Cannot concatenate multiple files into a single file.");
                    }
                } else {
                    writeline("/home/ubuntu/results/coverage/Copy/Copy_2_10.coverage", "7b865ffe-21af-4279-b9c8-f78b40d59471");
                    throw new BuildException("Cannot concatenate multiple files into a single file.");
                }
            }
        }
        writeline("/home/ubuntu/results/coverage/Copy/Copy_2_10.coverage", "50e36a7f-35be-48e0-94aa-d6d6c4600caa");
        if (destFile != null) {
            writeline("/home/ubuntu/results/coverage/Copy/Copy_2_10.coverage", "45c35e84-801b-4c37-a037-e613d503255f");
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
        writeline("/home/ubuntu/results/coverage/Copy/Copy_2_10.coverage", "a32db50f-0c01-40dd-87a5-09aeadbafe00");
        final FileNameMapper mapper = getMapper();
        writeline("/home/ubuntu/results/coverage/Copy/Copy_2_10.coverage", "26427b10-126c-4c10-9741-ad0ea08081d8");
        buildMap(fromDir, toDir, files, mapper, fileCopyMap);
        writeline("/home/ubuntu/results/coverage/Copy/Copy_2_10.coverage", "8b11118c-5969-40b0-9f39-520fb8b6ecee");
        if (includeEmpty) {
            writeline("/home/ubuntu/results/coverage/Copy/Copy_2_10.coverage", "42bc2835-9f0b-4d81-aa0e-62da80b2d008");
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
        writeline("/home/ubuntu/results/coverage/Copy/Copy_2_10.coverage", "e0b3642c-50ca-46fa-8c7e-855d58ae8ba5");
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
        writeline("/home/ubuntu/results/coverage/Copy/Copy_2_10.coverage", "c1e40d2e-a417-4825-a615-7ef6fe972c3f");
        String[] toCopy = null;
        writeline("/home/ubuntu/results/coverage/Copy/Copy_2_10.coverage", "91381357-a6c2-4eee-a1cc-9ea13b94952a");
        if (forceOverwrite) {
            writeline("/home/ubuntu/results/coverage/Copy/Copy_2_10.coverage", "30a18408-be67-4117-b5ad-2db77c1f5e41");
            final Vector<String> v = new Vector<String>();
            writeline("/home/ubuntu/results/coverage/Copy/Copy_2_10.coverage", "385f423c-2c53-4ca5-9d13-7054f47eda9b");
            for (int i = 0; i < names.length; i++) {
                writeline("/home/ubuntu/results/coverage/Copy/Copy_2_10.coverage", "cce62557-eed6-4cb5-9576-4411948f3cc1");
                if (mapper.mapFileName(names[i]) != null) {
                    writeline("/home/ubuntu/results/coverage/Copy/Copy_2_10.coverage", "7a3c0eba-9c8f-4634-983f-e61bcfa6e254");
                    v.addElement(names[i]);
                }
            }
            writeline("/home/ubuntu/results/coverage/Copy/Copy_2_10.coverage", "e48910fe-7f24-40b7-ad6f-d20df4283611");
            toCopy = new String[v.size()];
            writeline("/home/ubuntu/results/coverage/Copy/Copy_2_10.coverage", "44a1c63e-051b-4606-97da-d5f7fd1f4241");
            v.copyInto(toCopy);
        } else {
            writeline("/home/ubuntu/results/coverage/Copy/Copy_2_10.coverage", "46318703-b80b-4abb-af14-84b24cb2368a");
            final SourceFileScanner ds = new SourceFileScanner(this);
            writeline("/home/ubuntu/results/coverage/Copy/Copy_2_10.coverage", "a2ae8ebb-0192-433c-ada5-f1150d2fc4e8");
            toCopy = ds.restrict(names, fromDir, toDir, mapper, granularity);
        }
        writeline("/home/ubuntu/results/coverage/Copy/Copy_2_10.coverage", "0278f3ef-da1e-4b77-9480-5259e0a9936a");
        for (int i = 0; i < toCopy.length; i++) {
            writeline("/home/ubuntu/results/coverage/Copy/Copy_2_10.coverage", "fe44b7eb-eb5b-45e3-a299-c9ec38aafca6");
            final File src = new File(fromDir, toCopy[i]);
            writeline("/home/ubuntu/results/coverage/Copy/Copy_2_10.coverage", "0752f2c7-9508-4a63-a43a-1fc92de335a8");
            final String[] mappedFiles = mapper.mapFileName(toCopy[i]);
            writeline("/home/ubuntu/results/coverage/Copy/Copy_2_10.coverage", "88d72b69-aad4-4711-9ee4-7ad8b9816917");
            if (!enableMultipleMappings) {
                writeline("/home/ubuntu/results/coverage/Copy/Copy_2_10.coverage", "423ed666-bf6d-45e2-affe-7d7c476d2f4d");
                map.put(src.getAbsolutePath(), new String[] { new File(toDir, mappedFiles[0]).getAbsolutePath() });
            } else {
                writeline("/home/ubuntu/results/coverage/Copy/Copy_2_10.coverage", "d1abf983-dcfe-4c53-a166-7c49dc3642b8");
                // reuse the array created by the mapper
                for (int k = 0; k < mappedFiles.length; k++) {
                    writeline("/home/ubuntu/results/coverage/Copy/Copy_2_10.coverage", "d4e6486a-d914-4416-aa53-71c913f46e16");
                    mappedFiles[k] = new File(toDir, mappedFiles[k]).getAbsolutePath();
                }
                writeline("/home/ubuntu/results/coverage/Copy/Copy_2_10.coverage", "e4e7fdc8-3eec-4ac9-b7cc-de9c2a1f581b");
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
        writeline("/home/ubuntu/results/coverage/Copy/Copy_2_10.coverage", "1c1ef257-ab11-46ab-a91e-7441e4fc8610");
        final HashMap<Resource, String[]> map = new HashMap<Resource, String[]>();
        writeline("/home/ubuntu/results/coverage/Copy/Copy_2_10.coverage", "217b6823-02aa-4f4b-bd9f-b03a3ebd67b3");
        Resource[] toCopy = null;
        writeline("/home/ubuntu/results/coverage/Copy/Copy_2_10.coverage", "8649c67a-00e8-4c81-b12b-e821b488bebc");
        if (forceOverwrite) {
            writeline("/home/ubuntu/results/coverage/Copy/Copy_2_10.coverage", "5c43d045-186e-4bd2-b654-5f891e758d5b");
            final Vector<Resource> v = new Vector<Resource>();
            writeline("/home/ubuntu/results/coverage/Copy/Copy_2_10.coverage", "0636742a-1ce6-45be-bbf8-7e19f02828c9");
            for (int i = 0; i < fromResources.length; i++) {
                writeline("/home/ubuntu/results/coverage/Copy/Copy_2_10.coverage", "83120300-9fb9-4268-95ff-0fa92ec917c0");
                if (mapper.mapFileName(fromResources[i].getName()) != null) {
                    writeline("/home/ubuntu/results/coverage/Copy/Copy_2_10.coverage", "b27e7f95-2588-4a9b-9c93-175c8efeaf61");
                    v.addElement(fromResources[i]);
                }
            }
            writeline("/home/ubuntu/results/coverage/Copy/Copy_2_10.coverage", "0462da96-e018-47ee-8258-6229802e0cf8");
            toCopy = new Resource[v.size()];
            writeline("/home/ubuntu/results/coverage/Copy/Copy_2_10.coverage", "7be79570-5b0b-406b-b261-0dd0a3e50b16");
            v.copyInto(toCopy);
        } else {
            writeline("/home/ubuntu/results/coverage/Copy/Copy_2_10.coverage", "2de47090-cebc-41a4-99a3-ee96b9701ec0");
            toCopy = ResourceUtils.selectOutOfDateSources(this, fromResources, mapper, new ResourceFactory() {

                public Resource getResource(final String name) {
                    return new FileResource(toDir, name);
                }
            }, granularity);
        }
        writeline("/home/ubuntu/results/coverage/Copy/Copy_2_10.coverage", "f51e484a-0608-45f6-9a8d-b756246937cc");
        for (int i = 0; i < toCopy.length; i++) {
            writeline("/home/ubuntu/results/coverage/Copy/Copy_2_10.coverage", "cf3a84eb-0334-44ac-8e75-cd6ce83249e4");
            final String[] mappedFiles = mapper.mapFileName(toCopy[i].getName());
            writeline("/home/ubuntu/results/coverage/Copy/Copy_2_10.coverage", "f517c439-61b8-4744-82c0-511b9b89bc6c");
            for (int j = 0; j < mappedFiles.length; j++) {
                writeline("/home/ubuntu/results/coverage/Copy/Copy_2_10.coverage", "acd7487b-1217-485a-b050-4c298a2fdd5d");
                if (mappedFiles[j] == null) {
                    writeline("/home/ubuntu/results/coverage/Copy/Copy_2_10.coverage", "af221308-892b-475e-9492-57266d5f2062");
                    throw new BuildException("Can't copy a resource without a" + " name if the mapper doesn't" + " provide one.");
                }
            }
            writeline("/home/ubuntu/results/coverage/Copy/Copy_2_10.coverage", "1a7cddc6-6dd0-4722-ab23-bbfb8bc06ed7");
            if (!enableMultipleMappings) {
                writeline("/home/ubuntu/results/coverage/Copy/Copy_2_10.coverage", "6fefb1bd-70ba-4ac5-81bd-144671dc127b");
                map.put(toCopy[i], new String[] { new File(toDir, mappedFiles[0]).getAbsolutePath() });
            } else {
                writeline("/home/ubuntu/results/coverage/Copy/Copy_2_10.coverage", "f34712ef-96ac-4757-9f06-c6675b43c77b");
                // reuse the array created by the mapper
                for (int k = 0; k < mappedFiles.length; k++) {
                    writeline("/home/ubuntu/results/coverage/Copy/Copy_2_10.coverage", "daa7dda1-972c-4b02-9a96-a10b65e699dd");
                    mappedFiles[k] = new File(toDir, mappedFiles[k]).getAbsolutePath();
                }
                writeline("/home/ubuntu/results/coverage/Copy/Copy_2_10.coverage", "1cdda4ec-e80e-48f1-9a84-357a7c4fbde1");
                map.put(toCopy[i], mappedFiles);
            }
        }
        writeline("/home/ubuntu/results/coverage/Copy/Copy_2_10.coverage", "50e53263-0c73-4231-a06e-bc9f35ac0fd1");
        return map;
    }

    /**
     * Actually does the file (and possibly empty directory) copies.
     * This is a good method for subclasses to override.
     */
    protected void doFileOperations() {
        writeline("/home/ubuntu/results/coverage/Copy/Copy_2_10.coverage", "c16ac479-9329-4eb5-bc48-843bba01d3fe");
        if (fileCopyMap.size() > 0) {
            writeline("/home/ubuntu/results/coverage/Copy/Copy_2_10.coverage", "a29c5c9c-7dcc-4891-95de-4a0558bd4a90");
            log("Copying " + fileCopyMap.size() + " file" + (fileCopyMap.size() == 1 ? "" : "s") + " to " + destDir.getAbsolutePath());
            writeline("/home/ubuntu/results/coverage/Copy/Copy_2_10.coverage", "227b4a57-1129-4e41-a6e8-517a60a36933");
            for (final Map.Entry<String, String[]> e : fileCopyMap.entrySet()) {
                writeline("/home/ubuntu/results/coverage/Copy/Copy_2_10.coverage", "b1ab0d9a-636f-434b-ae9c-9562f9a61dd0");
                final String fromFile = e.getKey();
                writeline("/home/ubuntu/results/coverage/Copy/Copy_2_10.coverage", "5d71b6dd-97c9-4664-8f5d-7d2fd46cd21d");
                final String[] toFiles = e.getValue();
                writeline("/home/ubuntu/results/coverage/Copy/Copy_2_10.coverage", "0c74dafa-86a8-441f-8750-dcf2f5c9e994");
                for (int i = 0; i < toFiles.length; i++) {
                    writeline("/home/ubuntu/results/coverage/Copy/Copy_2_10.coverage", "15f476bc-bb00-454e-abe2-c7f23ea0ab34");
                    final String toFile = toFiles[i];
                    writeline("/home/ubuntu/results/coverage/Copy/Copy_2_10.coverage", "5fb2f2af-129b-4426-90b0-2a0b223bcf16");
                    if (fromFile.equals(toFile)) {
                        writeline("/home/ubuntu/results/coverage/Copy/Copy_2_10.coverage", "6e16c5cc-80c0-4a53-bf70-9ec3027e7eff");
                        log("Skipping self-copy of " + fromFile, verbosity);
                        writeline("/home/ubuntu/results/coverage/Copy/Copy_2_10.coverage", "47071153-d395-46ae-9837-f830e0268662");
                        continue;
                    }
                    writeline("/home/ubuntu/results/coverage/Copy/Copy_2_10.coverage", "7457b1de-044f-4293-a4c7-d0e8c1e6956b");
                    try {
                        writeline("/home/ubuntu/results/coverage/Copy/Copy_2_10.coverage", "a6729240-0f74-4baa-8ecb-372141335bfe");
                        log("Copying " + fromFile + " to " + toFile, verbosity);
                        writeline("/home/ubuntu/results/coverage/Copy/Copy_2_10.coverage", "ce8cd539-ac52-4224-9046-b8ca2fc79964");
                        final FilterSetCollection executionFilters = new FilterSetCollection();
                        writeline("/home/ubuntu/results/coverage/Copy/Copy_2_10.coverage", "685ce0d7-5f4b-4c6f-9976-9a0dc2040b93");
                        if (filtering) {
                            writeline("/home/ubuntu/results/coverage/Copy/Copy_2_10.coverage", "655366b8-a431-4df2-84b2-9baf304296f0");
                            executionFilters.addFilterSet(getProject().getGlobalFilterSet());
                        }
                        writeline("/home/ubuntu/results/coverage/Copy/Copy_2_10.coverage", "afa7f654-119a-4e22-b7d3-d7c0e39b6ce3");
                        for (final FilterSet filterSet : filterSets) {
                            writeline("/home/ubuntu/results/coverage/Copy/Copy_2_10.coverage", "30dd1821-2047-4eb8-aacf-be6da4a48ae8");
                            executionFilters.addFilterSet(filterSet);
                        }
                        writeline("/home/ubuntu/results/coverage/Copy/Copy_2_10.coverage", "1b2fc480-1a03-4364-a783-c322b9e9099b");
                        fileUtils.copyFile(new File(fromFile), new File(toFile), executionFilters, filterChains, forceOverwrite, preserveLastModified, /* append: */
                        false, inputEncoding, outputEncoding, getProject(), getForce());
                    } catch (final IOException ioe) {
                        writeline("/home/ubuntu/results/coverage/Copy/Copy_2_10.coverage", "940d1b7a-ff34-439e-b6c8-c103bb339300");
                        String msg = "Failed to copy " + fromFile + " to " + toFile + " due to " + getDueTo(ioe);
                        writeline("/home/ubuntu/results/coverage/Copy/Copy_2_10.coverage", "804eee5c-7247-422e-ab2b-633471e5c2b4");
                        final File targetFile = new File(toFile);
                        writeline("/home/ubuntu/results/coverage/Copy/Copy_2_10.coverage", "2120749c-11ba-4673-a9d8-5d9f6e9a8666");
                        if (!(ioe instanceof ResourceUtils.ReadOnlyTargetFileException) && targetFile.exists() && !targetFile.delete()) {
                            writeline("/home/ubuntu/results/coverage/Copy/Copy_2_10.coverage", "83db3f23-c38f-4d97-89d8-1cfe28867213");
                            msg += " and I couldn't delete the corrupt " + toFile;
                        }
                        writeline("/home/ubuntu/results/coverage/Copy/Copy_2_10.coverage", "f4160961-f398-46b7-8f66-daeff9518910");
                        if (failonerror) {
                            writeline("/home/ubuntu/results/coverage/Copy/Copy_2_10.coverage", "8ab670bf-71b5-4ee2-a716-816235c91cef");
                            throw new BuildException(msg, ioe, getLocation());
                        }
                        writeline("/home/ubuntu/results/coverage/Copy/Copy_2_10.coverage", "32f5b035-62ae-4c85-bd92-963282a75667");
                        log(msg, Project.MSG_ERR);
                    }
                }
            }
        }
        writeline("/home/ubuntu/results/coverage/Copy/Copy_2_10.coverage", "3c4c7bd5-9df7-4874-99e2-76187f04996c");
        if (includeEmpty) {
            writeline("/home/ubuntu/results/coverage/Copy/Copy_2_10.coverage", "b08b90b9-b12b-44a7-9386-5fe3350ac9da");
            int createCount = 0;
            writeline("/home/ubuntu/results/coverage/Copy/Copy_2_10.coverage", "e4683332-b21a-490d-83f1-d5687b5c74db");
            for (final String[] dirs : dirCopyMap.values()) {
                writeline("/home/ubuntu/results/coverage/Copy/Copy_2_10.coverage", "d72e57c5-6813-4530-b585-d5f96d0a3c69");
                for (int i = 0; i < dirs.length; i++) {
                    writeline("/home/ubuntu/results/coverage/Copy/Copy_2_10.coverage", "ee7fa190-e43c-4cea-b753-dfd903db7f37");
                    final File d = new File(dirs[i]);
                    writeline("/home/ubuntu/results/coverage/Copy/Copy_2_10.coverage", "e57a42d2-c90b-48b3-8192-54252ca75084");
                    if (!d.exists()) {
                        writeline("/home/ubuntu/results/coverage/Copy/Copy_2_10.coverage", "42874771-193c-4362-98d3-0aa4edc30918");
                        if (!(d.mkdirs() || d.isDirectory())) {
                            writeline("/home/ubuntu/results/coverage/Copy/Copy_2_10.coverage", "65a3c761-a129-4872-88e0-6010f92507b3");
                            log("Unable to create directory " + d.getAbsolutePath(), Project.MSG_ERR);
                        } else {
                            writeline("/home/ubuntu/results/coverage/Copy/Copy_2_10.coverage", "1cddf998-33d2-436d-9a4a-28852a70733e");
                            createCount++;
                        }
                    }
                }
            }
            writeline("/home/ubuntu/results/coverage/Copy/Copy_2_10.coverage", "d6ad1155-37c8-4997-b362-ae7f135ff414");
            if (createCount > 0) {
                writeline("/home/ubuntu/results/coverage/Copy/Copy_2_10.coverage", "ac6dc464-68d6-4688-b843-b2628a4ee3bd");
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
        writeline("/home/ubuntu/results/coverage/Copy/Copy_2_10.coverage", "e73fe3bd-6b42-4a61-8496-b204395d3f4f");
        if (map.size() > 0) {
            writeline("/home/ubuntu/results/coverage/Copy/Copy_2_10.coverage", "1aadbe6d-2fe5-4d4f-917e-da2ae5f499e9");
            log("Copying " + map.size() + " resource" + (map.size() == 1 ? "" : "s") + " to " + destDir.getAbsolutePath());
            writeline("/home/ubuntu/results/coverage/Copy/Copy_2_10.coverage", "7f15127b-a377-4179-9d6b-44782bb46c20");
            for (final Map.Entry<Resource, String[]> e : map.entrySet()) {
                writeline("/home/ubuntu/results/coverage/Copy/Copy_2_10.coverage", "c286d166-668c-44ef-b278-928fb2ae9072");
                final Resource fromResource = e.getKey();
                writeline("/home/ubuntu/results/coverage/Copy/Copy_2_10.coverage", "66cc195c-cee2-46fb-bdf2-631c903f947e");
                for (final String toFile : e.getValue()) {
                    writeline("/home/ubuntu/results/coverage/Copy/Copy_2_10.coverage", "20f65eb6-7e52-4403-98a3-7d562f417398");
                    try {
                        writeline("/home/ubuntu/results/coverage/Copy/Copy_2_10.coverage", "122534df-c36f-4a57-9fee-6086032822eb");
                        log("Copying " + fromResource + " to " + toFile, verbosity);
                        writeline("/home/ubuntu/results/coverage/Copy/Copy_2_10.coverage", "52fe903a-2363-48f5-9bd0-97e04d426ea0");
                        final FilterSetCollection executionFilters = new FilterSetCollection();
                        writeline("/home/ubuntu/results/coverage/Copy/Copy_2_10.coverage", "68748dec-9584-4cc4-a2da-cdc8c7dd07f8");
                        if (filtering) {
                            writeline("/home/ubuntu/results/coverage/Copy/Copy_2_10.coverage", "46c5693e-0b16-4fce-9ce1-76ee42222d92");
                            executionFilters.addFilterSet(getProject().getGlobalFilterSet());
                        }
                        writeline("/home/ubuntu/results/coverage/Copy/Copy_2_10.coverage", "04e12eb2-ed0e-4ac0-80e1-99aac4eb811c");
                        for (final FilterSet filterSet : filterSets) {
                            writeline("/home/ubuntu/results/coverage/Copy/Copy_2_10.coverage", "0ec51a42-e621-4511-a931-fa3563570d37");
                            executionFilters.addFilterSet(filterSet);
                        }
                        writeline("/home/ubuntu/results/coverage/Copy/Copy_2_10.coverage", "5823699b-546d-48fc-b640-ab9b105bf68a");
                        ResourceUtils.copyResource(fromResource, new FileResource(destDir, toFile), executionFilters, filterChains, forceOverwrite, preserveLastModified, /* append: */
                        false, inputEncoding, outputEncoding, getProject(), getForce());
                    } catch (final IOException ioe) {
                        writeline("/home/ubuntu/results/coverage/Copy/Copy_2_10.coverage", "f48f47e6-4cc2-4b2d-af94-84d9b050db90");
                        String msg = "Failed to copy " + fromResource + " to " + toFile + " due to " + getDueTo(ioe);
                        writeline("/home/ubuntu/results/coverage/Copy/Copy_2_10.coverage", "a6ba800f-84d7-4a20-8012-0936f82dbd14");
                        final File targetFile = new File(toFile);
                        writeline("/home/ubuntu/results/coverage/Copy/Copy_2_10.coverage", "c9acd846-d9a2-4427-9724-a07cbb3e808c");
                        if (!(ioe instanceof ResourceUtils.ReadOnlyTargetFileException) && targetFile.exists() && !targetFile.delete()) {
                            writeline("/home/ubuntu/results/coverage/Copy/Copy_2_10.coverage", "8c7a423f-fd5a-40e6-9c75-7e384c7e3d07");
                            msg += " and I couldn't delete the corrupt " + toFile;
                        }
                        writeline("/home/ubuntu/results/coverage/Copy/Copy_2_10.coverage", "731936e0-a6e3-4eed-bb59-ebae1d1f2583");
                        if (failonerror) {
                            writeline("/home/ubuntu/results/coverage/Copy/Copy_2_10.coverage", "a1faf190-ca14-4ddb-9c80-3363a3933f89");
                            throw new BuildException(msg, ioe, getLocation());
                        }
                        writeline("/home/ubuntu/results/coverage/Copy/Copy_2_10.coverage", "41d81460-52ae-4f32-a9b8-a235f6421468");
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
        writeline("/home/ubuntu/results/coverage/Copy/Copy_2_10.coverage", "ca6fb2c9-ae51-4f29-9878-a6c271eafc44");
        return getClass().equals(Copy.class);
    }

    /**
     * Adds the given strings to a list contained in the given map.
     * The file is the key into the map.
     */
    private static void add(File baseDir, final String[] names, final Map<File, List<String>> m) {
        writelineStatic("/home/ubuntu/results/coverage/Copy/Copy_2_10.coverage", "c4d7f8fc-5304-455e-b486-1f51a56638ac");
        if (names != null) {
            writelineStatic("/home/ubuntu/results/coverage/Copy/Copy_2_10.coverage", "65fd60b4-2854-48b0-b9b3-59a2d82785fa");
            baseDir = getKeyFile(baseDir);
            writelineStatic("/home/ubuntu/results/coverage/Copy/Copy_2_10.coverage", "6b2278c4-1f0c-4982-b0a2-faafddd029c3");
            List<String> l = m.get(baseDir);
            writelineStatic("/home/ubuntu/results/coverage/Copy/Copy_2_10.coverage", "b1d0b6ca-33a8-43cf-9d19-2684f319ed49");
            if (l == null) {
                writelineStatic("/home/ubuntu/results/coverage/Copy/Copy_2_10.coverage", "128055de-9774-4eff-8b13-939d149a9db1");
                l = new ArrayList<String>(names.length);
                writelineStatic("/home/ubuntu/results/coverage/Copy/Copy_2_10.coverage", "f8f19c32-fce1-453f-89c2-0f6df248aa08");
                m.put(baseDir, l);
            }
            writelineStatic("/home/ubuntu/results/coverage/Copy/Copy_2_10.coverage", "a39c6586-1792-46c1-bb28-143ae8b19edd");
            l.addAll(java.util.Arrays.asList(names));
        }
    }

    /**
     * Adds the given string to a list contained in the given map.
     * The file is the key into the map.
     */
    private static void add(final File baseDir, final String name, final Map<File, List<String>> m) {
        writelineStatic("/home/ubuntu/results/coverage/Copy/Copy_2_10.coverage", "4e7027bb-e420-45e4-9206-d608ceb40ced");
        if (name != null) {
            writelineStatic("/home/ubuntu/results/coverage/Copy/Copy_2_10.coverage", "7c511085-7939-4a16-ab5e-849be0983c29");
            add(baseDir, new String[] { name }, m);
        }
    }

    /**
     * Either returns its argument or a plaeholder if the argument is null.
     */
    private static File getKeyFile(final File f) {
        writelineStatic("/home/ubuntu/results/coverage/Copy/Copy_2_10.coverage", "f7000633-f46b-42e5-b306-e9ed1a43806f");
        return f == null ? NULL_FILE_PLACEHOLDER : f;
    }

    /**
     * returns the mapper to use based on nested elements or the
     * flatten attribute.
     */
    private FileNameMapper getMapper() {
        writeline("/home/ubuntu/results/coverage/Copy/Copy_2_10.coverage", "23207ce3-2cbc-4031-98b0-444b92001378");
        FileNameMapper mapper = null;
        writeline("/home/ubuntu/results/coverage/Copy/Copy_2_10.coverage", "fe8ac733-640a-4733-8633-1f64b27f5ef3");
        if (mapperElement != null) {
            writeline("/home/ubuntu/results/coverage/Copy/Copy_2_10.coverage", "75718bbf-0a9a-4107-915a-ebd1b38c2ec3");
            mapper = mapperElement.getImplementation();
        } else if (flatten) {
            writeline("/home/ubuntu/results/coverage/Copy/Copy_2_10.coverage", "16ef887a-19c6-4b70-8f15-bd707c27ffbf");
            mapper = new FlatFileNameMapper();
        } else {
            writeline("/home/ubuntu/results/coverage/Copy/Copy_2_10.coverage", "b44c74cb-9dd6-45cf-9796-06622bb8490e");
            mapper = new IdentityMapper();
        }
        writeline("/home/ubuntu/results/coverage/Copy/Copy_2_10.coverage", "53f9cb53-0641-4840-99f2-7628bf9e8226");
        return mapper;
    }

    /**
     * Handle getMessage() for exceptions.
     * @param ex the exception to handle
     * @return ex.getMessage() if ex.getMessage() is not null
     * otherwise return ex.toString()
     */
    private String getMessage(final Exception ex) {
        writeline("/home/ubuntu/results/coverage/Copy/Copy_2_10.coverage", "2110ab01-fed2-4229-898e-1f57eefa8b81");
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
        writeline("/home/ubuntu/results/coverage/Copy/Copy_2_10.coverage", "c3ca4288-63c4-42c0-b2ba-499eb000ad26");
        final boolean baseIOException = ex.getClass() == IOException.class;
        writeline("/home/ubuntu/results/coverage/Copy/Copy_2_10.coverage", "8bf4c9d0-7446-413c-889b-c376de62b130");
        final StringBuffer message = new StringBuffer();
        writeline("/home/ubuntu/results/coverage/Copy/Copy_2_10.coverage", "44b5e691-3ef8-45d5-811c-a9c31403a94f");
        if (!baseIOException || ex.getMessage() == null) {
            writeline("/home/ubuntu/results/coverage/Copy/Copy_2_10.coverage", "762b5f04-38c8-4d24-b411-f0870e734590");
            message.append(ex.getClass().getName());
        }
        writeline("/home/ubuntu/results/coverage/Copy/Copy_2_10.coverage", "82e6f7f3-5af8-43d0-a132-dc6105c39ad2");
        if (ex.getMessage() != null) {
            writeline("/home/ubuntu/results/coverage/Copy/Copy_2_10.coverage", "0c24c199-6761-4956-8fcd-6f4206983864");
            if (!baseIOException) {
                writeline("/home/ubuntu/results/coverage/Copy/Copy_2_10.coverage", "176f4300-efe5-4e01-9845-c3dabb3693df");
                message.append(" ");
            }
            writeline("/home/ubuntu/results/coverage/Copy/Copy_2_10.coverage", "c7ca43a3-8713-42d7-ae74-41107e3ef6d4");
            message.append(ex.getMessage());
        }
        writeline("/home/ubuntu/results/coverage/Copy/Copy_2_10.coverage", "4068b055-91b7-45ac-ab31-ab966908486b");
        if (ex.getClass().getName().indexOf("MalformedInput") != -1) {
            writeline("/home/ubuntu/results/coverage/Copy/Copy_2_10.coverage", "8317f265-d768-4258-9eca-a9246ceabf86");
            message.append(LINE_SEPARATOR);
            writeline("/home/ubuntu/results/coverage/Copy/Copy_2_10.coverage", "0bf37e6a-16c4-4ccf-85e8-8467b2a17bd8");
            message.append("This is normally due to the input file containing invalid");
            writeline("/home/ubuntu/results/coverage/Copy/Copy_2_10.coverage", "535d005e-1564-4a48-9b5d-d69753c1aee1");
            message.append(LINE_SEPARATOR);
            writeline("/home/ubuntu/results/coverage/Copy/Copy_2_10.coverage", "a7939678-92af-4b8b-b341-11350bc27855");
            message.append("bytes for the character encoding used : ");
            writeline("/home/ubuntu/results/coverage/Copy/Copy_2_10.coverage", "8c21595a-dbb0-49d4-8f2f-609367b7f970");
            message.append((inputEncoding == null ? fileUtils.getDefaultEncoding() : inputEncoding));
            writeline("/home/ubuntu/results/coverage/Copy/Copy_2_10.coverage", "01fd6a85-ae4e-4369-83f4-407ccc8cb5a0");
            message.append(LINE_SEPARATOR);
        }
        writeline("/home/ubuntu/results/coverage/Copy/Copy_2_10.coverage", "80ea970d-90a3-434f-ae29-c75ed8d1faa3");
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
