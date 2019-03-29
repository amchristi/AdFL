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
        writeline("/home/ubuntu/results/coverage/Copy/Copy_5_10.coverage", "1f7f8ae9-cf21-4059-8115-9ca6e849f3b9");
        return fileUtils;
    }

    /**
     * Set a single source file to copy.
     * @param file the file to copy.
     */
    public void setFile(final File file) {
        writeline("/home/ubuntu/results/coverage/Copy/Copy_5_10.coverage", "721e29f8-3cc0-4251-88d7-4bb5c1efee58");
        this.file = file;
    }

    /**
     * Set the destination file.
     * @param destFile the file to copy to.
     */
    public void setTofile(final File destFile) {
        writeline("/home/ubuntu/results/coverage/Copy/Copy_5_10.coverage", "ff10f3ab-7d84-4675-b55b-a9697414a06d");
        this.destFile = destFile;
    }

    /**
     * Set the destination directory.
     * @param destDir the destination directory.
     */
    public void setTodir(final File destDir) {
        writeline("/home/ubuntu/results/coverage/Copy/Copy_5_10.coverage", "d36ad98b-6128-42ec-ad00-2cb8900f89bf");
        this.destDir = destDir;
    }

    /**
     * Add a FilterChain.
     * @return a filter chain object.
     */
    public FilterChain createFilterChain() {
        writeline("/home/ubuntu/results/coverage/Copy/Copy_5_10.coverage", "8f05fb62-1f61-4307-b8ba-2b52ca0732d0");
        final FilterChain filterChain = new FilterChain();
        writeline("/home/ubuntu/results/coverage/Copy/Copy_5_10.coverage", "467d790f-783c-4cc7-be09-278b3c50511e");
        filterChains.addElement(filterChain);
        writeline("/home/ubuntu/results/coverage/Copy/Copy_5_10.coverage", "042a2065-fb89-402e-b022-3bcc9f20bc11");
        return filterChain;
    }

    /**
     * Add a filterset.
     * @return a filter set object.
     */
    public FilterSet createFilterSet() {
        writeline("/home/ubuntu/results/coverage/Copy/Copy_5_10.coverage", "62b99307-4135-47d2-ab4b-f1f2b712ea41");
        final FilterSet filterSet = new FilterSet();
        writeline("/home/ubuntu/results/coverage/Copy/Copy_5_10.coverage", "29afd867-4187-4bb3-96ce-384f68c82052");
        filterSets.addElement(filterSet);
        writeline("/home/ubuntu/results/coverage/Copy/Copy_5_10.coverage", "e1482a12-1058-4bad-9393-8a3a99deab51");
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
        writeline("/home/ubuntu/results/coverage/Copy/Copy_5_10.coverage", "996242dd-2d7a-4771-a978-a1f9ae4a0ca3");
        setPreserveLastModified(Project.toBoolean(preserve));
    }

    /**
     * Give the copied files the same last modified time as the original files.
     * @param preserve if true preserve the modified time; default is false.
     */
    public void setPreserveLastModified(final boolean preserve) {
        writeline("/home/ubuntu/results/coverage/Copy/Copy_5_10.coverage", "68ad2bb2-50d0-4043-89a7-8dbc9cbadf59");
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
        writeline("/home/ubuntu/results/coverage/Copy/Copy_5_10.coverage", "cde0ac56-e280-4f8a-beb8-50738e8a2015");
        return preserveLastModified;
    }

    /**
     * Get the filtersets being applied to this operation.
     *
     * @return a vector of FilterSet objects.
     */
    protected Vector<FilterSet> getFilterSets() {
        writeline("/home/ubuntu/results/coverage/Copy/Copy_5_10.coverage", "c51f4449-813d-47d4-881e-976c16adc558");
        return filterSets;
    }

    /**
     * Get the filterchains being applied to this operation.
     *
     * @return a vector of FilterChain objects.
     */
    protected Vector<FilterChain> getFilterChains() {
        writeline("/home/ubuntu/results/coverage/Copy/Copy_5_10.coverage", "ab547c2c-bfe6-4ac9-9f45-8a3a84bdbf48");
        return filterChains;
    }

    /**
     * Set filtering mode.
     * @param filtering if true enable filtering; default is false.
     */
    public void setFiltering(final boolean filtering) {
        writeline("/home/ubuntu/results/coverage/Copy/Copy_5_10.coverage", "3714a4ed-a1d7-4888-8d71-2637901fa5e2");
        this.filtering = filtering;
    }

    /**
     * Set overwrite mode regarding existing destination file(s).
     * @param overwrite if true force overwriting of destination file(s)
     * even if the destination file(s) are younger than
     * the corresponding source file. Default is false.
     */
    public void setOverwrite(final boolean overwrite) {
        writeline("/home/ubuntu/results/coverage/Copy/Copy_5_10.coverage", "f28a7411-e380-4f99-82c6-5edee3911bdc");
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
        writeline("/home/ubuntu/results/coverage/Copy/Copy_5_10.coverage", "632cb549-d82a-49eb-b60d-87366bf8f4c1");
        force = f;
    }

    /**
     * Whether read-only destinations will be overwritten.
     *
     * @since Ant 1.8.2
     */
    public boolean getForce() {
        writeline("/home/ubuntu/results/coverage/Copy/Copy_5_10.coverage", "87232af2-edb8-4409-b1c3-92fcadda4f9d");
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
        writeline("/home/ubuntu/results/coverage/Copy/Copy_5_10.coverage", "74239d2f-9556-4d2d-8541-42a0aee2995e");
        this.flatten = flatten;
    }

    /**
     * Set verbose mode. Used to force listing of all names of copied files.
     * @param verbose whether to output the names of copied files.
     * Default is false.
     */
    public void setVerbose(final boolean verbose) {
        writeline("/home/ubuntu/results/coverage/Copy/Copy_5_10.coverage", "22003eb6-c16f-4b0f-880c-ab5a87f762de");
        this.verbosity = verbose ? Project.MSG_INFO : Project.MSG_VERBOSE;
    }

    /**
     * Set whether to copy empty directories.
     * @param includeEmpty if true copy empty directories. Default is true.
     */
    public void setIncludeEmptyDirs(final boolean includeEmpty) {
        writeline("/home/ubuntu/results/coverage/Copy/Copy_5_10.coverage", "4b74b49b-ba02-4fda-aa75-a60245b87eae");
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
        writeline("/home/ubuntu/results/coverage/Copy/Copy_5_10.coverage", "e9182960-8cf4-4c7b-8047-02d950027912");
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
        writeline("/home/ubuntu/results/coverage/Copy/Copy_5_10.coverage", "0f601b31-649a-413f-a138-d95928d4eac4");
        this.enableMultipleMappings = enableMultipleMappings;
    }

    /**
     * Get whether multiple mapping is enabled.
     * @return true if multiple mapping is enabled; false otherwise.
     */
    public boolean isEnableMultipleMapping() {
        writeline("/home/ubuntu/results/coverage/Copy/Copy_5_10.coverage", "05911123-c988-48d9-896c-787478a0212e");
        return enableMultipleMappings;
    }

    /**
     * Set whether to fail when errors are encountered. If false, note errors
     * to the output but keep going. Default is true.
     * @param failonerror true or false.
     */
    public void setFailOnError(final boolean failonerror) {
        writeline("/home/ubuntu/results/coverage/Copy/Copy_5_10.coverage", "c8209d69-521a-4a58-94ce-d3fa0ef92de7");
        this.failonerror = failonerror;
    }

    /**
     * Add a set of files to copy.
     * @param set a set of files to copy.
     */
    public void addFileset(final FileSet set) {
        writeline("/home/ubuntu/results/coverage/Copy/Copy_5_10.coverage", "92d3fc89-f4ad-4b64-b7a6-c814645fbb98");
        add(set);
    }

    /**
     * Add a collection of files to copy.
     * @param res a resource collection to copy.
     * @since Ant 1.7
     */
    public void add(final ResourceCollection res) {
        writeline("/home/ubuntu/results/coverage/Copy/Copy_5_10.coverage", "38db9a2e-90c1-4acf-b1c7-a1a766f19c85");
        rcs.add(res);
    }

    /**
     * Define the mapper to map source to destination files.
     * @return a mapper to be configured.
     * @exception BuildException if more than one mapper is defined.
     */
    public Mapper createMapper() throws BuildException {
        writeline("/home/ubuntu/results/coverage/Copy/Copy_5_10.coverage", "0c7eef22-cc38-42aa-a92f-75350e30cf5f");
        if (mapperElement != null) {
            writeline("/home/ubuntu/results/coverage/Copy/Copy_5_10.coverage", "db4971de-8bc3-41b5-a95e-1b58a5b21295");
            throw new BuildException("Cannot define more than one mapper", getLocation());
        }
        writeline("/home/ubuntu/results/coverage/Copy/Copy_5_10.coverage", "e2259ae3-e26d-47ad-a8d9-d052c2dacf87");
        mapperElement = new Mapper(getProject());
        writeline("/home/ubuntu/results/coverage/Copy/Copy_5_10.coverage", "96f01525-4de6-4784-97ad-230ef326b0b9");
        return mapperElement;
    }

    /**
     * Add a nested filenamemapper.
     * @param fileNameMapper the mapper to add.
     * @since Ant 1.6.3
     */
    public void add(final FileNameMapper fileNameMapper) {
        writeline("/home/ubuntu/results/coverage/Copy/Copy_5_10.coverage", "565e749c-e440-4eea-9a91-933b55ab7136");
        createMapper().add(fileNameMapper);
    }

    /**
     * Set the character encoding.
     * @param encoding the character encoding.
     * @since 1.32, Ant 1.5
     */
    public void setEncoding(final String encoding) {
        writeline("/home/ubuntu/results/coverage/Copy/Copy_5_10.coverage", "62b5b415-3e29-44d8-ac32-da9bdc83e94f");
        this.inputEncoding = encoding;
        writeline("/home/ubuntu/results/coverage/Copy/Copy_5_10.coverage", "e9558904-ebb0-40b2-87bf-496f1886c1eb");
        if (outputEncoding == null) {
            writeline("/home/ubuntu/results/coverage/Copy/Copy_5_10.coverage", "96de230f-ddb0-46c1-b555-afcb6b8bd57b");
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
        writeline("/home/ubuntu/results/coverage/Copy/Copy_5_10.coverage", "fb7347e1-8f79-4ca6-812f-5fb798a1433a");
        return inputEncoding;
    }

    /**
     * Set the character encoding for output files.
     * @param encoding the output character encoding.
     * @since Ant 1.6
     */
    public void setOutputEncoding(final String encoding) {
        writeline("/home/ubuntu/results/coverage/Copy/Copy_5_10.coverage", "656f6495-473b-448c-a4b2-a8b0fa3a739f");
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
        writeline("/home/ubuntu/results/coverage/Copy/Copy_5_10.coverage", "d20983e4-a884-4ac0-b034-01eb06bb96a1");
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
        writeline("/home/ubuntu/results/coverage/Copy/Copy_5_10.coverage", "9cf30493-4287-4fa0-ab7d-6e23a7e61c8e");
        this.granularity = granularity;
    }

    /**
     * Perform the copy operation.
     * @exception BuildException if an error occurs.
     */
    @Override
    public void execute() throws BuildException {
        writeline("/home/ubuntu/results/coverage/Copy/Copy_5_10.coverage", "34232d6a-40df-4d45-b51b-607de101669f");
        // may be altered in validateAttributes
        final File savedFile = file;
        writeline("/home/ubuntu/results/coverage/Copy/Copy_5_10.coverage", "9ddef725-eec0-471c-a2c7-fffee3f54175");
        final File savedDestFile = destFile;
        writeline("/home/ubuntu/results/coverage/Copy/Copy_5_10.coverage", "7f84b001-680c-4213-8c0b-6f9191703fa2");
        final File savedDestDir = destDir;
        writeline("/home/ubuntu/results/coverage/Copy/Copy_5_10.coverage", "1ad7ae2a-3f06-4c4a-860f-3a30b4c32e52");
        ResourceCollection savedRc = null;
        writeline("/home/ubuntu/results/coverage/Copy/Copy_5_10.coverage", "ddf341af-2543-431b-ac1e-076f1b58632a");
        if (file == null && destFile != null && rcs.size() == 1) {
            writeline("/home/ubuntu/results/coverage/Copy/Copy_5_10.coverage", "bed553b0-335e-4b0f-b7ac-7df58d0d2068");
            // will be removed in validateAttributes
            savedRc = rcs.elementAt(0);
        }
        writeline("/home/ubuntu/results/coverage/Copy/Copy_5_10.coverage", "0fd27243-545d-44c1-a831-8c19fde35b25");
        try {
            writeline("/home/ubuntu/results/coverage/Copy/Copy_5_10.coverage", "b16c2262-8cd6-40de-8d1e-6b7df5abe9ef");
            // make sure we don't have an illegal set of options
            try {
                writeline("/home/ubuntu/results/coverage/Copy/Copy_5_10.coverage", "825878c5-9401-44e2-a526-ffb5a2a0bd5c");
                validateAttributes();
            } catch (final BuildException e) {
                writeline("/home/ubuntu/results/coverage/Copy/Copy_5_10.coverage", "51b11255-a16a-4f79-b779-a8752bcca81e");
                if (failonerror || !getMessage(e).equals(MSG_WHEN_COPYING_EMPTY_RC_TO_FILE)) {
                    writeline("/home/ubuntu/results/coverage/Copy/Copy_5_10.coverage", "20b0c139-f276-452a-952f-61efdf67dcaf");
                    throw e;
                } else {
                    writeline("/home/ubuntu/results/coverage/Copy/Copy_5_10.coverage", "c9911e46-d955-4c48-804b-3b653bbc43f2");
                    log("Warning: " + getMessage(e), Project.MSG_ERR);
                    writeline("/home/ubuntu/results/coverage/Copy/Copy_5_10.coverage", "3232cb96-b69f-4393-a430-ed7e9b9fe271");
                    return;
                }
            }
            writeline("/home/ubuntu/results/coverage/Copy/Copy_5_10.coverage", "0eea75a2-f4c4-4958-8b3e-92af011b27b6");
            // deal with the single file
            copySingleFile();
            writeline("/home/ubuntu/results/coverage/Copy/Copy_5_10.coverage", "ecd19838-831f-49a4-a633-253d9a550a8c");
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
            writeline("/home/ubuntu/results/coverage/Copy/Copy_5_10.coverage", "d17a7847-4ff6-4bb4-8b7c-a45ffd10256d");
            final HashMap<File, List<String>> dirsByBasedir = new HashMap<File, List<String>>();
            writeline("/home/ubuntu/results/coverage/Copy/Copy_5_10.coverage", "d108cd75-5c77-4eb8-ae45-56b6d922010b");
            final HashSet<File> baseDirs = new HashSet<File>();
            writeline("/home/ubuntu/results/coverage/Copy/Copy_5_10.coverage", "5405922b-a8bc-451c-96f3-936772e8e17e");
            final ArrayList<Resource> nonFileResources = new ArrayList<Resource>();
            writeline("/home/ubuntu/results/coverage/Copy/Copy_5_10.coverage", "18443e2a-26b6-4e9e-baa3-954497b0ec50");
            final int size = rcs.size();
            writeline("/home/ubuntu/results/coverage/Copy/Copy_5_10.coverage", "46fe6916-e4bc-4d1e-b02e-d4acb52ba50f");
            for (int i = 0; i < size; i++) {
                writeline("/home/ubuntu/results/coverage/Copy/Copy_5_10.coverage", "a99ef29f-6a8b-4b3b-9d17-754be983af28");
                final ResourceCollection rc = rcs.elementAt(i);
                writeline("/home/ubuntu/results/coverage/Copy/Copy_5_10.coverage", "6297fc6b-7752-48c6-8cac-e543ba8d2ca0");
                // Step (1) - beware of the ZipFileSet
                if (rc instanceof FileSet && rc.isFilesystemOnly()) {
                    writeline("/home/ubuntu/results/coverage/Copy/Copy_5_10.coverage", "c8eca9b4-e9bb-4e7c-80fc-4352a4532d6a");
                    final FileSet fs = (FileSet) rc;
                    writeline("/home/ubuntu/results/coverage/Copy/Copy_5_10.coverage", "49e5f3f9-ec7b-44f0-b467-402f7fe93765");
                    DirectoryScanner ds = null;
                    writeline("/home/ubuntu/results/coverage/Copy/Copy_5_10.coverage", "3011e164-a695-4f19-9ad4-7b9ad3862bcb");
                    try {
                        writeline("/home/ubuntu/results/coverage/Copy/Copy_5_10.coverage", "e517357e-478e-4bff-9a0a-a7735b3d99c9");
                        ds = fs.getDirectoryScanner(getProject());
                    } catch (final BuildException e) {
                        writeline("/home/ubuntu/results/coverage/Copy/Copy_5_10.coverage", "f22a42a0-60a2-49ff-bf94-74f59431bd36");
                        if (failonerror || !getMessage(e).endsWith(DirectoryScanner.DOES_NOT_EXIST_POSTFIX)) {
                            writeline("/home/ubuntu/results/coverage/Copy/Copy_5_10.coverage", "76c358af-1e5c-49cd-80ce-082b28a00c5b");
                            throw e;
                        } else {
                            writeline("/home/ubuntu/results/coverage/Copy/Copy_5_10.coverage", "07585f6a-8351-4003-80d5-4752721d9e17");
                            if (!quiet) {
                                writeline("/home/ubuntu/results/coverage/Copy/Copy_5_10.coverage", "c4c59b19-7c9e-4145-959c-82febdb2c879");
                                log("Warning: " + getMessage(e), Project.MSG_ERR);
                            }
                            writeline("/home/ubuntu/results/coverage/Copy/Copy_5_10.coverage", "251706dc-fdbf-4367-8ce8-9643eb48cabb");
                            continue;
                        }
                    }
                    writeline("/home/ubuntu/results/coverage/Copy/Copy_5_10.coverage", "be587a1e-d4ab-4b8e-a7e2-5bef1f054e23");
                    final File fromDir = fs.getDir(getProject());
                    writeline("/home/ubuntu/results/coverage/Copy/Copy_5_10.coverage", "a8bfb885-1e20-46ab-83cc-d0eca5cb6292");
                    final String[] srcFiles = ds.getIncludedFiles();
                    writeline("/home/ubuntu/results/coverage/Copy/Copy_5_10.coverage", "0f2e331a-87e9-4de1-a7b8-d993a8a5740a");
                    final String[] srcDirs = ds.getIncludedDirectories();
                    writeline("/home/ubuntu/results/coverage/Copy/Copy_5_10.coverage", "b7d8ac8b-4f03-4b5e-bc38-14e055a8a50a");
                    if (!flatten && mapperElement == null && ds.isEverythingIncluded() && !fs.hasPatterns()) {
                        writeline("/home/ubuntu/results/coverage/Copy/Copy_5_10.coverage", "c77c3d1c-0688-4ecd-90fe-3257b9849392");
                        completeDirMap.put(fromDir, destDir);
                    }
                    writeline("/home/ubuntu/results/coverage/Copy/Copy_5_10.coverage", "fb23e45b-0c40-4a03-b078-dec4b76a280f");
                    add(fromDir, srcFiles, filesByBasedir);
                    writeline("/home/ubuntu/results/coverage/Copy/Copy_5_10.coverage", "32389a94-1714-4859-8670-ad1116ac98f6");
                    add(fromDir, srcDirs, dirsByBasedir);
                    writeline("/home/ubuntu/results/coverage/Copy/Copy_5_10.coverage", "b2fe8567-89a2-49f8-b0a3-b8854bc8e910");
                    baseDirs.add(fromDir);
                } else {
                    writeline("/home/ubuntu/results/coverage/Copy/Copy_5_10.coverage", "c59d83db-aff7-486c-be72-b797b6c4d0d7");
                    if (!rc.isFilesystemOnly() && !supportsNonFileResources()) {
                        writeline("/home/ubuntu/results/coverage/Copy/Copy_5_10.coverage", "c154f83e-6065-47dc-9d41-2f37ad6e88d7");
                        throw new BuildException("Only FileSystem resources are supported.");
                    }
                    writeline("/home/ubuntu/results/coverage/Copy/Copy_5_10.coverage", "c111508b-81c4-428e-8886-19ae97792a3b");
                    for (final Resource r : rc) {
                        writeline("/home/ubuntu/results/coverage/Copy/Copy_5_10.coverage", "c8aad3f6-a179-4a3d-b599-78db5a57d64c");
                        if (!r.isExists()) {
                            writeline("/home/ubuntu/results/coverage/Copy/Copy_5_10.coverage", "de55bdb3-d3df-48d3-8753-e15b701ee8b7");
                            final String message = "Warning: Could not find resource " + r.toLongString() + " to copy.";
                            writeline("/home/ubuntu/results/coverage/Copy/Copy_5_10.coverage", "5b47fca6-2f50-49b2-88ac-921b88843091");
                            if (!failonerror) {
                                writeline("/home/ubuntu/results/coverage/Copy/Copy_5_10.coverage", "6c561e31-1814-4ba6-891a-769386adf7d6");
                                if (!quiet) {
                                    writeline("/home/ubuntu/results/coverage/Copy/Copy_5_10.coverage", "118f0f08-d364-4e4c-ad04-d64bdf504070");
                                    log(message, Project.MSG_ERR);
                                }
                            } else {
                                writeline("/home/ubuntu/results/coverage/Copy/Copy_5_10.coverage", "8947291b-5d0f-4cae-a038-5d0021bff4ac");
                                throw new BuildException(message);
                            }
                            writeline("/home/ubuntu/results/coverage/Copy/Copy_5_10.coverage", "07b18753-e32a-461c-958f-f8480884e1df");
                            continue;
                        }
                        writeline("/home/ubuntu/results/coverage/Copy/Copy_5_10.coverage", "e4ca73b1-4323-41dd-bcdb-1ebef3a2eb6e");
                        File baseDir = NULL_FILE_PLACEHOLDER;
                        writeline("/home/ubuntu/results/coverage/Copy/Copy_5_10.coverage", "99310dbd-634c-48b1-97ac-694e44446e84");
                        String name = r.getName();
                        writeline("/home/ubuntu/results/coverage/Copy/Copy_5_10.coverage", "5c5ea3e7-b1ce-4618-8161-6556c24fb9fd");
                        final FileProvider fp = r.as(FileProvider.class);
                        writeline("/home/ubuntu/results/coverage/Copy/Copy_5_10.coverage", "008d761f-672c-4c44-a2d3-91a6c2afb838");
                        if (fp != null) {
                            writeline("/home/ubuntu/results/coverage/Copy/Copy_5_10.coverage", "df182629-5f8b-4606-967f-8feaa8c2a55c");
                            final FileResource fr = ResourceUtils.asFileResource(fp);
                            writeline("/home/ubuntu/results/coverage/Copy/Copy_5_10.coverage", "e31b907e-7b9d-45d5-9fbc-df314bd37452");
                            baseDir = getKeyFile(fr.getBaseDir());
                            writeline("/home/ubuntu/results/coverage/Copy/Copy_5_10.coverage", "ef451f2f-7cfd-40f1-8b3a-12ed40d8898f");
                            if (fr.getBaseDir() == null) {
                                writeline("/home/ubuntu/results/coverage/Copy/Copy_5_10.coverage", "4153cbae-ddd7-45c2-a196-442b5ec8d61e");
                                name = fr.getFile().getAbsolutePath();
                            }
                        }
                        writeline("/home/ubuntu/results/coverage/Copy/Copy_5_10.coverage", "bb382d96-6f39-4b18-b2f6-7959608b6b95");
                        // files.
                        if (r.isDirectory() || fp != null) {
                            writeline("/home/ubuntu/results/coverage/Copy/Copy_5_10.coverage", "57aecbce-481d-47be-b9fa-aebd21e90dfb");
                            add(baseDir, name, r.isDirectory() ? dirsByBasedir : filesByBasedir);
                            writeline("/home/ubuntu/results/coverage/Copy/Copy_5_10.coverage", "11c19285-4d78-460e-851c-cf79865dba73");
                            baseDirs.add(baseDir);
                        } else {
                            writeline("/home/ubuntu/results/coverage/Copy/Copy_5_10.coverage", "00a036df-0ca6-4971-9732-8f9165ad8c8c");
                            // a not-directory file resource
                            // needs special treatment
                            nonFileResources.add(r);
                        }
                    }
                }
            }
            writeline("/home/ubuntu/results/coverage/Copy/Copy_5_10.coverage", "19371e38-c6a5-4d82-8308-19239c243d30");
            iterateOverBaseDirs(baseDirs, dirsByBasedir, filesByBasedir);
            writeline("/home/ubuntu/results/coverage/Copy/Copy_5_10.coverage", "bb05d786-02ff-4f9b-a36f-ae252667f41e");
            // do all the copy operations now...
            try {
                writeline("/home/ubuntu/results/coverage/Copy/Copy_5_10.coverage", "a5d39c7f-8e57-422c-893b-718a0f1800c5");
                doFileOperations();
            } catch (final BuildException e) {
                writeline("/home/ubuntu/results/coverage/Copy/Copy_5_10.coverage", "2ea6769f-058e-43ee-bc78-c5ddd67fad04");
                if (!failonerror) {
                    writeline("/home/ubuntu/results/coverage/Copy/Copy_5_10.coverage", "a98cf10b-af30-4131-b882-af27d6c7e997");
                    if (!quiet) {
                        writeline("/home/ubuntu/results/coverage/Copy/Copy_5_10.coverage", "b35258b3-b898-448b-93b0-b40166106532");
                        log("Warning: " + getMessage(e), Project.MSG_ERR);
                    }
                } else {
                    writeline("/home/ubuntu/results/coverage/Copy/Copy_5_10.coverage", "6df4a656-14b9-470f-99ee-6d8adaa1b42c");
                    throw e;
                }
            }
            writeline("/home/ubuntu/results/coverage/Copy/Copy_5_10.coverage", "1e5e954b-c9d7-40eb-92de-41f71981f79c");
            if (nonFileResources.size() > 0 || singleResource != null) {
                writeline("/home/ubuntu/results/coverage/Copy/Copy_5_10.coverage", "3210b313-99c2-4844-b22a-c68e4fbf8dd8");
                final Resource[] nonFiles = nonFileResources.toArray(new Resource[nonFileResources.size()]);
                writeline("/home/ubuntu/results/coverage/Copy/Copy_5_10.coverage", "53494c6f-b033-42de-9ac6-92bc0e705c2b");
                // restrict to out-of-date resources
                final Map<Resource, String[]> map = scan(nonFiles, destDir);
                writeline("/home/ubuntu/results/coverage/Copy/Copy_5_10.coverage", "3627b1a4-3e3e-428a-90d3-d69a1e25c734");
                if (singleResource != null) {
                    writeline("/home/ubuntu/results/coverage/Copy/Copy_5_10.coverage", "22586a87-105b-4e23-8602-fda7171d7d59");
                    map.put(singleResource, new String[] { destFile.getAbsolutePath() });
                }
                writeline("/home/ubuntu/results/coverage/Copy/Copy_5_10.coverage", "da27aa01-88a6-49cc-93a6-c4d454b80773");
                try {
                    writeline("/home/ubuntu/results/coverage/Copy/Copy_5_10.coverage", "94c01d60-f9e4-41ba-99f8-af62e151bf4e");
                    doResourceOperations(map);
                } catch (final BuildException e) {
                    writeline("/home/ubuntu/results/coverage/Copy/Copy_5_10.coverage", "73071a6e-7fd0-4599-b20c-57642b0e0dea");
                    if (!failonerror) {
                        writeline("/home/ubuntu/results/coverage/Copy/Copy_5_10.coverage", "6db01f39-66b5-467c-a0cb-ba6d7e29b800");
                        if (!quiet) {
                            writeline("/home/ubuntu/results/coverage/Copy/Copy_5_10.coverage", "cad43295-7942-4eba-9e6e-0fb56d8ea9b0");
                            log("Warning: " + getMessage(e), Project.MSG_ERR);
                        }
                    } else {
                        writeline("/home/ubuntu/results/coverage/Copy/Copy_5_10.coverage", "8008df4e-a9c1-43e6-85d1-cf2a158ec9b3");
                        throw e;
                    }
                }
            }
        } finally {
            writeline("/home/ubuntu/results/coverage/Copy/Copy_5_10.coverage", "e6b16dc9-151a-4a1f-86ba-0741b4680b53");
            // clean up again, so this instance can be used a second
            // time
            singleResource = null;
            writeline("/home/ubuntu/results/coverage/Copy/Copy_5_10.coverage", "17f8fd16-5570-45e0-82c8-87fafdcf936c");
            file = savedFile;
            writeline("/home/ubuntu/results/coverage/Copy/Copy_5_10.coverage", "6c54b94d-33aa-462a-8a41-8e2d4f3bb057");
            destFile = savedDestFile;
            writeline("/home/ubuntu/results/coverage/Copy/Copy_5_10.coverage", "3388bb0f-9f00-494d-b13b-781685f33c90");
            destDir = savedDestDir;
            writeline("/home/ubuntu/results/coverage/Copy/Copy_5_10.coverage", "ff1cd9c2-c7bc-4cfc-b77a-b5df715fc47c");
            if (savedRc != null) {
                writeline("/home/ubuntu/results/coverage/Copy/Copy_5_10.coverage", "c2c4ea6c-5b56-4ef3-acfa-7ef2a3bd35d4");
                rcs.insertElementAt(savedRc, 0);
            }
            writeline("/home/ubuntu/results/coverage/Copy/Copy_5_10.coverage", "39406834-2b10-4dad-979f-f9c7e3d5a855");
            fileCopyMap.clear();
            writeline("/home/ubuntu/results/coverage/Copy/Copy_5_10.coverage", "ce198829-bbe0-4ae4-88c2-179b98d8a4cf");
            dirCopyMap.clear();
            writeline("/home/ubuntu/results/coverage/Copy/Copy_5_10.coverage", "a4b46508-e02b-413e-a9a4-e4093d19a30c");
            completeDirMap.clear();
        }
    }

    /**
     * *********************************************************************
     * *  protected and private methods
     * **********************************************************************
     */
    private void copySingleFile() {
        writeline("/home/ubuntu/results/coverage/Copy/Copy_5_10.coverage", "f2f0406b-ad48-4195-bfea-5996cb12e23f");
        // deal with the single file
        if (file != null) {
            writeline("/home/ubuntu/results/coverage/Copy/Copy_5_10.coverage", "1b4c5af1-3230-46c6-8baf-576bbbd97ed8");
            if (file.exists()) {
                writeline("/home/ubuntu/results/coverage/Copy/Copy_5_10.coverage", "646c15a1-9854-45f6-aa1a-17f5cb950165");
                if (destFile == null) {
                    writeline("/home/ubuntu/results/coverage/Copy/Copy_5_10.coverage", "e598c013-79c3-4548-adf4-533dc902fa2a");
                    destFile = new File(destDir, file.getName());
                }
                writeline("/home/ubuntu/results/coverage/Copy/Copy_5_10.coverage", "444e4b2a-be2b-4033-a6b0-dc0654998be6");
                if (forceOverwrite || !destFile.exists() || (file.lastModified() - granularity > destFile.lastModified())) {
                    writeline("/home/ubuntu/results/coverage/Copy/Copy_5_10.coverage", "5b5978fc-4ca6-404c-a075-8dcfec9be3b2");
                    fileCopyMap.put(file.getAbsolutePath(), new String[] { destFile.getAbsolutePath() });
                } else {
                    writeline("/home/ubuntu/results/coverage/Copy/Copy_5_10.coverage", "7d8b6ba8-0644-4028-ac79-96c08b1677d6");
                    log(file + " omitted as " + destFile + " is up to date.", Project.MSG_VERBOSE);
                }
            } else {
                writeline("/home/ubuntu/results/coverage/Copy/Copy_5_10.coverage", "4d1b4f77-65b7-43fd-a98d-7319b7f21480");
                final String message = "Warning: Could not find file " + file.getAbsolutePath() + " to copy.";
                writeline("/home/ubuntu/results/coverage/Copy/Copy_5_10.coverage", "03716307-fc5e-4e66-9828-9b72657585dc");
                if (!failonerror) {
                    writeline("/home/ubuntu/results/coverage/Copy/Copy_5_10.coverage", "c059248e-bffe-42b0-9d5a-6d068c7f33ba");
                    if (!quiet) {
                        writeline("/home/ubuntu/results/coverage/Copy/Copy_5_10.coverage", "db4f3f9a-e467-4421-8f5d-930a2b8854e0");
                        log(message, Project.MSG_ERR);
                    }
                } else {
                    writeline("/home/ubuntu/results/coverage/Copy/Copy_5_10.coverage", "3399d4a5-2e51-4d57-9ee9-2672b51c26f9");
                    throw new BuildException(message);
                }
            }
        }
    }

    private void iterateOverBaseDirs(final HashSet<File> baseDirs, final HashMap<File, List<String>> dirsByBasedir, final HashMap<File, List<String>> filesByBasedir) {
        writeline("/home/ubuntu/results/coverage/Copy/Copy_5_10.coverage", "5e8e6e6a-24bc-417e-8349-a0ef50d32801");
        for (final File f : baseDirs) {
            writeline("/home/ubuntu/results/coverage/Copy/Copy_5_10.coverage", "0d6589f1-0d7f-4c02-b845-e21503fa03a0");
            final List<String> files = filesByBasedir.get(f);
            writeline("/home/ubuntu/results/coverage/Copy/Copy_5_10.coverage", "d8064264-7a25-4238-b76a-8fa3546350ca");
            final List<String> dirs = dirsByBasedir.get(f);
            writeline("/home/ubuntu/results/coverage/Copy/Copy_5_10.coverage", "4313bfce-3c8a-4162-b420-b5bf1d1ee8ab");
            String[] srcFiles = new String[0];
            writeline("/home/ubuntu/results/coverage/Copy/Copy_5_10.coverage", "734b7ed9-021b-4501-be05-78f6ffbffcba");
            if (files != null) {
                writeline("/home/ubuntu/results/coverage/Copy/Copy_5_10.coverage", "a265355e-e19f-449e-8cdf-a21e43d265c9");
                srcFiles = files.toArray(srcFiles);
            }
            writeline("/home/ubuntu/results/coverage/Copy/Copy_5_10.coverage", "1034f2f8-2731-478e-bb43-9c9f1332aa99");
            String[] srcDirs = new String[0];
            writeline("/home/ubuntu/results/coverage/Copy/Copy_5_10.coverage", "1b50c0dc-acdc-458e-baf3-72827db27995");
            if (dirs != null) {
                writeline("/home/ubuntu/results/coverage/Copy/Copy_5_10.coverage", "834cf923-349e-483f-8c3f-276494bdb299");
                srcDirs = dirs.toArray(srcDirs);
            }
            writeline("/home/ubuntu/results/coverage/Copy/Copy_5_10.coverage", "87778581-1216-4303-8a78-205993e2dbc2");
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
        writeline("/home/ubuntu/results/coverage/Copy/Copy_5_10.coverage", "38acb8ec-368b-45b0-b8df-352ca7d839ea");
        if (file == null && rcs.size() == 0) {
            writeline("/home/ubuntu/results/coverage/Copy/Copy_5_10.coverage", "038fcde3-b685-4a6c-852b-71c889b341ad");
            throw new BuildException("Specify at least one source--a file or a resource collection.");
        }
        writeline("/home/ubuntu/results/coverage/Copy/Copy_5_10.coverage", "7030c6a4-8b07-4e9a-a48f-2bab9875c436");
        if (destFile != null && destDir != null) {
            writeline("/home/ubuntu/results/coverage/Copy/Copy_5_10.coverage", "1e5cd4b9-a2d9-4011-814b-e6a8632528dc");
            throw new BuildException("Only one of tofile and todir may be set.");
        }
        writeline("/home/ubuntu/results/coverage/Copy/Copy_5_10.coverage", "7d967cad-595d-4f44-ab69-2053193a918e");
        if (destFile == null && destDir == null) {
            writeline("/home/ubuntu/results/coverage/Copy/Copy_5_10.coverage", "def2a480-a885-49ae-b58d-a217db7d0434");
            throw new BuildException("One of tofile or todir must be set.");
        }
        writeline("/home/ubuntu/results/coverage/Copy/Copy_5_10.coverage", "44abbb28-474a-46c8-80e4-fee5342adf90");
        if (file != null && file.isDirectory()) {
            writeline("/home/ubuntu/results/coverage/Copy/Copy_5_10.coverage", "bfd0c31a-cf8c-4656-b701-1fce40faee89");
            throw new BuildException("Use a resource collection to copy directories.");
        }
        writeline("/home/ubuntu/results/coverage/Copy/Copy_5_10.coverage", "0cba8a8e-832e-42b0-9e44-40f402dce33b");
        if (destFile != null && rcs.size() > 0) {
            writeline("/home/ubuntu/results/coverage/Copy/Copy_5_10.coverage", "4f4cf1ab-d870-426b-bca0-da958c43fc0f");
            if (rcs.size() > 1) {
                writeline("/home/ubuntu/results/coverage/Copy/Copy_5_10.coverage", "5e81bf19-8bfb-4696-9c73-f0b6cf7a6096");
                throw new BuildException("Cannot concatenate multiple files into a single file.");
            } else {
                writeline("/home/ubuntu/results/coverage/Copy/Copy_5_10.coverage", "a2d8575c-893c-4b83-a4c1-26bfb593fe81");
                final ResourceCollection rc = rcs.elementAt(0);
                writeline("/home/ubuntu/results/coverage/Copy/Copy_5_10.coverage", "8fb82856-cc6d-4a2f-8a3c-7b55f084881f");
                if (!rc.isFilesystemOnly() && !supportsNonFileResources()) {
                    writeline("/home/ubuntu/results/coverage/Copy/Copy_5_10.coverage", "6d549030-4940-4252-9b49-9377f9f041ef");
                    throw new BuildException("Only FileSystem resources are" + " supported.");
                }
                writeline("/home/ubuntu/results/coverage/Copy/Copy_5_10.coverage", "6f87ca44-6033-4ef1-9ab2-bacb9da35169");
                if (rc.size() == 0) {
                    writeline("/home/ubuntu/results/coverage/Copy/Copy_5_10.coverage", "cf78158a-17fe-4120-b9c8-7d823103a1f4");
                    throw new BuildException(MSG_WHEN_COPYING_EMPTY_RC_TO_FILE);
                } else if (rc.size() == 1) {
                    writeline("/home/ubuntu/results/coverage/Copy/Copy_5_10.coverage", "44ffb644-0c09-4dc8-80e1-8eb0f90feab7");
                    final Resource res = rc.iterator().next();
                    writeline("/home/ubuntu/results/coverage/Copy/Copy_5_10.coverage", "2bca08c7-f045-4d5a-8d05-d0abacd601a3");
                    final FileProvider r = res.as(FileProvider.class);
                    writeline("/home/ubuntu/results/coverage/Copy/Copy_5_10.coverage", "4c396492-d7da-4577-972e-2c0f84f64d8c");
                    if (file == null) {
                        writeline("/home/ubuntu/results/coverage/Copy/Copy_5_10.coverage", "f14caa6a-e685-4383-b295-eaa3ad80eaf4");
                        if (r != null) {
                            writeline("/home/ubuntu/results/coverage/Copy/Copy_5_10.coverage", "89cd2f18-e448-4005-b970-ff3aecc382ee");
                            file = r.getFile();
                        } else {
                            writeline("/home/ubuntu/results/coverage/Copy/Copy_5_10.coverage", "a2886a39-a4b5-45d4-a955-0b29b927cdab");
                            singleResource = res;
                        }
                        writeline("/home/ubuntu/results/coverage/Copy/Copy_5_10.coverage", "9dd58019-61c6-40a9-b918-a00714b53394");
                        rcs.removeElementAt(0);
                    } else {
                        writeline("/home/ubuntu/results/coverage/Copy/Copy_5_10.coverage", "af4337f2-d1aa-4418-b454-850652fa940e");
                        throw new BuildException("Cannot concatenate multiple files into a single file.");
                    }
                } else {
                    writeline("/home/ubuntu/results/coverage/Copy/Copy_5_10.coverage", "075fe536-0426-43f0-b3a3-cfc93bc2271d");
                    throw new BuildException("Cannot concatenate multiple files into a single file.");
                }
            }
        }
        writeline("/home/ubuntu/results/coverage/Copy/Copy_5_10.coverage", "06a49c9f-5828-449f-bdf4-30d61bda03cd");
        if (destFile != null) {
            writeline("/home/ubuntu/results/coverage/Copy/Copy_5_10.coverage", "48ee712d-16d5-429f-8a59-23c21ee292e0");
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
        writeline("/home/ubuntu/results/coverage/Copy/Copy_5_10.coverage", "c968f528-d026-466c-8201-74f3cef77f03");
        final FileNameMapper mapper = getMapper();
        writeline("/home/ubuntu/results/coverage/Copy/Copy_5_10.coverage", "7f9d6a69-cb61-44a0-a9e4-1dc79df28d08");
        buildMap(fromDir, toDir, files, mapper, fileCopyMap);
        writeline("/home/ubuntu/results/coverage/Copy/Copy_5_10.coverage", "4c5c1b92-b41e-4101-99f4-c2d4aca303f3");
        if (includeEmpty) {
            writeline("/home/ubuntu/results/coverage/Copy/Copy_5_10.coverage", "1ac4b3de-d4b0-4f61-8c73-c7ee4072ac3a");
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
        writeline("/home/ubuntu/results/coverage/Copy/Copy_5_10.coverage", "402f2b3d-07c7-4042-914b-7981602683db");
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
        writeline("/home/ubuntu/results/coverage/Copy/Copy_5_10.coverage", "cc7d582e-e9d2-4299-acda-9716890cbeb7");
        String[] toCopy = null;
        writeline("/home/ubuntu/results/coverage/Copy/Copy_5_10.coverage", "52ebe339-3786-47f3-87ee-0a3d7a814ab4");
        if (forceOverwrite) {
            writeline("/home/ubuntu/results/coverage/Copy/Copy_5_10.coverage", "f4ba53a7-241d-433d-9388-d0eaa6517660");
            final Vector<String> v = new Vector<String>();
            writeline("/home/ubuntu/results/coverage/Copy/Copy_5_10.coverage", "48c3cc39-2f58-4c24-888a-e92986a79f6f");
            for (int i = 0; i < names.length; i++) {
                writeline("/home/ubuntu/results/coverage/Copy/Copy_5_10.coverage", "67a4d808-1600-45b1-83de-41e2463ef3f4");
                if (mapper.mapFileName(names[i]) != null) {
                    writeline("/home/ubuntu/results/coverage/Copy/Copy_5_10.coverage", "f3c9fc34-1631-4869-a57b-8721ca23f64a");
                    v.addElement(names[i]);
                }
            }
            writeline("/home/ubuntu/results/coverage/Copy/Copy_5_10.coverage", "7d92ca98-1b43-4ad6-9ef6-2116e43f9062");
            toCopy = new String[v.size()];
            writeline("/home/ubuntu/results/coverage/Copy/Copy_5_10.coverage", "912050ae-bcd8-4c87-acd5-3098ec9f810b");
            v.copyInto(toCopy);
        } else {
            writeline("/home/ubuntu/results/coverage/Copy/Copy_5_10.coverage", "af75c400-46bf-42bb-a448-67e289b9f499");
            final SourceFileScanner ds = new SourceFileScanner(this);
            writeline("/home/ubuntu/results/coverage/Copy/Copy_5_10.coverage", "b548e260-ec08-4665-84ce-6d5309b7cf32");
            toCopy = ds.restrict(names, fromDir, toDir, mapper, granularity);
        }
        writeline("/home/ubuntu/results/coverage/Copy/Copy_5_10.coverage", "52a412cb-f021-42ec-9870-d8746c8ea91d");
        for (int i = 0; i < toCopy.length; i++) {
            writeline("/home/ubuntu/results/coverage/Copy/Copy_5_10.coverage", "72baaba1-dd2b-4682-b829-2db93ac24540");
            final File src = new File(fromDir, toCopy[i]);
            writeline("/home/ubuntu/results/coverage/Copy/Copy_5_10.coverage", "25425801-c6ec-49f5-aadc-dffa110f5bc2");
            final String[] mappedFiles = mapper.mapFileName(toCopy[i]);
            writeline("/home/ubuntu/results/coverage/Copy/Copy_5_10.coverage", "26adb742-36ea-45f2-aef4-3fe1a14ed9e8");
            if (!enableMultipleMappings) {
                writeline("/home/ubuntu/results/coverage/Copy/Copy_5_10.coverage", "8f5f2a33-63a8-4bba-93b4-053bd28484a3");
                map.put(src.getAbsolutePath(), new String[] { new File(toDir, mappedFiles[0]).getAbsolutePath() });
            } else {
                writeline("/home/ubuntu/results/coverage/Copy/Copy_5_10.coverage", "5de78d6e-ebb7-4d25-a9d0-5f405c6e6746");
                // reuse the array created by the mapper
                for (int k = 0; k < mappedFiles.length; k++) {
                    writeline("/home/ubuntu/results/coverage/Copy/Copy_5_10.coverage", "eb884a6a-2f2d-407f-9b3e-0bdb229554fc");
                    mappedFiles[k] = new File(toDir, mappedFiles[k]).getAbsolutePath();
                }
                writeline("/home/ubuntu/results/coverage/Copy/Copy_5_10.coverage", "d594634c-5ac7-4a7b-bae2-cf7763a64f43");
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
        writeline("/home/ubuntu/results/coverage/Copy/Copy_5_10.coverage", "941e4eca-efb7-4925-ae6d-02aabd660467");
        final HashMap<Resource, String[]> map = new HashMap<Resource, String[]>();
        writeline("/home/ubuntu/results/coverage/Copy/Copy_5_10.coverage", "d27ea47b-f77f-4548-b794-8912869b83a9");
        Resource[] toCopy = null;
        writeline("/home/ubuntu/results/coverage/Copy/Copy_5_10.coverage", "457bad8f-ac29-42ee-acb7-0209d5a752f6");
        if (forceOverwrite) {
            writeline("/home/ubuntu/results/coverage/Copy/Copy_5_10.coverage", "01e96028-096f-4b7a-9d12-2b160784e636");
            final Vector<Resource> v = new Vector<Resource>();
            writeline("/home/ubuntu/results/coverage/Copy/Copy_5_10.coverage", "95c7aada-ac51-4e48-9ffd-bc37315b5258");
            for (int i = 0; i < fromResources.length; i++) {
                writeline("/home/ubuntu/results/coverage/Copy/Copy_5_10.coverage", "4a298d3c-d5c8-43f6-9e6b-2a402b25ac7c");
                if (mapper.mapFileName(fromResources[i].getName()) != null) {
                    writeline("/home/ubuntu/results/coverage/Copy/Copy_5_10.coverage", "8393821e-0bc8-4967-bc2f-8bdb614b754e");
                    v.addElement(fromResources[i]);
                }
            }
            writeline("/home/ubuntu/results/coverage/Copy/Copy_5_10.coverage", "630b74b9-d937-47f3-a83d-424ed7814e6e");
            toCopy = new Resource[v.size()];
            writeline("/home/ubuntu/results/coverage/Copy/Copy_5_10.coverage", "b9304688-7431-4a1c-a657-a1195960565e");
            v.copyInto(toCopy);
        } else {
            writeline("/home/ubuntu/results/coverage/Copy/Copy_5_10.coverage", "69d40030-078e-4cd3-b62c-b9186a5c8a6b");
            toCopy = ResourceUtils.selectOutOfDateSources(this, fromResources, mapper, new ResourceFactory() {

                public Resource getResource(final String name) {
                    return new FileResource(toDir, name);
                }
            }, granularity);
        }
        writeline("/home/ubuntu/results/coverage/Copy/Copy_5_10.coverage", "3d95c57f-ecbb-4885-b3d7-e177122ac08e");
        for (int i = 0; i < toCopy.length; i++) {
            writeline("/home/ubuntu/results/coverage/Copy/Copy_5_10.coverage", "f93f9a81-4cc4-4767-9447-9b90da34a708");
            final String[] mappedFiles = mapper.mapFileName(toCopy[i].getName());
            writeline("/home/ubuntu/results/coverage/Copy/Copy_5_10.coverage", "f2168a93-2bcf-47e1-b19a-594b46d6cfd2");
            for (int j = 0; j < mappedFiles.length; j++) {
                writeline("/home/ubuntu/results/coverage/Copy/Copy_5_10.coverage", "8491ec2f-9dcb-41d1-bb14-76f8c1473ba0");
                if (mappedFiles[j] == null) {
                    writeline("/home/ubuntu/results/coverage/Copy/Copy_5_10.coverage", "f22a998f-a3f0-4cf2-8795-0d0ea37f58a4");
                    throw new BuildException("Can't copy a resource without a" + " name if the mapper doesn't" + " provide one.");
                }
            }
            writeline("/home/ubuntu/results/coverage/Copy/Copy_5_10.coverage", "fa74efdf-2f18-4ad1-864e-176d976e290d");
            if (!enableMultipleMappings) {
                writeline("/home/ubuntu/results/coverage/Copy/Copy_5_10.coverage", "9ac93550-b129-45c8-b0ec-e4c23224f309");
                map.put(toCopy[i], new String[] { new File(toDir, mappedFiles[0]).getAbsolutePath() });
            } else {
                writeline("/home/ubuntu/results/coverage/Copy/Copy_5_10.coverage", "272b11fc-9857-41b6-bd6f-f8d3b7cb11d1");
                // reuse the array created by the mapper
                for (int k = 0; k < mappedFiles.length; k++) {
                    writeline("/home/ubuntu/results/coverage/Copy/Copy_5_10.coverage", "35b28821-0b6a-4deb-968b-25374f187523");
                    mappedFiles[k] = new File(toDir, mappedFiles[k]).getAbsolutePath();
                }
                writeline("/home/ubuntu/results/coverage/Copy/Copy_5_10.coverage", "d7f22953-e9fb-4d0f-9f9c-93c1fd1486be");
                map.put(toCopy[i], mappedFiles);
            }
        }
        writeline("/home/ubuntu/results/coverage/Copy/Copy_5_10.coverage", "87f5569b-8220-4966-8374-29c4718a9bb0");
        return map;
    }

    /**
     * Actually does the file (and possibly empty directory) copies.
     * This is a good method for subclasses to override.
     */
    protected void doFileOperations() {
        writeline("/home/ubuntu/results/coverage/Copy/Copy_5_10.coverage", "38b29f54-eda0-4ac8-a8df-baed03bc1479");
        if (fileCopyMap.size() > 0) {
            writeline("/home/ubuntu/results/coverage/Copy/Copy_5_10.coverage", "54b05b75-07db-4e2e-a2d7-3ecf2bbea76a");
            log("Copying " + fileCopyMap.size() + " file" + (fileCopyMap.size() == 1 ? "" : "s") + " to " + destDir.getAbsolutePath());
            writeline("/home/ubuntu/results/coverage/Copy/Copy_5_10.coverage", "9daafce6-0e57-4be6-bd0f-6058af78982f");
            for (final Map.Entry<String, String[]> e : fileCopyMap.entrySet()) {
                writeline("/home/ubuntu/results/coverage/Copy/Copy_5_10.coverage", "294fd8ca-9edd-4ad1-abe2-f51f30a942f8");
                final String fromFile = e.getKey();
                writeline("/home/ubuntu/results/coverage/Copy/Copy_5_10.coverage", "01a921e6-4d91-49fe-a426-1dc2911f831a");
                final String[] toFiles = e.getValue();
                writeline("/home/ubuntu/results/coverage/Copy/Copy_5_10.coverage", "d46c2cbd-3cc0-4afd-a3ee-8f2f55e7bb08");
                for (int i = 0; i < toFiles.length; i++) {
                    writeline("/home/ubuntu/results/coverage/Copy/Copy_5_10.coverage", "04d6f466-fedf-40e0-89d9-052052dbf5a3");
                    final String toFile = toFiles[i];
                    writeline("/home/ubuntu/results/coverage/Copy/Copy_5_10.coverage", "ecb812af-c207-4f5b-99a5-6081c5299379");
                    if (fromFile.equals(toFile)) {
                        writeline("/home/ubuntu/results/coverage/Copy/Copy_5_10.coverage", "36c51b9c-601e-435a-8439-e7267c6b55d7");
                        log("Skipping self-copy of " + fromFile, verbosity);
                        writeline("/home/ubuntu/results/coverage/Copy/Copy_5_10.coverage", "f04ffa93-6678-46ab-a6e3-be9852ff16a1");
                        continue;
                    }
                    writeline("/home/ubuntu/results/coverage/Copy/Copy_5_10.coverage", "2b2dd199-952f-45a0-baa4-fcfb85a42785");
                    try {
                        writeline("/home/ubuntu/results/coverage/Copy/Copy_5_10.coverage", "9322c3f5-23dc-4a79-8bfe-21daa5980846");
                        log("Copying " + fromFile + " to " + toFile, verbosity);
                        writeline("/home/ubuntu/results/coverage/Copy/Copy_5_10.coverage", "b30a5598-d1b3-41e3-8acf-55dacf0f7781");
                        final FilterSetCollection executionFilters = new FilterSetCollection();
                        writeline("/home/ubuntu/results/coverage/Copy/Copy_5_10.coverage", "29f5f083-0842-455b-9cfa-66323a2521a3");
                        if (filtering) {
                            writeline("/home/ubuntu/results/coverage/Copy/Copy_5_10.coverage", "2d02f329-9570-4d09-bcad-1b58b334cf5b");
                            executionFilters.addFilterSet(getProject().getGlobalFilterSet());
                        }
                        writeline("/home/ubuntu/results/coverage/Copy/Copy_5_10.coverage", "989eb75d-86e5-4010-aa7d-e4c8b138f0bc");
                        for (final FilterSet filterSet : filterSets) {
                            writeline("/home/ubuntu/results/coverage/Copy/Copy_5_10.coverage", "0eee587c-1957-4b1e-b4f3-c7411b94c292");
                            executionFilters.addFilterSet(filterSet);
                        }
                        writeline("/home/ubuntu/results/coverage/Copy/Copy_5_10.coverage", "ff4dbb50-d23d-476d-afe9-8c76dea84914");
                        fileUtils.copyFile(new File(fromFile), new File(toFile), executionFilters, filterChains, forceOverwrite, preserveLastModified, /* append: */
                        false, inputEncoding, outputEncoding, getProject(), getForce());
                    } catch (final IOException ioe) {
                        writeline("/home/ubuntu/results/coverage/Copy/Copy_5_10.coverage", "70f1916f-3dc3-4f4f-9e0b-ebbbab28d5dd");
                        String msg = "Failed to copy " + fromFile + " to " + toFile + " due to " + getDueTo(ioe);
                        writeline("/home/ubuntu/results/coverage/Copy/Copy_5_10.coverage", "02bc9994-a61e-442c-9dc2-077ec9f11d0d");
                        final File targetFile = new File(toFile);
                        writeline("/home/ubuntu/results/coverage/Copy/Copy_5_10.coverage", "59482f56-9beb-4e5e-8e8b-1da98b9e7ca6");
                        if (!(ioe instanceof ResourceUtils.ReadOnlyTargetFileException) && targetFile.exists() && !targetFile.delete()) {
                            writeline("/home/ubuntu/results/coverage/Copy/Copy_5_10.coverage", "d98b80b8-6446-47a5-b482-8f3f0cf7174f");
                            msg += " and I couldn't delete the corrupt " + toFile;
                        }
                        writeline("/home/ubuntu/results/coverage/Copy/Copy_5_10.coverage", "a7eb5fd9-d8a9-4c71-b6a0-0b9ccfdb13a7");
                        if (failonerror) {
                            writeline("/home/ubuntu/results/coverage/Copy/Copy_5_10.coverage", "74e794b5-46ce-460d-b1cf-96962658c02c");
                            throw new BuildException(msg, ioe, getLocation());
                        }
                        writeline("/home/ubuntu/results/coverage/Copy/Copy_5_10.coverage", "7b489890-a691-408b-ac45-bc0b12366b27");
                        log(msg, Project.MSG_ERR);
                    }
                }
            }
        }
        writeline("/home/ubuntu/results/coverage/Copy/Copy_5_10.coverage", "49037ab5-ab9e-48eb-95a4-bedb1af87634");
        if (includeEmpty) {
            writeline("/home/ubuntu/results/coverage/Copy/Copy_5_10.coverage", "a7c94c59-759a-4edb-b97c-b8e3b6e23f1a");
            int createCount = 0;
            writeline("/home/ubuntu/results/coverage/Copy/Copy_5_10.coverage", "29cd5912-ba4a-400d-82a2-7dc8546e82cc");
            for (final String[] dirs : dirCopyMap.values()) {
                writeline("/home/ubuntu/results/coverage/Copy/Copy_5_10.coverage", "00c89e7c-9fd4-4fb9-868c-ba9b2fa6c504");
                for (int i = 0; i < dirs.length; i++) {
                    writeline("/home/ubuntu/results/coverage/Copy/Copy_5_10.coverage", "d1445112-ec60-4d5d-b80b-14e7108d8b92");
                    final File d = new File(dirs[i]);
                    writeline("/home/ubuntu/results/coverage/Copy/Copy_5_10.coverage", "1b7f8b5c-081e-49e4-9eb7-ac886f2e08ca");
                    if (!d.exists()) {
                        writeline("/home/ubuntu/results/coverage/Copy/Copy_5_10.coverage", "3c2f3ad1-7957-40e2-a82f-8d1fae351285");
                        if (!(d.mkdirs() || d.isDirectory())) {
                            writeline("/home/ubuntu/results/coverage/Copy/Copy_5_10.coverage", "4952ca1f-3d96-4ed3-9e06-3e3051372e8d");
                            log("Unable to create directory " + d.getAbsolutePath(), Project.MSG_ERR);
                        } else {
                            writeline("/home/ubuntu/results/coverage/Copy/Copy_5_10.coverage", "af10ab8a-2ec0-4a51-8c2b-9e266d5af909");
                            createCount++;
                        }
                    }
                }
            }
            writeline("/home/ubuntu/results/coverage/Copy/Copy_5_10.coverage", "ed56bf50-41f1-41eb-8c4a-4dcb24e7cb74");
            if (createCount > 0) {
                writeline("/home/ubuntu/results/coverage/Copy/Copy_5_10.coverage", "4fa2e133-6685-484a-ac04-9a4f9411fe63");
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
        writeline("/home/ubuntu/results/coverage/Copy/Copy_5_10.coverage", "944f6284-a020-4d2d-b938-95fae4f6f277");
        if (map.size() > 0) {
            writeline("/home/ubuntu/results/coverage/Copy/Copy_5_10.coverage", "b5408ebd-d8ac-4767-96b3-06cc4145c3f2");
            log("Copying " + map.size() + " resource" + (map.size() == 1 ? "" : "s") + " to " + destDir.getAbsolutePath());
            writeline("/home/ubuntu/results/coverage/Copy/Copy_5_10.coverage", "402392f3-73af-4a02-a01f-c2730987b226");
            for (final Map.Entry<Resource, String[]> e : map.entrySet()) {
                writeline("/home/ubuntu/results/coverage/Copy/Copy_5_10.coverage", "29d450cd-0de1-4322-80d7-97aa5501bed8");
                final Resource fromResource = e.getKey();
                writeline("/home/ubuntu/results/coverage/Copy/Copy_5_10.coverage", "804597f2-81d6-481e-9dae-2e1ea340c87a");
                for (final String toFile : e.getValue()) {
                    writeline("/home/ubuntu/results/coverage/Copy/Copy_5_10.coverage", "b8ec99c3-62ab-4bc8-a9ad-0d1b8a6a0e1c");
                    try {
                        writeline("/home/ubuntu/results/coverage/Copy/Copy_5_10.coverage", "7b624940-eaf7-438a-b82b-79eb13f28744");
                        log("Copying " + fromResource + " to " + toFile, verbosity);
                        writeline("/home/ubuntu/results/coverage/Copy/Copy_5_10.coverage", "3dcbd246-6e13-4e75-8f5d-abf1d526d7ba");
                        final FilterSetCollection executionFilters = new FilterSetCollection();
                        writeline("/home/ubuntu/results/coverage/Copy/Copy_5_10.coverage", "c171e31b-336e-42a2-9123-b8a7f0ac9580");
                        if (filtering) {
                            writeline("/home/ubuntu/results/coverage/Copy/Copy_5_10.coverage", "3aa4a852-97f0-4b2e-acdd-536ba5c0c30f");
                            executionFilters.addFilterSet(getProject().getGlobalFilterSet());
                        }
                        writeline("/home/ubuntu/results/coverage/Copy/Copy_5_10.coverage", "1cba5909-16b0-4de5-9ce4-86d415b46783");
                        for (final FilterSet filterSet : filterSets) {
                            writeline("/home/ubuntu/results/coverage/Copy/Copy_5_10.coverage", "725a93eb-1319-4dbb-bdb7-f66726f381c2");
                            executionFilters.addFilterSet(filterSet);
                        }
                        writeline("/home/ubuntu/results/coverage/Copy/Copy_5_10.coverage", "b464d949-9a3f-4626-a9da-94531f34493e");
                        ResourceUtils.copyResource(fromResource, new FileResource(destDir, toFile), executionFilters, filterChains, forceOverwrite, preserveLastModified, /* append: */
                        false, inputEncoding, outputEncoding, getProject(), getForce());
                    } catch (final IOException ioe) {
                        writeline("/home/ubuntu/results/coverage/Copy/Copy_5_10.coverage", "3ca345e0-663f-4666-bc08-50c85f543726");
                        String msg = "Failed to copy " + fromResource + " to " + toFile + " due to " + getDueTo(ioe);
                        writeline("/home/ubuntu/results/coverage/Copy/Copy_5_10.coverage", "40b52f24-53bf-4970-9059-dd9e83611c39");
                        final File targetFile = new File(toFile);
                        writeline("/home/ubuntu/results/coverage/Copy/Copy_5_10.coverage", "ec307649-7bac-4b0a-b481-a6aa0d70c94b");
                        if (!(ioe instanceof ResourceUtils.ReadOnlyTargetFileException) && targetFile.exists() && !targetFile.delete()) {
                            writeline("/home/ubuntu/results/coverage/Copy/Copy_5_10.coverage", "e2f3e4c6-8a7a-4bc7-90f6-413217f12eb6");
                            msg += " and I couldn't delete the corrupt " + toFile;
                        }
                        writeline("/home/ubuntu/results/coverage/Copy/Copy_5_10.coverage", "255da503-b9e3-4ab0-98db-15968af809a7");
                        if (failonerror) {
                            writeline("/home/ubuntu/results/coverage/Copy/Copy_5_10.coverage", "71f0cd80-2ad1-49fe-99ee-f021e6304f01");
                            throw new BuildException(msg, ioe, getLocation());
                        }
                        writeline("/home/ubuntu/results/coverage/Copy/Copy_5_10.coverage", "e592f8cc-198e-435c-b394-7c45c71fff98");
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
        writeline("/home/ubuntu/results/coverage/Copy/Copy_5_10.coverage", "d60e41e4-88a9-4a69-b556-60e01565f03e");
        return getClass().equals(Copy.class);
    }

    /**
     * Adds the given strings to a list contained in the given map.
     * The file is the key into the map.
     */
    private static void add(File baseDir, final String[] names, final Map<File, List<String>> m) {
        writelineStatic("/home/ubuntu/results/coverage/Copy/Copy_5_10.coverage", "c14870c4-a199-45a0-83df-ca6f8d03ec49");
        if (names != null) {
            writelineStatic("/home/ubuntu/results/coverage/Copy/Copy_5_10.coverage", "bfde9642-419f-471c-8894-2db32dd64fef");
            baseDir = getKeyFile(baseDir);
            writelineStatic("/home/ubuntu/results/coverage/Copy/Copy_5_10.coverage", "85904459-f55f-4438-9e2f-22147e3e1370");
            List<String> l = m.get(baseDir);
            writelineStatic("/home/ubuntu/results/coverage/Copy/Copy_5_10.coverage", "d81eed51-5d87-4050-a955-38bff614eaa5");
            if (l == null) {
                writelineStatic("/home/ubuntu/results/coverage/Copy/Copy_5_10.coverage", "99d9298f-5474-45a7-9be2-18f9d1cd7d83");
                l = new ArrayList<String>(names.length);
                writelineStatic("/home/ubuntu/results/coverage/Copy/Copy_5_10.coverage", "de278b5f-13a8-4b76-ac4b-f0e256171245");
                m.put(baseDir, l);
            }
            writelineStatic("/home/ubuntu/results/coverage/Copy/Copy_5_10.coverage", "b4530774-006b-4d00-9d47-c61eb675612e");
            l.addAll(java.util.Arrays.asList(names));
        }
    }

    /**
     * Adds the given string to a list contained in the given map.
     * The file is the key into the map.
     */
    private static void add(final File baseDir, final String name, final Map<File, List<String>> m) {
        writelineStatic("/home/ubuntu/results/coverage/Copy/Copy_5_10.coverage", "17b801f9-62dd-4e0e-91f3-a3f121574572");
        if (name != null) {
            writelineStatic("/home/ubuntu/results/coverage/Copy/Copy_5_10.coverage", "c267818a-a390-4fb3-a3e5-ab0672b123df");
            add(baseDir, new String[] { name }, m);
        }
    }

    /**
     * Either returns its argument or a plaeholder if the argument is null.
     */
    private static File getKeyFile(final File f) {
        writelineStatic("/home/ubuntu/results/coverage/Copy/Copy_5_10.coverage", "68bf0f77-edd1-4681-a2ff-befd0f22fcf2");
        return f == null ? NULL_FILE_PLACEHOLDER : f;
    }

    /**
     * returns the mapper to use based on nested elements or the
     * flatten attribute.
     */
    private FileNameMapper getMapper() {
        writeline("/home/ubuntu/results/coverage/Copy/Copy_5_10.coverage", "299ce313-24e2-41ca-87d6-35b66feb4eae");
        FileNameMapper mapper = null;
        writeline("/home/ubuntu/results/coverage/Copy/Copy_5_10.coverage", "b59b8231-18aa-4694-86b4-d8bcfdb067fd");
        if (mapperElement != null) {
            writeline("/home/ubuntu/results/coverage/Copy/Copy_5_10.coverage", "f91c308a-14bc-4795-b404-d7a7072b0d50");
            mapper = mapperElement.getImplementation();
        } else if (flatten) {
            writeline("/home/ubuntu/results/coverage/Copy/Copy_5_10.coverage", "fe6cceb2-6549-445e-8f81-400da464db5d");
            mapper = new FlatFileNameMapper();
        } else {
            writeline("/home/ubuntu/results/coverage/Copy/Copy_5_10.coverage", "2bdefbf5-6808-4267-8423-5c65736adc9c");
            mapper = new IdentityMapper();
        }
        writeline("/home/ubuntu/results/coverage/Copy/Copy_5_10.coverage", "0f8adfdd-8292-4dab-a3d8-5265b1752af2");
        return mapper;
    }

    /**
     * Handle getMessage() for exceptions.
     * @param ex the exception to handle
     * @return ex.getMessage() if ex.getMessage() is not null
     * otherwise return ex.toString()
     */
    private String getMessage(final Exception ex) {
        writeline("/home/ubuntu/results/coverage/Copy/Copy_5_10.coverage", "3f6d6b3e-5d3e-42a7-8610-a21ce713151d");
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
        writeline("/home/ubuntu/results/coverage/Copy/Copy_5_10.coverage", "2006d705-f23d-44d6-b662-0f2afc1fa965");
        final boolean baseIOException = ex.getClass() == IOException.class;
        writeline("/home/ubuntu/results/coverage/Copy/Copy_5_10.coverage", "a3217f15-6b55-4e00-bbec-a620485adcaa");
        final StringBuffer message = new StringBuffer();
        writeline("/home/ubuntu/results/coverage/Copy/Copy_5_10.coverage", "a12a9166-bb5f-4906-9cda-c82333abc2f3");
        if (!baseIOException || ex.getMessage() == null) {
            writeline("/home/ubuntu/results/coverage/Copy/Copy_5_10.coverage", "d174e344-b3d5-4eea-ad47-be61ba3b99c0");
            message.append(ex.getClass().getName());
        }
        writeline("/home/ubuntu/results/coverage/Copy/Copy_5_10.coverage", "0706feae-6521-4dc8-ab09-89d05314ab28");
        if (ex.getMessage() != null) {
            writeline("/home/ubuntu/results/coverage/Copy/Copy_5_10.coverage", "83fd8b77-2f4e-4279-b194-55475fdcae10");
            if (!baseIOException) {
                writeline("/home/ubuntu/results/coverage/Copy/Copy_5_10.coverage", "914a2751-1b04-4c98-afdc-0ea81f07bc12");
                message.append(" ");
            }
            writeline("/home/ubuntu/results/coverage/Copy/Copy_5_10.coverage", "2e629deb-a264-429a-9eb8-7fb293f167f6");
            message.append(ex.getMessage());
        }
        writeline("/home/ubuntu/results/coverage/Copy/Copy_5_10.coverage", "19c82602-8e1d-4b43-ac7a-d5184bd9fab5");
        if (ex.getClass().getName().indexOf("MalformedInput") != -1) {
            writeline("/home/ubuntu/results/coverage/Copy/Copy_5_10.coverage", "4fc03585-1320-41d2-8e44-0210e02e3b60");
            message.append(LINE_SEPARATOR);
            writeline("/home/ubuntu/results/coverage/Copy/Copy_5_10.coverage", "5fb06dd5-c2d8-42eb-a8e4-b20f43ebfaf0");
            message.append("This is normally due to the input file containing invalid");
            writeline("/home/ubuntu/results/coverage/Copy/Copy_5_10.coverage", "c634f452-5e9a-4f13-8c86-73d3896d2adc");
            message.append(LINE_SEPARATOR);
            writeline("/home/ubuntu/results/coverage/Copy/Copy_5_10.coverage", "155436c0-4293-48dc-81a8-2011670edccb");
            message.append("bytes for the character encoding used : ");
            writeline("/home/ubuntu/results/coverage/Copy/Copy_5_10.coverage", "b25cbedb-407e-4491-93de-dc7cfa310791");
            message.append((inputEncoding == null ? fileUtils.getDefaultEncoding() : inputEncoding));
            writeline("/home/ubuntu/results/coverage/Copy/Copy_5_10.coverage", "a5d2e9d8-78ee-4772-91a7-0b4aaa7715aa");
            message.append(LINE_SEPARATOR);
        }
        writeline("/home/ubuntu/results/coverage/Copy/Copy_5_10.coverage", "0677ecf1-7c5e-4c70-ae77-7974c17304f3");
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
