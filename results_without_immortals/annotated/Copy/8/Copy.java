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
        writeline("/home/ubuntu/results/coverage/Copy/Copy_8_10.coverage", "f2c26817-3aac-447c-a8bf-aa279a702965");
        return fileUtils;
    }

    /**
     * Set a single source file to copy.
     * @param file the file to copy.
     */
    public void setFile(final File file) {
        writeline("/home/ubuntu/results/coverage/Copy/Copy_8_10.coverage", "18fe949c-308e-4f7c-b211-2385c274f6dd");
        this.file = file;
    }

    /**
     * Set the destination file.
     * @param destFile the file to copy to.
     */
    public void setTofile(final File destFile) {
        writeline("/home/ubuntu/results/coverage/Copy/Copy_8_10.coverage", "e6088de0-c81e-4453-95d8-20ada6eaca17");
        this.destFile = destFile;
    }

    /**
     * Set the destination directory.
     * @param destDir the destination directory.
     */
    public void setTodir(final File destDir) {
        writeline("/home/ubuntu/results/coverage/Copy/Copy_8_10.coverage", "962fca1f-5b85-4c61-9c12-4826c26c51b1");
        this.destDir = destDir;
    }

    /**
     * Add a FilterChain.
     * @return a filter chain object.
     */
    public FilterChain createFilterChain() {
        writeline("/home/ubuntu/results/coverage/Copy/Copy_8_10.coverage", "45ab04ca-e8e4-45f8-b7ef-a0135e5f683a");
        final FilterChain filterChain = new FilterChain();
        writeline("/home/ubuntu/results/coverage/Copy/Copy_8_10.coverage", "6e70048a-ce40-436e-b756-40d2362caaf0");
        filterChains.addElement(filterChain);
        writeline("/home/ubuntu/results/coverage/Copy/Copy_8_10.coverage", "138ccca2-e560-4733-a8bb-e9c0721a7877");
        return filterChain;
    }

    /**
     * Add a filterset.
     * @return a filter set object.
     */
    public FilterSet createFilterSet() {
        writeline("/home/ubuntu/results/coverage/Copy/Copy_8_10.coverage", "72c3317c-95f3-4c10-94e0-a53d2d46bfd6");
        final FilterSet filterSet = new FilterSet();
        writeline("/home/ubuntu/results/coverage/Copy/Copy_8_10.coverage", "b96552eb-8e27-4fc2-8121-bb2026555614");
        filterSets.addElement(filterSet);
        writeline("/home/ubuntu/results/coverage/Copy/Copy_8_10.coverage", "add48d71-371e-4374-86d3-b3147069ccbb");
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
        writeline("/home/ubuntu/results/coverage/Copy/Copy_8_10.coverage", "d4b099f8-965d-4221-8836-2e7e49c59c31");
        setPreserveLastModified(Project.toBoolean(preserve));
    }

    /**
     * Give the copied files the same last modified time as the original files.
     * @param preserve if true preserve the modified time; default is false.
     */
    public void setPreserveLastModified(final boolean preserve) {
        writeline("/home/ubuntu/results/coverage/Copy/Copy_8_10.coverage", "f7c87b15-5ddf-4b2f-ba50-859ffc7dd37e");
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
        writeline("/home/ubuntu/results/coverage/Copy/Copy_8_10.coverage", "440e75f5-1476-4aad-b1a6-87fa1f24835e");
        return preserveLastModified;
    }

    /**
     * Get the filtersets being applied to this operation.
     *
     * @return a vector of FilterSet objects.
     */
    protected Vector<FilterSet> getFilterSets() {
        writeline("/home/ubuntu/results/coverage/Copy/Copy_8_10.coverage", "821dadb6-41b0-480a-a72e-50deebd8b5e8");
        return filterSets;
    }

    /**
     * Get the filterchains being applied to this operation.
     *
     * @return a vector of FilterChain objects.
     */
    protected Vector<FilterChain> getFilterChains() {
        writeline("/home/ubuntu/results/coverage/Copy/Copy_8_10.coverage", "3147acc7-c8ed-4f11-a4a3-580183a093d9");
        return filterChains;
    }

    /**
     * Set filtering mode.
     * @param filtering if true enable filtering; default is false.
     */
    public void setFiltering(final boolean filtering) {
        writeline("/home/ubuntu/results/coverage/Copy/Copy_8_10.coverage", "ce878f52-d8be-4493-818d-ca488cbe56cb");
        this.filtering = filtering;
    }

    /**
     * Set overwrite mode regarding existing destination file(s).
     * @param overwrite if true force overwriting of destination file(s)
     * even if the destination file(s) are younger than
     * the corresponding source file. Default is false.
     */
    public void setOverwrite(final boolean overwrite) {
        writeline("/home/ubuntu/results/coverage/Copy/Copy_8_10.coverage", "54d3b946-daa6-40a0-bdbd-42ac417df87f");
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
        writeline("/home/ubuntu/results/coverage/Copy/Copy_8_10.coverage", "de879296-6b7b-4e0d-875a-9d45fc879935");
        force = f;
    }

    /**
     * Whether read-only destinations will be overwritten.
     *
     * @since Ant 1.8.2
     */
    public boolean getForce() {
        writeline("/home/ubuntu/results/coverage/Copy/Copy_8_10.coverage", "8690a45a-9995-4156-b9f1-3f292e6b0f7a");
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
        writeline("/home/ubuntu/results/coverage/Copy/Copy_8_10.coverage", "820051b6-c300-4c4d-b828-7124a1834ea6");
        this.flatten = flatten;
    }

    /**
     * Set verbose mode. Used to force listing of all names of copied files.
     * @param verbose whether to output the names of copied files.
     * Default is false.
     */
    public void setVerbose(final boolean verbose) {
        writeline("/home/ubuntu/results/coverage/Copy/Copy_8_10.coverage", "e8e20da9-4c09-4577-9623-d43b71189c07");
        this.verbosity = verbose ? Project.MSG_INFO : Project.MSG_VERBOSE;
    }

    /**
     * Set whether to copy empty directories.
     * @param includeEmpty if true copy empty directories. Default is true.
     */
    public void setIncludeEmptyDirs(final boolean includeEmpty) {
        writeline("/home/ubuntu/results/coverage/Copy/Copy_8_10.coverage", "2f3316ad-2257-4314-9be4-077668ff806f");
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
        writeline("/home/ubuntu/results/coverage/Copy/Copy_8_10.coverage", "ba245bb5-b1f9-43e3-b463-ee4c509601c0");
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
        writeline("/home/ubuntu/results/coverage/Copy/Copy_8_10.coverage", "ce5d3881-08f1-48fb-a5e9-e536a120a81e");
        this.enableMultipleMappings = enableMultipleMappings;
    }

    /**
     * Get whether multiple mapping is enabled.
     * @return true if multiple mapping is enabled; false otherwise.
     */
    public boolean isEnableMultipleMapping() {
        writeline("/home/ubuntu/results/coverage/Copy/Copy_8_10.coverage", "36ad7a0e-8043-436b-9fe6-3769146f1af9");
        return enableMultipleMappings;
    }

    /**
     * Set whether to fail when errors are encountered. If false, note errors
     * to the output but keep going. Default is true.
     * @param failonerror true or false.
     */
    public void setFailOnError(final boolean failonerror) {
        writeline("/home/ubuntu/results/coverage/Copy/Copy_8_10.coverage", "8baa895a-aae8-4cb1-bca0-675b23d83544");
        this.failonerror = failonerror;
    }

    /**
     * Add a set of files to copy.
     * @param set a set of files to copy.
     */
    public void addFileset(final FileSet set) {
        writeline("/home/ubuntu/results/coverage/Copy/Copy_8_10.coverage", "9ab5340f-c258-4ef0-818f-55973e698928");
        add(set);
    }

    /**
     * Add a collection of files to copy.
     * @param res a resource collection to copy.
     * @since Ant 1.7
     */
    public void add(final ResourceCollection res) {
        writeline("/home/ubuntu/results/coverage/Copy/Copy_8_10.coverage", "74d27f4f-76c6-4715-83dc-e2132483c7bc");
        rcs.add(res);
    }

    /**
     * Define the mapper to map source to destination files.
     * @return a mapper to be configured.
     * @exception BuildException if more than one mapper is defined.
     */
    public Mapper createMapper() throws BuildException {
        writeline("/home/ubuntu/results/coverage/Copy/Copy_8_10.coverage", "271fce11-384e-4895-820f-b780818f8395");
        if (mapperElement != null) {
            writeline("/home/ubuntu/results/coverage/Copy/Copy_8_10.coverage", "e2e8083a-20bb-4dbd-a38d-024d6f7b0128");
            throw new BuildException("Cannot define more than one mapper", getLocation());
        }
        writeline("/home/ubuntu/results/coverage/Copy/Copy_8_10.coverage", "b796a15a-cf9b-4a9e-9959-d9ffde549d12");
        mapperElement = new Mapper(getProject());
        writeline("/home/ubuntu/results/coverage/Copy/Copy_8_10.coverage", "f24566b4-9436-43f0-8bf0-b4adaff95a04");
        return mapperElement;
    }

    /**
     * Add a nested filenamemapper.
     * @param fileNameMapper the mapper to add.
     * @since Ant 1.6.3
     */
    public void add(final FileNameMapper fileNameMapper) {
        writeline("/home/ubuntu/results/coverage/Copy/Copy_8_10.coverage", "2a9aeba1-3d30-45b5-bbba-ec578814f8a2");
        createMapper().add(fileNameMapper);
    }

    /**
     * Set the character encoding.
     * @param encoding the character encoding.
     * @since 1.32, Ant 1.5
     */
    public void setEncoding(final String encoding) {
        writeline("/home/ubuntu/results/coverage/Copy/Copy_8_10.coverage", "eab496b6-1ef5-477f-ba94-07f6aad2237b");
        this.inputEncoding = encoding;
        writeline("/home/ubuntu/results/coverage/Copy/Copy_8_10.coverage", "66d786a1-cee9-4ec8-b130-8c0bf5842be1");
        if (outputEncoding == null) {
            writeline("/home/ubuntu/results/coverage/Copy/Copy_8_10.coverage", "4eb0f303-49ff-43cb-893b-b2061f4bfac2");
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
        writeline("/home/ubuntu/results/coverage/Copy/Copy_8_10.coverage", "73757b4a-44ca-49d0-b064-25ec42a03292");
        return inputEncoding;
    }

    /**
     * Set the character encoding for output files.
     * @param encoding the output character encoding.
     * @since Ant 1.6
     */
    public void setOutputEncoding(final String encoding) {
        writeline("/home/ubuntu/results/coverage/Copy/Copy_8_10.coverage", "c746da15-c219-4521-b734-afd2fb1c8695");
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
        writeline("/home/ubuntu/results/coverage/Copy/Copy_8_10.coverage", "5b41b80b-f667-4b4c-bcd0-2267b2634be2");
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
        writeline("/home/ubuntu/results/coverage/Copy/Copy_8_10.coverage", "08db3d66-01f7-4d3f-8347-961398f85b21");
        this.granularity = granularity;
    }

    /**
     * Perform the copy operation.
     * @exception BuildException if an error occurs.
     */
    @Override
    public void execute() throws BuildException {
        writeline("/home/ubuntu/results/coverage/Copy/Copy_8_10.coverage", "5e6b34bb-7063-43aa-875c-20ccdbffb2b7");
        // may be altered in validateAttributes
        final File savedFile = file;
        writeline("/home/ubuntu/results/coverage/Copy/Copy_8_10.coverage", "7f2b1840-6174-48a2-8ae4-219fa98e3548");
        final File savedDestFile = destFile;
        writeline("/home/ubuntu/results/coverage/Copy/Copy_8_10.coverage", "a45a845a-8ba5-45f8-b16a-2b7d47827416");
        final File savedDestDir = destDir;
        writeline("/home/ubuntu/results/coverage/Copy/Copy_8_10.coverage", "c9a414cd-c866-4bb1-b68b-2fb25210c241");
        ResourceCollection savedRc = null;
        writeline("/home/ubuntu/results/coverage/Copy/Copy_8_10.coverage", "e7992961-c845-4908-8d27-8e8efadc6758");
        if (file == null && destFile != null && rcs.size() == 1) {
            writeline("/home/ubuntu/results/coverage/Copy/Copy_8_10.coverage", "51af7e62-fdd3-434e-b660-fb2af71d89a8");
            // will be removed in validateAttributes
            savedRc = rcs.elementAt(0);
        }
        writeline("/home/ubuntu/results/coverage/Copy/Copy_8_10.coverage", "54e4452c-f486-451d-9342-abc46519c327");
        try {
            writeline("/home/ubuntu/results/coverage/Copy/Copy_8_10.coverage", "81a62420-b4bf-4b9b-acda-6a1aa114d613");
            // make sure we don't have an illegal set of options
            try {
                writeline("/home/ubuntu/results/coverage/Copy/Copy_8_10.coverage", "c77c1483-13dc-454a-9caa-50e0c4a31ffe");
                validateAttributes();
            } catch (final BuildException e) {
                writeline("/home/ubuntu/results/coverage/Copy/Copy_8_10.coverage", "d4bd06aa-b5c6-457d-a66f-dc2b63bd249b");
                if (failonerror || !getMessage(e).equals(MSG_WHEN_COPYING_EMPTY_RC_TO_FILE)) {
                    writeline("/home/ubuntu/results/coverage/Copy/Copy_8_10.coverage", "451a50a0-0fbd-48bf-a595-5bb7aa9e71ee");
                    throw e;
                } else {
                    writeline("/home/ubuntu/results/coverage/Copy/Copy_8_10.coverage", "66f4088a-ddd0-4961-951a-ff267aa29828");
                    log("Warning: " + getMessage(e), Project.MSG_ERR);
                    writeline("/home/ubuntu/results/coverage/Copy/Copy_8_10.coverage", "7d63e3e4-f323-4747-bd47-c8c4db0272b9");
                    return;
                }
            }
            writeline("/home/ubuntu/results/coverage/Copy/Copy_8_10.coverage", "979be1c9-a0cf-405e-8a4f-bd7e07fe7d96");
            // deal with the single file
            copySingleFile();
            writeline("/home/ubuntu/results/coverage/Copy/Copy_8_10.coverage", "bf601c7f-7727-4f2a-b4ee-1577953d1e6d");
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
            writeline("/home/ubuntu/results/coverage/Copy/Copy_8_10.coverage", "7f4932d3-d251-4668-bfdb-f5518e4f752e");
            final HashMap<File, List<String>> dirsByBasedir = new HashMap<File, List<String>>();
            writeline("/home/ubuntu/results/coverage/Copy/Copy_8_10.coverage", "a8c840fd-fefb-4562-ac8c-4405bd592c2c");
            final HashSet<File> baseDirs = new HashSet<File>();
            writeline("/home/ubuntu/results/coverage/Copy/Copy_8_10.coverage", "b227d979-6739-418f-9cf9-aee97ef2c7f4");
            final ArrayList<Resource> nonFileResources = new ArrayList<Resource>();
            writeline("/home/ubuntu/results/coverage/Copy/Copy_8_10.coverage", "d12fda31-1d05-4349-b47d-0c1c5a801dc4");
            final int size = rcs.size();
            writeline("/home/ubuntu/results/coverage/Copy/Copy_8_10.coverage", "5c48a1c5-7cc2-4fa2-bd77-c8b6128f5607");
            for (int i = 0; i < size; i++) {
                writeline("/home/ubuntu/results/coverage/Copy/Copy_8_10.coverage", "65ec5356-759c-4088-aed6-802b635ed7d0");
                final ResourceCollection rc = rcs.elementAt(i);
                writeline("/home/ubuntu/results/coverage/Copy/Copy_8_10.coverage", "aa0a5d52-d055-47c0-8692-e587dd9cf9ee");
                // Step (1) - beware of the ZipFileSet
                if (rc instanceof FileSet && rc.isFilesystemOnly()) {
                    writeline("/home/ubuntu/results/coverage/Copy/Copy_8_10.coverage", "815bc99b-b31a-44dc-88ed-af0e45e5bdd7");
                    final FileSet fs = (FileSet) rc;
                    writeline("/home/ubuntu/results/coverage/Copy/Copy_8_10.coverage", "1e30de3a-7e5c-4bf1-9a66-3190b6736c1b");
                    DirectoryScanner ds = null;
                    writeline("/home/ubuntu/results/coverage/Copy/Copy_8_10.coverage", "c7bdb737-9e70-4669-a8df-6cd44de69302");
                    try {
                        writeline("/home/ubuntu/results/coverage/Copy/Copy_8_10.coverage", "11772297-3eaa-4b42-b3e5-901da364cd3a");
                        ds = fs.getDirectoryScanner(getProject());
                    } catch (final BuildException e) {
                        writeline("/home/ubuntu/results/coverage/Copy/Copy_8_10.coverage", "77f448af-26ed-4d71-82b6-3ae36ba2d43a");
                        if (failonerror || !getMessage(e).endsWith(DirectoryScanner.DOES_NOT_EXIST_POSTFIX)) {
                            writeline("/home/ubuntu/results/coverage/Copy/Copy_8_10.coverage", "84a12d3f-5213-465f-9ee5-466c39dedda1");
                            throw e;
                        } else {
                            writeline("/home/ubuntu/results/coverage/Copy/Copy_8_10.coverage", "f4b50eff-b435-4002-b60c-6c4973133363");
                            if (!quiet) {
                                writeline("/home/ubuntu/results/coverage/Copy/Copy_8_10.coverage", "c65045aa-539c-48c3-9991-eebc69fc36b0");
                                log("Warning: " + getMessage(e), Project.MSG_ERR);
                            }
                            writeline("/home/ubuntu/results/coverage/Copy/Copy_8_10.coverage", "32d835d1-5bd2-4aaf-b547-2d6634186f97");
                            continue;
                        }
                    }
                    writeline("/home/ubuntu/results/coverage/Copy/Copy_8_10.coverage", "c3e4be21-c331-44e2-b40e-f1987e3f6f3a");
                    final File fromDir = fs.getDir(getProject());
                    writeline("/home/ubuntu/results/coverage/Copy/Copy_8_10.coverage", "203d2848-cb05-4836-885b-818d713ce19f");
                    final String[] srcFiles = ds.getIncludedFiles();
                    writeline("/home/ubuntu/results/coverage/Copy/Copy_8_10.coverage", "2e9b77fe-14cf-434e-ad65-6ea4febd522b");
                    final String[] srcDirs = ds.getIncludedDirectories();
                    writeline("/home/ubuntu/results/coverage/Copy/Copy_8_10.coverage", "5e194022-2b66-4726-819d-4f22f6608a2d");
                    if (!flatten && mapperElement == null && ds.isEverythingIncluded() && !fs.hasPatterns()) {
                        writeline("/home/ubuntu/results/coverage/Copy/Copy_8_10.coverage", "b5bc22ce-451e-4067-93f2-0845b5f7ae0e");
                        completeDirMap.put(fromDir, destDir);
                    }
                    writeline("/home/ubuntu/results/coverage/Copy/Copy_8_10.coverage", "fa2bb11f-fcfd-46c4-9e89-f799349183bb");
                    add(fromDir, srcFiles, filesByBasedir);
                    writeline("/home/ubuntu/results/coverage/Copy/Copy_8_10.coverage", "f0b6408c-e613-45d1-bcb3-433a9b2e02a7");
                    add(fromDir, srcDirs, dirsByBasedir);
                    writeline("/home/ubuntu/results/coverage/Copy/Copy_8_10.coverage", "2df48ead-df7d-4a11-96ce-ebb511c792b6");
                    baseDirs.add(fromDir);
                } else {
                    writeline("/home/ubuntu/results/coverage/Copy/Copy_8_10.coverage", "cb42f189-1fbf-4d0f-bf6d-65cf3eb4d839");
                    if (!rc.isFilesystemOnly() && !supportsNonFileResources()) {
                        writeline("/home/ubuntu/results/coverage/Copy/Copy_8_10.coverage", "3b4b9096-4b55-4e7a-83eb-11b01c7c069b");
                        throw new BuildException("Only FileSystem resources are supported.");
                    }
                    writeline("/home/ubuntu/results/coverage/Copy/Copy_8_10.coverage", "b2a7b5e9-1ef8-466d-ada3-28bc12a292b4");
                    for (final Resource r : rc) {
                        writeline("/home/ubuntu/results/coverage/Copy/Copy_8_10.coverage", "fe52485f-a600-40a9-bb38-313317fcf343");
                        if (!r.isExists()) {
                            writeline("/home/ubuntu/results/coverage/Copy/Copy_8_10.coverage", "f3229bf0-16ed-4de7-b34c-cec977c3f118");
                            final String message = "Warning: Could not find resource " + r.toLongString() + " to copy.";
                            writeline("/home/ubuntu/results/coverage/Copy/Copy_8_10.coverage", "86770463-b89b-4100-8976-68baf5135868");
                            if (!failonerror) {
                                writeline("/home/ubuntu/results/coverage/Copy/Copy_8_10.coverage", "ae4b3b72-607b-4ef9-ac7b-80a17f2aeaeb");
                                if (!quiet) {
                                    writeline("/home/ubuntu/results/coverage/Copy/Copy_8_10.coverage", "58b59034-23cb-4263-a11e-9559fe29a336");
                                    log(message, Project.MSG_ERR);
                                }
                            } else {
                                writeline("/home/ubuntu/results/coverage/Copy/Copy_8_10.coverage", "9ea09fc9-5ab2-4602-a45e-02763b12a836");
                                throw new BuildException(message);
                            }
                            writeline("/home/ubuntu/results/coverage/Copy/Copy_8_10.coverage", "d47943f0-5529-40c6-9482-61b9672a93ad");
                            continue;
                        }
                        writeline("/home/ubuntu/results/coverage/Copy/Copy_8_10.coverage", "0c2c929f-4576-4b9c-8721-3129d2a4e298");
                        File baseDir = NULL_FILE_PLACEHOLDER;
                        writeline("/home/ubuntu/results/coverage/Copy/Copy_8_10.coverage", "d3e27704-40b3-42d4-9297-e681f3730258");
                        String name = r.getName();
                        writeline("/home/ubuntu/results/coverage/Copy/Copy_8_10.coverage", "e77be3f7-c392-4206-9a1a-bb0cda14c881");
                        final FileProvider fp = r.as(FileProvider.class);
                        writeline("/home/ubuntu/results/coverage/Copy/Copy_8_10.coverage", "c0158411-c16e-4a07-9dff-ed5c33ac81bb");
                        if (fp != null) {
                            writeline("/home/ubuntu/results/coverage/Copy/Copy_8_10.coverage", "337a085b-2c6a-48a8-ad08-639130d80cfa");
                            final FileResource fr = ResourceUtils.asFileResource(fp);
                            writeline("/home/ubuntu/results/coverage/Copy/Copy_8_10.coverage", "ae397b9e-5736-4ba2-bd17-686bfe2be3f3");
                            baseDir = getKeyFile(fr.getBaseDir());
                            writeline("/home/ubuntu/results/coverage/Copy/Copy_8_10.coverage", "99606333-20a3-4d96-9fd6-6240cadc549b");
                            if (fr.getBaseDir() == null) {
                                writeline("/home/ubuntu/results/coverage/Copy/Copy_8_10.coverage", "f913f7d2-0d08-4860-a991-db273e890ccb");
                                name = fr.getFile().getAbsolutePath();
                            }
                        }
                        writeline("/home/ubuntu/results/coverage/Copy/Copy_8_10.coverage", "2f71c42e-b9ec-4308-a33a-a65374e01ccf");
                        // files.
                        if (r.isDirectory() || fp != null) {
                            writeline("/home/ubuntu/results/coverage/Copy/Copy_8_10.coverage", "b86c2dff-077d-4c1c-ad98-4dd33473c652");
                            add(baseDir, name, r.isDirectory() ? dirsByBasedir : filesByBasedir);
                            writeline("/home/ubuntu/results/coverage/Copy/Copy_8_10.coverage", "e6497e59-43b6-46f0-9806-658e386c3a9c");
                            baseDirs.add(baseDir);
                        } else {
                            writeline("/home/ubuntu/results/coverage/Copy/Copy_8_10.coverage", "d7fd0f58-398a-48dd-8721-d4e0bf8d82d9");
                            // a not-directory file resource
                            // needs special treatment
                            nonFileResources.add(r);
                        }
                    }
                }
            }
            writeline("/home/ubuntu/results/coverage/Copy/Copy_8_10.coverage", "82752564-6cce-4395-8781-cea796aa814d");
            iterateOverBaseDirs(baseDirs, dirsByBasedir, filesByBasedir);
            writeline("/home/ubuntu/results/coverage/Copy/Copy_8_10.coverage", "06e6211a-1179-4c79-a9d4-9c9bd380a3c8");
            // do all the copy operations now...
            try {
                writeline("/home/ubuntu/results/coverage/Copy/Copy_8_10.coverage", "7131d933-4c22-4d9d-95a7-88be7b02be9b");
                doFileOperations();
            } catch (final BuildException e) {
                writeline("/home/ubuntu/results/coverage/Copy/Copy_8_10.coverage", "a0a747cb-5cba-4e72-9676-659554cd3366");
                if (!failonerror) {
                    writeline("/home/ubuntu/results/coverage/Copy/Copy_8_10.coverage", "b7f8dc3d-3128-4387-96e8-8ee065c77731");
                    if (!quiet) {
                        writeline("/home/ubuntu/results/coverage/Copy/Copy_8_10.coverage", "75d12b6e-a569-4e46-9f8f-8bbe77feb49f");
                        log("Warning: " + getMessage(e), Project.MSG_ERR);
                    }
                } else {
                    writeline("/home/ubuntu/results/coverage/Copy/Copy_8_10.coverage", "3740bbaa-89a7-413f-b868-b4e77c845efc");
                    throw e;
                }
            }
            writeline("/home/ubuntu/results/coverage/Copy/Copy_8_10.coverage", "84fdf016-6df4-48ce-9a96-e6e93697fd9d");
            if (nonFileResources.size() > 0 || singleResource != null) {
                writeline("/home/ubuntu/results/coverage/Copy/Copy_8_10.coverage", "2e8554b5-9fe8-46a3-aa88-77202dc5f7b4");
                final Resource[] nonFiles = nonFileResources.toArray(new Resource[nonFileResources.size()]);
                writeline("/home/ubuntu/results/coverage/Copy/Copy_8_10.coverage", "0dd8cbc6-c09c-4f7e-a93a-7243b4122ac4");
                // restrict to out-of-date resources
                final Map<Resource, String[]> map = scan(nonFiles, destDir);
                writeline("/home/ubuntu/results/coverage/Copy/Copy_8_10.coverage", "31bfb173-a2f7-466c-976d-84e07529b945");
                if (singleResource != null) {
                    writeline("/home/ubuntu/results/coverage/Copy/Copy_8_10.coverage", "32933356-6658-42cb-ae44-b63009c93bc2");
                    map.put(singleResource, new String[] { destFile.getAbsolutePath() });
                }
                writeline("/home/ubuntu/results/coverage/Copy/Copy_8_10.coverage", "43c9beb1-4b4e-4f56-8c88-4cd4f1695844");
                try {
                    writeline("/home/ubuntu/results/coverage/Copy/Copy_8_10.coverage", "fdfbcbd4-8b33-4ca6-b146-840aa2001091");
                    doResourceOperations(map);
                } catch (final BuildException e) {
                    writeline("/home/ubuntu/results/coverage/Copy/Copy_8_10.coverage", "811e4d0c-e189-4b51-ab07-722a6c182eb4");
                    if (!failonerror) {
                        writeline("/home/ubuntu/results/coverage/Copy/Copy_8_10.coverage", "bcad15ce-33a4-422f-a2af-32a997f13c70");
                        if (!quiet) {
                            writeline("/home/ubuntu/results/coverage/Copy/Copy_8_10.coverage", "239f8ac0-04c3-4f02-a00d-e0c7a9346012");
                            log("Warning: " + getMessage(e), Project.MSG_ERR);
                        }
                    } else {
                        writeline("/home/ubuntu/results/coverage/Copy/Copy_8_10.coverage", "beea9fb0-9b9e-46f7-b14b-5ec067aeb8c5");
                        throw e;
                    }
                }
            }
        } finally {
            writeline("/home/ubuntu/results/coverage/Copy/Copy_8_10.coverage", "f0d59109-0052-4178-a51b-574775cb5b2b");
            // clean up again, so this instance can be used a second
            // time
            singleResource = null;
            writeline("/home/ubuntu/results/coverage/Copy/Copy_8_10.coverage", "aa105f27-79ed-44b6-a415-9a2896313a8f");
            file = savedFile;
            writeline("/home/ubuntu/results/coverage/Copy/Copy_8_10.coverage", "2dae03b4-640f-4f38-83cf-599cd8178360");
            destFile = savedDestFile;
            writeline("/home/ubuntu/results/coverage/Copy/Copy_8_10.coverage", "8d6d03f6-e6a9-44cd-adfb-2130dc755a98");
            destDir = savedDestDir;
            writeline("/home/ubuntu/results/coverage/Copy/Copy_8_10.coverage", "5a4dc1a6-98e6-4bfd-8524-c351e08bbb02");
            if (savedRc != null) {
                writeline("/home/ubuntu/results/coverage/Copy/Copy_8_10.coverage", "136980e7-0312-4a76-a76e-baddd2b8a998");
                rcs.insertElementAt(savedRc, 0);
            }
            writeline("/home/ubuntu/results/coverage/Copy/Copy_8_10.coverage", "b559032b-d87e-4d72-8b11-c4e850473c18");
            fileCopyMap.clear();
            writeline("/home/ubuntu/results/coverage/Copy/Copy_8_10.coverage", "62384fe7-a17e-491c-8300-0c7261131d75");
            dirCopyMap.clear();
            writeline("/home/ubuntu/results/coverage/Copy/Copy_8_10.coverage", "3a18b8f0-a9db-45ea-a86e-835b1df07507");
            completeDirMap.clear();
        }
    }

    /**
     * *********************************************************************
     * *  protected and private methods
     * **********************************************************************
     */
    private void copySingleFile() {
        writeline("/home/ubuntu/results/coverage/Copy/Copy_8_10.coverage", "531f88f8-329a-49d7-922f-1d8cdf77a774");
        // deal with the single file
        if (file != null) {
            writeline("/home/ubuntu/results/coverage/Copy/Copy_8_10.coverage", "12728b78-4901-4227-8dc8-15a3db73bb80");
            if (file.exists()) {
                writeline("/home/ubuntu/results/coverage/Copy/Copy_8_10.coverage", "e3ee3630-99ca-436d-be1b-3b825211cef5");
                if (destFile == null) {
                    writeline("/home/ubuntu/results/coverage/Copy/Copy_8_10.coverage", "1be750c2-850f-48c7-ab57-c9800380e509");
                    destFile = new File(destDir, file.getName());
                }
                writeline("/home/ubuntu/results/coverage/Copy/Copy_8_10.coverage", "70eb18ab-f400-40e1-aeac-36472391aea2");
                if (forceOverwrite || !destFile.exists() || (file.lastModified() - granularity > destFile.lastModified())) {
                    writeline("/home/ubuntu/results/coverage/Copy/Copy_8_10.coverage", "a91b993e-a60a-4b47-9044-7bccc33c9d63");
                    fileCopyMap.put(file.getAbsolutePath(), new String[] { destFile.getAbsolutePath() });
                } else {
                    writeline("/home/ubuntu/results/coverage/Copy/Copy_8_10.coverage", "8fac3763-3d63-4207-8311-875b1cc0fe0a");
                    log(file + " omitted as " + destFile + " is up to date.", Project.MSG_VERBOSE);
                }
            } else {
                writeline("/home/ubuntu/results/coverage/Copy/Copy_8_10.coverage", "7cd65f37-b0c1-40e7-afaf-2591dca6dc38");
                final String message = "Warning: Could not find file " + file.getAbsolutePath() + " to copy.";
                writeline("/home/ubuntu/results/coverage/Copy/Copy_8_10.coverage", "01ac483f-1f8b-4dc7-97b0-3e3f0eb98bc5");
                if (!failonerror) {
                    writeline("/home/ubuntu/results/coverage/Copy/Copy_8_10.coverage", "68e26325-c255-4430-a98b-0f969f36d393");
                    if (!quiet) {
                        writeline("/home/ubuntu/results/coverage/Copy/Copy_8_10.coverage", "d0fe3771-9b54-429f-bc05-db29c29d973a");
                        log(message, Project.MSG_ERR);
                    }
                } else {
                    writeline("/home/ubuntu/results/coverage/Copy/Copy_8_10.coverage", "2f37aec3-5296-453e-bd46-274e075ea4cd");
                    throw new BuildException(message);
                }
            }
        }
    }

    private void iterateOverBaseDirs(final HashSet<File> baseDirs, final HashMap<File, List<String>> dirsByBasedir, final HashMap<File, List<String>> filesByBasedir) {
        writeline("/home/ubuntu/results/coverage/Copy/Copy_8_10.coverage", "d4decafe-1d9b-48ad-88c3-735f56ff7e02");
        for (final File f : baseDirs) {
            writeline("/home/ubuntu/results/coverage/Copy/Copy_8_10.coverage", "05cf29ea-946e-4a46-8d55-29c132f225d8");
            final List<String> files = filesByBasedir.get(f);
            writeline("/home/ubuntu/results/coverage/Copy/Copy_8_10.coverage", "2ad01518-f29d-4142-9535-03a3fcba6380");
            final List<String> dirs = dirsByBasedir.get(f);
            writeline("/home/ubuntu/results/coverage/Copy/Copy_8_10.coverage", "b3f95411-603d-40f4-8683-11ecca8f2277");
            String[] srcFiles = new String[0];
            writeline("/home/ubuntu/results/coverage/Copy/Copy_8_10.coverage", "5fcbe77d-0f06-4e15-a80d-7e420ad1d8d3");
            if (files != null) {
                writeline("/home/ubuntu/results/coverage/Copy/Copy_8_10.coverage", "a8c35d2d-4abc-47e8-bf50-e87d6a31694d");
                srcFiles = files.toArray(srcFiles);
            }
            writeline("/home/ubuntu/results/coverage/Copy/Copy_8_10.coverage", "efd33f84-623a-4a61-9c92-d528a3cedd50");
            String[] srcDirs = new String[0];
            writeline("/home/ubuntu/results/coverage/Copy/Copy_8_10.coverage", "6d4f3c71-0d5c-4daa-aee0-fd0d14cd4543");
            if (dirs != null) {
                writeline("/home/ubuntu/results/coverage/Copy/Copy_8_10.coverage", "7ea62e0a-510c-4bff-a77a-a304f122a5b5");
                srcDirs = dirs.toArray(srcDirs);
            }
            writeline("/home/ubuntu/results/coverage/Copy/Copy_8_10.coverage", "4b0aee20-df09-4c99-873a-089babfebbaa");
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
        writeline("/home/ubuntu/results/coverage/Copy/Copy_8_10.coverage", "f5ba544c-445c-4c27-8499-9d52c1fe19b9");
        if (file == null && rcs.size() == 0) {
            writeline("/home/ubuntu/results/coverage/Copy/Copy_8_10.coverage", "12e8f7e2-8f3b-4ed7-9300-848eeb1249f5");
            throw new BuildException("Specify at least one source--a file or a resource collection.");
        }
        writeline("/home/ubuntu/results/coverage/Copy/Copy_8_10.coverage", "1c833610-7d85-4c44-b135-a99c707b2bf6");
        if (destFile != null && destDir != null) {
            writeline("/home/ubuntu/results/coverage/Copy/Copy_8_10.coverage", "713f4b33-f443-4033-944c-1cf670bd9606");
            throw new BuildException("Only one of tofile and todir may be set.");
        }
        writeline("/home/ubuntu/results/coverage/Copy/Copy_8_10.coverage", "0b750fd6-d9ac-4e27-b1d8-87384dd5cd3b");
        if (destFile == null && destDir == null) {
            writeline("/home/ubuntu/results/coverage/Copy/Copy_8_10.coverage", "03a152b9-f853-46fc-a348-03c5072e7289");
            throw new BuildException("One of tofile or todir must be set.");
        }
        writeline("/home/ubuntu/results/coverage/Copy/Copy_8_10.coverage", "2cfaaaf4-dd0c-4e86-b96a-aceec1a5d74d");
        if (file != null && file.isDirectory()) {
            writeline("/home/ubuntu/results/coverage/Copy/Copy_8_10.coverage", "0cb226a7-bfc5-48b1-a729-b4301af1c71c");
            throw new BuildException("Use a resource collection to copy directories.");
        }
        writeline("/home/ubuntu/results/coverage/Copy/Copy_8_10.coverage", "b6e921a3-93a7-4980-b1b5-67104855718f");
        if (destFile != null && rcs.size() > 0) {
            writeline("/home/ubuntu/results/coverage/Copy/Copy_8_10.coverage", "5836dfba-687b-4438-8d72-d048219821aa");
            if (rcs.size() > 1) {
                writeline("/home/ubuntu/results/coverage/Copy/Copy_8_10.coverage", "30beee14-5698-4c71-91cd-b8cb0524cfdc");
                throw new BuildException("Cannot concatenate multiple files into a single file.");
            } else {
                writeline("/home/ubuntu/results/coverage/Copy/Copy_8_10.coverage", "7542c3b8-3211-4c07-996b-5404ab3986a2");
                final ResourceCollection rc = rcs.elementAt(0);
                writeline("/home/ubuntu/results/coverage/Copy/Copy_8_10.coverage", "aa7bfeb9-4562-4129-b573-09033ca1726b");
                if (!rc.isFilesystemOnly() && !supportsNonFileResources()) {
                    writeline("/home/ubuntu/results/coverage/Copy/Copy_8_10.coverage", "a8ec7110-1b0e-4f94-8615-a85e0b4f7267");
                    throw new BuildException("Only FileSystem resources are" + " supported.");
                }
                writeline("/home/ubuntu/results/coverage/Copy/Copy_8_10.coverage", "22aaa44e-6cd2-4bd2-b8a7-150ce0e6c9b8");
                if (rc.size() == 0) {
                    writeline("/home/ubuntu/results/coverage/Copy/Copy_8_10.coverage", "3c21da33-017a-4e85-a9d4-6d2c24754719");
                    throw new BuildException(MSG_WHEN_COPYING_EMPTY_RC_TO_FILE);
                } else if (rc.size() == 1) {
                    writeline("/home/ubuntu/results/coverage/Copy/Copy_8_10.coverage", "d79d1519-1512-4d94-a87a-68644a681b60");
                    final Resource res = rc.iterator().next();
                    writeline("/home/ubuntu/results/coverage/Copy/Copy_8_10.coverage", "10cc4960-ef81-47f3-8c0a-17f0e8de3a8d");
                    final FileProvider r = res.as(FileProvider.class);
                    writeline("/home/ubuntu/results/coverage/Copy/Copy_8_10.coverage", "c6836524-6ece-45b8-aca0-c2a63f05e83b");
                    if (file == null) {
                        writeline("/home/ubuntu/results/coverage/Copy/Copy_8_10.coverage", "b06c83cb-45d0-44bc-90af-07b4ff198a37");
                        if (r != null) {
                            writeline("/home/ubuntu/results/coverage/Copy/Copy_8_10.coverage", "94a2612d-8dac-4e67-a272-ae7860001a1b");
                            file = r.getFile();
                        } else {
                            writeline("/home/ubuntu/results/coverage/Copy/Copy_8_10.coverage", "1acc0780-3adb-4dca-acec-364785b66275");
                            singleResource = res;
                        }
                        writeline("/home/ubuntu/results/coverage/Copy/Copy_8_10.coverage", "7af593a4-3b98-4fb6-89e0-867751e19def");
                        rcs.removeElementAt(0);
                    } else {
                        writeline("/home/ubuntu/results/coverage/Copy/Copy_8_10.coverage", "6f31a9ef-88e5-4070-b530-9c1a89873e63");
                        throw new BuildException("Cannot concatenate multiple files into a single file.");
                    }
                } else {
                    writeline("/home/ubuntu/results/coverage/Copy/Copy_8_10.coverage", "148788e2-07f1-4477-a057-5d63f095933c");
                    throw new BuildException("Cannot concatenate multiple files into a single file.");
                }
            }
        }
        writeline("/home/ubuntu/results/coverage/Copy/Copy_8_10.coverage", "b02f314e-ddf7-4197-a581-6b27fb4a1bb9");
        if (destFile != null) {
            writeline("/home/ubuntu/results/coverage/Copy/Copy_8_10.coverage", "f5c24cb2-52d5-4596-8c28-9b38e720d016");
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
        writeline("/home/ubuntu/results/coverage/Copy/Copy_8_10.coverage", "7e51af84-59c6-40c9-8cb2-2043d4b33232");
        final FileNameMapper mapper = getMapper();
        writeline("/home/ubuntu/results/coverage/Copy/Copy_8_10.coverage", "73b6af43-0ecf-4306-b1e6-4ce6ba4f0eb3");
        buildMap(fromDir, toDir, files, mapper, fileCopyMap);
        writeline("/home/ubuntu/results/coverage/Copy/Copy_8_10.coverage", "9991d152-6f58-4910-a23a-375b2647422b");
        if (includeEmpty) {
            writeline("/home/ubuntu/results/coverage/Copy/Copy_8_10.coverage", "7f2f6565-d968-4497-81a5-f938ff5a7c94");
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
        writeline("/home/ubuntu/results/coverage/Copy/Copy_8_10.coverage", "677b53e4-9a7a-467e-b154-3f77004f9d5a");
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
        writeline("/home/ubuntu/results/coverage/Copy/Copy_8_10.coverage", "2e33c067-6b4a-48a4-94d6-79bb53311a4e");
        String[] toCopy = null;
        writeline("/home/ubuntu/results/coverage/Copy/Copy_8_10.coverage", "a843a270-6f0d-41e8-833a-06f6eb91ff9d");
        if (forceOverwrite) {
            writeline("/home/ubuntu/results/coverage/Copy/Copy_8_10.coverage", "0c71c7c5-b523-4e79-87a7-881004ff7900");
            final Vector<String> v = new Vector<String>();
            writeline("/home/ubuntu/results/coverage/Copy/Copy_8_10.coverage", "60c3d41a-640d-4734-a66c-a5d9033210f3");
            for (int i = 0; i < names.length; i++) {
                writeline("/home/ubuntu/results/coverage/Copy/Copy_8_10.coverage", "478bae66-5d76-4861-9859-76e09d32d2dc");
                if (mapper.mapFileName(names[i]) != null) {
                    writeline("/home/ubuntu/results/coverage/Copy/Copy_8_10.coverage", "86ed9583-3321-45e2-a49f-bcb3d11df273");
                    v.addElement(names[i]);
                }
            }
            writeline("/home/ubuntu/results/coverage/Copy/Copy_8_10.coverage", "0f0ceb26-0427-483c-8b4b-08a2a9b87b7e");
            toCopy = new String[v.size()];
            writeline("/home/ubuntu/results/coverage/Copy/Copy_8_10.coverage", "97f7478a-19c3-46f3-a02c-dfd52139ed41");
            v.copyInto(toCopy);
        } else {
            writeline("/home/ubuntu/results/coverage/Copy/Copy_8_10.coverage", "c4623ca1-5504-49fc-bf2e-f333f5535ef7");
            final SourceFileScanner ds = new SourceFileScanner(this);
            writeline("/home/ubuntu/results/coverage/Copy/Copy_8_10.coverage", "becfbebb-203c-4346-8ad5-8003467c4839");
            toCopy = ds.restrict(names, fromDir, toDir, mapper, granularity);
        }
        writeline("/home/ubuntu/results/coverage/Copy/Copy_8_10.coverage", "df3ec4a4-4ef4-40ce-bf93-4f0c23893549");
        for (int i = 0; i < toCopy.length; i++) {
            writeline("/home/ubuntu/results/coverage/Copy/Copy_8_10.coverage", "855463b0-84cd-416b-b691-fa7a152f8ae9");
            final File src = new File(fromDir, toCopy[i]);
            writeline("/home/ubuntu/results/coverage/Copy/Copy_8_10.coverage", "13c6040d-dc0a-4d0d-aea6-6a0b4f52d499");
            final String[] mappedFiles = mapper.mapFileName(toCopy[i]);
            writeline("/home/ubuntu/results/coverage/Copy/Copy_8_10.coverage", "91b61d89-3c0d-496b-ad4c-b06683e65b61");
            if (!enableMultipleMappings) {
                writeline("/home/ubuntu/results/coverage/Copy/Copy_8_10.coverage", "d6b45e52-3229-4250-8efb-0db87dfd1c54");
                map.put(src.getAbsolutePath(), new String[] { new File(toDir, mappedFiles[0]).getAbsolutePath() });
            } else {
                writeline("/home/ubuntu/results/coverage/Copy/Copy_8_10.coverage", "91b923e3-eb34-486c-9297-07835aa77304");
                // reuse the array created by the mapper
                for (int k = 0; k < mappedFiles.length; k++) {
                    writeline("/home/ubuntu/results/coverage/Copy/Copy_8_10.coverage", "57a3ee77-8e59-4598-8204-da38aad5fb1e");
                    mappedFiles[k] = new File(toDir, mappedFiles[k]).getAbsolutePath();
                }
                writeline("/home/ubuntu/results/coverage/Copy/Copy_8_10.coverage", "8b005092-3d2e-4328-8b9d-b318ec5b6f20");
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
        writeline("/home/ubuntu/results/coverage/Copy/Copy_8_10.coverage", "7923073e-87c4-447f-a418-be2d50c1c948");
        final HashMap<Resource, String[]> map = new HashMap<Resource, String[]>();
        writeline("/home/ubuntu/results/coverage/Copy/Copy_8_10.coverage", "a3aa57dd-d91c-4cb8-b405-f7eedfd4c44a");
        Resource[] toCopy = null;
        writeline("/home/ubuntu/results/coverage/Copy/Copy_8_10.coverage", "4dd81514-e90b-4aba-a927-dd88a485eee8");
        if (forceOverwrite) {
            writeline("/home/ubuntu/results/coverage/Copy/Copy_8_10.coverage", "1e470c10-d73e-40f2-b932-e3a1d2dc8c60");
            final Vector<Resource> v = new Vector<Resource>();
            writeline("/home/ubuntu/results/coverage/Copy/Copy_8_10.coverage", "ad749d1f-231e-4f49-8e59-c49ee06dd41f");
            for (int i = 0; i < fromResources.length; i++) {
                writeline("/home/ubuntu/results/coverage/Copy/Copy_8_10.coverage", "9bb4bd10-e0b6-4d6b-994d-1abe4db91aa6");
                if (mapper.mapFileName(fromResources[i].getName()) != null) {
                    writeline("/home/ubuntu/results/coverage/Copy/Copy_8_10.coverage", "5d372af6-7880-405c-b40e-2c5c8816a7e4");
                    v.addElement(fromResources[i]);
                }
            }
            writeline("/home/ubuntu/results/coverage/Copy/Copy_8_10.coverage", "7d234b64-f246-428c-b025-8504acd4e62b");
            toCopy = new Resource[v.size()];
            writeline("/home/ubuntu/results/coverage/Copy/Copy_8_10.coverage", "5e684022-7f1a-48f4-b795-27d180fe6180");
            v.copyInto(toCopy);
        } else {
            writeline("/home/ubuntu/results/coverage/Copy/Copy_8_10.coverage", "ba85a668-cb60-4143-add5-df2e4b2f0338");
            toCopy = ResourceUtils.selectOutOfDateSources(this, fromResources, mapper, new ResourceFactory() {

                public Resource getResource(final String name) {
                    return new FileResource(toDir, name);
                }
            }, granularity);
        }
        writeline("/home/ubuntu/results/coverage/Copy/Copy_8_10.coverage", "71708161-680f-45e9-8e1b-ace831c9daee");
        for (int i = 0; i < toCopy.length; i++) {
            writeline("/home/ubuntu/results/coverage/Copy/Copy_8_10.coverage", "5a053793-c5c3-4b41-a90c-c711bfc9aa9b");
            final String[] mappedFiles = mapper.mapFileName(toCopy[i].getName());
            writeline("/home/ubuntu/results/coverage/Copy/Copy_8_10.coverage", "30e20c8d-20d1-446b-8468-f55e370b5a9d");
            for (int j = 0; j < mappedFiles.length; j++) {
                writeline("/home/ubuntu/results/coverage/Copy/Copy_8_10.coverage", "04de0e56-7f91-477a-9221-fc274cd6adad");
                if (mappedFiles[j] == null) {
                    writeline("/home/ubuntu/results/coverage/Copy/Copy_8_10.coverage", "c23cc264-692b-4249-80bc-76a17d3c0e73");
                    throw new BuildException("Can't copy a resource without a" + " name if the mapper doesn't" + " provide one.");
                }
            }
            writeline("/home/ubuntu/results/coverage/Copy/Copy_8_10.coverage", "997a4e61-5afc-48f0-a857-a29d346ce836");
            if (!enableMultipleMappings) {
                writeline("/home/ubuntu/results/coverage/Copy/Copy_8_10.coverage", "304c9516-50e9-4b2c-8c5f-8bda55a11b3f");
                map.put(toCopy[i], new String[] { new File(toDir, mappedFiles[0]).getAbsolutePath() });
            } else {
                writeline("/home/ubuntu/results/coverage/Copy/Copy_8_10.coverage", "fcbd8ffb-82bc-4910-bacb-e4141cafa80b");
                // reuse the array created by the mapper
                for (int k = 0; k < mappedFiles.length; k++) {
                    writeline("/home/ubuntu/results/coverage/Copy/Copy_8_10.coverage", "966ba6b6-f8a8-4f9f-a519-a8f6cf092530");
                    mappedFiles[k] = new File(toDir, mappedFiles[k]).getAbsolutePath();
                }
                writeline("/home/ubuntu/results/coverage/Copy/Copy_8_10.coverage", "5f182345-0331-4aea-8ac0-f6a10332736f");
                map.put(toCopy[i], mappedFiles);
            }
        }
        writeline("/home/ubuntu/results/coverage/Copy/Copy_8_10.coverage", "c91d06c2-c9d8-4aeb-8f95-54f103a2530d");
        return map;
    }

    /**
     * Actually does the file (and possibly empty directory) copies.
     * This is a good method for subclasses to override.
     */
    protected void doFileOperations() {
        writeline("/home/ubuntu/results/coverage/Copy/Copy_8_10.coverage", "6bfa3d8d-8e56-4420-8fa0-a1e10024a806");
        if (fileCopyMap.size() > 0) {
            writeline("/home/ubuntu/results/coverage/Copy/Copy_8_10.coverage", "d2afa1eb-caf4-46c0-990a-74e42ba97aff");
            log("Copying " + fileCopyMap.size() + " file" + (fileCopyMap.size() == 1 ? "" : "s") + " to " + destDir.getAbsolutePath());
            writeline("/home/ubuntu/results/coverage/Copy/Copy_8_10.coverage", "720a6f65-3eef-4213-9346-d214d7efc226");
            for (final Map.Entry<String, String[]> e : fileCopyMap.entrySet()) {
                writeline("/home/ubuntu/results/coverage/Copy/Copy_8_10.coverage", "0ccabd8d-a4f6-433e-8bed-bea8e18bb9c0");
                final String fromFile = e.getKey();
                writeline("/home/ubuntu/results/coverage/Copy/Copy_8_10.coverage", "8799ba6a-dedf-48fb-bb7c-e38c6f21b9d7");
                final String[] toFiles = e.getValue();
                writeline("/home/ubuntu/results/coverage/Copy/Copy_8_10.coverage", "5297c60d-f1b9-4e74-b572-70491510c5a6");
                for (int i = 0; i < toFiles.length; i++) {
                    writeline("/home/ubuntu/results/coverage/Copy/Copy_8_10.coverage", "374b583c-460f-418a-8049-1917bc7da321");
                    final String toFile = toFiles[i];
                    writeline("/home/ubuntu/results/coverage/Copy/Copy_8_10.coverage", "0ec52fb0-c1ac-48bf-9bc4-28ea8297b673");
                    if (fromFile.equals(toFile)) {
                        writeline("/home/ubuntu/results/coverage/Copy/Copy_8_10.coverage", "a8c8ed40-d565-491c-8481-d26d1afc5a1d");
                        log("Skipping self-copy of " + fromFile, verbosity);
                        writeline("/home/ubuntu/results/coverage/Copy/Copy_8_10.coverage", "6abeb780-ade5-4200-af48-7d7e7ff00092");
                        continue;
                    }
                    writeline("/home/ubuntu/results/coverage/Copy/Copy_8_10.coverage", "6b431c83-b166-4ae5-8f83-120397366a03");
                    try {
                        writeline("/home/ubuntu/results/coverage/Copy/Copy_8_10.coverage", "18fc0ce2-3749-4f49-a5cb-2db5be468c58");
                        log("Copying " + fromFile + " to " + toFile, verbosity);
                        writeline("/home/ubuntu/results/coverage/Copy/Copy_8_10.coverage", "e7bcc72c-fd58-42f6-ae4e-91a0ed88287d");
                        final FilterSetCollection executionFilters = new FilterSetCollection();
                        writeline("/home/ubuntu/results/coverage/Copy/Copy_8_10.coverage", "ec97904f-47b5-4c0d-9a20-aa1d6b8c3fa1");
                        if (filtering) {
                            writeline("/home/ubuntu/results/coverage/Copy/Copy_8_10.coverage", "48fa389b-47e9-43e9-bfb5-1509e5c0f7fa");
                            executionFilters.addFilterSet(getProject().getGlobalFilterSet());
                        }
                        writeline("/home/ubuntu/results/coverage/Copy/Copy_8_10.coverage", "435973eb-846e-4a22-a5db-c4df1668b03d");
                        for (final FilterSet filterSet : filterSets) {
                            writeline("/home/ubuntu/results/coverage/Copy/Copy_8_10.coverage", "21c6b5fa-f2b0-4c1b-b0aa-697ed0115e39");
                            executionFilters.addFilterSet(filterSet);
                        }
                        writeline("/home/ubuntu/results/coverage/Copy/Copy_8_10.coverage", "4c099ea1-648d-4080-8d9d-7ed81a2ce37a");
                        fileUtils.copyFile(new File(fromFile), new File(toFile), executionFilters, filterChains, forceOverwrite, preserveLastModified, /* append: */
                        false, inputEncoding, outputEncoding, getProject(), getForce());
                    } catch (final IOException ioe) {
                        writeline("/home/ubuntu/results/coverage/Copy/Copy_8_10.coverage", "6b6eb486-1ef8-42a4-9aa5-8c9c785e8f16");
                        String msg = "Failed to copy " + fromFile + " to " + toFile + " due to " + getDueTo(ioe);
                        writeline("/home/ubuntu/results/coverage/Copy/Copy_8_10.coverage", "a8028524-3edb-410b-9b8a-0e45d4fb2963");
                        final File targetFile = new File(toFile);
                        writeline("/home/ubuntu/results/coverage/Copy/Copy_8_10.coverage", "9b36deb6-eaf8-4747-8cf4-286302c50b41");
                        if (!(ioe instanceof ResourceUtils.ReadOnlyTargetFileException) && targetFile.exists() && !targetFile.delete()) {
                            writeline("/home/ubuntu/results/coverage/Copy/Copy_8_10.coverage", "6670db2e-7873-494d-9de0-9239ea3d4ebe");
                            msg += " and I couldn't delete the corrupt " + toFile;
                        }
                        writeline("/home/ubuntu/results/coverage/Copy/Copy_8_10.coverage", "261e38a5-2234-4748-83da-5da7eba3a220");
                        if (failonerror) {
                            writeline("/home/ubuntu/results/coverage/Copy/Copy_8_10.coverage", "1f4d817e-543a-4f82-9893-68256499f876");
                            throw new BuildException(msg, ioe, getLocation());
                        }
                        writeline("/home/ubuntu/results/coverage/Copy/Copy_8_10.coverage", "d502fa49-e7f1-490f-af9a-f6a9961978ff");
                        log(msg, Project.MSG_ERR);
                    }
                }
            }
        }
        writeline("/home/ubuntu/results/coverage/Copy/Copy_8_10.coverage", "e4015caf-25f4-4527-ac94-583d5983bbd7");
        if (includeEmpty) {
            writeline("/home/ubuntu/results/coverage/Copy/Copy_8_10.coverage", "c00f0d1b-ff9c-4d91-ab75-822dc33c1571");
            int createCount = 0;
            writeline("/home/ubuntu/results/coverage/Copy/Copy_8_10.coverage", "6ded9215-ca98-4830-8d37-3fe0e57c6b91");
            for (final String[] dirs : dirCopyMap.values()) {
                writeline("/home/ubuntu/results/coverage/Copy/Copy_8_10.coverage", "4a708e54-bfed-4688-b1de-101dc0fb2e18");
                for (int i = 0; i < dirs.length; i++) {
                    writeline("/home/ubuntu/results/coverage/Copy/Copy_8_10.coverage", "5c899c5b-a765-46c4-a10b-6ec2e5dc9791");
                    final File d = new File(dirs[i]);
                    writeline("/home/ubuntu/results/coverage/Copy/Copy_8_10.coverage", "1c2d27f1-c549-4dd2-824a-3afee8b58296");
                    if (!d.exists()) {
                        writeline("/home/ubuntu/results/coverage/Copy/Copy_8_10.coverage", "46a478fd-1c9a-48a6-a53c-5545653b7caa");
                        if (!(d.mkdirs() || d.isDirectory())) {
                            writeline("/home/ubuntu/results/coverage/Copy/Copy_8_10.coverage", "295a9e93-7e70-4305-bbeb-275efb516111");
                            log("Unable to create directory " + d.getAbsolutePath(), Project.MSG_ERR);
                        } else {
                            writeline("/home/ubuntu/results/coverage/Copy/Copy_8_10.coverage", "a0dabd7c-b31e-4229-9b95-eb2f91119cf5");
                            createCount++;
                        }
                    }
                }
            }
            writeline("/home/ubuntu/results/coverage/Copy/Copy_8_10.coverage", "1b220238-27f1-4839-9518-c0b2fdfccbf7");
            if (createCount > 0) {
                writeline("/home/ubuntu/results/coverage/Copy/Copy_8_10.coverage", "914793a0-b9b8-47d8-8d1a-b711613a1ebf");
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
        writeline("/home/ubuntu/results/coverage/Copy/Copy_8_10.coverage", "d0d37759-f9f2-4e9b-9d75-b081d891cdc3");
        if (map.size() > 0) {
            writeline("/home/ubuntu/results/coverage/Copy/Copy_8_10.coverage", "8ca46e79-5c12-4b0c-8c01-aff08ee66832");
            log("Copying " + map.size() + " resource" + (map.size() == 1 ? "" : "s") + " to " + destDir.getAbsolutePath());
            writeline("/home/ubuntu/results/coverage/Copy/Copy_8_10.coverage", "02dc6a77-ee61-472e-b2bf-fc219470ebfc");
            for (final Map.Entry<Resource, String[]> e : map.entrySet()) {
                writeline("/home/ubuntu/results/coverage/Copy/Copy_8_10.coverage", "177c2afd-3e6d-4e38-878a-8c9ab45cf633");
                final Resource fromResource = e.getKey();
                writeline("/home/ubuntu/results/coverage/Copy/Copy_8_10.coverage", "79dad7e1-4acc-4e21-9859-85abe5255232");
                for (final String toFile : e.getValue()) {
                    writeline("/home/ubuntu/results/coverage/Copy/Copy_8_10.coverage", "6ae14268-0505-4105-88ee-96bbf2634c20");
                    try {
                        writeline("/home/ubuntu/results/coverage/Copy/Copy_8_10.coverage", "a921a6cd-bc97-4217-8cbc-f1662bc5cb46");
                        log("Copying " + fromResource + " to " + toFile, verbosity);
                        writeline("/home/ubuntu/results/coverage/Copy/Copy_8_10.coverage", "4c66bc8a-25d5-4d69-a3e3-f529014ca68e");
                        final FilterSetCollection executionFilters = new FilterSetCollection();
                        writeline("/home/ubuntu/results/coverage/Copy/Copy_8_10.coverage", "5fc4745b-015e-46ba-8095-723c3099ae62");
                        if (filtering) {
                            writeline("/home/ubuntu/results/coverage/Copy/Copy_8_10.coverage", "f68ebd4d-658a-457c-b979-4b53bdde21d5");
                            executionFilters.addFilterSet(getProject().getGlobalFilterSet());
                        }
                        writeline("/home/ubuntu/results/coverage/Copy/Copy_8_10.coverage", "2f1c9e6b-58fb-49f0-aa97-3a5a0bc15aba");
                        for (final FilterSet filterSet : filterSets) {
                            writeline("/home/ubuntu/results/coverage/Copy/Copy_8_10.coverage", "1b596490-3754-4e13-9485-3fe6fb951377");
                            executionFilters.addFilterSet(filterSet);
                        }
                        writeline("/home/ubuntu/results/coverage/Copy/Copy_8_10.coverage", "6043d6f8-65fe-4fd3-b6d6-965fec28dd99");
                        ResourceUtils.copyResource(fromResource, new FileResource(destDir, toFile), executionFilters, filterChains, forceOverwrite, preserveLastModified, /* append: */
                        false, inputEncoding, outputEncoding, getProject(), getForce());
                    } catch (final IOException ioe) {
                        writeline("/home/ubuntu/results/coverage/Copy/Copy_8_10.coverage", "0c626f39-59a6-49e5-a696-0f819264bc11");
                        String msg = "Failed to copy " + fromResource + " to " + toFile + " due to " + getDueTo(ioe);
                        writeline("/home/ubuntu/results/coverage/Copy/Copy_8_10.coverage", "b835dfd4-0c17-4a40-9133-f712f9145f45");
                        final File targetFile = new File(toFile);
                        writeline("/home/ubuntu/results/coverage/Copy/Copy_8_10.coverage", "9a12d266-6db8-451f-a808-2496a727eead");
                        if (!(ioe instanceof ResourceUtils.ReadOnlyTargetFileException) && targetFile.exists() && !targetFile.delete()) {
                            writeline("/home/ubuntu/results/coverage/Copy/Copy_8_10.coverage", "f639fa61-cd14-4716-a019-bbd40ea1753d");
                            msg += " and I couldn't delete the corrupt " + toFile;
                        }
                        writeline("/home/ubuntu/results/coverage/Copy/Copy_8_10.coverage", "100fbdce-8496-44ec-9510-82144547f937");
                        if (failonerror) {
                            writeline("/home/ubuntu/results/coverage/Copy/Copy_8_10.coverage", "f4f55a12-c0e0-4113-9ae5-7bbe7711c0e4");
                            throw new BuildException(msg, ioe, getLocation());
                        }
                        writeline("/home/ubuntu/results/coverage/Copy/Copy_8_10.coverage", "a8a5ede7-3d7b-4148-8169-d6741f5b0ebf");
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
        writeline("/home/ubuntu/results/coverage/Copy/Copy_8_10.coverage", "70335d31-2410-4dda-a379-b9d8c916f69a");
        return getClass().equals(Copy.class);
    }

    /**
     * Adds the given strings to a list contained in the given map.
     * The file is the key into the map.
     */
    private static void add(File baseDir, final String[] names, final Map<File, List<String>> m) {
        writelineStatic("/home/ubuntu/results/coverage/Copy/Copy_8_10.coverage", "745cacea-ebf7-4ba1-91e4-348a3628a8fd");
        if (names != null) {
            writelineStatic("/home/ubuntu/results/coverage/Copy/Copy_8_10.coverage", "e6ef063b-1e08-4b6f-99ec-a203c4953581");
            baseDir = getKeyFile(baseDir);
            writelineStatic("/home/ubuntu/results/coverage/Copy/Copy_8_10.coverage", "5340a977-4cfa-4c08-a376-7423671a7541");
            List<String> l = m.get(baseDir);
            writelineStatic("/home/ubuntu/results/coverage/Copy/Copy_8_10.coverage", "4462abd7-f26d-4fd9-8a91-d80194d006e5");
            if (l == null) {
                writelineStatic("/home/ubuntu/results/coverage/Copy/Copy_8_10.coverage", "5b0c2ebc-3582-4bab-adeb-1bd521e69698");
                l = new ArrayList<String>(names.length);
                writelineStatic("/home/ubuntu/results/coverage/Copy/Copy_8_10.coverage", "12bbe3cd-5f61-4316-a94d-6435b30a1477");
                m.put(baseDir, l);
            }
            writelineStatic("/home/ubuntu/results/coverage/Copy/Copy_8_10.coverage", "8e42b604-9f6d-4253-b540-704da09eb518");
            l.addAll(java.util.Arrays.asList(names));
        }
    }

    /**
     * Adds the given string to a list contained in the given map.
     * The file is the key into the map.
     */
    private static void add(final File baseDir, final String name, final Map<File, List<String>> m) {
        writelineStatic("/home/ubuntu/results/coverage/Copy/Copy_8_10.coverage", "1347d04b-d8fa-4070-89a6-7417ffba5931");
        if (name != null) {
            writelineStatic("/home/ubuntu/results/coverage/Copy/Copy_8_10.coverage", "765065f9-8357-4f79-8080-0565ed301ab3");
            add(baseDir, new String[] { name }, m);
        }
    }

    /**
     * Either returns its argument or a plaeholder if the argument is null.
     */
    private static File getKeyFile(final File f) {
        writelineStatic("/home/ubuntu/results/coverage/Copy/Copy_8_10.coverage", "0ffb627a-e7f7-4a68-9847-1ac4d3e794ba");
        return f == null ? NULL_FILE_PLACEHOLDER : f;
    }

    /**
     * returns the mapper to use based on nested elements or the
     * flatten attribute.
     */
    private FileNameMapper getMapper() {
        writeline("/home/ubuntu/results/coverage/Copy/Copy_8_10.coverage", "99a88f76-d48d-4a8a-9b72-5b1b31e0d1c5");
        FileNameMapper mapper = null;
        writeline("/home/ubuntu/results/coverage/Copy/Copy_8_10.coverage", "1cc29089-cf6e-42a4-abff-c7a60f1b9ec0");
        if (mapperElement != null) {
            writeline("/home/ubuntu/results/coverage/Copy/Copy_8_10.coverage", "3eee5f62-da86-467e-82c2-3f2bd348bb0f");
            mapper = mapperElement.getImplementation();
        } else if (flatten) {
            writeline("/home/ubuntu/results/coverage/Copy/Copy_8_10.coverage", "fc5eef94-23a2-4bfe-bf3d-882822198012");
            mapper = new FlatFileNameMapper();
        } else {
            writeline("/home/ubuntu/results/coverage/Copy/Copy_8_10.coverage", "f46c0047-2289-431c-b032-d6051ec509bc");
            mapper = new IdentityMapper();
        }
        writeline("/home/ubuntu/results/coverage/Copy/Copy_8_10.coverage", "3053f991-fba4-44dd-b020-643df874ec3d");
        return mapper;
    }

    /**
     * Handle getMessage() for exceptions.
     * @param ex the exception to handle
     * @return ex.getMessage() if ex.getMessage() is not null
     * otherwise return ex.toString()
     */
    private String getMessage(final Exception ex) {
        writeline("/home/ubuntu/results/coverage/Copy/Copy_8_10.coverage", "cea0c6e2-e933-4948-a03e-177a220589fe");
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
        writeline("/home/ubuntu/results/coverage/Copy/Copy_8_10.coverage", "972b31f5-2b49-4612-a430-80caae58f937");
        final boolean baseIOException = ex.getClass() == IOException.class;
        writeline("/home/ubuntu/results/coverage/Copy/Copy_8_10.coverage", "b9a4db43-45af-46a8-a5bb-4fe448b58a1f");
        final StringBuffer message = new StringBuffer();
        writeline("/home/ubuntu/results/coverage/Copy/Copy_8_10.coverage", "688a9d3a-f6a9-4454-884d-6389e91cdb70");
        if (!baseIOException || ex.getMessage() == null) {
            writeline("/home/ubuntu/results/coverage/Copy/Copy_8_10.coverage", "3fb91d4c-0198-4d43-87e8-8aab7063b703");
            message.append(ex.getClass().getName());
        }
        writeline("/home/ubuntu/results/coverage/Copy/Copy_8_10.coverage", "49cd97a4-3d64-4a7b-8133-d2845aa6ba8b");
        if (ex.getMessage() != null) {
            writeline("/home/ubuntu/results/coverage/Copy/Copy_8_10.coverage", "530594c8-50cd-4b94-b9e9-417b662103a7");
            if (!baseIOException) {
                writeline("/home/ubuntu/results/coverage/Copy/Copy_8_10.coverage", "c218d781-85f7-4073-ba38-ccd50eb10e99");
                message.append(" ");
            }
            writeline("/home/ubuntu/results/coverage/Copy/Copy_8_10.coverage", "14aaf4aa-25f2-42a6-b655-f8c3fefc27e8");
            message.append(ex.getMessage());
        }
        writeline("/home/ubuntu/results/coverage/Copy/Copy_8_10.coverage", "f355db6e-a980-4221-88f3-ad05b9466434");
        if (ex.getClass().getName().indexOf("MalformedInput") != -1) {
            writeline("/home/ubuntu/results/coverage/Copy/Copy_8_10.coverage", "2b485956-3e84-429a-b5fc-603cd057a102");
            message.append(LINE_SEPARATOR);
            writeline("/home/ubuntu/results/coverage/Copy/Copy_8_10.coverage", "097b3ad2-681a-450f-808b-c93ef04baccd");
            message.append("This is normally due to the input file containing invalid");
            writeline("/home/ubuntu/results/coverage/Copy/Copy_8_10.coverage", "437abbc4-ec31-405e-9532-df22a01426c7");
            message.append(LINE_SEPARATOR);
            writeline("/home/ubuntu/results/coverage/Copy/Copy_8_10.coverage", "70e38ead-e3dc-4f0b-9921-49a2cf0684dc");
            message.append("bytes for the character encoding used : ");
            writeline("/home/ubuntu/results/coverage/Copy/Copy_8_10.coverage", "9ab6e7aa-53ac-4903-9f83-082938e7ee4c");
            message.append((inputEncoding == null ? fileUtils.getDefaultEncoding() : inputEncoding));
            writeline("/home/ubuntu/results/coverage/Copy/Copy_8_10.coverage", "2be30b65-220e-4582-af18-bbaeac1b834a");
            message.append(LINE_SEPARATOR);
        }
        writeline("/home/ubuntu/results/coverage/Copy/Copy_8_10.coverage", "d1144ff5-9461-4096-97ac-4f427a41b4f7");
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
