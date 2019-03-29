Copy
~~~
getFileUtils
~~~
setFile
~~~
setTofile
~~~
setTodir
~~~
createFilterChain
~~~
createFilterSet
~~~
setPreserveLastModified
~
setPreserveLastModified(Project.toBoolean(preserve));
~~~
setPreserveLastModified
~
setPreserveLastModified(Project.toBoolean(preserve));
~~~
getPreserveLastModified
~~~
getFilterSets
~~~
getFilterChains
~~~
setFiltering
~
this.filtering = filtering;
~~~
setOverwrite
~
this.forceOverwrite = overwrite;
~~~
setForce
~
force = f;
~~~
getForce
~~~
setFlatten
~
this.flatten = flatten;
~~~
setVerbose
~
this.verbosity = verbose ? Project.MSG_INFO : Project.MSG_VERBOSE;
~~~
setIncludeEmptyDirs
~
this.includeEmpty = includeEmpty;
~~~
setQuiet
~
this.quiet = quiet;
~~~
setEnableMultipleMappings
~
this.enableMultipleMappings = enableMultipleMappings;
~~~
isEnableMultipleMapping
~~~
setFailOnError
~~~
addFileset
~~~
add
~~~
createMapper
~
if (mapperElement != null) {
    throw new BuildException("Cannot define more than one mapper", getLocation());
}
~
mapperElement = new Mapper(getProject());
~~~
add
~~~
setEncoding
~
if (outputEncoding == null) {
    outputEncoding = encoding;
}
~~~
getEncoding
~~~
setOutputEncoding
~
this.outputEncoding = encoding;
~~~
getOutputEncoding
~~~
setGranularity
~
this.granularity = granularity;
~~~
execute
~
// will be removed in validateAttributes
savedRc = rcs.elementAt(0);
~
// make sure we don't have an illegal set of options
try {
    validateAttributes();
} catch (final BuildException e) {
    if (failonerror || !getMessage(e).equals(MSG_WHEN_COPYING_EMPTY_RC_TO_FILE)) {
        throw e;
    } else {
        log("Warning: " + getMessage(e), Project.MSG_ERR);
        return;
    }
}
~
// deal with the single file
copySingleFile();
~
final HashMap<File, List<String>> filesByBasedir = new HashMap<File, List<String>>();
~
final HashMap<File, List<String>> dirsByBasedir = new HashMap<File, List<String>>();
~
final HashSet<File> baseDirs = new HashSet<File>();
~
final ArrayList<Resource> nonFileResources = new ArrayList<Resource>();
~
final int size = rcs.size();
~
for (int i = 0; i < size; i++) {
    final ResourceCollection rc = rcs.elementAt(i);
    // Step (1) - beware of the ZipFileSet
    if (rc instanceof FileSet && rc.isFilesystemOnly()) {
        final FileSet fs = (FileSet) rc;
        DirectoryScanner ds = null;
        try {
            ds = fs.getDirectoryScanner(getProject());
        } catch (final BuildException e) {
            if (failonerror || !getMessage(e).endsWith(DirectoryScanner.DOES_NOT_EXIST_POSTFIX)) {
                throw e;
            } else {
                if (!quiet) {
                    log("Warning: " + getMessage(e), Project.MSG_ERR);
                }
                continue;
            }
        }
        final File fromDir = fs.getDir(getProject());
        final String[] srcFiles = ds.getIncludedFiles();
        final String[] srcDirs = ds.getIncludedDirectories();
        if (!flatten && mapperElement == null && ds.isEverythingIncluded() && !fs.hasPatterns()) {
            completeDirMap.put(fromDir, destDir);
        }
        add(fromDir, srcFiles, filesByBasedir);
        add(fromDir, srcDirs, dirsByBasedir);
        baseDirs.add(fromDir);
    } else {
        if (!rc.isFilesystemOnly() && !supportsNonFileResources()) {
            throw new BuildException("Only FileSystem resources are supported.");
        }
        for (final Resource r : rc) {
            if (!r.isExists()) {
                final String message = "Warning: Could not find resource " + r.toLongString() + " to copy.";
                if (!failonerror) {
                    if (!quiet) {
                        log(message, Project.MSG_ERR);
                    }
                } else {
                    throw new BuildException(message);
                }
                continue;
            }
            File baseDir = NULL_FILE_PLACEHOLDER;
            String name = r.getName();
            final FileProvider fp = r.as(FileProvider.class);
            if (fp != null) {
                final FileResource fr = ResourceUtils.asFileResource(fp);
                baseDir = getKeyFile(fr.getBaseDir());
                if (fr.getBaseDir() == null) {
                    name = fr.getFile().getAbsolutePath();
                }
            }
            // files.
            if (r.isDirectory() || fp != null) {
                add(baseDir, name, r.isDirectory() ? dirsByBasedir : filesByBasedir);
                baseDirs.add(baseDir);
            } else {
                // needs special treatment
                nonFileResources.add(r);
            }
        }
    }
}
~
iterateOverBaseDirs(baseDirs, dirsByBasedir, filesByBasedir);
~
// do all the copy operations now...
try {
    doFileOperations();
} catch (final BuildException e) {
    if (!failonerror) {
        if (!quiet) {
            log("Warning: " + getMessage(e), Project.MSG_ERR);
        }
    } else {
        throw e;
    }
}
~
if (nonFileResources.size() > 0 || singleResource != null) {
    final Resource[] nonFiles = nonFileResources.toArray(new Resource[nonFileResources.size()]);
    // restrict to out-of-date resources
    final Map<Resource, String[]> map = scan(nonFiles, destDir);
    if (singleResource != null) {
        map.put(singleResource, new String[] { destFile.getAbsolutePath() });
    }
    try {
        doResourceOperations(map);
    } catch (final BuildException e) {
        if (!failonerror) {
            if (!quiet) {
                log("Warning: " + getMessage(e), Project.MSG_ERR);
            }
        } else {
            throw e;
        }
    }
}
~
// time
singleResource = null;
~
file = savedFile;
~
destFile = savedDestFile;
~
destDir = savedDestDir;
~
if (savedRc != null) {
    rcs.insertElementAt(savedRc, 0);
}
~
fileCopyMap.clear();
~
dirCopyMap.clear();
~
completeDirMap.clear();
~
// may be altered in validateAttributes
final File savedFile = file;
~
final File savedDestFile = destFile;
~
final File savedDestDir = destDir;
~
ResourceCollection savedRc = null;
~
if (file == null && destFile != null && rcs.size() == 1) {
    // will be removed in validateAttributes
    savedRc = rcs.elementAt(0);
}
~
try {
    // make sure we don't have an illegal set of options
    try {
        validateAttributes();
    } catch (final BuildException e) {
        if (failonerror || !getMessage(e).equals(MSG_WHEN_COPYING_EMPTY_RC_TO_FILE)) {
            throw e;
        } else {
            log("Warning: " + getMessage(e), Project.MSG_ERR);
            return;
        }
    }
    // deal with the single file
    copySingleFile();
    final HashMap<File, List<String>> filesByBasedir = new HashMap<File, List<String>>();
    final HashMap<File, List<String>> dirsByBasedir = new HashMap<File, List<String>>();
    final HashSet<File> baseDirs = new HashSet<File>();
    final ArrayList<Resource> nonFileResources = new ArrayList<Resource>();
    final int size = rcs.size();
    for (int i = 0; i < size; i++) {
        final ResourceCollection rc = rcs.elementAt(i);
        // Step (1) - beware of the ZipFileSet
        if (rc instanceof FileSet && rc.isFilesystemOnly()) {
            final FileSet fs = (FileSet) rc;
            DirectoryScanner ds = null;
            try {
                ds = fs.getDirectoryScanner(getProject());
            } catch (final BuildException e) {
                if (failonerror || !getMessage(e).endsWith(DirectoryScanner.DOES_NOT_EXIST_POSTFIX)) {
                    throw e;
                } else {
                    if (!quiet) {
                        log("Warning: " + getMessage(e), Project.MSG_ERR);
                    }
                    continue;
                }
            }
            final File fromDir = fs.getDir(getProject());
            final String[] srcFiles = ds.getIncludedFiles();
            final String[] srcDirs = ds.getIncludedDirectories();
            if (!flatten && mapperElement == null && ds.isEverythingIncluded() && !fs.hasPatterns()) {
                completeDirMap.put(fromDir, destDir);
            }
            add(fromDir, srcFiles, filesByBasedir);
            add(fromDir, srcDirs, dirsByBasedir);
            baseDirs.add(fromDir);
        } else {
            if (!rc.isFilesystemOnly() && !supportsNonFileResources()) {
                throw new BuildException("Only FileSystem resources are supported.");
            }
            for (final Resource r : rc) {
                if (!r.isExists()) {
                    final String message = "Warning: Could not find resource " + r.toLongString() + " to copy.";
                    if (!failonerror) {
                        if (!quiet) {
                            log(message, Project.MSG_ERR);
                        }
                    } else {
                        throw new BuildException(message);
                    }
                    continue;
                }
                File baseDir = NULL_FILE_PLACEHOLDER;
                String name = r.getName();
                final FileProvider fp = r.as(FileProvider.class);
                if (fp != null) {
                    final FileResource fr = ResourceUtils.asFileResource(fp);
                    baseDir = getKeyFile(fr.getBaseDir());
                    if (fr.getBaseDir() == null) {
                        name = fr.getFile().getAbsolutePath();
                    }
                }
                // files.
                if (r.isDirectory() || fp != null) {
                    add(baseDir, name, r.isDirectory() ? dirsByBasedir : filesByBasedir);
                    baseDirs.add(baseDir);
                } else {
                    // needs special treatment
                    nonFileResources.add(r);
                }
            }
        }
    }
    iterateOverBaseDirs(baseDirs, dirsByBasedir, filesByBasedir);
    // do all the copy operations now...
    try {
        doFileOperations();
    } catch (final BuildException e) {
        if (!failonerror) {
            if (!quiet) {
                log("Warning: " + getMessage(e), Project.MSG_ERR);
            }
        } else {
            throw e;
        }
    }
    if (nonFileResources.size() > 0 || singleResource != null) {
        final Resource[] nonFiles = nonFileResources.toArray(new Resource[nonFileResources.size()]);
        // restrict to out-of-date resources
        final Map<Resource, String[]> map = scan(nonFiles, destDir);
        if (singleResource != null) {
            map.put(singleResource, new String[] { destFile.getAbsolutePath() });
        }
        try {
            doResourceOperations(map);
        } catch (final BuildException e) {
            if (!failonerror) {
                if (!quiet) {
                    log("Warning: " + getMessage(e), Project.MSG_ERR);
                }
            } else {
                throw e;
            }
        }
    }
} finally {
    // time
    singleResource = null;
    file = savedFile;
    destFile = savedDestFile;
    destDir = savedDestDir;
    if (savedRc != null) {
        rcs.insertElementAt(savedRc, 0);
    }
    fileCopyMap.clear();
    dirCopyMap.clear();
    completeDirMap.clear();
}
~~~
copySingleFile
~
log(file + " omitted as " + destFile + " is up to date.", Project.MSG_VERBOSE);
~
throw new BuildException(message);
~~~
iterateOverBaseDirs
~
srcDirs = dirs.toArray(srcDirs);
~
final List<String> dirs = dirsByBasedir.get(f);
~
if (dirs != null) {
    srcDirs = dirs.toArray(srcDirs);
}
~~~
validateAttributes
~
singleResource = res;
~
throw new BuildException("Cannot concatenate multiple files into a single file.");
~
throw new BuildException("Cannot concatenate multiple files into a single file.");
~
throw new BuildException("Only FileSystem resources are" + " supported.");
~
throw new BuildException(MSG_WHEN_COPYING_EMPTY_RC_TO_FILE);
~
throw new BuildException("Cannot concatenate multiple files into a single file.");
~
if (!rc.isFilesystemOnly() && !supportsNonFileResources()) {
    throw new BuildException("Only FileSystem resources are" + " supported.");
}
~
throw new BuildException("Specify at least one source--a file or a resource collection.");
~
throw new BuildException("Only one of tofile and todir may be set.");
~
throw new BuildException("One of tofile or todir must be set.");
~
throw new BuildException("Use a resource collection to copy directories.");
~
if (file == null && rcs.size() == 0) {
    throw new BuildException("Specify at least one source--a file or a resource collection.");
}
~
if (destFile != null && destDir != null) {
    throw new BuildException("Only one of tofile and todir may be set.");
}
~
if (destFile == null && destDir == null) {
    throw new BuildException("One of tofile or todir must be set.");
}
~
if (file != null && file.isDirectory()) {
    throw new BuildException("Use a resource collection to copy directories.");
}
~~~
scan
~
if (includeEmpty) {
    buildMap(fromDir, toDir, dirs, mapper, dirCopyMap);
}
~~~
scan
~
if (includeEmpty) {
    buildMap(fromDir, toDir, dirs, mapper, dirCopyMap);
}
~~~
buildMap
~
v.addElement(names[i]);
~~~
buildMap
~
v.addElement(names[i]);
~~~
doFileOperations
~~~
doResourceOperations
~~~
supportsNonFileResources
~~~
add
~~~
add
~~~
getKeyFile
~~~
getMapper
~~~
getMessage
~~~
getDueTo
