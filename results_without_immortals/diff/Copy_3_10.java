Copy
~~~
getFileUtils
~~~
setFile
~
this.file = file;
~~~
setTofile
~
this.destFile = destFile;
~~~
setTodir
~~~
createFilterChain
~
filterChains.addElement(filterChain);
~~~
createFilterSet
~
filterSets.addElement(filterSet);
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
~
this.failonerror = failonerror;
~~~
addFileset
~
add(set);
~~~
add
~
rcs.add(res);
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
~
rcs.add(res);
~~~
setEncoding
~
outputEncoding = encoding;
~
this.inputEncoding = encoding;
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
log(message, Project.MSG_ERR);
~
log("Warning: " + getMessage(e), Project.MSG_ERR);
~
if (!quiet) {
    log(message, Project.MSG_ERR);
}
~
throw new BuildException(message);
~
name = fr.getFile().getAbsolutePath();
~
throw e;
~
if (!quiet) {
    log("Warning: " + getMessage(e), Project.MSG_ERR);
}
~
continue;
~
final String message = "Warning: Could not find resource " + r.toLongString() + " to copy.";
~
if (!failonerror) {
    if (!quiet) {
        log(message, Project.MSG_ERR);
    }
} else {
    throw new BuildException(message);
}
~
continue;
~
final FileResource fr = ResourceUtils.asFileResource(fp);
~
baseDir = getKeyFile(fr.getBaseDir());
~
if (fr.getBaseDir() == null) {
    name = fr.getFile().getAbsolutePath();
}
~
add(baseDir, name, r.isDirectory() ? dirsByBasedir : filesByBasedir);
~
baseDirs.add(baseDir);
~
// a not-directory file resource
// needs special treatment
nonFileResources.add(r);
~
log("Warning: " + getMessage(e), Project.MSG_ERR);
~
ds = fs.getDirectoryScanner(getProject());
~
if (failonerror || !getMessage(e).endsWith(DirectoryScanner.DOES_NOT_EXIST_POSTFIX)) {
    throw e;
} else {
    if (!quiet) {
        log("Warning: " + getMessage(e), Project.MSG_ERR);
    }
    continue;
}
~
completeDirMap.put(fromDir, destDir);
~
throw new BuildException("Only FileSystem resources are supported.");
~
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
~
File baseDir = NULL_FILE_PLACEHOLDER;
~
String name = r.getName();
~
final FileProvider fp = r.as(FileProvider.class);
~
if (fp != null) {
    final FileResource fr = ResourceUtils.asFileResource(fp);
    baseDir = getKeyFile(fr.getBaseDir());
    if (fr.getBaseDir() == null) {
        name = fr.getFile().getAbsolutePath();
    }
}
~
// files.
if (r.isDirectory() || fp != null) {
    add(baseDir, name, r.isDirectory() ? dirsByBasedir : filesByBasedir);
    baseDirs.add(baseDir);
} else {
    // a not-directory file resource
    // needs special treatment
    nonFileResources.add(r);
}
~
log("Warning: " + getMessage(e), Project.MSG_ERR);
~
if (!quiet) {
    log("Warning: " + getMessage(e), Project.MSG_ERR);
}
~
throw e;
~
throw e;
~
log("Warning: " + getMessage(e), Project.MSG_ERR);
~
return;
~
final FileSet fs = (FileSet) rc;
~
DirectoryScanner ds = null;
~
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
~
final File fromDir = fs.getDir(getProject());
~
final String[] srcFiles = ds.getIncludedFiles();
~
final String[] srcDirs = ds.getIncludedDirectories();
~
if (!flatten && mapperElement == null && ds.isEverythingIncluded() && !fs.hasPatterns()) {
    completeDirMap.put(fromDir, destDir);
}
~
add(fromDir, srcFiles, filesByBasedir);
~
add(fromDir, srcDirs, dirsByBasedir);
~
baseDirs.add(fromDir);
~
if (!rc.isFilesystemOnly() && !supportsNonFileResources()) {
    throw new BuildException("Only FileSystem resources are supported.");
}
~
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
        // a not-directory file resource
        // needs special treatment
        nonFileResources.add(r);
    }
}
~
if (!quiet) {
    log("Warning: " + getMessage(e), Project.MSG_ERR);
}
~
throw e;
~
map.put(singleResource, new String[] { destFile.getAbsolutePath() });
~
doResourceOperations(map);
~
if (!failonerror) {
    if (!quiet) {
        log("Warning: " + getMessage(e), Project.MSG_ERR);
    }
} else {
    throw e;
}
~
validateAttributes();
~
if (failonerror || !getMessage(e).equals(MSG_WHEN_COPYING_EMPTY_RC_TO_FILE)) {
    throw e;
} else {
    log("Warning: " + getMessage(e), Project.MSG_ERR);
    return;
}
~
final ResourceCollection rc = rcs.elementAt(i);
~
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
            // a not-directory file resource
            // needs special treatment
            nonFileResources.add(r);
        }
    }
}
~
doFileOperations();
~
if (!failonerror) {
    if (!quiet) {
        log("Warning: " + getMessage(e), Project.MSG_ERR);
    }
} else {
    throw e;
}
~
final Resource[] nonFiles = nonFileResources.toArray(new Resource[nonFileResources.size()]);
~
// restrict to out-of-date resources
final Map<Resource, String[]> map = scan(nonFiles, destDir);
~
if (singleResource != null) {
    map.put(singleResource, new String[] { destFile.getAbsolutePath() });
}
~
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
~
rcs.insertElementAt(savedRc, 0);
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
                // a not-directory file resource
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
// clean up again, so this instance can be used a second
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
                    // a not-directory file resource
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
    // clean up again, so this instance can be used a second
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
log(message, Project.MSG_ERR);
~
destFile = new File(destDir, file.getName());
~
fileCopyMap.put(file.getAbsolutePath(), new String[] { destFile.getAbsolutePath() });
~
log(file + " omitted as " + destFile + " is up to date.", Project.MSG_VERBOSE);
~
if (!quiet) {
    log(message, Project.MSG_ERR);
}
~
throw new BuildException(message);
~
if (destFile == null) {
    destFile = new File(destDir, file.getName());
}
~
if (forceOverwrite || !destFile.exists() || (file.lastModified() - granularity > destFile.lastModified())) {
    fileCopyMap.put(file.getAbsolutePath(), new String[] { destFile.getAbsolutePath() });
} else {
    log(file + " omitted as " + destFile + " is up to date.", Project.MSG_VERBOSE);
}
~
final String message = "Warning: Could not find file " + file.getAbsolutePath() + " to copy.";
~
if (!failonerror) {
    if (!quiet) {
        log(message, Project.MSG_ERR);
    }
} else {
    throw new BuildException(message);
}
~
if (file.exists()) {
    if (destFile == null) {
        destFile = new File(destDir, file.getName());
    }
    if (forceOverwrite || !destFile.exists() || (file.lastModified() - granularity > destFile.lastModified())) {
        fileCopyMap.put(file.getAbsolutePath(), new String[] { destFile.getAbsolutePath() });
    } else {
        log(file + " omitted as " + destFile + " is up to date.", Project.MSG_VERBOSE);
    }
} else {
    final String message = "Warning: Could not find file " + file.getAbsolutePath() + " to copy.";
    if (!failonerror) {
        if (!quiet) {
            log(message, Project.MSG_ERR);
        }
    } else {
        throw new BuildException(message);
    }
}
~
// deal with the single file
if (file != null) {
    if (file.exists()) {
        if (destFile == null) {
            destFile = new File(destDir, file.getName());
        }
        if (forceOverwrite || !destFile.exists() || (file.lastModified() - granularity > destFile.lastModified())) {
            fileCopyMap.put(file.getAbsolutePath(), new String[] { destFile.getAbsolutePath() });
        } else {
            log(file + " omitted as " + destFile + " is up to date.", Project.MSG_VERBOSE);
        }
    } else {
        final String message = "Warning: Could not find file " + file.getAbsolutePath() + " to copy.";
        if (!failonerror) {
            if (!quiet) {
                log(message, Project.MSG_ERR);
            }
        } else {
            throw new BuildException(message);
        }
    }
}
~~~
iterateOverBaseDirs
~~~
validateAttributes
~
file = r.getFile();
~
singleResource = res;
~
if (r != null) {
    file = r.getFile();
} else {
    singleResource = res;
}
~
rcs.removeElementAt(0);
~
throw new BuildException("Cannot concatenate multiple files into a single file.");
~
final Resource res = rc.iterator().next();
~
final FileProvider r = res.as(FileProvider.class);
~
if (file == null) {
    if (r != null) {
        file = r.getFile();
    } else {
        singleResource = res;
    }
    rcs.removeElementAt(0);
} else {
    throw new BuildException("Cannot concatenate multiple files into a single file.");
}
~
throw new BuildException("Cannot concatenate multiple files into a single file.");
~
throw new BuildException("Only FileSystem resources are" + " supported.");
~
throw new BuildException(MSG_WHEN_COPYING_EMPTY_RC_TO_FILE);
~
if (rc.size() == 1) {
    final Resource res = rc.iterator().next();
    final FileProvider r = res.as(FileProvider.class);
    if (file == null) {
        if (r != null) {
            file = r.getFile();
        } else {
            singleResource = res;
        }
        rcs.removeElementAt(0);
    } else {
        throw new BuildException("Cannot concatenate multiple files into a single file.");
    }
} else {
    throw new BuildException("Cannot concatenate multiple files into a single file.");
}
~
throw new BuildException("Cannot concatenate multiple files into a single file.");
~
final ResourceCollection rc = rcs.elementAt(0);
~
if (!rc.isFilesystemOnly() && !supportsNonFileResources()) {
    throw new BuildException("Only FileSystem resources are" + " supported.");
}
~
if (rc.size() == 0) {
    throw new BuildException(MSG_WHEN_COPYING_EMPTY_RC_TO_FILE);
} else if (rc.size() == 1) {
    final Resource res = rc.iterator().next();
    final FileProvider r = res.as(FileProvider.class);
    if (file == null) {
        if (r != null) {
            file = r.getFile();
        } else {
            singleResource = res;
        }
        rcs.removeElementAt(0);
    } else {
        throw new BuildException("Cannot concatenate multiple files into a single file.");
    }
} else {
    throw new BuildException("Cannot concatenate multiple files into a single file.");
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
if (rcs.size() > 1) {
    throw new BuildException("Cannot concatenate multiple files into a single file.");
} else {
    final ResourceCollection rc = rcs.elementAt(0);
    if (!rc.isFilesystemOnly() && !supportsNonFileResources()) {
        throw new BuildException("Only FileSystem resources are" + " supported.");
    }
    if (rc.size() == 0) {
        throw new BuildException(MSG_WHEN_COPYING_EMPTY_RC_TO_FILE);
    } else if (rc.size() == 1) {
        final Resource res = rc.iterator().next();
        final FileProvider r = res.as(FileProvider.class);
        if (file == null) {
            if (r != null) {
                file = r.getFile();
            } else {
                singleResource = res;
            }
            rcs.removeElementAt(0);
        } else {
            throw new BuildException("Cannot concatenate multiple files into a single file.");
        }
    } else {
        throw new BuildException("Cannot concatenate multiple files into a single file.");
    }
}
~
destDir = destFile.getParentFile();
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
~
if (destFile != null && rcs.size() > 0) {
    if (rcs.size() > 1) {
        throw new BuildException("Cannot concatenate multiple files into a single file.");
    } else {
        final ResourceCollection rc = rcs.elementAt(0);
        if (!rc.isFilesystemOnly() && !supportsNonFileResources()) {
            throw new BuildException("Only FileSystem resources are" + " supported.");
        }
        if (rc.size() == 0) {
            throw new BuildException(MSG_WHEN_COPYING_EMPTY_RC_TO_FILE);
        } else if (rc.size() == 1) {
            final Resource res = rc.iterator().next();
            final FileProvider r = res.as(FileProvider.class);
            if (file == null) {
                if (r != null) {
                    file = r.getFile();
                } else {
                    singleResource = res;
                }
                rcs.removeElementAt(0);
            } else {
                throw new BuildException("Cannot concatenate multiple files into a single file.");
            }
        } else {
            throw new BuildException("Cannot concatenate multiple files into a single file.");
        }
    }
}
~
if (destFile != null) {
    destDir = destFile.getParentFile();
}
~~~
scan
~
buildMap(fromDir, toDir, dirs, mapper, dirCopyMap);
~
final FileNameMapper mapper = getMapper();
~
buildMap(fromDir, toDir, files, mapper, fileCopyMap);
~
if (includeEmpty) {
    buildMap(fromDir, toDir, dirs, mapper, dirCopyMap);
}
~~~
scan
~
buildMap(fromDir, toDir, dirs, mapper, dirCopyMap);
~
final FileNameMapper mapper = getMapper();
~
buildMap(fromDir, toDir, files, mapper, fileCopyMap);
~
if (includeEmpty) {
    buildMap(fromDir, toDir, dirs, mapper, dirCopyMap);
}
~~~
buildMap
~
v.addElement(names[i]);
~
mappedFiles[k] = new File(toDir, mappedFiles[k]).getAbsolutePath();
~
if (mapper.mapFileName(names[i]) != null) {
    v.addElement(names[i]);
}
~
map.put(src.getAbsolutePath(), new String[] { new File(toDir, mappedFiles[0]).getAbsolutePath() });
~
// reuse the array created by the mapper
for (int k = 0; k < mappedFiles.length; k++) {
    mappedFiles[k] = new File(toDir, mappedFiles[k]).getAbsolutePath();
}
~
map.put(src.getAbsolutePath(), mappedFiles);
~
final Vector<String> v = new Vector<String>();
~
for (int i = 0; i < names.length; i++) {
    if (mapper.mapFileName(names[i]) != null) {
        v.addElement(names[i]);
    }
}
~
toCopy = new String[v.size()];
~
v.copyInto(toCopy);
~
final SourceFileScanner ds = new SourceFileScanner(this);
~
toCopy = ds.restrict(names, fromDir, toDir, mapper, granularity);
~
final File src = new File(fromDir, toCopy[i]);
~
final String[] mappedFiles = mapper.mapFileName(toCopy[i]);
~
if (!enableMultipleMappings) {
    map.put(src.getAbsolutePath(), new String[] { new File(toDir, mappedFiles[0]).getAbsolutePath() });
} else {
    // reuse the array created by the mapper
    for (int k = 0; k < mappedFiles.length; k++) {
        mappedFiles[k] = new File(toDir, mappedFiles[k]).getAbsolutePath();
    }
    map.put(src.getAbsolutePath(), mappedFiles);
}
~
String[] toCopy = null;
~
if (forceOverwrite) {
    final Vector<String> v = new Vector<String>();
    for (int i = 0; i < names.length; i++) {
        if (mapper.mapFileName(names[i]) != null) {
            v.addElement(names[i]);
        }
    }
    toCopy = new String[v.size()];
    v.copyInto(toCopy);
} else {
    final SourceFileScanner ds = new SourceFileScanner(this);
    toCopy = ds.restrict(names, fromDir, toDir, mapper, granularity);
}
~
for (int i = 0; i < toCopy.length; i++) {
    final File src = new File(fromDir, toCopy[i]);
    final String[] mappedFiles = mapper.mapFileName(toCopy[i]);
    if (!enableMultipleMappings) {
        map.put(src.getAbsolutePath(), new String[] { new File(toDir, mappedFiles[0]).getAbsolutePath() });
    } else {
        // reuse the array created by the mapper
        for (int k = 0; k < mappedFiles.length; k++) {
            mappedFiles[k] = new File(toDir, mappedFiles[k]).getAbsolutePath();
        }
        map.put(src.getAbsolutePath(), mappedFiles);
    }
}
~~~
buildMap
~
v.addElement(names[i]);
~
mappedFiles[k] = new File(toDir, mappedFiles[k]).getAbsolutePath();
~
if (mapper.mapFileName(names[i]) != null) {
    v.addElement(names[i]);
}
~
map.put(src.getAbsolutePath(), new String[] { new File(toDir, mappedFiles[0]).getAbsolutePath() });
~
// reuse the array created by the mapper
for (int k = 0; k < mappedFiles.length; k++) {
    mappedFiles[k] = new File(toDir, mappedFiles[k]).getAbsolutePath();
}
~
map.put(src.getAbsolutePath(), mappedFiles);
~
final Vector<String> v = new Vector<String>();
~
for (int i = 0; i < names.length; i++) {
    if (mapper.mapFileName(names[i]) != null) {
        v.addElement(names[i]);
    }
}
~
toCopy = new String[v.size()];
~
v.copyInto(toCopy);
~
final SourceFileScanner ds = new SourceFileScanner(this);
~
toCopy = ds.restrict(names, fromDir, toDir, mapper, granularity);
~
final File src = new File(fromDir, toCopy[i]);
~
final String[] mappedFiles = mapper.mapFileName(toCopy[i]);
~
if (!enableMultipleMappings) {
    map.put(src.getAbsolutePath(), new String[] { new File(toDir, mappedFiles[0]).getAbsolutePath() });
} else {
    // reuse the array created by the mapper
    for (int k = 0; k < mappedFiles.length; k++) {
        mappedFiles[k] = new File(toDir, mappedFiles[k]).getAbsolutePath();
    }
    map.put(src.getAbsolutePath(), mappedFiles);
}
~
String[] toCopy = null;
~
if (forceOverwrite) {
    final Vector<String> v = new Vector<String>();
    for (int i = 0; i < names.length; i++) {
        if (mapper.mapFileName(names[i]) != null) {
            v.addElement(names[i]);
        }
    }
    toCopy = new String[v.size()];
    v.copyInto(toCopy);
} else {
    final SourceFileScanner ds = new SourceFileScanner(this);
    toCopy = ds.restrict(names, fromDir, toDir, mapper, granularity);
}
~
for (int i = 0; i < toCopy.length; i++) {
    final File src = new File(fromDir, toCopy[i]);
    final String[] mappedFiles = mapper.mapFileName(toCopy[i]);
    if (!enableMultipleMappings) {
        map.put(src.getAbsolutePath(), new String[] { new File(toDir, mappedFiles[0]).getAbsolutePath() });
    } else {
        // reuse the array created by the mapper
        for (int k = 0; k < mappedFiles.length; k++) {
            mappedFiles[k] = new File(toDir, mappedFiles[k]).getAbsolutePath();
        }
        map.put(src.getAbsolutePath(), mappedFiles);
    }
}
~~~
doFileOperations
~
executionFilters.addFilterSet(getProject().getGlobalFilterSet());
~
executionFilters.addFilterSet(filterSet);
~
msg += " and I couldn't delete the corrupt " + toFile;
~
throw new BuildException(msg, ioe, getLocation());
~
log("Unable to create directory " + d.getAbsolutePath(), Project.MSG_ERR);
~
createCount++;
~
log("Skipping self-copy of " + fromFile, verbosity);
~
continue;
~
log("Copying " + fromFile + " to " + toFile, verbosity);
~
final FilterSetCollection executionFilters = new FilterSetCollection();
~
if (filtering) {
    executionFilters.addFilterSet(getProject().getGlobalFilterSet());
}
~
for (final FilterSet filterSet : filterSets) {
    executionFilters.addFilterSet(filterSet);
}
~
fileUtils.copyFile(new File(fromFile), new File(toFile), executionFilters, filterChains, forceOverwrite, preserveLastModified, /* append: */
false, inputEncoding, outputEncoding, getProject(), getForce());
~
String msg = "Failed to copy " + fromFile + " to " + toFile + " due to " + getDueTo(ioe);
~
final File targetFile = new File(toFile);
~
if (!(ioe instanceof ResourceUtils.ReadOnlyTargetFileException) && targetFile.exists() && !targetFile.delete()) {
    msg += " and I couldn't delete the corrupt " + toFile;
}
~
if (failonerror) {
    throw new BuildException(msg, ioe, getLocation());
}
~
log(msg, Project.MSG_ERR);
~
if (!(d.mkdirs() || d.isDirectory())) {
    log("Unable to create directory " + d.getAbsolutePath(), Project.MSG_ERR);
} else {
    createCount++;
}
~
final String toFile = toFiles[i];
~
if (fromFile.equals(toFile)) {
    log("Skipping self-copy of " + fromFile, verbosity);
    continue;
}
~
try {
    log("Copying " + fromFile + " to " + toFile, verbosity);
    final FilterSetCollection executionFilters = new FilterSetCollection();
    if (filtering) {
        executionFilters.addFilterSet(getProject().getGlobalFilterSet());
    }
    for (final FilterSet filterSet : filterSets) {
        executionFilters.addFilterSet(filterSet);
    }
    fileUtils.copyFile(new File(fromFile), new File(toFile), executionFilters, filterChains, forceOverwrite, preserveLastModified, /* append: */
    false, inputEncoding, outputEncoding, getProject(), getForce());
} catch (final IOException ioe) {
    String msg = "Failed to copy " + fromFile + " to " + toFile + " due to " + getDueTo(ioe);
    final File targetFile = new File(toFile);
    if (!(ioe instanceof ResourceUtils.ReadOnlyTargetFileException) && targetFile.exists() && !targetFile.delete()) {
        msg += " and I couldn't delete the corrupt " + toFile;
    }
    if (failonerror) {
        throw new BuildException(msg, ioe, getLocation());
    }
    log(msg, Project.MSG_ERR);
}
~
final File d = new File(dirs[i]);
~
if (!d.exists()) {
    if (!(d.mkdirs() || d.isDirectory())) {
        log("Unable to create directory " + d.getAbsolutePath(), Project.MSG_ERR);
    } else {
        createCount++;
    }
}
~
final String fromFile = e.getKey();
~
final String[] toFiles = e.getValue();
~
for (int i = 0; i < toFiles.length; i++) {
    final String toFile = toFiles[i];
    if (fromFile.equals(toFile)) {
        log("Skipping self-copy of " + fromFile, verbosity);
        continue;
    }
    try {
        log("Copying " + fromFile + " to " + toFile, verbosity);
        final FilterSetCollection executionFilters = new FilterSetCollection();
        if (filtering) {
            executionFilters.addFilterSet(getProject().getGlobalFilterSet());
        }
        for (final FilterSet filterSet : filterSets) {
            executionFilters.addFilterSet(filterSet);
        }
        fileUtils.copyFile(new File(fromFile), new File(toFile), executionFilters, filterChains, forceOverwrite, preserveLastModified, /* append: */
        false, inputEncoding, outputEncoding, getProject(), getForce());
    } catch (final IOException ioe) {
        String msg = "Failed to copy " + fromFile + " to " + toFile + " due to " + getDueTo(ioe);
        final File targetFile = new File(toFile);
        if (!(ioe instanceof ResourceUtils.ReadOnlyTargetFileException) && targetFile.exists() && !targetFile.delete()) {
            msg += " and I couldn't delete the corrupt " + toFile;
        }
        if (failonerror) {
            throw new BuildException(msg, ioe, getLocation());
        }
        log(msg, Project.MSG_ERR);
    }
}
~
for (int i = 0; i < dirs.length; i++) {
    final File d = new File(dirs[i]);
    if (!d.exists()) {
        if (!(d.mkdirs() || d.isDirectory())) {
            log("Unable to create directory " + d.getAbsolutePath(), Project.MSG_ERR);
        } else {
            createCount++;
        }
    }
}
~
log("Copied " + dirCopyMap.size() + " empty director" + (dirCopyMap.size() == 1 ? "y" : "ies") + " to " + createCount + " empty director" + (createCount == 1 ? "y" : "ies") + " under " + destDir.getAbsolutePath());
~
log("Copying " + fileCopyMap.size() + " file" + (fileCopyMap.size() == 1 ? "" : "s") + " to " + destDir.getAbsolutePath());
~
for (final Map.Entry<String, String[]> e : fileCopyMap.entrySet()) {
    final String fromFile = e.getKey();
    final String[] toFiles = e.getValue();
    for (int i = 0; i < toFiles.length; i++) {
        final String toFile = toFiles[i];
        if (fromFile.equals(toFile)) {
            log("Skipping self-copy of " + fromFile, verbosity);
            continue;
        }
        try {
            log("Copying " + fromFile + " to " + toFile, verbosity);
            final FilterSetCollection executionFilters = new FilterSetCollection();
            if (filtering) {
                executionFilters.addFilterSet(getProject().getGlobalFilterSet());
            }
            for (final FilterSet filterSet : filterSets) {
                executionFilters.addFilterSet(filterSet);
            }
            fileUtils.copyFile(new File(fromFile), new File(toFile), executionFilters, filterChains, forceOverwrite, preserveLastModified, /* append: */
            false, inputEncoding, outputEncoding, getProject(), getForce());
        } catch (final IOException ioe) {
            String msg = "Failed to copy " + fromFile + " to " + toFile + " due to " + getDueTo(ioe);
            final File targetFile = new File(toFile);
            if (!(ioe instanceof ResourceUtils.ReadOnlyTargetFileException) && targetFile.exists() && !targetFile.delete()) {
                msg += " and I couldn't delete the corrupt " + toFile;
            }
            if (failonerror) {
                throw new BuildException(msg, ioe, getLocation());
            }
            log(msg, Project.MSG_ERR);
        }
    }
}
~
int createCount = 0;
~
for (final String[] dirs : dirCopyMap.values()) {
    for (int i = 0; i < dirs.length; i++) {
        final File d = new File(dirs[i]);
        if (!d.exists()) {
            if (!(d.mkdirs() || d.isDirectory())) {
                log("Unable to create directory " + d.getAbsolutePath(), Project.MSG_ERR);
            } else {
                createCount++;
            }
        }
    }
}
~
if (createCount > 0) {
    log("Copied " + dirCopyMap.size() + " empty director" + (dirCopyMap.size() == 1 ? "y" : "ies") + " to " + createCount + " empty director" + (createCount == 1 ? "y" : "ies") + " under " + destDir.getAbsolutePath());
}
~
if (fileCopyMap.size() > 0) {
    log("Copying " + fileCopyMap.size() + " file" + (fileCopyMap.size() == 1 ? "" : "s") + " to " + destDir.getAbsolutePath());
    for (final Map.Entry<String, String[]> e : fileCopyMap.entrySet()) {
        final String fromFile = e.getKey();
        final String[] toFiles = e.getValue();
        for (int i = 0; i < toFiles.length; i++) {
            final String toFile = toFiles[i];
            if (fromFile.equals(toFile)) {
                log("Skipping self-copy of " + fromFile, verbosity);
                continue;
            }
            try {
                log("Copying " + fromFile + " to " + toFile, verbosity);
                final FilterSetCollection executionFilters = new FilterSetCollection();
                if (filtering) {
                    executionFilters.addFilterSet(getProject().getGlobalFilterSet());
                }
                for (final FilterSet filterSet : filterSets) {
                    executionFilters.addFilterSet(filterSet);
                }
                fileUtils.copyFile(new File(fromFile), new File(toFile), executionFilters, filterChains, forceOverwrite, preserveLastModified, /* append: */
                false, inputEncoding, outputEncoding, getProject(), getForce());
            } catch (final IOException ioe) {
                String msg = "Failed to copy " + fromFile + " to " + toFile + " due to " + getDueTo(ioe);
                final File targetFile = new File(toFile);
                if (!(ioe instanceof ResourceUtils.ReadOnlyTargetFileException) && targetFile.exists() && !targetFile.delete()) {
                    msg += " and I couldn't delete the corrupt " + toFile;
                }
                if (failonerror) {
                    throw new BuildException(msg, ioe, getLocation());
                }
                log(msg, Project.MSG_ERR);
            }
        }
    }
}
~
if (includeEmpty) {
    int createCount = 0;
    for (final String[] dirs : dirCopyMap.values()) {
        for (int i = 0; i < dirs.length; i++) {
            final File d = new File(dirs[i]);
            if (!d.exists()) {
                if (!(d.mkdirs() || d.isDirectory())) {
                    log("Unable to create directory " + d.getAbsolutePath(), Project.MSG_ERR);
                } else {
                    createCount++;
                }
            }
        }
    }
    if (createCount > 0) {
        log("Copied " + dirCopyMap.size() + " empty director" + (dirCopyMap.size() == 1 ? "y" : "ies") + " to " + createCount + " empty director" + (createCount == 1 ? "y" : "ies") + " under " + destDir.getAbsolutePath());
    }
}
~~~
doResourceOperations
~
executionFilters.addFilterSet(getProject().getGlobalFilterSet());
~
executionFilters.addFilterSet(filterSet);
~
msg += " and I couldn't delete the corrupt " + toFile;
~
throw new BuildException(msg, ioe, getLocation());
~
log("Copying " + fromResource + " to " + toFile, verbosity);
~
final FilterSetCollection executionFilters = new FilterSetCollection();
~
if (filtering) {
    executionFilters.addFilterSet(getProject().getGlobalFilterSet());
}
~
for (final FilterSet filterSet : filterSets) {
    executionFilters.addFilterSet(filterSet);
}
~
ResourceUtils.copyResource(fromResource, new FileResource(destDir, toFile), executionFilters, filterChains, forceOverwrite, preserveLastModified, /* append: */
false, inputEncoding, outputEncoding, getProject(), getForce());
~
String msg = "Failed to copy " + fromResource + " to " + toFile + " due to " + getDueTo(ioe);
~
final File targetFile = new File(toFile);
~
if (!(ioe instanceof ResourceUtils.ReadOnlyTargetFileException) && targetFile.exists() && !targetFile.delete()) {
    msg += " and I couldn't delete the corrupt " + toFile;
}
~
if (failonerror) {
    throw new BuildException(msg, ioe, getLocation());
}
~
log(msg, Project.MSG_ERR);
~
try {
    log("Copying " + fromResource + " to " + toFile, verbosity);
    final FilterSetCollection executionFilters = new FilterSetCollection();
    if (filtering) {
        executionFilters.addFilterSet(getProject().getGlobalFilterSet());
    }
    for (final FilterSet filterSet : filterSets) {
        executionFilters.addFilterSet(filterSet);
    }
    ResourceUtils.copyResource(fromResource, new FileResource(destDir, toFile), executionFilters, filterChains, forceOverwrite, preserveLastModified, /* append: */
    false, inputEncoding, outputEncoding, getProject(), getForce());
} catch (final IOException ioe) {
    String msg = "Failed to copy " + fromResource + " to " + toFile + " due to " + getDueTo(ioe);
    final File targetFile = new File(toFile);
    if (!(ioe instanceof ResourceUtils.ReadOnlyTargetFileException) && targetFile.exists() && !targetFile.delete()) {
        msg += " and I couldn't delete the corrupt " + toFile;
    }
    if (failonerror) {
        throw new BuildException(msg, ioe, getLocation());
    }
    log(msg, Project.MSG_ERR);
}
~
final Resource fromResource = e.getKey();
~
for (final String toFile : e.getValue()) {
    try {
        log("Copying " + fromResource + " to " + toFile, verbosity);
        final FilterSetCollection executionFilters = new FilterSetCollection();
        if (filtering) {
            executionFilters.addFilterSet(getProject().getGlobalFilterSet());
        }
        for (final FilterSet filterSet : filterSets) {
            executionFilters.addFilterSet(filterSet);
        }
        ResourceUtils.copyResource(fromResource, new FileResource(destDir, toFile), executionFilters, filterChains, forceOverwrite, preserveLastModified, /* append: */
        false, inputEncoding, outputEncoding, getProject(), getForce());
    } catch (final IOException ioe) {
        String msg = "Failed to copy " + fromResource + " to " + toFile + " due to " + getDueTo(ioe);
        final File targetFile = new File(toFile);
        if (!(ioe instanceof ResourceUtils.ReadOnlyTargetFileException) && targetFile.exists() && !targetFile.delete()) {
            msg += " and I couldn't delete the corrupt " + toFile;
        }
        if (failonerror) {
            throw new BuildException(msg, ioe, getLocation());
        }
        log(msg, Project.MSG_ERR);
    }
}
~
log("Copying " + map.size() + " resource" + (map.size() == 1 ? "" : "s") + " to " + destDir.getAbsolutePath());
~
for (final Map.Entry<Resource, String[]> e : map.entrySet()) {
    final Resource fromResource = e.getKey();
    for (final String toFile : e.getValue()) {
        try {
            log("Copying " + fromResource + " to " + toFile, verbosity);
            final FilterSetCollection executionFilters = new FilterSetCollection();
            if (filtering) {
                executionFilters.addFilterSet(getProject().getGlobalFilterSet());
            }
            for (final FilterSet filterSet : filterSets) {
                executionFilters.addFilterSet(filterSet);
            }
            ResourceUtils.copyResource(fromResource, new FileResource(destDir, toFile), executionFilters, filterChains, forceOverwrite, preserveLastModified, /* append: */
            false, inputEncoding, outputEncoding, getProject(), getForce());
        } catch (final IOException ioe) {
            String msg = "Failed to copy " + fromResource + " to " + toFile + " due to " + getDueTo(ioe);
            final File targetFile = new File(toFile);
            if (!(ioe instanceof ResourceUtils.ReadOnlyTargetFileException) && targetFile.exists() && !targetFile.delete()) {
                msg += " and I couldn't delete the corrupt " + toFile;
            }
            if (failonerror) {
                throw new BuildException(msg, ioe, getLocation());
            }
            log(msg, Project.MSG_ERR);
        }
    }
}
~
if (map.size() > 0) {
    log("Copying " + map.size() + " resource" + (map.size() == 1 ? "" : "s") + " to " + destDir.getAbsolutePath());
    for (final Map.Entry<Resource, String[]> e : map.entrySet()) {
        final Resource fromResource = e.getKey();
        for (final String toFile : e.getValue()) {
            try {
                log("Copying " + fromResource + " to " + toFile, verbosity);
                final FilterSetCollection executionFilters = new FilterSetCollection();
                if (filtering) {
                    executionFilters.addFilterSet(getProject().getGlobalFilterSet());
                }
                for (final FilterSet filterSet : filterSets) {
                    executionFilters.addFilterSet(filterSet);
                }
                ResourceUtils.copyResource(fromResource, new FileResource(destDir, toFile), executionFilters, filterChains, forceOverwrite, preserveLastModified, /* append: */
                false, inputEncoding, outputEncoding, getProject(), getForce());
            } catch (final IOException ioe) {
                String msg = "Failed to copy " + fromResource + " to " + toFile + " due to " + getDueTo(ioe);
                final File targetFile = new File(toFile);
                if (!(ioe instanceof ResourceUtils.ReadOnlyTargetFileException) && targetFile.exists() && !targetFile.delete()) {
                    msg += " and I couldn't delete the corrupt " + toFile;
                }
                if (failonerror) {
                    throw new BuildException(msg, ioe, getLocation());
                }
                log(msg, Project.MSG_ERR);
            }
        }
    }
}
~~~
supportsNonFileResources
~~~
add
~
rcs.add(res);
~~~
add
~
rcs.add(res);
~~~
getKeyFile
~~~
getMapper
~
if (mapperElement != null) {
    mapper = mapperElement.getImplementation();
} else if (flatten) {
    mapper = new FlatFileNameMapper();
} else {
    mapper = new IdentityMapper();
}
~~~
getMessage
~~~
getDueTo
~
final boolean baseIOException = ex.getClass() == IOException.class;
~
if (!baseIOException || ex.getMessage() == null) {
    message.append(ex.getClass().getName());
}
~
if (ex.getMessage() != null) {
    if (!baseIOException) {
        message.append(" ");
    }
    message.append(ex.getMessage());
}
~
if (ex.getClass().getName().indexOf("MalformedInput") != -1) {
    message.append(LINE_SEPARATOR);
    message.append("This is normally due to the input file containing invalid");
    message.append(LINE_SEPARATOR);
    message.append("bytes for the character encoding used : ");
    message.append((inputEncoding == null ? fileUtils.getDefaultEncoding() : inputEncoding));
    message.append(LINE_SEPARATOR);
}
