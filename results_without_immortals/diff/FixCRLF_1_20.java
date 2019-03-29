FixCRLF
~~~
chain
~~~
setSrcdir
~~~
setDestdir
~~~
setJavafiles
~~~
setFile
~~~
setEol
~~~
setCr
~
c.setValue("asis");
~
// must be "add"
c.setValue("crlf");
~
log("DEPRECATED: The cr attribute has been deprecated,", Project.MSG_WARN);
~
log("Please use the eol attribute instead", Project.MSG_WARN);
~~~
setTab
~~~
setTablength
~
filter.setTablength(tlength);
~
// a BuildException
throw new BuildException(e.getMessage(), e);
~
try {
    filter.setTablength(tlength);
} catch (IOException e) {
    // a BuildException
    throw new BuildException(e.getMessage(), e);
}
~~~
setEof
~~~
setEncoding
~~~
setOutputEncoding
~
this.outputEncoding = outputEncoding;
~~~
setFixlast
~~~
setPreserveLastModified
~~~
execute
~
// first off, make sure that we've got a srcdir and destdir
validate();
~
// log options used
String enc = encoding == null ? "default" : encoding;
~
log("options:" + " eol=" + filter.getEol().getValue() + " tab=" + filter.getTab().getValue() + " eof=" + filter.getEof().getValue() + " tablength=" + filter.getTablength() + " encoding=" + enc + " outputencoding=" + (outputEncoding == null ? enc : outputEncoding), Project.MSG_VERBOSE);
~
DirectoryScanner ds = super.getDirectoryScanner(srcDir);
~
String[] files = ds.getIncludedFiles();
~
for (int i = 0; i < files.length; i++) {
    processFile(files[i]);
}
~~~
validate
~
throw new BuildException(FIXCRLF_ERROR + "destdir does not exist: '" + destDir + "'");
~
throw new BuildException(FIXCRLF_ERROR + "destdir is not a directory: '" + destDir + "'");
~
// patch file into the fileset
fileset.setFile(file);
~
// set our parent dir
srcDir = file.getParentFile();
~
throw new BuildException(FIXCRLF_ERROR + "srcdir attribute must be set!");
~
throw new BuildException(FIXCRLF_ERROR + "srcdir does not exist: '" + srcDir + "'");
~
throw new BuildException(FIXCRLF_ERROR + "srcdir is not a directory: '" + srcDir + "'");
~
if (!destDir.exists()) {
    throw new BuildException(FIXCRLF_ERROR + "destdir does not exist: '" + destDir + "'");
}
~
if (!destDir.isDirectory()) {
    throw new BuildException(FIXCRLF_ERROR + "destdir is not a directory: '" + destDir + "'");
}
~
if (srcDir == null) {
    throw new BuildException(FIXCRLF_ERROR + "srcdir attribute must be set!");
}
~
if (!srcDir.exists()) {
    throw new BuildException(FIXCRLF_ERROR + "srcdir does not exist: '" + srcDir + "'");
}
~
if (!srcDir.isDirectory()) {
    throw new BuildException(FIXCRLF_ERROR + "srcdir is not a directory: '" + srcDir + "'");
}
~
if (destDir != null) {
    if (!destDir.exists()) {
        throw new BuildException(FIXCRLF_ERROR + "destdir does not exist: '" + destDir + "'");
    }
    if (!destDir.isDirectory()) {
        throw new BuildException(FIXCRLF_ERROR + "destdir is not a directory: '" + destDir + "'");
    }
}
~~~
processFile
~
log("preserved lastModified for " + destFile, Project.MSG_DEBUG);
~
// Compare the destination with the temp file
log("destFile " + destFile + " exists", Project.MSG_DEBUG);
~
log(destFile + (destIsWrong ? " is being written" : " is not written, as the contents are identical"), Project.MSG_DEBUG);
~
FILE_UTILS.tryHardToDelete(tmpFile);
~
throw new BuildException("error running fixcrlf on file " + srcFile, e);
~
if (tmpFile != null && tmpFile.exists()) {
    FILE_UTILS.tryHardToDelete(tmpFile);
}
