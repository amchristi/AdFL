AntBuilder
~~~
validate
~
LOG.warn("usedebug and usequiet are ignored if uselogger is not set to 'true'!");
~
ValidationHelper.assertTrue(saveLogDir.isDirectory(), "'saveLogDir' must exist and be a directory");
~
final File antHomeFile = new File(antHome);
~
ValidationHelper.assertTrue(antHomeFile.exists() && antHomeFile.isDirectory(), "'antHome' must exist and be a directory. Expected to find " + antHomeFile.getAbsolutePath());
~
antScript = antScriptInAntHome.getAbsolutePath();
~
LOG.warn("jvmargs will be ignored if you specify anthome or your own antscript!");
~
ValidationHelper.assertIsSet(buildFile, "buildfile", this.getClass());
~
ValidationHelper.assertIsSet(target, "target", this.getClass());
~
if (!useLogger && (useDebug || useQuiet)) {
    LOG.warn("usedebug and usequiet are ignored if uselogger is not set to 'true'!");
}
~
if (saveLogDir != null) {
    ValidationHelper.assertTrue(saveLogDir.isDirectory(), "'saveLogDir' must exist and be a directory");
}
~
if (antScript != null && !args.isEmpty()) {
    LOG.warn("jvmargs will be ignored if you specify anthome or your own antscript!");
}
~~~
build
~
throw new IllegalStateException("This builder was never validated." + " The build method should not be getting called.");
~
// TODO: I think there's a bug here when workingDir == null
buildOutputConsumer = getBuildOutputConsumer(buildProperties.get(Builder.BUILD_PROP_PROJECTNAME), workingDir, AntOutputLogger.DEFAULT_OUTFILE_NAME);
~
buildOutputConsumer = null;
~
LOG.warn("Build timeout timer of " + timeout + " seconds has expired");
~
buildLogElement = new Element("build");
~
buildLogElement.setAttribute("error", "build timeout");
~
// somebody should really fix ant's XmlLogger
if (logFile.exists()) {
    try {
        buildLogElement.setText(Util.readFileToString(logFile));
    } catch (IOException likely) {
    }
}
~
// read in log file as element, return it
buildLogElement = getAntLogAsElement(logFile);
~
saveAntLog(logFile);
~
logFile.delete();
~
if (!wasValidated) {
    throw new IllegalStateException("This builder was never validated." + " The build method should not be getting called.");
}
~
validateBuildFileExists();
~
final Progress progress = getShowProgress() ? progressIn : null;
~
final OSEnvironment antEnv = new OSEnvironment();
~
// Merge the environment with the configuration
mergeEnv(antEnv);
~
script.setUseLogger(useLogger);
~
script.setUseScript(antScript != null);
~
script.setWindows(Util.isWindows());
~
script.setAntScript(antScript);
~
script.setIsLoggerClassNameSet(isLoggerClassNameSet);
~
script.setShowAntOutput(isLiveOutput());
~
script.setTempFileName(tempFileName);
~
script.setUseDebug(useDebug);
~
script.setUseQuiet(useQuiet);
~
script.setKeepGoing(keepGoing);
~
script.setPropertyFile(propertyfile);
~
script.setProgressLoggerLib(progressLoggerLib);
~
script.setProgress(progress);
~
script.setAntEnv(antEnv);
~~~
runScript
~~~
buildWithTarget
~
target = buildTarget;
~
target = origTarget;
~
final String origTarget = target;
~
try {
    target = buildTarget;
    return build(properties, progress);
} finally {
    target = origTarget;
}
~~~
validateBuildFileExists
~~~
setSaveLogDir
~~~
saveAntLog
~~~
setAntWorkingDir
~~~
setAntScript
~~~
setAntHome
~~~
findAntScript
~~~
setTempFile
~
this.tempFileName = tempFileName;
~~~
setTarget
~~~
setBuildFile
~~~
setUseLogger
~~~
setShowAntOutput
~~~
getShowAntOutput
~~~
shouldAddDashboardLoggerJarToCommandLine
~~~
createJVMArg
~
args.add(arg);
~~~
createLib
~
libs.add(lib);
~~~
createListener
~
listeners.add(listener);
~~~
createProperty
~
properties.add(property);
~~~
getSystemClassPath
~~~
getAntLogAsElement
~
// instead of "xml-stylesheet": fix this
XMLFilter piFilter = new XMLFilterImpl() {

    public void processingInstruction(String target, String data) throws SAXException {
        if (target.equals("xml:stylesheet")) {
            target = "xml-stylesheet";
        }
        super.processingInstruction(target, data);
    }
};
~
// get rid of empty <task>- and <message>-elements created by Ant's XmlLogger
XMLFilter emptyTaskFilter = new EmptyElementFilter("task");
~
emptyTaskFilter.setParent(piFilter);
~
XMLFilter emptyMessageFilter = new EmptyElementFilter("message");
~
emptyMessageFilter.setParent(emptyTaskFilter);
~
builder.setXMLFilter(emptyMessageFilter);
~
if (ee instanceof CruiseControlException) {
    throw (CruiseControlException) ee;
}
~
file.renameTo(saveFile);
~~~
setUseDebug
~~~
setUseQuiet
~~~
setKeepGoing
~
this.keepGoing = keepGoing;
~~~
getLoggerClassName
~~~
setLoggerClassName
~
loggerClassName = string;
~
isLoggerClassNameSet = true;
~~~
setTimeout
~~~
setPropertyfile
~
this.propertyfile = propertyfile;
~~~
setProgressLoggerLib
~~~
getProgressLoggerLib
