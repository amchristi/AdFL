AntBuilder
~~~
validate
~
LOG.warn("usedebug and usequiet are ignored if uselogger is not set to 'true'!");
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
ValidationHelper.assertFalse(antScript != null && antHome != null, "'antHome' and 'antscript' cannot both be set");
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
~
if (!build.exists() && !build.isAbsolute() && antWorkingDir != null) {
    build = new File(antWorkingDir, buildFile);
}
~~~
setSaveLogDir
~
saveLogDir = null;
~~~
saveAntLog
~~~
setAntWorkingDir
~
antWorkingDir = dir;
~~~
setAntScript
~
this.antScript = antScript;
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
throw new CruiseControlException("ant logfile " + file.getAbsolutePath() + " does not exist.");
~
if (file.length() == 0) {
    throw new CruiseControlException("ant logfile " + file.getAbsolutePath() + " is empty. Your build probably failed. Check your CruiseControl logs.");
}
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
~
if (!file.exists()) {
    throw new CruiseControlException("ant logfile " + file.getAbsolutePath() + " does not exist.");
} else if (file.length() == 0) {
    throw new CruiseControlException("ant logfile " + file.getAbsolutePath() + " is empty. Your build probably failed. Check your CruiseControl logs.");
}
~
try {
    SAXBuilder builder = new SAXBuilder("org.apache.xerces.parsers.SAXParser");
    // instead of "xml-stylesheet": fix this
    XMLFilter piFilter = new XMLFilterImpl() {

        public void processingInstruction(String target, String data) throws SAXException {
            if (target.equals("xml:stylesheet")) {
                target = "xml-stylesheet";
            }
            super.processingInstruction(target, data);
        }
    };
    // get rid of empty <task>- and <message>-elements created by Ant's XmlLogger
    XMLFilter emptyTaskFilter = new EmptyElementFilter("task");
    emptyTaskFilter.setParent(piFilter);
    XMLFilter emptyMessageFilter = new EmptyElementFilter("message");
    emptyMessageFilter.setParent(emptyTaskFilter);
    builder.setXMLFilter(emptyMessageFilter);
    return builder.build(file).getRootElement();
} catch (Exception ee) {
    if (ee instanceof CruiseControlException) {
        throw (CruiseControlException) ee;
    }
    File saveFile = new File(file.getParentFile(), System.currentTimeMillis() + file.getName());
    file.renameTo(saveFile);
    throw new CruiseControlException("Error reading : " + file.getAbsolutePath() + ".  Saved as : " + saveFile.getAbsolutePath(), ee);
}
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
