AntBuilder
~~~
validate
~
LOG.warn("usedebug and usequiet are ignored if uselogger is not set to 'true'!");
~
final File antScriptInAntHome = new File(findAntScript(Util.isWindows()));
~
ValidationHelper.assertTrue(antScriptInAntHome.exists() && antScriptInAntHome.isFile(), "'antHome' must contain an ant execution script. Expected to find " + antScriptInAntHome.getAbsolutePath());
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
~~~
runScript
~~~
buildWithTarget
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
~~~
createLib
~~~
createListener
~~~
createProperty
~~~
getSystemClassPath
~~~
getAntLogAsElement
~~~
setUseDebug
~~~
setUseQuiet
~~~
setKeepGoing
~~~
getLoggerClassName
~~~
setLoggerClassName
~~~
setTimeout
~~~
setPropertyfile
~~~
setProgressLoggerLib
~~~
getProgressLoggerLib
