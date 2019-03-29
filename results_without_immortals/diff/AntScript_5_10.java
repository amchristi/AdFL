AntScript
~~~
buildCommandline
~
// empty args may break the command line
if (arg != null && arg.length() > 0) {
    cmdLine.createArgument(arg);
}
~
cmdLine.createArguments("-logger", getLoggerClassName());
~
cmdLine.createArguments("-logfile", tempFileName);
~
// need to use AntProgressXmlLogger as a listener
cmdLine.createArguments("-listener", CLASSNAME_ANTPROGRESS_XML_LISTENER);
~
cmdLine.createArgument("-DXmlLogger.file=" + tempFileName);
~
cmdLine.createArguments("-listener", AntBuilder.DEFAULT_LOGGER);
~
cmdLine.createArgument("-DXmlLogger.file=" + tempFileName);
~
cmdLine.createArgument("-quiet");
~
cmdLine.createArgument("-D" + property.getKey() + "=" + value);
~
for (final AntBuilder.JVMArg jvmArg : args) {
    final String arg = jvmArg.getArg();
    // empty args may break the command line
    if (arg != null && arg.length() > 0) {
        cmdLine.createArgument(arg);
    }
}
~
final List<String> classpathItems = getClasspathItems(systemClassPath, isWindows);
~
final String antLauncherJarLocation = getAntLauncherJarLocation(systemClassPath, classpathItems);
~
cmdLine.createArguments("-classpath", antLauncherJarLocation);
~
cmdLine.createArgument("org.apache.tools.ant.launch.Launcher");
~
cmdLine.createArguments("-lib", removeSaxonJars(classpathItems, isWindows));
~
if (useLogger) {
    cmdLine.createArguments("-logger", getLoggerClassName());
    cmdLine.createArguments("-logfile", tempFileName);
} else {
    cmdLine.createArguments("-listener", getLoggerClassName());
    cmdLine.createArgument("-DXmlLogger.file=" + tempFileName);
}
~
// use proper default logger if loggerClassName was not specified by config
setupResolvedLoggerClassname();
~
cmdLine.createArguments("-logger", getLoggerClassName());
~
if (useLogger) {
    // need to use AntProgressXmlLogger as a listener
    cmdLine.createArguments("-listener", CLASSNAME_ANTPROGRESS_XML_LISTENER);
    cmdLine.createArgument("-DXmlLogger.file=" + tempFileName);
} else {
    cmdLine.createArguments("-listener", AntBuilder.DEFAULT_LOGGER);
    cmdLine.createArgument("-DXmlLogger.file=" + tempFileName);
}
~
cmdLine.createArguments("-listener", CLASSNAME_DASHBOARD_LISTENER);
~
setupDefaultProgressLoggerLib();
~
// add -lib to progressLogger classes
cmdLine.createArguments("-lib", progressLoggerLib);
~
cmdLine.createArgument("-debug");
~
if (useQuiet) {
    cmdLine.createArgument("-quiet");
}
~
cmdLine.createArgument("-keep-going");
~
cmdLine.createArguments("-lib", lib.getSearchPath());
~
cmdLine.createArguments("-listener", listener.getClassName());
~
final String value = (String) property.getValue();
~
if (!"".equals(value)) {
    cmdLine.createArgument("-D" + property.getKey() + "=" + value);
}
~
cmdLine.createArgument("-D" + property.getName() + "=" + property.getValue());
~
cmdLine.createArguments("-propertyfile", propertyfile);
~
cmdLine.createArgument(targets.nextToken());
~~~
getAntLauncherJarLocation
~~~
getAntLauncherJarLocation
~~~
getClasspathItems
~~~
removeSaxonJars
~~~
removeSaxonJars
~~~
getSeparator
~~~
setupResolvedLoggerClassname
~~~
findDefaultProgressLoggerLib
~~~
setupDefaultProgressLoggerLib
~~~
consumeLine
~~~
setBuildProperties
~~~
getLoggerClassName
~~~
setLoggerClassName
~~~
setIsLoggerClassNameSet
~~~
setShowAntOutput
~~~
setAntScript
~~~
setArgs
~~~
setWindows
~~~
setBuildFile
~~~
setTempFileName
~~~
setUseDebug
~~~
setUseLogger
~~~
setUseQuiet
~~~
setKeepGoing
~~~
setUseScript
~~~
setSystemClassPath
~~~
setProperties
~~~
setLibs
~~~
setListeners
~~~
setTarget
~~~
getExitCode
~~~
setExitCode
~~~
setPropertyFile
~~~
setProgressLoggerLib
~~~
setProgress
~~~
setAntEnv
