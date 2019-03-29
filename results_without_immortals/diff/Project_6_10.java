Project
~~~
initializeTransientFields
~~~
readObject
~
stream.defaultReadObject();
~~~
execute
~
LOG.warn("not building project " + name + " because project has been stopped.");
~
buildFinished();
~
return;
~
if (isPaused) {
    LOG.info("not building project " + name + " because project has been paused.");
    buildFinished();
    return;
}
~
init();
~
LOG.error("exception attempting build in project " + name, e);
~
buildFinished();
~~~
build
~~~
useAndResetBuildTargetIfBuildWasForced
~
if (buildWasForced) {
    target = buildTarget;
    buildTarget = null;
}
~~~
resetBuildForcedOnlyIfBuildWasForced
~
buildForced = false;
~
if (buildWasForced) {
    buildForced = false;
}
~~~
setBuildStartTime
~~~
run
~
queue.requestBuild(projectConfig);
~
waitForBuildToFinish();
~
waitForNextBuild();
~
setState(ProjectState.QUEUED);
~
synchronized (scheduleMutex) {
    queue.requestBuild(projectConfig);
    waitForBuildToFinish();
}
~
waitIfPaused();
~
if (!stopped) {
    waitForNextBuild();
}
~
if (!stopped) {
    setState(ProjectState.QUEUED);
    synchronized (scheduleMutex) {
        queue.requestBuild(projectConfig);
        waitForBuildToFinish();
    }
}
~
final String message = "Project " + name + ".run() interrupted";
~
LOG.error(message, e);
~
throw new RuntimeException(message);
~
try {
    waitIfPaused();
    if (!stopped) {
        waitForNextBuild();
    }
    if (!stopped) {
        setState(ProjectState.QUEUED);
        synchronized (scheduleMutex) {
            queue.requestBuild(projectConfig);
            waitForBuildToFinish();
        }
    }
} catch (InterruptedException e) {
    final String message = "Project " + name + ".run() interrupted";
    LOG.error(message, e);
    throw new RuntimeException(message);
}
~
while (!stopped) {
    try {
        waitIfPaused();
        if (!stopped) {
            waitForNextBuild();
        }
        if (!stopped) {
            setState(ProjectState.QUEUED);
            synchronized (scheduleMutex) {
                queue.requestBuild(projectConfig);
                waitForBuildToFinish();
            }
        }
    } catch (InterruptedException e) {
        final String message = "Project " + name + ".run() interrupted";
        LOG.error(message, e);
        throw new RuntimeException(message);
    }
}
~
stopped = true;
~
LOG.info("Project " + name + " stopped");
~
LOG.info("Project " + name + " started");
~
try {
    while (!stopped) {
        try {
            waitIfPaused();
            if (!stopped) {
                waitForNextBuild();
            }
            if (!stopped) {
                setState(ProjectState.QUEUED);
                synchronized (scheduleMutex) {
                    queue.requestBuild(projectConfig);
                    waitForBuildToFinish();
                }
            }
        } catch (InterruptedException e) {
            final String message = "Project " + name + ".run() interrupted";
            LOG.error(message, e);
            throw new RuntimeException(message);
        }
    }
} finally {
    stopped = true;
    LOG.info("Project " + name + " stopped");
}
~~~
waitIfPaused
~
setState(ProjectState.PAUSED);
~~~
waitForNextBuild
~
setState(ProjectState.WAITING);
~
progress.setValue(msg);
~
final String msg = "next build in " + DateUtil.formatTime(waitTime);
~
info(msg);
~
synchronized (waitMutex) {
    setState(ProjectState.WAITING);
    progress.setValue(msg);
    waitMutex.wait(waitTime);
}
~~~
getTimeToNextBuild
~
debug("build finished within a minute, getting new time to next build");
~~~
needToWaitForNextBuild
~~~
isBuildForced
~~~
forceBuild
~~~
forceBuildWithTarget
~
this.buildTarget = buildTarget;
~
setBuildForced(true);
~~~
forceBuildWithTarget
~
this.buildTarget = buildTarget;
~
setBuildForced(true);
~~~
waitForBuildToFinish
~
debug("waiting for build to finish");
~~~
buildFinished
~
debug("build finished");
~~~
getModifications
~
info("Building anyway, since build was explicitly forced.");
~
info("Building anyway, since modifications not required");
~
info("no modification set but build was forced");
~
info("no modification set but no modifications required");
~
info("Building anyway, since buildAfterFailed is true and last build failed.");
~
debug("no modification set, nothing to detect.");
~
debug("getting changes since last build");
~
debug("getting changes since last successful build");
~
info("No modifications found, build not necessary.");
~
debug("new changes found; now getting complete set");
~
modifications = modificationSet.retrieveModificationsAsElement(lastSuccessfulBuild, progress);
~
if (checkNewChangesFirst) {
    debug("new changes found; now getting complete set");
    modifications = modificationSet.retrieveModificationsAsElement(lastSuccessfulBuild, progress);
}
~~~
checkOnlySinceLastBuild
~
if (lastBuild == null || lastSuccessfulBuild == null) {
    return false;
}
~~~
serializeProject
~
try {
    s.writeObject(this);
    s.flush();
    debug("Serializing project to [" + safeProjectName + ".ser]");
} finally {
    s.close();
}
~
LOG.warn("Error serializing project to [" + safeProjectName + ".ser]: " + e.getMessage(), e);
~~~
setLabelIncrementer
~
throw new IllegalArgumentException("label incrementer can't be null");
~
if (incrementer == null) {
    throw new IllegalArgumentException("label incrementer can't be null");
}
~
validateLabel(label, labelIncrementer);
~~~
getLabelIncrementer
~~~
setName
~~~
getName
~~~
setLabel
~~~
getLabel
~~~
setLastBuild
~~~
setLastSuccessfulBuild
~~~
getLastBuild
~
if (lastBuild == null) {
    return null;
}
~~~
getBuildForced
~~~
setBuildForced
~
if (forceNewBuildNow) {
    forceBuild();
}
~~~
getLastSuccessfulBuild
~
if (lastSuccessfulBuild == null) {
    return null;
}
~~~
getLogDir
~~~
getBuildInterval
~~~
overrideBuildInterval
~
overrideBuildInterval = sleepMillis;
~~~
isPaused
~~~
setPaused
~~~
setBuildAfterFailed
~
buildAfterFailed = rebuildEvenWithNoNewModifications;
~~~
getStatus
~~~
getStatusWithQueuePosition
~~~
getState
~~~
setState
~
state = newState;
~
info(getStatus());
~
notifyListeners(new ProjectStateChangedEvent(name, getState()));
~~~
setBuildQueue
~
queue = buildQueue;
~~~
getBuildStartTime
~~~
getLog
~~~
init
~
lastBuild = DateUtil.getMidnight();
~
lastSuccessfulBuild = lastBuild;
~
debug("buildInterval          = [" + getBuildInterval() + "]");
~
debug("buildForced            = [" + buildForced + "]");
~
debug("buildAfterFailed       = [" + buildAfterFailed + "]");
~
debug("requireModifcation     = [" + requiremodification + "]");
~
debug("forceOnly              = [" + forceOnly + "]");
~
debug("buildCounter           = [" + buildCounter + "]");
~
debug("isPaused               = [" + isPaused + "]");
~
debug("label                  = [" + label + "]");
~
debug("lastBuild              = [" + DateUtil.getFormattedTime(lastBuild) + "]");
~
debug("lastSuccessfulBuild    = [" + DateUtil.getFormattedTime(lastSuccessfulBuild) + "]");
~
debug("logDir                 = [" + projectConfig.getLog().getLogDir() + "]");
~
debug("logXmlEncoding         = [" + projectConfig.getLog().getLogXmlEncoding() + "]");
~
debug("wasLastBuildSuccessful = [" + wasLastBuildSuccessful + "]");
~
buildAfterFailed = projectConfig.shouldBuildAfterFailed();
~
if (lastBuild == null) {
    lastBuild = DateUtil.getMidnight();
}
~
if (lastSuccessfulBuild == null) {
    lastSuccessfulBuild = lastBuild;
}
~
if (LOG.isDebugEnabled()) {
    debug("buildInterval          = [" + getBuildInterval() + "]");
    debug("buildForced            = [" + buildForced + "]");
    debug("buildAfterFailed       = [" + buildAfterFailed + "]");
    debug("requireModifcation     = [" + requiremodification + "]");
    debug("forceOnly              = [" + forceOnly + "]");
    debug("buildCounter           = [" + buildCounter + "]");
    debug("isPaused               = [" + isPaused + "]");
    debug("label                  = [" + label + "]");
    debug("lastBuild              = [" + DateUtil.getFormattedTime(lastBuild) + "]");
    debug("lastSuccessfulBuild    = [" + DateUtil.getFormattedTime(lastSuccessfulBuild) + "]");
    debug("logDir                 = [" + projectConfig.getLog().getLogDir() + "]");
    debug("logXmlEncoding         = [" + projectConfig.getLog().getLogXmlEncoding() + "]");
    debug("wasLastBuildSuccessful = [" + wasLastBuildSuccessful + "]");
}
~~~
getProjectPropertiesElement
~~~
addProperty
~~~
getProjectPropertiesMap
~
buildProperties.putAll(additionalProperties);
~
additionalProperties.clear();
~
additionalProperties = null;
~
// TODO: Shouldn't have CVS specific properties here
buildProperties.put("cvstimestamp", CVSDateUtil.formatCVSDate(now));
~
buildProperties.put("cctimestamp", DateUtil.getFormattedTime(now));
~
buildProperties.put("cclastgoodbuildtimestamp", getLastSuccessfulBuild());
~
buildProperties.put("cclastbuildtimestamp", getLastBuild());
~
buildProperties.put("lastbuildsuccessful", String.valueOf(isLastBuildSuccessful()));
~
buildProperties.put("buildforced", String.valueOf(getBuildForced()));
~
if (projectConfig.getModificationSet() != null) {
    buildProperties.putAll(projectConfig.getModificationSet().getProperties());
}
~
if (additionalProperties != null && !additionalProperties.isEmpty()) {
    buildProperties.putAll(additionalProperties);
    additionalProperties.clear();
    additionalProperties = null;
}
~
return buildProperties;
~~~
getAdditionalProperties
~~~
publish
~
final StringBuilder message = new StringBuilder("exception publishing results");
~
message.append(" with ").append(publisher.getClass().getName());
~
message.append(" for project ").append(name);
~
LOG.error(message.toString(), t);
~~~
bootstrap
~
for (final Bootstrapper bootstrapper : projectConfig.getBootstrappers()) {
    bootstrapper.bootstrap();
}
~~~
validateLabel
~
debug(message);
~~~
isLastBuildSuccessful
~~~
setWasLastBuildSuccessful
~~~
info
~
LOG.info("Project " + name + ":  " + message);
~~~
debug
~
LOG.debug("Project " + name + ":  " + message);
~~~
start
~
LOG.info("Project " + name + " starting");
~~~
createNewSchedulingThread
~
LOG.warn("interrupted while waiting for scheduling thread to start", ie);
~~~
stop
~
LOG.info("Project " + name + " stopping");
~
stopped = true;
~
synchronized (pausedMutex) {
    pausedMutex.notifyAll();
}
~
synchronized (waitMutex) {
    waitMutex.notifyAll();
}
~
synchronized (scheduleMutex) {
    scheduleMutex.notifyAll();
}
~~~
toString
~~~
addBuildProgressListener
~~~
fireProgressEvent
~~~
addBuildResultListener
~~~
fireResultEvent
~~~
getListeners
~~~
setProjectConfig
~
if (projectConfig == null) {
    throw new IllegalArgumentException("project config can't be null");
}
~~~
notifyListeners
~
final StringBuilder message = new StringBuilder("exception notifying listener ");
~
message.append(listener.getClass().getName());
~
message.append(" for project ").append(name);
~
LOG.error(message.toString(), e);
~~~
equals
~
if (arg0 == null) {
    return false;
}
~
if (arg0.getClass().getName().equals(getClass().getName())) {
    final Project thatProject = (Project) arg0;
    return thatProject.name.equals(name);
}
~~~
hashCode
~~~
register
~
LOG.debug("Registering project mbean");
~
final ProjectController projectController = new ProjectController(this);
~
projectController.register(server);
~~~
getProjectConfig
~~~
getLastBuildDate
~~~
getProgress
~~~
getLogLabels
~~~
getLogLabelLines
