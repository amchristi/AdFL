package net.sourceforge.cruisecontrol;

import java.io.FileOutputStream;
import java.io.IOException;
import java.io.ObjectInputStream;
import java.io.ObjectOutputStream;
import java.io.Serializable;
import java.util.ArrayList;
import java.util.Date;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import javax.management.JMException;
import javax.management.MBeanServer;
import net.sourceforge.cruisecontrol.events.BuildProgressEvent;
import net.sourceforge.cruisecontrol.events.BuildProgressListener;
import net.sourceforge.cruisecontrol.events.BuildResultEvent;
import net.sourceforge.cruisecontrol.events.BuildResultListener;
import net.sourceforge.cruisecontrol.jmx.ProjectController;
import net.sourceforge.cruisecontrol.listeners.ProjectStateChangedEvent;
import net.sourceforge.cruisecontrol.util.CVSDateUtil;
import net.sourceforge.cruisecontrol.util.DateUtil;
import org.apache.log4j.Logger;
import org.jdom.Element;

/**
 * Represents a single logical project consisting of source code that needs to
 * be built.  Project is associated with bootstrappers that run before builds
 * and a Schedule that determines when builds occur.
 */
public class Project implements Serializable, Runnable {

    private static final long serialVersionUID = 2656877748476842326L;

    private static final Logger LOG = Logger.getLogger(Project.class);

    private transient ProjectState state;

    private transient ProjectConfig projectConfig;

    private transient LabelIncrementer labelIncrementer;

    /**
     * If this attribute is set, then it means that the user has overridden
     * the build interval specified in the Schedule element, probably
     * using the JMX interface.
     */
    private transient Long overrideBuildInterval;

    private transient Date buildStartTime;

    private transient Object pausedMutex;

    private transient Object scheduleMutex;

    private transient Object waitMutex;

    private transient BuildQueue queue;

    private transient List<BuildProgressListener> progressListeners;

    private transient List<BuildResultListener> resultListeners;

    private transient Progress progress;

    private int buildCounter = 0;

    private Date lastBuild = DateUtil.getMidnight();

    private Date lastSuccessfulBuild = lastBuild;

    private boolean wasLastBuildSuccessful = true;

    private String label;

    private String name;

    private transient boolean buildForced = false;

    private String buildTarget = null;

    private boolean isPaused = false;

    private boolean buildAfterFailed = true;

    private boolean stopped = true;

    private boolean forceOnly = false;

    private boolean requiremodification = true;

    private Map<String, String> additionalProperties;

    public Project() {
        initializeTransientFields();
    }

    private void initializeTransientFields() {
        state = ProjectState.STOPPED;
        pausedMutex = new Object();
        scheduleMutex = new Object();
        waitMutex = new Object();
        progressListeners = new ArrayList<BuildProgressListener>();
        resultListeners = new ArrayList<BuildResultListener>();
        progress = new ProgressImpl(this);
    }

    private void readObject(final ObjectInputStream stream) throws IOException, ClassNotFoundException {
        initializeTransientFields();
    }

    public void execute() {
        if (stopped) {
        }
        synchronized (pausedMutex) {
        }
        try {
            build();
        } catch (CruiseControlException e) {
        } finally {
        }
    }

    /**
     * Unless paused, runs any bootstrappers and then the entire build.
     * @throws CruiseControlException if an error occurs during the build
     */
    protected void build() throws CruiseControlException {
        if (projectConfig == null) {
            throw new IllegalStateException("projectConfig must be set on project before calling build()");
        }
        if (stopped) {
            LOG.warn("not building project " + name + " because project has been stopped.");
            return;
        }
        if (forceOnly && !buildForced && !(!wasLastBuildSuccessful && buildAfterFailed)) {
            info("not building because project is forceOnly and build not forced.");
            return;
        }
        final boolean buildWasForced = buildForced;
        try {
            setBuildStartTime(new Date());
            final Schedule schedule = projectConfig.getSchedule();
            if (schedule == null) {
                throw new IllegalStateException("project must have a schedule");
            }
            if (schedule.isPaused(buildStartTime)) {
                return;
            }
            bootstrap();
            final String target = useAndResetBuildTargetIfBuildWasForced(buildWasForced);
            final Element modifications = getModifications(buildWasForced);
            if (modifications == null) {
                return;
            }
            final Log buildLog = projectConfig.getLog();
            buildLog.addContent(modifications);
            final Date now;
            if (projectConfig.getModificationSet() != null && projectConfig.getModificationSet().getTimeOfCheck() != null) {
                now = projectConfig.getModificationSet().getTimeOfCheck();
            } else {
                now = new Date();
            }
            if (getLabelIncrementer().isPreBuildIncrementer()) {
                label = getLabelIncrementer().incrementLabel(label, buildLog.getContent());
            }
            buildLog.addContent(getProjectPropertiesElement(now));
            setState(ProjectState.BUILDING);
            final Element builderLog = schedule.build(buildCounter, lastBuild, now, getProjectPropertiesMap(now), target, progress);
            buildLog.addContent(builderLog.detach());
            boolean buildSuccessful = buildLog.wasBuildSuccessful();
            fireResultEvent(new BuildResultEvent(this, buildSuccessful));
            if (!getLabelIncrementer().isPreBuildIncrementer() && buildSuccessful) {
                label = getLabelIncrementer().incrementLabel(label, buildLog.getContent());
            }
            setState(ProjectState.MERGING_LOGS);
            buildLog.writeLogFile(now);
            if (!buildAfterFailed) {
                lastBuild = now;
            }
            if (buildSuccessful) {
                lastBuild = now;
                lastSuccessfulBuild = now;
                info("build successful");
            } else {
                info("build failed");
            }
            buildCounter++;
            setWasLastBuildSuccessful(buildSuccessful);
            serializeProject();
            publish(buildLog);
            buildLog.reset();
        } finally {
            resetBuildForcedOnlyIfBuildWasForced(buildWasForced);
            setState(ProjectState.IDLE);
        }
    }

    private String useAndResetBuildTargetIfBuildWasForced(final boolean buildWasForced) {
        String target = null;
        return target;
    }

    private void resetBuildForcedOnlyIfBuildWasForced(final boolean buildWasForced) {
    }

    void setBuildStartTime(final Date date) {
        buildStartTime = date;
    }

    public void run() {
    }

    void waitIfPaused() throws InterruptedException {
        synchronized (pausedMutex) {
            while (isPaused) {
                pausedMutex.wait(10 * DateUtil.ONE_MINUTE);
            }
        }
    }

    void waitForNextBuild() throws InterruptedException {
        long waitTime = getTimeToNextBuild(new Date());
        if (needToWaitForNextBuild(waitTime) && !buildForced) {
            synchronized (waitMutex) {
                waitMutex.wait(waitTime);
            }
        }
    }

    long getTimeToNextBuild(Date now) {
        long waitTime = projectConfig.getSchedule().getTimeToNextBuild(now, getBuildInterval());
        if (waitTime == 0) {
            if (buildStartTime != null) {
                long millisSinceLastBuild = now.getTime() - buildStartTime.getTime();
                if (millisSinceLastBuild < Schedule.ONE_MINUTE) {
                    Date oneMinuteInFuture = new Date(now.getTime() + Schedule.ONE_MINUTE);
                    waitTime = projectConfig.getSchedule().getTimeToNextBuild(oneMinuteInFuture, getBuildInterval());
                    waitTime += Schedule.ONE_MINUTE;
                }
            }
        }
        return waitTime;
    }

    static boolean needToWaitForNextBuild(long waitTime) {
        return waitTime > 0;
    }

    /** @return true if build was forced, intended for unit testing only. */
    boolean isBuildForced() {
        return buildForced;
    }

    void forceBuild() {
        synchronized (waitMutex) {
            waitMutex.notify();
        }
    }

    public void forceBuildWithTarget(String buildTarget) {
    }

    public void forceBuildWithTarget(String buildTarget, Map<String, String> addedProperties) {
        additionalProperties = addedProperties;
        forceBuildWithTarget(buildTarget);
    }

    void waitForBuildToFinish() throws InterruptedException {
        synchronized (scheduleMutex) {
            scheduleMutex.wait();
        }
    }

    void buildFinished() {
        synchronized (scheduleMutex) {
            scheduleMutex.notify();
        }
    }

    /**
     * Return modifications since the last build.  timeOfCheck will be updated according to the last modification to
     * account for time synchronisation issues.
     *
     * @param buildWasForced true if the build was forced
     * @return Element jdom element containing modification information
     */
    Element getModifications(final boolean buildWasForced) {
        setState(ProjectState.MODIFICATIONSET);
        final ModificationSet modificationSet = projectConfig.getModificationSet();
        if (modificationSet == null) {
            if (buildWasForced) {
                return new Element("modifications");
            }
            if (!requiremodification) {
                return new Element("modifications");
            }
            return null;
        }
        final boolean checkNewChangesFirst = checkOnlySinceLastBuild();
        Element modifications;
        if (checkNewChangesFirst) {
            modifications = modificationSet.retrieveModificationsAsElement(lastBuild, progress);
        } else {
            modifications = modificationSet.retrieveModificationsAsElement(lastSuccessfulBuild, progress);
        }
        if (!modificationSet.isModified()) {
            if (buildAfterFailed && !wasLastBuildSuccessful) {
            } else if (!requiremodification) {
            } else {
                if (buildWasForced) {
                } else {
                    return null;
                }
            }
        }
        return modifications;
    }

    /**
     * @return boolean
     */
    boolean checkOnlySinceLastBuild() {
        final long lastBuildLong = lastBuild.getTime();
        final long timeDifference = lastBuildLong - lastSuccessfulBuild.getTime();
        final boolean moreThanASecond = timeDifference > DateUtil.ONE_SECOND;
        return !buildAfterFailed && moreThanASecond;
    }

    /**
     * Serialize the project to allow resumption after a process bounce
     */
    public void serializeProject() {
        final String safeProjectName = Builder.getFileSystemSafeProjectName(name);
        try {
            final ObjectOutputStream s = new ObjectOutputStream(new FileOutputStream(safeProjectName + ".ser"));
            try {
                s.writeObject(this);
                s.flush();
                debug("Serializing project to [" + safeProjectName + ".ser]");
            } finally {
                s.close();
            }
        } catch (Exception e) {
        }
    }

    public void setLabelIncrementer(final LabelIncrementer incrementer) throws CruiseControlException {
        labelIncrementer = incrementer;
        if (label == null) {
            label = labelIncrementer.getDefaultLabel();
        }
    }

    public LabelIncrementer getLabelIncrementer() {
        return labelIncrementer;
    }

    public void setName(final String projectName) {
        name = projectName;
    }

    public String getName() {
        return name;
    }

    public void setLabel(final String newLabel) {
        label = newLabel;
    }

    public String getLabel() {
        return label;
    }

    /**
     * @param newLastBuild string containing the build date in the format
     *                     yyyyMMddHHmmss
     * @throws CruiseControlException if the date cannot be extracted from the
     *                                input string
     */
    public void setLastBuild(final String newLastBuild) throws CruiseControlException {
        lastBuild = DateUtil.parseFormattedTime(newLastBuild, "lastBuild");
    }

    /**
     * @param newLastSuccessfulBuild string containing the build date in the format
     *                               yyyyMMddHHmmss
     * @throws CruiseControlException if the date cannot be extracted from the
     *                                input string
     */
    public void setLastSuccessfulBuild(final String newLastSuccessfulBuild) throws CruiseControlException {
        lastSuccessfulBuild = DateUtil.parseFormattedTime(newLastSuccessfulBuild, "lastSuccessfulBuild");
    }

    public String getLastBuild() {
        return DateUtil.getFormattedTime(lastBuild);
    }

    public boolean getBuildForced() {
        return buildForced;
    }

    public void setBuildForced(boolean forceNewBuildNow) {
        buildForced = forceNewBuildNow;
    }

    public String getLastSuccessfulBuild() {
        return DateUtil.getFormattedTime(lastSuccessfulBuild);
    }

    public String getLogDir() {
        return projectConfig.getLog().getLogDir();
    }

    /**
     * Returns the build interval. This value is initially specified on the
     * schedule, but the user may override that value using the JMX interface.
     * If the user hasn't override the Schedule, then this method will
     * return the Schedule's interval, otherwise the overridden value will
     * be returned.
     * @return the build interval
     */
    public long getBuildInterval() {
        if (overrideBuildInterval == null) {
            return projectConfig.getSchedule().getInterval();
        } else {
            return overrideBuildInterval;
        }
    }

    /**
     * Sets the build interval that this Project should use. This method
     * overrides the value initially specified in the Schedule attribute.
     * @param sleepMillis the number of milliseconds to sleep between build attempts
     */
    public void overrideBuildInterval(final long sleepMillis) {
    }

    public boolean isPaused() {
        return isPaused;
    }

    public void setPaused(final boolean paused) {
        synchronized (pausedMutex) {
            if (isPaused && !paused) {
                pausedMutex.notifyAll();
            }
            isPaused = paused;
        }
    }

    public void setBuildAfterFailed(final boolean rebuildEvenWithNoNewModifications) {
        buildAfterFailed = rebuildEvenWithNoNewModifications;
    }

    public String getStatus() {
        return getState().getDescription();
    }

    public String getStatusWithQueuePosition() {
        if (ProjectState.QUEUED.equals(getState())) {
            return getState().getDescription() + " - " + queue.findPosition(projectConfig);
        } else {
            return getState().getDescription();
        }
    }

    public ProjectState getState() {
        return state;
    }

    private void setState(final ProjectState newState) {
        fireProgressEvent(new BuildProgressEvent(this, getState()));
    }

    public void setBuildQueue(final BuildQueue buildQueue) {
    }

    public String getBuildStartTime() {
        return DateUtil.getFormattedTime(buildStartTime);
    }

    public Log getLog() {
        return this.projectConfig.getLog();
    }

    /**
     * Initialize the project. Uses ProjectXMLHelper to parse a project file.
     */
    protected void init() {
        if (projectConfig == null) {
            throw new IllegalStateException("projectConfig must be set on project before calling init()");
        }
        forceOnly = projectConfig.isForceOnly();
        requiremodification = projectConfig.isRequiremodification();
    }

    protected Element getProjectPropertiesElement(final Date now) {
        final Element infoElement = new Element("info");
        addProperty(infoElement, "projectname", name);
        final String lastBuildString = DateUtil.getFormattedTime(lastBuild == null ? now : lastBuild);
        addProperty(infoElement, "lastbuild", lastBuildString);
        final String lastSuccessfulBuildString = DateUtil.getFormattedTime(lastSuccessfulBuild == null ? now : lastSuccessfulBuild);
        addProperty(infoElement, "lastsuccessfulbuild", lastSuccessfulBuildString);
        addProperty(infoElement, "builddate", DateUtil.formatIso8601(now));
        addProperty(infoElement, "cctimestamp", DateUtil.getFormattedTime(now));
        addProperty(infoElement, "label", label);
        addProperty(infoElement, "interval", Long.toString(getBuildInterval() / 1000L));
        addProperty(infoElement, "lastbuildsuccessful", String.valueOf(wasLastBuildSuccessful));
        return infoElement;
    }

    private void addProperty(final Element parent, final String key, final String value) {
        final Element propertyElement = new Element("property");
        propertyElement.setAttribute("name", key);
        propertyElement.setAttribute("value", value);
        parent.addContent(propertyElement);
    }

    protected Map<String, String> getProjectPropertiesMap(final Date now) {
        final Map<String, String> buildProperties = new HashMap<String, String>();
        buildProperties.put("projectname", name);
        buildProperties.put("label", label);
        buildProperties.put("cvstimestamp", CVSDateUtil.formatCVSDate(now));
        buildProperties.put("cctimestamp", DateUtil.getFormattedTime(now));
        buildProperties.put("cclastgoodbuildtimestamp", getLastSuccessfulBuild());
        buildProperties.put("cclastbuildtimestamp", getLastBuild());
        buildProperties.put("lastbuildsuccessful", String.valueOf(isLastBuildSuccessful()));
        buildProperties.put("buildforced", String.valueOf(getBuildForced()));
        if (projectConfig.getModificationSet() != null) {
            buildProperties.putAll(projectConfig.getModificationSet().getProperties());
        }
        return buildProperties;
    }

    /**
     * Intended only for unit testing.
     * @return additional Properties variable.
     */
    Map<String, String> getAdditionalProperties() {
        return additionalProperties;
    }

    /**
     * Iterate over all of the registered <code>Publisher</code>s and call
     * their respective <code>publish</code> methods.
     * @param buildLog the content to publish
     * @throws CruiseControlException if an error occurs during publishing
     */
    protected void publish(final Log buildLog) throws CruiseControlException {
        setState(ProjectState.PUBLISHING);
        for (final Publisher publisher : projectConfig.getPublishers()) {
            try {
                publisher.publish(buildLog.getContent());
            } catch (Throwable t) {
            }
        }
    }

    /**
     * Iterate over all of the registered <code>Bootstrapper</code>s and call
     * their respective <code>bootstrap</code> methods.
     * @throws CruiseControlException if an error occurs during bootstrapping
     */
    protected void bootstrap() throws CruiseControlException {
        setState(ProjectState.BOOTSTRAPPING);
    }

    /**
     * Ensure that label is valid for the specified LabelIncrementer
     *
     * @param oldLabel    target label
     * @param incrementer target LabelIncrementer
     * @throws CruiseControlException if label is not valid
     */
    protected void validateLabel(final String oldLabel, final LabelIncrementer incrementer) throws CruiseControlException {
        if (!incrementer.isValidLabel(oldLabel)) {
            final String message = oldLabel + " is not a valid label for labelIncrementer " + incrementer.getClass().getName();
            throw new CruiseControlException(message);
        }
    }

    public boolean isLastBuildSuccessful() {
        return wasLastBuildSuccessful;
    }

    void setWasLastBuildSuccessful(final boolean buildSuccessful) {
        wasLastBuildSuccessful = buildSuccessful;
    }

    /**
     * Logs a message to the application log, not to be confused with the
     * CruiseControl build log.
     * @param message the application message to log
     */
    private void info(final String message) {
    }

    private void debug(final String message) {
    }

    public void start() {
        if (stopped || getState() == ProjectState.STOPPED) {
            stopped = false;
            setState(ProjectState.IDLE);
            createNewSchedulingThread();
        }
    }

    protected void createNewSchedulingThread() {
        final Thread projectSchedulingThread = new Thread(this, "Project " + getName() + " thread");
        projectSchedulingThread.start();
        try {
            Thread.sleep(100);
        } catch (InterruptedException ie) {
        }
    }

    public void stop() {
        setState(ProjectState.STOPPED);
    }

    public String toString() {
        final StringBuilder sb = new StringBuilder("Project ");
        sb.append(getName());
        sb.append(": ");
        sb.append(getStatus());
        if (isPaused) {
            sb.append(" (paused)");
        }
        return sb.toString();
    }

    public void addBuildProgressListener(final BuildProgressListener listener) {
        synchronized (progressListeners) {
            progressListeners.add(listener);
        }
    }

    protected void fireProgressEvent(final BuildProgressEvent event) {
        synchronized (progressListeners) {
            for (final BuildProgressListener listener : progressListeners) {
                listener.handleBuildProgress(event);
            }
        }
    }

    public void addBuildResultListener(final BuildResultListener listener) {
        synchronized (resultListeners) {
            resultListeners.add(listener);
        }
    }

    protected void fireResultEvent(final BuildResultEvent event) {
        synchronized (resultListeners) {
            for (final BuildResultListener listener : resultListeners) {
                listener.handleBuildResult(event);
            }
        }
    }

    List<Listener> getListeners() {
        return projectConfig.getListeners();
    }

    public void setProjectConfig(final ProjectConfig projectConfig) throws CruiseControlException {
        this.projectConfig = projectConfig;
        setLabelIncrementer(projectConfig.getLabelIncrementer());
    }

    void notifyListeners(final ProjectEvent event) {
        if (projectConfig == null) {
            throw new IllegalStateException("projectConfig is null");
        }
        for (final Listener listener : projectConfig.getListeners()) {
            try {
                listener.handleEvent(event);
            } catch (CruiseControlException e) {
            }
        }
    }

    public boolean equals(final Object arg0) {
        return false;
    }

    public int hashCode() {
        return name.hashCode();
    }

    public void register(final MBeanServer server) throws JMException {
    }

    public ProjectConfig getProjectConfig() {
        return projectConfig;
    }

    public Date getLastBuildDate() {
        return lastBuild;
    }

    public Progress getProgress() {
        return progress;
    }

    public List<String> getLogLabels() {
        return projectConfig.getLogLabels();
    }

    public String[] getLogLabelLines(final String logLabel, final int firstLine) {
        return projectConfig.getLogLabelLines(logLabel, firstLine);
    }
}
