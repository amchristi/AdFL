/**
 * *****************************************************************************
 * CruiseControl, a Continuous Integration Toolkit
 * Copyright (c) 2001-2003, ThoughtWorks, Inc.
 * 200 E. Randolph, 25th Floor
 * Chicago, IL 60601 USA
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions
 * are met:
 *
 * + Redistributions of source code must retain the above copyright
 * notice, this list of conditions and the following disclaimer.
 *
 * + Redistributions in binary form must reproduce the above
 * copyright notice, this list of conditions and the following
 * disclaimer in the documentation and/or other materials provided
 * with the distribution.
 *
 * + Neither the name of ThoughtWorks, Inc., CruiseControl, nor the
 * names of its contributors may be used to endorse or promote
 * products derived from this software without specific prior
 * written permission.
 *
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
 * "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
 * LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
 * A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE REGENTS OR
 * CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL,
 * EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO,
 * PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR
 * PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF
 * LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
 * NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
 * SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 * ******************************************************************************
 */
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
import java.io.*;

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
        writeline("/home/ubuntu/results/coverage/Project/Project_8_10.coverage", "4a722e48-ea26-4d6a-9a97-2f2b9c2376ff");
        state = ProjectState.STOPPED;
        writeline("/home/ubuntu/results/coverage/Project/Project_8_10.coverage", "a594cd19-5a9f-4cbe-85df-3e6499dabc82");
        pausedMutex = new Object();
        writeline("/home/ubuntu/results/coverage/Project/Project_8_10.coverage", "633b3ef6-b1f3-4ed6-901f-9e23644eac8a");
        scheduleMutex = new Object();
        writeline("/home/ubuntu/results/coverage/Project/Project_8_10.coverage", "5d1d3793-c1b7-4c63-9468-344be209266f");
        waitMutex = new Object();
        writeline("/home/ubuntu/results/coverage/Project/Project_8_10.coverage", "2e5f2b8d-d0cc-435f-b049-a17ba20ffbed");
        progressListeners = new ArrayList<BuildProgressListener>();
        writeline("/home/ubuntu/results/coverage/Project/Project_8_10.coverage", "1b25f0f1-e00e-45f3-ab19-3cb1bfc45d8e");
        resultListeners = new ArrayList<BuildResultListener>();
        writeline("/home/ubuntu/results/coverage/Project/Project_8_10.coverage", "34a9b034-c770-4257-9ffb-3040bc6889ac");
        progress = new ProgressImpl(this);
    }

    private void readObject(final ObjectInputStream stream) throws IOException, ClassNotFoundException {
        writeline("/home/ubuntu/results/coverage/Project/Project_8_10.coverage", "46f79128-d05d-4f19-a5b1-b10b3bb3c950");
        stream.defaultReadObject();
        writeline("/home/ubuntu/results/coverage/Project/Project_8_10.coverage", "234063cf-7add-4864-979c-43d210b9b115");
        initializeTransientFields();
    }

    public void execute() {
        writeline("/home/ubuntu/results/coverage/Project/Project_8_10.coverage", "e9172400-b26a-47c9-9cea-875848ed44e1");
        if (stopped) {
            writeline("/home/ubuntu/results/coverage/Project/Project_8_10.coverage", "01877cd2-77a1-4ff4-a37c-bc6abb52b618");
            LOG.warn("not building project " + name + " because project has been stopped.");
            writeline("/home/ubuntu/results/coverage/Project/Project_8_10.coverage", "2b38dc8e-39e4-4d70-8415-d3747205b98e");
            buildFinished();
            writeline("/home/ubuntu/results/coverage/Project/Project_8_10.coverage", "577a3184-08fd-462a-a123-725d9e55145f");
            return;
        }
        writeline("/home/ubuntu/results/coverage/Project/Project_8_10.coverage", "8824146d-93f6-46d4-9950-e432e6ee8d22");
        synchronized (pausedMutex) {
            writeline("/home/ubuntu/results/coverage/Project/Project_8_10.coverage", "f279cd04-5ff4-49ce-b0a1-ff871e51ca65");
            if (isPaused) {
                writeline("/home/ubuntu/results/coverage/Project/Project_8_10.coverage", "3b765c42-c3c4-40a4-a93b-a56b7b2ecb26");
                LOG.info("not building project " + name + " because project has been paused.");
                writeline("/home/ubuntu/results/coverage/Project/Project_8_10.coverage", "3ab318e7-7873-454c-9c9c-35f1ae519326");
                buildFinished();
                writeline("/home/ubuntu/results/coverage/Project/Project_8_10.coverage", "6b184359-fb37-4be3-9ca9-cec4a099d3bf");
                return;
            }
        }
        writeline("/home/ubuntu/results/coverage/Project/Project_8_10.coverage", "cfe35929-73ec-485c-9d6b-5dcc2bcd134b");
        try {
            writeline("/home/ubuntu/results/coverage/Project/Project_8_10.coverage", "0e064946-29e9-4176-9d0a-44dbf9b2d3f0");
            init();
            writeline("/home/ubuntu/results/coverage/Project/Project_8_10.coverage", "71af84a0-96a6-4270-830e-c155ae8b6092");
            build();
        } catch (CruiseControlException e) {
            writeline("/home/ubuntu/results/coverage/Project/Project_8_10.coverage", "d3eb5ae1-0dd2-4e50-a1a4-6758283a3c2f");
            LOG.error("exception attempting build in project " + name, e);
        } finally {
            writeline("/home/ubuntu/results/coverage/Project/Project_8_10.coverage", "5939777b-3ae3-486d-ad90-58b33d657f7d");
            buildFinished();
        }
    }

    /**
     * Unless paused, runs any bootstrappers and then the entire build.
     * @throws CruiseControlException if an error occurs during the build
     */
    protected void build() throws CruiseControlException {
        writeline("/home/ubuntu/results/coverage/Project/Project_8_10.coverage", "5de98086-a6bd-430a-ad92-c2586d53c78a");
        if (projectConfig == null) {
            writeline("/home/ubuntu/results/coverage/Project/Project_8_10.coverage", "67747a9e-6d84-4c94-9cca-00a12dda7151");
            throw new IllegalStateException("projectConfig must be set on project before calling build()");
        }
        writeline("/home/ubuntu/results/coverage/Project/Project_8_10.coverage", "73c5a449-3cce-4914-ab4d-25be76e09f73");
        if (stopped) {
            writeline("/home/ubuntu/results/coverage/Project/Project_8_10.coverage", "10526bad-2ef3-41ff-bfda-b98df095c029");
            LOG.warn("not building project " + name + " because project has been stopped.");
            writeline("/home/ubuntu/results/coverage/Project/Project_8_10.coverage", "08ea08ad-0a5c-4e29-866d-39b156a9f275");
            return;
        }
        writeline("/home/ubuntu/results/coverage/Project/Project_8_10.coverage", "23f90585-31c6-405d-8698-a268c4844bc9");
        // or if the last build faild and we want to build on failures
        if (forceOnly && !buildForced && !(!wasLastBuildSuccessful && buildAfterFailed)) {
            writeline("/home/ubuntu/results/coverage/Project/Project_8_10.coverage", "b12c5451-8cb5-4c63-8be7-3c99adb34821");
            info("not building because project is forceOnly and build not forced.");
            writeline("/home/ubuntu/results/coverage/Project/Project_8_10.coverage", "c4f2b67c-09a0-456d-b364-b1e3415f6b3d");
            return;
        }
        writeline("/home/ubuntu/results/coverage/Project/Project_8_10.coverage", "a1929b8f-0a80-4a00-9ac6-47d8656305a1");
        final boolean buildWasForced = buildForced;
        writeline("/home/ubuntu/results/coverage/Project/Project_8_10.coverage", "9e6c3361-0f01-4ac9-8db8-56465addf065");
        try {
            writeline("/home/ubuntu/results/coverage/Project/Project_8_10.coverage", "7347e209-b00f-4e38-8f72-d81d67cb837f");
            setBuildStartTime(new Date());
            writeline("/home/ubuntu/results/coverage/Project/Project_8_10.coverage", "34e5acab-7126-49a3-a669-7a0bec28aa83");
            final Schedule schedule = projectConfig.getSchedule();
            writeline("/home/ubuntu/results/coverage/Project/Project_8_10.coverage", "b35b54aa-f403-4c8c-8a83-3c19b27757d9");
            if (schedule == null) {
                writeline("/home/ubuntu/results/coverage/Project/Project_8_10.coverage", "8e4704de-4a2b-47cd-9de4-993c026136af");
                throw new IllegalStateException("project must have a schedule");
            }
            writeline("/home/ubuntu/results/coverage/Project/Project_8_10.coverage", "974a6ab9-7a49-466d-aaa9-e685df97fa08");
            if (schedule.isPaused(buildStartTime)) {
                writeline("/home/ubuntu/results/coverage/Project/Project_8_10.coverage", "308a3cfc-7a4e-45b1-9590-b8f0a7c00e71");
                // is different than ProjectState.PAUSED
                return;
            }
            writeline("/home/ubuntu/results/coverage/Project/Project_8_10.coverage", "462fa431-afdb-45ef-8091-a14a23e6ec92");
            // @todo Add Progress param to Bootstrapper API?
            bootstrap();
            writeline("/home/ubuntu/results/coverage/Project/Project_8_10.coverage", "158a7399-ca5c-428a-9855-822d16cbcacc");
            final String target = useAndResetBuildTargetIfBuildWasForced(buildWasForced);
            writeline("/home/ubuntu/results/coverage/Project/Project_8_10.coverage", "a9f9550c-b2ab-4a0d-b3f9-3e21d96901b5");
            // @todo Add Progress param to ModificationSet API?
            // getModifications will only return null if we don't need to build
            final Element modifications = getModifications(buildWasForced);
            writeline("/home/ubuntu/results/coverage/Project/Project_8_10.coverage", "805b37c1-fa20-444c-937e-905f2601b82d");
            if (modifications == null) {
                writeline("/home/ubuntu/results/coverage/Project/Project_8_10.coverage", "d70e4a08-9aef-49b8-a1ca-50ffeee3ba7f");
                return;
            }
            writeline("/home/ubuntu/results/coverage/Project/Project_8_10.coverage", "97b2fd50-7525-4ec9-b0ef-e6b1e60427a4");
            // Using local reference to avoid NPE if config.xml is updated during build
            final Log buildLog = projectConfig.getLog();
            writeline("/home/ubuntu/results/coverage/Project/Project_8_10.coverage", "86134dd8-3632-48bf-a3cf-db2b1a0956cb");
            buildLog.addContent(modifications);
            writeline("/home/ubuntu/results/coverage/Project/Project_8_10.coverage", "e195a93f-b3ed-4304-adb8-80644fe41734");
            final Date now;
            writeline("/home/ubuntu/results/coverage/Project/Project_8_10.coverage", "58037073-98d5-4ac1-8abf-0929651bf35b");
            if (projectConfig.getModificationSet() != null && projectConfig.getModificationSet().getTimeOfCheck() != null) {
                writeline("/home/ubuntu/results/coverage/Project/Project_8_10.coverage", "9a2a0477-6018-4560-8b85-46929dfae62c");
                now = projectConfig.getModificationSet().getTimeOfCheck();
            } else {
                writeline("/home/ubuntu/results/coverage/Project/Project_8_10.coverage", "2c8d6829-0904-4dfb-b231-76df5c9db229");
                now = new Date();
            }
            writeline("/home/ubuntu/results/coverage/Project/Project_8_10.coverage", "cae8768d-7a9a-4aae-a307-8f885fd22bb3");
            if (getLabelIncrementer().isPreBuildIncrementer()) {
                writeline("/home/ubuntu/results/coverage/Project/Project_8_10.coverage", "7e80bff4-77fd-4263-8962-dad17a71a3f9");
                label = getLabelIncrementer().incrementLabel(label, buildLog.getContent());
            }
            writeline("/home/ubuntu/results/coverage/Project/Project_8_10.coverage", "7ce24dfb-1e83-4cab-8691-71f1d33d7922");
            // collect project information
            buildLog.addContent(getProjectPropertiesElement(now));
            writeline("/home/ubuntu/results/coverage/Project/Project_8_10.coverage", "6db4af2f-d4ba-4eb8-aebb-f08c56d53c82");
            setState(ProjectState.BUILDING);
            writeline("/home/ubuntu/results/coverage/Project/Project_8_10.coverage", "98ac5197-2165-40a7-9a92-f8e1f9a0d1b6");
            final Element builderLog = schedule.build(buildCounter, lastBuild, now, getProjectPropertiesMap(now), target, progress);
            writeline("/home/ubuntu/results/coverage/Project/Project_8_10.coverage", "a3c7806f-b534-4725-bb0f-7c6b44ea3bba");
            buildLog.addContent(builderLog.detach());
            writeline("/home/ubuntu/results/coverage/Project/Project_8_10.coverage", "1eafd585-a1df-4c0c-b1c3-5775a06765c3");
            boolean buildSuccessful = buildLog.wasBuildSuccessful();
            writeline("/home/ubuntu/results/coverage/Project/Project_8_10.coverage", "3ef25e3a-dd74-4e84-b935-967a7b57d3b3");
            fireResultEvent(new BuildResultEvent(this, buildSuccessful));
            writeline("/home/ubuntu/results/coverage/Project/Project_8_10.coverage", "32e23a21-6015-4686-8f8b-177ce63980a5");
            if (!getLabelIncrementer().isPreBuildIncrementer() && buildSuccessful) {
                writeline("/home/ubuntu/results/coverage/Project/Project_8_10.coverage", "4c6b2564-9a58-45d3-a3f6-40b8509bf66b");
                label = getLabelIncrementer().incrementLabel(label, buildLog.getContent());
            }
            writeline("/home/ubuntu/results/coverage/Project/Project_8_10.coverage", "93d98451-75a5-4fa7-bb39-906438da1dc0");
            setState(ProjectState.MERGING_LOGS);
            writeline("/home/ubuntu/results/coverage/Project/Project_8_10.coverage", "cb972a49-b539-4ad2-9560-e173eca4c981");
            buildLog.writeLogFile(now);
            writeline("/home/ubuntu/results/coverage/Project/Project_8_10.coverage", "91631422-c661-4cd1-890d-89b498eef2b5");
            // regardless of success or failure (buildAfterFailed = false in config.xml)
            if (!buildAfterFailed) {
                writeline("/home/ubuntu/results/coverage/Project/Project_8_10.coverage", "dae3cbea-045a-4962-8e5e-b3f1252d52bb");
                lastBuild = now;
            }
            writeline("/home/ubuntu/results/coverage/Project/Project_8_10.coverage", "b2ad9ed3-f2cc-4ddc-80c4-994c7a88b0f9");
            // If this was a successful build, update both last build and last successful build
            if (buildSuccessful) {
                writeline("/home/ubuntu/results/coverage/Project/Project_8_10.coverage", "86dd41c6-75e8-428b-bbaa-4b066c778118");
                lastBuild = now;
                writeline("/home/ubuntu/results/coverage/Project/Project_8_10.coverage", "c8da8a7d-d9db-47cd-810f-1c5c211b7ff2");
                lastSuccessfulBuild = now;
                writeline("/home/ubuntu/results/coverage/Project/Project_8_10.coverage", "e9653095-ff60-46de-9ee4-fecc2d1115a6");
                info("build successful");
            } else {
                writeline("/home/ubuntu/results/coverage/Project/Project_8_10.coverage", "2e0caf2c-fbe9-4e70-b852-f0015ae6a70b");
                info("build failed");
            }
            writeline("/home/ubuntu/results/coverage/Project/Project_8_10.coverage", "fb4f371b-436c-4c66-ac66-fff357f9cd35");
            buildCounter++;
            writeline("/home/ubuntu/results/coverage/Project/Project_8_10.coverage", "e5322b90-d901-4cd2-853b-f258c1d9950c");
            setWasLastBuildSuccessful(buildSuccessful);
            writeline("/home/ubuntu/results/coverage/Project/Project_8_10.coverage", "c5988c29-747c-4ff2-bea2-d409d703de01");
            // also need to reset forced flag before serializing, unless buildForced var is transient
            // resetBuildForcedOnlyIfBuildWasForced(buildWasForced);
            serializeProject();
            writeline("/home/ubuntu/results/coverage/Project/Project_8_10.coverage", "cc9f510c-813b-4571-9127-88a0d0aff6dc");
            // @todo Add Progress param to Publisher API?
            publish(buildLog);
            writeline("/home/ubuntu/results/coverage/Project/Project_8_10.coverage", "fa429768-5930-46c2-9cc9-f5662384eb75");
            buildLog.reset();
        } finally {
            writeline("/home/ubuntu/results/coverage/Project/Project_8_10.coverage", "d26eb63a-f0b8-4cb0-9c87-2d520f2084a1");
            resetBuildForcedOnlyIfBuildWasForced(buildWasForced);
            writeline("/home/ubuntu/results/coverage/Project/Project_8_10.coverage", "ad890a1e-f19a-4991-b399-1f7e9298e79e");
            setState(ProjectState.IDLE);
        }
    }

    private String useAndResetBuildTargetIfBuildWasForced(final boolean buildWasForced) {
        writeline("/home/ubuntu/results/coverage/Project/Project_8_10.coverage", "d878294b-54b0-4039-91a7-183ae151504c");
        String target = null;
        writeline("/home/ubuntu/results/coverage/Project/Project_8_10.coverage", "021dd9a7-af09-445d-afa9-95bad5dfe1d9");
        if (buildWasForced) {
            writeline("/home/ubuntu/results/coverage/Project/Project_8_10.coverage", "3eefa0e2-a226-404b-99d0-6698d332a120");
            target = buildTarget;
            writeline("/home/ubuntu/results/coverage/Project/Project_8_10.coverage", "122cb53d-7ba7-4b53-9cdb-50f6f5675507");
            buildTarget = null;
        }
        writeline("/home/ubuntu/results/coverage/Project/Project_8_10.coverage", "7096f04a-658a-4e8c-b1eb-107808cff50f");
        return target;
    }

    private void resetBuildForcedOnlyIfBuildWasForced(final boolean buildWasForced) {
        writeline("/home/ubuntu/results/coverage/Project/Project_8_10.coverage", "987f32e6-b5d9-4a9a-a520-39c13fac80e0");
        if (buildWasForced) {
            writeline("/home/ubuntu/results/coverage/Project/Project_8_10.coverage", "fbd71e34-edee-4b56-aa02-83256fa7c70f");
            buildForced = false;
        }
    }

    void setBuildStartTime(final Date date) {
        writeline("/home/ubuntu/results/coverage/Project/Project_8_10.coverage", "4e869024-9203-4f1c-8786-4991deca41fe");
        buildStartTime = date;
    }

    public void run() {
        writeline("/home/ubuntu/results/coverage/Project/Project_8_10.coverage", "7d4e556b-7df1-49b3-87fc-cb57b16a597c");
        LOG.info("Project " + name + " started");
        writeline("/home/ubuntu/results/coverage/Project/Project_8_10.coverage", "2cb0a602-a9bf-4842-87f6-d34bd4b6b0e2");
        try {
            writeline("/home/ubuntu/results/coverage/Project/Project_8_10.coverage", "be429d52-50ce-41c7-917b-50e5724ae091");
            while (!stopped) {
                writeline("/home/ubuntu/results/coverage/Project/Project_8_10.coverage", "777195e7-488c-47a8-805c-835c20ffa1e4");
                try {
                    writeline("/home/ubuntu/results/coverage/Project/Project_8_10.coverage", "88b0cf32-8f28-4b56-910a-3bb8bd459ce6");
                    waitIfPaused();
                    writeline("/home/ubuntu/results/coverage/Project/Project_8_10.coverage", "94671739-57d7-42c3-a95f-7b225dce748c");
                    if (!stopped) {
                        writeline("/home/ubuntu/results/coverage/Project/Project_8_10.coverage", "3b8c2b1e-8b74-4e35-9a1c-3daf86bae7ea");
                        waitForNextBuild();
                    }
                    writeline("/home/ubuntu/results/coverage/Project/Project_8_10.coverage", "0fd41276-9b60-4359-bcde-4d7c28554709");
                    if (!stopped) {
                        writeline("/home/ubuntu/results/coverage/Project/Project_8_10.coverage", "f8dfacc8-3bd9-4b82-8d98-76ea077df6d2");
                        setState(ProjectState.QUEUED);
                        writeline("/home/ubuntu/results/coverage/Project/Project_8_10.coverage", "fc934c76-1977-4d20-a11d-8d8c2a205951");
                        synchronized (scheduleMutex) {
                            writeline("/home/ubuntu/results/coverage/Project/Project_8_10.coverage", "03b368b5-89a5-48dd-a0e1-fbd4545a4e2b");
                            queue.requestBuild(projectConfig);
                            writeline("/home/ubuntu/results/coverage/Project/Project_8_10.coverage", "aebd60e2-06fd-4293-92e6-44fc8fb93f30");
                            waitForBuildToFinish();
                        }
                    }
                } catch (InterruptedException e) {
                    writeline("/home/ubuntu/results/coverage/Project/Project_8_10.coverage", "379eacb0-dcb7-407b-9fc3-4f407a68108b");
                    final String message = "Project " + name + ".run() interrupted";
                    writeline("/home/ubuntu/results/coverage/Project/Project_8_10.coverage", "e6b37f86-79b1-4622-95c0-402cf66110ee");
                    LOG.error(message, e);
                    writeline("/home/ubuntu/results/coverage/Project/Project_8_10.coverage", "f15817d3-08d2-4e7f-95d2-6b349e8c2c77");
                    throw new RuntimeException(message);
                }
            }
        } finally {
            writeline("/home/ubuntu/results/coverage/Project/Project_8_10.coverage", "c23ba133-33b4-4608-acbe-60231469f3eb");
            stopped = true;
            writeline("/home/ubuntu/results/coverage/Project/Project_8_10.coverage", "39e7d242-e1d4-4a78-8add-0f9616c7b263");
            LOG.info("Project " + name + " stopped");
        }
    }

    void waitIfPaused() throws InterruptedException {
        writeline("/home/ubuntu/results/coverage/Project/Project_8_10.coverage", "70c3810c-7024-49b0-8438-06593a020186");
        synchronized (pausedMutex) {
            writeline("/home/ubuntu/results/coverage/Project/Project_8_10.coverage", "dce57c84-5079-47d5-b125-7dc3c4633c03");
            while (isPaused) {
                writeline("/home/ubuntu/results/coverage/Project/Project_8_10.coverage", "32594157-c649-4564-801e-7f7c7e75e8da");
                setState(ProjectState.PAUSED);
                writeline("/home/ubuntu/results/coverage/Project/Project_8_10.coverage", "825a506d-78ad-4b71-bfe1-2408785b53f2");
                pausedMutex.wait(10 * DateUtil.ONE_MINUTE);
            }
        }
    }

    void waitForNextBuild() throws InterruptedException {
        writeline("/home/ubuntu/results/coverage/Project/Project_8_10.coverage", "e4095279-3fd7-4818-bacb-da25869470a1");
        long waitTime = getTimeToNextBuild(new Date());
        writeline("/home/ubuntu/results/coverage/Project/Project_8_10.coverage", "f6071cd8-4223-4289-aa7a-367505361dea");
        if (needToWaitForNextBuild(waitTime) && !buildForced) {
            writeline("/home/ubuntu/results/coverage/Project/Project_8_10.coverage", "544244d2-6edc-4fcc-a9da-b0e88c49ac9f");
            final String msg = "next build in " + DateUtil.formatTime(waitTime);
            writeline("/home/ubuntu/results/coverage/Project/Project_8_10.coverage", "599beac4-7ef0-4cd7-9bd5-63714decee6a");
            info(msg);
            writeline("/home/ubuntu/results/coverage/Project/Project_8_10.coverage", "61606dcb-8d2b-41ff-a8fc-176d87ad8399");
            synchronized (waitMutex) {
                writeline("/home/ubuntu/results/coverage/Project/Project_8_10.coverage", "51edcd8f-730d-4f5b-b20a-971ee55cb8af");
                setState(ProjectState.WAITING);
                writeline("/home/ubuntu/results/coverage/Project/Project_8_10.coverage", "bdd4bdbb-8a17-40a5-8aa8-faad6912a90c");
                progress.setValue(msg);
                writeline("/home/ubuntu/results/coverage/Project/Project_8_10.coverage", "80a6f63b-88db-4150-9512-be9ff596176a");
                waitMutex.wait(waitTime);
            }
        }
    }

    long getTimeToNextBuild(Date now) {
        writeline("/home/ubuntu/results/coverage/Project/Project_8_10.coverage", "9b0bdc68-a330-401e-bd3a-8cd04bfde0d7");
        long waitTime = projectConfig.getSchedule().getTimeToNextBuild(now, getBuildInterval());
        writeline("/home/ubuntu/results/coverage/Project/Project_8_10.coverage", "df9d31b2-dac1-4c3e-b01d-73df6d81e191");
        if (waitTime == 0) {
            writeline("/home/ubuntu/results/coverage/Project/Project_8_10.coverage", "38cc2448-63b9-4ea1-838b-532bbc1a8e1e");
            // project that has just built within a minute time
            if (buildStartTime != null) {
                writeline("/home/ubuntu/results/coverage/Project/Project_8_10.coverage", "0007830c-777f-46d8-a3ed-db2afd22f332");
                long millisSinceLastBuild = now.getTime() - buildStartTime.getTime();
                writeline("/home/ubuntu/results/coverage/Project/Project_8_10.coverage", "ab1aecac-54ed-4b9c-a7f1-ee720891da5b");
                if (millisSinceLastBuild < Schedule.ONE_MINUTE) {
                    writeline("/home/ubuntu/results/coverage/Project/Project_8_10.coverage", "a27c7b34-d1da-44e8-be25-bb1742ce391e");
                    debug("build finished within a minute, getting new time to next build");
                    writeline("/home/ubuntu/results/coverage/Project/Project_8_10.coverage", "5c948141-6308-47b1-9a7b-5132979ad837");
                    Date oneMinuteInFuture = new Date(now.getTime() + Schedule.ONE_MINUTE);
                    writeline("/home/ubuntu/results/coverage/Project/Project_8_10.coverage", "06a8adfd-94d1-4765-8435-9a9a7351f876");
                    waitTime = projectConfig.getSchedule().getTimeToNextBuild(oneMinuteInFuture, getBuildInterval());
                    writeline("/home/ubuntu/results/coverage/Project/Project_8_10.coverage", "27ba667b-0100-4664-bdb6-e699e370efe6");
                    waitTime += Schedule.ONE_MINUTE;
                }
            }
        }
        writeline("/home/ubuntu/results/coverage/Project/Project_8_10.coverage", "b1372033-abd9-4400-8159-8755075a229f");
        return waitTime;
    }

    static boolean needToWaitForNextBuild(long waitTime) {
        writelineStatic("/home/ubuntu/results/coverage/Project/Project_8_10.coverage", "531f4d0e-737a-4afa-943d-d03eb0247414");
        return waitTime > 0;
    }

    /**
     * @return true if build was forced, intended for unit testing only.
     */
    boolean isBuildForced() {
        writeline("/home/ubuntu/results/coverage/Project/Project_8_10.coverage", "1469842a-f4dd-4700-a1eb-6a436eca8e2e");
        return buildForced;
    }

    void forceBuild() {
        writeline("/home/ubuntu/results/coverage/Project/Project_8_10.coverage", "416b7606-5795-4db0-b86c-affe816d5488");
        synchronized (waitMutex) {
            writeline("/home/ubuntu/results/coverage/Project/Project_8_10.coverage", "c1a4fa7d-0fde-48c8-a34f-1eebcde7ce62");
            waitMutex.notify();
        }
    }

    public void forceBuildWithTarget(String buildTarget) {
        writeline("/home/ubuntu/results/coverage/Project/Project_8_10.coverage", "b78576e6-7f17-430d-aa44-e14c5a07e793");
        this.buildTarget = buildTarget;
        writeline("/home/ubuntu/results/coverage/Project/Project_8_10.coverage", "6dc83c36-3106-4162-85bd-ed14643f454a");
        setBuildForced(true);
    }

    public void forceBuildWithTarget(String buildTarget, Map<String, String> addedProperties) {
        writeline("/home/ubuntu/results/coverage/Project/Project_8_10.coverage", "db2005fa-71ca-4d2a-9920-7c3d9146d156");
        additionalProperties = addedProperties;
        writeline("/home/ubuntu/results/coverage/Project/Project_8_10.coverage", "f43ede22-9923-4bdd-bce7-39fe7104b940");
        forceBuildWithTarget(buildTarget);
    }

    void waitForBuildToFinish() throws InterruptedException {
        writeline("/home/ubuntu/results/coverage/Project/Project_8_10.coverage", "3ccb1eff-3c48-4028-963b-0d61964daaa5");
        synchronized (scheduleMutex) {
            writeline("/home/ubuntu/results/coverage/Project/Project_8_10.coverage", "d7ba8ad1-db12-47b0-b6ac-76fdbc5ef45a");
            debug("waiting for build to finish");
            writeline("/home/ubuntu/results/coverage/Project/Project_8_10.coverage", "beb0f3e8-5384-4d1b-8afa-c94a4c40dd11");
            scheduleMutex.wait();
        }
    }

    void buildFinished() {
        writeline("/home/ubuntu/results/coverage/Project/Project_8_10.coverage", "42b8712c-cbb1-49cc-b685-7e6f729b3513");
        synchronized (scheduleMutex) {
            writeline("/home/ubuntu/results/coverage/Project/Project_8_10.coverage", "52917fae-f615-4582-860d-85fae289f3bb");
            debug("build finished");
            writeline("/home/ubuntu/results/coverage/Project/Project_8_10.coverage", "d1fa4b82-d079-44e4-88d9-9cc8c3908928");
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
        writeline("/home/ubuntu/results/coverage/Project/Project_8_10.coverage", "1d7fbc07-201b-4050-b1ec-b748bab68207");
        setState(ProjectState.MODIFICATIONSET);
        writeline("/home/ubuntu/results/coverage/Project/Project_8_10.coverage", "45ca6fe2-b08e-45a7-8a2b-1414e7b59683");
        final ModificationSet modificationSet = projectConfig.getModificationSet();
        writeline("/home/ubuntu/results/coverage/Project/Project_8_10.coverage", "7d2d0a6b-e035-4925-9224-f64517d774b4");
        if (modificationSet == null) {
            writeline("/home/ubuntu/results/coverage/Project/Project_8_10.coverage", "f4555292-20d7-476c-b64c-50dfb23f91ec");
            debug("no modification set, nothing to detect.");
            writeline("/home/ubuntu/results/coverage/Project/Project_8_10.coverage", "a9ac4611-9bbc-42fc-bc18-b6136de1a5b0");
            if (buildWasForced) {
                writeline("/home/ubuntu/results/coverage/Project/Project_8_10.coverage", "95fc322c-a998-42d0-842e-0cffea055bec");
                info("no modification set but build was forced");
                writeline("/home/ubuntu/results/coverage/Project/Project_8_10.coverage", "f4897443-c982-4c19-8cd1-575dee2a32e5");
                return new Element("modifications");
            }
            writeline("/home/ubuntu/results/coverage/Project/Project_8_10.coverage", "ca75d6cd-5dbc-4a1c-bb97-463287275ac8");
            if (!requiremodification) {
                writeline("/home/ubuntu/results/coverage/Project/Project_8_10.coverage", "781f82fa-cc5a-4c2d-a89c-13520d7d7db7");
                info("no modification set but no modifications required");
                writeline("/home/ubuntu/results/coverage/Project/Project_8_10.coverage", "666b1e5e-ec25-4d37-8b07-2b5b639f5e6b");
                return new Element("modifications");
            }
            writeline("/home/ubuntu/results/coverage/Project/Project_8_10.coverage", "1975fc46-10d8-4217-b7ad-66417e4d2c96");
            return null;
        }
        writeline("/home/ubuntu/results/coverage/Project/Project_8_10.coverage", "6e51ce76-b1f3-462f-bb52-b276f94383da");
        final boolean checkNewChangesFirst = checkOnlySinceLastBuild();
        writeline("/home/ubuntu/results/coverage/Project/Project_8_10.coverage", "6ea9098f-3705-4da5-abc0-2a12173e1c27");
        Element modifications;
        writeline("/home/ubuntu/results/coverage/Project/Project_8_10.coverage", "c0329db9-a6ac-4bdc-94dd-0ef2a2a7b894");
        if (checkNewChangesFirst) {
            writeline("/home/ubuntu/results/coverage/Project/Project_8_10.coverage", "b92aa754-06bf-48d4-826c-8fae45617084");
            debug("getting changes since last build");
            writeline("/home/ubuntu/results/coverage/Project/Project_8_10.coverage", "10c120d4-c058-4afb-b9d0-d77fbceb6020");
            modifications = modificationSet.retrieveModificationsAsElement(lastBuild, progress);
        } else {
            writeline("/home/ubuntu/results/coverage/Project/Project_8_10.coverage", "fd44ead3-d786-4f7a-abeb-d80f4f510f18");
            debug("getting changes since last successful build");
            writeline("/home/ubuntu/results/coverage/Project/Project_8_10.coverage", "ab3393b4-f2f9-493c-8f1e-cdcd55970869");
            modifications = modificationSet.retrieveModificationsAsElement(lastSuccessfulBuild, progress);
        }
        writeline("/home/ubuntu/results/coverage/Project/Project_8_10.coverage", "9f974e2c-8392-4809-9230-433619c0fd19");
        if (!modificationSet.isModified()) {
            writeline("/home/ubuntu/results/coverage/Project/Project_8_10.coverage", "ae55e3db-a9fb-43af-b540-cbf676e3a489");
            info("No modifications found, build not necessary.");
            writeline("/home/ubuntu/results/coverage/Project/Project_8_10.coverage", "39fbb786-6f1d-4801-a2fd-99248ceb8931");
            // * build forced
            if (buildAfterFailed && !wasLastBuildSuccessful) {
                writeline("/home/ubuntu/results/coverage/Project/Project_8_10.coverage", "dc6dc49a-a823-4ae5-ada3-2f88179cd0f4");
                info("Building anyway, since buildAfterFailed is true and last build failed.");
            } else if (!requiremodification) {
                writeline("/home/ubuntu/results/coverage/Project/Project_8_10.coverage", "21bfaff7-ee45-4142-80f5-827877e3f746");
                info("Building anyway, since modifications not required");
            } else {
                writeline("/home/ubuntu/results/coverage/Project/Project_8_10.coverage", "5f338222-7bec-4138-8529-d8c00eace366");
                if (buildWasForced) {
                    writeline("/home/ubuntu/results/coverage/Project/Project_8_10.coverage", "39143692-262e-4f3d-ab2d-faa64fd33dd4");
                    info("Building anyway, since build was explicitly forced.");
                } else {
                    writeline("/home/ubuntu/results/coverage/Project/Project_8_10.coverage", "a47df97f-7ed2-42f5-a1b5-c977b906585e");
                    return null;
                }
            }
        }
        writeline("/home/ubuntu/results/coverage/Project/Project_8_10.coverage", "7d09928b-587b-4007-9143-f093fc7bda57");
        if (checkNewChangesFirst) {
            writeline("/home/ubuntu/results/coverage/Project/Project_8_10.coverage", "992b561a-9ed3-4962-a2d4-1c19d6d8d623");
            debug("new changes found; now getting complete set");
            writeline("/home/ubuntu/results/coverage/Project/Project_8_10.coverage", "99e4eb3b-5637-48cf-ac7a-1ae33c40e5f5");
            modifications = modificationSet.retrieveModificationsAsElement(lastSuccessfulBuild, progress);
        }
        writeline("/home/ubuntu/results/coverage/Project/Project_8_10.coverage", "15fc9dd1-3838-4de8-bbba-bd337c72e3e8");
        return modifications;
    }

    /**
     * @return boolean
     */
    boolean checkOnlySinceLastBuild() {
        writeline("/home/ubuntu/results/coverage/Project/Project_8_10.coverage", "eaff6984-4f85-4547-8eb0-769d7d20dc8b");
        if (lastBuild == null || lastSuccessfulBuild == null) {
            writeline("/home/ubuntu/results/coverage/Project/Project_8_10.coverage", "7d79957b-6fb7-47f0-a779-68fface558f5");
            return false;
        }
        writeline("/home/ubuntu/results/coverage/Project/Project_8_10.coverage", "77e6f518-3210-418a-ae19-04a01e0f1b0a");
        final long lastBuildLong = lastBuild.getTime();
        writeline("/home/ubuntu/results/coverage/Project/Project_8_10.coverage", "98abbc16-7216-44d3-8752-7c388e2efe7d");
        final long timeDifference = lastBuildLong - lastSuccessfulBuild.getTime();
        writeline("/home/ubuntu/results/coverage/Project/Project_8_10.coverage", "85b180ad-8995-40f8-9224-6d321f9a7d1e");
        final boolean moreThanASecond = timeDifference > DateUtil.ONE_SECOND;
        writeline("/home/ubuntu/results/coverage/Project/Project_8_10.coverage", "4daff3b2-af19-41e2-a5d0-8a8b65238df9");
        return !buildAfterFailed && moreThanASecond;
    }

    /**
     * Serialize the project to allow resumption after a process bounce
     */
    public void serializeProject() {
        writeline("/home/ubuntu/results/coverage/Project/Project_8_10.coverage", "b1f3b974-9241-4ad1-a649-51b807d03c11");
        final String safeProjectName = Builder.getFileSystemSafeProjectName(name);
        writeline("/home/ubuntu/results/coverage/Project/Project_8_10.coverage", "94b027be-4bc4-4a44-8721-e783201afca6");
        try {
            writeline("/home/ubuntu/results/coverage/Project/Project_8_10.coverage", "58129f15-fbf8-49ea-9a3c-ab081f82a931");
            final ObjectOutputStream s = new ObjectOutputStream(new FileOutputStream(safeProjectName + ".ser"));
            writeline("/home/ubuntu/results/coverage/Project/Project_8_10.coverage", "9888eb34-5410-4cbc-aaa8-41cb0108bab5");
            try {
                writeline("/home/ubuntu/results/coverage/Project/Project_8_10.coverage", "5ad0e4a6-f00c-4783-8666-860a9ff6d0ea");
                s.writeObject(this);
                writeline("/home/ubuntu/results/coverage/Project/Project_8_10.coverage", "584e59a5-76e1-4950-b399-606fb8300400");
                s.flush();
                writeline("/home/ubuntu/results/coverage/Project/Project_8_10.coverage", "9d40d311-cbfe-491e-8d0e-aed81b98d608");
                debug("Serializing project to [" + safeProjectName + ".ser]");
            } finally {
                writeline("/home/ubuntu/results/coverage/Project/Project_8_10.coverage", "37dce9c8-cb63-455c-94e9-754f2ea5d0b0");
                s.close();
            }
        } catch (Exception e) {
            writeline("/home/ubuntu/results/coverage/Project/Project_8_10.coverage", "2f231a85-dc08-471c-bbe2-a009279d8308");
            LOG.warn("Error serializing project to [" + safeProjectName + ".ser]: " + e.getMessage(), e);
        }
    }

    public void setLabelIncrementer(final LabelIncrementer incrementer) throws CruiseControlException {
        writeline("/home/ubuntu/results/coverage/Project/Project_8_10.coverage", "3124efea-c979-4d6f-9b4a-4c8931e9ad01");
        if (incrementer == null) {
            writeline("/home/ubuntu/results/coverage/Project/Project_8_10.coverage", "45c2ebc4-21a0-4d62-bae4-60b511409e4a");
            throw new IllegalArgumentException("label incrementer can't be null");
        }
        writeline("/home/ubuntu/results/coverage/Project/Project_8_10.coverage", "98e4d3c7-9764-4638-9c9d-35020e2a3e45");
        labelIncrementer = incrementer;
        writeline("/home/ubuntu/results/coverage/Project/Project_8_10.coverage", "a62172df-5210-4c58-9843-02ae35f40a6f");
        if (label == null) {
            writeline("/home/ubuntu/results/coverage/Project/Project_8_10.coverage", "4eb903e2-ecb9-4656-8aab-9eb5f4aa3728");
            label = labelIncrementer.getDefaultLabel();
        }
        writeline("/home/ubuntu/results/coverage/Project/Project_8_10.coverage", "71c5deef-7361-4fbb-84eb-dd4610d043f2");
        validateLabel(label, labelIncrementer);
    }

    public LabelIncrementer getLabelIncrementer() {
        writeline("/home/ubuntu/results/coverage/Project/Project_8_10.coverage", "fcdf21c8-bfe7-43ea-816d-d207b8d71ddd");
        return labelIncrementer;
    }

    public void setName(final String projectName) {
        writeline("/home/ubuntu/results/coverage/Project/Project_8_10.coverage", "1a2736d9-7e39-4e55-9c3b-af87c1f3a7af");
        name = projectName;
    }

    public String getName() {
        writeline("/home/ubuntu/results/coverage/Project/Project_8_10.coverage", "ef77d3ec-9db1-4468-a9f5-603b0c60f042");
        return name;
    }

    public void setLabel(final String newLabel) {
        writeline("/home/ubuntu/results/coverage/Project/Project_8_10.coverage", "a7db3d2c-4161-49c0-85a8-45f3128a3a1a");
        label = newLabel;
    }

    public String getLabel() {
        writeline("/home/ubuntu/results/coverage/Project/Project_8_10.coverage", "0c83b03d-2ca9-4825-8abe-42733cd3e9f2");
        return label;
    }

    /**
     * @param newLastBuild string containing the build date in the format
     * yyyyMMddHHmmss
     * @throws CruiseControlException if the date cannot be extracted from the
     * input string
     */
    public void setLastBuild(final String newLastBuild) throws CruiseControlException {
        writeline("/home/ubuntu/results/coverage/Project/Project_8_10.coverage", "9ba4e4eb-da9c-4c5e-a1f0-53b9deb8ba9e");
        lastBuild = DateUtil.parseFormattedTime(newLastBuild, "lastBuild");
    }

    /**
     * @param newLastSuccessfulBuild string containing the build date in the format
     * yyyyMMddHHmmss
     * @throws CruiseControlException if the date cannot be extracted from the
     * input string
     */
    public void setLastSuccessfulBuild(final String newLastSuccessfulBuild) throws CruiseControlException {
        writeline("/home/ubuntu/results/coverage/Project/Project_8_10.coverage", "83097d5c-8a6b-4d1e-9c37-57572b99aa16");
        lastSuccessfulBuild = DateUtil.parseFormattedTime(newLastSuccessfulBuild, "lastSuccessfulBuild");
    }

    public String getLastBuild() {
        writeline("/home/ubuntu/results/coverage/Project/Project_8_10.coverage", "b142014a-02b2-466c-8d5d-bbb86db89d9c");
        if (lastBuild == null) {
            writeline("/home/ubuntu/results/coverage/Project/Project_8_10.coverage", "40a6833a-c708-4ae9-a985-43406fd05727");
            return null;
        }
        writeline("/home/ubuntu/results/coverage/Project/Project_8_10.coverage", "fb83b5d5-4368-4644-9733-c144467e2a3c");
        return DateUtil.getFormattedTime(lastBuild);
    }

    public boolean getBuildForced() {
        writeline("/home/ubuntu/results/coverage/Project/Project_8_10.coverage", "53653bce-f1be-4ed8-abfd-05ecb2db6124");
        return buildForced;
    }

    public void setBuildForced(boolean forceNewBuildNow) {
        writeline("/home/ubuntu/results/coverage/Project/Project_8_10.coverage", "fd19b347-4b0f-4560-9fc6-e6ad69249c81");
        buildForced = forceNewBuildNow;
        writeline("/home/ubuntu/results/coverage/Project/Project_8_10.coverage", "c0c06dd4-b7fc-4df4-a43a-aa8b91d771bc");
        if (forceNewBuildNow) {
            writeline("/home/ubuntu/results/coverage/Project/Project_8_10.coverage", "938dbb0d-26d2-4f29-a92f-2b15a4e6f3d0");
            forceBuild();
        }
    }

    public String getLastSuccessfulBuild() {
        writeline("/home/ubuntu/results/coverage/Project/Project_8_10.coverage", "3f91e31e-69b0-49b4-9c47-3a8919a5cb82");
        if (lastSuccessfulBuild == null) {
            writeline("/home/ubuntu/results/coverage/Project/Project_8_10.coverage", "ba8ca749-ca31-4109-9532-ac230c0e0d43");
            return null;
        }
        writeline("/home/ubuntu/results/coverage/Project/Project_8_10.coverage", "0639c1c2-7042-4a66-bcac-65b4b6d7baf0");
        return DateUtil.getFormattedTime(lastSuccessfulBuild);
    }

    public String getLogDir() {
        writeline("/home/ubuntu/results/coverage/Project/Project_8_10.coverage", "6e523610-282d-47fd-9724-988cf1fccd55");
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
        writeline("/home/ubuntu/results/coverage/Project/Project_8_10.coverage", "c2a2c943-49a8-4638-b3a2-457dc92d6e66");
        if (overrideBuildInterval == null) {
            writeline("/home/ubuntu/results/coverage/Project/Project_8_10.coverage", "abfe6d34-915d-46b1-b7b5-1d1119b3a593");
            return projectConfig.getSchedule().getInterval();
        } else {
            writeline("/home/ubuntu/results/coverage/Project/Project_8_10.coverage", "0d41f472-fa48-4d2a-a2f1-7d1ba6be8c96");
            return overrideBuildInterval;
        }
    }

    /**
     * Sets the build interval that this Project should use. This method
     * overrides the value initially specified in the Schedule attribute.
     * @param sleepMillis the number of milliseconds to sleep between build attempts
     */
    public void overrideBuildInterval(final long sleepMillis) {
        writeline("/home/ubuntu/results/coverage/Project/Project_8_10.coverage", "57703c15-2165-44b8-ae5c-15aa44cdac86");
        overrideBuildInterval = sleepMillis;
    }

    public boolean isPaused() {
        writeline("/home/ubuntu/results/coverage/Project/Project_8_10.coverage", "8f9cba0e-696b-4425-8d7d-54b4874ff68d");
        return isPaused;
    }

    public void setPaused(final boolean paused) {
        writeline("/home/ubuntu/results/coverage/Project/Project_8_10.coverage", "edff3870-396e-481b-be0f-189f6ca9033a");
        synchronized (pausedMutex) {
            writeline("/home/ubuntu/results/coverage/Project/Project_8_10.coverage", "41e4b7f5-1fea-48e1-bdec-0934dfbb3480");
            if (isPaused && !paused) {
                writeline("/home/ubuntu/results/coverage/Project/Project_8_10.coverage", "6c231b18-4a3c-4c31-8695-5047a69fde77");
                pausedMutex.notifyAll();
            }
            writeline("/home/ubuntu/results/coverage/Project/Project_8_10.coverage", "af1b1205-5eea-4922-9b2f-ba29738f5944");
            isPaused = paused;
        }
    }

    public void setBuildAfterFailed(final boolean rebuildEvenWithNoNewModifications) {
        writeline("/home/ubuntu/results/coverage/Project/Project_8_10.coverage", "557ca1e6-6ee5-46dd-aeed-d9a0f4ad3277");
        buildAfterFailed = rebuildEvenWithNoNewModifications;
    }

    public String getStatus() {
        writeline("/home/ubuntu/results/coverage/Project/Project_8_10.coverage", "41262bdd-8ed9-4e5b-9e52-3e5e87664e0e");
        return getState().getDescription();
    }

    public String getStatusWithQueuePosition() {
        writeline("/home/ubuntu/results/coverage/Project/Project_8_10.coverage", "0f2f7fe9-c69f-45c1-97d9-34dec42e2738");
        if (ProjectState.QUEUED.equals(getState())) {
            writeline("/home/ubuntu/results/coverage/Project/Project_8_10.coverage", "12ef0d4e-cffc-45ee-9b3d-a134f1469919");
            return getState().getDescription() + " - " + queue.findPosition(projectConfig);
        } else {
            writeline("/home/ubuntu/results/coverage/Project/Project_8_10.coverage", "0012169c-119d-4a7e-b3f3-1d6148fa7adb");
            return getState().getDescription();
        }
    }

    public ProjectState getState() {
        writeline("/home/ubuntu/results/coverage/Project/Project_8_10.coverage", "91e6b136-0dd2-41a4-918e-2dd41cb2ebae");
        return state;
    }

    private void setState(final ProjectState newState) {
        writeline("/home/ubuntu/results/coverage/Project/Project_8_10.coverage", "319bd1d9-fb7e-44ac-a366-ad50d7399857");
        state = newState;
        writeline("/home/ubuntu/results/coverage/Project/Project_8_10.coverage", "b62adf41-1687-4b43-8453-b14e69803769");
        info(getStatus());
        writeline("/home/ubuntu/results/coverage/Project/Project_8_10.coverage", "5b26f5e4-f8a8-4cc2-9ac9-1902937f311f");
        notifyListeners(new ProjectStateChangedEvent(name, getState()));
        writeline("/home/ubuntu/results/coverage/Project/Project_8_10.coverage", "9508dad2-7b14-4913-a107-980c80aacb29");
        fireProgressEvent(new BuildProgressEvent(this, getState()));
    }

    public void setBuildQueue(final BuildQueue buildQueue) {
        writeline("/home/ubuntu/results/coverage/Project/Project_8_10.coverage", "5f5cb7d7-1212-46cd-ab67-7bb2141a70d2");
        queue = buildQueue;
    }

    public String getBuildStartTime() {
        writeline("/home/ubuntu/results/coverage/Project/Project_8_10.coverage", "f558ce39-86b6-4c5e-883a-e8efaf28c98e");
        return DateUtil.getFormattedTime(buildStartTime);
    }

    public Log getLog() {
        writeline("/home/ubuntu/results/coverage/Project/Project_8_10.coverage", "cf55dcf4-295c-48fe-bec2-e53a8461160b");
        return this.projectConfig.getLog();
    }

    /**
     * Initialize the project. Uses ProjectXMLHelper to parse a project file.
     */
    protected void init() {
        writeline("/home/ubuntu/results/coverage/Project/Project_8_10.coverage", "4eb52e8c-8548-4d3d-acda-ee09370774c7");
        if (projectConfig == null) {
            writeline("/home/ubuntu/results/coverage/Project/Project_8_10.coverage", "bd639dc0-a3ba-4a5f-8953-95054b8730c3");
            throw new IllegalStateException("projectConfig must be set on project before calling init()");
        }
        writeline("/home/ubuntu/results/coverage/Project/Project_8_10.coverage", "bd7e0649-0f99-49e4-a440-42b2cced31ef");
        buildAfterFailed = projectConfig.shouldBuildAfterFailed();
        writeline("/home/ubuntu/results/coverage/Project/Project_8_10.coverage", "7e23f236-15cc-4a98-904a-64d7174810de");
        forceOnly = projectConfig.isForceOnly();
        writeline("/home/ubuntu/results/coverage/Project/Project_8_10.coverage", "b8726b24-2469-4c0e-98e6-89e089c50496");
        requiremodification = projectConfig.isRequiremodification();
        writeline("/home/ubuntu/results/coverage/Project/Project_8_10.coverage", "d2871418-5fed-499f-a127-eeef6df83503");
        if (lastBuild == null) {
            writeline("/home/ubuntu/results/coverage/Project/Project_8_10.coverage", "a3ca0a5a-088d-40ce-8520-bb37fa3dcb8a");
            lastBuild = DateUtil.getMidnight();
        }
        writeline("/home/ubuntu/results/coverage/Project/Project_8_10.coverage", "fad5a3e4-adb0-4267-a3f8-1784729457a7");
        if (lastSuccessfulBuild == null) {
            writeline("/home/ubuntu/results/coverage/Project/Project_8_10.coverage", "610325bc-768b-4e80-b075-db803ae8b85d");
            lastSuccessfulBuild = lastBuild;
        }
        writeline("/home/ubuntu/results/coverage/Project/Project_8_10.coverage", "50d6bee6-0e2b-48b1-b57e-4cff7dfaca92");
        if (LOG.isDebugEnabled()) {
            writeline("/home/ubuntu/results/coverage/Project/Project_8_10.coverage", "18419559-0d5c-403b-9a2b-1ce3f82abd69");
            debug("buildInterval          = [" + getBuildInterval() + "]");
            writeline("/home/ubuntu/results/coverage/Project/Project_8_10.coverage", "6ecbb453-4024-4a7f-be5e-cd9dadd76a34");
            debug("buildForced            = [" + buildForced + "]");
            writeline("/home/ubuntu/results/coverage/Project/Project_8_10.coverage", "c290792d-e49a-4e00-b9e4-5024b770cfde");
            debug("buildAfterFailed       = [" + buildAfterFailed + "]");
            writeline("/home/ubuntu/results/coverage/Project/Project_8_10.coverage", "047e0ddd-2c43-4faa-a85d-daa626c365e6");
            debug("requireModifcation     = [" + requiremodification + "]");
            writeline("/home/ubuntu/results/coverage/Project/Project_8_10.coverage", "face3197-919a-47f9-923a-7914e17efa2b");
            debug("forceOnly              = [" + forceOnly + "]");
            writeline("/home/ubuntu/results/coverage/Project/Project_8_10.coverage", "e9fe1372-a3fe-4806-a448-1249f4e5e8be");
            debug("buildCounter           = [" + buildCounter + "]");
            writeline("/home/ubuntu/results/coverage/Project/Project_8_10.coverage", "ab06b8da-eaf9-43c3-901b-a24545522900");
            debug("isPaused               = [" + isPaused + "]");
            writeline("/home/ubuntu/results/coverage/Project/Project_8_10.coverage", "8425f3b4-56b8-4d43-930f-ff67e01efc18");
            debug("label                  = [" + label + "]");
            writeline("/home/ubuntu/results/coverage/Project/Project_8_10.coverage", "8f5a332e-040b-4924-99b4-f4b747ba5087");
            debug("lastBuild              = [" + DateUtil.getFormattedTime(lastBuild) + "]");
            writeline("/home/ubuntu/results/coverage/Project/Project_8_10.coverage", "abdaa42f-b2b1-4515-92b0-099a8dbc5722");
            debug("lastSuccessfulBuild    = [" + DateUtil.getFormattedTime(lastSuccessfulBuild) + "]");
            writeline("/home/ubuntu/results/coverage/Project/Project_8_10.coverage", "c448ef22-07cb-49be-a662-8df19ff32141");
            debug("logDir                 = [" + projectConfig.getLog().getLogDir() + "]");
            writeline("/home/ubuntu/results/coverage/Project/Project_8_10.coverage", "686f08ba-529d-4af8-b027-d194c66b030e");
            debug("logXmlEncoding         = [" + projectConfig.getLog().getLogXmlEncoding() + "]");
            writeline("/home/ubuntu/results/coverage/Project/Project_8_10.coverage", "8780814b-072e-4b0a-97b5-240ae470f21f");
            debug("wasLastBuildSuccessful = [" + wasLastBuildSuccessful + "]");
        }
    }

    protected Element getProjectPropertiesElement(final Date now) {
        writeline("/home/ubuntu/results/coverage/Project/Project_8_10.coverage", "51350459-1399-4360-a425-94ccc4d4bfe9");
        final Element infoElement = new Element("info");
        writeline("/home/ubuntu/results/coverage/Project/Project_8_10.coverage", "d97d0d5b-536e-4ffd-a660-b9a04e04fe1d");
        addProperty(infoElement, "projectname", name);
        writeline("/home/ubuntu/results/coverage/Project/Project_8_10.coverage", "18bf3848-f45b-442d-8dcc-23b9d43d0b25");
        final String lastBuildString = DateUtil.getFormattedTime(lastBuild == null ? now : lastBuild);
        writeline("/home/ubuntu/results/coverage/Project/Project_8_10.coverage", "9b0facb0-2e7a-4cac-b6a7-3a691a132b18");
        addProperty(infoElement, "lastbuild", lastBuildString);
        writeline("/home/ubuntu/results/coverage/Project/Project_8_10.coverage", "7fb971c6-4d21-4dd0-9f88-66cbb7d79a1c");
        final String lastSuccessfulBuildString = DateUtil.getFormattedTime(lastSuccessfulBuild == null ? now : lastSuccessfulBuild);
        writeline("/home/ubuntu/results/coverage/Project/Project_8_10.coverage", "2ad35f58-db1c-4878-aa3f-dd976d8bb88a");
        addProperty(infoElement, "lastsuccessfulbuild", lastSuccessfulBuildString);
        writeline("/home/ubuntu/results/coverage/Project/Project_8_10.coverage", "4d64157c-fe34-48ee-a586-5fa4b694e81b");
        addProperty(infoElement, "builddate", DateUtil.formatIso8601(now));
        writeline("/home/ubuntu/results/coverage/Project/Project_8_10.coverage", "f4c666f1-a6d0-409c-8053-34be03c73878");
        addProperty(infoElement, "cctimestamp", DateUtil.getFormattedTime(now));
        writeline("/home/ubuntu/results/coverage/Project/Project_8_10.coverage", "295fc2a7-062d-4029-b42a-1d52c733e832");
        addProperty(infoElement, "label", label);
        writeline("/home/ubuntu/results/coverage/Project/Project_8_10.coverage", "4e18bb73-43b4-4581-809d-7ff53edfd34c");
        addProperty(infoElement, "interval", Long.toString(getBuildInterval() / 1000L));
        writeline("/home/ubuntu/results/coverage/Project/Project_8_10.coverage", "606e95e1-40d2-4d3d-9a64-7d3611086f57");
        addProperty(infoElement, "lastbuildsuccessful", String.valueOf(wasLastBuildSuccessful));
        writeline("/home/ubuntu/results/coverage/Project/Project_8_10.coverage", "8018acd4-d34c-4f3a-af5b-a11ece8740cc");
        return infoElement;
    }

    private void addProperty(final Element parent, final String key, final String value) {
        writeline("/home/ubuntu/results/coverage/Project/Project_8_10.coverage", "c5cc8c53-b9d5-428d-b85b-f1f0fc5409b9");
        final Element propertyElement = new Element("property");
        writeline("/home/ubuntu/results/coverage/Project/Project_8_10.coverage", "9abbbdbb-b122-46fa-9710-f129eb10f216");
        propertyElement.setAttribute("name", key);
        writeline("/home/ubuntu/results/coverage/Project/Project_8_10.coverage", "fbf026f6-7bdb-48e4-848b-706d43ea5840");
        propertyElement.setAttribute("value", value);
        writeline("/home/ubuntu/results/coverage/Project/Project_8_10.coverage", "dd9e513c-c9c2-4b3b-a9dd-fe8975c17d99");
        parent.addContent(propertyElement);
    }

    protected Map<String, String> getProjectPropertiesMap(final Date now) {
        writeline("/home/ubuntu/results/coverage/Project/Project_8_10.coverage", "66027a62-ab03-485d-994e-8292dc5b3ada");
        final Map<String, String> buildProperties = new HashMap<String, String>();
        writeline("/home/ubuntu/results/coverage/Project/Project_8_10.coverage", "5f273f30-1729-4fcd-bed4-b63e698d1fbb");
        buildProperties.put("projectname", name);
        writeline("/home/ubuntu/results/coverage/Project/Project_8_10.coverage", "1313a65b-207e-43bb-93b2-ed47aa69e5eb");
        buildProperties.put("label", label);
        writeline("/home/ubuntu/results/coverage/Project/Project_8_10.coverage", "615f6d8b-de8b-48ee-9cfc-72c1e79e2cd2");
        // TODO: Shouldn't have CVS specific properties here
        buildProperties.put("cvstimestamp", CVSDateUtil.formatCVSDate(now));
        writeline("/home/ubuntu/results/coverage/Project/Project_8_10.coverage", "a56c155f-7000-49cc-a11d-6e59759d06db");
        buildProperties.put("cctimestamp", DateUtil.getFormattedTime(now));
        writeline("/home/ubuntu/results/coverage/Project/Project_8_10.coverage", "7354a8c3-7ad6-4e1f-8224-787b7f3addb7");
        buildProperties.put("cclastgoodbuildtimestamp", getLastSuccessfulBuild());
        writeline("/home/ubuntu/results/coverage/Project/Project_8_10.coverage", "59c5a701-e6cf-4bfc-8594-4fbb6bc9af73");
        buildProperties.put("cclastbuildtimestamp", getLastBuild());
        writeline("/home/ubuntu/results/coverage/Project/Project_8_10.coverage", "fb82ef3c-2f1f-4e0b-9657-11c01bbe6380");
        buildProperties.put("lastbuildsuccessful", String.valueOf(isLastBuildSuccessful()));
        writeline("/home/ubuntu/results/coverage/Project/Project_8_10.coverage", "fac7cb36-22ed-4f95-8720-17f604d71f43");
        buildProperties.put("buildforced", String.valueOf(getBuildForced()));
        writeline("/home/ubuntu/results/coverage/Project/Project_8_10.coverage", "4c73f2b6-08d0-42f6-abd0-c33c4de43259");
        if (projectConfig.getModificationSet() != null) {
            writeline("/home/ubuntu/results/coverage/Project/Project_8_10.coverage", "51142830-a3c7-420c-a273-c94a06f77381");
            buildProperties.putAll(projectConfig.getModificationSet().getProperties());
        }
        writeline("/home/ubuntu/results/coverage/Project/Project_8_10.coverage", "c5256a60-1e8e-4ec8-91ce-43a96a12b9fc");
        if (additionalProperties != null && !additionalProperties.isEmpty()) {
            writeline("/home/ubuntu/results/coverage/Project/Project_8_10.coverage", "ce43bfc3-16f0-4ad6-9909-0edd57eebdf7");
            buildProperties.putAll(additionalProperties);
            writeline("/home/ubuntu/results/coverage/Project/Project_8_10.coverage", "b0a91323-306a-4d8b-ae7f-177bd0980cb6");
            additionalProperties.clear();
            writeline("/home/ubuntu/results/coverage/Project/Project_8_10.coverage", "0e7cfced-a09d-4494-ad12-94db56dfb8fd");
            additionalProperties = null;
        }
        writeline("/home/ubuntu/results/coverage/Project/Project_8_10.coverage", "19387cfb-159c-45a7-bc0f-10ded848b1cf");
        return buildProperties;
    }

    /**
     * Intended only for unit testing.
     * @return additional Properties variable.
     */
    Map<String, String> getAdditionalProperties() {
        writeline("/home/ubuntu/results/coverage/Project/Project_8_10.coverage", "fdede088-b172-40c7-87d2-0f603545fb69");
        return additionalProperties;
    }

    /**
     * Iterate over all of the registered <code>Publisher</code>s and call
     * their respective <code>publish</code> methods.
     * @param buildLog the content to publish
     * @throws CruiseControlException if an error occurs during publishing
     */
    protected void publish(final Log buildLog) throws CruiseControlException {
        writeline("/home/ubuntu/results/coverage/Project/Project_8_10.coverage", "c9ba5ced-9f1a-4d85-96a5-223a2df81981");
        setState(ProjectState.PUBLISHING);
        writeline("/home/ubuntu/results/coverage/Project/Project_8_10.coverage", "a98e923c-791a-4aa8-9685-0b4e4673dde8");
        for (final Publisher publisher : projectConfig.getPublishers()) {
            writeline("/home/ubuntu/results/coverage/Project/Project_8_10.coverage", "75102e20-9545-42a6-8c7c-9a63cb163bb2");
            // catch all errors, Publishers shouldn't cause failures in the build method
            try {
                writeline("/home/ubuntu/results/coverage/Project/Project_8_10.coverage", "52487b52-1f32-4950-b18d-fe39c9d4a5bc");
                publisher.publish(buildLog.getContent());
            } catch (Throwable t) {
                writeline("/home/ubuntu/results/coverage/Project/Project_8_10.coverage", "caf550d4-efd3-4242-a150-78e7f5c9d34e");
                final StringBuilder message = new StringBuilder("exception publishing results");
                writeline("/home/ubuntu/results/coverage/Project/Project_8_10.coverage", "f0dcada4-ef25-4396-86fc-bd514da039b3");
                message.append(" with ").append(publisher.getClass().getName());
                writeline("/home/ubuntu/results/coverage/Project/Project_8_10.coverage", "f3fcb034-8a54-464f-8d62-a7776c847b3a");
                message.append(" for project ").append(name);
                writeline("/home/ubuntu/results/coverage/Project/Project_8_10.coverage", "2044ef7e-8262-4cc7-affc-f55481b364db");
                LOG.error(message.toString(), t);
            }
        }
    }

    /**
     * Iterate over all of the registered <code>Bootstrapper</code>s and call
     * their respective <code>bootstrap</code> methods.
     * @throws CruiseControlException if an error occurs during bootstrapping
     */
    protected void bootstrap() throws CruiseControlException {
        writeline("/home/ubuntu/results/coverage/Project/Project_8_10.coverage", "b9c9d282-208d-41b5-a22c-437bf5fe547f");
        setState(ProjectState.BOOTSTRAPPING);
        writeline("/home/ubuntu/results/coverage/Project/Project_8_10.coverage", "599397eb-12e7-4702-bcf1-4e9fba66fae7");
        for (final Bootstrapper bootstrapper : projectConfig.getBootstrappers()) {
            writeline("/home/ubuntu/results/coverage/Project/Project_8_10.coverage", "1f46b105-b537-4c7f-ba13-4579c7e7d6d2");
            bootstrapper.bootstrap();
        }
    }

    /**
     * Ensure that label is valid for the specified LabelIncrementer
     *
     * @param oldLabel    target label
     * @param incrementer target LabelIncrementer
     * @throws CruiseControlException if label is not valid
     */
    protected void validateLabel(final String oldLabel, final LabelIncrementer incrementer) throws CruiseControlException {
        writeline("/home/ubuntu/results/coverage/Project/Project_8_10.coverage", "e6c8c4b4-aba1-43e7-8124-55252aec21fb");
        if (!incrementer.isValidLabel(oldLabel)) {
            writeline("/home/ubuntu/results/coverage/Project/Project_8_10.coverage", "2e572a79-5a20-4b06-8ac7-186b16e7c1bb");
            final String message = oldLabel + " is not a valid label for labelIncrementer " + incrementer.getClass().getName();
            writeline("/home/ubuntu/results/coverage/Project/Project_8_10.coverage", "29699307-9fbd-4861-ba5e-74f564ebb3f7");
            debug(message);
            writeline("/home/ubuntu/results/coverage/Project/Project_8_10.coverage", "2169c6d4-44ac-4eb1-b3cb-99a249ccaf11");
            throw new CruiseControlException(message);
        }
    }

    public boolean isLastBuildSuccessful() {
        writeline("/home/ubuntu/results/coverage/Project/Project_8_10.coverage", "37b39490-d738-44a8-8ced-a1952272736d");
        return wasLastBuildSuccessful;
    }

    void setWasLastBuildSuccessful(final boolean buildSuccessful) {
        writeline("/home/ubuntu/results/coverage/Project/Project_8_10.coverage", "acf30f83-b1e8-4648-a6bf-d72577dbecbf");
        wasLastBuildSuccessful = buildSuccessful;
    }

    /**
     * Logs a message to the application log, not to be confused with the
     * CruiseControl build log.
     * @param message the application message to log
     */
    private void info(final String message) {
        writeline("/home/ubuntu/results/coverage/Project/Project_8_10.coverage", "6e5b933d-454a-4062-9702-b5d80282c4b2");
        LOG.info("Project " + name + ":  " + message);
    }

    private void debug(final String message) {
        writeline("/home/ubuntu/results/coverage/Project/Project_8_10.coverage", "90eedace-b276-4c25-9add-cb0baff049ac");
        LOG.debug("Project " + name + ":  " + message);
    }

    public void start() {
        writeline("/home/ubuntu/results/coverage/Project/Project_8_10.coverage", "e31b8c06-1610-4394-b850-5bece35332e3");
        if (stopped || getState() == ProjectState.STOPPED) {
            writeline("/home/ubuntu/results/coverage/Project/Project_8_10.coverage", "091567ad-ff08-465d-9bbc-4f7d4c775488");
            stopped = false;
            writeline("/home/ubuntu/results/coverage/Project/Project_8_10.coverage", "cab76861-0714-424e-b85f-41d2bac54d03");
            LOG.info("Project " + name + " starting");
            writeline("/home/ubuntu/results/coverage/Project/Project_8_10.coverage", "d85a8355-3838-43c6-9687-5d9e9c1b6565");
            setState(ProjectState.IDLE);
            writeline("/home/ubuntu/results/coverage/Project/Project_8_10.coverage", "cdbc6735-d589-4f3e-bbcf-0e968e0b44e2");
            createNewSchedulingThread();
        }
    }

    protected void createNewSchedulingThread() {
        writeline("/home/ubuntu/results/coverage/Project/Project_8_10.coverage", "31487488-a986-4a13-9190-20c63067d491");
        final Thread projectSchedulingThread = new Thread(this, "Project " + getName() + " thread");
        writeline("/home/ubuntu/results/coverage/Project/Project_8_10.coverage", "1bc30c39-aa7a-46aa-a923-b27a0b519348");
        projectSchedulingThread.start();
        writeline("/home/ubuntu/results/coverage/Project/Project_8_10.coverage", "2e79e960-1fe8-4cb4-85f3-fe598f2dd23e");
        // brief nap to allow thread to start
        try {
            writeline("/home/ubuntu/results/coverage/Project/Project_8_10.coverage", "f087fa36-3549-4c19-8533-82f5a8b0f44d");
            Thread.sleep(100);
        } catch (InterruptedException ie) {
            writeline("/home/ubuntu/results/coverage/Project/Project_8_10.coverage", "4a32b6ae-5be2-4e42-9e03-ecbda5d808f1");
            LOG.warn("interrupted while waiting for scheduling thread to start", ie);
        }
    }

    public void stop() {
        writeline("/home/ubuntu/results/coverage/Project/Project_8_10.coverage", "76aeec13-fd4f-4b8b-ac4f-c5102428493b");
        LOG.info("Project " + name + " stopping");
        writeline("/home/ubuntu/results/coverage/Project/Project_8_10.coverage", "a15072b9-7674-4fe2-a2e5-d37d8a079b33");
        stopped = true;
        writeline("/home/ubuntu/results/coverage/Project/Project_8_10.coverage", "529719bd-f05d-471f-aa03-1980f7a57088");
        setState(ProjectState.STOPPED);
        writeline("/home/ubuntu/results/coverage/Project/Project_8_10.coverage", "e87d43f3-1d47-4de6-af55-e8fd529ba62e");
        synchronized (pausedMutex) {
            writeline("/home/ubuntu/results/coverage/Project/Project_8_10.coverage", "bbbe49cf-b6f6-49d4-8482-4f4746583f89");
            pausedMutex.notifyAll();
        }
        writeline("/home/ubuntu/results/coverage/Project/Project_8_10.coverage", "51fa55e7-fc9f-469e-b78e-47f07aa748ae");
        synchronized (waitMutex) {
            writeline("/home/ubuntu/results/coverage/Project/Project_8_10.coverage", "77b5f29f-e935-4375-808d-fed52c1e4f9c");
            waitMutex.notifyAll();
        }
        writeline("/home/ubuntu/results/coverage/Project/Project_8_10.coverage", "f76b1fed-b1b1-4a0c-b337-7b9094dc788d");
        synchronized (scheduleMutex) {
            writeline("/home/ubuntu/results/coverage/Project/Project_8_10.coverage", "0dea20f9-da13-4696-8306-012a4b28b86c");
            scheduleMutex.notifyAll();
        }
    }

    public String toString() {
        writeline("/home/ubuntu/results/coverage/Project/Project_8_10.coverage", "3d9994e9-637f-43ad-86a8-4846520cba6d");
        final StringBuilder sb = new StringBuilder("Project ");
        writeline("/home/ubuntu/results/coverage/Project/Project_8_10.coverage", "8e587cea-a178-461f-a827-b98a6a7b275f");
        sb.append(getName());
        writeline("/home/ubuntu/results/coverage/Project/Project_8_10.coverage", "404cd706-a31d-41e0-a9d9-1f74478b7d55");
        sb.append(": ");
        writeline("/home/ubuntu/results/coverage/Project/Project_8_10.coverage", "ecdff77c-8aa8-4a72-9785-5eb4f2f69729");
        sb.append(getStatus());
        writeline("/home/ubuntu/results/coverage/Project/Project_8_10.coverage", "399a2cfd-86b5-4d23-91f1-75616e6cc4f7");
        if (isPaused) {
            writeline("/home/ubuntu/results/coverage/Project/Project_8_10.coverage", "1954fdb1-ce4d-4e32-b937-8adbd3915a6b");
            sb.append(" (paused)");
        }
        writeline("/home/ubuntu/results/coverage/Project/Project_8_10.coverage", "8ae03aa1-6d8d-4278-ba2c-6349150e6541");
        return sb.toString();
    }

    public void addBuildProgressListener(final BuildProgressListener listener) {
        writeline("/home/ubuntu/results/coverage/Project/Project_8_10.coverage", "99fd9a90-39e5-4ecb-abd7-f0fda59342b5");
        synchronized (progressListeners) {
            writeline("/home/ubuntu/results/coverage/Project/Project_8_10.coverage", "75ad7c99-3597-4b19-a27f-0003943e24c7");
            progressListeners.add(listener);
        }
    }

    protected void fireProgressEvent(final BuildProgressEvent event) {
        writeline("/home/ubuntu/results/coverage/Project/Project_8_10.coverage", "59e51218-7f31-4bcd-a577-12c3dc64501d");
        synchronized (progressListeners) {
            writeline("/home/ubuntu/results/coverage/Project/Project_8_10.coverage", "5428693a-8fd7-4721-bd14-1e3adb82272c");
            for (final BuildProgressListener listener : progressListeners) {
                writeline("/home/ubuntu/results/coverage/Project/Project_8_10.coverage", "f3c1f17e-64c9-451a-9eb9-0dd115883b3b");
                listener.handleBuildProgress(event);
            }
        }
    }

    public void addBuildResultListener(final BuildResultListener listener) {
        writeline("/home/ubuntu/results/coverage/Project/Project_8_10.coverage", "39312b1a-ae2b-4f68-bfd9-f5d481a3b3b1");
        synchronized (resultListeners) {
            writeline("/home/ubuntu/results/coverage/Project/Project_8_10.coverage", "e4692c47-80a9-4904-bc03-2bfffbbb8706");
            resultListeners.add(listener);
        }
    }

    protected void fireResultEvent(final BuildResultEvent event) {
        writeline("/home/ubuntu/results/coverage/Project/Project_8_10.coverage", "20136e46-30e2-49ef-b74b-6c9e54d9d21a");
        synchronized (resultListeners) {
            writeline("/home/ubuntu/results/coverage/Project/Project_8_10.coverage", "dc273de8-04f2-4bee-8d11-97f886221ce4");
            for (final BuildResultListener listener : resultListeners) {
                writeline("/home/ubuntu/results/coverage/Project/Project_8_10.coverage", "53e32fce-95b9-47ce-ba87-ab6a28696a88");
                listener.handleBuildResult(event);
            }
        }
    }

    List<Listener> getListeners() {
        writeline("/home/ubuntu/results/coverage/Project/Project_8_10.coverage", "9fc687c6-93bf-4ccb-bd77-8184a412e5b0");
        return projectConfig.getListeners();
    }

    public void setProjectConfig(final ProjectConfig projectConfig) throws CruiseControlException {
        writeline("/home/ubuntu/results/coverage/Project/Project_8_10.coverage", "0e40ede8-0e08-4c75-8cc9-11f7da2ba5d3");
        if (projectConfig == null) {
            writeline("/home/ubuntu/results/coverage/Project/Project_8_10.coverage", "f9bfbea2-93cd-4bd3-9398-ad8faa0a9b82");
            throw new IllegalArgumentException("project config can't be null");
        }
        writeline("/home/ubuntu/results/coverage/Project/Project_8_10.coverage", "2b8acd26-72dc-4050-b9c1-d66e94cd90f9");
        this.projectConfig = projectConfig;
        writeline("/home/ubuntu/results/coverage/Project/Project_8_10.coverage", "cd5ba6da-6dd7-4bb7-b5ab-dec5c8b966c2");
        setLabelIncrementer(projectConfig.getLabelIncrementer());
    }

    void notifyListeners(final ProjectEvent event) {
        writeline("/home/ubuntu/results/coverage/Project/Project_8_10.coverage", "9bb5a689-fa63-47cf-af81-3db2f39f154e");
        if (projectConfig == null) {
            writeline("/home/ubuntu/results/coverage/Project/Project_8_10.coverage", "70e8cce6-e3ce-4d8b-8a28-1d04248a6f2c");
            throw new IllegalStateException("projectConfig is null");
        }
        writeline("/home/ubuntu/results/coverage/Project/Project_8_10.coverage", "684bbce6-3102-4d06-87b6-dcac086ff4e8");
        for (final Listener listener : projectConfig.getListeners()) {
            writeline("/home/ubuntu/results/coverage/Project/Project_8_10.coverage", "563f5d09-53b2-48fd-8ae6-c48eec724eef");
            try {
                writeline("/home/ubuntu/results/coverage/Project/Project_8_10.coverage", "84aa41be-23c2-462c-a67b-bc685ad739d2");
                listener.handleEvent(event);
            } catch (CruiseControlException e) {
                writeline("/home/ubuntu/results/coverage/Project/Project_8_10.coverage", "33507a17-f26c-42d9-a75d-1fd55b19847b");
                final StringBuilder message = new StringBuilder("exception notifying listener ");
                writeline("/home/ubuntu/results/coverage/Project/Project_8_10.coverage", "b617a3d5-0eda-4a2c-8d20-5d809ee89014");
                message.append(listener.getClass().getName());
                writeline("/home/ubuntu/results/coverage/Project/Project_8_10.coverage", "4cb9348b-6231-48fe-aff3-81a623904169");
                message.append(" for project ").append(name);
                writeline("/home/ubuntu/results/coverage/Project/Project_8_10.coverage", "6a326d61-8ba0-4833-8dd2-623494df044f");
                LOG.error(message.toString(), e);
            }
        }
    }

    public boolean equals(final Object arg0) {
        writeline("/home/ubuntu/results/coverage/Project/Project_8_10.coverage", "32c7a845-2860-4c93-9e04-7fa6b9ede85b");
        if (arg0 == null) {
            writeline("/home/ubuntu/results/coverage/Project/Project_8_10.coverage", "feb53621-f441-417c-bc20-7092d9326773");
            return false;
        }
        writeline("/home/ubuntu/results/coverage/Project/Project_8_10.coverage", "cc331b5d-765d-480d-9e24-c5d233886610");
        if (arg0.getClass().getName().equals(getClass().getName())) {
            writeline("/home/ubuntu/results/coverage/Project/Project_8_10.coverage", "ec1b8647-ea07-4ac5-b53b-f24abdbcfd10");
            final Project thatProject = (Project) arg0;
            writeline("/home/ubuntu/results/coverage/Project/Project_8_10.coverage", "e3f632f7-a955-4006-855b-0d8a0507ed5b");
            return thatProject.name.equals(name);
        }
        writeline("/home/ubuntu/results/coverage/Project/Project_8_10.coverage", "a426642b-502e-45cd-bf19-105585cac8d2");
        return false;
    }

    public int hashCode() {
        writeline("/home/ubuntu/results/coverage/Project/Project_8_10.coverage", "1a831268-94d3-40af-883e-fa49b70de1a5");
        return name.hashCode();
    }

    public void register(final MBeanServer server) throws JMException {
        writeline("/home/ubuntu/results/coverage/Project/Project_8_10.coverage", "1a345a3a-a71d-44de-bd5c-38bf4576cfdb");
        LOG.debug("Registering project mbean");
        writeline("/home/ubuntu/results/coverage/Project/Project_8_10.coverage", "73fbeedb-308c-4b68-a468-d211fc2a5f72");
        final ProjectController projectController = new ProjectController(this);
        writeline("/home/ubuntu/results/coverage/Project/Project_8_10.coverage", "c69bb0b1-b935-4056-a72f-edf4675daa50");
        projectController.register(server);
    }

    public ProjectConfig getProjectConfig() {
        writeline("/home/ubuntu/results/coverage/Project/Project_8_10.coverage", "8a346e63-0914-4ff4-b564-495e8c735249");
        return projectConfig;
    }

    public Date getLastBuildDate() {
        writeline("/home/ubuntu/results/coverage/Project/Project_8_10.coverage", "d66b29ec-1399-48cf-a1c9-686df5691cf0");
        return lastBuild;
    }

    public Progress getProgress() {
        writeline("/home/ubuntu/results/coverage/Project/Project_8_10.coverage", "659e10a8-0467-4086-b7c9-65c727030161");
        return progress;
    }

    public List<String> getLogLabels() {
        writeline("/home/ubuntu/results/coverage/Project/Project_8_10.coverage", "0e05cfe5-a661-4634-9726-cdffe3286a9d");
        return projectConfig.getLogLabels();
    }

    public String[] getLogLabelLines(final String logLabel, final int firstLine) {
        writeline("/home/ubuntu/results/coverage/Project/Project_8_10.coverage", "d72da1e6-4e3c-4048-9ebc-4b10915a796c");
        return projectConfig.getLogLabelLines(logLabel, firstLine);
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
