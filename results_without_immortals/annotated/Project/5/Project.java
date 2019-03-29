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
        writeline("/home/ubuntu/results/coverage/Project/Project_5_10.coverage", "fb60f36b-af02-4ee4-8b8e-03c3830f91bc");
        state = ProjectState.STOPPED;
        writeline("/home/ubuntu/results/coverage/Project/Project_5_10.coverage", "85782eae-3abc-4ce1-834a-ca15d1542387");
        pausedMutex = new Object();
        writeline("/home/ubuntu/results/coverage/Project/Project_5_10.coverage", "8d72191e-840b-4bd0-a88b-7a26482b32a9");
        scheduleMutex = new Object();
        writeline("/home/ubuntu/results/coverage/Project/Project_5_10.coverage", "83c9b49f-b398-4df6-be64-5a24327bf26f");
        waitMutex = new Object();
        writeline("/home/ubuntu/results/coverage/Project/Project_5_10.coverage", "ac37ad2d-d371-4967-a8f6-ba32fce2e8b0");
        progressListeners = new ArrayList<BuildProgressListener>();
        writeline("/home/ubuntu/results/coverage/Project/Project_5_10.coverage", "1987fc36-3bc3-44ae-abd4-2e042b86b414");
        resultListeners = new ArrayList<BuildResultListener>();
        writeline("/home/ubuntu/results/coverage/Project/Project_5_10.coverage", "041792f9-7b0d-463b-93d8-e5001d8f4c1f");
        progress = new ProgressImpl(this);
    }

    private void readObject(final ObjectInputStream stream) throws IOException, ClassNotFoundException {
        writeline("/home/ubuntu/results/coverage/Project/Project_5_10.coverage", "2037abf5-6f12-4649-8e87-18d53e42f614");
        stream.defaultReadObject();
        writeline("/home/ubuntu/results/coverage/Project/Project_5_10.coverage", "13143852-02eb-4119-9b86-1f34cbc23167");
        initializeTransientFields();
    }

    public void execute() {
        writeline("/home/ubuntu/results/coverage/Project/Project_5_10.coverage", "d7d90900-27f2-4615-901b-d50dac15c147");
        if (stopped) {
            writeline("/home/ubuntu/results/coverage/Project/Project_5_10.coverage", "53680206-36a9-475f-843b-ed0b04ab485d");
            LOG.warn("not building project " + name + " because project has been stopped.");
            writeline("/home/ubuntu/results/coverage/Project/Project_5_10.coverage", "2fbf7d8a-9b02-4249-8988-2c4421881959");
            buildFinished();
            writeline("/home/ubuntu/results/coverage/Project/Project_5_10.coverage", "803fc4a4-cf13-4337-bc01-e9d1f6e0558a");
            return;
        }
        writeline("/home/ubuntu/results/coverage/Project/Project_5_10.coverage", "799f159d-234b-4747-ae4a-4dc59476fbbe");
        synchronized (pausedMutex) {
            writeline("/home/ubuntu/results/coverage/Project/Project_5_10.coverage", "d75291e7-e1ba-467c-a355-4d4c5d5c5dc9");
            if (isPaused) {
                writeline("/home/ubuntu/results/coverage/Project/Project_5_10.coverage", "91272470-3f57-47e5-ba37-3759faf28f65");
                LOG.info("not building project " + name + " because project has been paused.");
                writeline("/home/ubuntu/results/coverage/Project/Project_5_10.coverage", "e2ad7617-a163-46a8-aaab-a3919b048bd6");
                buildFinished();
                writeline("/home/ubuntu/results/coverage/Project/Project_5_10.coverage", "54d0322f-ef52-4466-b433-f7f6c338a402");
                return;
            }
        }
        writeline("/home/ubuntu/results/coverage/Project/Project_5_10.coverage", "4dc66c5f-188b-48fc-a8e5-9e70e720f7fd");
        try {
            writeline("/home/ubuntu/results/coverage/Project/Project_5_10.coverage", "124c4f4f-a182-4fae-92b1-ce9153593cc7");
            init();
            writeline("/home/ubuntu/results/coverage/Project/Project_5_10.coverage", "173f1a59-542a-4a54-b07e-9364450c963b");
            build();
        } catch (CruiseControlException e) {
            writeline("/home/ubuntu/results/coverage/Project/Project_5_10.coverage", "0b3eac7c-9f10-4a40-b256-8dd8ffb79672");
            LOG.error("exception attempting build in project " + name, e);
        } finally {
            writeline("/home/ubuntu/results/coverage/Project/Project_5_10.coverage", "a96e150c-cef2-437c-af69-43e86333eac6");
            buildFinished();
        }
    }

    /**
     * Unless paused, runs any bootstrappers and then the entire build.
     * @throws CruiseControlException if an error occurs during the build
     */
    protected void build() throws CruiseControlException {
        writeline("/home/ubuntu/results/coverage/Project/Project_5_10.coverage", "bbbbb115-f6e1-4b6f-8ce9-1478a0bbcd40");
        if (projectConfig == null) {
            writeline("/home/ubuntu/results/coverage/Project/Project_5_10.coverage", "537303d4-de17-43ef-8f9e-ac15bf24e7bb");
            throw new IllegalStateException("projectConfig must be set on project before calling build()");
        }
        writeline("/home/ubuntu/results/coverage/Project/Project_5_10.coverage", "72e3ab10-5b6b-4b02-8ba9-d3a9fae27498");
        if (stopped) {
            writeline("/home/ubuntu/results/coverage/Project/Project_5_10.coverage", "fd4948e7-fbf6-46bf-947a-7fcb6ec05937");
            LOG.warn("not building project " + name + " because project has been stopped.");
            writeline("/home/ubuntu/results/coverage/Project/Project_5_10.coverage", "eecf3d8c-0bb8-4912-affd-32d1ef56a259");
            return;
        }
        writeline("/home/ubuntu/results/coverage/Project/Project_5_10.coverage", "06b53943-9b3e-4643-be86-2ff8ac947964");
        // or if the last build faild and we want to build on failures
        if (forceOnly && !buildForced && !(!wasLastBuildSuccessful && buildAfterFailed)) {
            writeline("/home/ubuntu/results/coverage/Project/Project_5_10.coverage", "af96f075-b1db-4adc-bc74-1156da4f224a");
            info("not building because project is forceOnly and build not forced.");
            writeline("/home/ubuntu/results/coverage/Project/Project_5_10.coverage", "f305e4cd-6e4a-4855-8dea-cc1a09a0ba31");
            return;
        }
        writeline("/home/ubuntu/results/coverage/Project/Project_5_10.coverage", "6f73b738-dd5a-456b-9228-6a2ac3a55005");
        final boolean buildWasForced = buildForced;
        writeline("/home/ubuntu/results/coverage/Project/Project_5_10.coverage", "6bcea5e1-47c4-4eae-a8dc-63d2e7c26d72");
        try {
            writeline("/home/ubuntu/results/coverage/Project/Project_5_10.coverage", "b9eaee6b-b2ee-4add-8d63-a465613534e1");
            setBuildStartTime(new Date());
            writeline("/home/ubuntu/results/coverage/Project/Project_5_10.coverage", "36cdeaa7-4069-42d2-bd8f-02029a2f1a5b");
            final Schedule schedule = projectConfig.getSchedule();
            writeline("/home/ubuntu/results/coverage/Project/Project_5_10.coverage", "8f0ae4b6-83c1-4e99-9ff2-f839287567e5");
            if (schedule == null) {
                writeline("/home/ubuntu/results/coverage/Project/Project_5_10.coverage", "a6fe9ae3-fde9-4680-bad9-bbb77f993f23");
                throw new IllegalStateException("project must have a schedule");
            }
            writeline("/home/ubuntu/results/coverage/Project/Project_5_10.coverage", "ec53eb50-b516-444c-8d9e-ea104f0a61b3");
            if (schedule.isPaused(buildStartTime)) {
                writeline("/home/ubuntu/results/coverage/Project/Project_5_10.coverage", "a782faef-81a8-469c-8bdb-2f93cfd76a96");
                // is different than ProjectState.PAUSED
                return;
            }
            writeline("/home/ubuntu/results/coverage/Project/Project_5_10.coverage", "401d5485-0ae3-4e31-b649-d3a49659669d");
            // @todo Add Progress param to Bootstrapper API?
            bootstrap();
            writeline("/home/ubuntu/results/coverage/Project/Project_5_10.coverage", "2e86b316-b696-4096-bdd0-4b60ca90afee");
            final String target = useAndResetBuildTargetIfBuildWasForced(buildWasForced);
            writeline("/home/ubuntu/results/coverage/Project/Project_5_10.coverage", "671d54f2-4a6d-4d62-a839-9a9c980e3537");
            // @todo Add Progress param to ModificationSet API?
            // getModifications will only return null if we don't need to build
            final Element modifications = getModifications(buildWasForced);
            writeline("/home/ubuntu/results/coverage/Project/Project_5_10.coverage", "28eba80d-40c0-4851-aa3c-830454eeb492");
            if (modifications == null) {
                writeline("/home/ubuntu/results/coverage/Project/Project_5_10.coverage", "8e12a1a3-7e73-4822-8369-05d3edb80bbd");
                return;
            }
            writeline("/home/ubuntu/results/coverage/Project/Project_5_10.coverage", "b42d1846-4e21-424b-9056-9e428d6cc503");
            // Using local reference to avoid NPE if config.xml is updated during build
            final Log buildLog = projectConfig.getLog();
            writeline("/home/ubuntu/results/coverage/Project/Project_5_10.coverage", "7a218318-43bb-441b-aeb4-789646cf20e3");
            buildLog.addContent(modifications);
            writeline("/home/ubuntu/results/coverage/Project/Project_5_10.coverage", "12047e50-84e0-4a7a-9f88-6391bd5b2b71");
            final Date now;
            writeline("/home/ubuntu/results/coverage/Project/Project_5_10.coverage", "099630c9-21c8-4e5e-ba13-87bbe3fffc35");
            if (projectConfig.getModificationSet() != null && projectConfig.getModificationSet().getTimeOfCheck() != null) {
                writeline("/home/ubuntu/results/coverage/Project/Project_5_10.coverage", "6f930d2b-a77f-48c7-83ce-e8bcf15c6a67");
                now = projectConfig.getModificationSet().getTimeOfCheck();
            } else {
                writeline("/home/ubuntu/results/coverage/Project/Project_5_10.coverage", "a7c41e23-4158-4f54-a68d-4748c2b755b3");
                now = new Date();
            }
            writeline("/home/ubuntu/results/coverage/Project/Project_5_10.coverage", "a6231916-aa18-44ab-bfbb-cd112dfe2dca");
            if (getLabelIncrementer().isPreBuildIncrementer()) {
                writeline("/home/ubuntu/results/coverage/Project/Project_5_10.coverage", "650f043f-8a7b-479e-8259-34d6b66f548c");
                label = getLabelIncrementer().incrementLabel(label, buildLog.getContent());
            }
            writeline("/home/ubuntu/results/coverage/Project/Project_5_10.coverage", "145a2725-dae9-4398-844b-aa18086c8ddc");
            // collect project information
            buildLog.addContent(getProjectPropertiesElement(now));
            writeline("/home/ubuntu/results/coverage/Project/Project_5_10.coverage", "00a9319c-058c-45fa-8e5e-68c68b63ef2f");
            setState(ProjectState.BUILDING);
            writeline("/home/ubuntu/results/coverage/Project/Project_5_10.coverage", "a1c5c5db-2631-4066-90ae-5f3a196fc94b");
            final Element builderLog = schedule.build(buildCounter, lastBuild, now, getProjectPropertiesMap(now), target, progress);
            writeline("/home/ubuntu/results/coverage/Project/Project_5_10.coverage", "224a1434-2e8a-48a5-9b31-a64df8f108a7");
            buildLog.addContent(builderLog.detach());
            writeline("/home/ubuntu/results/coverage/Project/Project_5_10.coverage", "156684ea-5d43-4611-a2a8-6628fa3cb7d3");
            boolean buildSuccessful = buildLog.wasBuildSuccessful();
            writeline("/home/ubuntu/results/coverage/Project/Project_5_10.coverage", "cc4e3f41-5e0a-4b3e-b891-4fa28b1c8434");
            fireResultEvent(new BuildResultEvent(this, buildSuccessful));
            writeline("/home/ubuntu/results/coverage/Project/Project_5_10.coverage", "4a5d029b-21f6-4281-b149-d85a2c1136d8");
            if (!getLabelIncrementer().isPreBuildIncrementer() && buildSuccessful) {
                writeline("/home/ubuntu/results/coverage/Project/Project_5_10.coverage", "ceeb58e1-2a26-4835-b780-54589db0170a");
                label = getLabelIncrementer().incrementLabel(label, buildLog.getContent());
            }
            writeline("/home/ubuntu/results/coverage/Project/Project_5_10.coverage", "38ebbf25-af85-4c0b-a0f7-5bb7d58f44aa");
            setState(ProjectState.MERGING_LOGS);
            writeline("/home/ubuntu/results/coverage/Project/Project_5_10.coverage", "df2d78a4-91ee-4eaa-9951-66b8f23f871f");
            buildLog.writeLogFile(now);
            writeline("/home/ubuntu/results/coverage/Project/Project_5_10.coverage", "32f8410b-b023-4c03-b15b-2fd0121ed3f8");
            // regardless of success or failure (buildAfterFailed = false in config.xml)
            if (!buildAfterFailed) {
                writeline("/home/ubuntu/results/coverage/Project/Project_5_10.coverage", "0415cd25-89b9-443b-90d0-82694e82ba26");
                lastBuild = now;
            }
            writeline("/home/ubuntu/results/coverage/Project/Project_5_10.coverage", "f6505dad-fa37-4f1a-8c77-48d03fc62eee");
            // If this was a successful build, update both last build and last successful build
            if (buildSuccessful) {
                writeline("/home/ubuntu/results/coverage/Project/Project_5_10.coverage", "215cac21-2c95-458c-9dda-18bcd42c7abb");
                lastBuild = now;
                writeline("/home/ubuntu/results/coverage/Project/Project_5_10.coverage", "a8df20da-9588-4774-a35e-5179259c3379");
                lastSuccessfulBuild = now;
                writeline("/home/ubuntu/results/coverage/Project/Project_5_10.coverage", "3d713fc7-f0e5-408a-a305-330cc3fdce88");
                info("build successful");
            } else {
                writeline("/home/ubuntu/results/coverage/Project/Project_5_10.coverage", "eea9e387-abec-43b5-a69b-9f2a97660805");
                info("build failed");
            }
            writeline("/home/ubuntu/results/coverage/Project/Project_5_10.coverage", "50a87006-6928-4596-8809-3fe238dac19a");
            buildCounter++;
            writeline("/home/ubuntu/results/coverage/Project/Project_5_10.coverage", "17f8977a-b5b9-418a-b08a-15505de33729");
            setWasLastBuildSuccessful(buildSuccessful);
            writeline("/home/ubuntu/results/coverage/Project/Project_5_10.coverage", "1ddfe24a-ef82-4d64-b121-203e40e5f019");
            // also need to reset forced flag before serializing, unless buildForced var is transient
            // resetBuildForcedOnlyIfBuildWasForced(buildWasForced);
            serializeProject();
            writeline("/home/ubuntu/results/coverage/Project/Project_5_10.coverage", "2b29613e-b4ca-4b22-9200-5b4ac7010a09");
            // @todo Add Progress param to Publisher API?
            publish(buildLog);
            writeline("/home/ubuntu/results/coverage/Project/Project_5_10.coverage", "924fff9d-272d-4336-b6cb-5319dbf5234d");
            buildLog.reset();
        } finally {
            writeline("/home/ubuntu/results/coverage/Project/Project_5_10.coverage", "6b3cc61a-cd3e-4d27-9017-f0908c1f6668");
            resetBuildForcedOnlyIfBuildWasForced(buildWasForced);
            writeline("/home/ubuntu/results/coverage/Project/Project_5_10.coverage", "14b86a02-584f-4519-a82a-9f581e7d4bc9");
            setState(ProjectState.IDLE);
        }
    }

    private String useAndResetBuildTargetIfBuildWasForced(final boolean buildWasForced) {
        writeline("/home/ubuntu/results/coverage/Project/Project_5_10.coverage", "9b91c57e-29cd-4d6d-b5d1-05502cd831a1");
        String target = null;
        writeline("/home/ubuntu/results/coverage/Project/Project_5_10.coverage", "0b6ad33f-ac78-4307-8df1-2eb88c896ee5");
        if (buildWasForced) {
            writeline("/home/ubuntu/results/coverage/Project/Project_5_10.coverage", "b6053dcd-fc23-4812-a88e-99379167d560");
            target = buildTarget;
            writeline("/home/ubuntu/results/coverage/Project/Project_5_10.coverage", "1af9ecee-c930-4e4f-9f63-4ac6828dda3d");
            buildTarget = null;
        }
        writeline("/home/ubuntu/results/coverage/Project/Project_5_10.coverage", "b07ad578-df3d-43c3-859b-7ef7eb5dc226");
        return target;
    }

    private void resetBuildForcedOnlyIfBuildWasForced(final boolean buildWasForced) {
        writeline("/home/ubuntu/results/coverage/Project/Project_5_10.coverage", "1ca16dbc-e479-4a11-99f3-f868b774527d");
        if (buildWasForced) {
            writeline("/home/ubuntu/results/coverage/Project/Project_5_10.coverage", "19e7252a-ebe9-4a29-85a5-1e43134b83af");
            buildForced = false;
        }
    }

    void setBuildStartTime(final Date date) {
        writeline("/home/ubuntu/results/coverage/Project/Project_5_10.coverage", "3c712f46-f384-4de0-b0ed-ca39e5d912b4");
        buildStartTime = date;
    }

    public void run() {
        writeline("/home/ubuntu/results/coverage/Project/Project_5_10.coverage", "98b24bf8-c644-4b11-a9bf-49a89f087d6c");
        LOG.info("Project " + name + " started");
        writeline("/home/ubuntu/results/coverage/Project/Project_5_10.coverage", "cd7209bc-2626-46c0-a2f7-c9b5ba6e9d48");
        try {
            writeline("/home/ubuntu/results/coverage/Project/Project_5_10.coverage", "c47921cc-7d85-4dd3-9df9-4c16c181462a");
            while (!stopped) {
                writeline("/home/ubuntu/results/coverage/Project/Project_5_10.coverage", "9fe8cd68-4478-452a-8a61-11416630bb89");
                try {
                    writeline("/home/ubuntu/results/coverage/Project/Project_5_10.coverage", "2522f3c7-af31-4437-bbf2-c56fdfb34e25");
                    waitIfPaused();
                    writeline("/home/ubuntu/results/coverage/Project/Project_5_10.coverage", "70dfa4ee-0269-4165-a0e9-7411ac01d91a");
                    if (!stopped) {
                        writeline("/home/ubuntu/results/coverage/Project/Project_5_10.coverage", "253ec595-39f4-4c61-96de-dd8d0ce446d0");
                        waitForNextBuild();
                    }
                    writeline("/home/ubuntu/results/coverage/Project/Project_5_10.coverage", "fec8a8ed-ad0f-4d17-be79-31b0c506922b");
                    if (!stopped) {
                        writeline("/home/ubuntu/results/coverage/Project/Project_5_10.coverage", "ee8ff105-692e-415c-8c0c-fa8c8fcfb3d6");
                        setState(ProjectState.QUEUED);
                        writeline("/home/ubuntu/results/coverage/Project/Project_5_10.coverage", "5ac3e3e9-65e5-47ed-ad88-3aac3044a012");
                        synchronized (scheduleMutex) {
                            writeline("/home/ubuntu/results/coverage/Project/Project_5_10.coverage", "038f7596-c597-4252-add0-9e3cb81b5b07");
                            queue.requestBuild(projectConfig);
                            writeline("/home/ubuntu/results/coverage/Project/Project_5_10.coverage", "8cda3268-fcc4-47f4-88b1-bdce41386097");
                            waitForBuildToFinish();
                        }
                    }
                } catch (InterruptedException e) {
                    writeline("/home/ubuntu/results/coverage/Project/Project_5_10.coverage", "12ad9793-5049-40f2-8675-ac6792472ab5");
                    final String message = "Project " + name + ".run() interrupted";
                    writeline("/home/ubuntu/results/coverage/Project/Project_5_10.coverage", "a2608173-26af-42d7-adcb-6ef7578cd11b");
                    LOG.error(message, e);
                    writeline("/home/ubuntu/results/coverage/Project/Project_5_10.coverage", "3e43cd35-79cd-461f-a125-bf4c2aa375e8");
                    throw new RuntimeException(message);
                }
            }
        } finally {
            writeline("/home/ubuntu/results/coverage/Project/Project_5_10.coverage", "723e8523-3ad0-4c97-8140-50bdc269fb7f");
            stopped = true;
            writeline("/home/ubuntu/results/coverage/Project/Project_5_10.coverage", "fcc5d92a-61bf-4112-bfd7-540228e8fe4b");
            LOG.info("Project " + name + " stopped");
        }
    }

    void waitIfPaused() throws InterruptedException {
        writeline("/home/ubuntu/results/coverage/Project/Project_5_10.coverage", "04f551d8-97d8-4b73-9b92-f9488f0776ff");
        synchronized (pausedMutex) {
            writeline("/home/ubuntu/results/coverage/Project/Project_5_10.coverage", "a8264f55-efb0-4476-9cf9-7b81ce094431");
            while (isPaused) {
                writeline("/home/ubuntu/results/coverage/Project/Project_5_10.coverage", "526543ef-26c8-434d-b90d-46abe62efceb");
                setState(ProjectState.PAUSED);
                writeline("/home/ubuntu/results/coverage/Project/Project_5_10.coverage", "0d363cff-8466-4a03-a3e7-35beba521eda");
                pausedMutex.wait(10 * DateUtil.ONE_MINUTE);
            }
        }
    }

    void waitForNextBuild() throws InterruptedException {
        writeline("/home/ubuntu/results/coverage/Project/Project_5_10.coverage", "bb65fea9-e97c-4df3-abfe-823aa2f733ac");
        long waitTime = getTimeToNextBuild(new Date());
        writeline("/home/ubuntu/results/coverage/Project/Project_5_10.coverage", "3c9fb49a-bed6-4630-821a-d47cf21c258a");
        if (needToWaitForNextBuild(waitTime) && !buildForced) {
            writeline("/home/ubuntu/results/coverage/Project/Project_5_10.coverage", "6ba45acb-2c8d-40e7-b01b-aca5c33b6287");
            final String msg = "next build in " + DateUtil.formatTime(waitTime);
            writeline("/home/ubuntu/results/coverage/Project/Project_5_10.coverage", "92a94c26-9250-4adc-8282-fbef3c1366cf");
            info(msg);
            writeline("/home/ubuntu/results/coverage/Project/Project_5_10.coverage", "3be11d27-7eaf-44bb-bee9-c3e41591e0e5");
            synchronized (waitMutex) {
                writeline("/home/ubuntu/results/coverage/Project/Project_5_10.coverage", "23183d6b-20be-4fd7-b04a-a389f1841245");
                setState(ProjectState.WAITING);
                writeline("/home/ubuntu/results/coverage/Project/Project_5_10.coverage", "79651659-a47f-4b7b-9173-78e069145c75");
                progress.setValue(msg);
                writeline("/home/ubuntu/results/coverage/Project/Project_5_10.coverage", "07a245a0-cdaa-4bb3-86fd-4e56a6988d19");
                waitMutex.wait(waitTime);
            }
        }
    }

    long getTimeToNextBuild(Date now) {
        writeline("/home/ubuntu/results/coverage/Project/Project_5_10.coverage", "9cf959ae-8a98-4562-b033-ceb95d2a1a0b");
        long waitTime = projectConfig.getSchedule().getTimeToNextBuild(now, getBuildInterval());
        writeline("/home/ubuntu/results/coverage/Project/Project_5_10.coverage", "09eceeb0-1f62-44a1-b18c-10a2fd3b2d78");
        if (waitTime == 0) {
            writeline("/home/ubuntu/results/coverage/Project/Project_5_10.coverage", "6632649d-4e9a-4098-adb5-bb3bfe0ff003");
            // project that has just built within a minute time
            if (buildStartTime != null) {
                writeline("/home/ubuntu/results/coverage/Project/Project_5_10.coverage", "ba2ded8d-de11-43a1-bd74-f2de6e2b60e5");
                long millisSinceLastBuild = now.getTime() - buildStartTime.getTime();
                writeline("/home/ubuntu/results/coverage/Project/Project_5_10.coverage", "6da6224c-876d-4292-8089-f9f637730d03");
                if (millisSinceLastBuild < Schedule.ONE_MINUTE) {
                    writeline("/home/ubuntu/results/coverage/Project/Project_5_10.coverage", "2f7f42be-f768-49fb-814f-fd385c686b5b");
                    debug("build finished within a minute, getting new time to next build");
                    writeline("/home/ubuntu/results/coverage/Project/Project_5_10.coverage", "ca1ab692-8a0f-4e60-82c3-7e379b04905a");
                    Date oneMinuteInFuture = new Date(now.getTime() + Schedule.ONE_MINUTE);
                    writeline("/home/ubuntu/results/coverage/Project/Project_5_10.coverage", "05954511-7ec0-4dc6-b432-7e1de8061e34");
                    waitTime = projectConfig.getSchedule().getTimeToNextBuild(oneMinuteInFuture, getBuildInterval());
                    writeline("/home/ubuntu/results/coverage/Project/Project_5_10.coverage", "74d63b8f-d36b-456a-8265-b341c3f4a814");
                    waitTime += Schedule.ONE_MINUTE;
                }
            }
        }
        writeline("/home/ubuntu/results/coverage/Project/Project_5_10.coverage", "c3243c59-9089-4405-bcab-e4ac69641afd");
        return waitTime;
    }

    static boolean needToWaitForNextBuild(long waitTime) {
        writelineStatic("/home/ubuntu/results/coverage/Project/Project_5_10.coverage", "a6b829d8-c759-48d4-9f43-c73f410f641e");
        return waitTime > 0;
    }

    /**
     * @return true if build was forced, intended for unit testing only.
     */
    boolean isBuildForced() {
        writeline("/home/ubuntu/results/coverage/Project/Project_5_10.coverage", "ef192816-bd58-4fd3-972b-5e51eef28802");
        return buildForced;
    }

    void forceBuild() {
        writeline("/home/ubuntu/results/coverage/Project/Project_5_10.coverage", "4c0660ba-3ea0-4ead-adad-9fe9444f2bbe");
        synchronized (waitMutex) {
            writeline("/home/ubuntu/results/coverage/Project/Project_5_10.coverage", "c4933584-9d17-4c5e-8f69-41f026949521");
            waitMutex.notify();
        }
    }

    public void forceBuildWithTarget(String buildTarget) {
        writeline("/home/ubuntu/results/coverage/Project/Project_5_10.coverage", "2849b337-626b-4ca3-96da-bd9c02a67ac3");
        this.buildTarget = buildTarget;
        writeline("/home/ubuntu/results/coverage/Project/Project_5_10.coverage", "1b669cb2-c224-4221-b597-d260de816083");
        setBuildForced(true);
    }

    public void forceBuildWithTarget(String buildTarget, Map<String, String> addedProperties) {
        writeline("/home/ubuntu/results/coverage/Project/Project_5_10.coverage", "cc1545c2-1961-42e6-9f35-f1c26e0d9147");
        additionalProperties = addedProperties;
        writeline("/home/ubuntu/results/coverage/Project/Project_5_10.coverage", "b2bf15ff-ae18-4f4b-aae4-a79cc2e30c99");
        forceBuildWithTarget(buildTarget);
    }

    void waitForBuildToFinish() throws InterruptedException {
        writeline("/home/ubuntu/results/coverage/Project/Project_5_10.coverage", "8ef18ded-b997-41e3-a508-6eca37e3008e");
        synchronized (scheduleMutex) {
            writeline("/home/ubuntu/results/coverage/Project/Project_5_10.coverage", "e26cb566-53d1-4cf7-97f2-7872bbb73c4e");
            debug("waiting for build to finish");
            writeline("/home/ubuntu/results/coverage/Project/Project_5_10.coverage", "99218c29-0f3c-4cd0-9c43-cf7e4eece9d9");
            scheduleMutex.wait();
        }
    }

    void buildFinished() {
        writeline("/home/ubuntu/results/coverage/Project/Project_5_10.coverage", "69c1c399-142d-4577-9acb-c7f4cbd6a9ac");
        synchronized (scheduleMutex) {
            writeline("/home/ubuntu/results/coverage/Project/Project_5_10.coverage", "d6e2f21e-6dd2-4be2-a327-27905030033e");
            debug("build finished");
            writeline("/home/ubuntu/results/coverage/Project/Project_5_10.coverage", "43db0162-92ef-4896-938a-b196ad1ed420");
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
        writeline("/home/ubuntu/results/coverage/Project/Project_5_10.coverage", "6088611a-2a2a-4e92-916b-946d6b590b70");
        setState(ProjectState.MODIFICATIONSET);
        writeline("/home/ubuntu/results/coverage/Project/Project_5_10.coverage", "c45fa567-1f6f-497c-b24a-21847a1d1a64");
        final ModificationSet modificationSet = projectConfig.getModificationSet();
        writeline("/home/ubuntu/results/coverage/Project/Project_5_10.coverage", "2b11a52e-77f3-480e-a641-7f6385589224");
        if (modificationSet == null) {
            writeline("/home/ubuntu/results/coverage/Project/Project_5_10.coverage", "bbb9a691-dea9-4e35-89e2-d92ead8c59cf");
            debug("no modification set, nothing to detect.");
            writeline("/home/ubuntu/results/coverage/Project/Project_5_10.coverage", "2e4cc200-9d30-4e29-ac67-727232dff602");
            if (buildWasForced) {
                writeline("/home/ubuntu/results/coverage/Project/Project_5_10.coverage", "6b82b328-2f9f-49f3-9c62-cd31dda93620");
                info("no modification set but build was forced");
                writeline("/home/ubuntu/results/coverage/Project/Project_5_10.coverage", "5fede6c8-9895-4a25-a353-eb376c81f7e5");
                return new Element("modifications");
            }
            writeline("/home/ubuntu/results/coverage/Project/Project_5_10.coverage", "91543299-03d4-4bf0-9e00-6fecedccbce9");
            if (!requiremodification) {
                writeline("/home/ubuntu/results/coverage/Project/Project_5_10.coverage", "e1199606-2a20-44a0-b43d-98d004003272");
                info("no modification set but no modifications required");
                writeline("/home/ubuntu/results/coverage/Project/Project_5_10.coverage", "b1a69c17-9b4f-4713-aa5b-b183f667fc45");
                return new Element("modifications");
            }
            writeline("/home/ubuntu/results/coverage/Project/Project_5_10.coverage", "6ac461c2-3081-43a2-a26c-403b5bbcbfae");
            return null;
        }
        writeline("/home/ubuntu/results/coverage/Project/Project_5_10.coverage", "5d78319d-7eca-4a6d-a91b-e4176e72a380");
        final boolean checkNewChangesFirst = checkOnlySinceLastBuild();
        writeline("/home/ubuntu/results/coverage/Project/Project_5_10.coverage", "3fa5c761-53f7-4257-9798-2ec46192bf17");
        Element modifications;
        writeline("/home/ubuntu/results/coverage/Project/Project_5_10.coverage", "67f0e9b8-4ef9-416c-a2cf-7f70901e5dcb");
        if (checkNewChangesFirst) {
            writeline("/home/ubuntu/results/coverage/Project/Project_5_10.coverage", "d77de82e-d579-488d-b87e-22f46afa017e");
            debug("getting changes since last build");
            writeline("/home/ubuntu/results/coverage/Project/Project_5_10.coverage", "cee9ea01-dc38-4999-92ae-e8ce8e74074e");
            modifications = modificationSet.retrieveModificationsAsElement(lastBuild, progress);
        } else {
            writeline("/home/ubuntu/results/coverage/Project/Project_5_10.coverage", "e480519b-f57e-4c7b-be6e-a4dc65d816e3");
            debug("getting changes since last successful build");
            writeline("/home/ubuntu/results/coverage/Project/Project_5_10.coverage", "a67c257c-8ad2-4d8f-8b18-88e0d1e0351f");
            modifications = modificationSet.retrieveModificationsAsElement(lastSuccessfulBuild, progress);
        }
        writeline("/home/ubuntu/results/coverage/Project/Project_5_10.coverage", "1d147371-9214-4494-849b-81654270c39a");
        if (!modificationSet.isModified()) {
            writeline("/home/ubuntu/results/coverage/Project/Project_5_10.coverage", "89003c18-e966-4306-8015-39c991f35b1c");
            info("No modifications found, build not necessary.");
            writeline("/home/ubuntu/results/coverage/Project/Project_5_10.coverage", "815536c5-12e3-4672-9179-c2d7b625b9ad");
            // * build forced
            if (buildAfterFailed && !wasLastBuildSuccessful) {
                writeline("/home/ubuntu/results/coverage/Project/Project_5_10.coverage", "0f896e69-cc89-4197-a4a3-96f2cb8ec49d");
                info("Building anyway, since buildAfterFailed is true and last build failed.");
            } else if (!requiremodification) {
                writeline("/home/ubuntu/results/coverage/Project/Project_5_10.coverage", "fa92dd70-f60f-4e85-8194-99e869c3635d");
                info("Building anyway, since modifications not required");
            } else {
                writeline("/home/ubuntu/results/coverage/Project/Project_5_10.coverage", "2430fad1-d885-4d87-baac-6a1913b4acf2");
                if (buildWasForced) {
                    writeline("/home/ubuntu/results/coverage/Project/Project_5_10.coverage", "201394a6-3f2e-4907-a1b1-a7ef7375acdd");
                    info("Building anyway, since build was explicitly forced.");
                } else {
                    writeline("/home/ubuntu/results/coverage/Project/Project_5_10.coverage", "21d3a9ae-3a92-4208-b22e-1c4f383e02f6");
                    return null;
                }
            }
        }
        writeline("/home/ubuntu/results/coverage/Project/Project_5_10.coverage", "83bd5518-f849-4739-b7b0-dbf954be7756");
        if (checkNewChangesFirst) {
            writeline("/home/ubuntu/results/coverage/Project/Project_5_10.coverage", "e29604eb-c67c-45e6-a898-6b430a87e58e");
            debug("new changes found; now getting complete set");
            writeline("/home/ubuntu/results/coverage/Project/Project_5_10.coverage", "14162ba2-9ef1-4e8e-af96-e1f670378a26");
            modifications = modificationSet.retrieveModificationsAsElement(lastSuccessfulBuild, progress);
        }
        writeline("/home/ubuntu/results/coverage/Project/Project_5_10.coverage", "cbb23bc8-ebac-438f-8fb5-03606985077f");
        return modifications;
    }

    /**
     * @return boolean
     */
    boolean checkOnlySinceLastBuild() {
        writeline("/home/ubuntu/results/coverage/Project/Project_5_10.coverage", "68396d5c-9ed2-4c95-bd24-2ee43e167b02");
        if (lastBuild == null || lastSuccessfulBuild == null) {
            writeline("/home/ubuntu/results/coverage/Project/Project_5_10.coverage", "0940d949-8b8c-4e5f-a611-7d3a456c3ac7");
            return false;
        }
        writeline("/home/ubuntu/results/coverage/Project/Project_5_10.coverage", "d7e757c3-8978-4b2c-8798-b5c37f8ac5b1");
        final long lastBuildLong = lastBuild.getTime();
        writeline("/home/ubuntu/results/coverage/Project/Project_5_10.coverage", "785611ab-b0fd-4f9a-9795-6ebfaee7ed37");
        final long timeDifference = lastBuildLong - lastSuccessfulBuild.getTime();
        writeline("/home/ubuntu/results/coverage/Project/Project_5_10.coverage", "f1eb8e00-c8d9-4982-9cb0-8e7e8428dc2a");
        final boolean moreThanASecond = timeDifference > DateUtil.ONE_SECOND;
        writeline("/home/ubuntu/results/coverage/Project/Project_5_10.coverage", "8f2adef5-4a0b-4c76-ad90-9fd6fd23d9e0");
        return !buildAfterFailed && moreThanASecond;
    }

    /**
     * Serialize the project to allow resumption after a process bounce
     */
    public void serializeProject() {
        writeline("/home/ubuntu/results/coverage/Project/Project_5_10.coverage", "b3abdd58-62a9-4f03-ad0c-4cf7e77ea5a8");
        final String safeProjectName = Builder.getFileSystemSafeProjectName(name);
        writeline("/home/ubuntu/results/coverage/Project/Project_5_10.coverage", "77403bbc-ac18-482d-8375-4f5beebe7442");
        try {
            writeline("/home/ubuntu/results/coverage/Project/Project_5_10.coverage", "72209026-1e7e-4f7d-b399-e51ce8664318");
            final ObjectOutputStream s = new ObjectOutputStream(new FileOutputStream(safeProjectName + ".ser"));
            writeline("/home/ubuntu/results/coverage/Project/Project_5_10.coverage", "4fa4a36f-37b5-41c0-bf16-d32bda5a6d11");
            try {
                writeline("/home/ubuntu/results/coverage/Project/Project_5_10.coverage", "16929c00-914c-4894-be62-cba711b9cec6");
                s.writeObject(this);
                writeline("/home/ubuntu/results/coverage/Project/Project_5_10.coverage", "0feadbbf-f7f4-4379-a050-388b63e26659");
                s.flush();
                writeline("/home/ubuntu/results/coverage/Project/Project_5_10.coverage", "afdd4f2d-3960-481a-827d-976fec1e400b");
                debug("Serializing project to [" + safeProjectName + ".ser]");
            } finally {
                writeline("/home/ubuntu/results/coverage/Project/Project_5_10.coverage", "e171150f-ccbc-428f-a0a0-36f00e73cd35");
                s.close();
            }
        } catch (Exception e) {
            writeline("/home/ubuntu/results/coverage/Project/Project_5_10.coverage", "d7b62856-12cb-41eb-8824-c8a827fc6ef8");
            LOG.warn("Error serializing project to [" + safeProjectName + ".ser]: " + e.getMessage(), e);
        }
    }

    public void setLabelIncrementer(final LabelIncrementer incrementer) throws CruiseControlException {
        writeline("/home/ubuntu/results/coverage/Project/Project_5_10.coverage", "fc150660-5f12-41d9-b90f-2fafc029f034");
        if (incrementer == null) {
            writeline("/home/ubuntu/results/coverage/Project/Project_5_10.coverage", "7cf1c5d3-9087-4f90-a47a-6c4d99cc8432");
            throw new IllegalArgumentException("label incrementer can't be null");
        }
        writeline("/home/ubuntu/results/coverage/Project/Project_5_10.coverage", "28f91c45-9df4-460d-9ed2-23963798cd50");
        labelIncrementer = incrementer;
        writeline("/home/ubuntu/results/coverage/Project/Project_5_10.coverage", "40437bd3-1986-4adc-8e58-9701e8bf0ef8");
        if (label == null) {
            writeline("/home/ubuntu/results/coverage/Project/Project_5_10.coverage", "8f0b63ae-0e5b-4369-97e2-5b6db79d826c");
            label = labelIncrementer.getDefaultLabel();
        }
        writeline("/home/ubuntu/results/coverage/Project/Project_5_10.coverage", "d5945965-085d-4381-801b-c9956c9b2b12");
        validateLabel(label, labelIncrementer);
    }

    public LabelIncrementer getLabelIncrementer() {
        writeline("/home/ubuntu/results/coverage/Project/Project_5_10.coverage", "ce76562e-6a1d-4e02-82cc-56b87abf8b29");
        return labelIncrementer;
    }

    public void setName(final String projectName) {
        writeline("/home/ubuntu/results/coverage/Project/Project_5_10.coverage", "e979d84c-190d-4488-a39e-b3e76fecfab5");
        name = projectName;
    }

    public String getName() {
        writeline("/home/ubuntu/results/coverage/Project/Project_5_10.coverage", "138a821f-53b6-4d3b-b728-7fb54ce1cb3f");
        return name;
    }

    public void setLabel(final String newLabel) {
        writeline("/home/ubuntu/results/coverage/Project/Project_5_10.coverage", "66fc23fa-cc02-41d8-a18a-424ce27e733a");
        label = newLabel;
    }

    public String getLabel() {
        writeline("/home/ubuntu/results/coverage/Project/Project_5_10.coverage", "8c4c8798-657b-47bc-8d43-60d4992152be");
        return label;
    }

    /**
     * @param newLastBuild string containing the build date in the format
     * yyyyMMddHHmmss
     * @throws CruiseControlException if the date cannot be extracted from the
     * input string
     */
    public void setLastBuild(final String newLastBuild) throws CruiseControlException {
        writeline("/home/ubuntu/results/coverage/Project/Project_5_10.coverage", "32e44913-e4b1-4dd8-8d87-9c515c57cf28");
        lastBuild = DateUtil.parseFormattedTime(newLastBuild, "lastBuild");
    }

    /**
     * @param newLastSuccessfulBuild string containing the build date in the format
     * yyyyMMddHHmmss
     * @throws CruiseControlException if the date cannot be extracted from the
     * input string
     */
    public void setLastSuccessfulBuild(final String newLastSuccessfulBuild) throws CruiseControlException {
        writeline("/home/ubuntu/results/coverage/Project/Project_5_10.coverage", "9b2cecdd-4f9c-4922-bc85-39be72337cfb");
        lastSuccessfulBuild = DateUtil.parseFormattedTime(newLastSuccessfulBuild, "lastSuccessfulBuild");
    }

    public String getLastBuild() {
        writeline("/home/ubuntu/results/coverage/Project/Project_5_10.coverage", "643ffa70-b918-4f4f-ae4a-2e0318399ceb");
        if (lastBuild == null) {
            writeline("/home/ubuntu/results/coverage/Project/Project_5_10.coverage", "93dec758-05e6-40e6-8aeb-19013b370b06");
            return null;
        }
        writeline("/home/ubuntu/results/coverage/Project/Project_5_10.coverage", "78f0d9b2-be61-4329-baad-8d1f74dd03c7");
        return DateUtil.getFormattedTime(lastBuild);
    }

    public boolean getBuildForced() {
        writeline("/home/ubuntu/results/coverage/Project/Project_5_10.coverage", "ab0711aa-c7f0-486e-b2b7-929dd3e2536d");
        return buildForced;
    }

    public void setBuildForced(boolean forceNewBuildNow) {
        writeline("/home/ubuntu/results/coverage/Project/Project_5_10.coverage", "957e7acd-05d0-45a8-9145-680f2d02eacd");
        buildForced = forceNewBuildNow;
        writeline("/home/ubuntu/results/coverage/Project/Project_5_10.coverage", "4cfadcd8-99d3-4499-90e5-d6d4d67e25c1");
        if (forceNewBuildNow) {
            writeline("/home/ubuntu/results/coverage/Project/Project_5_10.coverage", "b894421a-8fe3-4582-998a-2706360815e2");
            forceBuild();
        }
    }

    public String getLastSuccessfulBuild() {
        writeline("/home/ubuntu/results/coverage/Project/Project_5_10.coverage", "748d28e6-f766-4e1a-84d2-de31fab91307");
        if (lastSuccessfulBuild == null) {
            writeline("/home/ubuntu/results/coverage/Project/Project_5_10.coverage", "65ddfa33-33f9-4677-99c6-010b51d80b1c");
            return null;
        }
        writeline("/home/ubuntu/results/coverage/Project/Project_5_10.coverage", "78f15d6d-d50a-4c32-946e-46c360f16316");
        return DateUtil.getFormattedTime(lastSuccessfulBuild);
    }

    public String getLogDir() {
        writeline("/home/ubuntu/results/coverage/Project/Project_5_10.coverage", "746dcfc7-61a2-473a-85fe-5bc7509f4d5b");
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
        writeline("/home/ubuntu/results/coverage/Project/Project_5_10.coverage", "bdf8677c-3e86-4a05-827d-a40478581515");
        if (overrideBuildInterval == null) {
            writeline("/home/ubuntu/results/coverage/Project/Project_5_10.coverage", "f2a3210e-e8ba-4b14-8c5c-5442691ca479");
            return projectConfig.getSchedule().getInterval();
        } else {
            writeline("/home/ubuntu/results/coverage/Project/Project_5_10.coverage", "1bcd9810-53e3-4884-a2fb-3b3cfa6cff6f");
            return overrideBuildInterval;
        }
    }

    /**
     * Sets the build interval that this Project should use. This method
     * overrides the value initially specified in the Schedule attribute.
     * @param sleepMillis the number of milliseconds to sleep between build attempts
     */
    public void overrideBuildInterval(final long sleepMillis) {
        writeline("/home/ubuntu/results/coverage/Project/Project_5_10.coverage", "7b8b4b1f-cc65-4606-8075-83ac0a58f5a5");
        overrideBuildInterval = sleepMillis;
    }

    public boolean isPaused() {
        writeline("/home/ubuntu/results/coverage/Project/Project_5_10.coverage", "af8acafa-a0a1-4c7f-a65b-711333c142ed");
        return isPaused;
    }

    public void setPaused(final boolean paused) {
        writeline("/home/ubuntu/results/coverage/Project/Project_5_10.coverage", "8550e5c6-e9a4-441f-ae90-2be897333125");
        synchronized (pausedMutex) {
            writeline("/home/ubuntu/results/coverage/Project/Project_5_10.coverage", "e4671275-f249-4ce2-b433-7a505759774c");
            if (isPaused && !paused) {
                writeline("/home/ubuntu/results/coverage/Project/Project_5_10.coverage", "b960d8cd-0eef-4018-a846-fee92aadfd82");
                pausedMutex.notifyAll();
            }
            writeline("/home/ubuntu/results/coverage/Project/Project_5_10.coverage", "c037f999-c267-40cb-a11f-201bfc98c836");
            isPaused = paused;
        }
    }

    public void setBuildAfterFailed(final boolean rebuildEvenWithNoNewModifications) {
        writeline("/home/ubuntu/results/coverage/Project/Project_5_10.coverage", "2d972c03-49da-49c0-b968-7f2afe82ca3d");
        buildAfterFailed = rebuildEvenWithNoNewModifications;
    }

    public String getStatus() {
        writeline("/home/ubuntu/results/coverage/Project/Project_5_10.coverage", "530f9675-ce1f-4efe-b8d9-f0ae56fb4696");
        return getState().getDescription();
    }

    public String getStatusWithQueuePosition() {
        writeline("/home/ubuntu/results/coverage/Project/Project_5_10.coverage", "9da890a3-8a89-4345-92d5-696732683b21");
        if (ProjectState.QUEUED.equals(getState())) {
            writeline("/home/ubuntu/results/coverage/Project/Project_5_10.coverage", "88d4bcda-979a-4a80-b593-d41d7709570b");
            return getState().getDescription() + " - " + queue.findPosition(projectConfig);
        } else {
            writeline("/home/ubuntu/results/coverage/Project/Project_5_10.coverage", "8bad8d53-25b5-4772-993c-628d218c0e32");
            return getState().getDescription();
        }
    }

    public ProjectState getState() {
        writeline("/home/ubuntu/results/coverage/Project/Project_5_10.coverage", "32b02259-93f7-42c4-a483-1ed4b333967b");
        return state;
    }

    private void setState(final ProjectState newState) {
        writeline("/home/ubuntu/results/coverage/Project/Project_5_10.coverage", "19ecab42-b5c7-4287-a410-81f71a914c50");
        state = newState;
        writeline("/home/ubuntu/results/coverage/Project/Project_5_10.coverage", "cbb185b3-b785-49ff-a302-e89e2d0dbd31");
        info(getStatus());
        writeline("/home/ubuntu/results/coverage/Project/Project_5_10.coverage", "f3779d71-25a9-4d72-a645-6c555cf834c5");
        notifyListeners(new ProjectStateChangedEvent(name, getState()));
        writeline("/home/ubuntu/results/coverage/Project/Project_5_10.coverage", "c773c526-ab71-4878-82c3-d65f49fde2e0");
        fireProgressEvent(new BuildProgressEvent(this, getState()));
    }

    public void setBuildQueue(final BuildQueue buildQueue) {
        writeline("/home/ubuntu/results/coverage/Project/Project_5_10.coverage", "5413cdbb-6ad9-4ed8-831b-446db18e8646");
        queue = buildQueue;
    }

    public String getBuildStartTime() {
        writeline("/home/ubuntu/results/coverage/Project/Project_5_10.coverage", "1c9eb00a-3e8d-4d5d-9390-d07f97eaa94a");
        return DateUtil.getFormattedTime(buildStartTime);
    }

    public Log getLog() {
        writeline("/home/ubuntu/results/coverage/Project/Project_5_10.coverage", "4958b872-69d9-429e-ae94-1591852f3b86");
        return this.projectConfig.getLog();
    }

    /**
     * Initialize the project. Uses ProjectXMLHelper to parse a project file.
     */
    protected void init() {
        writeline("/home/ubuntu/results/coverage/Project/Project_5_10.coverage", "d45d7c30-d9db-4a73-a96e-2deeb2bf4bd0");
        if (projectConfig == null) {
            writeline("/home/ubuntu/results/coverage/Project/Project_5_10.coverage", "c48eb5eb-5874-4dec-a6fb-184db2d3bc93");
            throw new IllegalStateException("projectConfig must be set on project before calling init()");
        }
        writeline("/home/ubuntu/results/coverage/Project/Project_5_10.coverage", "b6d21238-0163-49b5-b858-8d0f8aba6940");
        buildAfterFailed = projectConfig.shouldBuildAfterFailed();
        writeline("/home/ubuntu/results/coverage/Project/Project_5_10.coverage", "60608055-4624-4462-a033-dd0925923c39");
        forceOnly = projectConfig.isForceOnly();
        writeline("/home/ubuntu/results/coverage/Project/Project_5_10.coverage", "9dfec065-8b81-4088-968b-d9954254179e");
        requiremodification = projectConfig.isRequiremodification();
        writeline("/home/ubuntu/results/coverage/Project/Project_5_10.coverage", "58f4748d-1bd7-4b85-b3a5-6af8448f469c");
        if (lastBuild == null) {
            writeline("/home/ubuntu/results/coverage/Project/Project_5_10.coverage", "25f64405-ddd5-402e-b69e-3277fef4bce5");
            lastBuild = DateUtil.getMidnight();
        }
        writeline("/home/ubuntu/results/coverage/Project/Project_5_10.coverage", "b4700d14-b88b-4b5d-9fe1-14e4be563e02");
        if (lastSuccessfulBuild == null) {
            writeline("/home/ubuntu/results/coverage/Project/Project_5_10.coverage", "c0efff95-3704-4bff-8f1d-ae2d8b0973a7");
            lastSuccessfulBuild = lastBuild;
        }
        writeline("/home/ubuntu/results/coverage/Project/Project_5_10.coverage", "21abee72-8201-45d1-8ba2-532d62b5ba13");
        if (LOG.isDebugEnabled()) {
            writeline("/home/ubuntu/results/coverage/Project/Project_5_10.coverage", "3730e4bd-5dfe-49f8-8177-c649ff3f64d6");
            debug("buildInterval          = [" + getBuildInterval() + "]");
            writeline("/home/ubuntu/results/coverage/Project/Project_5_10.coverage", "636781f8-6798-4840-8ee5-5deefd39a54e");
            debug("buildForced            = [" + buildForced + "]");
            writeline("/home/ubuntu/results/coverage/Project/Project_5_10.coverage", "20037ad1-9501-4892-b413-8a048e703f59");
            debug("buildAfterFailed       = [" + buildAfterFailed + "]");
            writeline("/home/ubuntu/results/coverage/Project/Project_5_10.coverage", "f4907e2e-6b45-460b-aefd-e56bc0a43148");
            debug("requireModifcation     = [" + requiremodification + "]");
            writeline("/home/ubuntu/results/coverage/Project/Project_5_10.coverage", "5e89ddc9-11d4-454e-bd30-49913167ebc1");
            debug("forceOnly              = [" + forceOnly + "]");
            writeline("/home/ubuntu/results/coverage/Project/Project_5_10.coverage", "e1f9e7d8-a98d-4fda-8b19-be93b722c94d");
            debug("buildCounter           = [" + buildCounter + "]");
            writeline("/home/ubuntu/results/coverage/Project/Project_5_10.coverage", "d8ef7477-9d44-492a-9e52-14ca70f6e401");
            debug("isPaused               = [" + isPaused + "]");
            writeline("/home/ubuntu/results/coverage/Project/Project_5_10.coverage", "11062e7e-a431-48f4-8596-b626bc7a6634");
            debug("label                  = [" + label + "]");
            writeline("/home/ubuntu/results/coverage/Project/Project_5_10.coverage", "7bf12c1c-9cc5-424a-9155-9620c99fd8e6");
            debug("lastBuild              = [" + DateUtil.getFormattedTime(lastBuild) + "]");
            writeline("/home/ubuntu/results/coverage/Project/Project_5_10.coverage", "0f4bc92c-ef5e-4e02-81b6-401fb7fe2138");
            debug("lastSuccessfulBuild    = [" + DateUtil.getFormattedTime(lastSuccessfulBuild) + "]");
            writeline("/home/ubuntu/results/coverage/Project/Project_5_10.coverage", "f753b932-1df9-4e7d-afcb-925c04107700");
            debug("logDir                 = [" + projectConfig.getLog().getLogDir() + "]");
            writeline("/home/ubuntu/results/coverage/Project/Project_5_10.coverage", "be04d302-bc6a-47d4-9c7c-79c0852c75d2");
            debug("logXmlEncoding         = [" + projectConfig.getLog().getLogXmlEncoding() + "]");
            writeline("/home/ubuntu/results/coverage/Project/Project_5_10.coverage", "7a76ee03-f51f-4fc2-bd69-df7767c6b950");
            debug("wasLastBuildSuccessful = [" + wasLastBuildSuccessful + "]");
        }
    }

    protected Element getProjectPropertiesElement(final Date now) {
        writeline("/home/ubuntu/results/coverage/Project/Project_5_10.coverage", "96b69126-2d66-4623-b3c5-9f09087fbfe1");
        final Element infoElement = new Element("info");
        writeline("/home/ubuntu/results/coverage/Project/Project_5_10.coverage", "37cc0851-4637-4078-a9aa-4a93ad0997bc");
        addProperty(infoElement, "projectname", name);
        writeline("/home/ubuntu/results/coverage/Project/Project_5_10.coverage", "2c45d5ea-ff03-43f6-bdc1-a9fdd0b40bf9");
        final String lastBuildString = DateUtil.getFormattedTime(lastBuild == null ? now : lastBuild);
        writeline("/home/ubuntu/results/coverage/Project/Project_5_10.coverage", "fdec6a66-5838-44d3-b818-a27d109c6e62");
        addProperty(infoElement, "lastbuild", lastBuildString);
        writeline("/home/ubuntu/results/coverage/Project/Project_5_10.coverage", "27a5b7c8-eca5-49c1-97ea-a1fbd6913d80");
        final String lastSuccessfulBuildString = DateUtil.getFormattedTime(lastSuccessfulBuild == null ? now : lastSuccessfulBuild);
        writeline("/home/ubuntu/results/coverage/Project/Project_5_10.coverage", "ee27f932-478d-4e82-bb51-ba224e56eabe");
        addProperty(infoElement, "lastsuccessfulbuild", lastSuccessfulBuildString);
        writeline("/home/ubuntu/results/coverage/Project/Project_5_10.coverage", "73469a9c-c10c-48af-aebd-9a9782c467c5");
        addProperty(infoElement, "builddate", DateUtil.formatIso8601(now));
        writeline("/home/ubuntu/results/coverage/Project/Project_5_10.coverage", "b2ed27a7-939a-42a2-87bf-ea773e850baf");
        addProperty(infoElement, "cctimestamp", DateUtil.getFormattedTime(now));
        writeline("/home/ubuntu/results/coverage/Project/Project_5_10.coverage", "42ebc322-be7d-45f6-be0b-68aa9ee8cf94");
        addProperty(infoElement, "label", label);
        writeline("/home/ubuntu/results/coverage/Project/Project_5_10.coverage", "21dbf57b-e70b-484f-92f1-4434588661fa");
        addProperty(infoElement, "interval", Long.toString(getBuildInterval() / 1000L));
        writeline("/home/ubuntu/results/coverage/Project/Project_5_10.coverage", "04967bed-347e-4da1-80e9-0c78d91183fb");
        addProperty(infoElement, "lastbuildsuccessful", String.valueOf(wasLastBuildSuccessful));
        writeline("/home/ubuntu/results/coverage/Project/Project_5_10.coverage", "e3ab5c9c-4420-4161-b203-2e1713b52158");
        return infoElement;
    }

    private void addProperty(final Element parent, final String key, final String value) {
        writeline("/home/ubuntu/results/coverage/Project/Project_5_10.coverage", "0ababe08-5a88-413a-bc81-9140e5851371");
        final Element propertyElement = new Element("property");
        writeline("/home/ubuntu/results/coverage/Project/Project_5_10.coverage", "24c17edf-83f5-4a5c-b6d5-a71a18bc474f");
        propertyElement.setAttribute("name", key);
        writeline("/home/ubuntu/results/coverage/Project/Project_5_10.coverage", "10925daf-16a2-4d50-83ee-8354847178ed");
        propertyElement.setAttribute("value", value);
        writeline("/home/ubuntu/results/coverage/Project/Project_5_10.coverage", "cbb678db-0af1-41ea-a486-b9cce1acbdfe");
        parent.addContent(propertyElement);
    }

    protected Map<String, String> getProjectPropertiesMap(final Date now) {
        writeline("/home/ubuntu/results/coverage/Project/Project_5_10.coverage", "dcbd4e0e-f854-4e26-8903-e9cdfd401d39");
        final Map<String, String> buildProperties = new HashMap<String, String>();
        writeline("/home/ubuntu/results/coverage/Project/Project_5_10.coverage", "488a9fba-1dfa-4c9f-a7d2-b390d580e8b9");
        buildProperties.put("projectname", name);
        writeline("/home/ubuntu/results/coverage/Project/Project_5_10.coverage", "60831e1b-aa39-49c2-b999-497b83f2eb3d");
        buildProperties.put("label", label);
        writeline("/home/ubuntu/results/coverage/Project/Project_5_10.coverage", "7dc8203a-8342-4312-9e0b-1461223166f9");
        // TODO: Shouldn't have CVS specific properties here
        buildProperties.put("cvstimestamp", CVSDateUtil.formatCVSDate(now));
        writeline("/home/ubuntu/results/coverage/Project/Project_5_10.coverage", "936416d2-8716-4089-8e6c-d40d42023299");
        buildProperties.put("cctimestamp", DateUtil.getFormattedTime(now));
        writeline("/home/ubuntu/results/coverage/Project/Project_5_10.coverage", "44a4d52b-b9d9-43e0-a850-20632af31621");
        buildProperties.put("cclastgoodbuildtimestamp", getLastSuccessfulBuild());
        writeline("/home/ubuntu/results/coverage/Project/Project_5_10.coverage", "2d199347-9ca2-4db1-80e1-d477af29d051");
        buildProperties.put("cclastbuildtimestamp", getLastBuild());
        writeline("/home/ubuntu/results/coverage/Project/Project_5_10.coverage", "bc9d2bc2-c70a-4067-8956-ce00be768686");
        buildProperties.put("lastbuildsuccessful", String.valueOf(isLastBuildSuccessful()));
        writeline("/home/ubuntu/results/coverage/Project/Project_5_10.coverage", "005fdbd9-0397-45f2-a969-e12bc26f2371");
        buildProperties.put("buildforced", String.valueOf(getBuildForced()));
        writeline("/home/ubuntu/results/coverage/Project/Project_5_10.coverage", "951c7427-c574-4ede-95b8-1b9c6c2974f4");
        if (projectConfig.getModificationSet() != null) {
            writeline("/home/ubuntu/results/coverage/Project/Project_5_10.coverage", "e194d58b-f317-45e6-a2dd-ac9479940fb6");
            buildProperties.putAll(projectConfig.getModificationSet().getProperties());
        }
        writeline("/home/ubuntu/results/coverage/Project/Project_5_10.coverage", "48b27ad6-38be-4c9e-a3ac-e90da51f54d3");
        if (additionalProperties != null && !additionalProperties.isEmpty()) {
            writeline("/home/ubuntu/results/coverage/Project/Project_5_10.coverage", "6f86f1e7-feff-4c88-b4a6-b27ff6ca2ff2");
            buildProperties.putAll(additionalProperties);
            writeline("/home/ubuntu/results/coverage/Project/Project_5_10.coverage", "35674a6b-f58d-4e65-a04a-39c9343e7fd8");
            additionalProperties.clear();
            writeline("/home/ubuntu/results/coverage/Project/Project_5_10.coverage", "6bcbbb97-aa44-4b9d-ab2d-dd3cdecee5f4");
            additionalProperties = null;
        }
        writeline("/home/ubuntu/results/coverage/Project/Project_5_10.coverage", "4247db72-6b37-48b3-9177-9ebad90f1d41");
        return buildProperties;
    }

    /**
     * Intended only for unit testing.
     * @return additional Properties variable.
     */
    Map<String, String> getAdditionalProperties() {
        writeline("/home/ubuntu/results/coverage/Project/Project_5_10.coverage", "b5d143ac-d3b9-44fb-9dfe-d7048f8aae7e");
        return additionalProperties;
    }

    /**
     * Iterate over all of the registered <code>Publisher</code>s and call
     * their respective <code>publish</code> methods.
     * @param buildLog the content to publish
     * @throws CruiseControlException if an error occurs during publishing
     */
    protected void publish(final Log buildLog) throws CruiseControlException {
        writeline("/home/ubuntu/results/coverage/Project/Project_5_10.coverage", "9d6c2f66-ff47-4316-bb71-46d7d542b345");
        setState(ProjectState.PUBLISHING);
        writeline("/home/ubuntu/results/coverage/Project/Project_5_10.coverage", "9a3cfa3d-2c38-4030-a5a9-95c8b786056c");
        for (final Publisher publisher : projectConfig.getPublishers()) {
            writeline("/home/ubuntu/results/coverage/Project/Project_5_10.coverage", "97401320-112c-4b4c-852d-388d744b8d23");
            // catch all errors, Publishers shouldn't cause failures in the build method
            try {
                writeline("/home/ubuntu/results/coverage/Project/Project_5_10.coverage", "ff4676be-452e-45fc-a31f-ff058a684326");
                publisher.publish(buildLog.getContent());
            } catch (Throwable t) {
                writeline("/home/ubuntu/results/coverage/Project/Project_5_10.coverage", "d1c44c84-1eec-4fa9-971e-269ee5cf2f4b");
                final StringBuilder message = new StringBuilder("exception publishing results");
                writeline("/home/ubuntu/results/coverage/Project/Project_5_10.coverage", "021eb09c-3994-4a2b-a70c-47bd0513b28b");
                message.append(" with ").append(publisher.getClass().getName());
                writeline("/home/ubuntu/results/coverage/Project/Project_5_10.coverage", "107819b8-41c0-4fb6-a3b2-82bc94f4f67e");
                message.append(" for project ").append(name);
                writeline("/home/ubuntu/results/coverage/Project/Project_5_10.coverage", "5bad5632-cef1-4202-b47f-a48430a77dcb");
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
        writeline("/home/ubuntu/results/coverage/Project/Project_5_10.coverage", "52436e9e-0527-4055-91fa-8a86f5e4d898");
        setState(ProjectState.BOOTSTRAPPING);
        writeline("/home/ubuntu/results/coverage/Project/Project_5_10.coverage", "ccdd17d5-875e-445c-9234-41cd16d10793");
        for (final Bootstrapper bootstrapper : projectConfig.getBootstrappers()) {
            writeline("/home/ubuntu/results/coverage/Project/Project_5_10.coverage", "951afc81-dcac-4d48-af93-fe0a82474800");
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
        writeline("/home/ubuntu/results/coverage/Project/Project_5_10.coverage", "d32270c5-bbf1-4218-839e-d890fdabf849");
        if (!incrementer.isValidLabel(oldLabel)) {
            writeline("/home/ubuntu/results/coverage/Project/Project_5_10.coverage", "5ecdcff8-a00c-4421-a9cb-ef5b8a5b8e3a");
            final String message = oldLabel + " is not a valid label for labelIncrementer " + incrementer.getClass().getName();
            writeline("/home/ubuntu/results/coverage/Project/Project_5_10.coverage", "6bf6f489-6820-4b1f-9b75-a7ea67e4f199");
            debug(message);
            writeline("/home/ubuntu/results/coverage/Project/Project_5_10.coverage", "b228a71e-8434-4645-8504-4564f8333dcf");
            throw new CruiseControlException(message);
        }
    }

    public boolean isLastBuildSuccessful() {
        writeline("/home/ubuntu/results/coverage/Project/Project_5_10.coverage", "5c0bfff2-daf5-4a36-8ed5-76fc5a09e18b");
        return wasLastBuildSuccessful;
    }

    void setWasLastBuildSuccessful(final boolean buildSuccessful) {
        writeline("/home/ubuntu/results/coverage/Project/Project_5_10.coverage", "2171a451-d647-483e-94cc-57a78bbcd471");
        wasLastBuildSuccessful = buildSuccessful;
    }

    /**
     * Logs a message to the application log, not to be confused with the
     * CruiseControl build log.
     * @param message the application message to log
     */
    private void info(final String message) {
        writeline("/home/ubuntu/results/coverage/Project/Project_5_10.coverage", "9fd4ab1b-134c-400e-8d1a-3d54385f6c48");
        LOG.info("Project " + name + ":  " + message);
    }

    private void debug(final String message) {
        writeline("/home/ubuntu/results/coverage/Project/Project_5_10.coverage", "ba65289a-ac84-4ef3-b6b2-9e71fa97bcc6");
        LOG.debug("Project " + name + ":  " + message);
    }

    public void start() {
        writeline("/home/ubuntu/results/coverage/Project/Project_5_10.coverage", "57fc3f1b-5a5d-4681-9bb9-0f23744e7f4d");
        if (stopped || getState() == ProjectState.STOPPED) {
            writeline("/home/ubuntu/results/coverage/Project/Project_5_10.coverage", "86d4b276-08a1-46da-8861-e4026cf419ff");
            stopped = false;
            writeline("/home/ubuntu/results/coverage/Project/Project_5_10.coverage", "2f787c20-a871-466d-b857-f2a2709a8dbd");
            LOG.info("Project " + name + " starting");
            writeline("/home/ubuntu/results/coverage/Project/Project_5_10.coverage", "459abc69-dbc6-4d21-999d-f9974d5c3118");
            setState(ProjectState.IDLE);
            writeline("/home/ubuntu/results/coverage/Project/Project_5_10.coverage", "27fc905c-89f7-4c50-be34-db36e594d68e");
            createNewSchedulingThread();
        }
    }

    protected void createNewSchedulingThread() {
        writeline("/home/ubuntu/results/coverage/Project/Project_5_10.coverage", "78206f2e-4372-40a2-a1e4-483b1f57d8f9");
        final Thread projectSchedulingThread = new Thread(this, "Project " + getName() + " thread");
        writeline("/home/ubuntu/results/coverage/Project/Project_5_10.coverage", "a9fdca41-6012-4c3f-a417-bae4bbdeca77");
        projectSchedulingThread.start();
        writeline("/home/ubuntu/results/coverage/Project/Project_5_10.coverage", "8e77566a-c8f7-44e9-bae0-a24bc5d5b442");
        // brief nap to allow thread to start
        try {
            writeline("/home/ubuntu/results/coverage/Project/Project_5_10.coverage", "2b9dfe60-0202-48a3-8ca4-3fe137fb919b");
            Thread.sleep(100);
        } catch (InterruptedException ie) {
            writeline("/home/ubuntu/results/coverage/Project/Project_5_10.coverage", "4ffd8d12-dc45-45b3-98eb-00342a23f3f5");
            LOG.warn("interrupted while waiting for scheduling thread to start", ie);
        }
    }

    public void stop() {
        writeline("/home/ubuntu/results/coverage/Project/Project_5_10.coverage", "4f8dc71b-7c5d-49e0-bc1c-617e7dc9c023");
        LOG.info("Project " + name + " stopping");
        writeline("/home/ubuntu/results/coverage/Project/Project_5_10.coverage", "1253bbef-4447-4e74-83e1-d42394269f7f");
        stopped = true;
        writeline("/home/ubuntu/results/coverage/Project/Project_5_10.coverage", "0ff251a1-1cdb-4a41-aec7-1b8f8b47fe97");
        setState(ProjectState.STOPPED);
        writeline("/home/ubuntu/results/coverage/Project/Project_5_10.coverage", "1f3d2785-9acf-4f7a-a800-d9bf856d906a");
        synchronized (pausedMutex) {
            writeline("/home/ubuntu/results/coverage/Project/Project_5_10.coverage", "eef1fdf5-a15c-4697-9a71-0c8bb3265ded");
            pausedMutex.notifyAll();
        }
        writeline("/home/ubuntu/results/coverage/Project/Project_5_10.coverage", "5066f2f2-0e1f-4592-b445-a29b8904be06");
        synchronized (waitMutex) {
            writeline("/home/ubuntu/results/coverage/Project/Project_5_10.coverage", "e34a9148-1248-493e-88da-ec4db21a1b8e");
            waitMutex.notifyAll();
        }
        writeline("/home/ubuntu/results/coverage/Project/Project_5_10.coverage", "3b0a837b-2461-4f80-bc62-b7ad87a2ec51");
        synchronized (scheduleMutex) {
            writeline("/home/ubuntu/results/coverage/Project/Project_5_10.coverage", "56dd5bfa-d425-4e4d-a15a-8c9d65499113");
            scheduleMutex.notifyAll();
        }
    }

    public String toString() {
        writeline("/home/ubuntu/results/coverage/Project/Project_5_10.coverage", "744e58dd-053a-4fd9-a5ea-e27aaca17769");
        final StringBuilder sb = new StringBuilder("Project ");
        writeline("/home/ubuntu/results/coverage/Project/Project_5_10.coverage", "24d9e60f-4f8b-40b6-a175-188202ba990d");
        sb.append(getName());
        writeline("/home/ubuntu/results/coverage/Project/Project_5_10.coverage", "1b94a931-5d1d-4aba-a204-bfc6f8a1c8e4");
        sb.append(": ");
        writeline("/home/ubuntu/results/coverage/Project/Project_5_10.coverage", "2ae86d11-500d-4f87-9b4c-d831e4fe897a");
        sb.append(getStatus());
        writeline("/home/ubuntu/results/coverage/Project/Project_5_10.coverage", "9e6de5fa-23e1-454d-8168-dd8ff63c3838");
        if (isPaused) {
            writeline("/home/ubuntu/results/coverage/Project/Project_5_10.coverage", "2a737a29-1b18-441b-8f44-9ba5c3cca4a3");
            sb.append(" (paused)");
        }
        writeline("/home/ubuntu/results/coverage/Project/Project_5_10.coverage", "77d39336-1a4e-463b-8e53-ee5f17311ed1");
        return sb.toString();
    }

    public void addBuildProgressListener(final BuildProgressListener listener) {
        writeline("/home/ubuntu/results/coverage/Project/Project_5_10.coverage", "87f4b2aa-9742-440a-aaf0-5305b50bff87");
        synchronized (progressListeners) {
            writeline("/home/ubuntu/results/coverage/Project/Project_5_10.coverage", "f41cc555-6118-48f1-bed3-4e2a657ce81c");
            progressListeners.add(listener);
        }
    }

    protected void fireProgressEvent(final BuildProgressEvent event) {
        writeline("/home/ubuntu/results/coverage/Project/Project_5_10.coverage", "5161ab36-c821-4cfc-ab83-cef28e841094");
        synchronized (progressListeners) {
            writeline("/home/ubuntu/results/coverage/Project/Project_5_10.coverage", "6c9dd231-583a-4cb3-b3e8-c24191bb7e9b");
            for (final BuildProgressListener listener : progressListeners) {
                writeline("/home/ubuntu/results/coverage/Project/Project_5_10.coverage", "33e58f75-1b44-4299-9f82-d1609d59bb5f");
                listener.handleBuildProgress(event);
            }
        }
    }

    public void addBuildResultListener(final BuildResultListener listener) {
        writeline("/home/ubuntu/results/coverage/Project/Project_5_10.coverage", "449feb38-699a-4c9c-8b13-e5ea92de7fc9");
        synchronized (resultListeners) {
            writeline("/home/ubuntu/results/coverage/Project/Project_5_10.coverage", "7bad9fac-561d-461e-af07-198162aec69f");
            resultListeners.add(listener);
        }
    }

    protected void fireResultEvent(final BuildResultEvent event) {
        writeline("/home/ubuntu/results/coverage/Project/Project_5_10.coverage", "e1197ef4-9858-4a5d-ae0e-b19cfd3404cf");
        synchronized (resultListeners) {
            writeline("/home/ubuntu/results/coverage/Project/Project_5_10.coverage", "09843629-2ab3-45b0-9174-43e8bb94edc0");
            for (final BuildResultListener listener : resultListeners) {
                writeline("/home/ubuntu/results/coverage/Project/Project_5_10.coverage", "1ae849d2-899a-42c0-8af3-c7dc81c296d5");
                listener.handleBuildResult(event);
            }
        }
    }

    List<Listener> getListeners() {
        writeline("/home/ubuntu/results/coverage/Project/Project_5_10.coverage", "4b8476f2-af22-450b-b528-c027e07e3948");
        return projectConfig.getListeners();
    }

    public void setProjectConfig(final ProjectConfig projectConfig) throws CruiseControlException {
        writeline("/home/ubuntu/results/coverage/Project/Project_5_10.coverage", "1538a3be-f366-4a3e-9355-53fad9c2a23c");
        if (projectConfig == null) {
            writeline("/home/ubuntu/results/coverage/Project/Project_5_10.coverage", "c034211c-d7d1-4f29-acea-13a1d9a383fa");
            throw new IllegalArgumentException("project config can't be null");
        }
        writeline("/home/ubuntu/results/coverage/Project/Project_5_10.coverage", "7a0c3772-195f-4016-bd4d-247a7255a7fc");
        this.projectConfig = projectConfig;
        writeline("/home/ubuntu/results/coverage/Project/Project_5_10.coverage", "53014566-a542-46b3-bcd7-18eaa0d0f1ca");
        setLabelIncrementer(projectConfig.getLabelIncrementer());
    }

    void notifyListeners(final ProjectEvent event) {
        writeline("/home/ubuntu/results/coverage/Project/Project_5_10.coverage", "4394fc9a-841c-40f5-97db-daca3c615633");
        if (projectConfig == null) {
            writeline("/home/ubuntu/results/coverage/Project/Project_5_10.coverage", "00ccf13e-9150-4c56-aeb0-62389ad7b0ff");
            throw new IllegalStateException("projectConfig is null");
        }
        writeline("/home/ubuntu/results/coverage/Project/Project_5_10.coverage", "5674768b-1d3a-4563-8000-2c2adaf740d4");
        for (final Listener listener : projectConfig.getListeners()) {
            writeline("/home/ubuntu/results/coverage/Project/Project_5_10.coverage", "2c83c191-2f9f-4420-a485-5602466d4815");
            try {
                writeline("/home/ubuntu/results/coverage/Project/Project_5_10.coverage", "24517e84-dbed-477a-a72e-6262d3e4c1b8");
                listener.handleEvent(event);
            } catch (CruiseControlException e) {
                writeline("/home/ubuntu/results/coverage/Project/Project_5_10.coverage", "753e9047-1d6b-4782-b834-2d1ddac7c924");
                final StringBuilder message = new StringBuilder("exception notifying listener ");
                writeline("/home/ubuntu/results/coverage/Project/Project_5_10.coverage", "c49d691f-be3a-466b-af1c-49563147fdc8");
                message.append(listener.getClass().getName());
                writeline("/home/ubuntu/results/coverage/Project/Project_5_10.coverage", "f6952c4f-68a2-4499-9b9f-592819289fa6");
                message.append(" for project ").append(name);
                writeline("/home/ubuntu/results/coverage/Project/Project_5_10.coverage", "73da7ab6-5b4f-47ed-a46c-fb4b17fa0089");
                LOG.error(message.toString(), e);
            }
        }
    }

    public boolean equals(final Object arg0) {
        writeline("/home/ubuntu/results/coverage/Project/Project_5_10.coverage", "f4bd3126-a269-4734-9003-c4c31d86f64a");
        if (arg0 == null) {
            writeline("/home/ubuntu/results/coverage/Project/Project_5_10.coverage", "0f639eb9-aee4-4d55-8573-77278d0a7816");
            return false;
        }
        writeline("/home/ubuntu/results/coverage/Project/Project_5_10.coverage", "d575c2a0-be19-47dc-9aa5-f0a2886deea1");
        if (arg0.getClass().getName().equals(getClass().getName())) {
            writeline("/home/ubuntu/results/coverage/Project/Project_5_10.coverage", "9de5f66a-3e55-452b-bc0d-914a68534510");
            final Project thatProject = (Project) arg0;
            writeline("/home/ubuntu/results/coverage/Project/Project_5_10.coverage", "870270b9-2558-46e9-b9d8-b87a664f75a3");
            return thatProject.name.equals(name);
        }
        writeline("/home/ubuntu/results/coverage/Project/Project_5_10.coverage", "da3a8919-bc77-4eea-b5cc-e103c5a0aad7");
        return false;
    }

    public int hashCode() {
        writeline("/home/ubuntu/results/coverage/Project/Project_5_10.coverage", "4b974c7d-b1e5-4f50-8385-f0831ac2514c");
        return name.hashCode();
    }

    public void register(final MBeanServer server) throws JMException {
        writeline("/home/ubuntu/results/coverage/Project/Project_5_10.coverage", "6635ea87-9e16-4a00-b85a-4db9c1e45a24");
        LOG.debug("Registering project mbean");
        writeline("/home/ubuntu/results/coverage/Project/Project_5_10.coverage", "293534b8-6f9c-4c01-87c0-8f5523fd5fb6");
        final ProjectController projectController = new ProjectController(this);
        writeline("/home/ubuntu/results/coverage/Project/Project_5_10.coverage", "38921996-3ba8-4163-a17b-230291f494ea");
        projectController.register(server);
    }

    public ProjectConfig getProjectConfig() {
        writeline("/home/ubuntu/results/coverage/Project/Project_5_10.coverage", "241377f0-aaa8-47fd-a7df-051ac4a3386d");
        return projectConfig;
    }

    public Date getLastBuildDate() {
        writeline("/home/ubuntu/results/coverage/Project/Project_5_10.coverage", "0773c190-6215-4c68-8f08-b85e3ccd6db9");
        return lastBuild;
    }

    public Progress getProgress() {
        writeline("/home/ubuntu/results/coverage/Project/Project_5_10.coverage", "f33a63f9-e3bd-4819-9394-1cbc4edfcbc4");
        return progress;
    }

    public List<String> getLogLabels() {
        writeline("/home/ubuntu/results/coverage/Project/Project_5_10.coverage", "e4eb75a7-a7ba-4ef7-97d3-ba296823a022");
        return projectConfig.getLogLabels();
    }

    public String[] getLogLabelLines(final String logLabel, final int firstLine) {
        writeline("/home/ubuntu/results/coverage/Project/Project_5_10.coverage", "e7002100-69b0-486e-92dc-2292d064a745");
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
