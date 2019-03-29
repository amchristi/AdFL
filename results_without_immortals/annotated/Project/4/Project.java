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
        writeline("/home/ubuntu/results/coverage/Project/Project_4_10.coverage", "fe098a10-b6a3-41c9-89b3-a0b2ecb83a8f");
        state = ProjectState.STOPPED;
        writeline("/home/ubuntu/results/coverage/Project/Project_4_10.coverage", "44e35bec-19e4-4025-9fb5-121ea4334f25");
        pausedMutex = new Object();
        writeline("/home/ubuntu/results/coverage/Project/Project_4_10.coverage", "4072f343-d156-4d8f-8e23-bdee800de0ce");
        scheduleMutex = new Object();
        writeline("/home/ubuntu/results/coverage/Project/Project_4_10.coverage", "9b602b2b-b28e-4b4b-947c-45e3f8d5aba8");
        waitMutex = new Object();
        writeline("/home/ubuntu/results/coverage/Project/Project_4_10.coverage", "a7c5b705-370f-4dec-a776-49a79bfee386");
        progressListeners = new ArrayList<BuildProgressListener>();
        writeline("/home/ubuntu/results/coverage/Project/Project_4_10.coverage", "9f73f733-175a-4c04-b3a3-51f0fd023ac5");
        resultListeners = new ArrayList<BuildResultListener>();
        writeline("/home/ubuntu/results/coverage/Project/Project_4_10.coverage", "5b7088b2-2691-4f69-9eca-3884db0ad251");
        progress = new ProgressImpl(this);
    }

    private void readObject(final ObjectInputStream stream) throws IOException, ClassNotFoundException {
        writeline("/home/ubuntu/results/coverage/Project/Project_4_10.coverage", "3354f0f4-37ad-4613-a9e6-e93d4de3b9c6");
        stream.defaultReadObject();
        writeline("/home/ubuntu/results/coverage/Project/Project_4_10.coverage", "b32116d6-cb36-4df3-bf4c-fba83eab89a6");
        initializeTransientFields();
    }

    public void execute() {
        writeline("/home/ubuntu/results/coverage/Project/Project_4_10.coverage", "03304ad4-9f9e-4d35-a19c-8ce51f1ee7d0");
        if (stopped) {
            writeline("/home/ubuntu/results/coverage/Project/Project_4_10.coverage", "d8d543b9-47d6-4bc9-945d-85c8acdc435a");
            LOG.warn("not building project " + name + " because project has been stopped.");
            writeline("/home/ubuntu/results/coverage/Project/Project_4_10.coverage", "5c402220-17b0-4483-af99-756754abcf3c");
            buildFinished();
            writeline("/home/ubuntu/results/coverage/Project/Project_4_10.coverage", "4ab12233-a736-4e69-8794-0159b9f043e9");
            return;
        }
        writeline("/home/ubuntu/results/coverage/Project/Project_4_10.coverage", "3fb3eb60-fe55-40cc-b5ce-d67e0b215917");
        synchronized (pausedMutex) {
            writeline("/home/ubuntu/results/coverage/Project/Project_4_10.coverage", "8d6eef54-fd92-45dc-8ba1-b1e2d15451b7");
            if (isPaused) {
                writeline("/home/ubuntu/results/coverage/Project/Project_4_10.coverage", "16279ce5-862f-4ee4-a142-d54ae3349517");
                LOG.info("not building project " + name + " because project has been paused.");
                writeline("/home/ubuntu/results/coverage/Project/Project_4_10.coverage", "d9a896e6-c880-43fd-ae65-2e1705f48868");
                buildFinished();
                writeline("/home/ubuntu/results/coverage/Project/Project_4_10.coverage", "477c59a2-ec66-40ea-af6f-08fe1a69d645");
                return;
            }
        }
        writeline("/home/ubuntu/results/coverage/Project/Project_4_10.coverage", "91898a93-1e0c-434d-9696-af372da34e34");
        try {
            writeline("/home/ubuntu/results/coverage/Project/Project_4_10.coverage", "af153a52-339c-458d-a862-d4cc66d25457");
            init();
            writeline("/home/ubuntu/results/coverage/Project/Project_4_10.coverage", "c4ab261f-31d9-4b85-8250-3a32786a34de");
            build();
        } catch (CruiseControlException e) {
            writeline("/home/ubuntu/results/coverage/Project/Project_4_10.coverage", "316112a9-3289-48df-982a-012d46fa4787");
            LOG.error("exception attempting build in project " + name, e);
        } finally {
            writeline("/home/ubuntu/results/coverage/Project/Project_4_10.coverage", "a3981822-d9a7-4b2e-bec1-caf627b25938");
            buildFinished();
        }
    }

    /**
     * Unless paused, runs any bootstrappers and then the entire build.
     * @throws CruiseControlException if an error occurs during the build
     */
    protected void build() throws CruiseControlException {
        writeline("/home/ubuntu/results/coverage/Project/Project_4_10.coverage", "c9b4eabf-284e-498a-8977-b201f51cedbe");
        if (projectConfig == null) {
            writeline("/home/ubuntu/results/coverage/Project/Project_4_10.coverage", "40bd9b7c-18e1-492b-9065-be9b6c87869a");
            throw new IllegalStateException("projectConfig must be set on project before calling build()");
        }
        writeline("/home/ubuntu/results/coverage/Project/Project_4_10.coverage", "60bd75c8-cf3a-45f7-b704-a463dd5659d3");
        if (stopped) {
            writeline("/home/ubuntu/results/coverage/Project/Project_4_10.coverage", "f55f1e65-147b-472a-b935-577d74b2310d");
            LOG.warn("not building project " + name + " because project has been stopped.");
            writeline("/home/ubuntu/results/coverage/Project/Project_4_10.coverage", "cf11dde0-8f1b-4388-b96e-3f8654ed5a38");
            return;
        }
        writeline("/home/ubuntu/results/coverage/Project/Project_4_10.coverage", "43070522-a13a-4cd6-ac98-b671029b13c9");
        // or if the last build faild and we want to build on failures
        if (forceOnly && !buildForced && !(!wasLastBuildSuccessful && buildAfterFailed)) {
            writeline("/home/ubuntu/results/coverage/Project/Project_4_10.coverage", "e737f726-b6e1-4893-af73-e53b507c87f9");
            info("not building because project is forceOnly and build not forced.");
            writeline("/home/ubuntu/results/coverage/Project/Project_4_10.coverage", "89735687-d5f5-4f0f-9084-62396f83a181");
            return;
        }
        writeline("/home/ubuntu/results/coverage/Project/Project_4_10.coverage", "11dc2537-388a-4efc-a078-5683c58f37a5");
        final boolean buildWasForced = buildForced;
        writeline("/home/ubuntu/results/coverage/Project/Project_4_10.coverage", "32505cb0-7da6-46f7-a7fa-0639000945cf");
        try {
            writeline("/home/ubuntu/results/coverage/Project/Project_4_10.coverage", "e8a64a96-a291-4f52-a3df-4a571a3aa44f");
            setBuildStartTime(new Date());
            writeline("/home/ubuntu/results/coverage/Project/Project_4_10.coverage", "057cdd59-5989-40c0-bbd7-94857e57d374");
            final Schedule schedule = projectConfig.getSchedule();
            writeline("/home/ubuntu/results/coverage/Project/Project_4_10.coverage", "ae036166-ce06-4671-8397-1d84a036bbbb");
            if (schedule == null) {
                writeline("/home/ubuntu/results/coverage/Project/Project_4_10.coverage", "0042977c-45e9-48c1-a013-6cf2c696c7e7");
                throw new IllegalStateException("project must have a schedule");
            }
            writeline("/home/ubuntu/results/coverage/Project/Project_4_10.coverage", "ad5e378d-736d-4fc9-9c30-ae0728e0fd61");
            if (schedule.isPaused(buildStartTime)) {
                writeline("/home/ubuntu/results/coverage/Project/Project_4_10.coverage", "540e1fd4-f1be-402e-ba7b-df8aafd0548f");
                // is different than ProjectState.PAUSED
                return;
            }
            writeline("/home/ubuntu/results/coverage/Project/Project_4_10.coverage", "179bf886-ca56-4115-960c-893bcdbae779");
            // @todo Add Progress param to Bootstrapper API?
            bootstrap();
            writeline("/home/ubuntu/results/coverage/Project/Project_4_10.coverage", "3e1bfd82-5f91-4bd3-96e5-5afde74ac66f");
            final String target = useAndResetBuildTargetIfBuildWasForced(buildWasForced);
            writeline("/home/ubuntu/results/coverage/Project/Project_4_10.coverage", "ec4cd604-d3fe-4d06-826d-836a45c6d81d");
            // @todo Add Progress param to ModificationSet API?
            // getModifications will only return null if we don't need to build
            final Element modifications = getModifications(buildWasForced);
            writeline("/home/ubuntu/results/coverage/Project/Project_4_10.coverage", "131ca94f-7e2d-4c24-b666-0c2d3aefd664");
            if (modifications == null) {
                writeline("/home/ubuntu/results/coverage/Project/Project_4_10.coverage", "51062e7c-b6ef-4db1-b11b-acf8731d796a");
                return;
            }
            writeline("/home/ubuntu/results/coverage/Project/Project_4_10.coverage", "554046bb-066b-4e00-a221-5865a27e2c23");
            // Using local reference to avoid NPE if config.xml is updated during build
            final Log buildLog = projectConfig.getLog();
            writeline("/home/ubuntu/results/coverage/Project/Project_4_10.coverage", "726696c9-2cfa-4c44-b8e6-e436d1d94a50");
            buildLog.addContent(modifications);
            writeline("/home/ubuntu/results/coverage/Project/Project_4_10.coverage", "3ffaee71-30bd-443f-b474-dfa13d37837a");
            final Date now;
            writeline("/home/ubuntu/results/coverage/Project/Project_4_10.coverage", "e2f6d73d-0125-411b-879a-654c6e19d840");
            if (projectConfig.getModificationSet() != null && projectConfig.getModificationSet().getTimeOfCheck() != null) {
                writeline("/home/ubuntu/results/coverage/Project/Project_4_10.coverage", "3048dc6e-ed78-4f7f-9d53-ebc057621bca");
                now = projectConfig.getModificationSet().getTimeOfCheck();
            } else {
                writeline("/home/ubuntu/results/coverage/Project/Project_4_10.coverage", "e22f3279-5a49-42d9-92e2-1fa7caf20df8");
                now = new Date();
            }
            writeline("/home/ubuntu/results/coverage/Project/Project_4_10.coverage", "0d2072ea-c98a-4a51-bf47-acda55dca970");
            if (getLabelIncrementer().isPreBuildIncrementer()) {
                writeline("/home/ubuntu/results/coverage/Project/Project_4_10.coverage", "41c5d793-399d-4e43-a396-5ae5dd152448");
                label = getLabelIncrementer().incrementLabel(label, buildLog.getContent());
            }
            writeline("/home/ubuntu/results/coverage/Project/Project_4_10.coverage", "fbcd883c-a195-4083-9c55-b9d27a03bf5b");
            // collect project information
            buildLog.addContent(getProjectPropertiesElement(now));
            writeline("/home/ubuntu/results/coverage/Project/Project_4_10.coverage", "99617257-fa88-4570-b551-ad6f9c099eb2");
            setState(ProjectState.BUILDING);
            writeline("/home/ubuntu/results/coverage/Project/Project_4_10.coverage", "3e47664b-a21f-47ac-a89f-346d7aaf9c27");
            final Element builderLog = schedule.build(buildCounter, lastBuild, now, getProjectPropertiesMap(now), target, progress);
            writeline("/home/ubuntu/results/coverage/Project/Project_4_10.coverage", "c9a62499-d705-402c-806b-f7caf5798215");
            buildLog.addContent(builderLog.detach());
            writeline("/home/ubuntu/results/coverage/Project/Project_4_10.coverage", "ac39f63f-0461-49e1-b41e-b086d6b4332f");
            boolean buildSuccessful = buildLog.wasBuildSuccessful();
            writeline("/home/ubuntu/results/coverage/Project/Project_4_10.coverage", "a5f61e98-8f01-42b3-891d-080337a97f87");
            fireResultEvent(new BuildResultEvent(this, buildSuccessful));
            writeline("/home/ubuntu/results/coverage/Project/Project_4_10.coverage", "1712f90f-187e-422d-b0d4-bf3deca8fb89");
            if (!getLabelIncrementer().isPreBuildIncrementer() && buildSuccessful) {
                writeline("/home/ubuntu/results/coverage/Project/Project_4_10.coverage", "e53dcc04-0600-45dd-8300-02f3c7af4647");
                label = getLabelIncrementer().incrementLabel(label, buildLog.getContent());
            }
            writeline("/home/ubuntu/results/coverage/Project/Project_4_10.coverage", "4874c188-05cc-4d3a-860a-1db00fdf993a");
            setState(ProjectState.MERGING_LOGS);
            writeline("/home/ubuntu/results/coverage/Project/Project_4_10.coverage", "8d1fac55-a0b3-493c-a9bc-63078fe8feca");
            buildLog.writeLogFile(now);
            writeline("/home/ubuntu/results/coverage/Project/Project_4_10.coverage", "5cdaa4b7-76d9-4f60-a535-7a2eea67703a");
            // regardless of success or failure (buildAfterFailed = false in config.xml)
            if (!buildAfterFailed) {
                writeline("/home/ubuntu/results/coverage/Project/Project_4_10.coverage", "1fe1c47f-352f-4815-b554-023c636e3da4");
                lastBuild = now;
            }
            writeline("/home/ubuntu/results/coverage/Project/Project_4_10.coverage", "6090ec5f-ec40-4da3-b2c4-3c75f779b772");
            // If this was a successful build, update both last build and last successful build
            if (buildSuccessful) {
                writeline("/home/ubuntu/results/coverage/Project/Project_4_10.coverage", "9955f30c-2bc9-4d6c-8342-032287c9ada6");
                lastBuild = now;
                writeline("/home/ubuntu/results/coverage/Project/Project_4_10.coverage", "e5ce890e-0fbd-4c3c-82cd-303c2b4735bd");
                lastSuccessfulBuild = now;
                writeline("/home/ubuntu/results/coverage/Project/Project_4_10.coverage", "ea364023-d063-47a6-bcc9-627f50422d21");
                info("build successful");
            } else {
                writeline("/home/ubuntu/results/coverage/Project/Project_4_10.coverage", "fe025e3e-1e3d-4e60-9370-8f8dbfaa5066");
                info("build failed");
            }
            writeline("/home/ubuntu/results/coverage/Project/Project_4_10.coverage", "92a924eb-0564-4b03-a112-07cee5d59187");
            buildCounter++;
            writeline("/home/ubuntu/results/coverage/Project/Project_4_10.coverage", "d3602581-d44e-4586-a392-ba8f95ff6791");
            setWasLastBuildSuccessful(buildSuccessful);
            writeline("/home/ubuntu/results/coverage/Project/Project_4_10.coverage", "4500f96f-06d5-421c-b25e-4941c564fd1e");
            // also need to reset forced flag before serializing, unless buildForced var is transient
            // resetBuildForcedOnlyIfBuildWasForced(buildWasForced);
            serializeProject();
            writeline("/home/ubuntu/results/coverage/Project/Project_4_10.coverage", "e4d94d27-9877-467b-903b-f8d0943c29cf");
            // @todo Add Progress param to Publisher API?
            publish(buildLog);
            writeline("/home/ubuntu/results/coverage/Project/Project_4_10.coverage", "5536ce45-7564-423c-a576-8d58c047c584");
            buildLog.reset();
        } finally {
            writeline("/home/ubuntu/results/coverage/Project/Project_4_10.coverage", "50fdb04d-cb07-47a0-970d-b675c0488dfc");
            resetBuildForcedOnlyIfBuildWasForced(buildWasForced);
            writeline("/home/ubuntu/results/coverage/Project/Project_4_10.coverage", "392ab8a7-7cfa-4426-99b6-4e659c618dbc");
            setState(ProjectState.IDLE);
        }
    }

    private String useAndResetBuildTargetIfBuildWasForced(final boolean buildWasForced) {
        writeline("/home/ubuntu/results/coverage/Project/Project_4_10.coverage", "1067e372-39e5-4299-9901-a3af39e69564");
        String target = null;
        writeline("/home/ubuntu/results/coverage/Project/Project_4_10.coverage", "810b644a-2ebc-4d53-a186-c325ab0d156f");
        if (buildWasForced) {
            writeline("/home/ubuntu/results/coverage/Project/Project_4_10.coverage", "44685434-89f7-403f-a361-ca312e7ea695");
            target = buildTarget;
            writeline("/home/ubuntu/results/coverage/Project/Project_4_10.coverage", "d87d77d9-b9f8-4dd9-97bc-0ddef5a19f2c");
            buildTarget = null;
        }
        writeline("/home/ubuntu/results/coverage/Project/Project_4_10.coverage", "658afe90-e3e5-4cf7-b612-7e4c4c5bb14b");
        return target;
    }

    private void resetBuildForcedOnlyIfBuildWasForced(final boolean buildWasForced) {
        writeline("/home/ubuntu/results/coverage/Project/Project_4_10.coverage", "34420f88-790f-40d9-9424-971f43c82ce0");
        if (buildWasForced) {
            writeline("/home/ubuntu/results/coverage/Project/Project_4_10.coverage", "a3f0955d-58d8-48f9-87cf-4400244de8ba");
            buildForced = false;
        }
    }

    void setBuildStartTime(final Date date) {
        writeline("/home/ubuntu/results/coverage/Project/Project_4_10.coverage", "cff09992-6116-4da5-883e-05bc673f8914");
        buildStartTime = date;
    }

    public void run() {
        writeline("/home/ubuntu/results/coverage/Project/Project_4_10.coverage", "0097dc1e-dcb6-4a7d-8088-e3a5df2193aa");
        LOG.info("Project " + name + " started");
        writeline("/home/ubuntu/results/coverage/Project/Project_4_10.coverage", "0d54fc50-9eb0-4b36-9f11-db6bc0168360");
        try {
            writeline("/home/ubuntu/results/coverage/Project/Project_4_10.coverage", "683f3de4-b83b-41fe-8777-faca56451583");
            while (!stopped) {
                writeline("/home/ubuntu/results/coverage/Project/Project_4_10.coverage", "35968c21-8570-4ca2-a0e8-945a1abd4920");
                try {
                    writeline("/home/ubuntu/results/coverage/Project/Project_4_10.coverage", "c489ea12-7c17-4f88-a379-de8827cd344a");
                    waitIfPaused();
                    writeline("/home/ubuntu/results/coverage/Project/Project_4_10.coverage", "6d87588d-c6bd-4ec4-8f2e-9d4e05bcb840");
                    if (!stopped) {
                        writeline("/home/ubuntu/results/coverage/Project/Project_4_10.coverage", "fa05b5ec-8f4d-49c5-b72c-8e743bf8691c");
                        waitForNextBuild();
                    }
                    writeline("/home/ubuntu/results/coverage/Project/Project_4_10.coverage", "cd44b573-0952-416f-bc50-299c132b81b4");
                    if (!stopped) {
                        writeline("/home/ubuntu/results/coverage/Project/Project_4_10.coverage", "dc800dfb-ea15-4120-a434-0102d2050acd");
                        setState(ProjectState.QUEUED);
                        writeline("/home/ubuntu/results/coverage/Project/Project_4_10.coverage", "4c8806c8-1a0e-408f-a51a-f4d820a9601d");
                        synchronized (scheduleMutex) {
                            writeline("/home/ubuntu/results/coverage/Project/Project_4_10.coverage", "b75e5ae2-9f9a-49e6-9217-90d19dfa5f9d");
                            queue.requestBuild(projectConfig);
                            writeline("/home/ubuntu/results/coverage/Project/Project_4_10.coverage", "850e7d68-714f-4703-8bdd-1c13e7204f7b");
                            waitForBuildToFinish();
                        }
                    }
                } catch (InterruptedException e) {
                    writeline("/home/ubuntu/results/coverage/Project/Project_4_10.coverage", "c6db3af6-c454-4e6d-b2c4-d32723bcd6be");
                    final String message = "Project " + name + ".run() interrupted";
                    writeline("/home/ubuntu/results/coverage/Project/Project_4_10.coverage", "773a35dd-13ca-4e93-b925-b247cfa4c102");
                    LOG.error(message, e);
                    writeline("/home/ubuntu/results/coverage/Project/Project_4_10.coverage", "543beb18-eb7d-4575-bb78-3a4474c96a3a");
                    throw new RuntimeException(message);
                }
            }
        } finally {
            writeline("/home/ubuntu/results/coverage/Project/Project_4_10.coverage", "f890e498-1b74-41e0-81d6-09f189708da8");
            stopped = true;
            writeline("/home/ubuntu/results/coverage/Project/Project_4_10.coverage", "d7611bb3-f68c-47ef-8362-2229b3f52db4");
            LOG.info("Project " + name + " stopped");
        }
    }

    void waitIfPaused() throws InterruptedException {
        writeline("/home/ubuntu/results/coverage/Project/Project_4_10.coverage", "68583b9f-4178-4a8b-910e-6594001e7738");
        synchronized (pausedMutex) {
            writeline("/home/ubuntu/results/coverage/Project/Project_4_10.coverage", "cf4eb33d-a2a8-4e7d-9626-2d22884f056a");
            while (isPaused) {
                writeline("/home/ubuntu/results/coverage/Project/Project_4_10.coverage", "3c4c9312-111d-49ac-8c5a-fefe12f583b8");
                setState(ProjectState.PAUSED);
                writeline("/home/ubuntu/results/coverage/Project/Project_4_10.coverage", "bebb92cd-79d3-4b68-aebc-6d8974cf15ff");
                pausedMutex.wait(10 * DateUtil.ONE_MINUTE);
            }
        }
    }

    void waitForNextBuild() throws InterruptedException {
        writeline("/home/ubuntu/results/coverage/Project/Project_4_10.coverage", "dec0c503-e5c4-42ed-9011-5638339f1fdb");
        long waitTime = getTimeToNextBuild(new Date());
        writeline("/home/ubuntu/results/coverage/Project/Project_4_10.coverage", "08afd7ac-b672-401a-a64b-06f65b8c11d0");
        if (needToWaitForNextBuild(waitTime) && !buildForced) {
            writeline("/home/ubuntu/results/coverage/Project/Project_4_10.coverage", "dd9c19a0-4d1f-461c-8f9e-121b7a532763");
            final String msg = "next build in " + DateUtil.formatTime(waitTime);
            writeline("/home/ubuntu/results/coverage/Project/Project_4_10.coverage", "22ac5054-18a3-414d-820c-cb87218b979f");
            info(msg);
            writeline("/home/ubuntu/results/coverage/Project/Project_4_10.coverage", "ec2c9109-5849-4ecc-835f-7adcc8abc88d");
            synchronized (waitMutex) {
                writeline("/home/ubuntu/results/coverage/Project/Project_4_10.coverage", "bb7e3f9d-3073-4f8b-914c-36ac657e8743");
                setState(ProjectState.WAITING);
                writeline("/home/ubuntu/results/coverage/Project/Project_4_10.coverage", "de94affa-16df-4a7f-a946-1a95835ac0fd");
                progress.setValue(msg);
                writeline("/home/ubuntu/results/coverage/Project/Project_4_10.coverage", "3b63de79-0e5c-4185-a868-a07b29c355be");
                waitMutex.wait(waitTime);
            }
        }
    }

    long getTimeToNextBuild(Date now) {
        writeline("/home/ubuntu/results/coverage/Project/Project_4_10.coverage", "d2a3d9a4-0146-4b13-9fcf-a5276030e1d8");
        long waitTime = projectConfig.getSchedule().getTimeToNextBuild(now, getBuildInterval());
        writeline("/home/ubuntu/results/coverage/Project/Project_4_10.coverage", "9ef43f5f-2f45-4f30-9712-b4e771bc457d");
        if (waitTime == 0) {
            writeline("/home/ubuntu/results/coverage/Project/Project_4_10.coverage", "21d1b57c-f2a4-4464-98c6-81279b85b90d");
            // project that has just built within a minute time
            if (buildStartTime != null) {
                writeline("/home/ubuntu/results/coverage/Project/Project_4_10.coverage", "aabe3e77-f645-4b88-9294-fb9d766552b4");
                long millisSinceLastBuild = now.getTime() - buildStartTime.getTime();
                writeline("/home/ubuntu/results/coverage/Project/Project_4_10.coverage", "7715fb85-78c5-483f-8caf-5c85001de6e0");
                if (millisSinceLastBuild < Schedule.ONE_MINUTE) {
                    writeline("/home/ubuntu/results/coverage/Project/Project_4_10.coverage", "d9666931-f690-49d6-9839-0a8fa72d83d1");
                    debug("build finished within a minute, getting new time to next build");
                    writeline("/home/ubuntu/results/coverage/Project/Project_4_10.coverage", "6820088d-9b04-421b-b569-c5d7ab20fab8");
                    Date oneMinuteInFuture = new Date(now.getTime() + Schedule.ONE_MINUTE);
                    writeline("/home/ubuntu/results/coverage/Project/Project_4_10.coverage", "f4ad8511-b69b-4aec-9d0b-1a0c496fcaa4");
                    waitTime = projectConfig.getSchedule().getTimeToNextBuild(oneMinuteInFuture, getBuildInterval());
                    writeline("/home/ubuntu/results/coverage/Project/Project_4_10.coverage", "2467da4b-23f0-4bd4-9238-0014209f8d38");
                    waitTime += Schedule.ONE_MINUTE;
                }
            }
        }
        writeline("/home/ubuntu/results/coverage/Project/Project_4_10.coverage", "2cbe0cdb-167c-47c1-80a1-c66b12766419");
        return waitTime;
    }

    static boolean needToWaitForNextBuild(long waitTime) {
        writelineStatic("/home/ubuntu/results/coverage/Project/Project_4_10.coverage", "2f549d85-365b-4d47-902d-9191356963fa");
        return waitTime > 0;
    }

    /**
     * @return true if build was forced, intended for unit testing only.
     */
    boolean isBuildForced() {
        writeline("/home/ubuntu/results/coverage/Project/Project_4_10.coverage", "60dc5aa2-7a8d-4400-ac55-9bd787a40902");
        return buildForced;
    }

    void forceBuild() {
        writeline("/home/ubuntu/results/coverage/Project/Project_4_10.coverage", "6fbe5aa3-d7fb-4ea3-8f1a-2f2a82ac4716");
        synchronized (waitMutex) {
            writeline("/home/ubuntu/results/coverage/Project/Project_4_10.coverage", "9586f1bf-4e8f-48c0-a408-d01fb7bc7a87");
            waitMutex.notify();
        }
    }

    public void forceBuildWithTarget(String buildTarget) {
        writeline("/home/ubuntu/results/coverage/Project/Project_4_10.coverage", "a15e9bef-3e2d-4020-861e-eaaf77a47d1d");
        this.buildTarget = buildTarget;
        writeline("/home/ubuntu/results/coverage/Project/Project_4_10.coverage", "0d740847-a29e-459c-9305-7e312f1f41ce");
        setBuildForced(true);
    }

    public void forceBuildWithTarget(String buildTarget, Map<String, String> addedProperties) {
        writeline("/home/ubuntu/results/coverage/Project/Project_4_10.coverage", "33b1b86f-10be-4cad-8ca8-e5fd634fdce5");
        additionalProperties = addedProperties;
        writeline("/home/ubuntu/results/coverage/Project/Project_4_10.coverage", "35d19720-e074-4a3d-8909-1f37f1f1f998");
        forceBuildWithTarget(buildTarget);
    }

    void waitForBuildToFinish() throws InterruptedException {
        writeline("/home/ubuntu/results/coverage/Project/Project_4_10.coverage", "f9184206-370c-4152-a844-1a28d96d2df3");
        synchronized (scheduleMutex) {
            writeline("/home/ubuntu/results/coverage/Project/Project_4_10.coverage", "6bf85e88-eee3-4fdd-9517-df9217423b4b");
            debug("waiting for build to finish");
            writeline("/home/ubuntu/results/coverage/Project/Project_4_10.coverage", "cb75a27a-770e-4f29-b179-fef0efd45227");
            scheduleMutex.wait();
        }
    }

    void buildFinished() {
        writeline("/home/ubuntu/results/coverage/Project/Project_4_10.coverage", "c9b41c07-878f-4f61-a801-b120713bd501");
        synchronized (scheduleMutex) {
            writeline("/home/ubuntu/results/coverage/Project/Project_4_10.coverage", "f8ee4943-a487-4f4b-b0ba-b078b1392e67");
            debug("build finished");
            writeline("/home/ubuntu/results/coverage/Project/Project_4_10.coverage", "6bf591e8-9215-470d-9ee9-ad4ace01bb16");
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
        writeline("/home/ubuntu/results/coverage/Project/Project_4_10.coverage", "e2a1789e-b738-473f-9fe5-1d1146050e0e");
        setState(ProjectState.MODIFICATIONSET);
        writeline("/home/ubuntu/results/coverage/Project/Project_4_10.coverage", "394bd248-865c-4257-876f-102cf47f7f90");
        final ModificationSet modificationSet = projectConfig.getModificationSet();
        writeline("/home/ubuntu/results/coverage/Project/Project_4_10.coverage", "c6b3b223-9fab-43ff-b47e-da0044c4fc4c");
        if (modificationSet == null) {
            writeline("/home/ubuntu/results/coverage/Project/Project_4_10.coverage", "89b75dd8-3151-4a3e-8abf-eb49aa948a52");
            debug("no modification set, nothing to detect.");
            writeline("/home/ubuntu/results/coverage/Project/Project_4_10.coverage", "de5b0644-688a-4648-9d93-bd0db992fd48");
            if (buildWasForced) {
                writeline("/home/ubuntu/results/coverage/Project/Project_4_10.coverage", "c5a113b4-68a6-448c-9b96-8dffa3a42f52");
                info("no modification set but build was forced");
                writeline("/home/ubuntu/results/coverage/Project/Project_4_10.coverage", "efb33f69-f226-435f-8a2a-c5949b8d4a52");
                return new Element("modifications");
            }
            writeline("/home/ubuntu/results/coverage/Project/Project_4_10.coverage", "fc975cfd-4503-4b95-9b2b-373d99700ee7");
            if (!requiremodification) {
                writeline("/home/ubuntu/results/coverage/Project/Project_4_10.coverage", "89d9ecb9-2b8a-46ef-afed-8e5297147b00");
                info("no modification set but no modifications required");
                writeline("/home/ubuntu/results/coverage/Project/Project_4_10.coverage", "76db77f1-bf1b-497b-8398-229e19059df5");
                return new Element("modifications");
            }
            writeline("/home/ubuntu/results/coverage/Project/Project_4_10.coverage", "8abf64aa-0068-45e7-a86b-09f3c5ed05a9");
            return null;
        }
        writeline("/home/ubuntu/results/coverage/Project/Project_4_10.coverage", "534a04d9-4440-406a-a2c1-49e79e7853ac");
        final boolean checkNewChangesFirst = checkOnlySinceLastBuild();
        writeline("/home/ubuntu/results/coverage/Project/Project_4_10.coverage", "827e880f-dc0c-4399-8e5d-6835bbc1f555");
        Element modifications;
        writeline("/home/ubuntu/results/coverage/Project/Project_4_10.coverage", "1d05b3f8-b3f6-49b2-a4ff-33dd8e822fad");
        if (checkNewChangesFirst) {
            writeline("/home/ubuntu/results/coverage/Project/Project_4_10.coverage", "3da3290f-8713-41b3-9ed8-f532f34c2e21");
            debug("getting changes since last build");
            writeline("/home/ubuntu/results/coverage/Project/Project_4_10.coverage", "cda69358-e880-4a6d-9fef-c680cdd17658");
            modifications = modificationSet.retrieveModificationsAsElement(lastBuild, progress);
        } else {
            writeline("/home/ubuntu/results/coverage/Project/Project_4_10.coverage", "cc15a74c-f5f9-490a-82b7-0904ed079a91");
            debug("getting changes since last successful build");
            writeline("/home/ubuntu/results/coverage/Project/Project_4_10.coverage", "37c8dcb5-635a-40f6-b06f-7ab9c23cd268");
            modifications = modificationSet.retrieveModificationsAsElement(lastSuccessfulBuild, progress);
        }
        writeline("/home/ubuntu/results/coverage/Project/Project_4_10.coverage", "6aeb455d-a293-4a8a-832e-13f96069a9b2");
        if (!modificationSet.isModified()) {
            writeline("/home/ubuntu/results/coverage/Project/Project_4_10.coverage", "4dd96432-ec47-4e74-a583-2bca1c7d3189");
            info("No modifications found, build not necessary.");
            writeline("/home/ubuntu/results/coverage/Project/Project_4_10.coverage", "c0566311-eb63-4f04-8f08-cfd740c8bbd3");
            // * build forced
            if (buildAfterFailed && !wasLastBuildSuccessful) {
                writeline("/home/ubuntu/results/coverage/Project/Project_4_10.coverage", "cdad62d1-1942-44cb-88ef-06da6cc8175b");
                info("Building anyway, since buildAfterFailed is true and last build failed.");
            } else if (!requiremodification) {
                writeline("/home/ubuntu/results/coverage/Project/Project_4_10.coverage", "3271b09c-d829-4eb2-972b-3d6700f5a8b0");
                info("Building anyway, since modifications not required");
            } else {
                writeline("/home/ubuntu/results/coverage/Project/Project_4_10.coverage", "59aef0bd-63d5-4b86-b3a9-1bdaca674f43");
                if (buildWasForced) {
                    writeline("/home/ubuntu/results/coverage/Project/Project_4_10.coverage", "edbbee12-b68a-4ef8-87c0-82cfff8f0539");
                    info("Building anyway, since build was explicitly forced.");
                } else {
                    writeline("/home/ubuntu/results/coverage/Project/Project_4_10.coverage", "17315f83-70f6-4991-a9c6-513b4c4a39cf");
                    return null;
                }
            }
        }
        writeline("/home/ubuntu/results/coverage/Project/Project_4_10.coverage", "0513d4e5-43d7-4e05-be66-9f60e93d65c8");
        if (checkNewChangesFirst) {
            writeline("/home/ubuntu/results/coverage/Project/Project_4_10.coverage", "ee91aa8e-1c78-460b-a8d1-4d96ceec989a");
            debug("new changes found; now getting complete set");
            writeline("/home/ubuntu/results/coverage/Project/Project_4_10.coverage", "26340674-787d-4381-a258-1bf961143112");
            modifications = modificationSet.retrieveModificationsAsElement(lastSuccessfulBuild, progress);
        }
        writeline("/home/ubuntu/results/coverage/Project/Project_4_10.coverage", "a5d50f58-be22-44ed-97b4-23a94f2f8cec");
        return modifications;
    }

    /**
     * @return boolean
     */
    boolean checkOnlySinceLastBuild() {
        writeline("/home/ubuntu/results/coverage/Project/Project_4_10.coverage", "2016cf3c-6c3c-43ee-ab26-754e8e629959");
        if (lastBuild == null || lastSuccessfulBuild == null) {
            writeline("/home/ubuntu/results/coverage/Project/Project_4_10.coverage", "cf5bf776-42cd-4f88-bfbc-df633a8c6f91");
            return false;
        }
        writeline("/home/ubuntu/results/coverage/Project/Project_4_10.coverage", "450baa6f-6255-4d18-8d6b-6d4f1ad51e12");
        final long lastBuildLong = lastBuild.getTime();
        writeline("/home/ubuntu/results/coverage/Project/Project_4_10.coverage", "6287cd6c-589f-4559-8b1c-78e4f7a5be24");
        final long timeDifference = lastBuildLong - lastSuccessfulBuild.getTime();
        writeline("/home/ubuntu/results/coverage/Project/Project_4_10.coverage", "e9ff47cd-6129-4d0b-87f0-b77e92e0c03f");
        final boolean moreThanASecond = timeDifference > DateUtil.ONE_SECOND;
        writeline("/home/ubuntu/results/coverage/Project/Project_4_10.coverage", "a290c02b-c97f-4410-9cbb-7f2139363832");
        return !buildAfterFailed && moreThanASecond;
    }

    /**
     * Serialize the project to allow resumption after a process bounce
     */
    public void serializeProject() {
        writeline("/home/ubuntu/results/coverage/Project/Project_4_10.coverage", "5e171d40-6ddb-4a73-b853-c42db74610f5");
        final String safeProjectName = Builder.getFileSystemSafeProjectName(name);
        writeline("/home/ubuntu/results/coverage/Project/Project_4_10.coverage", "8b864893-d54f-4d33-9bc0-7ee09d0edc87");
        try {
            writeline("/home/ubuntu/results/coverage/Project/Project_4_10.coverage", "762cd51e-11b9-48b0-a42d-24e01804c30d");
            final ObjectOutputStream s = new ObjectOutputStream(new FileOutputStream(safeProjectName + ".ser"));
            writeline("/home/ubuntu/results/coverage/Project/Project_4_10.coverage", "b5654c3a-8fc4-4cc5-b299-9a9a70c82a2a");
            try {
                writeline("/home/ubuntu/results/coverage/Project/Project_4_10.coverage", "97281801-bd48-4651-ad3d-b407de11e4a0");
                s.writeObject(this);
                writeline("/home/ubuntu/results/coverage/Project/Project_4_10.coverage", "68315861-97ff-46e5-8482-f4f119626fd5");
                s.flush();
                writeline("/home/ubuntu/results/coverage/Project/Project_4_10.coverage", "25dbeca7-6f8a-4874-865b-f0119a785321");
                debug("Serializing project to [" + safeProjectName + ".ser]");
            } finally {
                writeline("/home/ubuntu/results/coverage/Project/Project_4_10.coverage", "1c8867d5-d678-4e41-b03f-df647fcdd0af");
                s.close();
            }
        } catch (Exception e) {
            writeline("/home/ubuntu/results/coverage/Project/Project_4_10.coverage", "75b4a928-d675-4f7f-9aca-14ebc3c761af");
            LOG.warn("Error serializing project to [" + safeProjectName + ".ser]: " + e.getMessage(), e);
        }
    }

    public void setLabelIncrementer(final LabelIncrementer incrementer) throws CruiseControlException {
        writeline("/home/ubuntu/results/coverage/Project/Project_4_10.coverage", "6d3f317a-4a75-4205-a9df-eca07fe0d8c0");
        if (incrementer == null) {
            writeline("/home/ubuntu/results/coverage/Project/Project_4_10.coverage", "670410df-1fdb-47ac-9a44-e014798b7b94");
            throw new IllegalArgumentException("label incrementer can't be null");
        }
        writeline("/home/ubuntu/results/coverage/Project/Project_4_10.coverage", "2e357bad-9f50-47cb-978c-1c7a4e2c9fe1");
        labelIncrementer = incrementer;
        writeline("/home/ubuntu/results/coverage/Project/Project_4_10.coverage", "522a9616-2d79-46e7-afb5-8b93d271f734");
        if (label == null) {
            writeline("/home/ubuntu/results/coverage/Project/Project_4_10.coverage", "2219a2db-315e-45e3-9b48-1eeb5a0bdffa");
            label = labelIncrementer.getDefaultLabel();
        }
        writeline("/home/ubuntu/results/coverage/Project/Project_4_10.coverage", "64a32aa0-0c48-45f7-a0ec-f0ac28662ced");
        validateLabel(label, labelIncrementer);
    }

    public LabelIncrementer getLabelIncrementer() {
        writeline("/home/ubuntu/results/coverage/Project/Project_4_10.coverage", "f6f2f5ee-9cd5-430d-9552-6807ce20beff");
        return labelIncrementer;
    }

    public void setName(final String projectName) {
        writeline("/home/ubuntu/results/coverage/Project/Project_4_10.coverage", "ec87d24e-bcb9-4d3e-b46c-f9f31695f513");
        name = projectName;
    }

    public String getName() {
        writeline("/home/ubuntu/results/coverage/Project/Project_4_10.coverage", "075c130a-577a-4c85-9a22-fa835faa0324");
        return name;
    }

    public void setLabel(final String newLabel) {
        writeline("/home/ubuntu/results/coverage/Project/Project_4_10.coverage", "82059169-1be3-4128-86ee-eff7db98bd10");
        label = newLabel;
    }

    public String getLabel() {
        writeline("/home/ubuntu/results/coverage/Project/Project_4_10.coverage", "7750afe7-2a69-4d8f-8b00-bdcada23e780");
        return label;
    }

    /**
     * @param newLastBuild string containing the build date in the format
     * yyyyMMddHHmmss
     * @throws CruiseControlException if the date cannot be extracted from the
     * input string
     */
    public void setLastBuild(final String newLastBuild) throws CruiseControlException {
        writeline("/home/ubuntu/results/coverage/Project/Project_4_10.coverage", "004bbf24-bed7-4045-af66-b581a5f59d5e");
        lastBuild = DateUtil.parseFormattedTime(newLastBuild, "lastBuild");
    }

    /**
     * @param newLastSuccessfulBuild string containing the build date in the format
     * yyyyMMddHHmmss
     * @throws CruiseControlException if the date cannot be extracted from the
     * input string
     */
    public void setLastSuccessfulBuild(final String newLastSuccessfulBuild) throws CruiseControlException {
        writeline("/home/ubuntu/results/coverage/Project/Project_4_10.coverage", "8604e3bc-ce9d-463a-a23d-799fea2cd57e");
        lastSuccessfulBuild = DateUtil.parseFormattedTime(newLastSuccessfulBuild, "lastSuccessfulBuild");
    }

    public String getLastBuild() {
        writeline("/home/ubuntu/results/coverage/Project/Project_4_10.coverage", "461871a6-ad3a-4d31-89b9-3530a3c00bf4");
        if (lastBuild == null) {
            writeline("/home/ubuntu/results/coverage/Project/Project_4_10.coverage", "ff415bc9-a39b-4be3-bf07-cde1c0c013c4");
            return null;
        }
        writeline("/home/ubuntu/results/coverage/Project/Project_4_10.coverage", "c6477776-dbf9-4977-ae92-85d1b5475414");
        return DateUtil.getFormattedTime(lastBuild);
    }

    public boolean getBuildForced() {
        writeline("/home/ubuntu/results/coverage/Project/Project_4_10.coverage", "4accfbe5-6a03-4e96-80b1-7af2d6fc4bd4");
        return buildForced;
    }

    public void setBuildForced(boolean forceNewBuildNow) {
        writeline("/home/ubuntu/results/coverage/Project/Project_4_10.coverage", "ed534e40-11d5-4ddf-9cc8-5c61f9fbb009");
        buildForced = forceNewBuildNow;
        writeline("/home/ubuntu/results/coverage/Project/Project_4_10.coverage", "5c0ebaf7-99bf-460b-bc0d-e26775da5da6");
        if (forceNewBuildNow) {
            writeline("/home/ubuntu/results/coverage/Project/Project_4_10.coverage", "7c9853f1-9a5d-4e63-b119-b78878dfe3d5");
            forceBuild();
        }
    }

    public String getLastSuccessfulBuild() {
        writeline("/home/ubuntu/results/coverage/Project/Project_4_10.coverage", "1aeb997b-de26-4cc8-b776-3a47dfece53a");
        if (lastSuccessfulBuild == null) {
            writeline("/home/ubuntu/results/coverage/Project/Project_4_10.coverage", "b0b89aae-1e4b-4fbf-9790-8f99bf2d2d67");
            return null;
        }
        writeline("/home/ubuntu/results/coverage/Project/Project_4_10.coverage", "c0d9ede1-583f-4f8c-a1e0-bb8d4db391d2");
        return DateUtil.getFormattedTime(lastSuccessfulBuild);
    }

    public String getLogDir() {
        writeline("/home/ubuntu/results/coverage/Project/Project_4_10.coverage", "77ec1ac1-09e3-4c61-bbf3-4b9d5dd021c6");
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
        writeline("/home/ubuntu/results/coverage/Project/Project_4_10.coverage", "70679bf6-f5ae-48da-8d69-d5c5a0582c88");
        if (overrideBuildInterval == null) {
            writeline("/home/ubuntu/results/coverage/Project/Project_4_10.coverage", "72ca48cf-b630-4d1d-8e5b-d6acea2acc46");
            return projectConfig.getSchedule().getInterval();
        } else {
            writeline("/home/ubuntu/results/coverage/Project/Project_4_10.coverage", "bf7880b7-4c24-48ee-956c-28c2bf63032f");
            return overrideBuildInterval;
        }
    }

    /**
     * Sets the build interval that this Project should use. This method
     * overrides the value initially specified in the Schedule attribute.
     * @param sleepMillis the number of milliseconds to sleep between build attempts
     */
    public void overrideBuildInterval(final long sleepMillis) {
        writeline("/home/ubuntu/results/coverage/Project/Project_4_10.coverage", "17a2ac6e-f97e-4041-af96-9836ff66bb74");
        overrideBuildInterval = sleepMillis;
    }

    public boolean isPaused() {
        writeline("/home/ubuntu/results/coverage/Project/Project_4_10.coverage", "1f6d6a17-d5be-4c42-b09c-fd32c371556c");
        return isPaused;
    }

    public void setPaused(final boolean paused) {
        writeline("/home/ubuntu/results/coverage/Project/Project_4_10.coverage", "5eacaa44-52d6-436d-803c-73594d094081");
        synchronized (pausedMutex) {
            writeline("/home/ubuntu/results/coverage/Project/Project_4_10.coverage", "cf9fd7d0-e912-4214-92d5-651f21bbe7e3");
            if (isPaused && !paused) {
                writeline("/home/ubuntu/results/coverage/Project/Project_4_10.coverage", "8b5fbf15-47d5-4e60-b669-2071df1b3967");
                pausedMutex.notifyAll();
            }
            writeline("/home/ubuntu/results/coverage/Project/Project_4_10.coverage", "15227b97-754c-44ea-b454-10165d80f920");
            isPaused = paused;
        }
    }

    public void setBuildAfterFailed(final boolean rebuildEvenWithNoNewModifications) {
        writeline("/home/ubuntu/results/coverage/Project/Project_4_10.coverage", "90ebb351-12a6-44a7-aee0-19905ac842c2");
        buildAfterFailed = rebuildEvenWithNoNewModifications;
    }

    public String getStatus() {
        writeline("/home/ubuntu/results/coverage/Project/Project_4_10.coverage", "d6941c5a-77a0-4b1c-9d29-f5a3606e87c3");
        return getState().getDescription();
    }

    public String getStatusWithQueuePosition() {
        writeline("/home/ubuntu/results/coverage/Project/Project_4_10.coverage", "d057f64a-5f46-4c18-b02a-5d80067ccc8e");
        if (ProjectState.QUEUED.equals(getState())) {
            writeline("/home/ubuntu/results/coverage/Project/Project_4_10.coverage", "99bd73d0-4fbc-4ff0-aa1f-e70217aaf32b");
            return getState().getDescription() + " - " + queue.findPosition(projectConfig);
        } else {
            writeline("/home/ubuntu/results/coverage/Project/Project_4_10.coverage", "172328b0-a3c5-4e4b-8b56-24748c3b75af");
            return getState().getDescription();
        }
    }

    public ProjectState getState() {
        writeline("/home/ubuntu/results/coverage/Project/Project_4_10.coverage", "ce8a913c-bfda-423d-b777-e4635da4af9d");
        return state;
    }

    private void setState(final ProjectState newState) {
        writeline("/home/ubuntu/results/coverage/Project/Project_4_10.coverage", "49c655ad-8d7d-40e7-b7aa-97f30618d2f9");
        state = newState;
        writeline("/home/ubuntu/results/coverage/Project/Project_4_10.coverage", "05ff5bc5-a0a8-4221-b814-4a0fed26dee6");
        info(getStatus());
        writeline("/home/ubuntu/results/coverage/Project/Project_4_10.coverage", "464707eb-f86c-433b-8fbe-521d25a92600");
        notifyListeners(new ProjectStateChangedEvent(name, getState()));
        writeline("/home/ubuntu/results/coverage/Project/Project_4_10.coverage", "c4cbec6f-1a42-4681-8822-dfe9eb629873");
        fireProgressEvent(new BuildProgressEvent(this, getState()));
    }

    public void setBuildQueue(final BuildQueue buildQueue) {
        writeline("/home/ubuntu/results/coverage/Project/Project_4_10.coverage", "c0359b9c-bb90-421c-a71f-05f4aca8b339");
        queue = buildQueue;
    }

    public String getBuildStartTime() {
        writeline("/home/ubuntu/results/coverage/Project/Project_4_10.coverage", "5e3cad66-098a-43e2-837c-e9f322f1dfa4");
        return DateUtil.getFormattedTime(buildStartTime);
    }

    public Log getLog() {
        writeline("/home/ubuntu/results/coverage/Project/Project_4_10.coverage", "cd7a71fb-72a6-4c9c-804a-5f596464d6d0");
        return this.projectConfig.getLog();
    }

    /**
     * Initialize the project. Uses ProjectXMLHelper to parse a project file.
     */
    protected void init() {
        writeline("/home/ubuntu/results/coverage/Project/Project_4_10.coverage", "70efdf42-170f-4363-bc03-46ef6c0ba97c");
        if (projectConfig == null) {
            writeline("/home/ubuntu/results/coverage/Project/Project_4_10.coverage", "fdbc465e-d226-415b-86c9-bfc1a7c1c5bb");
            throw new IllegalStateException("projectConfig must be set on project before calling init()");
        }
        writeline("/home/ubuntu/results/coverage/Project/Project_4_10.coverage", "d2f70aec-76ee-47f8-a4e9-da8854768377");
        buildAfterFailed = projectConfig.shouldBuildAfterFailed();
        writeline("/home/ubuntu/results/coverage/Project/Project_4_10.coverage", "ab9a651c-ce66-4665-8a6c-91eaa7aa594b");
        forceOnly = projectConfig.isForceOnly();
        writeline("/home/ubuntu/results/coverage/Project/Project_4_10.coverage", "994e4364-6307-43f6-9d1f-a2386248d4a5");
        requiremodification = projectConfig.isRequiremodification();
        writeline("/home/ubuntu/results/coverage/Project/Project_4_10.coverage", "1a023be8-97b2-4198-a6bb-c9b613c96752");
        if (lastBuild == null) {
            writeline("/home/ubuntu/results/coverage/Project/Project_4_10.coverage", "681d156a-86f3-4d87-81f7-ca656af2decf");
            lastBuild = DateUtil.getMidnight();
        }
        writeline("/home/ubuntu/results/coverage/Project/Project_4_10.coverage", "41b58f49-862f-4aeb-a548-84c30f739633");
        if (lastSuccessfulBuild == null) {
            writeline("/home/ubuntu/results/coverage/Project/Project_4_10.coverage", "23f749a9-aaab-41c2-9174-a5df8c52c7e4");
            lastSuccessfulBuild = lastBuild;
        }
        writeline("/home/ubuntu/results/coverage/Project/Project_4_10.coverage", "6e270a61-1bb0-44bc-8dd9-41b418ebff1d");
        if (LOG.isDebugEnabled()) {
            writeline("/home/ubuntu/results/coverage/Project/Project_4_10.coverage", "43ac20f9-1b18-4e89-a7d0-648a6dd7e0e6");
            debug("buildInterval          = [" + getBuildInterval() + "]");
            writeline("/home/ubuntu/results/coverage/Project/Project_4_10.coverage", "561be6c3-a037-421a-9dc2-cfe46f3646be");
            debug("buildForced            = [" + buildForced + "]");
            writeline("/home/ubuntu/results/coverage/Project/Project_4_10.coverage", "6923d995-a94d-4b16-9c63-5f26153ebca8");
            debug("buildAfterFailed       = [" + buildAfterFailed + "]");
            writeline("/home/ubuntu/results/coverage/Project/Project_4_10.coverage", "ade0e6c9-6812-402b-ac87-97c8b2257fb0");
            debug("requireModifcation     = [" + requiremodification + "]");
            writeline("/home/ubuntu/results/coverage/Project/Project_4_10.coverage", "5f2fdf7e-f15c-48e1-b18e-d5a24ecf2bf1");
            debug("forceOnly              = [" + forceOnly + "]");
            writeline("/home/ubuntu/results/coverage/Project/Project_4_10.coverage", "280f6ab7-b871-4315-a0c9-87551c687baa");
            debug("buildCounter           = [" + buildCounter + "]");
            writeline("/home/ubuntu/results/coverage/Project/Project_4_10.coverage", "9b52ce32-44be-49c3-97e2-e62fc6ba6f84");
            debug("isPaused               = [" + isPaused + "]");
            writeline("/home/ubuntu/results/coverage/Project/Project_4_10.coverage", "404eaf2b-b33f-4756-9e7a-bc7a39a8801d");
            debug("label                  = [" + label + "]");
            writeline("/home/ubuntu/results/coverage/Project/Project_4_10.coverage", "18347e22-e036-420b-904d-39b630f57854");
            debug("lastBuild              = [" + DateUtil.getFormattedTime(lastBuild) + "]");
            writeline("/home/ubuntu/results/coverage/Project/Project_4_10.coverage", "c92bd056-c2d6-4597-9fd2-cb4776cc5162");
            debug("lastSuccessfulBuild    = [" + DateUtil.getFormattedTime(lastSuccessfulBuild) + "]");
            writeline("/home/ubuntu/results/coverage/Project/Project_4_10.coverage", "58e7242f-ef52-4a3f-af2b-4cae2fc8a4b8");
            debug("logDir                 = [" + projectConfig.getLog().getLogDir() + "]");
            writeline("/home/ubuntu/results/coverage/Project/Project_4_10.coverage", "64252a5d-e4c4-4267-8000-303eff2b2b5e");
            debug("logXmlEncoding         = [" + projectConfig.getLog().getLogXmlEncoding() + "]");
            writeline("/home/ubuntu/results/coverage/Project/Project_4_10.coverage", "7925da5d-8569-41ae-9033-b493760c517d");
            debug("wasLastBuildSuccessful = [" + wasLastBuildSuccessful + "]");
        }
    }

    protected Element getProjectPropertiesElement(final Date now) {
        writeline("/home/ubuntu/results/coverage/Project/Project_4_10.coverage", "6d3bee50-232a-4a97-b43a-d264ca083ea1");
        final Element infoElement = new Element("info");
        writeline("/home/ubuntu/results/coverage/Project/Project_4_10.coverage", "87af6e73-fbee-46ff-8f5e-76bd962a2541");
        addProperty(infoElement, "projectname", name);
        writeline("/home/ubuntu/results/coverage/Project/Project_4_10.coverage", "f6a7bac8-fe3c-40ce-a6bd-41f287633cba");
        final String lastBuildString = DateUtil.getFormattedTime(lastBuild == null ? now : lastBuild);
        writeline("/home/ubuntu/results/coverage/Project/Project_4_10.coverage", "a46c396e-b53d-4481-be0d-9a65edc1e27d");
        addProperty(infoElement, "lastbuild", lastBuildString);
        writeline("/home/ubuntu/results/coverage/Project/Project_4_10.coverage", "9bfbf053-be92-487a-8ed9-dc77db469702");
        final String lastSuccessfulBuildString = DateUtil.getFormattedTime(lastSuccessfulBuild == null ? now : lastSuccessfulBuild);
        writeline("/home/ubuntu/results/coverage/Project/Project_4_10.coverage", "88439f8e-ce9c-427d-ab40-0ca7ae041486");
        addProperty(infoElement, "lastsuccessfulbuild", lastSuccessfulBuildString);
        writeline("/home/ubuntu/results/coverage/Project/Project_4_10.coverage", "ea90ce9a-6f51-4fab-8746-f5b6b59b9a7f");
        addProperty(infoElement, "builddate", DateUtil.formatIso8601(now));
        writeline("/home/ubuntu/results/coverage/Project/Project_4_10.coverage", "6988e9dc-bc7f-4cab-a367-593cc1244d8f");
        addProperty(infoElement, "cctimestamp", DateUtil.getFormattedTime(now));
        writeline("/home/ubuntu/results/coverage/Project/Project_4_10.coverage", "699ebc81-2c69-4acd-a906-7dcf016f0548");
        addProperty(infoElement, "label", label);
        writeline("/home/ubuntu/results/coverage/Project/Project_4_10.coverage", "ca87af3b-34cb-475c-93a1-9cf29106d2f9");
        addProperty(infoElement, "interval", Long.toString(getBuildInterval() / 1000L));
        writeline("/home/ubuntu/results/coverage/Project/Project_4_10.coverage", "7a706511-8867-4aae-8fe9-fc62a15a3723");
        addProperty(infoElement, "lastbuildsuccessful", String.valueOf(wasLastBuildSuccessful));
        writeline("/home/ubuntu/results/coverage/Project/Project_4_10.coverage", "ebbfcee9-7591-4ea2-b153-19dfa97d474b");
        return infoElement;
    }

    private void addProperty(final Element parent, final String key, final String value) {
        writeline("/home/ubuntu/results/coverage/Project/Project_4_10.coverage", "254d538d-bccd-4118-af5e-a4db319a2b4f");
        final Element propertyElement = new Element("property");
        writeline("/home/ubuntu/results/coverage/Project/Project_4_10.coverage", "bb561b93-9981-42eb-bfcf-6a03026b8a1c");
        propertyElement.setAttribute("name", key);
        writeline("/home/ubuntu/results/coverage/Project/Project_4_10.coverage", "9294315f-7f6b-404c-ba60-a1f097f3c189");
        propertyElement.setAttribute("value", value);
        writeline("/home/ubuntu/results/coverage/Project/Project_4_10.coverage", "0a972408-3813-45a6-a702-dd82834569a3");
        parent.addContent(propertyElement);
    }

    protected Map<String, String> getProjectPropertiesMap(final Date now) {
        writeline("/home/ubuntu/results/coverage/Project/Project_4_10.coverage", "c9f16a64-d2ab-45d5-93de-4acfbc91a558");
        final Map<String, String> buildProperties = new HashMap<String, String>();
        writeline("/home/ubuntu/results/coverage/Project/Project_4_10.coverage", "de73d803-475a-492e-9e3d-72d74f974278");
        buildProperties.put("projectname", name);
        writeline("/home/ubuntu/results/coverage/Project/Project_4_10.coverage", "d74965e5-0654-4b4c-ad84-6c493f7f92c8");
        buildProperties.put("label", label);
        writeline("/home/ubuntu/results/coverage/Project/Project_4_10.coverage", "fb9c9e22-8690-4361-b47e-28c96eb8d172");
        // TODO: Shouldn't have CVS specific properties here
        buildProperties.put("cvstimestamp", CVSDateUtil.formatCVSDate(now));
        writeline("/home/ubuntu/results/coverage/Project/Project_4_10.coverage", "b189e701-1ca7-4045-afae-934bfc4600b2");
        buildProperties.put("cctimestamp", DateUtil.getFormattedTime(now));
        writeline("/home/ubuntu/results/coverage/Project/Project_4_10.coverage", "28dd947a-cf75-4516-abbd-8f80704e69ea");
        buildProperties.put("cclastgoodbuildtimestamp", getLastSuccessfulBuild());
        writeline("/home/ubuntu/results/coverage/Project/Project_4_10.coverage", "dabd51d4-a5c0-4e71-aaf6-1d8fc726486e");
        buildProperties.put("cclastbuildtimestamp", getLastBuild());
        writeline("/home/ubuntu/results/coverage/Project/Project_4_10.coverage", "37d27894-09dd-4682-9b8c-b342c2cac452");
        buildProperties.put("lastbuildsuccessful", String.valueOf(isLastBuildSuccessful()));
        writeline("/home/ubuntu/results/coverage/Project/Project_4_10.coverage", "50c5fcba-23a6-4673-a401-834b4fe04edc");
        buildProperties.put("buildforced", String.valueOf(getBuildForced()));
        writeline("/home/ubuntu/results/coverage/Project/Project_4_10.coverage", "f7cc5d3f-e3ec-47cd-9672-68de17d96fc7");
        if (projectConfig.getModificationSet() != null) {
            writeline("/home/ubuntu/results/coverage/Project/Project_4_10.coverage", "36d41e5c-97f9-40d9-81d9-712f61b5bbe1");
            buildProperties.putAll(projectConfig.getModificationSet().getProperties());
        }
        writeline("/home/ubuntu/results/coverage/Project/Project_4_10.coverage", "53869ce2-efbb-47fd-9b96-755b3f0e7479");
        if (additionalProperties != null && !additionalProperties.isEmpty()) {
            writeline("/home/ubuntu/results/coverage/Project/Project_4_10.coverage", "64528751-6dfa-40dc-9a1b-ce146f78c827");
            buildProperties.putAll(additionalProperties);
            writeline("/home/ubuntu/results/coverage/Project/Project_4_10.coverage", "8ae53d95-f1f8-4925-a94a-d0b8f87dc354");
            additionalProperties.clear();
            writeline("/home/ubuntu/results/coverage/Project/Project_4_10.coverage", "6c09d6d8-f516-4c9d-a203-00d94c29c0aa");
            additionalProperties = null;
        }
        writeline("/home/ubuntu/results/coverage/Project/Project_4_10.coverage", "05d89917-58ef-4140-9414-5775a14d6700");
        return buildProperties;
    }

    /**
     * Intended only for unit testing.
     * @return additional Properties variable.
     */
    Map<String, String> getAdditionalProperties() {
        writeline("/home/ubuntu/results/coverage/Project/Project_4_10.coverage", "d5054e56-3711-459b-9c19-d66f94d0a601");
        return additionalProperties;
    }

    /**
     * Iterate over all of the registered <code>Publisher</code>s and call
     * their respective <code>publish</code> methods.
     * @param buildLog the content to publish
     * @throws CruiseControlException if an error occurs during publishing
     */
    protected void publish(final Log buildLog) throws CruiseControlException {
        writeline("/home/ubuntu/results/coverage/Project/Project_4_10.coverage", "8a9eba82-bd06-4723-b1d1-8f665e2526ac");
        setState(ProjectState.PUBLISHING);
        writeline("/home/ubuntu/results/coverage/Project/Project_4_10.coverage", "f417beb6-9008-42e9-b987-d4b9ae154b2a");
        for (final Publisher publisher : projectConfig.getPublishers()) {
            writeline("/home/ubuntu/results/coverage/Project/Project_4_10.coverage", "6498b85c-a361-47be-8336-32891fb4fc95");
            // catch all errors, Publishers shouldn't cause failures in the build method
            try {
                writeline("/home/ubuntu/results/coverage/Project/Project_4_10.coverage", "3092cee3-22d4-4b76-912f-1e50d27df165");
                publisher.publish(buildLog.getContent());
            } catch (Throwable t) {
                writeline("/home/ubuntu/results/coverage/Project/Project_4_10.coverage", "4735f355-5c7a-43fa-91d5-d099df42afc3");
                final StringBuilder message = new StringBuilder("exception publishing results");
                writeline("/home/ubuntu/results/coverage/Project/Project_4_10.coverage", "53ae3a14-fa98-448a-96b2-1659876efe4b");
                message.append(" with ").append(publisher.getClass().getName());
                writeline("/home/ubuntu/results/coverage/Project/Project_4_10.coverage", "640c47b0-049c-4cf7-b95b-f52aab9a7ec0");
                message.append(" for project ").append(name);
                writeline("/home/ubuntu/results/coverage/Project/Project_4_10.coverage", "47fd670e-6327-47d4-843e-9c73c97e2b5c");
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
        writeline("/home/ubuntu/results/coverage/Project/Project_4_10.coverage", "5d64c117-7237-491d-afad-d86a2656b41a");
        setState(ProjectState.BOOTSTRAPPING);
        writeline("/home/ubuntu/results/coverage/Project/Project_4_10.coverage", "e1c31ba1-95a2-41dc-9bc0-68d3fbec9c83");
        for (final Bootstrapper bootstrapper : projectConfig.getBootstrappers()) {
            writeline("/home/ubuntu/results/coverage/Project/Project_4_10.coverage", "a5b56caa-6992-414f-9652-459c7aa0f2b2");
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
        writeline("/home/ubuntu/results/coverage/Project/Project_4_10.coverage", "71ec2cfc-5039-4e9b-bb05-b6c9463255ea");
        if (!incrementer.isValidLabel(oldLabel)) {
            writeline("/home/ubuntu/results/coverage/Project/Project_4_10.coverage", "e6336382-9374-4418-b06f-4874c9030459");
            final String message = oldLabel + " is not a valid label for labelIncrementer " + incrementer.getClass().getName();
            writeline("/home/ubuntu/results/coverage/Project/Project_4_10.coverage", "29eb8108-a754-432f-8166-379cedd0651d");
            debug(message);
            writeline("/home/ubuntu/results/coverage/Project/Project_4_10.coverage", "69204cc8-1582-49ba-a75f-e8486ce9b6e2");
            throw new CruiseControlException(message);
        }
    }

    public boolean isLastBuildSuccessful() {
        writeline("/home/ubuntu/results/coverage/Project/Project_4_10.coverage", "6bdf19f2-a821-4dc5-aa34-48d75a805202");
        return wasLastBuildSuccessful;
    }

    void setWasLastBuildSuccessful(final boolean buildSuccessful) {
        writeline("/home/ubuntu/results/coverage/Project/Project_4_10.coverage", "184c2d2b-1846-488a-a074-c638ab639ec4");
        wasLastBuildSuccessful = buildSuccessful;
    }

    /**
     * Logs a message to the application log, not to be confused with the
     * CruiseControl build log.
     * @param message the application message to log
     */
    private void info(final String message) {
        writeline("/home/ubuntu/results/coverage/Project/Project_4_10.coverage", "0ed0ba8d-a8ca-4324-87ac-f3659f450a1d");
        LOG.info("Project " + name + ":  " + message);
    }

    private void debug(final String message) {
        writeline("/home/ubuntu/results/coverage/Project/Project_4_10.coverage", "24b8a410-e51e-462c-884f-aeddc899c5b7");
        LOG.debug("Project " + name + ":  " + message);
    }

    public void start() {
        writeline("/home/ubuntu/results/coverage/Project/Project_4_10.coverage", "a6285feb-266a-4307-8cc6-268cd00a6845");
        if (stopped || getState() == ProjectState.STOPPED) {
            writeline("/home/ubuntu/results/coverage/Project/Project_4_10.coverage", "100e9292-0327-4199-804e-f59709d38610");
            stopped = false;
            writeline("/home/ubuntu/results/coverage/Project/Project_4_10.coverage", "442d2f19-0b0c-4336-948a-c8d63b5ccffc");
            LOG.info("Project " + name + " starting");
            writeline("/home/ubuntu/results/coverage/Project/Project_4_10.coverage", "b412da52-2c29-443a-9b27-7db6901af7f2");
            setState(ProjectState.IDLE);
            writeline("/home/ubuntu/results/coverage/Project/Project_4_10.coverage", "3e6320ce-d029-48ab-b1ef-d49841d8b07a");
            createNewSchedulingThread();
        }
    }

    protected void createNewSchedulingThread() {
        writeline("/home/ubuntu/results/coverage/Project/Project_4_10.coverage", "b594db0c-6da5-43d3-ba1e-29a8160ae995");
        final Thread projectSchedulingThread = new Thread(this, "Project " + getName() + " thread");
        writeline("/home/ubuntu/results/coverage/Project/Project_4_10.coverage", "04557825-52cc-42bc-aaa8-c91a692d9061");
        projectSchedulingThread.start();
        writeline("/home/ubuntu/results/coverage/Project/Project_4_10.coverage", "54a067c3-908a-48cd-bad9-d428e106c401");
        // brief nap to allow thread to start
        try {
            writeline("/home/ubuntu/results/coverage/Project/Project_4_10.coverage", "57f9ea65-48ab-4566-b5e6-09aa679994b1");
            Thread.sleep(100);
        } catch (InterruptedException ie) {
            writeline("/home/ubuntu/results/coverage/Project/Project_4_10.coverage", "3dc8a2b2-2582-4b3c-bc9b-0080d1524906");
            LOG.warn("interrupted while waiting for scheduling thread to start", ie);
        }
    }

    public void stop() {
        writeline("/home/ubuntu/results/coverage/Project/Project_4_10.coverage", "658b643f-f1b8-4bcb-8266-9a7c6778c49e");
        LOG.info("Project " + name + " stopping");
        writeline("/home/ubuntu/results/coverage/Project/Project_4_10.coverage", "7019b682-0c8e-4145-b944-9eba8b2025e7");
        stopped = true;
        writeline("/home/ubuntu/results/coverage/Project/Project_4_10.coverage", "ee764d3e-a948-41da-b261-acc1b1696dff");
        setState(ProjectState.STOPPED);
        writeline("/home/ubuntu/results/coverage/Project/Project_4_10.coverage", "12925d39-de93-4114-8ac2-93cbfd2d3e08");
        synchronized (pausedMutex) {
            writeline("/home/ubuntu/results/coverage/Project/Project_4_10.coverage", "e3b64e70-e285-4f9f-9eb8-b3a2330d8bc8");
            pausedMutex.notifyAll();
        }
        writeline("/home/ubuntu/results/coverage/Project/Project_4_10.coverage", "7990e58f-6c9b-43f4-82d4-918cf7bed3b2");
        synchronized (waitMutex) {
            writeline("/home/ubuntu/results/coverage/Project/Project_4_10.coverage", "214f470c-849a-474d-b50d-10aafd2306e3");
            waitMutex.notifyAll();
        }
        writeline("/home/ubuntu/results/coverage/Project/Project_4_10.coverage", "4dbea5c1-906e-4ad9-a41c-2a3f1f3c5638");
        synchronized (scheduleMutex) {
            writeline("/home/ubuntu/results/coverage/Project/Project_4_10.coverage", "173a3f36-a2bd-4302-817d-99ba70d21d9c");
            scheduleMutex.notifyAll();
        }
    }

    public String toString() {
        writeline("/home/ubuntu/results/coverage/Project/Project_4_10.coverage", "fcafca3f-b66d-49cc-a88e-d5249fd1f13d");
        final StringBuilder sb = new StringBuilder("Project ");
        writeline("/home/ubuntu/results/coverage/Project/Project_4_10.coverage", "f6f24b58-2f15-48f9-97f2-78973ded817f");
        sb.append(getName());
        writeline("/home/ubuntu/results/coverage/Project/Project_4_10.coverage", "a7b1cbdd-1435-4633-bceb-906c0c5d93e3");
        sb.append(": ");
        writeline("/home/ubuntu/results/coverage/Project/Project_4_10.coverage", "f46becba-1fc2-4faf-a50a-35ed6df790e3");
        sb.append(getStatus());
        writeline("/home/ubuntu/results/coverage/Project/Project_4_10.coverage", "05148757-4903-49ed-adac-06e9ede57369");
        if (isPaused) {
            writeline("/home/ubuntu/results/coverage/Project/Project_4_10.coverage", "407493d9-4a1f-4468-9314-776318527aa1");
            sb.append(" (paused)");
        }
        writeline("/home/ubuntu/results/coverage/Project/Project_4_10.coverage", "7b0eff7d-4b2e-4318-9f19-4bc2f2f932f7");
        return sb.toString();
    }

    public void addBuildProgressListener(final BuildProgressListener listener) {
        writeline("/home/ubuntu/results/coverage/Project/Project_4_10.coverage", "04dedecb-1ad9-4a88-8f68-a662e95cbdc0");
        synchronized (progressListeners) {
            writeline("/home/ubuntu/results/coverage/Project/Project_4_10.coverage", "3a17ddab-78d8-4021-aef7-e2aed063d04d");
            progressListeners.add(listener);
        }
    }

    protected void fireProgressEvent(final BuildProgressEvent event) {
        writeline("/home/ubuntu/results/coverage/Project/Project_4_10.coverage", "218bb344-75fb-4b53-8256-691636d2b354");
        synchronized (progressListeners) {
            writeline("/home/ubuntu/results/coverage/Project/Project_4_10.coverage", "bbf27d37-e5a7-40c6-b41a-975edd0ca496");
            for (final BuildProgressListener listener : progressListeners) {
                writeline("/home/ubuntu/results/coverage/Project/Project_4_10.coverage", "a1fb7b1b-eb4b-4f6d-bcd1-c68703753ae8");
                listener.handleBuildProgress(event);
            }
        }
    }

    public void addBuildResultListener(final BuildResultListener listener) {
        writeline("/home/ubuntu/results/coverage/Project/Project_4_10.coverage", "20d47e67-f492-4f7b-acae-92f13432e558");
        synchronized (resultListeners) {
            writeline("/home/ubuntu/results/coverage/Project/Project_4_10.coverage", "b8fa4589-43ed-434b-b70c-1f4ad9d59633");
            resultListeners.add(listener);
        }
    }

    protected void fireResultEvent(final BuildResultEvent event) {
        writeline("/home/ubuntu/results/coverage/Project/Project_4_10.coverage", "f6694334-704d-46c8-9c98-ff39e9a66150");
        synchronized (resultListeners) {
            writeline("/home/ubuntu/results/coverage/Project/Project_4_10.coverage", "7b5d988a-4739-4b24-be63-cc83ed0a5a46");
            for (final BuildResultListener listener : resultListeners) {
                writeline("/home/ubuntu/results/coverage/Project/Project_4_10.coverage", "3e830fac-2561-49be-9cd0-6200392a5c79");
                listener.handleBuildResult(event);
            }
        }
    }

    List<Listener> getListeners() {
        writeline("/home/ubuntu/results/coverage/Project/Project_4_10.coverage", "9c4003a1-5ff0-4536-9ef2-1a7e85672b0c");
        return projectConfig.getListeners();
    }

    public void setProjectConfig(final ProjectConfig projectConfig) throws CruiseControlException {
        writeline("/home/ubuntu/results/coverage/Project/Project_4_10.coverage", "31c8664c-127c-4b0a-a129-0bf6c1e6815f");
        if (projectConfig == null) {
            writeline("/home/ubuntu/results/coverage/Project/Project_4_10.coverage", "304ee212-d14f-4748-b6d9-977a999be3d8");
            throw new IllegalArgumentException("project config can't be null");
        }
        writeline("/home/ubuntu/results/coverage/Project/Project_4_10.coverage", "22af9b2b-4e26-4169-b9cf-50069803abc8");
        this.projectConfig = projectConfig;
        writeline("/home/ubuntu/results/coverage/Project/Project_4_10.coverage", "d0c7742e-3e9e-4789-bbb8-0d6683875054");
        setLabelIncrementer(projectConfig.getLabelIncrementer());
    }

    void notifyListeners(final ProjectEvent event) {
        writeline("/home/ubuntu/results/coverage/Project/Project_4_10.coverage", "0ca73044-8624-4a1b-b0b5-879e1773aed1");
        if (projectConfig == null) {
            writeline("/home/ubuntu/results/coverage/Project/Project_4_10.coverage", "a63c49d9-723e-4569-a821-4bf6fe0cbcc8");
            throw new IllegalStateException("projectConfig is null");
        }
        writeline("/home/ubuntu/results/coverage/Project/Project_4_10.coverage", "c304bb3a-f8d9-4ee5-96a4-27d715015ece");
        for (final Listener listener : projectConfig.getListeners()) {
            writeline("/home/ubuntu/results/coverage/Project/Project_4_10.coverage", "1571828c-d61c-4d4f-9f4d-eb50f3daad8a");
            try {
                writeline("/home/ubuntu/results/coverage/Project/Project_4_10.coverage", "faacfba7-afcd-4d12-81e1-0efac1b61f41");
                listener.handleEvent(event);
            } catch (CruiseControlException e) {
                writeline("/home/ubuntu/results/coverage/Project/Project_4_10.coverage", "e9dbc4a9-a607-409f-ac31-519df61665a6");
                final StringBuilder message = new StringBuilder("exception notifying listener ");
                writeline("/home/ubuntu/results/coverage/Project/Project_4_10.coverage", "6d35bb90-508c-4bda-8a48-270414fb1bca");
                message.append(listener.getClass().getName());
                writeline("/home/ubuntu/results/coverage/Project/Project_4_10.coverage", "29690129-8ccc-4e48-ae9e-f2ede6439176");
                message.append(" for project ").append(name);
                writeline("/home/ubuntu/results/coverage/Project/Project_4_10.coverage", "72330448-a674-4e4e-9e56-340a453925fa");
                LOG.error(message.toString(), e);
            }
        }
    }

    public boolean equals(final Object arg0) {
        writeline("/home/ubuntu/results/coverage/Project/Project_4_10.coverage", "6c7a3478-1730-4ba4-a398-77e6a07077fa");
        if (arg0 == null) {
            writeline("/home/ubuntu/results/coverage/Project/Project_4_10.coverage", "3415658d-0dc7-4c50-b2cf-33a33fb7b347");
            return false;
        }
        writeline("/home/ubuntu/results/coverage/Project/Project_4_10.coverage", "02dd5b47-09b5-4952-9c7a-96c391cc6cf4");
        if (arg0.getClass().getName().equals(getClass().getName())) {
            writeline("/home/ubuntu/results/coverage/Project/Project_4_10.coverage", "05adfbbc-03d2-4f8f-aab9-6c12da6ab734");
            final Project thatProject = (Project) arg0;
            writeline("/home/ubuntu/results/coverage/Project/Project_4_10.coverage", "1c9fecfd-d203-4956-be76-88c6988bebf6");
            return thatProject.name.equals(name);
        }
        writeline("/home/ubuntu/results/coverage/Project/Project_4_10.coverage", "d9ff2406-9a6e-4b2a-b471-c080139f1876");
        return false;
    }

    public int hashCode() {
        writeline("/home/ubuntu/results/coverage/Project/Project_4_10.coverage", "ad600402-c08a-4172-8651-664e765fda8a");
        return name.hashCode();
    }

    public void register(final MBeanServer server) throws JMException {
        writeline("/home/ubuntu/results/coverage/Project/Project_4_10.coverage", "dc48cdad-452a-4081-9ccf-fa7fd662f6e3");
        LOG.debug("Registering project mbean");
        writeline("/home/ubuntu/results/coverage/Project/Project_4_10.coverage", "ef5b97dc-867e-4ab2-8cd3-5c5a802e5181");
        final ProjectController projectController = new ProjectController(this);
        writeline("/home/ubuntu/results/coverage/Project/Project_4_10.coverage", "80377ea2-0f98-4f88-9c67-469b258a658e");
        projectController.register(server);
    }

    public ProjectConfig getProjectConfig() {
        writeline("/home/ubuntu/results/coverage/Project/Project_4_10.coverage", "e853359e-190f-4c6a-9c27-36be911588cb");
        return projectConfig;
    }

    public Date getLastBuildDate() {
        writeline("/home/ubuntu/results/coverage/Project/Project_4_10.coverage", "3982036c-61f1-477f-9b5b-f12c8a3f6e58");
        return lastBuild;
    }

    public Progress getProgress() {
        writeline("/home/ubuntu/results/coverage/Project/Project_4_10.coverage", "b7171d03-6452-43b3-a8b9-a4b9195b73fa");
        return progress;
    }

    public List<String> getLogLabels() {
        writeline("/home/ubuntu/results/coverage/Project/Project_4_10.coverage", "42102001-41bd-440b-befe-187cfe7f8628");
        return projectConfig.getLogLabels();
    }

    public String[] getLogLabelLines(final String logLabel, final int firstLine) {
        writeline("/home/ubuntu/results/coverage/Project/Project_4_10.coverage", "283f245f-ec5b-4a73-892d-9116c748bc31");
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
