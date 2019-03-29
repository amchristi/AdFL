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
        writeline("/home/ubuntu/results/coverage/Project/Project_7_10.coverage", "fa77e5c9-f9e2-439c-a1a9-42448d5c7cf5");
        state = ProjectState.STOPPED;
        writeline("/home/ubuntu/results/coverage/Project/Project_7_10.coverage", "027b3c2a-7ff5-443b-8037-859e79731caa");
        pausedMutex = new Object();
        writeline("/home/ubuntu/results/coverage/Project/Project_7_10.coverage", "0ee4777e-9540-4d57-87a4-26c109e94045");
        scheduleMutex = new Object();
        writeline("/home/ubuntu/results/coverage/Project/Project_7_10.coverage", "09264a43-ddab-4608-b2fd-4c78a042146b");
        waitMutex = new Object();
        writeline("/home/ubuntu/results/coverage/Project/Project_7_10.coverage", "c675c9e7-1254-4680-8e6a-57b414946e1c");
        progressListeners = new ArrayList<BuildProgressListener>();
        writeline("/home/ubuntu/results/coverage/Project/Project_7_10.coverage", "ef48cb48-5326-4146-a435-faa9321403c3");
        resultListeners = new ArrayList<BuildResultListener>();
        writeline("/home/ubuntu/results/coverage/Project/Project_7_10.coverage", "80f8e0e3-6144-4935-bf46-4f27f0e694dc");
        progress = new ProgressImpl(this);
    }

    private void readObject(final ObjectInputStream stream) throws IOException, ClassNotFoundException {
        writeline("/home/ubuntu/results/coverage/Project/Project_7_10.coverage", "e0e693a6-b78f-47ff-b19d-5eb450b7a12e");
        stream.defaultReadObject();
        writeline("/home/ubuntu/results/coverage/Project/Project_7_10.coverage", "31092c65-53e5-41cf-be61-6b630254ba3d");
        initializeTransientFields();
    }

    public void execute() {
        writeline("/home/ubuntu/results/coverage/Project/Project_7_10.coverage", "00a8f2e8-2b1b-4ebd-a9a7-1dcb23a1599d");
        if (stopped) {
            writeline("/home/ubuntu/results/coverage/Project/Project_7_10.coverage", "961fd609-cfdc-40a8-87ac-2308085ec2d7");
            LOG.warn("not building project " + name + " because project has been stopped.");
            writeline("/home/ubuntu/results/coverage/Project/Project_7_10.coverage", "36b65e03-aa04-4e35-9c05-b856133a01f7");
            buildFinished();
            writeline("/home/ubuntu/results/coverage/Project/Project_7_10.coverage", "36011b23-88c2-4488-9eef-af1a0bc61eed");
            return;
        }
        writeline("/home/ubuntu/results/coverage/Project/Project_7_10.coverage", "e971f96f-13ae-46bd-8573-9e15cdbaa18c");
        synchronized (pausedMutex) {
            writeline("/home/ubuntu/results/coverage/Project/Project_7_10.coverage", "4dd5c9b2-3e5c-4722-b945-d8be1ca32080");
            if (isPaused) {
                writeline("/home/ubuntu/results/coverage/Project/Project_7_10.coverage", "7bb3a087-e389-41e1-b31d-20be92d59ad6");
                LOG.info("not building project " + name + " because project has been paused.");
                writeline("/home/ubuntu/results/coverage/Project/Project_7_10.coverage", "b8c06b7c-1fd6-43ee-b549-1a083611d06b");
                buildFinished();
                writeline("/home/ubuntu/results/coverage/Project/Project_7_10.coverage", "47500804-4d58-4705-980f-09d9831a5bc2");
                return;
            }
        }
        writeline("/home/ubuntu/results/coverage/Project/Project_7_10.coverage", "567d7bda-d694-4dea-b891-9425f101d0ae");
        try {
            writeline("/home/ubuntu/results/coverage/Project/Project_7_10.coverage", "ae6fe72b-304e-43a1-89e9-f3da781a5c12");
            init();
            writeline("/home/ubuntu/results/coverage/Project/Project_7_10.coverage", "44a76706-c962-44c4-b243-50fd6e447bfd");
            build();
        } catch (CruiseControlException e) {
            writeline("/home/ubuntu/results/coverage/Project/Project_7_10.coverage", "1f0e764c-31b9-48c9-9848-bd4dbdab9424");
            LOG.error("exception attempting build in project " + name, e);
        } finally {
            writeline("/home/ubuntu/results/coverage/Project/Project_7_10.coverage", "349306cb-9513-497b-abe2-a46e3d843730");
            buildFinished();
        }
    }

    /**
     * Unless paused, runs any bootstrappers and then the entire build.
     * @throws CruiseControlException if an error occurs during the build
     */
    protected void build() throws CruiseControlException {
        writeline("/home/ubuntu/results/coverage/Project/Project_7_10.coverage", "39c6dd65-6df4-42ab-a76f-beb28bf06cfa");
        if (projectConfig == null) {
            writeline("/home/ubuntu/results/coverage/Project/Project_7_10.coverage", "008bf0cf-1c7e-4675-9ff3-f8801b1cb477");
            throw new IllegalStateException("projectConfig must be set on project before calling build()");
        }
        writeline("/home/ubuntu/results/coverage/Project/Project_7_10.coverage", "5d28f5b8-0276-43e8-a0ec-63b2d4ce0ef8");
        if (stopped) {
            writeline("/home/ubuntu/results/coverage/Project/Project_7_10.coverage", "c4247bd6-5d93-40c0-9a91-55c87d791aaa");
            LOG.warn("not building project " + name + " because project has been stopped.");
            writeline("/home/ubuntu/results/coverage/Project/Project_7_10.coverage", "f4d4c89c-a55a-4af8-af70-e82349369a5f");
            return;
        }
        writeline("/home/ubuntu/results/coverage/Project/Project_7_10.coverage", "334b7c2a-5e7e-46b1-b2c6-42a4ecfc0226");
        // or if the last build faild and we want to build on failures
        if (forceOnly && !buildForced && !(!wasLastBuildSuccessful && buildAfterFailed)) {
            writeline("/home/ubuntu/results/coverage/Project/Project_7_10.coverage", "f3d38a5d-700e-454c-9e64-fca86edb04b3");
            info("not building because project is forceOnly and build not forced.");
            writeline("/home/ubuntu/results/coverage/Project/Project_7_10.coverage", "0f9498a5-9afa-4d5c-b8aa-75036f286080");
            return;
        }
        writeline("/home/ubuntu/results/coverage/Project/Project_7_10.coverage", "d435b5e4-ef9e-4b04-aa21-9cdd7ad9bde1");
        final boolean buildWasForced = buildForced;
        writeline("/home/ubuntu/results/coverage/Project/Project_7_10.coverage", "0eee3744-8d83-408f-be16-0f6e700994af");
        try {
            writeline("/home/ubuntu/results/coverage/Project/Project_7_10.coverage", "f3e8567b-0a78-43e4-847a-7141d4f37389");
            setBuildStartTime(new Date());
            writeline("/home/ubuntu/results/coverage/Project/Project_7_10.coverage", "58540314-b5c1-47d1-a431-77a2ad14b4bb");
            final Schedule schedule = projectConfig.getSchedule();
            writeline("/home/ubuntu/results/coverage/Project/Project_7_10.coverage", "654d61f2-81ad-4dda-aa83-d0f3a7ccac08");
            if (schedule == null) {
                writeline("/home/ubuntu/results/coverage/Project/Project_7_10.coverage", "49bad2f1-9e27-4e55-8d8b-caed73bcc8a6");
                throw new IllegalStateException("project must have a schedule");
            }
            writeline("/home/ubuntu/results/coverage/Project/Project_7_10.coverage", "515f7d4d-aefd-47ad-a0c5-def3e35f8fb4");
            if (schedule.isPaused(buildStartTime)) {
                writeline("/home/ubuntu/results/coverage/Project/Project_7_10.coverage", "63b1df07-bf30-423d-8a20-a92918aa73d2");
                // is different than ProjectState.PAUSED
                return;
            }
            writeline("/home/ubuntu/results/coverage/Project/Project_7_10.coverage", "9033ebe1-62a9-4aa2-927b-7fd36b183efc");
            // @todo Add Progress param to Bootstrapper API?
            bootstrap();
            writeline("/home/ubuntu/results/coverage/Project/Project_7_10.coverage", "008a027d-1b86-4160-84d5-596f0941a3a7");
            final String target = useAndResetBuildTargetIfBuildWasForced(buildWasForced);
            writeline("/home/ubuntu/results/coverage/Project/Project_7_10.coverage", "fa629421-2ec6-41cb-a8ee-720a5c49fd42");
            // @todo Add Progress param to ModificationSet API?
            // getModifications will only return null if we don't need to build
            final Element modifications = getModifications(buildWasForced);
            writeline("/home/ubuntu/results/coverage/Project/Project_7_10.coverage", "bc754acd-1e66-4af9-bf8c-aab5885b630a");
            if (modifications == null) {
                writeline("/home/ubuntu/results/coverage/Project/Project_7_10.coverage", "03b777b1-f538-4d60-9263-6d28930f874d");
                return;
            }
            writeline("/home/ubuntu/results/coverage/Project/Project_7_10.coverage", "cefd85f9-358f-43dd-bd2f-c94cd3a2e220");
            // Using local reference to avoid NPE if config.xml is updated during build
            final Log buildLog = projectConfig.getLog();
            writeline("/home/ubuntu/results/coverage/Project/Project_7_10.coverage", "d9463985-b110-44d3-a7eb-250ddb16fd3c");
            buildLog.addContent(modifications);
            writeline("/home/ubuntu/results/coverage/Project/Project_7_10.coverage", "f2fd2874-ddb2-4556-a32e-e9787f5adeba");
            final Date now;
            writeline("/home/ubuntu/results/coverage/Project/Project_7_10.coverage", "e910ea72-1507-4abc-8d7f-e8b6063751b3");
            if (projectConfig.getModificationSet() != null && projectConfig.getModificationSet().getTimeOfCheck() != null) {
                writeline("/home/ubuntu/results/coverage/Project/Project_7_10.coverage", "36e34cf3-3110-4154-aba9-df5cc54b5ee1");
                now = projectConfig.getModificationSet().getTimeOfCheck();
            } else {
                writeline("/home/ubuntu/results/coverage/Project/Project_7_10.coverage", "c4186723-9aa1-498f-87b6-9a66a2880f78");
                now = new Date();
            }
            writeline("/home/ubuntu/results/coverage/Project/Project_7_10.coverage", "66fc72bb-2e3e-4954-8a18-16eb197bcdda");
            if (getLabelIncrementer().isPreBuildIncrementer()) {
                writeline("/home/ubuntu/results/coverage/Project/Project_7_10.coverage", "78fab9fe-d6d8-49ab-940f-d494de2537e0");
                label = getLabelIncrementer().incrementLabel(label, buildLog.getContent());
            }
            writeline("/home/ubuntu/results/coverage/Project/Project_7_10.coverage", "e95d6279-6dc6-47b2-87c8-ed4073b6e1b2");
            // collect project information
            buildLog.addContent(getProjectPropertiesElement(now));
            writeline("/home/ubuntu/results/coverage/Project/Project_7_10.coverage", "91e77db3-60b3-41b8-b7b7-7ce6695152f6");
            setState(ProjectState.BUILDING);
            writeline("/home/ubuntu/results/coverage/Project/Project_7_10.coverage", "04e14774-4529-4edb-84c9-93aa7cea16d2");
            final Element builderLog = schedule.build(buildCounter, lastBuild, now, getProjectPropertiesMap(now), target, progress);
            writeline("/home/ubuntu/results/coverage/Project/Project_7_10.coverage", "133a5259-1edb-4398-b8da-1c336ce7cf92");
            buildLog.addContent(builderLog.detach());
            writeline("/home/ubuntu/results/coverage/Project/Project_7_10.coverage", "1e9037ba-bd0e-4ed5-914c-cb39870265b3");
            boolean buildSuccessful = buildLog.wasBuildSuccessful();
            writeline("/home/ubuntu/results/coverage/Project/Project_7_10.coverage", "5064aaa3-0899-45aa-bbd1-e1e8fbeaaa90");
            fireResultEvent(new BuildResultEvent(this, buildSuccessful));
            writeline("/home/ubuntu/results/coverage/Project/Project_7_10.coverage", "8740007e-8159-4716-b713-f1a6e6c2977b");
            if (!getLabelIncrementer().isPreBuildIncrementer() && buildSuccessful) {
                writeline("/home/ubuntu/results/coverage/Project/Project_7_10.coverage", "d1816e7c-d140-4034-b5f3-cc29e7d53efe");
                label = getLabelIncrementer().incrementLabel(label, buildLog.getContent());
            }
            writeline("/home/ubuntu/results/coverage/Project/Project_7_10.coverage", "219a7553-876b-4648-a1a0-5b3967d765f0");
            setState(ProjectState.MERGING_LOGS);
            writeline("/home/ubuntu/results/coverage/Project/Project_7_10.coverage", "93a6e3a2-f56c-45fd-a0c9-4a2c0fad80aa");
            buildLog.writeLogFile(now);
            writeline("/home/ubuntu/results/coverage/Project/Project_7_10.coverage", "11fa4a04-2a32-4487-ad72-6c243f67ff6a");
            // regardless of success or failure (buildAfterFailed = false in config.xml)
            if (!buildAfterFailed) {
                writeline("/home/ubuntu/results/coverage/Project/Project_7_10.coverage", "a1af5d8d-0c82-4319-835d-e0dcad3f721f");
                lastBuild = now;
            }
            writeline("/home/ubuntu/results/coverage/Project/Project_7_10.coverage", "09723292-e395-4dbb-b03e-7129168b7463");
            // If this was a successful build, update both last build and last successful build
            if (buildSuccessful) {
                writeline("/home/ubuntu/results/coverage/Project/Project_7_10.coverage", "bbd056e6-f010-448c-8265-f3b8896de377");
                lastBuild = now;
                writeline("/home/ubuntu/results/coverage/Project/Project_7_10.coverage", "ddc8e5a8-9c10-428d-8600-4ab712fae477");
                lastSuccessfulBuild = now;
                writeline("/home/ubuntu/results/coverage/Project/Project_7_10.coverage", "5190f2cb-3304-41b1-9a9d-a41777d696d4");
                info("build successful");
            } else {
                writeline("/home/ubuntu/results/coverage/Project/Project_7_10.coverage", "7c48bfbd-6f1a-45d9-9c1f-b74a4c8c9345");
                info("build failed");
            }
            writeline("/home/ubuntu/results/coverage/Project/Project_7_10.coverage", "e09bc353-387b-4721-98b3-d9cd2ae16e40");
            buildCounter++;
            writeline("/home/ubuntu/results/coverage/Project/Project_7_10.coverage", "1969b580-f737-48e0-9116-8243950fa7ba");
            setWasLastBuildSuccessful(buildSuccessful);
            writeline("/home/ubuntu/results/coverage/Project/Project_7_10.coverage", "3af27b8d-04d7-43a5-9a25-92e40387a021");
            // also need to reset forced flag before serializing, unless buildForced var is transient
            // resetBuildForcedOnlyIfBuildWasForced(buildWasForced);
            serializeProject();
            writeline("/home/ubuntu/results/coverage/Project/Project_7_10.coverage", "d020f9a5-8360-48c7-9002-6d571d6f0c95");
            // @todo Add Progress param to Publisher API?
            publish(buildLog);
            writeline("/home/ubuntu/results/coverage/Project/Project_7_10.coverage", "b52ec624-ecf1-4bd4-a330-bd2aabac1d70");
            buildLog.reset();
        } finally {
            writeline("/home/ubuntu/results/coverage/Project/Project_7_10.coverage", "71f008c9-a3e4-4a1f-a927-56299fab5515");
            resetBuildForcedOnlyIfBuildWasForced(buildWasForced);
            writeline("/home/ubuntu/results/coverage/Project/Project_7_10.coverage", "3b89fa38-49ab-4476-8940-61992238b3e6");
            setState(ProjectState.IDLE);
        }
    }

    private String useAndResetBuildTargetIfBuildWasForced(final boolean buildWasForced) {
        writeline("/home/ubuntu/results/coverage/Project/Project_7_10.coverage", "bab832c4-a278-4e44-852f-3d51933c833a");
        String target = null;
        writeline("/home/ubuntu/results/coverage/Project/Project_7_10.coverage", "46436957-c907-4282-9f49-3c6b5d3e4cb7");
        if (buildWasForced) {
            writeline("/home/ubuntu/results/coverage/Project/Project_7_10.coverage", "a48c1d13-69ab-48d0-aa21-154ed744eff7");
            target = buildTarget;
            writeline("/home/ubuntu/results/coverage/Project/Project_7_10.coverage", "b2018109-c629-45d3-b022-f05da7c68054");
            buildTarget = null;
        }
        writeline("/home/ubuntu/results/coverage/Project/Project_7_10.coverage", "1feea07e-163a-423a-95a4-889fd776f355");
        return target;
    }

    private void resetBuildForcedOnlyIfBuildWasForced(final boolean buildWasForced) {
        writeline("/home/ubuntu/results/coverage/Project/Project_7_10.coverage", "e8d2b428-2b3c-49f6-87df-3d53eeeff9d4");
        if (buildWasForced) {
            writeline("/home/ubuntu/results/coverage/Project/Project_7_10.coverage", "7c353e7b-9c57-4129-a21e-0ea7fc79f016");
            buildForced = false;
        }
    }

    void setBuildStartTime(final Date date) {
        writeline("/home/ubuntu/results/coverage/Project/Project_7_10.coverage", "e3a95752-abda-460b-a5cc-4cb76ce4142b");
        buildStartTime = date;
    }

    public void run() {
        writeline("/home/ubuntu/results/coverage/Project/Project_7_10.coverage", "ee7289c0-f422-4fdd-a8c3-1605e9c0dd2e");
        LOG.info("Project " + name + " started");
        writeline("/home/ubuntu/results/coverage/Project/Project_7_10.coverage", "1a0d8cb5-22fd-4b3a-a04d-887975b4dc53");
        try {
            writeline("/home/ubuntu/results/coverage/Project/Project_7_10.coverage", "7ac5357d-69c8-4ace-8b7d-057c75761a73");
            while (!stopped) {
                writeline("/home/ubuntu/results/coverage/Project/Project_7_10.coverage", "9be8cb1a-0e67-4867-975a-b880b2713b0e");
                try {
                    writeline("/home/ubuntu/results/coverage/Project/Project_7_10.coverage", "605a60a5-ff6a-48f7-8cb7-d4cf8611abea");
                    waitIfPaused();
                    writeline("/home/ubuntu/results/coverage/Project/Project_7_10.coverage", "65cbcffd-fb01-47df-bcc6-f36038c97a13");
                    if (!stopped) {
                        writeline("/home/ubuntu/results/coverage/Project/Project_7_10.coverage", "85ad74a0-79f7-46ff-a424-890a10f7af03");
                        waitForNextBuild();
                    }
                    writeline("/home/ubuntu/results/coverage/Project/Project_7_10.coverage", "4cbfee57-5738-4bd4-89b2-44b6c30bdf90");
                    if (!stopped) {
                        writeline("/home/ubuntu/results/coverage/Project/Project_7_10.coverage", "96e08df4-7a31-48fb-b549-afc2306028c7");
                        setState(ProjectState.QUEUED);
                        writeline("/home/ubuntu/results/coverage/Project/Project_7_10.coverage", "ecc11ab4-6726-471f-a457-0a2ade7bf043");
                        synchronized (scheduleMutex) {
                            writeline("/home/ubuntu/results/coverage/Project/Project_7_10.coverage", "e9b76457-7323-4045-b536-db328ee2878c");
                            queue.requestBuild(projectConfig);
                            writeline("/home/ubuntu/results/coverage/Project/Project_7_10.coverage", "9a8b6555-5170-46c9-81ba-19b2bdaea949");
                            waitForBuildToFinish();
                        }
                    }
                } catch (InterruptedException e) {
                    writeline("/home/ubuntu/results/coverage/Project/Project_7_10.coverage", "086dc977-c1e7-4028-ae55-8e08d79f30cb");
                    final String message = "Project " + name + ".run() interrupted";
                    writeline("/home/ubuntu/results/coverage/Project/Project_7_10.coverage", "eb852536-f228-4053-8ed4-7ea5bffd11ff");
                    LOG.error(message, e);
                    writeline("/home/ubuntu/results/coverage/Project/Project_7_10.coverage", "e0f3b71b-c9bb-4f66-820f-d36065c703b8");
                    throw new RuntimeException(message);
                }
            }
        } finally {
            writeline("/home/ubuntu/results/coverage/Project/Project_7_10.coverage", "9b322efa-b244-4240-bbd4-ba553d9dbfcd");
            stopped = true;
            writeline("/home/ubuntu/results/coverage/Project/Project_7_10.coverage", "c9715c92-39f3-44b0-9ee8-918e177659fc");
            LOG.info("Project " + name + " stopped");
        }
    }

    void waitIfPaused() throws InterruptedException {
        writeline("/home/ubuntu/results/coverage/Project/Project_7_10.coverage", "3de4696d-6cf7-447a-8f1b-0cc2e1066ab7");
        synchronized (pausedMutex) {
            writeline("/home/ubuntu/results/coverage/Project/Project_7_10.coverage", "13edc0db-f46a-4a60-9e71-e9793d90b3d4");
            while (isPaused) {
                writeline("/home/ubuntu/results/coverage/Project/Project_7_10.coverage", "0f9d0d9b-5866-4e1b-bf49-40df5536b2dc");
                setState(ProjectState.PAUSED);
                writeline("/home/ubuntu/results/coverage/Project/Project_7_10.coverage", "ca43d0ee-1d43-4faf-977e-87de7a7c1ccd");
                pausedMutex.wait(10 * DateUtil.ONE_MINUTE);
            }
        }
    }

    void waitForNextBuild() throws InterruptedException {
        writeline("/home/ubuntu/results/coverage/Project/Project_7_10.coverage", "c5325469-5db9-41b1-baeb-a530910401cc");
        long waitTime = getTimeToNextBuild(new Date());
        writeline("/home/ubuntu/results/coverage/Project/Project_7_10.coverage", "4abd7c3b-5b89-4e3e-a6d8-9bd639a6d8cd");
        if (needToWaitForNextBuild(waitTime) && !buildForced) {
            writeline("/home/ubuntu/results/coverage/Project/Project_7_10.coverage", "69ee8ad7-bc71-4a73-bd35-21cea828e01d");
            final String msg = "next build in " + DateUtil.formatTime(waitTime);
            writeline("/home/ubuntu/results/coverage/Project/Project_7_10.coverage", "8cbf9639-585e-4cb7-82da-9c6605a90c80");
            info(msg);
            writeline("/home/ubuntu/results/coverage/Project/Project_7_10.coverage", "d014cab3-4f04-4ca7-a591-b7da68369d80");
            synchronized (waitMutex) {
                writeline("/home/ubuntu/results/coverage/Project/Project_7_10.coverage", "ad00f2f4-8e7a-474e-9ae8-f6f18135ae4e");
                setState(ProjectState.WAITING);
                writeline("/home/ubuntu/results/coverage/Project/Project_7_10.coverage", "74753815-ea04-463c-a049-e0678e7521e9");
                progress.setValue(msg);
                writeline("/home/ubuntu/results/coverage/Project/Project_7_10.coverage", "8ea5cd0f-7e10-4d42-8e58-cffad87009f5");
                waitMutex.wait(waitTime);
            }
        }
    }

    long getTimeToNextBuild(Date now) {
        writeline("/home/ubuntu/results/coverage/Project/Project_7_10.coverage", "9a797db7-b999-4c04-a8e7-0526e22651e6");
        long waitTime = projectConfig.getSchedule().getTimeToNextBuild(now, getBuildInterval());
        writeline("/home/ubuntu/results/coverage/Project/Project_7_10.coverage", "29d7e84d-4061-4416-a822-8961e03eddb7");
        if (waitTime == 0) {
            writeline("/home/ubuntu/results/coverage/Project/Project_7_10.coverage", "6f91ac44-e9d3-4bbc-83e7-945829001634");
            // project that has just built within a minute time
            if (buildStartTime != null) {
                writeline("/home/ubuntu/results/coverage/Project/Project_7_10.coverage", "846a4fbb-44b4-4e7f-84c6-faeedaa305ca");
                long millisSinceLastBuild = now.getTime() - buildStartTime.getTime();
                writeline("/home/ubuntu/results/coverage/Project/Project_7_10.coverage", "2a51fbbd-54b7-45c4-bd7e-b39f40d6e05f");
                if (millisSinceLastBuild < Schedule.ONE_MINUTE) {
                    writeline("/home/ubuntu/results/coverage/Project/Project_7_10.coverage", "2e753775-018f-4a1a-a4ef-b97b3a1e5dec");
                    debug("build finished within a minute, getting new time to next build");
                    writeline("/home/ubuntu/results/coverage/Project/Project_7_10.coverage", "8b5e2d47-1f83-4176-a2ab-ae55808ff9ee");
                    Date oneMinuteInFuture = new Date(now.getTime() + Schedule.ONE_MINUTE);
                    writeline("/home/ubuntu/results/coverage/Project/Project_7_10.coverage", "4d1a9c23-460f-4c5d-94c7-13abece1710b");
                    waitTime = projectConfig.getSchedule().getTimeToNextBuild(oneMinuteInFuture, getBuildInterval());
                    writeline("/home/ubuntu/results/coverage/Project/Project_7_10.coverage", "2ef75d24-f2e7-4fe8-b1d3-618754c6639e");
                    waitTime += Schedule.ONE_MINUTE;
                }
            }
        }
        writeline("/home/ubuntu/results/coverage/Project/Project_7_10.coverage", "7a2eeda1-a08c-4f77-b175-1a9349e697d9");
        return waitTime;
    }

    static boolean needToWaitForNextBuild(long waitTime) {
        writelineStatic("/home/ubuntu/results/coverage/Project/Project_7_10.coverage", "dd2a71cf-87be-4dfd-a8e4-d009aab77fe2");
        return waitTime > 0;
    }

    /**
     * @return true if build was forced, intended for unit testing only.
     */
    boolean isBuildForced() {
        writeline("/home/ubuntu/results/coverage/Project/Project_7_10.coverage", "e24b28d1-60af-430d-aeb8-dc44ea187448");
        return buildForced;
    }

    void forceBuild() {
        writeline("/home/ubuntu/results/coverage/Project/Project_7_10.coverage", "198fdf67-c3f8-4e3e-ae0a-ff6f4368b98a");
        synchronized (waitMutex) {
            writeline("/home/ubuntu/results/coverage/Project/Project_7_10.coverage", "7754b78e-3629-41e9-a181-5158a5185256");
            waitMutex.notify();
        }
    }

    public void forceBuildWithTarget(String buildTarget) {
        writeline("/home/ubuntu/results/coverage/Project/Project_7_10.coverage", "dbb69ebb-a683-41ed-b4dc-1bd926fe25a3");
        this.buildTarget = buildTarget;
        writeline("/home/ubuntu/results/coverage/Project/Project_7_10.coverage", "13051086-0470-4077-967f-6c9e3ad5adde");
        setBuildForced(true);
    }

    public void forceBuildWithTarget(String buildTarget, Map<String, String> addedProperties) {
        writeline("/home/ubuntu/results/coverage/Project/Project_7_10.coverage", "e3fa136c-7b76-4999-b272-468ad6becc9d");
        additionalProperties = addedProperties;
        writeline("/home/ubuntu/results/coverage/Project/Project_7_10.coverage", "775f86f5-5d97-4cbf-8227-0efefcbdb5bf");
        forceBuildWithTarget(buildTarget);
    }

    void waitForBuildToFinish() throws InterruptedException {
        writeline("/home/ubuntu/results/coverage/Project/Project_7_10.coverage", "baee4073-1491-475b-884f-aaa7d032f36f");
        synchronized (scheduleMutex) {
            writeline("/home/ubuntu/results/coverage/Project/Project_7_10.coverage", "ccf22057-86c1-440a-bfb5-1c06da1fd348");
            debug("waiting for build to finish");
            writeline("/home/ubuntu/results/coverage/Project/Project_7_10.coverage", "90ad7c5f-e2ca-4fbd-8074-a8cf657b6b25");
            scheduleMutex.wait();
        }
    }

    void buildFinished() {
        writeline("/home/ubuntu/results/coverage/Project/Project_7_10.coverage", "8c11b55f-3740-468c-a28d-56cca8cf9cac");
        synchronized (scheduleMutex) {
            writeline("/home/ubuntu/results/coverage/Project/Project_7_10.coverage", "0273178c-5ec3-409d-aa87-3f7c825d7843");
            debug("build finished");
            writeline("/home/ubuntu/results/coverage/Project/Project_7_10.coverage", "381280f9-f633-4806-a53e-e6a7e541378b");
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
        writeline("/home/ubuntu/results/coverage/Project/Project_7_10.coverage", "82bcb703-f913-4fc3-9d97-ed3bf92f3099");
        setState(ProjectState.MODIFICATIONSET);
        writeline("/home/ubuntu/results/coverage/Project/Project_7_10.coverage", "823f819c-cb7e-48f7-8c0b-550c01e16a7e");
        final ModificationSet modificationSet = projectConfig.getModificationSet();
        writeline("/home/ubuntu/results/coverage/Project/Project_7_10.coverage", "c0f5a3b2-00b6-4abd-8bb4-16b104ee4358");
        if (modificationSet == null) {
            writeline("/home/ubuntu/results/coverage/Project/Project_7_10.coverage", "4f444249-057b-4a16-87b0-e29a7cea9d4f");
            debug("no modification set, nothing to detect.");
            writeline("/home/ubuntu/results/coverage/Project/Project_7_10.coverage", "17fd29e0-6565-4ecb-833a-05b40465415d");
            if (buildWasForced) {
                writeline("/home/ubuntu/results/coverage/Project/Project_7_10.coverage", "dd2ef28b-5005-4f5e-a58b-11a71acbafaf");
                info("no modification set but build was forced");
                writeline("/home/ubuntu/results/coverage/Project/Project_7_10.coverage", "2447e302-e567-4f49-b0f1-c65e3b43a04e");
                return new Element("modifications");
            }
            writeline("/home/ubuntu/results/coverage/Project/Project_7_10.coverage", "f80c9e1b-ee75-4f0f-81f9-6bbe7222ecb9");
            if (!requiremodification) {
                writeline("/home/ubuntu/results/coverage/Project/Project_7_10.coverage", "c31ad4f4-a98d-4354-a00c-b236eb74664a");
                info("no modification set but no modifications required");
                writeline("/home/ubuntu/results/coverage/Project/Project_7_10.coverage", "089c360f-4e5d-40d5-a0ea-5dbf8f743fcc");
                return new Element("modifications");
            }
            writeline("/home/ubuntu/results/coverage/Project/Project_7_10.coverage", "5b7d04fc-12e0-4a0c-ad36-072ee40df3b0");
            return null;
        }
        writeline("/home/ubuntu/results/coverage/Project/Project_7_10.coverage", "0ce1187b-dfde-4c4f-b0a6-83c807b99a6f");
        final boolean checkNewChangesFirst = checkOnlySinceLastBuild();
        writeline("/home/ubuntu/results/coverage/Project/Project_7_10.coverage", "c4b32910-81b5-4219-80fb-6349e6dfd3e2");
        Element modifications;
        writeline("/home/ubuntu/results/coverage/Project/Project_7_10.coverage", "44aceb6e-1f03-412b-a042-1a71dd862983");
        if (checkNewChangesFirst) {
            writeline("/home/ubuntu/results/coverage/Project/Project_7_10.coverage", "21d08827-e2e0-4158-b111-ab53a55625e9");
            debug("getting changes since last build");
            writeline("/home/ubuntu/results/coverage/Project/Project_7_10.coverage", "f44cde26-aedd-4c0f-a7ad-423c0220767f");
            modifications = modificationSet.retrieveModificationsAsElement(lastBuild, progress);
        } else {
            writeline("/home/ubuntu/results/coverage/Project/Project_7_10.coverage", "590668d5-eea8-40b0-9bdc-b58b93feb8d8");
            debug("getting changes since last successful build");
            writeline("/home/ubuntu/results/coverage/Project/Project_7_10.coverage", "6a210ff4-b2fd-45be-9c0f-d29846dfd53f");
            modifications = modificationSet.retrieveModificationsAsElement(lastSuccessfulBuild, progress);
        }
        writeline("/home/ubuntu/results/coverage/Project/Project_7_10.coverage", "35819156-0983-4787-aaa0-be72db792e2e");
        if (!modificationSet.isModified()) {
            writeline("/home/ubuntu/results/coverage/Project/Project_7_10.coverage", "77f12ce2-4517-4023-b3f7-f4287d32c741");
            info("No modifications found, build not necessary.");
            writeline("/home/ubuntu/results/coverage/Project/Project_7_10.coverage", "13aaf4fc-bf04-448c-b84f-8f90b7c5877e");
            // * build forced
            if (buildAfterFailed && !wasLastBuildSuccessful) {
                writeline("/home/ubuntu/results/coverage/Project/Project_7_10.coverage", "04371524-7834-4470-bfda-d9f027045f58");
                info("Building anyway, since buildAfterFailed is true and last build failed.");
            } else if (!requiremodification) {
                writeline("/home/ubuntu/results/coverage/Project/Project_7_10.coverage", "8b6863ac-4b28-47e9-9061-49b43d14746c");
                info("Building anyway, since modifications not required");
            } else {
                writeline("/home/ubuntu/results/coverage/Project/Project_7_10.coverage", "e7af3bc8-ca68-48b2-ba2e-8cae20b15fb5");
                if (buildWasForced) {
                    writeline("/home/ubuntu/results/coverage/Project/Project_7_10.coverage", "ff8ee53b-5b5a-4ac0-a26e-e64646d4ae10");
                    info("Building anyway, since build was explicitly forced.");
                } else {
                    writeline("/home/ubuntu/results/coverage/Project/Project_7_10.coverage", "6af93b84-7ac2-4965-8275-42ae71c139f9");
                    return null;
                }
            }
        }
        writeline("/home/ubuntu/results/coverage/Project/Project_7_10.coverage", "95eb82a3-2478-42d9-9ff2-423133fe8b60");
        if (checkNewChangesFirst) {
            writeline("/home/ubuntu/results/coverage/Project/Project_7_10.coverage", "b13d3a66-ca62-46f7-9a70-2eff3ee15fc5");
            debug("new changes found; now getting complete set");
            writeline("/home/ubuntu/results/coverage/Project/Project_7_10.coverage", "49cef1f3-98f6-4be4-8f7a-0921d4730e06");
            modifications = modificationSet.retrieveModificationsAsElement(lastSuccessfulBuild, progress);
        }
        writeline("/home/ubuntu/results/coverage/Project/Project_7_10.coverage", "e934e290-0629-44ea-8511-fd0bde976c08");
        return modifications;
    }

    /**
     * @return boolean
     */
    boolean checkOnlySinceLastBuild() {
        writeline("/home/ubuntu/results/coverage/Project/Project_7_10.coverage", "d26b1fb6-fd39-4cc0-8aea-70b36670b93b");
        if (lastBuild == null || lastSuccessfulBuild == null) {
            writeline("/home/ubuntu/results/coverage/Project/Project_7_10.coverage", "94b12646-3385-44ed-bebf-690985503904");
            return false;
        }
        writeline("/home/ubuntu/results/coverage/Project/Project_7_10.coverage", "8a878cc9-d271-4b5c-9c0f-ebd836c157fc");
        final long lastBuildLong = lastBuild.getTime();
        writeline("/home/ubuntu/results/coverage/Project/Project_7_10.coverage", "4ab5e9ad-1496-4d44-b85f-1f93f0605b6e");
        final long timeDifference = lastBuildLong - lastSuccessfulBuild.getTime();
        writeline("/home/ubuntu/results/coverage/Project/Project_7_10.coverage", "4db6f7ff-506d-4de8-b015-68cebed60d8e");
        final boolean moreThanASecond = timeDifference > DateUtil.ONE_SECOND;
        writeline("/home/ubuntu/results/coverage/Project/Project_7_10.coverage", "78ade2e8-0cf1-45fb-b3c5-73a696bcc96f");
        return !buildAfterFailed && moreThanASecond;
    }

    /**
     * Serialize the project to allow resumption after a process bounce
     */
    public void serializeProject() {
        writeline("/home/ubuntu/results/coverage/Project/Project_7_10.coverage", "dc3f060f-d958-4375-8ec3-bc798850f77d");
        final String safeProjectName = Builder.getFileSystemSafeProjectName(name);
        writeline("/home/ubuntu/results/coverage/Project/Project_7_10.coverage", "461463c9-0f86-4a79-bd1e-002a86081368");
        try {
            writeline("/home/ubuntu/results/coverage/Project/Project_7_10.coverage", "8ca32475-2ee4-4f63-bea8-58979c194225");
            final ObjectOutputStream s = new ObjectOutputStream(new FileOutputStream(safeProjectName + ".ser"));
            writeline("/home/ubuntu/results/coverage/Project/Project_7_10.coverage", "00c8b18f-e488-42a5-a383-b332a93a11b9");
            try {
                writeline("/home/ubuntu/results/coverage/Project/Project_7_10.coverage", "da7c99db-2d31-4536-97fb-ed0791f55d82");
                s.writeObject(this);
                writeline("/home/ubuntu/results/coverage/Project/Project_7_10.coverage", "61df6054-5b2b-46ba-a74b-b80e7dfeb545");
                s.flush();
                writeline("/home/ubuntu/results/coverage/Project/Project_7_10.coverage", "60f7eada-2313-4cff-b1ff-9127cd640dc9");
                debug("Serializing project to [" + safeProjectName + ".ser]");
            } finally {
                writeline("/home/ubuntu/results/coverage/Project/Project_7_10.coverage", "aac47ee9-c517-4e79-a43f-6d85c8caaa4c");
                s.close();
            }
        } catch (Exception e) {
            writeline("/home/ubuntu/results/coverage/Project/Project_7_10.coverage", "fb5bbc43-6aa0-46e1-aa0b-c5ae4cde0f06");
            LOG.warn("Error serializing project to [" + safeProjectName + ".ser]: " + e.getMessage(), e);
        }
    }

    public void setLabelIncrementer(final LabelIncrementer incrementer) throws CruiseControlException {
        writeline("/home/ubuntu/results/coverage/Project/Project_7_10.coverage", "d22a3cff-2c97-48f7-9767-bf58f70f4707");
        if (incrementer == null) {
            writeline("/home/ubuntu/results/coverage/Project/Project_7_10.coverage", "28fb236a-8ca9-49f6-a107-14d9582212b7");
            throw new IllegalArgumentException("label incrementer can't be null");
        }
        writeline("/home/ubuntu/results/coverage/Project/Project_7_10.coverage", "54dcfa0d-4589-48e0-9656-ce18b844bd9e");
        labelIncrementer = incrementer;
        writeline("/home/ubuntu/results/coverage/Project/Project_7_10.coverage", "72d26671-bae3-4f67-8c7c-48d3867f04ca");
        if (label == null) {
            writeline("/home/ubuntu/results/coverage/Project/Project_7_10.coverage", "3cbf37a4-8974-477b-88ac-9f9078cd9632");
            label = labelIncrementer.getDefaultLabel();
        }
        writeline("/home/ubuntu/results/coverage/Project/Project_7_10.coverage", "60869b92-3b2c-4105-b566-c97fd5806895");
        validateLabel(label, labelIncrementer);
    }

    public LabelIncrementer getLabelIncrementer() {
        writeline("/home/ubuntu/results/coverage/Project/Project_7_10.coverage", "591fa362-b9ca-4da7-87aa-9ad884841b67");
        return labelIncrementer;
    }

    public void setName(final String projectName) {
        writeline("/home/ubuntu/results/coverage/Project/Project_7_10.coverage", "94a1ac90-1cb9-4a39-b168-aceafd67287a");
        name = projectName;
    }

    public String getName() {
        writeline("/home/ubuntu/results/coverage/Project/Project_7_10.coverage", "845b653f-6d86-472c-9a2f-9d36cd907d92");
        return name;
    }

    public void setLabel(final String newLabel) {
        writeline("/home/ubuntu/results/coverage/Project/Project_7_10.coverage", "2229e399-0009-4820-9be4-e7aa92439107");
        label = newLabel;
    }

    public String getLabel() {
        writeline("/home/ubuntu/results/coverage/Project/Project_7_10.coverage", "4edb6acc-7bd8-4394-8d04-82df3ab71ca8");
        return label;
    }

    /**
     * @param newLastBuild string containing the build date in the format
     * yyyyMMddHHmmss
     * @throws CruiseControlException if the date cannot be extracted from the
     * input string
     */
    public void setLastBuild(final String newLastBuild) throws CruiseControlException {
        writeline("/home/ubuntu/results/coverage/Project/Project_7_10.coverage", "d7ce322d-ae91-434f-b46b-1449f6ce0feb");
        lastBuild = DateUtil.parseFormattedTime(newLastBuild, "lastBuild");
    }

    /**
     * @param newLastSuccessfulBuild string containing the build date in the format
     * yyyyMMddHHmmss
     * @throws CruiseControlException if the date cannot be extracted from the
     * input string
     */
    public void setLastSuccessfulBuild(final String newLastSuccessfulBuild) throws CruiseControlException {
        writeline("/home/ubuntu/results/coverage/Project/Project_7_10.coverage", "a274023e-1909-4270-9055-d4c6fb9e2ef3");
        lastSuccessfulBuild = DateUtil.parseFormattedTime(newLastSuccessfulBuild, "lastSuccessfulBuild");
    }

    public String getLastBuild() {
        writeline("/home/ubuntu/results/coverage/Project/Project_7_10.coverage", "a0b64419-6624-4b86-9c85-bcc32f3a8474");
        if (lastBuild == null) {
            writeline("/home/ubuntu/results/coverage/Project/Project_7_10.coverage", "5c2be038-9e02-42b6-9891-34eed2b2c913");
            return null;
        }
        writeline("/home/ubuntu/results/coverage/Project/Project_7_10.coverage", "e73f5044-6bd7-4037-bb0b-a1a61ce4e01d");
        return DateUtil.getFormattedTime(lastBuild);
    }

    public boolean getBuildForced() {
        writeline("/home/ubuntu/results/coverage/Project/Project_7_10.coverage", "93ca5525-5d75-49d9-93d7-e1547dd73486");
        return buildForced;
    }

    public void setBuildForced(boolean forceNewBuildNow) {
        writeline("/home/ubuntu/results/coverage/Project/Project_7_10.coverage", "95f8527f-4d1c-431f-aee6-587865122413");
        buildForced = forceNewBuildNow;
        writeline("/home/ubuntu/results/coverage/Project/Project_7_10.coverage", "0e253a47-5f9b-4c34-9cdd-3a92b11540a5");
        if (forceNewBuildNow) {
            writeline("/home/ubuntu/results/coverage/Project/Project_7_10.coverage", "101553e6-8c0f-417a-ab01-2ca9d73b83b6");
            forceBuild();
        }
    }

    public String getLastSuccessfulBuild() {
        writeline("/home/ubuntu/results/coverage/Project/Project_7_10.coverage", "314d7b7b-d9a3-40b3-a2ce-06a0fe920dc7");
        if (lastSuccessfulBuild == null) {
            writeline("/home/ubuntu/results/coverage/Project/Project_7_10.coverage", "2f15f3fa-3edc-44eb-a2f3-33c4cf67b427");
            return null;
        }
        writeline("/home/ubuntu/results/coverage/Project/Project_7_10.coverage", "b92dc99a-9cb4-4997-8a03-59ec04bb0d9e");
        return DateUtil.getFormattedTime(lastSuccessfulBuild);
    }

    public String getLogDir() {
        writeline("/home/ubuntu/results/coverage/Project/Project_7_10.coverage", "e0feb866-6959-4136-8c11-2b06559c425c");
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
        writeline("/home/ubuntu/results/coverage/Project/Project_7_10.coverage", "e9316478-8161-4858-ba84-e16aa83880a9");
        if (overrideBuildInterval == null) {
            writeline("/home/ubuntu/results/coverage/Project/Project_7_10.coverage", "76b701ed-5a09-4693-85c8-2b112c049d45");
            return projectConfig.getSchedule().getInterval();
        } else {
            writeline("/home/ubuntu/results/coverage/Project/Project_7_10.coverage", "d0623f5c-3ee6-4430-a415-9f0533f00f0f");
            return overrideBuildInterval;
        }
    }

    /**
     * Sets the build interval that this Project should use. This method
     * overrides the value initially specified in the Schedule attribute.
     * @param sleepMillis the number of milliseconds to sleep between build attempts
     */
    public void overrideBuildInterval(final long sleepMillis) {
        writeline("/home/ubuntu/results/coverage/Project/Project_7_10.coverage", "49d4ecac-5e31-4ed6-8d00-22e0e049bdcc");
        overrideBuildInterval = sleepMillis;
    }

    public boolean isPaused() {
        writeline("/home/ubuntu/results/coverage/Project/Project_7_10.coverage", "c07ef887-17bd-48ff-ad39-6ef676009dad");
        return isPaused;
    }

    public void setPaused(final boolean paused) {
        writeline("/home/ubuntu/results/coverage/Project/Project_7_10.coverage", "a5aad167-bb26-4bd8-b236-6b63875df4d1");
        synchronized (pausedMutex) {
            writeline("/home/ubuntu/results/coverage/Project/Project_7_10.coverage", "e4aea10e-392d-4864-82d0-3c70e14fc1ec");
            if (isPaused && !paused) {
                writeline("/home/ubuntu/results/coverage/Project/Project_7_10.coverage", "70fed3a1-83d5-4346-9917-eaaf24b7b4b0");
                pausedMutex.notifyAll();
            }
            writeline("/home/ubuntu/results/coverage/Project/Project_7_10.coverage", "ff89c6f4-17c6-48fc-b0ca-9e293f45559b");
            isPaused = paused;
        }
    }

    public void setBuildAfterFailed(final boolean rebuildEvenWithNoNewModifications) {
        writeline("/home/ubuntu/results/coverage/Project/Project_7_10.coverage", "36974b7e-607e-4731-ab23-c85dd89b5e9d");
        buildAfterFailed = rebuildEvenWithNoNewModifications;
    }

    public String getStatus() {
        writeline("/home/ubuntu/results/coverage/Project/Project_7_10.coverage", "72d751bb-5abe-483e-9d46-dc2435959cca");
        return getState().getDescription();
    }

    public String getStatusWithQueuePosition() {
        writeline("/home/ubuntu/results/coverage/Project/Project_7_10.coverage", "4406b88b-7ec3-4bce-a450-b5a1c1001da1");
        if (ProjectState.QUEUED.equals(getState())) {
            writeline("/home/ubuntu/results/coverage/Project/Project_7_10.coverage", "7d242538-2f98-4418-a36e-037a2a615425");
            return getState().getDescription() + " - " + queue.findPosition(projectConfig);
        } else {
            writeline("/home/ubuntu/results/coverage/Project/Project_7_10.coverage", "4a61c707-c57e-4cb1-afc8-818877d32d38");
            return getState().getDescription();
        }
    }

    public ProjectState getState() {
        writeline("/home/ubuntu/results/coverage/Project/Project_7_10.coverage", "0b0742fc-4501-484b-aa01-cb4dd564b930");
        return state;
    }

    private void setState(final ProjectState newState) {
        writeline("/home/ubuntu/results/coverage/Project/Project_7_10.coverage", "bff8ead5-80fd-4cdb-927f-c040dd6ad6ad");
        state = newState;
        writeline("/home/ubuntu/results/coverage/Project/Project_7_10.coverage", "788e6009-16de-458d-8336-a3aabd9cf97a");
        info(getStatus());
        writeline("/home/ubuntu/results/coverage/Project/Project_7_10.coverage", "a5a2fd16-2a5f-4ee4-acca-95c47294aa52");
        notifyListeners(new ProjectStateChangedEvent(name, getState()));
        writeline("/home/ubuntu/results/coverage/Project/Project_7_10.coverage", "77826a6a-8482-4d8f-98c8-9d59cce05f4d");
        fireProgressEvent(new BuildProgressEvent(this, getState()));
    }

    public void setBuildQueue(final BuildQueue buildQueue) {
        writeline("/home/ubuntu/results/coverage/Project/Project_7_10.coverage", "a62528f4-aacd-4761-bd12-24736bd50bb7");
        queue = buildQueue;
    }

    public String getBuildStartTime() {
        writeline("/home/ubuntu/results/coverage/Project/Project_7_10.coverage", "eb21113d-c826-4562-9088-5dc8a17abb92");
        return DateUtil.getFormattedTime(buildStartTime);
    }

    public Log getLog() {
        writeline("/home/ubuntu/results/coverage/Project/Project_7_10.coverage", "bf49a5ac-f932-4b20-ac9d-581d52a34580");
        return this.projectConfig.getLog();
    }

    /**
     * Initialize the project. Uses ProjectXMLHelper to parse a project file.
     */
    protected void init() {
        writeline("/home/ubuntu/results/coverage/Project/Project_7_10.coverage", "79b390a1-03b7-4fb2-8442-f22840d0c67d");
        if (projectConfig == null) {
            writeline("/home/ubuntu/results/coverage/Project/Project_7_10.coverage", "1738561d-a352-4083-ab94-7a9493800bfa");
            throw new IllegalStateException("projectConfig must be set on project before calling init()");
        }
        writeline("/home/ubuntu/results/coverage/Project/Project_7_10.coverage", "ebe0c406-a6e3-433f-9912-afa38b7cbb43");
        buildAfterFailed = projectConfig.shouldBuildAfterFailed();
        writeline("/home/ubuntu/results/coverage/Project/Project_7_10.coverage", "c3fe674b-194f-4533-9e79-c18478fb83af");
        forceOnly = projectConfig.isForceOnly();
        writeline("/home/ubuntu/results/coverage/Project/Project_7_10.coverage", "1aa59450-6669-4a97-837a-6e614bfe63d4");
        requiremodification = projectConfig.isRequiremodification();
        writeline("/home/ubuntu/results/coverage/Project/Project_7_10.coverage", "6cd6e23c-7415-48a4-8e49-47e1bc151728");
        if (lastBuild == null) {
            writeline("/home/ubuntu/results/coverage/Project/Project_7_10.coverage", "807725e1-be93-4269-99e3-e42334391cfd");
            lastBuild = DateUtil.getMidnight();
        }
        writeline("/home/ubuntu/results/coverage/Project/Project_7_10.coverage", "5a1478d0-fa40-40f1-bb01-82e50daf1d56");
        if (lastSuccessfulBuild == null) {
            writeline("/home/ubuntu/results/coverage/Project/Project_7_10.coverage", "1fbb6265-478a-48b2-aa02-6e26cc9cc409");
            lastSuccessfulBuild = lastBuild;
        }
        writeline("/home/ubuntu/results/coverage/Project/Project_7_10.coverage", "be1e8bf3-05a7-4525-ab59-aa44857c0d8f");
        if (LOG.isDebugEnabled()) {
            writeline("/home/ubuntu/results/coverage/Project/Project_7_10.coverage", "b6a8043c-a516-4b51-b35a-d59da4b4adde");
            debug("buildInterval          = [" + getBuildInterval() + "]");
            writeline("/home/ubuntu/results/coverage/Project/Project_7_10.coverage", "ec5f140a-b9ab-4801-b9c0-117aaa5c7b43");
            debug("buildForced            = [" + buildForced + "]");
            writeline("/home/ubuntu/results/coverage/Project/Project_7_10.coverage", "6daa4bfc-9fa7-4edf-bd18-b0abf28a4e19");
            debug("buildAfterFailed       = [" + buildAfterFailed + "]");
            writeline("/home/ubuntu/results/coverage/Project/Project_7_10.coverage", "f601dd34-5ad2-468b-bb10-16ef65976bd8");
            debug("requireModifcation     = [" + requiremodification + "]");
            writeline("/home/ubuntu/results/coverage/Project/Project_7_10.coverage", "70635938-a454-4fe0-a93c-2e643929977c");
            debug("forceOnly              = [" + forceOnly + "]");
            writeline("/home/ubuntu/results/coverage/Project/Project_7_10.coverage", "6375ccd2-13d3-4107-a7fd-3b72fb005676");
            debug("buildCounter           = [" + buildCounter + "]");
            writeline("/home/ubuntu/results/coverage/Project/Project_7_10.coverage", "5be11679-8425-4738-8748-5efa5a23b5ec");
            debug("isPaused               = [" + isPaused + "]");
            writeline("/home/ubuntu/results/coverage/Project/Project_7_10.coverage", "d8a67274-494c-4c43-b9e8-ad97c45453e0");
            debug("label                  = [" + label + "]");
            writeline("/home/ubuntu/results/coverage/Project/Project_7_10.coverage", "327756eb-caa4-45f8-a9f7-6ff50d43a7c1");
            debug("lastBuild              = [" + DateUtil.getFormattedTime(lastBuild) + "]");
            writeline("/home/ubuntu/results/coverage/Project/Project_7_10.coverage", "3f337c5e-3fcb-43e8-8e8a-8c516bdaa91e");
            debug("lastSuccessfulBuild    = [" + DateUtil.getFormattedTime(lastSuccessfulBuild) + "]");
            writeline("/home/ubuntu/results/coverage/Project/Project_7_10.coverage", "6d5caa6f-1d1c-4719-8e39-00949f78f427");
            debug("logDir                 = [" + projectConfig.getLog().getLogDir() + "]");
            writeline("/home/ubuntu/results/coverage/Project/Project_7_10.coverage", "b1a999b5-c0ca-43b6-b4dc-cd644121ffea");
            debug("logXmlEncoding         = [" + projectConfig.getLog().getLogXmlEncoding() + "]");
            writeline("/home/ubuntu/results/coverage/Project/Project_7_10.coverage", "80a3b396-6a56-4517-8f8a-c26cf670b60e");
            debug("wasLastBuildSuccessful = [" + wasLastBuildSuccessful + "]");
        }
    }

    protected Element getProjectPropertiesElement(final Date now) {
        writeline("/home/ubuntu/results/coverage/Project/Project_7_10.coverage", "205a0b7d-b574-4070-90ed-328f0c7e3066");
        final Element infoElement = new Element("info");
        writeline("/home/ubuntu/results/coverage/Project/Project_7_10.coverage", "f17c6b9a-85c8-4d04-a705-dbab4b1db063");
        addProperty(infoElement, "projectname", name);
        writeline("/home/ubuntu/results/coverage/Project/Project_7_10.coverage", "14b921de-d4ce-4ada-8709-8d3864d9c662");
        final String lastBuildString = DateUtil.getFormattedTime(lastBuild == null ? now : lastBuild);
        writeline("/home/ubuntu/results/coverage/Project/Project_7_10.coverage", "b46816d7-6dbe-4287-8333-430d25bffaac");
        addProperty(infoElement, "lastbuild", lastBuildString);
        writeline("/home/ubuntu/results/coverage/Project/Project_7_10.coverage", "40b5bc0c-1482-4ecf-82e8-4c4efd2f79a8");
        final String lastSuccessfulBuildString = DateUtil.getFormattedTime(lastSuccessfulBuild == null ? now : lastSuccessfulBuild);
        writeline("/home/ubuntu/results/coverage/Project/Project_7_10.coverage", "65f73c0d-1ba0-4901-a15a-a8d79bad8e35");
        addProperty(infoElement, "lastsuccessfulbuild", lastSuccessfulBuildString);
        writeline("/home/ubuntu/results/coverage/Project/Project_7_10.coverage", "64dfb771-1866-4c7b-8bc8-1baf75bd595c");
        addProperty(infoElement, "builddate", DateUtil.formatIso8601(now));
        writeline("/home/ubuntu/results/coverage/Project/Project_7_10.coverage", "0e812657-e20d-4337-a6dd-1c1ee4ea2f38");
        addProperty(infoElement, "cctimestamp", DateUtil.getFormattedTime(now));
        writeline("/home/ubuntu/results/coverage/Project/Project_7_10.coverage", "11152326-a250-4918-8358-b63c915e128f");
        addProperty(infoElement, "label", label);
        writeline("/home/ubuntu/results/coverage/Project/Project_7_10.coverage", "d93e939c-9f61-40ab-89ae-db0c4b3ef1da");
        addProperty(infoElement, "interval", Long.toString(getBuildInterval() / 1000L));
        writeline("/home/ubuntu/results/coverage/Project/Project_7_10.coverage", "f76e034b-e542-40fd-931f-2ef54f296563");
        addProperty(infoElement, "lastbuildsuccessful", String.valueOf(wasLastBuildSuccessful));
        writeline("/home/ubuntu/results/coverage/Project/Project_7_10.coverage", "f6f8f754-613e-4949-977c-c806ed58b01e");
        return infoElement;
    }

    private void addProperty(final Element parent, final String key, final String value) {
        writeline("/home/ubuntu/results/coverage/Project/Project_7_10.coverage", "08152bca-40d2-4a47-b2d9-919da2f6ba90");
        final Element propertyElement = new Element("property");
        writeline("/home/ubuntu/results/coverage/Project/Project_7_10.coverage", "bbd61994-2851-45e0-a490-87adf7a8d769");
        propertyElement.setAttribute("name", key);
        writeline("/home/ubuntu/results/coverage/Project/Project_7_10.coverage", "c3b02986-fae5-4e31-a625-0b644cdb71ca");
        propertyElement.setAttribute("value", value);
        writeline("/home/ubuntu/results/coverage/Project/Project_7_10.coverage", "9e42a7aa-b620-4189-a9b6-3da2c39bbc88");
        parent.addContent(propertyElement);
    }

    protected Map<String, String> getProjectPropertiesMap(final Date now) {
        writeline("/home/ubuntu/results/coverage/Project/Project_7_10.coverage", "94663f95-883d-4319-b970-3c31429f25cf");
        final Map<String, String> buildProperties = new HashMap<String, String>();
        writeline("/home/ubuntu/results/coverage/Project/Project_7_10.coverage", "ccbabdeb-a10a-481d-92d0-fcd0ff337f60");
        buildProperties.put("projectname", name);
        writeline("/home/ubuntu/results/coverage/Project/Project_7_10.coverage", "b7bb3cd5-7f78-4776-bbe1-7b89697a226f");
        buildProperties.put("label", label);
        writeline("/home/ubuntu/results/coverage/Project/Project_7_10.coverage", "6314b71b-3a44-48c8-a62f-4f6a884d1b4b");
        // TODO: Shouldn't have CVS specific properties here
        buildProperties.put("cvstimestamp", CVSDateUtil.formatCVSDate(now));
        writeline("/home/ubuntu/results/coverage/Project/Project_7_10.coverage", "c815c51c-f8f0-4a39-b477-7eaec941db74");
        buildProperties.put("cctimestamp", DateUtil.getFormattedTime(now));
        writeline("/home/ubuntu/results/coverage/Project/Project_7_10.coverage", "ebf2a43d-9807-4a9e-a2d0-a90285f9c6b7");
        buildProperties.put("cclastgoodbuildtimestamp", getLastSuccessfulBuild());
        writeline("/home/ubuntu/results/coverage/Project/Project_7_10.coverage", "5f117ed0-cf01-4171-96db-12bf6abc5cc4");
        buildProperties.put("cclastbuildtimestamp", getLastBuild());
        writeline("/home/ubuntu/results/coverage/Project/Project_7_10.coverage", "885fb1de-70ed-48be-898a-1eb135e80b64");
        buildProperties.put("lastbuildsuccessful", String.valueOf(isLastBuildSuccessful()));
        writeline("/home/ubuntu/results/coverage/Project/Project_7_10.coverage", "50b02d81-2a11-427b-ae3d-d227e0144b03");
        buildProperties.put("buildforced", String.valueOf(getBuildForced()));
        writeline("/home/ubuntu/results/coverage/Project/Project_7_10.coverage", "90fee663-9639-40e7-bd19-bf6c00be5aa9");
        if (projectConfig.getModificationSet() != null) {
            writeline("/home/ubuntu/results/coverage/Project/Project_7_10.coverage", "c1255250-0d26-4d2c-9e50-3454613d1a41");
            buildProperties.putAll(projectConfig.getModificationSet().getProperties());
        }
        writeline("/home/ubuntu/results/coverage/Project/Project_7_10.coverage", "b6559b37-e491-4889-abd7-74d03e7dcc6d");
        if (additionalProperties != null && !additionalProperties.isEmpty()) {
            writeline("/home/ubuntu/results/coverage/Project/Project_7_10.coverage", "10229b98-4163-4949-80b7-af5790cbff5e");
            buildProperties.putAll(additionalProperties);
            writeline("/home/ubuntu/results/coverage/Project/Project_7_10.coverage", "fca1d1b6-5b19-4a6d-ab6d-36e894651a1f");
            additionalProperties.clear();
            writeline("/home/ubuntu/results/coverage/Project/Project_7_10.coverage", "1b307341-f1b6-4b9f-a5f7-4c4b74f5c283");
            additionalProperties = null;
        }
        writeline("/home/ubuntu/results/coverage/Project/Project_7_10.coverage", "8cb4cdcd-dddf-4ee1-afa4-efc4a9f2eb74");
        return buildProperties;
    }

    /**
     * Intended only for unit testing.
     * @return additional Properties variable.
     */
    Map<String, String> getAdditionalProperties() {
        writeline("/home/ubuntu/results/coverage/Project/Project_7_10.coverage", "bec29071-3a70-43eb-b4b7-d309a6a5836f");
        return additionalProperties;
    }

    /**
     * Iterate over all of the registered <code>Publisher</code>s and call
     * their respective <code>publish</code> methods.
     * @param buildLog the content to publish
     * @throws CruiseControlException if an error occurs during publishing
     */
    protected void publish(final Log buildLog) throws CruiseControlException {
        writeline("/home/ubuntu/results/coverage/Project/Project_7_10.coverage", "483bc298-c32d-4d93-804a-f1fac93f2857");
        setState(ProjectState.PUBLISHING);
        writeline("/home/ubuntu/results/coverage/Project/Project_7_10.coverage", "f883d4e0-e09d-4168-aec4-4dc0253622e6");
        for (final Publisher publisher : projectConfig.getPublishers()) {
            writeline("/home/ubuntu/results/coverage/Project/Project_7_10.coverage", "6e71a300-1703-4e3b-b36b-b91b315926a6");
            // catch all errors, Publishers shouldn't cause failures in the build method
            try {
                writeline("/home/ubuntu/results/coverage/Project/Project_7_10.coverage", "3baea741-556f-41e1-8c6f-4dd429c0e443");
                publisher.publish(buildLog.getContent());
            } catch (Throwable t) {
                writeline("/home/ubuntu/results/coverage/Project/Project_7_10.coverage", "31e77977-d20b-4349-b406-3c57f297440d");
                final StringBuilder message = new StringBuilder("exception publishing results");
                writeline("/home/ubuntu/results/coverage/Project/Project_7_10.coverage", "b50e9420-0cf7-409e-99b5-d948316cade8");
                message.append(" with ").append(publisher.getClass().getName());
                writeline("/home/ubuntu/results/coverage/Project/Project_7_10.coverage", "dbdb6dcb-c0ef-49a6-ace4-dfa321437733");
                message.append(" for project ").append(name);
                writeline("/home/ubuntu/results/coverage/Project/Project_7_10.coverage", "cf47f39c-b815-4e8b-9b78-db6d11464b38");
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
        writeline("/home/ubuntu/results/coverage/Project/Project_7_10.coverage", "74f8b0b6-be4b-4fb1-b9e8-6ec2fdb3633b");
        setState(ProjectState.BOOTSTRAPPING);
        writeline("/home/ubuntu/results/coverage/Project/Project_7_10.coverage", "19efd77e-a985-44b1-b202-a84eb7511241");
        for (final Bootstrapper bootstrapper : projectConfig.getBootstrappers()) {
            writeline("/home/ubuntu/results/coverage/Project/Project_7_10.coverage", "ae5ee0fc-0135-4651-96f4-371ba88cae81");
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
        writeline("/home/ubuntu/results/coverage/Project/Project_7_10.coverage", "2a5c841c-d2f5-4fe8-a743-52ca0d62d411");
        if (!incrementer.isValidLabel(oldLabel)) {
            writeline("/home/ubuntu/results/coverage/Project/Project_7_10.coverage", "6acfc5cb-a0dd-4202-8d9d-e94056e2478e");
            final String message = oldLabel + " is not a valid label for labelIncrementer " + incrementer.getClass().getName();
            writeline("/home/ubuntu/results/coverage/Project/Project_7_10.coverage", "4f40b3f9-01a7-40bd-b913-871cb9f97259");
            debug(message);
            writeline("/home/ubuntu/results/coverage/Project/Project_7_10.coverage", "a412dbcb-7841-4829-bd66-1ed0c726c7f8");
            throw new CruiseControlException(message);
        }
    }

    public boolean isLastBuildSuccessful() {
        writeline("/home/ubuntu/results/coverage/Project/Project_7_10.coverage", "ca062fc7-483d-4f5b-96be-5b8bb9aeb86a");
        return wasLastBuildSuccessful;
    }

    void setWasLastBuildSuccessful(final boolean buildSuccessful) {
        writeline("/home/ubuntu/results/coverage/Project/Project_7_10.coverage", "e4b4c56c-26ce-4fe7-95cd-6f3033993937");
        wasLastBuildSuccessful = buildSuccessful;
    }

    /**
     * Logs a message to the application log, not to be confused with the
     * CruiseControl build log.
     * @param message the application message to log
     */
    private void info(final String message) {
        writeline("/home/ubuntu/results/coverage/Project/Project_7_10.coverage", "b71d6650-70ff-4223-b588-187255e66236");
        LOG.info("Project " + name + ":  " + message);
    }

    private void debug(final String message) {
        writeline("/home/ubuntu/results/coverage/Project/Project_7_10.coverage", "fd398194-7466-4704-96a6-943ccc79ff1f");
        LOG.debug("Project " + name + ":  " + message);
    }

    public void start() {
        writeline("/home/ubuntu/results/coverage/Project/Project_7_10.coverage", "3aeeeb90-f9c7-4901-a5ba-91f1a85eab27");
        if (stopped || getState() == ProjectState.STOPPED) {
            writeline("/home/ubuntu/results/coverage/Project/Project_7_10.coverage", "7bcbacfc-86c6-4100-a835-64dd09111074");
            stopped = false;
            writeline("/home/ubuntu/results/coverage/Project/Project_7_10.coverage", "7c8066ee-aeb4-434c-88ce-d5fd19b4e9e5");
            LOG.info("Project " + name + " starting");
            writeline("/home/ubuntu/results/coverage/Project/Project_7_10.coverage", "b6967a72-557a-492d-b293-0624530c434a");
            setState(ProjectState.IDLE);
            writeline("/home/ubuntu/results/coverage/Project/Project_7_10.coverage", "a5eb6612-9ef5-4ea7-8a11-f47f53ad0b1e");
            createNewSchedulingThread();
        }
    }

    protected void createNewSchedulingThread() {
        writeline("/home/ubuntu/results/coverage/Project/Project_7_10.coverage", "cc3b95d4-06da-49b5-89dd-8da057e8bfba");
        final Thread projectSchedulingThread = new Thread(this, "Project " + getName() + " thread");
        writeline("/home/ubuntu/results/coverage/Project/Project_7_10.coverage", "06497de0-839f-4275-a8d9-ef869719d9da");
        projectSchedulingThread.start();
        writeline("/home/ubuntu/results/coverage/Project/Project_7_10.coverage", "ae7c72d2-e2ce-4fdf-be6d-dddc7a110957");
        // brief nap to allow thread to start
        try {
            writeline("/home/ubuntu/results/coverage/Project/Project_7_10.coverage", "f8747443-f386-42f6-bb04-bbceb2df4c5a");
            Thread.sleep(100);
        } catch (InterruptedException ie) {
            writeline("/home/ubuntu/results/coverage/Project/Project_7_10.coverage", "fbc96e63-69cd-4a52-a0b7-5ea13dccb207");
            LOG.warn("interrupted while waiting for scheduling thread to start", ie);
        }
    }

    public void stop() {
        writeline("/home/ubuntu/results/coverage/Project/Project_7_10.coverage", "b540413b-928c-42f3-8271-89082a152406");
        LOG.info("Project " + name + " stopping");
        writeline("/home/ubuntu/results/coverage/Project/Project_7_10.coverage", "d7d4ab19-4ce8-4a1e-aa61-75b4ca288fff");
        stopped = true;
        writeline("/home/ubuntu/results/coverage/Project/Project_7_10.coverage", "00b8742d-947f-4ed2-b0fc-962fffef78b3");
        setState(ProjectState.STOPPED);
        writeline("/home/ubuntu/results/coverage/Project/Project_7_10.coverage", "bd3e2c1b-c75f-464b-8b84-20743ab3349b");
        synchronized (pausedMutex) {
            writeline("/home/ubuntu/results/coverage/Project/Project_7_10.coverage", "fd838497-1c2a-4f04-b9fc-4acfe28a426c");
            pausedMutex.notifyAll();
        }
        writeline("/home/ubuntu/results/coverage/Project/Project_7_10.coverage", "2e8e68f6-f639-4aa3-8d8d-e29c62ccf9d3");
        synchronized (waitMutex) {
            writeline("/home/ubuntu/results/coverage/Project/Project_7_10.coverage", "c15b569d-cf93-419f-89d7-91f96d7b2b98");
            waitMutex.notifyAll();
        }
        writeline("/home/ubuntu/results/coverage/Project/Project_7_10.coverage", "12f6b17d-ca02-45a5-bd62-dc0d8316d2cb");
        synchronized (scheduleMutex) {
            writeline("/home/ubuntu/results/coverage/Project/Project_7_10.coverage", "1b9da03b-4e80-418b-ae7d-a443206b44ed");
            scheduleMutex.notifyAll();
        }
    }

    public String toString() {
        writeline("/home/ubuntu/results/coverage/Project/Project_7_10.coverage", "326aa025-7a4c-4acc-bb2c-5c624ec6ab8f");
        final StringBuilder sb = new StringBuilder("Project ");
        writeline("/home/ubuntu/results/coverage/Project/Project_7_10.coverage", "1405d704-94db-4a4e-bbd6-cbf3e9110df2");
        sb.append(getName());
        writeline("/home/ubuntu/results/coverage/Project/Project_7_10.coverage", "45d8d6cb-1b3d-4055-82ed-c2f5756325d0");
        sb.append(": ");
        writeline("/home/ubuntu/results/coverage/Project/Project_7_10.coverage", "8e9bcb3d-0927-445e-abd8-3776fa9bbe42");
        sb.append(getStatus());
        writeline("/home/ubuntu/results/coverage/Project/Project_7_10.coverage", "80d61951-cde7-4d46-a9fb-8ce731e6ab46");
        if (isPaused) {
            writeline("/home/ubuntu/results/coverage/Project/Project_7_10.coverage", "04caf070-d1af-4fd2-83db-99be11173eda");
            sb.append(" (paused)");
        }
        writeline("/home/ubuntu/results/coverage/Project/Project_7_10.coverage", "2c9b6c77-034d-4e38-b899-cfb6a8d67796");
        return sb.toString();
    }

    public void addBuildProgressListener(final BuildProgressListener listener) {
        writeline("/home/ubuntu/results/coverage/Project/Project_7_10.coverage", "4d6cb837-d772-47d9-95cc-b214818ce6eb");
        synchronized (progressListeners) {
            writeline("/home/ubuntu/results/coverage/Project/Project_7_10.coverage", "126c1188-54fd-4902-8551-96cbe09faa4e");
            progressListeners.add(listener);
        }
    }

    protected void fireProgressEvent(final BuildProgressEvent event) {
        writeline("/home/ubuntu/results/coverage/Project/Project_7_10.coverage", "b5e94a52-8100-4b9f-b635-669a6c9448eb");
        synchronized (progressListeners) {
            writeline("/home/ubuntu/results/coverage/Project/Project_7_10.coverage", "4e74a133-f774-43d3-98c9-bad539de5e7e");
            for (final BuildProgressListener listener : progressListeners) {
                writeline("/home/ubuntu/results/coverage/Project/Project_7_10.coverage", "efc01771-ec23-4575-addf-be91109b8252");
                listener.handleBuildProgress(event);
            }
        }
    }

    public void addBuildResultListener(final BuildResultListener listener) {
        writeline("/home/ubuntu/results/coverage/Project/Project_7_10.coverage", "cbfeb314-4834-4363-821f-9f0877ec7641");
        synchronized (resultListeners) {
            writeline("/home/ubuntu/results/coverage/Project/Project_7_10.coverage", "e56736ea-877b-498e-ad86-62ed7afd4455");
            resultListeners.add(listener);
        }
    }

    protected void fireResultEvent(final BuildResultEvent event) {
        writeline("/home/ubuntu/results/coverage/Project/Project_7_10.coverage", "61397032-36ad-4125-8f72-d1a1da21c2ff");
        synchronized (resultListeners) {
            writeline("/home/ubuntu/results/coverage/Project/Project_7_10.coverage", "7ce04306-b0d3-4f04-b163-027cadf38712");
            for (final BuildResultListener listener : resultListeners) {
                writeline("/home/ubuntu/results/coverage/Project/Project_7_10.coverage", "5e945446-1c6f-411e-a84e-923bd38b6923");
                listener.handleBuildResult(event);
            }
        }
    }

    List<Listener> getListeners() {
        writeline("/home/ubuntu/results/coverage/Project/Project_7_10.coverage", "c95c722a-28f6-479d-8f21-d685e0dac34e");
        return projectConfig.getListeners();
    }

    public void setProjectConfig(final ProjectConfig projectConfig) throws CruiseControlException {
        writeline("/home/ubuntu/results/coverage/Project/Project_7_10.coverage", "def85b66-0f6e-409f-946c-9add9457e24d");
        if (projectConfig == null) {
            writeline("/home/ubuntu/results/coverage/Project/Project_7_10.coverage", "79101f19-0980-4af2-a5a6-fa7d410d2c70");
            throw new IllegalArgumentException("project config can't be null");
        }
        writeline("/home/ubuntu/results/coverage/Project/Project_7_10.coverage", "2af350f0-d425-4032-aec2-5d71d8c50d5f");
        this.projectConfig = projectConfig;
        writeline("/home/ubuntu/results/coverage/Project/Project_7_10.coverage", "7cf13656-d419-4921-9cae-1b26b7a7a9f6");
        setLabelIncrementer(projectConfig.getLabelIncrementer());
    }

    void notifyListeners(final ProjectEvent event) {
        writeline("/home/ubuntu/results/coverage/Project/Project_7_10.coverage", "dac219a3-d9d1-4c57-bca8-c7c1fc80c750");
        if (projectConfig == null) {
            writeline("/home/ubuntu/results/coverage/Project/Project_7_10.coverage", "32e5c7f0-e090-4e9e-86e0-7eaa9d3b1c2d");
            throw new IllegalStateException("projectConfig is null");
        }
        writeline("/home/ubuntu/results/coverage/Project/Project_7_10.coverage", "8af7d9a9-5403-4e0b-b8f8-225150952666");
        for (final Listener listener : projectConfig.getListeners()) {
            writeline("/home/ubuntu/results/coverage/Project/Project_7_10.coverage", "e64684b1-62fa-423b-929a-12cc9357c511");
            try {
                writeline("/home/ubuntu/results/coverage/Project/Project_7_10.coverage", "b2e3337f-c4d0-42ca-85fa-a0697371f60d");
                listener.handleEvent(event);
            } catch (CruiseControlException e) {
                writeline("/home/ubuntu/results/coverage/Project/Project_7_10.coverage", "571e15ee-688d-4721-82fe-4a915ecf54ac");
                final StringBuilder message = new StringBuilder("exception notifying listener ");
                writeline("/home/ubuntu/results/coverage/Project/Project_7_10.coverage", "e34ed7d0-6dcb-4776-97dd-9b6e2e193a6f");
                message.append(listener.getClass().getName());
                writeline("/home/ubuntu/results/coverage/Project/Project_7_10.coverage", "54f7bf67-a273-40d4-8213-8d1a14f1c2f2");
                message.append(" for project ").append(name);
                writeline("/home/ubuntu/results/coverage/Project/Project_7_10.coverage", "92ef4ebe-5fdf-400d-9e2f-eb5438f645c0");
                LOG.error(message.toString(), e);
            }
        }
    }

    public boolean equals(final Object arg0) {
        writeline("/home/ubuntu/results/coverage/Project/Project_7_10.coverage", "b570c38e-28cd-4fca-bd12-c75235427ce5");
        if (arg0 == null) {
            writeline("/home/ubuntu/results/coverage/Project/Project_7_10.coverage", "ee7dee35-15c3-4064-be0f-70544fe20848");
            return false;
        }
        writeline("/home/ubuntu/results/coverage/Project/Project_7_10.coverage", "22d7c453-4f0f-4f67-a5e3-c8a7044215f6");
        if (arg0.getClass().getName().equals(getClass().getName())) {
            writeline("/home/ubuntu/results/coverage/Project/Project_7_10.coverage", "e625cec1-8fff-48e6-aeb3-b245a9aa7471");
            final Project thatProject = (Project) arg0;
            writeline("/home/ubuntu/results/coverage/Project/Project_7_10.coverage", "4379a050-bf9b-4cbc-8d10-547d2cdfcbab");
            return thatProject.name.equals(name);
        }
        writeline("/home/ubuntu/results/coverage/Project/Project_7_10.coverage", "aedb8b4e-2f37-418c-8857-8f1dd5b24399");
        return false;
    }

    public int hashCode() {
        writeline("/home/ubuntu/results/coverage/Project/Project_7_10.coverage", "820ac388-978e-4f11-876b-2ea5e8e4bff7");
        return name.hashCode();
    }

    public void register(final MBeanServer server) throws JMException {
        writeline("/home/ubuntu/results/coverage/Project/Project_7_10.coverage", "6d559b20-0e1b-4463-95c7-86892436ce78");
        LOG.debug("Registering project mbean");
        writeline("/home/ubuntu/results/coverage/Project/Project_7_10.coverage", "3c4f8135-e458-4d82-892b-59b0c089ce20");
        final ProjectController projectController = new ProjectController(this);
        writeline("/home/ubuntu/results/coverage/Project/Project_7_10.coverage", "59a607c0-7ab9-474c-aa20-f8337aae7130");
        projectController.register(server);
    }

    public ProjectConfig getProjectConfig() {
        writeline("/home/ubuntu/results/coverage/Project/Project_7_10.coverage", "9ab9797b-d63d-4947-bc32-878f58619cc1");
        return projectConfig;
    }

    public Date getLastBuildDate() {
        writeline("/home/ubuntu/results/coverage/Project/Project_7_10.coverage", "1e5bfacf-381e-4a96-a21a-9d2d9de2e450");
        return lastBuild;
    }

    public Progress getProgress() {
        writeline("/home/ubuntu/results/coverage/Project/Project_7_10.coverage", "28243c39-dc33-42a4-911e-a0d3f9f59b03");
        return progress;
    }

    public List<String> getLogLabels() {
        writeline("/home/ubuntu/results/coverage/Project/Project_7_10.coverage", "b46bead0-bbae-4b42-bd86-7e618ee5510f");
        return projectConfig.getLogLabels();
    }

    public String[] getLogLabelLines(final String logLabel, final int firstLine) {
        writeline("/home/ubuntu/results/coverage/Project/Project_7_10.coverage", "b37ef420-2e68-4b59-b596-d392e9cff4e4");
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
