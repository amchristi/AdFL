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
        writeline("/home/ubuntu/results/coverage/Project/Project_2_10.coverage", "24174afe-614a-4748-9384-3bd4d7e6e91f");
        state = ProjectState.STOPPED;
        writeline("/home/ubuntu/results/coverage/Project/Project_2_10.coverage", "2fa5fc41-9b3d-4985-9991-3d1ad941067e");
        pausedMutex = new Object();
        writeline("/home/ubuntu/results/coverage/Project/Project_2_10.coverage", "dcd79b08-c146-4c69-b10c-aaf902de45a7");
        scheduleMutex = new Object();
        writeline("/home/ubuntu/results/coverage/Project/Project_2_10.coverage", "c277b016-fcef-4e18-9d76-9825a79bd9a7");
        waitMutex = new Object();
        writeline("/home/ubuntu/results/coverage/Project/Project_2_10.coverage", "b9cc4997-7d28-4ffc-a8f9-b28a9c538091");
        progressListeners = new ArrayList<BuildProgressListener>();
        writeline("/home/ubuntu/results/coverage/Project/Project_2_10.coverage", "1958a945-102c-48de-9c8e-a09cff32870f");
        resultListeners = new ArrayList<BuildResultListener>();
        writeline("/home/ubuntu/results/coverage/Project/Project_2_10.coverage", "0d8f2dbb-69d5-4b65-b937-ed3ebc8f692c");
        progress = new ProgressImpl(this);
    }

    private void readObject(final ObjectInputStream stream) throws IOException, ClassNotFoundException {
        writeline("/home/ubuntu/results/coverage/Project/Project_2_10.coverage", "2135d205-6648-4bea-988d-845b2d075dc8");
        stream.defaultReadObject();
        writeline("/home/ubuntu/results/coverage/Project/Project_2_10.coverage", "9d9e5aac-0e9c-4434-ae01-d8f316f3ea5b");
        initializeTransientFields();
    }

    public void execute() {
        writeline("/home/ubuntu/results/coverage/Project/Project_2_10.coverage", "c956ae4b-8262-4991-a4c3-f7ad343bb275");
        if (stopped) {
            writeline("/home/ubuntu/results/coverage/Project/Project_2_10.coverage", "ea766900-0e8a-4492-aa1b-151ea93a0744");
            LOG.warn("not building project " + name + " because project has been stopped.");
            writeline("/home/ubuntu/results/coverage/Project/Project_2_10.coverage", "d6cbeba1-799d-4d18-a518-c04fd4675250");
            buildFinished();
            writeline("/home/ubuntu/results/coverage/Project/Project_2_10.coverage", "32c1624d-4d38-4523-9ed7-f7218fe62b2f");
            return;
        }
        writeline("/home/ubuntu/results/coverage/Project/Project_2_10.coverage", "7c9d00ea-1664-49ad-be66-ce5ee76b4f9a");
        synchronized (pausedMutex) {
            writeline("/home/ubuntu/results/coverage/Project/Project_2_10.coverage", "0a352ab4-6f50-4ca4-9246-f0d94459812c");
            if (isPaused) {
                writeline("/home/ubuntu/results/coverage/Project/Project_2_10.coverage", "2caca102-37e9-4d20-b4a1-17634ccb2256");
                LOG.info("not building project " + name + " because project has been paused.");
                writeline("/home/ubuntu/results/coverage/Project/Project_2_10.coverage", "71c58b54-c6f6-45e6-bef5-cb21c815f4e4");
                buildFinished();
                writeline("/home/ubuntu/results/coverage/Project/Project_2_10.coverage", "ae2b7b8b-8774-4193-85dd-ae2ef7bc3825");
                return;
            }
        }
        writeline("/home/ubuntu/results/coverage/Project/Project_2_10.coverage", "47223b20-ffa6-4bb1-93aa-56c5f7225ce3");
        try {
            writeline("/home/ubuntu/results/coverage/Project/Project_2_10.coverage", "ef22a79e-dff5-43fa-a9e9-1f6e135850d9");
            init();
            writeline("/home/ubuntu/results/coverage/Project/Project_2_10.coverage", "82aa4ac4-d0b3-44ae-84df-52ebac34f80a");
            build();
        } catch (CruiseControlException e) {
            writeline("/home/ubuntu/results/coverage/Project/Project_2_10.coverage", "e7e47c41-ca23-4a48-a81d-b6abc92dd58f");
            LOG.error("exception attempting build in project " + name, e);
        } finally {
            writeline("/home/ubuntu/results/coverage/Project/Project_2_10.coverage", "4db5bfe6-7fc5-4eba-aaf4-38ce2a6b72e3");
            buildFinished();
        }
    }

    /**
     * Unless paused, runs any bootstrappers and then the entire build.
     * @throws CruiseControlException if an error occurs during the build
     */
    protected void build() throws CruiseControlException {
        writeline("/home/ubuntu/results/coverage/Project/Project_2_10.coverage", "6d99a151-77c5-4f5f-a4d3-f739b4ba9562");
        if (projectConfig == null) {
            writeline("/home/ubuntu/results/coverage/Project/Project_2_10.coverage", "1272b58f-22d3-4283-b5e8-a37a3a3e54b2");
            throw new IllegalStateException("projectConfig must be set on project before calling build()");
        }
        writeline("/home/ubuntu/results/coverage/Project/Project_2_10.coverage", "db5533f2-18bb-4dde-a6c3-58dd6d5ae69b");
        if (stopped) {
            writeline("/home/ubuntu/results/coverage/Project/Project_2_10.coverage", "af58fa96-8e51-46d5-9dc2-c00693ca299c");
            LOG.warn("not building project " + name + " because project has been stopped.");
            writeline("/home/ubuntu/results/coverage/Project/Project_2_10.coverage", "0edac76f-cbd5-498f-933a-8d7badcc3f13");
            return;
        }
        writeline("/home/ubuntu/results/coverage/Project/Project_2_10.coverage", "77840bd3-abeb-4a86-b082-a758a6814f5a");
        // or if the last build faild and we want to build on failures
        if (forceOnly && !buildForced && !(!wasLastBuildSuccessful && buildAfterFailed)) {
            writeline("/home/ubuntu/results/coverage/Project/Project_2_10.coverage", "a14687a4-481d-4bbb-9f23-3c63c22b2cc5");
            info("not building because project is forceOnly and build not forced.");
            writeline("/home/ubuntu/results/coverage/Project/Project_2_10.coverage", "410f8b76-abe2-4d32-a8b2-62efbfdfde0f");
            return;
        }
        writeline("/home/ubuntu/results/coverage/Project/Project_2_10.coverage", "28d98ce2-03e1-4153-8eb7-a35e8b39af2f");
        final boolean buildWasForced = buildForced;
        writeline("/home/ubuntu/results/coverage/Project/Project_2_10.coverage", "5cb21763-6fb8-4049-99e2-9c80faf35642");
        try {
            writeline("/home/ubuntu/results/coverage/Project/Project_2_10.coverage", "b701b70a-0394-4384-a153-96fcff8314b1");
            setBuildStartTime(new Date());
            writeline("/home/ubuntu/results/coverage/Project/Project_2_10.coverage", "62d97058-ddc8-42ed-ae3a-0e67e0912579");
            final Schedule schedule = projectConfig.getSchedule();
            writeline("/home/ubuntu/results/coverage/Project/Project_2_10.coverage", "4528f008-b4e2-453f-ac60-7b2e296efa1b");
            if (schedule == null) {
                writeline("/home/ubuntu/results/coverage/Project/Project_2_10.coverage", "f6dcc2c9-20e7-4fb8-aa9b-8aa859b2dc47");
                throw new IllegalStateException("project must have a schedule");
            }
            writeline("/home/ubuntu/results/coverage/Project/Project_2_10.coverage", "52b66e35-3a0d-4fcb-aa4f-b9a89b86fbdb");
            if (schedule.isPaused(buildStartTime)) {
                writeline("/home/ubuntu/results/coverage/Project/Project_2_10.coverage", "a87ff3ed-d15e-4864-8a1f-c1624cc7ccea");
                // is different than ProjectState.PAUSED
                return;
            }
            writeline("/home/ubuntu/results/coverage/Project/Project_2_10.coverage", "72717756-2665-42dd-b3e5-b9a68af0683c");
            // @todo Add Progress param to Bootstrapper API?
            bootstrap();
            writeline("/home/ubuntu/results/coverage/Project/Project_2_10.coverage", "c9448dbd-21a0-4afc-bbca-fa7d24aee509");
            final String target = useAndResetBuildTargetIfBuildWasForced(buildWasForced);
            writeline("/home/ubuntu/results/coverage/Project/Project_2_10.coverage", "a6dfa232-32e0-451f-8343-dbaeb3165e26");
            // @todo Add Progress param to ModificationSet API?
            // getModifications will only return null if we don't need to build
            final Element modifications = getModifications(buildWasForced);
            writeline("/home/ubuntu/results/coverage/Project/Project_2_10.coverage", "23bb33ae-db2a-479c-a8fe-ac06f247cecb");
            if (modifications == null) {
                writeline("/home/ubuntu/results/coverage/Project/Project_2_10.coverage", "4a9a4f87-0bdb-45f9-a053-db5ca6f69370");
                return;
            }
            writeline("/home/ubuntu/results/coverage/Project/Project_2_10.coverage", "4cdeb1f3-e028-4953-afed-3c60bc6b5c25");
            // Using local reference to avoid NPE if config.xml is updated during build
            final Log buildLog = projectConfig.getLog();
            writeline("/home/ubuntu/results/coverage/Project/Project_2_10.coverage", "78da2c19-ff88-490d-875b-76551343e6a3");
            buildLog.addContent(modifications);
            writeline("/home/ubuntu/results/coverage/Project/Project_2_10.coverage", "9c87f7c5-bd63-426a-b287-ae9bc5cb0c01");
            final Date now;
            writeline("/home/ubuntu/results/coverage/Project/Project_2_10.coverage", "c86c3561-9eb4-4fed-b26f-388419a32325");
            if (projectConfig.getModificationSet() != null && projectConfig.getModificationSet().getTimeOfCheck() != null) {
                writeline("/home/ubuntu/results/coverage/Project/Project_2_10.coverage", "53b2c958-1a03-420d-8c91-727e178766d4");
                now = projectConfig.getModificationSet().getTimeOfCheck();
            } else {
                writeline("/home/ubuntu/results/coverage/Project/Project_2_10.coverage", "7eec19e7-ef06-4425-998c-125f5c44f2c3");
                now = new Date();
            }
            writeline("/home/ubuntu/results/coverage/Project/Project_2_10.coverage", "d861c4d8-6fb2-4310-ae2b-5421f6bfedf7");
            if (getLabelIncrementer().isPreBuildIncrementer()) {
                writeline("/home/ubuntu/results/coverage/Project/Project_2_10.coverage", "bfbe4ad4-0f09-43db-833f-8d9c8a985274");
                label = getLabelIncrementer().incrementLabel(label, buildLog.getContent());
            }
            writeline("/home/ubuntu/results/coverage/Project/Project_2_10.coverage", "d8b3a82b-a109-44f6-a4e4-56b817d67efa");
            // collect project information
            buildLog.addContent(getProjectPropertiesElement(now));
            writeline("/home/ubuntu/results/coverage/Project/Project_2_10.coverage", "4bed73f0-3d3b-4210-afd3-d36af120c077");
            setState(ProjectState.BUILDING);
            writeline("/home/ubuntu/results/coverage/Project/Project_2_10.coverage", "7eb943fc-2640-48f4-9b80-22504a69834b");
            final Element builderLog = schedule.build(buildCounter, lastBuild, now, getProjectPropertiesMap(now), target, progress);
            writeline("/home/ubuntu/results/coverage/Project/Project_2_10.coverage", "41f1099c-f77b-4ff5-8abc-791558df1ee6");
            buildLog.addContent(builderLog.detach());
            writeline("/home/ubuntu/results/coverage/Project/Project_2_10.coverage", "14154917-11af-4748-9c9c-ccb84c048d51");
            boolean buildSuccessful = buildLog.wasBuildSuccessful();
            writeline("/home/ubuntu/results/coverage/Project/Project_2_10.coverage", "685888ac-f6bd-4ec7-b833-3eb9c0c427c4");
            fireResultEvent(new BuildResultEvent(this, buildSuccessful));
            writeline("/home/ubuntu/results/coverage/Project/Project_2_10.coverage", "40e39094-1ba8-4e80-bef2-cb2fdc348cf2");
            if (!getLabelIncrementer().isPreBuildIncrementer() && buildSuccessful) {
                writeline("/home/ubuntu/results/coverage/Project/Project_2_10.coverage", "6a4c0b17-2c39-4a40-9ee1-5fbf707cff2a");
                label = getLabelIncrementer().incrementLabel(label, buildLog.getContent());
            }
            writeline("/home/ubuntu/results/coverage/Project/Project_2_10.coverage", "d2b2e6eb-037b-4916-b8cb-8fffbfdf7769");
            setState(ProjectState.MERGING_LOGS);
            writeline("/home/ubuntu/results/coverage/Project/Project_2_10.coverage", "e3a4df34-45f0-42b8-b055-300592a0c2f6");
            buildLog.writeLogFile(now);
            writeline("/home/ubuntu/results/coverage/Project/Project_2_10.coverage", "74a9c026-8ea2-466b-b768-687eccbb154c");
            // regardless of success or failure (buildAfterFailed = false in config.xml)
            if (!buildAfterFailed) {
                writeline("/home/ubuntu/results/coverage/Project/Project_2_10.coverage", "9dac9c0d-0e9d-478a-b1c2-e26a28727b38");
                lastBuild = now;
            }
            writeline("/home/ubuntu/results/coverage/Project/Project_2_10.coverage", "d7fcccb8-34bf-4dc4-9f56-4b8bbbae9081");
            // If this was a successful build, update both last build and last successful build
            if (buildSuccessful) {
                writeline("/home/ubuntu/results/coverage/Project/Project_2_10.coverage", "dad38bfa-7cc0-4106-83c3-2d734577de1c");
                lastBuild = now;
                writeline("/home/ubuntu/results/coverage/Project/Project_2_10.coverage", "46a0addb-1e6b-45e5-bc93-92a114323fa9");
                lastSuccessfulBuild = now;
                writeline("/home/ubuntu/results/coverage/Project/Project_2_10.coverage", "da55458f-458a-4849-be9a-098cbf30c1fe");
                info("build successful");
            } else {
                writeline("/home/ubuntu/results/coverage/Project/Project_2_10.coverage", "37eec247-2811-44d3-b106-ec113d623cba");
                info("build failed");
            }
            writeline("/home/ubuntu/results/coverage/Project/Project_2_10.coverage", "59505e2a-e341-4936-968b-d3af171861f4");
            buildCounter++;
            writeline("/home/ubuntu/results/coverage/Project/Project_2_10.coverage", "9b4ee93f-33bb-424f-ad62-787b7e7d7ee2");
            setWasLastBuildSuccessful(buildSuccessful);
            writeline("/home/ubuntu/results/coverage/Project/Project_2_10.coverage", "eea8fc81-942e-4e3a-8c33-151456f33e88");
            // also need to reset forced flag before serializing, unless buildForced var is transient
            // resetBuildForcedOnlyIfBuildWasForced(buildWasForced);
            serializeProject();
            writeline("/home/ubuntu/results/coverage/Project/Project_2_10.coverage", "763844c9-4473-4a0d-9052-57c582369df5");
            // @todo Add Progress param to Publisher API?
            publish(buildLog);
            writeline("/home/ubuntu/results/coverage/Project/Project_2_10.coverage", "ae87d65e-53ac-490c-8f11-c0c03fb621fc");
            buildLog.reset();
        } finally {
            writeline("/home/ubuntu/results/coverage/Project/Project_2_10.coverage", "d759bbb5-8ef9-45cb-96e8-988e9514743e");
            resetBuildForcedOnlyIfBuildWasForced(buildWasForced);
            writeline("/home/ubuntu/results/coverage/Project/Project_2_10.coverage", "97f84038-4644-4601-8278-66a6228c97ce");
            setState(ProjectState.IDLE);
        }
    }

    private String useAndResetBuildTargetIfBuildWasForced(final boolean buildWasForced) {
        writeline("/home/ubuntu/results/coverage/Project/Project_2_10.coverage", "b3ee386c-36eb-4275-bc54-c13632e18f45");
        String target = null;
        writeline("/home/ubuntu/results/coverage/Project/Project_2_10.coverage", "05217e88-3fcb-4f5a-bdc4-0bec70c1602e");
        if (buildWasForced) {
            writeline("/home/ubuntu/results/coverage/Project/Project_2_10.coverage", "fefbabb5-45d8-4b3d-a1d8-c98696df59c1");
            target = buildTarget;
            writeline("/home/ubuntu/results/coverage/Project/Project_2_10.coverage", "9cb73c4d-cf05-4049-9e9b-679ac3865937");
            buildTarget = null;
        }
        writeline("/home/ubuntu/results/coverage/Project/Project_2_10.coverage", "719675bf-dedb-438a-80d0-cea8132630c5");
        return target;
    }

    private void resetBuildForcedOnlyIfBuildWasForced(final boolean buildWasForced) {
        writeline("/home/ubuntu/results/coverage/Project/Project_2_10.coverage", "1734154a-9065-4ee2-b180-8244d325a0eb");
        if (buildWasForced) {
            writeline("/home/ubuntu/results/coverage/Project/Project_2_10.coverage", "e9889f13-2615-45e0-8db2-6c0e4b3de674");
            buildForced = false;
        }
    }

    void setBuildStartTime(final Date date) {
        writeline("/home/ubuntu/results/coverage/Project/Project_2_10.coverage", "3adbea36-d029-49b5-a867-83b11960f325");
        buildStartTime = date;
    }

    public void run() {
        writeline("/home/ubuntu/results/coverage/Project/Project_2_10.coverage", "ec1581ca-d0e6-4654-ae4e-9bf34839a4cf");
        LOG.info("Project " + name + " started");
        writeline("/home/ubuntu/results/coverage/Project/Project_2_10.coverage", "73504c55-df05-41a7-8477-60adaa707664");
        try {
            writeline("/home/ubuntu/results/coverage/Project/Project_2_10.coverage", "ed9599c2-83c3-4079-b760-34e0df787b4c");
            while (!stopped) {
                writeline("/home/ubuntu/results/coverage/Project/Project_2_10.coverage", "43ee2d1b-9d03-4e85-9a42-b381cd33064e");
                try {
                    writeline("/home/ubuntu/results/coverage/Project/Project_2_10.coverage", "81a62efa-de54-45a7-8923-dc47cbd116c4");
                    waitIfPaused();
                    writeline("/home/ubuntu/results/coverage/Project/Project_2_10.coverage", "1739e497-56c8-4438-b24f-baa52e9b9b62");
                    if (!stopped) {
                        writeline("/home/ubuntu/results/coverage/Project/Project_2_10.coverage", "020e4ad8-72cd-4a11-ae9c-a92c89c383c0");
                        waitForNextBuild();
                    }
                    writeline("/home/ubuntu/results/coverage/Project/Project_2_10.coverage", "4baa3673-85d9-473c-bac4-c76d0fae48d0");
                    if (!stopped) {
                        writeline("/home/ubuntu/results/coverage/Project/Project_2_10.coverage", "874a568b-cdda-435c-982b-cbbd80f6cb1c");
                        setState(ProjectState.QUEUED);
                        writeline("/home/ubuntu/results/coverage/Project/Project_2_10.coverage", "75a575f6-9278-4752-9cff-0520d254f925");
                        synchronized (scheduleMutex) {
                            writeline("/home/ubuntu/results/coverage/Project/Project_2_10.coverage", "302e728a-8aff-47fc-887d-1e58a91431be");
                            queue.requestBuild(projectConfig);
                            writeline("/home/ubuntu/results/coverage/Project/Project_2_10.coverage", "6643a783-afd9-42bb-a2f0-b3ef0fab2e5d");
                            waitForBuildToFinish();
                        }
                    }
                } catch (InterruptedException e) {
                    writeline("/home/ubuntu/results/coverage/Project/Project_2_10.coverage", "617b91b3-dc64-4cf4-8368-e99ad2fe4192");
                    final String message = "Project " + name + ".run() interrupted";
                    writeline("/home/ubuntu/results/coverage/Project/Project_2_10.coverage", "bb0a620d-6542-40ff-99be-0caed9144c44");
                    LOG.error(message, e);
                    writeline("/home/ubuntu/results/coverage/Project/Project_2_10.coverage", "61dd1da8-5335-4f92-af4d-0b1bc67c4beb");
                    throw new RuntimeException(message);
                }
            }
        } finally {
            writeline("/home/ubuntu/results/coverage/Project/Project_2_10.coverage", "585be72b-140c-4e3a-b7f6-fc14ada7708a");
            stopped = true;
            writeline("/home/ubuntu/results/coverage/Project/Project_2_10.coverage", "e2b1aeb7-7852-4c96-9ab9-5d6b6262bd36");
            LOG.info("Project " + name + " stopped");
        }
    }

    void waitIfPaused() throws InterruptedException {
        writeline("/home/ubuntu/results/coverage/Project/Project_2_10.coverage", "764ad019-b947-4f41-81c6-aa611e848a85");
        synchronized (pausedMutex) {
            writeline("/home/ubuntu/results/coverage/Project/Project_2_10.coverage", "f20eb8a8-42f1-4710-8801-00d07e0777bb");
            while (isPaused) {
                writeline("/home/ubuntu/results/coverage/Project/Project_2_10.coverage", "ebcc0f36-2b1f-4d7f-afa1-324639be54ae");
                setState(ProjectState.PAUSED);
                writeline("/home/ubuntu/results/coverage/Project/Project_2_10.coverage", "9eaac26c-ba09-4370-9bd9-055d8884dc92");
                pausedMutex.wait(10 * DateUtil.ONE_MINUTE);
            }
        }
    }

    void waitForNextBuild() throws InterruptedException {
        writeline("/home/ubuntu/results/coverage/Project/Project_2_10.coverage", "59ce9396-4bd2-46a7-8e03-788b11c272f5");
        long waitTime = getTimeToNextBuild(new Date());
        writeline("/home/ubuntu/results/coverage/Project/Project_2_10.coverage", "de63d27c-c5ca-447e-97dc-e0a5db8cd23b");
        if (needToWaitForNextBuild(waitTime) && !buildForced) {
            writeline("/home/ubuntu/results/coverage/Project/Project_2_10.coverage", "5bea1352-ee21-42af-bd53-9c5ea407bf9d");
            final String msg = "next build in " + DateUtil.formatTime(waitTime);
            writeline("/home/ubuntu/results/coverage/Project/Project_2_10.coverage", "7e91ea82-617e-4547-a689-787e168f49a8");
            info(msg);
            writeline("/home/ubuntu/results/coverage/Project/Project_2_10.coverage", "1b2698cf-f56b-4199-8478-13fbb0d067cc");
            synchronized (waitMutex) {
                writeline("/home/ubuntu/results/coverage/Project/Project_2_10.coverage", "685fb0f8-fd24-4cca-8c4e-156d61b9f3b3");
                setState(ProjectState.WAITING);
                writeline("/home/ubuntu/results/coverage/Project/Project_2_10.coverage", "de6c465d-d9a9-4d7d-9af2-8d1f17c5e43a");
                progress.setValue(msg);
                writeline("/home/ubuntu/results/coverage/Project/Project_2_10.coverage", "c48ca409-037a-47c3-a4c8-7ff58dafa964");
                waitMutex.wait(waitTime);
            }
        }
    }

    long getTimeToNextBuild(Date now) {
        writeline("/home/ubuntu/results/coverage/Project/Project_2_10.coverage", "f013b47c-03ea-4b40-b3ef-4d2ab0b0b9b5");
        long waitTime = projectConfig.getSchedule().getTimeToNextBuild(now, getBuildInterval());
        writeline("/home/ubuntu/results/coverage/Project/Project_2_10.coverage", "b62dc125-7404-410d-97f0-8930ad1d9eb8");
        if (waitTime == 0) {
            writeline("/home/ubuntu/results/coverage/Project/Project_2_10.coverage", "dcd8a4ce-f9e4-4708-992b-657e2727acf1");
            // project that has just built within a minute time
            if (buildStartTime != null) {
                writeline("/home/ubuntu/results/coverage/Project/Project_2_10.coverage", "971b75a7-ba4e-4267-9069-63666f6be954");
                long millisSinceLastBuild = now.getTime() - buildStartTime.getTime();
                writeline("/home/ubuntu/results/coverage/Project/Project_2_10.coverage", "7e668cf9-4d39-4902-83a2-ac6c9ff4dd2f");
                if (millisSinceLastBuild < Schedule.ONE_MINUTE) {
                    writeline("/home/ubuntu/results/coverage/Project/Project_2_10.coverage", "092870a8-2f7a-4c96-a9a1-2482fb9be41b");
                    debug("build finished within a minute, getting new time to next build");
                    writeline("/home/ubuntu/results/coverage/Project/Project_2_10.coverage", "896788b5-8619-46ca-8599-e5e3c0b22be5");
                    Date oneMinuteInFuture = new Date(now.getTime() + Schedule.ONE_MINUTE);
                    writeline("/home/ubuntu/results/coverage/Project/Project_2_10.coverage", "c788f73c-ce4f-405d-bf57-c950908e66f1");
                    waitTime = projectConfig.getSchedule().getTimeToNextBuild(oneMinuteInFuture, getBuildInterval());
                    writeline("/home/ubuntu/results/coverage/Project/Project_2_10.coverage", "f7982c19-db50-4584-8531-43b6d548acf6");
                    waitTime += Schedule.ONE_MINUTE;
                }
            }
        }
        writeline("/home/ubuntu/results/coverage/Project/Project_2_10.coverage", "348abb23-5055-499f-9542-0159b502e475");
        return waitTime;
    }

    static boolean needToWaitForNextBuild(long waitTime) {
        writelineStatic("/home/ubuntu/results/coverage/Project/Project_2_10.coverage", "016250a1-166c-4a3c-9a29-ab5119d99e14");
        return waitTime > 0;
    }

    /**
     * @return true if build was forced, intended for unit testing only.
     */
    boolean isBuildForced() {
        writeline("/home/ubuntu/results/coverage/Project/Project_2_10.coverage", "7dd7d90e-c97a-4ca9-9bf7-04f74b1bfc34");
        return buildForced;
    }

    void forceBuild() {
        writeline("/home/ubuntu/results/coverage/Project/Project_2_10.coverage", "fa30ea23-ac0c-4b5a-b101-96113edb686b");
        synchronized (waitMutex) {
            writeline("/home/ubuntu/results/coverage/Project/Project_2_10.coverage", "2ce96281-a3d1-4d57-bd0a-3f0e15dd1e0f");
            waitMutex.notify();
        }
    }

    public void forceBuildWithTarget(String buildTarget) {
        writeline("/home/ubuntu/results/coverage/Project/Project_2_10.coverage", "41bc9558-f0f3-483a-adba-779df81cc9c8");
        this.buildTarget = buildTarget;
        writeline("/home/ubuntu/results/coverage/Project/Project_2_10.coverage", "d6357dd2-701b-4c4c-839b-eb0f9517625b");
        setBuildForced(true);
    }

    public void forceBuildWithTarget(String buildTarget, Map<String, String> addedProperties) {
        writeline("/home/ubuntu/results/coverage/Project/Project_2_10.coverage", "b7e50140-e584-4ae6-b919-e9ea4ae8d84e");
        additionalProperties = addedProperties;
        writeline("/home/ubuntu/results/coverage/Project/Project_2_10.coverage", "8d234cc3-8357-47f1-82f0-0ec757c3e769");
        forceBuildWithTarget(buildTarget);
    }

    void waitForBuildToFinish() throws InterruptedException {
        writeline("/home/ubuntu/results/coverage/Project/Project_2_10.coverage", "9a70f2b1-84dd-4521-804e-f52941f5e0ee");
        synchronized (scheduleMutex) {
            writeline("/home/ubuntu/results/coverage/Project/Project_2_10.coverage", "b612fc62-2128-46e8-bdb3-1f06224c1bee");
            debug("waiting for build to finish");
            writeline("/home/ubuntu/results/coverage/Project/Project_2_10.coverage", "4e5b011e-6c3b-46a8-904d-7cc4a0374ffa");
            scheduleMutex.wait();
        }
    }

    void buildFinished() {
        writeline("/home/ubuntu/results/coverage/Project/Project_2_10.coverage", "bdbcf019-90e7-4420-8d8c-48cbe3c3e13d");
        synchronized (scheduleMutex) {
            writeline("/home/ubuntu/results/coverage/Project/Project_2_10.coverage", "24c68e43-8461-4f10-8e55-fdb207e4389c");
            debug("build finished");
            writeline("/home/ubuntu/results/coverage/Project/Project_2_10.coverage", "0cc8a318-4d6e-4b4f-9863-ffecd68a3161");
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
        writeline("/home/ubuntu/results/coverage/Project/Project_2_10.coverage", "69af2a77-c671-497a-8ea8-9c5376304a61");
        setState(ProjectState.MODIFICATIONSET);
        writeline("/home/ubuntu/results/coverage/Project/Project_2_10.coverage", "19ffa022-f09d-41a5-a0a8-a4140a255613");
        final ModificationSet modificationSet = projectConfig.getModificationSet();
        writeline("/home/ubuntu/results/coverage/Project/Project_2_10.coverage", "fa63fbdf-2f3e-4aba-9235-973ab42ce018");
        if (modificationSet == null) {
            writeline("/home/ubuntu/results/coverage/Project/Project_2_10.coverage", "d98de40c-6271-40d4-92c1-f28bb547dcad");
            debug("no modification set, nothing to detect.");
            writeline("/home/ubuntu/results/coverage/Project/Project_2_10.coverage", "34c46da6-a65d-4c12-9359-acaa9d26d6fd");
            if (buildWasForced) {
                writeline("/home/ubuntu/results/coverage/Project/Project_2_10.coverage", "20c037c8-6cef-4dfa-8df0-e8371faf350f");
                info("no modification set but build was forced");
                writeline("/home/ubuntu/results/coverage/Project/Project_2_10.coverage", "165fca17-c0c3-448e-9241-b6d76ec051d7");
                return new Element("modifications");
            }
            writeline("/home/ubuntu/results/coverage/Project/Project_2_10.coverage", "d184a107-e0f6-4587-9811-634b7adb6f85");
            if (!requiremodification) {
                writeline("/home/ubuntu/results/coverage/Project/Project_2_10.coverage", "245c8eba-ec72-470c-a619-28eba39467e6");
                info("no modification set but no modifications required");
                writeline("/home/ubuntu/results/coverage/Project/Project_2_10.coverage", "81a875e3-11de-4c73-bd60-cc4aee214564");
                return new Element("modifications");
            }
            writeline("/home/ubuntu/results/coverage/Project/Project_2_10.coverage", "9c7561ff-2d0b-419f-93fb-73b117913152");
            return null;
        }
        writeline("/home/ubuntu/results/coverage/Project/Project_2_10.coverage", "2991aaae-7c31-4651-bf1f-57f0c12f927f");
        final boolean checkNewChangesFirst = checkOnlySinceLastBuild();
        writeline("/home/ubuntu/results/coverage/Project/Project_2_10.coverage", "88c353c7-b6c3-4bf3-9b08-033f0f76cdc9");
        Element modifications;
        writeline("/home/ubuntu/results/coverage/Project/Project_2_10.coverage", "17f93815-420b-4c1b-a9db-9aa4db0c5b99");
        if (checkNewChangesFirst) {
            writeline("/home/ubuntu/results/coverage/Project/Project_2_10.coverage", "8170a430-9832-4456-b0f7-09bc24f67822");
            debug("getting changes since last build");
            writeline("/home/ubuntu/results/coverage/Project/Project_2_10.coverage", "2a8b0343-7428-4781-a55d-922b9257bd8a");
            modifications = modificationSet.retrieveModificationsAsElement(lastBuild, progress);
        } else {
            writeline("/home/ubuntu/results/coverage/Project/Project_2_10.coverage", "f4661671-e8a8-40c9-af0c-b48cdae882a3");
            debug("getting changes since last successful build");
            writeline("/home/ubuntu/results/coverage/Project/Project_2_10.coverage", "b23c3c36-6e94-4ce7-b345-c800ebd0d293");
            modifications = modificationSet.retrieveModificationsAsElement(lastSuccessfulBuild, progress);
        }
        writeline("/home/ubuntu/results/coverage/Project/Project_2_10.coverage", "69e5445a-520f-4cc4-8be6-8152d0b51a3b");
        if (!modificationSet.isModified()) {
            writeline("/home/ubuntu/results/coverage/Project/Project_2_10.coverage", "9861a74c-6e0f-4645-9d73-154ced5b95e2");
            info("No modifications found, build not necessary.");
            writeline("/home/ubuntu/results/coverage/Project/Project_2_10.coverage", "18467c66-ec27-4d9f-a7d1-ae0f82a88246");
            // * build forced
            if (buildAfterFailed && !wasLastBuildSuccessful) {
                writeline("/home/ubuntu/results/coverage/Project/Project_2_10.coverage", "7ada7090-49f9-4320-9e18-93c0fe28ad3e");
                info("Building anyway, since buildAfterFailed is true and last build failed.");
            } else if (!requiremodification) {
                writeline("/home/ubuntu/results/coverage/Project/Project_2_10.coverage", "bbc6e46c-cece-4161-b7ff-5ade02bed349");
                info("Building anyway, since modifications not required");
            } else {
                writeline("/home/ubuntu/results/coverage/Project/Project_2_10.coverage", "2d644ec5-fb48-41fd-bcc1-fb8a9ccc8d35");
                if (buildWasForced) {
                    writeline("/home/ubuntu/results/coverage/Project/Project_2_10.coverage", "a21ec590-a3ad-45ec-9add-7a0c47aa9c34");
                    info("Building anyway, since build was explicitly forced.");
                } else {
                    writeline("/home/ubuntu/results/coverage/Project/Project_2_10.coverage", "590968db-5e74-4fb8-ad8f-94ab02a75a20");
                    return null;
                }
            }
        }
        writeline("/home/ubuntu/results/coverage/Project/Project_2_10.coverage", "6cf08d16-88e3-4624-9c04-77dde337403d");
        if (checkNewChangesFirst) {
            writeline("/home/ubuntu/results/coverage/Project/Project_2_10.coverage", "b088ea38-c773-4a29-8572-32f98b7646a2");
            debug("new changes found; now getting complete set");
            writeline("/home/ubuntu/results/coverage/Project/Project_2_10.coverage", "ff1cdfd1-396f-4adb-9f71-e2b85c9415c3");
            modifications = modificationSet.retrieveModificationsAsElement(lastSuccessfulBuild, progress);
        }
        writeline("/home/ubuntu/results/coverage/Project/Project_2_10.coverage", "21a0a850-d565-467f-a9f3-36455f414f01");
        return modifications;
    }

    /**
     * @return boolean
     */
    boolean checkOnlySinceLastBuild() {
        writeline("/home/ubuntu/results/coverage/Project/Project_2_10.coverage", "c8fb9a1a-6411-4e31-b9a6-da0dbb8e89df");
        if (lastBuild == null || lastSuccessfulBuild == null) {
            writeline("/home/ubuntu/results/coverage/Project/Project_2_10.coverage", "13278379-f538-4b65-8e8f-393eaeed6b18");
            return false;
        }
        writeline("/home/ubuntu/results/coverage/Project/Project_2_10.coverage", "b09652af-bd90-495b-b312-bc871a53cd08");
        final long lastBuildLong = lastBuild.getTime();
        writeline("/home/ubuntu/results/coverage/Project/Project_2_10.coverage", "b7146635-0212-45e3-8dd8-11b4d01f6431");
        final long timeDifference = lastBuildLong - lastSuccessfulBuild.getTime();
        writeline("/home/ubuntu/results/coverage/Project/Project_2_10.coverage", "3982d412-7e44-4b3e-847e-16679540da87");
        final boolean moreThanASecond = timeDifference > DateUtil.ONE_SECOND;
        writeline("/home/ubuntu/results/coverage/Project/Project_2_10.coverage", "e1967024-90e2-47b6-ba31-c875f93e711d");
        return !buildAfterFailed && moreThanASecond;
    }

    /**
     * Serialize the project to allow resumption after a process bounce
     */
    public void serializeProject() {
        writeline("/home/ubuntu/results/coverage/Project/Project_2_10.coverage", "e676117e-1856-49a7-b5bb-26422088e9b1");
        final String safeProjectName = Builder.getFileSystemSafeProjectName(name);
        writeline("/home/ubuntu/results/coverage/Project/Project_2_10.coverage", "a57f57a1-0abf-4996-bf46-716ca957cb64");
        try {
            writeline("/home/ubuntu/results/coverage/Project/Project_2_10.coverage", "b4b5fccd-94b8-43c1-ae87-799c1fa09391");
            final ObjectOutputStream s = new ObjectOutputStream(new FileOutputStream(safeProjectName + ".ser"));
            writeline("/home/ubuntu/results/coverage/Project/Project_2_10.coverage", "4a9d694f-c5ed-4223-a4bf-fc76940833b9");
            try {
                writeline("/home/ubuntu/results/coverage/Project/Project_2_10.coverage", "6a3ea9a3-0708-40fe-a9d6-80358938447a");
                s.writeObject(this);
                writeline("/home/ubuntu/results/coverage/Project/Project_2_10.coverage", "caed56f1-6fc2-4c9b-b05a-c43a37720155");
                s.flush();
                writeline("/home/ubuntu/results/coverage/Project/Project_2_10.coverage", "e059a87a-1f45-413d-ae39-a76a63aa306d");
                debug("Serializing project to [" + safeProjectName + ".ser]");
            } finally {
                writeline("/home/ubuntu/results/coverage/Project/Project_2_10.coverage", "b379afcf-e111-46aa-b930-499fcdd882e5");
                s.close();
            }
        } catch (Exception e) {
            writeline("/home/ubuntu/results/coverage/Project/Project_2_10.coverage", "02349af3-3f0b-433e-9609-22fa8169f972");
            LOG.warn("Error serializing project to [" + safeProjectName + ".ser]: " + e.getMessage(), e);
        }
    }

    public void setLabelIncrementer(final LabelIncrementer incrementer) throws CruiseControlException {
        writeline("/home/ubuntu/results/coverage/Project/Project_2_10.coverage", "f0dcfff4-7f14-4d16-b10a-8567d139bc91");
        if (incrementer == null) {
            writeline("/home/ubuntu/results/coverage/Project/Project_2_10.coverage", "dd009f6f-d2e0-4d67-84ec-c1c51c8fb868");
            throw new IllegalArgumentException("label incrementer can't be null");
        }
        writeline("/home/ubuntu/results/coverage/Project/Project_2_10.coverage", "4c6c685c-e815-43e8-807a-24d2827829ef");
        labelIncrementer = incrementer;
        writeline("/home/ubuntu/results/coverage/Project/Project_2_10.coverage", "2fd16ff7-6cd3-46f8-8841-11f0489250bc");
        if (label == null) {
            writeline("/home/ubuntu/results/coverage/Project/Project_2_10.coverage", "1f910dbe-fc6a-4729-b8d6-28dbf8b9137a");
            label = labelIncrementer.getDefaultLabel();
        }
        writeline("/home/ubuntu/results/coverage/Project/Project_2_10.coverage", "53af07e8-c29b-47a7-83bc-b256af874f00");
        validateLabel(label, labelIncrementer);
    }

    public LabelIncrementer getLabelIncrementer() {
        writeline("/home/ubuntu/results/coverage/Project/Project_2_10.coverage", "87691bd1-2412-42f0-800e-4d670e34afad");
        return labelIncrementer;
    }

    public void setName(final String projectName) {
        writeline("/home/ubuntu/results/coverage/Project/Project_2_10.coverage", "0906cd1e-6904-493b-acdd-20080a771824");
        name = projectName;
    }

    public String getName() {
        writeline("/home/ubuntu/results/coverage/Project/Project_2_10.coverage", "ae13c61b-dad2-4c40-b389-d3873c58fb5a");
        return name;
    }

    public void setLabel(final String newLabel) {
        writeline("/home/ubuntu/results/coverage/Project/Project_2_10.coverage", "9fe0126a-145a-42bf-9bd5-a9ab2d27eb4d");
        label = newLabel;
    }

    public String getLabel() {
        writeline("/home/ubuntu/results/coverage/Project/Project_2_10.coverage", "79848dfd-5057-4486-ae38-b55c5c4f4490");
        return label;
    }

    /**
     * @param newLastBuild string containing the build date in the format
     * yyyyMMddHHmmss
     * @throws CruiseControlException if the date cannot be extracted from the
     * input string
     */
    public void setLastBuild(final String newLastBuild) throws CruiseControlException {
        writeline("/home/ubuntu/results/coverage/Project/Project_2_10.coverage", "7fe738dc-1a94-4059-9402-ae75c8922a2d");
        lastBuild = DateUtil.parseFormattedTime(newLastBuild, "lastBuild");
    }

    /**
     * @param newLastSuccessfulBuild string containing the build date in the format
     * yyyyMMddHHmmss
     * @throws CruiseControlException if the date cannot be extracted from the
     * input string
     */
    public void setLastSuccessfulBuild(final String newLastSuccessfulBuild) throws CruiseControlException {
        writeline("/home/ubuntu/results/coverage/Project/Project_2_10.coverage", "0f5c34a6-e513-4d8f-ba95-c890184fa858");
        lastSuccessfulBuild = DateUtil.parseFormattedTime(newLastSuccessfulBuild, "lastSuccessfulBuild");
    }

    public String getLastBuild() {
        writeline("/home/ubuntu/results/coverage/Project/Project_2_10.coverage", "e090fd4d-f52f-4447-91cb-9efb47069d90");
        if (lastBuild == null) {
            writeline("/home/ubuntu/results/coverage/Project/Project_2_10.coverage", "cc75f91f-ca22-43a7-9991-e2bd0ed0f3ef");
            return null;
        }
        writeline("/home/ubuntu/results/coverage/Project/Project_2_10.coverage", "ed19c015-4c9b-476a-b8cf-9e210c8dc5cc");
        return DateUtil.getFormattedTime(lastBuild);
    }

    public boolean getBuildForced() {
        writeline("/home/ubuntu/results/coverage/Project/Project_2_10.coverage", "7c8d4235-adaf-497a-bb53-b28e171a34ff");
        return buildForced;
    }

    public void setBuildForced(boolean forceNewBuildNow) {
        writeline("/home/ubuntu/results/coverage/Project/Project_2_10.coverage", "ff89a52f-3a60-4541-9ec8-b3202b9feff9");
        buildForced = forceNewBuildNow;
        writeline("/home/ubuntu/results/coverage/Project/Project_2_10.coverage", "260ef5d8-0ab0-4f7b-8bc6-c5e2b6a83ef7");
        if (forceNewBuildNow) {
            writeline("/home/ubuntu/results/coverage/Project/Project_2_10.coverage", "49c442ff-57fa-4813-b597-648fc2f42328");
            forceBuild();
        }
    }

    public String getLastSuccessfulBuild() {
        writeline("/home/ubuntu/results/coverage/Project/Project_2_10.coverage", "93bb0171-6294-4972-ba9b-9105b20ac435");
        if (lastSuccessfulBuild == null) {
            writeline("/home/ubuntu/results/coverage/Project/Project_2_10.coverage", "e435e3d4-3982-4cd7-9d86-a8402da8deb2");
            return null;
        }
        writeline("/home/ubuntu/results/coverage/Project/Project_2_10.coverage", "d656c680-010c-4247-9603-acd53c853141");
        return DateUtil.getFormattedTime(lastSuccessfulBuild);
    }

    public String getLogDir() {
        writeline("/home/ubuntu/results/coverage/Project/Project_2_10.coverage", "f2c729cc-47f9-4635-96b4-f3aeebc758ac");
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
        writeline("/home/ubuntu/results/coverage/Project/Project_2_10.coverage", "6841e3e9-e28a-4f5f-bb56-ef38b12c42aa");
        if (overrideBuildInterval == null) {
            writeline("/home/ubuntu/results/coverage/Project/Project_2_10.coverage", "50c2dd21-6464-48df-a5e9-f89aa7d6c2d8");
            return projectConfig.getSchedule().getInterval();
        } else {
            writeline("/home/ubuntu/results/coverage/Project/Project_2_10.coverage", "48d2e368-1920-4345-8e9f-b3cc01b07e41");
            return overrideBuildInterval;
        }
    }

    /**
     * Sets the build interval that this Project should use. This method
     * overrides the value initially specified in the Schedule attribute.
     * @param sleepMillis the number of milliseconds to sleep between build attempts
     */
    public void overrideBuildInterval(final long sleepMillis) {
        writeline("/home/ubuntu/results/coverage/Project/Project_2_10.coverage", "49168b21-0a14-4fbf-8b5e-ef5d90b32fca");
        overrideBuildInterval = sleepMillis;
    }

    public boolean isPaused() {
        writeline("/home/ubuntu/results/coverage/Project/Project_2_10.coverage", "5881deef-a8e9-4c7f-8165-475868b783a4");
        return isPaused;
    }

    public void setPaused(final boolean paused) {
        writeline("/home/ubuntu/results/coverage/Project/Project_2_10.coverage", "aebf4987-9185-4fe4-a2d2-5ec88bdd6593");
        synchronized (pausedMutex) {
            writeline("/home/ubuntu/results/coverage/Project/Project_2_10.coverage", "85c8b5b8-964b-4a90-b4a2-e0375e970037");
            if (isPaused && !paused) {
                writeline("/home/ubuntu/results/coverage/Project/Project_2_10.coverage", "6e591bab-5f2b-4268-8b10-731952a1f90a");
                pausedMutex.notifyAll();
            }
            writeline("/home/ubuntu/results/coverage/Project/Project_2_10.coverage", "fa8edfda-09e7-49bc-8b38-e5a36b491b7b");
            isPaused = paused;
        }
    }

    public void setBuildAfterFailed(final boolean rebuildEvenWithNoNewModifications) {
        writeline("/home/ubuntu/results/coverage/Project/Project_2_10.coverage", "f65683a1-b3e8-4eb1-bde1-6b0cde560098");
        buildAfterFailed = rebuildEvenWithNoNewModifications;
    }

    public String getStatus() {
        writeline("/home/ubuntu/results/coverage/Project/Project_2_10.coverage", "574a0c34-0913-49a2-a8eb-8fa0be7b32c1");
        return getState().getDescription();
    }

    public String getStatusWithQueuePosition() {
        writeline("/home/ubuntu/results/coverage/Project/Project_2_10.coverage", "722b6f83-7d3c-4c3b-95db-6087ffb51f3e");
        if (ProjectState.QUEUED.equals(getState())) {
            writeline("/home/ubuntu/results/coverage/Project/Project_2_10.coverage", "3d5bccd8-e910-4af9-8894-ffb57a866976");
            return getState().getDescription() + " - " + queue.findPosition(projectConfig);
        } else {
            writeline("/home/ubuntu/results/coverage/Project/Project_2_10.coverage", "4e2294e1-aaf8-4e7a-9d8e-0a9b1b64de96");
            return getState().getDescription();
        }
    }

    public ProjectState getState() {
        writeline("/home/ubuntu/results/coverage/Project/Project_2_10.coverage", "6095ae15-947c-4fe9-8368-2ce7be618c5b");
        return state;
    }

    private void setState(final ProjectState newState) {
        writeline("/home/ubuntu/results/coverage/Project/Project_2_10.coverage", "987a3181-f595-457a-a5df-1dae073cfb13");
        state = newState;
        writeline("/home/ubuntu/results/coverage/Project/Project_2_10.coverage", "968472f8-8d19-44e1-a163-881932641352");
        info(getStatus());
        writeline("/home/ubuntu/results/coverage/Project/Project_2_10.coverage", "da6b3520-8487-4433-8668-3c94fb0ea912");
        notifyListeners(new ProjectStateChangedEvent(name, getState()));
        writeline("/home/ubuntu/results/coverage/Project/Project_2_10.coverage", "22710d66-df8b-4b7d-924f-dbbffa024d26");
        fireProgressEvent(new BuildProgressEvent(this, getState()));
    }

    public void setBuildQueue(final BuildQueue buildQueue) {
        writeline("/home/ubuntu/results/coverage/Project/Project_2_10.coverage", "9327e60b-a2d9-49e4-ab8a-b0c2cb9663af");
        queue = buildQueue;
    }

    public String getBuildStartTime() {
        writeline("/home/ubuntu/results/coverage/Project/Project_2_10.coverage", "6bf39070-8447-4c71-a806-35235c5e6efa");
        return DateUtil.getFormattedTime(buildStartTime);
    }

    public Log getLog() {
        writeline("/home/ubuntu/results/coverage/Project/Project_2_10.coverage", "0770a9a2-b365-469d-9637-fdf5b3296805");
        return this.projectConfig.getLog();
    }

    /**
     * Initialize the project. Uses ProjectXMLHelper to parse a project file.
     */
    protected void init() {
        writeline("/home/ubuntu/results/coverage/Project/Project_2_10.coverage", "8ea6d117-498b-4a95-ad82-70811824dadd");
        if (projectConfig == null) {
            writeline("/home/ubuntu/results/coverage/Project/Project_2_10.coverage", "aa98d0bd-99cc-4813-bce0-a1c2cdd52fa5");
            throw new IllegalStateException("projectConfig must be set on project before calling init()");
        }
        writeline("/home/ubuntu/results/coverage/Project/Project_2_10.coverage", "01f2be42-a9b1-47f1-94de-791222109291");
        buildAfterFailed = projectConfig.shouldBuildAfterFailed();
        writeline("/home/ubuntu/results/coverage/Project/Project_2_10.coverage", "63080cf4-b9b3-4d6e-894e-686a45a8038f");
        forceOnly = projectConfig.isForceOnly();
        writeline("/home/ubuntu/results/coverage/Project/Project_2_10.coverage", "140b2304-cd6d-442a-b71e-1889c09b1ea1");
        requiremodification = projectConfig.isRequiremodification();
        writeline("/home/ubuntu/results/coverage/Project/Project_2_10.coverage", "2cc17a02-15f0-4688-8211-db3c13065b1f");
        if (lastBuild == null) {
            writeline("/home/ubuntu/results/coverage/Project/Project_2_10.coverage", "a370b845-ad1e-4cfa-aa4e-d6d8e6cc88b3");
            lastBuild = DateUtil.getMidnight();
        }
        writeline("/home/ubuntu/results/coverage/Project/Project_2_10.coverage", "488c21ba-c928-4b94-8069-86501ef5e406");
        if (lastSuccessfulBuild == null) {
            writeline("/home/ubuntu/results/coverage/Project/Project_2_10.coverage", "d4f52c38-4cb1-4927-8321-84037a38cf1c");
            lastSuccessfulBuild = lastBuild;
        }
        writeline("/home/ubuntu/results/coverage/Project/Project_2_10.coverage", "b9f7aa47-25a3-4483-ac9e-4bf77544f434");
        if (LOG.isDebugEnabled()) {
            writeline("/home/ubuntu/results/coverage/Project/Project_2_10.coverage", "42ab3c25-9c5f-4e47-a0db-a81fb613816f");
            debug("buildInterval          = [" + getBuildInterval() + "]");
            writeline("/home/ubuntu/results/coverage/Project/Project_2_10.coverage", "ab38b49e-8e39-449b-9592-eff3136b6d25");
            debug("buildForced            = [" + buildForced + "]");
            writeline("/home/ubuntu/results/coverage/Project/Project_2_10.coverage", "4a015256-3685-4937-b2c1-13f16978e9e8");
            debug("buildAfterFailed       = [" + buildAfterFailed + "]");
            writeline("/home/ubuntu/results/coverage/Project/Project_2_10.coverage", "d59682c9-ff69-4cf6-97d0-7961c31f0070");
            debug("requireModifcation     = [" + requiremodification + "]");
            writeline("/home/ubuntu/results/coverage/Project/Project_2_10.coverage", "0e373ba0-4b75-40d3-a22e-da9a6182a9cf");
            debug("forceOnly              = [" + forceOnly + "]");
            writeline("/home/ubuntu/results/coverage/Project/Project_2_10.coverage", "868d8000-dcd5-4952-83b9-2b39b856a025");
            debug("buildCounter           = [" + buildCounter + "]");
            writeline("/home/ubuntu/results/coverage/Project/Project_2_10.coverage", "3766d297-b72c-418b-9ca1-2838b7866143");
            debug("isPaused               = [" + isPaused + "]");
            writeline("/home/ubuntu/results/coverage/Project/Project_2_10.coverage", "938ec812-8a75-4abc-92ee-d38bd04af226");
            debug("label                  = [" + label + "]");
            writeline("/home/ubuntu/results/coverage/Project/Project_2_10.coverage", "e7a15901-c51f-4750-bb7d-805f65f825af");
            debug("lastBuild              = [" + DateUtil.getFormattedTime(lastBuild) + "]");
            writeline("/home/ubuntu/results/coverage/Project/Project_2_10.coverage", "d1a0df6f-c986-4f8b-adc4-209e2461e973");
            debug("lastSuccessfulBuild    = [" + DateUtil.getFormattedTime(lastSuccessfulBuild) + "]");
            writeline("/home/ubuntu/results/coverage/Project/Project_2_10.coverage", "69c06cb8-0f8b-4b39-968e-5bf7cb0e6366");
            debug("logDir                 = [" + projectConfig.getLog().getLogDir() + "]");
            writeline("/home/ubuntu/results/coverage/Project/Project_2_10.coverage", "2acd0080-3b09-4d2a-9d07-c1db3f197c2c");
            debug("logXmlEncoding         = [" + projectConfig.getLog().getLogXmlEncoding() + "]");
            writeline("/home/ubuntu/results/coverage/Project/Project_2_10.coverage", "72a49147-d1f4-4ff1-98f4-cd7b4e59d807");
            debug("wasLastBuildSuccessful = [" + wasLastBuildSuccessful + "]");
        }
    }

    protected Element getProjectPropertiesElement(final Date now) {
        writeline("/home/ubuntu/results/coverage/Project/Project_2_10.coverage", "6cf3fece-7ed3-4458-8a67-10c19544c366");
        final Element infoElement = new Element("info");
        writeline("/home/ubuntu/results/coverage/Project/Project_2_10.coverage", "76e7222e-e1b7-4489-baff-2aa175253b4e");
        addProperty(infoElement, "projectname", name);
        writeline("/home/ubuntu/results/coverage/Project/Project_2_10.coverage", "aff526a4-0392-4de1-89ec-185ba11f7bb1");
        final String lastBuildString = DateUtil.getFormattedTime(lastBuild == null ? now : lastBuild);
        writeline("/home/ubuntu/results/coverage/Project/Project_2_10.coverage", "b4118bb5-4438-4ae6-b2d0-b75a0631df2e");
        addProperty(infoElement, "lastbuild", lastBuildString);
        writeline("/home/ubuntu/results/coverage/Project/Project_2_10.coverage", "c148abde-0257-49e1-8e31-c197ae347c08");
        final String lastSuccessfulBuildString = DateUtil.getFormattedTime(lastSuccessfulBuild == null ? now : lastSuccessfulBuild);
        writeline("/home/ubuntu/results/coverage/Project/Project_2_10.coverage", "00612d1c-2972-4c85-91a2-31f2efc9e648");
        addProperty(infoElement, "lastsuccessfulbuild", lastSuccessfulBuildString);
        writeline("/home/ubuntu/results/coverage/Project/Project_2_10.coverage", "47a24daa-561a-4d3c-9a18-8ca1719531bb");
        addProperty(infoElement, "builddate", DateUtil.formatIso8601(now));
        writeline("/home/ubuntu/results/coverage/Project/Project_2_10.coverage", "27533246-3394-4063-8aa1-a491a9c8cf02");
        addProperty(infoElement, "cctimestamp", DateUtil.getFormattedTime(now));
        writeline("/home/ubuntu/results/coverage/Project/Project_2_10.coverage", "37ea692e-ec69-40c2-a1ef-f1d92bd88385");
        addProperty(infoElement, "label", label);
        writeline("/home/ubuntu/results/coverage/Project/Project_2_10.coverage", "10ada908-2175-4628-b890-32efa241069f");
        addProperty(infoElement, "interval", Long.toString(getBuildInterval() / 1000L));
        writeline("/home/ubuntu/results/coverage/Project/Project_2_10.coverage", "4b114c2c-7de7-4ebf-a401-04257526fee8");
        addProperty(infoElement, "lastbuildsuccessful", String.valueOf(wasLastBuildSuccessful));
        writeline("/home/ubuntu/results/coverage/Project/Project_2_10.coverage", "e56603e1-b0b3-4b56-acb2-2695eb67f427");
        return infoElement;
    }

    private void addProperty(final Element parent, final String key, final String value) {
        writeline("/home/ubuntu/results/coverage/Project/Project_2_10.coverage", "d324758c-8022-47ac-89a8-58c168614915");
        final Element propertyElement = new Element("property");
        writeline("/home/ubuntu/results/coverage/Project/Project_2_10.coverage", "e6e5add9-e999-497d-9895-a4eb565a199f");
        propertyElement.setAttribute("name", key);
        writeline("/home/ubuntu/results/coverage/Project/Project_2_10.coverage", "01121a87-6c69-4408-8fa8-b1dd16cc8d49");
        propertyElement.setAttribute("value", value);
        writeline("/home/ubuntu/results/coverage/Project/Project_2_10.coverage", "107c1913-7909-4158-9488-3fb56d2abd7e");
        parent.addContent(propertyElement);
    }

    protected Map<String, String> getProjectPropertiesMap(final Date now) {
        writeline("/home/ubuntu/results/coverage/Project/Project_2_10.coverage", "a66db113-601c-4183-a3c6-50b24cfb71cb");
        final Map<String, String> buildProperties = new HashMap<String, String>();
        writeline("/home/ubuntu/results/coverage/Project/Project_2_10.coverage", "d42ca31c-0e01-4833-a5f0-6213e748cc83");
        buildProperties.put("projectname", name);
        writeline("/home/ubuntu/results/coverage/Project/Project_2_10.coverage", "c8d74e51-eb82-4292-bc3b-2864286be03c");
        buildProperties.put("label", label);
        writeline("/home/ubuntu/results/coverage/Project/Project_2_10.coverage", "caab2a21-7dcc-4c38-b3eb-d7a901ab336f");
        // TODO: Shouldn't have CVS specific properties here
        buildProperties.put("cvstimestamp", CVSDateUtil.formatCVSDate(now));
        writeline("/home/ubuntu/results/coverage/Project/Project_2_10.coverage", "880f97c7-7dbd-40cd-839c-2256dfd5efad");
        buildProperties.put("cctimestamp", DateUtil.getFormattedTime(now));
        writeline("/home/ubuntu/results/coverage/Project/Project_2_10.coverage", "e8759bdb-de8f-4a5b-8454-8b0d0a57d337");
        buildProperties.put("cclastgoodbuildtimestamp", getLastSuccessfulBuild());
        writeline("/home/ubuntu/results/coverage/Project/Project_2_10.coverage", "f21bc9de-cb0f-429a-8801-c00945e7c868");
        buildProperties.put("cclastbuildtimestamp", getLastBuild());
        writeline("/home/ubuntu/results/coverage/Project/Project_2_10.coverage", "8a58105d-214a-4813-afa3-c76a19482ec3");
        buildProperties.put("lastbuildsuccessful", String.valueOf(isLastBuildSuccessful()));
        writeline("/home/ubuntu/results/coverage/Project/Project_2_10.coverage", "38b4a7f0-b0ca-4e7f-a950-7fb222616c06");
        buildProperties.put("buildforced", String.valueOf(getBuildForced()));
        writeline("/home/ubuntu/results/coverage/Project/Project_2_10.coverage", "94b61eed-2a85-46b4-b28b-5c27b9f6170c");
        if (projectConfig.getModificationSet() != null) {
            writeline("/home/ubuntu/results/coverage/Project/Project_2_10.coverage", "936f6fd0-5b8b-4c02-a6b4-49c1b3a6707a");
            buildProperties.putAll(projectConfig.getModificationSet().getProperties());
        }
        writeline("/home/ubuntu/results/coverage/Project/Project_2_10.coverage", "18d22ccb-4dc1-4fe3-b02d-8632bbd3f96d");
        if (additionalProperties != null && !additionalProperties.isEmpty()) {
            writeline("/home/ubuntu/results/coverage/Project/Project_2_10.coverage", "9fe5443f-fe37-4082-8002-4e412acd21a3");
            buildProperties.putAll(additionalProperties);
            writeline("/home/ubuntu/results/coverage/Project/Project_2_10.coverage", "7a9b961e-b8ed-4edb-b454-c816ed673de5");
            additionalProperties.clear();
            writeline("/home/ubuntu/results/coverage/Project/Project_2_10.coverage", "cbfef410-d15b-426f-8bd7-49ec9a282bdb");
            additionalProperties = null;
        }
        writeline("/home/ubuntu/results/coverage/Project/Project_2_10.coverage", "735f8a76-175d-43bb-88da-7fd6e7b09e09");
        return buildProperties;
    }

    /**
     * Intended only for unit testing.
     * @return additional Properties variable.
     */
    Map<String, String> getAdditionalProperties() {
        writeline("/home/ubuntu/results/coverage/Project/Project_2_10.coverage", "92d2b86a-41c3-4547-8526-c9e8dd394bfc");
        return additionalProperties;
    }

    /**
     * Iterate over all of the registered <code>Publisher</code>s and call
     * their respective <code>publish</code> methods.
     * @param buildLog the content to publish
     * @throws CruiseControlException if an error occurs during publishing
     */
    protected void publish(final Log buildLog) throws CruiseControlException {
        writeline("/home/ubuntu/results/coverage/Project/Project_2_10.coverage", "0e7b079c-3453-497d-8659-4e3377c976e1");
        setState(ProjectState.PUBLISHING);
        writeline("/home/ubuntu/results/coverage/Project/Project_2_10.coverage", "e28097db-0d57-4749-9de8-6c3945103ae3");
        for (final Publisher publisher : projectConfig.getPublishers()) {
            writeline("/home/ubuntu/results/coverage/Project/Project_2_10.coverage", "dcc7adc7-60ec-42dc-b4db-11b0bac27b2c");
            // catch all errors, Publishers shouldn't cause failures in the build method
            try {
                writeline("/home/ubuntu/results/coverage/Project/Project_2_10.coverage", "4fb261c0-9be3-42e1-b01a-d6ad0107e9c7");
                publisher.publish(buildLog.getContent());
            } catch (Throwable t) {
                writeline("/home/ubuntu/results/coverage/Project/Project_2_10.coverage", "c132fc9a-5472-486b-a71b-12e24662875f");
                final StringBuilder message = new StringBuilder("exception publishing results");
                writeline("/home/ubuntu/results/coverage/Project/Project_2_10.coverage", "790bb363-8f5f-441c-baa7-2ef71a4a34b3");
                message.append(" with ").append(publisher.getClass().getName());
                writeline("/home/ubuntu/results/coverage/Project/Project_2_10.coverage", "ac928975-3b6d-4cb4-accb-e8d9e6896436");
                message.append(" for project ").append(name);
                writeline("/home/ubuntu/results/coverage/Project/Project_2_10.coverage", "d96768de-3045-4c1c-96d0-1da7d19116c4");
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
        writeline("/home/ubuntu/results/coverage/Project/Project_2_10.coverage", "d52b28ee-a4df-4d83-bdc8-c28bd7ab84dd");
        setState(ProjectState.BOOTSTRAPPING);
        writeline("/home/ubuntu/results/coverage/Project/Project_2_10.coverage", "9e5a70a0-e03b-4ea1-bc30-3987697ff538");
        for (final Bootstrapper bootstrapper : projectConfig.getBootstrappers()) {
            writeline("/home/ubuntu/results/coverage/Project/Project_2_10.coverage", "d9cb3210-54b1-49b5-bd1c-f2644b7b48dc");
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
        writeline("/home/ubuntu/results/coverage/Project/Project_2_10.coverage", "252597f8-af0b-4268-8e6d-7176f76e7594");
        if (!incrementer.isValidLabel(oldLabel)) {
            writeline("/home/ubuntu/results/coverage/Project/Project_2_10.coverage", "00024969-7123-433f-9c0a-516a334b1b93");
            final String message = oldLabel + " is not a valid label for labelIncrementer " + incrementer.getClass().getName();
            writeline("/home/ubuntu/results/coverage/Project/Project_2_10.coverage", "60b99894-6c55-4125-a19e-ae991ea29002");
            debug(message);
            writeline("/home/ubuntu/results/coverage/Project/Project_2_10.coverage", "8212b83c-8066-4a33-9811-bb76e53aaae8");
            throw new CruiseControlException(message);
        }
    }

    public boolean isLastBuildSuccessful() {
        writeline("/home/ubuntu/results/coverage/Project/Project_2_10.coverage", "2f7d4816-fe24-4d57-a59a-36a3ea464864");
        return wasLastBuildSuccessful;
    }

    void setWasLastBuildSuccessful(final boolean buildSuccessful) {
        writeline("/home/ubuntu/results/coverage/Project/Project_2_10.coverage", "f6d45d66-46de-4926-b5a6-60ee29f2c803");
        wasLastBuildSuccessful = buildSuccessful;
    }

    /**
     * Logs a message to the application log, not to be confused with the
     * CruiseControl build log.
     * @param message the application message to log
     */
    private void info(final String message) {
        writeline("/home/ubuntu/results/coverage/Project/Project_2_10.coverage", "418c54fe-f6d9-4305-b484-c5c3daa06bfc");
        LOG.info("Project " + name + ":  " + message);
    }

    private void debug(final String message) {
        writeline("/home/ubuntu/results/coverage/Project/Project_2_10.coverage", "41f45cb5-51f5-465d-83c7-cd6a5c604d75");
        LOG.debug("Project " + name + ":  " + message);
    }

    public void start() {
        writeline("/home/ubuntu/results/coverage/Project/Project_2_10.coverage", "2661d17d-ef1d-4f78-8088-eab960d96daa");
        if (stopped || getState() == ProjectState.STOPPED) {
            writeline("/home/ubuntu/results/coverage/Project/Project_2_10.coverage", "8135d08e-6550-4be0-96ce-5a2efbbe431c");
            stopped = false;
            writeline("/home/ubuntu/results/coverage/Project/Project_2_10.coverage", "9ed5b375-3b88-4fa0-bab4-418d04db1d8b");
            LOG.info("Project " + name + " starting");
            writeline("/home/ubuntu/results/coverage/Project/Project_2_10.coverage", "81701655-5e7e-4214-afb2-376465c5ad66");
            setState(ProjectState.IDLE);
            writeline("/home/ubuntu/results/coverage/Project/Project_2_10.coverage", "9de8a0a4-2d08-4dc2-9d03-fc921ebf0201");
            createNewSchedulingThread();
        }
    }

    protected void createNewSchedulingThread() {
        writeline("/home/ubuntu/results/coverage/Project/Project_2_10.coverage", "1b4eb73e-8f4f-47c7-97b1-7003f6d60cf7");
        final Thread projectSchedulingThread = new Thread(this, "Project " + getName() + " thread");
        writeline("/home/ubuntu/results/coverage/Project/Project_2_10.coverage", "95937b8c-a00b-4cf0-802d-c0946774d066");
        projectSchedulingThread.start();
        writeline("/home/ubuntu/results/coverage/Project/Project_2_10.coverage", "b7632716-632d-4c23-9ef3-3635519883a9");
        // brief nap to allow thread to start
        try {
            writeline("/home/ubuntu/results/coverage/Project/Project_2_10.coverage", "392fd18c-4210-43f1-abb0-c2c333a31282");
            Thread.sleep(100);
        } catch (InterruptedException ie) {
            writeline("/home/ubuntu/results/coverage/Project/Project_2_10.coverage", "206dd46a-3459-47c6-963a-cc7d27ebe4c9");
            LOG.warn("interrupted while waiting for scheduling thread to start", ie);
        }
    }

    public void stop() {
        writeline("/home/ubuntu/results/coverage/Project/Project_2_10.coverage", "26fda7a7-4d44-4eeb-a1e8-208f41142358");
        LOG.info("Project " + name + " stopping");
        writeline("/home/ubuntu/results/coverage/Project/Project_2_10.coverage", "669098a4-484c-48dd-8a4d-3947278aada6");
        stopped = true;
        writeline("/home/ubuntu/results/coverage/Project/Project_2_10.coverage", "cafff117-dad4-4281-945b-3522ad40cb82");
        setState(ProjectState.STOPPED);
        writeline("/home/ubuntu/results/coverage/Project/Project_2_10.coverage", "62960a07-f9e2-49c2-8945-513cc3a62728");
        synchronized (pausedMutex) {
            writeline("/home/ubuntu/results/coverage/Project/Project_2_10.coverage", "6a4c7279-5637-42df-831c-e7aca5bc08e8");
            pausedMutex.notifyAll();
        }
        writeline("/home/ubuntu/results/coverage/Project/Project_2_10.coverage", "a2a2e1d1-21c5-41f1-a0bc-ced0a996303b");
        synchronized (waitMutex) {
            writeline("/home/ubuntu/results/coverage/Project/Project_2_10.coverage", "d9bf328a-2c79-4fe8-8369-5b20dac9c275");
            waitMutex.notifyAll();
        }
        writeline("/home/ubuntu/results/coverage/Project/Project_2_10.coverage", "1e62a017-69fc-4c54-a25f-4bd7090b8d81");
        synchronized (scheduleMutex) {
            writeline("/home/ubuntu/results/coverage/Project/Project_2_10.coverage", "04dd45be-c8e0-4079-be98-f93e6877a494");
            scheduleMutex.notifyAll();
        }
    }

    public String toString() {
        writeline("/home/ubuntu/results/coverage/Project/Project_2_10.coverage", "264d61c9-3a3f-40af-a3b9-68d5a4d0fa3b");
        final StringBuilder sb = new StringBuilder("Project ");
        writeline("/home/ubuntu/results/coverage/Project/Project_2_10.coverage", "2b9dacbe-e6b9-4890-9b6c-c2f7d05be5e4");
        sb.append(getName());
        writeline("/home/ubuntu/results/coverage/Project/Project_2_10.coverage", "776bdcec-b5ee-464b-991e-43cc33b73428");
        sb.append(": ");
        writeline("/home/ubuntu/results/coverage/Project/Project_2_10.coverage", "2d325ef4-df67-41ab-bdaf-6ca44dcfd217");
        sb.append(getStatus());
        writeline("/home/ubuntu/results/coverage/Project/Project_2_10.coverage", "3558c90a-d713-4eb2-829a-08f653481924");
        if (isPaused) {
            writeline("/home/ubuntu/results/coverage/Project/Project_2_10.coverage", "b7946c2e-2c06-44b3-9aec-432a8461dd87");
            sb.append(" (paused)");
        }
        writeline("/home/ubuntu/results/coverage/Project/Project_2_10.coverage", "1eb27bc6-2af8-4de7-b955-ffdb50812f8d");
        return sb.toString();
    }

    public void addBuildProgressListener(final BuildProgressListener listener) {
        writeline("/home/ubuntu/results/coverage/Project/Project_2_10.coverage", "57c0e88e-f214-4a99-8105-ab3f12cede69");
        synchronized (progressListeners) {
            writeline("/home/ubuntu/results/coverage/Project/Project_2_10.coverage", "15eb9cce-6777-4a4c-9d7c-f2e5be896005");
            progressListeners.add(listener);
        }
    }

    protected void fireProgressEvent(final BuildProgressEvent event) {
        writeline("/home/ubuntu/results/coverage/Project/Project_2_10.coverage", "34a2a8d8-0741-452c-95b4-e536a9cee30d");
        synchronized (progressListeners) {
            writeline("/home/ubuntu/results/coverage/Project/Project_2_10.coverage", "5edfee77-eb5a-4139-be45-12f09b3d5748");
            for (final BuildProgressListener listener : progressListeners) {
                writeline("/home/ubuntu/results/coverage/Project/Project_2_10.coverage", "a6194105-1697-40d2-a4f0-5e892d98f736");
                listener.handleBuildProgress(event);
            }
        }
    }

    public void addBuildResultListener(final BuildResultListener listener) {
        writeline("/home/ubuntu/results/coverage/Project/Project_2_10.coverage", "9001bc51-67d2-40e0-932f-8c40384d7448");
        synchronized (resultListeners) {
            writeline("/home/ubuntu/results/coverage/Project/Project_2_10.coverage", "dd1109b0-295d-49c5-831a-a93c9818c022");
            resultListeners.add(listener);
        }
    }

    protected void fireResultEvent(final BuildResultEvent event) {
        writeline("/home/ubuntu/results/coverage/Project/Project_2_10.coverage", "480ace29-8761-4d3a-b553-8590a8edd531");
        synchronized (resultListeners) {
            writeline("/home/ubuntu/results/coverage/Project/Project_2_10.coverage", "798ba424-390f-4fba-86f7-6a0ccd199c80");
            for (final BuildResultListener listener : resultListeners) {
                writeline("/home/ubuntu/results/coverage/Project/Project_2_10.coverage", "c75c3b41-afc2-4dcb-afc3-a843e5a8d1dd");
                listener.handleBuildResult(event);
            }
        }
    }

    List<Listener> getListeners() {
        writeline("/home/ubuntu/results/coverage/Project/Project_2_10.coverage", "83d75648-ee77-46b6-b8ac-463293dddcae");
        return projectConfig.getListeners();
    }

    public void setProjectConfig(final ProjectConfig projectConfig) throws CruiseControlException {
        writeline("/home/ubuntu/results/coverage/Project/Project_2_10.coverage", "ffcfaafc-df26-43a2-a2db-d4964d889f37");
        if (projectConfig == null) {
            writeline("/home/ubuntu/results/coverage/Project/Project_2_10.coverage", "3fc84546-32b1-45bd-888f-229d340dba48");
            throw new IllegalArgumentException("project config can't be null");
        }
        writeline("/home/ubuntu/results/coverage/Project/Project_2_10.coverage", "5bb39403-9972-4c35-a9b9-b1151296094f");
        this.projectConfig = projectConfig;
        writeline("/home/ubuntu/results/coverage/Project/Project_2_10.coverage", "0fea9d25-1b1c-4bc1-a774-475eb63546dc");
        setLabelIncrementer(projectConfig.getLabelIncrementer());
    }

    void notifyListeners(final ProjectEvent event) {
        writeline("/home/ubuntu/results/coverage/Project/Project_2_10.coverage", "549730ca-5945-47b1-87d7-2cce0add8502");
        if (projectConfig == null) {
            writeline("/home/ubuntu/results/coverage/Project/Project_2_10.coverage", "2368653c-eb3d-4cdd-aa42-0ea247a4c866");
            throw new IllegalStateException("projectConfig is null");
        }
        writeline("/home/ubuntu/results/coverage/Project/Project_2_10.coverage", "5aa2cafa-1095-44b7-900f-ebb2669ac8eb");
        for (final Listener listener : projectConfig.getListeners()) {
            writeline("/home/ubuntu/results/coverage/Project/Project_2_10.coverage", "fe66bf84-29e3-4526-bc69-e114800e2272");
            try {
                writeline("/home/ubuntu/results/coverage/Project/Project_2_10.coverage", "a62f0fc5-8583-4855-ae33-e88f0337b8d9");
                listener.handleEvent(event);
            } catch (CruiseControlException e) {
                writeline("/home/ubuntu/results/coverage/Project/Project_2_10.coverage", "7d312f43-b7d2-4d82-9ac8-21cd2a875ef3");
                final StringBuilder message = new StringBuilder("exception notifying listener ");
                writeline("/home/ubuntu/results/coverage/Project/Project_2_10.coverage", "b7356fb3-cad3-41ed-a105-40b3554a7a54");
                message.append(listener.getClass().getName());
                writeline("/home/ubuntu/results/coverage/Project/Project_2_10.coverage", "41eab87f-1d81-44f8-bdfc-46cf4130f599");
                message.append(" for project ").append(name);
                writeline("/home/ubuntu/results/coverage/Project/Project_2_10.coverage", "4b6d9908-38e2-4437-9aab-e2189f44d228");
                LOG.error(message.toString(), e);
            }
        }
    }

    public boolean equals(final Object arg0) {
        writeline("/home/ubuntu/results/coverage/Project/Project_2_10.coverage", "b7c30c9d-5413-4008-953c-60dbacdde8a2");
        if (arg0 == null) {
            writeline("/home/ubuntu/results/coverage/Project/Project_2_10.coverage", "bcfbdbdd-e49e-4d28-91a8-a6bb8a0cb0e6");
            return false;
        }
        writeline("/home/ubuntu/results/coverage/Project/Project_2_10.coverage", "7f1aee8e-70ca-4d88-b140-2aa46ef0291c");
        if (arg0.getClass().getName().equals(getClass().getName())) {
            writeline("/home/ubuntu/results/coverage/Project/Project_2_10.coverage", "33191995-3a1e-4525-8e83-33594229678c");
            final Project thatProject = (Project) arg0;
            writeline("/home/ubuntu/results/coverage/Project/Project_2_10.coverage", "772c58b3-61ef-4fad-8974-5d1acf075cda");
            return thatProject.name.equals(name);
        }
        writeline("/home/ubuntu/results/coverage/Project/Project_2_10.coverage", "5e4e6b43-c6ab-480f-b028-2d361a4d4f58");
        return false;
    }

    public int hashCode() {
        writeline("/home/ubuntu/results/coverage/Project/Project_2_10.coverage", "8f9726b5-696e-4550-8d92-deae4b055df8");
        return name.hashCode();
    }

    public void register(final MBeanServer server) throws JMException {
        writeline("/home/ubuntu/results/coverage/Project/Project_2_10.coverage", "6e9f7077-c899-418a-afd4-d33b14e484cb");
        LOG.debug("Registering project mbean");
        writeline("/home/ubuntu/results/coverage/Project/Project_2_10.coverage", "53740b8d-9a53-42d1-9a41-8dd7567c76f5");
        final ProjectController projectController = new ProjectController(this);
        writeline("/home/ubuntu/results/coverage/Project/Project_2_10.coverage", "e8b9fafa-d179-415a-8587-3a534444471a");
        projectController.register(server);
    }

    public ProjectConfig getProjectConfig() {
        writeline("/home/ubuntu/results/coverage/Project/Project_2_10.coverage", "612dae76-5155-4275-92ac-8813266bee70");
        return projectConfig;
    }

    public Date getLastBuildDate() {
        writeline("/home/ubuntu/results/coverage/Project/Project_2_10.coverage", "9a6dae09-8e20-4ecc-b5aa-88cf5cfd5c52");
        return lastBuild;
    }

    public Progress getProgress() {
        writeline("/home/ubuntu/results/coverage/Project/Project_2_10.coverage", "870047d3-ad52-4d4b-86c5-29bbe6abcbec");
        return progress;
    }

    public List<String> getLogLabels() {
        writeline("/home/ubuntu/results/coverage/Project/Project_2_10.coverage", "51890132-6f10-427c-9521-d8e305b31922");
        return projectConfig.getLogLabels();
    }

    public String[] getLogLabelLines(final String logLabel, final int firstLine) {
        writeline("/home/ubuntu/results/coverage/Project/Project_2_10.coverage", "0134bcbb-acec-43ec-9bf0-2e62b74738d2");
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
