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
        writeline("/home/ubuntu/results/coverage/Project/Project_9_10.coverage", "e6011605-a76f-4f63-834d-3caca92149ee");
        state = ProjectState.STOPPED;
        writeline("/home/ubuntu/results/coverage/Project/Project_9_10.coverage", "067e24dc-0ab4-44c0-a56b-77e3d4835995");
        pausedMutex = new Object();
        writeline("/home/ubuntu/results/coverage/Project/Project_9_10.coverage", "44f0bdd0-7cbf-4b43-a020-4a5243ba3f03");
        scheduleMutex = new Object();
        writeline("/home/ubuntu/results/coverage/Project/Project_9_10.coverage", "c867525a-26b6-47ae-9ae4-eb9debf55b99");
        waitMutex = new Object();
        writeline("/home/ubuntu/results/coverage/Project/Project_9_10.coverage", "a912598a-76e5-4075-814f-77cc3ec13078");
        progressListeners = new ArrayList<BuildProgressListener>();
        writeline("/home/ubuntu/results/coverage/Project/Project_9_10.coverage", "b7f960d3-4095-4202-9143-672ff62604a1");
        resultListeners = new ArrayList<BuildResultListener>();
        writeline("/home/ubuntu/results/coverage/Project/Project_9_10.coverage", "55d8a9b9-e84d-4938-a9c1-a044319e4357");
        progress = new ProgressImpl(this);
    }

    private void readObject(final ObjectInputStream stream) throws IOException, ClassNotFoundException {
        writeline("/home/ubuntu/results/coverage/Project/Project_9_10.coverage", "afa8a529-cecd-4d72-a5b7-2c10b202b871");
        stream.defaultReadObject();
        writeline("/home/ubuntu/results/coverage/Project/Project_9_10.coverage", "84d21ce9-90be-4d94-8187-37ad9c522215");
        initializeTransientFields();
    }

    public void execute() {
        writeline("/home/ubuntu/results/coverage/Project/Project_9_10.coverage", "4bad253b-92a0-4759-804a-fca78deab576");
        if (stopped) {
            writeline("/home/ubuntu/results/coverage/Project/Project_9_10.coverage", "4ce59e67-72e1-418c-ba52-96e61039044d");
            LOG.warn("not building project " + name + " because project has been stopped.");
            writeline("/home/ubuntu/results/coverage/Project/Project_9_10.coverage", "6c4c4d36-54ff-4557-adcd-571bf2a66f9a");
            buildFinished();
            writeline("/home/ubuntu/results/coverage/Project/Project_9_10.coverage", "6fb7f3d5-e1ac-4c71-a1ed-fd559ebb4c3e");
            return;
        }
        writeline("/home/ubuntu/results/coverage/Project/Project_9_10.coverage", "4c08393c-7a08-484b-9644-f66cab0cf980");
        synchronized (pausedMutex) {
            writeline("/home/ubuntu/results/coverage/Project/Project_9_10.coverage", "cb74236e-239e-4c95-94d7-5957f86a9011");
            if (isPaused) {
                writeline("/home/ubuntu/results/coverage/Project/Project_9_10.coverage", "7e527967-1e4c-4ad0-947f-d484b1dcd672");
                LOG.info("not building project " + name + " because project has been paused.");
                writeline("/home/ubuntu/results/coverage/Project/Project_9_10.coverage", "b4ee4faf-202c-4302-bc43-c72a74653975");
                buildFinished();
                writeline("/home/ubuntu/results/coverage/Project/Project_9_10.coverage", "99a5447a-70b6-4d80-9b1c-f89741cc6689");
                return;
            }
        }
        writeline("/home/ubuntu/results/coverage/Project/Project_9_10.coverage", "a99c7986-9347-4f16-966f-0678f7881bc9");
        try {
            writeline("/home/ubuntu/results/coverage/Project/Project_9_10.coverage", "395ea300-ac84-4a12-a734-bcf0c32996ed");
            init();
            writeline("/home/ubuntu/results/coverage/Project/Project_9_10.coverage", "b256685d-5342-4218-88c2-1dcc6a4e8eba");
            build();
        } catch (CruiseControlException e) {
            writeline("/home/ubuntu/results/coverage/Project/Project_9_10.coverage", "ad246fe9-ce97-41ff-9724-af58e4431513");
            LOG.error("exception attempting build in project " + name, e);
        } finally {
            writeline("/home/ubuntu/results/coverage/Project/Project_9_10.coverage", "063f3ccf-2254-481b-9973-4e1b7529eac3");
            buildFinished();
        }
    }

    /**
     * Unless paused, runs any bootstrappers and then the entire build.
     * @throws CruiseControlException if an error occurs during the build
     */
    protected void build() throws CruiseControlException {
        writeline("/home/ubuntu/results/coverage/Project/Project_9_10.coverage", "48a6197f-505b-452d-bef1-56556fc0f887");
        if (projectConfig == null) {
            writeline("/home/ubuntu/results/coverage/Project/Project_9_10.coverage", "ccd27ee0-b987-49bd-b177-2ea9dce13924");
            throw new IllegalStateException("projectConfig must be set on project before calling build()");
        }
        writeline("/home/ubuntu/results/coverage/Project/Project_9_10.coverage", "0be52643-1272-40e3-b9d8-41c78d5b0fe4");
        if (stopped) {
            writeline("/home/ubuntu/results/coverage/Project/Project_9_10.coverage", "ae34847d-0097-4b01-8e7b-f433268ba5ea");
            LOG.warn("not building project " + name + " because project has been stopped.");
            writeline("/home/ubuntu/results/coverage/Project/Project_9_10.coverage", "e468c7c1-4934-4701-9872-0d65cec478c7");
            return;
        }
        writeline("/home/ubuntu/results/coverage/Project/Project_9_10.coverage", "838ca2ae-b9b1-4f29-a1be-b3486248d229");
        // or if the last build faild and we want to build on failures
        if (forceOnly && !buildForced && !(!wasLastBuildSuccessful && buildAfterFailed)) {
            writeline("/home/ubuntu/results/coverage/Project/Project_9_10.coverage", "641f8ca8-623f-4dc2-bee8-ff52411649ef");
            info("not building because project is forceOnly and build not forced.");
            writeline("/home/ubuntu/results/coverage/Project/Project_9_10.coverage", "392d1a49-9f3e-4820-9eac-e227f9112b72");
            return;
        }
        writeline("/home/ubuntu/results/coverage/Project/Project_9_10.coverage", "e93d6a26-7c0e-4c4f-90e6-c6893d325fbf");
        final boolean buildWasForced = buildForced;
        writeline("/home/ubuntu/results/coverage/Project/Project_9_10.coverage", "fd48346d-5719-4801-9be7-fcb26525da0c");
        try {
            writeline("/home/ubuntu/results/coverage/Project/Project_9_10.coverage", "21dbe6d4-0834-4d07-8f8d-a84adb831ef3");
            setBuildStartTime(new Date());
            writeline("/home/ubuntu/results/coverage/Project/Project_9_10.coverage", "5a66ed95-89ce-4305-934b-5e360d5b829d");
            final Schedule schedule = projectConfig.getSchedule();
            writeline("/home/ubuntu/results/coverage/Project/Project_9_10.coverage", "761ff773-dd86-4ac0-af10-19fc6bd522e7");
            if (schedule == null) {
                writeline("/home/ubuntu/results/coverage/Project/Project_9_10.coverage", "3abdc438-20b8-46f3-92eb-ae3e7eaeb5fe");
                throw new IllegalStateException("project must have a schedule");
            }
            writeline("/home/ubuntu/results/coverage/Project/Project_9_10.coverage", "c0f6b190-f744-4d60-b124-d35245529a00");
            if (schedule.isPaused(buildStartTime)) {
                writeline("/home/ubuntu/results/coverage/Project/Project_9_10.coverage", "fd8d33a4-d9e3-42f6-9264-d3182687fe62");
                // is different than ProjectState.PAUSED
                return;
            }
            writeline("/home/ubuntu/results/coverage/Project/Project_9_10.coverage", "a02948d6-9503-4eb6-b69a-a027d504bb34");
            // @todo Add Progress param to Bootstrapper API?
            bootstrap();
            writeline("/home/ubuntu/results/coverage/Project/Project_9_10.coverage", "4c3b8c0b-f1d2-4bbe-b41c-229a3ad74bc2");
            final String target = useAndResetBuildTargetIfBuildWasForced(buildWasForced);
            writeline("/home/ubuntu/results/coverage/Project/Project_9_10.coverage", "915d1e75-2cea-4f96-902d-2cac4d91ce36");
            // @todo Add Progress param to ModificationSet API?
            // getModifications will only return null if we don't need to build
            final Element modifications = getModifications(buildWasForced);
            writeline("/home/ubuntu/results/coverage/Project/Project_9_10.coverage", "f41ee9be-2213-4c6f-89c7-8e390c9dbade");
            if (modifications == null) {
                writeline("/home/ubuntu/results/coverage/Project/Project_9_10.coverage", "6f9b2cf8-7c8b-426d-bc55-2efd88897e87");
                return;
            }
            writeline("/home/ubuntu/results/coverage/Project/Project_9_10.coverage", "eea92cea-4361-4ee0-9f19-c5c689427aeb");
            // Using local reference to avoid NPE if config.xml is updated during build
            final Log buildLog = projectConfig.getLog();
            writeline("/home/ubuntu/results/coverage/Project/Project_9_10.coverage", "f5aedda9-f5da-4d58-a1ed-e8ef26b44f78");
            buildLog.addContent(modifications);
            writeline("/home/ubuntu/results/coverage/Project/Project_9_10.coverage", "c0761c46-067b-4dbe-86fe-ff6269d20581");
            final Date now;
            writeline("/home/ubuntu/results/coverage/Project/Project_9_10.coverage", "ee091858-b2e0-4974-8466-c928957bd466");
            if (projectConfig.getModificationSet() != null && projectConfig.getModificationSet().getTimeOfCheck() != null) {
                writeline("/home/ubuntu/results/coverage/Project/Project_9_10.coverage", "eb2e0023-3889-4f2a-be15-1e8fdaa7edde");
                now = projectConfig.getModificationSet().getTimeOfCheck();
            } else {
                writeline("/home/ubuntu/results/coverage/Project/Project_9_10.coverage", "c3b6d4c0-e598-4035-b282-ce06a873fd56");
                now = new Date();
            }
            writeline("/home/ubuntu/results/coverage/Project/Project_9_10.coverage", "52d67873-09e8-4907-891f-b484045de4b6");
            if (getLabelIncrementer().isPreBuildIncrementer()) {
                writeline("/home/ubuntu/results/coverage/Project/Project_9_10.coverage", "d45c29dd-f875-4bce-b113-bb881a27a483");
                label = getLabelIncrementer().incrementLabel(label, buildLog.getContent());
            }
            writeline("/home/ubuntu/results/coverage/Project/Project_9_10.coverage", "87a86741-6678-4161-a329-10e53e906b90");
            // collect project information
            buildLog.addContent(getProjectPropertiesElement(now));
            writeline("/home/ubuntu/results/coverage/Project/Project_9_10.coverage", "ab043cb1-82f0-42b7-a60d-c8f6ccad7c18");
            setState(ProjectState.BUILDING);
            writeline("/home/ubuntu/results/coverage/Project/Project_9_10.coverage", "2879252f-ed1a-447b-aa96-ee9597e49464");
            final Element builderLog = schedule.build(buildCounter, lastBuild, now, getProjectPropertiesMap(now), target, progress);
            writeline("/home/ubuntu/results/coverage/Project/Project_9_10.coverage", "912c3c0c-58ca-4d10-b830-c23d5384718f");
            buildLog.addContent(builderLog.detach());
            writeline("/home/ubuntu/results/coverage/Project/Project_9_10.coverage", "0ad4efb6-8832-4f68-95ad-5efb5bde8e06");
            boolean buildSuccessful = buildLog.wasBuildSuccessful();
            writeline("/home/ubuntu/results/coverage/Project/Project_9_10.coverage", "c82a092c-c744-427f-8ce1-640de32ab8a4");
            fireResultEvent(new BuildResultEvent(this, buildSuccessful));
            writeline("/home/ubuntu/results/coverage/Project/Project_9_10.coverage", "01fbd6ce-eb30-47bd-8674-873280e79f87");
            if (!getLabelIncrementer().isPreBuildIncrementer() && buildSuccessful) {
                writeline("/home/ubuntu/results/coverage/Project/Project_9_10.coverage", "d969a1e4-d6b0-4a40-b7e5-4a2a2b0c3294");
                label = getLabelIncrementer().incrementLabel(label, buildLog.getContent());
            }
            writeline("/home/ubuntu/results/coverage/Project/Project_9_10.coverage", "33b4c1da-cbe9-4b22-afbc-191a5a50d031");
            setState(ProjectState.MERGING_LOGS);
            writeline("/home/ubuntu/results/coverage/Project/Project_9_10.coverage", "63637d9e-919a-47a2-af8e-8ac9b8acbc41");
            buildLog.writeLogFile(now);
            writeline("/home/ubuntu/results/coverage/Project/Project_9_10.coverage", "e6e5619d-a633-468b-9884-5cc875e2df92");
            // regardless of success or failure (buildAfterFailed = false in config.xml)
            if (!buildAfterFailed) {
                writeline("/home/ubuntu/results/coverage/Project/Project_9_10.coverage", "8e2d6ec9-1986-41bc-b253-9db292c8a8bd");
                lastBuild = now;
            }
            writeline("/home/ubuntu/results/coverage/Project/Project_9_10.coverage", "5d6e87f4-2201-4b86-9743-1d1969ed8076");
            // If this was a successful build, update both last build and last successful build
            if (buildSuccessful) {
                writeline("/home/ubuntu/results/coverage/Project/Project_9_10.coverage", "b9278928-e450-476e-9fda-715c3d1fcab9");
                lastBuild = now;
                writeline("/home/ubuntu/results/coverage/Project/Project_9_10.coverage", "bb250aac-7a76-4641-a20a-82009d261a23");
                lastSuccessfulBuild = now;
                writeline("/home/ubuntu/results/coverage/Project/Project_9_10.coverage", "6e5d7b1d-b3f7-44e0-b99a-2dad05f8c3b0");
                info("build successful");
            } else {
                writeline("/home/ubuntu/results/coverage/Project/Project_9_10.coverage", "6b6fc4c7-f32b-44a2-8871-b67eeca0f674");
                info("build failed");
            }
            writeline("/home/ubuntu/results/coverage/Project/Project_9_10.coverage", "6065dbac-4e40-4e77-a325-7a26a49a17d9");
            buildCounter++;
            writeline("/home/ubuntu/results/coverage/Project/Project_9_10.coverage", "303cdfaa-8019-4988-854f-df1c4cda4310");
            setWasLastBuildSuccessful(buildSuccessful);
            writeline("/home/ubuntu/results/coverage/Project/Project_9_10.coverage", "380b35e0-f58d-4178-bc61-7ec6918267cb");
            // also need to reset forced flag before serializing, unless buildForced var is transient
            // resetBuildForcedOnlyIfBuildWasForced(buildWasForced);
            serializeProject();
            writeline("/home/ubuntu/results/coverage/Project/Project_9_10.coverage", "6d53ecf1-337a-4c9c-a253-c148c6905dbe");
            // @todo Add Progress param to Publisher API?
            publish(buildLog);
            writeline("/home/ubuntu/results/coverage/Project/Project_9_10.coverage", "2a093cab-af88-44a7-a651-7ea715f45731");
            buildLog.reset();
        } finally {
            writeline("/home/ubuntu/results/coverage/Project/Project_9_10.coverage", "2e746ed0-dedd-4cb0-b043-3c6542ba861e");
            resetBuildForcedOnlyIfBuildWasForced(buildWasForced);
            writeline("/home/ubuntu/results/coverage/Project/Project_9_10.coverage", "691b5e63-4cf5-405e-b308-c55d71d69b8d");
            setState(ProjectState.IDLE);
        }
    }

    private String useAndResetBuildTargetIfBuildWasForced(final boolean buildWasForced) {
        writeline("/home/ubuntu/results/coverage/Project/Project_9_10.coverage", "0d52396a-cbe7-46c2-ab81-c9eaddd8aa26");
        String target = null;
        writeline("/home/ubuntu/results/coverage/Project/Project_9_10.coverage", "718743f1-8d5a-4b19-a63b-11746b67dd6f");
        if (buildWasForced) {
            writeline("/home/ubuntu/results/coverage/Project/Project_9_10.coverage", "41d77e9c-fec3-41ec-a116-0da2ca135bc1");
            target = buildTarget;
            writeline("/home/ubuntu/results/coverage/Project/Project_9_10.coverage", "eca0a8f4-e38e-4ed7-b3a1-64272f4f0969");
            buildTarget = null;
        }
        writeline("/home/ubuntu/results/coverage/Project/Project_9_10.coverage", "2acffe46-0ee7-4a18-9c59-2cbfe4d6140d");
        return target;
    }

    private void resetBuildForcedOnlyIfBuildWasForced(final boolean buildWasForced) {
        writeline("/home/ubuntu/results/coverage/Project/Project_9_10.coverage", "921446cb-5041-4bf0-bced-eee2881ea172");
        if (buildWasForced) {
            writeline("/home/ubuntu/results/coverage/Project/Project_9_10.coverage", "96ab8a56-2b51-4352-bfc1-b81284756f95");
            buildForced = false;
        }
    }

    void setBuildStartTime(final Date date) {
        writeline("/home/ubuntu/results/coverage/Project/Project_9_10.coverage", "f1b35ba8-0b33-46c3-a508-48037054e5fe");
        buildStartTime = date;
    }

    public void run() {
        writeline("/home/ubuntu/results/coverage/Project/Project_9_10.coverage", "9363ed7e-4f72-44f1-a3f6-1ff66b9173c0");
        LOG.info("Project " + name + " started");
        writeline("/home/ubuntu/results/coverage/Project/Project_9_10.coverage", "cb333086-2ab6-4600-ae95-8aeb3f7020e2");
        try {
            writeline("/home/ubuntu/results/coverage/Project/Project_9_10.coverage", "5e294f4f-4b69-499c-8a38-cb8f519365b4");
            while (!stopped) {
                writeline("/home/ubuntu/results/coverage/Project/Project_9_10.coverage", "8f063af2-f241-4a8f-9bd6-fcbf19cf7777");
                try {
                    writeline("/home/ubuntu/results/coverage/Project/Project_9_10.coverage", "c4738b83-294d-47a5-947c-0dcab1be0377");
                    waitIfPaused();
                    writeline("/home/ubuntu/results/coverage/Project/Project_9_10.coverage", "1d9b3ad2-7670-4cc4-8d8e-a38478590174");
                    if (!stopped) {
                        writeline("/home/ubuntu/results/coverage/Project/Project_9_10.coverage", "ff661add-32f9-46cf-bf04-202d2bfd8a82");
                        waitForNextBuild();
                    }
                    writeline("/home/ubuntu/results/coverage/Project/Project_9_10.coverage", "43ea8f86-34cc-4ca0-a92d-b1b037f31d73");
                    if (!stopped) {
                        writeline("/home/ubuntu/results/coverage/Project/Project_9_10.coverage", "be172bae-a022-4d9d-b3cd-d9937dcf1b98");
                        setState(ProjectState.QUEUED);
                        writeline("/home/ubuntu/results/coverage/Project/Project_9_10.coverage", "fcfa773f-63b0-42da-bba7-09c6370096cc");
                        synchronized (scheduleMutex) {
                            writeline("/home/ubuntu/results/coverage/Project/Project_9_10.coverage", "c2bec6f0-cbd1-4d14-bd8e-b607b4f631cb");
                            queue.requestBuild(projectConfig);
                            writeline("/home/ubuntu/results/coverage/Project/Project_9_10.coverage", "75dd32c8-be4f-45c8-97a5-9f92ca82ac6e");
                            waitForBuildToFinish();
                        }
                    }
                } catch (InterruptedException e) {
                    writeline("/home/ubuntu/results/coverage/Project/Project_9_10.coverage", "c9f882ae-e393-4162-b49f-1e47ed406cda");
                    final String message = "Project " + name + ".run() interrupted";
                    writeline("/home/ubuntu/results/coverage/Project/Project_9_10.coverage", "01dbeb4d-e6a3-40b8-a31f-ae304f92de0b");
                    LOG.error(message, e);
                    writeline("/home/ubuntu/results/coverage/Project/Project_9_10.coverage", "b69cdbab-db2a-407a-8440-3f31c959401a");
                    throw new RuntimeException(message);
                }
            }
        } finally {
            writeline("/home/ubuntu/results/coverage/Project/Project_9_10.coverage", "944dd93b-b425-4abd-97b6-24607d13e8b7");
            stopped = true;
            writeline("/home/ubuntu/results/coverage/Project/Project_9_10.coverage", "d9cdebbf-9363-4243-a936-1c40ba41f6b4");
            LOG.info("Project " + name + " stopped");
        }
    }

    void waitIfPaused() throws InterruptedException {
        writeline("/home/ubuntu/results/coverage/Project/Project_9_10.coverage", "cb2696b1-3ad3-4ed0-9a18-73e860cecda4");
        synchronized (pausedMutex) {
            writeline("/home/ubuntu/results/coverage/Project/Project_9_10.coverage", "0d7815f2-09f3-4a88-8969-8ae62b3c13f8");
            while (isPaused) {
                writeline("/home/ubuntu/results/coverage/Project/Project_9_10.coverage", "02313349-ecd0-4d57-b8cc-26cf15c99db5");
                setState(ProjectState.PAUSED);
                writeline("/home/ubuntu/results/coverage/Project/Project_9_10.coverage", "3063049f-467b-485a-b59d-19b9fb5d55a9");
                pausedMutex.wait(10 * DateUtil.ONE_MINUTE);
            }
        }
    }

    void waitForNextBuild() throws InterruptedException {
        writeline("/home/ubuntu/results/coverage/Project/Project_9_10.coverage", "45e93633-3d03-4747-a843-513a00ede4b1");
        long waitTime = getTimeToNextBuild(new Date());
        writeline("/home/ubuntu/results/coverage/Project/Project_9_10.coverage", "ef4d8a86-1593-45cc-848b-b6b3a4a9d8de");
        if (needToWaitForNextBuild(waitTime) && !buildForced) {
            writeline("/home/ubuntu/results/coverage/Project/Project_9_10.coverage", "a5e016e4-fcb3-4ae7-b0c3-d3108d3fa326");
            final String msg = "next build in " + DateUtil.formatTime(waitTime);
            writeline("/home/ubuntu/results/coverage/Project/Project_9_10.coverage", "a64490a8-2c89-4d6d-9911-fe2dc9983a1c");
            info(msg);
            writeline("/home/ubuntu/results/coverage/Project/Project_9_10.coverage", "93ee3700-fec7-4405-b3f8-11276b0fea46");
            synchronized (waitMutex) {
                writeline("/home/ubuntu/results/coverage/Project/Project_9_10.coverage", "257ee098-7114-47b0-ac62-20c960eaa839");
                setState(ProjectState.WAITING);
                writeline("/home/ubuntu/results/coverage/Project/Project_9_10.coverage", "0d5902dc-abf8-4143-960d-85bb89b8c4a9");
                progress.setValue(msg);
                writeline("/home/ubuntu/results/coverage/Project/Project_9_10.coverage", "24ed7e8b-f575-4d5d-8945-e2fa43f9ccec");
                waitMutex.wait(waitTime);
            }
        }
    }

    long getTimeToNextBuild(Date now) {
        writeline("/home/ubuntu/results/coverage/Project/Project_9_10.coverage", "456ebbf8-b65b-4563-8a17-33fdc7d733ed");
        long waitTime = projectConfig.getSchedule().getTimeToNextBuild(now, getBuildInterval());
        writeline("/home/ubuntu/results/coverage/Project/Project_9_10.coverage", "22f7207b-bcb6-4957-9e6f-7775f623b168");
        if (waitTime == 0) {
            writeline("/home/ubuntu/results/coverage/Project/Project_9_10.coverage", "b221f5a6-3320-4ea7-af86-2ed8c596f661");
            // project that has just built within a minute time
            if (buildStartTime != null) {
                writeline("/home/ubuntu/results/coverage/Project/Project_9_10.coverage", "3dcd22aa-ff34-498a-9f22-9ca1ed0d614b");
                long millisSinceLastBuild = now.getTime() - buildStartTime.getTime();
                writeline("/home/ubuntu/results/coverage/Project/Project_9_10.coverage", "aba26728-4c73-43ab-868f-238d1b92b3d0");
                if (millisSinceLastBuild < Schedule.ONE_MINUTE) {
                    writeline("/home/ubuntu/results/coverage/Project/Project_9_10.coverage", "27395ec6-1171-4a06-9ab4-31cd1375d150");
                    debug("build finished within a minute, getting new time to next build");
                    writeline("/home/ubuntu/results/coverage/Project/Project_9_10.coverage", "f67788a1-ceb1-4809-ad5b-a78c1db5e0fb");
                    Date oneMinuteInFuture = new Date(now.getTime() + Schedule.ONE_MINUTE);
                    writeline("/home/ubuntu/results/coverage/Project/Project_9_10.coverage", "d2eb4509-d64d-49c7-af29-744e9b7afa0e");
                    waitTime = projectConfig.getSchedule().getTimeToNextBuild(oneMinuteInFuture, getBuildInterval());
                    writeline("/home/ubuntu/results/coverage/Project/Project_9_10.coverage", "ce674646-e5c7-408a-a7a0-81578ab6714f");
                    waitTime += Schedule.ONE_MINUTE;
                }
            }
        }
        writeline("/home/ubuntu/results/coverage/Project/Project_9_10.coverage", "32142bfd-81dc-4021-a5ff-3c6fe7beee60");
        return waitTime;
    }

    static boolean needToWaitForNextBuild(long waitTime) {
        writelineStatic("/home/ubuntu/results/coverage/Project/Project_9_10.coverage", "46eebc6b-9964-4630-9f29-cdb53467d08d");
        return waitTime > 0;
    }

    /**
     * @return true if build was forced, intended for unit testing only.
     */
    boolean isBuildForced() {
        writeline("/home/ubuntu/results/coverage/Project/Project_9_10.coverage", "2fdf208e-a429-46f3-9445-0bc69614a19c");
        return buildForced;
    }

    void forceBuild() {
        writeline("/home/ubuntu/results/coverage/Project/Project_9_10.coverage", "12d88245-6cf1-47ac-b23f-87e6fd54b939");
        synchronized (waitMutex) {
            writeline("/home/ubuntu/results/coverage/Project/Project_9_10.coverage", "e4ba39ac-cbb3-444b-bab6-73f65a477870");
            waitMutex.notify();
        }
    }

    public void forceBuildWithTarget(String buildTarget) {
        writeline("/home/ubuntu/results/coverage/Project/Project_9_10.coverage", "1a08f09e-3e82-4c0d-8f1b-afd1c94811ec");
        this.buildTarget = buildTarget;
        writeline("/home/ubuntu/results/coverage/Project/Project_9_10.coverage", "8c3d8b56-1dd8-4b78-bbe1-18b52348bbef");
        setBuildForced(true);
    }

    public void forceBuildWithTarget(String buildTarget, Map<String, String> addedProperties) {
        writeline("/home/ubuntu/results/coverage/Project/Project_9_10.coverage", "65bcb9e6-dba3-484f-8439-18215b5671b4");
        additionalProperties = addedProperties;
        writeline("/home/ubuntu/results/coverage/Project/Project_9_10.coverage", "eecc5c64-4242-4a18-9064-4268d42b4d46");
        forceBuildWithTarget(buildTarget);
    }

    void waitForBuildToFinish() throws InterruptedException {
        writeline("/home/ubuntu/results/coverage/Project/Project_9_10.coverage", "cd075cca-b85b-4550-b82a-451694e2610c");
        synchronized (scheduleMutex) {
            writeline("/home/ubuntu/results/coverage/Project/Project_9_10.coverage", "e83b70ef-db28-4601-ab64-5437fd667aef");
            debug("waiting for build to finish");
            writeline("/home/ubuntu/results/coverage/Project/Project_9_10.coverage", "aa70a705-e2a9-4c5b-8b2b-e9d4684843e2");
            scheduleMutex.wait();
        }
    }

    void buildFinished() {
        writeline("/home/ubuntu/results/coverage/Project/Project_9_10.coverage", "c5ec5766-9189-4605-96a6-b5241bef9669");
        synchronized (scheduleMutex) {
            writeline("/home/ubuntu/results/coverage/Project/Project_9_10.coverage", "aacecf32-f56f-4dca-b170-f8230c20b1de");
            debug("build finished");
            writeline("/home/ubuntu/results/coverage/Project/Project_9_10.coverage", "02e84a20-19fc-4741-82bb-eab39bb2bf7c");
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
        writeline("/home/ubuntu/results/coverage/Project/Project_9_10.coverage", "fc8e946a-dc65-49bf-b87d-d541fed55cbf");
        setState(ProjectState.MODIFICATIONSET);
        writeline("/home/ubuntu/results/coverage/Project/Project_9_10.coverage", "7c0c081a-3825-4809-9128-01f336d557af");
        final ModificationSet modificationSet = projectConfig.getModificationSet();
        writeline("/home/ubuntu/results/coverage/Project/Project_9_10.coverage", "ae91526a-e537-4d12-858f-28890e0480b9");
        if (modificationSet == null) {
            writeline("/home/ubuntu/results/coverage/Project/Project_9_10.coverage", "b3511e71-ac52-4391-9e79-c285c27d1f1b");
            debug("no modification set, nothing to detect.");
            writeline("/home/ubuntu/results/coverage/Project/Project_9_10.coverage", "2785ce62-3930-4724-b0e6-a94cfa9871fe");
            if (buildWasForced) {
                writeline("/home/ubuntu/results/coverage/Project/Project_9_10.coverage", "f15d60db-4890-456d-b40e-89d49ef1d75e");
                info("no modification set but build was forced");
                writeline("/home/ubuntu/results/coverage/Project/Project_9_10.coverage", "7cd1d762-e4fc-4bc8-b85c-da56bacf602c");
                return new Element("modifications");
            }
            writeline("/home/ubuntu/results/coverage/Project/Project_9_10.coverage", "48399f87-3336-458d-ad5d-0c7a14108de1");
            if (!requiremodification) {
                writeline("/home/ubuntu/results/coverage/Project/Project_9_10.coverage", "58b8a554-b9dd-4ae5-a6d5-0d17a6098c46");
                info("no modification set but no modifications required");
                writeline("/home/ubuntu/results/coverage/Project/Project_9_10.coverage", "50f3bf72-ec93-481b-84d0-35c0a891107f");
                return new Element("modifications");
            }
            writeline("/home/ubuntu/results/coverage/Project/Project_9_10.coverage", "b1717368-2453-4a2c-a576-a46612e96ab6");
            return null;
        }
        writeline("/home/ubuntu/results/coverage/Project/Project_9_10.coverage", "e00a3d8e-8dc3-469d-b583-d78e4866f89d");
        final boolean checkNewChangesFirst = checkOnlySinceLastBuild();
        writeline("/home/ubuntu/results/coverage/Project/Project_9_10.coverage", "dc95044f-4fce-486d-bf72-33b13fa5a032");
        Element modifications;
        writeline("/home/ubuntu/results/coverage/Project/Project_9_10.coverage", "c605d9cf-ca98-4a9b-8f00-bcfef6349963");
        if (checkNewChangesFirst) {
            writeline("/home/ubuntu/results/coverage/Project/Project_9_10.coverage", "9c41e6e1-bb59-4e30-a7bb-a1794b0bc320");
            debug("getting changes since last build");
            writeline("/home/ubuntu/results/coverage/Project/Project_9_10.coverage", "56c110f8-4d7b-49af-81c3-873824ac09d8");
            modifications = modificationSet.retrieveModificationsAsElement(lastBuild, progress);
        } else {
            writeline("/home/ubuntu/results/coverage/Project/Project_9_10.coverage", "b3baf0e6-73c2-4e44-8661-67828ff1f1cc");
            debug("getting changes since last successful build");
            writeline("/home/ubuntu/results/coverage/Project/Project_9_10.coverage", "6448f70c-bb15-43f5-82ed-9478ec37cf66");
            modifications = modificationSet.retrieveModificationsAsElement(lastSuccessfulBuild, progress);
        }
        writeline("/home/ubuntu/results/coverage/Project/Project_9_10.coverage", "507aa328-2221-48b1-85db-c96c4be83971");
        if (!modificationSet.isModified()) {
            writeline("/home/ubuntu/results/coverage/Project/Project_9_10.coverage", "9ccfd926-e3fc-46b1-acd5-222dc20a203f");
            info("No modifications found, build not necessary.");
            writeline("/home/ubuntu/results/coverage/Project/Project_9_10.coverage", "ae4af4bd-eb8c-4eec-81d4-3bf9c50593ce");
            // * build forced
            if (buildAfterFailed && !wasLastBuildSuccessful) {
                writeline("/home/ubuntu/results/coverage/Project/Project_9_10.coverage", "5024b297-917a-4760-a484-dcb3a68053b4");
                info("Building anyway, since buildAfterFailed is true and last build failed.");
            } else if (!requiremodification) {
                writeline("/home/ubuntu/results/coverage/Project/Project_9_10.coverage", "913b9eef-e33c-4b2d-aeb6-1fc40a27e3ae");
                info("Building anyway, since modifications not required");
            } else {
                writeline("/home/ubuntu/results/coverage/Project/Project_9_10.coverage", "4eee49ae-7277-49dc-8141-1f41868df0ba");
                if (buildWasForced) {
                    writeline("/home/ubuntu/results/coverage/Project/Project_9_10.coverage", "72a0bb77-9656-4bf0-b0f1-9e327014046c");
                    info("Building anyway, since build was explicitly forced.");
                } else {
                    writeline("/home/ubuntu/results/coverage/Project/Project_9_10.coverage", "ac513632-2ac6-4fe3-8e97-b4109e60c3f1");
                    return null;
                }
            }
        }
        writeline("/home/ubuntu/results/coverage/Project/Project_9_10.coverage", "428e4c0c-b268-466f-b1f4-7d9cd095b9c1");
        if (checkNewChangesFirst) {
            writeline("/home/ubuntu/results/coverage/Project/Project_9_10.coverage", "84bd6053-d62a-494c-9f7f-b3802101d61a");
            debug("new changes found; now getting complete set");
            writeline("/home/ubuntu/results/coverage/Project/Project_9_10.coverage", "d4b1e8bc-bb7e-4454-8053-7f6ac345123b");
            modifications = modificationSet.retrieveModificationsAsElement(lastSuccessfulBuild, progress);
        }
        writeline("/home/ubuntu/results/coverage/Project/Project_9_10.coverage", "ef6eb845-a1a9-4d4d-be9f-afe9261b6851");
        return modifications;
    }

    /**
     * @return boolean
     */
    boolean checkOnlySinceLastBuild() {
        writeline("/home/ubuntu/results/coverage/Project/Project_9_10.coverage", "7d3c8591-cdc3-4d30-9e35-80201208cd8b");
        if (lastBuild == null || lastSuccessfulBuild == null) {
            writeline("/home/ubuntu/results/coverage/Project/Project_9_10.coverage", "fe01eb91-d851-4904-b7c1-d11b2f568d46");
            return false;
        }
        writeline("/home/ubuntu/results/coverage/Project/Project_9_10.coverage", "0a2da777-d68f-463e-a635-53a5063b82a6");
        final long lastBuildLong = lastBuild.getTime();
        writeline("/home/ubuntu/results/coverage/Project/Project_9_10.coverage", "ae136313-5911-4d61-91f0-847ad4abcc2f");
        final long timeDifference = lastBuildLong - lastSuccessfulBuild.getTime();
        writeline("/home/ubuntu/results/coverage/Project/Project_9_10.coverage", "86b2ec98-7fa1-47d7-b690-754d1de590be");
        final boolean moreThanASecond = timeDifference > DateUtil.ONE_SECOND;
        writeline("/home/ubuntu/results/coverage/Project/Project_9_10.coverage", "adbd9542-a75b-46f8-bb4c-53e5871c0f7d");
        return !buildAfterFailed && moreThanASecond;
    }

    /**
     * Serialize the project to allow resumption after a process bounce
     */
    public void serializeProject() {
        writeline("/home/ubuntu/results/coverage/Project/Project_9_10.coverage", "81c3398f-12ce-481e-8028-4958523ef64b");
        final String safeProjectName = Builder.getFileSystemSafeProjectName(name);
        writeline("/home/ubuntu/results/coverage/Project/Project_9_10.coverage", "ab04a708-4f05-42c2-853b-1d485dd086eb");
        try {
            writeline("/home/ubuntu/results/coverage/Project/Project_9_10.coverage", "d3e14233-0da7-44ed-a44a-2208074582b9");
            final ObjectOutputStream s = new ObjectOutputStream(new FileOutputStream(safeProjectName + ".ser"));
            writeline("/home/ubuntu/results/coverage/Project/Project_9_10.coverage", "b5c6a781-b7df-4670-a876-e93b35afb83d");
            try {
                writeline("/home/ubuntu/results/coverage/Project/Project_9_10.coverage", "ec999025-df56-4d43-8b1f-d941af24e123");
                s.writeObject(this);
                writeline("/home/ubuntu/results/coverage/Project/Project_9_10.coverage", "189c7c78-0b11-4168-af76-eef5bf4be589");
                s.flush();
                writeline("/home/ubuntu/results/coverage/Project/Project_9_10.coverage", "43a615d1-2dac-435e-8596-129d8ddab916");
                debug("Serializing project to [" + safeProjectName + ".ser]");
            } finally {
                writeline("/home/ubuntu/results/coverage/Project/Project_9_10.coverage", "8bc73592-cc17-407e-9b0e-069725aac088");
                s.close();
            }
        } catch (Exception e) {
            writeline("/home/ubuntu/results/coverage/Project/Project_9_10.coverage", "53b807ad-7c37-4367-9bb6-3c5a8ff18e0e");
            LOG.warn("Error serializing project to [" + safeProjectName + ".ser]: " + e.getMessage(), e);
        }
    }

    public void setLabelIncrementer(final LabelIncrementer incrementer) throws CruiseControlException {
        writeline("/home/ubuntu/results/coverage/Project/Project_9_10.coverage", "4ddfd3ea-78b3-4137-8e4c-bb15f2873e42");
        if (incrementer == null) {
            writeline("/home/ubuntu/results/coverage/Project/Project_9_10.coverage", "ee22fafa-8aad-4373-83de-bc29f80a64ff");
            throw new IllegalArgumentException("label incrementer can't be null");
        }
        writeline("/home/ubuntu/results/coverage/Project/Project_9_10.coverage", "e58a1b0f-9f61-4f3c-8abe-a770eab7e3d0");
        labelIncrementer = incrementer;
        writeline("/home/ubuntu/results/coverage/Project/Project_9_10.coverage", "e9db429f-e20b-4b78-babf-367a3783d8bb");
        if (label == null) {
            writeline("/home/ubuntu/results/coverage/Project/Project_9_10.coverage", "a2846c34-0c66-4c59-825f-798ca45bae0a");
            label = labelIncrementer.getDefaultLabel();
        }
        writeline("/home/ubuntu/results/coverage/Project/Project_9_10.coverage", "f45a5cad-cb18-4df9-8345-8edd8eac6c30");
        validateLabel(label, labelIncrementer);
    }

    public LabelIncrementer getLabelIncrementer() {
        writeline("/home/ubuntu/results/coverage/Project/Project_9_10.coverage", "1bc9ab1f-ee77-4cf0-83b7-7a7411164475");
        return labelIncrementer;
    }

    public void setName(final String projectName) {
        writeline("/home/ubuntu/results/coverage/Project/Project_9_10.coverage", "24f60435-9e0a-4fd8-ad73-40e5b2446000");
        name = projectName;
    }

    public String getName() {
        writeline("/home/ubuntu/results/coverage/Project/Project_9_10.coverage", "8189012c-ab05-40a1-9b1e-17b16de17ffa");
        return name;
    }

    public void setLabel(final String newLabel) {
        writeline("/home/ubuntu/results/coverage/Project/Project_9_10.coverage", "08429fd3-ad7e-4bbc-aba1-a400a380ba64");
        label = newLabel;
    }

    public String getLabel() {
        writeline("/home/ubuntu/results/coverage/Project/Project_9_10.coverage", "03d51539-156e-4741-bb11-1a95a4b30afd");
        return label;
    }

    /**
     * @param newLastBuild string containing the build date in the format
     * yyyyMMddHHmmss
     * @throws CruiseControlException if the date cannot be extracted from the
     * input string
     */
    public void setLastBuild(final String newLastBuild) throws CruiseControlException {
        writeline("/home/ubuntu/results/coverage/Project/Project_9_10.coverage", "81e5eef9-d9e0-4bba-b7f3-6e47a4c56f99");
        lastBuild = DateUtil.parseFormattedTime(newLastBuild, "lastBuild");
    }

    /**
     * @param newLastSuccessfulBuild string containing the build date in the format
     * yyyyMMddHHmmss
     * @throws CruiseControlException if the date cannot be extracted from the
     * input string
     */
    public void setLastSuccessfulBuild(final String newLastSuccessfulBuild) throws CruiseControlException {
        writeline("/home/ubuntu/results/coverage/Project/Project_9_10.coverage", "0a9ec243-dcff-42c8-91bf-883790997f48");
        lastSuccessfulBuild = DateUtil.parseFormattedTime(newLastSuccessfulBuild, "lastSuccessfulBuild");
    }

    public String getLastBuild() {
        writeline("/home/ubuntu/results/coverage/Project/Project_9_10.coverage", "ae19642a-bac8-4682-b965-36f5c47fcc10");
        if (lastBuild == null) {
            writeline("/home/ubuntu/results/coverage/Project/Project_9_10.coverage", "70c85489-333d-45c4-aa19-b4f784aa1578");
            return null;
        }
        writeline("/home/ubuntu/results/coverage/Project/Project_9_10.coverage", "041c2073-5a93-430b-b31a-f878d16dcaf0");
        return DateUtil.getFormattedTime(lastBuild);
    }

    public boolean getBuildForced() {
        writeline("/home/ubuntu/results/coverage/Project/Project_9_10.coverage", "1bc084c9-15c6-4472-87b8-88dd9b2e42c8");
        return buildForced;
    }

    public void setBuildForced(boolean forceNewBuildNow) {
        writeline("/home/ubuntu/results/coverage/Project/Project_9_10.coverage", "57841d9c-d78f-42a9-a0d9-d802a95d20cb");
        buildForced = forceNewBuildNow;
        writeline("/home/ubuntu/results/coverage/Project/Project_9_10.coverage", "20ad65d2-dabf-422c-86a1-1ead25c056ff");
        if (forceNewBuildNow) {
            writeline("/home/ubuntu/results/coverage/Project/Project_9_10.coverage", "f5925520-3d3a-444e-8ca2-f20aff6b804a");
            forceBuild();
        }
    }

    public String getLastSuccessfulBuild() {
        writeline("/home/ubuntu/results/coverage/Project/Project_9_10.coverage", "74217e8e-7787-40f2-b8f7-e5112a80a076");
        if (lastSuccessfulBuild == null) {
            writeline("/home/ubuntu/results/coverage/Project/Project_9_10.coverage", "61937421-f96b-4df6-bf4d-94b6728d85be");
            return null;
        }
        writeline("/home/ubuntu/results/coverage/Project/Project_9_10.coverage", "2ce3993b-ff75-41a5-8a6d-bfe389994a3d");
        return DateUtil.getFormattedTime(lastSuccessfulBuild);
    }

    public String getLogDir() {
        writeline("/home/ubuntu/results/coverage/Project/Project_9_10.coverage", "029cab20-515a-4814-9916-95be14541715");
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
        writeline("/home/ubuntu/results/coverage/Project/Project_9_10.coverage", "e595bbfd-b149-45e4-9997-a473357d8ac1");
        if (overrideBuildInterval == null) {
            writeline("/home/ubuntu/results/coverage/Project/Project_9_10.coverage", "20c7492a-c20c-42b8-9616-9f2dcd336b85");
            return projectConfig.getSchedule().getInterval();
        } else {
            writeline("/home/ubuntu/results/coverage/Project/Project_9_10.coverage", "7d0a0e4b-53a9-418c-8aa2-88944dfbdf8e");
            return overrideBuildInterval;
        }
    }

    /**
     * Sets the build interval that this Project should use. This method
     * overrides the value initially specified in the Schedule attribute.
     * @param sleepMillis the number of milliseconds to sleep between build attempts
     */
    public void overrideBuildInterval(final long sleepMillis) {
        writeline("/home/ubuntu/results/coverage/Project/Project_9_10.coverage", "e80f966c-6c9b-48ea-8206-10e2cda7d39d");
        overrideBuildInterval = sleepMillis;
    }

    public boolean isPaused() {
        writeline("/home/ubuntu/results/coverage/Project/Project_9_10.coverage", "b2cc713f-9c68-46cc-8495-bfd6d43c5af8");
        return isPaused;
    }

    public void setPaused(final boolean paused) {
        writeline("/home/ubuntu/results/coverage/Project/Project_9_10.coverage", "72054c46-7453-4586-bd22-bdaf48efaf1d");
        synchronized (pausedMutex) {
            writeline("/home/ubuntu/results/coverage/Project/Project_9_10.coverage", "6235f5bc-115f-4acb-8c42-13f6f048250a");
            if (isPaused && !paused) {
                writeline("/home/ubuntu/results/coverage/Project/Project_9_10.coverage", "0501e36f-eed9-44c7-bbf2-8b6e3fea51ca");
                pausedMutex.notifyAll();
            }
            writeline("/home/ubuntu/results/coverage/Project/Project_9_10.coverage", "36d90a3a-81f7-44f7-b611-c1738681b02f");
            isPaused = paused;
        }
    }

    public void setBuildAfterFailed(final boolean rebuildEvenWithNoNewModifications) {
        writeline("/home/ubuntu/results/coverage/Project/Project_9_10.coverage", "c87633b5-f44b-45df-bcc6-199c72bda078");
        buildAfterFailed = rebuildEvenWithNoNewModifications;
    }

    public String getStatus() {
        writeline("/home/ubuntu/results/coverage/Project/Project_9_10.coverage", "ad93f67b-c7c4-4f1b-b6d7-07f97b1de195");
        return getState().getDescription();
    }

    public String getStatusWithQueuePosition() {
        writeline("/home/ubuntu/results/coverage/Project/Project_9_10.coverage", "4f56d75d-8d77-41df-a781-1533f37192ad");
        if (ProjectState.QUEUED.equals(getState())) {
            writeline("/home/ubuntu/results/coverage/Project/Project_9_10.coverage", "03664478-0783-48a3-a9fa-114285b9b3a6");
            return getState().getDescription() + " - " + queue.findPosition(projectConfig);
        } else {
            writeline("/home/ubuntu/results/coverage/Project/Project_9_10.coverage", "67fa3799-53cd-46e7-826b-281a833f8d60");
            return getState().getDescription();
        }
    }

    public ProjectState getState() {
        writeline("/home/ubuntu/results/coverage/Project/Project_9_10.coverage", "3adc6751-ecfa-4255-b55d-c003b6671c0b");
        return state;
    }

    private void setState(final ProjectState newState) {
        writeline("/home/ubuntu/results/coverage/Project/Project_9_10.coverage", "178c6a29-9f00-421f-a1ae-828ae8bdd52e");
        state = newState;
        writeline("/home/ubuntu/results/coverage/Project/Project_9_10.coverage", "d04c14e2-9c16-4aa1-8b8a-52b04b2b5766");
        info(getStatus());
        writeline("/home/ubuntu/results/coverage/Project/Project_9_10.coverage", "697be5ca-611b-4273-a09d-8f8a8485293a");
        notifyListeners(new ProjectStateChangedEvent(name, getState()));
        writeline("/home/ubuntu/results/coverage/Project/Project_9_10.coverage", "058f5a5b-e25a-4e57-8fb7-ea394803acc1");
        fireProgressEvent(new BuildProgressEvent(this, getState()));
    }

    public void setBuildQueue(final BuildQueue buildQueue) {
        writeline("/home/ubuntu/results/coverage/Project/Project_9_10.coverage", "afe17baa-adb9-4aea-8c36-1aa733dec4a8");
        queue = buildQueue;
    }

    public String getBuildStartTime() {
        writeline("/home/ubuntu/results/coverage/Project/Project_9_10.coverage", "537bad7c-987a-4394-8257-86f9d156a2b7");
        return DateUtil.getFormattedTime(buildStartTime);
    }

    public Log getLog() {
        writeline("/home/ubuntu/results/coverage/Project/Project_9_10.coverage", "d92051c7-5178-40f7-b0e5-1d8a1c72162a");
        return this.projectConfig.getLog();
    }

    /**
     * Initialize the project. Uses ProjectXMLHelper to parse a project file.
     */
    protected void init() {
        writeline("/home/ubuntu/results/coverage/Project/Project_9_10.coverage", "9dc8d664-08b2-4cc1-a61e-f83e11a3346d");
        if (projectConfig == null) {
            writeline("/home/ubuntu/results/coverage/Project/Project_9_10.coverage", "311aa977-ddd8-40e0-aff8-d46d7ba43c0e");
            throw new IllegalStateException("projectConfig must be set on project before calling init()");
        }
        writeline("/home/ubuntu/results/coverage/Project/Project_9_10.coverage", "84a41b02-c753-41dd-9e7b-789a7f6497e3");
        buildAfterFailed = projectConfig.shouldBuildAfterFailed();
        writeline("/home/ubuntu/results/coverage/Project/Project_9_10.coverage", "04bfa8e3-3001-4516-b015-2e5941694031");
        forceOnly = projectConfig.isForceOnly();
        writeline("/home/ubuntu/results/coverage/Project/Project_9_10.coverage", "919c50b4-0bc7-4764-a9ba-9cc31b6ad6c9");
        requiremodification = projectConfig.isRequiremodification();
        writeline("/home/ubuntu/results/coverage/Project/Project_9_10.coverage", "930e1c0b-70cd-4484-a6c3-ce1ea7066a89");
        if (lastBuild == null) {
            writeline("/home/ubuntu/results/coverage/Project/Project_9_10.coverage", "a5e31333-a324-4bc8-97ee-2fd1bc323c24");
            lastBuild = DateUtil.getMidnight();
        }
        writeline("/home/ubuntu/results/coverage/Project/Project_9_10.coverage", "930dad4e-6a83-458f-abc2-b5c33a8c92ea");
        if (lastSuccessfulBuild == null) {
            writeline("/home/ubuntu/results/coverage/Project/Project_9_10.coverage", "976b0938-3aba-4259-8d1e-b75507e9a767");
            lastSuccessfulBuild = lastBuild;
        }
        writeline("/home/ubuntu/results/coverage/Project/Project_9_10.coverage", "e4315635-26bf-4bc6-9a9b-bcc34487775d");
        if (LOG.isDebugEnabled()) {
            writeline("/home/ubuntu/results/coverage/Project/Project_9_10.coverage", "d03b4fe6-11da-488b-b81a-08b965d3afce");
            debug("buildInterval          = [" + getBuildInterval() + "]");
            writeline("/home/ubuntu/results/coverage/Project/Project_9_10.coverage", "d0353f36-73e8-47a0-acdd-da84eb124569");
            debug("buildForced            = [" + buildForced + "]");
            writeline("/home/ubuntu/results/coverage/Project/Project_9_10.coverage", "91d3ab01-d8d4-4fb7-b9cc-a7a14716f454");
            debug("buildAfterFailed       = [" + buildAfterFailed + "]");
            writeline("/home/ubuntu/results/coverage/Project/Project_9_10.coverage", "e94e70d8-6f99-465f-bab9-aecae7be911f");
            debug("requireModifcation     = [" + requiremodification + "]");
            writeline("/home/ubuntu/results/coverage/Project/Project_9_10.coverage", "66bf20fd-899d-4a06-a701-c436ea01cb38");
            debug("forceOnly              = [" + forceOnly + "]");
            writeline("/home/ubuntu/results/coverage/Project/Project_9_10.coverage", "40405319-374f-499c-855d-f185e7e09e5e");
            debug("buildCounter           = [" + buildCounter + "]");
            writeline("/home/ubuntu/results/coverage/Project/Project_9_10.coverage", "0a603a22-08fd-4a98-8468-8c6f82748522");
            debug("isPaused               = [" + isPaused + "]");
            writeline("/home/ubuntu/results/coverage/Project/Project_9_10.coverage", "77dec199-e04d-4d84-9394-1da30bac8ecf");
            debug("label                  = [" + label + "]");
            writeline("/home/ubuntu/results/coverage/Project/Project_9_10.coverage", "7c9e919b-476d-4b51-8da7-1a62ab66cfc5");
            debug("lastBuild              = [" + DateUtil.getFormattedTime(lastBuild) + "]");
            writeline("/home/ubuntu/results/coverage/Project/Project_9_10.coverage", "54854c68-6532-43c1-9d7b-9f8bd274d9c6");
            debug("lastSuccessfulBuild    = [" + DateUtil.getFormattedTime(lastSuccessfulBuild) + "]");
            writeline("/home/ubuntu/results/coverage/Project/Project_9_10.coverage", "163dcfc6-5207-4f35-a42e-75fef943e216");
            debug("logDir                 = [" + projectConfig.getLog().getLogDir() + "]");
            writeline("/home/ubuntu/results/coverage/Project/Project_9_10.coverage", "a7f33f68-cd22-423b-beb7-75fee2d6efe3");
            debug("logXmlEncoding         = [" + projectConfig.getLog().getLogXmlEncoding() + "]");
            writeline("/home/ubuntu/results/coverage/Project/Project_9_10.coverage", "addacab2-443e-443b-9166-ba5dad4b6354");
            debug("wasLastBuildSuccessful = [" + wasLastBuildSuccessful + "]");
        }
    }

    protected Element getProjectPropertiesElement(final Date now) {
        writeline("/home/ubuntu/results/coverage/Project/Project_9_10.coverage", "d58d5cd1-57ed-40f0-a309-2945b99f9e2a");
        final Element infoElement = new Element("info");
        writeline("/home/ubuntu/results/coverage/Project/Project_9_10.coverage", "42f2f21b-9d6a-4bf7-81b8-782310f29890");
        addProperty(infoElement, "projectname", name);
        writeline("/home/ubuntu/results/coverage/Project/Project_9_10.coverage", "48ef6f51-38be-41db-9bdb-583d9202f383");
        final String lastBuildString = DateUtil.getFormattedTime(lastBuild == null ? now : lastBuild);
        writeline("/home/ubuntu/results/coverage/Project/Project_9_10.coverage", "374423bb-8568-4028-8c2e-a763f8ec55a7");
        addProperty(infoElement, "lastbuild", lastBuildString);
        writeline("/home/ubuntu/results/coverage/Project/Project_9_10.coverage", "0c843e00-fa30-42f4-a606-ab1700f8b889");
        final String lastSuccessfulBuildString = DateUtil.getFormattedTime(lastSuccessfulBuild == null ? now : lastSuccessfulBuild);
        writeline("/home/ubuntu/results/coverage/Project/Project_9_10.coverage", "0b3f4140-30f6-4140-95b8-ef95578b07d8");
        addProperty(infoElement, "lastsuccessfulbuild", lastSuccessfulBuildString);
        writeline("/home/ubuntu/results/coverage/Project/Project_9_10.coverage", "acbc4b6c-73d7-483c-93c1-d0c1b96d4383");
        addProperty(infoElement, "builddate", DateUtil.formatIso8601(now));
        writeline("/home/ubuntu/results/coverage/Project/Project_9_10.coverage", "d807edb1-4147-4a99-bad6-4fb4ce7320a2");
        addProperty(infoElement, "cctimestamp", DateUtil.getFormattedTime(now));
        writeline("/home/ubuntu/results/coverage/Project/Project_9_10.coverage", "32935a4c-4d96-4e7c-b472-5209e9ebb9db");
        addProperty(infoElement, "label", label);
        writeline("/home/ubuntu/results/coverage/Project/Project_9_10.coverage", "671874d4-c5e2-47b6-842f-afd432d724a3");
        addProperty(infoElement, "interval", Long.toString(getBuildInterval() / 1000L));
        writeline("/home/ubuntu/results/coverage/Project/Project_9_10.coverage", "87457d89-94f8-45cb-8608-17e975e4f344");
        addProperty(infoElement, "lastbuildsuccessful", String.valueOf(wasLastBuildSuccessful));
        writeline("/home/ubuntu/results/coverage/Project/Project_9_10.coverage", "acfe60ac-b3b5-4291-9f1e-985583ce1226");
        return infoElement;
    }

    private void addProperty(final Element parent, final String key, final String value) {
        writeline("/home/ubuntu/results/coverage/Project/Project_9_10.coverage", "f0e44985-c65b-4ebe-87e3-4ec203318e30");
        final Element propertyElement = new Element("property");
        writeline("/home/ubuntu/results/coverage/Project/Project_9_10.coverage", "baec4657-79d4-4fb4-819b-242574000407");
        propertyElement.setAttribute("name", key);
        writeline("/home/ubuntu/results/coverage/Project/Project_9_10.coverage", "ec5754bf-9967-46ef-8f58-256c2521a096");
        propertyElement.setAttribute("value", value);
        writeline("/home/ubuntu/results/coverage/Project/Project_9_10.coverage", "d104dfdb-9539-49f9-8590-fdae2cae2f76");
        parent.addContent(propertyElement);
    }

    protected Map<String, String> getProjectPropertiesMap(final Date now) {
        writeline("/home/ubuntu/results/coverage/Project/Project_9_10.coverage", "1a0e5a7a-da99-40a2-8277-c70c8555bb2b");
        final Map<String, String> buildProperties = new HashMap<String, String>();
        writeline("/home/ubuntu/results/coverage/Project/Project_9_10.coverage", "667f835f-8478-4c64-8c8e-554a50259c41");
        buildProperties.put("projectname", name);
        writeline("/home/ubuntu/results/coverage/Project/Project_9_10.coverage", "fecd6875-8fe7-42e2-b6c5-942257c04161");
        buildProperties.put("label", label);
        writeline("/home/ubuntu/results/coverage/Project/Project_9_10.coverage", "056d5362-8f8e-41c4-8f00-11d387e0c59c");
        // TODO: Shouldn't have CVS specific properties here
        buildProperties.put("cvstimestamp", CVSDateUtil.formatCVSDate(now));
        writeline("/home/ubuntu/results/coverage/Project/Project_9_10.coverage", "ff9609e1-86eb-46e5-8093-a48bc693d9b5");
        buildProperties.put("cctimestamp", DateUtil.getFormattedTime(now));
        writeline("/home/ubuntu/results/coverage/Project/Project_9_10.coverage", "00b3b9ea-413f-4b19-8702-0e4ad56e0792");
        buildProperties.put("cclastgoodbuildtimestamp", getLastSuccessfulBuild());
        writeline("/home/ubuntu/results/coverage/Project/Project_9_10.coverage", "0dfcb41c-bcd3-482b-ba3a-4583d3517765");
        buildProperties.put("cclastbuildtimestamp", getLastBuild());
        writeline("/home/ubuntu/results/coverage/Project/Project_9_10.coverage", "97df844b-c46b-4cec-9b59-9597bbb96ad9");
        buildProperties.put("lastbuildsuccessful", String.valueOf(isLastBuildSuccessful()));
        writeline("/home/ubuntu/results/coverage/Project/Project_9_10.coverage", "738cc007-9069-498c-a48f-9a5c52e66dce");
        buildProperties.put("buildforced", String.valueOf(getBuildForced()));
        writeline("/home/ubuntu/results/coverage/Project/Project_9_10.coverage", "03ad3c51-343d-49a7-bfd7-807821ac3483");
        if (projectConfig.getModificationSet() != null) {
            writeline("/home/ubuntu/results/coverage/Project/Project_9_10.coverage", "4658e2f7-fcf0-4dc6-a883-06798b643ae5");
            buildProperties.putAll(projectConfig.getModificationSet().getProperties());
        }
        writeline("/home/ubuntu/results/coverage/Project/Project_9_10.coverage", "69295a9e-899c-4578-9c25-a865108bc9de");
        if (additionalProperties != null && !additionalProperties.isEmpty()) {
            writeline("/home/ubuntu/results/coverage/Project/Project_9_10.coverage", "52efc3e5-3074-4e64-99aa-a9f73120db10");
            buildProperties.putAll(additionalProperties);
            writeline("/home/ubuntu/results/coverage/Project/Project_9_10.coverage", "827a2e4e-5c80-4751-9ef3-95e784c17339");
            additionalProperties.clear();
            writeline("/home/ubuntu/results/coverage/Project/Project_9_10.coverage", "03436a17-d725-4b8e-aac9-061e2cfd76f3");
            additionalProperties = null;
        }
        writeline("/home/ubuntu/results/coverage/Project/Project_9_10.coverage", "1f60fd24-e72e-472e-ba53-8a6f15beab39");
        return buildProperties;
    }

    /**
     * Intended only for unit testing.
     * @return additional Properties variable.
     */
    Map<String, String> getAdditionalProperties() {
        writeline("/home/ubuntu/results/coverage/Project/Project_9_10.coverage", "bc6710c8-c8c9-4ef1-ba70-ac2da157c1ad");
        return additionalProperties;
    }

    /**
     * Iterate over all of the registered <code>Publisher</code>s and call
     * their respective <code>publish</code> methods.
     * @param buildLog the content to publish
     * @throws CruiseControlException if an error occurs during publishing
     */
    protected void publish(final Log buildLog) throws CruiseControlException {
        writeline("/home/ubuntu/results/coverage/Project/Project_9_10.coverage", "6f5654a6-76c9-4dee-aff6-e0d3e2d73e1b");
        setState(ProjectState.PUBLISHING);
        writeline("/home/ubuntu/results/coverage/Project/Project_9_10.coverage", "8648f763-07a4-46ad-aff6-7dff977880eb");
        for (final Publisher publisher : projectConfig.getPublishers()) {
            writeline("/home/ubuntu/results/coverage/Project/Project_9_10.coverage", "5a33e9de-7053-4b96-b229-0173517911ec");
            // catch all errors, Publishers shouldn't cause failures in the build method
            try {
                writeline("/home/ubuntu/results/coverage/Project/Project_9_10.coverage", "da564d65-01f4-477c-a31a-ca294a463d5a");
                publisher.publish(buildLog.getContent());
            } catch (Throwable t) {
                writeline("/home/ubuntu/results/coverage/Project/Project_9_10.coverage", "17813298-f293-4c83-8b76-41bc22592b72");
                final StringBuilder message = new StringBuilder("exception publishing results");
                writeline("/home/ubuntu/results/coverage/Project/Project_9_10.coverage", "1592b51c-c168-4011-9977-6b02bde43cdf");
                message.append(" with ").append(publisher.getClass().getName());
                writeline("/home/ubuntu/results/coverage/Project/Project_9_10.coverage", "4a1f6187-be18-4a6a-9148-64ab3a2387f5");
                message.append(" for project ").append(name);
                writeline("/home/ubuntu/results/coverage/Project/Project_9_10.coverage", "62cc11c2-be57-45a0-8b41-434a4465cd46");
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
        writeline("/home/ubuntu/results/coverage/Project/Project_9_10.coverage", "3c6415f6-fe98-49cd-b326-3f2d963a5a3b");
        setState(ProjectState.BOOTSTRAPPING);
        writeline("/home/ubuntu/results/coverage/Project/Project_9_10.coverage", "3f7a3f02-3744-4f8d-99a8-a3cff94edfa7");
        for (final Bootstrapper bootstrapper : projectConfig.getBootstrappers()) {
            writeline("/home/ubuntu/results/coverage/Project/Project_9_10.coverage", "b30be790-7ad5-4a9b-840b-f577d952a165");
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
        writeline("/home/ubuntu/results/coverage/Project/Project_9_10.coverage", "012c4154-a63f-4dc2-b62d-11d2b31df3f0");
        if (!incrementer.isValidLabel(oldLabel)) {
            writeline("/home/ubuntu/results/coverage/Project/Project_9_10.coverage", "8aebd73e-1e55-4481-a891-c9208a5c1039");
            final String message = oldLabel + " is not a valid label for labelIncrementer " + incrementer.getClass().getName();
            writeline("/home/ubuntu/results/coverage/Project/Project_9_10.coverage", "3e1ca7fd-44fb-4ceb-bf83-8786f113a4a8");
            debug(message);
            writeline("/home/ubuntu/results/coverage/Project/Project_9_10.coverage", "1f89ba35-cc83-45e2-8c78-b71b6630e077");
            throw new CruiseControlException(message);
        }
    }

    public boolean isLastBuildSuccessful() {
        writeline("/home/ubuntu/results/coverage/Project/Project_9_10.coverage", "068da69e-6e6c-40ff-8904-060c2cced04b");
        return wasLastBuildSuccessful;
    }

    void setWasLastBuildSuccessful(final boolean buildSuccessful) {
        writeline("/home/ubuntu/results/coverage/Project/Project_9_10.coverage", "a04ac1ab-48ca-4c7b-87e0-0fa5b3e8e9de");
        wasLastBuildSuccessful = buildSuccessful;
    }

    /**
     * Logs a message to the application log, not to be confused with the
     * CruiseControl build log.
     * @param message the application message to log
     */
    private void info(final String message) {
        writeline("/home/ubuntu/results/coverage/Project/Project_9_10.coverage", "69933786-a294-4196-bc08-0a04aa0b5703");
        LOG.info("Project " + name + ":  " + message);
    }

    private void debug(final String message) {
        writeline("/home/ubuntu/results/coverage/Project/Project_9_10.coverage", "50a2f965-def6-4db8-87f9-0d56c0fddc99");
        LOG.debug("Project " + name + ":  " + message);
    }

    public void start() {
        writeline("/home/ubuntu/results/coverage/Project/Project_9_10.coverage", "546a7401-f683-426b-bcd5-432f2be34800");
        if (stopped || getState() == ProjectState.STOPPED) {
            writeline("/home/ubuntu/results/coverage/Project/Project_9_10.coverage", "e7092524-edd4-46e8-ab6a-bd68866d3435");
            stopped = false;
            writeline("/home/ubuntu/results/coverage/Project/Project_9_10.coverage", "b1f7906b-70f6-4904-94e2-1ab3655ff5b4");
            LOG.info("Project " + name + " starting");
            writeline("/home/ubuntu/results/coverage/Project/Project_9_10.coverage", "fb8326ff-62a2-4609-becd-b69c039c1727");
            setState(ProjectState.IDLE);
            writeline("/home/ubuntu/results/coverage/Project/Project_9_10.coverage", "4597ef29-2000-485e-9a13-d43f81ac769f");
            createNewSchedulingThread();
        }
    }

    protected void createNewSchedulingThread() {
        writeline("/home/ubuntu/results/coverage/Project/Project_9_10.coverage", "3f4c6627-0958-467f-bc37-230a982a2bd4");
        final Thread projectSchedulingThread = new Thread(this, "Project " + getName() + " thread");
        writeline("/home/ubuntu/results/coverage/Project/Project_9_10.coverage", "51166d1e-2f15-4928-a711-707a5295cf1f");
        projectSchedulingThread.start();
        writeline("/home/ubuntu/results/coverage/Project/Project_9_10.coverage", "ba5cf851-b172-4104-ade0-f50c922bee12");
        // brief nap to allow thread to start
        try {
            writeline("/home/ubuntu/results/coverage/Project/Project_9_10.coverage", "3ba1851d-fb6e-4943-a64a-7c6e753e86e7");
            Thread.sleep(100);
        } catch (InterruptedException ie) {
            writeline("/home/ubuntu/results/coverage/Project/Project_9_10.coverage", "8391de0f-3067-49bb-9523-f897c43602de");
            LOG.warn("interrupted while waiting for scheduling thread to start", ie);
        }
    }

    public void stop() {
        writeline("/home/ubuntu/results/coverage/Project/Project_9_10.coverage", "d637dcb8-0981-486d-aae9-be7ca8207636");
        LOG.info("Project " + name + " stopping");
        writeline("/home/ubuntu/results/coverage/Project/Project_9_10.coverage", "507ca940-7b05-4807-8699-9cfc8825fe20");
        stopped = true;
        writeline("/home/ubuntu/results/coverage/Project/Project_9_10.coverage", "a89e0df1-8417-4515-a53f-e7141892bf8d");
        setState(ProjectState.STOPPED);
        writeline("/home/ubuntu/results/coverage/Project/Project_9_10.coverage", "684531a7-9a4e-4e48-a14c-cb61dadd9f95");
        synchronized (pausedMutex) {
            writeline("/home/ubuntu/results/coverage/Project/Project_9_10.coverage", "43dc17be-a07a-433e-afd1-ec9da572e56e");
            pausedMutex.notifyAll();
        }
        writeline("/home/ubuntu/results/coverage/Project/Project_9_10.coverage", "da6931bc-3ee4-4252-9bee-7dc61113cf9c");
        synchronized (waitMutex) {
            writeline("/home/ubuntu/results/coverage/Project/Project_9_10.coverage", "0a552a1a-a93b-4280-a0be-2eb9ed3b0727");
            waitMutex.notifyAll();
        }
        writeline("/home/ubuntu/results/coverage/Project/Project_9_10.coverage", "07d29705-7762-42dd-9b95-df134e417bb7");
        synchronized (scheduleMutex) {
            writeline("/home/ubuntu/results/coverage/Project/Project_9_10.coverage", "e70b5a2c-e34a-42f8-8dbf-d6888349871a");
            scheduleMutex.notifyAll();
        }
    }

    public String toString() {
        writeline("/home/ubuntu/results/coverage/Project/Project_9_10.coverage", "76eb24b7-996f-4f54-8518-90c17119eea9");
        final StringBuilder sb = new StringBuilder("Project ");
        writeline("/home/ubuntu/results/coverage/Project/Project_9_10.coverage", "1809b4dd-886a-469a-9cd9-64e4f36efaff");
        sb.append(getName());
        writeline("/home/ubuntu/results/coverage/Project/Project_9_10.coverage", "a0af4247-d8b3-4248-adbe-78b26c661ccf");
        sb.append(": ");
        writeline("/home/ubuntu/results/coverage/Project/Project_9_10.coverage", "72c41148-03dd-4847-bcb5-63391359c82b");
        sb.append(getStatus());
        writeline("/home/ubuntu/results/coverage/Project/Project_9_10.coverage", "b3b425cb-6336-4fa8-80be-95e8b8bd4ad2");
        if (isPaused) {
            writeline("/home/ubuntu/results/coverage/Project/Project_9_10.coverage", "966a2500-ea61-4b8a-b97d-450593e7abe5");
            sb.append(" (paused)");
        }
        writeline("/home/ubuntu/results/coverage/Project/Project_9_10.coverage", "1766f843-7e2c-4446-b52c-5a033731124a");
        return sb.toString();
    }

    public void addBuildProgressListener(final BuildProgressListener listener) {
        writeline("/home/ubuntu/results/coverage/Project/Project_9_10.coverage", "4e522d56-1c22-4b36-b755-83ae1e73935b");
        synchronized (progressListeners) {
            writeline("/home/ubuntu/results/coverage/Project/Project_9_10.coverage", "9c98117d-1e68-470b-8cc8-d40442761eed");
            progressListeners.add(listener);
        }
    }

    protected void fireProgressEvent(final BuildProgressEvent event) {
        writeline("/home/ubuntu/results/coverage/Project/Project_9_10.coverage", "6fcf937d-6959-440c-b378-d5edfc3c7b5a");
        synchronized (progressListeners) {
            writeline("/home/ubuntu/results/coverage/Project/Project_9_10.coverage", "366c249c-ae36-427b-9100-335df416ba5f");
            for (final BuildProgressListener listener : progressListeners) {
                writeline("/home/ubuntu/results/coverage/Project/Project_9_10.coverage", "7c25cbbe-5ecb-494d-98ed-5129fd963423");
                listener.handleBuildProgress(event);
            }
        }
    }

    public void addBuildResultListener(final BuildResultListener listener) {
        writeline("/home/ubuntu/results/coverage/Project/Project_9_10.coverage", "b4a44103-b278-4c08-9f3b-2e47cdac7753");
        synchronized (resultListeners) {
            writeline("/home/ubuntu/results/coverage/Project/Project_9_10.coverage", "ec305155-a9b5-4749-93e9-4a0c74b8231c");
            resultListeners.add(listener);
        }
    }

    protected void fireResultEvent(final BuildResultEvent event) {
        writeline("/home/ubuntu/results/coverage/Project/Project_9_10.coverage", "abcdcf61-f410-4800-8916-1ab56a2554a8");
        synchronized (resultListeners) {
            writeline("/home/ubuntu/results/coverage/Project/Project_9_10.coverage", "982419b3-0f84-43f0-af47-3111ad6bacd6");
            for (final BuildResultListener listener : resultListeners) {
                writeline("/home/ubuntu/results/coverage/Project/Project_9_10.coverage", "a97d3cc4-b92a-4606-bfba-643b05397434");
                listener.handleBuildResult(event);
            }
        }
    }

    List<Listener> getListeners() {
        writeline("/home/ubuntu/results/coverage/Project/Project_9_10.coverage", "3b2da201-1f24-4ac8-b94b-e029f00538a4");
        return projectConfig.getListeners();
    }

    public void setProjectConfig(final ProjectConfig projectConfig) throws CruiseControlException {
        writeline("/home/ubuntu/results/coverage/Project/Project_9_10.coverage", "215f64d5-5601-447e-923d-a3b6f5c4a6a4");
        if (projectConfig == null) {
            writeline("/home/ubuntu/results/coverage/Project/Project_9_10.coverage", "089f483b-b725-4bbb-be00-9294068e77a5");
            throw new IllegalArgumentException("project config can't be null");
        }
        writeline("/home/ubuntu/results/coverage/Project/Project_9_10.coverage", "b11427fd-05f1-48a1-b6e5-78b4b656c67e");
        this.projectConfig = projectConfig;
        writeline("/home/ubuntu/results/coverage/Project/Project_9_10.coverage", "3d169f35-adb3-43b6-be7d-33862a405fc0");
        setLabelIncrementer(projectConfig.getLabelIncrementer());
    }

    void notifyListeners(final ProjectEvent event) {
        writeline("/home/ubuntu/results/coverage/Project/Project_9_10.coverage", "7053e041-f5cf-48e8-bcc5-ee854305dbd7");
        if (projectConfig == null) {
            writeline("/home/ubuntu/results/coverage/Project/Project_9_10.coverage", "f1c20dd6-d188-40da-95c3-4e6aebf76e8b");
            throw new IllegalStateException("projectConfig is null");
        }
        writeline("/home/ubuntu/results/coverage/Project/Project_9_10.coverage", "cbd01d79-cd82-4fa2-83df-b7a210987df0");
        for (final Listener listener : projectConfig.getListeners()) {
            writeline("/home/ubuntu/results/coverage/Project/Project_9_10.coverage", "2da0b353-1609-45c7-8557-efb50958f1a3");
            try {
                writeline("/home/ubuntu/results/coverage/Project/Project_9_10.coverage", "e13513db-f1d2-4c8f-b2cf-a9d076534077");
                listener.handleEvent(event);
            } catch (CruiseControlException e) {
                writeline("/home/ubuntu/results/coverage/Project/Project_9_10.coverage", "f6db122b-50a2-44f6-a178-f9fb96c6e0cb");
                final StringBuilder message = new StringBuilder("exception notifying listener ");
                writeline("/home/ubuntu/results/coverage/Project/Project_9_10.coverage", "19ab8b52-a512-48ff-9b4a-8533bd4c3013");
                message.append(listener.getClass().getName());
                writeline("/home/ubuntu/results/coverage/Project/Project_9_10.coverage", "b1a82a1e-c9ab-4fc4-af86-0f0a2f6f6098");
                message.append(" for project ").append(name);
                writeline("/home/ubuntu/results/coverage/Project/Project_9_10.coverage", "16b4d9bd-79c3-46db-8cd2-40107cc1067b");
                LOG.error(message.toString(), e);
            }
        }
    }

    public boolean equals(final Object arg0) {
        writeline("/home/ubuntu/results/coverage/Project/Project_9_10.coverage", "8ca9d37a-fd67-4acb-a376-0b822041ad50");
        if (arg0 == null) {
            writeline("/home/ubuntu/results/coverage/Project/Project_9_10.coverage", "98124a4f-4717-4d85-b04d-c916666e8db8");
            return false;
        }
        writeline("/home/ubuntu/results/coverage/Project/Project_9_10.coverage", "d33c5a08-cfea-4f69-ae88-8489221486b2");
        if (arg0.getClass().getName().equals(getClass().getName())) {
            writeline("/home/ubuntu/results/coverage/Project/Project_9_10.coverage", "f5ecd038-ead3-47d2-869b-23e627d53475");
            final Project thatProject = (Project) arg0;
            writeline("/home/ubuntu/results/coverage/Project/Project_9_10.coverage", "15e74275-d77b-4298-994d-38897dc4e61a");
            return thatProject.name.equals(name);
        }
        writeline("/home/ubuntu/results/coverage/Project/Project_9_10.coverage", "5774f8f2-d03b-45c7-a16a-20d342aaff01");
        return false;
    }

    public int hashCode() {
        writeline("/home/ubuntu/results/coverage/Project/Project_9_10.coverage", "4b09c9ef-61f6-49eb-b506-53b9bf06de03");
        return name.hashCode();
    }

    public void register(final MBeanServer server) throws JMException {
        writeline("/home/ubuntu/results/coverage/Project/Project_9_10.coverage", "54a73089-3a22-4d97-93c6-6898a2000e10");
        LOG.debug("Registering project mbean");
        writeline("/home/ubuntu/results/coverage/Project/Project_9_10.coverage", "db6f9a35-f8be-401d-bd54-35d3e4feb554");
        final ProjectController projectController = new ProjectController(this);
        writeline("/home/ubuntu/results/coverage/Project/Project_9_10.coverage", "1a963552-2dd5-4702-8dcf-6f21915a1155");
        projectController.register(server);
    }

    public ProjectConfig getProjectConfig() {
        writeline("/home/ubuntu/results/coverage/Project/Project_9_10.coverage", "bb27b6b4-1c37-415c-b548-9c9ee92bd0bc");
        return projectConfig;
    }

    public Date getLastBuildDate() {
        writeline("/home/ubuntu/results/coverage/Project/Project_9_10.coverage", "e69fee03-03af-4b86-9847-30384f1bc10c");
        return lastBuild;
    }

    public Progress getProgress() {
        writeline("/home/ubuntu/results/coverage/Project/Project_9_10.coverage", "5363f09d-0497-48b9-af14-b248eb66bab9");
        return progress;
    }

    public List<String> getLogLabels() {
        writeline("/home/ubuntu/results/coverage/Project/Project_9_10.coverage", "eeb96a86-ac96-47b8-bb5a-b9da45d2add2");
        return projectConfig.getLogLabels();
    }

    public String[] getLogLabelLines(final String logLabel, final int firstLine) {
        writeline("/home/ubuntu/results/coverage/Project/Project_9_10.coverage", "b9ee7ff9-43ef-4d61-9448-c3730e0525f8");
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
