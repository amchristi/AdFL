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
        writeline("/home/ubuntu/results/coverage/Project/Project_10_10.coverage", "a6e39c0a-eeee-4364-a25d-b63a3385218c");
        state = ProjectState.STOPPED;
        writeline("/home/ubuntu/results/coverage/Project/Project_10_10.coverage", "60875d4c-6c12-4345-b47e-b3e3f63d3d23");
        pausedMutex = new Object();
        writeline("/home/ubuntu/results/coverage/Project/Project_10_10.coverage", "d697ac3b-e1e7-495c-a4c1-9159d7f4fcae");
        scheduleMutex = new Object();
        writeline("/home/ubuntu/results/coverage/Project/Project_10_10.coverage", "ea2934ed-8646-459b-9867-004df79717be");
        waitMutex = new Object();
        writeline("/home/ubuntu/results/coverage/Project/Project_10_10.coverage", "d2cca0df-65f6-445a-8836-66f8756d45e3");
        progressListeners = new ArrayList<BuildProgressListener>();
        writeline("/home/ubuntu/results/coverage/Project/Project_10_10.coverage", "0d23bc9a-2dea-406e-a9df-10e14c677e20");
        resultListeners = new ArrayList<BuildResultListener>();
        writeline("/home/ubuntu/results/coverage/Project/Project_10_10.coverage", "6657d663-e873-4e5a-bd45-6e87d0cb408b");
        progress = new ProgressImpl(this);
    }

    private void readObject(final ObjectInputStream stream) throws IOException, ClassNotFoundException {
        writeline("/home/ubuntu/results/coverage/Project/Project_10_10.coverage", "1e5f07e6-ed2d-4e72-9e77-f55d37b06967");
        stream.defaultReadObject();
        writeline("/home/ubuntu/results/coverage/Project/Project_10_10.coverage", "3be22225-ab88-42af-994c-c90f0e841e14");
        initializeTransientFields();
    }

    public void execute() {
        writeline("/home/ubuntu/results/coverage/Project/Project_10_10.coverage", "9023418d-2920-41b0-abed-821babed5a19");
        if (stopped) {
            writeline("/home/ubuntu/results/coverage/Project/Project_10_10.coverage", "017d6693-5598-474b-ae75-d8727969eb48");
            LOG.warn("not building project " + name + " because project has been stopped.");
            writeline("/home/ubuntu/results/coverage/Project/Project_10_10.coverage", "f8a13edd-4323-4a37-aa6c-649ab4e0248a");
            buildFinished();
            writeline("/home/ubuntu/results/coverage/Project/Project_10_10.coverage", "6cacd496-a6fb-4204-9357-91ec808ebe77");
            return;
        }
        writeline("/home/ubuntu/results/coverage/Project/Project_10_10.coverage", "87ebeb0c-e81e-4eb4-9fa4-c2e160227906");
        synchronized (pausedMutex) {
            writeline("/home/ubuntu/results/coverage/Project/Project_10_10.coverage", "1aae2f35-a959-4498-9992-98140282b6b9");
            if (isPaused) {
                writeline("/home/ubuntu/results/coverage/Project/Project_10_10.coverage", "8c35293d-1c92-40d4-87a7-e99678f70fb8");
                LOG.info("not building project " + name + " because project has been paused.");
                writeline("/home/ubuntu/results/coverage/Project/Project_10_10.coverage", "f42c1056-2607-4532-b6f0-c7b0183ba413");
                buildFinished();
                writeline("/home/ubuntu/results/coverage/Project/Project_10_10.coverage", "5a0e2b7f-3b44-4ab3-a1bc-36a2656bf3f1");
                return;
            }
        }
        writeline("/home/ubuntu/results/coverage/Project/Project_10_10.coverage", "588b128f-b710-4150-b3dd-151df1960309");
        try {
            writeline("/home/ubuntu/results/coverage/Project/Project_10_10.coverage", "9fc62f92-555a-4a07-8bc8-6dc0603c3c26");
            init();
            writeline("/home/ubuntu/results/coverage/Project/Project_10_10.coverage", "b84054cb-58a5-4549-b4ae-49679023fa56");
            build();
        } catch (CruiseControlException e) {
            writeline("/home/ubuntu/results/coverage/Project/Project_10_10.coverage", "bd66ca60-3e86-4a05-bc86-a5b6296615c2");
            LOG.error("exception attempting build in project " + name, e);
        } finally {
            writeline("/home/ubuntu/results/coverage/Project/Project_10_10.coverage", "366bd148-bddf-404d-9b4f-ae7bf171f4f0");
            buildFinished();
        }
    }

    /**
     * Unless paused, runs any bootstrappers and then the entire build.
     * @throws CruiseControlException if an error occurs during the build
     */
    protected void build() throws CruiseControlException {
        writeline("/home/ubuntu/results/coverage/Project/Project_10_10.coverage", "65ded417-dec4-4527-af7a-b4f66bf70933");
        if (projectConfig == null) {
            writeline("/home/ubuntu/results/coverage/Project/Project_10_10.coverage", "c543c3fb-8034-49c3-b98e-a20bcffbf429");
            throw new IllegalStateException("projectConfig must be set on project before calling build()");
        }
        writeline("/home/ubuntu/results/coverage/Project/Project_10_10.coverage", "772ae18a-5945-47ac-9ab3-e6a1fe7582a9");
        if (stopped) {
            writeline("/home/ubuntu/results/coverage/Project/Project_10_10.coverage", "13d277c4-3d76-4c93-9d9b-24ddfd826e92");
            LOG.warn("not building project " + name + " because project has been stopped.");
            writeline("/home/ubuntu/results/coverage/Project/Project_10_10.coverage", "ed986613-60ef-4a06-abec-c88ecf2d3047");
            return;
        }
        writeline("/home/ubuntu/results/coverage/Project/Project_10_10.coverage", "9f3e1b26-4528-43dc-b933-f86d2708ec7a");
        // or if the last build faild and we want to build on failures
        if (forceOnly && !buildForced && !(!wasLastBuildSuccessful && buildAfterFailed)) {
            writeline("/home/ubuntu/results/coverage/Project/Project_10_10.coverage", "de52e1fb-787f-4c5d-9273-33ce24102960");
            info("not building because project is forceOnly and build not forced.");
            writeline("/home/ubuntu/results/coverage/Project/Project_10_10.coverage", "6b5ac024-5100-46c1-97bf-674dffa21ec4");
            return;
        }
        writeline("/home/ubuntu/results/coverage/Project/Project_10_10.coverage", "35a447c4-4563-4b0b-9ef8-4aa8cc039bb5");
        final boolean buildWasForced = buildForced;
        writeline("/home/ubuntu/results/coverage/Project/Project_10_10.coverage", "b65df18f-0274-4e8a-b283-23afca81683a");
        try {
            writeline("/home/ubuntu/results/coverage/Project/Project_10_10.coverage", "1b7d85c9-b2d9-4ddf-bfb9-2c89b703eaee");
            setBuildStartTime(new Date());
            writeline("/home/ubuntu/results/coverage/Project/Project_10_10.coverage", "2579bc2f-9092-46f9-87c5-80451a548aa0");
            final Schedule schedule = projectConfig.getSchedule();
            writeline("/home/ubuntu/results/coverage/Project/Project_10_10.coverage", "8055bbdb-1c16-4579-b815-fa75d03c8f1d");
            if (schedule == null) {
                writeline("/home/ubuntu/results/coverage/Project/Project_10_10.coverage", "41b00b98-3c42-42d5-a4c1-cb614b3e390b");
                throw new IllegalStateException("project must have a schedule");
            }
            writeline("/home/ubuntu/results/coverage/Project/Project_10_10.coverage", "b9b46d60-edc2-4edd-b399-402c19101d37");
            if (schedule.isPaused(buildStartTime)) {
                writeline("/home/ubuntu/results/coverage/Project/Project_10_10.coverage", "e8838949-53d8-4580-8200-ba465ab9d4ef");
                // is different than ProjectState.PAUSED
                return;
            }
            writeline("/home/ubuntu/results/coverage/Project/Project_10_10.coverage", "52dda81b-fa17-4539-b807-e622f9bfe762");
            // @todo Add Progress param to Bootstrapper API?
            bootstrap();
            writeline("/home/ubuntu/results/coverage/Project/Project_10_10.coverage", "22162ecd-b2f2-49cf-9813-fad187a3c94f");
            final String target = useAndResetBuildTargetIfBuildWasForced(buildWasForced);
            writeline("/home/ubuntu/results/coverage/Project/Project_10_10.coverage", "6918467b-67c9-4b0f-95ba-629b86cea0ef");
            // @todo Add Progress param to ModificationSet API?
            // getModifications will only return null if we don't need to build
            final Element modifications = getModifications(buildWasForced);
            writeline("/home/ubuntu/results/coverage/Project/Project_10_10.coverage", "e309aafe-cbfe-4664-a81f-0dc6380aa166");
            if (modifications == null) {
                writeline("/home/ubuntu/results/coverage/Project/Project_10_10.coverage", "b14c9b33-2041-4576-867c-f8c78eabc997");
                return;
            }
            writeline("/home/ubuntu/results/coverage/Project/Project_10_10.coverage", "4c97e75c-6d8a-4530-b024-3f3ce58e1c59");
            // Using local reference to avoid NPE if config.xml is updated during build
            final Log buildLog = projectConfig.getLog();
            writeline("/home/ubuntu/results/coverage/Project/Project_10_10.coverage", "e571432c-4e4e-4b5f-86c0-4fbf0824c028");
            buildLog.addContent(modifications);
            writeline("/home/ubuntu/results/coverage/Project/Project_10_10.coverage", "86e58f34-63c8-43f2-bd76-87fc2f97bcbf");
            final Date now;
            writeline("/home/ubuntu/results/coverage/Project/Project_10_10.coverage", "1e55ad34-e13e-4d23-aaf2-b80e0077c0bd");
            if (projectConfig.getModificationSet() != null && projectConfig.getModificationSet().getTimeOfCheck() != null) {
                writeline("/home/ubuntu/results/coverage/Project/Project_10_10.coverage", "2c4421e9-2c8d-4a4c-845e-1e2943ecb607");
                now = projectConfig.getModificationSet().getTimeOfCheck();
            } else {
                writeline("/home/ubuntu/results/coverage/Project/Project_10_10.coverage", "18c1127e-ab0c-48dc-a43c-148df8c2effc");
                now = new Date();
            }
            writeline("/home/ubuntu/results/coverage/Project/Project_10_10.coverage", "0cb80120-3fe3-4130-9537-e1ece6f40159");
            if (getLabelIncrementer().isPreBuildIncrementer()) {
                writeline("/home/ubuntu/results/coverage/Project/Project_10_10.coverage", "b8414200-9638-455f-ab8c-6c493d9844b0");
                label = getLabelIncrementer().incrementLabel(label, buildLog.getContent());
            }
            writeline("/home/ubuntu/results/coverage/Project/Project_10_10.coverage", "b257bc65-cca0-4fbb-82da-7d5aba3baf29");
            // collect project information
            buildLog.addContent(getProjectPropertiesElement(now));
            writeline("/home/ubuntu/results/coverage/Project/Project_10_10.coverage", "c8a6769f-e1e3-451a-9b3b-9c06447c1b30");
            setState(ProjectState.BUILDING);
            writeline("/home/ubuntu/results/coverage/Project/Project_10_10.coverage", "c800e1a3-ee0f-4556-ab02-a80aecf060eb");
            final Element builderLog = schedule.build(buildCounter, lastBuild, now, getProjectPropertiesMap(now), target, progress);
            writeline("/home/ubuntu/results/coverage/Project/Project_10_10.coverage", "dc35b5f2-0634-46a9-948a-e80b21b26f5c");
            buildLog.addContent(builderLog.detach());
            writeline("/home/ubuntu/results/coverage/Project/Project_10_10.coverage", "92d023e1-17b6-4dec-b39a-044f5c84a7aa");
            boolean buildSuccessful = buildLog.wasBuildSuccessful();
            writeline("/home/ubuntu/results/coverage/Project/Project_10_10.coverage", "116c35ea-9f98-4bb5-811f-6adb74f8de65");
            fireResultEvent(new BuildResultEvent(this, buildSuccessful));
            writeline("/home/ubuntu/results/coverage/Project/Project_10_10.coverage", "192f2ec7-4d9f-4dfe-aeab-63ca08d1b046");
            if (!getLabelIncrementer().isPreBuildIncrementer() && buildSuccessful) {
                writeline("/home/ubuntu/results/coverage/Project/Project_10_10.coverage", "c1dd54d0-07be-4afa-91f6-cddad7b46243");
                label = getLabelIncrementer().incrementLabel(label, buildLog.getContent());
            }
            writeline("/home/ubuntu/results/coverage/Project/Project_10_10.coverage", "b339ac70-a6ac-4350-b8a3-7a7776c84401");
            setState(ProjectState.MERGING_LOGS);
            writeline("/home/ubuntu/results/coverage/Project/Project_10_10.coverage", "5e8faa54-2701-40dd-a003-2f70e77419fc");
            buildLog.writeLogFile(now);
            writeline("/home/ubuntu/results/coverage/Project/Project_10_10.coverage", "f128576d-9538-4de5-94c6-9d8eb53e7758");
            // regardless of success or failure (buildAfterFailed = false in config.xml)
            if (!buildAfterFailed) {
                writeline("/home/ubuntu/results/coverage/Project/Project_10_10.coverage", "8f73dd6a-e206-46ea-9e7d-bedc9a7b3e0a");
                lastBuild = now;
            }
            writeline("/home/ubuntu/results/coverage/Project/Project_10_10.coverage", "327b7b6f-fe01-407a-8a49-f92f9187e687");
            // If this was a successful build, update both last build and last successful build
            if (buildSuccessful) {
                writeline("/home/ubuntu/results/coverage/Project/Project_10_10.coverage", "cac92773-6051-4450-924b-8696da91e8f4");
                lastBuild = now;
                writeline("/home/ubuntu/results/coverage/Project/Project_10_10.coverage", "89c5b1c7-ea68-43a4-93ed-cadecf27b55b");
                lastSuccessfulBuild = now;
                writeline("/home/ubuntu/results/coverage/Project/Project_10_10.coverage", "61b82d37-816b-47a5-b639-d193ec7e855d");
                info("build successful");
            } else {
                writeline("/home/ubuntu/results/coverage/Project/Project_10_10.coverage", "52a1123a-c1cc-43f8-8512-0e4e745604bf");
                info("build failed");
            }
            writeline("/home/ubuntu/results/coverage/Project/Project_10_10.coverage", "09b6f8e7-710d-470a-84b8-e591ac4cb63a");
            buildCounter++;
            writeline("/home/ubuntu/results/coverage/Project/Project_10_10.coverage", "cec202ea-e8bd-43c0-b910-d2b7fcd1d6a0");
            setWasLastBuildSuccessful(buildSuccessful);
            writeline("/home/ubuntu/results/coverage/Project/Project_10_10.coverage", "82db2396-fc3d-4dc1-b027-2ce4b7ae0013");
            // also need to reset forced flag before serializing, unless buildForced var is transient
            // resetBuildForcedOnlyIfBuildWasForced(buildWasForced);
            serializeProject();
            writeline("/home/ubuntu/results/coverage/Project/Project_10_10.coverage", "6c89ab0e-cbc6-4d9b-a5b1-e2a0348fc63f");
            // @todo Add Progress param to Publisher API?
            publish(buildLog);
            writeline("/home/ubuntu/results/coverage/Project/Project_10_10.coverage", "fa003f4d-4ab8-4807-9ef1-230f436d7ce3");
            buildLog.reset();
        } finally {
            writeline("/home/ubuntu/results/coverage/Project/Project_10_10.coverage", "d9ca9715-cecf-4cf6-896d-0c5975a03029");
            resetBuildForcedOnlyIfBuildWasForced(buildWasForced);
            writeline("/home/ubuntu/results/coverage/Project/Project_10_10.coverage", "6f865b5b-d3ac-44f9-b10d-0662746ce794");
            setState(ProjectState.IDLE);
        }
    }

    private String useAndResetBuildTargetIfBuildWasForced(final boolean buildWasForced) {
        writeline("/home/ubuntu/results/coverage/Project/Project_10_10.coverage", "ee91d9cf-eb85-4072-a98f-18566fe9eefe");
        String target = null;
        writeline("/home/ubuntu/results/coverage/Project/Project_10_10.coverage", "0e50b5a3-db2e-476b-8057-9430ba4e549e");
        if (buildWasForced) {
            writeline("/home/ubuntu/results/coverage/Project/Project_10_10.coverage", "a096aae3-77fe-49c2-9678-b5b2eaee70be");
            target = buildTarget;
            writeline("/home/ubuntu/results/coverage/Project/Project_10_10.coverage", "aebad27d-93e3-43e9-91ae-f834ec513d2c");
            buildTarget = null;
        }
        writeline("/home/ubuntu/results/coverage/Project/Project_10_10.coverage", "f1e87841-d9e4-4d8c-8b84-fc5601813180");
        return target;
    }

    private void resetBuildForcedOnlyIfBuildWasForced(final boolean buildWasForced) {
        writeline("/home/ubuntu/results/coverage/Project/Project_10_10.coverage", "e7942991-9858-4c99-9469-c05cbc82d69e");
        if (buildWasForced) {
            writeline("/home/ubuntu/results/coverage/Project/Project_10_10.coverage", "b01f2d90-80f4-4eee-8f96-a6f897af86fc");
            buildForced = false;
        }
    }

    void setBuildStartTime(final Date date) {
        writeline("/home/ubuntu/results/coverage/Project/Project_10_10.coverage", "989c8e3d-ded3-4ec0-8154-1046aaba7929");
        buildStartTime = date;
    }

    public void run() {
        writeline("/home/ubuntu/results/coverage/Project/Project_10_10.coverage", "57c5ea23-938d-4bba-b1c8-e0c47c3b6a18");
        LOG.info("Project " + name + " started");
        writeline("/home/ubuntu/results/coverage/Project/Project_10_10.coverage", "8570f0ce-fa86-4d76-b766-bc713de52d31");
        try {
            writeline("/home/ubuntu/results/coverage/Project/Project_10_10.coverage", "b8f2c489-5297-40eb-b07e-ca9d343e1c50");
            while (!stopped) {
                writeline("/home/ubuntu/results/coverage/Project/Project_10_10.coverage", "a61d1ee3-a002-4734-b01f-5ad28daf7641");
                try {
                    writeline("/home/ubuntu/results/coverage/Project/Project_10_10.coverage", "457ba25c-d344-4152-b9a8-c4324a3d8e35");
                    waitIfPaused();
                    writeline("/home/ubuntu/results/coverage/Project/Project_10_10.coverage", "ddc23f84-e4e1-4a65-a9d3-ec01cbf5655c");
                    if (!stopped) {
                        writeline("/home/ubuntu/results/coverage/Project/Project_10_10.coverage", "9e3b1a86-d7cb-47ba-a4eb-df3af7db44db");
                        waitForNextBuild();
                    }
                    writeline("/home/ubuntu/results/coverage/Project/Project_10_10.coverage", "8a691dbe-76dc-404b-bf83-b64a8ddf390f");
                    if (!stopped) {
                        writeline("/home/ubuntu/results/coverage/Project/Project_10_10.coverage", "2cb73924-4d6e-4c0f-9bb9-f0c10e06bb9c");
                        setState(ProjectState.QUEUED);
                        writeline("/home/ubuntu/results/coverage/Project/Project_10_10.coverage", "3f6134df-73ec-4730-a742-4d300fe79c91");
                        synchronized (scheduleMutex) {
                            writeline("/home/ubuntu/results/coverage/Project/Project_10_10.coverage", "d49ac2bc-660d-4588-98b6-b53e480d2633");
                            queue.requestBuild(projectConfig);
                            writeline("/home/ubuntu/results/coverage/Project/Project_10_10.coverage", "31667660-c557-41ed-841a-cbb667a3fae3");
                            waitForBuildToFinish();
                        }
                    }
                } catch (InterruptedException e) {
                    writeline("/home/ubuntu/results/coverage/Project/Project_10_10.coverage", "4d2d33be-f52d-44c8-acdd-149960e24cae");
                    final String message = "Project " + name + ".run() interrupted";
                    writeline("/home/ubuntu/results/coverage/Project/Project_10_10.coverage", "7d0db68a-5e99-41b4-b598-09beb2decbd8");
                    LOG.error(message, e);
                    writeline("/home/ubuntu/results/coverage/Project/Project_10_10.coverage", "9c6ad8d6-d5a2-4c9c-8560-4b640d7528a0");
                    throw new RuntimeException(message);
                }
            }
        } finally {
            writeline("/home/ubuntu/results/coverage/Project/Project_10_10.coverage", "de7cd9e3-f866-425b-8092-9e2f5517909c");
            stopped = true;
            writeline("/home/ubuntu/results/coverage/Project/Project_10_10.coverage", "39ea91b2-638e-4f7b-93e6-fdc7d9c10abc");
            LOG.info("Project " + name + " stopped");
        }
    }

    void waitIfPaused() throws InterruptedException {
        writeline("/home/ubuntu/results/coverage/Project/Project_10_10.coverage", "422c804b-e858-41e1-9a1f-3802c278a321");
        synchronized (pausedMutex) {
            writeline("/home/ubuntu/results/coverage/Project/Project_10_10.coverage", "b6d9b219-4cd5-4d5d-8588-753d9bfac236");
            while (isPaused) {
                writeline("/home/ubuntu/results/coverage/Project/Project_10_10.coverage", "03c001f5-181d-4b1a-81b9-a70fc368b6f4");
                setState(ProjectState.PAUSED);
                writeline("/home/ubuntu/results/coverage/Project/Project_10_10.coverage", "a95369b2-be27-49b8-a697-d67dab80c208");
                pausedMutex.wait(10 * DateUtil.ONE_MINUTE);
            }
        }
    }

    void waitForNextBuild() throws InterruptedException {
        writeline("/home/ubuntu/results/coverage/Project/Project_10_10.coverage", "d937921e-8bcc-4ff6-9cf7-25e202a0082b");
        long waitTime = getTimeToNextBuild(new Date());
        writeline("/home/ubuntu/results/coverage/Project/Project_10_10.coverage", "f0a9cf20-dec6-498b-88a3-5ca364fee7eb");
        if (needToWaitForNextBuild(waitTime) && !buildForced) {
            writeline("/home/ubuntu/results/coverage/Project/Project_10_10.coverage", "cc7ff4ad-a4b1-4574-b122-2ec860dffc39");
            final String msg = "next build in " + DateUtil.formatTime(waitTime);
            writeline("/home/ubuntu/results/coverage/Project/Project_10_10.coverage", "4c048c9e-00d8-488a-bd13-9700dd244f4e");
            info(msg);
            writeline("/home/ubuntu/results/coverage/Project/Project_10_10.coverage", "dba13ca3-3741-40e3-82c8-3902125820fb");
            synchronized (waitMutex) {
                writeline("/home/ubuntu/results/coverage/Project/Project_10_10.coverage", "a712f6b9-1196-45f9-94c5-3b910899a27a");
                setState(ProjectState.WAITING);
                writeline("/home/ubuntu/results/coverage/Project/Project_10_10.coverage", "673b044a-8ffd-4d68-a2f4-19bab185308b");
                progress.setValue(msg);
                writeline("/home/ubuntu/results/coverage/Project/Project_10_10.coverage", "2df0643e-1114-4786-afa2-78f87bf35839");
                waitMutex.wait(waitTime);
            }
        }
    }

    long getTimeToNextBuild(Date now) {
        writeline("/home/ubuntu/results/coverage/Project/Project_10_10.coverage", "18fb5cc5-43cd-44a7-a172-f0ab0104d67c");
        long waitTime = projectConfig.getSchedule().getTimeToNextBuild(now, getBuildInterval());
        writeline("/home/ubuntu/results/coverage/Project/Project_10_10.coverage", "5d003d35-a7c4-48f3-90f2-bf3b314bdf98");
        if (waitTime == 0) {
            writeline("/home/ubuntu/results/coverage/Project/Project_10_10.coverage", "0ebfb174-af68-4c13-96cd-1917d889865b");
            // project that has just built within a minute time
            if (buildStartTime != null) {
                writeline("/home/ubuntu/results/coverage/Project/Project_10_10.coverage", "bd8558ee-6b3d-48c7-8c54-fe30a4889ce4");
                long millisSinceLastBuild = now.getTime() - buildStartTime.getTime();
                writeline("/home/ubuntu/results/coverage/Project/Project_10_10.coverage", "2ccc87ba-6499-45c2-b974-b8682e803604");
                if (millisSinceLastBuild < Schedule.ONE_MINUTE) {
                    writeline("/home/ubuntu/results/coverage/Project/Project_10_10.coverage", "314a03d2-04ed-44a6-8245-eb25bcb395e0");
                    debug("build finished within a minute, getting new time to next build");
                    writeline("/home/ubuntu/results/coverage/Project/Project_10_10.coverage", "ac058a3c-b866-4fd9-9bc2-26d01eec9681");
                    Date oneMinuteInFuture = new Date(now.getTime() + Schedule.ONE_MINUTE);
                    writeline("/home/ubuntu/results/coverage/Project/Project_10_10.coverage", "3dbf2969-5531-449b-a7ff-2d832d605c3a");
                    waitTime = projectConfig.getSchedule().getTimeToNextBuild(oneMinuteInFuture, getBuildInterval());
                    writeline("/home/ubuntu/results/coverage/Project/Project_10_10.coverage", "dcc587e8-d887-47ff-9df3-2e300362f8c1");
                    waitTime += Schedule.ONE_MINUTE;
                }
            }
        }
        writeline("/home/ubuntu/results/coverage/Project/Project_10_10.coverage", "9e6961cf-2974-45dd-afd6-392e4a198653");
        return waitTime;
    }

    static boolean needToWaitForNextBuild(long waitTime) {
        writelineStatic("/home/ubuntu/results/coverage/Project/Project_10_10.coverage", "0cd693d0-3ab5-414b-829b-badfb7cc691b");
        return waitTime > 0;
    }

    /**
     * @return true if build was forced, intended for unit testing only.
     */
    boolean isBuildForced() {
        writeline("/home/ubuntu/results/coverage/Project/Project_10_10.coverage", "763b6bde-b310-4d4d-aa67-7469613ef72a");
        return buildForced;
    }

    void forceBuild() {
        writeline("/home/ubuntu/results/coverage/Project/Project_10_10.coverage", "ce694343-a028-4eae-a0cc-4edab5f138c3");
        synchronized (waitMutex) {
            writeline("/home/ubuntu/results/coverage/Project/Project_10_10.coverage", "1ef19d88-be64-4b23-8078-e59e85bb4631");
            waitMutex.notify();
        }
    }

    public void forceBuildWithTarget(String buildTarget) {
        writeline("/home/ubuntu/results/coverage/Project/Project_10_10.coverage", "00467abd-af9d-4543-8cfd-d5676120ec1b");
        this.buildTarget = buildTarget;
        writeline("/home/ubuntu/results/coverage/Project/Project_10_10.coverage", "7fc033ac-4b77-4e03-b2a0-e1dfa87f1c4f");
        setBuildForced(true);
    }

    public void forceBuildWithTarget(String buildTarget, Map<String, String> addedProperties) {
        writeline("/home/ubuntu/results/coverage/Project/Project_10_10.coverage", "c3070684-1a46-4262-ae58-4347b7cae0b1");
        additionalProperties = addedProperties;
        writeline("/home/ubuntu/results/coverage/Project/Project_10_10.coverage", "35911ff4-3735-4641-ba35-62f4b553d3ee");
        forceBuildWithTarget(buildTarget);
    }

    void waitForBuildToFinish() throws InterruptedException {
        writeline("/home/ubuntu/results/coverage/Project/Project_10_10.coverage", "1de37ed2-abd9-44ad-9438-75e720493b0c");
        synchronized (scheduleMutex) {
            writeline("/home/ubuntu/results/coverage/Project/Project_10_10.coverage", "e244612e-1fbc-4725-ab7e-f4bb16873366");
            debug("waiting for build to finish");
            writeline("/home/ubuntu/results/coverage/Project/Project_10_10.coverage", "6a809d42-6345-40fe-9021-96ef5ed805ff");
            scheduleMutex.wait();
        }
    }

    void buildFinished() {
        writeline("/home/ubuntu/results/coverage/Project/Project_10_10.coverage", "67876808-20cc-4405-9c8c-3b60fed76eb0");
        synchronized (scheduleMutex) {
            writeline("/home/ubuntu/results/coverage/Project/Project_10_10.coverage", "f3cb79da-15a2-4be9-8c6c-101b6ebd9942");
            debug("build finished");
            writeline("/home/ubuntu/results/coverage/Project/Project_10_10.coverage", "8435ffa5-02fc-4df1-a7fc-158fc2ddc46d");
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
        writeline("/home/ubuntu/results/coverage/Project/Project_10_10.coverage", "30d60c6b-a98f-4c62-ab46-7ecb693c792f");
        setState(ProjectState.MODIFICATIONSET);
        writeline("/home/ubuntu/results/coverage/Project/Project_10_10.coverage", "ca332e13-b87b-4cd7-99f6-880db4817391");
        final ModificationSet modificationSet = projectConfig.getModificationSet();
        writeline("/home/ubuntu/results/coverage/Project/Project_10_10.coverage", "4978db22-1681-4396-b02b-1de78f8135ab");
        if (modificationSet == null) {
            writeline("/home/ubuntu/results/coverage/Project/Project_10_10.coverage", "b8c83da3-10c6-4c9f-a2b9-ab3ab9f4eb2b");
            debug("no modification set, nothing to detect.");
            writeline("/home/ubuntu/results/coverage/Project/Project_10_10.coverage", "c86b3a8c-84ef-4f05-8dd6-3a78e22562e4");
            if (buildWasForced) {
                writeline("/home/ubuntu/results/coverage/Project/Project_10_10.coverage", "0f9a65c4-3870-4f59-b4c0-c8e82f635e4f");
                info("no modification set but build was forced");
                writeline("/home/ubuntu/results/coverage/Project/Project_10_10.coverage", "457c0e3d-39ba-4caa-b29a-2bfb9c2251f5");
                return new Element("modifications");
            }
            writeline("/home/ubuntu/results/coverage/Project/Project_10_10.coverage", "f92d0991-77e9-4879-916a-cbbf46b2293d");
            if (!requiremodification) {
                writeline("/home/ubuntu/results/coverage/Project/Project_10_10.coverage", "083bb330-15b4-47d4-99ec-356370d09acb");
                info("no modification set but no modifications required");
                writeline("/home/ubuntu/results/coverage/Project/Project_10_10.coverage", "4701bc3b-ea3f-4a6d-a28b-0c38a68e5a45");
                return new Element("modifications");
            }
            writeline("/home/ubuntu/results/coverage/Project/Project_10_10.coverage", "80d6794d-358b-4db3-b64a-bdc44a8af9b8");
            return null;
        }
        writeline("/home/ubuntu/results/coverage/Project/Project_10_10.coverage", "bff96471-804c-49e5-8f6a-a82e04cd4af2");
        final boolean checkNewChangesFirst = checkOnlySinceLastBuild();
        writeline("/home/ubuntu/results/coverage/Project/Project_10_10.coverage", "ecf83551-e4d3-49e5-9089-bc82d0016a3e");
        Element modifications;
        writeline("/home/ubuntu/results/coverage/Project/Project_10_10.coverage", "fd104e8f-f3e0-48a2-b1a5-b541d19f8115");
        if (checkNewChangesFirst) {
            writeline("/home/ubuntu/results/coverage/Project/Project_10_10.coverage", "aeae1f93-c088-41ef-b66a-558adc0cea47");
            debug("getting changes since last build");
            writeline("/home/ubuntu/results/coverage/Project/Project_10_10.coverage", "7004cc35-7658-4eb7-bfb3-72f3cc667764");
            modifications = modificationSet.retrieveModificationsAsElement(lastBuild, progress);
        } else {
            writeline("/home/ubuntu/results/coverage/Project/Project_10_10.coverage", "0abab1e8-ac21-4fe9-97b9-2640c06a6bd9");
            debug("getting changes since last successful build");
            writeline("/home/ubuntu/results/coverage/Project/Project_10_10.coverage", "c8dbacd7-67db-4b78-a478-ff83ee5945f4");
            modifications = modificationSet.retrieveModificationsAsElement(lastSuccessfulBuild, progress);
        }
        writeline("/home/ubuntu/results/coverage/Project/Project_10_10.coverage", "013433db-43ea-4ae6-82de-3b158d421d64");
        if (!modificationSet.isModified()) {
            writeline("/home/ubuntu/results/coverage/Project/Project_10_10.coverage", "2739a8ff-45bf-4760-87fa-9ec2afeff645");
            info("No modifications found, build not necessary.");
            writeline("/home/ubuntu/results/coverage/Project/Project_10_10.coverage", "13b62cbb-a9ea-4ebc-82e9-88ed8b680224");
            // * build forced
            if (buildAfterFailed && !wasLastBuildSuccessful) {
                writeline("/home/ubuntu/results/coverage/Project/Project_10_10.coverage", "f7dec576-66aa-4c4e-bb57-adeb08e73c89");
                info("Building anyway, since buildAfterFailed is true and last build failed.");
            } else if (!requiremodification) {
                writeline("/home/ubuntu/results/coverage/Project/Project_10_10.coverage", "5336d78f-074f-4b27-90f8-518f5a463d25");
                info("Building anyway, since modifications not required");
            } else {
                writeline("/home/ubuntu/results/coverage/Project/Project_10_10.coverage", "4936bdc3-84f2-47b3-96e5-71cb63635dd3");
                if (buildWasForced) {
                    writeline("/home/ubuntu/results/coverage/Project/Project_10_10.coverage", "f2daef8a-9712-4a3c-9d37-a3382ffd7bde");
                    info("Building anyway, since build was explicitly forced.");
                } else {
                    writeline("/home/ubuntu/results/coverage/Project/Project_10_10.coverage", "c72899d5-d3e6-474f-a357-fefa18928ef0");
                    return null;
                }
            }
        }
        writeline("/home/ubuntu/results/coverage/Project/Project_10_10.coverage", "7771013f-7aa5-47ac-9627-863643951ab0");
        if (checkNewChangesFirst) {
            writeline("/home/ubuntu/results/coverage/Project/Project_10_10.coverage", "1b922d17-f670-45fd-b627-6d6c125c9144");
            debug("new changes found; now getting complete set");
            writeline("/home/ubuntu/results/coverage/Project/Project_10_10.coverage", "b132fc1b-b891-47f2-966d-9883ea62cc39");
            modifications = modificationSet.retrieveModificationsAsElement(lastSuccessfulBuild, progress);
        }
        writeline("/home/ubuntu/results/coverage/Project/Project_10_10.coverage", "66f19d76-b88c-46a1-8bc8-4d914e097285");
        return modifications;
    }

    /**
     * @return boolean
     */
    boolean checkOnlySinceLastBuild() {
        writeline("/home/ubuntu/results/coverage/Project/Project_10_10.coverage", "7af378af-c306-458a-9a70-f282568d9a51");
        if (lastBuild == null || lastSuccessfulBuild == null) {
            writeline("/home/ubuntu/results/coverage/Project/Project_10_10.coverage", "eaaf72bf-f6f3-4310-97be-6d44d5f12158");
            return false;
        }
        writeline("/home/ubuntu/results/coverage/Project/Project_10_10.coverage", "51c7e734-ff95-479c-a43d-047a8801b786");
        final long lastBuildLong = lastBuild.getTime();
        writeline("/home/ubuntu/results/coverage/Project/Project_10_10.coverage", "2a6ce8ee-bc92-49d1-b11b-f0276034d335");
        final long timeDifference = lastBuildLong - lastSuccessfulBuild.getTime();
        writeline("/home/ubuntu/results/coverage/Project/Project_10_10.coverage", "26c21d9b-4250-4061-ba8d-f10ec546e5bd");
        final boolean moreThanASecond = timeDifference > DateUtil.ONE_SECOND;
        writeline("/home/ubuntu/results/coverage/Project/Project_10_10.coverage", "50e6a29e-815b-481e-9761-610c6d11e1fc");
        return !buildAfterFailed && moreThanASecond;
    }

    /**
     * Serialize the project to allow resumption after a process bounce
     */
    public void serializeProject() {
        writeline("/home/ubuntu/results/coverage/Project/Project_10_10.coverage", "97d50a93-7a58-4615-8d55-1b7ef5b0e5ac");
        final String safeProjectName = Builder.getFileSystemSafeProjectName(name);
        writeline("/home/ubuntu/results/coverage/Project/Project_10_10.coverage", "a57d7567-cc23-4b2a-b0a6-109ee160c4c4");
        try {
            writeline("/home/ubuntu/results/coverage/Project/Project_10_10.coverage", "a8c7462c-3b2d-4a47-8cb1-36ee39a85a37");
            final ObjectOutputStream s = new ObjectOutputStream(new FileOutputStream(safeProjectName + ".ser"));
            writeline("/home/ubuntu/results/coverage/Project/Project_10_10.coverage", "437772eb-96b1-4341-97d5-bcc0645cefcf");
            try {
                writeline("/home/ubuntu/results/coverage/Project/Project_10_10.coverage", "a69e8daf-4bb3-4e62-b8e1-cd052a8758e0");
                s.writeObject(this);
                writeline("/home/ubuntu/results/coverage/Project/Project_10_10.coverage", "1261704b-5a2b-4086-9850-17b0703ed9c7");
                s.flush();
                writeline("/home/ubuntu/results/coverage/Project/Project_10_10.coverage", "cd3a03aa-b205-4af7-bdf5-3b4055e00ab1");
                debug("Serializing project to [" + safeProjectName + ".ser]");
            } finally {
                writeline("/home/ubuntu/results/coverage/Project/Project_10_10.coverage", "2e0a69f9-3470-4af3-b0cf-b5ed64a281b0");
                s.close();
            }
        } catch (Exception e) {
            writeline("/home/ubuntu/results/coverage/Project/Project_10_10.coverage", "9ba07ad8-6be5-45d3-99cc-43305b501187");
            LOG.warn("Error serializing project to [" + safeProjectName + ".ser]: " + e.getMessage(), e);
        }
    }

    public void setLabelIncrementer(final LabelIncrementer incrementer) throws CruiseControlException {
        writeline("/home/ubuntu/results/coverage/Project/Project_10_10.coverage", "a7ee9298-7807-4f7f-875e-823924933f32");
        if (incrementer == null) {
            writeline("/home/ubuntu/results/coverage/Project/Project_10_10.coverage", "604cb4f7-0b34-43ce-89ea-3fb9d2dc1922");
            throw new IllegalArgumentException("label incrementer can't be null");
        }
        writeline("/home/ubuntu/results/coverage/Project/Project_10_10.coverage", "b144cf41-fc2a-49b6-b4fa-de0b278753ac");
        labelIncrementer = incrementer;
        writeline("/home/ubuntu/results/coverage/Project/Project_10_10.coverage", "32579b2c-c13b-44e0-9c10-71903b6f54f1");
        if (label == null) {
            writeline("/home/ubuntu/results/coverage/Project/Project_10_10.coverage", "099568ca-3c9f-4ab0-af41-b2f53e54ec63");
            label = labelIncrementer.getDefaultLabel();
        }
        writeline("/home/ubuntu/results/coverage/Project/Project_10_10.coverage", "f2cb238c-6584-4945-a65c-05b146145e29");
        validateLabel(label, labelIncrementer);
    }

    public LabelIncrementer getLabelIncrementer() {
        writeline("/home/ubuntu/results/coverage/Project/Project_10_10.coverage", "7125efa8-8c68-42c5-8183-efdfe9767338");
        return labelIncrementer;
    }

    public void setName(final String projectName) {
        writeline("/home/ubuntu/results/coverage/Project/Project_10_10.coverage", "cd5f7e53-389c-429a-833e-2d9a00dcb747");
        name = projectName;
    }

    public String getName() {
        writeline("/home/ubuntu/results/coverage/Project/Project_10_10.coverage", "cb18711f-7431-48a1-bfca-11540881a529");
        return name;
    }

    public void setLabel(final String newLabel) {
        writeline("/home/ubuntu/results/coverage/Project/Project_10_10.coverage", "795762bf-782f-4a7f-b222-e07f66ee7a69");
        label = newLabel;
    }

    public String getLabel() {
        writeline("/home/ubuntu/results/coverage/Project/Project_10_10.coverage", "0afc9395-c609-4acf-9b6b-a24fa714bcf7");
        return label;
    }

    /**
     * @param newLastBuild string containing the build date in the format
     * yyyyMMddHHmmss
     * @throws CruiseControlException if the date cannot be extracted from the
     * input string
     */
    public void setLastBuild(final String newLastBuild) throws CruiseControlException {
        writeline("/home/ubuntu/results/coverage/Project/Project_10_10.coverage", "4395bdb3-bbb8-44b6-95a5-c1806234d943");
        lastBuild = DateUtil.parseFormattedTime(newLastBuild, "lastBuild");
    }

    /**
     * @param newLastSuccessfulBuild string containing the build date in the format
     * yyyyMMddHHmmss
     * @throws CruiseControlException if the date cannot be extracted from the
     * input string
     */
    public void setLastSuccessfulBuild(final String newLastSuccessfulBuild) throws CruiseControlException {
        writeline("/home/ubuntu/results/coverage/Project/Project_10_10.coverage", "14934499-734a-4cdb-ae08-a67b29bce9c1");
        lastSuccessfulBuild = DateUtil.parseFormattedTime(newLastSuccessfulBuild, "lastSuccessfulBuild");
    }

    public String getLastBuild() {
        writeline("/home/ubuntu/results/coverage/Project/Project_10_10.coverage", "51d7a032-6191-4229-8371-d4c78903702b");
        if (lastBuild == null) {
            writeline("/home/ubuntu/results/coverage/Project/Project_10_10.coverage", "19b1c458-4310-4b20-b7fe-964ee84db50b");
            return null;
        }
        writeline("/home/ubuntu/results/coverage/Project/Project_10_10.coverage", "8be89e99-c3dd-4c01-9225-55b4fb12f229");
        return DateUtil.getFormattedTime(lastBuild);
    }

    public boolean getBuildForced() {
        writeline("/home/ubuntu/results/coverage/Project/Project_10_10.coverage", "8b42e75b-76bc-4152-86a3-12672f3e0b82");
        return buildForced;
    }

    public void setBuildForced(boolean forceNewBuildNow) {
        writeline("/home/ubuntu/results/coverage/Project/Project_10_10.coverage", "977c6aa2-8bbb-4141-921a-6a0d3b93141d");
        buildForced = forceNewBuildNow;
        writeline("/home/ubuntu/results/coverage/Project/Project_10_10.coverage", "fcc21b89-c1ce-45b4-86ce-389b351e7f36");
        if (forceNewBuildNow) {
            writeline("/home/ubuntu/results/coverage/Project/Project_10_10.coverage", "e0eb28b3-5bfe-4b26-8792-86c9e43fd16b");
            forceBuild();
        }
    }

    public String getLastSuccessfulBuild() {
        writeline("/home/ubuntu/results/coverage/Project/Project_10_10.coverage", "d5e5e4b4-436f-4924-977a-cb686563a0ab");
        if (lastSuccessfulBuild == null) {
            writeline("/home/ubuntu/results/coverage/Project/Project_10_10.coverage", "dae97e85-a797-4d94-89ed-d4c38d4a9f3e");
            return null;
        }
        writeline("/home/ubuntu/results/coverage/Project/Project_10_10.coverage", "f55bc307-f429-460a-9ff5-5c2bcf5a22e4");
        return DateUtil.getFormattedTime(lastSuccessfulBuild);
    }

    public String getLogDir() {
        writeline("/home/ubuntu/results/coverage/Project/Project_10_10.coverage", "600c53b7-6cc9-447f-a2f0-e036ea0f9faf");
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
        writeline("/home/ubuntu/results/coverage/Project/Project_10_10.coverage", "f2d9f833-b8a6-40bb-81f5-568f59829488");
        if (overrideBuildInterval == null) {
            writeline("/home/ubuntu/results/coverage/Project/Project_10_10.coverage", "fad602b0-70e0-4376-aff4-3d9f8d40b776");
            return projectConfig.getSchedule().getInterval();
        } else {
            writeline("/home/ubuntu/results/coverage/Project/Project_10_10.coverage", "763337e9-154e-4788-9786-fe03a5a27d0f");
            return overrideBuildInterval;
        }
    }

    /**
     * Sets the build interval that this Project should use. This method
     * overrides the value initially specified in the Schedule attribute.
     * @param sleepMillis the number of milliseconds to sleep between build attempts
     */
    public void overrideBuildInterval(final long sleepMillis) {
        writeline("/home/ubuntu/results/coverage/Project/Project_10_10.coverage", "c25de60e-b6f2-4548-afc2-0b1fa02e513a");
        overrideBuildInterval = sleepMillis;
    }

    public boolean isPaused() {
        writeline("/home/ubuntu/results/coverage/Project/Project_10_10.coverage", "3cb2cbef-0e88-4803-8d3c-00c0d5aac779");
        return isPaused;
    }

    public void setPaused(final boolean paused) {
        writeline("/home/ubuntu/results/coverage/Project/Project_10_10.coverage", "21efd3a2-dc99-4ed2-816d-eed07c6d2e7d");
        synchronized (pausedMutex) {
            writeline("/home/ubuntu/results/coverage/Project/Project_10_10.coverage", "ed9d8a2e-2b93-42fc-86de-268b9dc8bc5d");
            if (isPaused && !paused) {
                writeline("/home/ubuntu/results/coverage/Project/Project_10_10.coverage", "39b6f4ee-1c40-490d-9290-61c52e033649");
                pausedMutex.notifyAll();
            }
            writeline("/home/ubuntu/results/coverage/Project/Project_10_10.coverage", "403ebbf5-cc24-4146-ba78-3844b3e02785");
            isPaused = paused;
        }
    }

    public void setBuildAfterFailed(final boolean rebuildEvenWithNoNewModifications) {
        writeline("/home/ubuntu/results/coverage/Project/Project_10_10.coverage", "78393769-ceb7-4c7a-a2ff-b96b7f24a530");
        buildAfterFailed = rebuildEvenWithNoNewModifications;
    }

    public String getStatus() {
        writeline("/home/ubuntu/results/coverage/Project/Project_10_10.coverage", "d0aef244-7cfc-42dd-921c-54f44b6b1d88");
        return getState().getDescription();
    }

    public String getStatusWithQueuePosition() {
        writeline("/home/ubuntu/results/coverage/Project/Project_10_10.coverage", "e54481f9-d28d-436f-86a2-c0c79f0d1305");
        if (ProjectState.QUEUED.equals(getState())) {
            writeline("/home/ubuntu/results/coverage/Project/Project_10_10.coverage", "4dad8fa8-adb1-4349-8b08-496ab7c6ef0d");
            return getState().getDescription() + " - " + queue.findPosition(projectConfig);
        } else {
            writeline("/home/ubuntu/results/coverage/Project/Project_10_10.coverage", "40274533-119b-42c6-b543-f5e792461ad7");
            return getState().getDescription();
        }
    }

    public ProjectState getState() {
        writeline("/home/ubuntu/results/coverage/Project/Project_10_10.coverage", "c9097fa3-3f3e-442c-96ec-2e61b0e9c15b");
        return state;
    }

    private void setState(final ProjectState newState) {
        writeline("/home/ubuntu/results/coverage/Project/Project_10_10.coverage", "62c5a31e-99c6-49de-9389-216756d58f62");
        state = newState;
        writeline("/home/ubuntu/results/coverage/Project/Project_10_10.coverage", "4c0792c2-a805-48c2-b46f-72b369d9809f");
        info(getStatus());
        writeline("/home/ubuntu/results/coverage/Project/Project_10_10.coverage", "7d8c81e0-df1b-4882-9040-76e1e14a179c");
        notifyListeners(new ProjectStateChangedEvent(name, getState()));
        writeline("/home/ubuntu/results/coverage/Project/Project_10_10.coverage", "d3b1e634-5016-4da6-869c-cb114ab5c28f");
        fireProgressEvent(new BuildProgressEvent(this, getState()));
    }

    public void setBuildQueue(final BuildQueue buildQueue) {
        writeline("/home/ubuntu/results/coverage/Project/Project_10_10.coverage", "9d0caec6-b992-40b5-b1f6-ed1e72fd7fb1");
        queue = buildQueue;
    }

    public String getBuildStartTime() {
        writeline("/home/ubuntu/results/coverage/Project/Project_10_10.coverage", "78571f04-d68b-450e-a3b2-d1aaf81a4081");
        return DateUtil.getFormattedTime(buildStartTime);
    }

    public Log getLog() {
        writeline("/home/ubuntu/results/coverage/Project/Project_10_10.coverage", "11ca6c33-1ad2-4d17-b882-abc7ddf516c1");
        return this.projectConfig.getLog();
    }

    /**
     * Initialize the project. Uses ProjectXMLHelper to parse a project file.
     */
    protected void init() {
        writeline("/home/ubuntu/results/coverage/Project/Project_10_10.coverage", "d4dbf32b-013c-4c1e-9d30-1d19771ae8d1");
        if (projectConfig == null) {
            writeline("/home/ubuntu/results/coverage/Project/Project_10_10.coverage", "8003edb0-a5a3-497e-ad44-1245d4483bde");
            throw new IllegalStateException("projectConfig must be set on project before calling init()");
        }
        writeline("/home/ubuntu/results/coverage/Project/Project_10_10.coverage", "7666ee94-f547-48b6-b380-ac6e4d4292c9");
        buildAfterFailed = projectConfig.shouldBuildAfterFailed();
        writeline("/home/ubuntu/results/coverage/Project/Project_10_10.coverage", "87e88912-bd97-42e0-a8a4-ffd7640c3517");
        forceOnly = projectConfig.isForceOnly();
        writeline("/home/ubuntu/results/coverage/Project/Project_10_10.coverage", "346ce936-8707-46cd-812f-1f02049a163f");
        requiremodification = projectConfig.isRequiremodification();
        writeline("/home/ubuntu/results/coverage/Project/Project_10_10.coverage", "175b89f3-5c4c-43fb-8af7-49c11d7cdd55");
        if (lastBuild == null) {
            writeline("/home/ubuntu/results/coverage/Project/Project_10_10.coverage", "1622a6dd-ab01-4649-92ba-1d786c3787cb");
            lastBuild = DateUtil.getMidnight();
        }
        writeline("/home/ubuntu/results/coverage/Project/Project_10_10.coverage", "986af347-20ea-43f7-ab85-9ce9dc2982d1");
        if (lastSuccessfulBuild == null) {
            writeline("/home/ubuntu/results/coverage/Project/Project_10_10.coverage", "8f1ed46a-6418-401c-8aec-468b7d3a6ed3");
            lastSuccessfulBuild = lastBuild;
        }
        writeline("/home/ubuntu/results/coverage/Project/Project_10_10.coverage", "f46a84fc-54e1-473f-9ef7-cb1137ed41cd");
        if (LOG.isDebugEnabled()) {
            writeline("/home/ubuntu/results/coverage/Project/Project_10_10.coverage", "53fd036c-e47b-4a35-b620-4a58f15c884e");
            debug("buildInterval          = [" + getBuildInterval() + "]");
            writeline("/home/ubuntu/results/coverage/Project/Project_10_10.coverage", "2ebff27c-c96e-40de-ae60-f43da2073fe2");
            debug("buildForced            = [" + buildForced + "]");
            writeline("/home/ubuntu/results/coverage/Project/Project_10_10.coverage", "ea9da84e-87b3-4edc-a13f-5c3b5ae398fa");
            debug("buildAfterFailed       = [" + buildAfterFailed + "]");
            writeline("/home/ubuntu/results/coverage/Project/Project_10_10.coverage", "c42955af-081f-4376-8fda-8752cd77c3d7");
            debug("requireModifcation     = [" + requiremodification + "]");
            writeline("/home/ubuntu/results/coverage/Project/Project_10_10.coverage", "31b27443-7342-4fc5-8eb4-8d3f8f8dfa3d");
            debug("forceOnly              = [" + forceOnly + "]");
            writeline("/home/ubuntu/results/coverage/Project/Project_10_10.coverage", "dda868db-e96b-495b-bb4b-1ffc7bb9a078");
            debug("buildCounter           = [" + buildCounter + "]");
            writeline("/home/ubuntu/results/coverage/Project/Project_10_10.coverage", "cf52d701-b19d-4e15-a39a-3a02705241fb");
            debug("isPaused               = [" + isPaused + "]");
            writeline("/home/ubuntu/results/coverage/Project/Project_10_10.coverage", "04bafe3b-4778-4ee6-a9dc-9d9457e73117");
            debug("label                  = [" + label + "]");
            writeline("/home/ubuntu/results/coverage/Project/Project_10_10.coverage", "428c7cda-bd22-4834-8170-f7ad494f73bc");
            debug("lastBuild              = [" + DateUtil.getFormattedTime(lastBuild) + "]");
            writeline("/home/ubuntu/results/coverage/Project/Project_10_10.coverage", "fd019356-afc7-4a13-94d4-08832ea8b0d0");
            debug("lastSuccessfulBuild    = [" + DateUtil.getFormattedTime(lastSuccessfulBuild) + "]");
            writeline("/home/ubuntu/results/coverage/Project/Project_10_10.coverage", "6187d0de-1b19-491b-9666-89e930506843");
            debug("logDir                 = [" + projectConfig.getLog().getLogDir() + "]");
            writeline("/home/ubuntu/results/coverage/Project/Project_10_10.coverage", "b6f7e187-ab10-4951-97ba-9e80c02fae41");
            debug("logXmlEncoding         = [" + projectConfig.getLog().getLogXmlEncoding() + "]");
            writeline("/home/ubuntu/results/coverage/Project/Project_10_10.coverage", "e3bd860f-e4b2-4558-bbeb-3dd11a92e3dc");
            debug("wasLastBuildSuccessful = [" + wasLastBuildSuccessful + "]");
        }
    }

    protected Element getProjectPropertiesElement(final Date now) {
        writeline("/home/ubuntu/results/coverage/Project/Project_10_10.coverage", "8714bdd1-59b4-4794-97fb-0048330dee0d");
        final Element infoElement = new Element("info");
        writeline("/home/ubuntu/results/coverage/Project/Project_10_10.coverage", "5d687b3e-c8c7-4088-b7ce-d8297911f7ca");
        addProperty(infoElement, "projectname", name);
        writeline("/home/ubuntu/results/coverage/Project/Project_10_10.coverage", "f6342416-27ab-4d98-947c-f911bccba9b0");
        final String lastBuildString = DateUtil.getFormattedTime(lastBuild == null ? now : lastBuild);
        writeline("/home/ubuntu/results/coverage/Project/Project_10_10.coverage", "5e02765d-4be2-4275-ac30-7fe8bb45a782");
        addProperty(infoElement, "lastbuild", lastBuildString);
        writeline("/home/ubuntu/results/coverage/Project/Project_10_10.coverage", "9af01b5c-57a7-4162-93b7-3662436c47e9");
        final String lastSuccessfulBuildString = DateUtil.getFormattedTime(lastSuccessfulBuild == null ? now : lastSuccessfulBuild);
        writeline("/home/ubuntu/results/coverage/Project/Project_10_10.coverage", "80204c66-02a6-414c-9f26-7c3c051f7c90");
        addProperty(infoElement, "lastsuccessfulbuild", lastSuccessfulBuildString);
        writeline("/home/ubuntu/results/coverage/Project/Project_10_10.coverage", "0b1b1a23-9234-497c-a28f-a0d89e379399");
        addProperty(infoElement, "builddate", DateUtil.formatIso8601(now));
        writeline("/home/ubuntu/results/coverage/Project/Project_10_10.coverage", "5dfeb9ff-8352-487f-b280-4e463e359e46");
        addProperty(infoElement, "cctimestamp", DateUtil.getFormattedTime(now));
        writeline("/home/ubuntu/results/coverage/Project/Project_10_10.coverage", "ee20f2b7-a4d5-44a8-97e0-350aae8d1f7b");
        addProperty(infoElement, "label", label);
        writeline("/home/ubuntu/results/coverage/Project/Project_10_10.coverage", "8551d7b3-2732-4765-b060-79f1abc05297");
        addProperty(infoElement, "interval", Long.toString(getBuildInterval() / 1000L));
        writeline("/home/ubuntu/results/coverage/Project/Project_10_10.coverage", "6b64ba24-5dfe-47db-bf82-2472e075650d");
        addProperty(infoElement, "lastbuildsuccessful", String.valueOf(wasLastBuildSuccessful));
        writeline("/home/ubuntu/results/coverage/Project/Project_10_10.coverage", "38635576-571b-493f-b6cd-8c62d8ef98c0");
        return infoElement;
    }

    private void addProperty(final Element parent, final String key, final String value) {
        writeline("/home/ubuntu/results/coverage/Project/Project_10_10.coverage", "ff524091-f6fd-454e-be80-290e137d074c");
        final Element propertyElement = new Element("property");
        writeline("/home/ubuntu/results/coverage/Project/Project_10_10.coverage", "d4200009-9782-444a-a649-2cbc4b736f46");
        propertyElement.setAttribute("name", key);
        writeline("/home/ubuntu/results/coverage/Project/Project_10_10.coverage", "b70a092c-617f-407e-b8d9-a84628a6d0de");
        propertyElement.setAttribute("value", value);
        writeline("/home/ubuntu/results/coverage/Project/Project_10_10.coverage", "08c3125d-cde1-4a6b-9332-0b2be03785f8");
        parent.addContent(propertyElement);
    }

    protected Map<String, String> getProjectPropertiesMap(final Date now) {
        writeline("/home/ubuntu/results/coverage/Project/Project_10_10.coverage", "c975679d-ee87-4de6-8512-44bdbf95d2ce");
        final Map<String, String> buildProperties = new HashMap<String, String>();
        writeline("/home/ubuntu/results/coverage/Project/Project_10_10.coverage", "be7b600d-d972-49e2-b477-9aa3fd344b29");
        buildProperties.put("projectname", name);
        writeline("/home/ubuntu/results/coverage/Project/Project_10_10.coverage", "615bcba9-a4bd-419c-9b0d-9279d75b6759");
        buildProperties.put("label", label);
        writeline("/home/ubuntu/results/coverage/Project/Project_10_10.coverage", "26276af7-e782-4d16-9606-e06b27db8dcf");
        // TODO: Shouldn't have CVS specific properties here
        buildProperties.put("cvstimestamp", CVSDateUtil.formatCVSDate(now));
        writeline("/home/ubuntu/results/coverage/Project/Project_10_10.coverage", "0321333f-31cc-453d-897f-421e7f2ee442");
        buildProperties.put("cctimestamp", DateUtil.getFormattedTime(now));
        writeline("/home/ubuntu/results/coverage/Project/Project_10_10.coverage", "984e14f8-d0f8-4950-b544-a79c61e06d65");
        buildProperties.put("cclastgoodbuildtimestamp", getLastSuccessfulBuild());
        writeline("/home/ubuntu/results/coverage/Project/Project_10_10.coverage", "a6bd4ce3-1cf8-46a2-bf80-17fcef6510a3");
        buildProperties.put("cclastbuildtimestamp", getLastBuild());
        writeline("/home/ubuntu/results/coverage/Project/Project_10_10.coverage", "4f5439d1-7844-4a79-8903-79259530c21f");
        buildProperties.put("lastbuildsuccessful", String.valueOf(isLastBuildSuccessful()));
        writeline("/home/ubuntu/results/coverage/Project/Project_10_10.coverage", "9c42c3f5-9720-43d1-b22c-6c5dd109a0d2");
        buildProperties.put("buildforced", String.valueOf(getBuildForced()));
        writeline("/home/ubuntu/results/coverage/Project/Project_10_10.coverage", "67ff7dba-653b-4e50-ac9f-a5fa381f100e");
        if (projectConfig.getModificationSet() != null) {
            writeline("/home/ubuntu/results/coverage/Project/Project_10_10.coverage", "deccae4d-11cb-46c2-81d8-78720a40fb16");
            buildProperties.putAll(projectConfig.getModificationSet().getProperties());
        }
        writeline("/home/ubuntu/results/coverage/Project/Project_10_10.coverage", "2cef6a1b-701e-41e0-a240-98bcae584947");
        if (additionalProperties != null && !additionalProperties.isEmpty()) {
            writeline("/home/ubuntu/results/coverage/Project/Project_10_10.coverage", "8dcdb17c-66b7-4e8b-ba8a-edbd7144d738");
            buildProperties.putAll(additionalProperties);
            writeline("/home/ubuntu/results/coverage/Project/Project_10_10.coverage", "c86a4815-0bc5-4fbc-86a3-59576cd80ec7");
            additionalProperties.clear();
            writeline("/home/ubuntu/results/coverage/Project/Project_10_10.coverage", "d26f86be-cbdd-40bd-b9ae-0e26de4df4d7");
            additionalProperties = null;
        }
        writeline("/home/ubuntu/results/coverage/Project/Project_10_10.coverage", "b1b944b4-a1f9-48e0-858a-729f287ca8c2");
        return buildProperties;
    }

    /**
     * Intended only for unit testing.
     * @return additional Properties variable.
     */
    Map<String, String> getAdditionalProperties() {
        writeline("/home/ubuntu/results/coverage/Project/Project_10_10.coverage", "763d13d3-fe15-4718-950d-af248e9b16dd");
        return additionalProperties;
    }

    /**
     * Iterate over all of the registered <code>Publisher</code>s and call
     * their respective <code>publish</code> methods.
     * @param buildLog the content to publish
     * @throws CruiseControlException if an error occurs during publishing
     */
    protected void publish(final Log buildLog) throws CruiseControlException {
        writeline("/home/ubuntu/results/coverage/Project/Project_10_10.coverage", "ca60c9e2-969c-4018-8aba-926992821de3");
        setState(ProjectState.PUBLISHING);
        writeline("/home/ubuntu/results/coverage/Project/Project_10_10.coverage", "e6e6b675-d104-4b5f-986e-decb4daf8bf3");
        for (final Publisher publisher : projectConfig.getPublishers()) {
            writeline("/home/ubuntu/results/coverage/Project/Project_10_10.coverage", "9992d5c7-7c91-42f7-a9c5-f085c3d968b2");
            // catch all errors, Publishers shouldn't cause failures in the build method
            try {
                writeline("/home/ubuntu/results/coverage/Project/Project_10_10.coverage", "0f7d238d-7c8e-40db-bf1a-0c3afdb8454d");
                publisher.publish(buildLog.getContent());
            } catch (Throwable t) {
                writeline("/home/ubuntu/results/coverage/Project/Project_10_10.coverage", "361c5cfe-03a2-4d4f-8e9c-9a8f982ee85a");
                final StringBuilder message = new StringBuilder("exception publishing results");
                writeline("/home/ubuntu/results/coverage/Project/Project_10_10.coverage", "b56be2d9-4c6a-48e9-9612-6c5ced7295dc");
                message.append(" with ").append(publisher.getClass().getName());
                writeline("/home/ubuntu/results/coverage/Project/Project_10_10.coverage", "ca5e2f53-f349-477a-81b3-3e09264b0826");
                message.append(" for project ").append(name);
                writeline("/home/ubuntu/results/coverage/Project/Project_10_10.coverage", "48dfb7e4-c3ae-4d6d-9fe9-6dbe8f67a2bd");
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
        writeline("/home/ubuntu/results/coverage/Project/Project_10_10.coverage", "9c20a9fe-a047-41e0-9d48-a980a72eba81");
        setState(ProjectState.BOOTSTRAPPING);
        writeline("/home/ubuntu/results/coverage/Project/Project_10_10.coverage", "8c2307d6-09d4-485f-8737-e2fb9faf0557");
        for (final Bootstrapper bootstrapper : projectConfig.getBootstrappers()) {
            writeline("/home/ubuntu/results/coverage/Project/Project_10_10.coverage", "dd66ab91-e7c2-4edf-a183-66e8dd3d474c");
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
        writeline("/home/ubuntu/results/coverage/Project/Project_10_10.coverage", "759593d8-d069-4852-adb6-8b209639e835");
        if (!incrementer.isValidLabel(oldLabel)) {
            writeline("/home/ubuntu/results/coverage/Project/Project_10_10.coverage", "70a7d083-1b69-4827-8d63-f7dba25ac0a8");
            final String message = oldLabel + " is not a valid label for labelIncrementer " + incrementer.getClass().getName();
            writeline("/home/ubuntu/results/coverage/Project/Project_10_10.coverage", "cad7736b-afa0-4a39-98a7-ef21c979d62a");
            debug(message);
            writeline("/home/ubuntu/results/coverage/Project/Project_10_10.coverage", "b52d215c-c04f-489e-90a0-c5dcfe2c5bd3");
            throw new CruiseControlException(message);
        }
    }

    public boolean isLastBuildSuccessful() {
        writeline("/home/ubuntu/results/coverage/Project/Project_10_10.coverage", "15c81f26-6d8d-4ea5-9d7c-010cbc1d336f");
        return wasLastBuildSuccessful;
    }

    void setWasLastBuildSuccessful(final boolean buildSuccessful) {
        writeline("/home/ubuntu/results/coverage/Project/Project_10_10.coverage", "803b899f-3d17-41aa-a872-cb4381e061a3");
        wasLastBuildSuccessful = buildSuccessful;
    }

    /**
     * Logs a message to the application log, not to be confused with the
     * CruiseControl build log.
     * @param message the application message to log
     */
    private void info(final String message) {
        writeline("/home/ubuntu/results/coverage/Project/Project_10_10.coverage", "38ac67b5-f367-4adf-bcb9-79c72ce8c9e9");
        LOG.info("Project " + name + ":  " + message);
    }

    private void debug(final String message) {
        writeline("/home/ubuntu/results/coverage/Project/Project_10_10.coverage", "bb00516f-b0d8-4945-bf81-2f42dbfcdab4");
        LOG.debug("Project " + name + ":  " + message);
    }

    public void start() {
        writeline("/home/ubuntu/results/coverage/Project/Project_10_10.coverage", "91748b10-4417-478b-bdd6-dafa742cb978");
        if (stopped || getState() == ProjectState.STOPPED) {
            writeline("/home/ubuntu/results/coverage/Project/Project_10_10.coverage", "1e8b86b1-6eb0-44b1-861b-5a18854ca8e0");
            stopped = false;
            writeline("/home/ubuntu/results/coverage/Project/Project_10_10.coverage", "336d9180-a1d0-4834-8ced-74b258d60c5f");
            LOG.info("Project " + name + " starting");
            writeline("/home/ubuntu/results/coverage/Project/Project_10_10.coverage", "531bdf9d-031b-40ff-86ab-5c5af0345876");
            setState(ProjectState.IDLE);
            writeline("/home/ubuntu/results/coverage/Project/Project_10_10.coverage", "da9cbab7-8726-4606-8bab-ec7d0b873963");
            createNewSchedulingThread();
        }
    }

    protected void createNewSchedulingThread() {
        writeline("/home/ubuntu/results/coverage/Project/Project_10_10.coverage", "4c10e8e4-4ffa-4e23-9a18-77269bce2dfc");
        final Thread projectSchedulingThread = new Thread(this, "Project " + getName() + " thread");
        writeline("/home/ubuntu/results/coverage/Project/Project_10_10.coverage", "86e7395e-02b2-4ae6-bb63-885c663e5476");
        projectSchedulingThread.start();
        writeline("/home/ubuntu/results/coverage/Project/Project_10_10.coverage", "713b0207-b806-4415-baa9-30b91cb9f836");
        // brief nap to allow thread to start
        try {
            writeline("/home/ubuntu/results/coverage/Project/Project_10_10.coverage", "1f01d89f-98c7-4595-9e24-30116df717b2");
            Thread.sleep(100);
        } catch (InterruptedException ie) {
            writeline("/home/ubuntu/results/coverage/Project/Project_10_10.coverage", "5d57c605-152b-43b7-a3c2-7dd1caed4bb8");
            LOG.warn("interrupted while waiting for scheduling thread to start", ie);
        }
    }

    public void stop() {
        writeline("/home/ubuntu/results/coverage/Project/Project_10_10.coverage", "b4387252-64fc-4988-a738-f8927c0bd588");
        LOG.info("Project " + name + " stopping");
        writeline("/home/ubuntu/results/coverage/Project/Project_10_10.coverage", "309a9bf5-c58d-41fa-9b7e-87b740f2d2bb");
        stopped = true;
        writeline("/home/ubuntu/results/coverage/Project/Project_10_10.coverage", "0855a50b-db20-47b6-b516-82549a22dd42");
        setState(ProjectState.STOPPED);
        writeline("/home/ubuntu/results/coverage/Project/Project_10_10.coverage", "9e647e1f-8497-4b2e-8356-c12b68a34d81");
        synchronized (pausedMutex) {
            writeline("/home/ubuntu/results/coverage/Project/Project_10_10.coverage", "03f75467-bc2e-45d8-8180-5a02404adb8b");
            pausedMutex.notifyAll();
        }
        writeline("/home/ubuntu/results/coverage/Project/Project_10_10.coverage", "70ccd9f5-bd09-4556-b5b4-48680fbc9656");
        synchronized (waitMutex) {
            writeline("/home/ubuntu/results/coverage/Project/Project_10_10.coverage", "5bc497bf-b765-433b-bd17-741261d47885");
            waitMutex.notifyAll();
        }
        writeline("/home/ubuntu/results/coverage/Project/Project_10_10.coverage", "9c94d1bd-3520-45a2-a9ea-7a7307af9eec");
        synchronized (scheduleMutex) {
            writeline("/home/ubuntu/results/coverage/Project/Project_10_10.coverage", "04ea693c-223d-43cb-9053-87832b0edc83");
            scheduleMutex.notifyAll();
        }
    }

    public String toString() {
        writeline("/home/ubuntu/results/coverage/Project/Project_10_10.coverage", "c3fadd93-2042-474a-b6e7-b11653769fea");
        final StringBuilder sb = new StringBuilder("Project ");
        writeline("/home/ubuntu/results/coverage/Project/Project_10_10.coverage", "c2acbdd1-e7d2-42f4-a561-853a3913336f");
        sb.append(getName());
        writeline("/home/ubuntu/results/coverage/Project/Project_10_10.coverage", "1148a58e-8bea-4689-a714-45692e42b101");
        sb.append(": ");
        writeline("/home/ubuntu/results/coverage/Project/Project_10_10.coverage", "8e299f43-459f-453e-aec6-323a50f1382e");
        sb.append(getStatus());
        writeline("/home/ubuntu/results/coverage/Project/Project_10_10.coverage", "cd1b38bc-42d3-42ea-8473-9eba2051a8aa");
        if (isPaused) {
            writeline("/home/ubuntu/results/coverage/Project/Project_10_10.coverage", "fc571bd6-c885-4b96-af9c-6be9d8e80606");
            sb.append(" (paused)");
        }
        writeline("/home/ubuntu/results/coverage/Project/Project_10_10.coverage", "0076cf6f-94d4-47b9-b5fe-7693aa5d0cb7");
        return sb.toString();
    }

    public void addBuildProgressListener(final BuildProgressListener listener) {
        writeline("/home/ubuntu/results/coverage/Project/Project_10_10.coverage", "262021e3-7766-4633-824b-d5732f45ba01");
        synchronized (progressListeners) {
            writeline("/home/ubuntu/results/coverage/Project/Project_10_10.coverage", "f963029d-49a3-4e0c-918c-996111dd29c2");
            progressListeners.add(listener);
        }
    }

    protected void fireProgressEvent(final BuildProgressEvent event) {
        writeline("/home/ubuntu/results/coverage/Project/Project_10_10.coverage", "3acaaf00-2bf0-4c3b-a501-759e94707cad");
        synchronized (progressListeners) {
            writeline("/home/ubuntu/results/coverage/Project/Project_10_10.coverage", "698b9af9-bbfe-4df6-9e95-7a6fe834d6c2");
            for (final BuildProgressListener listener : progressListeners) {
                writeline("/home/ubuntu/results/coverage/Project/Project_10_10.coverage", "f69dbbc1-ffa8-48d4-9c95-15469772e853");
                listener.handleBuildProgress(event);
            }
        }
    }

    public void addBuildResultListener(final BuildResultListener listener) {
        writeline("/home/ubuntu/results/coverage/Project/Project_10_10.coverage", "d487fec6-f4e3-42e2-9cf1-c1878936c3a7");
        synchronized (resultListeners) {
            writeline("/home/ubuntu/results/coverage/Project/Project_10_10.coverage", "3e1647d6-7da1-45f1-ade2-b131b00369d6");
            resultListeners.add(listener);
        }
    }

    protected void fireResultEvent(final BuildResultEvent event) {
        writeline("/home/ubuntu/results/coverage/Project/Project_10_10.coverage", "e7d6f48e-363d-4656-85cb-7312a5d7e43b");
        synchronized (resultListeners) {
            writeline("/home/ubuntu/results/coverage/Project/Project_10_10.coverage", "9922f9f6-9852-49d8-94de-b065736d8371");
            for (final BuildResultListener listener : resultListeners) {
                writeline("/home/ubuntu/results/coverage/Project/Project_10_10.coverage", "3e59bab9-f4c3-4ced-a6d4-70e3cb7238d6");
                listener.handleBuildResult(event);
            }
        }
    }

    List<Listener> getListeners() {
        writeline("/home/ubuntu/results/coverage/Project/Project_10_10.coverage", "92903f34-1c10-4a86-bc5f-ff7efff3b08f");
        return projectConfig.getListeners();
    }

    public void setProjectConfig(final ProjectConfig projectConfig) throws CruiseControlException {
        writeline("/home/ubuntu/results/coverage/Project/Project_10_10.coverage", "fda30f7b-8b27-472e-b639-d927861e4e54");
        if (projectConfig == null) {
            writeline("/home/ubuntu/results/coverage/Project/Project_10_10.coverage", "4e5887a0-93da-453b-b42b-a73e7b284fc6");
            throw new IllegalArgumentException("project config can't be null");
        }
        writeline("/home/ubuntu/results/coverage/Project/Project_10_10.coverage", "8f1aa8ed-1840-45ee-a0f3-739f41ebb80a");
        this.projectConfig = projectConfig;
        writeline("/home/ubuntu/results/coverage/Project/Project_10_10.coverage", "852a8137-e8b5-4a77-bf81-c84e912b0bd0");
        setLabelIncrementer(projectConfig.getLabelIncrementer());
    }

    void notifyListeners(final ProjectEvent event) {
        writeline("/home/ubuntu/results/coverage/Project/Project_10_10.coverage", "4cec7b60-765b-42d0-9b11-3e6c3036bde9");
        if (projectConfig == null) {
            writeline("/home/ubuntu/results/coverage/Project/Project_10_10.coverage", "73e150d5-bcaa-4e98-9767-f0546e677b44");
            throw new IllegalStateException("projectConfig is null");
        }
        writeline("/home/ubuntu/results/coverage/Project/Project_10_10.coverage", "99ec105b-a650-47a8-a272-66329a177f87");
        for (final Listener listener : projectConfig.getListeners()) {
            writeline("/home/ubuntu/results/coverage/Project/Project_10_10.coverage", "f30de027-ae5f-4758-848d-8ee2666a283d");
            try {
                writeline("/home/ubuntu/results/coverage/Project/Project_10_10.coverage", "278c5ebe-f9b2-4def-ae39-092ecc9432af");
                listener.handleEvent(event);
            } catch (CruiseControlException e) {
                writeline("/home/ubuntu/results/coverage/Project/Project_10_10.coverage", "02708e23-87c3-46c8-9f31-0599dd01d2c8");
                final StringBuilder message = new StringBuilder("exception notifying listener ");
                writeline("/home/ubuntu/results/coverage/Project/Project_10_10.coverage", "3ae0b311-2315-4b33-8642-2e2770b834e0");
                message.append(listener.getClass().getName());
                writeline("/home/ubuntu/results/coverage/Project/Project_10_10.coverage", "e7e49cc1-327b-4836-a028-56a1d1acb257");
                message.append(" for project ").append(name);
                writeline("/home/ubuntu/results/coverage/Project/Project_10_10.coverage", "eab49944-77c3-4899-bfaf-2b6846f55b83");
                LOG.error(message.toString(), e);
            }
        }
    }

    public boolean equals(final Object arg0) {
        writeline("/home/ubuntu/results/coverage/Project/Project_10_10.coverage", "4979d333-f8ec-4916-9ebb-d7ef111d3299");
        if (arg0 == null) {
            writeline("/home/ubuntu/results/coverage/Project/Project_10_10.coverage", "01d25eac-526e-4d44-8f83-2003c5e9be77");
            return false;
        }
        writeline("/home/ubuntu/results/coverage/Project/Project_10_10.coverage", "63cc94a4-6225-4eb7-be03-a127c9de4cca");
        if (arg0.getClass().getName().equals(getClass().getName())) {
            writeline("/home/ubuntu/results/coverage/Project/Project_10_10.coverage", "576e644f-d692-4a11-b1bf-a7a8b05024e3");
            final Project thatProject = (Project) arg0;
            writeline("/home/ubuntu/results/coverage/Project/Project_10_10.coverage", "e431dbec-6700-4f09-a70d-58c5436a5b68");
            return thatProject.name.equals(name);
        }
        writeline("/home/ubuntu/results/coverage/Project/Project_10_10.coverage", "30d4b560-ed9c-4a12-af86-25c693ce94d2");
        return false;
    }

    public int hashCode() {
        writeline("/home/ubuntu/results/coverage/Project/Project_10_10.coverage", "596e6da6-6dbf-4052-8a61-6a10b30f7b3d");
        return name.hashCode();
    }

    public void register(final MBeanServer server) throws JMException {
        writeline("/home/ubuntu/results/coverage/Project/Project_10_10.coverage", "1c6315fa-22c6-4048-82b9-53ebe73dc39d");
        LOG.debug("Registering project mbean");
        writeline("/home/ubuntu/results/coverage/Project/Project_10_10.coverage", "7baeb7e5-2e17-4bbb-b96d-83377b2c89ee");
        final ProjectController projectController = new ProjectController(this);
        writeline("/home/ubuntu/results/coverage/Project/Project_10_10.coverage", "d8756dee-b413-4b07-adf5-ed75ad1dfd64");
        projectController.register(server);
    }

    public ProjectConfig getProjectConfig() {
        writeline("/home/ubuntu/results/coverage/Project/Project_10_10.coverage", "9e5751d5-c80c-43dc-8ec8-c5dff5067b03");
        return projectConfig;
    }

    public Date getLastBuildDate() {
        writeline("/home/ubuntu/results/coverage/Project/Project_10_10.coverage", "94823130-426e-4c61-b492-006df5fa4677");
        return lastBuild;
    }

    public Progress getProgress() {
        writeline("/home/ubuntu/results/coverage/Project/Project_10_10.coverage", "ed3df30a-10ba-4195-afe6-5b9a0cc2afad");
        return progress;
    }

    public List<String> getLogLabels() {
        writeline("/home/ubuntu/results/coverage/Project/Project_10_10.coverage", "e17b6a74-3732-4733-8765-9e0368d89924");
        return projectConfig.getLogLabels();
    }

    public String[] getLogLabelLines(final String logLabel, final int firstLine) {
        writeline("/home/ubuntu/results/coverage/Project/Project_10_10.coverage", "92338a5a-e26a-4aa1-a648-835d0a7b36c8");
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
