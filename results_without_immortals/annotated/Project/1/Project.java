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
        writeline("/home/ubuntu/results/coverage/Project/Project_1_10.coverage", "1f92496b-0eef-4f1a-ad44-b81570683749");
        state = ProjectState.STOPPED;
        writeline("/home/ubuntu/results/coverage/Project/Project_1_10.coverage", "35f9cb56-1cd3-4296-823b-aeb7e8d3c4a7");
        pausedMutex = new Object();
        writeline("/home/ubuntu/results/coverage/Project/Project_1_10.coverage", "12448f6e-237d-4e8d-bebc-12b685b18676");
        scheduleMutex = new Object();
        writeline("/home/ubuntu/results/coverage/Project/Project_1_10.coverage", "746b9e31-b635-49ea-a219-5476f1169fcd");
        waitMutex = new Object();
        writeline("/home/ubuntu/results/coverage/Project/Project_1_10.coverage", "1857a529-4d82-4122-9189-d46cf69efa2d");
        progressListeners = new ArrayList<BuildProgressListener>();
        writeline("/home/ubuntu/results/coverage/Project/Project_1_10.coverage", "3788b206-5429-4050-a0f2-e2ef90792477");
        resultListeners = new ArrayList<BuildResultListener>();
        writeline("/home/ubuntu/results/coverage/Project/Project_1_10.coverage", "20fed567-ac5f-4dd3-a4ad-2896ce821a1a");
        progress = new ProgressImpl(this);
    }

    private void readObject(final ObjectInputStream stream) throws IOException, ClassNotFoundException {
        writeline("/home/ubuntu/results/coverage/Project/Project_1_10.coverage", "6d619169-aca0-44a6-b47c-ebdac0cef393");
        stream.defaultReadObject();
        writeline("/home/ubuntu/results/coverage/Project/Project_1_10.coverage", "aa241407-457c-4afd-87a2-f61f85f13c1a");
        initializeTransientFields();
    }

    public void execute() {
        writeline("/home/ubuntu/results/coverage/Project/Project_1_10.coverage", "627b4dcd-a154-4141-ac37-5b7b85a923a3");
        if (stopped) {
            writeline("/home/ubuntu/results/coverage/Project/Project_1_10.coverage", "b362bd3b-b2a3-4965-907b-1b5c8bc09423");
            LOG.warn("not building project " + name + " because project has been stopped.");
            writeline("/home/ubuntu/results/coverage/Project/Project_1_10.coverage", "834b27d5-42e1-42a8-a026-d9cffbfb947d");
            buildFinished();
            writeline("/home/ubuntu/results/coverage/Project/Project_1_10.coverage", "fd10b19e-84fc-4e3f-8836-9b1475cc9a5d");
            return;
        }
        writeline("/home/ubuntu/results/coverage/Project/Project_1_10.coverage", "d9f79e27-5909-4ac2-b88b-9f3de749eed6");
        synchronized (pausedMutex) {
            writeline("/home/ubuntu/results/coverage/Project/Project_1_10.coverage", "39ddfff0-c298-4167-81ab-6d40348f36a0");
            if (isPaused) {
                writeline("/home/ubuntu/results/coverage/Project/Project_1_10.coverage", "6d45b290-693e-4111-a926-b9ceafba6b12");
                LOG.info("not building project " + name + " because project has been paused.");
                writeline("/home/ubuntu/results/coverage/Project/Project_1_10.coverage", "a427e136-e6e4-443d-a2f1-3a946238f9cc");
                buildFinished();
                writeline("/home/ubuntu/results/coverage/Project/Project_1_10.coverage", "4d2d12b0-42c4-4da5-bb98-34243623c3ac");
                return;
            }
        }
        writeline("/home/ubuntu/results/coverage/Project/Project_1_10.coverage", "6459eda3-f532-48ed-8ff8-c184cfb750f1");
        try {
            writeline("/home/ubuntu/results/coverage/Project/Project_1_10.coverage", "88e99b54-51ca-450e-b248-d3c1f794440e");
            init();
            writeline("/home/ubuntu/results/coverage/Project/Project_1_10.coverage", "7dbf30ce-c8d3-4c85-822c-c7715d4fa1ce");
            build();
        } catch (CruiseControlException e) {
            writeline("/home/ubuntu/results/coverage/Project/Project_1_10.coverage", "9eeaed80-77e6-4445-94a9-8a27d0711d45");
            LOG.error("exception attempting build in project " + name, e);
        } finally {
            writeline("/home/ubuntu/results/coverage/Project/Project_1_10.coverage", "97b586d7-52cc-436c-9213-c3f55a961659");
            buildFinished();
        }
    }

    /**
     * Unless paused, runs any bootstrappers and then the entire build.
     * @throws CruiseControlException if an error occurs during the build
     */
    protected void build() throws CruiseControlException {
        writeline("/home/ubuntu/results/coverage/Project/Project_1_10.coverage", "9cc029ef-d330-4f8a-89cb-c887d1808e22");
        if (projectConfig == null) {
            writeline("/home/ubuntu/results/coverage/Project/Project_1_10.coverage", "c78d39e3-734b-494f-8e2f-45c022ded0d3");
            throw new IllegalStateException("projectConfig must be set on project before calling build()");
        }
        writeline("/home/ubuntu/results/coverage/Project/Project_1_10.coverage", "c1bccec4-36b6-4cea-9f4b-11b7ff9f8c7f");
        if (stopped) {
            writeline("/home/ubuntu/results/coverage/Project/Project_1_10.coverage", "1b12e055-726c-4ec3-879b-8ddc018e2e56");
            LOG.warn("not building project " + name + " because project has been stopped.");
            writeline("/home/ubuntu/results/coverage/Project/Project_1_10.coverage", "3f8bcb08-78f7-46d5-91fb-2b15975ed558");
            return;
        }
        writeline("/home/ubuntu/results/coverage/Project/Project_1_10.coverage", "fd5bc604-01f6-4cec-a07a-9f0e48a07fe1");
        // or if the last build faild and we want to build on failures
        if (forceOnly && !buildForced && !(!wasLastBuildSuccessful && buildAfterFailed)) {
            writeline("/home/ubuntu/results/coverage/Project/Project_1_10.coverage", "8ba7fd79-6d63-4b4c-9a3a-a12e9dd249c0");
            info("not building because project is forceOnly and build not forced.");
            writeline("/home/ubuntu/results/coverage/Project/Project_1_10.coverage", "3c898466-dceb-4d7a-a07a-f98a6a2b080e");
            return;
        }
        writeline("/home/ubuntu/results/coverage/Project/Project_1_10.coverage", "a980d6fa-f387-4c0f-8bb2-ccdb75d7c972");
        final boolean buildWasForced = buildForced;
        writeline("/home/ubuntu/results/coverage/Project/Project_1_10.coverage", "70f56ce0-2405-4a09-b6f0-b7b7d083503f");
        try {
            writeline("/home/ubuntu/results/coverage/Project/Project_1_10.coverage", "e112b42a-62ff-43dc-a7f8-ad4abc6121a3");
            setBuildStartTime(new Date());
            writeline("/home/ubuntu/results/coverage/Project/Project_1_10.coverage", "b88559ec-25bb-4ca0-85b4-862e33b228ec");
            final Schedule schedule = projectConfig.getSchedule();
            writeline("/home/ubuntu/results/coverage/Project/Project_1_10.coverage", "a4f7110d-d10d-4479-9f32-9bca7388a334");
            if (schedule == null) {
                writeline("/home/ubuntu/results/coverage/Project/Project_1_10.coverage", "746cf3d6-515a-4f69-809a-aebda8caa111");
                throw new IllegalStateException("project must have a schedule");
            }
            writeline("/home/ubuntu/results/coverage/Project/Project_1_10.coverage", "7b878457-a968-4c46-b35d-e9a42272b47f");
            if (schedule.isPaused(buildStartTime)) {
                writeline("/home/ubuntu/results/coverage/Project/Project_1_10.coverage", "ccf6ede5-3579-4c59-9be4-df044670339c");
                // is different than ProjectState.PAUSED
                return;
            }
            writeline("/home/ubuntu/results/coverage/Project/Project_1_10.coverage", "f83523b2-f416-48b0-96d5-b597259e7055");
            // @todo Add Progress param to Bootstrapper API?
            bootstrap();
            writeline("/home/ubuntu/results/coverage/Project/Project_1_10.coverage", "7b195888-f210-4742-8302-3732761d2641");
            final String target = useAndResetBuildTargetIfBuildWasForced(buildWasForced);
            writeline("/home/ubuntu/results/coverage/Project/Project_1_10.coverage", "5c5ae7ad-5d9c-4011-8a9f-53aaa9b4384d");
            // @todo Add Progress param to ModificationSet API?
            // getModifications will only return null if we don't need to build
            final Element modifications = getModifications(buildWasForced);
            writeline("/home/ubuntu/results/coverage/Project/Project_1_10.coverage", "04a9c59b-3dc7-4de1-8d50-66ae0dc19862");
            if (modifications == null) {
                writeline("/home/ubuntu/results/coverage/Project/Project_1_10.coverage", "d9b9f365-dec3-4ae7-9e12-0c1f53d6b823");
                return;
            }
            writeline("/home/ubuntu/results/coverage/Project/Project_1_10.coverage", "49c393dd-7c9b-4e93-90c8-93731e43d235");
            // Using local reference to avoid NPE if config.xml is updated during build
            final Log buildLog = projectConfig.getLog();
            writeline("/home/ubuntu/results/coverage/Project/Project_1_10.coverage", "663cdd34-580f-43f5-abf9-1476d977435f");
            buildLog.addContent(modifications);
            writeline("/home/ubuntu/results/coverage/Project/Project_1_10.coverage", "430562f8-763c-43bf-a903-3933b8c6b2dd");
            final Date now;
            writeline("/home/ubuntu/results/coverage/Project/Project_1_10.coverage", "0a4d43be-ea3b-4bb4-838d-e6aa9d6af38d");
            if (projectConfig.getModificationSet() != null && projectConfig.getModificationSet().getTimeOfCheck() != null) {
                writeline("/home/ubuntu/results/coverage/Project/Project_1_10.coverage", "9c0cc01d-1c12-48cf-97ad-a0438a84660e");
                now = projectConfig.getModificationSet().getTimeOfCheck();
            } else {
                writeline("/home/ubuntu/results/coverage/Project/Project_1_10.coverage", "e1dbe251-ed7c-44d5-84a6-c77ed789a13b");
                now = new Date();
            }
            writeline("/home/ubuntu/results/coverage/Project/Project_1_10.coverage", "dc818d90-8a7c-43b2-aa0b-758f08a5346d");
            if (getLabelIncrementer().isPreBuildIncrementer()) {
                writeline("/home/ubuntu/results/coverage/Project/Project_1_10.coverage", "a009baf1-18a2-43e9-a752-87ab41b53d07");
                label = getLabelIncrementer().incrementLabel(label, buildLog.getContent());
            }
            writeline("/home/ubuntu/results/coverage/Project/Project_1_10.coverage", "663e90e2-2a51-4462-84f8-9b1923b5f383");
            // collect project information
            buildLog.addContent(getProjectPropertiesElement(now));
            writeline("/home/ubuntu/results/coverage/Project/Project_1_10.coverage", "d09ab51f-9cb1-45c4-9c66-7ca156fff6b7");
            setState(ProjectState.BUILDING);
            writeline("/home/ubuntu/results/coverage/Project/Project_1_10.coverage", "a8915a51-99bd-4ae7-b5fe-a5f2ac40548e");
            final Element builderLog = schedule.build(buildCounter, lastBuild, now, getProjectPropertiesMap(now), target, progress);
            writeline("/home/ubuntu/results/coverage/Project/Project_1_10.coverage", "d503036b-daaf-414b-af02-227bc0b94cc3");
            buildLog.addContent(builderLog.detach());
            writeline("/home/ubuntu/results/coverage/Project/Project_1_10.coverage", "ef8e8721-a8b6-498c-b8c2-9828bf34aa3a");
            boolean buildSuccessful = buildLog.wasBuildSuccessful();
            writeline("/home/ubuntu/results/coverage/Project/Project_1_10.coverage", "cde2f53b-2123-435b-bb9d-eba7f0a9f7b6");
            fireResultEvent(new BuildResultEvent(this, buildSuccessful));
            writeline("/home/ubuntu/results/coverage/Project/Project_1_10.coverage", "8b56449a-db7a-4859-a31c-eaacafe2ab72");
            if (!getLabelIncrementer().isPreBuildIncrementer() && buildSuccessful) {
                writeline("/home/ubuntu/results/coverage/Project/Project_1_10.coverage", "c1fcac3c-2b4e-4f3d-8e64-f49f4b272883");
                label = getLabelIncrementer().incrementLabel(label, buildLog.getContent());
            }
            writeline("/home/ubuntu/results/coverage/Project/Project_1_10.coverage", "b4555cfe-1395-4b51-b009-88d62400ed8f");
            setState(ProjectState.MERGING_LOGS);
            writeline("/home/ubuntu/results/coverage/Project/Project_1_10.coverage", "90729795-684c-4353-9fb1-6665711845cd");
            buildLog.writeLogFile(now);
            writeline("/home/ubuntu/results/coverage/Project/Project_1_10.coverage", "3d8c6a5a-5479-4b7d-87cb-88338c11213d");
            // regardless of success or failure (buildAfterFailed = false in config.xml)
            if (!buildAfterFailed) {
                writeline("/home/ubuntu/results/coverage/Project/Project_1_10.coverage", "848e7c64-0a98-4a5e-8db8-9cac54243185");
                lastBuild = now;
            }
            writeline("/home/ubuntu/results/coverage/Project/Project_1_10.coverage", "97bc501d-6f88-446d-b1a6-124a1ee7d13f");
            // If this was a successful build, update both last build and last successful build
            if (buildSuccessful) {
                writeline("/home/ubuntu/results/coverage/Project/Project_1_10.coverage", "107f476e-c121-48ff-921d-5afc36530d0d");
                lastBuild = now;
                writeline("/home/ubuntu/results/coverage/Project/Project_1_10.coverage", "f5b798b6-2392-4f73-b987-39bd33aba1c9");
                lastSuccessfulBuild = now;
                writeline("/home/ubuntu/results/coverage/Project/Project_1_10.coverage", "13c36ceb-b1f6-4170-9a78-3b52ec85edec");
                info("build successful");
            } else {
                writeline("/home/ubuntu/results/coverage/Project/Project_1_10.coverage", "cf03ce86-08a7-4836-9fef-3166e2229554");
                info("build failed");
            }
            writeline("/home/ubuntu/results/coverage/Project/Project_1_10.coverage", "420e96fb-c909-4142-8a57-e285534294ce");
            buildCounter++;
            writeline("/home/ubuntu/results/coverage/Project/Project_1_10.coverage", "3c8e35ba-cea3-4e3e-a5ec-eb533374a12d");
            setWasLastBuildSuccessful(buildSuccessful);
            writeline("/home/ubuntu/results/coverage/Project/Project_1_10.coverage", "8092ea8f-8010-4787-9eeb-abdd621b12e6");
            // also need to reset forced flag before serializing, unless buildForced var is transient
            // resetBuildForcedOnlyIfBuildWasForced(buildWasForced);
            serializeProject();
            writeline("/home/ubuntu/results/coverage/Project/Project_1_10.coverage", "f60bc070-3d74-4287-af8a-7ccfd2d37522");
            // @todo Add Progress param to Publisher API?
            publish(buildLog);
            writeline("/home/ubuntu/results/coverage/Project/Project_1_10.coverage", "ffb74a5f-461d-49ab-91b5-c844c1f257ac");
            buildLog.reset();
        } finally {
            writeline("/home/ubuntu/results/coverage/Project/Project_1_10.coverage", "c8b08491-23d3-48f9-9e65-1e2d56fdc18a");
            resetBuildForcedOnlyIfBuildWasForced(buildWasForced);
            writeline("/home/ubuntu/results/coverage/Project/Project_1_10.coverage", "de71f288-13f6-41a8-8a63-be5d11e3fb62");
            setState(ProjectState.IDLE);
        }
    }

    private String useAndResetBuildTargetIfBuildWasForced(final boolean buildWasForced) {
        writeline("/home/ubuntu/results/coverage/Project/Project_1_10.coverage", "8390ac39-dfe4-4134-b7e5-e1f9bf257e68");
        String target = null;
        writeline("/home/ubuntu/results/coverage/Project/Project_1_10.coverage", "30a37038-28cc-46a6-97e5-31ec4f47f6ce");
        if (buildWasForced) {
            writeline("/home/ubuntu/results/coverage/Project/Project_1_10.coverage", "1497c459-a047-4660-825f-b36a98b8e3c5");
            target = buildTarget;
            writeline("/home/ubuntu/results/coverage/Project/Project_1_10.coverage", "7e7bb993-7785-4891-b34e-557be9e7c3b5");
            buildTarget = null;
        }
        writeline("/home/ubuntu/results/coverage/Project/Project_1_10.coverage", "6abdf473-b8d0-44fe-adca-0bcf463465ac");
        return target;
    }

    private void resetBuildForcedOnlyIfBuildWasForced(final boolean buildWasForced) {
        writeline("/home/ubuntu/results/coverage/Project/Project_1_10.coverage", "6b4abebd-3fd3-4d06-aa7c-16c4640650b3");
        if (buildWasForced) {
            writeline("/home/ubuntu/results/coverage/Project/Project_1_10.coverage", "3a011a08-30a2-4d39-ae71-89270503b6dd");
            buildForced = false;
        }
    }

    void setBuildStartTime(final Date date) {
        writeline("/home/ubuntu/results/coverage/Project/Project_1_10.coverage", "6f2b3490-2892-4aba-8076-f37a951b90bf");
        buildStartTime = date;
    }

    public void run() {
        writeline("/home/ubuntu/results/coverage/Project/Project_1_10.coverage", "9c34739b-ae1c-498b-acf7-38b9b10d1ff2");
        LOG.info("Project " + name + " started");
        writeline("/home/ubuntu/results/coverage/Project/Project_1_10.coverage", "03e20c94-6374-4f1d-b92a-fcfdf607e7de");
        try {
            writeline("/home/ubuntu/results/coverage/Project/Project_1_10.coverage", "fa6e49f5-40d6-4aa9-abce-4a443c097995");
            while (!stopped) {
                writeline("/home/ubuntu/results/coverage/Project/Project_1_10.coverage", "f9dde111-1fe4-4830-a0e9-69c46b166489");
                try {
                    writeline("/home/ubuntu/results/coverage/Project/Project_1_10.coverage", "ae0aa725-ba3f-4112-851a-133310d7192d");
                    waitIfPaused();
                    writeline("/home/ubuntu/results/coverage/Project/Project_1_10.coverage", "a75944e8-79bf-43ab-a6e0-152eafa1bbd8");
                    if (!stopped) {
                        writeline("/home/ubuntu/results/coverage/Project/Project_1_10.coverage", "ae2fc960-9a72-4633-844d-e94a285c2254");
                        waitForNextBuild();
                    }
                    writeline("/home/ubuntu/results/coverage/Project/Project_1_10.coverage", "ef73273d-233d-4308-9451-f7450cccff7b");
                    if (!stopped) {
                        writeline("/home/ubuntu/results/coverage/Project/Project_1_10.coverage", "22bbb99f-ddfd-4b16-b550-f13760b6f925");
                        setState(ProjectState.QUEUED);
                        writeline("/home/ubuntu/results/coverage/Project/Project_1_10.coverage", "754e7e2a-036d-47c2-bc76-4a9276d69ed2");
                        synchronized (scheduleMutex) {
                            writeline("/home/ubuntu/results/coverage/Project/Project_1_10.coverage", "ebd413e3-3547-4147-9bf1-21124eeb93e7");
                            queue.requestBuild(projectConfig);
                            writeline("/home/ubuntu/results/coverage/Project/Project_1_10.coverage", "8d2f0755-3754-4906-b608-05a187cadb8e");
                            waitForBuildToFinish();
                        }
                    }
                } catch (InterruptedException e) {
                    writeline("/home/ubuntu/results/coverage/Project/Project_1_10.coverage", "344408f2-0d25-46cf-be32-7e890cdb77ce");
                    final String message = "Project " + name + ".run() interrupted";
                    writeline("/home/ubuntu/results/coverage/Project/Project_1_10.coverage", "4be14248-b7e7-4421-8bd7-18be31a58f7b");
                    LOG.error(message, e);
                    writeline("/home/ubuntu/results/coverage/Project/Project_1_10.coverage", "e0e92d44-2861-4753-bb40-1d6a8e8dbc69");
                    throw new RuntimeException(message);
                }
            }
        } finally {
            writeline("/home/ubuntu/results/coverage/Project/Project_1_10.coverage", "9a204ffc-8bf8-4d00-b470-ddae2f7c352c");
            stopped = true;
            writeline("/home/ubuntu/results/coverage/Project/Project_1_10.coverage", "040a2e2f-11c3-4228-89c9-1e7c048fd908");
            LOG.info("Project " + name + " stopped");
        }
    }

    void waitIfPaused() throws InterruptedException {
        writeline("/home/ubuntu/results/coverage/Project/Project_1_10.coverage", "de626bb7-2402-4c23-a1f6-8778428b377e");
        synchronized (pausedMutex) {
            writeline("/home/ubuntu/results/coverage/Project/Project_1_10.coverage", "12a2a418-b2c9-4866-9efd-8ad198c7d05c");
            while (isPaused) {
                writeline("/home/ubuntu/results/coverage/Project/Project_1_10.coverage", "f43b5657-03a7-435b-8357-cec669cfcaf3");
                setState(ProjectState.PAUSED);
                writeline("/home/ubuntu/results/coverage/Project/Project_1_10.coverage", "c944bf02-c212-4e79-a7f9-385a750785fc");
                pausedMutex.wait(10 * DateUtil.ONE_MINUTE);
            }
        }
    }

    void waitForNextBuild() throws InterruptedException {
        writeline("/home/ubuntu/results/coverage/Project/Project_1_10.coverage", "d3e88542-599a-4717-918d-89d1e3590002");
        long waitTime = getTimeToNextBuild(new Date());
        writeline("/home/ubuntu/results/coverage/Project/Project_1_10.coverage", "3635a7ad-b6b4-4052-ad05-0fffe0807cfa");
        if (needToWaitForNextBuild(waitTime) && !buildForced) {
            writeline("/home/ubuntu/results/coverage/Project/Project_1_10.coverage", "94f6ab21-af36-46a9-9ec9-ea43a942806d");
            final String msg = "next build in " + DateUtil.formatTime(waitTime);
            writeline("/home/ubuntu/results/coverage/Project/Project_1_10.coverage", "9030f8e5-8120-4af3-818a-45ab435c50b1");
            info(msg);
            writeline("/home/ubuntu/results/coverage/Project/Project_1_10.coverage", "957dad90-e7be-42f0-8562-56e0b553796c");
            synchronized (waitMutex) {
                writeline("/home/ubuntu/results/coverage/Project/Project_1_10.coverage", "8fd9cdc3-f516-4a48-bd3f-4be07bf9f98d");
                setState(ProjectState.WAITING);
                writeline("/home/ubuntu/results/coverage/Project/Project_1_10.coverage", "7b9616f2-d0fc-4935-8f28-1a14f8a0a081");
                progress.setValue(msg);
                writeline("/home/ubuntu/results/coverage/Project/Project_1_10.coverage", "1ce045a9-70b9-4941-86b2-e7acf7bdc0f9");
                waitMutex.wait(waitTime);
            }
        }
    }

    long getTimeToNextBuild(Date now) {
        writeline("/home/ubuntu/results/coverage/Project/Project_1_10.coverage", "f46977b0-5388-4b1e-baef-66250d998e81");
        long waitTime = projectConfig.getSchedule().getTimeToNextBuild(now, getBuildInterval());
        writeline("/home/ubuntu/results/coverage/Project/Project_1_10.coverage", "b1292ea3-b51f-4fd6-b8eb-ca138ecc2abb");
        if (waitTime == 0) {
            writeline("/home/ubuntu/results/coverage/Project/Project_1_10.coverage", "9e0328bb-8679-43cd-a11c-029c9dfae3c0");
            // project that has just built within a minute time
            if (buildStartTime != null) {
                writeline("/home/ubuntu/results/coverage/Project/Project_1_10.coverage", "82f16cd8-4b62-4477-b2a8-ac6caa3fa861");
                long millisSinceLastBuild = now.getTime() - buildStartTime.getTime();
                writeline("/home/ubuntu/results/coverage/Project/Project_1_10.coverage", "baecc2c6-2536-41d5-9b34-fead1358e239");
                if (millisSinceLastBuild < Schedule.ONE_MINUTE) {
                    writeline("/home/ubuntu/results/coverage/Project/Project_1_10.coverage", "10e3cb57-2a1b-4950-89f8-a021018c5a44");
                    debug("build finished within a minute, getting new time to next build");
                    writeline("/home/ubuntu/results/coverage/Project/Project_1_10.coverage", "2bf4c8cc-4e4a-4ea5-9b13-1fae87df4afb");
                    Date oneMinuteInFuture = new Date(now.getTime() + Schedule.ONE_MINUTE);
                    writeline("/home/ubuntu/results/coverage/Project/Project_1_10.coverage", "c3b72d54-3f76-4e16-b189-e7c5e91d59d2");
                    waitTime = projectConfig.getSchedule().getTimeToNextBuild(oneMinuteInFuture, getBuildInterval());
                    writeline("/home/ubuntu/results/coverage/Project/Project_1_10.coverage", "37d4a4ba-6975-41b9-b478-f683e52bb74f");
                    waitTime += Schedule.ONE_MINUTE;
                }
            }
        }
        writeline("/home/ubuntu/results/coverage/Project/Project_1_10.coverage", "d4a5046b-7e51-4eaf-bfd5-15213d935074");
        return waitTime;
    }

    static boolean needToWaitForNextBuild(long waitTime) {
        writelineStatic("/home/ubuntu/results/coverage/Project/Project_1_10.coverage", "d5fd9bd7-e318-4b74-8f27-b4b3f4a5e9c1");
        return waitTime > 0;
    }

    /**
     * @return true if build was forced, intended for unit testing only.
     */
    boolean isBuildForced() {
        writeline("/home/ubuntu/results/coverage/Project/Project_1_10.coverage", "340d799f-6c9a-424c-a7de-a6a9a2efba6e");
        return buildForced;
    }

    void forceBuild() {
        writeline("/home/ubuntu/results/coverage/Project/Project_1_10.coverage", "7eff462e-69b4-40f6-b2c3-6a638a6e9702");
        synchronized (waitMutex) {
            writeline("/home/ubuntu/results/coverage/Project/Project_1_10.coverage", "2c5ba721-61f2-4d3d-b138-77f45bb8e554");
            waitMutex.notify();
        }
    }

    public void forceBuildWithTarget(String buildTarget) {
        writeline("/home/ubuntu/results/coverage/Project/Project_1_10.coverage", "fb0fb998-3f0e-4515-a304-eb8336227a7d");
        this.buildTarget = buildTarget;
        writeline("/home/ubuntu/results/coverage/Project/Project_1_10.coverage", "840c25d1-ac1f-4e34-8cba-94d3b67c0b8b");
        setBuildForced(true);
    }

    public void forceBuildWithTarget(String buildTarget, Map<String, String> addedProperties) {
        writeline("/home/ubuntu/results/coverage/Project/Project_1_10.coverage", "5518b6ef-3b70-4099-96e8-f0b076d9279b");
        additionalProperties = addedProperties;
        writeline("/home/ubuntu/results/coverage/Project/Project_1_10.coverage", "23941b37-ef80-428c-9d89-7a1b734948e1");
        forceBuildWithTarget(buildTarget);
    }

    void waitForBuildToFinish() throws InterruptedException {
        writeline("/home/ubuntu/results/coverage/Project/Project_1_10.coverage", "b8d489d4-b798-4efb-95f4-6642d7ea89bd");
        synchronized (scheduleMutex) {
            writeline("/home/ubuntu/results/coverage/Project/Project_1_10.coverage", "eabb3a25-710f-4208-8e99-ff8eca16d89b");
            debug("waiting for build to finish");
            writeline("/home/ubuntu/results/coverage/Project/Project_1_10.coverage", "d38604c6-fb4a-4dfb-a722-9067483220bd");
            scheduleMutex.wait();
        }
    }

    void buildFinished() {
        writeline("/home/ubuntu/results/coverage/Project/Project_1_10.coverage", "e5ea2c87-4918-4dca-88d6-4aa2cd2fb2f8");
        synchronized (scheduleMutex) {
            writeline("/home/ubuntu/results/coverage/Project/Project_1_10.coverage", "7b52eaf3-d6a6-4542-b05b-93a96bfbf8dc");
            debug("build finished");
            writeline("/home/ubuntu/results/coverage/Project/Project_1_10.coverage", "9eb37dd3-65eb-4679-9d84-2a1e80b8321b");
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
        writeline("/home/ubuntu/results/coverage/Project/Project_1_10.coverage", "2069dceb-56cd-4ced-bab5-088ecc389ce4");
        setState(ProjectState.MODIFICATIONSET);
        writeline("/home/ubuntu/results/coverage/Project/Project_1_10.coverage", "44cf8a40-7bcc-4d99-a229-6030c7851f24");
        final ModificationSet modificationSet = projectConfig.getModificationSet();
        writeline("/home/ubuntu/results/coverage/Project/Project_1_10.coverage", "81e70c62-b360-400e-a969-00e9b94d9940");
        if (modificationSet == null) {
            writeline("/home/ubuntu/results/coverage/Project/Project_1_10.coverage", "2a0bacdb-1693-42b1-a953-b865cfb130d2");
            debug("no modification set, nothing to detect.");
            writeline("/home/ubuntu/results/coverage/Project/Project_1_10.coverage", "12832ae4-55d5-494b-a597-db3202bc690f");
            if (buildWasForced) {
                writeline("/home/ubuntu/results/coverage/Project/Project_1_10.coverage", "d9b6dd3b-7330-4164-ac44-554ee9532ee3");
                info("no modification set but build was forced");
                writeline("/home/ubuntu/results/coverage/Project/Project_1_10.coverage", "955ca468-367e-47f2-a880-a7044bf75f63");
                return new Element("modifications");
            }
            writeline("/home/ubuntu/results/coverage/Project/Project_1_10.coverage", "6edcff9b-bee3-4fa7-b911-fee71061cd39");
            if (!requiremodification) {
                writeline("/home/ubuntu/results/coverage/Project/Project_1_10.coverage", "cc9ce377-ea76-4feb-bf6a-e600fe67a4b1");
                info("no modification set but no modifications required");
                writeline("/home/ubuntu/results/coverage/Project/Project_1_10.coverage", "5062eb7b-a181-4641-b7e6-e4ade1bdca43");
                return new Element("modifications");
            }
            writeline("/home/ubuntu/results/coverage/Project/Project_1_10.coverage", "06b70913-a2a9-4270-af60-b67a50a5fc4b");
            return null;
        }
        writeline("/home/ubuntu/results/coverage/Project/Project_1_10.coverage", "c1fc1f51-151d-49c3-a89e-3b336caa444e");
        final boolean checkNewChangesFirst = checkOnlySinceLastBuild();
        writeline("/home/ubuntu/results/coverage/Project/Project_1_10.coverage", "a2c3f490-bd79-48ca-9b71-97c325095ec9");
        Element modifications;
        writeline("/home/ubuntu/results/coverage/Project/Project_1_10.coverage", "953f09ee-dea4-4585-a9bf-0e59b0467de4");
        if (checkNewChangesFirst) {
            writeline("/home/ubuntu/results/coverage/Project/Project_1_10.coverage", "9dc2f4e9-ad4e-486b-856b-39c02ec246be");
            debug("getting changes since last build");
            writeline("/home/ubuntu/results/coverage/Project/Project_1_10.coverage", "285066a2-50db-4306-88ba-16f1b3fb4aff");
            modifications = modificationSet.retrieveModificationsAsElement(lastBuild, progress);
        } else {
            writeline("/home/ubuntu/results/coverage/Project/Project_1_10.coverage", "d0f9b99c-450b-48b7-abbf-9d0fd2e9d041");
            debug("getting changes since last successful build");
            writeline("/home/ubuntu/results/coverage/Project/Project_1_10.coverage", "fff38003-36fa-431f-91f7-b7cf951ce5d4");
            modifications = modificationSet.retrieveModificationsAsElement(lastSuccessfulBuild, progress);
        }
        writeline("/home/ubuntu/results/coverage/Project/Project_1_10.coverage", "4130c19e-da4d-45c3-be98-8ca687ed6b83");
        if (!modificationSet.isModified()) {
            writeline("/home/ubuntu/results/coverage/Project/Project_1_10.coverage", "d5104d87-6588-4473-843f-32914e781938");
            info("No modifications found, build not necessary.");
            writeline("/home/ubuntu/results/coverage/Project/Project_1_10.coverage", "43638394-159d-4614-ae58-c16741964e40");
            // * build forced
            if (buildAfterFailed && !wasLastBuildSuccessful) {
                writeline("/home/ubuntu/results/coverage/Project/Project_1_10.coverage", "dd61e8fa-3b4d-4e0f-91a3-bf56c193a2d1");
                info("Building anyway, since buildAfterFailed is true and last build failed.");
            } else if (!requiremodification) {
                writeline("/home/ubuntu/results/coverage/Project/Project_1_10.coverage", "5433745b-d0ec-4c54-be72-9da65485b598");
                info("Building anyway, since modifications not required");
            } else {
                writeline("/home/ubuntu/results/coverage/Project/Project_1_10.coverage", "45cb304a-b733-4178-ba26-bcddb97ed9a2");
                if (buildWasForced) {
                    writeline("/home/ubuntu/results/coverage/Project/Project_1_10.coverage", "18aca447-ccc6-4afe-90f6-5584037be310");
                    info("Building anyway, since build was explicitly forced.");
                } else {
                    writeline("/home/ubuntu/results/coverage/Project/Project_1_10.coverage", "5703cd48-ad88-44fd-93af-df613b4ac893");
                    return null;
                }
            }
        }
        writeline("/home/ubuntu/results/coverage/Project/Project_1_10.coverage", "83cc1164-cf8a-4887-b632-449eedb8f2ed");
        if (checkNewChangesFirst) {
            writeline("/home/ubuntu/results/coverage/Project/Project_1_10.coverage", "f5913f53-548e-4009-97de-7dd365256f6a");
            debug("new changes found; now getting complete set");
            writeline("/home/ubuntu/results/coverage/Project/Project_1_10.coverage", "37dc0de9-cca4-46c8-aaab-6055eb10daf0");
            modifications = modificationSet.retrieveModificationsAsElement(lastSuccessfulBuild, progress);
        }
        writeline("/home/ubuntu/results/coverage/Project/Project_1_10.coverage", "5417e511-5e68-491d-8459-a16991158314");
        return modifications;
    }

    /**
     * @return boolean
     */
    boolean checkOnlySinceLastBuild() {
        writeline("/home/ubuntu/results/coverage/Project/Project_1_10.coverage", "98247cdf-c043-4228-8ad1-82f3cafdf566");
        if (lastBuild == null || lastSuccessfulBuild == null) {
            writeline("/home/ubuntu/results/coverage/Project/Project_1_10.coverage", "0ef8fbf3-59fd-4975-b8ec-cd94732494ae");
            return false;
        }
        writeline("/home/ubuntu/results/coverage/Project/Project_1_10.coverage", "50089bc6-e025-4fd3-a837-f299e168d7fb");
        final long lastBuildLong = lastBuild.getTime();
        writeline("/home/ubuntu/results/coverage/Project/Project_1_10.coverage", "0eeb267b-bdb7-4af2-946b-26a41fd7a311");
        final long timeDifference = lastBuildLong - lastSuccessfulBuild.getTime();
        writeline("/home/ubuntu/results/coverage/Project/Project_1_10.coverage", "3df50b98-f512-4175-af3b-aae64646eac8");
        final boolean moreThanASecond = timeDifference > DateUtil.ONE_SECOND;
        writeline("/home/ubuntu/results/coverage/Project/Project_1_10.coverage", "99670bcd-e955-4e4b-b02a-47d0015b35cc");
        return !buildAfterFailed && moreThanASecond;
    }

    /**
     * Serialize the project to allow resumption after a process bounce
     */
    public void serializeProject() {
        writeline("/home/ubuntu/results/coverage/Project/Project_1_10.coverage", "81a203a3-c066-4e35-873d-4ff8fc1d02ad");
        final String safeProjectName = Builder.getFileSystemSafeProjectName(name);
        writeline("/home/ubuntu/results/coverage/Project/Project_1_10.coverage", "752c5528-97be-432b-bb41-999060fa4a35");
        try {
            writeline("/home/ubuntu/results/coverage/Project/Project_1_10.coverage", "3c96474b-0987-4731-9d14-16ce0dce1637");
            final ObjectOutputStream s = new ObjectOutputStream(new FileOutputStream(safeProjectName + ".ser"));
            writeline("/home/ubuntu/results/coverage/Project/Project_1_10.coverage", "4826d3fc-2aca-4c64-bdfd-739ea2edbcd2");
            try {
                writeline("/home/ubuntu/results/coverage/Project/Project_1_10.coverage", "169440b5-2580-4545-b9ed-c27ac6b73db8");
                s.writeObject(this);
                writeline("/home/ubuntu/results/coverage/Project/Project_1_10.coverage", "8060e2ad-2bbc-44db-86c7-c8eea2be1949");
                s.flush();
                writeline("/home/ubuntu/results/coverage/Project/Project_1_10.coverage", "1f3ddf04-bf9b-4ab0-80ae-69777a27c9ba");
                debug("Serializing project to [" + safeProjectName + ".ser]");
            } finally {
                writeline("/home/ubuntu/results/coverage/Project/Project_1_10.coverage", "9e2a5b3a-c6a4-41fd-a159-6a83b6025c76");
                s.close();
            }
        } catch (Exception e) {
            writeline("/home/ubuntu/results/coverage/Project/Project_1_10.coverage", "135a6ca1-1ed2-4c1f-a87c-950331a85394");
            LOG.warn("Error serializing project to [" + safeProjectName + ".ser]: " + e.getMessage(), e);
        }
    }

    public void setLabelIncrementer(final LabelIncrementer incrementer) throws CruiseControlException {
        writeline("/home/ubuntu/results/coverage/Project/Project_1_10.coverage", "5e7c1c96-62ea-4265-965b-af6a211a4768");
        if (incrementer == null) {
            writeline("/home/ubuntu/results/coverage/Project/Project_1_10.coverage", "e9f332c4-a081-453e-adb5-b9af2d7dd6dc");
            throw new IllegalArgumentException("label incrementer can't be null");
        }
        writeline("/home/ubuntu/results/coverage/Project/Project_1_10.coverage", "eed0a1b5-f443-4849-98fa-497a1e477b92");
        labelIncrementer = incrementer;
        writeline("/home/ubuntu/results/coverage/Project/Project_1_10.coverage", "7eb49c52-dffa-4754-bbf5-b7735105e0d3");
        if (label == null) {
            writeline("/home/ubuntu/results/coverage/Project/Project_1_10.coverage", "d1bdc828-0bfc-4775-89de-6b420f107464");
            label = labelIncrementer.getDefaultLabel();
        }
        writeline("/home/ubuntu/results/coverage/Project/Project_1_10.coverage", "9874409e-c68c-4ecb-910c-586c31d273a2");
        validateLabel(label, labelIncrementer);
    }

    public LabelIncrementer getLabelIncrementer() {
        writeline("/home/ubuntu/results/coverage/Project/Project_1_10.coverage", "beb0371f-8527-44c2-af76-62e564cfb52f");
        return labelIncrementer;
    }

    public void setName(final String projectName) {
        writeline("/home/ubuntu/results/coverage/Project/Project_1_10.coverage", "ee7d6686-1904-4781-9e71-25f7e4180a9c");
        name = projectName;
    }

    public String getName() {
        writeline("/home/ubuntu/results/coverage/Project/Project_1_10.coverage", "18dd0c04-441b-4979-8fef-20e72d117834");
        return name;
    }

    public void setLabel(final String newLabel) {
        writeline("/home/ubuntu/results/coverage/Project/Project_1_10.coverage", "be43f91f-add2-4ae0-8a43-da619223258a");
        label = newLabel;
    }

    public String getLabel() {
        writeline("/home/ubuntu/results/coverage/Project/Project_1_10.coverage", "3d935d73-8aa0-4130-a7d8-9f343d83f0b4");
        return label;
    }

    /**
     * @param newLastBuild string containing the build date in the format
     * yyyyMMddHHmmss
     * @throws CruiseControlException if the date cannot be extracted from the
     * input string
     */
    public void setLastBuild(final String newLastBuild) throws CruiseControlException {
        writeline("/home/ubuntu/results/coverage/Project/Project_1_10.coverage", "25b3c0f8-553f-4781-bb1c-e64e45058a7f");
        lastBuild = DateUtil.parseFormattedTime(newLastBuild, "lastBuild");
    }

    /**
     * @param newLastSuccessfulBuild string containing the build date in the format
     * yyyyMMddHHmmss
     * @throws CruiseControlException if the date cannot be extracted from the
     * input string
     */
    public void setLastSuccessfulBuild(final String newLastSuccessfulBuild) throws CruiseControlException {
        writeline("/home/ubuntu/results/coverage/Project/Project_1_10.coverage", "b61e0570-b331-4266-8024-3d14c69bcf7e");
        lastSuccessfulBuild = DateUtil.parseFormattedTime(newLastSuccessfulBuild, "lastSuccessfulBuild");
    }

    public String getLastBuild() {
        writeline("/home/ubuntu/results/coverage/Project/Project_1_10.coverage", "9d4530c0-d7a5-4810-9140-24b872f492b5");
        if (lastBuild == null) {
            writeline("/home/ubuntu/results/coverage/Project/Project_1_10.coverage", "26560676-ff50-42d0-84d6-06958dcda663");
            return null;
        }
        writeline("/home/ubuntu/results/coverage/Project/Project_1_10.coverage", "bc9dbf7a-6a90-4fb3-8b58-909deb4df6f7");
        return DateUtil.getFormattedTime(lastBuild);
    }

    public boolean getBuildForced() {
        writeline("/home/ubuntu/results/coverage/Project/Project_1_10.coverage", "768dcf62-7993-421a-b4c6-7bb1d1b47ab4");
        return buildForced;
    }

    public void setBuildForced(boolean forceNewBuildNow) {
        writeline("/home/ubuntu/results/coverage/Project/Project_1_10.coverage", "b1315e06-fd07-4835-a82e-5b460136c458");
        buildForced = forceNewBuildNow;
        writeline("/home/ubuntu/results/coverage/Project/Project_1_10.coverage", "d4b94d02-a703-4aba-87f4-244d7a565a4e");
        if (forceNewBuildNow) {
            writeline("/home/ubuntu/results/coverage/Project/Project_1_10.coverage", "60f3afed-3743-4d02-bfe3-f5325f3bbabd");
            forceBuild();
        }
    }

    public String getLastSuccessfulBuild() {
        writeline("/home/ubuntu/results/coverage/Project/Project_1_10.coverage", "bb5b76f6-9211-4e76-a993-2eee508d0bba");
        if (lastSuccessfulBuild == null) {
            writeline("/home/ubuntu/results/coverage/Project/Project_1_10.coverage", "fd76fdb0-94cd-4776-8695-1d74b795c287");
            return null;
        }
        writeline("/home/ubuntu/results/coverage/Project/Project_1_10.coverage", "c9a7ff46-9551-4e55-b955-0aa1e2f062e5");
        return DateUtil.getFormattedTime(lastSuccessfulBuild);
    }

    public String getLogDir() {
        writeline("/home/ubuntu/results/coverage/Project/Project_1_10.coverage", "b33e951b-ad7c-4335-b17a-9712b4b35cf3");
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
        writeline("/home/ubuntu/results/coverage/Project/Project_1_10.coverage", "a5fd63ce-195f-48c6-8c1a-e5f0ad460ad6");
        if (overrideBuildInterval == null) {
            writeline("/home/ubuntu/results/coverage/Project/Project_1_10.coverage", "a9b05e19-5a64-48fb-86be-3032a62e5511");
            return projectConfig.getSchedule().getInterval();
        } else {
            writeline("/home/ubuntu/results/coverage/Project/Project_1_10.coverage", "d0fd8351-51e9-4de1-9ed2-069d44ddbdf8");
            return overrideBuildInterval;
        }
    }

    /**
     * Sets the build interval that this Project should use. This method
     * overrides the value initially specified in the Schedule attribute.
     * @param sleepMillis the number of milliseconds to sleep between build attempts
     */
    public void overrideBuildInterval(final long sleepMillis) {
        writeline("/home/ubuntu/results/coverage/Project/Project_1_10.coverage", "e7dced23-0b4c-4479-84ca-b1f7fc6195c0");
        overrideBuildInterval = sleepMillis;
    }

    public boolean isPaused() {
        writeline("/home/ubuntu/results/coverage/Project/Project_1_10.coverage", "d689817c-107a-4399-84b1-6170b063c711");
        return isPaused;
    }

    public void setPaused(final boolean paused) {
        writeline("/home/ubuntu/results/coverage/Project/Project_1_10.coverage", "6c0e22f4-7bb3-45c0-a873-1c3934e158e9");
        synchronized (pausedMutex) {
            writeline("/home/ubuntu/results/coverage/Project/Project_1_10.coverage", "523e376e-34c4-485f-b9d6-2cb27aac58d1");
            if (isPaused && !paused) {
                writeline("/home/ubuntu/results/coverage/Project/Project_1_10.coverage", "134a2285-b279-44d5-b9ba-3cf0647ec701");
                pausedMutex.notifyAll();
            }
            writeline("/home/ubuntu/results/coverage/Project/Project_1_10.coverage", "f1f32d8b-3cfe-46f0-b308-53721320ac6f");
            isPaused = paused;
        }
    }

    public void setBuildAfterFailed(final boolean rebuildEvenWithNoNewModifications) {
        writeline("/home/ubuntu/results/coverage/Project/Project_1_10.coverage", "e092693a-4036-42d8-8894-b51a469f9630");
        buildAfterFailed = rebuildEvenWithNoNewModifications;
    }

    public String getStatus() {
        writeline("/home/ubuntu/results/coverage/Project/Project_1_10.coverage", "1f88a89a-807f-4c40-8645-bbb9fb472aa1");
        return getState().getDescription();
    }

    public String getStatusWithQueuePosition() {
        writeline("/home/ubuntu/results/coverage/Project/Project_1_10.coverage", "41d786e3-dc29-4005-a6f9-9c77051dddde");
        if (ProjectState.QUEUED.equals(getState())) {
            writeline("/home/ubuntu/results/coverage/Project/Project_1_10.coverage", "dc4cb1d5-3370-4f6c-939e-39f76f8d9449");
            return getState().getDescription() + " - " + queue.findPosition(projectConfig);
        } else {
            writeline("/home/ubuntu/results/coverage/Project/Project_1_10.coverage", "7f49b3e9-0508-40c8-a2bd-ac732ffbf501");
            return getState().getDescription();
        }
    }

    public ProjectState getState() {
        writeline("/home/ubuntu/results/coverage/Project/Project_1_10.coverage", "f237c5dd-8a7e-4503-a794-508a5448479d");
        return state;
    }

    private void setState(final ProjectState newState) {
        writeline("/home/ubuntu/results/coverage/Project/Project_1_10.coverage", "ec173a7a-4c18-4b8e-8db5-ad476073f45c");
        state = newState;
        writeline("/home/ubuntu/results/coverage/Project/Project_1_10.coverage", "3509511c-5ba4-45b0-a8a2-2f927b61c4f4");
        info(getStatus());
        writeline("/home/ubuntu/results/coverage/Project/Project_1_10.coverage", "9a6f57cb-1687-49a9-aad8-29f5eaa7295f");
        notifyListeners(new ProjectStateChangedEvent(name, getState()));
        writeline("/home/ubuntu/results/coverage/Project/Project_1_10.coverage", "b7a54579-722c-4997-804d-e957689261b6");
        fireProgressEvent(new BuildProgressEvent(this, getState()));
    }

    public void setBuildQueue(final BuildQueue buildQueue) {
        writeline("/home/ubuntu/results/coverage/Project/Project_1_10.coverage", "2a3ef10a-cd80-48de-a4d9-fc773ea969af");
        queue = buildQueue;
    }

    public String getBuildStartTime() {
        writeline("/home/ubuntu/results/coverage/Project/Project_1_10.coverage", "7b15c12f-8e32-4ff9-81ef-3eeedb1bcc16");
        return DateUtil.getFormattedTime(buildStartTime);
    }

    public Log getLog() {
        writeline("/home/ubuntu/results/coverage/Project/Project_1_10.coverage", "6ded0bc4-7f05-4b63-86d2-82c0092420e1");
        return this.projectConfig.getLog();
    }

    /**
     * Initialize the project. Uses ProjectXMLHelper to parse a project file.
     */
    protected void init() {
        writeline("/home/ubuntu/results/coverage/Project/Project_1_10.coverage", "fae3c3c7-b3e3-401c-a655-2ac22a8c48f8");
        if (projectConfig == null) {
            writeline("/home/ubuntu/results/coverage/Project/Project_1_10.coverage", "e03b2df8-7ebb-47fb-adfd-e3164ce09fe6");
            throw new IllegalStateException("projectConfig must be set on project before calling init()");
        }
        writeline("/home/ubuntu/results/coverage/Project/Project_1_10.coverage", "30159a3e-ab99-4ead-9069-1bbc89d2ce48");
        buildAfterFailed = projectConfig.shouldBuildAfterFailed();
        writeline("/home/ubuntu/results/coverage/Project/Project_1_10.coverage", "b0790024-02a1-486c-97ca-29aeb0baf648");
        forceOnly = projectConfig.isForceOnly();
        writeline("/home/ubuntu/results/coverage/Project/Project_1_10.coverage", "6bcf95dd-608a-458c-970f-821282ef4566");
        requiremodification = projectConfig.isRequiremodification();
        writeline("/home/ubuntu/results/coverage/Project/Project_1_10.coverage", "d002b6a4-8715-4bd9-921e-c95d03036710");
        if (lastBuild == null) {
            writeline("/home/ubuntu/results/coverage/Project/Project_1_10.coverage", "a0c1b552-6f97-4146-afd9-d3f6e4e76ce1");
            lastBuild = DateUtil.getMidnight();
        }
        writeline("/home/ubuntu/results/coverage/Project/Project_1_10.coverage", "2d681ab4-e6ce-4970-b3a4-eadcaba6a1d2");
        if (lastSuccessfulBuild == null) {
            writeline("/home/ubuntu/results/coverage/Project/Project_1_10.coverage", "2e1051d2-db20-4814-8105-d4dbb2717e93");
            lastSuccessfulBuild = lastBuild;
        }
        writeline("/home/ubuntu/results/coverage/Project/Project_1_10.coverage", "640305a5-1ec3-4b55-8a44-6e0dca289e0a");
        if (LOG.isDebugEnabled()) {
            writeline("/home/ubuntu/results/coverage/Project/Project_1_10.coverage", "97e13255-9d64-48d3-b819-c02baaf2cf32");
            debug("buildInterval          = [" + getBuildInterval() + "]");
            writeline("/home/ubuntu/results/coverage/Project/Project_1_10.coverage", "c8fb258f-9099-4d2d-87f7-0cf186e487a4");
            debug("buildForced            = [" + buildForced + "]");
            writeline("/home/ubuntu/results/coverage/Project/Project_1_10.coverage", "2bde0555-d9d9-4796-a56e-a41ad90cc258");
            debug("buildAfterFailed       = [" + buildAfterFailed + "]");
            writeline("/home/ubuntu/results/coverage/Project/Project_1_10.coverage", "652046ca-ac7b-4f4c-b303-29fa31288697");
            debug("requireModifcation     = [" + requiremodification + "]");
            writeline("/home/ubuntu/results/coverage/Project/Project_1_10.coverage", "d591a9f2-8572-4efd-865e-5f2f1c33a074");
            debug("forceOnly              = [" + forceOnly + "]");
            writeline("/home/ubuntu/results/coverage/Project/Project_1_10.coverage", "cd6e3ad5-f1cf-47f9-a5c4-3a0cdee0c876");
            debug("buildCounter           = [" + buildCounter + "]");
            writeline("/home/ubuntu/results/coverage/Project/Project_1_10.coverage", "6f226426-e109-434a-848b-4281a1508cf9");
            debug("isPaused               = [" + isPaused + "]");
            writeline("/home/ubuntu/results/coverage/Project/Project_1_10.coverage", "595c587b-0731-4c0e-9d35-a1b5678d4856");
            debug("label                  = [" + label + "]");
            writeline("/home/ubuntu/results/coverage/Project/Project_1_10.coverage", "b853401c-5a21-49b2-8dc1-6e5d1064e5fd");
            debug("lastBuild              = [" + DateUtil.getFormattedTime(lastBuild) + "]");
            writeline("/home/ubuntu/results/coverage/Project/Project_1_10.coverage", "a078fa31-19a3-4289-a62b-9f7942f114d7");
            debug("lastSuccessfulBuild    = [" + DateUtil.getFormattedTime(lastSuccessfulBuild) + "]");
            writeline("/home/ubuntu/results/coverage/Project/Project_1_10.coverage", "702f3c14-00ee-46d6-a679-c15676d28528");
            debug("logDir                 = [" + projectConfig.getLog().getLogDir() + "]");
            writeline("/home/ubuntu/results/coverage/Project/Project_1_10.coverage", "e4fde8bf-f3bb-44f9-b7eb-4285deb4b754");
            debug("logXmlEncoding         = [" + projectConfig.getLog().getLogXmlEncoding() + "]");
            writeline("/home/ubuntu/results/coverage/Project/Project_1_10.coverage", "789a7398-e4b0-4e8b-841a-75609b5561a3");
            debug("wasLastBuildSuccessful = [" + wasLastBuildSuccessful + "]");
        }
    }

    protected Element getProjectPropertiesElement(final Date now) {
        writeline("/home/ubuntu/results/coverage/Project/Project_1_10.coverage", "f17f31f2-3af9-4ed8-b284-cd09bd4283a0");
        final Element infoElement = new Element("info");
        writeline("/home/ubuntu/results/coverage/Project/Project_1_10.coverage", "36e71383-0d4b-405a-a58a-57e79906e79c");
        addProperty(infoElement, "projectname", name);
        writeline("/home/ubuntu/results/coverage/Project/Project_1_10.coverage", "35652b8a-9a2a-43e8-8891-396d47eeb868");
        final String lastBuildString = DateUtil.getFormattedTime(lastBuild == null ? now : lastBuild);
        writeline("/home/ubuntu/results/coverage/Project/Project_1_10.coverage", "5a3d210d-cd02-4d87-bb3e-9f064f600af8");
        addProperty(infoElement, "lastbuild", lastBuildString);
        writeline("/home/ubuntu/results/coverage/Project/Project_1_10.coverage", "a40a5063-f4ba-4124-8e4b-7063971ded58");
        final String lastSuccessfulBuildString = DateUtil.getFormattedTime(lastSuccessfulBuild == null ? now : lastSuccessfulBuild);
        writeline("/home/ubuntu/results/coverage/Project/Project_1_10.coverage", "c164e6d0-2821-4495-9dd9-9480f63624c9");
        addProperty(infoElement, "lastsuccessfulbuild", lastSuccessfulBuildString);
        writeline("/home/ubuntu/results/coverage/Project/Project_1_10.coverage", "254631bc-a77a-491a-9bfc-28f111832d99");
        addProperty(infoElement, "builddate", DateUtil.formatIso8601(now));
        writeline("/home/ubuntu/results/coverage/Project/Project_1_10.coverage", "87991fd8-121a-4b9a-8b0c-5c7499d5b222");
        addProperty(infoElement, "cctimestamp", DateUtil.getFormattedTime(now));
        writeline("/home/ubuntu/results/coverage/Project/Project_1_10.coverage", "8340ccf9-9ff9-43b1-bdbf-4890285a7f5e");
        addProperty(infoElement, "label", label);
        writeline("/home/ubuntu/results/coverage/Project/Project_1_10.coverage", "419a0250-f757-4611-b3a0-fbe0f8d31a96");
        addProperty(infoElement, "interval", Long.toString(getBuildInterval() / 1000L));
        writeline("/home/ubuntu/results/coverage/Project/Project_1_10.coverage", "6c3a401f-9af8-47d8-b658-cfad247d75ec");
        addProperty(infoElement, "lastbuildsuccessful", String.valueOf(wasLastBuildSuccessful));
        writeline("/home/ubuntu/results/coverage/Project/Project_1_10.coverage", "1e892951-712b-4dac-a531-b2b4cef7f07a");
        return infoElement;
    }

    private void addProperty(final Element parent, final String key, final String value) {
        writeline("/home/ubuntu/results/coverage/Project/Project_1_10.coverage", "101c2cf5-eff7-4390-a3aa-1897e4dd4171");
        final Element propertyElement = new Element("property");
        writeline("/home/ubuntu/results/coverage/Project/Project_1_10.coverage", "26dc0bfe-a610-495e-8f91-95b61e27e4f6");
        propertyElement.setAttribute("name", key);
        writeline("/home/ubuntu/results/coverage/Project/Project_1_10.coverage", "546f9360-7702-4894-84ab-63027870f3d8");
        propertyElement.setAttribute("value", value);
        writeline("/home/ubuntu/results/coverage/Project/Project_1_10.coverage", "dcc4f75e-c540-4a24-84bc-60359955b3ef");
        parent.addContent(propertyElement);
    }

    protected Map<String, String> getProjectPropertiesMap(final Date now) {
        writeline("/home/ubuntu/results/coverage/Project/Project_1_10.coverage", "f12c6d60-d753-4f8f-a3cc-721feb4544a5");
        final Map<String, String> buildProperties = new HashMap<String, String>();
        writeline("/home/ubuntu/results/coverage/Project/Project_1_10.coverage", "73b06cac-48f8-41c8-a392-6bcce12ee747");
        buildProperties.put("projectname", name);
        writeline("/home/ubuntu/results/coverage/Project/Project_1_10.coverage", "6e775059-f5f0-4f3f-8d35-5e0d581af9d6");
        buildProperties.put("label", label);
        writeline("/home/ubuntu/results/coverage/Project/Project_1_10.coverage", "9b09fd81-ecff-4b01-acea-d3800eede703");
        // TODO: Shouldn't have CVS specific properties here
        buildProperties.put("cvstimestamp", CVSDateUtil.formatCVSDate(now));
        writeline("/home/ubuntu/results/coverage/Project/Project_1_10.coverage", "0c6a2f0c-6f6a-441d-bb13-c0523b1e0b3d");
        buildProperties.put("cctimestamp", DateUtil.getFormattedTime(now));
        writeline("/home/ubuntu/results/coverage/Project/Project_1_10.coverage", "c933eeac-4a8a-470a-b604-766a43e428e4");
        buildProperties.put("cclastgoodbuildtimestamp", getLastSuccessfulBuild());
        writeline("/home/ubuntu/results/coverage/Project/Project_1_10.coverage", "9390f636-ee52-496f-ac74-bceba845fc8b");
        buildProperties.put("cclastbuildtimestamp", getLastBuild());
        writeline("/home/ubuntu/results/coverage/Project/Project_1_10.coverage", "fa3763fa-a16c-47c0-9155-099b36d3490a");
        buildProperties.put("lastbuildsuccessful", String.valueOf(isLastBuildSuccessful()));
        writeline("/home/ubuntu/results/coverage/Project/Project_1_10.coverage", "d0623685-541e-429a-a81d-52b2e0d33ee3");
        buildProperties.put("buildforced", String.valueOf(getBuildForced()));
        writeline("/home/ubuntu/results/coverage/Project/Project_1_10.coverage", "8aa0d939-5808-4fcd-b203-2c9452b054d8");
        if (projectConfig.getModificationSet() != null) {
            writeline("/home/ubuntu/results/coverage/Project/Project_1_10.coverage", "e7023897-43fe-4e42-8096-532203e7de28");
            buildProperties.putAll(projectConfig.getModificationSet().getProperties());
        }
        writeline("/home/ubuntu/results/coverage/Project/Project_1_10.coverage", "603ab271-96b1-48a1-a0b4-dcedb0a53e52");
        if (additionalProperties != null && !additionalProperties.isEmpty()) {
            writeline("/home/ubuntu/results/coverage/Project/Project_1_10.coverage", "ef5b5088-691a-4513-ae32-26b2a7a6db62");
            buildProperties.putAll(additionalProperties);
            writeline("/home/ubuntu/results/coverage/Project/Project_1_10.coverage", "fc2d2304-0435-4c8b-806e-60e07fa6b885");
            additionalProperties.clear();
            writeline("/home/ubuntu/results/coverage/Project/Project_1_10.coverage", "fdaa295e-6f75-48c7-adf8-2701ec1c0222");
            additionalProperties = null;
        }
        writeline("/home/ubuntu/results/coverage/Project/Project_1_10.coverage", "9829b0f9-ddf8-4bf0-a550-78f01ad1d143");
        return buildProperties;
    }

    /**
     * Intended only for unit testing.
     * @return additional Properties variable.
     */
    Map<String, String> getAdditionalProperties() {
        writeline("/home/ubuntu/results/coverage/Project/Project_1_10.coverage", "92a88744-35d0-461c-a352-90fa90fdb142");
        return additionalProperties;
    }

    /**
     * Iterate over all of the registered <code>Publisher</code>s and call
     * their respective <code>publish</code> methods.
     * @param buildLog the content to publish
     * @throws CruiseControlException if an error occurs during publishing
     */
    protected void publish(final Log buildLog) throws CruiseControlException {
        writeline("/home/ubuntu/results/coverage/Project/Project_1_10.coverage", "11726b0a-1a3e-4873-ab12-ce025a628a24");
        setState(ProjectState.PUBLISHING);
        writeline("/home/ubuntu/results/coverage/Project/Project_1_10.coverage", "ce9a8338-e5cb-4e78-b18b-318af2a46f97");
        for (final Publisher publisher : projectConfig.getPublishers()) {
            writeline("/home/ubuntu/results/coverage/Project/Project_1_10.coverage", "be1ae673-0c6d-4def-9f65-99cc8a24a68b");
            // catch all errors, Publishers shouldn't cause failures in the build method
            try {
                writeline("/home/ubuntu/results/coverage/Project/Project_1_10.coverage", "65c1f4a9-bbed-4f14-aaca-2c4771a716d4");
                publisher.publish(buildLog.getContent());
            } catch (Throwable t) {
                writeline("/home/ubuntu/results/coverage/Project/Project_1_10.coverage", "24f68f78-c906-4f8d-8668-7ea4d0b06c52");
                final StringBuilder message = new StringBuilder("exception publishing results");
                writeline("/home/ubuntu/results/coverage/Project/Project_1_10.coverage", "ef7cae9f-2ec2-4fc1-a4d1-4bf784d554e2");
                message.append(" with ").append(publisher.getClass().getName());
                writeline("/home/ubuntu/results/coverage/Project/Project_1_10.coverage", "08025543-09ea-4f23-ab01-29dc82ccb4df");
                message.append(" for project ").append(name);
                writeline("/home/ubuntu/results/coverage/Project/Project_1_10.coverage", "d61f8a0b-f78b-401a-a6c3-ff07c8ea85a9");
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
        writeline("/home/ubuntu/results/coverage/Project/Project_1_10.coverage", "6154cd51-7216-4c60-9cb2-e29e1e546d1e");
        setState(ProjectState.BOOTSTRAPPING);
        writeline("/home/ubuntu/results/coverage/Project/Project_1_10.coverage", "8b3cdcc3-d5d4-4254-98cd-4ef5632faba7");
        for (final Bootstrapper bootstrapper : projectConfig.getBootstrappers()) {
            writeline("/home/ubuntu/results/coverage/Project/Project_1_10.coverage", "5968684b-52e3-40ec-8dfa-207a49d4e1ff");
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
        writeline("/home/ubuntu/results/coverage/Project/Project_1_10.coverage", "a6460510-604e-4808-b9e4-ed5937c34031");
        if (!incrementer.isValidLabel(oldLabel)) {
            writeline("/home/ubuntu/results/coverage/Project/Project_1_10.coverage", "a406601f-845e-4584-a354-9e688464a938");
            final String message = oldLabel + " is not a valid label for labelIncrementer " + incrementer.getClass().getName();
            writeline("/home/ubuntu/results/coverage/Project/Project_1_10.coverage", "16032820-c000-4133-8e81-3d7ed7392cfc");
            debug(message);
            writeline("/home/ubuntu/results/coverage/Project/Project_1_10.coverage", "3b58acb3-cad8-4fa0-aa57-d122f28c4d2c");
            throw new CruiseControlException(message);
        }
    }

    public boolean isLastBuildSuccessful() {
        writeline("/home/ubuntu/results/coverage/Project/Project_1_10.coverage", "59e0638a-cead-40ea-8d88-7de52499e88b");
        return wasLastBuildSuccessful;
    }

    void setWasLastBuildSuccessful(final boolean buildSuccessful) {
        writeline("/home/ubuntu/results/coverage/Project/Project_1_10.coverage", "32e0d95f-208a-40a1-8651-525e701bfb1a");
        wasLastBuildSuccessful = buildSuccessful;
    }

    /**
     * Logs a message to the application log, not to be confused with the
     * CruiseControl build log.
     * @param message the application message to log
     */
    private void info(final String message) {
        writeline("/home/ubuntu/results/coverage/Project/Project_1_10.coverage", "4d2f6ce6-5c56-4b17-bd61-9d4d487763a8");
        LOG.info("Project " + name + ":  " + message);
    }

    private void debug(final String message) {
        writeline("/home/ubuntu/results/coverage/Project/Project_1_10.coverage", "16fc4d23-c93f-41a4-b801-c7e9b00862e1");
        LOG.debug("Project " + name + ":  " + message);
    }

    public void start() {
        writeline("/home/ubuntu/results/coverage/Project/Project_1_10.coverage", "cea206f4-acd0-47a6-825d-57e646e134b6");
        if (stopped || getState() == ProjectState.STOPPED) {
            writeline("/home/ubuntu/results/coverage/Project/Project_1_10.coverage", "16dfe4d8-4f13-4d61-be95-b7b6233d802d");
            stopped = false;
            writeline("/home/ubuntu/results/coverage/Project/Project_1_10.coverage", "8a3343b8-88fe-4c45-8594-ad6e9c560d6e");
            LOG.info("Project " + name + " starting");
            writeline("/home/ubuntu/results/coverage/Project/Project_1_10.coverage", "91e36cec-b470-4934-9d40-d871343f535a");
            setState(ProjectState.IDLE);
            writeline("/home/ubuntu/results/coverage/Project/Project_1_10.coverage", "4be48196-4788-4d8e-8adc-571bacb00d31");
            createNewSchedulingThread();
        }
    }

    protected void createNewSchedulingThread() {
        writeline("/home/ubuntu/results/coverage/Project/Project_1_10.coverage", "08a353db-a76d-4ec7-99f8-ddbdd2ee3b9d");
        final Thread projectSchedulingThread = new Thread(this, "Project " + getName() + " thread");
        writeline("/home/ubuntu/results/coverage/Project/Project_1_10.coverage", "8c3f385e-8936-41d3-a574-3b87cd253597");
        projectSchedulingThread.start();
        writeline("/home/ubuntu/results/coverage/Project/Project_1_10.coverage", "37ecbb9d-f6d2-4b42-869e-43848d03d5df");
        // brief nap to allow thread to start
        try {
            writeline("/home/ubuntu/results/coverage/Project/Project_1_10.coverage", "06fcf617-0953-4232-97c7-41cf371d885a");
            Thread.sleep(100);
        } catch (InterruptedException ie) {
            writeline("/home/ubuntu/results/coverage/Project/Project_1_10.coverage", "03266f60-da17-4139-ab2f-bdb0528e009f");
            LOG.warn("interrupted while waiting for scheduling thread to start", ie);
        }
    }

    public void stop() {
        writeline("/home/ubuntu/results/coverage/Project/Project_1_10.coverage", "886c5ad2-c3c6-4b80-9bcf-f52b9a21c390");
        LOG.info("Project " + name + " stopping");
        writeline("/home/ubuntu/results/coverage/Project/Project_1_10.coverage", "889c5133-09d5-48f2-8983-d7d944df1ab9");
        stopped = true;
        writeline("/home/ubuntu/results/coverage/Project/Project_1_10.coverage", "f7354f82-a34b-4a58-b42a-2dd1486c6cae");
        setState(ProjectState.STOPPED);
        writeline("/home/ubuntu/results/coverage/Project/Project_1_10.coverage", "c8aa6426-21ae-4dc0-bbed-461f0a435a52");
        synchronized (pausedMutex) {
            writeline("/home/ubuntu/results/coverage/Project/Project_1_10.coverage", "e6c8e620-1d61-4944-80de-b74f2c2fba93");
            pausedMutex.notifyAll();
        }
        writeline("/home/ubuntu/results/coverage/Project/Project_1_10.coverage", "5c04598f-9fa6-4081-af06-23d426e4d3a2");
        synchronized (waitMutex) {
            writeline("/home/ubuntu/results/coverage/Project/Project_1_10.coverage", "edb015af-2b78-4406-825f-9371b6fa2a02");
            waitMutex.notifyAll();
        }
        writeline("/home/ubuntu/results/coverage/Project/Project_1_10.coverage", "ebb7732d-32c1-4fee-b054-3bceb06a0dc9");
        synchronized (scheduleMutex) {
            writeline("/home/ubuntu/results/coverage/Project/Project_1_10.coverage", "c34513ca-8cea-4967-838a-e0037938d499");
            scheduleMutex.notifyAll();
        }
    }

    public String toString() {
        writeline("/home/ubuntu/results/coverage/Project/Project_1_10.coverage", "868bef18-2e17-4e7e-8247-7a8788746c86");
        final StringBuilder sb = new StringBuilder("Project ");
        writeline("/home/ubuntu/results/coverage/Project/Project_1_10.coverage", "8f749cdd-c4b2-4d91-92e9-6e2535d471b2");
        sb.append(getName());
        writeline("/home/ubuntu/results/coverage/Project/Project_1_10.coverage", "266a00c8-ad8b-40d3-bb2c-5ab53641ed21");
        sb.append(": ");
        writeline("/home/ubuntu/results/coverage/Project/Project_1_10.coverage", "f0d87b42-fc84-43a0-8466-64dcf8812784");
        sb.append(getStatus());
        writeline("/home/ubuntu/results/coverage/Project/Project_1_10.coverage", "d3c0cc9a-7598-4d40-b62a-ee804390f1a4");
        if (isPaused) {
            writeline("/home/ubuntu/results/coverage/Project/Project_1_10.coverage", "033ddcbb-7431-459e-b8e4-d8b49b8d7d6d");
            sb.append(" (paused)");
        }
        writeline("/home/ubuntu/results/coverage/Project/Project_1_10.coverage", "7ca0ff8b-e8e0-4639-83b8-02dde8092b1b");
        return sb.toString();
    }

    public void addBuildProgressListener(final BuildProgressListener listener) {
        writeline("/home/ubuntu/results/coverage/Project/Project_1_10.coverage", "58c7a59c-205f-4944-92b3-fa8a95301b91");
        synchronized (progressListeners) {
            writeline("/home/ubuntu/results/coverage/Project/Project_1_10.coverage", "13d25d19-8c5f-4785-b583-b29dda2482b5");
            progressListeners.add(listener);
        }
    }

    protected void fireProgressEvent(final BuildProgressEvent event) {
        writeline("/home/ubuntu/results/coverage/Project/Project_1_10.coverage", "ea0a044f-a845-4f76-8fdd-97a2e67418a9");
        synchronized (progressListeners) {
            writeline("/home/ubuntu/results/coverage/Project/Project_1_10.coverage", "8b421f50-ba60-4c31-a57a-93ec3b2814f2");
            for (final BuildProgressListener listener : progressListeners) {
                writeline("/home/ubuntu/results/coverage/Project/Project_1_10.coverage", "83af7a11-7afc-4291-b087-01317a337512");
                listener.handleBuildProgress(event);
            }
        }
    }

    public void addBuildResultListener(final BuildResultListener listener) {
        writeline("/home/ubuntu/results/coverage/Project/Project_1_10.coverage", "e2baec60-efc6-4670-ba7c-1d20eeb3678d");
        synchronized (resultListeners) {
            writeline("/home/ubuntu/results/coverage/Project/Project_1_10.coverage", "8d51d814-f7a8-4bb0-93d8-787f50f964b5");
            resultListeners.add(listener);
        }
    }

    protected void fireResultEvent(final BuildResultEvent event) {
        writeline("/home/ubuntu/results/coverage/Project/Project_1_10.coverage", "90b88e88-4afe-4807-866f-82ecf45a6a5d");
        synchronized (resultListeners) {
            writeline("/home/ubuntu/results/coverage/Project/Project_1_10.coverage", "17b0216e-d7c9-43f6-83a0-fa9a5a5405d1");
            for (final BuildResultListener listener : resultListeners) {
                writeline("/home/ubuntu/results/coverage/Project/Project_1_10.coverage", "57b0c998-b80b-4e23-835f-514456088e40");
                listener.handleBuildResult(event);
            }
        }
    }

    List<Listener> getListeners() {
        writeline("/home/ubuntu/results/coverage/Project/Project_1_10.coverage", "661cec4f-3c14-4657-a17f-dfd1a317bb8b");
        return projectConfig.getListeners();
    }

    public void setProjectConfig(final ProjectConfig projectConfig) throws CruiseControlException {
        writeline("/home/ubuntu/results/coverage/Project/Project_1_10.coverage", "df77c272-b6cd-40d7-96e2-c2f24ad68cdb");
        if (projectConfig == null) {
            writeline("/home/ubuntu/results/coverage/Project/Project_1_10.coverage", "557a235c-0dee-4b94-96bc-66061bfc071f");
            throw new IllegalArgumentException("project config can't be null");
        }
        writeline("/home/ubuntu/results/coverage/Project/Project_1_10.coverage", "c9f284e7-359c-4c45-9983-ba15a35f7331");
        this.projectConfig = projectConfig;
        writeline("/home/ubuntu/results/coverage/Project/Project_1_10.coverage", "80e1203e-b14c-443a-b116-8d9b37d28113");
        setLabelIncrementer(projectConfig.getLabelIncrementer());
    }

    void notifyListeners(final ProjectEvent event) {
        writeline("/home/ubuntu/results/coverage/Project/Project_1_10.coverage", "e480dfc7-40e2-4057-acf9-6bd862aae7ae");
        if (projectConfig == null) {
            writeline("/home/ubuntu/results/coverage/Project/Project_1_10.coverage", "0bf953e3-8e54-48cc-8e9f-a6fd1cdeff1f");
            throw new IllegalStateException("projectConfig is null");
        }
        writeline("/home/ubuntu/results/coverage/Project/Project_1_10.coverage", "a7b0ea2e-9ea1-483b-b605-199857f0ee90");
        for (final Listener listener : projectConfig.getListeners()) {
            writeline("/home/ubuntu/results/coverage/Project/Project_1_10.coverage", "25d70849-0bfe-4811-ac9b-4ba7aabbd23c");
            try {
                writeline("/home/ubuntu/results/coverage/Project/Project_1_10.coverage", "95d04fd8-2097-4e28-b805-351b1940c537");
                listener.handleEvent(event);
            } catch (CruiseControlException e) {
                writeline("/home/ubuntu/results/coverage/Project/Project_1_10.coverage", "7c9d03bb-eb7a-44a8-919c-32f4e7ca3f98");
                final StringBuilder message = new StringBuilder("exception notifying listener ");
                writeline("/home/ubuntu/results/coverage/Project/Project_1_10.coverage", "41a037af-5299-4c04-a7c0-e454174c5f43");
                message.append(listener.getClass().getName());
                writeline("/home/ubuntu/results/coverage/Project/Project_1_10.coverage", "f7b4002c-9ef5-45c2-980b-70c1fb3ed616");
                message.append(" for project ").append(name);
                writeline("/home/ubuntu/results/coverage/Project/Project_1_10.coverage", "61fa568c-a193-4cdc-8645-271a5a7e00c9");
                LOG.error(message.toString(), e);
            }
        }
    }

    public boolean equals(final Object arg0) {
        writeline("/home/ubuntu/results/coverage/Project/Project_1_10.coverage", "ed195242-19c8-4f1a-94a6-996a38ca9f6d");
        if (arg0 == null) {
            writeline("/home/ubuntu/results/coverage/Project/Project_1_10.coverage", "b39227a7-ea61-4c09-afd9-bf0c59a24f4c");
            return false;
        }
        writeline("/home/ubuntu/results/coverage/Project/Project_1_10.coverage", "4104c77b-19b0-4803-98d6-e2c9df9dfd87");
        if (arg0.getClass().getName().equals(getClass().getName())) {
            writeline("/home/ubuntu/results/coverage/Project/Project_1_10.coverage", "ca93c069-efee-4aac-bdf7-382d80637a85");
            final Project thatProject = (Project) arg0;
            writeline("/home/ubuntu/results/coverage/Project/Project_1_10.coverage", "b1df040a-6ca2-42a7-88d5-8277af3af0e1");
            return thatProject.name.equals(name);
        }
        writeline("/home/ubuntu/results/coverage/Project/Project_1_10.coverage", "8771df70-e238-4cbe-9970-0c3fd01204e1");
        return false;
    }

    public int hashCode() {
        writeline("/home/ubuntu/results/coverage/Project/Project_1_10.coverage", "9f288823-a5cb-4d24-a092-0c6242cb8ac6");
        return name.hashCode();
    }

    public void register(final MBeanServer server) throws JMException {
        writeline("/home/ubuntu/results/coverage/Project/Project_1_10.coverage", "bcd883f3-7d71-4ff1-b625-2055ed496ff9");
        LOG.debug("Registering project mbean");
        writeline("/home/ubuntu/results/coverage/Project/Project_1_10.coverage", "921997b3-ca99-4b5e-a977-0d8e1128ec2b");
        final ProjectController projectController = new ProjectController(this);
        writeline("/home/ubuntu/results/coverage/Project/Project_1_10.coverage", "388193bc-d8a3-46f6-8e0f-8811ee4e5ace");
        projectController.register(server);
    }

    public ProjectConfig getProjectConfig() {
        writeline("/home/ubuntu/results/coverage/Project/Project_1_10.coverage", "1560a686-0654-43a9-b2fa-567525010f00");
        return projectConfig;
    }

    public Date getLastBuildDate() {
        writeline("/home/ubuntu/results/coverage/Project/Project_1_10.coverage", "12e4ace6-e385-4202-8d0e-d52071cb9d0e");
        return lastBuild;
    }

    public Progress getProgress() {
        writeline("/home/ubuntu/results/coverage/Project/Project_1_10.coverage", "29395ce2-2700-4890-abc9-abb654d3a959");
        return progress;
    }

    public List<String> getLogLabels() {
        writeline("/home/ubuntu/results/coverage/Project/Project_1_10.coverage", "9724dd0f-1203-4af3-a754-08326090566f");
        return projectConfig.getLogLabels();
    }

    public String[] getLogLabelLines(final String logLabel, final int firstLine) {
        writeline("/home/ubuntu/results/coverage/Project/Project_1_10.coverage", "88771973-d1dd-40d3-ac8b-ef28cd09dc2b");
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
