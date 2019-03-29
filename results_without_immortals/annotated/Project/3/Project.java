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
        writeline("/home/ubuntu/results/coverage/Project/Project_3_10.coverage", "b1da33f5-b2b1-45fd-a81b-af7c5528dbba");
        state = ProjectState.STOPPED;
        writeline("/home/ubuntu/results/coverage/Project/Project_3_10.coverage", "d40a8db2-f544-4ef2-b648-2e3683a40481");
        pausedMutex = new Object();
        writeline("/home/ubuntu/results/coverage/Project/Project_3_10.coverage", "0e28822f-2beb-4d76-8873-52d07dc089b9");
        scheduleMutex = new Object();
        writeline("/home/ubuntu/results/coverage/Project/Project_3_10.coverage", "e49866a6-1842-4b41-a664-ce17118c3d02");
        waitMutex = new Object();
        writeline("/home/ubuntu/results/coverage/Project/Project_3_10.coverage", "88c6f77d-ed25-43b5-86ff-801adb6e43c9");
        progressListeners = new ArrayList<BuildProgressListener>();
        writeline("/home/ubuntu/results/coverage/Project/Project_3_10.coverage", "49216888-4b56-41dc-adc7-c3397e7bf001");
        resultListeners = new ArrayList<BuildResultListener>();
        writeline("/home/ubuntu/results/coverage/Project/Project_3_10.coverage", "58eeb6d4-6d38-44ed-95d7-1a3cf1698b7a");
        progress = new ProgressImpl(this);
    }

    private void readObject(final ObjectInputStream stream) throws IOException, ClassNotFoundException {
        writeline("/home/ubuntu/results/coverage/Project/Project_3_10.coverage", "75e7137a-d15e-46c5-b6b9-5fd025ac77f6");
        stream.defaultReadObject();
        writeline("/home/ubuntu/results/coverage/Project/Project_3_10.coverage", "a0c73ce3-7cce-40a4-8bfd-af5515369613");
        initializeTransientFields();
    }

    public void execute() {
        writeline("/home/ubuntu/results/coverage/Project/Project_3_10.coverage", "b86d5007-6465-438b-8988-14026b242694");
        if (stopped) {
            writeline("/home/ubuntu/results/coverage/Project/Project_3_10.coverage", "236768fc-72ec-4508-a890-5c9ed21cbdfa");
            LOG.warn("not building project " + name + " because project has been stopped.");
            writeline("/home/ubuntu/results/coverage/Project/Project_3_10.coverage", "103a4da0-aa07-4590-ab3c-ef817e47c4f6");
            buildFinished();
            writeline("/home/ubuntu/results/coverage/Project/Project_3_10.coverage", "7f3705da-18a7-4b57-a801-71695bdf0f98");
            return;
        }
        writeline("/home/ubuntu/results/coverage/Project/Project_3_10.coverage", "2c1da017-4c5f-4e74-bf4a-81c0fc737896");
        synchronized (pausedMutex) {
            writeline("/home/ubuntu/results/coverage/Project/Project_3_10.coverage", "15b6331e-6e2d-4a09-9c2c-01ddf34b2e8c");
            if (isPaused) {
                writeline("/home/ubuntu/results/coverage/Project/Project_3_10.coverage", "ac84f038-8123-4771-abb6-71bf04896491");
                LOG.info("not building project " + name + " because project has been paused.");
                writeline("/home/ubuntu/results/coverage/Project/Project_3_10.coverage", "329f9b8b-b9d4-4ca5-88f3-395a414a2ff9");
                buildFinished();
                writeline("/home/ubuntu/results/coverage/Project/Project_3_10.coverage", "83590d9c-11b2-48e3-9d6a-e04021e9fd5d");
                return;
            }
        }
        writeline("/home/ubuntu/results/coverage/Project/Project_3_10.coverage", "1e6144eb-4407-44db-ac41-042aa25f6a6e");
        try {
            writeline("/home/ubuntu/results/coverage/Project/Project_3_10.coverage", "3641a7ee-0a15-4a9c-8915-39b9fa908470");
            init();
            writeline("/home/ubuntu/results/coverage/Project/Project_3_10.coverage", "223fac43-edce-4195-a00c-8060e9afa7e9");
            build();
        } catch (CruiseControlException e) {
            writeline("/home/ubuntu/results/coverage/Project/Project_3_10.coverage", "9accf097-f127-48ca-b1b3-5b6be6e68f23");
            LOG.error("exception attempting build in project " + name, e);
        } finally {
            writeline("/home/ubuntu/results/coverage/Project/Project_3_10.coverage", "86a286c5-46a9-4fe4-9efd-4db1e897597d");
            buildFinished();
        }
    }

    /**
     * Unless paused, runs any bootstrappers and then the entire build.
     * @throws CruiseControlException if an error occurs during the build
     */
    protected void build() throws CruiseControlException {
        writeline("/home/ubuntu/results/coverage/Project/Project_3_10.coverage", "38938af6-653d-4bc1-816a-a46fbf5820e2");
        if (projectConfig == null) {
            writeline("/home/ubuntu/results/coverage/Project/Project_3_10.coverage", "73ab9368-22bf-4b20-bec7-581da1e50625");
            throw new IllegalStateException("projectConfig must be set on project before calling build()");
        }
        writeline("/home/ubuntu/results/coverage/Project/Project_3_10.coverage", "c0f1dcd3-0405-4ff6-a89b-10b8a0bebd66");
        if (stopped) {
            writeline("/home/ubuntu/results/coverage/Project/Project_3_10.coverage", "6d761269-3b31-45e6-8c65-80fdad811312");
            LOG.warn("not building project " + name + " because project has been stopped.");
            writeline("/home/ubuntu/results/coverage/Project/Project_3_10.coverage", "f0168cc8-178c-46de-b5f8-0cdce64592ea");
            return;
        }
        writeline("/home/ubuntu/results/coverage/Project/Project_3_10.coverage", "148cb3b0-4eda-4ffc-989e-e3c8d1cba275");
        // or if the last build faild and we want to build on failures
        if (forceOnly && !buildForced && !(!wasLastBuildSuccessful && buildAfterFailed)) {
            writeline("/home/ubuntu/results/coverage/Project/Project_3_10.coverage", "1242cf76-5912-44bf-98a4-55c411664a58");
            info("not building because project is forceOnly and build not forced.");
            writeline("/home/ubuntu/results/coverage/Project/Project_3_10.coverage", "8b8543e6-6c80-494b-81dc-442f1e51fd93");
            return;
        }
        writeline("/home/ubuntu/results/coverage/Project/Project_3_10.coverage", "cd5316ac-407b-4d5b-93a5-900b31ff2002");
        final boolean buildWasForced = buildForced;
        writeline("/home/ubuntu/results/coverage/Project/Project_3_10.coverage", "7852d5c5-0a5d-40dd-bb26-64ba3ccaf005");
        try {
            writeline("/home/ubuntu/results/coverage/Project/Project_3_10.coverage", "5cb118c8-180f-499c-81f2-5b6163915b6b");
            setBuildStartTime(new Date());
            writeline("/home/ubuntu/results/coverage/Project/Project_3_10.coverage", "ef65fead-4d03-4435-bbd8-67a2365ca271");
            final Schedule schedule = projectConfig.getSchedule();
            writeline("/home/ubuntu/results/coverage/Project/Project_3_10.coverage", "bb07f658-2d74-4ca3-b287-ca8888f117f5");
            if (schedule == null) {
                writeline("/home/ubuntu/results/coverage/Project/Project_3_10.coverage", "2237b7fb-eda9-4eb2-b36d-4496062dcd6b");
                throw new IllegalStateException("project must have a schedule");
            }
            writeline("/home/ubuntu/results/coverage/Project/Project_3_10.coverage", "644eed70-56fc-454f-a884-0966424ffba7");
            if (schedule.isPaused(buildStartTime)) {
                writeline("/home/ubuntu/results/coverage/Project/Project_3_10.coverage", "b41c0f87-027a-4e61-970b-8e578d59dffe");
                // is different than ProjectState.PAUSED
                return;
            }
            writeline("/home/ubuntu/results/coverage/Project/Project_3_10.coverage", "536b3f02-1123-4dc9-a74c-f63b70ae4d85");
            // @todo Add Progress param to Bootstrapper API?
            bootstrap();
            writeline("/home/ubuntu/results/coverage/Project/Project_3_10.coverage", "906423ed-5f13-444a-88b3-4f6b9eb29e11");
            final String target = useAndResetBuildTargetIfBuildWasForced(buildWasForced);
            writeline("/home/ubuntu/results/coverage/Project/Project_3_10.coverage", "9832a086-7e47-45de-8f52-a667ff3779d1");
            // @todo Add Progress param to ModificationSet API?
            // getModifications will only return null if we don't need to build
            final Element modifications = getModifications(buildWasForced);
            writeline("/home/ubuntu/results/coverage/Project/Project_3_10.coverage", "09fe0044-74c7-4db0-891e-8e12fccaed58");
            if (modifications == null) {
                writeline("/home/ubuntu/results/coverage/Project/Project_3_10.coverage", "88ec3bfc-59c2-410d-9818-3b0d4359b37e");
                return;
            }
            writeline("/home/ubuntu/results/coverage/Project/Project_3_10.coverage", "38eb9910-cdbd-4a17-a62c-489e6f04cdc5");
            // Using local reference to avoid NPE if config.xml is updated during build
            final Log buildLog = projectConfig.getLog();
            writeline("/home/ubuntu/results/coverage/Project/Project_3_10.coverage", "94e7b3fe-8804-45cf-90e7-99e882506ff0");
            buildLog.addContent(modifications);
            writeline("/home/ubuntu/results/coverage/Project/Project_3_10.coverage", "4141b00b-ff2d-4222-989c-c62318ad28e9");
            final Date now;
            writeline("/home/ubuntu/results/coverage/Project/Project_3_10.coverage", "1704c185-a0c0-49c4-9dc9-64800ead7303");
            if (projectConfig.getModificationSet() != null && projectConfig.getModificationSet().getTimeOfCheck() != null) {
                writeline("/home/ubuntu/results/coverage/Project/Project_3_10.coverage", "6a2ac533-02c7-469e-b11d-63693ec6bca0");
                now = projectConfig.getModificationSet().getTimeOfCheck();
            } else {
                writeline("/home/ubuntu/results/coverage/Project/Project_3_10.coverage", "7444d519-7743-4c54-8988-9c6f4d10db10");
                now = new Date();
            }
            writeline("/home/ubuntu/results/coverage/Project/Project_3_10.coverage", "298711d0-d950-4a3f-bfc2-177b98f8fc45");
            if (getLabelIncrementer().isPreBuildIncrementer()) {
                writeline("/home/ubuntu/results/coverage/Project/Project_3_10.coverage", "6d55502f-35d9-405c-b105-97b9cbbe31ae");
                label = getLabelIncrementer().incrementLabel(label, buildLog.getContent());
            }
            writeline("/home/ubuntu/results/coverage/Project/Project_3_10.coverage", "a919b5e6-49bf-4ae6-81bd-ba4b99d3d04d");
            // collect project information
            buildLog.addContent(getProjectPropertiesElement(now));
            writeline("/home/ubuntu/results/coverage/Project/Project_3_10.coverage", "10db9d0b-871f-4534-aaea-a3c1fdd196b7");
            setState(ProjectState.BUILDING);
            writeline("/home/ubuntu/results/coverage/Project/Project_3_10.coverage", "1d89ac56-fcf1-4105-923c-92487a2b05bd");
            final Element builderLog = schedule.build(buildCounter, lastBuild, now, getProjectPropertiesMap(now), target, progress);
            writeline("/home/ubuntu/results/coverage/Project/Project_3_10.coverage", "e6f2116c-b1af-40dc-a9d4-bd1aba2c48c2");
            buildLog.addContent(builderLog.detach());
            writeline("/home/ubuntu/results/coverage/Project/Project_3_10.coverage", "a3db870b-e1a9-4e68-b6f6-4818e5456543");
            boolean buildSuccessful = buildLog.wasBuildSuccessful();
            writeline("/home/ubuntu/results/coverage/Project/Project_3_10.coverage", "a02dee6a-e8ee-462a-ab12-eccb1db4e47b");
            fireResultEvent(new BuildResultEvent(this, buildSuccessful));
            writeline("/home/ubuntu/results/coverage/Project/Project_3_10.coverage", "b1efae06-d647-405d-aa3b-d164ea350635");
            if (!getLabelIncrementer().isPreBuildIncrementer() && buildSuccessful) {
                writeline("/home/ubuntu/results/coverage/Project/Project_3_10.coverage", "bbe17950-2acb-41bb-a6b8-48115ef6312d");
                label = getLabelIncrementer().incrementLabel(label, buildLog.getContent());
            }
            writeline("/home/ubuntu/results/coverage/Project/Project_3_10.coverage", "3fa1fcd2-9f78-4507-9dd8-af75044c05e7");
            setState(ProjectState.MERGING_LOGS);
            writeline("/home/ubuntu/results/coverage/Project/Project_3_10.coverage", "c279c255-ac87-4d97-949b-96965f61e626");
            buildLog.writeLogFile(now);
            writeline("/home/ubuntu/results/coverage/Project/Project_3_10.coverage", "893d3a92-c9f8-489f-b051-a133e054f44d");
            // regardless of success or failure (buildAfterFailed = false in config.xml)
            if (!buildAfterFailed) {
                writeline("/home/ubuntu/results/coverage/Project/Project_3_10.coverage", "8b742e0c-ae2d-479a-b9d7-9c28b4cb16dd");
                lastBuild = now;
            }
            writeline("/home/ubuntu/results/coverage/Project/Project_3_10.coverage", "91692684-0720-48d9-bd67-613aedcbfa90");
            // If this was a successful build, update both last build and last successful build
            if (buildSuccessful) {
                writeline("/home/ubuntu/results/coverage/Project/Project_3_10.coverage", "726fbbae-50e2-4388-b7cf-15f1a5f437b4");
                lastBuild = now;
                writeline("/home/ubuntu/results/coverage/Project/Project_3_10.coverage", "fa2fbba5-336d-4d4d-a577-04a9be1a9db2");
                lastSuccessfulBuild = now;
                writeline("/home/ubuntu/results/coverage/Project/Project_3_10.coverage", "b3fdd5f3-9ba6-43f5-87ec-35cf2643dc02");
                info("build successful");
            } else {
                writeline("/home/ubuntu/results/coverage/Project/Project_3_10.coverage", "1f361aa9-feda-4e49-bf83-0b42fcdba23f");
                info("build failed");
            }
            writeline("/home/ubuntu/results/coverage/Project/Project_3_10.coverage", "91ae18fd-0714-4e7d-b467-ab4bee6e0f38");
            buildCounter++;
            writeline("/home/ubuntu/results/coverage/Project/Project_3_10.coverage", "5ef163dc-d33f-4535-a293-d3c3c8fc2a0b");
            setWasLastBuildSuccessful(buildSuccessful);
            writeline("/home/ubuntu/results/coverage/Project/Project_3_10.coverage", "bd4ab68d-ea5d-464c-b46f-b80ef9d8443f");
            // also need to reset forced flag before serializing, unless buildForced var is transient
            // resetBuildForcedOnlyIfBuildWasForced(buildWasForced);
            serializeProject();
            writeline("/home/ubuntu/results/coverage/Project/Project_3_10.coverage", "f7af0af1-f8a5-4da9-be37-63a0e8f49032");
            // @todo Add Progress param to Publisher API?
            publish(buildLog);
            writeline("/home/ubuntu/results/coverage/Project/Project_3_10.coverage", "1b5e4921-2b34-405b-88f6-c2fb1eee49df");
            buildLog.reset();
        } finally {
            writeline("/home/ubuntu/results/coverage/Project/Project_3_10.coverage", "9c1c5a54-36ff-4fa0-8a54-8893135fcc11");
            resetBuildForcedOnlyIfBuildWasForced(buildWasForced);
            writeline("/home/ubuntu/results/coverage/Project/Project_3_10.coverage", "27ab8995-a043-4ba4-8a4a-1eb7a8b2be37");
            setState(ProjectState.IDLE);
        }
    }

    private String useAndResetBuildTargetIfBuildWasForced(final boolean buildWasForced) {
        writeline("/home/ubuntu/results/coverage/Project/Project_3_10.coverage", "3c52fefe-4917-4c62-ae13-fec15b235713");
        String target = null;
        writeline("/home/ubuntu/results/coverage/Project/Project_3_10.coverage", "1d0f1c1d-4811-4311-a7e0-49ad407c6168");
        if (buildWasForced) {
            writeline("/home/ubuntu/results/coverage/Project/Project_3_10.coverage", "34dd4d1e-05d2-450b-993c-7efaff746a2b");
            target = buildTarget;
            writeline("/home/ubuntu/results/coverage/Project/Project_3_10.coverage", "4d67eb5e-2ed9-4517-86c1-999043eb89e7");
            buildTarget = null;
        }
        writeline("/home/ubuntu/results/coverage/Project/Project_3_10.coverage", "db4eb289-3ff4-4335-8b57-b15b6f773b37");
        return target;
    }

    private void resetBuildForcedOnlyIfBuildWasForced(final boolean buildWasForced) {
        writeline("/home/ubuntu/results/coverage/Project/Project_3_10.coverage", "22729e24-83f7-4c36-81e0-9a35c03b020c");
        if (buildWasForced) {
            writeline("/home/ubuntu/results/coverage/Project/Project_3_10.coverage", "cd6010f2-d74f-46a2-9404-5b33ca167633");
            buildForced = false;
        }
    }

    void setBuildStartTime(final Date date) {
        writeline("/home/ubuntu/results/coverage/Project/Project_3_10.coverage", "67a19f2a-72a9-4774-9981-6bb5612d1ef1");
        buildStartTime = date;
    }

    public void run() {
        writeline("/home/ubuntu/results/coverage/Project/Project_3_10.coverage", "81faa2ae-d568-47cf-adc0-bb4405bfa5f6");
        LOG.info("Project " + name + " started");
        writeline("/home/ubuntu/results/coverage/Project/Project_3_10.coverage", "05f6b3a1-43c2-42b5-9f78-48db589b4462");
        try {
            writeline("/home/ubuntu/results/coverage/Project/Project_3_10.coverage", "34f6591a-92d1-4f53-9e3b-62fd713246e3");
            while (!stopped) {
                writeline("/home/ubuntu/results/coverage/Project/Project_3_10.coverage", "89eac87a-975d-40c0-99dc-3468a9462518");
                try {
                    writeline("/home/ubuntu/results/coverage/Project/Project_3_10.coverage", "67271b21-169e-4101-ab74-c23762a6f372");
                    waitIfPaused();
                    writeline("/home/ubuntu/results/coverage/Project/Project_3_10.coverage", "0364f070-77b9-4202-ac03-912465aafcfd");
                    if (!stopped) {
                        writeline("/home/ubuntu/results/coverage/Project/Project_3_10.coverage", "7c526c1c-9d8b-4a1b-8be1-85dff5f51896");
                        waitForNextBuild();
                    }
                    writeline("/home/ubuntu/results/coverage/Project/Project_3_10.coverage", "3c774c9e-a453-43b1-a06d-38194fbfc520");
                    if (!stopped) {
                        writeline("/home/ubuntu/results/coverage/Project/Project_3_10.coverage", "3305c0d7-2251-43fa-88a5-ff1e60b27000");
                        setState(ProjectState.QUEUED);
                        writeline("/home/ubuntu/results/coverage/Project/Project_3_10.coverage", "2e2a57f0-566c-4428-9020-a464708ab85b");
                        synchronized (scheduleMutex) {
                            writeline("/home/ubuntu/results/coverage/Project/Project_3_10.coverage", "1fd941ec-181a-4b0a-8fb4-adae818061c4");
                            queue.requestBuild(projectConfig);
                            writeline("/home/ubuntu/results/coverage/Project/Project_3_10.coverage", "eed7e999-432c-453c-a35a-1a979bc7039c");
                            waitForBuildToFinish();
                        }
                    }
                } catch (InterruptedException e) {
                    writeline("/home/ubuntu/results/coverage/Project/Project_3_10.coverage", "76edfd4e-443d-408b-8daa-857165544a34");
                    final String message = "Project " + name + ".run() interrupted";
                    writeline("/home/ubuntu/results/coverage/Project/Project_3_10.coverage", "e19ba86a-63c4-4346-b0cc-72ae2962d60b");
                    LOG.error(message, e);
                    writeline("/home/ubuntu/results/coverage/Project/Project_3_10.coverage", "ea2359a4-dc65-4f52-97cb-4ecc6e5f6930");
                    throw new RuntimeException(message);
                }
            }
        } finally {
            writeline("/home/ubuntu/results/coverage/Project/Project_3_10.coverage", "5438fdf3-e84d-4b35-9583-386815744003");
            stopped = true;
            writeline("/home/ubuntu/results/coverage/Project/Project_3_10.coverage", "09acc0f3-34d6-418c-90b9-12eca8f9adab");
            LOG.info("Project " + name + " stopped");
        }
    }

    void waitIfPaused() throws InterruptedException {
        writeline("/home/ubuntu/results/coverage/Project/Project_3_10.coverage", "b41014f2-22c2-446b-a1e3-93e30c8af079");
        synchronized (pausedMutex) {
            writeline("/home/ubuntu/results/coverage/Project/Project_3_10.coverage", "10fc3bf7-e8ff-4408-9fa1-a0f3cae15bd7");
            while (isPaused) {
                writeline("/home/ubuntu/results/coverage/Project/Project_3_10.coverage", "f0327910-6336-4792-ad26-5430a1122c8a");
                setState(ProjectState.PAUSED);
                writeline("/home/ubuntu/results/coverage/Project/Project_3_10.coverage", "42ade17a-70fe-4a83-85cd-c261e952baf3");
                pausedMutex.wait(10 * DateUtil.ONE_MINUTE);
            }
        }
    }

    void waitForNextBuild() throws InterruptedException {
        writeline("/home/ubuntu/results/coverage/Project/Project_3_10.coverage", "d7b4badb-6c99-4f3b-aec4-54ceff64923b");
        long waitTime = getTimeToNextBuild(new Date());
        writeline("/home/ubuntu/results/coverage/Project/Project_3_10.coverage", "c223df7b-19ff-4ead-9814-64e13935cc3c");
        if (needToWaitForNextBuild(waitTime) && !buildForced) {
            writeline("/home/ubuntu/results/coverage/Project/Project_3_10.coverage", "c424c9e7-499e-40cf-81f8-a4da2ad7a5fd");
            final String msg = "next build in " + DateUtil.formatTime(waitTime);
            writeline("/home/ubuntu/results/coverage/Project/Project_3_10.coverage", "3d6907db-1c4b-4243-a28d-cef8e98b05c5");
            info(msg);
            writeline("/home/ubuntu/results/coverage/Project/Project_3_10.coverage", "4c9f020d-3659-4e7d-9fae-235115c9335d");
            synchronized (waitMutex) {
                writeline("/home/ubuntu/results/coverage/Project/Project_3_10.coverage", "102fd786-30b0-4966-80f1-e7cbced1b85d");
                setState(ProjectState.WAITING);
                writeline("/home/ubuntu/results/coverage/Project/Project_3_10.coverage", "24b4948c-f466-45a8-8144-93574f60536b");
                progress.setValue(msg);
                writeline("/home/ubuntu/results/coverage/Project/Project_3_10.coverage", "72cf3232-2813-497d-b345-2bfe5ca256e1");
                waitMutex.wait(waitTime);
            }
        }
    }

    long getTimeToNextBuild(Date now) {
        writeline("/home/ubuntu/results/coverage/Project/Project_3_10.coverage", "135ac73c-946b-4414-b368-212f39ad9d90");
        long waitTime = projectConfig.getSchedule().getTimeToNextBuild(now, getBuildInterval());
        writeline("/home/ubuntu/results/coverage/Project/Project_3_10.coverage", "f2cf33b7-6d52-4de0-8872-bac32067ecd4");
        if (waitTime == 0) {
            writeline("/home/ubuntu/results/coverage/Project/Project_3_10.coverage", "37e553c9-ebc3-4fc1-9be6-1ce102eaccd8");
            // project that has just built within a minute time
            if (buildStartTime != null) {
                writeline("/home/ubuntu/results/coverage/Project/Project_3_10.coverage", "b8207e4a-bc88-4db6-9e70-1c61837ebc39");
                long millisSinceLastBuild = now.getTime() - buildStartTime.getTime();
                writeline("/home/ubuntu/results/coverage/Project/Project_3_10.coverage", "e27b6c52-bcb9-44a5-acfd-e064cbd1c261");
                if (millisSinceLastBuild < Schedule.ONE_MINUTE) {
                    writeline("/home/ubuntu/results/coverage/Project/Project_3_10.coverage", "a8e78844-9a2e-4de4-8e77-ad88aa7de81e");
                    debug("build finished within a minute, getting new time to next build");
                    writeline("/home/ubuntu/results/coverage/Project/Project_3_10.coverage", "c3075a0b-1f1b-4b8a-b146-1ed06b7a602b");
                    Date oneMinuteInFuture = new Date(now.getTime() + Schedule.ONE_MINUTE);
                    writeline("/home/ubuntu/results/coverage/Project/Project_3_10.coverage", "d6bcb590-0386-4c22-a39c-29711faac4a7");
                    waitTime = projectConfig.getSchedule().getTimeToNextBuild(oneMinuteInFuture, getBuildInterval());
                    writeline("/home/ubuntu/results/coverage/Project/Project_3_10.coverage", "cdbea5ad-251f-4257-889b-8d27f53e34bd");
                    waitTime += Schedule.ONE_MINUTE;
                }
            }
        }
        writeline("/home/ubuntu/results/coverage/Project/Project_3_10.coverage", "3c2ad46c-ea93-477e-acb6-25e6f4199169");
        return waitTime;
    }

    static boolean needToWaitForNextBuild(long waitTime) {
        writelineStatic("/home/ubuntu/results/coverage/Project/Project_3_10.coverage", "c4adb621-5693-444b-a07a-566a7cb23e72");
        return waitTime > 0;
    }

    /**
     * @return true if build was forced, intended for unit testing only.
     */
    boolean isBuildForced() {
        writeline("/home/ubuntu/results/coverage/Project/Project_3_10.coverage", "dc224018-81b7-4a59-9c97-0ebd00263c2f");
        return buildForced;
    }

    void forceBuild() {
        writeline("/home/ubuntu/results/coverage/Project/Project_3_10.coverage", "44caa197-3e4c-464c-b631-076bcac750d6");
        synchronized (waitMutex) {
            writeline("/home/ubuntu/results/coverage/Project/Project_3_10.coverage", "99af1f45-feeb-45e4-b8b3-a9ea8b43912c");
            waitMutex.notify();
        }
    }

    public void forceBuildWithTarget(String buildTarget) {
        writeline("/home/ubuntu/results/coverage/Project/Project_3_10.coverage", "47235b85-ea63-4f3a-aa47-b4d3ee7aab4b");
        this.buildTarget = buildTarget;
        writeline("/home/ubuntu/results/coverage/Project/Project_3_10.coverage", "96fcbd1a-9aa2-47d2-9dac-108c53be3562");
        setBuildForced(true);
    }

    public void forceBuildWithTarget(String buildTarget, Map<String, String> addedProperties) {
        writeline("/home/ubuntu/results/coverage/Project/Project_3_10.coverage", "b6736b22-e429-404f-aefb-453340d23ae4");
        additionalProperties = addedProperties;
        writeline("/home/ubuntu/results/coverage/Project/Project_3_10.coverage", "c2ef989a-c206-4fb8-85a6-908be066f557");
        forceBuildWithTarget(buildTarget);
    }

    void waitForBuildToFinish() throws InterruptedException {
        writeline("/home/ubuntu/results/coverage/Project/Project_3_10.coverage", "6083bcd8-b417-4d6e-a59f-a4beedbd4f11");
        synchronized (scheduleMutex) {
            writeline("/home/ubuntu/results/coverage/Project/Project_3_10.coverage", "96bdd046-c813-430c-96d2-42b98978b175");
            debug("waiting for build to finish");
            writeline("/home/ubuntu/results/coverage/Project/Project_3_10.coverage", "1f0af334-89d5-4eed-9f73-e3f067ab9b05");
            scheduleMutex.wait();
        }
    }

    void buildFinished() {
        writeline("/home/ubuntu/results/coverage/Project/Project_3_10.coverage", "a1cda156-0f93-4397-805d-80d2555a7bb9");
        synchronized (scheduleMutex) {
            writeline("/home/ubuntu/results/coverage/Project/Project_3_10.coverage", "1db5f328-0220-414a-9661-29250a069c1f");
            debug("build finished");
            writeline("/home/ubuntu/results/coverage/Project/Project_3_10.coverage", "3d5bb52e-5d54-4e23-aa5f-cb13fc1fcc9d");
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
        writeline("/home/ubuntu/results/coverage/Project/Project_3_10.coverage", "4fba7ea3-e4f8-4340-a59e-64ba86d563fa");
        setState(ProjectState.MODIFICATIONSET);
        writeline("/home/ubuntu/results/coverage/Project/Project_3_10.coverage", "eb250557-419c-4a58-990f-e884d9716d90");
        final ModificationSet modificationSet = projectConfig.getModificationSet();
        writeline("/home/ubuntu/results/coverage/Project/Project_3_10.coverage", "710120c6-3c13-4336-a984-bbc33f2213a8");
        if (modificationSet == null) {
            writeline("/home/ubuntu/results/coverage/Project/Project_3_10.coverage", "f3f23f40-cf3d-463c-b651-22649e492c48");
            debug("no modification set, nothing to detect.");
            writeline("/home/ubuntu/results/coverage/Project/Project_3_10.coverage", "995e1ed9-8956-4895-96d2-77998e570270");
            if (buildWasForced) {
                writeline("/home/ubuntu/results/coverage/Project/Project_3_10.coverage", "6553d582-adba-4e59-b4ad-dbe00a92d58f");
                info("no modification set but build was forced");
                writeline("/home/ubuntu/results/coverage/Project/Project_3_10.coverage", "6c1280ce-a006-4f84-9fa6-8d0646b47f5f");
                return new Element("modifications");
            }
            writeline("/home/ubuntu/results/coverage/Project/Project_3_10.coverage", "439798d8-919c-46ee-8bc5-3b0bb6c82cd3");
            if (!requiremodification) {
                writeline("/home/ubuntu/results/coverage/Project/Project_3_10.coverage", "66057cde-0c6e-400a-8a72-0a1edd4637fb");
                info("no modification set but no modifications required");
                writeline("/home/ubuntu/results/coverage/Project/Project_3_10.coverage", "7500e943-ad44-4708-a3a5-81a7cfb163b9");
                return new Element("modifications");
            }
            writeline("/home/ubuntu/results/coverage/Project/Project_3_10.coverage", "8b567106-ab4f-45d9-ab21-45677495c8cc");
            return null;
        }
        writeline("/home/ubuntu/results/coverage/Project/Project_3_10.coverage", "0245e9f4-dce4-44ae-871a-3f23ff9f0aaf");
        final boolean checkNewChangesFirst = checkOnlySinceLastBuild();
        writeline("/home/ubuntu/results/coverage/Project/Project_3_10.coverage", "3b44dc4e-92ef-470a-a835-fb689703fa56");
        Element modifications;
        writeline("/home/ubuntu/results/coverage/Project/Project_3_10.coverage", "b078c2e5-f1a7-467a-bed0-6d5fd0914f28");
        if (checkNewChangesFirst) {
            writeline("/home/ubuntu/results/coverage/Project/Project_3_10.coverage", "e93e75f5-7e19-443a-bf3a-c9fb1c8eadad");
            debug("getting changes since last build");
            writeline("/home/ubuntu/results/coverage/Project/Project_3_10.coverage", "958243bb-e577-42c0-acf5-dfe106e4ee14");
            modifications = modificationSet.retrieveModificationsAsElement(lastBuild, progress);
        } else {
            writeline("/home/ubuntu/results/coverage/Project/Project_3_10.coverage", "ae61825f-cac7-4e09-abd9-507483bb144b");
            debug("getting changes since last successful build");
            writeline("/home/ubuntu/results/coverage/Project/Project_3_10.coverage", "5d9360a3-d0e0-4bd7-8d75-896f6515cae7");
            modifications = modificationSet.retrieveModificationsAsElement(lastSuccessfulBuild, progress);
        }
        writeline("/home/ubuntu/results/coverage/Project/Project_3_10.coverage", "afee4a66-645f-487b-830e-a98d986acea1");
        if (!modificationSet.isModified()) {
            writeline("/home/ubuntu/results/coverage/Project/Project_3_10.coverage", "139f8422-ee52-4bad-9148-097444104b93");
            info("No modifications found, build not necessary.");
            writeline("/home/ubuntu/results/coverage/Project/Project_3_10.coverage", "86d539be-c764-4de0-a18b-ae2f61e5c980");
            // * build forced
            if (buildAfterFailed && !wasLastBuildSuccessful) {
                writeline("/home/ubuntu/results/coverage/Project/Project_3_10.coverage", "139028d3-ede8-4c6a-835c-4992ec44cd8b");
                info("Building anyway, since buildAfterFailed is true and last build failed.");
            } else if (!requiremodification) {
                writeline("/home/ubuntu/results/coverage/Project/Project_3_10.coverage", "942a68b8-0f99-4db1-9275-a3ad0e08a4b7");
                info("Building anyway, since modifications not required");
            } else {
                writeline("/home/ubuntu/results/coverage/Project/Project_3_10.coverage", "fff8d3b1-1303-4eda-bb72-a6cbe1e41dab");
                if (buildWasForced) {
                    writeline("/home/ubuntu/results/coverage/Project/Project_3_10.coverage", "f17555a1-0c20-482a-a541-a0b3ec705653");
                    info("Building anyway, since build was explicitly forced.");
                } else {
                    writeline("/home/ubuntu/results/coverage/Project/Project_3_10.coverage", "4f812cb2-8991-4ac6-8131-ca81e6f12503");
                    return null;
                }
            }
        }
        writeline("/home/ubuntu/results/coverage/Project/Project_3_10.coverage", "6122a9da-97fc-45ff-b54c-c9f8b01a96da");
        if (checkNewChangesFirst) {
            writeline("/home/ubuntu/results/coverage/Project/Project_3_10.coverage", "1c185ae1-b44f-4613-b5f5-ec713c0c8b39");
            debug("new changes found; now getting complete set");
            writeline("/home/ubuntu/results/coverage/Project/Project_3_10.coverage", "59146aa1-8242-4bc4-996b-ca54426f9348");
            modifications = modificationSet.retrieveModificationsAsElement(lastSuccessfulBuild, progress);
        }
        writeline("/home/ubuntu/results/coverage/Project/Project_3_10.coverage", "60a7932f-d680-4bfa-a3c4-4f6a76430d50");
        return modifications;
    }

    /**
     * @return boolean
     */
    boolean checkOnlySinceLastBuild() {
        writeline("/home/ubuntu/results/coverage/Project/Project_3_10.coverage", "17d08a52-036b-49d7-b877-c4df2aaf73bf");
        if (lastBuild == null || lastSuccessfulBuild == null) {
            writeline("/home/ubuntu/results/coverage/Project/Project_3_10.coverage", "24b5affd-6a13-46b0-8cd0-0ff1f3627646");
            return false;
        }
        writeline("/home/ubuntu/results/coverage/Project/Project_3_10.coverage", "03e1c9fd-735c-4ff6-afaa-714c35577ec7");
        final long lastBuildLong = lastBuild.getTime();
        writeline("/home/ubuntu/results/coverage/Project/Project_3_10.coverage", "9bdacd26-45b3-48c2-a0e4-f79ba855e59c");
        final long timeDifference = lastBuildLong - lastSuccessfulBuild.getTime();
        writeline("/home/ubuntu/results/coverage/Project/Project_3_10.coverage", "390881af-c097-4626-a2ff-08af0bd09bad");
        final boolean moreThanASecond = timeDifference > DateUtil.ONE_SECOND;
        writeline("/home/ubuntu/results/coverage/Project/Project_3_10.coverage", "4ea02f78-5229-4ee1-b82e-4364cf1799b2");
        return !buildAfterFailed && moreThanASecond;
    }

    /**
     * Serialize the project to allow resumption after a process bounce
     */
    public void serializeProject() {
        writeline("/home/ubuntu/results/coverage/Project/Project_3_10.coverage", "9e4f31a2-9e3a-438e-ab6c-5d8ba6ee317c");
        final String safeProjectName = Builder.getFileSystemSafeProjectName(name);
        writeline("/home/ubuntu/results/coverage/Project/Project_3_10.coverage", "0145313d-2e9a-4f88-b373-274ddbfc68bb");
        try {
            writeline("/home/ubuntu/results/coverage/Project/Project_3_10.coverage", "8e200c0c-3455-406f-90a7-4ae564123d56");
            final ObjectOutputStream s = new ObjectOutputStream(new FileOutputStream(safeProjectName + ".ser"));
            writeline("/home/ubuntu/results/coverage/Project/Project_3_10.coverage", "2a429628-8ea7-45c2-91d8-9ebdcecb9f77");
            try {
                writeline("/home/ubuntu/results/coverage/Project/Project_3_10.coverage", "78252656-7dac-408e-beb1-6976302c1082");
                s.writeObject(this);
                writeline("/home/ubuntu/results/coverage/Project/Project_3_10.coverage", "88595516-4122-4f29-b398-4074202ba343");
                s.flush();
                writeline("/home/ubuntu/results/coverage/Project/Project_3_10.coverage", "4ce4ca7a-0ea7-4fe0-8c42-b9ee7879daad");
                debug("Serializing project to [" + safeProjectName + ".ser]");
            } finally {
                writeline("/home/ubuntu/results/coverage/Project/Project_3_10.coverage", "08bc3cf3-3cb8-4b4c-8b8e-609fce0ee982");
                s.close();
            }
        } catch (Exception e) {
            writeline("/home/ubuntu/results/coverage/Project/Project_3_10.coverage", "74ef689c-ce92-4be1-bc77-95bbad108940");
            LOG.warn("Error serializing project to [" + safeProjectName + ".ser]: " + e.getMessage(), e);
        }
    }

    public void setLabelIncrementer(final LabelIncrementer incrementer) throws CruiseControlException {
        writeline("/home/ubuntu/results/coverage/Project/Project_3_10.coverage", "66756c9d-c5a6-4bf4-9e7e-2830ca53c046");
        if (incrementer == null) {
            writeline("/home/ubuntu/results/coverage/Project/Project_3_10.coverage", "17feae64-353c-4539-82f5-c5ffdf0e5ca2");
            throw new IllegalArgumentException("label incrementer can't be null");
        }
        writeline("/home/ubuntu/results/coverage/Project/Project_3_10.coverage", "0414611e-2280-41a8-8b4f-b28e2a31a152");
        labelIncrementer = incrementer;
        writeline("/home/ubuntu/results/coverage/Project/Project_3_10.coverage", "160c3263-9473-4772-baf7-fd490df40fcb");
        if (label == null) {
            writeline("/home/ubuntu/results/coverage/Project/Project_3_10.coverage", "3a5ec217-01fa-46c1-bc5e-fffc08924bcb");
            label = labelIncrementer.getDefaultLabel();
        }
        writeline("/home/ubuntu/results/coverage/Project/Project_3_10.coverage", "ceb54c01-bcad-4f9b-9c95-4f4001b454b0");
        validateLabel(label, labelIncrementer);
    }

    public LabelIncrementer getLabelIncrementer() {
        writeline("/home/ubuntu/results/coverage/Project/Project_3_10.coverage", "ff1c3d28-d1db-40fe-9097-cbf36edda8b3");
        return labelIncrementer;
    }

    public void setName(final String projectName) {
        writeline("/home/ubuntu/results/coverage/Project/Project_3_10.coverage", "668b81ab-00ef-4308-a7d4-78d8c2e102b3");
        name = projectName;
    }

    public String getName() {
        writeline("/home/ubuntu/results/coverage/Project/Project_3_10.coverage", "454516fe-032f-465d-8757-0e6499d9235d");
        return name;
    }

    public void setLabel(final String newLabel) {
        writeline("/home/ubuntu/results/coverage/Project/Project_3_10.coverage", "d3d4c3b8-478d-4c69-95e4-b211a52cfc9c");
        label = newLabel;
    }

    public String getLabel() {
        writeline("/home/ubuntu/results/coverage/Project/Project_3_10.coverage", "90c0434c-ac1a-4b14-a72b-2091f9a8729c");
        return label;
    }

    /**
     * @param newLastBuild string containing the build date in the format
     * yyyyMMddHHmmss
     * @throws CruiseControlException if the date cannot be extracted from the
     * input string
     */
    public void setLastBuild(final String newLastBuild) throws CruiseControlException {
        writeline("/home/ubuntu/results/coverage/Project/Project_3_10.coverage", "e4cfeba9-f921-4aa1-beeb-cb759c3e93ae");
        lastBuild = DateUtil.parseFormattedTime(newLastBuild, "lastBuild");
    }

    /**
     * @param newLastSuccessfulBuild string containing the build date in the format
     * yyyyMMddHHmmss
     * @throws CruiseControlException if the date cannot be extracted from the
     * input string
     */
    public void setLastSuccessfulBuild(final String newLastSuccessfulBuild) throws CruiseControlException {
        writeline("/home/ubuntu/results/coverage/Project/Project_3_10.coverage", "16b5d8ad-77c5-4688-9777-cc567b397421");
        lastSuccessfulBuild = DateUtil.parseFormattedTime(newLastSuccessfulBuild, "lastSuccessfulBuild");
    }

    public String getLastBuild() {
        writeline("/home/ubuntu/results/coverage/Project/Project_3_10.coverage", "bdd8334b-d7bc-46cc-8862-4713a77f8bb1");
        if (lastBuild == null) {
            writeline("/home/ubuntu/results/coverage/Project/Project_3_10.coverage", "2886847d-fdd6-4da2-ac73-2ea0d5fb7e25");
            return null;
        }
        writeline("/home/ubuntu/results/coverage/Project/Project_3_10.coverage", "b0082a33-ea49-47ed-8830-0434b9ae2991");
        return DateUtil.getFormattedTime(lastBuild);
    }

    public boolean getBuildForced() {
        writeline("/home/ubuntu/results/coverage/Project/Project_3_10.coverage", "8a3bb62e-ac48-4aa1-9bd0-5bc93baf5a98");
        return buildForced;
    }

    public void setBuildForced(boolean forceNewBuildNow) {
        writeline("/home/ubuntu/results/coverage/Project/Project_3_10.coverage", "62d07226-0f03-467f-8c7d-7f9e3ec5c0b0");
        buildForced = forceNewBuildNow;
        writeline("/home/ubuntu/results/coverage/Project/Project_3_10.coverage", "88c7b875-8f09-4fde-b673-fb896e639601");
        if (forceNewBuildNow) {
            writeline("/home/ubuntu/results/coverage/Project/Project_3_10.coverage", "fde25c85-3e7d-4239-81fb-4b776e2c090f");
            forceBuild();
        }
    }

    public String getLastSuccessfulBuild() {
        writeline("/home/ubuntu/results/coverage/Project/Project_3_10.coverage", "4833ad8c-5ef7-4ec1-b1da-ab8bcaa02186");
        if (lastSuccessfulBuild == null) {
            writeline("/home/ubuntu/results/coverage/Project/Project_3_10.coverage", "1dc7ca37-38c7-4989-8517-c0e999126af0");
            return null;
        }
        writeline("/home/ubuntu/results/coverage/Project/Project_3_10.coverage", "bbaf5de7-c8fa-45f7-9106-c444bb60e0f0");
        return DateUtil.getFormattedTime(lastSuccessfulBuild);
    }

    public String getLogDir() {
        writeline("/home/ubuntu/results/coverage/Project/Project_3_10.coverage", "ecb249a2-b721-4488-af6d-8c4a7f86b595");
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
        writeline("/home/ubuntu/results/coverage/Project/Project_3_10.coverage", "5afa890a-65f1-43c6-aae9-f5ab128ffe80");
        if (overrideBuildInterval == null) {
            writeline("/home/ubuntu/results/coverage/Project/Project_3_10.coverage", "495e6409-7774-464c-ace8-7d5baebc37ef");
            return projectConfig.getSchedule().getInterval();
        } else {
            writeline("/home/ubuntu/results/coverage/Project/Project_3_10.coverage", "abc7e2f8-dcd6-4548-a5b4-1b04fcee9654");
            return overrideBuildInterval;
        }
    }

    /**
     * Sets the build interval that this Project should use. This method
     * overrides the value initially specified in the Schedule attribute.
     * @param sleepMillis the number of milliseconds to sleep between build attempts
     */
    public void overrideBuildInterval(final long sleepMillis) {
        writeline("/home/ubuntu/results/coverage/Project/Project_3_10.coverage", "d05734fd-1700-4346-abe2-8aa7a6ba943c");
        overrideBuildInterval = sleepMillis;
    }

    public boolean isPaused() {
        writeline("/home/ubuntu/results/coverage/Project/Project_3_10.coverage", "9264ddd5-2735-4488-84d0-1b18259d3a32");
        return isPaused;
    }

    public void setPaused(final boolean paused) {
        writeline("/home/ubuntu/results/coverage/Project/Project_3_10.coverage", "2e1654f3-6219-4da8-8e6d-5455625aa3ac");
        synchronized (pausedMutex) {
            writeline("/home/ubuntu/results/coverage/Project/Project_3_10.coverage", "e67d184b-d17a-47cc-8671-a3f67cf6c52f");
            if (isPaused && !paused) {
                writeline("/home/ubuntu/results/coverage/Project/Project_3_10.coverage", "c614749e-75a8-41af-b4f2-16dbb24595d1");
                pausedMutex.notifyAll();
            }
            writeline("/home/ubuntu/results/coverage/Project/Project_3_10.coverage", "b0960b9d-db90-43b1-94d9-38cf3f60a712");
            isPaused = paused;
        }
    }

    public void setBuildAfterFailed(final boolean rebuildEvenWithNoNewModifications) {
        writeline("/home/ubuntu/results/coverage/Project/Project_3_10.coverage", "343148a1-1518-47bc-a628-800cd4e298b5");
        buildAfterFailed = rebuildEvenWithNoNewModifications;
    }

    public String getStatus() {
        writeline("/home/ubuntu/results/coverage/Project/Project_3_10.coverage", "0cfd525d-4c72-44bc-ad7a-6af68ff95c91");
        return getState().getDescription();
    }

    public String getStatusWithQueuePosition() {
        writeline("/home/ubuntu/results/coverage/Project/Project_3_10.coverage", "378b1f56-1824-45d6-9992-4082c9fe6f4c");
        if (ProjectState.QUEUED.equals(getState())) {
            writeline("/home/ubuntu/results/coverage/Project/Project_3_10.coverage", "427f3fca-33fd-481e-a0ed-3f95f6b1b0f0");
            return getState().getDescription() + " - " + queue.findPosition(projectConfig);
        } else {
            writeline("/home/ubuntu/results/coverage/Project/Project_3_10.coverage", "e5979b8d-ea40-45aa-9f5f-1c88d8895523");
            return getState().getDescription();
        }
    }

    public ProjectState getState() {
        writeline("/home/ubuntu/results/coverage/Project/Project_3_10.coverage", "5da3f998-b470-4cba-929e-b9875d109828");
        return state;
    }

    private void setState(final ProjectState newState) {
        writeline("/home/ubuntu/results/coverage/Project/Project_3_10.coverage", "97e4aebb-100f-4bb4-8646-1076fba7094c");
        state = newState;
        writeline("/home/ubuntu/results/coverage/Project/Project_3_10.coverage", "8e312456-7d65-4c60-95a0-1b18de6800ec");
        info(getStatus());
        writeline("/home/ubuntu/results/coverage/Project/Project_3_10.coverage", "0971a9e2-80e4-42a4-9b09-fac18e1e1ee9");
        notifyListeners(new ProjectStateChangedEvent(name, getState()));
        writeline("/home/ubuntu/results/coverage/Project/Project_3_10.coverage", "419ba7e1-8fda-471c-92f7-c2d1bfb86c75");
        fireProgressEvent(new BuildProgressEvent(this, getState()));
    }

    public void setBuildQueue(final BuildQueue buildQueue) {
        writeline("/home/ubuntu/results/coverage/Project/Project_3_10.coverage", "8805470f-a9ea-4863-b2b0-06821f294f5c");
        queue = buildQueue;
    }

    public String getBuildStartTime() {
        writeline("/home/ubuntu/results/coverage/Project/Project_3_10.coverage", "5460cf9d-0138-44e9-9c9f-fbfef62bc3b3");
        return DateUtil.getFormattedTime(buildStartTime);
    }

    public Log getLog() {
        writeline("/home/ubuntu/results/coverage/Project/Project_3_10.coverage", "b5a20b1d-d516-40e6-9c7c-52d8276ca925");
        return this.projectConfig.getLog();
    }

    /**
     * Initialize the project. Uses ProjectXMLHelper to parse a project file.
     */
    protected void init() {
        writeline("/home/ubuntu/results/coverage/Project/Project_3_10.coverage", "2f7d23d8-898e-4ee5-b379-14c855665550");
        if (projectConfig == null) {
            writeline("/home/ubuntu/results/coverage/Project/Project_3_10.coverage", "12f153aa-c9d8-4d85-b35f-7ae490286cb4");
            throw new IllegalStateException("projectConfig must be set on project before calling init()");
        }
        writeline("/home/ubuntu/results/coverage/Project/Project_3_10.coverage", "a9d41ba2-d261-4d7d-81a3-5e159d1135c6");
        buildAfterFailed = projectConfig.shouldBuildAfterFailed();
        writeline("/home/ubuntu/results/coverage/Project/Project_3_10.coverage", "62ac7262-f94a-491e-a1a8-58272aa9b6d5");
        forceOnly = projectConfig.isForceOnly();
        writeline("/home/ubuntu/results/coverage/Project/Project_3_10.coverage", "00836066-18f8-4a6e-93e1-7c3ece82fb5d");
        requiremodification = projectConfig.isRequiremodification();
        writeline("/home/ubuntu/results/coverage/Project/Project_3_10.coverage", "d6ddb7dd-28b4-482f-9c48-a743c40a10ca");
        if (lastBuild == null) {
            writeline("/home/ubuntu/results/coverage/Project/Project_3_10.coverage", "acb80418-7eed-472a-a8b1-bda880ba7e0e");
            lastBuild = DateUtil.getMidnight();
        }
        writeline("/home/ubuntu/results/coverage/Project/Project_3_10.coverage", "5f2d4866-e0c1-47a4-833d-d62733997a09");
        if (lastSuccessfulBuild == null) {
            writeline("/home/ubuntu/results/coverage/Project/Project_3_10.coverage", "38a0d4c9-70af-49ae-8ac0-0a87e15cec6e");
            lastSuccessfulBuild = lastBuild;
        }
        writeline("/home/ubuntu/results/coverage/Project/Project_3_10.coverage", "a8e6fae8-a40f-4afd-98ca-00792ed6ca8b");
        if (LOG.isDebugEnabled()) {
            writeline("/home/ubuntu/results/coverage/Project/Project_3_10.coverage", "b17bdf22-8b00-461a-8b90-f1835e900300");
            debug("buildInterval          = [" + getBuildInterval() + "]");
            writeline("/home/ubuntu/results/coverage/Project/Project_3_10.coverage", "41cdf2a1-cc55-44ee-9710-0e2aedddb546");
            debug("buildForced            = [" + buildForced + "]");
            writeline("/home/ubuntu/results/coverage/Project/Project_3_10.coverage", "6f8be3d1-4f22-4dba-b0ea-e6b11b205561");
            debug("buildAfterFailed       = [" + buildAfterFailed + "]");
            writeline("/home/ubuntu/results/coverage/Project/Project_3_10.coverage", "921cbdcd-dcf4-4c0e-b8c5-4478d08c3754");
            debug("requireModifcation     = [" + requiremodification + "]");
            writeline("/home/ubuntu/results/coverage/Project/Project_3_10.coverage", "7322aaca-0e8d-4266-ad5a-9d08fb2bbacc");
            debug("forceOnly              = [" + forceOnly + "]");
            writeline("/home/ubuntu/results/coverage/Project/Project_3_10.coverage", "2a914b38-2280-4f77-97c6-71a7617d9742");
            debug("buildCounter           = [" + buildCounter + "]");
            writeline("/home/ubuntu/results/coverage/Project/Project_3_10.coverage", "419f6f23-685c-4c1f-833d-b41a5a27fa47");
            debug("isPaused               = [" + isPaused + "]");
            writeline("/home/ubuntu/results/coverage/Project/Project_3_10.coverage", "8636c6b9-939e-4186-b760-4b7118b85a57");
            debug("label                  = [" + label + "]");
            writeline("/home/ubuntu/results/coverage/Project/Project_3_10.coverage", "0751fa35-753c-48d2-a5e5-9abb7d8b6606");
            debug("lastBuild              = [" + DateUtil.getFormattedTime(lastBuild) + "]");
            writeline("/home/ubuntu/results/coverage/Project/Project_3_10.coverage", "1f10615b-9561-4b36-a1f7-c3a1a0240fd7");
            debug("lastSuccessfulBuild    = [" + DateUtil.getFormattedTime(lastSuccessfulBuild) + "]");
            writeline("/home/ubuntu/results/coverage/Project/Project_3_10.coverage", "5578d15c-07ee-4e77-89a0-1d301cbf8372");
            debug("logDir                 = [" + projectConfig.getLog().getLogDir() + "]");
            writeline("/home/ubuntu/results/coverage/Project/Project_3_10.coverage", "7e7c82fa-3595-4ec2-84fe-e3df992c80bc");
            debug("logXmlEncoding         = [" + projectConfig.getLog().getLogXmlEncoding() + "]");
            writeline("/home/ubuntu/results/coverage/Project/Project_3_10.coverage", "7a35b9a3-fcfc-4e62-a8ee-5da1f0bdb696");
            debug("wasLastBuildSuccessful = [" + wasLastBuildSuccessful + "]");
        }
    }

    protected Element getProjectPropertiesElement(final Date now) {
        writeline("/home/ubuntu/results/coverage/Project/Project_3_10.coverage", "bd09e8fb-6bd4-4636-a040-84d0e66e7070");
        final Element infoElement = new Element("info");
        writeline("/home/ubuntu/results/coverage/Project/Project_3_10.coverage", "6877a79c-1f2e-4148-ad0d-d77684f57805");
        addProperty(infoElement, "projectname", name);
        writeline("/home/ubuntu/results/coverage/Project/Project_3_10.coverage", "b6bda4b1-2ce5-4f7f-bf7d-3ebc2479ebb7");
        final String lastBuildString = DateUtil.getFormattedTime(lastBuild == null ? now : lastBuild);
        writeline("/home/ubuntu/results/coverage/Project/Project_3_10.coverage", "a7322bec-8ee4-4f87-bdc6-fed5ef79cf61");
        addProperty(infoElement, "lastbuild", lastBuildString);
        writeline("/home/ubuntu/results/coverage/Project/Project_3_10.coverage", "d08a4703-a93d-4d7d-b1cd-cc2e72a6a6b9");
        final String lastSuccessfulBuildString = DateUtil.getFormattedTime(lastSuccessfulBuild == null ? now : lastSuccessfulBuild);
        writeline("/home/ubuntu/results/coverage/Project/Project_3_10.coverage", "53e87c1a-1429-431b-9b39-44b552313a5b");
        addProperty(infoElement, "lastsuccessfulbuild", lastSuccessfulBuildString);
        writeline("/home/ubuntu/results/coverage/Project/Project_3_10.coverage", "c6b4aa0e-3494-4817-87f1-af5f54af3c66");
        addProperty(infoElement, "builddate", DateUtil.formatIso8601(now));
        writeline("/home/ubuntu/results/coverage/Project/Project_3_10.coverage", "d0925ad6-b664-41f8-9464-3c58d8bc9c4e");
        addProperty(infoElement, "cctimestamp", DateUtil.getFormattedTime(now));
        writeline("/home/ubuntu/results/coverage/Project/Project_3_10.coverage", "0866b371-744f-4055-aca7-fb10c0e891cb");
        addProperty(infoElement, "label", label);
        writeline("/home/ubuntu/results/coverage/Project/Project_3_10.coverage", "c29d38ea-1ce2-4e16-88b8-dc3cfd6f1a1b");
        addProperty(infoElement, "interval", Long.toString(getBuildInterval() / 1000L));
        writeline("/home/ubuntu/results/coverage/Project/Project_3_10.coverage", "3e1fd5c3-1624-48da-a7b1-612ce7f68c0f");
        addProperty(infoElement, "lastbuildsuccessful", String.valueOf(wasLastBuildSuccessful));
        writeline("/home/ubuntu/results/coverage/Project/Project_3_10.coverage", "45e02be5-7e36-44cb-880e-7dddc0a891fd");
        return infoElement;
    }

    private void addProperty(final Element parent, final String key, final String value) {
        writeline("/home/ubuntu/results/coverage/Project/Project_3_10.coverage", "cebc50e2-50e8-4ce6-b8d4-6882dd76fe95");
        final Element propertyElement = new Element("property");
        writeline("/home/ubuntu/results/coverage/Project/Project_3_10.coverage", "2137ef89-d957-449b-bdc8-2e3eaff105d2");
        propertyElement.setAttribute("name", key);
        writeline("/home/ubuntu/results/coverage/Project/Project_3_10.coverage", "cc1af2c0-b58d-4784-815f-a6a73a69e0a8");
        propertyElement.setAttribute("value", value);
        writeline("/home/ubuntu/results/coverage/Project/Project_3_10.coverage", "5a76498b-2a19-48fe-a530-a05528f2c54c");
        parent.addContent(propertyElement);
    }

    protected Map<String, String> getProjectPropertiesMap(final Date now) {
        writeline("/home/ubuntu/results/coverage/Project/Project_3_10.coverage", "ef38fe8e-6a30-44bb-bd18-205a68ad99ae");
        final Map<String, String> buildProperties = new HashMap<String, String>();
        writeline("/home/ubuntu/results/coverage/Project/Project_3_10.coverage", "a3363d61-ceaf-40eb-887f-9eca625ec7e9");
        buildProperties.put("projectname", name);
        writeline("/home/ubuntu/results/coverage/Project/Project_3_10.coverage", "830417d7-1d48-43bb-90b0-c6a83bb9205a");
        buildProperties.put("label", label);
        writeline("/home/ubuntu/results/coverage/Project/Project_3_10.coverage", "214d5472-9740-4269-8cab-ba5d55080cea");
        // TODO: Shouldn't have CVS specific properties here
        buildProperties.put("cvstimestamp", CVSDateUtil.formatCVSDate(now));
        writeline("/home/ubuntu/results/coverage/Project/Project_3_10.coverage", "67909a24-e5b4-42d7-9f2c-779015985836");
        buildProperties.put("cctimestamp", DateUtil.getFormattedTime(now));
        writeline("/home/ubuntu/results/coverage/Project/Project_3_10.coverage", "3c230ca5-8e33-4677-892e-65a7bfa58781");
        buildProperties.put("cclastgoodbuildtimestamp", getLastSuccessfulBuild());
        writeline("/home/ubuntu/results/coverage/Project/Project_3_10.coverage", "5790ee80-4778-417c-93bd-c3802ae35144");
        buildProperties.put("cclastbuildtimestamp", getLastBuild());
        writeline("/home/ubuntu/results/coverage/Project/Project_3_10.coverage", "a796c295-2ba0-42fa-8c4c-4e00a1e779a0");
        buildProperties.put("lastbuildsuccessful", String.valueOf(isLastBuildSuccessful()));
        writeline("/home/ubuntu/results/coverage/Project/Project_3_10.coverage", "0741aca5-1a6e-431d-b85a-7ff814e4b89a");
        buildProperties.put("buildforced", String.valueOf(getBuildForced()));
        writeline("/home/ubuntu/results/coverage/Project/Project_3_10.coverage", "91f0acc7-c8e4-4482-b9ff-32bee5df982d");
        if (projectConfig.getModificationSet() != null) {
            writeline("/home/ubuntu/results/coverage/Project/Project_3_10.coverage", "c49b74e3-0e52-4057-adb4-a91e62f50d96");
            buildProperties.putAll(projectConfig.getModificationSet().getProperties());
        }
        writeline("/home/ubuntu/results/coverage/Project/Project_3_10.coverage", "6adc327e-eccb-4b19-8e89-ec3da399a5f5");
        if (additionalProperties != null && !additionalProperties.isEmpty()) {
            writeline("/home/ubuntu/results/coverage/Project/Project_3_10.coverage", "ed2b352a-7daa-49e9-b846-b16e16325004");
            buildProperties.putAll(additionalProperties);
            writeline("/home/ubuntu/results/coverage/Project/Project_3_10.coverage", "2c7078ae-9307-4c63-b68c-c6a84cb9d2d6");
            additionalProperties.clear();
            writeline("/home/ubuntu/results/coverage/Project/Project_3_10.coverage", "831844a2-993a-4807-9165-c00f80bccd92");
            additionalProperties = null;
        }
        writeline("/home/ubuntu/results/coverage/Project/Project_3_10.coverage", "b328c413-2bcb-40c3-8cb2-abb59b3ed6fb");
        return buildProperties;
    }

    /**
     * Intended only for unit testing.
     * @return additional Properties variable.
     */
    Map<String, String> getAdditionalProperties() {
        writeline("/home/ubuntu/results/coverage/Project/Project_3_10.coverage", "982df5ce-8d15-4938-b185-f49b5eb12bbc");
        return additionalProperties;
    }

    /**
     * Iterate over all of the registered <code>Publisher</code>s and call
     * their respective <code>publish</code> methods.
     * @param buildLog the content to publish
     * @throws CruiseControlException if an error occurs during publishing
     */
    protected void publish(final Log buildLog) throws CruiseControlException {
        writeline("/home/ubuntu/results/coverage/Project/Project_3_10.coverage", "89068fdb-156b-43d3-b0cf-53bb22158be2");
        setState(ProjectState.PUBLISHING);
        writeline("/home/ubuntu/results/coverage/Project/Project_3_10.coverage", "6aecad37-9e25-4e5e-a726-0b842ff66b53");
        for (final Publisher publisher : projectConfig.getPublishers()) {
            writeline("/home/ubuntu/results/coverage/Project/Project_3_10.coverage", "623eb46e-e991-470c-a8fc-9581435029be");
            // catch all errors, Publishers shouldn't cause failures in the build method
            try {
                writeline("/home/ubuntu/results/coverage/Project/Project_3_10.coverage", "e4493b69-f274-412a-856c-90dfb114f83f");
                publisher.publish(buildLog.getContent());
            } catch (Throwable t) {
                writeline("/home/ubuntu/results/coverage/Project/Project_3_10.coverage", "6fd14b24-2d66-4cf7-bf4e-ca571b9d76c0");
                final StringBuilder message = new StringBuilder("exception publishing results");
                writeline("/home/ubuntu/results/coverage/Project/Project_3_10.coverage", "9831269e-92df-491c-b4d3-3089280aff46");
                message.append(" with ").append(publisher.getClass().getName());
                writeline("/home/ubuntu/results/coverage/Project/Project_3_10.coverage", "10c6d1cb-8591-4ab7-985c-f6eb5de09608");
                message.append(" for project ").append(name);
                writeline("/home/ubuntu/results/coverage/Project/Project_3_10.coverage", "f63d7605-41be-4daf-ab87-65b3b21ef3f4");
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
        writeline("/home/ubuntu/results/coverage/Project/Project_3_10.coverage", "2355363b-3c32-4c95-b74b-ff40a90f624d");
        setState(ProjectState.BOOTSTRAPPING);
        writeline("/home/ubuntu/results/coverage/Project/Project_3_10.coverage", "b6ebe920-3a3d-4be6-8a63-c79598048c8d");
        for (final Bootstrapper bootstrapper : projectConfig.getBootstrappers()) {
            writeline("/home/ubuntu/results/coverage/Project/Project_3_10.coverage", "8a9d3923-c051-4756-94a2-1018645ea7d5");
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
        writeline("/home/ubuntu/results/coverage/Project/Project_3_10.coverage", "4eab2579-c05e-4f88-b22c-ca158c33a3f6");
        if (!incrementer.isValidLabel(oldLabel)) {
            writeline("/home/ubuntu/results/coverage/Project/Project_3_10.coverage", "aed6c6d1-3425-47fb-a2fc-c6a69cd93fac");
            final String message = oldLabel + " is not a valid label for labelIncrementer " + incrementer.getClass().getName();
            writeline("/home/ubuntu/results/coverage/Project/Project_3_10.coverage", "ff592d94-bed5-41a4-947f-bcbb24a30507");
            debug(message);
            writeline("/home/ubuntu/results/coverage/Project/Project_3_10.coverage", "0fe850af-f0ec-434f-b22d-980221ea59d8");
            throw new CruiseControlException(message);
        }
    }

    public boolean isLastBuildSuccessful() {
        writeline("/home/ubuntu/results/coverage/Project/Project_3_10.coverage", "38031131-280a-4649-b1cf-6aacd0291ba7");
        return wasLastBuildSuccessful;
    }

    void setWasLastBuildSuccessful(final boolean buildSuccessful) {
        writeline("/home/ubuntu/results/coverage/Project/Project_3_10.coverage", "e8fc6caf-58df-42db-ba9d-8757b7f93b3b");
        wasLastBuildSuccessful = buildSuccessful;
    }

    /**
     * Logs a message to the application log, not to be confused with the
     * CruiseControl build log.
     * @param message the application message to log
     */
    private void info(final String message) {
        writeline("/home/ubuntu/results/coverage/Project/Project_3_10.coverage", "47c639e4-859f-43aa-a0a9-8658a426ef02");
        LOG.info("Project " + name + ":  " + message);
    }

    private void debug(final String message) {
        writeline("/home/ubuntu/results/coverage/Project/Project_3_10.coverage", "6c35932c-b609-4ddf-bb37-6b060a3fc341");
        LOG.debug("Project " + name + ":  " + message);
    }

    public void start() {
        writeline("/home/ubuntu/results/coverage/Project/Project_3_10.coverage", "4c88f23e-5650-47f0-bd0f-23f9d74c3969");
        if (stopped || getState() == ProjectState.STOPPED) {
            writeline("/home/ubuntu/results/coverage/Project/Project_3_10.coverage", "57a011c4-9534-4b25-99b0-2ff4bdbb5ff1");
            stopped = false;
            writeline("/home/ubuntu/results/coverage/Project/Project_3_10.coverage", "db399f5b-b982-4ccb-b613-ec27200fa5d3");
            LOG.info("Project " + name + " starting");
            writeline("/home/ubuntu/results/coverage/Project/Project_3_10.coverage", "a968da40-3288-4b11-a237-445e1a27459b");
            setState(ProjectState.IDLE);
            writeline("/home/ubuntu/results/coverage/Project/Project_3_10.coverage", "9bbad751-84d7-4243-9e7b-292725c555a0");
            createNewSchedulingThread();
        }
    }

    protected void createNewSchedulingThread() {
        writeline("/home/ubuntu/results/coverage/Project/Project_3_10.coverage", "626d3189-fb3d-4b23-b0f0-36fc18fe5001");
        final Thread projectSchedulingThread = new Thread(this, "Project " + getName() + " thread");
        writeline("/home/ubuntu/results/coverage/Project/Project_3_10.coverage", "c01a4c41-d3b7-4694-9813-63c8e3bbea7d");
        projectSchedulingThread.start();
        writeline("/home/ubuntu/results/coverage/Project/Project_3_10.coverage", "ecc1d62d-3a9d-4cd9-b4d5-f489c2ab04aa");
        // brief nap to allow thread to start
        try {
            writeline("/home/ubuntu/results/coverage/Project/Project_3_10.coverage", "ab8a613e-bfae-4c58-887c-71031e12056b");
            Thread.sleep(100);
        } catch (InterruptedException ie) {
            writeline("/home/ubuntu/results/coverage/Project/Project_3_10.coverage", "c1067f62-defe-4a25-ad91-bc235de10402");
            LOG.warn("interrupted while waiting for scheduling thread to start", ie);
        }
    }

    public void stop() {
        writeline("/home/ubuntu/results/coverage/Project/Project_3_10.coverage", "f76be2bc-0c9c-4710-b301-a8b1da18e956");
        LOG.info("Project " + name + " stopping");
        writeline("/home/ubuntu/results/coverage/Project/Project_3_10.coverage", "c8a3455f-dfd3-4510-8e29-7949796b4c12");
        stopped = true;
        writeline("/home/ubuntu/results/coverage/Project/Project_3_10.coverage", "c40b8486-0c1c-4bff-b46c-e5b152b8cd4f");
        setState(ProjectState.STOPPED);
        writeline("/home/ubuntu/results/coverage/Project/Project_3_10.coverage", "3ce8e012-1ab4-43e0-a099-f55ccaa2eb18");
        synchronized (pausedMutex) {
            writeline("/home/ubuntu/results/coverage/Project/Project_3_10.coverage", "1b2db01a-f5b9-4c53-a982-b2281c21a0ce");
            pausedMutex.notifyAll();
        }
        writeline("/home/ubuntu/results/coverage/Project/Project_3_10.coverage", "11b51a69-8208-42ed-b937-d7de389d0dd1");
        synchronized (waitMutex) {
            writeline("/home/ubuntu/results/coverage/Project/Project_3_10.coverage", "656023b9-1445-48a7-8f9b-91f42598de98");
            waitMutex.notifyAll();
        }
        writeline("/home/ubuntu/results/coverage/Project/Project_3_10.coverage", "3c1fa524-67d2-4b2c-8b8f-6a2fabf1964f");
        synchronized (scheduleMutex) {
            writeline("/home/ubuntu/results/coverage/Project/Project_3_10.coverage", "65114bbc-a725-4192-ba40-6f2706df8c4b");
            scheduleMutex.notifyAll();
        }
    }

    public String toString() {
        writeline("/home/ubuntu/results/coverage/Project/Project_3_10.coverage", "412e8b2b-54e8-45a2-95f4-f85eef73388b");
        final StringBuilder sb = new StringBuilder("Project ");
        writeline("/home/ubuntu/results/coverage/Project/Project_3_10.coverage", "c95312fa-9508-461f-a897-bd6100600790");
        sb.append(getName());
        writeline("/home/ubuntu/results/coverage/Project/Project_3_10.coverage", "10efd098-c151-4fe0-bd33-6229ba01b414");
        sb.append(": ");
        writeline("/home/ubuntu/results/coverage/Project/Project_3_10.coverage", "96c1164d-192f-481f-a646-df4ecabf779c");
        sb.append(getStatus());
        writeline("/home/ubuntu/results/coverage/Project/Project_3_10.coverage", "6a36aa24-6441-45f5-95c0-7a0c47e84e53");
        if (isPaused) {
            writeline("/home/ubuntu/results/coverage/Project/Project_3_10.coverage", "b0f68d7d-3c6a-4b25-a509-db98ada2251f");
            sb.append(" (paused)");
        }
        writeline("/home/ubuntu/results/coverage/Project/Project_3_10.coverage", "f2e8cb86-32cc-457d-9a1c-d13f7547d5e6");
        return sb.toString();
    }

    public void addBuildProgressListener(final BuildProgressListener listener) {
        writeline("/home/ubuntu/results/coverage/Project/Project_3_10.coverage", "1921f2fc-d99b-41a9-87c0-2fe4184f62d9");
        synchronized (progressListeners) {
            writeline("/home/ubuntu/results/coverage/Project/Project_3_10.coverage", "57e92281-f4a1-40db-b689-9b97130a5f4e");
            progressListeners.add(listener);
        }
    }

    protected void fireProgressEvent(final BuildProgressEvent event) {
        writeline("/home/ubuntu/results/coverage/Project/Project_3_10.coverage", "d0470774-bf16-473b-a08b-4980fee05e33");
        synchronized (progressListeners) {
            writeline("/home/ubuntu/results/coverage/Project/Project_3_10.coverage", "e0db8f9e-2769-4161-b434-8f51d5caa1f9");
            for (final BuildProgressListener listener : progressListeners) {
                writeline("/home/ubuntu/results/coverage/Project/Project_3_10.coverage", "da544917-d0bc-4d13-a00c-bfbf70043281");
                listener.handleBuildProgress(event);
            }
        }
    }

    public void addBuildResultListener(final BuildResultListener listener) {
        writeline("/home/ubuntu/results/coverage/Project/Project_3_10.coverage", "76b3bb63-1794-4d56-9d56-847419939b56");
        synchronized (resultListeners) {
            writeline("/home/ubuntu/results/coverage/Project/Project_3_10.coverage", "a8cd0386-c2b8-4bc1-89f4-8fbd043ea873");
            resultListeners.add(listener);
        }
    }

    protected void fireResultEvent(final BuildResultEvent event) {
        writeline("/home/ubuntu/results/coverage/Project/Project_3_10.coverage", "0a48e96f-d321-4ffd-a808-ef977cbd550a");
        synchronized (resultListeners) {
            writeline("/home/ubuntu/results/coverage/Project/Project_3_10.coverage", "2b11a701-2769-4ca4-b1d7-f211d9baaedb");
            for (final BuildResultListener listener : resultListeners) {
                writeline("/home/ubuntu/results/coverage/Project/Project_3_10.coverage", "4c5cb004-1aa6-43e6-b54d-a885529e16b5");
                listener.handleBuildResult(event);
            }
        }
    }

    List<Listener> getListeners() {
        writeline("/home/ubuntu/results/coverage/Project/Project_3_10.coverage", "a15d66d9-e1de-4166-93aa-f9170e40c27e");
        return projectConfig.getListeners();
    }

    public void setProjectConfig(final ProjectConfig projectConfig) throws CruiseControlException {
        writeline("/home/ubuntu/results/coverage/Project/Project_3_10.coverage", "547d0a77-0834-44ed-9caa-94c218ddc1b9");
        if (projectConfig == null) {
            writeline("/home/ubuntu/results/coverage/Project/Project_3_10.coverage", "25ef9325-5134-4ea4-baa0-81f37b155fab");
            throw new IllegalArgumentException("project config can't be null");
        }
        writeline("/home/ubuntu/results/coverage/Project/Project_3_10.coverage", "3c1c7c78-f86e-4d0b-a781-34ec74faed7c");
        this.projectConfig = projectConfig;
        writeline("/home/ubuntu/results/coverage/Project/Project_3_10.coverage", "d94a84b5-79eb-487a-9f16-0cad8a6adc9d");
        setLabelIncrementer(projectConfig.getLabelIncrementer());
    }

    void notifyListeners(final ProjectEvent event) {
        writeline("/home/ubuntu/results/coverage/Project/Project_3_10.coverage", "2ca19165-73fd-4fd8-808c-5c67dd8cc70a");
        if (projectConfig == null) {
            writeline("/home/ubuntu/results/coverage/Project/Project_3_10.coverage", "985365d8-380a-4315-bce3-94579e6b603f");
            throw new IllegalStateException("projectConfig is null");
        }
        writeline("/home/ubuntu/results/coverage/Project/Project_3_10.coverage", "e7e41b8c-0334-4a6d-8871-a83163339bea");
        for (final Listener listener : projectConfig.getListeners()) {
            writeline("/home/ubuntu/results/coverage/Project/Project_3_10.coverage", "e5cffd39-5252-4e91-87b5-9fdeeb3acd4c");
            try {
                writeline("/home/ubuntu/results/coverage/Project/Project_3_10.coverage", "b02ac9c8-e53c-432f-9bf5-44249df792cd");
                listener.handleEvent(event);
            } catch (CruiseControlException e) {
                writeline("/home/ubuntu/results/coverage/Project/Project_3_10.coverage", "e218d023-0b21-4a3b-b70d-b3d2e6e55d85");
                final StringBuilder message = new StringBuilder("exception notifying listener ");
                writeline("/home/ubuntu/results/coverage/Project/Project_3_10.coverage", "6791cbf0-fa7d-422a-b4b1-0c1aca293d68");
                message.append(listener.getClass().getName());
                writeline("/home/ubuntu/results/coverage/Project/Project_3_10.coverage", "63ec137e-80e2-4e04-923a-96dcdd2361a5");
                message.append(" for project ").append(name);
                writeline("/home/ubuntu/results/coverage/Project/Project_3_10.coverage", "92ce0f35-eff6-49d9-94ce-274c0df78a73");
                LOG.error(message.toString(), e);
            }
        }
    }

    public boolean equals(final Object arg0) {
        writeline("/home/ubuntu/results/coverage/Project/Project_3_10.coverage", "97e296d5-fec9-4b14-9858-c049e6427ebb");
        if (arg0 == null) {
            writeline("/home/ubuntu/results/coverage/Project/Project_3_10.coverage", "dd358551-7d00-4807-a4e5-b551ea31d0ea");
            return false;
        }
        writeline("/home/ubuntu/results/coverage/Project/Project_3_10.coverage", "1ca40423-f9fa-4ffe-aa7e-6eea120d4239");
        if (arg0.getClass().getName().equals(getClass().getName())) {
            writeline("/home/ubuntu/results/coverage/Project/Project_3_10.coverage", "2a96a7fe-7f65-4031-b486-44711b83ee9f");
            final Project thatProject = (Project) arg0;
            writeline("/home/ubuntu/results/coverage/Project/Project_3_10.coverage", "e8f63045-44c3-4d21-bada-b8d5e2053883");
            return thatProject.name.equals(name);
        }
        writeline("/home/ubuntu/results/coverage/Project/Project_3_10.coverage", "8a46f556-7546-4bc7-9150-314087899da2");
        return false;
    }

    public int hashCode() {
        writeline("/home/ubuntu/results/coverage/Project/Project_3_10.coverage", "10663ba6-290f-4058-bbea-a1433229d65c");
        return name.hashCode();
    }

    public void register(final MBeanServer server) throws JMException {
        writeline("/home/ubuntu/results/coverage/Project/Project_3_10.coverage", "d1a80fd3-1b54-47e1-bb71-974f9cf3e7cd");
        LOG.debug("Registering project mbean");
        writeline("/home/ubuntu/results/coverage/Project/Project_3_10.coverage", "bc3ec624-ef6e-4046-b18a-99df287bc94b");
        final ProjectController projectController = new ProjectController(this);
        writeline("/home/ubuntu/results/coverage/Project/Project_3_10.coverage", "5e7d1dac-b611-4de4-b3c4-fb374cea7166");
        projectController.register(server);
    }

    public ProjectConfig getProjectConfig() {
        writeline("/home/ubuntu/results/coverage/Project/Project_3_10.coverage", "112cfe43-15f2-41b7-8954-deaf106df880");
        return projectConfig;
    }

    public Date getLastBuildDate() {
        writeline("/home/ubuntu/results/coverage/Project/Project_3_10.coverage", "b2dd1bbc-3429-41ef-888f-0e7093dd7d4e");
        return lastBuild;
    }

    public Progress getProgress() {
        writeline("/home/ubuntu/results/coverage/Project/Project_3_10.coverage", "253aa5c0-335d-4a3b-810e-13ee083955df");
        return progress;
    }

    public List<String> getLogLabels() {
        writeline("/home/ubuntu/results/coverage/Project/Project_3_10.coverage", "ee5cf26e-d3ad-4a39-8a57-d5f2e4c173e4");
        return projectConfig.getLogLabels();
    }

    public String[] getLogLabelLines(final String logLabel, final int firstLine) {
        writeline("/home/ubuntu/results/coverage/Project/Project_3_10.coverage", "7de2b4e1-4087-495a-9fa9-d7fe035af185");
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
