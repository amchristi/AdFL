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
        writeline("/home/ubuntu/results/coverage/Project/Project_6_10.coverage", "a7082755-db22-48c0-a018-d16332300d1a");
        state = ProjectState.STOPPED;
        writeline("/home/ubuntu/results/coverage/Project/Project_6_10.coverage", "df9a9fff-3c2a-4905-b60d-16beba6c8a30");
        pausedMutex = new Object();
        writeline("/home/ubuntu/results/coverage/Project/Project_6_10.coverage", "824a85ad-06cc-45be-a466-87a1eb2d5b17");
        scheduleMutex = new Object();
        writeline("/home/ubuntu/results/coverage/Project/Project_6_10.coverage", "9b8a47dd-93e3-4f0e-a406-1f07c7e3b889");
        waitMutex = new Object();
        writeline("/home/ubuntu/results/coverage/Project/Project_6_10.coverage", "662edfdc-14a8-4676-b405-77024d46e760");
        progressListeners = new ArrayList<BuildProgressListener>();
        writeline("/home/ubuntu/results/coverage/Project/Project_6_10.coverage", "de5ec46a-c017-426f-8953-2042d01e6d43");
        resultListeners = new ArrayList<BuildResultListener>();
        writeline("/home/ubuntu/results/coverage/Project/Project_6_10.coverage", "b2d84cb0-de93-4de9-ac6e-bae819902d94");
        progress = new ProgressImpl(this);
    }

    private void readObject(final ObjectInputStream stream) throws IOException, ClassNotFoundException {
        writeline("/home/ubuntu/results/coverage/Project/Project_6_10.coverage", "9871cfac-58e1-44bd-b475-acb951a727be");
        stream.defaultReadObject();
        writeline("/home/ubuntu/results/coverage/Project/Project_6_10.coverage", "f8c123c5-b052-4fe1-9191-8e7661e9072a");
        initializeTransientFields();
    }

    public void execute() {
        writeline("/home/ubuntu/results/coverage/Project/Project_6_10.coverage", "013746d4-a8d6-4d24-86c7-5cde9e7e5425");
        if (stopped) {
            writeline("/home/ubuntu/results/coverage/Project/Project_6_10.coverage", "492e4887-467c-443f-bb00-cd9d086879bf");
            LOG.warn("not building project " + name + " because project has been stopped.");
            writeline("/home/ubuntu/results/coverage/Project/Project_6_10.coverage", "317ae077-b564-48c8-817d-b5c0585eda91");
            buildFinished();
            writeline("/home/ubuntu/results/coverage/Project/Project_6_10.coverage", "f9efb50a-aca1-4db7-93db-d37ae102e106");
            return;
        }
        writeline("/home/ubuntu/results/coverage/Project/Project_6_10.coverage", "540ba76d-bd53-417d-8e2d-ebfaf0795782");
        synchronized (pausedMutex) {
            writeline("/home/ubuntu/results/coverage/Project/Project_6_10.coverage", "a4dea70f-f5fa-43b1-bc7c-52198fae21bf");
            if (isPaused) {
                writeline("/home/ubuntu/results/coverage/Project/Project_6_10.coverage", "1d01cc5e-f543-41a9-bf26-d5feb717154c");
                LOG.info("not building project " + name + " because project has been paused.");
                writeline("/home/ubuntu/results/coverage/Project/Project_6_10.coverage", "432b3d20-c639-40af-a7b9-8c35284c06a7");
                buildFinished();
                writeline("/home/ubuntu/results/coverage/Project/Project_6_10.coverage", "c8973b0a-b854-4eca-8252-eeae9c6934d9");
                return;
            }
        }
        writeline("/home/ubuntu/results/coverage/Project/Project_6_10.coverage", "fcab95e0-8b31-4d57-9612-f9b4d4ccc03c");
        try {
            writeline("/home/ubuntu/results/coverage/Project/Project_6_10.coverage", "1e14dfa6-da67-4ec1-aee8-a7063be18cdd");
            init();
            writeline("/home/ubuntu/results/coverage/Project/Project_6_10.coverage", "82485d36-eb37-4fe2-9604-03b708192b6d");
            build();
        } catch (CruiseControlException e) {
            writeline("/home/ubuntu/results/coverage/Project/Project_6_10.coverage", "22768a05-9b35-49a1-8b06-92094447aa97");
            LOG.error("exception attempting build in project " + name, e);
        } finally {
            writeline("/home/ubuntu/results/coverage/Project/Project_6_10.coverage", "3f73c0a8-254d-4752-a476-d052ecbe52cd");
            buildFinished();
        }
    }

    /**
     * Unless paused, runs any bootstrappers and then the entire build.
     * @throws CruiseControlException if an error occurs during the build
     */
    protected void build() throws CruiseControlException {
        writeline("/home/ubuntu/results/coverage/Project/Project_6_10.coverage", "221a07aa-9477-43fb-b2be-0e6ea9e432cc");
        if (projectConfig == null) {
            writeline("/home/ubuntu/results/coverage/Project/Project_6_10.coverage", "5026ba8c-e558-4cdc-b592-5d0495685f6c");
            throw new IllegalStateException("projectConfig must be set on project before calling build()");
        }
        writeline("/home/ubuntu/results/coverage/Project/Project_6_10.coverage", "d261ad6b-8aa2-4529-a604-9aa5ef1cb6ad");
        if (stopped) {
            writeline("/home/ubuntu/results/coverage/Project/Project_6_10.coverage", "3fa8022b-fea2-45be-ac1a-fc89812c7c9a");
            LOG.warn("not building project " + name + " because project has been stopped.");
            writeline("/home/ubuntu/results/coverage/Project/Project_6_10.coverage", "f2fd633e-d774-47f2-9a26-c88d13d720fa");
            return;
        }
        writeline("/home/ubuntu/results/coverage/Project/Project_6_10.coverage", "aa473bc2-1351-43f2-9811-b725a826c0ed");
        // or if the last build faild and we want to build on failures
        if (forceOnly && !buildForced && !(!wasLastBuildSuccessful && buildAfterFailed)) {
            writeline("/home/ubuntu/results/coverage/Project/Project_6_10.coverage", "73f0657c-a47f-4fed-b5b7-4f00b01b0327");
            info("not building because project is forceOnly and build not forced.");
            writeline("/home/ubuntu/results/coverage/Project/Project_6_10.coverage", "71d728fd-479c-4bb6-a16f-a000395fff50");
            return;
        }
        writeline("/home/ubuntu/results/coverage/Project/Project_6_10.coverage", "45fe86b2-3484-4228-9536-e56720f572bc");
        final boolean buildWasForced = buildForced;
        writeline("/home/ubuntu/results/coverage/Project/Project_6_10.coverage", "78da0dd2-3f3a-4bf9-947c-6a2703fe6bea");
        try {
            writeline("/home/ubuntu/results/coverage/Project/Project_6_10.coverage", "41aa332b-74b5-4f53-b9be-7695e778c108");
            setBuildStartTime(new Date());
            writeline("/home/ubuntu/results/coverage/Project/Project_6_10.coverage", "0d7984d0-369c-40f8-84fe-1190ffde9bab");
            final Schedule schedule = projectConfig.getSchedule();
            writeline("/home/ubuntu/results/coverage/Project/Project_6_10.coverage", "4c042827-c183-4b8d-9623-c8d40413cd2c");
            if (schedule == null) {
                writeline("/home/ubuntu/results/coverage/Project/Project_6_10.coverage", "7eda8a3d-73f3-4e56-82ae-739b3cc767af");
                throw new IllegalStateException("project must have a schedule");
            }
            writeline("/home/ubuntu/results/coverage/Project/Project_6_10.coverage", "becf5f15-e537-4df5-8936-67cd6a3f48d4");
            if (schedule.isPaused(buildStartTime)) {
                writeline("/home/ubuntu/results/coverage/Project/Project_6_10.coverage", "5d974535-e3e7-4573-8bac-d82632edb2be");
                // is different than ProjectState.PAUSED
                return;
            }
            writeline("/home/ubuntu/results/coverage/Project/Project_6_10.coverage", "7d0aa71e-46f4-42f0-b034-90c1cd47e1b6");
            // @todo Add Progress param to Bootstrapper API?
            bootstrap();
            writeline("/home/ubuntu/results/coverage/Project/Project_6_10.coverage", "6d9e1dae-e91d-41b0-80a4-7a43632d26b1");
            final String target = useAndResetBuildTargetIfBuildWasForced(buildWasForced);
            writeline("/home/ubuntu/results/coverage/Project/Project_6_10.coverage", "8dda819b-eaf5-4ebc-acdc-d411f7d5a51a");
            // @todo Add Progress param to ModificationSet API?
            // getModifications will only return null if we don't need to build
            final Element modifications = getModifications(buildWasForced);
            writeline("/home/ubuntu/results/coverage/Project/Project_6_10.coverage", "740ed549-2617-40ee-8598-ad23168f5a22");
            if (modifications == null) {
                writeline("/home/ubuntu/results/coverage/Project/Project_6_10.coverage", "26926fdf-6533-45fe-9c02-235fa7230e92");
                return;
            }
            writeline("/home/ubuntu/results/coverage/Project/Project_6_10.coverage", "f9a4f20a-a874-4136-a036-e49318030601");
            // Using local reference to avoid NPE if config.xml is updated during build
            final Log buildLog = projectConfig.getLog();
            writeline("/home/ubuntu/results/coverage/Project/Project_6_10.coverage", "0e992db1-ebba-4c2d-b889-ba3685ce5dea");
            buildLog.addContent(modifications);
            writeline("/home/ubuntu/results/coverage/Project/Project_6_10.coverage", "09ce8fd3-1238-42d8-8e19-70714fc22824");
            final Date now;
            writeline("/home/ubuntu/results/coverage/Project/Project_6_10.coverage", "ab3cdf5f-b530-4cd0-9cd8-a1b91861174d");
            if (projectConfig.getModificationSet() != null && projectConfig.getModificationSet().getTimeOfCheck() != null) {
                writeline("/home/ubuntu/results/coverage/Project/Project_6_10.coverage", "089f37f5-0f5f-462e-aa26-f3709bace890");
                now = projectConfig.getModificationSet().getTimeOfCheck();
            } else {
                writeline("/home/ubuntu/results/coverage/Project/Project_6_10.coverage", "28262e3c-c2ef-445b-9c4b-e4b0043aca26");
                now = new Date();
            }
            writeline("/home/ubuntu/results/coverage/Project/Project_6_10.coverage", "0ad4a2f6-1bff-4281-8610-fe5d994d6ce1");
            if (getLabelIncrementer().isPreBuildIncrementer()) {
                writeline("/home/ubuntu/results/coverage/Project/Project_6_10.coverage", "9e54cf99-1aa7-4bc4-9114-beec29605cd0");
                label = getLabelIncrementer().incrementLabel(label, buildLog.getContent());
            }
            writeline("/home/ubuntu/results/coverage/Project/Project_6_10.coverage", "04879271-feeb-4365-b2c9-8f4ae384a0a5");
            // collect project information
            buildLog.addContent(getProjectPropertiesElement(now));
            writeline("/home/ubuntu/results/coverage/Project/Project_6_10.coverage", "7615a435-873d-4cef-95af-4be0b875cf68");
            setState(ProjectState.BUILDING);
            writeline("/home/ubuntu/results/coverage/Project/Project_6_10.coverage", "3cac1768-037e-4469-8c4a-60ce9a4954d3");
            final Element builderLog = schedule.build(buildCounter, lastBuild, now, getProjectPropertiesMap(now), target, progress);
            writeline("/home/ubuntu/results/coverage/Project/Project_6_10.coverage", "a601935c-7e0a-4926-83e0-4d8e6fb8a2ba");
            buildLog.addContent(builderLog.detach());
            writeline("/home/ubuntu/results/coverage/Project/Project_6_10.coverage", "ecc1f2c5-b1f7-462f-bf8a-0dca81123709");
            boolean buildSuccessful = buildLog.wasBuildSuccessful();
            writeline("/home/ubuntu/results/coverage/Project/Project_6_10.coverage", "3cc2adc8-2f75-4a86-a2ec-39b74dc10dd0");
            fireResultEvent(new BuildResultEvent(this, buildSuccessful));
            writeline("/home/ubuntu/results/coverage/Project/Project_6_10.coverage", "e7e2692f-c554-4f1c-bb91-656d496c3c69");
            if (!getLabelIncrementer().isPreBuildIncrementer() && buildSuccessful) {
                writeline("/home/ubuntu/results/coverage/Project/Project_6_10.coverage", "953de8ab-9bef-4968-be53-9aa1302723b7");
                label = getLabelIncrementer().incrementLabel(label, buildLog.getContent());
            }
            writeline("/home/ubuntu/results/coverage/Project/Project_6_10.coverage", "0adfaca9-e9e1-4aa6-b679-8ed83a24d9ec");
            setState(ProjectState.MERGING_LOGS);
            writeline("/home/ubuntu/results/coverage/Project/Project_6_10.coverage", "7e8732eb-f385-4672-b363-12ba85525520");
            buildLog.writeLogFile(now);
            writeline("/home/ubuntu/results/coverage/Project/Project_6_10.coverage", "2f05fac6-3dcd-4250-a2e2-6e1052b6bd0c");
            // regardless of success or failure (buildAfterFailed = false in config.xml)
            if (!buildAfterFailed) {
                writeline("/home/ubuntu/results/coverage/Project/Project_6_10.coverage", "2e49ddc3-39eb-414d-b1e3-7d69ea47992f");
                lastBuild = now;
            }
            writeline("/home/ubuntu/results/coverage/Project/Project_6_10.coverage", "afbca602-1986-4d0a-9db0-9fac4d7645e7");
            // If this was a successful build, update both last build and last successful build
            if (buildSuccessful) {
                writeline("/home/ubuntu/results/coverage/Project/Project_6_10.coverage", "cd440580-8955-41a1-b4c7-c4dc65248c70");
                lastBuild = now;
                writeline("/home/ubuntu/results/coverage/Project/Project_6_10.coverage", "c8349322-cdaf-47ce-b56e-8e4bd58f86cb");
                lastSuccessfulBuild = now;
                writeline("/home/ubuntu/results/coverage/Project/Project_6_10.coverage", "b31052c6-00ff-42ac-b618-996581b39a2a");
                info("build successful");
            } else {
                writeline("/home/ubuntu/results/coverage/Project/Project_6_10.coverage", "97aa7325-1c9f-480c-8148-c769f9ebc9bd");
                info("build failed");
            }
            writeline("/home/ubuntu/results/coverage/Project/Project_6_10.coverage", "3fa1725d-669f-43fe-93a1-a2ceea9575c4");
            buildCounter++;
            writeline("/home/ubuntu/results/coverage/Project/Project_6_10.coverage", "9488e29a-fc4f-42a2-9770-459f689a3d16");
            setWasLastBuildSuccessful(buildSuccessful);
            writeline("/home/ubuntu/results/coverage/Project/Project_6_10.coverage", "447d274b-6027-43ee-9991-20d14568783f");
            // also need to reset forced flag before serializing, unless buildForced var is transient
            // resetBuildForcedOnlyIfBuildWasForced(buildWasForced);
            serializeProject();
            writeline("/home/ubuntu/results/coverage/Project/Project_6_10.coverage", "ca4ad9db-f9ff-434c-9600-039d0b030cc3");
            // @todo Add Progress param to Publisher API?
            publish(buildLog);
            writeline("/home/ubuntu/results/coverage/Project/Project_6_10.coverage", "73462926-28d7-4853-a77b-e741ba6559f6");
            buildLog.reset();
        } finally {
            writeline("/home/ubuntu/results/coverage/Project/Project_6_10.coverage", "4986dec2-00b4-4b83-9351-6b02fad8f5e1");
            resetBuildForcedOnlyIfBuildWasForced(buildWasForced);
            writeline("/home/ubuntu/results/coverage/Project/Project_6_10.coverage", "dc4e5ec8-d8f1-4bae-88c8-fb46cc412c56");
            setState(ProjectState.IDLE);
        }
    }

    private String useAndResetBuildTargetIfBuildWasForced(final boolean buildWasForced) {
        writeline("/home/ubuntu/results/coverage/Project/Project_6_10.coverage", "d4512b5f-bb8b-45f3-99c6-d586e8fce0d3");
        String target = null;
        writeline("/home/ubuntu/results/coverage/Project/Project_6_10.coverage", "dc271116-d5f9-4515-ba42-ca8574c65c77");
        if (buildWasForced) {
            writeline("/home/ubuntu/results/coverage/Project/Project_6_10.coverage", "0bd552e3-d6c2-461d-860d-6beb2e122879");
            target = buildTarget;
            writeline("/home/ubuntu/results/coverage/Project/Project_6_10.coverage", "ac9d8750-6460-4e9f-8f31-8be24d0aa960");
            buildTarget = null;
        }
        writeline("/home/ubuntu/results/coverage/Project/Project_6_10.coverage", "6cfcdd5c-7ba8-4100-b404-af413fc7421c");
        return target;
    }

    private void resetBuildForcedOnlyIfBuildWasForced(final boolean buildWasForced) {
        writeline("/home/ubuntu/results/coverage/Project/Project_6_10.coverage", "93871d2d-7c4d-46ca-9aff-c65934fb7256");
        if (buildWasForced) {
            writeline("/home/ubuntu/results/coverage/Project/Project_6_10.coverage", "62a8f2f0-c7a5-4aa6-a5da-bf0e4da3bcec");
            buildForced = false;
        }
    }

    void setBuildStartTime(final Date date) {
        writeline("/home/ubuntu/results/coverage/Project/Project_6_10.coverage", "f3b53a45-280b-4570-b676-0c0783688800");
        buildStartTime = date;
    }

    public void run() {
        writeline("/home/ubuntu/results/coverage/Project/Project_6_10.coverage", "3513d1ef-6517-44c8-a054-2111818e3dea");
        LOG.info("Project " + name + " started");
        writeline("/home/ubuntu/results/coverage/Project/Project_6_10.coverage", "f42d040b-d6ea-465b-b7fe-3a5be0bbb9bd");
        try {
            writeline("/home/ubuntu/results/coverage/Project/Project_6_10.coverage", "4967d73c-5223-4c71-9c09-4797ba813380");
            while (!stopped) {
                writeline("/home/ubuntu/results/coverage/Project/Project_6_10.coverage", "71661d8c-c154-4d16-a8e8-227acf97d90a");
                try {
                    writeline("/home/ubuntu/results/coverage/Project/Project_6_10.coverage", "3679b1fc-0398-4e49-b4f5-f5fdc5ab6ae4");
                    waitIfPaused();
                    writeline("/home/ubuntu/results/coverage/Project/Project_6_10.coverage", "f297ff7c-70d3-4e86-ba82-271977ba48b0");
                    if (!stopped) {
                        writeline("/home/ubuntu/results/coverage/Project/Project_6_10.coverage", "79ea69bd-c8fb-4ed2-8d74-87eb2f0a0e2a");
                        waitForNextBuild();
                    }
                    writeline("/home/ubuntu/results/coverage/Project/Project_6_10.coverage", "415ffb5c-da88-4076-9e43-0f4ac78ded90");
                    if (!stopped) {
                        writeline("/home/ubuntu/results/coverage/Project/Project_6_10.coverage", "c72ca904-347d-48e6-acf3-a3b461119a22");
                        setState(ProjectState.QUEUED);
                        writeline("/home/ubuntu/results/coverage/Project/Project_6_10.coverage", "3cf22aba-fd2e-47f3-8255-9bdb3d0da3a1");
                        synchronized (scheduleMutex) {
                            writeline("/home/ubuntu/results/coverage/Project/Project_6_10.coverage", "2e211731-028d-40f2-a88c-256c8faa4c35");
                            queue.requestBuild(projectConfig);
                            writeline("/home/ubuntu/results/coverage/Project/Project_6_10.coverage", "f652ecf1-d973-4d48-a361-26ad3d0b0fd0");
                            waitForBuildToFinish();
                        }
                    }
                } catch (InterruptedException e) {
                    writeline("/home/ubuntu/results/coverage/Project/Project_6_10.coverage", "80e64b25-624c-4549-8c5f-859913efaa15");
                    final String message = "Project " + name + ".run() interrupted";
                    writeline("/home/ubuntu/results/coverage/Project/Project_6_10.coverage", "06133689-2b89-42b2-83cc-24185c597e2e");
                    LOG.error(message, e);
                    writeline("/home/ubuntu/results/coverage/Project/Project_6_10.coverage", "2c7c4bba-cf04-44e8-8521-89412d54f822");
                    throw new RuntimeException(message);
                }
            }
        } finally {
            writeline("/home/ubuntu/results/coverage/Project/Project_6_10.coverage", "3d9ee3fe-dd95-438b-b94d-4d825ea44656");
            stopped = true;
            writeline("/home/ubuntu/results/coverage/Project/Project_6_10.coverage", "5574705b-4dbc-4dea-a274-1567b735585a");
            LOG.info("Project " + name + " stopped");
        }
    }

    void waitIfPaused() throws InterruptedException {
        writeline("/home/ubuntu/results/coverage/Project/Project_6_10.coverage", "76162bb4-3057-4b47-841c-f3b48ffa06ef");
        synchronized (pausedMutex) {
            writeline("/home/ubuntu/results/coverage/Project/Project_6_10.coverage", "c69fb8af-a8e5-47c9-a06f-13bbe4ebbee0");
            while (isPaused) {
                writeline("/home/ubuntu/results/coverage/Project/Project_6_10.coverage", "b5c7a93b-2229-4f1d-9319-614045bff500");
                setState(ProjectState.PAUSED);
                writeline("/home/ubuntu/results/coverage/Project/Project_6_10.coverage", "22733b88-685e-4d1a-b6ee-170e11064d73");
                pausedMutex.wait(10 * DateUtil.ONE_MINUTE);
            }
        }
    }

    void waitForNextBuild() throws InterruptedException {
        writeline("/home/ubuntu/results/coverage/Project/Project_6_10.coverage", "5d05e6e0-906e-4774-b49a-7163d14b7b1c");
        long waitTime = getTimeToNextBuild(new Date());
        writeline("/home/ubuntu/results/coverage/Project/Project_6_10.coverage", "5298813c-cb32-412d-a2f0-3086cd70ca23");
        if (needToWaitForNextBuild(waitTime) && !buildForced) {
            writeline("/home/ubuntu/results/coverage/Project/Project_6_10.coverage", "308e8040-bcaa-42ee-a759-13b9ff77a503");
            final String msg = "next build in " + DateUtil.formatTime(waitTime);
            writeline("/home/ubuntu/results/coverage/Project/Project_6_10.coverage", "664fabc5-e2a5-4059-b003-80be390ed87e");
            info(msg);
            writeline("/home/ubuntu/results/coverage/Project/Project_6_10.coverage", "cf3238f0-2ae7-4c9b-81f9-a4d8be1a2132");
            synchronized (waitMutex) {
                writeline("/home/ubuntu/results/coverage/Project/Project_6_10.coverage", "162d57db-2d7a-4404-843c-92ef71a9e27b");
                setState(ProjectState.WAITING);
                writeline("/home/ubuntu/results/coverage/Project/Project_6_10.coverage", "0ef71e07-278a-42d5-9755-471a8a9cfb69");
                progress.setValue(msg);
                writeline("/home/ubuntu/results/coverage/Project/Project_6_10.coverage", "eee70502-5b1f-45c4-ae85-da0b5d584a4e");
                waitMutex.wait(waitTime);
            }
        }
    }

    long getTimeToNextBuild(Date now) {
        writeline("/home/ubuntu/results/coverage/Project/Project_6_10.coverage", "d5fce759-d3bb-41b4-8d4f-768e449733d6");
        long waitTime = projectConfig.getSchedule().getTimeToNextBuild(now, getBuildInterval());
        writeline("/home/ubuntu/results/coverage/Project/Project_6_10.coverage", "7378542f-f0f2-4b75-88a9-bab35961e9a9");
        if (waitTime == 0) {
            writeline("/home/ubuntu/results/coverage/Project/Project_6_10.coverage", "107ddf70-71ec-46f0-a24e-ae6bce9a2d34");
            // project that has just built within a minute time
            if (buildStartTime != null) {
                writeline("/home/ubuntu/results/coverage/Project/Project_6_10.coverage", "d851685e-4057-477e-970e-1481e5a22fa8");
                long millisSinceLastBuild = now.getTime() - buildStartTime.getTime();
                writeline("/home/ubuntu/results/coverage/Project/Project_6_10.coverage", "860ee8ef-e2ba-4ea8-b994-4c037fc830fe");
                if (millisSinceLastBuild < Schedule.ONE_MINUTE) {
                    writeline("/home/ubuntu/results/coverage/Project/Project_6_10.coverage", "5518015e-3230-4b90-a2db-c914f666c9d8");
                    debug("build finished within a minute, getting new time to next build");
                    writeline("/home/ubuntu/results/coverage/Project/Project_6_10.coverage", "c3d44be9-f3f3-4a4b-897b-7131ccef754f");
                    Date oneMinuteInFuture = new Date(now.getTime() + Schedule.ONE_MINUTE);
                    writeline("/home/ubuntu/results/coverage/Project/Project_6_10.coverage", "36b9878f-cd5b-40d4-b32a-52b1c8849756");
                    waitTime = projectConfig.getSchedule().getTimeToNextBuild(oneMinuteInFuture, getBuildInterval());
                    writeline("/home/ubuntu/results/coverage/Project/Project_6_10.coverage", "7145e2af-1717-4b29-b5af-ca7856a40d2d");
                    waitTime += Schedule.ONE_MINUTE;
                }
            }
        }
        writeline("/home/ubuntu/results/coverage/Project/Project_6_10.coverage", "39939bb6-4186-4f68-8c38-e981742ac602");
        return waitTime;
    }

    static boolean needToWaitForNextBuild(long waitTime) {
        writelineStatic("/home/ubuntu/results/coverage/Project/Project_6_10.coverage", "4d56848a-5812-40f0-95d0-340098f878a6");
        return waitTime > 0;
    }

    /**
     * @return true if build was forced, intended for unit testing only.
     */
    boolean isBuildForced() {
        writeline("/home/ubuntu/results/coverage/Project/Project_6_10.coverage", "110ecda1-3698-4b6c-b75a-8272438606bc");
        return buildForced;
    }

    void forceBuild() {
        writeline("/home/ubuntu/results/coverage/Project/Project_6_10.coverage", "21246fe9-f21b-4e3d-a369-b2ff283b90e3");
        synchronized (waitMutex) {
            writeline("/home/ubuntu/results/coverage/Project/Project_6_10.coverage", "0e7b43e7-c9e0-4379-b36d-51e7ec4792b7");
            waitMutex.notify();
        }
    }

    public void forceBuildWithTarget(String buildTarget) {
        writeline("/home/ubuntu/results/coverage/Project/Project_6_10.coverage", "cdc4b555-5063-47d6-a35d-4e3d4889e174");
        this.buildTarget = buildTarget;
        writeline("/home/ubuntu/results/coverage/Project/Project_6_10.coverage", "2d840bc2-2e0b-46fd-951b-158c4eaa3260");
        setBuildForced(true);
    }

    public void forceBuildWithTarget(String buildTarget, Map<String, String> addedProperties) {
        writeline("/home/ubuntu/results/coverage/Project/Project_6_10.coverage", "07dfed51-1146-4916-bf9e-5475906f4380");
        additionalProperties = addedProperties;
        writeline("/home/ubuntu/results/coverage/Project/Project_6_10.coverage", "482cfd3c-945c-4598-95a6-6a093731acc7");
        forceBuildWithTarget(buildTarget);
    }

    void waitForBuildToFinish() throws InterruptedException {
        writeline("/home/ubuntu/results/coverage/Project/Project_6_10.coverage", "3993463e-1d34-4bb8-a9a1-99c2013e5d66");
        synchronized (scheduleMutex) {
            writeline("/home/ubuntu/results/coverage/Project/Project_6_10.coverage", "b5bd04fc-4c9e-40cd-862c-c466b3dde00e");
            debug("waiting for build to finish");
            writeline("/home/ubuntu/results/coverage/Project/Project_6_10.coverage", "5a0f7f8d-bd98-4bdf-95c9-ac7483bfe662");
            scheduleMutex.wait();
        }
    }

    void buildFinished() {
        writeline("/home/ubuntu/results/coverage/Project/Project_6_10.coverage", "410f3f36-cea5-4b96-9d81-e535d9d87b42");
        synchronized (scheduleMutex) {
            writeline("/home/ubuntu/results/coverage/Project/Project_6_10.coverage", "3d93e5b7-507d-46ac-a5c8-a6a973651786");
            debug("build finished");
            writeline("/home/ubuntu/results/coverage/Project/Project_6_10.coverage", "7fa37b02-aa9e-49b2-aee9-1edd5ec02617");
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
        writeline("/home/ubuntu/results/coverage/Project/Project_6_10.coverage", "e23230a0-e471-4f68-8c46-9bd874059269");
        setState(ProjectState.MODIFICATIONSET);
        writeline("/home/ubuntu/results/coverage/Project/Project_6_10.coverage", "2e8eaf7b-a7d4-4892-89ac-e9c35c738110");
        final ModificationSet modificationSet = projectConfig.getModificationSet();
        writeline("/home/ubuntu/results/coverage/Project/Project_6_10.coverage", "9f9e459b-2fdf-4fcf-a5f5-a965c896ec6a");
        if (modificationSet == null) {
            writeline("/home/ubuntu/results/coverage/Project/Project_6_10.coverage", "0ea83b0a-e2a7-4080-988d-1e853d301fad");
            debug("no modification set, nothing to detect.");
            writeline("/home/ubuntu/results/coverage/Project/Project_6_10.coverage", "0009f9f9-7b4a-4a00-bab3-78bef453c5b2");
            if (buildWasForced) {
                writeline("/home/ubuntu/results/coverage/Project/Project_6_10.coverage", "37bb6076-0aad-4ba2-9387-07f6010ed6a4");
                info("no modification set but build was forced");
                writeline("/home/ubuntu/results/coverage/Project/Project_6_10.coverage", "90e92cba-4a42-4545-859f-df08b003c37d");
                return new Element("modifications");
            }
            writeline("/home/ubuntu/results/coverage/Project/Project_6_10.coverage", "60ddd89c-0c95-4739-8aee-3d6329f71bf0");
            if (!requiremodification) {
                writeline("/home/ubuntu/results/coverage/Project/Project_6_10.coverage", "6795a2f4-4916-4d6d-bf57-5eb86471d23e");
                info("no modification set but no modifications required");
                writeline("/home/ubuntu/results/coverage/Project/Project_6_10.coverage", "780769e8-d603-4021-93ac-04ce9e314d3f");
                return new Element("modifications");
            }
            writeline("/home/ubuntu/results/coverage/Project/Project_6_10.coverage", "777faea3-862e-4216-b1f3-4ab15622e09b");
            return null;
        }
        writeline("/home/ubuntu/results/coverage/Project/Project_6_10.coverage", "48e14469-9f7d-4a68-ad89-d77c078b717c");
        final boolean checkNewChangesFirst = checkOnlySinceLastBuild();
        writeline("/home/ubuntu/results/coverage/Project/Project_6_10.coverage", "280225a5-d71b-4c31-bf74-aa59ea698bab");
        Element modifications;
        writeline("/home/ubuntu/results/coverage/Project/Project_6_10.coverage", "1ce0d845-1ada-4b2a-a405-a870de9b3d17");
        if (checkNewChangesFirst) {
            writeline("/home/ubuntu/results/coverage/Project/Project_6_10.coverage", "76c33294-bceb-4b97-bda3-e2022d144586");
            debug("getting changes since last build");
            writeline("/home/ubuntu/results/coverage/Project/Project_6_10.coverage", "8983fb1c-aa03-4de6-b232-94d37db760c8");
            modifications = modificationSet.retrieveModificationsAsElement(lastBuild, progress);
        } else {
            writeline("/home/ubuntu/results/coverage/Project/Project_6_10.coverage", "abe5b48e-e3c9-4cef-9597-ceb7f3320cb9");
            debug("getting changes since last successful build");
            writeline("/home/ubuntu/results/coverage/Project/Project_6_10.coverage", "c8615a5f-c7a3-4526-9f4d-fdcea7161c8d");
            modifications = modificationSet.retrieveModificationsAsElement(lastSuccessfulBuild, progress);
        }
        writeline("/home/ubuntu/results/coverage/Project/Project_6_10.coverage", "dd884cd1-e814-4113-80eb-36eaf6f90dc4");
        if (!modificationSet.isModified()) {
            writeline("/home/ubuntu/results/coverage/Project/Project_6_10.coverage", "25e004f0-ba34-4b4b-96ea-c0d0d25474e8");
            info("No modifications found, build not necessary.");
            writeline("/home/ubuntu/results/coverage/Project/Project_6_10.coverage", "bbe0a883-b786-42a9-98ad-ddaa7263c287");
            // * build forced
            if (buildAfterFailed && !wasLastBuildSuccessful) {
                writeline("/home/ubuntu/results/coverage/Project/Project_6_10.coverage", "aa10a1da-1c63-40d0-9ce9-51d2ef646ee9");
                info("Building anyway, since buildAfterFailed is true and last build failed.");
            } else if (!requiremodification) {
                writeline("/home/ubuntu/results/coverage/Project/Project_6_10.coverage", "c59e24a6-6139-48c4-afd1-a37a54d2b109");
                info("Building anyway, since modifications not required");
            } else {
                writeline("/home/ubuntu/results/coverage/Project/Project_6_10.coverage", "cdf8e3ac-78e2-4979-8fda-c926f1c74577");
                if (buildWasForced) {
                    writeline("/home/ubuntu/results/coverage/Project/Project_6_10.coverage", "54a70128-49de-4c93-ba25-831cc469f3e1");
                    info("Building anyway, since build was explicitly forced.");
                } else {
                    writeline("/home/ubuntu/results/coverage/Project/Project_6_10.coverage", "be1df2c6-4444-4c7b-bea9-d1bcf3b51c48");
                    return null;
                }
            }
        }
        writeline("/home/ubuntu/results/coverage/Project/Project_6_10.coverage", "b2531fac-541f-4d86-86c9-e532ae4413e3");
        if (checkNewChangesFirst) {
            writeline("/home/ubuntu/results/coverage/Project/Project_6_10.coverage", "e36997e4-45b9-4c0a-ac72-e1232c187d03");
            debug("new changes found; now getting complete set");
            writeline("/home/ubuntu/results/coverage/Project/Project_6_10.coverage", "a119cf04-3af9-4574-a0c4-e812c739207e");
            modifications = modificationSet.retrieveModificationsAsElement(lastSuccessfulBuild, progress);
        }
        writeline("/home/ubuntu/results/coverage/Project/Project_6_10.coverage", "268a1435-679f-4c5e-a504-d7d8399ae68e");
        return modifications;
    }

    /**
     * @return boolean
     */
    boolean checkOnlySinceLastBuild() {
        writeline("/home/ubuntu/results/coverage/Project/Project_6_10.coverage", "55208fb1-85dc-4ef2-9d5e-b5f5a9496199");
        if (lastBuild == null || lastSuccessfulBuild == null) {
            writeline("/home/ubuntu/results/coverage/Project/Project_6_10.coverage", "71d7e28b-ff5e-4209-a0f0-8f4b8290aa5a");
            return false;
        }
        writeline("/home/ubuntu/results/coverage/Project/Project_6_10.coverage", "40a4e881-6b0b-4da5-a67e-7b6b204317d8");
        final long lastBuildLong = lastBuild.getTime();
        writeline("/home/ubuntu/results/coverage/Project/Project_6_10.coverage", "59d8ab3a-4800-4117-a572-0f5ebcd19057");
        final long timeDifference = lastBuildLong - lastSuccessfulBuild.getTime();
        writeline("/home/ubuntu/results/coverage/Project/Project_6_10.coverage", "affd16c8-58bc-4827-8061-fed48e428827");
        final boolean moreThanASecond = timeDifference > DateUtil.ONE_SECOND;
        writeline("/home/ubuntu/results/coverage/Project/Project_6_10.coverage", "119e9550-104c-441e-9ab1-ba54daa120cc");
        return !buildAfterFailed && moreThanASecond;
    }

    /**
     * Serialize the project to allow resumption after a process bounce
     */
    public void serializeProject() {
        writeline("/home/ubuntu/results/coverage/Project/Project_6_10.coverage", "e5746b28-0f7d-452a-8568-736dddc4174d");
        final String safeProjectName = Builder.getFileSystemSafeProjectName(name);
        writeline("/home/ubuntu/results/coverage/Project/Project_6_10.coverage", "54f757bf-8885-40e9-b1b1-b4e4cc53279d");
        try {
            writeline("/home/ubuntu/results/coverage/Project/Project_6_10.coverage", "5fbb41ac-5265-41e2-80c6-14fa8452555f");
            final ObjectOutputStream s = new ObjectOutputStream(new FileOutputStream(safeProjectName + ".ser"));
            writeline("/home/ubuntu/results/coverage/Project/Project_6_10.coverage", "9e945f3e-b021-4ca3-9317-640c34b50b15");
            try {
                writeline("/home/ubuntu/results/coverage/Project/Project_6_10.coverage", "e7d5cb35-53fe-4276-998f-0f29bb3eff0c");
                s.writeObject(this);
                writeline("/home/ubuntu/results/coverage/Project/Project_6_10.coverage", "4169252e-30d9-45f6-b79a-59d648d52525");
                s.flush();
                writeline("/home/ubuntu/results/coverage/Project/Project_6_10.coverage", "43be9907-3c46-44dd-9c3a-c4027d83ecf3");
                debug("Serializing project to [" + safeProjectName + ".ser]");
            } finally {
                writeline("/home/ubuntu/results/coverage/Project/Project_6_10.coverage", "2177689b-adec-4dac-bb60-b548030784a3");
                s.close();
            }
        } catch (Exception e) {
            writeline("/home/ubuntu/results/coverage/Project/Project_6_10.coverage", "3905516c-b4b2-4ee6-aee2-5d38b0a53180");
            LOG.warn("Error serializing project to [" + safeProjectName + ".ser]: " + e.getMessage(), e);
        }
    }

    public void setLabelIncrementer(final LabelIncrementer incrementer) throws CruiseControlException {
        writeline("/home/ubuntu/results/coverage/Project/Project_6_10.coverage", "e265406c-c236-4f8f-8fcf-fe7ae0ca3428");
        if (incrementer == null) {
            writeline("/home/ubuntu/results/coverage/Project/Project_6_10.coverage", "2d042a81-9f93-4379-a6d9-418a9dfddc26");
            throw new IllegalArgumentException("label incrementer can't be null");
        }
        writeline("/home/ubuntu/results/coverage/Project/Project_6_10.coverage", "3c02879c-2814-4c81-8102-d044262df90a");
        labelIncrementer = incrementer;
        writeline("/home/ubuntu/results/coverage/Project/Project_6_10.coverage", "38041c09-526a-44b9-b3f0-eb569fbaaa9a");
        if (label == null) {
            writeline("/home/ubuntu/results/coverage/Project/Project_6_10.coverage", "b241eeef-e9c9-47ad-9f7a-f0414a461227");
            label = labelIncrementer.getDefaultLabel();
        }
        writeline("/home/ubuntu/results/coverage/Project/Project_6_10.coverage", "ffda3d2d-1b87-418e-9b10-d35bb6f515ba");
        validateLabel(label, labelIncrementer);
    }

    public LabelIncrementer getLabelIncrementer() {
        writeline("/home/ubuntu/results/coverage/Project/Project_6_10.coverage", "d22cef2f-f141-46f4-8ba6-952cd348fc90");
        return labelIncrementer;
    }

    public void setName(final String projectName) {
        writeline("/home/ubuntu/results/coverage/Project/Project_6_10.coverage", "775513bc-629c-4940-aed4-cd7649887fd1");
        name = projectName;
    }

    public String getName() {
        writeline("/home/ubuntu/results/coverage/Project/Project_6_10.coverage", "cc1e7d15-44e8-4841-a8b0-b523c8b1b5de");
        return name;
    }

    public void setLabel(final String newLabel) {
        writeline("/home/ubuntu/results/coverage/Project/Project_6_10.coverage", "63b6f215-6bc7-4574-9878-5f313e4b411a");
        label = newLabel;
    }

    public String getLabel() {
        writeline("/home/ubuntu/results/coverage/Project/Project_6_10.coverage", "d2aa2b5d-5bc7-44e9-b47b-64906ade959a");
        return label;
    }

    /**
     * @param newLastBuild string containing the build date in the format
     * yyyyMMddHHmmss
     * @throws CruiseControlException if the date cannot be extracted from the
     * input string
     */
    public void setLastBuild(final String newLastBuild) throws CruiseControlException {
        writeline("/home/ubuntu/results/coverage/Project/Project_6_10.coverage", "396be80d-5f27-4e28-a57e-2af853a21c8b");
        lastBuild = DateUtil.parseFormattedTime(newLastBuild, "lastBuild");
    }

    /**
     * @param newLastSuccessfulBuild string containing the build date in the format
     * yyyyMMddHHmmss
     * @throws CruiseControlException if the date cannot be extracted from the
     * input string
     */
    public void setLastSuccessfulBuild(final String newLastSuccessfulBuild) throws CruiseControlException {
        writeline("/home/ubuntu/results/coverage/Project/Project_6_10.coverage", "c2ae4d4e-6766-4c0e-9ee2-e46089a228c2");
        lastSuccessfulBuild = DateUtil.parseFormattedTime(newLastSuccessfulBuild, "lastSuccessfulBuild");
    }

    public String getLastBuild() {
        writeline("/home/ubuntu/results/coverage/Project/Project_6_10.coverage", "54fddc88-97d5-4c4a-a9f8-f18bb43a73fd");
        if (lastBuild == null) {
            writeline("/home/ubuntu/results/coverage/Project/Project_6_10.coverage", "4459a2c7-97ba-471c-822f-bfd3875ed92a");
            return null;
        }
        writeline("/home/ubuntu/results/coverage/Project/Project_6_10.coverage", "eb5e1993-8e15-4375-823e-de8b7fb70804");
        return DateUtil.getFormattedTime(lastBuild);
    }

    public boolean getBuildForced() {
        writeline("/home/ubuntu/results/coverage/Project/Project_6_10.coverage", "27ee9d6c-042f-4be5-9bf1-530876e45505");
        return buildForced;
    }

    public void setBuildForced(boolean forceNewBuildNow) {
        writeline("/home/ubuntu/results/coverage/Project/Project_6_10.coverage", "b3867c54-734f-4ade-8926-d30e165b3ae3");
        buildForced = forceNewBuildNow;
        writeline("/home/ubuntu/results/coverage/Project/Project_6_10.coverage", "b7735b74-8f9a-4c79-a24c-818053b36b2c");
        if (forceNewBuildNow) {
            writeline("/home/ubuntu/results/coverage/Project/Project_6_10.coverage", "8bb3573b-446d-4130-84ba-2b0c756ce1c8");
            forceBuild();
        }
    }

    public String getLastSuccessfulBuild() {
        writeline("/home/ubuntu/results/coverage/Project/Project_6_10.coverage", "d4421a76-2c1e-4c0d-9c63-96c8c213770e");
        if (lastSuccessfulBuild == null) {
            writeline("/home/ubuntu/results/coverage/Project/Project_6_10.coverage", "a3940081-62f8-4c1a-a623-51ae32dfa7ed");
            return null;
        }
        writeline("/home/ubuntu/results/coverage/Project/Project_6_10.coverage", "5465bf50-c620-4e86-8f0a-a619a2bea8d7");
        return DateUtil.getFormattedTime(lastSuccessfulBuild);
    }

    public String getLogDir() {
        writeline("/home/ubuntu/results/coverage/Project/Project_6_10.coverage", "2f7328a5-c5f5-4c75-bf71-db8de1180e42");
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
        writeline("/home/ubuntu/results/coverage/Project/Project_6_10.coverage", "f92969c1-b4b4-45f9-9169-8686fb50bb68");
        if (overrideBuildInterval == null) {
            writeline("/home/ubuntu/results/coverage/Project/Project_6_10.coverage", "acce645e-0c4a-4a9b-860e-24363954433c");
            return projectConfig.getSchedule().getInterval();
        } else {
            writeline("/home/ubuntu/results/coverage/Project/Project_6_10.coverage", "e664b04f-0d8e-41aa-b614-7a4d9d2d7bef");
            return overrideBuildInterval;
        }
    }

    /**
     * Sets the build interval that this Project should use. This method
     * overrides the value initially specified in the Schedule attribute.
     * @param sleepMillis the number of milliseconds to sleep between build attempts
     */
    public void overrideBuildInterval(final long sleepMillis) {
        writeline("/home/ubuntu/results/coverage/Project/Project_6_10.coverage", "08ab9f8a-88ac-4155-a0bd-514bfc425c53");
        overrideBuildInterval = sleepMillis;
    }

    public boolean isPaused() {
        writeline("/home/ubuntu/results/coverage/Project/Project_6_10.coverage", "bc61cdde-19c4-4d08-8006-e5254d5cdc89");
        return isPaused;
    }

    public void setPaused(final boolean paused) {
        writeline("/home/ubuntu/results/coverage/Project/Project_6_10.coverage", "84d5b89b-c65f-45d7-8d52-b65643780d4a");
        synchronized (pausedMutex) {
            writeline("/home/ubuntu/results/coverage/Project/Project_6_10.coverage", "b107bd8c-6521-42d7-9bbd-3b54fa9ebbca");
            if (isPaused && !paused) {
                writeline("/home/ubuntu/results/coverage/Project/Project_6_10.coverage", "46d5e75a-8da6-481d-8dfe-0ac939daa2cb");
                pausedMutex.notifyAll();
            }
            writeline("/home/ubuntu/results/coverage/Project/Project_6_10.coverage", "a4991bfa-9caf-4978-8359-74dc6ee36964");
            isPaused = paused;
        }
    }

    public void setBuildAfterFailed(final boolean rebuildEvenWithNoNewModifications) {
        writeline("/home/ubuntu/results/coverage/Project/Project_6_10.coverage", "37f51dad-1bde-4970-8520-c836098715e8");
        buildAfterFailed = rebuildEvenWithNoNewModifications;
    }

    public String getStatus() {
        writeline("/home/ubuntu/results/coverage/Project/Project_6_10.coverage", "1175d372-b1b3-4daf-b14f-3f018b75ba1b");
        return getState().getDescription();
    }

    public String getStatusWithQueuePosition() {
        writeline("/home/ubuntu/results/coverage/Project/Project_6_10.coverage", "262ed1d9-0267-4124-a643-911a8bf976ef");
        if (ProjectState.QUEUED.equals(getState())) {
            writeline("/home/ubuntu/results/coverage/Project/Project_6_10.coverage", "3ed2cfaa-7dfb-4dde-9136-9fc8cf500a8f");
            return getState().getDescription() + " - " + queue.findPosition(projectConfig);
        } else {
            writeline("/home/ubuntu/results/coverage/Project/Project_6_10.coverage", "44b454f0-3fb0-4c7f-bc06-a78ab12ba025");
            return getState().getDescription();
        }
    }

    public ProjectState getState() {
        writeline("/home/ubuntu/results/coverage/Project/Project_6_10.coverage", "ee32869b-5f28-4134-95ee-0e3ff914b9af");
        return state;
    }

    private void setState(final ProjectState newState) {
        writeline("/home/ubuntu/results/coverage/Project/Project_6_10.coverage", "e9e80fdf-9c40-4d65-be77-aed059a62717");
        state = newState;
        writeline("/home/ubuntu/results/coverage/Project/Project_6_10.coverage", "8c0d96fb-c179-4c02-a003-93c220b726c5");
        info(getStatus());
        writeline("/home/ubuntu/results/coverage/Project/Project_6_10.coverage", "7f4addfa-2b78-4b7e-8495-826129870fdc");
        notifyListeners(new ProjectStateChangedEvent(name, getState()));
        writeline("/home/ubuntu/results/coverage/Project/Project_6_10.coverage", "58bbef84-6584-4f8e-b7fe-41f13df06179");
        fireProgressEvent(new BuildProgressEvent(this, getState()));
    }

    public void setBuildQueue(final BuildQueue buildQueue) {
        writeline("/home/ubuntu/results/coverage/Project/Project_6_10.coverage", "7ba49e49-0f3e-47ad-ab59-5ef4f63cd2f3");
        queue = buildQueue;
    }

    public String getBuildStartTime() {
        writeline("/home/ubuntu/results/coverage/Project/Project_6_10.coverage", "c67d594f-e432-45e6-8917-83914436eda9");
        return DateUtil.getFormattedTime(buildStartTime);
    }

    public Log getLog() {
        writeline("/home/ubuntu/results/coverage/Project/Project_6_10.coverage", "9c74f3bd-11c0-41fd-beab-b87ee084362e");
        return this.projectConfig.getLog();
    }

    /**
     * Initialize the project. Uses ProjectXMLHelper to parse a project file.
     */
    protected void init() {
        writeline("/home/ubuntu/results/coverage/Project/Project_6_10.coverage", "31bbfdb9-0c45-4766-8fce-d7969af8de8f");
        if (projectConfig == null) {
            writeline("/home/ubuntu/results/coverage/Project/Project_6_10.coverage", "6b727085-0657-4935-b2d1-4e72c07afc86");
            throw new IllegalStateException("projectConfig must be set on project before calling init()");
        }
        writeline("/home/ubuntu/results/coverage/Project/Project_6_10.coverage", "b8710491-6fbd-455c-8ede-6430e61d2d50");
        buildAfterFailed = projectConfig.shouldBuildAfterFailed();
        writeline("/home/ubuntu/results/coverage/Project/Project_6_10.coverage", "572944f6-01bd-4e8b-8771-cc5f016251d4");
        forceOnly = projectConfig.isForceOnly();
        writeline("/home/ubuntu/results/coverage/Project/Project_6_10.coverage", "e3d847e2-cc70-47dc-8dcb-8761f4e7bd60");
        requiremodification = projectConfig.isRequiremodification();
        writeline("/home/ubuntu/results/coverage/Project/Project_6_10.coverage", "7b9c8a8c-0ce5-4770-a334-b9c0be46146d");
        if (lastBuild == null) {
            writeline("/home/ubuntu/results/coverage/Project/Project_6_10.coverage", "2a390e94-a942-4eae-9821-d8b689f2abd6");
            lastBuild = DateUtil.getMidnight();
        }
        writeline("/home/ubuntu/results/coverage/Project/Project_6_10.coverage", "dbbafc68-1fca-4c0a-bef9-06d1d3b15e17");
        if (lastSuccessfulBuild == null) {
            writeline("/home/ubuntu/results/coverage/Project/Project_6_10.coverage", "110ee20d-9161-412b-8ea4-90aa5b8c586f");
            lastSuccessfulBuild = lastBuild;
        }
        writeline("/home/ubuntu/results/coverage/Project/Project_6_10.coverage", "d25d08f7-da1e-458f-a723-7c0d491053b8");
        if (LOG.isDebugEnabled()) {
            writeline("/home/ubuntu/results/coverage/Project/Project_6_10.coverage", "b9d9193d-2ca9-4160-95f3-0384e0c92c49");
            debug("buildInterval          = [" + getBuildInterval() + "]");
            writeline("/home/ubuntu/results/coverage/Project/Project_6_10.coverage", "75910e63-74b6-41ed-b90c-1d6cc8a301f5");
            debug("buildForced            = [" + buildForced + "]");
            writeline("/home/ubuntu/results/coverage/Project/Project_6_10.coverage", "691a8aaa-9878-4fe4-ac94-68c21d8b8623");
            debug("buildAfterFailed       = [" + buildAfterFailed + "]");
            writeline("/home/ubuntu/results/coverage/Project/Project_6_10.coverage", "d02b6391-45af-4b4a-ad2d-3112e394b896");
            debug("requireModifcation     = [" + requiremodification + "]");
            writeline("/home/ubuntu/results/coverage/Project/Project_6_10.coverage", "630ef3f5-eaa6-4b15-a1e4-227027eb41b2");
            debug("forceOnly              = [" + forceOnly + "]");
            writeline("/home/ubuntu/results/coverage/Project/Project_6_10.coverage", "a3b8429f-0e51-44db-83f4-d377df12eef2");
            debug("buildCounter           = [" + buildCounter + "]");
            writeline("/home/ubuntu/results/coverage/Project/Project_6_10.coverage", "94470dfc-a11b-45bf-8e6e-4dd07cae47ac");
            debug("isPaused               = [" + isPaused + "]");
            writeline("/home/ubuntu/results/coverage/Project/Project_6_10.coverage", "aa5ab8f2-2c1e-4838-8b2d-8a3a599a34a7");
            debug("label                  = [" + label + "]");
            writeline("/home/ubuntu/results/coverage/Project/Project_6_10.coverage", "255cf658-0d9b-47d2-9283-cf7a38303487");
            debug("lastBuild              = [" + DateUtil.getFormattedTime(lastBuild) + "]");
            writeline("/home/ubuntu/results/coverage/Project/Project_6_10.coverage", "21ddbfac-7825-4bad-a46f-e6ed66af46f8");
            debug("lastSuccessfulBuild    = [" + DateUtil.getFormattedTime(lastSuccessfulBuild) + "]");
            writeline("/home/ubuntu/results/coverage/Project/Project_6_10.coverage", "9cc39a8c-e825-48e0-b2d8-34ff130b91bb");
            debug("logDir                 = [" + projectConfig.getLog().getLogDir() + "]");
            writeline("/home/ubuntu/results/coverage/Project/Project_6_10.coverage", "c1abdf8a-8377-4a62-8a6d-58d0bc405933");
            debug("logXmlEncoding         = [" + projectConfig.getLog().getLogXmlEncoding() + "]");
            writeline("/home/ubuntu/results/coverage/Project/Project_6_10.coverage", "bb0d4c53-0af8-4f9c-af5a-57a125fae20e");
            debug("wasLastBuildSuccessful = [" + wasLastBuildSuccessful + "]");
        }
    }

    protected Element getProjectPropertiesElement(final Date now) {
        writeline("/home/ubuntu/results/coverage/Project/Project_6_10.coverage", "02cce5d8-ed79-4792-8d53-27c3d96b3c2c");
        final Element infoElement = new Element("info");
        writeline("/home/ubuntu/results/coverage/Project/Project_6_10.coverage", "5977c4fe-1f80-4ed4-825b-65ff7204cecf");
        addProperty(infoElement, "projectname", name);
        writeline("/home/ubuntu/results/coverage/Project/Project_6_10.coverage", "dd5635d1-e776-4fc4-899c-d4d2e7146141");
        final String lastBuildString = DateUtil.getFormattedTime(lastBuild == null ? now : lastBuild);
        writeline("/home/ubuntu/results/coverage/Project/Project_6_10.coverage", "fa663f3f-fdf3-4942-ac5b-7bc22eb11c53");
        addProperty(infoElement, "lastbuild", lastBuildString);
        writeline("/home/ubuntu/results/coverage/Project/Project_6_10.coverage", "f23b91a6-306b-4787-9b33-4db6c51383f4");
        final String lastSuccessfulBuildString = DateUtil.getFormattedTime(lastSuccessfulBuild == null ? now : lastSuccessfulBuild);
        writeline("/home/ubuntu/results/coverage/Project/Project_6_10.coverage", "21ac0428-b58a-4ee9-9f4c-4197e5d6efd2");
        addProperty(infoElement, "lastsuccessfulbuild", lastSuccessfulBuildString);
        writeline("/home/ubuntu/results/coverage/Project/Project_6_10.coverage", "9084dd2b-fb94-4e09-b9c4-2fdc8609659a");
        addProperty(infoElement, "builddate", DateUtil.formatIso8601(now));
        writeline("/home/ubuntu/results/coverage/Project/Project_6_10.coverage", "3634b884-691e-48f4-b059-c1e111392879");
        addProperty(infoElement, "cctimestamp", DateUtil.getFormattedTime(now));
        writeline("/home/ubuntu/results/coverage/Project/Project_6_10.coverage", "9f3bafa9-7fae-41c0-8db4-ba6ef36a8671");
        addProperty(infoElement, "label", label);
        writeline("/home/ubuntu/results/coverage/Project/Project_6_10.coverage", "2b8001dd-b7b7-4251-8378-8a009f7f2964");
        addProperty(infoElement, "interval", Long.toString(getBuildInterval() / 1000L));
        writeline("/home/ubuntu/results/coverage/Project/Project_6_10.coverage", "ed53ef37-c2c9-4556-a3f2-533a02f0c061");
        addProperty(infoElement, "lastbuildsuccessful", String.valueOf(wasLastBuildSuccessful));
        writeline("/home/ubuntu/results/coverage/Project/Project_6_10.coverage", "c78efdbc-dcb4-4ec0-aaa9-37048a5986c5");
        return infoElement;
    }

    private void addProperty(final Element parent, final String key, final String value) {
        writeline("/home/ubuntu/results/coverage/Project/Project_6_10.coverage", "dc771564-31ac-4536-a8de-79cb98f9c251");
        final Element propertyElement = new Element("property");
        writeline("/home/ubuntu/results/coverage/Project/Project_6_10.coverage", "8219faa8-2791-4d61-a3a7-a196931553d6");
        propertyElement.setAttribute("name", key);
        writeline("/home/ubuntu/results/coverage/Project/Project_6_10.coverage", "ea2eb427-d01b-4798-bc19-9b8a57052bca");
        propertyElement.setAttribute("value", value);
        writeline("/home/ubuntu/results/coverage/Project/Project_6_10.coverage", "bcdab838-12bb-46ed-9421-460ccd03cb27");
        parent.addContent(propertyElement);
    }

    protected Map<String, String> getProjectPropertiesMap(final Date now) {
        writeline("/home/ubuntu/results/coverage/Project/Project_6_10.coverage", "4784c1ad-b862-4811-8f00-e272fe727300");
        final Map<String, String> buildProperties = new HashMap<String, String>();
        writeline("/home/ubuntu/results/coverage/Project/Project_6_10.coverage", "57b5bf2c-f806-4ca3-8cc2-b9e1ae026256");
        buildProperties.put("projectname", name);
        writeline("/home/ubuntu/results/coverage/Project/Project_6_10.coverage", "416fa9a3-d5a0-4d46-beb8-82feb7fd5cd0");
        buildProperties.put("label", label);
        writeline("/home/ubuntu/results/coverage/Project/Project_6_10.coverage", "4790ac11-010d-421b-932c-dbc4f92a865f");
        // TODO: Shouldn't have CVS specific properties here
        buildProperties.put("cvstimestamp", CVSDateUtil.formatCVSDate(now));
        writeline("/home/ubuntu/results/coverage/Project/Project_6_10.coverage", "2a5eb2f6-43e1-429d-8f5b-ff361d57e840");
        buildProperties.put("cctimestamp", DateUtil.getFormattedTime(now));
        writeline("/home/ubuntu/results/coverage/Project/Project_6_10.coverage", "f52a196c-04df-4849-8032-369b83fbda1a");
        buildProperties.put("cclastgoodbuildtimestamp", getLastSuccessfulBuild());
        writeline("/home/ubuntu/results/coverage/Project/Project_6_10.coverage", "2c9eb000-098a-43a7-98b2-dac62c1b3ea3");
        buildProperties.put("cclastbuildtimestamp", getLastBuild());
        writeline("/home/ubuntu/results/coverage/Project/Project_6_10.coverage", "ef8f75e0-cc60-445c-b59f-3a40ce75554d");
        buildProperties.put("lastbuildsuccessful", String.valueOf(isLastBuildSuccessful()));
        writeline("/home/ubuntu/results/coverage/Project/Project_6_10.coverage", "0984d8cc-7598-4c5a-88af-22ee5b10ed09");
        buildProperties.put("buildforced", String.valueOf(getBuildForced()));
        writeline("/home/ubuntu/results/coverage/Project/Project_6_10.coverage", "d66efbfd-54c3-45cf-a294-50bbcc11a595");
        if (projectConfig.getModificationSet() != null) {
            writeline("/home/ubuntu/results/coverage/Project/Project_6_10.coverage", "a5cb5c68-baa1-4e80-870d-c7cecdeba149");
            buildProperties.putAll(projectConfig.getModificationSet().getProperties());
        }
        writeline("/home/ubuntu/results/coverage/Project/Project_6_10.coverage", "2e563a0d-31d4-4966-913f-f869fb7d0bfb");
        if (additionalProperties != null && !additionalProperties.isEmpty()) {
            writeline("/home/ubuntu/results/coverage/Project/Project_6_10.coverage", "fc14ebd2-9809-438f-8593-c2e11e0dc592");
            buildProperties.putAll(additionalProperties);
            writeline("/home/ubuntu/results/coverage/Project/Project_6_10.coverage", "aa850269-c814-4760-8037-8727b4a923e6");
            additionalProperties.clear();
            writeline("/home/ubuntu/results/coverage/Project/Project_6_10.coverage", "b87ba444-e6ce-4569-94fb-b39980c69fd0");
            additionalProperties = null;
        }
        writeline("/home/ubuntu/results/coverage/Project/Project_6_10.coverage", "f65ca074-533e-4faf-9da4-249da2784acb");
        return buildProperties;
    }

    /**
     * Intended only for unit testing.
     * @return additional Properties variable.
     */
    Map<String, String> getAdditionalProperties() {
        writeline("/home/ubuntu/results/coverage/Project/Project_6_10.coverage", "91068656-661e-4bc5-b903-7ee50be77bf8");
        return additionalProperties;
    }

    /**
     * Iterate over all of the registered <code>Publisher</code>s and call
     * their respective <code>publish</code> methods.
     * @param buildLog the content to publish
     * @throws CruiseControlException if an error occurs during publishing
     */
    protected void publish(final Log buildLog) throws CruiseControlException {
        writeline("/home/ubuntu/results/coverage/Project/Project_6_10.coverage", "0325fb3e-dfec-4efa-b953-8d735170d8c0");
        setState(ProjectState.PUBLISHING);
        writeline("/home/ubuntu/results/coverage/Project/Project_6_10.coverage", "deedda4f-c04a-4fbe-8170-8366b9d40761");
        for (final Publisher publisher : projectConfig.getPublishers()) {
            writeline("/home/ubuntu/results/coverage/Project/Project_6_10.coverage", "a8beb42d-a1e0-43a4-8c46-36865214ff46");
            // catch all errors, Publishers shouldn't cause failures in the build method
            try {
                writeline("/home/ubuntu/results/coverage/Project/Project_6_10.coverage", "c223b8c8-bcbd-43dc-b5b2-527629528768");
                publisher.publish(buildLog.getContent());
            } catch (Throwable t) {
                writeline("/home/ubuntu/results/coverage/Project/Project_6_10.coverage", "b472ad28-0f62-4401-a275-e20811f6c5c6");
                final StringBuilder message = new StringBuilder("exception publishing results");
                writeline("/home/ubuntu/results/coverage/Project/Project_6_10.coverage", "ea21d577-3001-45b7-8c7c-456e90fb7693");
                message.append(" with ").append(publisher.getClass().getName());
                writeline("/home/ubuntu/results/coverage/Project/Project_6_10.coverage", "3bcde0c0-b9f1-4ae1-a6ff-00dbb28a09a8");
                message.append(" for project ").append(name);
                writeline("/home/ubuntu/results/coverage/Project/Project_6_10.coverage", "7c7a957e-de51-4d55-95ac-5ef8b4cd5d6a");
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
        writeline("/home/ubuntu/results/coverage/Project/Project_6_10.coverage", "12612476-6ff1-4483-b6d6-bb102494db60");
        setState(ProjectState.BOOTSTRAPPING);
        writeline("/home/ubuntu/results/coverage/Project/Project_6_10.coverage", "84557bfb-c083-4830-861f-a2cef917097e");
        for (final Bootstrapper bootstrapper : projectConfig.getBootstrappers()) {
            writeline("/home/ubuntu/results/coverage/Project/Project_6_10.coverage", "cf18a614-c86c-4313-a527-531e1d1d141b");
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
        writeline("/home/ubuntu/results/coverage/Project/Project_6_10.coverage", "c53ae9aa-5629-4a8d-94bf-9f9adf767471");
        if (!incrementer.isValidLabel(oldLabel)) {
            writeline("/home/ubuntu/results/coverage/Project/Project_6_10.coverage", "e0087f9c-303d-4452-94d7-efc6e02b2bdf");
            final String message = oldLabel + " is not a valid label for labelIncrementer " + incrementer.getClass().getName();
            writeline("/home/ubuntu/results/coverage/Project/Project_6_10.coverage", "541dee6d-c333-4ad0-ab43-3c0a8c0d15ae");
            debug(message);
            writeline("/home/ubuntu/results/coverage/Project/Project_6_10.coverage", "03437361-d501-47f9-9a9f-9f064e345e71");
            throw new CruiseControlException(message);
        }
    }

    public boolean isLastBuildSuccessful() {
        writeline("/home/ubuntu/results/coverage/Project/Project_6_10.coverage", "7c9cc6a8-7269-4aab-87f1-f491845f0fe9");
        return wasLastBuildSuccessful;
    }

    void setWasLastBuildSuccessful(final boolean buildSuccessful) {
        writeline("/home/ubuntu/results/coverage/Project/Project_6_10.coverage", "121a69c5-85ba-4ff2-9967-66c0c20948cf");
        wasLastBuildSuccessful = buildSuccessful;
    }

    /**
     * Logs a message to the application log, not to be confused with the
     * CruiseControl build log.
     * @param message the application message to log
     */
    private void info(final String message) {
        writeline("/home/ubuntu/results/coverage/Project/Project_6_10.coverage", "7b9631f3-9295-4954-ad67-7aabce7f4a4d");
        LOG.info("Project " + name + ":  " + message);
    }

    private void debug(final String message) {
        writeline("/home/ubuntu/results/coverage/Project/Project_6_10.coverage", "16de1dbb-0d01-44cf-a5fe-dde8c4752c82");
        LOG.debug("Project " + name + ":  " + message);
    }

    public void start() {
        writeline("/home/ubuntu/results/coverage/Project/Project_6_10.coverage", "24eefbcb-e1e1-4635-8eda-1fe7ec0872c6");
        if (stopped || getState() == ProjectState.STOPPED) {
            writeline("/home/ubuntu/results/coverage/Project/Project_6_10.coverage", "9946880b-fa38-42b3-8daf-4d0a2e94e224");
            stopped = false;
            writeline("/home/ubuntu/results/coverage/Project/Project_6_10.coverage", "8dd5928c-53aa-45ae-9a9b-185f323588f4");
            LOG.info("Project " + name + " starting");
            writeline("/home/ubuntu/results/coverage/Project/Project_6_10.coverage", "e3042776-aebd-4c17-bb74-527f83d53591");
            setState(ProjectState.IDLE);
            writeline("/home/ubuntu/results/coverage/Project/Project_6_10.coverage", "9960b674-e6b6-405b-aaf5-fbac90ca6358");
            createNewSchedulingThread();
        }
    }

    protected void createNewSchedulingThread() {
        writeline("/home/ubuntu/results/coverage/Project/Project_6_10.coverage", "72d20311-1c55-49c4-8d05-5d156db3a34e");
        final Thread projectSchedulingThread = new Thread(this, "Project " + getName() + " thread");
        writeline("/home/ubuntu/results/coverage/Project/Project_6_10.coverage", "f3ec8418-9265-41c4-b207-740f4e185262");
        projectSchedulingThread.start();
        writeline("/home/ubuntu/results/coverage/Project/Project_6_10.coverage", "564d53e5-9a4b-401b-a2e8-9a2b8e77eed7");
        // brief nap to allow thread to start
        try {
            writeline("/home/ubuntu/results/coverage/Project/Project_6_10.coverage", "eddabce8-133e-4bcb-87e4-de18aa9ac249");
            Thread.sleep(100);
        } catch (InterruptedException ie) {
            writeline("/home/ubuntu/results/coverage/Project/Project_6_10.coverage", "54319bc1-d0b9-4094-bc89-500f8e18c123");
            LOG.warn("interrupted while waiting for scheduling thread to start", ie);
        }
    }

    public void stop() {
        writeline("/home/ubuntu/results/coverage/Project/Project_6_10.coverage", "41bf8de6-d4cd-4aa2-8640-b40b535109db");
        LOG.info("Project " + name + " stopping");
        writeline("/home/ubuntu/results/coverage/Project/Project_6_10.coverage", "d6d3f22f-5dab-45db-8e92-c62bebf5ba93");
        stopped = true;
        writeline("/home/ubuntu/results/coverage/Project/Project_6_10.coverage", "de4b7c94-a839-414d-b2ff-a893d39e143e");
        setState(ProjectState.STOPPED);
        writeline("/home/ubuntu/results/coverage/Project/Project_6_10.coverage", "c81ae264-717c-4394-a91f-936799f09efb");
        synchronized (pausedMutex) {
            writeline("/home/ubuntu/results/coverage/Project/Project_6_10.coverage", "62e6fded-60e4-4e52-a4b3-52e223ac41d7");
            pausedMutex.notifyAll();
        }
        writeline("/home/ubuntu/results/coverage/Project/Project_6_10.coverage", "e6c7acfc-0bed-462e-a3c8-1c3ccaf98f4b");
        synchronized (waitMutex) {
            writeline("/home/ubuntu/results/coverage/Project/Project_6_10.coverage", "4796ad73-07e2-4f6b-b32d-1d1864d0b54f");
            waitMutex.notifyAll();
        }
        writeline("/home/ubuntu/results/coverage/Project/Project_6_10.coverage", "97520716-33f1-4eab-ad53-b175e40ce86d");
        synchronized (scheduleMutex) {
            writeline("/home/ubuntu/results/coverage/Project/Project_6_10.coverage", "e907da4f-7e37-44cf-9c6b-cb9e101b4148");
            scheduleMutex.notifyAll();
        }
    }

    public String toString() {
        writeline("/home/ubuntu/results/coverage/Project/Project_6_10.coverage", "a5de4a0b-32df-4ea8-8435-fc8a0dd6260d");
        final StringBuilder sb = new StringBuilder("Project ");
        writeline("/home/ubuntu/results/coverage/Project/Project_6_10.coverage", "f90fd079-a966-436e-bcbf-b584f5babe0c");
        sb.append(getName());
        writeline("/home/ubuntu/results/coverage/Project/Project_6_10.coverage", "5218eee2-4313-4d59-97aa-a623468b3210");
        sb.append(": ");
        writeline("/home/ubuntu/results/coverage/Project/Project_6_10.coverage", "859cc2f9-f1a9-49cb-9a20-947c52044031");
        sb.append(getStatus());
        writeline("/home/ubuntu/results/coverage/Project/Project_6_10.coverage", "19aec2d6-6cc3-40de-8dff-de7e5dc68af7");
        if (isPaused) {
            writeline("/home/ubuntu/results/coverage/Project/Project_6_10.coverage", "0641b729-b9cf-4ed9-b956-f4374297cc4c");
            sb.append(" (paused)");
        }
        writeline("/home/ubuntu/results/coverage/Project/Project_6_10.coverage", "4cc68222-dc1e-4454-a37f-b4c065393c50");
        return sb.toString();
    }

    public void addBuildProgressListener(final BuildProgressListener listener) {
        writeline("/home/ubuntu/results/coverage/Project/Project_6_10.coverage", "4bec7508-9b26-4008-ace4-224ac5127caa");
        synchronized (progressListeners) {
            writeline("/home/ubuntu/results/coverage/Project/Project_6_10.coverage", "f0d8dac9-490f-4c6f-9b94-64775563ebfd");
            progressListeners.add(listener);
        }
    }

    protected void fireProgressEvent(final BuildProgressEvent event) {
        writeline("/home/ubuntu/results/coverage/Project/Project_6_10.coverage", "ac612c12-7ea5-4055-b788-49eb314377d5");
        synchronized (progressListeners) {
            writeline("/home/ubuntu/results/coverage/Project/Project_6_10.coverage", "a42d1f5f-ac6c-424f-b7f4-97f440b0c759");
            for (final BuildProgressListener listener : progressListeners) {
                writeline("/home/ubuntu/results/coverage/Project/Project_6_10.coverage", "b4a5c14a-b56e-4dc7-a950-b42c717b3bed");
                listener.handleBuildProgress(event);
            }
        }
    }

    public void addBuildResultListener(final BuildResultListener listener) {
        writeline("/home/ubuntu/results/coverage/Project/Project_6_10.coverage", "4d3085cf-4616-41e3-b386-d57da30a6849");
        synchronized (resultListeners) {
            writeline("/home/ubuntu/results/coverage/Project/Project_6_10.coverage", "88a28e99-36c1-4413-bd1e-9327bfc4ebf4");
            resultListeners.add(listener);
        }
    }

    protected void fireResultEvent(final BuildResultEvent event) {
        writeline("/home/ubuntu/results/coverage/Project/Project_6_10.coverage", "07ad1724-cec0-45f0-b707-38a9f02bb38e");
        synchronized (resultListeners) {
            writeline("/home/ubuntu/results/coverage/Project/Project_6_10.coverage", "b19ce30e-e1c6-42f6-9078-f69b5b9f8047");
            for (final BuildResultListener listener : resultListeners) {
                writeline("/home/ubuntu/results/coverage/Project/Project_6_10.coverage", "7c6a5204-1c1a-4df2-af79-c51d842eb6eb");
                listener.handleBuildResult(event);
            }
        }
    }

    List<Listener> getListeners() {
        writeline("/home/ubuntu/results/coverage/Project/Project_6_10.coverage", "cd54f2f4-7927-429e-a564-825e77a61a25");
        return projectConfig.getListeners();
    }

    public void setProjectConfig(final ProjectConfig projectConfig) throws CruiseControlException {
        writeline("/home/ubuntu/results/coverage/Project/Project_6_10.coverage", "ace42865-c3e8-4003-86e0-ffc590acb80c");
        if (projectConfig == null) {
            writeline("/home/ubuntu/results/coverage/Project/Project_6_10.coverage", "1e5743e0-cd2e-46c1-8ee9-cf444013bb94");
            throw new IllegalArgumentException("project config can't be null");
        }
        writeline("/home/ubuntu/results/coverage/Project/Project_6_10.coverage", "70c0d8bf-ccc1-4003-aa93-897362614aed");
        this.projectConfig = projectConfig;
        writeline("/home/ubuntu/results/coverage/Project/Project_6_10.coverage", "c41b626a-385e-4380-8033-dfb0b8235248");
        setLabelIncrementer(projectConfig.getLabelIncrementer());
    }

    void notifyListeners(final ProjectEvent event) {
        writeline("/home/ubuntu/results/coverage/Project/Project_6_10.coverage", "431d547c-d2c2-4d86-b580-542444144a1a");
        if (projectConfig == null) {
            writeline("/home/ubuntu/results/coverage/Project/Project_6_10.coverage", "882a47b0-42aa-4832-8656-7b226028c0b0");
            throw new IllegalStateException("projectConfig is null");
        }
        writeline("/home/ubuntu/results/coverage/Project/Project_6_10.coverage", "bbd1aa3b-5c9a-4282-9b29-6731b8484ca4");
        for (final Listener listener : projectConfig.getListeners()) {
            writeline("/home/ubuntu/results/coverage/Project/Project_6_10.coverage", "444204f8-0961-4f48-a894-1a48131319d9");
            try {
                writeline("/home/ubuntu/results/coverage/Project/Project_6_10.coverage", "3a668b54-9663-4465-a0a9-cafab7d27693");
                listener.handleEvent(event);
            } catch (CruiseControlException e) {
                writeline("/home/ubuntu/results/coverage/Project/Project_6_10.coverage", "6c9a6513-c901-4c19-ba87-b789b76397ef");
                final StringBuilder message = new StringBuilder("exception notifying listener ");
                writeline("/home/ubuntu/results/coverage/Project/Project_6_10.coverage", "33d7c251-b880-45c1-aa74-34e0b8598cb7");
                message.append(listener.getClass().getName());
                writeline("/home/ubuntu/results/coverage/Project/Project_6_10.coverage", "38206327-6330-48a6-90b7-2f4799f7306a");
                message.append(" for project ").append(name);
                writeline("/home/ubuntu/results/coverage/Project/Project_6_10.coverage", "b8d202ed-c56a-4b19-b016-f2b15af0d9bd");
                LOG.error(message.toString(), e);
            }
        }
    }

    public boolean equals(final Object arg0) {
        writeline("/home/ubuntu/results/coverage/Project/Project_6_10.coverage", "7b242762-9ae3-45fb-901c-51b1ee4e285d");
        if (arg0 == null) {
            writeline("/home/ubuntu/results/coverage/Project/Project_6_10.coverage", "b5f36df9-a6eb-48a5-b36c-f45bd629f451");
            return false;
        }
        writeline("/home/ubuntu/results/coverage/Project/Project_6_10.coverage", "90989ff0-6655-4e95-b58c-1a40b8f1458b");
        if (arg0.getClass().getName().equals(getClass().getName())) {
            writeline("/home/ubuntu/results/coverage/Project/Project_6_10.coverage", "97dc0ae4-f41b-438f-a509-9fe0ac8fc45a");
            final Project thatProject = (Project) arg0;
            writeline("/home/ubuntu/results/coverage/Project/Project_6_10.coverage", "fd3e88eb-3832-4a7b-b918-d97bc25e1df6");
            return thatProject.name.equals(name);
        }
        writeline("/home/ubuntu/results/coverage/Project/Project_6_10.coverage", "72625c1e-b3a0-43ed-8515-439fbb17f180");
        return false;
    }

    public int hashCode() {
        writeline("/home/ubuntu/results/coverage/Project/Project_6_10.coverage", "581d2259-e8f8-4a18-a40b-ba7ff33f931b");
        return name.hashCode();
    }

    public void register(final MBeanServer server) throws JMException {
        writeline("/home/ubuntu/results/coverage/Project/Project_6_10.coverage", "9125c279-329e-47e7-b58b-f7b6f77d3bd8");
        LOG.debug("Registering project mbean");
        writeline("/home/ubuntu/results/coverage/Project/Project_6_10.coverage", "90d557f6-8d80-4e64-bf10-58989510d3a4");
        final ProjectController projectController = new ProjectController(this);
        writeline("/home/ubuntu/results/coverage/Project/Project_6_10.coverage", "a14bc7f4-d0e2-4e1b-9675-afcdf2db47fe");
        projectController.register(server);
    }

    public ProjectConfig getProjectConfig() {
        writeline("/home/ubuntu/results/coverage/Project/Project_6_10.coverage", "13c4c2ce-008d-48f1-88d1-12fe68edeea7");
        return projectConfig;
    }

    public Date getLastBuildDate() {
        writeline("/home/ubuntu/results/coverage/Project/Project_6_10.coverage", "8be42eae-e5fc-4da3-9bf3-05ff676344e9");
        return lastBuild;
    }

    public Progress getProgress() {
        writeline("/home/ubuntu/results/coverage/Project/Project_6_10.coverage", "7bfbe849-033e-4d51-b563-8ac9560fa3de");
        return progress;
    }

    public List<String> getLogLabels() {
        writeline("/home/ubuntu/results/coverage/Project/Project_6_10.coverage", "8614ca6f-37a6-4339-a5fc-0265aaef10ea");
        return projectConfig.getLogLabels();
    }

    public String[] getLogLabelLines(final String logLabel, final int firstLine) {
        writeline("/home/ubuntu/results/coverage/Project/Project_6_10.coverage", "5f23d59c-7ad7-4c6c-93b0-ab5c630a5de1");
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
