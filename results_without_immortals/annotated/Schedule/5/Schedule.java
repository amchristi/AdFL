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

import java.io.Serializable;
import java.text.DateFormat;
import java.text.DateFormatSymbols;
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Calendar;
import java.util.Collections;
import java.util.Comparator;
import java.util.Date;
import java.util.List;
import java.util.Locale;
import java.util.Map;
import net.sourceforge.cruisecontrol.util.DateUtil;
import net.sourceforge.cruisecontrol.util.ValidationHelper;
import org.apache.log4j.Logger;
import org.jdom.Element;
import java.io.*;

/**
 * Handles scheduling different builds.
 *
 * @author alden almagro, ThoughtWorks, Inc. 2001-2
 */
public class Schedule implements Serializable {

    private static final long serialVersionUID = -33682332427948426L;

    private static final Logger LOG = Logger.getLogger(Schedule.class);

    static final long ONE_SECOND = 1000;

    static final long ONE_MINUTE = 60 * ONE_SECOND;

    static final long ONE_DAY = 24 * 60 * ONE_MINUTE;

    static final long ONE_YEAR = ONE_DAY * 365;

    static final long MAX_INTERVAL_SECONDS = 60 * 60 * 24 * 365;

    static final long MAX_INTERVAL_MILLISECONDS = MAX_INTERVAL_SECONDS * 1000;

    private final List<Builder> builders = new ArrayList<Builder>();

    private final List<PauseBuilder> pauseBuilders = new ArrayList<PauseBuilder>();

    private long interval = 300 * ONE_SECOND;

    private boolean showProgress = true;

    /**
     * date formatting for time statements
     */
    private final DateFormat timeFormatter = new SimpleDateFormat("HH:mm");

    private final Comparator<Builder> builderComparator = new BuilderComparitor();

    public void add(final Builder builder) {
        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_5_10.coverage", "d997a40c-a0d3-4275-966a-c2d0aeba5b0c");
        checkParamNotNull("builder", builder);
        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_5_10.coverage", "4991d9cc-4661-4cc6-83ed-9a8be73baad8");
        builders.add(builder);
        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_5_10.coverage", "a706c09a-4051-48f5-b298-066eb3d8ca01");
        Collections.sort(builders, builderComparator);
    }

    public void add(final PauseBuilder pause) {
        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_5_10.coverage", "1efd2f53-6467-4369-887c-a9edf92e6f79");
        checkParamNotNull("pauseBuilder", pause);
        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_5_10.coverage", "65942827-030c-423d-9fd9-b971e621668b");
        pauseBuilders.add(pause);
    }

    /**
     * Determine if CruiseControl should run a build, given the current time.
     *
     * @param now
     * The current date
     * @return true if CruiseControl is currently paused (no build should run).
     */
    public boolean isPaused(Date now) {
        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_5_10.coverage", "9a38f034-b402-4615-998e-b9bea2b63f6e");
        checkParamNotNull("date", now);
        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_5_10.coverage", "02b3401d-a04b-4150-81a6-abab9ce22f82");
        PauseBuilder pause = findPause(now);
        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_5_10.coverage", "b38687b8-5711-4d00-92f1-444147305a0c");
        if (pause != null) {
            writeline("/home/ubuntu/results/coverage/Schedule/Schedule_5_10.coverage", "057b8241-a7b8-44f4-88fd-cb80f0ff4519");
            LOG.info("CruiseControl is paused until: " + getEndTimeString(pause));
            writeline("/home/ubuntu/results/coverage/Schedule/Schedule_5_10.coverage", "eca9e0dd-eadb-45ee-8553-f19473a13fc9");
            return true;
        }
        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_5_10.coverage", "34ca7a6c-d788-4d60-a0d1-f3b23b66cbcb");
        return false;
    }

    /**
     * Returns a String representing the time following the end time of the given {@link PauseBuilder}.
     *
     * @param builder
     * the <code>PauseBuilder</code> to be considered.
     * @return a String representing the time following the end time of the <code>PauseBuilder</code>.
     */
    private String getEndTimeString(final PauseBuilder builder) {
        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_5_10.coverage", "4a27a488-69bd-4789-87fa-a9deb5a5de70");
        final Calendar cal = Calendar.getInstance();
        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_5_10.coverage", "b9b66f82-fea4-41eb-96a8-28d841e0ba87");
        cal.set(Calendar.HOUR_OF_DAY, builder.getEndTime() / 100);
        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_5_10.coverage", "2f305177-a5fe-4aa4-9506-05b9cafcacfa");
        cal.set(Calendar.MINUTE, builder.getEndTime() % 100);
        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_5_10.coverage", "7d3940f7-e510-4c95-9286-8b4d415f8b31");
        cal.add(Calendar.MINUTE, 1);
        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_5_10.coverage", "3f6a9b40-3711-4e2f-8ee1-d9afb54387dc");
        return timeFormatter.format(cal.getTime());
    }

    PauseBuilder findPause(final Date date) {
        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_5_10.coverage", "cb517c11-2317-4541-8069-67e3bdedaf93");
        checkParamNotNull("date", date);
        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_5_10.coverage", "be7fa9c8-fbe3-4fe5-8337-edeab34a758e");
        for (final PauseBuilder pause : pauseBuilders) {
            writeline("/home/ubuntu/results/coverage/Schedule/Schedule_5_10.coverage", "536f7c41-012d-4f19-bddb-933edd9195d2");
            if (pause.isPaused(date)) {
                writeline("/home/ubuntu/results/coverage/Schedule/Schedule_5_10.coverage", "ecb95c75-0c24-4c3e-99b2-2166f456cad7");
                return pause;
            }
        }
        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_5_10.coverage", "48849777-9b61-45e5-9bc2-2e3c9dfb571f");
        return null;
    }

    /**
     * Select the correct <code>Builder</code> and start a build.
     *
     * @param buildNumber
     * The sequential build number.
     * @param lastBuild
     * The date of the last build.
     * @param now
     * The current time.
     * @param properties
     * Properties that would need to be passed in to the actual build tool.
     * @param buildTarget
     * the build target to use instead of the configured one (pass in null if no override is needed)
     * @param progress
     * the progress callback object.
     * @return JDOM Element representation of build log.
     * @throws CruiseControlException if something fails
     */
    public Element build(final int buildNumber, final Date lastBuild, final Date now, final Map<String, String> properties, final String buildTarget, final Progress progress) throws CruiseControlException {
        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_5_10.coverage", "0bd52e44-2a64-4347-8641-1e715aa59115");
        final Builder builder = selectBuilder(buildNumber, lastBuild, now);
        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_5_10.coverage", "d535ef7b-5610-47d2-9e88-ad3fb236c8e2");
        if (buildTarget != null) {
            writeline("/home/ubuntu/results/coverage/Schedule/Schedule_5_10.coverage", "7ddf5af9-6e3a-4a86-8c46-543176a20de0");
            LOG.info("Overriding build target with \"" + buildTarget + "\"");
            writeline("/home/ubuntu/results/coverage/Schedule/Schedule_5_10.coverage", "b3e2fc17-a9ff-4802-8a3b-9210adc8f9e0");
            return builder.buildWithTarget(properties, buildTarget, (getShowProgress() ? progress : null));
        }
        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_5_10.coverage", "7f1f5861-fdd4-43e2-832b-3a4b780479f7");
        return builder.build(properties, (getShowProgress() ? progress : null));
    }

    /**
     * Select the correct build based on the current buildNumber and time.
     *
     * @param buildNumber
     * The sequential build number
     * @param lastBuild
     * The date of the last build.
     * @param now
     * The current time.
     * @return The <code>Builder</code> that should be run.
     * @throws CruiseControlException if something fails
     */
    protected Builder selectBuilder(final int buildNumber, final Date lastBuild, final Date now) throws CruiseControlException {
        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_5_10.coverage", "b32e2ba1-5aec-4378-b60a-629ea0994a69");
        Builder builder = findBuilder(buildNumber, lastBuild, now);
        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_5_10.coverage", "e2fe05ed-eae7-43f9-9a84-ef418f539974");
        if (builder == null) {
            writeline("/home/ubuntu/results/coverage/Schedule/Schedule_5_10.coverage", "7c024e96-ce27-4ef6-b2bc-65705d47b48b");
            final long timeToNextBuild = getTimeToNextBuild(now, ONE_MINUTE);
            writeline("/home/ubuntu/results/coverage/Schedule/Schedule_5_10.coverage", "dbe20edc-44c0-41e7-9349-b9dc24b0929c");
            final Date futureDate = getFutureDate(now, timeToNextBuild);
            writeline("/home/ubuntu/results/coverage/Schedule/Schedule_5_10.coverage", "ae3a1679-b1c1-456b-b3b8-10993351e10c");
            builder = findBuilder(buildNumber, now, futureDate);
        }
        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_5_10.coverage", "722ec6cc-8c22-4216-833d-ac604067c314");
        if (builder == null) {
            writeline("/home/ubuntu/results/coverage/Schedule/Schedule_5_10.coverage", "2962bb7f-d060-419a-8f01-c934d75bce23");
            validate();
            writeline("/home/ubuntu/results/coverage/Schedule/Schedule_5_10.coverage", "49e765b3-37b4-48c1-b7d4-1214c5b40c77");
            throw new CruiseControlException("configuration error not caught by validate? no builder selected");
        }
        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_5_10.coverage", "c16b82c9-161b-4bfd-9cc4-8fd3f56dfe16");
        return builder;
    }

    private Builder findBuilder(final int buildNumber, final Date lastBuild, final Date now) throws CruiseControlException {
        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_5_10.coverage", "fb7d74b2-c7ee-4698-9167-9a53bd89431f");
        for (final Builder builder : builders) {
            writeline("/home/ubuntu/results/coverage/Schedule/Schedule_5_10.coverage", "b0823d19-3458-49e4-9b7b-a5341ebd2199");
            if (builder.isTimeBuilder()) {
                writeline("/home/ubuntu/results/coverage/Schedule/Schedule_5_10.coverage", "8e23e468-a6e0-466e-8119-8fb0f618c2ac");
                final int buildTime = builder.getTime();
                writeline("/home/ubuntu/results/coverage/Schedule/Schedule_5_10.coverage", "4cfa3bc0-063f-4f55-9b0f-3bc4dd5be64c");
                final boolean didntBuildToday = builderDidntBuildToday(lastBuild, now, buildTime);
                writeline("/home/ubuntu/results/coverage/Schedule/Schedule_5_10.coverage", "35260e99-c17f-474f-9950-93c567c0a253");
                final int nowTime = DateUtil.getTimeFromDate(now);
                writeline("/home/ubuntu/results/coverage/Schedule/Schedule_5_10.coverage", "dfdf7564-e8a6-4566-a674-cf9d5a809986");
                final boolean isAfterBuildTime = buildTime <= nowTime;
                writeline("/home/ubuntu/results/coverage/Schedule/Schedule_5_10.coverage", "db1bc702-b671-49e7-bb2e-52f9d84abe4c");
                final boolean isValidDay = builder.isValidDay(now);
                writeline("/home/ubuntu/results/coverage/Schedule/Schedule_5_10.coverage", "f93884dc-51cc-4de8-95f9-0ae928a54f18");
                if (didntBuildToday && isAfterBuildTime && isValidDay) {
                    writeline("/home/ubuntu/results/coverage/Schedule/Schedule_5_10.coverage", "9a3c27d8-1de0-4bc1-9291-182dfcb47140");
                    return builder;
                }
            } else if (builder.getMultiple() > 0) {
                writeline("/home/ubuntu/results/coverage/Schedule/Schedule_5_10.coverage", "d2b72965-00e4-471a-8ccb-9f2268841934");
                if (builder.isValidDay(now)) {
                    writeline("/home/ubuntu/results/coverage/Schedule/Schedule_5_10.coverage", "0098fdb5-8622-49a6-ab5d-adc03620ce48");
                    if ((buildNumber % builder.getMultiple()) == 0) {
                        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_5_10.coverage", "5ff4abba-d8b9-4af7-937b-74ee566bacb3");
                        return builder;
                    }
                }
            } else {
                writeline("/home/ubuntu/results/coverage/Schedule/Schedule_5_10.coverage", "38eec484-f59d-4e64-9d3c-cba0072e3f78");
                throw new CruiseControlException("The selected Builder is not properly configured");
            }
        }
        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_5_10.coverage", "0263ec8c-6ed6-4681-bff4-b9d2e6e35324");
        return null;
    }

    boolean builderDidntBuildToday(Date lastBuild, Date now, int buildTime) {
        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_5_10.coverage", "7f583bf3-8daf-4a05-bada-9869c974468f");
        int time = DateUtil.getTimeFromDate(now);
        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_5_10.coverage", "86884376-b5b9-42cb-b35d-e5e42c682cb9");
        long timeMillis = DateUtil.convertToMillis(time);
        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_5_10.coverage", "c21ff0d5-624b-4384-8abe-834d94283978");
        long startOfToday = now.getTime() - timeMillis;
        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_5_10.coverage", "cfcc4ede-3d5d-4652-ad67-52f1016af767");
        boolean lastBuildYesterday = lastBuild.getTime() < startOfToday;
        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_5_10.coverage", "2023cae2-281d-4093-b964-61dc5ccf7ac4");
        boolean lastBuildTimeBeforeBuildTime = DateUtil.getTimeFromDate(lastBuild) < buildTime;
        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_5_10.coverage", "ecbef158-9e4c-4deb-96d6-91d762276184");
        return lastBuildYesterday || lastBuildTimeBeforeBuildTime;
    }

    long getTimeToNextBuild(Date now, long sleepInterval) {
        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_5_10.coverage", "98ffa453-433a-416d-9e55-ea9a0c6dd6e1");
        return getTimeToNextBuild(now, sleepInterval, 0);
    }

    private long getTimeToNextBuild(Date now, long sleepInterval, long priorPauseAdjustment) {
        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_5_10.coverage", "7ffb0a5c-6380-42c2-a6e4-115a8ff34016");
        long timeToNextBuild = sleepInterval;
        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_5_10.coverage", "1c215ff5-bd53-4fc2-a38c-2d870f27a227");
        LOG.debug("getTimeToNextBuild: initial timeToNextBuild = " + timeToNextBuild);
        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_5_10.coverage", "d7a91cd3-d532-47e4-9913-b443ac86d237");
        timeToNextBuild = checkMultipleBuilders(now, timeToNextBuild);
        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_5_10.coverage", "4416914e-e4f0-40ab-97b8-bf1211ab7cf6");
        LOG.debug("getTimeToNextBuild: after checkMultipleBuilders = " + timeToNextBuild);
        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_5_10.coverage", "67d16428-44e4-4a84-91b5-b476cd5ccc7f");
        timeToNextBuild = checkTimeBuilders(now, timeToNextBuild);
        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_5_10.coverage", "9a4d7d94-7ea2-4c12-b067-1c4d89df92f8");
        LOG.debug("getTimeToNextBuild: after checkTimeBuilders = " + timeToNextBuild);
        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_5_10.coverage", "def57306-42bd-4afc-9aa7-aab646d5fac7");
        long timeTillNotPaused = checkPauseBuilders(now, timeToNextBuild);
        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_5_10.coverage", "883e6a72-65e1-481c-9b2d-507b41a6e9b9");
        LOG.debug("getTimeToNextBuild: after checkPauseBuilders = " + timeToNextBuild);
        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_5_10.coverage", "2d3e29dd-d753-4f8f-a331-f856f7ce9de7");
        if (timeToNextBuild != timeTillNotPaused) {
            writeline("/home/ubuntu/results/coverage/Schedule/Schedule_5_10.coverage", "aacfd703-2baa-4815-bffe-e5941593c8ca");
            boolean atMaxTime = timeTillNotPaused >= MAX_INTERVAL_MILLISECONDS || priorPauseAdjustment >= MAX_INTERVAL_MILLISECONDS;
            writeline("/home/ubuntu/results/coverage/Schedule/Schedule_5_10.coverage", "bc001f1c-f81c-46d5-b2d6-31bcacf8dfc0");
            if (hasOnlyTimeBuilders() && !atMaxTime) {
                writeline("/home/ubuntu/results/coverage/Schedule/Schedule_5_10.coverage", "b85d4f79-1d91-4933-a4c3-af6f86ac712a");
                Date dateAfterPause = getFutureDate(now, timeTillNotPaused);
                writeline("/home/ubuntu/results/coverage/Schedule/Schedule_5_10.coverage", "216a621c-6cfd-4115-bd02-0ee7c9ee2bb5");
                long adjustmentFromEndOfPause = getTimeToNextBuild(dateAfterPause, 0, priorPauseAdjustment + timeTillNotPaused);
                writeline("/home/ubuntu/results/coverage/Schedule/Schedule_5_10.coverage", "265b5a11-149c-4f08-be52-9c7a432f0e2d");
                timeToNextBuild = timeTillNotPaused + adjustmentFromEndOfPause;
                writeline("/home/ubuntu/results/coverage/Schedule/Schedule_5_10.coverage", "439332c1-53ed-448d-9958-d1b60f701b19");
                timeToNextBuild = checkMaximumInterval(timeToNextBuild);
            } else {
                writeline("/home/ubuntu/results/coverage/Schedule/Schedule_5_10.coverage", "3276d795-af38-4c41-b285-e393094e2ec7");
                timeToNextBuild = timeTillNotPaused;
            }
        }
        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_5_10.coverage", "6f66bde6-dbab-4d1f-b1c2-191e98b372dc");
        return timeToNextBuild;
    }

    private long checkMultipleBuilders(final Date now, final long interval) {
        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_5_10.coverage", "16a5e46e-4d37-4dfc-ade6-f847f89b91ad");
        if (hasOnlyTimeBuilders()) {
            writeline("/home/ubuntu/results/coverage/Schedule/Schedule_5_10.coverage", "eff2d339-325b-484e-b3d5-9fde638d4847");
            LOG.debug("has only time builders, so no correction for multiple builders.");
            writeline("/home/ubuntu/results/coverage/Schedule/Schedule_5_10.coverage", "b6412abb-225a-4b64-8542-b23b497b7d0b");
            return interval;
        }
        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_5_10.coverage", "153b5235-19b3-46a5-96c9-2f4424336172");
        Date then = getFutureDate(now, interval);
        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_5_10.coverage", "a5aee37a-0766-43df-9584-c2b6e2443ac4");
        final List<Builder> buildersForOtherDays = new ArrayList<Builder>();
        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_5_10.coverage", "d2fd6141-a780-4b72-a8dd-12801fd828c6");
        for (final Builder builder : builders) {
            writeline("/home/ubuntu/results/coverage/Schedule/Schedule_5_10.coverage", "f868d47f-6d9b-4fb6-b55b-809c058cc4c8");
            if (!builder.isTimeBuilder()) {
                writeline("/home/ubuntu/results/coverage/Schedule/Schedule_5_10.coverage", "8e184a56-138a-430b-88bb-a7beea4a1c7c");
                if (builder.getMultiple() == 1) {
                    writeline("/home/ubuntu/results/coverage/Schedule/Schedule_5_10.coverage", "b3e57512-19b3-415a-994b-70dce323025d");
                    if (builder.isValidDay(then)) {
                        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_5_10.coverage", "be0d22ef-6a09-4001-a89c-31630ed88f1f");
                        LOG.debug("multiple=1 builder found that could run on " + then);
                        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_5_10.coverage", "558b602f-08c3-4e80-b5a0-dc8c3b6cc636");
                        return interval;
                    } else {
                        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_5_10.coverage", "b3646db1-7ab5-410f-b7a7-fcbef51b5704");
                        buildersForOtherDays.add(builder);
                    }
                }
            }
        }
        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_5_10.coverage", "2267d5bf-2cce-4011-930e-8f59ab92f470");
        if (buildersForOtherDays.size() == 0) {
            writeline("/home/ubuntu/results/coverage/Schedule/Schedule_5_10.coverage", "ce01f909-9f78-41a2-80ce-83eba980a0a2");
            LOG.error("configuration error: has some multiple builders but no multiple=1 builders found!");
            writeline("/home/ubuntu/results/coverage/Schedule/Schedule_5_10.coverage", "a4c0b393-7e7b-4f9e-8d49-9be5dba0145b");
            return interval;
        } else {
            writeline("/home/ubuntu/results/coverage/Schedule/Schedule_5_10.coverage", "3e2b6f05-3809-4531-9212-27cb35ef6995");
            LOG.debug("no multiple=1 builders found for " + then + ". checking other days");
        }
        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_5_10.coverage", "501abaa4-abf5-4918-9064-ce8fc2309ed9");
        for (int i = 1; i < 7; i++) {
            writeline("/home/ubuntu/results/coverage/Schedule/Schedule_5_10.coverage", "5b93d9ae-28bd-4845-aff8-e9ee6bdb6726");
            long daysPastInitialInterval = i * ONE_DAY;
            writeline("/home/ubuntu/results/coverage/Schedule/Schedule_5_10.coverage", "7075bc09-947e-4152-b096-5bac6336bc90");
            then = getFutureDate(now, interval + daysPastInitialInterval);
            writeline("/home/ubuntu/results/coverage/Schedule/Schedule_5_10.coverage", "14b8890f-acb0-464c-8666-5920f73470fe");
            for (final Builder builder : builders) {
                writeline("/home/ubuntu/results/coverage/Schedule/Schedule_5_10.coverage", "6da46f2b-4a84-4340-a923-bcb03d48e42e");
                if (builder.isValidDay(then)) {
                    writeline("/home/ubuntu/results/coverage/Schedule/Schedule_5_10.coverage", "f3d4d65d-c4bb-45aa-84f8-3c6ba770cbe2");
                    LOG.debug("multiple=1 builder found that could run on " + then);
                    writeline("/home/ubuntu/results/coverage/Schedule/Schedule_5_10.coverage", "905d9c29-d41d-44f6-9ac6-b41f456fe1eb");
                    long correctionToMidnight = getTimePastMidnight(then);
                    writeline("/home/ubuntu/results/coverage/Schedule/Schedule_5_10.coverage", "f84b1c9f-2d75-4e04-b9be-8c83a5910aec");
                    return interval + daysPastInitialInterval - correctionToMidnight;
                }
            }
        }
        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_5_10.coverage", "4f53576f-36c5-4aef-91e5-78338e9b6fba");
        LOG.error("configuration error? could not find appropriate multiple=1 builder.");
        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_5_10.coverage", "294bc017-e00e-48d5-9524-fe978745a9e3");
        return interval;
    }

    private long getTimePastMidnight(Date date) {
        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_5_10.coverage", "d3dfd34e-1c9b-4253-a90e-bce28fb7c3a8");
        Calendar cal = Calendar.getInstance();
        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_5_10.coverage", "4a457064-bb6e-459a-bb28-a13fb67f2fd4");
        cal.setTime(date);
        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_5_10.coverage", "72fa3c61-63eb-4861-a43e-0e0d84e9e3d0");
        long time = 60 * ONE_MINUTE * cal.get(Calendar.HOUR_OF_DAY);
        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_5_10.coverage", "bb9a8e7e-96c5-4af6-a7c2-5801f2e92ff5");
        time += ONE_MINUTE * cal.get(Calendar.MINUTE);
        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_5_10.coverage", "a2fc29ad-da6e-41dc-a345-605513f7efbf");
        return time;
    }

    private boolean hasOnlyTimeBuilders() {
        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_5_10.coverage", "1e668f98-b639-4c8e-ade8-ebbd3057634a");
        boolean onlyTimeBuilders = true;
        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_5_10.coverage", "fa314d00-0a09-484d-a616-383b26baff78");
        for (final Builder builder : builders) {
            writeline("/home/ubuntu/results/coverage/Schedule/Schedule_5_10.coverage", "ea2c0005-4f3a-4b12-8583-0c6a2685e3c1");
            if (!builder.isTimeBuilder()) {
                writeline("/home/ubuntu/results/coverage/Schedule/Schedule_5_10.coverage", "582a6d91-fb2a-4d77-9da4-9758b0599544");
                onlyTimeBuilders = false;
                writeline("/home/ubuntu/results/coverage/Schedule/Schedule_5_10.coverage", "ea1d03a8-7549-483d-9273-3bd5e5f85b8b");
                break;
            }
        }
        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_5_10.coverage", "1bac5ab9-75b6-49eb-9ef8-630a9bc28488");
        return onlyTimeBuilders;
    }

    long checkTimeBuilders(final Date now, final long proposedTime) {
        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_5_10.coverage", "b55c0fe7-284d-4dd7-bc63-1ec5173fdbc3");
        long timeToNextBuild = proposedTime;
        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_5_10.coverage", "df318254-9481-4af4-b820-7b09d05082f1");
        if (hasOnlyTimeBuilders()) {
            writeline("/home/ubuntu/results/coverage/Schedule/Schedule_5_10.coverage", "647cb070-baa7-40fa-98af-6657ea88bc14");
            timeToNextBuild = Long.MAX_VALUE;
        }
        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_5_10.coverage", "7a6faa39-1a37-45c6-a9ab-4f778522e6e9");
        final int nowTime = DateUtil.getTimeFromDate(now);
        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_5_10.coverage", "44743053-4992-4d1c-91e5-2acde6a244cc");
        for (final Builder builder : builders) {
            writeline("/home/ubuntu/results/coverage/Schedule/Schedule_5_10.coverage", "d84911d0-feeb-49c7-bfbb-edcc857381c5");
            if (builder.isTimeBuilder()) {
                writeline("/home/ubuntu/results/coverage/Schedule/Schedule_5_10.coverage", "f1595dbb-8b74-4600-9623-53d087ab64dc");
                long timeToThisBuild = Long.MAX_VALUE;
                writeline("/home/ubuntu/results/coverage/Schedule/Schedule_5_10.coverage", "99072655-5a4d-4593-b1a4-be2e3ac39187");
                final Calendar cal = Calendar.getInstance();
                writeline("/home/ubuntu/results/coverage/Schedule/Schedule_5_10.coverage", "972b190c-21e9-42a3-8477-9a87e301ee66");
                final long oneYear = 365;
                writeline("/home/ubuntu/results/coverage/Schedule/Schedule_5_10.coverage", "9fe496c8-46a0-4787-a0ec-4c9c24078c33");
                for (int daysInTheFuture = 0; daysInTheFuture < oneYear; daysInTheFuture++) {
                    writeline("/home/ubuntu/results/coverage/Schedule/Schedule_5_10.coverage", "e41cd405-65be-47ba-b2f2-bfe53231d709");
                    cal.setTime(now);
                    writeline("/home/ubuntu/results/coverage/Schedule/Schedule_5_10.coverage", "f38b378f-c7c2-43f5-9687-8e80f417e64d");
                    cal.add(Calendar.DATE, daysInTheFuture);
                    writeline("/home/ubuntu/results/coverage/Schedule/Schedule_5_10.coverage", "f7ebff6d-f7f0-4c35-8aa5-630b76f10abe");
                    Date future = cal.getTime();
                    writeline("/home/ubuntu/results/coverage/Schedule/Schedule_5_10.coverage", "60b404b6-79a7-4105-bd48-dc9940a1fb8c");
                    final boolean dayIsValid = builder.isValidDay(future);
                    writeline("/home/ubuntu/results/coverage/Schedule/Schedule_5_10.coverage", "8b84addf-d7de-4a85-bdcb-a81ba3a224aa");
                    if (dayIsValid) {
                        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_5_10.coverage", "151a3ce3-ba33-48e2-8f2c-08ff9e945f8a");
                        int thisBuildTime = builder.getTime();
                        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_5_10.coverage", "4e89580d-bfb2-4d7d-b910-214afd6c1c18");
                        boolean timePassedToday = (daysInTheFuture == 0) && (nowTime > thisBuildTime);
                        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_5_10.coverage", "ab542551-9c0f-49e3-b489-a6d305d8e836");
                        if (!timePassedToday) {
                            writeline("/home/ubuntu/results/coverage/Schedule/Schedule_5_10.coverage", "b886ad3e-c1f7-4bc5-b5bb-2050ea6c085e");
                            int buildHour = thisBuildTime / 100;
                            writeline("/home/ubuntu/results/coverage/Schedule/Schedule_5_10.coverage", "a3752735-c456-4bff-8e93-b82dec729de5");
                            int buildMinute = thisBuildTime % 100;
                            writeline("/home/ubuntu/results/coverage/Schedule/Schedule_5_10.coverage", "6272c8e0-8a97-4c11-99fd-b82a5dfd1c67");
                            cal.set(Calendar.HOUR_OF_DAY, buildHour);
                            writeline("/home/ubuntu/results/coverage/Schedule/Schedule_5_10.coverage", "acfaaceb-3b24-4749-8be0-4df82dd4c5ca");
                            cal.set(Calendar.MINUTE, buildMinute);
                            writeline("/home/ubuntu/results/coverage/Schedule/Schedule_5_10.coverage", "4c7a7545-63d5-47f9-bfd4-7eb77888f834");
                            future = cal.getTime();
                            writeline("/home/ubuntu/results/coverage/Schedule/Schedule_5_10.coverage", "12678679-a56b-4f7d-b162-dc31531f741f");
                            timeToThisBuild = future.getTime() - now.getTime();
                            writeline("/home/ubuntu/results/coverage/Schedule/Schedule_5_10.coverage", "8bfc5a8d-5a00-41b1-a19f-ced1f7fbd7c0");
                            break;
                        }
                    }
                }
                writeline("/home/ubuntu/results/coverage/Schedule/Schedule_5_10.coverage", "64ba1c8d-491f-4354-80ba-92d54fed7238");
                if (timeToThisBuild < timeToNextBuild) {
                    writeline("/home/ubuntu/results/coverage/Schedule/Schedule_5_10.coverage", "461a6ab3-a497-4d41-ae7b-212e77c00d8e");
                    timeToNextBuild = timeToThisBuild;
                }
            }
        }
        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_5_10.coverage", "e28b358a-91cb-46d3-ba46-40f85bfb5c64");
        if (timeToNextBuild > MAX_INTERVAL_MILLISECONDS) {
            writeline("/home/ubuntu/results/coverage/Schedule/Schedule_5_10.coverage", "ade76088-1caa-4cd6-a2aa-5566ca08109a");
            LOG.error("checkTimeBuilders exceeding maximum interval. using proposed value [" + proposedTime + "] instead");
            writeline("/home/ubuntu/results/coverage/Schedule/Schedule_5_10.coverage", "46ed5203-5510-483c-ac5f-9ba6545369f0");
            timeToNextBuild = proposedTime;
        }
        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_5_10.coverage", "3b5f3d6b-6da8-4498-bd24-15a0b1ee919b");
        return timeToNextBuild;
    }

    long checkPauseBuilders(Date now, long proposedTime) {
        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_5_10.coverage", "9520e11e-a53c-4a3a-aebc-5e6d6873c03f");
        long oldTime = proposedTime;
        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_5_10.coverage", "4a524c6c-d96a-4e8f-ba85-5fe5cb4e7b18");
        long newTime = checkForPauseAtProposedTime(now, oldTime);
        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_5_10.coverage", "3519d664-e219-4447-9ede-70a92b96a9c4");
        while (oldTime != newTime) {
            writeline("/home/ubuntu/results/coverage/Schedule/Schedule_5_10.coverage", "5ecc0eaf-9058-45d3-810a-808085b26826");
            oldTime = newTime;
            writeline("/home/ubuntu/results/coverage/Schedule/Schedule_5_10.coverage", "17e4f5be-c1e3-4cd9-b790-d657e0e523d4");
            newTime = checkForPauseAtProposedTime(now, oldTime);
        }
        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_5_10.coverage", "bd034fef-48f3-4c1d-9b5e-e2bd0bd39e95");
        return newTime;
    }

    private long checkForPauseAtProposedTime(Date now, long proposedTime) {
        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_5_10.coverage", "9a782130-9753-493f-90ae-cb62720d7e55");
        Date futureDate = getFutureDate(now, proposedTime);
        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_5_10.coverage", "c4db8ae9-c4d9-4f9d-a6c8-924a9a65af3b");
        PauseBuilder pause = findPause(futureDate);
        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_5_10.coverage", "13d3a4ee-496e-4684-8659-0cc9f42849b1");
        if (pause == null) {
            writeline("/home/ubuntu/results/coverage/Schedule/Schedule_5_10.coverage", "f8279eb7-f87d-4a99-971f-4e2c8e0ffa9d");
            return proposedTime;
        }
        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_5_10.coverage", "6056c928-87c8-476d-9335-fcad698fe526");
        int endPause = pause.getEndTime();
        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_5_10.coverage", "17ae0685-1901-45f5-83e0-dc0513030a6e");
        int futureTime = DateUtil.getTimeFromDate(futureDate);
        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_5_10.coverage", "7a47d05b-b821-4aa5-bb85-e320441bf8bf");
        long timeToEndOfPause = proposedTime + DateUtil.milliTimeDifference(futureTime, endPause);
        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_5_10.coverage", "4c3ca579-8081-4dd8-8988-3794cc745bb4");
        timeToEndOfPause = checkMaximumInterval(timeToEndOfPause);
        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_5_10.coverage", "8e713723-43c6-49b3-938d-a040a9debf20");
        return timeToEndOfPause == MAX_INTERVAL_MILLISECONDS ? timeToEndOfPause : timeToEndOfPause + ONE_MINUTE;
    }

    private long checkMaximumInterval(long timeToEndOfPause) {
        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_5_10.coverage", "e9772e88-b753-41f4-b30a-ff9138dc6449");
        if (timeToEndOfPause > MAX_INTERVAL_MILLISECONDS) {
            writeline("/home/ubuntu/results/coverage/Schedule/Schedule_5_10.coverage", "c82dcdc0-b077-42fc-8356-4df635210c16");
            LOG.error("maximum interval exceeded! project perpetually paused?");
            writeline("/home/ubuntu/results/coverage/Schedule/Schedule_5_10.coverage", "0830ea08-1d84-4b9d-946c-870b46621d7a");
            return MAX_INTERVAL_MILLISECONDS;
        }
        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_5_10.coverage", "3e45b63a-d4fa-4d3e-9183-13f96edeecca");
        return timeToEndOfPause;
    }

    private Date getFutureDate(Date now, long delay) {
        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_5_10.coverage", "e03c4347-031a-487e-8692-e88d3b49d3f5");
        long futureMillis = now.getTime() + delay;
        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_5_10.coverage", "c3427699-4e2c-40a9-92a0-73373146442b");
        return new Date(futureMillis);
    }

    public void setInterval(long intervalBetweenModificationChecks) {
        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_5_10.coverage", "3277689c-f9be-49f1-bea4-3c2b7e168227");
        if (intervalBetweenModificationChecks <= 0) {
            writeline("/home/ubuntu/results/coverage/Schedule/Schedule_5_10.coverage", "3919bb11-38ec-4bd6-83da-9482db8663c9");
            throw new IllegalArgumentException("interval must be greater than zero");
        }
        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_5_10.coverage", "da91742e-9ac5-463a-a8cc-0424c37576c3");
        interval = intervalBetweenModificationChecks * ONE_SECOND;
    }

    public long getInterval() {
        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_5_10.coverage", "5e579c79-5cc0-46d0-a805-f8728ddabc70");
        return interval;
    }

    public void setShowProgress(final boolean showProgress) {
        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_5_10.coverage", "e815f911-7123-4e56-a481-8c3ec9625098");
        this.showProgress = showProgress;
    }

    public boolean getShowProgress() {
        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_5_10.coverage", "1222a990-6adc-4861-b9d0-7e738dc061d0");
        return showProgress;
    }

    public void validate() throws CruiseControlException {
        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_5_10.coverage", "b56e0612-e1ef-4fd1-942e-6d535f574ea0");
        ValidationHelper.assertTrue(builders.size() > 0, "schedule element requires at least one nested builder element");
        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_5_10.coverage", "56180f80-3f50-4bc1-a192-146be299bc27");
        ValidationHelper.assertFalse(interval > ONE_YEAR, "maximum interval value is " + MAX_INTERVAL_SECONDS + " (one year)");
        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_5_10.coverage", "9e403e7e-a8d5-4540-8fc2-52e361a4995f");
        if (hasOnlyTimeBuilders()) {
            writeline("/home/ubuntu/results/coverage/Schedule/Schedule_5_10.coverage", "ef02cf08-ca6d-4777-8209-43ec7b74c492");
            LOG.warn("schedule has all time based builders: interval value will be ignored.");
            writeline("/home/ubuntu/results/coverage/Schedule/Schedule_5_10.coverage", "58fbf47f-ec32-4d9d-9da5-a40449dc44ff");
            ValidationHelper.assertFalse(checkWithinPause(new ArrayList<Builder>(builders)), "all build times during pauses.");
        }
        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_5_10.coverage", "3351143e-5948-4211-99c8-93d8f9062d3b");
        // Validate the child builders, since no one else seems to be doing it.
        for (final Builder next : builders) {
            writeline("/home/ubuntu/results/coverage/Schedule/Schedule_5_10.coverage", "e7b5ab38-7c63-41d4-baad-0dd48671d8b1");
            next.validate();
        }
    }

    private boolean checkWithinPause(List timeBuilders) {
        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_5_10.coverage", "dbe8337d-5548-4ba6-9964-0387ad846d4f");
        for (int i = 0; i < timeBuilders.size(); i++) {
            writeline("/home/ubuntu/results/coverage/Schedule/Schedule_5_10.coverage", "dae60058-9825-43fb-8785-a719c96fe887");
            Builder builder = (Builder) timeBuilders.get(i);
            writeline("/home/ubuntu/results/coverage/Schedule/Schedule_5_10.coverage", "0ba9fb2c-96bf-4fd2-8eea-9abce0954514");
            for (final PauseBuilder pauseBuilder : pauseBuilders) {
                writeline("/home/ubuntu/results/coverage/Schedule/Schedule_5_10.coverage", "8142cfe1-2a6e-47b5-bd25-95683ff9e4f7");
                if (buildDaySameAsPauseDay(builder, pauseBuilder) && buildTimeWithinPauseTime(builder, pauseBuilder)) {
                    writeline("/home/ubuntu/results/coverage/Schedule/Schedule_5_10.coverage", "3295a24d-b853-45d6-ac3f-2697fe8e7d56");
                    timeBuilders.remove(builder);
                    writeline("/home/ubuntu/results/coverage/Schedule/Schedule_5_10.coverage", "eb9e4ad2-8847-4841-944c-bb83f4264e9b");
                    StringBuffer message = new StringBuffer();
                    writeline("/home/ubuntu/results/coverage/Schedule/Schedule_5_10.coverage", "9c9cb94d-cc34-467c-b220-576078c5f807");
                    message.append("time Builder for time ");
                    writeline("/home/ubuntu/results/coverage/Schedule/Schedule_5_10.coverage", "f8ddfb07-0ee4-441d-80cf-53b610f3a66d");
                    message.append(Integer.toString(builder.getTime()));
                    writeline("/home/ubuntu/results/coverage/Schedule/Schedule_5_10.coverage", "f5be7d1d-d220-461d-bc29-8e1d3565a586");
                    if (builder.getDay() != Builder.NOT_SET) {
                        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_5_10.coverage", "a5a6267d-be16-48c9-89f7-62e93a468695");
                        message.append(" and day of ");
                        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_5_10.coverage", "92194482-e6d0-4467-a54e-73fc4cdd82a6");
                        message.append(getDayString(builder.getDay()));
                    }
                    writeline("/home/ubuntu/results/coverage/Schedule/Schedule_5_10.coverage", "f263f59e-14c6-4c0a-92b2-c30e10a7fd05");
                    message.append(" is always within a pause and will never build");
                    writeline("/home/ubuntu/results/coverage/Schedule/Schedule_5_10.coverage", "be980f90-4806-47e5-973b-f3482b15312c");
                    LOG.error(message.toString());
                }
            }
        }
        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_5_10.coverage", "699215e3-6910-4cf5-b2bf-4b945b0b8cad");
        return timeBuilders.isEmpty();
    }

    /**
     * @param day
     * int value
     * @return english string value
     */
    String getDayString(int day) {
        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_5_10.coverage", "864184ec-c998-4835-b422-288af5834885");
        if (day < 1 || day > 7) {
            writeline("/home/ubuntu/results/coverage/Schedule/Schedule_5_10.coverage", "678892cf-9c90-4b1e-8170-274e582a3d9a");
            throw new IllegalArgumentException("valid values of days are between 1 and 7, was " + day);
        }
        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_5_10.coverage", "59dbcf2d-16c6-4537-a74c-520fa3de5294");
        DateFormatSymbols symbols = new DateFormatSymbols(Locale.ENGLISH);
        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_5_10.coverage", "7f853c12-968b-48e1-a378-fd81c52d8604");
        String[] weekdays = symbols.getWeekdays();
        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_5_10.coverage", "7f43d9dc-5960-4217-aa2e-4ad6eaa28954");
        return weekdays[day];
    }

    private boolean buildDaySameAsPauseDay(Builder builder, PauseBuilder pauseBuilder) {
        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_5_10.coverage", "104db11e-8e4f-4d97-909f-e68e5d966a76");
        return pauseBuilder.getDay() == PauseBuilder.NOT_SET || pauseBuilder.getDay() == builder.getDay();
    }

    private boolean buildTimeWithinPauseTime(Builder builder, PauseBuilder pauseBuilder) {
        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_5_10.coverage", "3b81cf2f-c932-4e17-ba74-f7bfb49318bb");
        return pauseBuilder.getStartTime() < builder.getTime() && builder.getTime() < pauseBuilder.getEndTime();
    }

    /**
     * utility method to check method parameters and ensure they're not null
     *
     * @param paramName
     * name of the parameter to check
     * @param param
     * parameter to check
     */
    private void checkParamNotNull(String paramName, Object param) {
        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_5_10.coverage", "db10a286-bd55-4266-8eff-8e5d351dc30a");
        if (param == null) {
            writeline("/home/ubuntu/results/coverage/Schedule/Schedule_5_10.coverage", "08ae76ba-7464-43c4-9cbd-ac1c125b53fc");
            throw new IllegalArgumentException(paramName + " can't be null");
        }
    }

    public List getBuilders() {
        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_5_10.coverage", "3da44463-0b7b-48ac-8665-de59faff67fd");
        return builders;
    }

    /**
     * sort time builders before non-time builders, then by multiple (higher first)
     * then ones with days before non-days
     */
    private static class BuilderComparitor implements Comparator<Builder> {

        public int compare(Builder b1, Builder b2) {
            if (b1.isTimeBuilder() && !b2.isTimeBuilder()) {
                return -1;
            }
            if (!b1.isTimeBuilder() && b2.isTimeBuilder()) {
                return 1;
            }
            if (b1.isTimeBuilder() && b2.isTimeBuilder()) {
                return checkDays(b1, b2);
            }
            if (b1.getMultiple() > b2.getMultiple()) {
                return -1;
            }
            if (b1.getMultiple() < b2.getMultiple()) {
                return 1;
            }
            return checkDays(b1, b2);
        }

        private int checkDays(Builder b1, Builder b2) {
            boolean b1HasDaySet = b1.getDay() >= 0;
            boolean b2HasDaySet = b2.getDay() >= 0;
            if (b1HasDaySet && !b2HasDaySet) {
                return -1;
            }
            if (!b1HasDaySet && b2HasDaySet) {
                return 1;
            }
            return 0;
        }
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
