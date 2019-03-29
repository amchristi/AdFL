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
        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_8_10.coverage", "5f85002b-5fa2-4c91-af90-320fa91786ad");
        checkParamNotNull("builder", builder);
        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_8_10.coverage", "e7c70ebb-ca64-4c6c-b61b-0c83d4d406bd");
        builders.add(builder);
        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_8_10.coverage", "68b35ac4-a04d-414f-9300-7e927bc87445");
        Collections.sort(builders, builderComparator);
    }

    public void add(final PauseBuilder pause) {
        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_8_10.coverage", "bf421f2a-8391-4ff2-816e-8cda316dc270");
        checkParamNotNull("pauseBuilder", pause);
        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_8_10.coverage", "e45742fa-19e3-4f4e-88bf-6ced2abe94d2");
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
        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_8_10.coverage", "4062af3b-16d3-4216-8116-97c0fc09ede9");
        checkParamNotNull("date", now);
        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_8_10.coverage", "3ce712c4-478a-464a-b34c-c4c8183105af");
        PauseBuilder pause = findPause(now);
        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_8_10.coverage", "c247f195-fd82-4771-ba05-31c14d0222e0");
        if (pause != null) {
            writeline("/home/ubuntu/results/coverage/Schedule/Schedule_8_10.coverage", "f207a4c6-c459-41f7-aee1-be5a4603ff37");
            LOG.info("CruiseControl is paused until: " + getEndTimeString(pause));
            writeline("/home/ubuntu/results/coverage/Schedule/Schedule_8_10.coverage", "1925a3e6-6ca3-4742-95b8-ca8a6bce1cef");
            return true;
        }
        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_8_10.coverage", "8580c7fe-c6e8-4f92-9512-cc76bba30005");
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
        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_8_10.coverage", "7aa6c089-c6f2-4614-a75a-4c8c6a9a87db");
        final Calendar cal = Calendar.getInstance();
        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_8_10.coverage", "551f7b00-24a6-42a4-b2f4-1df32e39e58b");
        cal.set(Calendar.HOUR_OF_DAY, builder.getEndTime() / 100);
        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_8_10.coverage", "a2953a02-4f3c-47fd-bf95-a6de4351e044");
        cal.set(Calendar.MINUTE, builder.getEndTime() % 100);
        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_8_10.coverage", "1912dd4d-ae7a-4990-b9b1-604832b41f45");
        cal.add(Calendar.MINUTE, 1);
        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_8_10.coverage", "bbc7a5e2-ee5c-466c-8da9-5d6b213572ee");
        return timeFormatter.format(cal.getTime());
    }

    PauseBuilder findPause(final Date date) {
        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_8_10.coverage", "f437f21f-f12b-4057-a54d-b20f72ab6a22");
        checkParamNotNull("date", date);
        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_8_10.coverage", "203de3fa-146c-407b-b0bc-4672bc417f68");
        for (final PauseBuilder pause : pauseBuilders) {
            writeline("/home/ubuntu/results/coverage/Schedule/Schedule_8_10.coverage", "cbc08930-49d0-468f-822f-6626f76c6e1e");
            if (pause.isPaused(date)) {
                writeline("/home/ubuntu/results/coverage/Schedule/Schedule_8_10.coverage", "fdda3a77-46a0-4037-8cfe-7e74c2f0367f");
                return pause;
            }
        }
        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_8_10.coverage", "65494985-5641-4454-9da4-632f34c2cc8f");
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
        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_8_10.coverage", "58714b01-4640-488e-a064-3d176a8e9870");
        final Builder builder = selectBuilder(buildNumber, lastBuild, now);
        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_8_10.coverage", "3e509dcc-9af4-4c48-aad5-da0cf2d908cf");
        if (buildTarget != null) {
            writeline("/home/ubuntu/results/coverage/Schedule/Schedule_8_10.coverage", "5c986b6f-308a-43c2-8a77-23f5fbb014bc");
            LOG.info("Overriding build target with \"" + buildTarget + "\"");
            writeline("/home/ubuntu/results/coverage/Schedule/Schedule_8_10.coverage", "337a1df4-2df0-4088-a146-d303010cd1f8");
            return builder.buildWithTarget(properties, buildTarget, (getShowProgress() ? progress : null));
        }
        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_8_10.coverage", "8c159ef1-b5b7-4a75-a670-447596499a18");
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
        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_8_10.coverage", "309f4d6b-d4c3-434b-9def-fbb9089a0073");
        Builder builder = findBuilder(buildNumber, lastBuild, now);
        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_8_10.coverage", "7821b03f-1419-4e9a-be3e-ed5104b84159");
        if (builder == null) {
            writeline("/home/ubuntu/results/coverage/Schedule/Schedule_8_10.coverage", "62befda6-9c8e-454d-8b37-06615cac20f8");
            final long timeToNextBuild = getTimeToNextBuild(now, ONE_MINUTE);
            writeline("/home/ubuntu/results/coverage/Schedule/Schedule_8_10.coverage", "14f18b80-3de2-48d2-9553-ba708c11b9aa");
            final Date futureDate = getFutureDate(now, timeToNextBuild);
            writeline("/home/ubuntu/results/coverage/Schedule/Schedule_8_10.coverage", "cb8d593d-cc39-45ec-b888-0d66931d0e18");
            builder = findBuilder(buildNumber, now, futureDate);
        }
        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_8_10.coverage", "54236f7b-0339-4803-a4fc-b595a4246b1d");
        if (builder == null) {
            writeline("/home/ubuntu/results/coverage/Schedule/Schedule_8_10.coverage", "05f302fa-aa2c-4a82-b276-b12838f8dfeb");
            validate();
            writeline("/home/ubuntu/results/coverage/Schedule/Schedule_8_10.coverage", "ed106d06-5221-4438-b25a-249858dccca0");
            throw new CruiseControlException("configuration error not caught by validate? no builder selected");
        }
        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_8_10.coverage", "4ec01507-ec67-4478-95ff-205ef13f09d9");
        return builder;
    }

    private Builder findBuilder(final int buildNumber, final Date lastBuild, final Date now) throws CruiseControlException {
        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_8_10.coverage", "0dace77e-1c23-4995-b4d5-38bd3d131994");
        for (final Builder builder : builders) {
            writeline("/home/ubuntu/results/coverage/Schedule/Schedule_8_10.coverage", "4c59a35b-dd2a-4dbb-8d9e-d68a9466388b");
            if (builder.isTimeBuilder()) {
                writeline("/home/ubuntu/results/coverage/Schedule/Schedule_8_10.coverage", "a877efa0-3088-4d77-bee1-70a57cdd9d83");
                final int buildTime = builder.getTime();
                writeline("/home/ubuntu/results/coverage/Schedule/Schedule_8_10.coverage", "26a6659b-36fb-4a63-b4fe-71a1e7f82b0b");
                final boolean didntBuildToday = builderDidntBuildToday(lastBuild, now, buildTime);
                writeline("/home/ubuntu/results/coverage/Schedule/Schedule_8_10.coverage", "e37fc15c-ac49-4f5e-9a24-0c202fe200c9");
                final int nowTime = DateUtil.getTimeFromDate(now);
                writeline("/home/ubuntu/results/coverage/Schedule/Schedule_8_10.coverage", "f209ecba-a9cb-4ebe-b01c-5518d93ff214");
                final boolean isAfterBuildTime = buildTime <= nowTime;
                writeline("/home/ubuntu/results/coverage/Schedule/Schedule_8_10.coverage", "f3b6c07d-c065-4ee4-b0fa-ff2a080eb72c");
                final boolean isValidDay = builder.isValidDay(now);
                writeline("/home/ubuntu/results/coverage/Schedule/Schedule_8_10.coverage", "ad220a04-87c6-45db-b7ac-6d8156d7d659");
                if (didntBuildToday && isAfterBuildTime && isValidDay) {
                    writeline("/home/ubuntu/results/coverage/Schedule/Schedule_8_10.coverage", "44b16bf8-709e-4702-acaa-63819703b82e");
                    return builder;
                }
            } else if (builder.getMultiple() > 0) {
                writeline("/home/ubuntu/results/coverage/Schedule/Schedule_8_10.coverage", "16c5fb45-0607-418d-aad3-b591b2779b18");
                if (builder.isValidDay(now)) {
                    writeline("/home/ubuntu/results/coverage/Schedule/Schedule_8_10.coverage", "b7f86af3-0f48-4cfc-aec3-fdcb08acb60b");
                    if ((buildNumber % builder.getMultiple()) == 0) {
                        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_8_10.coverage", "8d20f6fa-db81-4949-a73a-657bfcbbe8e8");
                        return builder;
                    }
                }
            } else {
                writeline("/home/ubuntu/results/coverage/Schedule/Schedule_8_10.coverage", "dc3a6919-8864-4014-bb26-e812fc0c1e1e");
                throw new CruiseControlException("The selected Builder is not properly configured");
            }
        }
        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_8_10.coverage", "1e485e34-f9c7-48c2-bbe0-078194f49caf");
        return null;
    }

    boolean builderDidntBuildToday(Date lastBuild, Date now, int buildTime) {
        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_8_10.coverage", "bfd2fc0d-ab8d-4ea1-84f0-297eb7cb50f1");
        int time = DateUtil.getTimeFromDate(now);
        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_8_10.coverage", "76e0bd3f-5dfb-48ea-a0a0-7cfab1bdb566");
        long timeMillis = DateUtil.convertToMillis(time);
        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_8_10.coverage", "b8b1c3a4-d45e-485f-a8db-e191ea87ef13");
        long startOfToday = now.getTime() - timeMillis;
        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_8_10.coverage", "c9311dd7-e955-4285-8b92-f5a04a0c0ea5");
        boolean lastBuildYesterday = lastBuild.getTime() < startOfToday;
        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_8_10.coverage", "2284ca45-7281-440f-b042-ddc8ccaa75a6");
        boolean lastBuildTimeBeforeBuildTime = DateUtil.getTimeFromDate(lastBuild) < buildTime;
        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_8_10.coverage", "2540a60d-6aa0-4749-8e83-c0427182315c");
        return lastBuildYesterday || lastBuildTimeBeforeBuildTime;
    }

    long getTimeToNextBuild(Date now, long sleepInterval) {
        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_8_10.coverage", "7c03e64d-303d-434d-bb8e-8cac554d58ee");
        return getTimeToNextBuild(now, sleepInterval, 0);
    }

    private long getTimeToNextBuild(Date now, long sleepInterval, long priorPauseAdjustment) {
        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_8_10.coverage", "e085d2b0-ee1a-42e9-abae-d94b8e7a4627");
        long timeToNextBuild = sleepInterval;
        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_8_10.coverage", "2bb50ab7-9967-4901-b30e-55d16d24f2da");
        LOG.debug("getTimeToNextBuild: initial timeToNextBuild = " + timeToNextBuild);
        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_8_10.coverage", "0e49e972-84a5-4972-a838-aba4dca1c952");
        timeToNextBuild = checkMultipleBuilders(now, timeToNextBuild);
        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_8_10.coverage", "e4e11de2-bd84-49ac-8dbe-b8c8e6307d53");
        LOG.debug("getTimeToNextBuild: after checkMultipleBuilders = " + timeToNextBuild);
        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_8_10.coverage", "087383fc-f76b-4b03-b22a-99d6c51a0f53");
        timeToNextBuild = checkTimeBuilders(now, timeToNextBuild);
        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_8_10.coverage", "20113848-b8b5-42b4-b227-45b56f32c7de");
        LOG.debug("getTimeToNextBuild: after checkTimeBuilders = " + timeToNextBuild);
        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_8_10.coverage", "b7b8ee10-0bc1-42e6-8ebe-984b850d7ff8");
        long timeTillNotPaused = checkPauseBuilders(now, timeToNextBuild);
        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_8_10.coverage", "fac967b9-e019-4ccf-9625-45b2e2cdbd93");
        LOG.debug("getTimeToNextBuild: after checkPauseBuilders = " + timeToNextBuild);
        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_8_10.coverage", "2fc18d7b-4a8d-4801-a7cd-b7d47b6c25e6");
        if (timeToNextBuild != timeTillNotPaused) {
            writeline("/home/ubuntu/results/coverage/Schedule/Schedule_8_10.coverage", "9711f554-ee0d-4f52-b9af-014365c10775");
            boolean atMaxTime = timeTillNotPaused >= MAX_INTERVAL_MILLISECONDS || priorPauseAdjustment >= MAX_INTERVAL_MILLISECONDS;
            writeline("/home/ubuntu/results/coverage/Schedule/Schedule_8_10.coverage", "698d141d-377a-4122-a4c2-ce272c0e2956");
            if (hasOnlyTimeBuilders() && !atMaxTime) {
                writeline("/home/ubuntu/results/coverage/Schedule/Schedule_8_10.coverage", "11078fa7-c254-42e3-82f6-8c57c777d17a");
                Date dateAfterPause = getFutureDate(now, timeTillNotPaused);
                writeline("/home/ubuntu/results/coverage/Schedule/Schedule_8_10.coverage", "0e37b440-f3f4-47e6-ad71-4a94a99b6c8f");
                long adjustmentFromEndOfPause = getTimeToNextBuild(dateAfterPause, 0, priorPauseAdjustment + timeTillNotPaused);
                writeline("/home/ubuntu/results/coverage/Schedule/Schedule_8_10.coverage", "c726c82d-552b-4e45-b354-b0dc32ec9bb9");
                timeToNextBuild = timeTillNotPaused + adjustmentFromEndOfPause;
                writeline("/home/ubuntu/results/coverage/Schedule/Schedule_8_10.coverage", "58c05401-816f-4151-96cb-d63cf950d6e1");
                timeToNextBuild = checkMaximumInterval(timeToNextBuild);
            } else {
                writeline("/home/ubuntu/results/coverage/Schedule/Schedule_8_10.coverage", "325ae487-ac63-44c9-9a4d-02dbc944cd76");
                timeToNextBuild = timeTillNotPaused;
            }
        }
        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_8_10.coverage", "a37a97ff-2bf8-4c30-aa20-28f2dcae2ea7");
        return timeToNextBuild;
    }

    private long checkMultipleBuilders(final Date now, final long interval) {
        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_8_10.coverage", "278dd0c3-90ad-4a47-b16f-9fcf2247df99");
        if (hasOnlyTimeBuilders()) {
            writeline("/home/ubuntu/results/coverage/Schedule/Schedule_8_10.coverage", "d51885e3-7396-440d-9878-ddcf5f5283cc");
            LOG.debug("has only time builders, so no correction for multiple builders.");
            writeline("/home/ubuntu/results/coverage/Schedule/Schedule_8_10.coverage", "2166d696-1208-4e13-822d-a8f86e47762d");
            return interval;
        }
        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_8_10.coverage", "593b627d-351a-47ed-9a8e-a9aa4891a14e");
        Date then = getFutureDate(now, interval);
        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_8_10.coverage", "352486b0-097a-41de-979b-e6938250b04c");
        final List<Builder> buildersForOtherDays = new ArrayList<Builder>();
        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_8_10.coverage", "4097afd4-ad0d-4020-a3f8-c2b45130ac3a");
        for (final Builder builder : builders) {
            writeline("/home/ubuntu/results/coverage/Schedule/Schedule_8_10.coverage", "1374feda-bb22-4cb5-9941-40270fb18a69");
            if (!builder.isTimeBuilder()) {
                writeline("/home/ubuntu/results/coverage/Schedule/Schedule_8_10.coverage", "5d8848ce-0d92-4017-9660-5df511f0ac06");
                if (builder.getMultiple() == 1) {
                    writeline("/home/ubuntu/results/coverage/Schedule/Schedule_8_10.coverage", "04439e92-3196-4847-8a2c-a11a29482a2d");
                    if (builder.isValidDay(then)) {
                        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_8_10.coverage", "6a6adbf4-ef7b-4bb5-81b2-bc8affb81291");
                        LOG.debug("multiple=1 builder found that could run on " + then);
                        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_8_10.coverage", "1d7d7287-85b7-4958-a732-3709fe9377ac");
                        return interval;
                    } else {
                        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_8_10.coverage", "8ad387bc-1a67-4b6e-8e2a-9334f7ea7ebd");
                        buildersForOtherDays.add(builder);
                    }
                }
            }
        }
        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_8_10.coverage", "5c9f5174-ccb8-426d-837a-506bf9e31d36");
        if (buildersForOtherDays.size() == 0) {
            writeline("/home/ubuntu/results/coverage/Schedule/Schedule_8_10.coverage", "83efb4a2-0a89-4173-bd99-5ae74e2ab476");
            LOG.error("configuration error: has some multiple builders but no multiple=1 builders found!");
            writeline("/home/ubuntu/results/coverage/Schedule/Schedule_8_10.coverage", "9e1c4840-564d-4495-8472-8d97b12bf51b");
            return interval;
        } else {
            writeline("/home/ubuntu/results/coverage/Schedule/Schedule_8_10.coverage", "f16e1b46-f083-49fd-b70b-48f1c11f01ee");
            LOG.debug("no multiple=1 builders found for " + then + ". checking other days");
        }
        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_8_10.coverage", "10da52be-3374-46b3-a096-4c1d201afa65");
        for (int i = 1; i < 7; i++) {
            writeline("/home/ubuntu/results/coverage/Schedule/Schedule_8_10.coverage", "3388bdae-7c5f-4f37-9e99-cb2d6801a254");
            long daysPastInitialInterval = i * ONE_DAY;
            writeline("/home/ubuntu/results/coverage/Schedule/Schedule_8_10.coverage", "2f45bc51-e35a-428d-afa1-08bce3db2f59");
            then = getFutureDate(now, interval + daysPastInitialInterval);
            writeline("/home/ubuntu/results/coverage/Schedule/Schedule_8_10.coverage", "ab38fa7e-ee59-4d8d-afc2-45a1a997090d");
            for (final Builder builder : builders) {
                writeline("/home/ubuntu/results/coverage/Schedule/Schedule_8_10.coverage", "a5fe9a04-12ca-4205-a3e1-21ebca489326");
                if (builder.isValidDay(then)) {
                    writeline("/home/ubuntu/results/coverage/Schedule/Schedule_8_10.coverage", "13534a4b-c1e0-4482-b269-3bc6f2bbdf64");
                    LOG.debug("multiple=1 builder found that could run on " + then);
                    writeline("/home/ubuntu/results/coverage/Schedule/Schedule_8_10.coverage", "8ac31bdd-f4df-418f-b3f4-c6ba4e8d8f2d");
                    long correctionToMidnight = getTimePastMidnight(then);
                    writeline("/home/ubuntu/results/coverage/Schedule/Schedule_8_10.coverage", "d1aceafb-6269-4316-b1f0-626d38fe5e31");
                    return interval + daysPastInitialInterval - correctionToMidnight;
                }
            }
        }
        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_8_10.coverage", "eecab328-a791-44ce-920b-9bc5fd9bd1cc");
        LOG.error("configuration error? could not find appropriate multiple=1 builder.");
        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_8_10.coverage", "d2d77893-2713-433a-bd9d-9072e85ed582");
        return interval;
    }

    private long getTimePastMidnight(Date date) {
        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_8_10.coverage", "4a6c15e0-bd5c-43c1-bbfb-cfdf7715eaa4");
        Calendar cal = Calendar.getInstance();
        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_8_10.coverage", "735fe1e2-33c0-4bbe-ade9-5d1f8210cf94");
        cal.setTime(date);
        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_8_10.coverage", "42c824a1-efad-425c-b0e9-6e126e2c7c84");
        long time = 60 * ONE_MINUTE * cal.get(Calendar.HOUR_OF_DAY);
        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_8_10.coverage", "34ed06d5-8ed4-40c1-b049-4c034c8ae362");
        time += ONE_MINUTE * cal.get(Calendar.MINUTE);
        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_8_10.coverage", "d6e329bf-1fff-4b62-bb88-62dcf0be474a");
        return time;
    }

    private boolean hasOnlyTimeBuilders() {
        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_8_10.coverage", "fc1a745a-2b61-4a77-a8c8-84016c445348");
        boolean onlyTimeBuilders = true;
        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_8_10.coverage", "88ec6b18-a771-4034-bb71-eea744f292d9");
        for (final Builder builder : builders) {
            writeline("/home/ubuntu/results/coverage/Schedule/Schedule_8_10.coverage", "e0dad401-1e16-4019-b217-e737ac448ee7");
            if (!builder.isTimeBuilder()) {
                writeline("/home/ubuntu/results/coverage/Schedule/Schedule_8_10.coverage", "c98ce41e-1ef1-438a-8529-1ac9a18a7897");
                onlyTimeBuilders = false;
                writeline("/home/ubuntu/results/coverage/Schedule/Schedule_8_10.coverage", "e07f8b8e-049f-48f5-9138-7c3ef42bc972");
                break;
            }
        }
        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_8_10.coverage", "4f114ca6-3c76-42c4-a70a-4e0f4449364e");
        return onlyTimeBuilders;
    }

    long checkTimeBuilders(final Date now, final long proposedTime) {
        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_8_10.coverage", "efa3cc3b-4ac9-4a8c-8f40-651eda01fc98");
        long timeToNextBuild = proposedTime;
        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_8_10.coverage", "aa1f4b89-4181-4e9d-bd21-271e1ae422b2");
        if (hasOnlyTimeBuilders()) {
            writeline("/home/ubuntu/results/coverage/Schedule/Schedule_8_10.coverage", "74c5dd65-e84d-4c03-bb55-7f7cbc0eff1f");
            timeToNextBuild = Long.MAX_VALUE;
        }
        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_8_10.coverage", "160d8298-b9ac-4113-8662-7ef4f9293d8a");
        final int nowTime = DateUtil.getTimeFromDate(now);
        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_8_10.coverage", "fecc4abd-bb25-423d-89af-22f1984932e7");
        for (final Builder builder : builders) {
            writeline("/home/ubuntu/results/coverage/Schedule/Schedule_8_10.coverage", "f1031455-28c4-4785-85f3-1466135912d4");
            if (builder.isTimeBuilder()) {
                writeline("/home/ubuntu/results/coverage/Schedule/Schedule_8_10.coverage", "561916b5-ad1f-433b-ae14-9b714c4e99e2");
                long timeToThisBuild = Long.MAX_VALUE;
                writeline("/home/ubuntu/results/coverage/Schedule/Schedule_8_10.coverage", "8b47bc72-8c2f-491e-b05d-9d82d4682ce8");
                final Calendar cal = Calendar.getInstance();
                writeline("/home/ubuntu/results/coverage/Schedule/Schedule_8_10.coverage", "bdcdb8d3-bf49-468c-8000-55039621c377");
                final long oneYear = 365;
                writeline("/home/ubuntu/results/coverage/Schedule/Schedule_8_10.coverage", "bdec133e-8354-44cf-aefc-5d10adb6bf31");
                for (int daysInTheFuture = 0; daysInTheFuture < oneYear; daysInTheFuture++) {
                    writeline("/home/ubuntu/results/coverage/Schedule/Schedule_8_10.coverage", "cc555cdb-820f-4d2d-949e-dbb278c00dec");
                    cal.setTime(now);
                    writeline("/home/ubuntu/results/coverage/Schedule/Schedule_8_10.coverage", "0b6f54cc-5ae3-4f3e-8677-bf860efefbab");
                    cal.add(Calendar.DATE, daysInTheFuture);
                    writeline("/home/ubuntu/results/coverage/Schedule/Schedule_8_10.coverage", "8cc23413-00c3-42ac-9fdb-8f3ddeec77ab");
                    Date future = cal.getTime();
                    writeline("/home/ubuntu/results/coverage/Schedule/Schedule_8_10.coverage", "072b102b-7fc8-405b-ac46-6494503f3d81");
                    final boolean dayIsValid = builder.isValidDay(future);
                    writeline("/home/ubuntu/results/coverage/Schedule/Schedule_8_10.coverage", "81c33b86-3ebb-4d15-af4a-3288b88239f9");
                    if (dayIsValid) {
                        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_8_10.coverage", "fac330ed-887b-41e2-b018-8772d606bed2");
                        int thisBuildTime = builder.getTime();
                        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_8_10.coverage", "cf953227-d4d5-4548-843c-d7f24eea9079");
                        boolean timePassedToday = (daysInTheFuture == 0) && (nowTime > thisBuildTime);
                        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_8_10.coverage", "a3a12901-76e5-402b-9da8-ab1541979cd2");
                        if (!timePassedToday) {
                            writeline("/home/ubuntu/results/coverage/Schedule/Schedule_8_10.coverage", "b1425cee-8c96-4eaf-b8a3-167033d6ade6");
                            int buildHour = thisBuildTime / 100;
                            writeline("/home/ubuntu/results/coverage/Schedule/Schedule_8_10.coverage", "be22229b-a5e9-4eba-a73c-260d1ed615fb");
                            int buildMinute = thisBuildTime % 100;
                            writeline("/home/ubuntu/results/coverage/Schedule/Schedule_8_10.coverage", "925d38a7-e6db-418c-bd3c-4e8043e041a7");
                            cal.set(Calendar.HOUR_OF_DAY, buildHour);
                            writeline("/home/ubuntu/results/coverage/Schedule/Schedule_8_10.coverage", "6d78a647-4b6b-48b3-acb4-352ad5c91a3e");
                            cal.set(Calendar.MINUTE, buildMinute);
                            writeline("/home/ubuntu/results/coverage/Schedule/Schedule_8_10.coverage", "2c398681-f932-4dbb-931b-c63f4eb6ea7a");
                            future = cal.getTime();
                            writeline("/home/ubuntu/results/coverage/Schedule/Schedule_8_10.coverage", "d1fc1629-d1f2-4e1c-a83e-0da3c302ec58");
                            timeToThisBuild = future.getTime() - now.getTime();
                            writeline("/home/ubuntu/results/coverage/Schedule/Schedule_8_10.coverage", "c12468d5-a830-43da-8633-63d1a052c040");
                            break;
                        }
                    }
                }
                writeline("/home/ubuntu/results/coverage/Schedule/Schedule_8_10.coverage", "ae0cee7f-adcb-43ec-8c2c-417d787fdf5c");
                if (timeToThisBuild < timeToNextBuild) {
                    writeline("/home/ubuntu/results/coverage/Schedule/Schedule_8_10.coverage", "e024a927-6a74-4f29-a1f2-a517965f48a0");
                    timeToNextBuild = timeToThisBuild;
                }
            }
        }
        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_8_10.coverage", "1f472f75-befc-4334-9ce9-6c0033aac785");
        if (timeToNextBuild > MAX_INTERVAL_MILLISECONDS) {
            writeline("/home/ubuntu/results/coverage/Schedule/Schedule_8_10.coverage", "76e037c0-d90e-4785-b83d-9b18ab65ab2f");
            LOG.error("checkTimeBuilders exceeding maximum interval. using proposed value [" + proposedTime + "] instead");
            writeline("/home/ubuntu/results/coverage/Schedule/Schedule_8_10.coverage", "46110428-2b87-47e3-af1e-3ed54d40f3dc");
            timeToNextBuild = proposedTime;
        }
        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_8_10.coverage", "850da514-383d-4adb-b1c6-b4235e83df3f");
        return timeToNextBuild;
    }

    long checkPauseBuilders(Date now, long proposedTime) {
        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_8_10.coverage", "bee29e88-7ffc-4636-8936-7cf9fb2dc0e1");
        long oldTime = proposedTime;
        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_8_10.coverage", "0799b599-6bf3-4fb1-aef6-4dc930f772a7");
        long newTime = checkForPauseAtProposedTime(now, oldTime);
        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_8_10.coverage", "4d1a6453-db99-42a7-825d-c6957a266bed");
        while (oldTime != newTime) {
            writeline("/home/ubuntu/results/coverage/Schedule/Schedule_8_10.coverage", "caa7c7f6-f5b4-44eb-99e8-ff565c4eb8d5");
            oldTime = newTime;
            writeline("/home/ubuntu/results/coverage/Schedule/Schedule_8_10.coverage", "a3e368db-ba78-496d-9749-a43d90dc52dc");
            newTime = checkForPauseAtProposedTime(now, oldTime);
        }
        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_8_10.coverage", "b9d55dac-638d-4625-a68f-86c008e772af");
        return newTime;
    }

    private long checkForPauseAtProposedTime(Date now, long proposedTime) {
        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_8_10.coverage", "3fbbaf94-d6c1-4b9f-8003-601c116f07d7");
        Date futureDate = getFutureDate(now, proposedTime);
        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_8_10.coverage", "7b0c74c7-a5a9-4fbf-9b59-93ad16e5d34f");
        PauseBuilder pause = findPause(futureDate);
        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_8_10.coverage", "c2adaded-1e04-4452-be4e-4e8458fd02b7");
        if (pause == null) {
            writeline("/home/ubuntu/results/coverage/Schedule/Schedule_8_10.coverage", "e77f137f-09d5-440d-b35e-05a8a5429a36");
            return proposedTime;
        }
        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_8_10.coverage", "f3d5d0d4-997f-46bf-a238-79fff33c541e");
        int endPause = pause.getEndTime();
        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_8_10.coverage", "0150f482-cf60-4f4c-a9bc-4a17916a317e");
        int futureTime = DateUtil.getTimeFromDate(futureDate);
        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_8_10.coverage", "fe5be19c-a790-4aff-a010-0267ec28386e");
        long timeToEndOfPause = proposedTime + DateUtil.milliTimeDifference(futureTime, endPause);
        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_8_10.coverage", "b86fd076-fa7d-44c6-b691-a52119acec88");
        timeToEndOfPause = checkMaximumInterval(timeToEndOfPause);
        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_8_10.coverage", "f8d04748-c470-4b9e-bdb7-3899e00fd86f");
        return timeToEndOfPause == MAX_INTERVAL_MILLISECONDS ? timeToEndOfPause : timeToEndOfPause + ONE_MINUTE;
    }

    private long checkMaximumInterval(long timeToEndOfPause) {
        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_8_10.coverage", "e4858209-3ae6-451b-8557-a849088f5db1");
        if (timeToEndOfPause > MAX_INTERVAL_MILLISECONDS) {
            writeline("/home/ubuntu/results/coverage/Schedule/Schedule_8_10.coverage", "af595eb7-76ca-49bf-a59c-694eba385c7c");
            LOG.error("maximum interval exceeded! project perpetually paused?");
            writeline("/home/ubuntu/results/coverage/Schedule/Schedule_8_10.coverage", "196099ed-aee4-46dc-b9a2-36300b193055");
            return MAX_INTERVAL_MILLISECONDS;
        }
        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_8_10.coverage", "c291365c-cd07-4d93-b3b9-f7ce087e5ef0");
        return timeToEndOfPause;
    }

    private Date getFutureDate(Date now, long delay) {
        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_8_10.coverage", "d7b9a413-84ab-462d-90c6-208f4a396ec1");
        long futureMillis = now.getTime() + delay;
        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_8_10.coverage", "ee71fb7b-b4db-4040-8ea7-cd2b42e1b906");
        return new Date(futureMillis);
    }

    public void setInterval(long intervalBetweenModificationChecks) {
        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_8_10.coverage", "8fbd5d7a-565e-4e81-9254-1d6bd6b8d8b7");
        if (intervalBetweenModificationChecks <= 0) {
            writeline("/home/ubuntu/results/coverage/Schedule/Schedule_8_10.coverage", "bfb377a5-ce5f-4079-8cad-a7b02fa7cc7f");
            throw new IllegalArgumentException("interval must be greater than zero");
        }
        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_8_10.coverage", "731e2e0c-547b-4803-91a8-cbb9250d0bc0");
        interval = intervalBetweenModificationChecks * ONE_SECOND;
    }

    public long getInterval() {
        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_8_10.coverage", "1f88c225-ae48-4edb-86e1-8254a7d1b272");
        return interval;
    }

    public void setShowProgress(final boolean showProgress) {
        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_8_10.coverage", "4f6458fe-56a5-4553-959f-54dccc8af042");
        this.showProgress = showProgress;
    }

    public boolean getShowProgress() {
        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_8_10.coverage", "973d541c-c5fe-4c07-a721-54cc7a26fc60");
        return showProgress;
    }

    public void validate() throws CruiseControlException {
        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_8_10.coverage", "1045d9dc-3182-4b89-81c4-799f0ca3d83e");
        ValidationHelper.assertTrue(builders.size() > 0, "schedule element requires at least one nested builder element");
        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_8_10.coverage", "ef76ab85-5dac-4546-bd7e-f6652dbbb8be");
        ValidationHelper.assertFalse(interval > ONE_YEAR, "maximum interval value is " + MAX_INTERVAL_SECONDS + " (one year)");
        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_8_10.coverage", "89d1b297-724e-416b-8641-e9ae0a0af5b5");
        if (hasOnlyTimeBuilders()) {
            writeline("/home/ubuntu/results/coverage/Schedule/Schedule_8_10.coverage", "2dd5964c-60a7-418d-8ece-f1eb103fcf68");
            LOG.warn("schedule has all time based builders: interval value will be ignored.");
            writeline("/home/ubuntu/results/coverage/Schedule/Schedule_8_10.coverage", "4ecf9c45-8c41-431a-ae62-44ff9dc44a92");
            ValidationHelper.assertFalse(checkWithinPause(new ArrayList<Builder>(builders)), "all build times during pauses.");
        }
        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_8_10.coverage", "9d38603e-afba-45c3-adba-a7916a5e1756");
        // Validate the child builders, since no one else seems to be doing it.
        for (final Builder next : builders) {
            writeline("/home/ubuntu/results/coverage/Schedule/Schedule_8_10.coverage", "22644367-8bd3-4d9f-b4f3-e2fdbc3ea0e4");
            next.validate();
        }
    }

    private boolean checkWithinPause(List timeBuilders) {
        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_8_10.coverage", "c47db60c-2cf9-4131-9f83-bce983e6621f");
        for (int i = 0; i < timeBuilders.size(); i++) {
            writeline("/home/ubuntu/results/coverage/Schedule/Schedule_8_10.coverage", "9c63987e-95c1-4945-892a-800297c079b8");
            Builder builder = (Builder) timeBuilders.get(i);
            writeline("/home/ubuntu/results/coverage/Schedule/Schedule_8_10.coverage", "e3999dd3-5c13-4cfb-8904-59ad79397ed1");
            for (final PauseBuilder pauseBuilder : pauseBuilders) {
                writeline("/home/ubuntu/results/coverage/Schedule/Schedule_8_10.coverage", "30ae6550-2e41-4571-91eb-dc9562d7231c");
                if (buildDaySameAsPauseDay(builder, pauseBuilder) && buildTimeWithinPauseTime(builder, pauseBuilder)) {
                    writeline("/home/ubuntu/results/coverage/Schedule/Schedule_8_10.coverage", "4153bc42-3380-4735-9323-bb3a9a5bcde1");
                    timeBuilders.remove(builder);
                    writeline("/home/ubuntu/results/coverage/Schedule/Schedule_8_10.coverage", "a21fcce1-a79d-4133-8269-f3301fd9810c");
                    StringBuffer message = new StringBuffer();
                    writeline("/home/ubuntu/results/coverage/Schedule/Schedule_8_10.coverage", "af632670-1df6-48ff-97cd-506ca38abd5e");
                    message.append("time Builder for time ");
                    writeline("/home/ubuntu/results/coverage/Schedule/Schedule_8_10.coverage", "061cad75-48c0-4d22-9da3-1876c3ee70e6");
                    message.append(Integer.toString(builder.getTime()));
                    writeline("/home/ubuntu/results/coverage/Schedule/Schedule_8_10.coverage", "060fbb97-be2b-46c5-a21a-292228d91f8b");
                    if (builder.getDay() != Builder.NOT_SET) {
                        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_8_10.coverage", "78eb1119-eaf0-4fb0-9ccf-827ddb7a81a9");
                        message.append(" and day of ");
                        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_8_10.coverage", "10610606-209e-4437-a5b8-e351f2222acb");
                        message.append(getDayString(builder.getDay()));
                    }
                    writeline("/home/ubuntu/results/coverage/Schedule/Schedule_8_10.coverage", "9f8dde59-1ba0-46ae-bd48-4895f423e4d8");
                    message.append(" is always within a pause and will never build");
                    writeline("/home/ubuntu/results/coverage/Schedule/Schedule_8_10.coverage", "5610f334-c43f-4742-935a-c38e20959ad3");
                    LOG.error(message.toString());
                }
            }
        }
        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_8_10.coverage", "90c1baac-34be-4fd1-ae4e-4009cac5f557");
        return timeBuilders.isEmpty();
    }

    /**
     * @param day
     * int value
     * @return english string value
     */
    String getDayString(int day) {
        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_8_10.coverage", "37526700-b6d6-4b06-9e39-3ba6ddd2364e");
        if (day < 1 || day > 7) {
            writeline("/home/ubuntu/results/coverage/Schedule/Schedule_8_10.coverage", "36762c1b-e1cf-42e0-b8be-5df9bf0313a6");
            throw new IllegalArgumentException("valid values of days are between 1 and 7, was " + day);
        }
        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_8_10.coverage", "ee1ca460-c382-437e-a36e-b6168b2904b8");
        DateFormatSymbols symbols = new DateFormatSymbols(Locale.ENGLISH);
        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_8_10.coverage", "b2be6f90-f273-4e94-99a7-1fa5c0d1ea02");
        String[] weekdays = symbols.getWeekdays();
        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_8_10.coverage", "fda7ddf2-5b7d-4331-b155-26145c97e094");
        return weekdays[day];
    }

    private boolean buildDaySameAsPauseDay(Builder builder, PauseBuilder pauseBuilder) {
        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_8_10.coverage", "2e0c87d4-b07a-4f4a-a45b-5c5355d71d2b");
        return pauseBuilder.getDay() == PauseBuilder.NOT_SET || pauseBuilder.getDay() == builder.getDay();
    }

    private boolean buildTimeWithinPauseTime(Builder builder, PauseBuilder pauseBuilder) {
        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_8_10.coverage", "552274c5-2c9c-47ba-b451-24c3839c0382");
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
        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_8_10.coverage", "58e6b4af-6383-4955-87ae-54da450f79cf");
        if (param == null) {
            writeline("/home/ubuntu/results/coverage/Schedule/Schedule_8_10.coverage", "2bf7cb3b-c1e0-46b3-bf1b-70281f87c534");
            throw new IllegalArgumentException(paramName + " can't be null");
        }
    }

    public List getBuilders() {
        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_8_10.coverage", "d8da6af8-44e2-4c2c-9e5e-b37ed389f090");
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
