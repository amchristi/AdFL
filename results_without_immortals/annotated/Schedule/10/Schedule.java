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
        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_10_10.coverage", "991916cc-2f34-4e95-a206-2b8235f4b7ed");
        checkParamNotNull("builder", builder);
        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_10_10.coverage", "20a341fe-82b5-4fa3-981d-a9b74dc6de64");
        builders.add(builder);
        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_10_10.coverage", "c4c5c279-d8d8-4e7b-9ea0-9e1562fe03f5");
        Collections.sort(builders, builderComparator);
    }

    public void add(final PauseBuilder pause) {
        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_10_10.coverage", "f4b1afdc-7f63-4918-bd92-649336671f4a");
        checkParamNotNull("pauseBuilder", pause);
        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_10_10.coverage", "648adbe3-f39f-4684-9af3-41c73b53449e");
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
        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_10_10.coverage", "c8cd7a22-5193-44c7-b3db-142b6988281a");
        checkParamNotNull("date", now);
        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_10_10.coverage", "65b2d18d-0682-4239-a09a-bf1e9b754b15");
        PauseBuilder pause = findPause(now);
        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_10_10.coverage", "e9ae8127-4243-41c2-92dd-3452ccebd5d6");
        if (pause != null) {
            writeline("/home/ubuntu/results/coverage/Schedule/Schedule_10_10.coverage", "f158140d-10c1-4d59-84d2-e9da54429457");
            LOG.info("CruiseControl is paused until: " + getEndTimeString(pause));
            writeline("/home/ubuntu/results/coverage/Schedule/Schedule_10_10.coverage", "7cbf4464-176d-4c14-8d89-6b04280f7bec");
            return true;
        }
        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_10_10.coverage", "b250e4e2-c68c-4529-bff6-cb5189cdfb0f");
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
        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_10_10.coverage", "4bd02e7f-959c-4489-b0db-889ecec94ddd");
        final Calendar cal = Calendar.getInstance();
        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_10_10.coverage", "c70ba93f-cefd-434a-a772-73766aa521a6");
        cal.set(Calendar.HOUR_OF_DAY, builder.getEndTime() / 100);
        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_10_10.coverage", "1c2959fd-2d58-41d9-a7d1-2ea573cabf2e");
        cal.set(Calendar.MINUTE, builder.getEndTime() % 100);
        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_10_10.coverage", "2a5da654-fbe3-4bac-9950-c4e882f6d4d8");
        cal.add(Calendar.MINUTE, 1);
        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_10_10.coverage", "91d3b1c3-77cf-468a-b202-dd7041d1446e");
        return timeFormatter.format(cal.getTime());
    }

    PauseBuilder findPause(final Date date) {
        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_10_10.coverage", "e94c2122-cb70-4445-8423-e74381c3a71d");
        checkParamNotNull("date", date);
        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_10_10.coverage", "07b2041b-637e-4222-9e44-26cd3f1179c8");
        for (final PauseBuilder pause : pauseBuilders) {
            writeline("/home/ubuntu/results/coverage/Schedule/Schedule_10_10.coverage", "7b2b64a4-5d4b-4e4b-8335-ccd63d801009");
            if (pause.isPaused(date)) {
                writeline("/home/ubuntu/results/coverage/Schedule/Schedule_10_10.coverage", "4608a8a4-58e0-4038-964e-15878b15991a");
                return pause;
            }
        }
        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_10_10.coverage", "070f0d61-be76-481a-a64f-36e34b81c362");
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
        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_10_10.coverage", "40d4c806-8ed0-4567-91f2-8edabf35af9e");
        final Builder builder = selectBuilder(buildNumber, lastBuild, now);
        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_10_10.coverage", "44e3db1c-e044-4cd6-85f8-c18ae911bfb3");
        if (buildTarget != null) {
            writeline("/home/ubuntu/results/coverage/Schedule/Schedule_10_10.coverage", "1cee15d4-dcad-4861-a6d0-3b3654a2f720");
            LOG.info("Overriding build target with \"" + buildTarget + "\"");
            writeline("/home/ubuntu/results/coverage/Schedule/Schedule_10_10.coverage", "0ebe30b1-36b5-418d-919b-c16f89606fa0");
            return builder.buildWithTarget(properties, buildTarget, (getShowProgress() ? progress : null));
        }
        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_10_10.coverage", "763554f9-3667-4d79-a358-f2b6b84b212a");
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
        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_10_10.coverage", "e0b4ad3c-8a59-45d7-8c4e-9572ffc5c78c");
        Builder builder = findBuilder(buildNumber, lastBuild, now);
        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_10_10.coverage", "1080eedc-1189-4c02-9775-41db022839ae");
        if (builder == null) {
            writeline("/home/ubuntu/results/coverage/Schedule/Schedule_10_10.coverage", "76123208-6b0e-431f-8cc4-caa909fe99ee");
            final long timeToNextBuild = getTimeToNextBuild(now, ONE_MINUTE);
            writeline("/home/ubuntu/results/coverage/Schedule/Schedule_10_10.coverage", "d531ca55-b80c-4c72-ac4f-89764ec5ad56");
            final Date futureDate = getFutureDate(now, timeToNextBuild);
            writeline("/home/ubuntu/results/coverage/Schedule/Schedule_10_10.coverage", "5db2b7fc-cdf6-4b1e-9a9a-b49a9cf1bb5d");
            builder = findBuilder(buildNumber, now, futureDate);
        }
        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_10_10.coverage", "3722e39c-8dc7-4367-9268-8732852557f8");
        if (builder == null) {
            writeline("/home/ubuntu/results/coverage/Schedule/Schedule_10_10.coverage", "8c6d4132-be16-4c35-8f55-2eb2857d7c6e");
            validate();
            writeline("/home/ubuntu/results/coverage/Schedule/Schedule_10_10.coverage", "c69f401b-f88c-42b7-ab9a-c211b5c08a9e");
            throw new CruiseControlException("configuration error not caught by validate? no builder selected");
        }
        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_10_10.coverage", "76ed8685-4159-43d2-9baa-873c7d42142c");
        return builder;
    }

    private Builder findBuilder(final int buildNumber, final Date lastBuild, final Date now) throws CruiseControlException {
        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_10_10.coverage", "0841cd1f-bc41-4379-9d20-6239ae02e4a7");
        for (final Builder builder : builders) {
            writeline("/home/ubuntu/results/coverage/Schedule/Schedule_10_10.coverage", "502ad495-6dff-4ca7-b24b-dd07475867bc");
            if (builder.isTimeBuilder()) {
                writeline("/home/ubuntu/results/coverage/Schedule/Schedule_10_10.coverage", "9e560ce7-87b1-45f4-9801-4965d6e27de9");
                final int buildTime = builder.getTime();
                writeline("/home/ubuntu/results/coverage/Schedule/Schedule_10_10.coverage", "a20a8423-0f23-4320-a6b1-91320b8c0a5a");
                final boolean didntBuildToday = builderDidntBuildToday(lastBuild, now, buildTime);
                writeline("/home/ubuntu/results/coverage/Schedule/Schedule_10_10.coverage", "8d2e66bd-a953-4329-8939-cfbfaa5c84e8");
                final int nowTime = DateUtil.getTimeFromDate(now);
                writeline("/home/ubuntu/results/coverage/Schedule/Schedule_10_10.coverage", "1ed85874-0364-4806-b82a-6b69f7b0f9d8");
                final boolean isAfterBuildTime = buildTime <= nowTime;
                writeline("/home/ubuntu/results/coverage/Schedule/Schedule_10_10.coverage", "7651fdda-2067-4be0-aa20-d511f8cc6938");
                final boolean isValidDay = builder.isValidDay(now);
                writeline("/home/ubuntu/results/coverage/Schedule/Schedule_10_10.coverage", "cda63770-d77a-435a-87fa-9c80e84c84ad");
                if (didntBuildToday && isAfterBuildTime && isValidDay) {
                    writeline("/home/ubuntu/results/coverage/Schedule/Schedule_10_10.coverage", "1f2c2eaa-995c-46c0-9e27-28a3e80afe97");
                    return builder;
                }
            } else if (builder.getMultiple() > 0) {
                writeline("/home/ubuntu/results/coverage/Schedule/Schedule_10_10.coverage", "36be721a-e125-4149-8ba2-7ebdf9b7b610");
                if (builder.isValidDay(now)) {
                    writeline("/home/ubuntu/results/coverage/Schedule/Schedule_10_10.coverage", "e7fb669c-94e1-48ba-a39a-678fab395085");
                    if ((buildNumber % builder.getMultiple()) == 0) {
                        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_10_10.coverage", "6b941256-195e-430a-89a8-55683158cd42");
                        return builder;
                    }
                }
            } else {
                writeline("/home/ubuntu/results/coverage/Schedule/Schedule_10_10.coverage", "1d0d5b94-b96f-4d62-8cfb-3c34880b1609");
                throw new CruiseControlException("The selected Builder is not properly configured");
            }
        }
        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_10_10.coverage", "ab6d7030-386d-4b47-9d7c-7c76be19bf5b");
        return null;
    }

    boolean builderDidntBuildToday(Date lastBuild, Date now, int buildTime) {
        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_10_10.coverage", "fa2417aa-e039-430b-9c48-5346ab0c3813");
        int time = DateUtil.getTimeFromDate(now);
        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_10_10.coverage", "35170dee-f1ee-4511-a805-ded5ddcc2df2");
        long timeMillis = DateUtil.convertToMillis(time);
        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_10_10.coverage", "32890ac2-7940-49b8-b757-f746eb648e3a");
        long startOfToday = now.getTime() - timeMillis;
        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_10_10.coverage", "856e1421-5820-440e-97a9-99749537cc32");
        boolean lastBuildYesterday = lastBuild.getTime() < startOfToday;
        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_10_10.coverage", "00738dc5-8465-4d0b-9ba1-b397f8d311cf");
        boolean lastBuildTimeBeforeBuildTime = DateUtil.getTimeFromDate(lastBuild) < buildTime;
        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_10_10.coverage", "66a87112-1826-489d-85c8-287151ee08df");
        return lastBuildYesterday || lastBuildTimeBeforeBuildTime;
    }

    long getTimeToNextBuild(Date now, long sleepInterval) {
        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_10_10.coverage", "7aa29ead-a552-4615-8147-35a1f2a9ac84");
        return getTimeToNextBuild(now, sleepInterval, 0);
    }

    private long getTimeToNextBuild(Date now, long sleepInterval, long priorPauseAdjustment) {
        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_10_10.coverage", "0144615a-c2d8-429f-bbb7-58753ec9c051");
        long timeToNextBuild = sleepInterval;
        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_10_10.coverage", "eba8f208-3858-4a07-bc52-ee394fccda76");
        LOG.debug("getTimeToNextBuild: initial timeToNextBuild = " + timeToNextBuild);
        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_10_10.coverage", "21a73d83-a02f-4a08-aeb6-54009d2b1eaf");
        timeToNextBuild = checkMultipleBuilders(now, timeToNextBuild);
        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_10_10.coverage", "91e35713-d8ca-4c1e-8167-a876e9639e49");
        LOG.debug("getTimeToNextBuild: after checkMultipleBuilders = " + timeToNextBuild);
        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_10_10.coverage", "fbc8eaee-819e-44b6-b7d8-5b24edf01686");
        timeToNextBuild = checkTimeBuilders(now, timeToNextBuild);
        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_10_10.coverage", "f6cc4d69-0d2f-448b-a917-900a53a89673");
        LOG.debug("getTimeToNextBuild: after checkTimeBuilders = " + timeToNextBuild);
        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_10_10.coverage", "68c8656a-1576-442f-8b25-d2688ecc980e");
        long timeTillNotPaused = checkPauseBuilders(now, timeToNextBuild);
        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_10_10.coverage", "14979a51-d3c4-4b51-a6f2-b7cbdfd35a76");
        LOG.debug("getTimeToNextBuild: after checkPauseBuilders = " + timeToNextBuild);
        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_10_10.coverage", "c4cce366-1a2f-4d49-830a-1db82f1ebbf8");
        if (timeToNextBuild != timeTillNotPaused) {
            writeline("/home/ubuntu/results/coverage/Schedule/Schedule_10_10.coverage", "a9461261-f1be-4e05-98ca-ded1e8df63b6");
            boolean atMaxTime = timeTillNotPaused >= MAX_INTERVAL_MILLISECONDS || priorPauseAdjustment >= MAX_INTERVAL_MILLISECONDS;
            writeline("/home/ubuntu/results/coverage/Schedule/Schedule_10_10.coverage", "b5f8eb96-ed68-4f55-b994-3d7a4cddbd2b");
            if (hasOnlyTimeBuilders() && !atMaxTime) {
                writeline("/home/ubuntu/results/coverage/Schedule/Schedule_10_10.coverage", "912a5a4b-5e48-480d-ad7c-854e7abe26f4");
                Date dateAfterPause = getFutureDate(now, timeTillNotPaused);
                writeline("/home/ubuntu/results/coverage/Schedule/Schedule_10_10.coverage", "4e2c5de3-415b-451b-9e07-a336677c6045");
                long adjustmentFromEndOfPause = getTimeToNextBuild(dateAfterPause, 0, priorPauseAdjustment + timeTillNotPaused);
                writeline("/home/ubuntu/results/coverage/Schedule/Schedule_10_10.coverage", "14a423b1-ea03-4aee-93de-a58da1e46783");
                timeToNextBuild = timeTillNotPaused + adjustmentFromEndOfPause;
                writeline("/home/ubuntu/results/coverage/Schedule/Schedule_10_10.coverage", "26e118ad-8fcd-4409-81dc-512c21f9c388");
                timeToNextBuild = checkMaximumInterval(timeToNextBuild);
            } else {
                writeline("/home/ubuntu/results/coverage/Schedule/Schedule_10_10.coverage", "2d4b1052-d4a0-4ea5-9f23-34353a4215a5");
                timeToNextBuild = timeTillNotPaused;
            }
        }
        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_10_10.coverage", "f5cbe9ab-ef23-4ce4-b8b0-53e0335fa15a");
        return timeToNextBuild;
    }

    private long checkMultipleBuilders(final Date now, final long interval) {
        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_10_10.coverage", "bfd618b7-3522-4cef-8647-1375f900b9a9");
        if (hasOnlyTimeBuilders()) {
            writeline("/home/ubuntu/results/coverage/Schedule/Schedule_10_10.coverage", "bf2565a4-5515-4125-888b-1ffe23a49b6a");
            LOG.debug("has only time builders, so no correction for multiple builders.");
            writeline("/home/ubuntu/results/coverage/Schedule/Schedule_10_10.coverage", "7f26ba56-1cf4-4e7e-ba4e-121326ddfb5f");
            return interval;
        }
        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_10_10.coverage", "1016bde1-0fb1-4554-a938-c25a4cdbfaae");
        Date then = getFutureDate(now, interval);
        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_10_10.coverage", "63760bf7-8397-48ed-90bc-b03f42ed4e02");
        final List<Builder> buildersForOtherDays = new ArrayList<Builder>();
        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_10_10.coverage", "f9d1c980-f7f0-4d04-93af-4035cd14bc45");
        for (final Builder builder : builders) {
            writeline("/home/ubuntu/results/coverage/Schedule/Schedule_10_10.coverage", "4124a156-63d3-4c6b-8721-a834a5b40768");
            if (!builder.isTimeBuilder()) {
                writeline("/home/ubuntu/results/coverage/Schedule/Schedule_10_10.coverage", "a5ba4644-26d4-4b8c-9a93-ecdb30d351a5");
                if (builder.getMultiple() == 1) {
                    writeline("/home/ubuntu/results/coverage/Schedule/Schedule_10_10.coverage", "35ed04a2-c5cf-4466-8609-73ea12ad022f");
                    if (builder.isValidDay(then)) {
                        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_10_10.coverage", "0ea0d870-eae1-4ba0-9889-46bcf55f11eb");
                        LOG.debug("multiple=1 builder found that could run on " + then);
                        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_10_10.coverage", "ce124371-ea5d-4f28-aedf-a5ebe62efb69");
                        return interval;
                    } else {
                        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_10_10.coverage", "22553c7c-3cb7-47a1-83c2-6ee9482fd90b");
                        buildersForOtherDays.add(builder);
                    }
                }
            }
        }
        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_10_10.coverage", "4266c902-6116-4094-a8b6-249beebe72a5");
        if (buildersForOtherDays.size() == 0) {
            writeline("/home/ubuntu/results/coverage/Schedule/Schedule_10_10.coverage", "41a87f1b-96bd-4d18-84c0-978b936b358d");
            LOG.error("configuration error: has some multiple builders but no multiple=1 builders found!");
            writeline("/home/ubuntu/results/coverage/Schedule/Schedule_10_10.coverage", "e5afbd51-7470-4840-b6ca-05033eba6274");
            return interval;
        } else {
            writeline("/home/ubuntu/results/coverage/Schedule/Schedule_10_10.coverage", "1a1d7b73-8767-4729-9732-8dbeac1e77d9");
            LOG.debug("no multiple=1 builders found for " + then + ". checking other days");
        }
        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_10_10.coverage", "3d0fda9d-c051-4ae3-a270-ba304b6247b7");
        for (int i = 1; i < 7; i++) {
            writeline("/home/ubuntu/results/coverage/Schedule/Schedule_10_10.coverage", "2015fb20-c203-4d22-a06a-06c22c999075");
            long daysPastInitialInterval = i * ONE_DAY;
            writeline("/home/ubuntu/results/coverage/Schedule/Schedule_10_10.coverage", "29848f6d-0bb7-4f30-b5e0-6ead3a624055");
            then = getFutureDate(now, interval + daysPastInitialInterval);
            writeline("/home/ubuntu/results/coverage/Schedule/Schedule_10_10.coverage", "a3db9f75-61ed-49ed-b681-2d189b37a8a3");
            for (final Builder builder : builders) {
                writeline("/home/ubuntu/results/coverage/Schedule/Schedule_10_10.coverage", "33dd9c17-9709-406a-8f16-939b7acd855a");
                if (builder.isValidDay(then)) {
                    writeline("/home/ubuntu/results/coverage/Schedule/Schedule_10_10.coverage", "96e4a6dd-02c9-4b8e-8fcc-ab27f5000131");
                    LOG.debug("multiple=1 builder found that could run on " + then);
                    writeline("/home/ubuntu/results/coverage/Schedule/Schedule_10_10.coverage", "22ae5d4f-0b5c-425e-9cb9-63837efe5031");
                    long correctionToMidnight = getTimePastMidnight(then);
                    writeline("/home/ubuntu/results/coverage/Schedule/Schedule_10_10.coverage", "a8d596bb-0b9a-4698-bbb0-2a496a7a6b1d");
                    return interval + daysPastInitialInterval - correctionToMidnight;
                }
            }
        }
        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_10_10.coverage", "57a937d2-5700-43be-9915-2b41737dd6a8");
        LOG.error("configuration error? could not find appropriate multiple=1 builder.");
        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_10_10.coverage", "7068b09e-53ae-4a87-be20-a2d4b61fdf3f");
        return interval;
    }

    private long getTimePastMidnight(Date date) {
        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_10_10.coverage", "27c7d14a-12f0-4ae4-a1a1-19c375d3f57f");
        Calendar cal = Calendar.getInstance();
        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_10_10.coverage", "62b66b56-7fa2-4f09-a08f-65065af1d726");
        cal.setTime(date);
        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_10_10.coverage", "fa9bb1ab-c323-46fc-9b70-fc9dcf225cf0");
        long time = 60 * ONE_MINUTE * cal.get(Calendar.HOUR_OF_DAY);
        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_10_10.coverage", "aaf17453-b40b-412b-b827-532aaffc5809");
        time += ONE_MINUTE * cal.get(Calendar.MINUTE);
        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_10_10.coverage", "2c3d3e43-6ee1-4833-a090-eeb719be7b35");
        return time;
    }

    private boolean hasOnlyTimeBuilders() {
        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_10_10.coverage", "fac93db5-ef6a-447d-8555-98b2cb2adc7f");
        boolean onlyTimeBuilders = true;
        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_10_10.coverage", "fa0e28d4-3681-401e-b423-37fc81d22c25");
        for (final Builder builder : builders) {
            writeline("/home/ubuntu/results/coverage/Schedule/Schedule_10_10.coverage", "17fa6a64-a5d4-4b58-ac61-c285f1bba748");
            if (!builder.isTimeBuilder()) {
                writeline("/home/ubuntu/results/coverage/Schedule/Schedule_10_10.coverage", "7170d031-7eef-4f0c-8210-cb367ccb45fa");
                onlyTimeBuilders = false;
                writeline("/home/ubuntu/results/coverage/Schedule/Schedule_10_10.coverage", "b819f6a3-fa58-4225-a210-88cfc20f8dbd");
                break;
            }
        }
        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_10_10.coverage", "20459310-8ecb-44df-a77c-b14f8ca15b80");
        return onlyTimeBuilders;
    }

    long checkTimeBuilders(final Date now, final long proposedTime) {
        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_10_10.coverage", "025f49a5-11ff-4c44-8a11-ea4ce7153773");
        long timeToNextBuild = proposedTime;
        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_10_10.coverage", "018b23a8-43a8-45b2-8388-0445b016395d");
        if (hasOnlyTimeBuilders()) {
            writeline("/home/ubuntu/results/coverage/Schedule/Schedule_10_10.coverage", "b25804bb-8b7b-4b3a-a338-38a6f9cf5fdd");
            timeToNextBuild = Long.MAX_VALUE;
        }
        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_10_10.coverage", "59d3e5d2-3aec-4d4d-92f2-bc91bbaf24ff");
        final int nowTime = DateUtil.getTimeFromDate(now);
        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_10_10.coverage", "48d8ba2a-4245-41e0-b1e4-4f8c99bd144e");
        for (final Builder builder : builders) {
            writeline("/home/ubuntu/results/coverage/Schedule/Schedule_10_10.coverage", "e6933453-3006-4c8f-9ca0-b992ebd4244b");
            if (builder.isTimeBuilder()) {
                writeline("/home/ubuntu/results/coverage/Schedule/Schedule_10_10.coverage", "b6f9046a-30b3-4fbb-ba40-127c9fd6812e");
                long timeToThisBuild = Long.MAX_VALUE;
                writeline("/home/ubuntu/results/coverage/Schedule/Schedule_10_10.coverage", "73bdb25f-6e91-4042-94e2-b74d546a9c74");
                final Calendar cal = Calendar.getInstance();
                writeline("/home/ubuntu/results/coverage/Schedule/Schedule_10_10.coverage", "1241b83a-105e-4d7f-a2ae-fcfce97b1a03");
                final long oneYear = 365;
                writeline("/home/ubuntu/results/coverage/Schedule/Schedule_10_10.coverage", "266b8014-cce1-4b3a-93b4-3166e9c83939");
                for (int daysInTheFuture = 0; daysInTheFuture < oneYear; daysInTheFuture++) {
                    writeline("/home/ubuntu/results/coverage/Schedule/Schedule_10_10.coverage", "d76f0e97-29da-4dec-b807-074f119f6905");
                    cal.setTime(now);
                    writeline("/home/ubuntu/results/coverage/Schedule/Schedule_10_10.coverage", "a6b176b2-7f86-4e30-981a-1e022b70cfc2");
                    cal.add(Calendar.DATE, daysInTheFuture);
                    writeline("/home/ubuntu/results/coverage/Schedule/Schedule_10_10.coverage", "cd619f85-2de0-4b19-b4a6-c5a0b0eacf55");
                    Date future = cal.getTime();
                    writeline("/home/ubuntu/results/coverage/Schedule/Schedule_10_10.coverage", "09fa4c4f-0a41-4017-9952-a297ecf98a55");
                    final boolean dayIsValid = builder.isValidDay(future);
                    writeline("/home/ubuntu/results/coverage/Schedule/Schedule_10_10.coverage", "fcc590da-756b-4b10-86db-c949967d2ade");
                    if (dayIsValid) {
                        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_10_10.coverage", "512df24b-6537-420d-ac44-9f1af2e5d732");
                        int thisBuildTime = builder.getTime();
                        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_10_10.coverage", "d0942780-7826-4a0a-97db-3a7781c84050");
                        boolean timePassedToday = (daysInTheFuture == 0) && (nowTime > thisBuildTime);
                        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_10_10.coverage", "47dbda5b-a569-414f-bc46-d0713fbd49d3");
                        if (!timePassedToday) {
                            writeline("/home/ubuntu/results/coverage/Schedule/Schedule_10_10.coverage", "417fc12d-94ad-4f1d-a8d4-e77897951098");
                            int buildHour = thisBuildTime / 100;
                            writeline("/home/ubuntu/results/coverage/Schedule/Schedule_10_10.coverage", "9a2dd704-f7cf-4f6b-ad81-94c5a5d39802");
                            int buildMinute = thisBuildTime % 100;
                            writeline("/home/ubuntu/results/coverage/Schedule/Schedule_10_10.coverage", "f0c06b1c-56f8-4def-a787-d426e6f4ab7a");
                            cal.set(Calendar.HOUR_OF_DAY, buildHour);
                            writeline("/home/ubuntu/results/coverage/Schedule/Schedule_10_10.coverage", "115267c2-e9cb-44c5-afde-3d0d56a1ef84");
                            cal.set(Calendar.MINUTE, buildMinute);
                            writeline("/home/ubuntu/results/coverage/Schedule/Schedule_10_10.coverage", "bee87299-53ea-43f7-bc4e-59a5ec50784d");
                            future = cal.getTime();
                            writeline("/home/ubuntu/results/coverage/Schedule/Schedule_10_10.coverage", "d63fbcae-9805-436f-a905-6ed0f19775ed");
                            timeToThisBuild = future.getTime() - now.getTime();
                            writeline("/home/ubuntu/results/coverage/Schedule/Schedule_10_10.coverage", "b681a1d3-87e2-40a3-b9fa-5e224f1c1dea");
                            break;
                        }
                    }
                }
                writeline("/home/ubuntu/results/coverage/Schedule/Schedule_10_10.coverage", "563ec442-754f-4df1-821c-c1cc462adefb");
                if (timeToThisBuild < timeToNextBuild) {
                    writeline("/home/ubuntu/results/coverage/Schedule/Schedule_10_10.coverage", "0c3536b1-8818-4848-9e78-6c3b6bf111a1");
                    timeToNextBuild = timeToThisBuild;
                }
            }
        }
        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_10_10.coverage", "92813182-62da-4ee4-9308-931d6922fde2");
        if (timeToNextBuild > MAX_INTERVAL_MILLISECONDS) {
            writeline("/home/ubuntu/results/coverage/Schedule/Schedule_10_10.coverage", "e7708876-3565-4d38-84d4-3d4153b04176");
            LOG.error("checkTimeBuilders exceeding maximum interval. using proposed value [" + proposedTime + "] instead");
            writeline("/home/ubuntu/results/coverage/Schedule/Schedule_10_10.coverage", "8530f8e7-1de4-4dc3-a1e1-380ed133ced7");
            timeToNextBuild = proposedTime;
        }
        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_10_10.coverage", "08c0a5a7-7a16-4a47-9f88-8ec454bc0377");
        return timeToNextBuild;
    }

    long checkPauseBuilders(Date now, long proposedTime) {
        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_10_10.coverage", "09b5b5f5-5e83-4333-8711-2b71b151cff7");
        long oldTime = proposedTime;
        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_10_10.coverage", "79c64270-e8b9-4ccc-8c5d-3fe824806073");
        long newTime = checkForPauseAtProposedTime(now, oldTime);
        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_10_10.coverage", "c4ebbd7a-b71d-4135-a62a-30b5029559b5");
        while (oldTime != newTime) {
            writeline("/home/ubuntu/results/coverage/Schedule/Schedule_10_10.coverage", "62bc308a-217b-4e75-9e5b-8c327a886dcf");
            oldTime = newTime;
            writeline("/home/ubuntu/results/coverage/Schedule/Schedule_10_10.coverage", "4eb37b50-6d1b-435a-936f-28539cd56cec");
            newTime = checkForPauseAtProposedTime(now, oldTime);
        }
        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_10_10.coverage", "711bd275-f5d6-44cb-943b-7204d2dc5330");
        return newTime;
    }

    private long checkForPauseAtProposedTime(Date now, long proposedTime) {
        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_10_10.coverage", "11802e5f-fa38-4633-89e4-c1686f28bc2a");
        Date futureDate = getFutureDate(now, proposedTime);
        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_10_10.coverage", "7427c8d7-8ce6-4827-9dba-62ab2a3c1bae");
        PauseBuilder pause = findPause(futureDate);
        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_10_10.coverage", "cee210e3-8ad3-40dd-ba4b-d1f3c1c8eec9");
        if (pause == null) {
            writeline("/home/ubuntu/results/coverage/Schedule/Schedule_10_10.coverage", "60352731-3321-4191-9895-7cbd9e2bd69d");
            return proposedTime;
        }
        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_10_10.coverage", "40227cb7-49b3-4765-b3ce-87fa87ea1b57");
        int endPause = pause.getEndTime();
        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_10_10.coverage", "7d78bab6-d64c-4e33-8b55-67fef58ba080");
        int futureTime = DateUtil.getTimeFromDate(futureDate);
        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_10_10.coverage", "e5a4fdc8-86ef-4874-bef2-204ca3ea51c3");
        long timeToEndOfPause = proposedTime + DateUtil.milliTimeDifference(futureTime, endPause);
        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_10_10.coverage", "262e2642-5ffc-49d1-93d3-630727e8060b");
        timeToEndOfPause = checkMaximumInterval(timeToEndOfPause);
        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_10_10.coverage", "8b3e4f35-6817-472f-ba57-7262149c0c1f");
        return timeToEndOfPause == MAX_INTERVAL_MILLISECONDS ? timeToEndOfPause : timeToEndOfPause + ONE_MINUTE;
    }

    private long checkMaximumInterval(long timeToEndOfPause) {
        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_10_10.coverage", "22036eac-8945-4a0f-aecb-8e30a23a0835");
        if (timeToEndOfPause > MAX_INTERVAL_MILLISECONDS) {
            writeline("/home/ubuntu/results/coverage/Schedule/Schedule_10_10.coverage", "7b6fae8f-304c-41c5-b935-37f964323fdf");
            LOG.error("maximum interval exceeded! project perpetually paused?");
            writeline("/home/ubuntu/results/coverage/Schedule/Schedule_10_10.coverage", "f311e15d-533e-434a-9df3-340fa42b8f56");
            return MAX_INTERVAL_MILLISECONDS;
        }
        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_10_10.coverage", "6c866e1a-dc17-4b1c-9a7b-e0df7938a3a5");
        return timeToEndOfPause;
    }

    private Date getFutureDate(Date now, long delay) {
        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_10_10.coverage", "5dcb7c30-ef48-463a-b145-e0acfd6b360b");
        long futureMillis = now.getTime() + delay;
        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_10_10.coverage", "8914970c-5b2a-4ea6-83cc-3d8ad41eddfd");
        return new Date(futureMillis);
    }

    public void setInterval(long intervalBetweenModificationChecks) {
        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_10_10.coverage", "e430ff85-8e87-41b4-84e5-9a3ec6ea5ddf");
        if (intervalBetweenModificationChecks <= 0) {
            writeline("/home/ubuntu/results/coverage/Schedule/Schedule_10_10.coverage", "8f13c8b1-93c2-4e3d-ac57-2cff0342ca6e");
            throw new IllegalArgumentException("interval must be greater than zero");
        }
        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_10_10.coverage", "e0f86803-f9f9-4b42-9dd2-de1e59cf6070");
        interval = intervalBetweenModificationChecks * ONE_SECOND;
    }

    public long getInterval() {
        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_10_10.coverage", "dedcf098-e91a-47ac-8349-df9e1eb16644");
        return interval;
    }

    public void setShowProgress(final boolean showProgress) {
        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_10_10.coverage", "9d2bdc1a-81bc-421d-8079-b428e2268e2c");
        this.showProgress = showProgress;
    }

    public boolean getShowProgress() {
        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_10_10.coverage", "03b6a3e9-335e-42c3-af0d-5a27c91249cd");
        return showProgress;
    }

    public void validate() throws CruiseControlException {
        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_10_10.coverage", "741b5f80-8cfd-4374-a68b-bc7631e61098");
        ValidationHelper.assertTrue(builders.size() > 0, "schedule element requires at least one nested builder element");
        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_10_10.coverage", "3523c786-804b-462a-ab75-1ca805f5179a");
        ValidationHelper.assertFalse(interval > ONE_YEAR, "maximum interval value is " + MAX_INTERVAL_SECONDS + " (one year)");
        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_10_10.coverage", "a61bc866-6886-4c99-b619-e888a4f2bd8b");
        if (hasOnlyTimeBuilders()) {
            writeline("/home/ubuntu/results/coverage/Schedule/Schedule_10_10.coverage", "8a6ce2d8-1aea-42b7-8b8f-c1d224cd8365");
            LOG.warn("schedule has all time based builders: interval value will be ignored.");
            writeline("/home/ubuntu/results/coverage/Schedule/Schedule_10_10.coverage", "c93eef1f-0aef-455d-858a-dbf4f4d8e152");
            ValidationHelper.assertFalse(checkWithinPause(new ArrayList<Builder>(builders)), "all build times during pauses.");
        }
        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_10_10.coverage", "b745dec5-c603-4daf-a80d-1955c1f94b18");
        // Validate the child builders, since no one else seems to be doing it.
        for (final Builder next : builders) {
            writeline("/home/ubuntu/results/coverage/Schedule/Schedule_10_10.coverage", "06df90c1-1c7e-41ab-bc44-8bd4a054bc1b");
            next.validate();
        }
    }

    private boolean checkWithinPause(List timeBuilders) {
        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_10_10.coverage", "ee22840c-f442-460f-8d6d-1223f65c6478");
        for (int i = 0; i < timeBuilders.size(); i++) {
            writeline("/home/ubuntu/results/coverage/Schedule/Schedule_10_10.coverage", "2497d848-be9a-465a-b712-f3dc4b57bc44");
            Builder builder = (Builder) timeBuilders.get(i);
            writeline("/home/ubuntu/results/coverage/Schedule/Schedule_10_10.coverage", "fa64cd5b-a62f-4fca-aee3-6ed22517063e");
            for (final PauseBuilder pauseBuilder : pauseBuilders) {
                writeline("/home/ubuntu/results/coverage/Schedule/Schedule_10_10.coverage", "b3b7fa5f-1041-4298-a931-a5134a750bb5");
                if (buildDaySameAsPauseDay(builder, pauseBuilder) && buildTimeWithinPauseTime(builder, pauseBuilder)) {
                    writeline("/home/ubuntu/results/coverage/Schedule/Schedule_10_10.coverage", "f623f49f-1f3c-4566-843c-251a6839369d");
                    timeBuilders.remove(builder);
                    writeline("/home/ubuntu/results/coverage/Schedule/Schedule_10_10.coverage", "ef0cfe65-868f-4348-aecb-851789653432");
                    StringBuffer message = new StringBuffer();
                    writeline("/home/ubuntu/results/coverage/Schedule/Schedule_10_10.coverage", "8078d0a3-bcc0-4e2d-bb7d-c89413f5fa95");
                    message.append("time Builder for time ");
                    writeline("/home/ubuntu/results/coverage/Schedule/Schedule_10_10.coverage", "2fe0e356-dea4-4363-bdf7-4edfed7aefff");
                    message.append(Integer.toString(builder.getTime()));
                    writeline("/home/ubuntu/results/coverage/Schedule/Schedule_10_10.coverage", "19fc694f-17f3-4196-b474-3be56f8eaba2");
                    if (builder.getDay() != Builder.NOT_SET) {
                        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_10_10.coverage", "c7ad7289-2139-4fcc-a744-35f2c3e56e4d");
                        message.append(" and day of ");
                        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_10_10.coverage", "5a12b2c4-8bca-4c6e-bc2a-0725aaca3580");
                        message.append(getDayString(builder.getDay()));
                    }
                    writeline("/home/ubuntu/results/coverage/Schedule/Schedule_10_10.coverage", "9f3ac770-e3e2-44d8-a5b7-49b967ea567e");
                    message.append(" is always within a pause and will never build");
                    writeline("/home/ubuntu/results/coverage/Schedule/Schedule_10_10.coverage", "40627c5d-4dcd-4f86-b736-332de89ef532");
                    LOG.error(message.toString());
                }
            }
        }
        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_10_10.coverage", "c72d9ced-b9b8-4e37-82dd-83fa5c1f7530");
        return timeBuilders.isEmpty();
    }

    /**
     * @param day
     * int value
     * @return english string value
     */
    String getDayString(int day) {
        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_10_10.coverage", "b9d1b660-3e98-4730-b2ca-95b5aa4ac0f9");
        if (day < 1 || day > 7) {
            writeline("/home/ubuntu/results/coverage/Schedule/Schedule_10_10.coverage", "7536738e-3900-49bb-bd07-115789d2bb74");
            throw new IllegalArgumentException("valid values of days are between 1 and 7, was " + day);
        }
        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_10_10.coverage", "4a6e365c-6b4e-4318-b20c-1b9e44562749");
        DateFormatSymbols symbols = new DateFormatSymbols(Locale.ENGLISH);
        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_10_10.coverage", "5c83b4dd-c395-49b3-a3af-923626bd2b71");
        String[] weekdays = symbols.getWeekdays();
        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_10_10.coverage", "e5325fe2-16a5-4b0d-b6e4-c3073dbc54e3");
        return weekdays[day];
    }

    private boolean buildDaySameAsPauseDay(Builder builder, PauseBuilder pauseBuilder) {
        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_10_10.coverage", "3f09cee4-b65c-49a6-ad6d-516f557418f6");
        return pauseBuilder.getDay() == PauseBuilder.NOT_SET || pauseBuilder.getDay() == builder.getDay();
    }

    private boolean buildTimeWithinPauseTime(Builder builder, PauseBuilder pauseBuilder) {
        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_10_10.coverage", "fd291262-498b-4f89-921b-8f3e6a7f22a6");
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
        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_10_10.coverage", "e740683a-36f2-43b5-83c4-df53753cf6e4");
        if (param == null) {
            writeline("/home/ubuntu/results/coverage/Schedule/Schedule_10_10.coverage", "b4af0b16-d792-49b1-b22b-85fc5e25dfd5");
            throw new IllegalArgumentException(paramName + " can't be null");
        }
    }

    public List getBuilders() {
        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_10_10.coverage", "1d7839d7-783f-42a2-ade3-aa4843e639d8");
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
