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
        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_2_10.coverage", "aec3b45f-f826-424f-a545-da8659f8f068");
        checkParamNotNull("builder", builder);
        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_2_10.coverage", "fd5e164d-1a77-42a2-8bb1-61330cb1483e");
        builders.add(builder);
        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_2_10.coverage", "dfe7dd31-37a1-4821-b427-907dc6753ccd");
        Collections.sort(builders, builderComparator);
    }

    public void add(final PauseBuilder pause) {
        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_2_10.coverage", "d49443b1-0663-4ce6-8f30-4ac417c65f1a");
        checkParamNotNull("pauseBuilder", pause);
        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_2_10.coverage", "0830bdef-3a7d-4ca1-91c3-c14b686c1488");
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
        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_2_10.coverage", "fe92d799-ff22-4031-ad66-ce66c26c1c65");
        checkParamNotNull("date", now);
        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_2_10.coverage", "d9596b97-632a-488d-af2c-8aa298fab618");
        PauseBuilder pause = findPause(now);
        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_2_10.coverage", "bdbfe2c2-c810-4908-94e7-d15a35df09dd");
        if (pause != null) {
            writeline("/home/ubuntu/results/coverage/Schedule/Schedule_2_10.coverage", "b37c3649-6fdc-4cad-9def-4920aba0e673");
            LOG.info("CruiseControl is paused until: " + getEndTimeString(pause));
            writeline("/home/ubuntu/results/coverage/Schedule/Schedule_2_10.coverage", "3f3a4976-9c0c-40b3-8007-d62115aa9de2");
            return true;
        }
        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_2_10.coverage", "027abecd-5304-421b-8214-f28b3ce3034e");
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
        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_2_10.coverage", "610c24fc-831a-4084-87cc-232e9dba19a5");
        final Calendar cal = Calendar.getInstance();
        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_2_10.coverage", "4f9d7461-29e5-4737-8ada-809a875e97d3");
        cal.set(Calendar.HOUR_OF_DAY, builder.getEndTime() / 100);
        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_2_10.coverage", "998f9a21-b0af-44d3-96b9-4c4b3dd08484");
        cal.set(Calendar.MINUTE, builder.getEndTime() % 100);
        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_2_10.coverage", "3717b52a-4b84-4f8e-97bc-dad786d292f2");
        cal.add(Calendar.MINUTE, 1);
        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_2_10.coverage", "fd801ce3-2b0d-4e5d-a79e-6cfb7e15db58");
        return timeFormatter.format(cal.getTime());
    }

    PauseBuilder findPause(final Date date) {
        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_2_10.coverage", "95a1ceab-df30-46b7-bfe9-23c551ad979d");
        checkParamNotNull("date", date);
        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_2_10.coverage", "a96d3206-ad92-4a42-a22a-b0d8aeb037fd");
        for (final PauseBuilder pause : pauseBuilders) {
            writeline("/home/ubuntu/results/coverage/Schedule/Schedule_2_10.coverage", "ad662911-8563-40c9-a10a-a0d0e3018b3d");
            if (pause.isPaused(date)) {
                writeline("/home/ubuntu/results/coverage/Schedule/Schedule_2_10.coverage", "8aa156bc-9f49-41cf-9cee-9e903c2fd054");
                return pause;
            }
        }
        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_2_10.coverage", "d6033788-4be7-4ca5-beff-b274830805f8");
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
        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_2_10.coverage", "dcaca5b9-cc91-4f89-a748-d7097de4dc3e");
        final Builder builder = selectBuilder(buildNumber, lastBuild, now);
        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_2_10.coverage", "3485ce11-1970-4d00-b10a-013a185d354e");
        if (buildTarget != null) {
            writeline("/home/ubuntu/results/coverage/Schedule/Schedule_2_10.coverage", "3695c9c5-3cf8-4d98-a5bd-ff3e5e32011b");
            LOG.info("Overriding build target with \"" + buildTarget + "\"");
            writeline("/home/ubuntu/results/coverage/Schedule/Schedule_2_10.coverage", "ff057bae-4705-49ab-b68b-5e1c217e4a57");
            return builder.buildWithTarget(properties, buildTarget, (getShowProgress() ? progress : null));
        }
        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_2_10.coverage", "a4fdd0c6-5be3-4828-b59f-0fbeb82630b1");
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
        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_2_10.coverage", "4a35539b-d677-4c01-985d-dd65699248b8");
        Builder builder = findBuilder(buildNumber, lastBuild, now);
        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_2_10.coverage", "b6fc8e72-6365-4975-b5bf-60ab88b963d5");
        if (builder == null) {
            writeline("/home/ubuntu/results/coverage/Schedule/Schedule_2_10.coverage", "36cfe4f5-062e-45eb-acbf-c7cf4d29d787");
            final long timeToNextBuild = getTimeToNextBuild(now, ONE_MINUTE);
            writeline("/home/ubuntu/results/coverage/Schedule/Schedule_2_10.coverage", "13eeb9cb-84de-450c-bb9f-a3e10e614c19");
            final Date futureDate = getFutureDate(now, timeToNextBuild);
            writeline("/home/ubuntu/results/coverage/Schedule/Schedule_2_10.coverage", "2db186a7-9b28-4616-a2a6-9b613d7bf479");
            builder = findBuilder(buildNumber, now, futureDate);
        }
        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_2_10.coverage", "c8f5e21e-6a11-4646-b0bf-0844921967a8");
        if (builder == null) {
            writeline("/home/ubuntu/results/coverage/Schedule/Schedule_2_10.coverage", "d3dfca08-ab67-4e11-aedd-168392dd6736");
            validate();
            writeline("/home/ubuntu/results/coverage/Schedule/Schedule_2_10.coverage", "b836ba99-9476-4a5b-a007-83bc3df2560a");
            throw new CruiseControlException("configuration error not caught by validate? no builder selected");
        }
        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_2_10.coverage", "7fcf8b76-74ad-4767-b94c-b08af3dcc467");
        return builder;
    }

    private Builder findBuilder(final int buildNumber, final Date lastBuild, final Date now) throws CruiseControlException {
        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_2_10.coverage", "c8c5157f-0aab-477e-a133-0126cc28594e");
        for (final Builder builder : builders) {
            writeline("/home/ubuntu/results/coverage/Schedule/Schedule_2_10.coverage", "b98161ad-6be6-49cd-a432-6e0b48c1bef1");
            if (builder.isTimeBuilder()) {
                writeline("/home/ubuntu/results/coverage/Schedule/Schedule_2_10.coverage", "1cee8836-9dac-484d-96e7-da6ed4661db1");
                final int buildTime = builder.getTime();
                writeline("/home/ubuntu/results/coverage/Schedule/Schedule_2_10.coverage", "593a4249-22ee-49f7-840a-04cd35540e0f");
                final boolean didntBuildToday = builderDidntBuildToday(lastBuild, now, buildTime);
                writeline("/home/ubuntu/results/coverage/Schedule/Schedule_2_10.coverage", "3f0ee45d-ea43-4d97-826f-f3141436a17f");
                final int nowTime = DateUtil.getTimeFromDate(now);
                writeline("/home/ubuntu/results/coverage/Schedule/Schedule_2_10.coverage", "dd59a63a-a17a-4008-ac74-f60a342a610a");
                final boolean isAfterBuildTime = buildTime <= nowTime;
                writeline("/home/ubuntu/results/coverage/Schedule/Schedule_2_10.coverage", "53b54664-ca6d-42aa-94d8-a0cc09528409");
                final boolean isValidDay = builder.isValidDay(now);
                writeline("/home/ubuntu/results/coverage/Schedule/Schedule_2_10.coverage", "e2570e74-2f14-4a73-aedd-b9e0cf96ea7e");
                if (didntBuildToday && isAfterBuildTime && isValidDay) {
                    writeline("/home/ubuntu/results/coverage/Schedule/Schedule_2_10.coverage", "5bf0ca6a-3f15-4b19-b54f-13f80fa0789d");
                    return builder;
                }
            } else if (builder.getMultiple() > 0) {
                writeline("/home/ubuntu/results/coverage/Schedule/Schedule_2_10.coverage", "68c6c31e-8440-4581-9264-b1b50377ef1b");
                if (builder.isValidDay(now)) {
                    writeline("/home/ubuntu/results/coverage/Schedule/Schedule_2_10.coverage", "e151da79-4549-4e6a-b797-1f8824306c49");
                    if ((buildNumber % builder.getMultiple()) == 0) {
                        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_2_10.coverage", "08fcaeca-5e1f-4af3-95df-d464e3401e05");
                        return builder;
                    }
                }
            } else {
                writeline("/home/ubuntu/results/coverage/Schedule/Schedule_2_10.coverage", "f1ce11e7-8c52-40f3-aac6-4b620c8e42aa");
                throw new CruiseControlException("The selected Builder is not properly configured");
            }
        }
        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_2_10.coverage", "5f05e3d4-e286-4d6e-9e48-3a369ce630aa");
        return null;
    }

    boolean builderDidntBuildToday(Date lastBuild, Date now, int buildTime) {
        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_2_10.coverage", "dfc4e0d3-0d62-4cec-9c44-d24cea219970");
        int time = DateUtil.getTimeFromDate(now);
        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_2_10.coverage", "969f341f-d9c8-4c5c-9378-cff369af0848");
        long timeMillis = DateUtil.convertToMillis(time);
        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_2_10.coverage", "ef41cfc5-cd68-4044-ac9d-c841b03dfe5e");
        long startOfToday = now.getTime() - timeMillis;
        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_2_10.coverage", "0bd638c8-e23f-4c2a-a434-f54350fb9056");
        boolean lastBuildYesterday = lastBuild.getTime() < startOfToday;
        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_2_10.coverage", "444e30c3-3364-4c79-b2db-7902994131f0");
        boolean lastBuildTimeBeforeBuildTime = DateUtil.getTimeFromDate(lastBuild) < buildTime;
        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_2_10.coverage", "5e358226-5894-4f18-8dcb-8747a079a3f6");
        return lastBuildYesterday || lastBuildTimeBeforeBuildTime;
    }

    long getTimeToNextBuild(Date now, long sleepInterval) {
        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_2_10.coverage", "74ef1547-2b21-45d6-9486-35ebc60a3091");
        return getTimeToNextBuild(now, sleepInterval, 0);
    }

    private long getTimeToNextBuild(Date now, long sleepInterval, long priorPauseAdjustment) {
        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_2_10.coverage", "17e04dee-02df-4f3f-9171-066f953d2281");
        long timeToNextBuild = sleepInterval;
        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_2_10.coverage", "24516b93-3ba2-475f-a25f-761ad85cbfde");
        LOG.debug("getTimeToNextBuild: initial timeToNextBuild = " + timeToNextBuild);
        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_2_10.coverage", "d4fe5a7b-700d-471e-b783-cd24900ae875");
        timeToNextBuild = checkMultipleBuilders(now, timeToNextBuild);
        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_2_10.coverage", "bf1dba1f-2586-4cca-83ec-efc8ec3c3f78");
        LOG.debug("getTimeToNextBuild: after checkMultipleBuilders = " + timeToNextBuild);
        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_2_10.coverage", "144648bd-7ab5-45fd-a804-3e47156a7b90");
        timeToNextBuild = checkTimeBuilders(now, timeToNextBuild);
        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_2_10.coverage", "8ea8f0f2-5e57-4fd9-a522-27c146813f9f");
        LOG.debug("getTimeToNextBuild: after checkTimeBuilders = " + timeToNextBuild);
        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_2_10.coverage", "6de04e3f-05cc-40f5-a0e2-501cad6583fd");
        long timeTillNotPaused = checkPauseBuilders(now, timeToNextBuild);
        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_2_10.coverage", "544ed13c-cc07-4f32-b05b-95fcd03c3c09");
        LOG.debug("getTimeToNextBuild: after checkPauseBuilders = " + timeToNextBuild);
        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_2_10.coverage", "7c97dc7a-847f-4484-b399-12499085f912");
        if (timeToNextBuild != timeTillNotPaused) {
            writeline("/home/ubuntu/results/coverage/Schedule/Schedule_2_10.coverage", "05cf5c27-c5a6-4acc-8b40-5c9dd0753b4f");
            boolean atMaxTime = timeTillNotPaused >= MAX_INTERVAL_MILLISECONDS || priorPauseAdjustment >= MAX_INTERVAL_MILLISECONDS;
            writeline("/home/ubuntu/results/coverage/Schedule/Schedule_2_10.coverage", "1f7a8191-d883-421d-bbd1-df7ab56e4c36");
            if (hasOnlyTimeBuilders() && !atMaxTime) {
                writeline("/home/ubuntu/results/coverage/Schedule/Schedule_2_10.coverage", "c59c2a6f-c86a-4fa4-8d7c-7dac517241ad");
                Date dateAfterPause = getFutureDate(now, timeTillNotPaused);
                writeline("/home/ubuntu/results/coverage/Schedule/Schedule_2_10.coverage", "2bfa2581-b138-4e15-81d4-500b53424b45");
                long adjustmentFromEndOfPause = getTimeToNextBuild(dateAfterPause, 0, priorPauseAdjustment + timeTillNotPaused);
                writeline("/home/ubuntu/results/coverage/Schedule/Schedule_2_10.coverage", "1a13f0c0-5152-4ad7-a5d0-14de2b4fb465");
                timeToNextBuild = timeTillNotPaused + adjustmentFromEndOfPause;
                writeline("/home/ubuntu/results/coverage/Schedule/Schedule_2_10.coverage", "2cb3cba8-1159-4c43-95a8-14ccef823d76");
                timeToNextBuild = checkMaximumInterval(timeToNextBuild);
            } else {
                writeline("/home/ubuntu/results/coverage/Schedule/Schedule_2_10.coverage", "79a5f551-4946-4b73-bc56-bbd075441307");
                timeToNextBuild = timeTillNotPaused;
            }
        }
        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_2_10.coverage", "4e276b04-eff6-4ce2-a268-e528e9e12a58");
        return timeToNextBuild;
    }

    private long checkMultipleBuilders(final Date now, final long interval) {
        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_2_10.coverage", "f793a0db-bfe9-4cb9-aa4c-587c06633466");
        if (hasOnlyTimeBuilders()) {
            writeline("/home/ubuntu/results/coverage/Schedule/Schedule_2_10.coverage", "46f066a6-fd92-47d2-83d3-1f25563d5377");
            LOG.debug("has only time builders, so no correction for multiple builders.");
            writeline("/home/ubuntu/results/coverage/Schedule/Schedule_2_10.coverage", "fcf06434-e55f-4d7a-86cc-517e0b287cab");
            return interval;
        }
        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_2_10.coverage", "51cb0fde-83ca-4bfe-ba8e-35c62e00aff9");
        Date then = getFutureDate(now, interval);
        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_2_10.coverage", "c6448f81-4efd-477a-a417-49cd3461d6a5");
        final List<Builder> buildersForOtherDays = new ArrayList<Builder>();
        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_2_10.coverage", "8b1e6a12-2e4c-400f-a0f7-038eabbaf6b3");
        for (final Builder builder : builders) {
            writeline("/home/ubuntu/results/coverage/Schedule/Schedule_2_10.coverage", "dd9ef274-5d94-4278-a618-3aecb7138b74");
            if (!builder.isTimeBuilder()) {
                writeline("/home/ubuntu/results/coverage/Schedule/Schedule_2_10.coverage", "4ddd2124-81e2-442e-a5dd-e6ce5a0781cd");
                if (builder.getMultiple() == 1) {
                    writeline("/home/ubuntu/results/coverage/Schedule/Schedule_2_10.coverage", "e208135d-28cc-4943-bec6-90dc1fc4cf65");
                    if (builder.isValidDay(then)) {
                        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_2_10.coverage", "4cf4d06c-7a41-4f92-a9be-a7cf1ed88b78");
                        LOG.debug("multiple=1 builder found that could run on " + then);
                        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_2_10.coverage", "9a003727-a8bb-4c52-9f01-eba63e8195ac");
                        return interval;
                    } else {
                        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_2_10.coverage", "f6e46430-c616-444b-81bc-cc9e2076ff8d");
                        buildersForOtherDays.add(builder);
                    }
                }
            }
        }
        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_2_10.coverage", "ddbef896-472b-493f-803f-0b74cac85b11");
        if (buildersForOtherDays.size() == 0) {
            writeline("/home/ubuntu/results/coverage/Schedule/Schedule_2_10.coverage", "5ed6f8da-f383-4687-afcd-de3303e38f49");
            LOG.error("configuration error: has some multiple builders but no multiple=1 builders found!");
            writeline("/home/ubuntu/results/coverage/Schedule/Schedule_2_10.coverage", "a0ff697c-a519-4293-a996-9e4dfc6c5553");
            return interval;
        } else {
            writeline("/home/ubuntu/results/coverage/Schedule/Schedule_2_10.coverage", "40e0ab06-2e24-4e93-aa37-40b89f0c1379");
            LOG.debug("no multiple=1 builders found for " + then + ". checking other days");
        }
        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_2_10.coverage", "79b2efbc-1152-4e38-b1ed-7055ce4689e9");
        for (int i = 1; i < 7; i++) {
            writeline("/home/ubuntu/results/coverage/Schedule/Schedule_2_10.coverage", "ab404ef0-0aba-400f-835e-e5e689d1774a");
            long daysPastInitialInterval = i * ONE_DAY;
            writeline("/home/ubuntu/results/coverage/Schedule/Schedule_2_10.coverage", "72529f8b-f4c2-4536-931b-3d69bbead50a");
            then = getFutureDate(now, interval + daysPastInitialInterval);
            writeline("/home/ubuntu/results/coverage/Schedule/Schedule_2_10.coverage", "27a72c83-4be2-438e-8eff-0ad3726d0216");
            for (final Builder builder : builders) {
                writeline("/home/ubuntu/results/coverage/Schedule/Schedule_2_10.coverage", "2c602366-be92-43ae-b7b2-7369f6db5314");
                if (builder.isValidDay(then)) {
                    writeline("/home/ubuntu/results/coverage/Schedule/Schedule_2_10.coverage", "b1b8d959-9033-4cfa-9661-a33ebb05225f");
                    LOG.debug("multiple=1 builder found that could run on " + then);
                    writeline("/home/ubuntu/results/coverage/Schedule/Schedule_2_10.coverage", "7fb2fc91-920e-49bf-95e6-7495be59b28e");
                    long correctionToMidnight = getTimePastMidnight(then);
                    writeline("/home/ubuntu/results/coverage/Schedule/Schedule_2_10.coverage", "51847f0d-de9e-4447-9990-2c89ef79da0e");
                    return interval + daysPastInitialInterval - correctionToMidnight;
                }
            }
        }
        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_2_10.coverage", "95be5966-f08a-4b55-a9b3-7b052d49d960");
        LOG.error("configuration error? could not find appropriate multiple=1 builder.");
        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_2_10.coverage", "015bcc3b-0ec9-497e-8602-117f26d2c98c");
        return interval;
    }

    private long getTimePastMidnight(Date date) {
        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_2_10.coverage", "0afe87b5-a07a-4382-b1e6-8c18a56f7d72");
        Calendar cal = Calendar.getInstance();
        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_2_10.coverage", "bd5bdb46-1cbf-4b8b-bfea-037a77ec4ca2");
        cal.setTime(date);
        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_2_10.coverage", "27c0aafc-3f52-45f5-8acf-a20ffa9504bc");
        long time = 60 * ONE_MINUTE * cal.get(Calendar.HOUR_OF_DAY);
        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_2_10.coverage", "05b3297c-123d-4593-9f7f-e0e5388e6b11");
        time += ONE_MINUTE * cal.get(Calendar.MINUTE);
        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_2_10.coverage", "fb9092ca-3487-4a21-a102-a7429c939c18");
        return time;
    }

    private boolean hasOnlyTimeBuilders() {
        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_2_10.coverage", "4fd6ce08-5c62-4fde-b548-75fd1abc11d0");
        boolean onlyTimeBuilders = true;
        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_2_10.coverage", "f3682232-ab3e-44f9-8eff-a2bf5ac9739e");
        for (final Builder builder : builders) {
            writeline("/home/ubuntu/results/coverage/Schedule/Schedule_2_10.coverage", "b20b7283-cb02-4a07-8419-8bce8bf771bd");
            if (!builder.isTimeBuilder()) {
                writeline("/home/ubuntu/results/coverage/Schedule/Schedule_2_10.coverage", "e5720f31-48e8-4a6f-80e7-fec889c47149");
                onlyTimeBuilders = false;
                writeline("/home/ubuntu/results/coverage/Schedule/Schedule_2_10.coverage", "ee6ada98-69a0-423a-88ba-07846adc7664");
                break;
            }
        }
        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_2_10.coverage", "bb2e99df-7971-41dd-93a1-8ff4c093d8a7");
        return onlyTimeBuilders;
    }

    long checkTimeBuilders(final Date now, final long proposedTime) {
        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_2_10.coverage", "c9288f19-2c64-490e-98f2-7687b43e4171");
        long timeToNextBuild = proposedTime;
        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_2_10.coverage", "876f7ebc-a862-4ea5-830d-5a70a9862e4c");
        if (hasOnlyTimeBuilders()) {
            writeline("/home/ubuntu/results/coverage/Schedule/Schedule_2_10.coverage", "32c61de0-3f07-4d14-93f7-38c6d781e8b5");
            timeToNextBuild = Long.MAX_VALUE;
        }
        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_2_10.coverage", "6a431add-31f0-44c4-8caf-44cc9dabce8e");
        final int nowTime = DateUtil.getTimeFromDate(now);
        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_2_10.coverage", "420ec4b3-f12e-43b3-8809-76b9ff2e269c");
        for (final Builder builder : builders) {
            writeline("/home/ubuntu/results/coverage/Schedule/Schedule_2_10.coverage", "4c675c0f-8b8e-44b1-a608-44dbe71076b1");
            if (builder.isTimeBuilder()) {
                writeline("/home/ubuntu/results/coverage/Schedule/Schedule_2_10.coverage", "9485ab7b-1f0d-4483-bcf9-b39eeb4556fe");
                long timeToThisBuild = Long.MAX_VALUE;
                writeline("/home/ubuntu/results/coverage/Schedule/Schedule_2_10.coverage", "95651d7c-b942-406b-aa83-ae1523e52aec");
                final Calendar cal = Calendar.getInstance();
                writeline("/home/ubuntu/results/coverage/Schedule/Schedule_2_10.coverage", "0dd360e6-4578-46fd-96cf-1545bc635d86");
                final long oneYear = 365;
                writeline("/home/ubuntu/results/coverage/Schedule/Schedule_2_10.coverage", "79cc8df3-7689-45d8-bd87-d243b9065204");
                for (int daysInTheFuture = 0; daysInTheFuture < oneYear; daysInTheFuture++) {
                    writeline("/home/ubuntu/results/coverage/Schedule/Schedule_2_10.coverage", "781a7a27-a078-4cd3-b24c-286e0031977b");
                    cal.setTime(now);
                    writeline("/home/ubuntu/results/coverage/Schedule/Schedule_2_10.coverage", "f0761d29-8093-4891-8b34-870800b213d6");
                    cal.add(Calendar.DATE, daysInTheFuture);
                    writeline("/home/ubuntu/results/coverage/Schedule/Schedule_2_10.coverage", "a1ead00e-3d89-4da8-ba43-12f968aff514");
                    Date future = cal.getTime();
                    writeline("/home/ubuntu/results/coverage/Schedule/Schedule_2_10.coverage", "76c186d5-171a-41b0-aada-3b5f372237f0");
                    final boolean dayIsValid = builder.isValidDay(future);
                    writeline("/home/ubuntu/results/coverage/Schedule/Schedule_2_10.coverage", "b50a080c-c69d-46d7-86ea-1c6e498709ba");
                    if (dayIsValid) {
                        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_2_10.coverage", "d5d15abb-f6bc-4f30-a799-5802a3092bbd");
                        int thisBuildTime = builder.getTime();
                        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_2_10.coverage", "f9019270-ff01-4238-8b94-b8a25f8f30bb");
                        boolean timePassedToday = (daysInTheFuture == 0) && (nowTime > thisBuildTime);
                        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_2_10.coverage", "13f4a28d-eeda-4b93-9be7-f6e5600e04c1");
                        if (!timePassedToday) {
                            writeline("/home/ubuntu/results/coverage/Schedule/Schedule_2_10.coverage", "c6b9c229-bb66-4d5a-a047-f23d5911b2ca");
                            int buildHour = thisBuildTime / 100;
                            writeline("/home/ubuntu/results/coverage/Schedule/Schedule_2_10.coverage", "330bd7a3-0a3c-45f1-98d8-202fe5ad20fa");
                            int buildMinute = thisBuildTime % 100;
                            writeline("/home/ubuntu/results/coverage/Schedule/Schedule_2_10.coverage", "91e4f149-c0f2-4789-98c0-6b7cb35e1d40");
                            cal.set(Calendar.HOUR_OF_DAY, buildHour);
                            writeline("/home/ubuntu/results/coverage/Schedule/Schedule_2_10.coverage", "4e7f7595-42f0-4d55-b765-9131a17b5b75");
                            cal.set(Calendar.MINUTE, buildMinute);
                            writeline("/home/ubuntu/results/coverage/Schedule/Schedule_2_10.coverage", "bbf55241-cef2-4050-a2c8-5ce0ceb53635");
                            future = cal.getTime();
                            writeline("/home/ubuntu/results/coverage/Schedule/Schedule_2_10.coverage", "ba51dfa7-1386-43ad-a3a4-db02a4df9edc");
                            timeToThisBuild = future.getTime() - now.getTime();
                            writeline("/home/ubuntu/results/coverage/Schedule/Schedule_2_10.coverage", "e60d0273-9db2-4ff4-8c45-b19c34bcb25d");
                            break;
                        }
                    }
                }
                writeline("/home/ubuntu/results/coverage/Schedule/Schedule_2_10.coverage", "3d7da936-c4ac-4983-9691-8796bfba448a");
                if (timeToThisBuild < timeToNextBuild) {
                    writeline("/home/ubuntu/results/coverage/Schedule/Schedule_2_10.coverage", "2a464d9f-cd4f-4806-a680-c8da1eb407e9");
                    timeToNextBuild = timeToThisBuild;
                }
            }
        }
        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_2_10.coverage", "591713e0-a917-4ae1-9c55-6890aeb5c7d3");
        if (timeToNextBuild > MAX_INTERVAL_MILLISECONDS) {
            writeline("/home/ubuntu/results/coverage/Schedule/Schedule_2_10.coverage", "66892157-2954-48fb-acb1-bd537eb22e27");
            LOG.error("checkTimeBuilders exceeding maximum interval. using proposed value [" + proposedTime + "] instead");
            writeline("/home/ubuntu/results/coverage/Schedule/Schedule_2_10.coverage", "8f06eb46-2595-4398-87a9-c3d7e1e36444");
            timeToNextBuild = proposedTime;
        }
        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_2_10.coverage", "40a288ef-8717-4176-a20b-2e7dd64ea0b9");
        return timeToNextBuild;
    }

    long checkPauseBuilders(Date now, long proposedTime) {
        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_2_10.coverage", "25fc00a4-8236-41d9-86d7-06bb90e0cde4");
        long oldTime = proposedTime;
        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_2_10.coverage", "038d82f4-3926-4885-af18-3d325b092119");
        long newTime = checkForPauseAtProposedTime(now, oldTime);
        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_2_10.coverage", "2a7b1adf-127c-4ed0-8994-f450315c42ea");
        while (oldTime != newTime) {
            writeline("/home/ubuntu/results/coverage/Schedule/Schedule_2_10.coverage", "d33616c3-b565-486b-af1c-9d4fe1163675");
            oldTime = newTime;
            writeline("/home/ubuntu/results/coverage/Schedule/Schedule_2_10.coverage", "34df27c7-ebf4-442a-9160-8d49f1ebb600");
            newTime = checkForPauseAtProposedTime(now, oldTime);
        }
        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_2_10.coverage", "d9e365b4-c974-4a15-9e96-60ca3d722cff");
        return newTime;
    }

    private long checkForPauseAtProposedTime(Date now, long proposedTime) {
        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_2_10.coverage", "71a3b336-621e-4322-b661-8715bc32ba90");
        Date futureDate = getFutureDate(now, proposedTime);
        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_2_10.coverage", "b352c096-cd98-4e20-a44f-82d394b0e3b2");
        PauseBuilder pause = findPause(futureDate);
        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_2_10.coverage", "68a1c477-1365-480d-8c93-7e6462f941c7");
        if (pause == null) {
            writeline("/home/ubuntu/results/coverage/Schedule/Schedule_2_10.coverage", "608c1773-bd76-4d2d-8c30-946ba377640f");
            return proposedTime;
        }
        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_2_10.coverage", "fd6bb9b3-aabe-46b0-aefe-929e5c8317a8");
        int endPause = pause.getEndTime();
        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_2_10.coverage", "ccee08a7-004e-4d5b-8948-46ed814ab0c6");
        int futureTime = DateUtil.getTimeFromDate(futureDate);
        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_2_10.coverage", "f561dd26-4147-472a-a356-091416c30106");
        long timeToEndOfPause = proposedTime + DateUtil.milliTimeDifference(futureTime, endPause);
        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_2_10.coverage", "6aaaf6b1-1a45-4dcf-a7b7-3615489053f8");
        timeToEndOfPause = checkMaximumInterval(timeToEndOfPause);
        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_2_10.coverage", "959236ec-4731-4d5d-a0a9-d82fc15aeaf2");
        return timeToEndOfPause == MAX_INTERVAL_MILLISECONDS ? timeToEndOfPause : timeToEndOfPause + ONE_MINUTE;
    }

    private long checkMaximumInterval(long timeToEndOfPause) {
        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_2_10.coverage", "c0c5d3f7-d97b-429e-90a5-e3d49fc8b03a");
        if (timeToEndOfPause > MAX_INTERVAL_MILLISECONDS) {
            writeline("/home/ubuntu/results/coverage/Schedule/Schedule_2_10.coverage", "9d27e8e3-74a5-4931-b97b-265a7d8cfee8");
            LOG.error("maximum interval exceeded! project perpetually paused?");
            writeline("/home/ubuntu/results/coverage/Schedule/Schedule_2_10.coverage", "d0746ae7-92c2-4dd5-9560-8804e2e13a79");
            return MAX_INTERVAL_MILLISECONDS;
        }
        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_2_10.coverage", "01c4d2f2-00e8-4c41-b4f0-c2beecddbdcb");
        return timeToEndOfPause;
    }

    private Date getFutureDate(Date now, long delay) {
        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_2_10.coverage", "69bb1493-6356-4ed2-b406-74938f90be2f");
        long futureMillis = now.getTime() + delay;
        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_2_10.coverage", "af24ad8e-d48b-44c3-91eb-83875f972eed");
        return new Date(futureMillis);
    }

    public void setInterval(long intervalBetweenModificationChecks) {
        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_2_10.coverage", "98c897fe-c162-450d-953d-6cd2f7bb1428");
        if (intervalBetweenModificationChecks <= 0) {
            writeline("/home/ubuntu/results/coverage/Schedule/Schedule_2_10.coverage", "1c6c9ee9-6c50-4559-b8ff-cbf9ff03dae2");
            throw new IllegalArgumentException("interval must be greater than zero");
        }
        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_2_10.coverage", "8523a209-a5ac-4364-88ea-1562b2476814");
        interval = intervalBetweenModificationChecks * ONE_SECOND;
    }

    public long getInterval() {
        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_2_10.coverage", "9d9ad70c-914b-4997-b9cd-c3e6194ae8dc");
        return interval;
    }

    public void setShowProgress(final boolean showProgress) {
        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_2_10.coverage", "97cbbf89-f2ef-43d7-b7ea-5637daf452ea");
        this.showProgress = showProgress;
    }

    public boolean getShowProgress() {
        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_2_10.coverage", "8c7cf12f-92ea-488d-bd5c-c05cbb33a2eb");
        return showProgress;
    }

    public void validate() throws CruiseControlException {
        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_2_10.coverage", "ac4facff-5521-4905-9e92-37c6cefef84f");
        ValidationHelper.assertTrue(builders.size() > 0, "schedule element requires at least one nested builder element");
        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_2_10.coverage", "7e301408-b4b7-4571-9072-dc34cc72c783");
        ValidationHelper.assertFalse(interval > ONE_YEAR, "maximum interval value is " + MAX_INTERVAL_SECONDS + " (one year)");
        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_2_10.coverage", "438ebecc-3723-4b72-a9cf-716d73cfbe3f");
        if (hasOnlyTimeBuilders()) {
            writeline("/home/ubuntu/results/coverage/Schedule/Schedule_2_10.coverage", "116aad6d-c403-4b9d-9824-0a75d83597b9");
            LOG.warn("schedule has all time based builders: interval value will be ignored.");
            writeline("/home/ubuntu/results/coverage/Schedule/Schedule_2_10.coverage", "3b70acc8-c08a-4d24-aaa7-87f32bda9b29");
            ValidationHelper.assertFalse(checkWithinPause(new ArrayList<Builder>(builders)), "all build times during pauses.");
        }
        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_2_10.coverage", "14850137-24d0-4513-a6b5-ae67ad99feb6");
        // Validate the child builders, since no one else seems to be doing it.
        for (final Builder next : builders) {
            writeline("/home/ubuntu/results/coverage/Schedule/Schedule_2_10.coverage", "166b9797-1a84-4641-a601-bd1bd0f1af99");
            next.validate();
        }
    }

    private boolean checkWithinPause(List timeBuilders) {
        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_2_10.coverage", "c4d3b70e-e531-4797-b826-aecca4c942e9");
        for (int i = 0; i < timeBuilders.size(); i++) {
            writeline("/home/ubuntu/results/coverage/Schedule/Schedule_2_10.coverage", "fd0f5c2e-8829-4b08-9b83-d3351cb15eac");
            Builder builder = (Builder) timeBuilders.get(i);
            writeline("/home/ubuntu/results/coverage/Schedule/Schedule_2_10.coverage", "61989792-576c-4b0f-b2a0-42eec79148fe");
            for (final PauseBuilder pauseBuilder : pauseBuilders) {
                writeline("/home/ubuntu/results/coverage/Schedule/Schedule_2_10.coverage", "46d33305-8fa0-4cfb-b9cc-fdd4160ccb94");
                if (buildDaySameAsPauseDay(builder, pauseBuilder) && buildTimeWithinPauseTime(builder, pauseBuilder)) {
                    writeline("/home/ubuntu/results/coverage/Schedule/Schedule_2_10.coverage", "a2711312-bfd0-4da7-a72b-721910dd1f24");
                    timeBuilders.remove(builder);
                    writeline("/home/ubuntu/results/coverage/Schedule/Schedule_2_10.coverage", "463f20fc-e044-46ca-847b-7db472172f86");
                    StringBuffer message = new StringBuffer();
                    writeline("/home/ubuntu/results/coverage/Schedule/Schedule_2_10.coverage", "a96dfd64-8935-4c6b-8bc7-a331c70e81c2");
                    message.append("time Builder for time ");
                    writeline("/home/ubuntu/results/coverage/Schedule/Schedule_2_10.coverage", "7defa74a-6745-4b6a-8a23-27ed014afc45");
                    message.append(Integer.toString(builder.getTime()));
                    writeline("/home/ubuntu/results/coverage/Schedule/Schedule_2_10.coverage", "d10b42df-0739-4daf-b470-81991b07b7e7");
                    if (builder.getDay() != Builder.NOT_SET) {
                        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_2_10.coverage", "1490ad06-40cd-4582-9382-abd3d31298b5");
                        message.append(" and day of ");
                        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_2_10.coverage", "b1657c09-3a2a-4e85-b84b-ca7d5f149d26");
                        message.append(getDayString(builder.getDay()));
                    }
                    writeline("/home/ubuntu/results/coverage/Schedule/Schedule_2_10.coverage", "d21f2382-dcf7-4eca-aae1-ffeac29ebc80");
                    message.append(" is always within a pause and will never build");
                    writeline("/home/ubuntu/results/coverage/Schedule/Schedule_2_10.coverage", "72bfbbfe-a94f-4438-9f99-fc47407c1372");
                    LOG.error(message.toString());
                }
            }
        }
        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_2_10.coverage", "b7cac8d7-7767-4800-978b-606cea744cea");
        return timeBuilders.isEmpty();
    }

    /**
     * @param day
     * int value
     * @return english string value
     */
    String getDayString(int day) {
        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_2_10.coverage", "0048c361-842c-41cc-a170-f707498136a4");
        if (day < 1 || day > 7) {
            writeline("/home/ubuntu/results/coverage/Schedule/Schedule_2_10.coverage", "347ca5df-457a-4cc6-8ef0-564d91865a93");
            throw new IllegalArgumentException("valid values of days are between 1 and 7, was " + day);
        }
        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_2_10.coverage", "21aae984-7255-4c8b-b490-d07569c0055e");
        DateFormatSymbols symbols = new DateFormatSymbols(Locale.ENGLISH);
        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_2_10.coverage", "0b1e1253-8c62-4237-abdd-8a0df93d1c82");
        String[] weekdays = symbols.getWeekdays();
        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_2_10.coverage", "3f47eaa1-0364-4478-933f-93a8062187b6");
        return weekdays[day];
    }

    private boolean buildDaySameAsPauseDay(Builder builder, PauseBuilder pauseBuilder) {
        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_2_10.coverage", "d6d40999-0f36-42df-a04b-b7396ec801f7");
        return pauseBuilder.getDay() == PauseBuilder.NOT_SET || pauseBuilder.getDay() == builder.getDay();
    }

    private boolean buildTimeWithinPauseTime(Builder builder, PauseBuilder pauseBuilder) {
        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_2_10.coverage", "e2556b3e-4e71-4128-bab7-1998dbdbd0b9");
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
        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_2_10.coverage", "cf29cddc-2739-48be-913b-2119021803fd");
        if (param == null) {
            writeline("/home/ubuntu/results/coverage/Schedule/Schedule_2_10.coverage", "11659ba7-49dd-4896-bc7f-4e7a0e9546f5");
            throw new IllegalArgumentException(paramName + " can't be null");
        }
    }

    public List getBuilders() {
        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_2_10.coverage", "56cca25a-7a2a-4da4-94bf-ed45845c679a");
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
