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
        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_9_10.coverage", "1a3eab7b-7d6b-403d-a6cc-9cbe0dd3a03c");
        checkParamNotNull("builder", builder);
        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_9_10.coverage", "0399c9d2-51ea-48ff-ab29-688ffcceeee5");
        builders.add(builder);
        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_9_10.coverage", "b80d272c-c6b6-4aa3-b748-94cbd3e84fb0");
        Collections.sort(builders, builderComparator);
    }

    public void add(final PauseBuilder pause) {
        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_9_10.coverage", "7e88d1ce-5f6f-4fa0-8a7c-d26a2a870d79");
        checkParamNotNull("pauseBuilder", pause);
        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_9_10.coverage", "8188c797-f82f-4839-adab-bee301abf419");
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
        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_9_10.coverage", "c3cb0828-3038-4bcf-94ce-9b90ef231497");
        checkParamNotNull("date", now);
        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_9_10.coverage", "743e86ba-7859-4769-a8e3-e31a69707164");
        PauseBuilder pause = findPause(now);
        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_9_10.coverage", "c84da086-55c9-44f4-82f0-0c07ef737a4d");
        if (pause != null) {
            writeline("/home/ubuntu/results/coverage/Schedule/Schedule_9_10.coverage", "14398a1b-9d99-4eee-823c-a4c80dd81e42");
            LOG.info("CruiseControl is paused until: " + getEndTimeString(pause));
            writeline("/home/ubuntu/results/coverage/Schedule/Schedule_9_10.coverage", "fbfbcc75-14e2-420a-87a0-433e77204d64");
            return true;
        }
        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_9_10.coverage", "80e38fca-2305-4962-928f-f2cbc6f3dbf8");
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
        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_9_10.coverage", "75b8c241-e85f-4f4f-853c-3433be630ba4");
        final Calendar cal = Calendar.getInstance();
        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_9_10.coverage", "4805bb02-7431-4251-9f1a-8b00a46f86f1");
        cal.set(Calendar.HOUR_OF_DAY, builder.getEndTime() / 100);
        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_9_10.coverage", "fb65d9ca-82fc-4d0c-b80b-b1dc06b123fe");
        cal.set(Calendar.MINUTE, builder.getEndTime() % 100);
        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_9_10.coverage", "8eda990e-9959-4a86-a8d5-bc27069d875f");
        cal.add(Calendar.MINUTE, 1);
        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_9_10.coverage", "95865a87-cb66-4b7b-80f0-f8aaf50ce4e4");
        return timeFormatter.format(cal.getTime());
    }

    PauseBuilder findPause(final Date date) {
        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_9_10.coverage", "411ec440-c63d-4e96-84f2-227a06368ab7");
        checkParamNotNull("date", date);
        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_9_10.coverage", "660445d9-5594-40a3-b0ce-06e26865547d");
        for (final PauseBuilder pause : pauseBuilders) {
            writeline("/home/ubuntu/results/coverage/Schedule/Schedule_9_10.coverage", "4ffa9fb5-f15b-482c-93d5-65d273165131");
            if (pause.isPaused(date)) {
                writeline("/home/ubuntu/results/coverage/Schedule/Schedule_9_10.coverage", "5f404a9d-ed92-4469-a066-ebf4549bd77c");
                return pause;
            }
        }
        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_9_10.coverage", "60d646f3-5fa4-4438-9461-c2c68ccf722e");
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
        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_9_10.coverage", "8fb87ef3-2751-4a57-8d40-a2d7e1ba1644");
        final Builder builder = selectBuilder(buildNumber, lastBuild, now);
        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_9_10.coverage", "3e452d4a-ce55-4fab-b78a-c3d1d3d1475d");
        if (buildTarget != null) {
            writeline("/home/ubuntu/results/coverage/Schedule/Schedule_9_10.coverage", "50382972-87fb-4abf-8989-349284c63da4");
            LOG.info("Overriding build target with \"" + buildTarget + "\"");
            writeline("/home/ubuntu/results/coverage/Schedule/Schedule_9_10.coverage", "e88cbd44-c4ba-4500-9006-1e47b2c992b2");
            return builder.buildWithTarget(properties, buildTarget, (getShowProgress() ? progress : null));
        }
        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_9_10.coverage", "b13e4569-830e-42f2-b2a3-267574e45ff2");
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
        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_9_10.coverage", "0cbcc723-16cc-4479-9196-5335ba709966");
        Builder builder = findBuilder(buildNumber, lastBuild, now);
        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_9_10.coverage", "5b0ad4d6-b0df-4d0f-bc37-266a1d2a994b");
        if (builder == null) {
            writeline("/home/ubuntu/results/coverage/Schedule/Schedule_9_10.coverage", "75db7c80-3d03-4c2b-90de-9adb4457cd9d");
            final long timeToNextBuild = getTimeToNextBuild(now, ONE_MINUTE);
            writeline("/home/ubuntu/results/coverage/Schedule/Schedule_9_10.coverage", "79272296-229d-4d4a-8558-fc9ec684157b");
            final Date futureDate = getFutureDate(now, timeToNextBuild);
            writeline("/home/ubuntu/results/coverage/Schedule/Schedule_9_10.coverage", "3d491eee-7d1b-4800-8776-42d7542407f2");
            builder = findBuilder(buildNumber, now, futureDate);
        }
        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_9_10.coverage", "20705000-06dd-4709-91e8-1e5071e4ba57");
        if (builder == null) {
            writeline("/home/ubuntu/results/coverage/Schedule/Schedule_9_10.coverage", "bb9f1998-24e1-4f59-ad67-0a31b8f0e110");
            validate();
            writeline("/home/ubuntu/results/coverage/Schedule/Schedule_9_10.coverage", "fc6b3ab3-dcf4-44b3-a415-4acf93bbe700");
            throw new CruiseControlException("configuration error not caught by validate? no builder selected");
        }
        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_9_10.coverage", "39a96867-8b31-4d73-a02b-fb81ef618ada");
        return builder;
    }

    private Builder findBuilder(final int buildNumber, final Date lastBuild, final Date now) throws CruiseControlException {
        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_9_10.coverage", "68efff65-421c-40df-a2bd-3c10fb7bea1e");
        for (final Builder builder : builders) {
            writeline("/home/ubuntu/results/coverage/Schedule/Schedule_9_10.coverage", "b3f5d4f7-1d48-4f65-92af-f72fcb92f514");
            if (builder.isTimeBuilder()) {
                writeline("/home/ubuntu/results/coverage/Schedule/Schedule_9_10.coverage", "755a52f0-62b9-4fa8-bdca-42e9bb35bd81");
                final int buildTime = builder.getTime();
                writeline("/home/ubuntu/results/coverage/Schedule/Schedule_9_10.coverage", "9f400259-be95-4923-b5cc-ec4fc567a0ca");
                final boolean didntBuildToday = builderDidntBuildToday(lastBuild, now, buildTime);
                writeline("/home/ubuntu/results/coverage/Schedule/Schedule_9_10.coverage", "69b43ee5-9def-49af-a7d3-c6a48c3ac15a");
                final int nowTime = DateUtil.getTimeFromDate(now);
                writeline("/home/ubuntu/results/coverage/Schedule/Schedule_9_10.coverage", "132e5bc7-3056-463e-b1bf-073044fece8d");
                final boolean isAfterBuildTime = buildTime <= nowTime;
                writeline("/home/ubuntu/results/coverage/Schedule/Schedule_9_10.coverage", "29d81dfd-7597-46be-b8ab-2e264342b648");
                final boolean isValidDay = builder.isValidDay(now);
                writeline("/home/ubuntu/results/coverage/Schedule/Schedule_9_10.coverage", "efffce0a-fd60-4173-962e-659751cb0846");
                if (didntBuildToday && isAfterBuildTime && isValidDay) {
                    writeline("/home/ubuntu/results/coverage/Schedule/Schedule_9_10.coverage", "43be23ce-3725-44da-b9ae-1874e9d9a7f3");
                    return builder;
                }
            } else if (builder.getMultiple() > 0) {
                writeline("/home/ubuntu/results/coverage/Schedule/Schedule_9_10.coverage", "b3900eb9-bf45-4e5d-b7e0-f0e9f37445ca");
                if (builder.isValidDay(now)) {
                    writeline("/home/ubuntu/results/coverage/Schedule/Schedule_9_10.coverage", "2792bf81-8aca-48f1-8860-56738422755e");
                    if ((buildNumber % builder.getMultiple()) == 0) {
                        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_9_10.coverage", "4003b098-6c67-4ae8-b3f3-5e50c5467a86");
                        return builder;
                    }
                }
            } else {
                writeline("/home/ubuntu/results/coverage/Schedule/Schedule_9_10.coverage", "a6f1c5f3-c888-4062-be85-3a739140f828");
                throw new CruiseControlException("The selected Builder is not properly configured");
            }
        }
        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_9_10.coverage", "38d76f4c-79d1-409a-a361-573392b5323c");
        return null;
    }

    boolean builderDidntBuildToday(Date lastBuild, Date now, int buildTime) {
        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_9_10.coverage", "56148c0a-b6f3-4305-b949-a1c883cafe91");
        int time = DateUtil.getTimeFromDate(now);
        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_9_10.coverage", "47ca9e73-8f52-46fb-beb0-d51003d86bb5");
        long timeMillis = DateUtil.convertToMillis(time);
        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_9_10.coverage", "214a6560-cd0f-4491-9762-1bf30d60d6ca");
        long startOfToday = now.getTime() - timeMillis;
        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_9_10.coverage", "dd2a57a9-aef3-4812-9b56-3e201a4f369b");
        boolean lastBuildYesterday = lastBuild.getTime() < startOfToday;
        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_9_10.coverage", "7094168b-f28c-442a-a245-5c80ff500b6a");
        boolean lastBuildTimeBeforeBuildTime = DateUtil.getTimeFromDate(lastBuild) < buildTime;
        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_9_10.coverage", "0bdbb912-8966-4895-a164-e3e480a66319");
        return lastBuildYesterday || lastBuildTimeBeforeBuildTime;
    }

    long getTimeToNextBuild(Date now, long sleepInterval) {
        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_9_10.coverage", "f1b03297-b457-442d-be6d-691e586d6a42");
        return getTimeToNextBuild(now, sleepInterval, 0);
    }

    private long getTimeToNextBuild(Date now, long sleepInterval, long priorPauseAdjustment) {
        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_9_10.coverage", "b7d7cdd5-cfcb-44f0-8478-0e04366a9c92");
        long timeToNextBuild = sleepInterval;
        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_9_10.coverage", "1faef46a-4538-4164-8910-9c86b7a6db0f");
        LOG.debug("getTimeToNextBuild: initial timeToNextBuild = " + timeToNextBuild);
        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_9_10.coverage", "2097f7dd-b474-488e-a2c7-cb5244dc5ea6");
        timeToNextBuild = checkMultipleBuilders(now, timeToNextBuild);
        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_9_10.coverage", "4318ef78-afa7-45f2-9532-68994811ebb3");
        LOG.debug("getTimeToNextBuild: after checkMultipleBuilders = " + timeToNextBuild);
        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_9_10.coverage", "72dbb8b9-1937-46a4-abee-bb8df8a42a9d");
        timeToNextBuild = checkTimeBuilders(now, timeToNextBuild);
        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_9_10.coverage", "11c3e369-c97b-410b-bddf-025425645b75");
        LOG.debug("getTimeToNextBuild: after checkTimeBuilders = " + timeToNextBuild);
        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_9_10.coverage", "654cf28b-4787-464b-984d-1564fe335834");
        long timeTillNotPaused = checkPauseBuilders(now, timeToNextBuild);
        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_9_10.coverage", "92bc2a78-9c37-4dc6-bb68-1165023a38e6");
        LOG.debug("getTimeToNextBuild: after checkPauseBuilders = " + timeToNextBuild);
        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_9_10.coverage", "cebdf830-c0e8-4d1a-9866-2a270f8f2586");
        if (timeToNextBuild != timeTillNotPaused) {
            writeline("/home/ubuntu/results/coverage/Schedule/Schedule_9_10.coverage", "b979b5f5-9b86-46da-94c0-7b0f7586862f");
            boolean atMaxTime = timeTillNotPaused >= MAX_INTERVAL_MILLISECONDS || priorPauseAdjustment >= MAX_INTERVAL_MILLISECONDS;
            writeline("/home/ubuntu/results/coverage/Schedule/Schedule_9_10.coverage", "74156797-d752-46ef-9bee-991e0d3b0e4e");
            if (hasOnlyTimeBuilders() && !atMaxTime) {
                writeline("/home/ubuntu/results/coverage/Schedule/Schedule_9_10.coverage", "045c79bc-0d5e-4ef3-9dfe-7331773c0f78");
                Date dateAfterPause = getFutureDate(now, timeTillNotPaused);
                writeline("/home/ubuntu/results/coverage/Schedule/Schedule_9_10.coverage", "99049d47-18fa-437f-82b2-c8c8bf0d832a");
                long adjustmentFromEndOfPause = getTimeToNextBuild(dateAfterPause, 0, priorPauseAdjustment + timeTillNotPaused);
                writeline("/home/ubuntu/results/coverage/Schedule/Schedule_9_10.coverage", "f01115de-1049-498c-a612-b01f0ada0aeb");
                timeToNextBuild = timeTillNotPaused + adjustmentFromEndOfPause;
                writeline("/home/ubuntu/results/coverage/Schedule/Schedule_9_10.coverage", "fe4a8801-6ce5-4d1e-997c-370e1866f476");
                timeToNextBuild = checkMaximumInterval(timeToNextBuild);
            } else {
                writeline("/home/ubuntu/results/coverage/Schedule/Schedule_9_10.coverage", "f69640a3-b151-49fd-8380-0c7fc80d84c6");
                timeToNextBuild = timeTillNotPaused;
            }
        }
        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_9_10.coverage", "3f48d537-ce1a-42c2-8a95-64dca3fd4f86");
        return timeToNextBuild;
    }

    private long checkMultipleBuilders(final Date now, final long interval) {
        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_9_10.coverage", "6358c70c-b83e-42ab-8abf-5aac65c18d24");
        if (hasOnlyTimeBuilders()) {
            writeline("/home/ubuntu/results/coverage/Schedule/Schedule_9_10.coverage", "b0e0dda6-e264-4cb7-a48a-86fc21c96587");
            LOG.debug("has only time builders, so no correction for multiple builders.");
            writeline("/home/ubuntu/results/coverage/Schedule/Schedule_9_10.coverage", "b0fbddce-edde-4d25-95e1-c3622dcace2d");
            return interval;
        }
        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_9_10.coverage", "d172a456-f5c2-4dc5-9d0a-bed497894f23");
        Date then = getFutureDate(now, interval);
        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_9_10.coverage", "b9db176b-b92f-479a-b15d-04c66824632a");
        final List<Builder> buildersForOtherDays = new ArrayList<Builder>();
        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_9_10.coverage", "4c15aada-5e00-4e5f-88de-417c676acc58");
        for (final Builder builder : builders) {
            writeline("/home/ubuntu/results/coverage/Schedule/Schedule_9_10.coverage", "64f260bc-ba3f-4c97-a4ec-f9ce6b8b7f72");
            if (!builder.isTimeBuilder()) {
                writeline("/home/ubuntu/results/coverage/Schedule/Schedule_9_10.coverage", "00ee0e04-c363-4464-b22a-139cd0f69f0c");
                if (builder.getMultiple() == 1) {
                    writeline("/home/ubuntu/results/coverage/Schedule/Schedule_9_10.coverage", "cb8ca4ef-d15a-4e2c-a92c-23c28848d645");
                    if (builder.isValidDay(then)) {
                        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_9_10.coverage", "23a542a0-f958-4343-872a-de9618bde602");
                        LOG.debug("multiple=1 builder found that could run on " + then);
                        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_9_10.coverage", "fdf6dd34-abcf-4df1-b5f0-7b33b313ad6f");
                        return interval;
                    } else {
                        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_9_10.coverage", "771e2a74-63b1-46e0-ac75-f3337b62b542");
                        buildersForOtherDays.add(builder);
                    }
                }
            }
        }
        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_9_10.coverage", "9e6ede73-aa04-4497-9058-4ec576dcde51");
        if (buildersForOtherDays.size() == 0) {
            writeline("/home/ubuntu/results/coverage/Schedule/Schedule_9_10.coverage", "e05fceac-57a1-4bf1-8776-8d7ff79f9d7b");
            LOG.error("configuration error: has some multiple builders but no multiple=1 builders found!");
            writeline("/home/ubuntu/results/coverage/Schedule/Schedule_9_10.coverage", "9c0dd675-85ed-433e-8e72-0ee68a4ba42b");
            return interval;
        } else {
            writeline("/home/ubuntu/results/coverage/Schedule/Schedule_9_10.coverage", "82c02b9a-d1d4-44d7-b6a8-98601d06987d");
            LOG.debug("no multiple=1 builders found for " + then + ". checking other days");
        }
        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_9_10.coverage", "bfa20bf8-dbb2-4afd-9411-7fe45858fdea");
        for (int i = 1; i < 7; i++) {
            writeline("/home/ubuntu/results/coverage/Schedule/Schedule_9_10.coverage", "544a8f2e-5f4e-417e-a618-e801f8beba72");
            long daysPastInitialInterval = i * ONE_DAY;
            writeline("/home/ubuntu/results/coverage/Schedule/Schedule_9_10.coverage", "ff0c06cf-5992-422b-954d-cdd9e4a1bc5b");
            then = getFutureDate(now, interval + daysPastInitialInterval);
            writeline("/home/ubuntu/results/coverage/Schedule/Schedule_9_10.coverage", "8a196496-9008-489c-96da-1a49a72c7d34");
            for (final Builder builder : builders) {
                writeline("/home/ubuntu/results/coverage/Schedule/Schedule_9_10.coverage", "36da35d8-fdd1-4c45-a3a6-f64d98f2ee29");
                if (builder.isValidDay(then)) {
                    writeline("/home/ubuntu/results/coverage/Schedule/Schedule_9_10.coverage", "d3fb0132-a3c2-41e1-9e14-5454cb0f86ce");
                    LOG.debug("multiple=1 builder found that could run on " + then);
                    writeline("/home/ubuntu/results/coverage/Schedule/Schedule_9_10.coverage", "6b5b2416-6793-4959-a031-6db3bc336709");
                    long correctionToMidnight = getTimePastMidnight(then);
                    writeline("/home/ubuntu/results/coverage/Schedule/Schedule_9_10.coverage", "6e532e9e-7e10-4af3-aafc-59dd52ba6213");
                    return interval + daysPastInitialInterval - correctionToMidnight;
                }
            }
        }
        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_9_10.coverage", "563bc520-e037-41a5-a88b-95dbd2291d48");
        LOG.error("configuration error? could not find appropriate multiple=1 builder.");
        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_9_10.coverage", "f27ad907-6265-49d4-9a55-f51c103e29e7");
        return interval;
    }

    private long getTimePastMidnight(Date date) {
        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_9_10.coverage", "621c1a08-8c28-48f9-92c1-d3936c2f3573");
        Calendar cal = Calendar.getInstance();
        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_9_10.coverage", "d0ac7b35-4b23-466d-9ce2-897cccde4a29");
        cal.setTime(date);
        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_9_10.coverage", "ce6edb87-4e72-4b4b-83ba-f5c1ab3378c2");
        long time = 60 * ONE_MINUTE * cal.get(Calendar.HOUR_OF_DAY);
        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_9_10.coverage", "bb9b16f6-6655-4acc-b1cc-e50e10e7359d");
        time += ONE_MINUTE * cal.get(Calendar.MINUTE);
        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_9_10.coverage", "3c91d39e-913c-41ba-884c-29aeba0a6d28");
        return time;
    }

    private boolean hasOnlyTimeBuilders() {
        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_9_10.coverage", "ee3575f7-a344-4be6-8b66-df88cf2a7e1f");
        boolean onlyTimeBuilders = true;
        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_9_10.coverage", "22fb6bee-0b77-4dda-a9bc-bf9330d21836");
        for (final Builder builder : builders) {
            writeline("/home/ubuntu/results/coverage/Schedule/Schedule_9_10.coverage", "1edd6ae1-f7a0-4275-928c-3b4bda97f842");
            if (!builder.isTimeBuilder()) {
                writeline("/home/ubuntu/results/coverage/Schedule/Schedule_9_10.coverage", "a20f0ee4-4562-482f-bfb3-5e6b9dbdf47f");
                onlyTimeBuilders = false;
                writeline("/home/ubuntu/results/coverage/Schedule/Schedule_9_10.coverage", "ed7169b6-b83f-42c2-9a69-e704d507da4d");
                break;
            }
        }
        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_9_10.coverage", "fe965edb-36fa-43cd-9886-b509cf0bcb26");
        return onlyTimeBuilders;
    }

    long checkTimeBuilders(final Date now, final long proposedTime) {
        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_9_10.coverage", "d89609e7-5542-429f-a914-007900d3ade7");
        long timeToNextBuild = proposedTime;
        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_9_10.coverage", "026d5f50-0298-4e00-bc48-f3b8e5d1e629");
        if (hasOnlyTimeBuilders()) {
            writeline("/home/ubuntu/results/coverage/Schedule/Schedule_9_10.coverage", "f4cde74c-5047-427b-8f30-e76644451cce");
            timeToNextBuild = Long.MAX_VALUE;
        }
        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_9_10.coverage", "09fe13a9-e5f5-49fc-a1f1-e1155d7a3172");
        final int nowTime = DateUtil.getTimeFromDate(now);
        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_9_10.coverage", "d89ddf3e-4cb3-4f58-ad39-cd5c1c0221a8");
        for (final Builder builder : builders) {
            writeline("/home/ubuntu/results/coverage/Schedule/Schedule_9_10.coverage", "bc62ae85-bc45-4559-a92e-8e6894d4305d");
            if (builder.isTimeBuilder()) {
                writeline("/home/ubuntu/results/coverage/Schedule/Schedule_9_10.coverage", "588a5b0b-9813-496a-92d5-abc5a3be3eae");
                long timeToThisBuild = Long.MAX_VALUE;
                writeline("/home/ubuntu/results/coverage/Schedule/Schedule_9_10.coverage", "47631458-eba7-456c-af16-f543f00eca22");
                final Calendar cal = Calendar.getInstance();
                writeline("/home/ubuntu/results/coverage/Schedule/Schedule_9_10.coverage", "b2147dc1-286f-45f9-b281-af866f1a658c");
                final long oneYear = 365;
                writeline("/home/ubuntu/results/coverage/Schedule/Schedule_9_10.coverage", "ef764534-530b-41f8-ac71-db8f26b11a41");
                for (int daysInTheFuture = 0; daysInTheFuture < oneYear; daysInTheFuture++) {
                    writeline("/home/ubuntu/results/coverage/Schedule/Schedule_9_10.coverage", "82c6ede6-d61e-491d-aea2-7e22cfeec043");
                    cal.setTime(now);
                    writeline("/home/ubuntu/results/coverage/Schedule/Schedule_9_10.coverage", "8d36499e-aba5-44e1-ba6b-e465c20bfc99");
                    cal.add(Calendar.DATE, daysInTheFuture);
                    writeline("/home/ubuntu/results/coverage/Schedule/Schedule_9_10.coverage", "40b61027-a2bf-43cb-9aa7-280fb12c2888");
                    Date future = cal.getTime();
                    writeline("/home/ubuntu/results/coverage/Schedule/Schedule_9_10.coverage", "ca3242a4-1828-439b-982a-cab73e28e1af");
                    final boolean dayIsValid = builder.isValidDay(future);
                    writeline("/home/ubuntu/results/coverage/Schedule/Schedule_9_10.coverage", "ad8db103-b9ed-4013-ad53-d743d5c73da0");
                    if (dayIsValid) {
                        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_9_10.coverage", "52006602-8763-4601-a9c0-7fdc72ee04f8");
                        int thisBuildTime = builder.getTime();
                        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_9_10.coverage", "8efd65ed-6c64-4c15-9188-27b011ecd01f");
                        boolean timePassedToday = (daysInTheFuture == 0) && (nowTime > thisBuildTime);
                        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_9_10.coverage", "f02197e0-0558-482c-8c09-d8821140d362");
                        if (!timePassedToday) {
                            writeline("/home/ubuntu/results/coverage/Schedule/Schedule_9_10.coverage", "79b1a0af-2754-4501-95ac-dfadf723eee7");
                            int buildHour = thisBuildTime / 100;
                            writeline("/home/ubuntu/results/coverage/Schedule/Schedule_9_10.coverage", "bc431528-3983-4381-8b7d-9ba845c19568");
                            int buildMinute = thisBuildTime % 100;
                            writeline("/home/ubuntu/results/coverage/Schedule/Schedule_9_10.coverage", "03f6cf4f-c20d-45ca-97f6-e1398bfb964d");
                            cal.set(Calendar.HOUR_OF_DAY, buildHour);
                            writeline("/home/ubuntu/results/coverage/Schedule/Schedule_9_10.coverage", "0d5b5870-0454-4b91-8307-a3cc7f5312ad");
                            cal.set(Calendar.MINUTE, buildMinute);
                            writeline("/home/ubuntu/results/coverage/Schedule/Schedule_9_10.coverage", "2ed4ece0-8028-49e1-9d2d-67c231ab8e00");
                            future = cal.getTime();
                            writeline("/home/ubuntu/results/coverage/Schedule/Schedule_9_10.coverage", "8157d0c6-4918-4f4e-be3c-086252cc8299");
                            timeToThisBuild = future.getTime() - now.getTime();
                            writeline("/home/ubuntu/results/coverage/Schedule/Schedule_9_10.coverage", "da5541c9-99eb-435d-b5f7-934f77076349");
                            break;
                        }
                    }
                }
                writeline("/home/ubuntu/results/coverage/Schedule/Schedule_9_10.coverage", "d5900282-f3a4-4a64-853a-c0bf57f54a46");
                if (timeToThisBuild < timeToNextBuild) {
                    writeline("/home/ubuntu/results/coverage/Schedule/Schedule_9_10.coverage", "e9be6f6d-1ff8-4864-afd7-0681b17b20e7");
                    timeToNextBuild = timeToThisBuild;
                }
            }
        }
        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_9_10.coverage", "0a88ba02-0036-4b44-b142-9b9ceb2e19ab");
        if (timeToNextBuild > MAX_INTERVAL_MILLISECONDS) {
            writeline("/home/ubuntu/results/coverage/Schedule/Schedule_9_10.coverage", "4f7844e6-1e94-4f3c-8d6b-0f785e213a56");
            LOG.error("checkTimeBuilders exceeding maximum interval. using proposed value [" + proposedTime + "] instead");
            writeline("/home/ubuntu/results/coverage/Schedule/Schedule_9_10.coverage", "952db60d-88d4-4ce3-9f5f-bd5baebcf183");
            timeToNextBuild = proposedTime;
        }
        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_9_10.coverage", "0f4225e5-4e1d-492c-bf8e-b89ba8e487c9");
        return timeToNextBuild;
    }

    long checkPauseBuilders(Date now, long proposedTime) {
        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_9_10.coverage", "23894c8c-d616-4c94-81d7-46a6432a2e85");
        long oldTime = proposedTime;
        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_9_10.coverage", "646f72b8-fcc4-4d0f-bca6-ce54e98b4bb9");
        long newTime = checkForPauseAtProposedTime(now, oldTime);
        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_9_10.coverage", "e407e08f-5d3c-4c2e-95c2-adfe6be6f3cc");
        while (oldTime != newTime) {
            writeline("/home/ubuntu/results/coverage/Schedule/Schedule_9_10.coverage", "3502626e-9942-4c96-8648-51a5cd80bec8");
            oldTime = newTime;
            writeline("/home/ubuntu/results/coverage/Schedule/Schedule_9_10.coverage", "50041a7d-a858-4d77-b287-ac5a8094fd98");
            newTime = checkForPauseAtProposedTime(now, oldTime);
        }
        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_9_10.coverage", "2f4b6f99-05cb-4c83-ac43-313d3b1bcfb3");
        return newTime;
    }

    private long checkForPauseAtProposedTime(Date now, long proposedTime) {
        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_9_10.coverage", "d9667cdc-c827-420d-a47e-5c6d9997c572");
        Date futureDate = getFutureDate(now, proposedTime);
        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_9_10.coverage", "6e5903ba-95aa-4fe8-a843-08d0ede897a3");
        PauseBuilder pause = findPause(futureDate);
        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_9_10.coverage", "441ce097-38f4-4641-9ad4-bd047ed8d45d");
        if (pause == null) {
            writeline("/home/ubuntu/results/coverage/Schedule/Schedule_9_10.coverage", "022a52ae-226a-406d-bd11-292513ce9c2b");
            return proposedTime;
        }
        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_9_10.coverage", "e2e0d515-86ac-4018-9f2a-68efc258b17a");
        int endPause = pause.getEndTime();
        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_9_10.coverage", "3299e7ba-b66a-4e28-8aca-6ec8ebb280ec");
        int futureTime = DateUtil.getTimeFromDate(futureDate);
        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_9_10.coverage", "ef2fe939-73b2-48b1-80f0-622e133dba12");
        long timeToEndOfPause = proposedTime + DateUtil.milliTimeDifference(futureTime, endPause);
        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_9_10.coverage", "ae46f7db-9466-46a9-818b-e7c92cde0014");
        timeToEndOfPause = checkMaximumInterval(timeToEndOfPause);
        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_9_10.coverage", "458315ce-c9ae-4cb9-a626-83ac2d5d6bd8");
        return timeToEndOfPause == MAX_INTERVAL_MILLISECONDS ? timeToEndOfPause : timeToEndOfPause + ONE_MINUTE;
    }

    private long checkMaximumInterval(long timeToEndOfPause) {
        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_9_10.coverage", "19088f0f-7353-4839-abb0-5a991467d823");
        if (timeToEndOfPause > MAX_INTERVAL_MILLISECONDS) {
            writeline("/home/ubuntu/results/coverage/Schedule/Schedule_9_10.coverage", "2fc38629-abd8-4dab-b7c6-df9c46ab95aa");
            LOG.error("maximum interval exceeded! project perpetually paused?");
            writeline("/home/ubuntu/results/coverage/Schedule/Schedule_9_10.coverage", "66390667-75f4-409e-8e6f-d41563fdbf56");
            return MAX_INTERVAL_MILLISECONDS;
        }
        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_9_10.coverage", "fb7c52c6-f417-4a14-8a89-3890ed578d91");
        return timeToEndOfPause;
    }

    private Date getFutureDate(Date now, long delay) {
        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_9_10.coverage", "e9596108-780b-4cde-b208-db98c048452f");
        long futureMillis = now.getTime() + delay;
        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_9_10.coverage", "e6e6ed6f-f4ca-47d2-879c-fae969d59cc7");
        return new Date(futureMillis);
    }

    public void setInterval(long intervalBetweenModificationChecks) {
        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_9_10.coverage", "783bdf74-d7bd-43af-b256-efd30d35e8a4");
        if (intervalBetweenModificationChecks <= 0) {
            writeline("/home/ubuntu/results/coverage/Schedule/Schedule_9_10.coverage", "bec20840-51d0-470b-85c7-b92a2d5f48be");
            throw new IllegalArgumentException("interval must be greater than zero");
        }
        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_9_10.coverage", "d7418fbe-3457-45aa-a98c-c2894157a174");
        interval = intervalBetweenModificationChecks * ONE_SECOND;
    }

    public long getInterval() {
        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_9_10.coverage", "9b4f2c6b-ecd8-404e-816a-fc7fdce7ae01");
        return interval;
    }

    public void setShowProgress(final boolean showProgress) {
        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_9_10.coverage", "52f56400-5fbe-48e3-b12a-ff0d8852da67");
        this.showProgress = showProgress;
    }

    public boolean getShowProgress() {
        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_9_10.coverage", "65c5e031-10db-4715-b816-ef83287ed25c");
        return showProgress;
    }

    public void validate() throws CruiseControlException {
        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_9_10.coverage", "d0ca8c75-7270-4a84-9e1e-1128046056cb");
        ValidationHelper.assertTrue(builders.size() > 0, "schedule element requires at least one nested builder element");
        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_9_10.coverage", "c1c1e8dd-b92e-4f20-8f56-230ac5317908");
        ValidationHelper.assertFalse(interval > ONE_YEAR, "maximum interval value is " + MAX_INTERVAL_SECONDS + " (one year)");
        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_9_10.coverage", "fdefccfb-54f4-40b9-96dd-1c0c25a2890c");
        if (hasOnlyTimeBuilders()) {
            writeline("/home/ubuntu/results/coverage/Schedule/Schedule_9_10.coverage", "788501fa-ad00-4f6b-83e6-19a38bcb2f68");
            LOG.warn("schedule has all time based builders: interval value will be ignored.");
            writeline("/home/ubuntu/results/coverage/Schedule/Schedule_9_10.coverage", "faa532ea-8401-495c-8874-21cf3d6a2e01");
            ValidationHelper.assertFalse(checkWithinPause(new ArrayList<Builder>(builders)), "all build times during pauses.");
        }
        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_9_10.coverage", "487b314a-b59b-42e7-aa5b-1ff48019191b");
        // Validate the child builders, since no one else seems to be doing it.
        for (final Builder next : builders) {
            writeline("/home/ubuntu/results/coverage/Schedule/Schedule_9_10.coverage", "112d9339-f87b-4271-8b07-1eb1bae52719");
            next.validate();
        }
    }

    private boolean checkWithinPause(List timeBuilders) {
        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_9_10.coverage", "3ce7a6ad-af60-4528-b8d0-c3ee840195ee");
        for (int i = 0; i < timeBuilders.size(); i++) {
            writeline("/home/ubuntu/results/coverage/Schedule/Schedule_9_10.coverage", "ac4f48f3-6300-4e85-af82-db29cd0eee95");
            Builder builder = (Builder) timeBuilders.get(i);
            writeline("/home/ubuntu/results/coverage/Schedule/Schedule_9_10.coverage", "9cda20ff-eb23-44c2-94e8-a525f6cec7ca");
            for (final PauseBuilder pauseBuilder : pauseBuilders) {
                writeline("/home/ubuntu/results/coverage/Schedule/Schedule_9_10.coverage", "e4f985c4-b49d-4a06-a196-61b9c33165a9");
                if (buildDaySameAsPauseDay(builder, pauseBuilder) && buildTimeWithinPauseTime(builder, pauseBuilder)) {
                    writeline("/home/ubuntu/results/coverage/Schedule/Schedule_9_10.coverage", "24d68cdb-ace1-45c9-9de5-2c9aa473d9b1");
                    timeBuilders.remove(builder);
                    writeline("/home/ubuntu/results/coverage/Schedule/Schedule_9_10.coverage", "f434f475-f4f0-40ca-bc42-a40c8376ab77");
                    StringBuffer message = new StringBuffer();
                    writeline("/home/ubuntu/results/coverage/Schedule/Schedule_9_10.coverage", "6a7f5a28-1cef-4029-962a-180b179b4018");
                    message.append("time Builder for time ");
                    writeline("/home/ubuntu/results/coverage/Schedule/Schedule_9_10.coverage", "6e7ebe98-0c03-451c-b736-dc67cd97d9e4");
                    message.append(Integer.toString(builder.getTime()));
                    writeline("/home/ubuntu/results/coverage/Schedule/Schedule_9_10.coverage", "7d4cdc4c-4037-4545-9b9c-0856f189eff3");
                    if (builder.getDay() != Builder.NOT_SET) {
                        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_9_10.coverage", "a129abdc-6b1a-4351-9eee-9966e8cb98d6");
                        message.append(" and day of ");
                        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_9_10.coverage", "2ecb6f09-ffd8-4f93-882f-416b454ae129");
                        message.append(getDayString(builder.getDay()));
                    }
                    writeline("/home/ubuntu/results/coverage/Schedule/Schedule_9_10.coverage", "73613078-1dc5-40b3-a088-75113d39ed41");
                    message.append(" is always within a pause and will never build");
                    writeline("/home/ubuntu/results/coverage/Schedule/Schedule_9_10.coverage", "1a7add65-dae9-4bf4-8c45-8a988fd760cf");
                    LOG.error(message.toString());
                }
            }
        }
        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_9_10.coverage", "bedc53aa-6c88-4b0f-b680-5960ade6731e");
        return timeBuilders.isEmpty();
    }

    /**
     * @param day
     * int value
     * @return english string value
     */
    String getDayString(int day) {
        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_9_10.coverage", "12664cc7-3df9-4742-885e-24efd2fa6f38");
        if (day < 1 || day > 7) {
            writeline("/home/ubuntu/results/coverage/Schedule/Schedule_9_10.coverage", "e6845af0-ac5e-4139-86cd-95265589c06b");
            throw new IllegalArgumentException("valid values of days are between 1 and 7, was " + day);
        }
        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_9_10.coverage", "ea2774fd-4f79-402a-a90b-e75101015afd");
        DateFormatSymbols symbols = new DateFormatSymbols(Locale.ENGLISH);
        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_9_10.coverage", "80e3b2fd-abf9-479d-a1ec-cdf4b7f6a1d6");
        String[] weekdays = symbols.getWeekdays();
        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_9_10.coverage", "e52cd67d-656f-43f0-b8fb-a3643128a8a1");
        return weekdays[day];
    }

    private boolean buildDaySameAsPauseDay(Builder builder, PauseBuilder pauseBuilder) {
        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_9_10.coverage", "e45da563-4712-408a-80a3-db5bff0b7db6");
        return pauseBuilder.getDay() == PauseBuilder.NOT_SET || pauseBuilder.getDay() == builder.getDay();
    }

    private boolean buildTimeWithinPauseTime(Builder builder, PauseBuilder pauseBuilder) {
        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_9_10.coverage", "003dd2d5-eeca-49ae-aae4-5c932a135fe2");
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
        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_9_10.coverage", "05952ead-4d95-460f-b3b6-d4b40f89315b");
        if (param == null) {
            writeline("/home/ubuntu/results/coverage/Schedule/Schedule_9_10.coverage", "4b47d156-403d-40ea-b34c-85d4aaf2ae67");
            throw new IllegalArgumentException(paramName + " can't be null");
        }
    }

    public List getBuilders() {
        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_9_10.coverage", "33839bfd-eeb2-4504-8898-4ddcf8666e66");
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
