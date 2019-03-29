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
        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_4_10.coverage", "23c710c4-596f-4a5b-9922-24399e37aba4");
        checkParamNotNull("builder", builder);
        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_4_10.coverage", "4ba94e8b-15c5-4154-9733-94b5a6e0f15f");
        builders.add(builder);
        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_4_10.coverage", "fccd145f-17ed-452e-9029-94314a8c176f");
        Collections.sort(builders, builderComparator);
    }

    public void add(final PauseBuilder pause) {
        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_4_10.coverage", "60a4f2d6-fc55-42c6-b22a-a670f95b1d45");
        checkParamNotNull("pauseBuilder", pause);
        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_4_10.coverage", "6a5862e6-426e-4a91-a4e8-5ef4a8b55988");
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
        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_4_10.coverage", "e519f026-2b81-4554-9a38-5ddb65bb05c8");
        checkParamNotNull("date", now);
        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_4_10.coverage", "b4c889d7-4fe7-4de1-b7ff-29716382da8b");
        PauseBuilder pause = findPause(now);
        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_4_10.coverage", "3dededde-417e-4b78-90e3-52200cbd6b69");
        if (pause != null) {
            writeline("/home/ubuntu/results/coverage/Schedule/Schedule_4_10.coverage", "3f625e85-5630-4d13-900e-04e4a8d5482d");
            LOG.info("CruiseControl is paused until: " + getEndTimeString(pause));
            writeline("/home/ubuntu/results/coverage/Schedule/Schedule_4_10.coverage", "c081b085-55d1-4575-930a-7fbc683ee7e7");
            return true;
        }
        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_4_10.coverage", "b77ceb8b-b686-4aa9-a08c-e3a45adb21aa");
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
        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_4_10.coverage", "ad6e9c80-8d58-4c99-a4cd-6cd2e63d1ecd");
        final Calendar cal = Calendar.getInstance();
        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_4_10.coverage", "91ef7532-1df8-47d8-8784-3c994d65d990");
        cal.set(Calendar.HOUR_OF_DAY, builder.getEndTime() / 100);
        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_4_10.coverage", "2c2dc0d5-c032-4493-aae8-f248ba5487f0");
        cal.set(Calendar.MINUTE, builder.getEndTime() % 100);
        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_4_10.coverage", "9b91a57f-1247-4802-8ba6-60c1af42b5d8");
        cal.add(Calendar.MINUTE, 1);
        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_4_10.coverage", "7e4cff8c-b0fc-4443-ae12-d3ec1ecf3548");
        return timeFormatter.format(cal.getTime());
    }

    PauseBuilder findPause(final Date date) {
        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_4_10.coverage", "97755d3c-36c4-4881-a39c-c253b8149119");
        checkParamNotNull("date", date);
        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_4_10.coverage", "650d4fef-5e81-4fa3-9a2d-e696bbc137b0");
        for (final PauseBuilder pause : pauseBuilders) {
            writeline("/home/ubuntu/results/coverage/Schedule/Schedule_4_10.coverage", "f2357470-4e25-4530-810a-ded9609c5d9a");
            if (pause.isPaused(date)) {
                writeline("/home/ubuntu/results/coverage/Schedule/Schedule_4_10.coverage", "1e6822ec-8c09-4857-804e-ff06fba7aa59");
                return pause;
            }
        }
        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_4_10.coverage", "18a915bf-5759-48a8-b384-cea218878949");
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
        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_4_10.coverage", "9ab20c66-4f3a-4cf4-b958-9e96ac753a8d");
        final Builder builder = selectBuilder(buildNumber, lastBuild, now);
        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_4_10.coverage", "6d6fe07c-8421-4cb0-a43a-1e13ede5a4c8");
        if (buildTarget != null) {
            writeline("/home/ubuntu/results/coverage/Schedule/Schedule_4_10.coverage", "3e1aea1d-6dce-4ac2-acfa-e0947434b56c");
            LOG.info("Overriding build target with \"" + buildTarget + "\"");
            writeline("/home/ubuntu/results/coverage/Schedule/Schedule_4_10.coverage", "0ad9c4e0-b7d7-49ae-85da-44161b88ba59");
            return builder.buildWithTarget(properties, buildTarget, (getShowProgress() ? progress : null));
        }
        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_4_10.coverage", "888d2478-a77e-482c-8cd1-072d28119e1f");
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
        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_4_10.coverage", "238bfbc6-0c4d-4b18-a1b7-27cc1c0ec1fa");
        Builder builder = findBuilder(buildNumber, lastBuild, now);
        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_4_10.coverage", "3d8579d8-2e76-4209-b51b-5dce139c9d8e");
        if (builder == null) {
            writeline("/home/ubuntu/results/coverage/Schedule/Schedule_4_10.coverage", "e367ca96-e0a8-4d52-8169-5cf017d937b4");
            final long timeToNextBuild = getTimeToNextBuild(now, ONE_MINUTE);
            writeline("/home/ubuntu/results/coverage/Schedule/Schedule_4_10.coverage", "100cc90a-3d49-46a0-948a-17cae4f8d9d7");
            final Date futureDate = getFutureDate(now, timeToNextBuild);
            writeline("/home/ubuntu/results/coverage/Schedule/Schedule_4_10.coverage", "f1ae5695-c2a5-4850-8c6a-c23ebf3156fe");
            builder = findBuilder(buildNumber, now, futureDate);
        }
        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_4_10.coverage", "ba904dc2-e32b-4f29-bd06-f511119fc50e");
        if (builder == null) {
            writeline("/home/ubuntu/results/coverage/Schedule/Schedule_4_10.coverage", "43b60d93-1b91-4bf2-ae25-7e4f0fcc8e42");
            validate();
            writeline("/home/ubuntu/results/coverage/Schedule/Schedule_4_10.coverage", "8045fdb5-38a1-4c84-959e-a6fafabdfeb9");
            throw new CruiseControlException("configuration error not caught by validate? no builder selected");
        }
        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_4_10.coverage", "1e13745b-0b14-4132-9bbf-b6884114024d");
        return builder;
    }

    private Builder findBuilder(final int buildNumber, final Date lastBuild, final Date now) throws CruiseControlException {
        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_4_10.coverage", "bdb358f1-4b7c-433a-84ca-4ff383b79592");
        for (final Builder builder : builders) {
            writeline("/home/ubuntu/results/coverage/Schedule/Schedule_4_10.coverage", "c9d92a14-fc6d-4bc1-8d90-2938268b14f5");
            if (builder.isTimeBuilder()) {
                writeline("/home/ubuntu/results/coverage/Schedule/Schedule_4_10.coverage", "7708bbd0-8ea9-48e8-8ae9-dfdc6e02d889");
                final int buildTime = builder.getTime();
                writeline("/home/ubuntu/results/coverage/Schedule/Schedule_4_10.coverage", "9000b68a-ced7-4845-a3de-d34fe48a874a");
                final boolean didntBuildToday = builderDidntBuildToday(lastBuild, now, buildTime);
                writeline("/home/ubuntu/results/coverage/Schedule/Schedule_4_10.coverage", "cd4373c8-c3a6-4d59-ba7e-62b35927696b");
                final int nowTime = DateUtil.getTimeFromDate(now);
                writeline("/home/ubuntu/results/coverage/Schedule/Schedule_4_10.coverage", "d44cc465-971b-40e2-b930-3c7ba62493bf");
                final boolean isAfterBuildTime = buildTime <= nowTime;
                writeline("/home/ubuntu/results/coverage/Schedule/Schedule_4_10.coverage", "12208c30-da73-478e-b3e3-b5f35a035fb6");
                final boolean isValidDay = builder.isValidDay(now);
                writeline("/home/ubuntu/results/coverage/Schedule/Schedule_4_10.coverage", "af8df880-8a39-48aa-8be9-d48ff134f14f");
                if (didntBuildToday && isAfterBuildTime && isValidDay) {
                    writeline("/home/ubuntu/results/coverage/Schedule/Schedule_4_10.coverage", "3ce85a9a-dd05-4d37-89d5-8797bc05246d");
                    return builder;
                }
            } else if (builder.getMultiple() > 0) {
                writeline("/home/ubuntu/results/coverage/Schedule/Schedule_4_10.coverage", "60faa3e0-6d2a-494d-ab69-d27972eecd3d");
                if (builder.isValidDay(now)) {
                    writeline("/home/ubuntu/results/coverage/Schedule/Schedule_4_10.coverage", "d6bb6da9-f606-438e-9ab1-6ae04953031e");
                    if ((buildNumber % builder.getMultiple()) == 0) {
                        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_4_10.coverage", "a933d57d-110b-4c86-b344-1bef768e12aa");
                        return builder;
                    }
                }
            } else {
                writeline("/home/ubuntu/results/coverage/Schedule/Schedule_4_10.coverage", "6ffee3a9-9d16-49c8-be23-0f32b56e27fd");
                throw new CruiseControlException("The selected Builder is not properly configured");
            }
        }
        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_4_10.coverage", "062d3e23-3907-432c-b812-326e826cd633");
        return null;
    }

    boolean builderDidntBuildToday(Date lastBuild, Date now, int buildTime) {
        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_4_10.coverage", "e4536a2e-5021-4b46-9a20-a1370cc297d7");
        int time = DateUtil.getTimeFromDate(now);
        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_4_10.coverage", "d05ac058-e34f-4792-8d75-57b728b9c23a");
        long timeMillis = DateUtil.convertToMillis(time);
        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_4_10.coverage", "599d3b0b-b110-45b0-ad19-6f22f915cebb");
        long startOfToday = now.getTime() - timeMillis;
        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_4_10.coverage", "bf71d80a-694c-46db-911c-743bc275910b");
        boolean lastBuildYesterday = lastBuild.getTime() < startOfToday;
        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_4_10.coverage", "4c12927f-f127-4f5d-80e3-8f61480c0c85");
        boolean lastBuildTimeBeforeBuildTime = DateUtil.getTimeFromDate(lastBuild) < buildTime;
        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_4_10.coverage", "921debee-6494-456e-9fcb-e22fba41fa24");
        return lastBuildYesterday || lastBuildTimeBeforeBuildTime;
    }

    long getTimeToNextBuild(Date now, long sleepInterval) {
        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_4_10.coverage", "1c2b3233-e1fd-4feb-a547-be0a125506c6");
        return getTimeToNextBuild(now, sleepInterval, 0);
    }

    private long getTimeToNextBuild(Date now, long sleepInterval, long priorPauseAdjustment) {
        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_4_10.coverage", "e0d03b80-3f2d-480b-933b-fffc659a92f6");
        long timeToNextBuild = sleepInterval;
        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_4_10.coverage", "7f5afba1-2189-408b-b4cf-12f717833e21");
        LOG.debug("getTimeToNextBuild: initial timeToNextBuild = " + timeToNextBuild);
        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_4_10.coverage", "a4de3afd-6342-4917-bb6b-f26d5e0314cf");
        timeToNextBuild = checkMultipleBuilders(now, timeToNextBuild);
        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_4_10.coverage", "aba233ea-ca99-4fe6-a954-2a0162681049");
        LOG.debug("getTimeToNextBuild: after checkMultipleBuilders = " + timeToNextBuild);
        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_4_10.coverage", "77e6d19b-b21c-4d91-8be0-351e3b7e761d");
        timeToNextBuild = checkTimeBuilders(now, timeToNextBuild);
        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_4_10.coverage", "fbc35e76-aebf-4c64-8ca3-d88c568c18b9");
        LOG.debug("getTimeToNextBuild: after checkTimeBuilders = " + timeToNextBuild);
        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_4_10.coverage", "5b0a0aef-ae84-4ebb-8c16-9e5188d1cdd3");
        long timeTillNotPaused = checkPauseBuilders(now, timeToNextBuild);
        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_4_10.coverage", "2c36d74d-ed46-494d-8387-04975f421a81");
        LOG.debug("getTimeToNextBuild: after checkPauseBuilders = " + timeToNextBuild);
        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_4_10.coverage", "35043760-f273-4e19-a295-bff4c89879ed");
        if (timeToNextBuild != timeTillNotPaused) {
            writeline("/home/ubuntu/results/coverage/Schedule/Schedule_4_10.coverage", "3cd7dd3c-5b25-4c41-b37d-7cafceb42e84");
            boolean atMaxTime = timeTillNotPaused >= MAX_INTERVAL_MILLISECONDS || priorPauseAdjustment >= MAX_INTERVAL_MILLISECONDS;
            writeline("/home/ubuntu/results/coverage/Schedule/Schedule_4_10.coverage", "5b2ea5f6-d874-4d83-aa61-37c316a15723");
            if (hasOnlyTimeBuilders() && !atMaxTime) {
                writeline("/home/ubuntu/results/coverage/Schedule/Schedule_4_10.coverage", "258e3a3d-a68a-423c-861b-22f1e1bb7f86");
                Date dateAfterPause = getFutureDate(now, timeTillNotPaused);
                writeline("/home/ubuntu/results/coverage/Schedule/Schedule_4_10.coverage", "f7e6028f-7d86-4862-89c8-e9b14789daf0");
                long adjustmentFromEndOfPause = getTimeToNextBuild(dateAfterPause, 0, priorPauseAdjustment + timeTillNotPaused);
                writeline("/home/ubuntu/results/coverage/Schedule/Schedule_4_10.coverage", "67fdb0de-7774-404a-894c-9fbb1210058c");
                timeToNextBuild = timeTillNotPaused + adjustmentFromEndOfPause;
                writeline("/home/ubuntu/results/coverage/Schedule/Schedule_4_10.coverage", "4c933b3b-5cc8-4b71-b459-d1cc2fcffbf3");
                timeToNextBuild = checkMaximumInterval(timeToNextBuild);
            } else {
                writeline("/home/ubuntu/results/coverage/Schedule/Schedule_4_10.coverage", "dca4dc08-1c64-4720-b825-4d3a535b5c28");
                timeToNextBuild = timeTillNotPaused;
            }
        }
        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_4_10.coverage", "e638cb79-29ac-443e-8a70-fa95e2bc667c");
        return timeToNextBuild;
    }

    private long checkMultipleBuilders(final Date now, final long interval) {
        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_4_10.coverage", "c4f0826a-e0a3-414f-9d5a-c6e72f586ea1");
        if (hasOnlyTimeBuilders()) {
            writeline("/home/ubuntu/results/coverage/Schedule/Schedule_4_10.coverage", "ba24db58-ab27-4717-94df-b11573354229");
            LOG.debug("has only time builders, so no correction for multiple builders.");
            writeline("/home/ubuntu/results/coverage/Schedule/Schedule_4_10.coverage", "90420f57-a706-4365-9c23-9976725156b2");
            return interval;
        }
        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_4_10.coverage", "85a546bf-b511-4f73-b77c-a1dde715cc0d");
        Date then = getFutureDate(now, interval);
        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_4_10.coverage", "2c554f09-4651-4860-b0a2-2b69686a8062");
        final List<Builder> buildersForOtherDays = new ArrayList<Builder>();
        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_4_10.coverage", "c749e841-2f64-42f7-811a-d19b6ecd79ee");
        for (final Builder builder : builders) {
            writeline("/home/ubuntu/results/coverage/Schedule/Schedule_4_10.coverage", "bd244fea-9189-4b13-8576-e56410bc0b02");
            if (!builder.isTimeBuilder()) {
                writeline("/home/ubuntu/results/coverage/Schedule/Schedule_4_10.coverage", "ef45b2be-cc7b-4d33-8d56-c5e732120d03");
                if (builder.getMultiple() == 1) {
                    writeline("/home/ubuntu/results/coverage/Schedule/Schedule_4_10.coverage", "b4017b23-6b01-4002-8fce-dd5fc618c059");
                    if (builder.isValidDay(then)) {
                        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_4_10.coverage", "675991b8-86fb-4eb2-af77-9eb470f0ade8");
                        LOG.debug("multiple=1 builder found that could run on " + then);
                        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_4_10.coverage", "f377c60f-f5ab-45bf-a3ea-2d79590299c9");
                        return interval;
                    } else {
                        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_4_10.coverage", "4ff0d097-b790-401d-baee-d5b7f1a1f714");
                        buildersForOtherDays.add(builder);
                    }
                }
            }
        }
        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_4_10.coverage", "bc234e7e-35e4-424c-b424-a7a187fe5d00");
        if (buildersForOtherDays.size() == 0) {
            writeline("/home/ubuntu/results/coverage/Schedule/Schedule_4_10.coverage", "0c316f42-9799-4190-981d-18fb66b1ab23");
            LOG.error("configuration error: has some multiple builders but no multiple=1 builders found!");
            writeline("/home/ubuntu/results/coverage/Schedule/Schedule_4_10.coverage", "4b727086-dc86-46df-b20c-4c7c1688e6c5");
            return interval;
        } else {
            writeline("/home/ubuntu/results/coverage/Schedule/Schedule_4_10.coverage", "6bb2e690-5772-4c88-b511-99d55d13086f");
            LOG.debug("no multiple=1 builders found for " + then + ". checking other days");
        }
        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_4_10.coverage", "efe59a54-1cc3-4709-92b7-f340071ed2a4");
        for (int i = 1; i < 7; i++) {
            writeline("/home/ubuntu/results/coverage/Schedule/Schedule_4_10.coverage", "7e01bfb5-f8f2-4fcc-ae4c-d3943567a7d6");
            long daysPastInitialInterval = i * ONE_DAY;
            writeline("/home/ubuntu/results/coverage/Schedule/Schedule_4_10.coverage", "5873a227-cfcc-4cc1-8790-b6bdf711b39c");
            then = getFutureDate(now, interval + daysPastInitialInterval);
            writeline("/home/ubuntu/results/coverage/Schedule/Schedule_4_10.coverage", "ec5ee052-844c-49f6-bf7f-28f6bbf1745d");
            for (final Builder builder : builders) {
                writeline("/home/ubuntu/results/coverage/Schedule/Schedule_4_10.coverage", "35b689c4-7729-4b31-b2ba-68f5e7bde8d0");
                if (builder.isValidDay(then)) {
                    writeline("/home/ubuntu/results/coverage/Schedule/Schedule_4_10.coverage", "21c24a7d-85e7-4f23-9b06-24a8ac70b376");
                    LOG.debug("multiple=1 builder found that could run on " + then);
                    writeline("/home/ubuntu/results/coverage/Schedule/Schedule_4_10.coverage", "99a6cd2f-398e-4f9c-8167-03bb212a2e99");
                    long correctionToMidnight = getTimePastMidnight(then);
                    writeline("/home/ubuntu/results/coverage/Schedule/Schedule_4_10.coverage", "00026107-cf34-4dc3-839a-9e0e12f46b6b");
                    return interval + daysPastInitialInterval - correctionToMidnight;
                }
            }
        }
        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_4_10.coverage", "31f6125c-b040-4e70-bcfc-ce050a8ff722");
        LOG.error("configuration error? could not find appropriate multiple=1 builder.");
        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_4_10.coverage", "b1ad4f18-689d-482d-9c37-f4585d0ccd78");
        return interval;
    }

    private long getTimePastMidnight(Date date) {
        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_4_10.coverage", "893c6066-231a-4e12-8194-cbf00a5257b8");
        Calendar cal = Calendar.getInstance();
        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_4_10.coverage", "bc75875f-fe14-4f2a-a3af-bfe130496634");
        cal.setTime(date);
        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_4_10.coverage", "c7d6cf40-906e-46bf-a304-41bb8f7cabe4");
        long time = 60 * ONE_MINUTE * cal.get(Calendar.HOUR_OF_DAY);
        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_4_10.coverage", "25aed389-cefd-41f4-aba4-332328aa86e8");
        time += ONE_MINUTE * cal.get(Calendar.MINUTE);
        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_4_10.coverage", "a8e03758-152f-48b2-9111-ea82b0a7904c");
        return time;
    }

    private boolean hasOnlyTimeBuilders() {
        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_4_10.coverage", "ea2070a8-d1f1-4818-83e6-b8b3238a5540");
        boolean onlyTimeBuilders = true;
        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_4_10.coverage", "2cfedadf-8595-42fc-8824-e4131166869e");
        for (final Builder builder : builders) {
            writeline("/home/ubuntu/results/coverage/Schedule/Schedule_4_10.coverage", "3c233054-6e9f-45c7-b5ef-902d5b1469b7");
            if (!builder.isTimeBuilder()) {
                writeline("/home/ubuntu/results/coverage/Schedule/Schedule_4_10.coverage", "c659548e-630d-41e4-b1de-e5b786d19ada");
                onlyTimeBuilders = false;
                writeline("/home/ubuntu/results/coverage/Schedule/Schedule_4_10.coverage", "6a14d5d5-5250-48e7-8d8a-9f3b71fc74cc");
                break;
            }
        }
        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_4_10.coverage", "23563b9c-7dae-48ee-80f3-18ed7f610cf9");
        return onlyTimeBuilders;
    }

    long checkTimeBuilders(final Date now, final long proposedTime) {
        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_4_10.coverage", "b98f1fd6-8c44-4886-aec9-09b16577635a");
        long timeToNextBuild = proposedTime;
        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_4_10.coverage", "2bfc4a34-b2f8-4bf6-a2db-b9478b06315e");
        if (hasOnlyTimeBuilders()) {
            writeline("/home/ubuntu/results/coverage/Schedule/Schedule_4_10.coverage", "e8ebc484-8aa8-4a24-a22e-3c5254ac2c6a");
            timeToNextBuild = Long.MAX_VALUE;
        }
        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_4_10.coverage", "dcb98623-48b5-44a0-842b-ad136715cdd0");
        final int nowTime = DateUtil.getTimeFromDate(now);
        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_4_10.coverage", "ea7fb2f2-8b3a-4514-be62-3993393a9849");
        for (final Builder builder : builders) {
            writeline("/home/ubuntu/results/coverage/Schedule/Schedule_4_10.coverage", "ec12e78c-416d-4521-b7d0-887061217c1a");
            if (builder.isTimeBuilder()) {
                writeline("/home/ubuntu/results/coverage/Schedule/Schedule_4_10.coverage", "c94bfeb4-2951-474b-9d01-cfabcdb2b11a");
                long timeToThisBuild = Long.MAX_VALUE;
                writeline("/home/ubuntu/results/coverage/Schedule/Schedule_4_10.coverage", "2fed08e2-9ac6-45bd-b962-558b09824f98");
                final Calendar cal = Calendar.getInstance();
                writeline("/home/ubuntu/results/coverage/Schedule/Schedule_4_10.coverage", "efde3dbd-6a40-466b-abba-98db381963ba");
                final long oneYear = 365;
                writeline("/home/ubuntu/results/coverage/Schedule/Schedule_4_10.coverage", "2f7c4e54-4103-43b0-affd-9f7db2fa635d");
                for (int daysInTheFuture = 0; daysInTheFuture < oneYear; daysInTheFuture++) {
                    writeline("/home/ubuntu/results/coverage/Schedule/Schedule_4_10.coverage", "3116d8f4-3a47-4faa-9b4c-b4169f4bd005");
                    cal.setTime(now);
                    writeline("/home/ubuntu/results/coverage/Schedule/Schedule_4_10.coverage", "b1c1baf6-a77a-46e0-b673-c9195ba88a0d");
                    cal.add(Calendar.DATE, daysInTheFuture);
                    writeline("/home/ubuntu/results/coverage/Schedule/Schedule_4_10.coverage", "bdd63672-2c52-4e2f-b2d6-f1a07958a8d3");
                    Date future = cal.getTime();
                    writeline("/home/ubuntu/results/coverage/Schedule/Schedule_4_10.coverage", "43192b2d-3917-4fff-8c3b-223c3828f9f5");
                    final boolean dayIsValid = builder.isValidDay(future);
                    writeline("/home/ubuntu/results/coverage/Schedule/Schedule_4_10.coverage", "abca8568-307b-4e01-b9c6-e012b73a47ac");
                    if (dayIsValid) {
                        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_4_10.coverage", "bb2e7a54-916c-49f3-b842-63429fb164f0");
                        int thisBuildTime = builder.getTime();
                        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_4_10.coverage", "ba3e8b95-707e-4f37-8e68-f91ac10ccf82");
                        boolean timePassedToday = (daysInTheFuture == 0) && (nowTime > thisBuildTime);
                        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_4_10.coverage", "4c83a61a-4bbc-4b59-b44f-e0a60e1748fb");
                        if (!timePassedToday) {
                            writeline("/home/ubuntu/results/coverage/Schedule/Schedule_4_10.coverage", "c9850bbd-c652-4ccb-9769-73f8ee7fe0ea");
                            int buildHour = thisBuildTime / 100;
                            writeline("/home/ubuntu/results/coverage/Schedule/Schedule_4_10.coverage", "2bcf591b-a509-4b14-97bb-7def82d431ce");
                            int buildMinute = thisBuildTime % 100;
                            writeline("/home/ubuntu/results/coverage/Schedule/Schedule_4_10.coverage", "f1f0d3ff-3ace-489c-894e-850f479766f8");
                            cal.set(Calendar.HOUR_OF_DAY, buildHour);
                            writeline("/home/ubuntu/results/coverage/Schedule/Schedule_4_10.coverage", "e021a804-e596-470d-ae40-0836700ed914");
                            cal.set(Calendar.MINUTE, buildMinute);
                            writeline("/home/ubuntu/results/coverage/Schedule/Schedule_4_10.coverage", "70a00c66-86d9-4298-b34b-1aadadb732c3");
                            future = cal.getTime();
                            writeline("/home/ubuntu/results/coverage/Schedule/Schedule_4_10.coverage", "5cf381f2-7c13-4f96-8933-96bdb22fcdb2");
                            timeToThisBuild = future.getTime() - now.getTime();
                            writeline("/home/ubuntu/results/coverage/Schedule/Schedule_4_10.coverage", "d25d519b-b640-4886-a5fe-2f7bcca78364");
                            break;
                        }
                    }
                }
                writeline("/home/ubuntu/results/coverage/Schedule/Schedule_4_10.coverage", "d126488f-8539-4a8a-8b26-58b83e186c1e");
                if (timeToThisBuild < timeToNextBuild) {
                    writeline("/home/ubuntu/results/coverage/Schedule/Schedule_4_10.coverage", "f0ccf2d7-aafe-4446-85f9-b2cb27cc333b");
                    timeToNextBuild = timeToThisBuild;
                }
            }
        }
        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_4_10.coverage", "e89de24a-ed48-46fd-bd51-0bba4645eaaf");
        if (timeToNextBuild > MAX_INTERVAL_MILLISECONDS) {
            writeline("/home/ubuntu/results/coverage/Schedule/Schedule_4_10.coverage", "78d9bd45-c229-4609-8dd6-ab173cfb1bbf");
            LOG.error("checkTimeBuilders exceeding maximum interval. using proposed value [" + proposedTime + "] instead");
            writeline("/home/ubuntu/results/coverage/Schedule/Schedule_4_10.coverage", "36b3a2d9-e055-44d2-88ef-74d1833db665");
            timeToNextBuild = proposedTime;
        }
        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_4_10.coverage", "89d0692d-308d-4ecc-b2f5-5a2d16581dbe");
        return timeToNextBuild;
    }

    long checkPauseBuilders(Date now, long proposedTime) {
        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_4_10.coverage", "27ee3a4c-eae5-4ba3-a09b-6af7fbbfb1e5");
        long oldTime = proposedTime;
        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_4_10.coverage", "e2f23c77-c400-49ee-b940-3ef2874c67ef");
        long newTime = checkForPauseAtProposedTime(now, oldTime);
        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_4_10.coverage", "46d96ca6-3b1e-449e-b718-480c3bc64a9b");
        while (oldTime != newTime) {
            writeline("/home/ubuntu/results/coverage/Schedule/Schedule_4_10.coverage", "12ab57cd-20c5-4fca-bef4-714193d64270");
            oldTime = newTime;
            writeline("/home/ubuntu/results/coverage/Schedule/Schedule_4_10.coverage", "33bef832-48d9-415b-a85f-7ed40ffeb75b");
            newTime = checkForPauseAtProposedTime(now, oldTime);
        }
        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_4_10.coverage", "01b708e2-528c-41c4-a817-6ef282a5bd89");
        return newTime;
    }

    private long checkForPauseAtProposedTime(Date now, long proposedTime) {
        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_4_10.coverage", "2057088f-b3d1-4ef6-b1b3-4f9ea86990c1");
        Date futureDate = getFutureDate(now, proposedTime);
        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_4_10.coverage", "a6262de7-7617-40c1-bafb-fc41ce107f22");
        PauseBuilder pause = findPause(futureDate);
        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_4_10.coverage", "717ff893-8a0e-4f84-b12f-4307f5e6b44c");
        if (pause == null) {
            writeline("/home/ubuntu/results/coverage/Schedule/Schedule_4_10.coverage", "111cbd46-55e1-4e6d-abaa-11abefbf88a0");
            return proposedTime;
        }
        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_4_10.coverage", "e76ec113-7605-4b06-abcd-5a9bec894809");
        int endPause = pause.getEndTime();
        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_4_10.coverage", "bb4e95ad-5b35-42dd-80e7-9991deb72dcb");
        int futureTime = DateUtil.getTimeFromDate(futureDate);
        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_4_10.coverage", "36fce6e3-a6d0-425a-be73-f23ad2c7749d");
        long timeToEndOfPause = proposedTime + DateUtil.milliTimeDifference(futureTime, endPause);
        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_4_10.coverage", "83cc15db-907c-4fcd-af4a-19dcadf1ae8c");
        timeToEndOfPause = checkMaximumInterval(timeToEndOfPause);
        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_4_10.coverage", "0702c712-9b72-4d5d-bd04-d0022df97b3a");
        return timeToEndOfPause == MAX_INTERVAL_MILLISECONDS ? timeToEndOfPause : timeToEndOfPause + ONE_MINUTE;
    }

    private long checkMaximumInterval(long timeToEndOfPause) {
        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_4_10.coverage", "d411b721-3ac7-442e-aedb-2916358a285c");
        if (timeToEndOfPause > MAX_INTERVAL_MILLISECONDS) {
            writeline("/home/ubuntu/results/coverage/Schedule/Schedule_4_10.coverage", "5ad5552b-1bf2-4788-9cf8-a9a069f23966");
            LOG.error("maximum interval exceeded! project perpetually paused?");
            writeline("/home/ubuntu/results/coverage/Schedule/Schedule_4_10.coverage", "5a0e8e47-a66d-4f75-9dbf-5c72cfeff693");
            return MAX_INTERVAL_MILLISECONDS;
        }
        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_4_10.coverage", "b580d28b-5144-46f0-899e-f830702db007");
        return timeToEndOfPause;
    }

    private Date getFutureDate(Date now, long delay) {
        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_4_10.coverage", "36151af1-8195-426b-a185-8999243285dc");
        long futureMillis = now.getTime() + delay;
        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_4_10.coverage", "6dd9327f-b146-4aac-84e8-cf20ad97123b");
        return new Date(futureMillis);
    }

    public void setInterval(long intervalBetweenModificationChecks) {
        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_4_10.coverage", "c976d515-839e-4849-8811-909f1c157025");
        if (intervalBetweenModificationChecks <= 0) {
            writeline("/home/ubuntu/results/coverage/Schedule/Schedule_4_10.coverage", "4320240e-ccb5-4a31-befc-5ba155753f27");
            throw new IllegalArgumentException("interval must be greater than zero");
        }
        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_4_10.coverage", "cbbc41a6-a63f-4178-b294-04d74f75cf10");
        interval = intervalBetweenModificationChecks * ONE_SECOND;
    }

    public long getInterval() {
        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_4_10.coverage", "b4f33ebc-5ce0-4d52-9f9a-e76e77060d2b");
        return interval;
    }

    public void setShowProgress(final boolean showProgress) {
        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_4_10.coverage", "57f4568b-7684-4222-bfdc-8ddb28d8c2dd");
        this.showProgress = showProgress;
    }

    public boolean getShowProgress() {
        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_4_10.coverage", "ffffb65b-bf5b-496b-977b-8ce1a96774b8");
        return showProgress;
    }

    public void validate() throws CruiseControlException {
        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_4_10.coverage", "20366b72-1f42-4b9d-ab9f-2270b87099fd");
        ValidationHelper.assertTrue(builders.size() > 0, "schedule element requires at least one nested builder element");
        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_4_10.coverage", "c6e3aa18-b1b7-4b01-aaa9-82a9e8f8a4e5");
        ValidationHelper.assertFalse(interval > ONE_YEAR, "maximum interval value is " + MAX_INTERVAL_SECONDS + " (one year)");
        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_4_10.coverage", "ee878c15-0116-41ec-9420-288222be6115");
        if (hasOnlyTimeBuilders()) {
            writeline("/home/ubuntu/results/coverage/Schedule/Schedule_4_10.coverage", "329b9bcb-8289-4a9c-8a70-e9f3549de178");
            LOG.warn("schedule has all time based builders: interval value will be ignored.");
            writeline("/home/ubuntu/results/coverage/Schedule/Schedule_4_10.coverage", "b7714004-09f1-498e-b3b1-d2f8c28b676b");
            ValidationHelper.assertFalse(checkWithinPause(new ArrayList<Builder>(builders)), "all build times during pauses.");
        }
        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_4_10.coverage", "c41d2750-cfef-45b2-a3c8-8e07dcb2a1b3");
        // Validate the child builders, since no one else seems to be doing it.
        for (final Builder next : builders) {
            writeline("/home/ubuntu/results/coverage/Schedule/Schedule_4_10.coverage", "6bdd8ece-0b86-40f6-a8a6-d1aece510d11");
            next.validate();
        }
    }

    private boolean checkWithinPause(List timeBuilders) {
        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_4_10.coverage", "a956b7f4-cbc1-4ae1-8e2e-38f9a78d3bb9");
        for (int i = 0; i < timeBuilders.size(); i++) {
            writeline("/home/ubuntu/results/coverage/Schedule/Schedule_4_10.coverage", "5205b62a-3fe3-4800-b398-59f371592bc6");
            Builder builder = (Builder) timeBuilders.get(i);
            writeline("/home/ubuntu/results/coverage/Schedule/Schedule_4_10.coverage", "ae6f82cb-8fb5-4353-af93-81f100904905");
            for (final PauseBuilder pauseBuilder : pauseBuilders) {
                writeline("/home/ubuntu/results/coverage/Schedule/Schedule_4_10.coverage", "721f4765-8356-43a2-a19f-54b75f506f3f");
                if (buildDaySameAsPauseDay(builder, pauseBuilder) && buildTimeWithinPauseTime(builder, pauseBuilder)) {
                    writeline("/home/ubuntu/results/coverage/Schedule/Schedule_4_10.coverage", "e156e69f-3ae6-4fb3-80fc-a7c87142b365");
                    timeBuilders.remove(builder);
                    writeline("/home/ubuntu/results/coverage/Schedule/Schedule_4_10.coverage", "d5b6a650-0bd4-4116-96fd-b089b3c47364");
                    StringBuffer message = new StringBuffer();
                    writeline("/home/ubuntu/results/coverage/Schedule/Schedule_4_10.coverage", "85dc9856-4c99-4b17-8ed1-b1aee203d9c8");
                    message.append("time Builder for time ");
                    writeline("/home/ubuntu/results/coverage/Schedule/Schedule_4_10.coverage", "fc168f95-3d1c-4b38-8c13-289dba36f296");
                    message.append(Integer.toString(builder.getTime()));
                    writeline("/home/ubuntu/results/coverage/Schedule/Schedule_4_10.coverage", "fef4cbcf-8d4b-49ec-a5b7-81a8c8714816");
                    if (builder.getDay() != Builder.NOT_SET) {
                        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_4_10.coverage", "b1102df9-2ab6-4219-af0c-d9ddeb55f342");
                        message.append(" and day of ");
                        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_4_10.coverage", "3c01cdb5-b3ad-4af6-822d-da842836fe17");
                        message.append(getDayString(builder.getDay()));
                    }
                    writeline("/home/ubuntu/results/coverage/Schedule/Schedule_4_10.coverage", "d26b3004-f5b1-490a-969c-a25776b2fc6b");
                    message.append(" is always within a pause and will never build");
                    writeline("/home/ubuntu/results/coverage/Schedule/Schedule_4_10.coverage", "dfc95c05-7ff6-4469-bb9c-d06a94ac3103");
                    LOG.error(message.toString());
                }
            }
        }
        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_4_10.coverage", "40fc836a-18c0-468b-86cd-7efe57c0f04e");
        return timeBuilders.isEmpty();
    }

    /**
     * @param day
     * int value
     * @return english string value
     */
    String getDayString(int day) {
        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_4_10.coverage", "933053cd-ee5f-4d0e-b553-d2adf70a41a2");
        if (day < 1 || day > 7) {
            writeline("/home/ubuntu/results/coverage/Schedule/Schedule_4_10.coverage", "5d4ecc39-4509-4aba-aee0-1891774e41ee");
            throw new IllegalArgumentException("valid values of days are between 1 and 7, was " + day);
        }
        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_4_10.coverage", "9491c3ab-b7a9-401d-a303-a5b46e4ed1e6");
        DateFormatSymbols symbols = new DateFormatSymbols(Locale.ENGLISH);
        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_4_10.coverage", "aaab05e9-1ae0-4212-aa86-b75668bca6a3");
        String[] weekdays = symbols.getWeekdays();
        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_4_10.coverage", "5fa8bfd7-fdfe-4226-a3a2-cfc1bf1799f5");
        return weekdays[day];
    }

    private boolean buildDaySameAsPauseDay(Builder builder, PauseBuilder pauseBuilder) {
        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_4_10.coverage", "77c5ab96-e2c1-400b-8361-53712fc10e0e");
        return pauseBuilder.getDay() == PauseBuilder.NOT_SET || pauseBuilder.getDay() == builder.getDay();
    }

    private boolean buildTimeWithinPauseTime(Builder builder, PauseBuilder pauseBuilder) {
        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_4_10.coverage", "8a6643fc-91d2-4060-baec-f283c1a57f5a");
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
        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_4_10.coverage", "5f777fcc-9c3d-43dc-8748-4d79946b7397");
        if (param == null) {
            writeline("/home/ubuntu/results/coverage/Schedule/Schedule_4_10.coverage", "3d4fcbef-e415-4546-b1b0-d66063f3f34d");
            throw new IllegalArgumentException(paramName + " can't be null");
        }
    }

    public List getBuilders() {
        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_4_10.coverage", "238db455-a09a-428e-a09f-be0ac4187f83");
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
