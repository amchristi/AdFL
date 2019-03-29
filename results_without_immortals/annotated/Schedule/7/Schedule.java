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
        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_7_10.coverage", "5716ae56-4443-4bcf-9a1f-7a6181212b23");
        checkParamNotNull("builder", builder);
        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_7_10.coverage", "fda7aade-ebf3-434a-ad27-b1b5d9e256dd");
        builders.add(builder);
        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_7_10.coverage", "7ab62d9a-9860-41c6-896e-d1c5eacc72d7");
        Collections.sort(builders, builderComparator);
    }

    public void add(final PauseBuilder pause) {
        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_7_10.coverage", "b2ebcb77-7023-4fc3-b626-3699811008c8");
        checkParamNotNull("pauseBuilder", pause);
        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_7_10.coverage", "153d5adc-379b-4106-8f3e-ce2c112bb293");
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
        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_7_10.coverage", "50ade551-98b8-44bb-a269-afa26b05d71c");
        checkParamNotNull("date", now);
        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_7_10.coverage", "3c9c757b-989d-472f-8d6e-431fe47034be");
        PauseBuilder pause = findPause(now);
        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_7_10.coverage", "e69d00cd-59a5-45e3-acb3-7521e0b6dd55");
        if (pause != null) {
            writeline("/home/ubuntu/results/coverage/Schedule/Schedule_7_10.coverage", "e5059366-06be-41d2-a124-fc02fccca80e");
            LOG.info("CruiseControl is paused until: " + getEndTimeString(pause));
            writeline("/home/ubuntu/results/coverage/Schedule/Schedule_7_10.coverage", "5d5264dd-2559-4f18-bed7-36150da5b274");
            return true;
        }
        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_7_10.coverage", "4722352c-4c74-45e5-87ac-75e2c6fbf8dc");
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
        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_7_10.coverage", "038202d0-ba92-444d-9a8a-07882d117ab5");
        final Calendar cal = Calendar.getInstance();
        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_7_10.coverage", "2b819cc5-3302-425f-9385-06d0433ab53c");
        cal.set(Calendar.HOUR_OF_DAY, builder.getEndTime() / 100);
        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_7_10.coverage", "99a7fc04-54cb-4bfd-8f21-ac12c7f34947");
        cal.set(Calendar.MINUTE, builder.getEndTime() % 100);
        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_7_10.coverage", "545370eb-9891-4443-80ad-4ddaf5e4fd28");
        cal.add(Calendar.MINUTE, 1);
        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_7_10.coverage", "568e3a5e-6fef-4559-8914-7df20a2690ae");
        return timeFormatter.format(cal.getTime());
    }

    PauseBuilder findPause(final Date date) {
        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_7_10.coverage", "2f142986-de1d-40b2-b5f9-382552fbacd9");
        checkParamNotNull("date", date);
        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_7_10.coverage", "30de2769-8a7f-4035-bbb1-31ed67ba565b");
        for (final PauseBuilder pause : pauseBuilders) {
            writeline("/home/ubuntu/results/coverage/Schedule/Schedule_7_10.coverage", "dac1239c-212e-4db3-8a66-34c7345e7e52");
            if (pause.isPaused(date)) {
                writeline("/home/ubuntu/results/coverage/Schedule/Schedule_7_10.coverage", "3644f274-40bd-478d-a924-9f9982f4d557");
                return pause;
            }
        }
        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_7_10.coverage", "0b3d1de7-084a-43f2-8df2-42a197475935");
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
        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_7_10.coverage", "15fc4c86-4343-4ead-b49e-9d534be23397");
        final Builder builder = selectBuilder(buildNumber, lastBuild, now);
        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_7_10.coverage", "6b80180b-2cea-49f8-beed-c954cfcd3f76");
        if (buildTarget != null) {
            writeline("/home/ubuntu/results/coverage/Schedule/Schedule_7_10.coverage", "d3141fa1-4b91-45fa-80c7-30a3d5d2e84c");
            LOG.info("Overriding build target with \"" + buildTarget + "\"");
            writeline("/home/ubuntu/results/coverage/Schedule/Schedule_7_10.coverage", "0dd77689-38c7-4006-a5b1-1bc38bc823f5");
            return builder.buildWithTarget(properties, buildTarget, (getShowProgress() ? progress : null));
        }
        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_7_10.coverage", "db87f4ff-6e05-4ac8-a3f6-0c2dacbaf6c9");
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
        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_7_10.coverage", "5f5f6bdc-5db4-4141-bac6-7c32b7e6696e");
        Builder builder = findBuilder(buildNumber, lastBuild, now);
        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_7_10.coverage", "b4c0eec8-17c3-40b2-9aa7-4f10bf158f72");
        if (builder == null) {
            writeline("/home/ubuntu/results/coverage/Schedule/Schedule_7_10.coverage", "5c77f061-6af3-462b-9cca-fd78a34d5bea");
            final long timeToNextBuild = getTimeToNextBuild(now, ONE_MINUTE);
            writeline("/home/ubuntu/results/coverage/Schedule/Schedule_7_10.coverage", "9456f043-e5b2-45b8-ad22-0d7c04cfdc71");
            final Date futureDate = getFutureDate(now, timeToNextBuild);
            writeline("/home/ubuntu/results/coverage/Schedule/Schedule_7_10.coverage", "35043d54-17b2-4056-a104-e3ed89e137bc");
            builder = findBuilder(buildNumber, now, futureDate);
        }
        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_7_10.coverage", "d60ce4d7-29f1-41d4-84b9-f78d8cb01483");
        if (builder == null) {
            writeline("/home/ubuntu/results/coverage/Schedule/Schedule_7_10.coverage", "5337bdf6-a752-4312-86f8-eb2905d80f4f");
            validate();
            writeline("/home/ubuntu/results/coverage/Schedule/Schedule_7_10.coverage", "d7ce9fa9-716d-4606-96d9-44914990a271");
            throw new CruiseControlException("configuration error not caught by validate? no builder selected");
        }
        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_7_10.coverage", "6aa59bc3-ddea-49bf-ba82-fa0f88da5ee4");
        return builder;
    }

    private Builder findBuilder(final int buildNumber, final Date lastBuild, final Date now) throws CruiseControlException {
        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_7_10.coverage", "0f8cb1a9-c902-4229-8d36-536b4ed7350e");
        for (final Builder builder : builders) {
            writeline("/home/ubuntu/results/coverage/Schedule/Schedule_7_10.coverage", "3aa31c8d-672f-4c56-81db-6d2d6f356b9c");
            if (builder.isTimeBuilder()) {
                writeline("/home/ubuntu/results/coverage/Schedule/Schedule_7_10.coverage", "1ed64cb8-599d-484c-8b49-9358b48a871b");
                final int buildTime = builder.getTime();
                writeline("/home/ubuntu/results/coverage/Schedule/Schedule_7_10.coverage", "52a90444-913b-4bc3-b782-c9e9048b8d78");
                final boolean didntBuildToday = builderDidntBuildToday(lastBuild, now, buildTime);
                writeline("/home/ubuntu/results/coverage/Schedule/Schedule_7_10.coverage", "44a1e37f-de97-468a-9da0-9821bce67932");
                final int nowTime = DateUtil.getTimeFromDate(now);
                writeline("/home/ubuntu/results/coverage/Schedule/Schedule_7_10.coverage", "1e8d6965-63f5-455a-ae73-2c5b808438dc");
                final boolean isAfterBuildTime = buildTime <= nowTime;
                writeline("/home/ubuntu/results/coverage/Schedule/Schedule_7_10.coverage", "43e914e5-63a4-441a-b6d3-d64c1a01bbff");
                final boolean isValidDay = builder.isValidDay(now);
                writeline("/home/ubuntu/results/coverage/Schedule/Schedule_7_10.coverage", "922d28bf-ea78-46dd-8db8-4284226e6b1b");
                if (didntBuildToday && isAfterBuildTime && isValidDay) {
                    writeline("/home/ubuntu/results/coverage/Schedule/Schedule_7_10.coverage", "f8794278-7311-41a1-8457-8546bd1b0141");
                    return builder;
                }
            } else if (builder.getMultiple() > 0) {
                writeline("/home/ubuntu/results/coverage/Schedule/Schedule_7_10.coverage", "0a4bbcbc-c8ee-4b7b-82a4-46920ce8ec54");
                if (builder.isValidDay(now)) {
                    writeline("/home/ubuntu/results/coverage/Schedule/Schedule_7_10.coverage", "7d88240d-868f-4a78-a885-d96357b40208");
                    if ((buildNumber % builder.getMultiple()) == 0) {
                        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_7_10.coverage", "3e927f57-13e7-4b1d-b108-6d41b22929fb");
                        return builder;
                    }
                }
            } else {
                writeline("/home/ubuntu/results/coverage/Schedule/Schedule_7_10.coverage", "f18fb44f-57ae-4a9a-b1be-e651b5eaebfb");
                throw new CruiseControlException("The selected Builder is not properly configured");
            }
        }
        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_7_10.coverage", "008018db-bdde-4bcc-b9fc-7f6e4a8297a9");
        return null;
    }

    boolean builderDidntBuildToday(Date lastBuild, Date now, int buildTime) {
        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_7_10.coverage", "c24e7c51-748c-4055-b717-d0a5dc2c4651");
        int time = DateUtil.getTimeFromDate(now);
        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_7_10.coverage", "65def068-2381-49a6-9275-494431466ae0");
        long timeMillis = DateUtil.convertToMillis(time);
        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_7_10.coverage", "8fa59374-b570-4a3c-9d6f-2decfdac035d");
        long startOfToday = now.getTime() - timeMillis;
        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_7_10.coverage", "b99a4bfc-7bba-4fa3-b382-8df2f3449085");
        boolean lastBuildYesterday = lastBuild.getTime() < startOfToday;
        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_7_10.coverage", "fa242c84-a75b-4a77-853f-fb98a55eb3a7");
        boolean lastBuildTimeBeforeBuildTime = DateUtil.getTimeFromDate(lastBuild) < buildTime;
        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_7_10.coverage", "5cbbd02f-2b7a-4c63-973b-0f756b4c479d");
        return lastBuildYesterday || lastBuildTimeBeforeBuildTime;
    }

    long getTimeToNextBuild(Date now, long sleepInterval) {
        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_7_10.coverage", "f0ef10b3-fb5a-4b15-a30a-c1c6e93a206a");
        return getTimeToNextBuild(now, sleepInterval, 0);
    }

    private long getTimeToNextBuild(Date now, long sleepInterval, long priorPauseAdjustment) {
        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_7_10.coverage", "97cba5f3-dbfd-423d-bf5e-475f748a85ea");
        long timeToNextBuild = sleepInterval;
        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_7_10.coverage", "19de4079-8c55-465f-bcbf-8f461b8cfd7a");
        LOG.debug("getTimeToNextBuild: initial timeToNextBuild = " + timeToNextBuild);
        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_7_10.coverage", "12e0acc4-0cf8-4c18-8471-8c737479c807");
        timeToNextBuild = checkMultipleBuilders(now, timeToNextBuild);
        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_7_10.coverage", "d244edf3-96d3-4212-a99a-7c694c851427");
        LOG.debug("getTimeToNextBuild: after checkMultipleBuilders = " + timeToNextBuild);
        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_7_10.coverage", "1673d0cb-42b8-4cb2-aded-fc8de9042393");
        timeToNextBuild = checkTimeBuilders(now, timeToNextBuild);
        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_7_10.coverage", "43c662a6-3af0-42dd-9fdb-83aca0bee115");
        LOG.debug("getTimeToNextBuild: after checkTimeBuilders = " + timeToNextBuild);
        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_7_10.coverage", "9d2e7b05-ce5b-42c3-8d37-21d41efd191d");
        long timeTillNotPaused = checkPauseBuilders(now, timeToNextBuild);
        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_7_10.coverage", "9ee93195-e16a-43ba-aede-299c18b1aed3");
        LOG.debug("getTimeToNextBuild: after checkPauseBuilders = " + timeToNextBuild);
        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_7_10.coverage", "fc37dee7-1bfa-49ea-83a0-a84e714b4601");
        if (timeToNextBuild != timeTillNotPaused) {
            writeline("/home/ubuntu/results/coverage/Schedule/Schedule_7_10.coverage", "00506755-45b9-4740-990f-443ae6683319");
            boolean atMaxTime = timeTillNotPaused >= MAX_INTERVAL_MILLISECONDS || priorPauseAdjustment >= MAX_INTERVAL_MILLISECONDS;
            writeline("/home/ubuntu/results/coverage/Schedule/Schedule_7_10.coverage", "8d960375-aacb-40b8-b548-de21920d9f9f");
            if (hasOnlyTimeBuilders() && !atMaxTime) {
                writeline("/home/ubuntu/results/coverage/Schedule/Schedule_7_10.coverage", "f9fa46ab-0c1e-4c80-8f2f-fd8ecad18696");
                Date dateAfterPause = getFutureDate(now, timeTillNotPaused);
                writeline("/home/ubuntu/results/coverage/Schedule/Schedule_7_10.coverage", "d9186140-cf5c-4999-9bb9-ada01a06a463");
                long adjustmentFromEndOfPause = getTimeToNextBuild(dateAfterPause, 0, priorPauseAdjustment + timeTillNotPaused);
                writeline("/home/ubuntu/results/coverage/Schedule/Schedule_7_10.coverage", "35af7334-f590-4a76-98a6-a1dcec7e8d1b");
                timeToNextBuild = timeTillNotPaused + adjustmentFromEndOfPause;
                writeline("/home/ubuntu/results/coverage/Schedule/Schedule_7_10.coverage", "6add05dd-5500-4d05-9853-884439caff47");
                timeToNextBuild = checkMaximumInterval(timeToNextBuild);
            } else {
                writeline("/home/ubuntu/results/coverage/Schedule/Schedule_7_10.coverage", "0a2d98dd-46ea-40f0-aad4-2d518a6dc25f");
                timeToNextBuild = timeTillNotPaused;
            }
        }
        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_7_10.coverage", "78b91154-7206-41ce-9f5d-34a8bdebe8f2");
        return timeToNextBuild;
    }

    private long checkMultipleBuilders(final Date now, final long interval) {
        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_7_10.coverage", "fb6cdf8f-6857-4130-a91b-961e4c49769f");
        if (hasOnlyTimeBuilders()) {
            writeline("/home/ubuntu/results/coverage/Schedule/Schedule_7_10.coverage", "fd8652db-d867-4c4d-a581-30c5af6dbb6a");
            LOG.debug("has only time builders, so no correction for multiple builders.");
            writeline("/home/ubuntu/results/coverage/Schedule/Schedule_7_10.coverage", "a276a67d-d5c5-4344-9e98-bb811b2f9c2d");
            return interval;
        }
        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_7_10.coverage", "d9091108-1892-48ac-af9e-65b1b1c7ca40");
        Date then = getFutureDate(now, interval);
        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_7_10.coverage", "dd9793ac-28c0-4854-a1c0-e37d48fed83e");
        final List<Builder> buildersForOtherDays = new ArrayList<Builder>();
        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_7_10.coverage", "a9397439-6344-4237-8e21-032a8effa004");
        for (final Builder builder : builders) {
            writeline("/home/ubuntu/results/coverage/Schedule/Schedule_7_10.coverage", "f00b1f12-59fb-4fe1-9990-0a580db6f1cf");
            if (!builder.isTimeBuilder()) {
                writeline("/home/ubuntu/results/coverage/Schedule/Schedule_7_10.coverage", "bd09be8c-4582-4727-8289-c65b2c122f10");
                if (builder.getMultiple() == 1) {
                    writeline("/home/ubuntu/results/coverage/Schedule/Schedule_7_10.coverage", "48469fb2-ab1d-453c-82b8-bfb61d46b815");
                    if (builder.isValidDay(then)) {
                        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_7_10.coverage", "60217576-7d6c-44e1-8a44-13c3971f48e1");
                        LOG.debug("multiple=1 builder found that could run on " + then);
                        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_7_10.coverage", "d88c5a73-34ae-4a6d-8e8d-6fe6058dc78c");
                        return interval;
                    } else {
                        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_7_10.coverage", "3e2f54b3-1392-4b59-b697-f3006647c43a");
                        buildersForOtherDays.add(builder);
                    }
                }
            }
        }
        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_7_10.coverage", "1d299d1d-81a4-4103-8d43-3dd0c416806f");
        if (buildersForOtherDays.size() == 0) {
            writeline("/home/ubuntu/results/coverage/Schedule/Schedule_7_10.coverage", "4e1c37cf-01a9-4796-b0c7-16570d9548e7");
            LOG.error("configuration error: has some multiple builders but no multiple=1 builders found!");
            writeline("/home/ubuntu/results/coverage/Schedule/Schedule_7_10.coverage", "a9289c39-7a81-4b56-b3e1-dcc8166d5e3a");
            return interval;
        } else {
            writeline("/home/ubuntu/results/coverage/Schedule/Schedule_7_10.coverage", "ecfa72e2-4823-455d-aee1-abc7edb7fbe8");
            LOG.debug("no multiple=1 builders found for " + then + ". checking other days");
        }
        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_7_10.coverage", "b6d62c46-55af-4a59-a0cd-ecbbcd6dfd64");
        for (int i = 1; i < 7; i++) {
            writeline("/home/ubuntu/results/coverage/Schedule/Schedule_7_10.coverage", "4e3d98b7-2ecd-42b0-a93e-3a1577f3f11c");
            long daysPastInitialInterval = i * ONE_DAY;
            writeline("/home/ubuntu/results/coverage/Schedule/Schedule_7_10.coverage", "2bd69ca0-5fa8-42e0-bb2e-b5ed6f34b3ff");
            then = getFutureDate(now, interval + daysPastInitialInterval);
            writeline("/home/ubuntu/results/coverage/Schedule/Schedule_7_10.coverage", "5c1375d1-a141-4894-8b70-1ccae2953fd6");
            for (final Builder builder : builders) {
                writeline("/home/ubuntu/results/coverage/Schedule/Schedule_7_10.coverage", "6651de3c-9601-46b0-a591-925f57efebbe");
                if (builder.isValidDay(then)) {
                    writeline("/home/ubuntu/results/coverage/Schedule/Schedule_7_10.coverage", "b7fa827c-3b82-4c4c-b930-2ce2e76e957a");
                    LOG.debug("multiple=1 builder found that could run on " + then);
                    writeline("/home/ubuntu/results/coverage/Schedule/Schedule_7_10.coverage", "94c61dd6-5004-451f-9f93-12b9f1020f32");
                    long correctionToMidnight = getTimePastMidnight(then);
                    writeline("/home/ubuntu/results/coverage/Schedule/Schedule_7_10.coverage", "15ac8d2e-3d8a-42e1-9aff-0d9b1440bffa");
                    return interval + daysPastInitialInterval - correctionToMidnight;
                }
            }
        }
        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_7_10.coverage", "39c66345-f886-42c5-8493-4c6daf1944ab");
        LOG.error("configuration error? could not find appropriate multiple=1 builder.");
        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_7_10.coverage", "208b9f45-0f85-48a5-a706-4dc9d323f29a");
        return interval;
    }

    private long getTimePastMidnight(Date date) {
        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_7_10.coverage", "19e82dd3-bbd0-40e4-9265-16940e55dff6");
        Calendar cal = Calendar.getInstance();
        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_7_10.coverage", "37f220b6-a916-49f4-a2e6-6271167086ee");
        cal.setTime(date);
        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_7_10.coverage", "07e0e058-89f4-4a08-87d0-381b323a7c9f");
        long time = 60 * ONE_MINUTE * cal.get(Calendar.HOUR_OF_DAY);
        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_7_10.coverage", "5df9ee64-c877-4323-8539-d34e116d0197");
        time += ONE_MINUTE * cal.get(Calendar.MINUTE);
        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_7_10.coverage", "ca356a24-0df6-41a5-8223-864127fb5864");
        return time;
    }

    private boolean hasOnlyTimeBuilders() {
        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_7_10.coverage", "919ab92f-d27f-4a51-ad15-0336718a3cd5");
        boolean onlyTimeBuilders = true;
        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_7_10.coverage", "0bddbc69-8b00-4911-a3a4-c76c289dd1d8");
        for (final Builder builder : builders) {
            writeline("/home/ubuntu/results/coverage/Schedule/Schedule_7_10.coverage", "d22f7f1f-d3b5-4725-9707-3786ee63a3cd");
            if (!builder.isTimeBuilder()) {
                writeline("/home/ubuntu/results/coverage/Schedule/Schedule_7_10.coverage", "fefae9e9-cec7-416b-8434-930aaf03db9b");
                onlyTimeBuilders = false;
                writeline("/home/ubuntu/results/coverage/Schedule/Schedule_7_10.coverage", "21d3e76b-0b6f-439a-b5bb-c4e6f7f10586");
                break;
            }
        }
        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_7_10.coverage", "f479e33b-2ff0-4d32-893a-15da8117d486");
        return onlyTimeBuilders;
    }

    long checkTimeBuilders(final Date now, final long proposedTime) {
        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_7_10.coverage", "975608d8-fd32-491c-8682-ae07d6a3d4d3");
        long timeToNextBuild = proposedTime;
        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_7_10.coverage", "3edb6e41-09c8-4683-b1a7-eaecd9ca47f5");
        if (hasOnlyTimeBuilders()) {
            writeline("/home/ubuntu/results/coverage/Schedule/Schedule_7_10.coverage", "bfc7df12-b018-4bff-b3fa-02331aba0d86");
            timeToNextBuild = Long.MAX_VALUE;
        }
        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_7_10.coverage", "e666f54f-ab9b-4593-88be-73eb291c24d3");
        final int nowTime = DateUtil.getTimeFromDate(now);
        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_7_10.coverage", "a4b3c577-31ec-42c2-afee-98d59a9729f0");
        for (final Builder builder : builders) {
            writeline("/home/ubuntu/results/coverage/Schedule/Schedule_7_10.coverage", "e894723d-3df9-41ad-a09a-2dafedd464b4");
            if (builder.isTimeBuilder()) {
                writeline("/home/ubuntu/results/coverage/Schedule/Schedule_7_10.coverage", "d3589c01-a87f-49d3-b361-0f91f7c2b8cf");
                long timeToThisBuild = Long.MAX_VALUE;
                writeline("/home/ubuntu/results/coverage/Schedule/Schedule_7_10.coverage", "e7487db8-9f23-40c0-ad57-170fa57fc2b3");
                final Calendar cal = Calendar.getInstance();
                writeline("/home/ubuntu/results/coverage/Schedule/Schedule_7_10.coverage", "6a8805f1-a8ca-4c77-8903-24de205fad0a");
                final long oneYear = 365;
                writeline("/home/ubuntu/results/coverage/Schedule/Schedule_7_10.coverage", "68cc26a0-45c3-4a59-9d8e-d0af0c4f51d1");
                for (int daysInTheFuture = 0; daysInTheFuture < oneYear; daysInTheFuture++) {
                    writeline("/home/ubuntu/results/coverage/Schedule/Schedule_7_10.coverage", "3ec88ba7-1502-4244-8777-5f4c4e962550");
                    cal.setTime(now);
                    writeline("/home/ubuntu/results/coverage/Schedule/Schedule_7_10.coverage", "2994eeb3-73fd-4f8f-9b05-ffee03ef4f93");
                    cal.add(Calendar.DATE, daysInTheFuture);
                    writeline("/home/ubuntu/results/coverage/Schedule/Schedule_7_10.coverage", "5815db89-1dd6-4110-8266-a0f8446feddc");
                    Date future = cal.getTime();
                    writeline("/home/ubuntu/results/coverage/Schedule/Schedule_7_10.coverage", "a13c62f3-1e5b-4bdf-8696-0aeee47cd1e1");
                    final boolean dayIsValid = builder.isValidDay(future);
                    writeline("/home/ubuntu/results/coverage/Schedule/Schedule_7_10.coverage", "62e8f20a-e912-4375-9211-ac64c3e08920");
                    if (dayIsValid) {
                        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_7_10.coverage", "1f12aba9-bdc5-4b03-99d5-8f3b5e1a6aa9");
                        int thisBuildTime = builder.getTime();
                        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_7_10.coverage", "234209ed-59b8-49e7-b2fb-1e6976837d3e");
                        boolean timePassedToday = (daysInTheFuture == 0) && (nowTime > thisBuildTime);
                        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_7_10.coverage", "ed49bd28-7506-4100-8451-75df7b7c094d");
                        if (!timePassedToday) {
                            writeline("/home/ubuntu/results/coverage/Schedule/Schedule_7_10.coverage", "79e1db73-95dc-49a9-b9ba-b0457ac9da8a");
                            int buildHour = thisBuildTime / 100;
                            writeline("/home/ubuntu/results/coverage/Schedule/Schedule_7_10.coverage", "bc6aa674-0b4c-4380-9d9e-d61691826d2b");
                            int buildMinute = thisBuildTime % 100;
                            writeline("/home/ubuntu/results/coverage/Schedule/Schedule_7_10.coverage", "ef13e955-f533-48b4-9450-2301cfe771ff");
                            cal.set(Calendar.HOUR_OF_DAY, buildHour);
                            writeline("/home/ubuntu/results/coverage/Schedule/Schedule_7_10.coverage", "2c047b55-ddaa-40dc-bfa8-3f5df3ced27b");
                            cal.set(Calendar.MINUTE, buildMinute);
                            writeline("/home/ubuntu/results/coverage/Schedule/Schedule_7_10.coverage", "9dc7f0fc-b11f-484b-9feb-85646bdbc443");
                            future = cal.getTime();
                            writeline("/home/ubuntu/results/coverage/Schedule/Schedule_7_10.coverage", "917f79d6-4818-45b2-b4f2-8bd1255c383c");
                            timeToThisBuild = future.getTime() - now.getTime();
                            writeline("/home/ubuntu/results/coverage/Schedule/Schedule_7_10.coverage", "4f1e9599-bdc4-4ae3-8ebb-1dec5f368f06");
                            break;
                        }
                    }
                }
                writeline("/home/ubuntu/results/coverage/Schedule/Schedule_7_10.coverage", "5dfb5daf-9bbc-425d-a45b-6da981923b66");
                if (timeToThisBuild < timeToNextBuild) {
                    writeline("/home/ubuntu/results/coverage/Schedule/Schedule_7_10.coverage", "dafaf942-f987-43a2-8808-bfe5403baab0");
                    timeToNextBuild = timeToThisBuild;
                }
            }
        }
        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_7_10.coverage", "d04cfd86-37a7-4b4a-bc3c-25a4225d1cbb");
        if (timeToNextBuild > MAX_INTERVAL_MILLISECONDS) {
            writeline("/home/ubuntu/results/coverage/Schedule/Schedule_7_10.coverage", "1cd8b2e5-4299-4143-9982-812a8b1993c7");
            LOG.error("checkTimeBuilders exceeding maximum interval. using proposed value [" + proposedTime + "] instead");
            writeline("/home/ubuntu/results/coverage/Schedule/Schedule_7_10.coverage", "aa0a04c9-c121-4268-908c-fe37a69d252d");
            timeToNextBuild = proposedTime;
        }
        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_7_10.coverage", "711faae5-54a7-4fba-ab51-554aaf388b14");
        return timeToNextBuild;
    }

    long checkPauseBuilders(Date now, long proposedTime) {
        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_7_10.coverage", "a6fc914a-4bef-45d2-8b0b-84c22d6ac016");
        long oldTime = proposedTime;
        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_7_10.coverage", "a42858c5-5212-4456-b042-72a338b727e2");
        long newTime = checkForPauseAtProposedTime(now, oldTime);
        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_7_10.coverage", "a2dfd474-e3e3-47a6-beb5-5b6fcdc5e090");
        while (oldTime != newTime) {
            writeline("/home/ubuntu/results/coverage/Schedule/Schedule_7_10.coverage", "6ac3896a-ca4e-41d7-b24f-c50e72dc0580");
            oldTime = newTime;
            writeline("/home/ubuntu/results/coverage/Schedule/Schedule_7_10.coverage", "350e3d5f-f6bb-4974-92e9-0e1a5091bb92");
            newTime = checkForPauseAtProposedTime(now, oldTime);
        }
        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_7_10.coverage", "cd3dbde1-1228-4dcb-ba1f-083fb61e9361");
        return newTime;
    }

    private long checkForPauseAtProposedTime(Date now, long proposedTime) {
        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_7_10.coverage", "3e9fe8df-22d8-4ce1-80b5-2f898f542b60");
        Date futureDate = getFutureDate(now, proposedTime);
        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_7_10.coverage", "6b2d0224-c3a9-49c2-9c39-7d8c444b4987");
        PauseBuilder pause = findPause(futureDate);
        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_7_10.coverage", "be0eeca4-cfc1-46bf-ad89-536ef81e14ae");
        if (pause == null) {
            writeline("/home/ubuntu/results/coverage/Schedule/Schedule_7_10.coverage", "79b5e626-1695-47c7-a77b-28be7804a046");
            return proposedTime;
        }
        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_7_10.coverage", "94443b13-bf52-4ca9-8b67-ae01772933ba");
        int endPause = pause.getEndTime();
        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_7_10.coverage", "df64c9d8-5db4-4fbe-a797-8e8103ca85b6");
        int futureTime = DateUtil.getTimeFromDate(futureDate);
        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_7_10.coverage", "89eb1c99-1b2b-4d43-a96f-36d7e80edb79");
        long timeToEndOfPause = proposedTime + DateUtil.milliTimeDifference(futureTime, endPause);
        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_7_10.coverage", "a78fd577-d5fd-45bb-9cae-4702607e0a28");
        timeToEndOfPause = checkMaximumInterval(timeToEndOfPause);
        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_7_10.coverage", "8a7d96ae-846f-4204-8187-4e389b6dd030");
        return timeToEndOfPause == MAX_INTERVAL_MILLISECONDS ? timeToEndOfPause : timeToEndOfPause + ONE_MINUTE;
    }

    private long checkMaximumInterval(long timeToEndOfPause) {
        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_7_10.coverage", "bfd3e587-0645-4e26-9262-4cd7a011fc29");
        if (timeToEndOfPause > MAX_INTERVAL_MILLISECONDS) {
            writeline("/home/ubuntu/results/coverage/Schedule/Schedule_7_10.coverage", "6a3eb8e1-e0d0-4308-9c36-4be2f619c4c3");
            LOG.error("maximum interval exceeded! project perpetually paused?");
            writeline("/home/ubuntu/results/coverage/Schedule/Schedule_7_10.coverage", "02abb58b-c933-4754-b8fc-8a5833b0737a");
            return MAX_INTERVAL_MILLISECONDS;
        }
        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_7_10.coverage", "f8c09853-c7a5-4159-a5e1-08a35f05c3c5");
        return timeToEndOfPause;
    }

    private Date getFutureDate(Date now, long delay) {
        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_7_10.coverage", "b06dfb93-a0e4-4ed1-a29b-13854134e48c");
        long futureMillis = now.getTime() + delay;
        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_7_10.coverage", "33e4faab-f50c-4cb6-a0c6-78084096de67");
        return new Date(futureMillis);
    }

    public void setInterval(long intervalBetweenModificationChecks) {
        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_7_10.coverage", "101e4fbb-a0ee-444f-b166-bc221d64aee7");
        if (intervalBetweenModificationChecks <= 0) {
            writeline("/home/ubuntu/results/coverage/Schedule/Schedule_7_10.coverage", "b7caa088-d073-4ab9-abdc-559448915854");
            throw new IllegalArgumentException("interval must be greater than zero");
        }
        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_7_10.coverage", "e3888144-38c9-4640-841f-a060e126e3c9");
        interval = intervalBetweenModificationChecks * ONE_SECOND;
    }

    public long getInterval() {
        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_7_10.coverage", "14ca0e59-973d-4917-8363-e2ca7310cad2");
        return interval;
    }

    public void setShowProgress(final boolean showProgress) {
        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_7_10.coverage", "0907a228-d402-40ee-a782-d71b74fce1e5");
        this.showProgress = showProgress;
    }

    public boolean getShowProgress() {
        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_7_10.coverage", "8e6879e0-cccb-4eff-aef5-e13b97dfce2f");
        return showProgress;
    }

    public void validate() throws CruiseControlException {
        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_7_10.coverage", "f2de0a82-97cc-4436-bdcb-e804d71c6ba8");
        ValidationHelper.assertTrue(builders.size() > 0, "schedule element requires at least one nested builder element");
        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_7_10.coverage", "5547c03d-8a87-40e9-93c9-f9bdb5994f26");
        ValidationHelper.assertFalse(interval > ONE_YEAR, "maximum interval value is " + MAX_INTERVAL_SECONDS + " (one year)");
        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_7_10.coverage", "71bb80e6-e65e-42dc-a88f-4dbe660dd420");
        if (hasOnlyTimeBuilders()) {
            writeline("/home/ubuntu/results/coverage/Schedule/Schedule_7_10.coverage", "8858ed60-8e47-442b-b63f-779169681796");
            LOG.warn("schedule has all time based builders: interval value will be ignored.");
            writeline("/home/ubuntu/results/coverage/Schedule/Schedule_7_10.coverage", "27589a1f-683f-48cb-9782-f993d2b67bb1");
            ValidationHelper.assertFalse(checkWithinPause(new ArrayList<Builder>(builders)), "all build times during pauses.");
        }
        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_7_10.coverage", "85777073-7288-431c-a33a-73e423043306");
        // Validate the child builders, since no one else seems to be doing it.
        for (final Builder next : builders) {
            writeline("/home/ubuntu/results/coverage/Schedule/Schedule_7_10.coverage", "42002512-aa6b-408e-97b6-ce505f9a109d");
            next.validate();
        }
    }

    private boolean checkWithinPause(List timeBuilders) {
        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_7_10.coverage", "e94e7471-7794-4967-b78c-49f0443b0edb");
        for (int i = 0; i < timeBuilders.size(); i++) {
            writeline("/home/ubuntu/results/coverage/Schedule/Schedule_7_10.coverage", "54290fe1-4291-4842-9359-769a8120e575");
            Builder builder = (Builder) timeBuilders.get(i);
            writeline("/home/ubuntu/results/coverage/Schedule/Schedule_7_10.coverage", "258ee1d5-d1ff-40b8-aad3-449d5397ca30");
            for (final PauseBuilder pauseBuilder : pauseBuilders) {
                writeline("/home/ubuntu/results/coverage/Schedule/Schedule_7_10.coverage", "7d509f35-178b-4c20-9ff5-534ddd4ce8a9");
                if (buildDaySameAsPauseDay(builder, pauseBuilder) && buildTimeWithinPauseTime(builder, pauseBuilder)) {
                    writeline("/home/ubuntu/results/coverage/Schedule/Schedule_7_10.coverage", "a24ee795-6d95-416b-a7ae-9c4e221833fa");
                    timeBuilders.remove(builder);
                    writeline("/home/ubuntu/results/coverage/Schedule/Schedule_7_10.coverage", "2a54a924-899e-4f01-8503-ca34c1cd5a74");
                    StringBuffer message = new StringBuffer();
                    writeline("/home/ubuntu/results/coverage/Schedule/Schedule_7_10.coverage", "de396b3c-536a-44bd-a070-11d8dcb22e8c");
                    message.append("time Builder for time ");
                    writeline("/home/ubuntu/results/coverage/Schedule/Schedule_7_10.coverage", "ea21fc57-2558-407b-93be-d2af2c7b5072");
                    message.append(Integer.toString(builder.getTime()));
                    writeline("/home/ubuntu/results/coverage/Schedule/Schedule_7_10.coverage", "397ad4e3-ebfe-4d22-abac-d494b1d97661");
                    if (builder.getDay() != Builder.NOT_SET) {
                        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_7_10.coverage", "c01b6aca-dda7-43ad-9c81-bfc8f316d407");
                        message.append(" and day of ");
                        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_7_10.coverage", "e513b74e-1e06-495c-b9ee-0a3dce8633ab");
                        message.append(getDayString(builder.getDay()));
                    }
                    writeline("/home/ubuntu/results/coverage/Schedule/Schedule_7_10.coverage", "976bf6dc-16f6-4fc0-bc22-0ada4da39a85");
                    message.append(" is always within a pause and will never build");
                    writeline("/home/ubuntu/results/coverage/Schedule/Schedule_7_10.coverage", "935fc862-65b4-468c-a503-a55ae390b475");
                    LOG.error(message.toString());
                }
            }
        }
        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_7_10.coverage", "46a49dc5-58a6-4450-bbe4-cdb38f9dddb3");
        return timeBuilders.isEmpty();
    }

    /**
     * @param day
     * int value
     * @return english string value
     */
    String getDayString(int day) {
        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_7_10.coverage", "58da9a96-e7d3-4cae-bc48-7383cbf9ae30");
        if (day < 1 || day > 7) {
            writeline("/home/ubuntu/results/coverage/Schedule/Schedule_7_10.coverage", "a3958ad4-2b90-4aa5-a627-2dcb7b9ceaf9");
            throw new IllegalArgumentException("valid values of days are between 1 and 7, was " + day);
        }
        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_7_10.coverage", "5398179e-960a-4b72-8468-2c6564ee04c4");
        DateFormatSymbols symbols = new DateFormatSymbols(Locale.ENGLISH);
        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_7_10.coverage", "649d1dbd-793f-4e2d-9406-d56f360f24f6");
        String[] weekdays = symbols.getWeekdays();
        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_7_10.coverage", "4f27c647-fc56-4659-ab3c-6e00039999dd");
        return weekdays[day];
    }

    private boolean buildDaySameAsPauseDay(Builder builder, PauseBuilder pauseBuilder) {
        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_7_10.coverage", "883af1d7-a6b9-4a12-a30b-03324ac88c52");
        return pauseBuilder.getDay() == PauseBuilder.NOT_SET || pauseBuilder.getDay() == builder.getDay();
    }

    private boolean buildTimeWithinPauseTime(Builder builder, PauseBuilder pauseBuilder) {
        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_7_10.coverage", "d9ba7934-d5e3-4389-b595-f04ccbc29627");
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
        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_7_10.coverage", "356cd32e-2360-45e6-8593-7fce8cfd048d");
        if (param == null) {
            writeline("/home/ubuntu/results/coverage/Schedule/Schedule_7_10.coverage", "51f62e18-ba06-4a8f-8bd5-54b56cb759c8");
            throw new IllegalArgumentException(paramName + " can't be null");
        }
    }

    public List getBuilders() {
        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_7_10.coverage", "ea636da8-0cbc-4f6a-b4a3-657607689879");
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
