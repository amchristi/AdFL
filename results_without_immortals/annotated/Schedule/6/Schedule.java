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
        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_6_10.coverage", "b23567b9-0f5a-4524-ae4b-1e3eb9a6be01");
        checkParamNotNull("builder", builder);
        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_6_10.coverage", "dc83395e-ca24-4d88-aee6-90c6b77e7f25");
        builders.add(builder);
        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_6_10.coverage", "f4977475-0427-4027-ae5b-6309d7e90fd6");
        Collections.sort(builders, builderComparator);
    }

    public void add(final PauseBuilder pause) {
        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_6_10.coverage", "af74bf53-ae0c-482d-98f0-7537b8b24d06");
        checkParamNotNull("pauseBuilder", pause);
        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_6_10.coverage", "3358fa84-a231-43ed-b960-b46e95475db4");
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
        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_6_10.coverage", "95848102-0a45-4c1c-842a-701755167868");
        checkParamNotNull("date", now);
        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_6_10.coverage", "1dcba3c6-9577-4a25-a7e3-ac89050a23f6");
        PauseBuilder pause = findPause(now);
        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_6_10.coverage", "2095d470-e6fc-4ccc-9147-13d839873add");
        if (pause != null) {
            writeline("/home/ubuntu/results/coverage/Schedule/Schedule_6_10.coverage", "02f49e65-aac5-44be-a575-3b72eb4e3a2f");
            LOG.info("CruiseControl is paused until: " + getEndTimeString(pause));
            writeline("/home/ubuntu/results/coverage/Schedule/Schedule_6_10.coverage", "39903e89-caf8-4cc2-83bc-15bce9f8b522");
            return true;
        }
        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_6_10.coverage", "92fd6eed-3216-4c73-91fa-54935026f06d");
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
        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_6_10.coverage", "ebbf7072-3d4f-4321-b41f-41a3e705ad0f");
        final Calendar cal = Calendar.getInstance();
        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_6_10.coverage", "2d6cbe8e-1984-4013-bf47-6969ea22ca40");
        cal.set(Calendar.HOUR_OF_DAY, builder.getEndTime() / 100);
        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_6_10.coverage", "58a9b994-eff2-412a-8bcb-1eda8fdd5c15");
        cal.set(Calendar.MINUTE, builder.getEndTime() % 100);
        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_6_10.coverage", "db946d27-6fd2-47c0-8ec9-29175f1a2986");
        cal.add(Calendar.MINUTE, 1);
        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_6_10.coverage", "a0adbcba-4c71-4e87-9bc9-0eec40ea60e7");
        return timeFormatter.format(cal.getTime());
    }

    PauseBuilder findPause(final Date date) {
        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_6_10.coverage", "a8193397-ef06-4b75-909b-6b4964896402");
        checkParamNotNull("date", date);
        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_6_10.coverage", "190d2812-0e5e-4c3a-8d8d-d47dc51d4e70");
        for (final PauseBuilder pause : pauseBuilders) {
            writeline("/home/ubuntu/results/coverage/Schedule/Schedule_6_10.coverage", "ab09ae73-a8c8-4aab-9241-ffe228569f7b");
            if (pause.isPaused(date)) {
                writeline("/home/ubuntu/results/coverage/Schedule/Schedule_6_10.coverage", "685844ea-1cbf-4e2a-9803-fb421ae293df");
                return pause;
            }
        }
        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_6_10.coverage", "20b1cd1a-4604-4161-b1e4-6347325b3a9d");
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
        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_6_10.coverage", "b8588463-3ff6-4a6d-b17b-f003c284f19c");
        final Builder builder = selectBuilder(buildNumber, lastBuild, now);
        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_6_10.coverage", "cee7d1d1-234a-4050-9607-0180b2612fbe");
        if (buildTarget != null) {
            writeline("/home/ubuntu/results/coverage/Schedule/Schedule_6_10.coverage", "e054b355-3214-4f19-8626-2835eb643fda");
            LOG.info("Overriding build target with \"" + buildTarget + "\"");
            writeline("/home/ubuntu/results/coverage/Schedule/Schedule_6_10.coverage", "80be4b8c-f43a-4864-a2da-58419670ac62");
            return builder.buildWithTarget(properties, buildTarget, (getShowProgress() ? progress : null));
        }
        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_6_10.coverage", "32b86da7-569d-4249-afcf-7ef13feebec9");
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
        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_6_10.coverage", "98f75dc8-afda-41b2-963c-253dc86ead8b");
        Builder builder = findBuilder(buildNumber, lastBuild, now);
        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_6_10.coverage", "afa03b92-2b15-49fe-8685-6987ca11dd77");
        if (builder == null) {
            writeline("/home/ubuntu/results/coverage/Schedule/Schedule_6_10.coverage", "0770d043-c24a-4f37-bb60-9c587d98f889");
            final long timeToNextBuild = getTimeToNextBuild(now, ONE_MINUTE);
            writeline("/home/ubuntu/results/coverage/Schedule/Schedule_6_10.coverage", "25bfa250-ceda-4e0b-96cb-add95f8c14d1");
            final Date futureDate = getFutureDate(now, timeToNextBuild);
            writeline("/home/ubuntu/results/coverage/Schedule/Schedule_6_10.coverage", "71d0142c-30ee-4177-b85f-ffe55b1b8005");
            builder = findBuilder(buildNumber, now, futureDate);
        }
        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_6_10.coverage", "c871a923-1912-4e0b-b075-2a5be53e4b62");
        if (builder == null) {
            writeline("/home/ubuntu/results/coverage/Schedule/Schedule_6_10.coverage", "94f972c3-13b6-42fb-90b4-add593afb750");
            validate();
            writeline("/home/ubuntu/results/coverage/Schedule/Schedule_6_10.coverage", "4e793606-1853-4949-83b0-97c9b5ce8b7d");
            throw new CruiseControlException("configuration error not caught by validate? no builder selected");
        }
        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_6_10.coverage", "141e5096-614a-49d0-985b-98cad9ebf77a");
        return builder;
    }

    private Builder findBuilder(final int buildNumber, final Date lastBuild, final Date now) throws CruiseControlException {
        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_6_10.coverage", "e1a8cf40-c82d-4cff-96ec-1cbe33552e65");
        for (final Builder builder : builders) {
            writeline("/home/ubuntu/results/coverage/Schedule/Schedule_6_10.coverage", "02bdc5d7-8630-48f2-b4bd-01ec260d6570");
            if (builder.isTimeBuilder()) {
                writeline("/home/ubuntu/results/coverage/Schedule/Schedule_6_10.coverage", "7ca4ddb7-3a0d-4231-bacc-8e2e2a311d8a");
                final int buildTime = builder.getTime();
                writeline("/home/ubuntu/results/coverage/Schedule/Schedule_6_10.coverage", "9b0733d3-198b-4184-a842-090e61693318");
                final boolean didntBuildToday = builderDidntBuildToday(lastBuild, now, buildTime);
                writeline("/home/ubuntu/results/coverage/Schedule/Schedule_6_10.coverage", "b622e57c-5cd0-458d-a980-d1404fc455b0");
                final int nowTime = DateUtil.getTimeFromDate(now);
                writeline("/home/ubuntu/results/coverage/Schedule/Schedule_6_10.coverage", "d16516b6-e9c6-41cf-9b83-f70278fa23c8");
                final boolean isAfterBuildTime = buildTime <= nowTime;
                writeline("/home/ubuntu/results/coverage/Schedule/Schedule_6_10.coverage", "14904514-5e5b-4639-8369-8e60edec155e");
                final boolean isValidDay = builder.isValidDay(now);
                writeline("/home/ubuntu/results/coverage/Schedule/Schedule_6_10.coverage", "a9826249-75e7-4868-93c8-28b1129e02f4");
                if (didntBuildToday && isAfterBuildTime && isValidDay) {
                    writeline("/home/ubuntu/results/coverage/Schedule/Schedule_6_10.coverage", "e9c1c034-c75e-4090-9391-611bb1388a77");
                    return builder;
                }
            } else if (builder.getMultiple() > 0) {
                writeline("/home/ubuntu/results/coverage/Schedule/Schedule_6_10.coverage", "8b9c7d5d-9dd6-4a8f-9379-e56f654823f5");
                if (builder.isValidDay(now)) {
                    writeline("/home/ubuntu/results/coverage/Schedule/Schedule_6_10.coverage", "f0068096-cf16-474a-8f84-555f82b307c2");
                    if ((buildNumber % builder.getMultiple()) == 0) {
                        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_6_10.coverage", "e5ee4823-27e5-4c4e-95c5-34c5ebc006c6");
                        return builder;
                    }
                }
            } else {
                writeline("/home/ubuntu/results/coverage/Schedule/Schedule_6_10.coverage", "4c046d53-5abe-493f-b26b-8f228ba8e286");
                throw new CruiseControlException("The selected Builder is not properly configured");
            }
        }
        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_6_10.coverage", "25f7fc99-937f-4338-a180-4934fcebd191");
        return null;
    }

    boolean builderDidntBuildToday(Date lastBuild, Date now, int buildTime) {
        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_6_10.coverage", "42dc8867-8e8b-42bc-a957-a32bff4382c0");
        int time = DateUtil.getTimeFromDate(now);
        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_6_10.coverage", "2020a6d7-a06d-4cf3-834f-4ecfb9692f2d");
        long timeMillis = DateUtil.convertToMillis(time);
        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_6_10.coverage", "074c3084-821c-4f4a-b233-02dbf397215c");
        long startOfToday = now.getTime() - timeMillis;
        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_6_10.coverage", "4b55f167-40b2-443c-a6e8-052adb389a3d");
        boolean lastBuildYesterday = lastBuild.getTime() < startOfToday;
        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_6_10.coverage", "322844a9-5de1-4706-96cb-1acd42a87273");
        boolean lastBuildTimeBeforeBuildTime = DateUtil.getTimeFromDate(lastBuild) < buildTime;
        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_6_10.coverage", "e87d643a-9eb7-41c3-9d33-c9e8506deab2");
        return lastBuildYesterday || lastBuildTimeBeforeBuildTime;
    }

    long getTimeToNextBuild(Date now, long sleepInterval) {
        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_6_10.coverage", "c78fbaf5-6c11-40cf-a422-99909a6166b7");
        return getTimeToNextBuild(now, sleepInterval, 0);
    }

    private long getTimeToNextBuild(Date now, long sleepInterval, long priorPauseAdjustment) {
        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_6_10.coverage", "067ccc26-7255-474e-adc4-ab107791bea5");
        long timeToNextBuild = sleepInterval;
        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_6_10.coverage", "7b045645-c2c9-4e38-835f-69300e24da08");
        LOG.debug("getTimeToNextBuild: initial timeToNextBuild = " + timeToNextBuild);
        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_6_10.coverage", "30677a85-50b6-4533-bda5-f2f0af6af443");
        timeToNextBuild = checkMultipleBuilders(now, timeToNextBuild);
        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_6_10.coverage", "a83abb1c-528a-4210-87f3-1ab76fc0885c");
        LOG.debug("getTimeToNextBuild: after checkMultipleBuilders = " + timeToNextBuild);
        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_6_10.coverage", "4eecf71c-6793-42e8-a275-22d03b44f599");
        timeToNextBuild = checkTimeBuilders(now, timeToNextBuild);
        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_6_10.coverage", "f792b0e0-4222-44c9-b7a2-f8d165f5977a");
        LOG.debug("getTimeToNextBuild: after checkTimeBuilders = " + timeToNextBuild);
        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_6_10.coverage", "f9ed1a81-8781-45cb-b7b4-6dba127cf37b");
        long timeTillNotPaused = checkPauseBuilders(now, timeToNextBuild);
        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_6_10.coverage", "cd14d378-ec3b-4ba5-a9a8-dc791242c3b0");
        LOG.debug("getTimeToNextBuild: after checkPauseBuilders = " + timeToNextBuild);
        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_6_10.coverage", "2cd95c5f-62f5-4dc2-8e13-612c98dab3f9");
        if (timeToNextBuild != timeTillNotPaused) {
            writeline("/home/ubuntu/results/coverage/Schedule/Schedule_6_10.coverage", "a0af6870-f2cc-4c9c-9cb8-d8556de0c7bf");
            boolean atMaxTime = timeTillNotPaused >= MAX_INTERVAL_MILLISECONDS || priorPauseAdjustment >= MAX_INTERVAL_MILLISECONDS;
            writeline("/home/ubuntu/results/coverage/Schedule/Schedule_6_10.coverage", "21158e21-59e9-44d4-9417-08277414294d");
            if (hasOnlyTimeBuilders() && !atMaxTime) {
                writeline("/home/ubuntu/results/coverage/Schedule/Schedule_6_10.coverage", "5d384195-c4ea-44b3-894a-68dc07dcf3ce");
                Date dateAfterPause = getFutureDate(now, timeTillNotPaused);
                writeline("/home/ubuntu/results/coverage/Schedule/Schedule_6_10.coverage", "4909c64e-105f-4a3f-b91a-41ba51e599c5");
                long adjustmentFromEndOfPause = getTimeToNextBuild(dateAfterPause, 0, priorPauseAdjustment + timeTillNotPaused);
                writeline("/home/ubuntu/results/coverage/Schedule/Schedule_6_10.coverage", "64830784-82e3-4d75-be48-4afb85d52ef8");
                timeToNextBuild = timeTillNotPaused + adjustmentFromEndOfPause;
                writeline("/home/ubuntu/results/coverage/Schedule/Schedule_6_10.coverage", "49c140d0-2065-4377-a027-75c435188daa");
                timeToNextBuild = checkMaximumInterval(timeToNextBuild);
            } else {
                writeline("/home/ubuntu/results/coverage/Schedule/Schedule_6_10.coverage", "7d07c7c7-dc4b-40fb-bb38-aa9d57f4a1d0");
                timeToNextBuild = timeTillNotPaused;
            }
        }
        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_6_10.coverage", "70e0d202-b601-44d3-86f1-f8fbc700de87");
        return timeToNextBuild;
    }

    private long checkMultipleBuilders(final Date now, final long interval) {
        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_6_10.coverage", "b8b89e48-f403-4b17-9f79-2c1f5e318930");
        if (hasOnlyTimeBuilders()) {
            writeline("/home/ubuntu/results/coverage/Schedule/Schedule_6_10.coverage", "a5484c21-d3e1-4bf0-9969-a1b8350ce17f");
            LOG.debug("has only time builders, so no correction for multiple builders.");
            writeline("/home/ubuntu/results/coverage/Schedule/Schedule_6_10.coverage", "833a16a0-c11b-48fc-b356-8fb4626ae784");
            return interval;
        }
        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_6_10.coverage", "9431b001-52bc-4d36-98db-d1b6b5e9de02");
        Date then = getFutureDate(now, interval);
        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_6_10.coverage", "7faf2d3f-2e46-45a7-98e1-f872b190da03");
        final List<Builder> buildersForOtherDays = new ArrayList<Builder>();
        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_6_10.coverage", "f31d1af7-8449-41c0-bdd7-dea4269329b1");
        for (final Builder builder : builders) {
            writeline("/home/ubuntu/results/coverage/Schedule/Schedule_6_10.coverage", "8beef1a8-099f-48d3-ad7c-aeee872ee932");
            if (!builder.isTimeBuilder()) {
                writeline("/home/ubuntu/results/coverage/Schedule/Schedule_6_10.coverage", "4afd2bca-c38a-41d2-9284-f4f56854bd47");
                if (builder.getMultiple() == 1) {
                    writeline("/home/ubuntu/results/coverage/Schedule/Schedule_6_10.coverage", "16c64476-5d0b-4a07-9b22-6b8f7c0cc4e5");
                    if (builder.isValidDay(then)) {
                        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_6_10.coverage", "484534f6-7fbf-4ded-987b-d1aafc0b9502");
                        LOG.debug("multiple=1 builder found that could run on " + then);
                        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_6_10.coverage", "588302ff-1fc2-4544-bd8e-8b1597487f82");
                        return interval;
                    } else {
                        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_6_10.coverage", "a8353a21-48cd-45c7-9d9b-05fa8255fffd");
                        buildersForOtherDays.add(builder);
                    }
                }
            }
        }
        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_6_10.coverage", "81a55445-22a2-43b0-a51d-edd89e3edb9c");
        if (buildersForOtherDays.size() == 0) {
            writeline("/home/ubuntu/results/coverage/Schedule/Schedule_6_10.coverage", "60eac0ee-9c82-419c-9ba1-d4549f47234d");
            LOG.error("configuration error: has some multiple builders but no multiple=1 builders found!");
            writeline("/home/ubuntu/results/coverage/Schedule/Schedule_6_10.coverage", "c817c6fb-81fb-472b-b079-e787d83d090e");
            return interval;
        } else {
            writeline("/home/ubuntu/results/coverage/Schedule/Schedule_6_10.coverage", "37c74e18-344b-404f-8df3-0dcc8ab86526");
            LOG.debug("no multiple=1 builders found for " + then + ". checking other days");
        }
        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_6_10.coverage", "5e3a77b2-8e33-49dd-bdf5-71b44b609391");
        for (int i = 1; i < 7; i++) {
            writeline("/home/ubuntu/results/coverage/Schedule/Schedule_6_10.coverage", "b4e1b4a3-86a9-4169-b255-b12a46ba8f4f");
            long daysPastInitialInterval = i * ONE_DAY;
            writeline("/home/ubuntu/results/coverage/Schedule/Schedule_6_10.coverage", "ed96c643-5884-453e-bb7c-b68b6aa1f858");
            then = getFutureDate(now, interval + daysPastInitialInterval);
            writeline("/home/ubuntu/results/coverage/Schedule/Schedule_6_10.coverage", "799f2527-b597-4758-86cd-4d12949a707b");
            for (final Builder builder : builders) {
                writeline("/home/ubuntu/results/coverage/Schedule/Schedule_6_10.coverage", "44229960-bbf9-4120-ba85-1446e861ba9e");
                if (builder.isValidDay(then)) {
                    writeline("/home/ubuntu/results/coverage/Schedule/Schedule_6_10.coverage", "a12b6a2a-9e80-4489-8e30-fbf43aa33ffe");
                    LOG.debug("multiple=1 builder found that could run on " + then);
                    writeline("/home/ubuntu/results/coverage/Schedule/Schedule_6_10.coverage", "6ec01f46-e9c3-4382-b8a0-2e9034d4e373");
                    long correctionToMidnight = getTimePastMidnight(then);
                    writeline("/home/ubuntu/results/coverage/Schedule/Schedule_6_10.coverage", "ef19e17c-9d85-47b3-b1d2-0fa7c19a8131");
                    return interval + daysPastInitialInterval - correctionToMidnight;
                }
            }
        }
        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_6_10.coverage", "5b73b7c8-5426-4893-90da-51e373542de1");
        LOG.error("configuration error? could not find appropriate multiple=1 builder.");
        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_6_10.coverage", "c6230b8a-6ef3-43a2-b8ba-25179860b8fb");
        return interval;
    }

    private long getTimePastMidnight(Date date) {
        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_6_10.coverage", "0ba8c6b7-86e3-401d-b4f8-1c94dce0cc80");
        Calendar cal = Calendar.getInstance();
        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_6_10.coverage", "3a157f34-3ecf-4d38-939c-216b8b48eff6");
        cal.setTime(date);
        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_6_10.coverage", "698f3fc7-6ec8-422c-bcc4-9f7c8431c9a0");
        long time = 60 * ONE_MINUTE * cal.get(Calendar.HOUR_OF_DAY);
        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_6_10.coverage", "0b78b095-0980-405a-8078-a3ec504d94e6");
        time += ONE_MINUTE * cal.get(Calendar.MINUTE);
        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_6_10.coverage", "76184860-c3f1-4852-ab19-a83369e719b7");
        return time;
    }

    private boolean hasOnlyTimeBuilders() {
        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_6_10.coverage", "20b31a5a-00a4-45be-aaab-028501e9ce02");
        boolean onlyTimeBuilders = true;
        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_6_10.coverage", "a036bd89-72da-4111-8b8f-2e404cb0e5e9");
        for (final Builder builder : builders) {
            writeline("/home/ubuntu/results/coverage/Schedule/Schedule_6_10.coverage", "995d622a-8473-43b1-b607-aadc59edefd4");
            if (!builder.isTimeBuilder()) {
                writeline("/home/ubuntu/results/coverage/Schedule/Schedule_6_10.coverage", "fa7c2da8-8374-4d44-9103-c5f80675bbbc");
                onlyTimeBuilders = false;
                writeline("/home/ubuntu/results/coverage/Schedule/Schedule_6_10.coverage", "6540fd04-9689-4fa8-a29d-4eff4ecf37d1");
                break;
            }
        }
        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_6_10.coverage", "111a357f-30f4-4b58-ba26-6db3be5c1930");
        return onlyTimeBuilders;
    }

    long checkTimeBuilders(final Date now, final long proposedTime) {
        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_6_10.coverage", "bc741d57-4e87-49b6-8314-cfdb2ca410c4");
        long timeToNextBuild = proposedTime;
        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_6_10.coverage", "cc273e3d-3516-4a94-93a8-6b74a9364a87");
        if (hasOnlyTimeBuilders()) {
            writeline("/home/ubuntu/results/coverage/Schedule/Schedule_6_10.coverage", "f057e3de-0416-463f-85b1-16c9a63649d2");
            timeToNextBuild = Long.MAX_VALUE;
        }
        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_6_10.coverage", "b5168177-d490-4a02-874e-941d37cb9f48");
        final int nowTime = DateUtil.getTimeFromDate(now);
        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_6_10.coverage", "86767ef5-dece-4aae-8c46-80f1a365dd17");
        for (final Builder builder : builders) {
            writeline("/home/ubuntu/results/coverage/Schedule/Schedule_6_10.coverage", "533903e2-54d0-4b96-8874-c016810e1d47");
            if (builder.isTimeBuilder()) {
                writeline("/home/ubuntu/results/coverage/Schedule/Schedule_6_10.coverage", "505eef2f-d748-428e-85be-5d60817962a1");
                long timeToThisBuild = Long.MAX_VALUE;
                writeline("/home/ubuntu/results/coverage/Schedule/Schedule_6_10.coverage", "50426863-ceb4-4aa5-98ab-1e47fe2b39f7");
                final Calendar cal = Calendar.getInstance();
                writeline("/home/ubuntu/results/coverage/Schedule/Schedule_6_10.coverage", "359b8ef4-6dc0-42e9-b746-1b68b76ba9c5");
                final long oneYear = 365;
                writeline("/home/ubuntu/results/coverage/Schedule/Schedule_6_10.coverage", "c5c99fe2-3b42-4bf8-9512-32b30f78ca9e");
                for (int daysInTheFuture = 0; daysInTheFuture < oneYear; daysInTheFuture++) {
                    writeline("/home/ubuntu/results/coverage/Schedule/Schedule_6_10.coverage", "300744db-4664-4512-b8d8-c8c3628dd1bf");
                    cal.setTime(now);
                    writeline("/home/ubuntu/results/coverage/Schedule/Schedule_6_10.coverage", "16780563-33b1-4f9d-8cc3-fd00ddd57f23");
                    cal.add(Calendar.DATE, daysInTheFuture);
                    writeline("/home/ubuntu/results/coverage/Schedule/Schedule_6_10.coverage", "1f74dc96-cd50-45b6-abfa-94b7097d6a60");
                    Date future = cal.getTime();
                    writeline("/home/ubuntu/results/coverage/Schedule/Schedule_6_10.coverage", "c91ebdb4-4051-49b3-96a7-caafdd2e4aae");
                    final boolean dayIsValid = builder.isValidDay(future);
                    writeline("/home/ubuntu/results/coverage/Schedule/Schedule_6_10.coverage", "72875848-a975-4552-ad38-108748ca0074");
                    if (dayIsValid) {
                        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_6_10.coverage", "266c467d-cfc8-48b7-ac7c-32f65a5b6935");
                        int thisBuildTime = builder.getTime();
                        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_6_10.coverage", "b69c4e54-01f1-4885-93dc-6d6b797873db");
                        boolean timePassedToday = (daysInTheFuture == 0) && (nowTime > thisBuildTime);
                        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_6_10.coverage", "02f9fd51-7e20-4b47-8b6b-6e923c0a9422");
                        if (!timePassedToday) {
                            writeline("/home/ubuntu/results/coverage/Schedule/Schedule_6_10.coverage", "8f4d2f54-0684-4f54-b1c8-f9ecdb11a1e8");
                            int buildHour = thisBuildTime / 100;
                            writeline("/home/ubuntu/results/coverage/Schedule/Schedule_6_10.coverage", "a2fefbae-11f0-4972-955e-716754b0920e");
                            int buildMinute = thisBuildTime % 100;
                            writeline("/home/ubuntu/results/coverage/Schedule/Schedule_6_10.coverage", "ee2f04e7-e328-419f-a09a-c6c37f9b350e");
                            cal.set(Calendar.HOUR_OF_DAY, buildHour);
                            writeline("/home/ubuntu/results/coverage/Schedule/Schedule_6_10.coverage", "e64f09d6-515a-4301-9430-bd243f0aee9c");
                            cal.set(Calendar.MINUTE, buildMinute);
                            writeline("/home/ubuntu/results/coverage/Schedule/Schedule_6_10.coverage", "20ac182b-801d-40fa-846c-631893ac60c6");
                            future = cal.getTime();
                            writeline("/home/ubuntu/results/coverage/Schedule/Schedule_6_10.coverage", "0de15e2b-996d-4112-87e8-1d0450128c19");
                            timeToThisBuild = future.getTime() - now.getTime();
                            writeline("/home/ubuntu/results/coverage/Schedule/Schedule_6_10.coverage", "9b771c0c-9fce-4bca-b083-a809fc3b1ea8");
                            break;
                        }
                    }
                }
                writeline("/home/ubuntu/results/coverage/Schedule/Schedule_6_10.coverage", "a2743225-3d74-4a46-87db-63481a83447b");
                if (timeToThisBuild < timeToNextBuild) {
                    writeline("/home/ubuntu/results/coverage/Schedule/Schedule_6_10.coverage", "83934da7-44dc-41fc-a906-f8c0232c531a");
                    timeToNextBuild = timeToThisBuild;
                }
            }
        }
        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_6_10.coverage", "18055a68-9b7d-4b5e-808b-754028a7ad5e");
        if (timeToNextBuild > MAX_INTERVAL_MILLISECONDS) {
            writeline("/home/ubuntu/results/coverage/Schedule/Schedule_6_10.coverage", "76dde50d-3933-42f1-97fb-4525c9046874");
            LOG.error("checkTimeBuilders exceeding maximum interval. using proposed value [" + proposedTime + "] instead");
            writeline("/home/ubuntu/results/coverage/Schedule/Schedule_6_10.coverage", "1e16efe5-ca84-4aed-bc07-dc4fa1c84efd");
            timeToNextBuild = proposedTime;
        }
        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_6_10.coverage", "a2fda235-a8b8-4397-b791-be775f0a9824");
        return timeToNextBuild;
    }

    long checkPauseBuilders(Date now, long proposedTime) {
        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_6_10.coverage", "7319adad-4350-45db-9035-41c74faff87f");
        long oldTime = proposedTime;
        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_6_10.coverage", "8c7221e1-9a56-4bcd-9506-07a78efe070a");
        long newTime = checkForPauseAtProposedTime(now, oldTime);
        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_6_10.coverage", "1ab686ef-871d-4f5c-a889-b9e470a9a551");
        while (oldTime != newTime) {
            writeline("/home/ubuntu/results/coverage/Schedule/Schedule_6_10.coverage", "ada33164-5802-46d3-aa9b-53684f5b0744");
            oldTime = newTime;
            writeline("/home/ubuntu/results/coverage/Schedule/Schedule_6_10.coverage", "d6dc37c5-d48a-4666-b780-20a88edc3ad6");
            newTime = checkForPauseAtProposedTime(now, oldTime);
        }
        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_6_10.coverage", "2e3debd4-6cae-40d4-94fc-80987241e822");
        return newTime;
    }

    private long checkForPauseAtProposedTime(Date now, long proposedTime) {
        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_6_10.coverage", "c7aa5898-f02a-4fda-acfa-ac92cb55d668");
        Date futureDate = getFutureDate(now, proposedTime);
        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_6_10.coverage", "7773b538-eeb2-4ea3-b02a-128ae5546d58");
        PauseBuilder pause = findPause(futureDate);
        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_6_10.coverage", "71c22c0c-d71e-464b-a31f-5c2352737c11");
        if (pause == null) {
            writeline("/home/ubuntu/results/coverage/Schedule/Schedule_6_10.coverage", "f5aaa848-3f7c-4c12-916a-d58fceb972c9");
            return proposedTime;
        }
        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_6_10.coverage", "0e057375-a95b-47d2-ba56-bce5e6ba405f");
        int endPause = pause.getEndTime();
        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_6_10.coverage", "0832891f-f809-4d22-8aa5-be63a62701f9");
        int futureTime = DateUtil.getTimeFromDate(futureDate);
        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_6_10.coverage", "016ba8e0-3f6d-4d21-9b6b-b01ea6e4da06");
        long timeToEndOfPause = proposedTime + DateUtil.milliTimeDifference(futureTime, endPause);
        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_6_10.coverage", "21b1b6dd-56e4-4a11-8cca-86a49ef7b5b4");
        timeToEndOfPause = checkMaximumInterval(timeToEndOfPause);
        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_6_10.coverage", "710b2f6c-68fe-439e-a1a1-bbe25afaeaa8");
        return timeToEndOfPause == MAX_INTERVAL_MILLISECONDS ? timeToEndOfPause : timeToEndOfPause + ONE_MINUTE;
    }

    private long checkMaximumInterval(long timeToEndOfPause) {
        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_6_10.coverage", "4a060a44-5225-4e51-b0f3-1a756bdc2a70");
        if (timeToEndOfPause > MAX_INTERVAL_MILLISECONDS) {
            writeline("/home/ubuntu/results/coverage/Schedule/Schedule_6_10.coverage", "4b035858-6f00-4a75-a922-b1f478ed37a5");
            LOG.error("maximum interval exceeded! project perpetually paused?");
            writeline("/home/ubuntu/results/coverage/Schedule/Schedule_6_10.coverage", "e38aa635-e9fd-449f-8592-484d5da01bd6");
            return MAX_INTERVAL_MILLISECONDS;
        }
        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_6_10.coverage", "5dc53c50-4fa1-4362-b1fa-4a1b50c4c464");
        return timeToEndOfPause;
    }

    private Date getFutureDate(Date now, long delay) {
        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_6_10.coverage", "ceb233fa-4c2f-4745-924f-097989d6e1d0");
        long futureMillis = now.getTime() + delay;
        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_6_10.coverage", "d418404d-c9c6-4ece-bf53-a8f6fd206bd6");
        return new Date(futureMillis);
    }

    public void setInterval(long intervalBetweenModificationChecks) {
        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_6_10.coverage", "8b29046b-6948-4028-aeb4-1753ff173bb4");
        if (intervalBetweenModificationChecks <= 0) {
            writeline("/home/ubuntu/results/coverage/Schedule/Schedule_6_10.coverage", "d847e195-94c4-47a5-817d-67c813ee15c4");
            throw new IllegalArgumentException("interval must be greater than zero");
        }
        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_6_10.coverage", "d76c0830-edd3-49b0-b034-ddf49b72aa06");
        interval = intervalBetweenModificationChecks * ONE_SECOND;
    }

    public long getInterval() {
        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_6_10.coverage", "9420af21-0e1d-487b-9c7d-1a4ebac15982");
        return interval;
    }

    public void setShowProgress(final boolean showProgress) {
        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_6_10.coverage", "b61802f3-e6d6-47d3-bb17-4cb971425942");
        this.showProgress = showProgress;
    }

    public boolean getShowProgress() {
        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_6_10.coverage", "8b7378d2-b9e4-4025-a6b1-6a118b3b8d2b");
        return showProgress;
    }

    public void validate() throws CruiseControlException {
        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_6_10.coverage", "3c87dae2-eeec-4e7b-9a55-61a217bdf391");
        ValidationHelper.assertTrue(builders.size() > 0, "schedule element requires at least one nested builder element");
        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_6_10.coverage", "c92815be-5a20-4d96-b577-03ab52920e4e");
        ValidationHelper.assertFalse(interval > ONE_YEAR, "maximum interval value is " + MAX_INTERVAL_SECONDS + " (one year)");
        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_6_10.coverage", "32428526-0687-4013-81a4-ab1f97fcceba");
        if (hasOnlyTimeBuilders()) {
            writeline("/home/ubuntu/results/coverage/Schedule/Schedule_6_10.coverage", "100196e2-f5e0-47a3-9110-4a63a95a38c7");
            LOG.warn("schedule has all time based builders: interval value will be ignored.");
            writeline("/home/ubuntu/results/coverage/Schedule/Schedule_6_10.coverage", "50ffff9b-5a5d-4f58-b8d6-40573c49eb32");
            ValidationHelper.assertFalse(checkWithinPause(new ArrayList<Builder>(builders)), "all build times during pauses.");
        }
        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_6_10.coverage", "f5922123-0d59-4aa3-ac95-d2851a3eb8c5");
        // Validate the child builders, since no one else seems to be doing it.
        for (final Builder next : builders) {
            writeline("/home/ubuntu/results/coverage/Schedule/Schedule_6_10.coverage", "1e91ccd6-1b38-41e3-a0a5-d4cb988110c1");
            next.validate();
        }
    }

    private boolean checkWithinPause(List timeBuilders) {
        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_6_10.coverage", "e9959525-99bd-49b6-af16-bd9f922928ca");
        for (int i = 0; i < timeBuilders.size(); i++) {
            writeline("/home/ubuntu/results/coverage/Schedule/Schedule_6_10.coverage", "88acf253-cde2-4f09-8821-0f74efdf5e07");
            Builder builder = (Builder) timeBuilders.get(i);
            writeline("/home/ubuntu/results/coverage/Schedule/Schedule_6_10.coverage", "ba5aa47d-a77a-4a21-8125-6d46c8dbc65a");
            for (final PauseBuilder pauseBuilder : pauseBuilders) {
                writeline("/home/ubuntu/results/coverage/Schedule/Schedule_6_10.coverage", "0753a590-7cee-4f86-b7b4-7876b4cac8c7");
                if (buildDaySameAsPauseDay(builder, pauseBuilder) && buildTimeWithinPauseTime(builder, pauseBuilder)) {
                    writeline("/home/ubuntu/results/coverage/Schedule/Schedule_6_10.coverage", "f99959c6-4dce-4dc0-b7f2-9ff7f765f16d");
                    timeBuilders.remove(builder);
                    writeline("/home/ubuntu/results/coverage/Schedule/Schedule_6_10.coverage", "751a7082-7b50-49f2-9f61-758d7daa5308");
                    StringBuffer message = new StringBuffer();
                    writeline("/home/ubuntu/results/coverage/Schedule/Schedule_6_10.coverage", "c8214258-7fe2-4405-aff3-95c835afd8a0");
                    message.append("time Builder for time ");
                    writeline("/home/ubuntu/results/coverage/Schedule/Schedule_6_10.coverage", "b7ef5ada-e60b-42ad-8b27-c01b1f3c92e0");
                    message.append(Integer.toString(builder.getTime()));
                    writeline("/home/ubuntu/results/coverage/Schedule/Schedule_6_10.coverage", "c79229bc-40b7-462b-b0cd-51df4a3ff3dc");
                    if (builder.getDay() != Builder.NOT_SET) {
                        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_6_10.coverage", "6b7966b0-6258-4690-8c83-a20d699602a0");
                        message.append(" and day of ");
                        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_6_10.coverage", "16ee4fdb-abb9-41dd-a017-5d3810253865");
                        message.append(getDayString(builder.getDay()));
                    }
                    writeline("/home/ubuntu/results/coverage/Schedule/Schedule_6_10.coverage", "7acdb09e-1e65-4ba2-8378-1c2c87284ad4");
                    message.append(" is always within a pause and will never build");
                    writeline("/home/ubuntu/results/coverage/Schedule/Schedule_6_10.coverage", "a58bf6bb-f562-4921-bad8-0fd538eb6663");
                    LOG.error(message.toString());
                }
            }
        }
        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_6_10.coverage", "036f2a9f-6937-4659-a5b3-5f30d77756d4");
        return timeBuilders.isEmpty();
    }

    /**
     * @param day
     * int value
     * @return english string value
     */
    String getDayString(int day) {
        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_6_10.coverage", "58a39e95-c613-4abb-8d83-49e50060c5bd");
        if (day < 1 || day > 7) {
            writeline("/home/ubuntu/results/coverage/Schedule/Schedule_6_10.coverage", "02c41fc0-0238-4ff3-848d-f60f8045228c");
            throw new IllegalArgumentException("valid values of days are between 1 and 7, was " + day);
        }
        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_6_10.coverage", "2b0b87c5-2f66-4336-b047-47cff49146ed");
        DateFormatSymbols symbols = new DateFormatSymbols(Locale.ENGLISH);
        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_6_10.coverage", "a7e55b73-3bc1-40f3-8351-66243887f525");
        String[] weekdays = symbols.getWeekdays();
        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_6_10.coverage", "2bf78ba2-2367-4e90-97a1-ec23939a01b7");
        return weekdays[day];
    }

    private boolean buildDaySameAsPauseDay(Builder builder, PauseBuilder pauseBuilder) {
        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_6_10.coverage", "14422e03-0546-47cc-b4dd-7c5e643b7a18");
        return pauseBuilder.getDay() == PauseBuilder.NOT_SET || pauseBuilder.getDay() == builder.getDay();
    }

    private boolean buildTimeWithinPauseTime(Builder builder, PauseBuilder pauseBuilder) {
        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_6_10.coverage", "5df25618-05e8-46d6-946b-38ccf57d4d42");
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
        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_6_10.coverage", "cec2934f-381f-42df-83d0-e0b37e92c515");
        if (param == null) {
            writeline("/home/ubuntu/results/coverage/Schedule/Schedule_6_10.coverage", "d23dfcc7-d68c-4e3e-98fd-1e1b21c2b5f2");
            throw new IllegalArgumentException(paramName + " can't be null");
        }
    }

    public List getBuilders() {
        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_6_10.coverage", "0b38f8ef-767b-470b-ae94-bcf0911e08a3");
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
