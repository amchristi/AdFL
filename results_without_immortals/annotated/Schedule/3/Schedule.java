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
        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_3_10.coverage", "03315f47-ce44-4f68-a0cc-4fb1db25a6d8");
        checkParamNotNull("builder", builder);
        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_3_10.coverage", "c84b1d8b-4d0c-4fed-ab59-015ddadda0a4");
        builders.add(builder);
        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_3_10.coverage", "2e6bc12b-9165-4c1f-bec7-ee13366d8655");
        Collections.sort(builders, builderComparator);
    }

    public void add(final PauseBuilder pause) {
        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_3_10.coverage", "9fcdfd19-cfe3-41eb-a442-010ffb516d6a");
        checkParamNotNull("pauseBuilder", pause);
        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_3_10.coverage", "b89294f1-a129-4277-adb3-67b9de33bdc7");
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
        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_3_10.coverage", "97e8f162-be37-41cd-9da3-aefe4db75a03");
        checkParamNotNull("date", now);
        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_3_10.coverage", "36a6d225-a176-4f4b-aea0-25d858655929");
        PauseBuilder pause = findPause(now);
        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_3_10.coverage", "f9030d86-8f8b-4a84-be92-383330072486");
        if (pause != null) {
            writeline("/home/ubuntu/results/coverage/Schedule/Schedule_3_10.coverage", "ae161273-9e01-4195-b48a-82f99654634f");
            LOG.info("CruiseControl is paused until: " + getEndTimeString(pause));
            writeline("/home/ubuntu/results/coverage/Schedule/Schedule_3_10.coverage", "10aac070-945a-4f19-9732-87ac4d3cfc88");
            return true;
        }
        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_3_10.coverage", "431388b3-feea-4d5a-b421-d471638ef198");
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
        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_3_10.coverage", "f13d10dc-f8d4-44ee-990c-ace5c88b7d35");
        final Calendar cal = Calendar.getInstance();
        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_3_10.coverage", "ee443138-bd16-4557-b521-38af397d9f9a");
        cal.set(Calendar.HOUR_OF_DAY, builder.getEndTime() / 100);
        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_3_10.coverage", "c5356d53-c420-41de-8aa5-f5ecb553cef7");
        cal.set(Calendar.MINUTE, builder.getEndTime() % 100);
        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_3_10.coverage", "d6d5ec44-986f-44de-8237-f2f12f8cfb68");
        cal.add(Calendar.MINUTE, 1);
        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_3_10.coverage", "13cd830b-0d9d-4edf-b748-157198fcff82");
        return timeFormatter.format(cal.getTime());
    }

    PauseBuilder findPause(final Date date) {
        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_3_10.coverage", "063d819f-9121-45b1-9315-5ad0e6762fc5");
        checkParamNotNull("date", date);
        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_3_10.coverage", "4547a0f8-e2d7-410e-9dc3-f14a01c8baef");
        for (final PauseBuilder pause : pauseBuilders) {
            writeline("/home/ubuntu/results/coverage/Schedule/Schedule_3_10.coverage", "c418c6ca-a3ee-4d62-8e2a-48fbb0a570a7");
            if (pause.isPaused(date)) {
                writeline("/home/ubuntu/results/coverage/Schedule/Schedule_3_10.coverage", "064d098c-6f08-450a-ad6e-ec5a829309d5");
                return pause;
            }
        }
        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_3_10.coverage", "6852bfa7-e137-4df2-abd9-b33410a561fe");
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
        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_3_10.coverage", "bb692b72-dee1-4bd7-b23f-9bc987f8e5f8");
        final Builder builder = selectBuilder(buildNumber, lastBuild, now);
        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_3_10.coverage", "21275e8d-ff02-4619-9ca6-ef4a0914a05b");
        if (buildTarget != null) {
            writeline("/home/ubuntu/results/coverage/Schedule/Schedule_3_10.coverage", "06b74a13-045c-485a-9926-38758049d155");
            LOG.info("Overriding build target with \"" + buildTarget + "\"");
            writeline("/home/ubuntu/results/coverage/Schedule/Schedule_3_10.coverage", "b06869af-0ace-4339-890a-02e3de977ac9");
            return builder.buildWithTarget(properties, buildTarget, (getShowProgress() ? progress : null));
        }
        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_3_10.coverage", "ca823252-68c0-4de2-bab3-d718224a63a7");
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
        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_3_10.coverage", "2e87e702-c345-4a07-9d9c-25ae52c3c65f");
        Builder builder = findBuilder(buildNumber, lastBuild, now);
        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_3_10.coverage", "a38779f2-f199-4e65-9434-b5917c4bdb41");
        if (builder == null) {
            writeline("/home/ubuntu/results/coverage/Schedule/Schedule_3_10.coverage", "c6540f86-9e7b-4f8b-aad4-aaf816971b8f");
            final long timeToNextBuild = getTimeToNextBuild(now, ONE_MINUTE);
            writeline("/home/ubuntu/results/coverage/Schedule/Schedule_3_10.coverage", "9e207f7a-0c94-45fd-8a9e-dc9ce71c0f2d");
            final Date futureDate = getFutureDate(now, timeToNextBuild);
            writeline("/home/ubuntu/results/coverage/Schedule/Schedule_3_10.coverage", "ffa347bb-8e10-4002-bea7-499d7f958df0");
            builder = findBuilder(buildNumber, now, futureDate);
        }
        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_3_10.coverage", "319e81a1-92c0-47d7-ad0f-f22382add9d1");
        if (builder == null) {
            writeline("/home/ubuntu/results/coverage/Schedule/Schedule_3_10.coverage", "64bc6e56-a3ba-421a-be4b-5b00e122fb93");
            validate();
            writeline("/home/ubuntu/results/coverage/Schedule/Schedule_3_10.coverage", "3f0236c8-a4e7-4f7a-9fe6-2abdd19932f2");
            throw new CruiseControlException("configuration error not caught by validate? no builder selected");
        }
        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_3_10.coverage", "86b5282d-b539-4a5f-ae9c-94038b59d21c");
        return builder;
    }

    private Builder findBuilder(final int buildNumber, final Date lastBuild, final Date now) throws CruiseControlException {
        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_3_10.coverage", "b4c231bd-c3cb-4c6f-bf31-4370b9b99f44");
        for (final Builder builder : builders) {
            writeline("/home/ubuntu/results/coverage/Schedule/Schedule_3_10.coverage", "6d22a9ee-9950-4c2f-9c45-bd95ab50fae4");
            if (builder.isTimeBuilder()) {
                writeline("/home/ubuntu/results/coverage/Schedule/Schedule_3_10.coverage", "e24a9eda-f5a4-45ff-aa0b-1e4b0b5e3340");
                final int buildTime = builder.getTime();
                writeline("/home/ubuntu/results/coverage/Schedule/Schedule_3_10.coverage", "1cac43f2-4e56-4439-86fc-bbbd822df041");
                final boolean didntBuildToday = builderDidntBuildToday(lastBuild, now, buildTime);
                writeline("/home/ubuntu/results/coverage/Schedule/Schedule_3_10.coverage", "9243980d-5337-4484-9170-e8abcdda3d75");
                final int nowTime = DateUtil.getTimeFromDate(now);
                writeline("/home/ubuntu/results/coverage/Schedule/Schedule_3_10.coverage", "06638251-13a6-45df-8e46-4d7939a34147");
                final boolean isAfterBuildTime = buildTime <= nowTime;
                writeline("/home/ubuntu/results/coverage/Schedule/Schedule_3_10.coverage", "ad82b589-8835-4b8e-b280-351ee9c69f1e");
                final boolean isValidDay = builder.isValidDay(now);
                writeline("/home/ubuntu/results/coverage/Schedule/Schedule_3_10.coverage", "b61ec336-da2a-4a2a-8f53-f4b5ad94238b");
                if (didntBuildToday && isAfterBuildTime && isValidDay) {
                    writeline("/home/ubuntu/results/coverage/Schedule/Schedule_3_10.coverage", "4c737db0-ca6b-43c4-a95b-683f361f7442");
                    return builder;
                }
            } else if (builder.getMultiple() > 0) {
                writeline("/home/ubuntu/results/coverage/Schedule/Schedule_3_10.coverage", "2ed9b50e-4fe2-4c52-8463-dd0675206f5b");
                if (builder.isValidDay(now)) {
                    writeline("/home/ubuntu/results/coverage/Schedule/Schedule_3_10.coverage", "dc70d84e-97f0-44fb-a4b6-2a974bc8cb48");
                    if ((buildNumber % builder.getMultiple()) == 0) {
                        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_3_10.coverage", "d90c2e9a-d78f-4b20-acb5-4a4671d3c5dd");
                        return builder;
                    }
                }
            } else {
                writeline("/home/ubuntu/results/coverage/Schedule/Schedule_3_10.coverage", "57c4ea83-d7cd-4867-ac0c-c2e7ecc0e35d");
                throw new CruiseControlException("The selected Builder is not properly configured");
            }
        }
        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_3_10.coverage", "7eb35bd7-d805-4bcd-a331-19608da79f20");
        return null;
    }

    boolean builderDidntBuildToday(Date lastBuild, Date now, int buildTime) {
        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_3_10.coverage", "c4bb56f8-a561-46e8-a3d1-7459a06ef1f5");
        int time = DateUtil.getTimeFromDate(now);
        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_3_10.coverage", "f7f71355-107e-4004-a7a8-6388e6691fe7");
        long timeMillis = DateUtil.convertToMillis(time);
        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_3_10.coverage", "08dfde42-47ae-4e35-8606-2d27010f2fa0");
        long startOfToday = now.getTime() - timeMillis;
        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_3_10.coverage", "2c828fe3-b4ba-48ac-95c4-7e77d43850da");
        boolean lastBuildYesterday = lastBuild.getTime() < startOfToday;
        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_3_10.coverage", "31e288ac-a7fe-48a7-9d49-e988cf017105");
        boolean lastBuildTimeBeforeBuildTime = DateUtil.getTimeFromDate(lastBuild) < buildTime;
        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_3_10.coverage", "744ab6bc-13ce-41f7-901a-78156590a874");
        return lastBuildYesterday || lastBuildTimeBeforeBuildTime;
    }

    long getTimeToNextBuild(Date now, long sleepInterval) {
        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_3_10.coverage", "03f0c080-9ae4-4992-9557-02cc97c226df");
        return getTimeToNextBuild(now, sleepInterval, 0);
    }

    private long getTimeToNextBuild(Date now, long sleepInterval, long priorPauseAdjustment) {
        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_3_10.coverage", "efe24d26-8560-4142-b1dc-a0f38379f223");
        long timeToNextBuild = sleepInterval;
        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_3_10.coverage", "2db90006-a993-42f7-9e15-e1d3603216b3");
        LOG.debug("getTimeToNextBuild: initial timeToNextBuild = " + timeToNextBuild);
        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_3_10.coverage", "ff811eae-b6f5-4daf-91b8-5b1b9527ce43");
        timeToNextBuild = checkMultipleBuilders(now, timeToNextBuild);
        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_3_10.coverage", "5b5f7a70-b0c4-45c1-8817-9649096bb49d");
        LOG.debug("getTimeToNextBuild: after checkMultipleBuilders = " + timeToNextBuild);
        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_3_10.coverage", "f1cab8d2-55fb-4294-b0c7-ced1a0019c35");
        timeToNextBuild = checkTimeBuilders(now, timeToNextBuild);
        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_3_10.coverage", "ff5c8874-4620-4120-a674-ecbc142b883a");
        LOG.debug("getTimeToNextBuild: after checkTimeBuilders = " + timeToNextBuild);
        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_3_10.coverage", "2ff70f17-8de6-4dce-bb82-6544f6a64575");
        long timeTillNotPaused = checkPauseBuilders(now, timeToNextBuild);
        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_3_10.coverage", "951abdff-d43b-4594-903d-478533dc8ff7");
        LOG.debug("getTimeToNextBuild: after checkPauseBuilders = " + timeToNextBuild);
        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_3_10.coverage", "f48e2a2b-f825-401b-8b3e-0def0d7f0e88");
        if (timeToNextBuild != timeTillNotPaused) {
            writeline("/home/ubuntu/results/coverage/Schedule/Schedule_3_10.coverage", "f8c2d469-4cf4-473a-822d-8de9429e53fe");
            boolean atMaxTime = timeTillNotPaused >= MAX_INTERVAL_MILLISECONDS || priorPauseAdjustment >= MAX_INTERVAL_MILLISECONDS;
            writeline("/home/ubuntu/results/coverage/Schedule/Schedule_3_10.coverage", "d41ce6fc-9625-45ab-aef5-882418ee41bf");
            if (hasOnlyTimeBuilders() && !atMaxTime) {
                writeline("/home/ubuntu/results/coverage/Schedule/Schedule_3_10.coverage", "5a691ac0-34dd-4415-a68c-4808a79f1292");
                Date dateAfterPause = getFutureDate(now, timeTillNotPaused);
                writeline("/home/ubuntu/results/coverage/Schedule/Schedule_3_10.coverage", "7f0fd004-c398-4273-a7d4-2d37e699cbbf");
                long adjustmentFromEndOfPause = getTimeToNextBuild(dateAfterPause, 0, priorPauseAdjustment + timeTillNotPaused);
                writeline("/home/ubuntu/results/coverage/Schedule/Schedule_3_10.coverage", "8a1a482b-f6b1-4504-b75b-01171010905b");
                timeToNextBuild = timeTillNotPaused + adjustmentFromEndOfPause;
                writeline("/home/ubuntu/results/coverage/Schedule/Schedule_3_10.coverage", "c5f936c8-f2bd-4bb7-940c-f7e3e597a1e4");
                timeToNextBuild = checkMaximumInterval(timeToNextBuild);
            } else {
                writeline("/home/ubuntu/results/coverage/Schedule/Schedule_3_10.coverage", "8270e2d9-e19a-4e05-b74c-92d942b11e30");
                timeToNextBuild = timeTillNotPaused;
            }
        }
        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_3_10.coverage", "a4c5dd48-0ae8-4ef1-9f64-b089fb03d8ff");
        return timeToNextBuild;
    }

    private long checkMultipleBuilders(final Date now, final long interval) {
        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_3_10.coverage", "05d2846a-bfc3-4818-8874-ae8647ffdc23");
        if (hasOnlyTimeBuilders()) {
            writeline("/home/ubuntu/results/coverage/Schedule/Schedule_3_10.coverage", "7ae10ad8-c2b4-4152-aba7-4181f6aed5e1");
            LOG.debug("has only time builders, so no correction for multiple builders.");
            writeline("/home/ubuntu/results/coverage/Schedule/Schedule_3_10.coverage", "8241f2e5-abe7-4d1e-8063-b189e857b4c0");
            return interval;
        }
        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_3_10.coverage", "f4e412c4-2d85-4514-ada5-1979de731de4");
        Date then = getFutureDate(now, interval);
        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_3_10.coverage", "5fd8935f-c507-47c6-bc3d-606102ccf4b1");
        final List<Builder> buildersForOtherDays = new ArrayList<Builder>();
        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_3_10.coverage", "2e5f07d5-eb89-406f-a90f-86250326109f");
        for (final Builder builder : builders) {
            writeline("/home/ubuntu/results/coverage/Schedule/Schedule_3_10.coverage", "d2fbb0a2-48e5-49e4-b5de-839586c56940");
            if (!builder.isTimeBuilder()) {
                writeline("/home/ubuntu/results/coverage/Schedule/Schedule_3_10.coverage", "eb39abe3-691f-4c6e-8ec4-f0da03c89b33");
                if (builder.getMultiple() == 1) {
                    writeline("/home/ubuntu/results/coverage/Schedule/Schedule_3_10.coverage", "aae5675e-e2b2-4da3-b0b4-f431c50c70c0");
                    if (builder.isValidDay(then)) {
                        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_3_10.coverage", "fc8ee8aa-d4ad-41f9-a6c8-030a2b831357");
                        LOG.debug("multiple=1 builder found that could run on " + then);
                        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_3_10.coverage", "61cd9147-d28f-4824-9f95-a49849ce3798");
                        return interval;
                    } else {
                        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_3_10.coverage", "31d6554f-eb06-45b4-af4a-d4619ca1e576");
                        buildersForOtherDays.add(builder);
                    }
                }
            }
        }
        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_3_10.coverage", "59cb098e-a0ff-422d-be73-c2fb444f3dcd");
        if (buildersForOtherDays.size() == 0) {
            writeline("/home/ubuntu/results/coverage/Schedule/Schedule_3_10.coverage", "ec1f1172-54c8-43eb-87c5-c105202498e0");
            LOG.error("configuration error: has some multiple builders but no multiple=1 builders found!");
            writeline("/home/ubuntu/results/coverage/Schedule/Schedule_3_10.coverage", "6d642157-bb48-4d60-9581-7990d8e3fe3e");
            return interval;
        } else {
            writeline("/home/ubuntu/results/coverage/Schedule/Schedule_3_10.coverage", "c0b4a8bd-ce7b-4597-aa39-356b7d294258");
            LOG.debug("no multiple=1 builders found for " + then + ". checking other days");
        }
        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_3_10.coverage", "41bb5374-c103-4b11-993c-9bc4cbbda819");
        for (int i = 1; i < 7; i++) {
            writeline("/home/ubuntu/results/coverage/Schedule/Schedule_3_10.coverage", "c7035b3b-c11e-49e5-8c6d-5439303addd2");
            long daysPastInitialInterval = i * ONE_DAY;
            writeline("/home/ubuntu/results/coverage/Schedule/Schedule_3_10.coverage", "2658de3b-2249-4d37-a854-cc72ed18c9ae");
            then = getFutureDate(now, interval + daysPastInitialInterval);
            writeline("/home/ubuntu/results/coverage/Schedule/Schedule_3_10.coverage", "efa7b751-7c6f-4671-aa8a-70dff2b2426d");
            for (final Builder builder : builders) {
                writeline("/home/ubuntu/results/coverage/Schedule/Schedule_3_10.coverage", "38ec4fce-3539-4d0e-a9ea-3816dd02f492");
                if (builder.isValidDay(then)) {
                    writeline("/home/ubuntu/results/coverage/Schedule/Schedule_3_10.coverage", "003ff7d4-f7db-45c6-8345-a4e4c8b0189a");
                    LOG.debug("multiple=1 builder found that could run on " + then);
                    writeline("/home/ubuntu/results/coverage/Schedule/Schedule_3_10.coverage", "1afd4b1d-5bb4-40df-85f5-5b04aad55553");
                    long correctionToMidnight = getTimePastMidnight(then);
                    writeline("/home/ubuntu/results/coverage/Schedule/Schedule_3_10.coverage", "ecc960a3-8bf1-4bd5-be28-f6064e04ffc1");
                    return interval + daysPastInitialInterval - correctionToMidnight;
                }
            }
        }
        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_3_10.coverage", "976e1e76-82a4-454d-9930-088a77a4bfa2");
        LOG.error("configuration error? could not find appropriate multiple=1 builder.");
        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_3_10.coverage", "4dac9b99-14f2-4452-950f-d990ad86f4fe");
        return interval;
    }

    private long getTimePastMidnight(Date date) {
        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_3_10.coverage", "4dd276c8-60e6-4239-9465-125d4f4fd54e");
        Calendar cal = Calendar.getInstance();
        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_3_10.coverage", "e0853a79-efa9-4e7d-85a7-efc8ce1c6097");
        cal.setTime(date);
        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_3_10.coverage", "f08d4e1d-34e5-4f1b-bdc2-ac7240253c4d");
        long time = 60 * ONE_MINUTE * cal.get(Calendar.HOUR_OF_DAY);
        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_3_10.coverage", "9feab154-6d1c-413a-b638-cf512ba289f6");
        time += ONE_MINUTE * cal.get(Calendar.MINUTE);
        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_3_10.coverage", "91398d17-63f6-4ba3-a87c-697f2e3f5eef");
        return time;
    }

    private boolean hasOnlyTimeBuilders() {
        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_3_10.coverage", "43302dcb-6ee6-4117-a320-42a4a690d9f5");
        boolean onlyTimeBuilders = true;
        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_3_10.coverage", "ff473c32-8be9-420c-9dce-675c8e0af5f4");
        for (final Builder builder : builders) {
            writeline("/home/ubuntu/results/coverage/Schedule/Schedule_3_10.coverage", "4be49a5b-ad70-4e16-bc4d-6579c08a379b");
            if (!builder.isTimeBuilder()) {
                writeline("/home/ubuntu/results/coverage/Schedule/Schedule_3_10.coverage", "1ec427a3-ba55-47a3-aa6f-d3b058e4e6dc");
                onlyTimeBuilders = false;
                writeline("/home/ubuntu/results/coverage/Schedule/Schedule_3_10.coverage", "1d7d220c-93d3-49e9-bd50-28b3da852345");
                break;
            }
        }
        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_3_10.coverage", "92fe94f4-63df-4bf7-8f61-fb26f2275578");
        return onlyTimeBuilders;
    }

    long checkTimeBuilders(final Date now, final long proposedTime) {
        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_3_10.coverage", "6d7ba2b7-a6f2-44cc-9849-d2e11ad09dc4");
        long timeToNextBuild = proposedTime;
        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_3_10.coverage", "4c6258bf-bfcb-446f-a24d-e19abb07f37e");
        if (hasOnlyTimeBuilders()) {
            writeline("/home/ubuntu/results/coverage/Schedule/Schedule_3_10.coverage", "a0ab5188-4186-4b17-a3d5-4b100c4da8e5");
            timeToNextBuild = Long.MAX_VALUE;
        }
        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_3_10.coverage", "bbcf81df-cc74-42e4-a104-2b767fbbaaad");
        final int nowTime = DateUtil.getTimeFromDate(now);
        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_3_10.coverage", "442bcd03-98da-4777-be0a-dc15d1b764a3");
        for (final Builder builder : builders) {
            writeline("/home/ubuntu/results/coverage/Schedule/Schedule_3_10.coverage", "6dd473c3-e6b6-4fa6-beae-8e6bd8b51325");
            if (builder.isTimeBuilder()) {
                writeline("/home/ubuntu/results/coverage/Schedule/Schedule_3_10.coverage", "c2565642-26e0-4f07-9828-5f2cf802da74");
                long timeToThisBuild = Long.MAX_VALUE;
                writeline("/home/ubuntu/results/coverage/Schedule/Schedule_3_10.coverage", "2008a0c0-62a9-460b-af40-41daaee6af65");
                final Calendar cal = Calendar.getInstance();
                writeline("/home/ubuntu/results/coverage/Schedule/Schedule_3_10.coverage", "927a7665-076a-4633-93a9-dc85753c7428");
                final long oneYear = 365;
                writeline("/home/ubuntu/results/coverage/Schedule/Schedule_3_10.coverage", "80c136bd-63b1-4efc-830e-4d96644c3151");
                for (int daysInTheFuture = 0; daysInTheFuture < oneYear; daysInTheFuture++) {
                    writeline("/home/ubuntu/results/coverage/Schedule/Schedule_3_10.coverage", "64498240-c26a-4017-863e-be7ee26e3d48");
                    cal.setTime(now);
                    writeline("/home/ubuntu/results/coverage/Schedule/Schedule_3_10.coverage", "2c8d9032-b4bc-41bc-97e2-0d0d37f4d484");
                    cal.add(Calendar.DATE, daysInTheFuture);
                    writeline("/home/ubuntu/results/coverage/Schedule/Schedule_3_10.coverage", "f792fe89-c752-4391-8cd8-00f254b72587");
                    Date future = cal.getTime();
                    writeline("/home/ubuntu/results/coverage/Schedule/Schedule_3_10.coverage", "84cb756c-d2c6-49cc-9a3d-43f3b515fbbe");
                    final boolean dayIsValid = builder.isValidDay(future);
                    writeline("/home/ubuntu/results/coverage/Schedule/Schedule_3_10.coverage", "58fd30d8-3cdd-4e98-bd84-89e159df819d");
                    if (dayIsValid) {
                        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_3_10.coverage", "c1fe2369-169a-4a76-9c65-434dea2daf6a");
                        int thisBuildTime = builder.getTime();
                        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_3_10.coverage", "9d28bf85-4062-4441-9242-debb880ef8f3");
                        boolean timePassedToday = (daysInTheFuture == 0) && (nowTime > thisBuildTime);
                        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_3_10.coverage", "dbe75ea0-9d08-41e8-8ac8-817128c2183e");
                        if (!timePassedToday) {
                            writeline("/home/ubuntu/results/coverage/Schedule/Schedule_3_10.coverage", "4be21414-8ef5-4e4e-8c28-759262db8ee4");
                            int buildHour = thisBuildTime / 100;
                            writeline("/home/ubuntu/results/coverage/Schedule/Schedule_3_10.coverage", "7729500f-0ef8-46d2-a6e6-7108925d7d7f");
                            int buildMinute = thisBuildTime % 100;
                            writeline("/home/ubuntu/results/coverage/Schedule/Schedule_3_10.coverage", "df31b416-3abf-4f38-9a87-4590278a1afa");
                            cal.set(Calendar.HOUR_OF_DAY, buildHour);
                            writeline("/home/ubuntu/results/coverage/Schedule/Schedule_3_10.coverage", "8c54c3aa-fed7-498c-9774-565a27a495b7");
                            cal.set(Calendar.MINUTE, buildMinute);
                            writeline("/home/ubuntu/results/coverage/Schedule/Schedule_3_10.coverage", "3e34df6a-9602-4e76-ac0b-e4f903345ad9");
                            future = cal.getTime();
                            writeline("/home/ubuntu/results/coverage/Schedule/Schedule_3_10.coverage", "dc945e5d-454b-4e9d-b448-a488a05344d8");
                            timeToThisBuild = future.getTime() - now.getTime();
                            writeline("/home/ubuntu/results/coverage/Schedule/Schedule_3_10.coverage", "b323c95e-834d-4198-a345-792ad30b4782");
                            break;
                        }
                    }
                }
                writeline("/home/ubuntu/results/coverage/Schedule/Schedule_3_10.coverage", "efb087bb-1fa8-4e9f-b353-1af2dc803c9a");
                if (timeToThisBuild < timeToNextBuild) {
                    writeline("/home/ubuntu/results/coverage/Schedule/Schedule_3_10.coverage", "1dccbd7f-47f1-428c-91a1-02388cfa87ef");
                    timeToNextBuild = timeToThisBuild;
                }
            }
        }
        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_3_10.coverage", "19164c76-eb71-493e-b9cb-f6304473dda3");
        if (timeToNextBuild > MAX_INTERVAL_MILLISECONDS) {
            writeline("/home/ubuntu/results/coverage/Schedule/Schedule_3_10.coverage", "5562573c-ef7a-48c8-b2be-855078c6c22c");
            LOG.error("checkTimeBuilders exceeding maximum interval. using proposed value [" + proposedTime + "] instead");
            writeline("/home/ubuntu/results/coverage/Schedule/Schedule_3_10.coverage", "3436e085-8072-4752-a909-f3da98d145eb");
            timeToNextBuild = proposedTime;
        }
        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_3_10.coverage", "cebb4c59-1fae-4202-84cd-5f6991af23fe");
        return timeToNextBuild;
    }

    long checkPauseBuilders(Date now, long proposedTime) {
        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_3_10.coverage", "266c7cd4-794c-4c32-a9e2-d5ccca4913b5");
        long oldTime = proposedTime;
        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_3_10.coverage", "10b877cb-e994-47f1-944f-596a92f62997");
        long newTime = checkForPauseAtProposedTime(now, oldTime);
        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_3_10.coverage", "d81ff2f6-b050-4164-89e1-70b796d9e9b8");
        while (oldTime != newTime) {
            writeline("/home/ubuntu/results/coverage/Schedule/Schedule_3_10.coverage", "0fb3eef4-4923-4267-817c-0255e190060a");
            oldTime = newTime;
            writeline("/home/ubuntu/results/coverage/Schedule/Schedule_3_10.coverage", "fbeb2315-15e3-4bb1-8b0d-026de082311b");
            newTime = checkForPauseAtProposedTime(now, oldTime);
        }
        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_3_10.coverage", "27214e69-7442-45e4-a3c9-6db51e7cd6a5");
        return newTime;
    }

    private long checkForPauseAtProposedTime(Date now, long proposedTime) {
        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_3_10.coverage", "78d3795b-dbe7-4d3a-9259-2e581a643cdf");
        Date futureDate = getFutureDate(now, proposedTime);
        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_3_10.coverage", "eb4c60d2-b43f-4e56-8a5f-80abce9df89a");
        PauseBuilder pause = findPause(futureDate);
        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_3_10.coverage", "a1ee148a-7932-450e-8bc2-ba9bc1e832c9");
        if (pause == null) {
            writeline("/home/ubuntu/results/coverage/Schedule/Schedule_3_10.coverage", "940b754b-ec9f-4721-a13b-aa1fa687c897");
            return proposedTime;
        }
        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_3_10.coverage", "19326f61-d8d6-4fba-94f1-a17b5e690dc2");
        int endPause = pause.getEndTime();
        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_3_10.coverage", "83654ddf-ce16-43bd-ba8a-c204cb4e630f");
        int futureTime = DateUtil.getTimeFromDate(futureDate);
        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_3_10.coverage", "2e5ad910-94c9-4607-bea1-1a8fccb35aa1");
        long timeToEndOfPause = proposedTime + DateUtil.milliTimeDifference(futureTime, endPause);
        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_3_10.coverage", "e36858dd-ed4d-4b2c-9ee0-5324192b0c75");
        timeToEndOfPause = checkMaximumInterval(timeToEndOfPause);
        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_3_10.coverage", "6e1356c2-727b-492f-bb18-8a56fd69e20c");
        return timeToEndOfPause == MAX_INTERVAL_MILLISECONDS ? timeToEndOfPause : timeToEndOfPause + ONE_MINUTE;
    }

    private long checkMaximumInterval(long timeToEndOfPause) {
        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_3_10.coverage", "dd8f70a3-b94a-470e-9826-2dd943b62ad0");
        if (timeToEndOfPause > MAX_INTERVAL_MILLISECONDS) {
            writeline("/home/ubuntu/results/coverage/Schedule/Schedule_3_10.coverage", "71340552-bf85-45f4-82f5-1224db8ddd1a");
            LOG.error("maximum interval exceeded! project perpetually paused?");
            writeline("/home/ubuntu/results/coverage/Schedule/Schedule_3_10.coverage", "f191a19a-b80a-4ed3-ae8f-a6a0ecf1bd77");
            return MAX_INTERVAL_MILLISECONDS;
        }
        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_3_10.coverage", "69614264-77af-40eb-b8bc-68ac0a85cae6");
        return timeToEndOfPause;
    }

    private Date getFutureDate(Date now, long delay) {
        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_3_10.coverage", "231a1956-732b-4652-94f4-a9cd9611565d");
        long futureMillis = now.getTime() + delay;
        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_3_10.coverage", "46f81d4f-fd67-4877-ac5c-44b06414590e");
        return new Date(futureMillis);
    }

    public void setInterval(long intervalBetweenModificationChecks) {
        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_3_10.coverage", "2b140e97-b09e-4152-a362-23f34a07268d");
        if (intervalBetweenModificationChecks <= 0) {
            writeline("/home/ubuntu/results/coverage/Schedule/Schedule_3_10.coverage", "4fae19a5-0561-490f-b266-7ef9255d2103");
            throw new IllegalArgumentException("interval must be greater than zero");
        }
        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_3_10.coverage", "2e9c7510-5cbe-4b7c-9eac-1393a6c281f9");
        interval = intervalBetweenModificationChecks * ONE_SECOND;
    }

    public long getInterval() {
        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_3_10.coverage", "44568797-fc53-4e68-9778-a1e574a1df07");
        return interval;
    }

    public void setShowProgress(final boolean showProgress) {
        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_3_10.coverage", "d7c7db99-11b8-4ca5-b4bf-ad6753976fc1");
        this.showProgress = showProgress;
    }

    public boolean getShowProgress() {
        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_3_10.coverage", "0e14b09c-1a1f-47c3-9dca-97875c5ac93a");
        return showProgress;
    }

    public void validate() throws CruiseControlException {
        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_3_10.coverage", "93914efa-7fbc-41d3-9f06-e89b2b386607");
        ValidationHelper.assertTrue(builders.size() > 0, "schedule element requires at least one nested builder element");
        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_3_10.coverage", "8aa297a8-c15d-4bd1-8295-f351e40880e3");
        ValidationHelper.assertFalse(interval > ONE_YEAR, "maximum interval value is " + MAX_INTERVAL_SECONDS + " (one year)");
        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_3_10.coverage", "c27310f9-8d5f-43d0-bd19-977effd40415");
        if (hasOnlyTimeBuilders()) {
            writeline("/home/ubuntu/results/coverage/Schedule/Schedule_3_10.coverage", "e461afb9-adc2-4f6d-b106-a4750c503a93");
            LOG.warn("schedule has all time based builders: interval value will be ignored.");
            writeline("/home/ubuntu/results/coverage/Schedule/Schedule_3_10.coverage", "18ec64bc-6e6f-408c-9279-476e8d60677d");
            ValidationHelper.assertFalse(checkWithinPause(new ArrayList<Builder>(builders)), "all build times during pauses.");
        }
        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_3_10.coverage", "f01b79d8-dac3-4747-a4fe-c4976de0b6a5");
        // Validate the child builders, since no one else seems to be doing it.
        for (final Builder next : builders) {
            writeline("/home/ubuntu/results/coverage/Schedule/Schedule_3_10.coverage", "28069c59-2afc-4cf3-9bd9-97bffc9d77f9");
            next.validate();
        }
    }

    private boolean checkWithinPause(List timeBuilders) {
        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_3_10.coverage", "7f3846ca-e491-44a8-97e7-2128e33b7621");
        for (int i = 0; i < timeBuilders.size(); i++) {
            writeline("/home/ubuntu/results/coverage/Schedule/Schedule_3_10.coverage", "ff96a834-a60d-4ef8-bf60-cb8048c0d777");
            Builder builder = (Builder) timeBuilders.get(i);
            writeline("/home/ubuntu/results/coverage/Schedule/Schedule_3_10.coverage", "91a33057-6818-4ce0-be27-06b5616d1206");
            for (final PauseBuilder pauseBuilder : pauseBuilders) {
                writeline("/home/ubuntu/results/coverage/Schedule/Schedule_3_10.coverage", "61756656-be85-4a71-8afa-cf7d8cf9a086");
                if (buildDaySameAsPauseDay(builder, pauseBuilder) && buildTimeWithinPauseTime(builder, pauseBuilder)) {
                    writeline("/home/ubuntu/results/coverage/Schedule/Schedule_3_10.coverage", "17ee4d21-a312-4e47-ac32-c5e80447f878");
                    timeBuilders.remove(builder);
                    writeline("/home/ubuntu/results/coverage/Schedule/Schedule_3_10.coverage", "4f8c4c73-1b31-4708-84e0-9f57684050b2");
                    StringBuffer message = new StringBuffer();
                    writeline("/home/ubuntu/results/coverage/Schedule/Schedule_3_10.coverage", "6e65241f-a1d8-44f8-aef7-7e94f3bebfca");
                    message.append("time Builder for time ");
                    writeline("/home/ubuntu/results/coverage/Schedule/Schedule_3_10.coverage", "88fc2aab-c8b1-4275-8a1d-23b506b96c21");
                    message.append(Integer.toString(builder.getTime()));
                    writeline("/home/ubuntu/results/coverage/Schedule/Schedule_3_10.coverage", "418ae964-91cf-4716-8ff5-50c7f6254978");
                    if (builder.getDay() != Builder.NOT_SET) {
                        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_3_10.coverage", "ab285f73-fdf8-4d05-b29f-731d07e701e1");
                        message.append(" and day of ");
                        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_3_10.coverage", "c34ad7f1-fe8d-44f5-b012-11a342e832a0");
                        message.append(getDayString(builder.getDay()));
                    }
                    writeline("/home/ubuntu/results/coverage/Schedule/Schedule_3_10.coverage", "bdec426d-bcaf-4872-8e71-0ef3f65258d3");
                    message.append(" is always within a pause and will never build");
                    writeline("/home/ubuntu/results/coverage/Schedule/Schedule_3_10.coverage", "4059cddc-fc81-4470-a8d9-ed071fa5d799");
                    LOG.error(message.toString());
                }
            }
        }
        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_3_10.coverage", "820dcfe0-e640-46fd-aed5-b01f8e168aff");
        return timeBuilders.isEmpty();
    }

    /**
     * @param day
     * int value
     * @return english string value
     */
    String getDayString(int day) {
        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_3_10.coverage", "49534146-2350-4bba-9d87-760aa0d75fd8");
        if (day < 1 || day > 7) {
            writeline("/home/ubuntu/results/coverage/Schedule/Schedule_3_10.coverage", "91ab23fd-252e-4050-8fc7-d232e725a97f");
            throw new IllegalArgumentException("valid values of days are between 1 and 7, was " + day);
        }
        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_3_10.coverage", "2e973b48-c03c-422c-b3da-76f76e3d1dea");
        DateFormatSymbols symbols = new DateFormatSymbols(Locale.ENGLISH);
        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_3_10.coverage", "399e531c-8e3d-4eaa-9eef-1bdf46c76739");
        String[] weekdays = symbols.getWeekdays();
        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_3_10.coverage", "bf520a2c-ca93-4242-a984-018c2e7a350b");
        return weekdays[day];
    }

    private boolean buildDaySameAsPauseDay(Builder builder, PauseBuilder pauseBuilder) {
        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_3_10.coverage", "260e24d3-3e32-46bd-956e-f99664ca8768");
        return pauseBuilder.getDay() == PauseBuilder.NOT_SET || pauseBuilder.getDay() == builder.getDay();
    }

    private boolean buildTimeWithinPauseTime(Builder builder, PauseBuilder pauseBuilder) {
        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_3_10.coverage", "a798fc47-0448-441f-8ceb-cb8803955d6a");
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
        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_3_10.coverage", "fcff6536-d4a6-4215-b72f-4f3f8fdda500");
        if (param == null) {
            writeline("/home/ubuntu/results/coverage/Schedule/Schedule_3_10.coverage", "758dabcd-514e-493f-84ed-36f8e6ec325a");
            throw new IllegalArgumentException(paramName + " can't be null");
        }
    }

    public List getBuilders() {
        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_3_10.coverage", "fbc0b858-a2d8-4923-abd2-03e9dc84f838");
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
