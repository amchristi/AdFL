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
        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_1_10.coverage", "8357a161-3c3d-4fae-a3cc-e30a94873aec");
        checkParamNotNull("builder", builder);
        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_1_10.coverage", "fdc02770-f0aa-419c-963e-970f7bc4d1b9");
        builders.add(builder);
        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_1_10.coverage", "15340740-5d06-43f9-b9fd-d33cb689d330");
        Collections.sort(builders, builderComparator);
    }

    public void add(final PauseBuilder pause) {
        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_1_10.coverage", "c9c89ece-f4e5-454d-aa92-4e8454abe2fa");
        checkParamNotNull("pauseBuilder", pause);
        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_1_10.coverage", "ee080196-72db-4fad-af63-b95cbfd43ad0");
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
        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_1_10.coverage", "73f9c81e-5d3f-4d37-8875-e4c2fc5abf8b");
        checkParamNotNull("date", now);
        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_1_10.coverage", "baa1ed7c-e175-46cb-89c0-58d926b28f56");
        PauseBuilder pause = findPause(now);
        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_1_10.coverage", "277b4d41-64b9-40e4-b38a-3046220e5047");
        if (pause != null) {
            writeline("/home/ubuntu/results/coverage/Schedule/Schedule_1_10.coverage", "4ffb3566-240c-465a-9bfc-b8c64d098692");
            LOG.info("CruiseControl is paused until: " + getEndTimeString(pause));
            writeline("/home/ubuntu/results/coverage/Schedule/Schedule_1_10.coverage", "368cb2f0-45c5-4e94-9f45-2e0c2bc409ce");
            return true;
        }
        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_1_10.coverage", "405900dc-1b60-4a41-8e78-26e5541e8ca8");
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
        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_1_10.coverage", "a703f078-42ed-4013-90a9-3911b2367db2");
        final Calendar cal = Calendar.getInstance();
        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_1_10.coverage", "0562cc14-101f-4d5d-8824-f2b159c4b509");
        cal.set(Calendar.HOUR_OF_DAY, builder.getEndTime() / 100);
        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_1_10.coverage", "752269bd-4dac-461e-8cd0-957b1c261a65");
        cal.set(Calendar.MINUTE, builder.getEndTime() % 100);
        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_1_10.coverage", "d2b398ed-5582-4a54-9949-ce0b09a4ee9d");
        cal.add(Calendar.MINUTE, 1);
        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_1_10.coverage", "f5722355-9446-4633-8094-e9e4000e3bea");
        return timeFormatter.format(cal.getTime());
    }

    PauseBuilder findPause(final Date date) {
        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_1_10.coverage", "2be1047f-68ec-4388-b5b1-775cdd206d11");
        checkParamNotNull("date", date);
        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_1_10.coverage", "a8a43dfe-9745-4bea-9a62-6caa709155a7");
        for (final PauseBuilder pause : pauseBuilders) {
            writeline("/home/ubuntu/results/coverage/Schedule/Schedule_1_10.coverage", "c4bf315c-99cc-4cce-aa4f-784cf45c0870");
            if (pause.isPaused(date)) {
                writeline("/home/ubuntu/results/coverage/Schedule/Schedule_1_10.coverage", "edda7dab-1225-44c1-9cc3-a77d3a3dcf4f");
                return pause;
            }
        }
        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_1_10.coverage", "77652eb9-63c4-4887-99fd-82ab31a251fc");
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
        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_1_10.coverage", "963df6bb-84d1-4b4b-8c52-9b9991647dd2");
        final Builder builder = selectBuilder(buildNumber, lastBuild, now);
        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_1_10.coverage", "593f15e2-da51-4533-b100-35b7e44aa014");
        if (buildTarget != null) {
            writeline("/home/ubuntu/results/coverage/Schedule/Schedule_1_10.coverage", "2cf055f3-7b0c-49b1-8225-d5015ac044ca");
            LOG.info("Overriding build target with \"" + buildTarget + "\"");
            writeline("/home/ubuntu/results/coverage/Schedule/Schedule_1_10.coverage", "02858bb8-d1e5-4c69-bc5a-1827f332543d");
            return builder.buildWithTarget(properties, buildTarget, (getShowProgress() ? progress : null));
        }
        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_1_10.coverage", "01753212-b571-45d8-afb9-54af30e288f3");
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
        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_1_10.coverage", "0356ec2d-980b-4eb0-881f-f5d8dcd93a86");
        Builder builder = findBuilder(buildNumber, lastBuild, now);
        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_1_10.coverage", "0413b5ca-4ff0-4966-9383-19d067b22721");
        if (builder == null) {
            writeline("/home/ubuntu/results/coverage/Schedule/Schedule_1_10.coverage", "50c60bdb-fefe-4048-80ef-38fb46f69f8e");
            final long timeToNextBuild = getTimeToNextBuild(now, ONE_MINUTE);
            writeline("/home/ubuntu/results/coverage/Schedule/Schedule_1_10.coverage", "549566f0-9f1d-4506-9067-2a2ef7de3325");
            final Date futureDate = getFutureDate(now, timeToNextBuild);
            writeline("/home/ubuntu/results/coverage/Schedule/Schedule_1_10.coverage", "7077e184-0d30-499f-b42a-6d365d4d80d6");
            builder = findBuilder(buildNumber, now, futureDate);
        }
        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_1_10.coverage", "787e33e7-5f51-42a3-8f0c-567c95471473");
        if (builder == null) {
            writeline("/home/ubuntu/results/coverage/Schedule/Schedule_1_10.coverage", "1832e34a-d74a-472f-b86e-2b10b0d990b7");
            validate();
            writeline("/home/ubuntu/results/coverage/Schedule/Schedule_1_10.coverage", "e7fee917-d327-4792-b54d-ce7c57f7dc55");
            throw new CruiseControlException("configuration error not caught by validate? no builder selected");
        }
        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_1_10.coverage", "c26bb037-59b3-4ac3-91d4-cea71385f85c");
        return builder;
    }

    private Builder findBuilder(final int buildNumber, final Date lastBuild, final Date now) throws CruiseControlException {
        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_1_10.coverage", "017a1502-e127-460f-b4b8-c1bf5af57402");
        for (final Builder builder : builders) {
            writeline("/home/ubuntu/results/coverage/Schedule/Schedule_1_10.coverage", "59b55610-3f0a-4b5c-ac5f-11336b0b3c36");
            if (builder.isTimeBuilder()) {
                writeline("/home/ubuntu/results/coverage/Schedule/Schedule_1_10.coverage", "111f943e-0b23-4872-b468-b682310dfa47");
                final int buildTime = builder.getTime();
                writeline("/home/ubuntu/results/coverage/Schedule/Schedule_1_10.coverage", "b69064a5-1895-47d2-862f-38da4d403ba3");
                final boolean didntBuildToday = builderDidntBuildToday(lastBuild, now, buildTime);
                writeline("/home/ubuntu/results/coverage/Schedule/Schedule_1_10.coverage", "7a3c9711-a87f-4937-bcc3-fce122a792a1");
                final int nowTime = DateUtil.getTimeFromDate(now);
                writeline("/home/ubuntu/results/coverage/Schedule/Schedule_1_10.coverage", "ca0bc80f-cf39-42d8-bfdc-587fda793a1f");
                final boolean isAfterBuildTime = buildTime <= nowTime;
                writeline("/home/ubuntu/results/coverage/Schedule/Schedule_1_10.coverage", "c353e67f-feb7-4234-8c55-82a5bb376060");
                final boolean isValidDay = builder.isValidDay(now);
                writeline("/home/ubuntu/results/coverage/Schedule/Schedule_1_10.coverage", "f4de2101-0edf-437e-a849-a01dde1504a3");
                if (didntBuildToday && isAfterBuildTime && isValidDay) {
                    writeline("/home/ubuntu/results/coverage/Schedule/Schedule_1_10.coverage", "7b6703b4-7d89-49f4-b184-3988693af19e");
                    return builder;
                }
            } else if (builder.getMultiple() > 0) {
                writeline("/home/ubuntu/results/coverage/Schedule/Schedule_1_10.coverage", "26553181-b620-4499-8ac8-bcd3faa00462");
                if (builder.isValidDay(now)) {
                    writeline("/home/ubuntu/results/coverage/Schedule/Schedule_1_10.coverage", "5fed17d4-2230-4d29-aa2e-d02df5bfed4c");
                    if ((buildNumber % builder.getMultiple()) == 0) {
                        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_1_10.coverage", "49a4eef6-01bf-4935-92ad-1a23fe4818e1");
                        return builder;
                    }
                }
            } else {
                writeline("/home/ubuntu/results/coverage/Schedule/Schedule_1_10.coverage", "fa3ed898-026b-4d49-ad78-a6b60e57253c");
                throw new CruiseControlException("The selected Builder is not properly configured");
            }
        }
        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_1_10.coverage", "2b2bddb2-d27d-49e2-ab3f-ce1c0fe1de50");
        return null;
    }

    boolean builderDidntBuildToday(Date lastBuild, Date now, int buildTime) {
        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_1_10.coverage", "60f4e558-7d19-4bf7-bc98-026fa50d6254");
        int time = DateUtil.getTimeFromDate(now);
        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_1_10.coverage", "e76892d0-67b6-4218-a65f-5fd955c9b392");
        long timeMillis = DateUtil.convertToMillis(time);
        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_1_10.coverage", "5150b9bf-85b9-4cc8-8ebc-d4090045b0c7");
        long startOfToday = now.getTime() - timeMillis;
        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_1_10.coverage", "d0004415-3dcc-4d5e-a8b2-324af831c171");
        boolean lastBuildYesterday = lastBuild.getTime() < startOfToday;
        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_1_10.coverage", "8ed82c26-9433-4220-b2a5-ae839ae09524");
        boolean lastBuildTimeBeforeBuildTime = DateUtil.getTimeFromDate(lastBuild) < buildTime;
        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_1_10.coverage", "711649ea-6266-4c7c-a4a1-955a8084b49f");
        return lastBuildYesterday || lastBuildTimeBeforeBuildTime;
    }

    long getTimeToNextBuild(Date now, long sleepInterval) {
        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_1_10.coverage", "1988d727-160e-4f48-8189-e8386184ab69");
        return getTimeToNextBuild(now, sleepInterval, 0);
    }

    private long getTimeToNextBuild(Date now, long sleepInterval, long priorPauseAdjustment) {
        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_1_10.coverage", "e689dafb-bec6-4818-b22d-3c9989ce29d7");
        long timeToNextBuild = sleepInterval;
        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_1_10.coverage", "f42c5d8c-d92b-4f9f-b46f-8e81161610a6");
        LOG.debug("getTimeToNextBuild: initial timeToNextBuild = " + timeToNextBuild);
        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_1_10.coverage", "9c15f960-59d7-43df-ab0e-62761f2fb590");
        timeToNextBuild = checkMultipleBuilders(now, timeToNextBuild);
        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_1_10.coverage", "aceed378-3a0c-4ae5-9970-4bfa227dbbb3");
        LOG.debug("getTimeToNextBuild: after checkMultipleBuilders = " + timeToNextBuild);
        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_1_10.coverage", "432781e5-1873-417d-b69f-28de9b34445a");
        timeToNextBuild = checkTimeBuilders(now, timeToNextBuild);
        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_1_10.coverage", "77854114-c15f-4a3b-9977-800ea31eea54");
        LOG.debug("getTimeToNextBuild: after checkTimeBuilders = " + timeToNextBuild);
        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_1_10.coverage", "d9821632-0b57-40b2-9e18-c1a52816870d");
        long timeTillNotPaused = checkPauseBuilders(now, timeToNextBuild);
        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_1_10.coverage", "736d9396-1e9a-44f8-8604-3a95ba8065bf");
        LOG.debug("getTimeToNextBuild: after checkPauseBuilders = " + timeToNextBuild);
        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_1_10.coverage", "ad919a5c-3d73-45a4-9ed4-e630f38d3475");
        if (timeToNextBuild != timeTillNotPaused) {
            writeline("/home/ubuntu/results/coverage/Schedule/Schedule_1_10.coverage", "7bf8d041-bcd6-440f-88e5-651588c79b0f");
            boolean atMaxTime = timeTillNotPaused >= MAX_INTERVAL_MILLISECONDS || priorPauseAdjustment >= MAX_INTERVAL_MILLISECONDS;
            writeline("/home/ubuntu/results/coverage/Schedule/Schedule_1_10.coverage", "ca42a15d-98c1-4eb3-8112-397f646bdcdc");
            if (hasOnlyTimeBuilders() && !atMaxTime) {
                writeline("/home/ubuntu/results/coverage/Schedule/Schedule_1_10.coverage", "aa064e06-f414-4256-aa9a-ca0efdeae182");
                Date dateAfterPause = getFutureDate(now, timeTillNotPaused);
                writeline("/home/ubuntu/results/coverage/Schedule/Schedule_1_10.coverage", "7698ec63-0548-46a9-bd35-2768f951884c");
                long adjustmentFromEndOfPause = getTimeToNextBuild(dateAfterPause, 0, priorPauseAdjustment + timeTillNotPaused);
                writeline("/home/ubuntu/results/coverage/Schedule/Schedule_1_10.coverage", "f50665f4-a71b-4016-b134-2b083cf75ef2");
                timeToNextBuild = timeTillNotPaused + adjustmentFromEndOfPause;
                writeline("/home/ubuntu/results/coverage/Schedule/Schedule_1_10.coverage", "b9557a6b-01dd-49e7-a91d-5b3ce10afe50");
                timeToNextBuild = checkMaximumInterval(timeToNextBuild);
            } else {
                writeline("/home/ubuntu/results/coverage/Schedule/Schedule_1_10.coverage", "a98c511a-3a13-4f07-b55d-3d3c0044fa01");
                timeToNextBuild = timeTillNotPaused;
            }
        }
        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_1_10.coverage", "74b38025-e20c-4f03-83cb-6e6a802dfeaa");
        return timeToNextBuild;
    }

    private long checkMultipleBuilders(final Date now, final long interval) {
        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_1_10.coverage", "c79ab21a-fdf4-4d01-ac1c-e01c77e15508");
        if (hasOnlyTimeBuilders()) {
            writeline("/home/ubuntu/results/coverage/Schedule/Schedule_1_10.coverage", "1409c721-4e5b-4c29-9637-0712961d87f0");
            LOG.debug("has only time builders, so no correction for multiple builders.");
            writeline("/home/ubuntu/results/coverage/Schedule/Schedule_1_10.coverage", "49369fd5-1b4b-4628-8cdb-762e58b6ed7b");
            return interval;
        }
        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_1_10.coverage", "f3689820-6490-45d6-a0cf-b78865a2d64c");
        Date then = getFutureDate(now, interval);
        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_1_10.coverage", "103f0c32-cb7e-412b-bf15-3d2dec5354b1");
        final List<Builder> buildersForOtherDays = new ArrayList<Builder>();
        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_1_10.coverage", "bc1ea199-79d4-4381-8415-9a52c99f4543");
        for (final Builder builder : builders) {
            writeline("/home/ubuntu/results/coverage/Schedule/Schedule_1_10.coverage", "70d52c46-2be2-40c8-974f-f77e0ee11a98");
            if (!builder.isTimeBuilder()) {
                writeline("/home/ubuntu/results/coverage/Schedule/Schedule_1_10.coverage", "856fc3de-baee-4255-bd99-387125ca0518");
                if (builder.getMultiple() == 1) {
                    writeline("/home/ubuntu/results/coverage/Schedule/Schedule_1_10.coverage", "f96c76e7-6f18-437e-8b37-2ebba9af1463");
                    if (builder.isValidDay(then)) {
                        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_1_10.coverage", "538f0960-e840-4bc2-888c-063ed91facec");
                        LOG.debug("multiple=1 builder found that could run on " + then);
                        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_1_10.coverage", "5497241d-60cf-4d60-82b5-95dcf7a98a37");
                        return interval;
                    } else {
                        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_1_10.coverage", "7c5f0afb-c150-416d-a2e9-6153e60e4aa9");
                        buildersForOtherDays.add(builder);
                    }
                }
            }
        }
        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_1_10.coverage", "bcb950b0-600f-486c-aa45-5efa0ed2d7f1");
        if (buildersForOtherDays.size() == 0) {
            writeline("/home/ubuntu/results/coverage/Schedule/Schedule_1_10.coverage", "5016a805-167e-4118-8f26-2a2786e06798");
            LOG.error("configuration error: has some multiple builders but no multiple=1 builders found!");
            writeline("/home/ubuntu/results/coverage/Schedule/Schedule_1_10.coverage", "27ab094f-3462-40f3-804c-8780d81fd4d0");
            return interval;
        } else {
            writeline("/home/ubuntu/results/coverage/Schedule/Schedule_1_10.coverage", "856ead86-8db9-4ca5-9102-d11bf0a3c4f8");
            LOG.debug("no multiple=1 builders found for " + then + ". checking other days");
        }
        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_1_10.coverage", "6493e519-37b3-42c2-8755-72464bf35111");
        for (int i = 1; i < 7; i++) {
            writeline("/home/ubuntu/results/coverage/Schedule/Schedule_1_10.coverage", "a9fc59ef-df97-40d0-92b8-42d83ce57089");
            long daysPastInitialInterval = i * ONE_DAY;
            writeline("/home/ubuntu/results/coverage/Schedule/Schedule_1_10.coverage", "b91b1720-8a48-46da-b4ae-87960bf1f71d");
            then = getFutureDate(now, interval + daysPastInitialInterval);
            writeline("/home/ubuntu/results/coverage/Schedule/Schedule_1_10.coverage", "fe6299fe-d286-4800-a199-8595bdeed8f0");
            for (final Builder builder : builders) {
                writeline("/home/ubuntu/results/coverage/Schedule/Schedule_1_10.coverage", "d1d10687-f3aa-455d-93b8-24095d0e349d");
                if (builder.isValidDay(then)) {
                    writeline("/home/ubuntu/results/coverage/Schedule/Schedule_1_10.coverage", "7ee101dc-8e71-4966-8044-ec39c5f77afe");
                    LOG.debug("multiple=1 builder found that could run on " + then);
                    writeline("/home/ubuntu/results/coverage/Schedule/Schedule_1_10.coverage", "7a55aae2-1485-49db-bccd-90ecb5446b98");
                    long correctionToMidnight = getTimePastMidnight(then);
                    writeline("/home/ubuntu/results/coverage/Schedule/Schedule_1_10.coverage", "a2d0f1ba-ccae-40f6-b678-8cf8dbed0f6c");
                    return interval + daysPastInitialInterval - correctionToMidnight;
                }
            }
        }
        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_1_10.coverage", "d76ff78b-d2d5-4f69-a943-1a56790356c9");
        LOG.error("configuration error? could not find appropriate multiple=1 builder.");
        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_1_10.coverage", "1e4a97d5-f35d-4a6a-b077-811b7a9b0158");
        return interval;
    }

    private long getTimePastMidnight(Date date) {
        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_1_10.coverage", "bb2fedc0-3e9a-47bf-9b35-6100b2de9999");
        Calendar cal = Calendar.getInstance();
        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_1_10.coverage", "b5d5a5fe-f254-4152-94b3-25a1637de251");
        cal.setTime(date);
        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_1_10.coverage", "c145d9f7-5bfe-4b7a-b134-61f257e9d5f3");
        long time = 60 * ONE_MINUTE * cal.get(Calendar.HOUR_OF_DAY);
        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_1_10.coverage", "0ea9c2ef-9030-4fce-9e84-6eaaddc7afea");
        time += ONE_MINUTE * cal.get(Calendar.MINUTE);
        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_1_10.coverage", "7fe7f3cb-0d5f-498b-a863-934bb25f7fbd");
        return time;
    }

    private boolean hasOnlyTimeBuilders() {
        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_1_10.coverage", "9daffd67-8241-4dab-855f-bdc0be285b18");
        boolean onlyTimeBuilders = true;
        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_1_10.coverage", "963a24cc-a0b0-4367-89ba-56b4323a455d");
        for (final Builder builder : builders) {
            writeline("/home/ubuntu/results/coverage/Schedule/Schedule_1_10.coverage", "4409cc76-4f43-43f0-8170-0fe72e2d5ca4");
            if (!builder.isTimeBuilder()) {
                writeline("/home/ubuntu/results/coverage/Schedule/Schedule_1_10.coverage", "fa08a2ac-4c48-4296-82d8-aaf1abf9e762");
                onlyTimeBuilders = false;
                writeline("/home/ubuntu/results/coverage/Schedule/Schedule_1_10.coverage", "9bef9b5e-0068-45ca-a49c-f4bc257f4ad5");
                break;
            }
        }
        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_1_10.coverage", "b31294aa-e604-43eb-8181-477ace602db5");
        return onlyTimeBuilders;
    }

    long checkTimeBuilders(final Date now, final long proposedTime) {
        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_1_10.coverage", "ce4bb496-4128-46c1-8297-6141a94c8422");
        long timeToNextBuild = proposedTime;
        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_1_10.coverage", "5fb120b6-cb5d-448b-a0c7-b969de7c5396");
        if (hasOnlyTimeBuilders()) {
            writeline("/home/ubuntu/results/coverage/Schedule/Schedule_1_10.coverage", "1e8def66-f943-48eb-924d-8b2ad28fb14f");
            timeToNextBuild = Long.MAX_VALUE;
        }
        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_1_10.coverage", "1d34484d-082d-46c0-b157-41f233a4c046");
        final int nowTime = DateUtil.getTimeFromDate(now);
        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_1_10.coverage", "1b07e45b-0843-4662-8548-89bb2f347557");
        for (final Builder builder : builders) {
            writeline("/home/ubuntu/results/coverage/Schedule/Schedule_1_10.coverage", "a7c490e8-2991-458b-910d-2b131d153093");
            if (builder.isTimeBuilder()) {
                writeline("/home/ubuntu/results/coverage/Schedule/Schedule_1_10.coverage", "5a4049a3-578d-4fc7-8df6-2888435d4c7e");
                long timeToThisBuild = Long.MAX_VALUE;
                writeline("/home/ubuntu/results/coverage/Schedule/Schedule_1_10.coverage", "0630a802-6a10-4fe3-bb15-ecf6c7a1c670");
                final Calendar cal = Calendar.getInstance();
                writeline("/home/ubuntu/results/coverage/Schedule/Schedule_1_10.coverage", "3ad2f21a-715e-4b78-93fa-c7cae5ff6b18");
                final long oneYear = 365;
                writeline("/home/ubuntu/results/coverage/Schedule/Schedule_1_10.coverage", "302042b1-f4a8-4f9f-8414-78dcd10e08d4");
                for (int daysInTheFuture = 0; daysInTheFuture < oneYear; daysInTheFuture++) {
                    writeline("/home/ubuntu/results/coverage/Schedule/Schedule_1_10.coverage", "e173d751-4ffc-4754-b6f6-e75b2715fecf");
                    cal.setTime(now);
                    writeline("/home/ubuntu/results/coverage/Schedule/Schedule_1_10.coverage", "e038aa99-a7d4-41d9-9658-07c4f1b71343");
                    cal.add(Calendar.DATE, daysInTheFuture);
                    writeline("/home/ubuntu/results/coverage/Schedule/Schedule_1_10.coverage", "cb5af2ef-6144-4f38-91f9-27c0c90fad9e");
                    Date future = cal.getTime();
                    writeline("/home/ubuntu/results/coverage/Schedule/Schedule_1_10.coverage", "895be97a-7960-494a-afa3-27146395d7dd");
                    final boolean dayIsValid = builder.isValidDay(future);
                    writeline("/home/ubuntu/results/coverage/Schedule/Schedule_1_10.coverage", "44b303d7-2b3d-4952-a275-87648c013e6a");
                    if (dayIsValid) {
                        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_1_10.coverage", "c1fcce36-f843-4d7a-8d29-c23ed1a74317");
                        int thisBuildTime = builder.getTime();
                        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_1_10.coverage", "9ad9a061-1772-460b-8c13-318d5d45ef66");
                        boolean timePassedToday = (daysInTheFuture == 0) && (nowTime > thisBuildTime);
                        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_1_10.coverage", "3451af2f-5913-4273-8637-1e9ad53fe6d0");
                        if (!timePassedToday) {
                            writeline("/home/ubuntu/results/coverage/Schedule/Schedule_1_10.coverage", "f0220484-3074-4fc1-8d0e-0e684e8a72e3");
                            int buildHour = thisBuildTime / 100;
                            writeline("/home/ubuntu/results/coverage/Schedule/Schedule_1_10.coverage", "8c17ecca-e22c-4a68-b3bd-cea35609af4b");
                            int buildMinute = thisBuildTime % 100;
                            writeline("/home/ubuntu/results/coverage/Schedule/Schedule_1_10.coverage", "218a8503-5e55-4006-b299-81457b591e25");
                            cal.set(Calendar.HOUR_OF_DAY, buildHour);
                            writeline("/home/ubuntu/results/coverage/Schedule/Schedule_1_10.coverage", "109fe17b-35a3-4e46-8abf-c2eaafd446f2");
                            cal.set(Calendar.MINUTE, buildMinute);
                            writeline("/home/ubuntu/results/coverage/Schedule/Schedule_1_10.coverage", "b5c021b7-ab4d-4935-a0b1-f01befc43fab");
                            future = cal.getTime();
                            writeline("/home/ubuntu/results/coverage/Schedule/Schedule_1_10.coverage", "8632332c-c8c5-4029-9c8c-99108fc325f9");
                            timeToThisBuild = future.getTime() - now.getTime();
                            writeline("/home/ubuntu/results/coverage/Schedule/Schedule_1_10.coverage", "ca21176c-a310-4c55-9076-46e7b16ddef0");
                            break;
                        }
                    }
                }
                writeline("/home/ubuntu/results/coverage/Schedule/Schedule_1_10.coverage", "a47d2285-0329-42f2-aa5f-c5b7af946db0");
                if (timeToThisBuild < timeToNextBuild) {
                    writeline("/home/ubuntu/results/coverage/Schedule/Schedule_1_10.coverage", "47637a2f-d61e-43ad-9b6f-eadb32993a4e");
                    timeToNextBuild = timeToThisBuild;
                }
            }
        }
        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_1_10.coverage", "dd685403-12e2-46a1-8777-4b7c7bfd1270");
        if (timeToNextBuild > MAX_INTERVAL_MILLISECONDS) {
            writeline("/home/ubuntu/results/coverage/Schedule/Schedule_1_10.coverage", "fd226921-ec35-40dc-b339-7682704f8c7e");
            LOG.error("checkTimeBuilders exceeding maximum interval. using proposed value [" + proposedTime + "] instead");
            writeline("/home/ubuntu/results/coverage/Schedule/Schedule_1_10.coverage", "f3a3ce86-db1c-4a47-920a-6d31d55ba2ca");
            timeToNextBuild = proposedTime;
        }
        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_1_10.coverage", "4f209480-8a32-4ccd-b5b9-0f6139c3d264");
        return timeToNextBuild;
    }

    long checkPauseBuilders(Date now, long proposedTime) {
        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_1_10.coverage", "bb94b5af-833b-469f-8ac4-2d687e5da367");
        long oldTime = proposedTime;
        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_1_10.coverage", "85e611c5-8b6c-4d0b-9ed6-5af618b50fa6");
        long newTime = checkForPauseAtProposedTime(now, oldTime);
        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_1_10.coverage", "4d275b9c-acb0-4e11-9d1b-6afbbb9c917b");
        while (oldTime != newTime) {
            writeline("/home/ubuntu/results/coverage/Schedule/Schedule_1_10.coverage", "c5d9cb3c-9869-4d5f-bdcb-a8eafacc62a6");
            oldTime = newTime;
            writeline("/home/ubuntu/results/coverage/Schedule/Schedule_1_10.coverage", "816c4fd7-77ea-41b6-8017-2f36e7355911");
            newTime = checkForPauseAtProposedTime(now, oldTime);
        }
        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_1_10.coverage", "9045d5d2-4504-4a24-939d-4ac5f658e2a2");
        return newTime;
    }

    private long checkForPauseAtProposedTime(Date now, long proposedTime) {
        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_1_10.coverage", "632b6035-4273-448e-bef8-4f6a22fd46aa");
        Date futureDate = getFutureDate(now, proposedTime);
        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_1_10.coverage", "401f15a8-9791-410a-8447-a7a7b452b17b");
        PauseBuilder pause = findPause(futureDate);
        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_1_10.coverage", "19653ffc-844c-4bfb-8808-865bf609d84c");
        if (pause == null) {
            writeline("/home/ubuntu/results/coverage/Schedule/Schedule_1_10.coverage", "f6f3592d-d879-4acb-a0ec-30e169f80f5f");
            return proposedTime;
        }
        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_1_10.coverage", "ee15c699-a132-4a78-90b6-afb72cbacc1b");
        int endPause = pause.getEndTime();
        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_1_10.coverage", "f473b4ac-3930-47ea-80c3-aa1d1e8d8310");
        int futureTime = DateUtil.getTimeFromDate(futureDate);
        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_1_10.coverage", "0c4450ff-3bbb-42f4-a41d-31001701777f");
        long timeToEndOfPause = proposedTime + DateUtil.milliTimeDifference(futureTime, endPause);
        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_1_10.coverage", "375c112b-c764-4d24-bf9a-882c5d7a4410");
        timeToEndOfPause = checkMaximumInterval(timeToEndOfPause);
        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_1_10.coverage", "88d879a9-bcf3-4612-8ea1-144eaeb69600");
        return timeToEndOfPause == MAX_INTERVAL_MILLISECONDS ? timeToEndOfPause : timeToEndOfPause + ONE_MINUTE;
    }

    private long checkMaximumInterval(long timeToEndOfPause) {
        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_1_10.coverage", "f23d9f12-fcf9-41ad-a512-121ef1004717");
        if (timeToEndOfPause > MAX_INTERVAL_MILLISECONDS) {
            writeline("/home/ubuntu/results/coverage/Schedule/Schedule_1_10.coverage", "a399b300-e68f-40e6-b81a-a5e0803ba8d8");
            LOG.error("maximum interval exceeded! project perpetually paused?");
            writeline("/home/ubuntu/results/coverage/Schedule/Schedule_1_10.coverage", "4c6a9570-8003-4bee-a9df-24a3e2200502");
            return MAX_INTERVAL_MILLISECONDS;
        }
        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_1_10.coverage", "561f5785-7fa0-4b6c-a940-3807e86bd19e");
        return timeToEndOfPause;
    }

    private Date getFutureDate(Date now, long delay) {
        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_1_10.coverage", "6880cf1e-35bc-4489-9347-dd566ac994af");
        long futureMillis = now.getTime() + delay;
        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_1_10.coverage", "9253fd2c-8f83-4e0f-893a-d4e310c9b397");
        return new Date(futureMillis);
    }

    public void setInterval(long intervalBetweenModificationChecks) {
        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_1_10.coverage", "4c6494f4-e94c-4439-8973-ddd306af470a");
        if (intervalBetweenModificationChecks <= 0) {
            writeline("/home/ubuntu/results/coverage/Schedule/Schedule_1_10.coverage", "8ed9aaa8-7986-44e7-81c7-d408b3813230");
            throw new IllegalArgumentException("interval must be greater than zero");
        }
        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_1_10.coverage", "a74bff1c-b928-495e-8f9b-2f765941decd");
        interval = intervalBetweenModificationChecks * ONE_SECOND;
    }

    public long getInterval() {
        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_1_10.coverage", "02309cd6-cdaa-4cfe-a74a-c671ba93ffa3");
        return interval;
    }

    public void setShowProgress(final boolean showProgress) {
        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_1_10.coverage", "efbdd111-ca67-4c58-a002-6f6be7068882");
        this.showProgress = showProgress;
    }

    public boolean getShowProgress() {
        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_1_10.coverage", "897a2ff6-8cac-4b4d-ab5d-3e6c294bbb7b");
        return showProgress;
    }

    public void validate() throws CruiseControlException {
        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_1_10.coverage", "da50ad75-7b9e-4ef9-96d2-c5a0e3115097");
        ValidationHelper.assertTrue(builders.size() > 0, "schedule element requires at least one nested builder element");
        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_1_10.coverage", "d8ad79d9-4f13-45cd-aa92-e37d6d4a80b4");
        ValidationHelper.assertFalse(interval > ONE_YEAR, "maximum interval value is " + MAX_INTERVAL_SECONDS + " (one year)");
        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_1_10.coverage", "cccccdb3-6a70-4698-8c3e-c9b65c849e6a");
        if (hasOnlyTimeBuilders()) {
            writeline("/home/ubuntu/results/coverage/Schedule/Schedule_1_10.coverage", "88b2ef85-a288-4153-b516-2024522876e4");
            LOG.warn("schedule has all time based builders: interval value will be ignored.");
            writeline("/home/ubuntu/results/coverage/Schedule/Schedule_1_10.coverage", "784c715d-f2d2-443a-86e7-78af2d25469a");
            ValidationHelper.assertFalse(checkWithinPause(new ArrayList<Builder>(builders)), "all build times during pauses.");
        }
        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_1_10.coverage", "64f616c9-8e07-457f-aa3b-f520ede5ef4b");
        // Validate the child builders, since no one else seems to be doing it.
        for (final Builder next : builders) {
            writeline("/home/ubuntu/results/coverage/Schedule/Schedule_1_10.coverage", "8600e084-e93e-4cfd-8549-d0245503f1d0");
            next.validate();
        }
    }

    private boolean checkWithinPause(List timeBuilders) {
        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_1_10.coverage", "8461a361-980f-46f1-a288-bb67e96f2aac");
        for (int i = 0; i < timeBuilders.size(); i++) {
            writeline("/home/ubuntu/results/coverage/Schedule/Schedule_1_10.coverage", "b1cb83a8-0a35-4aba-82e6-d8645d792354");
            Builder builder = (Builder) timeBuilders.get(i);
            writeline("/home/ubuntu/results/coverage/Schedule/Schedule_1_10.coverage", "123a2544-2a9c-448f-9f31-e110b2c8be40");
            for (final PauseBuilder pauseBuilder : pauseBuilders) {
                writeline("/home/ubuntu/results/coverage/Schedule/Schedule_1_10.coverage", "2fe772b0-b00e-4913-8800-d85fb42d87fc");
                if (buildDaySameAsPauseDay(builder, pauseBuilder) && buildTimeWithinPauseTime(builder, pauseBuilder)) {
                    writeline("/home/ubuntu/results/coverage/Schedule/Schedule_1_10.coverage", "9c1fe287-4881-4197-9ab6-93794906d5fd");
                    timeBuilders.remove(builder);
                    writeline("/home/ubuntu/results/coverage/Schedule/Schedule_1_10.coverage", "d412ecae-d676-4696-a822-32cceeb7487c");
                    StringBuffer message = new StringBuffer();
                    writeline("/home/ubuntu/results/coverage/Schedule/Schedule_1_10.coverage", "cb00d2f6-9fa5-4c63-9b60-5d16538ddc00");
                    message.append("time Builder for time ");
                    writeline("/home/ubuntu/results/coverage/Schedule/Schedule_1_10.coverage", "c8fbf680-aa67-4303-8642-8517b1da6838");
                    message.append(Integer.toString(builder.getTime()));
                    writeline("/home/ubuntu/results/coverage/Schedule/Schedule_1_10.coverage", "979f8024-e7bb-4100-9030-58826d32d2ec");
                    if (builder.getDay() != Builder.NOT_SET) {
                        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_1_10.coverage", "67517aaa-cf9a-4e08-8efe-439a773706b2");
                        message.append(" and day of ");
                        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_1_10.coverage", "68a0537a-a6f1-420c-bdf0-02f23f558ab8");
                        message.append(getDayString(builder.getDay()));
                    }
                    writeline("/home/ubuntu/results/coverage/Schedule/Schedule_1_10.coverage", "1a4cd097-a325-4d9e-9f98-0252e8ccbd89");
                    message.append(" is always within a pause and will never build");
                    writeline("/home/ubuntu/results/coverage/Schedule/Schedule_1_10.coverage", "94ebc3cf-70a9-4bbf-9ae8-0dab7952fa29");
                    LOG.error(message.toString());
                }
            }
        }
        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_1_10.coverage", "4b9f82e9-ce00-4e0b-a494-134b478f66bd");
        return timeBuilders.isEmpty();
    }

    /**
     * @param day
     * int value
     * @return english string value
     */
    String getDayString(int day) {
        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_1_10.coverage", "37891761-4edc-4840-a1f7-fda3d06d9c44");
        if (day < 1 || day > 7) {
            writeline("/home/ubuntu/results/coverage/Schedule/Schedule_1_10.coverage", "05690efa-f3d5-42c9-bc0e-af3c9f931e8e");
            throw new IllegalArgumentException("valid values of days are between 1 and 7, was " + day);
        }
        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_1_10.coverage", "e9d2279e-8176-4cb9-bcb2-b5f7d25046d4");
        DateFormatSymbols symbols = new DateFormatSymbols(Locale.ENGLISH);
        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_1_10.coverage", "f942bfcb-96e1-4372-9424-1379f36deaba");
        String[] weekdays = symbols.getWeekdays();
        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_1_10.coverage", "a7dceae5-0840-4fcf-8f58-53fec75a4a8f");
        return weekdays[day];
    }

    private boolean buildDaySameAsPauseDay(Builder builder, PauseBuilder pauseBuilder) {
        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_1_10.coverage", "66eeb6fa-2456-4060-b2a6-cc0908254aab");
        return pauseBuilder.getDay() == PauseBuilder.NOT_SET || pauseBuilder.getDay() == builder.getDay();
    }

    private boolean buildTimeWithinPauseTime(Builder builder, PauseBuilder pauseBuilder) {
        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_1_10.coverage", "1b5fb9f8-0918-4058-ad5a-497c2a389174");
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
        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_1_10.coverage", "7ba1501f-bf1f-475b-89d8-8e7257ccc6e6");
        if (param == null) {
            writeline("/home/ubuntu/results/coverage/Schedule/Schedule_1_10.coverage", "c8ac837b-f0b1-4515-9041-fa421fe59fd3");
            throw new IllegalArgumentException(paramName + " can't be null");
        }
    }

    public List getBuilders() {
        writeline("/home/ubuntu/results/coverage/Schedule/Schedule_1_10.coverage", "2f9cf490-af23-4f2e-9e17-165795747e04");
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
