package org.jfree.data.time;

import java.io.Serializable;
import java.util.Calendar;
import java.util.Date;
import java.util.Locale;
import java.util.TimeZone;
import org.jfree.chart.util.Args;
import java.io.*;

/**
 * Represents an hour in a specific day.  This class is immutable, which is a
 * requirement for all {@link RegularTimePeriod} subclasses.
 */
public class Hour extends RegularTimePeriod implements Serializable {

    /** For serialization. */
    private static final long serialVersionUID = -835471579831937652L;

    /** Useful constant for the first hour in the day. */
    public static final int FIRST_HOUR_IN_DAY = 0;

    /** Useful constant for the last hour in the day. */
    public static final int LAST_HOUR_IN_DAY = 23;

    /** The day. */
    private Day day;

    /** The hour. */
    private byte hour;

    /** The first millisecond. */
    private long firstMillisecond;

    /** The last millisecond. */
    private long lastMillisecond;

    /**
     * Constructs a new Hour, based on the system date/time.
     */
    public Hour() {
        this(new Date());
    }

    /**
     * Constructs a new Hour.
     *
     * @param hour  the hour (in the range 0 to 23).
     * @param day  the day ({@code null} not permitted).
     */
    public Hour(int hour, Day day) {
        Args.nullNotPermitted(day, "day");
        this.hour = (byte) hour;
        this.day = day;
        peg(Calendar.getInstance());
    }

    /**
     * Creates a new hour.
     *
     * @param hour  the hour (0-23).
     * @param day  the day (1-31).
     * @param month  the month (1-12).
     * @param year  the year (1900-9999).
     */
    public Hour(int hour, int day, int month, int year) {
        this(hour, new Day(day, month, year));
    }

    /**
     * Constructs a new instance, based on the supplied date/time and
     * the default time zone.
     *
     * @param time  the date-time ({@code null} not permitted).
     *
     * @see #Hour(Date, TimeZone, Locale)
     */
    public Hour(Date time) {
        this(time, TimeZone.getDefault(), Locale.getDefault());
    }

    /**
     * Constructs a new instance, based on the supplied date/time evaluated
     * in the specified time zone.
     *
     * @param time  the date-time ({@code null} not permitted).
     * @param zone  the time zone ({@code null} not permitted).
     * @param locale  the locale ({@code null} not permitted).
     *
     * @since 1.0.13
     */
    public Hour(Date time, TimeZone zone, Locale locale) {
        Args.nullNotPermitted(time, "time");
        Args.nullNotPermitted(zone, "zone");
        Args.nullNotPermitted(locale, "locale");
        Calendar calendar = Calendar.getInstance(zone, locale);
        calendar.setTime(time);
        this.hour = (byte) calendar.get(Calendar.HOUR_OF_DAY);
        this.day = new Day(time, zone, locale);
        peg(calendar);
    }

    /**
     * Returns the hour.
     *
     * @return The hour (0 &lt;= hour &lt;= 23).
     */
    public int getHour() {
        writeline("/home/ubuntu/results/coverage/Hour/Hour_2_10.coverage", "337aecc8-8669-4650-b85b-c234fe49519f");
        return this.hour;
    }

    /**
     * Returns the day in which this hour falls.
     *
     * @return The day.
     */
    public Day getDay() {
        writeline("/home/ubuntu/results/coverage/Hour/Hour_2_10.coverage", "8d46978d-6559-4a4f-9602-22d854e437e1");
        return this.day;
    }

    /**
     * Returns the year in which this hour falls.
     *
     * @return The year.
     */
    public int getYear() {
        writeline("/home/ubuntu/results/coverage/Hour/Hour_2_10.coverage", "a090edcf-3ab4-4209-a5c5-544f63074bab");
        return this.day.getYear();
    }

    /**
     * Returns the month in which this hour falls.
     *
     * @return The month.
     */
    public int getMonth() {
        writeline("/home/ubuntu/results/coverage/Hour/Hour_2_10.coverage", "60e0eee1-7c66-490d-bf00-12e2616eb0d1");
        return this.day.getMonth();
    }

    /**
     * Returns the day-of-the-month in which this hour falls.
     *
     * @return The day-of-the-month.
     */
    public int getDayOfMonth() {
        writeline("/home/ubuntu/results/coverage/Hour/Hour_2_10.coverage", "7ecca72f-cad7-4a64-a18c-c9e2052f5750");
        return this.day.getDayOfMonth();
    }

    /**
     * Returns the first millisecond of the hour.  This will be determined
     * relative to the time zone specified in the constructor, or in the
     * calendar instance passed in the most recent call to the
     * {@link #peg(Calendar)} method.
     *
     * @return The first millisecond of the hour.
     *
     * @see #getLastMillisecond()
     */
    @Override
    public long getFirstMillisecond() {
        writeline("/home/ubuntu/results/coverage/Hour/Hour_2_10.coverage", "fe06ab3c-e395-434d-8ba1-775ebbd7720a");
        return this.firstMillisecond;
    }

    /**
     * Returns the last millisecond of the hour.  This will be
     * determined relative to the time zone specified in the constructor, or
     * in the calendar instance passed in the most recent call to the
     * {@link #peg(Calendar)} method.
     *
     * @return The last millisecond of the hour.
     *
     * @see #getFirstMillisecond()
     */
    @Override
    public long getLastMillisecond() {
        writeline("/home/ubuntu/results/coverage/Hour/Hour_2_10.coverage", "92ec0daf-983a-47a4-8afa-acf3615df5d3");
        return this.lastMillisecond;
    }

    /**
     * Recalculates the start date/time and end date/time for this time period
     * relative to the supplied calendar (which incorporates a time zone).
     *
     * @param calendar  the calendar ({@code null} not permitted).
     *
     * @since 1.0.3
     */
    @Override
    public void peg(Calendar calendar) {
        writeline("/home/ubuntu/results/coverage/Hour/Hour_2_10.coverage", "e6a4bc4f-5b11-4bd7-b84f-29947ad61244");
        this.firstMillisecond = getFirstMillisecond(calendar);
        writeline("/home/ubuntu/results/coverage/Hour/Hour_2_10.coverage", "11cf7cce-1f86-4c5d-9ff8-0752919b4ec0");
        this.lastMillisecond = getLastMillisecond(calendar);
    }

    /**
     * Returns the hour preceding this one.
     *
     * @return The hour preceding this one.
     */
    @Override
    public RegularTimePeriod previous() {
        writeline("/home/ubuntu/results/coverage/Hour/Hour_2_10.coverage", "5611bd6c-1c99-47ae-8d40-a3f316eb0ce6");
        Hour result;
        writeline("/home/ubuntu/results/coverage/Hour/Hour_2_10.coverage", "a0bf15a4-0fbc-44a6-94b9-b079ff50d5ef");
        if (this.hour != FIRST_HOUR_IN_DAY) {
            writeline("/home/ubuntu/results/coverage/Hour/Hour_2_10.coverage", "fb77988f-e7d9-474a-94f2-8b5216e06d5b");
            result = new Hour(this.hour - 1, this.day);
        } else {
            writeline("/home/ubuntu/results/coverage/Hour/Hour_2_10.coverage", "2e2fdb51-e785-410c-95c8-d061c505195a");
            Day prevDay = (Day) this.day.previous();
            writeline("/home/ubuntu/results/coverage/Hour/Hour_2_10.coverage", "cad013fc-2ae1-4b95-87fc-eacbc897082a");
            if (prevDay != null) {
                writeline("/home/ubuntu/results/coverage/Hour/Hour_2_10.coverage", "c6ae7a47-1396-4c07-986a-847da49f3282");
                result = new Hour(LAST_HOUR_IN_DAY, prevDay);
            } else {
                writeline("/home/ubuntu/results/coverage/Hour/Hour_2_10.coverage", "99b15429-4287-43f6-928b-d0ade633ba8a");
                result = null;
            }
        }
        writeline("/home/ubuntu/results/coverage/Hour/Hour_2_10.coverage", "90a8e862-a78c-4752-abd8-9f2917d3ce9e");
        return result;
    }

    /**
     * Returns the hour following this one.
     *
     * @return The hour following this one.
     */
    @Override
    public RegularTimePeriod next() {
        writeline("/home/ubuntu/results/coverage/Hour/Hour_2_10.coverage", "1afda3df-1ea8-408c-87f2-5d53f2ebf012");
        Hour result;
        writeline("/home/ubuntu/results/coverage/Hour/Hour_2_10.coverage", "d1d5171d-7628-44f4-8bdd-c25031e63b6d");
        if (this.hour != LAST_HOUR_IN_DAY) {
            writeline("/home/ubuntu/results/coverage/Hour/Hour_2_10.coverage", "cba5f1d1-ea90-4eaa-b0bd-277a363a7d83");
            result = new Hour(this.hour + 1, this.day);
        } else {
            writeline("/home/ubuntu/results/coverage/Hour/Hour_2_10.coverage", "09a10c49-b36e-4542-98d5-eee2446cb853");
            Day nextDay = (Day) this.day.next();
            writeline("/home/ubuntu/results/coverage/Hour/Hour_2_10.coverage", "75af7696-f252-48d2-a435-3ecd0582d9be");
            if (nextDay != null) {
                writeline("/home/ubuntu/results/coverage/Hour/Hour_2_10.coverage", "253dd970-b96c-4b27-af39-c188272c5586");
                result = new Hour(FIRST_HOUR_IN_DAY, nextDay);
            } else {
                writeline("/home/ubuntu/results/coverage/Hour/Hour_2_10.coverage", "b5457dd6-e0fc-4ef9-88cf-df9ebb18f4fe");
                result = null;
            }
        }
        writeline("/home/ubuntu/results/coverage/Hour/Hour_2_10.coverage", "e8b1706a-2923-4ffb-abd7-5653d718fc9e");
        return result;
    }

    /**
     * Returns a serial index number for the hour.
     *
     * @return The serial index number.
     */
    @Override
    public long getSerialIndex() {
        writeline("/home/ubuntu/results/coverage/Hour/Hour_2_10.coverage", "f3e5405e-4d53-4b96-8456-e6db82602435");
        return this.day.getSerialIndex() * 24L + this.hour;
    }

    /**
     * Returns the first millisecond of the hour.
     *
     * @param calendar  the calendar/timezone ({@code null} not permitted).
     *
     * @return The first millisecond.
     *
     * @throws NullPointerException if {@code calendar} is
     *     {@code null}.
     */
    @Override
    public long getFirstMillisecond(Calendar calendar) {
        writeline("/home/ubuntu/results/coverage/Hour/Hour_2_10.coverage", "792766e5-0216-4f05-bfa0-fdcb89f37acf");
        int year = this.day.getYear();
        writeline("/home/ubuntu/results/coverage/Hour/Hour_2_10.coverage", "60f1f365-df0f-41de-9cb0-a2b98d76646d");
        int month = this.day.getMonth() - 1;
        writeline("/home/ubuntu/results/coverage/Hour/Hour_2_10.coverage", "eb5c2ebb-969f-429b-b610-0abd5534ac03");
        int dom = this.day.getDayOfMonth();
        writeline("/home/ubuntu/results/coverage/Hour/Hour_2_10.coverage", "76082ed7-d050-4da9-8e42-7c6421d8c513");
        calendar.set(year, month, dom, this.hour, 0, 0);
        writeline("/home/ubuntu/results/coverage/Hour/Hour_2_10.coverage", "c726e32f-99c4-4ae8-b7ee-8f1fd24c3ef4");
        calendar.set(Calendar.MILLISECOND, 0);
        writeline("/home/ubuntu/results/coverage/Hour/Hour_2_10.coverage", "99efcf6e-966a-4ec4-80e6-93de7ef321cc");
        return calendar.getTimeInMillis();
    }

    /**
     * Returns the last millisecond of the hour.
     *
     * @param calendar  the calendar/timezone ({@code null} not permitted).
     *
     * @return The last millisecond.
     *
     * @throws NullPointerException if {@code calendar} is
     *     {@code null}.
     */
    @Override
    public long getLastMillisecond(Calendar calendar) {
        writeline("/home/ubuntu/results/coverage/Hour/Hour_2_10.coverage", "8729d1ef-9598-4b6e-a25a-9d096da2a703");
        int year = this.day.getYear();
        writeline("/home/ubuntu/results/coverage/Hour/Hour_2_10.coverage", "4ed84e0c-48f4-4396-9b61-791a329eca18");
        int month = this.day.getMonth() - 1;
        writeline("/home/ubuntu/results/coverage/Hour/Hour_2_10.coverage", "8f8a6299-f79b-459f-be04-8a60449b3aea");
        int dom = this.day.getDayOfMonth();
        writeline("/home/ubuntu/results/coverage/Hour/Hour_2_10.coverage", "7bca2daa-186b-47d5-9978-9c16b8576396");
        calendar.set(year, month, dom, this.hour, 59, 59);
        writeline("/home/ubuntu/results/coverage/Hour/Hour_2_10.coverage", "52b5453a-9663-4d4a-bfb7-23519e262c0c");
        calendar.set(Calendar.MILLISECOND, 999);
        writeline("/home/ubuntu/results/coverage/Hour/Hour_2_10.coverage", "f8305289-6277-4332-9038-d0ffd6cdbbc4");
        return calendar.getTimeInMillis();
    }

    /**
     * Tests the equality of this object against an arbitrary Object.
     * <P>
     * This method will return true ONLY if the object is an Hour object
     * representing the same hour as this instance.
     *
     * @param obj  the object to compare ({@code null} permitted).
     *
     * @return {@code true} if the hour and day value of the object
     *      is the same as this.
     */
    @Override
    public boolean equals(Object obj) {
        writeline("/home/ubuntu/results/coverage/Hour/Hour_2_10.coverage", "94467d5d-0264-435f-bfd3-6fcf4e0c0d3f");
        if (obj == this) {
            writeline("/home/ubuntu/results/coverage/Hour/Hour_2_10.coverage", "0e6cccd7-af8b-4918-ae7d-157c56d9b7ce");
            return true;
        }
        writeline("/home/ubuntu/results/coverage/Hour/Hour_2_10.coverage", "5d895b20-af50-4ace-b312-d08abb4ce09b");
        if (!(obj instanceof Hour)) {
            writeline("/home/ubuntu/results/coverage/Hour/Hour_2_10.coverage", "837bd325-50c9-4615-a526-1cbf4725be61");
            return false;
        }
        writeline("/home/ubuntu/results/coverage/Hour/Hour_2_10.coverage", "1fed7360-bc20-459a-9213-8f16d95eed41");
        Hour that = (Hour) obj;
        writeline("/home/ubuntu/results/coverage/Hour/Hour_2_10.coverage", "050017ca-b3a5-48ba-9c97-22c06730de78");
        if (this.hour != that.hour) {
            writeline("/home/ubuntu/results/coverage/Hour/Hour_2_10.coverage", "48a31d99-4be1-45d9-b048-107ff0aba75d");
            return false;
        }
        writeline("/home/ubuntu/results/coverage/Hour/Hour_2_10.coverage", "b5d5149e-4cd2-46e7-af23-6f754199d1b1");
        if (!this.day.equals(that.day)) {
            writeline("/home/ubuntu/results/coverage/Hour/Hour_2_10.coverage", "4da7e2cd-19d2-47a4-97b3-486e672c9c44");
            return false;
        }
        writeline("/home/ubuntu/results/coverage/Hour/Hour_2_10.coverage", "bb189d0d-3b9e-4a8f-af05-ccaf79406be6");
        return true;
    }

    /**
     * Returns a string representation of this instance, for debugging
     * purposes.
     *
     * @return A string.
     */
    @Override
    public String toString() {
        writeline("/home/ubuntu/results/coverage/Hour/Hour_2_10.coverage", "b016524c-3e29-4252-8550-da83a0096141");
        return "[" + this.hour + "," + getDayOfMonth() + "/" + getMonth() + "/" + getYear() + "]";
    }

    /**
     * Returns a hash code for this object instance.  The approach described by
     * Joshua Bloch in "Effective Java" has been used here:
     * <p>
     * {@code http://developer.java.sun.com/developer/Books/effectivejava
     * /Chapter3.pdf}
     *
     * @return A hash code.
     */
    @Override
    public int hashCode() {
        writeline("/home/ubuntu/results/coverage/Hour/Hour_2_10.coverage", "f51ebb29-14f6-4e24-8124-d93e0ee8567e");
        int result = 17;
        writeline("/home/ubuntu/results/coverage/Hour/Hour_2_10.coverage", "40c90e48-b81f-478e-b1d9-3f99a71c849d");
        result = 37 * result + this.hour;
        writeline("/home/ubuntu/results/coverage/Hour/Hour_2_10.coverage", "53a29550-3f61-4919-bfe0-052411ae387b");
        result = 37 * result + this.day.hashCode();
        writeline("/home/ubuntu/results/coverage/Hour/Hour_2_10.coverage", "6cf9cfbb-b1dc-485d-a2cb-78768191337f");
        return result;
    }

    /**
     * Returns an integer indicating the order of this Hour object relative to
     * the specified object:
     *
     * negative == before, zero == same, positive == after.
     *
     * @param o1  the object to compare.
     *
     * @return negative == before, zero == same, positive == after.
     */
    @Override
    public int compareTo(Object o1) {
        writeline("/home/ubuntu/results/coverage/Hour/Hour_2_10.coverage", "fe53bd24-f6ed-4139-8c04-b2af62126bac");
        int result;
        writeline("/home/ubuntu/results/coverage/Hour/Hour_2_10.coverage", "eb882f71-2564-48c5-9ec6-ded1ead83cb3");
        if (o1 instanceof Hour) {
            writeline("/home/ubuntu/results/coverage/Hour/Hour_2_10.coverage", "bd03e693-f7c7-4f9c-9912-50e85476e13d");
            Hour h = (Hour) o1;
            writeline("/home/ubuntu/results/coverage/Hour/Hour_2_10.coverage", "6b9459df-3965-4aaf-b802-2c947166c728");
            result = getDay().compareTo(h.getDay());
            writeline("/home/ubuntu/results/coverage/Hour/Hour_2_10.coverage", "fdff595a-5e7d-46c3-8293-7c3de0e71470");
            if (result == 0) {
                writeline("/home/ubuntu/results/coverage/Hour/Hour_2_10.coverage", "8b539d8d-e743-4e9c-9988-e768a89cf9de");
                result = this.hour - h.getHour();
            }
        } else if (o1 instanceof RegularTimePeriod) {
            writeline("/home/ubuntu/results/coverage/Hour/Hour_2_10.coverage", "809deeea-a730-4a35-9a05-86df4d75b177");
            result = 0;
        } else {
            writeline("/home/ubuntu/results/coverage/Hour/Hour_2_10.coverage", "6dccc5b2-e5e4-4ff6-95ec-2e66463125b0");
            result = 1;
        }
        writeline("/home/ubuntu/results/coverage/Hour/Hour_2_10.coverage", "07f01525-1477-42ce-bebd-6c371aa5dab9");
        return result;
    }

    /**
     * Creates an Hour instance by parsing a string.  The string is assumed to
     * be in the format "YYYY-MM-DD HH", perhaps with leading or trailing
     * whitespace.
     *
     * @param s  the hour string to parse.
     *
     * @return {@code null} if the string is not parseable, the hour
     *         otherwise.
     */
    public static Hour parseHour(String s) {
        writeline("/home/ubuntu/results/coverage/Hour/Hour_2_10.coverage", "4e3ecad1-3517-498e-bf34-e3969b3938cd");
        Hour result = null;
        writeline("/home/ubuntu/results/coverage/Hour/Hour_2_10.coverage", "02f93b46-68b3-492b-9de4-98b8d6353214");
        s = s.trim();
        writeline("/home/ubuntu/results/coverage/Hour/Hour_2_10.coverage", "3861dde6-880d-4118-8b7f-ebb8044ae35d");
        String daystr = s.substring(0, Math.min(10, s.length()));
        writeline("/home/ubuntu/results/coverage/Hour/Hour_2_10.coverage", "1adb2598-0ebb-4796-a2ba-429dab82ae0a");
        Day day = Day.parseDay(daystr);
        writeline("/home/ubuntu/results/coverage/Hour/Hour_2_10.coverage", "bd925535-0962-41d1-bfb7-64a5c0e9cf76");
        if (day != null) {
            writeline("/home/ubuntu/results/coverage/Hour/Hour_2_10.coverage", "105f3fbc-d9d8-4e77-9f0b-05462e117489");
            String hourstr = s.substring(Math.min(daystr.length() + 1, s.length()), s.length());
            writeline("/home/ubuntu/results/coverage/Hour/Hour_2_10.coverage", "3d427029-c347-43de-bda5-9c82197c9fcc");
            hourstr = hourstr.trim();
            writeline("/home/ubuntu/results/coverage/Hour/Hour_2_10.coverage", "da5a9d3e-11c7-4bb4-8a43-2653e8e78abf");
            int hour = Integer.parseInt(hourstr);
            writeline("/home/ubuntu/results/coverage/Hour/Hour_2_10.coverage", "8131c7ab-59c6-43f7-a983-3bcb28217550");
            if ((hour >= FIRST_HOUR_IN_DAY) && (hour <= LAST_HOUR_IN_DAY)) {
                writeline("/home/ubuntu/results/coverage/Hour/Hour_2_10.coverage", "03a9ad57-b357-4f59-aa70-d718f18c2dc2");
                result = new Hour(hour, day);
            }
        }
        writeline("/home/ubuntu/results/coverage/Hour/Hour_2_10.coverage", "b4a2c0e3-ee22-4ce9-844a-d90f5e5a184c");
        return result;
    }

    public void writeline(String fullFilePath, String text) {
        try {
            java.io.File file = new File(fullFilePath);
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
