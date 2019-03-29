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
        writeline("/home/ubuntu/results/coverage/Hour/Hour_7_10.coverage", "ae15d182-e717-4e48-8f40-c6d1b1bf4892");
        return this.hour;
    }

    /**
     * Returns the day in which this hour falls.
     *
     * @return The day.
     */
    public Day getDay() {
        writeline("/home/ubuntu/results/coverage/Hour/Hour_7_10.coverage", "f78d5835-e867-4eeb-a0f8-30c901350467");
        return this.day;
    }

    /**
     * Returns the year in which this hour falls.
     *
     * @return The year.
     */
    public int getYear() {
        writeline("/home/ubuntu/results/coverage/Hour/Hour_7_10.coverage", "8d5e6504-124d-41cf-b6e4-b7c50f8a804f");
        return this.day.getYear();
    }

    /**
     * Returns the month in which this hour falls.
     *
     * @return The month.
     */
    public int getMonth() {
        writeline("/home/ubuntu/results/coverage/Hour/Hour_7_10.coverage", "9e6d035c-7651-4907-ba68-af835799ecc4");
        return this.day.getMonth();
    }

    /**
     * Returns the day-of-the-month in which this hour falls.
     *
     * @return The day-of-the-month.
     */
    public int getDayOfMonth() {
        writeline("/home/ubuntu/results/coverage/Hour/Hour_7_10.coverage", "d91b5488-d5d4-4c9a-9d60-971a890c59b3");
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
        writeline("/home/ubuntu/results/coverage/Hour/Hour_7_10.coverage", "e6f7f54e-2032-4f92-8283-b68b26cb511a");
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
        writeline("/home/ubuntu/results/coverage/Hour/Hour_7_10.coverage", "3081e575-d9d1-4d31-8382-f8745ea80133");
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
        writeline("/home/ubuntu/results/coverage/Hour/Hour_7_10.coverage", "e5a80ecb-e688-45f2-b16b-c297400a6dd9");
        this.firstMillisecond = getFirstMillisecond(calendar);
        writeline("/home/ubuntu/results/coverage/Hour/Hour_7_10.coverage", "bc218fa3-d438-46ea-9fbb-9413d94d4c8d");
        this.lastMillisecond = getLastMillisecond(calendar);
    }

    /**
     * Returns the hour preceding this one.
     *
     * @return The hour preceding this one.
     */
    @Override
    public RegularTimePeriod previous() {
        writeline("/home/ubuntu/results/coverage/Hour/Hour_7_10.coverage", "2ff8f98e-1f11-45c2-9972-5e1fa3324fc8");
        Hour result;
        writeline("/home/ubuntu/results/coverage/Hour/Hour_7_10.coverage", "1502d18c-4856-474d-aadf-a12b15022e8a");
        if (this.hour != FIRST_HOUR_IN_DAY) {
            writeline("/home/ubuntu/results/coverage/Hour/Hour_7_10.coverage", "41c37dff-fbae-4e6c-901c-c5a0c09b9d78");
            result = new Hour(this.hour - 1, this.day);
        } else {
            writeline("/home/ubuntu/results/coverage/Hour/Hour_7_10.coverage", "7e391fba-719a-40b1-b4dc-fa49d607980e");
            Day prevDay = (Day) this.day.previous();
            writeline("/home/ubuntu/results/coverage/Hour/Hour_7_10.coverage", "4ce345c4-5d1b-48fc-a941-8a91b48c6e55");
            if (prevDay != null) {
                writeline("/home/ubuntu/results/coverage/Hour/Hour_7_10.coverage", "1f77c3fd-154f-4311-afec-7b3b337aaeb9");
                result = new Hour(LAST_HOUR_IN_DAY, prevDay);
            } else {
                writeline("/home/ubuntu/results/coverage/Hour/Hour_7_10.coverage", "db4a4b7f-a763-4e43-a05b-aa45a9292b02");
                result = null;
            }
        }
        writeline("/home/ubuntu/results/coverage/Hour/Hour_7_10.coverage", "4530c589-13b7-4db3-8ba3-cf18e6e16bff");
        return result;
    }

    /**
     * Returns the hour following this one.
     *
     * @return The hour following this one.
     */
    @Override
    public RegularTimePeriod next() {
        writeline("/home/ubuntu/results/coverage/Hour/Hour_7_10.coverage", "39b90fad-0727-40c2-bc01-34372b48b707");
        Hour result;
        writeline("/home/ubuntu/results/coverage/Hour/Hour_7_10.coverage", "37c94683-3d27-4eb5-a60f-f554280bf89d");
        if (this.hour != LAST_HOUR_IN_DAY) {
            writeline("/home/ubuntu/results/coverage/Hour/Hour_7_10.coverage", "bb456e08-78df-48e3-973f-bd73342f4ab8");
            result = new Hour(this.hour + 1, this.day);
        } else {
            writeline("/home/ubuntu/results/coverage/Hour/Hour_7_10.coverage", "e05422d2-bda3-48d9-b4bf-fa57c293fd5c");
            Day nextDay = (Day) this.day.next();
            writeline("/home/ubuntu/results/coverage/Hour/Hour_7_10.coverage", "f488f41b-cbd3-44b4-ba36-2c8fccf16d4d");
            if (nextDay != null) {
                writeline("/home/ubuntu/results/coverage/Hour/Hour_7_10.coverage", "c6b5e3fb-853c-4440-b561-b33cf284b1f6");
                result = new Hour(FIRST_HOUR_IN_DAY, nextDay);
            } else {
                writeline("/home/ubuntu/results/coverage/Hour/Hour_7_10.coverage", "f3ec4011-8744-4a6e-8d17-d4c4147af296");
                result = null;
            }
        }
        writeline("/home/ubuntu/results/coverage/Hour/Hour_7_10.coverage", "341186cc-3bb8-4dbb-99b7-cf25f132084a");
        return result;
    }

    /**
     * Returns a serial index number for the hour.
     *
     * @return The serial index number.
     */
    @Override
    public long getSerialIndex() {
        writeline("/home/ubuntu/results/coverage/Hour/Hour_7_10.coverage", "34a07b46-6056-4f08-a718-3ad294912c35");
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
        writeline("/home/ubuntu/results/coverage/Hour/Hour_7_10.coverage", "9c27966e-7a0d-4f5f-8581-0b6476b352f8");
        int year = this.day.getYear();
        writeline("/home/ubuntu/results/coverage/Hour/Hour_7_10.coverage", "2560eb6e-f2bb-4883-8267-6fd107beb650");
        int month = this.day.getMonth() - 1;
        writeline("/home/ubuntu/results/coverage/Hour/Hour_7_10.coverage", "889454f4-64a1-47d9-927a-d2cca39a4232");
        int dom = this.day.getDayOfMonth();
        writeline("/home/ubuntu/results/coverage/Hour/Hour_7_10.coverage", "ead63e33-2dd7-4cf2-a44b-9326e652a87a");
        calendar.set(year, month, dom, this.hour, 0, 0);
        writeline("/home/ubuntu/results/coverage/Hour/Hour_7_10.coverage", "a7d0c6f4-bbc7-42d5-afce-8c97851b3ac8");
        calendar.set(Calendar.MILLISECOND, 0);
        writeline("/home/ubuntu/results/coverage/Hour/Hour_7_10.coverage", "19aa8d80-9146-418a-b0f8-92e036d9b87b");
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
        writeline("/home/ubuntu/results/coverage/Hour/Hour_7_10.coverage", "66aab6f0-8660-4bb0-afc0-29e90e59b9b0");
        int year = this.day.getYear();
        writeline("/home/ubuntu/results/coverage/Hour/Hour_7_10.coverage", "d926e77a-e470-4baf-bf09-148c1cb8c26d");
        int month = this.day.getMonth() - 1;
        writeline("/home/ubuntu/results/coverage/Hour/Hour_7_10.coverage", "6b5cdb2d-384c-49ea-b666-6ded7242b9b1");
        int dom = this.day.getDayOfMonth();
        writeline("/home/ubuntu/results/coverage/Hour/Hour_7_10.coverage", "f28849e6-794c-47aa-b0fc-90681a4b7a33");
        calendar.set(year, month, dom, this.hour, 59, 59);
        writeline("/home/ubuntu/results/coverage/Hour/Hour_7_10.coverage", "d05c24d1-aef3-4182-8e87-29dd2fbfe8f3");
        calendar.set(Calendar.MILLISECOND, 999);
        writeline("/home/ubuntu/results/coverage/Hour/Hour_7_10.coverage", "b5d4fee3-7200-4014-a081-351490cab6be");
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
        writeline("/home/ubuntu/results/coverage/Hour/Hour_7_10.coverage", "64a7c152-006a-48d1-98b9-f0bb244aef25");
        if (obj == this) {
            writeline("/home/ubuntu/results/coverage/Hour/Hour_7_10.coverage", "0d25d9b7-636f-4c34-9d49-25bf27a2cd05");
            return true;
        }
        writeline("/home/ubuntu/results/coverage/Hour/Hour_7_10.coverage", "335b53a6-3389-4398-b26e-ae7833ca6878");
        if (!(obj instanceof Hour)) {
            writeline("/home/ubuntu/results/coverage/Hour/Hour_7_10.coverage", "aa414afb-a7b0-418a-9964-fe0d4da0502b");
            return false;
        }
        writeline("/home/ubuntu/results/coverage/Hour/Hour_7_10.coverage", "ea442300-c1df-425a-9001-5240fe242099");
        Hour that = (Hour) obj;
        writeline("/home/ubuntu/results/coverage/Hour/Hour_7_10.coverage", "0965847a-01fe-4484-a60e-680a9dfe8943");
        if (this.hour != that.hour) {
            writeline("/home/ubuntu/results/coverage/Hour/Hour_7_10.coverage", "1401b6b0-0d35-4d1a-9a41-7b534d2d972b");
            return false;
        }
        writeline("/home/ubuntu/results/coverage/Hour/Hour_7_10.coverage", "6bb67e61-40c0-43b7-af8d-e2a727cb8c8d");
        if (!this.day.equals(that.day)) {
            writeline("/home/ubuntu/results/coverage/Hour/Hour_7_10.coverage", "4474b02e-646c-4115-bfcf-f1eab4046f50");
            return false;
        }
        writeline("/home/ubuntu/results/coverage/Hour/Hour_7_10.coverage", "b11cb9a8-3b2e-4aee-88ba-f9e882862d67");
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
        writeline("/home/ubuntu/results/coverage/Hour/Hour_7_10.coverage", "5a35c112-eae5-4e78-a17e-8c00adbc585e");
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
        writeline("/home/ubuntu/results/coverage/Hour/Hour_7_10.coverage", "cc6092b5-cf39-4712-9967-51430a6a9b44");
        int result = 17;
        writeline("/home/ubuntu/results/coverage/Hour/Hour_7_10.coverage", "0e203edc-4782-4f96-b742-0952a1069034");
        result = 37 * result + this.hour;
        writeline("/home/ubuntu/results/coverage/Hour/Hour_7_10.coverage", "4be81070-2c21-4b64-ba7f-18d6af6f71f0");
        result = 37 * result + this.day.hashCode();
        writeline("/home/ubuntu/results/coverage/Hour/Hour_7_10.coverage", "4bbcb5dd-b29c-48e3-bb53-a562dcab778f");
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
        writeline("/home/ubuntu/results/coverage/Hour/Hour_7_10.coverage", "f0619267-97fb-46b9-8297-ae6fb10c0cbc");
        int result;
        writeline("/home/ubuntu/results/coverage/Hour/Hour_7_10.coverage", "d3e3dddb-fc2e-4871-bfc7-bccb8a31aef2");
        if (o1 instanceof Hour) {
            writeline("/home/ubuntu/results/coverage/Hour/Hour_7_10.coverage", "efb17fb5-e0d7-48cc-8f4d-8f10e8d81a3f");
            Hour h = (Hour) o1;
            writeline("/home/ubuntu/results/coverage/Hour/Hour_7_10.coverage", "8a0957f2-96fd-45c5-95b9-b22eae91586b");
            result = getDay().compareTo(h.getDay());
            writeline("/home/ubuntu/results/coverage/Hour/Hour_7_10.coverage", "3b6b8ca9-e868-4e23-8314-b6842b69efb4");
            if (result == 0) {
                writeline("/home/ubuntu/results/coverage/Hour/Hour_7_10.coverage", "3e1fea8c-39ab-4c51-aff0-fbfd55ea5b4d");
                result = this.hour - h.getHour();
            }
        } else if (o1 instanceof RegularTimePeriod) {
            writeline("/home/ubuntu/results/coverage/Hour/Hour_7_10.coverage", "01e96040-87d1-44c9-a824-68918d7acd95");
            result = 0;
        } else {
            writeline("/home/ubuntu/results/coverage/Hour/Hour_7_10.coverage", "6104f606-6ab0-46cd-b180-19f0f84ef307");
            result = 1;
        }
        writeline("/home/ubuntu/results/coverage/Hour/Hour_7_10.coverage", "5c46a37f-4b04-494e-9c49-ba5ffee24ac5");
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
        writeline("/home/ubuntu/results/coverage/Hour/Hour_7_10.coverage", "7a6915e3-6a8b-43c4-947b-f30395cacd3d");
        Hour result = null;
        writeline("/home/ubuntu/results/coverage/Hour/Hour_7_10.coverage", "d0a1fd14-3097-4d98-8e05-e36210cf15f7");
        s = s.trim();
        writeline("/home/ubuntu/results/coverage/Hour/Hour_7_10.coverage", "0ef4adce-5abc-44c8-a2e4-093599ffb4dc");
        String daystr = s.substring(0, Math.min(10, s.length()));
        writeline("/home/ubuntu/results/coverage/Hour/Hour_7_10.coverage", "ccfa74d4-66f8-4e7b-8ebe-ad16a13507ae");
        Day day = Day.parseDay(daystr);
        writeline("/home/ubuntu/results/coverage/Hour/Hour_7_10.coverage", "bd1aa1e6-4553-44a9-8ce3-5b08e648de41");
        if (day != null) {
            writeline("/home/ubuntu/results/coverage/Hour/Hour_7_10.coverage", "98c309f8-c48b-4a9c-81c5-dd66f234c1ee");
            String hourstr = s.substring(Math.min(daystr.length() + 1, s.length()), s.length());
            writeline("/home/ubuntu/results/coverage/Hour/Hour_7_10.coverage", "48f6ac4c-7ff1-4ffd-99b8-5599b86cb4c5");
            hourstr = hourstr.trim();
            writeline("/home/ubuntu/results/coverage/Hour/Hour_7_10.coverage", "82e762fc-258a-4c9a-89d6-553bcb6d95a0");
            int hour = Integer.parseInt(hourstr);
            writeline("/home/ubuntu/results/coverage/Hour/Hour_7_10.coverage", "61c5df38-1289-4b06-aff9-f281658b80a7");
            if ((hour >= FIRST_HOUR_IN_DAY) && (hour <= LAST_HOUR_IN_DAY)) {
                writeline("/home/ubuntu/results/coverage/Hour/Hour_7_10.coverage", "b7f90410-8261-4c2a-b795-4c4a898aec1a");
                result = new Hour(hour, day);
            }
        }
        writeline("/home/ubuntu/results/coverage/Hour/Hour_7_10.coverage", "af08a68f-1408-4515-9ae6-28b0f0f544af");
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
