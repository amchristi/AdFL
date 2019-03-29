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
        writeline("/home/ubuntu/results/coverage/Hour/Hour_9_10.coverage", "2bbda532-f1d8-4492-aab6-60504780a643");
        return this.hour;
    }

    /**
     * Returns the day in which this hour falls.
     *
     * @return The day.
     */
    public Day getDay() {
        writeline("/home/ubuntu/results/coverage/Hour/Hour_9_10.coverage", "356284e6-1fca-4847-8fe5-0a6065545cc4");
        return this.day;
    }

    /**
     * Returns the year in which this hour falls.
     *
     * @return The year.
     */
    public int getYear() {
        writeline("/home/ubuntu/results/coverage/Hour/Hour_9_10.coverage", "246c83ab-ee90-4c22-a74a-2824d8fe9f28");
        return this.day.getYear();
    }

    /**
     * Returns the month in which this hour falls.
     *
     * @return The month.
     */
    public int getMonth() {
        writeline("/home/ubuntu/results/coverage/Hour/Hour_9_10.coverage", "9595fb8e-e17d-4af2-80ae-9cd469feeea5");
        return this.day.getMonth();
    }

    /**
     * Returns the day-of-the-month in which this hour falls.
     *
     * @return The day-of-the-month.
     */
    public int getDayOfMonth() {
        writeline("/home/ubuntu/results/coverage/Hour/Hour_9_10.coverage", "a732eedd-2341-48ba-bda6-32cffd56d020");
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
        writeline("/home/ubuntu/results/coverage/Hour/Hour_9_10.coverage", "ed2239dd-6f2d-423f-b2c4-60798789c397");
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
        writeline("/home/ubuntu/results/coverage/Hour/Hour_9_10.coverage", "69d082fe-7835-41ac-a2a8-44c8969aa33e");
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
        writeline("/home/ubuntu/results/coverage/Hour/Hour_9_10.coverage", "524b4573-e716-463d-be8f-0f3df9500149");
        this.firstMillisecond = getFirstMillisecond(calendar);
        writeline("/home/ubuntu/results/coverage/Hour/Hour_9_10.coverage", "a836d41b-8c6c-4100-9592-e8c32bdeca3a");
        this.lastMillisecond = getLastMillisecond(calendar);
    }

    /**
     * Returns the hour preceding this one.
     *
     * @return The hour preceding this one.
     */
    @Override
    public RegularTimePeriod previous() {
        writeline("/home/ubuntu/results/coverage/Hour/Hour_9_10.coverage", "98c1ef6d-a5aa-4cb9-a1e6-7eb60630ced7");
        Hour result;
        writeline("/home/ubuntu/results/coverage/Hour/Hour_9_10.coverage", "2d85118f-fe16-4783-ba28-1ef9ae474956");
        if (this.hour != FIRST_HOUR_IN_DAY) {
            writeline("/home/ubuntu/results/coverage/Hour/Hour_9_10.coverage", "83f01523-4553-4351-ae50-ba60ec52d3cd");
            result = new Hour(this.hour - 1, this.day);
        } else {
            writeline("/home/ubuntu/results/coverage/Hour/Hour_9_10.coverage", "252b9845-aed1-4b3a-8ccc-9cc1be8104d9");
            Day prevDay = (Day) this.day.previous();
            writeline("/home/ubuntu/results/coverage/Hour/Hour_9_10.coverage", "e0a9e344-a65a-43d6-9885-1db4dfe2efa9");
            if (prevDay != null) {
                writeline("/home/ubuntu/results/coverage/Hour/Hour_9_10.coverage", "b1a68b38-d023-40df-bc35-5a8e09e17663");
                result = new Hour(LAST_HOUR_IN_DAY, prevDay);
            } else {
                writeline("/home/ubuntu/results/coverage/Hour/Hour_9_10.coverage", "e5cb1a37-ef2e-4ecd-ad4a-89246e2ff77a");
                result = null;
            }
        }
        writeline("/home/ubuntu/results/coverage/Hour/Hour_9_10.coverage", "c042793b-b57a-474f-a495-b344b854f03a");
        return result;
    }

    /**
     * Returns the hour following this one.
     *
     * @return The hour following this one.
     */
    @Override
    public RegularTimePeriod next() {
        writeline("/home/ubuntu/results/coverage/Hour/Hour_9_10.coverage", "9046104b-239f-4cda-b196-7d35312278cd");
        Hour result;
        writeline("/home/ubuntu/results/coverage/Hour/Hour_9_10.coverage", "0762242f-0675-4fe3-b17b-cfca46dc2c87");
        if (this.hour != LAST_HOUR_IN_DAY) {
            writeline("/home/ubuntu/results/coverage/Hour/Hour_9_10.coverage", "004995eb-0e60-4c82-acec-905c6685d85c");
            result = new Hour(this.hour + 1, this.day);
        } else {
            writeline("/home/ubuntu/results/coverage/Hour/Hour_9_10.coverage", "b3f97a59-aae4-4fbc-b7f3-161cbc802ada");
            Day nextDay = (Day) this.day.next();
            writeline("/home/ubuntu/results/coverage/Hour/Hour_9_10.coverage", "a22d97d3-0257-4ab9-9a16-8b5c04176b80");
            if (nextDay != null) {
                writeline("/home/ubuntu/results/coverage/Hour/Hour_9_10.coverage", "25ee7e35-6c94-48bf-bede-fdcc1f6775a1");
                result = new Hour(FIRST_HOUR_IN_DAY, nextDay);
            } else {
                writeline("/home/ubuntu/results/coverage/Hour/Hour_9_10.coverage", "9bc90af9-e3e2-4a32-b51b-6f7525314593");
                result = null;
            }
        }
        writeline("/home/ubuntu/results/coverage/Hour/Hour_9_10.coverage", "e4de2091-300a-43e7-b85c-e31f973607bf");
        return result;
    }

    /**
     * Returns a serial index number for the hour.
     *
     * @return The serial index number.
     */
    @Override
    public long getSerialIndex() {
        writeline("/home/ubuntu/results/coverage/Hour/Hour_9_10.coverage", "6f3624c0-dd50-4e8b-8256-c21f45b83bca");
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
        writeline("/home/ubuntu/results/coverage/Hour/Hour_9_10.coverage", "f8c2efe6-14ba-40e4-a00b-0af5a3fdc752");
        int year = this.day.getYear();
        writeline("/home/ubuntu/results/coverage/Hour/Hour_9_10.coverage", "61d57a4a-9a35-4ba7-b4de-befb46a8a7de");
        int month = this.day.getMonth() - 1;
        writeline("/home/ubuntu/results/coverage/Hour/Hour_9_10.coverage", "5e6cde41-5fbd-4b69-ae6f-93ea8c4de036");
        int dom = this.day.getDayOfMonth();
        writeline("/home/ubuntu/results/coverage/Hour/Hour_9_10.coverage", "1f077256-20ce-4200-a4f4-6eb3ec851bcb");
        calendar.set(year, month, dom, this.hour, 0, 0);
        writeline("/home/ubuntu/results/coverage/Hour/Hour_9_10.coverage", "4e65c076-9012-4170-b327-6af87c5bdbf9");
        calendar.set(Calendar.MILLISECOND, 0);
        writeline("/home/ubuntu/results/coverage/Hour/Hour_9_10.coverage", "8ccc17c7-05c7-480b-b0b4-b0ad4f2bde3f");
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
        writeline("/home/ubuntu/results/coverage/Hour/Hour_9_10.coverage", "65cef30b-6220-43dc-b151-e3261c0e947e");
        int year = this.day.getYear();
        writeline("/home/ubuntu/results/coverage/Hour/Hour_9_10.coverage", "84bfb4f8-e413-4ca2-a0d0-2ab3d2f5c655");
        int month = this.day.getMonth() - 1;
        writeline("/home/ubuntu/results/coverage/Hour/Hour_9_10.coverage", "384d0000-fd82-43c9-8fcb-75c527bb9b57");
        int dom = this.day.getDayOfMonth();
        writeline("/home/ubuntu/results/coverage/Hour/Hour_9_10.coverage", "dbff0861-9780-484e-ae6f-1f740d30dd13");
        calendar.set(year, month, dom, this.hour, 59, 59);
        writeline("/home/ubuntu/results/coverage/Hour/Hour_9_10.coverage", "ece321d5-aa0a-40d5-a0bf-70d3e77ceadd");
        calendar.set(Calendar.MILLISECOND, 999);
        writeline("/home/ubuntu/results/coverage/Hour/Hour_9_10.coverage", "7eb3a661-285a-4576-9970-62502d8b506e");
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
        writeline("/home/ubuntu/results/coverage/Hour/Hour_9_10.coverage", "e83a145d-e8bb-420b-a232-f25507c5cc37");
        if (obj == this) {
            writeline("/home/ubuntu/results/coverage/Hour/Hour_9_10.coverage", "aab4c667-7bec-48b3-a10c-e867ee07975c");
            return true;
        }
        writeline("/home/ubuntu/results/coverage/Hour/Hour_9_10.coverage", "ccc68079-86f4-4930-ae26-b3e5240ad272");
        if (!(obj instanceof Hour)) {
            writeline("/home/ubuntu/results/coverage/Hour/Hour_9_10.coverage", "e775abcb-a090-48b2-97f4-f3f71b4933d4");
            return false;
        }
        writeline("/home/ubuntu/results/coverage/Hour/Hour_9_10.coverage", "d90d3c4c-f82f-4a4d-9083-a9bea321c914");
        Hour that = (Hour) obj;
        writeline("/home/ubuntu/results/coverage/Hour/Hour_9_10.coverage", "edb59c86-709e-4409-b25f-bf8dee21314e");
        if (this.hour != that.hour) {
            writeline("/home/ubuntu/results/coverage/Hour/Hour_9_10.coverage", "ce5073c4-b516-4fe5-8be6-24d9d3e52a20");
            return false;
        }
        writeline("/home/ubuntu/results/coverage/Hour/Hour_9_10.coverage", "090d8c12-1894-429c-9e14-a583b73a3552");
        if (!this.day.equals(that.day)) {
            writeline("/home/ubuntu/results/coverage/Hour/Hour_9_10.coverage", "0deaf215-f308-4681-94b8-fd10c1f8ce91");
            return false;
        }
        writeline("/home/ubuntu/results/coverage/Hour/Hour_9_10.coverage", "45eb7453-1e6b-4223-bdb6-5501eced73ad");
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
        writeline("/home/ubuntu/results/coverage/Hour/Hour_9_10.coverage", "f1a92b92-647d-4c25-9757-3a2692900f26");
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
        writeline("/home/ubuntu/results/coverage/Hour/Hour_9_10.coverage", "675f4873-09ec-41ba-bb78-13cb735ff289");
        int result = 17;
        writeline("/home/ubuntu/results/coverage/Hour/Hour_9_10.coverage", "897dd532-b255-441d-a1f8-421befa4a3e6");
        result = 37 * result + this.hour;
        writeline("/home/ubuntu/results/coverage/Hour/Hour_9_10.coverage", "0c4e0b61-697b-464f-ac30-3554ed8815f0");
        result = 37 * result + this.day.hashCode();
        writeline("/home/ubuntu/results/coverage/Hour/Hour_9_10.coverage", "6bfba7c9-dfae-4e96-a4fc-a22d43edf912");
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
        writeline("/home/ubuntu/results/coverage/Hour/Hour_9_10.coverage", "c44a4f43-5ace-4ead-988b-097a53605895");
        int result;
        writeline("/home/ubuntu/results/coverage/Hour/Hour_9_10.coverage", "c8f7aa0b-a6b7-4c7e-846a-5c24543d91b0");
        if (o1 instanceof Hour) {
            writeline("/home/ubuntu/results/coverage/Hour/Hour_9_10.coverage", "35cf0c9a-7cc6-4fdd-81cd-7903faf86e30");
            Hour h = (Hour) o1;
            writeline("/home/ubuntu/results/coverage/Hour/Hour_9_10.coverage", "9f451817-461e-4844-a7ae-8651a06df85c");
            result = getDay().compareTo(h.getDay());
            writeline("/home/ubuntu/results/coverage/Hour/Hour_9_10.coverage", "c5cbe98c-4158-4c90-a999-9851192c1e1a");
            if (result == 0) {
                writeline("/home/ubuntu/results/coverage/Hour/Hour_9_10.coverage", "178b8d7d-28c7-4ee5-bbdf-b3cf805c043f");
                result = this.hour - h.getHour();
            }
        } else if (o1 instanceof RegularTimePeriod) {
            writeline("/home/ubuntu/results/coverage/Hour/Hour_9_10.coverage", "08fe5017-3143-464e-83aa-065916f98ac0");
            result = 0;
        } else {
            writeline("/home/ubuntu/results/coverage/Hour/Hour_9_10.coverage", "352a70b0-e740-47e1-8602-8711ae79e63c");
            result = 1;
        }
        writeline("/home/ubuntu/results/coverage/Hour/Hour_9_10.coverage", "168a27e8-4488-401c-bd52-0bc457d6aafc");
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
        writeline("/home/ubuntu/results/coverage/Hour/Hour_9_10.coverage", "60e0d814-a57f-4ffe-9503-1dcefd2899cf");
        Hour result = null;
        writeline("/home/ubuntu/results/coverage/Hour/Hour_9_10.coverage", "b781a717-7184-4195-a3e4-524eb3e36708");
        s = s.trim();
        writeline("/home/ubuntu/results/coverage/Hour/Hour_9_10.coverage", "0a6f2468-e3b6-4eb0-becb-a2e080bb8ea4");
        String daystr = s.substring(0, Math.min(10, s.length()));
        writeline("/home/ubuntu/results/coverage/Hour/Hour_9_10.coverage", "cc771a67-1097-4f9c-9273-5260ec946e0b");
        Day day = Day.parseDay(daystr);
        writeline("/home/ubuntu/results/coverage/Hour/Hour_9_10.coverage", "955d5301-7277-4d86-8e16-7c36c4660e18");
        if (day != null) {
            writeline("/home/ubuntu/results/coverage/Hour/Hour_9_10.coverage", "7abcdc54-6de8-4a68-bdd3-13b0c31d18ec");
            String hourstr = s.substring(Math.min(daystr.length() + 1, s.length()), s.length());
            writeline("/home/ubuntu/results/coverage/Hour/Hour_9_10.coverage", "e9cbd411-8abc-4e3e-b54f-0b12165ceb0b");
            hourstr = hourstr.trim();
            writeline("/home/ubuntu/results/coverage/Hour/Hour_9_10.coverage", "8898296f-c205-48ef-ac7a-9d413eb2c8a1");
            int hour = Integer.parseInt(hourstr);
            writeline("/home/ubuntu/results/coverage/Hour/Hour_9_10.coverage", "28f9d559-e0ba-4406-9b3b-be544b210c47");
            if ((hour >= FIRST_HOUR_IN_DAY) && (hour <= LAST_HOUR_IN_DAY)) {
                writeline("/home/ubuntu/results/coverage/Hour/Hour_9_10.coverage", "88858882-d512-4171-bd66-6d95c33406ef");
                result = new Hour(hour, day);
            }
        }
        writeline("/home/ubuntu/results/coverage/Hour/Hour_9_10.coverage", "a8e556c6-23bd-4de5-ab7a-2892b690d1c5");
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
