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
        writeline("/home/ubuntu/results/coverage/Hour/Hour_3_10.coverage", "1ea81d12-0aa5-4407-b254-612489dddafc");
        return this.hour;
    }

    /**
     * Returns the day in which this hour falls.
     *
     * @return The day.
     */
    public Day getDay() {
        writeline("/home/ubuntu/results/coverage/Hour/Hour_3_10.coverage", "eb4c92e4-7c29-48a5-8f8a-575f217514b1");
        return this.day;
    }

    /**
     * Returns the year in which this hour falls.
     *
     * @return The year.
     */
    public int getYear() {
        writeline("/home/ubuntu/results/coverage/Hour/Hour_3_10.coverage", "bed59c34-d4e1-4075-b560-a3d23615fd85");
        return this.day.getYear();
    }

    /**
     * Returns the month in which this hour falls.
     *
     * @return The month.
     */
    public int getMonth() {
        writeline("/home/ubuntu/results/coverage/Hour/Hour_3_10.coverage", "5181e7d5-eb74-4b7e-90d3-0da951ac6c1f");
        return this.day.getMonth();
    }

    /**
     * Returns the day-of-the-month in which this hour falls.
     *
     * @return The day-of-the-month.
     */
    public int getDayOfMonth() {
        writeline("/home/ubuntu/results/coverage/Hour/Hour_3_10.coverage", "56991c1a-7f7c-4f79-b3d1-c634451844d5");
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
        writeline("/home/ubuntu/results/coverage/Hour/Hour_3_10.coverage", "5741141d-bfbd-408b-8740-7ac914abd15b");
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
        writeline("/home/ubuntu/results/coverage/Hour/Hour_3_10.coverage", "f107b0db-fead-48dd-84f3-f9346a9ae7cc");
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
        writeline("/home/ubuntu/results/coverage/Hour/Hour_3_10.coverage", "5f194eab-a727-42a8-a209-ceae1f1df7ce");
        this.firstMillisecond = getFirstMillisecond(calendar);
        writeline("/home/ubuntu/results/coverage/Hour/Hour_3_10.coverage", "c9a5f086-4039-4f3e-809c-186b92c04dc2");
        this.lastMillisecond = getLastMillisecond(calendar);
    }

    /**
     * Returns the hour preceding this one.
     *
     * @return The hour preceding this one.
     */
    @Override
    public RegularTimePeriod previous() {
        writeline("/home/ubuntu/results/coverage/Hour/Hour_3_10.coverage", "c31d6c4f-f605-4f38-ad05-238dea72c41a");
        Hour result;
        writeline("/home/ubuntu/results/coverage/Hour/Hour_3_10.coverage", "4d5a813e-412b-4295-97dd-ba3998f53f9a");
        if (this.hour != FIRST_HOUR_IN_DAY) {
            writeline("/home/ubuntu/results/coverage/Hour/Hour_3_10.coverage", "49484eee-4edc-4aaa-85d9-522d652029ec");
            result = new Hour(this.hour - 1, this.day);
        } else {
            writeline("/home/ubuntu/results/coverage/Hour/Hour_3_10.coverage", "8e352cc6-7ccf-40c8-8ca8-f4287b9b057f");
            Day prevDay = (Day) this.day.previous();
            writeline("/home/ubuntu/results/coverage/Hour/Hour_3_10.coverage", "19054f46-b044-4fd7-a794-76bbd009f14f");
            if (prevDay != null) {
                writeline("/home/ubuntu/results/coverage/Hour/Hour_3_10.coverage", "f516bc99-d982-40ff-ac79-5a13d13bb89d");
                result = new Hour(LAST_HOUR_IN_DAY, prevDay);
            } else {
                writeline("/home/ubuntu/results/coverage/Hour/Hour_3_10.coverage", "fe0c2565-f575-4bc0-ba8e-1a933969b234");
                result = null;
            }
        }
        writeline("/home/ubuntu/results/coverage/Hour/Hour_3_10.coverage", "8be886c5-8bc4-4fd1-8692-da055d139be0");
        return result;
    }

    /**
     * Returns the hour following this one.
     *
     * @return The hour following this one.
     */
    @Override
    public RegularTimePeriod next() {
        writeline("/home/ubuntu/results/coverage/Hour/Hour_3_10.coverage", "cf99be2a-1b4b-4936-8c71-31e7d2997f87");
        Hour result;
        writeline("/home/ubuntu/results/coverage/Hour/Hour_3_10.coverage", "2771ba19-620e-4015-9f42-8bb3b3956b32");
        if (this.hour != LAST_HOUR_IN_DAY) {
            writeline("/home/ubuntu/results/coverage/Hour/Hour_3_10.coverage", "da2e7ff1-bb74-4c61-9321-ed631f8e3cef");
            result = new Hour(this.hour + 1, this.day);
        } else {
            writeline("/home/ubuntu/results/coverage/Hour/Hour_3_10.coverage", "4be0e33e-ea36-4d0c-b873-976a200b89df");
            Day nextDay = (Day) this.day.next();
            writeline("/home/ubuntu/results/coverage/Hour/Hour_3_10.coverage", "4795031f-4355-47c5-b3e6-1ea138292575");
            if (nextDay != null) {
                writeline("/home/ubuntu/results/coverage/Hour/Hour_3_10.coverage", "1fcd7e63-7aad-445b-a52b-3d8fd5a303cb");
                result = new Hour(FIRST_HOUR_IN_DAY, nextDay);
            } else {
                writeline("/home/ubuntu/results/coverage/Hour/Hour_3_10.coverage", "d1d2fae1-f257-4c05-a5a6-5476ff22f3e5");
                result = null;
            }
        }
        writeline("/home/ubuntu/results/coverage/Hour/Hour_3_10.coverage", "be8737ec-3c61-4063-aae9-b4bde11e4d13");
        return result;
    }

    /**
     * Returns a serial index number for the hour.
     *
     * @return The serial index number.
     */
    @Override
    public long getSerialIndex() {
        writeline("/home/ubuntu/results/coverage/Hour/Hour_3_10.coverage", "3bf1ed4f-0456-4d26-9e44-7f3e0f8731a0");
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
        writeline("/home/ubuntu/results/coverage/Hour/Hour_3_10.coverage", "9b4314fd-d2e1-478f-bc6c-0eb35e0dfe14");
        int year = this.day.getYear();
        writeline("/home/ubuntu/results/coverage/Hour/Hour_3_10.coverage", "ecdc5b91-32d2-4935-a507-8e01fad82ecd");
        int month = this.day.getMonth() - 1;
        writeline("/home/ubuntu/results/coverage/Hour/Hour_3_10.coverage", "ddc041bd-71d2-4815-9b2d-99fcd7b06f68");
        int dom = this.day.getDayOfMonth();
        writeline("/home/ubuntu/results/coverage/Hour/Hour_3_10.coverage", "bad79299-0852-44ea-a243-e893fc06dea5");
        calendar.set(year, month, dom, this.hour, 0, 0);
        writeline("/home/ubuntu/results/coverage/Hour/Hour_3_10.coverage", "0931e62d-0870-4efc-af4f-38ad283814de");
        calendar.set(Calendar.MILLISECOND, 0);
        writeline("/home/ubuntu/results/coverage/Hour/Hour_3_10.coverage", "619dfdff-3db9-4320-b207-9fc092ab6e8c");
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
        writeline("/home/ubuntu/results/coverage/Hour/Hour_3_10.coverage", "4b254145-0a8a-4936-b5d9-ca1a324ce629");
        int year = this.day.getYear();
        writeline("/home/ubuntu/results/coverage/Hour/Hour_3_10.coverage", "9f0e66e3-a0a9-4925-962d-4faf326c0bf3");
        int month = this.day.getMonth() - 1;
        writeline("/home/ubuntu/results/coverage/Hour/Hour_3_10.coverage", "fa9b88f7-1443-4172-ac04-fda7c5c3bd03");
        int dom = this.day.getDayOfMonth();
        writeline("/home/ubuntu/results/coverage/Hour/Hour_3_10.coverage", "c5785b37-794b-4dcc-bc45-abb62edfa59f");
        calendar.set(year, month, dom, this.hour, 59, 59);
        writeline("/home/ubuntu/results/coverage/Hour/Hour_3_10.coverage", "457b05bb-14bd-4097-ae78-02a731becf8e");
        calendar.set(Calendar.MILLISECOND, 999);
        writeline("/home/ubuntu/results/coverage/Hour/Hour_3_10.coverage", "1d58a735-68eb-411b-bfec-93a5d72bfcd7");
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
        writeline("/home/ubuntu/results/coverage/Hour/Hour_3_10.coverage", "0a654ff4-2857-4fda-ac3e-e46edfcb5e66");
        if (obj == this) {
            writeline("/home/ubuntu/results/coverage/Hour/Hour_3_10.coverage", "28a73d0f-7c81-42b1-8750-1a07b037876e");
            return true;
        }
        writeline("/home/ubuntu/results/coverage/Hour/Hour_3_10.coverage", "a96f4e81-ad4e-4b78-9a7e-a1ac9ef5be25");
        if (!(obj instanceof Hour)) {
            writeline("/home/ubuntu/results/coverage/Hour/Hour_3_10.coverage", "435a965a-8454-4136-ae16-c9cd6ab827c0");
            return false;
        }
        writeline("/home/ubuntu/results/coverage/Hour/Hour_3_10.coverage", "ea062239-ee74-4186-babc-35581c235754");
        Hour that = (Hour) obj;
        writeline("/home/ubuntu/results/coverage/Hour/Hour_3_10.coverage", "13753157-da0e-4a41-b146-d44556c7b6ce");
        if (this.hour != that.hour) {
            writeline("/home/ubuntu/results/coverage/Hour/Hour_3_10.coverage", "d034ee16-8b49-45e0-bd12-993e9ebaa017");
            return false;
        }
        writeline("/home/ubuntu/results/coverage/Hour/Hour_3_10.coverage", "69ed2ffa-4289-49b4-9966-1e6ef3ab5877");
        if (!this.day.equals(that.day)) {
            writeline("/home/ubuntu/results/coverage/Hour/Hour_3_10.coverage", "16878672-096a-41ba-98f1-f0e27cdc7e61");
            return false;
        }
        writeline("/home/ubuntu/results/coverage/Hour/Hour_3_10.coverage", "c7516470-8efe-4540-8ab5-8f2b18dde9d1");
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
        writeline("/home/ubuntu/results/coverage/Hour/Hour_3_10.coverage", "63c4a83b-3c81-4078-af83-0f4f176b6cc4");
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
        writeline("/home/ubuntu/results/coverage/Hour/Hour_3_10.coverage", "58f755cd-b4f5-42bf-a40a-ac17b91088df");
        int result = 17;
        writeline("/home/ubuntu/results/coverage/Hour/Hour_3_10.coverage", "984b6fb7-0826-45c6-80fe-f91329b04898");
        result = 37 * result + this.hour;
        writeline("/home/ubuntu/results/coverage/Hour/Hour_3_10.coverage", "33c227f0-93f1-40bb-8f08-77786ca64970");
        result = 37 * result + this.day.hashCode();
        writeline("/home/ubuntu/results/coverage/Hour/Hour_3_10.coverage", "859fa045-7f15-49e2-87f0-7f0a1409ac14");
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
        writeline("/home/ubuntu/results/coverage/Hour/Hour_3_10.coverage", "2c6a1cbd-f000-4c55-8012-42281ae6b8e4");
        int result;
        writeline("/home/ubuntu/results/coverage/Hour/Hour_3_10.coverage", "7ed3b045-b87c-4409-88f4-fc1e23168cc8");
        if (o1 instanceof Hour) {
            writeline("/home/ubuntu/results/coverage/Hour/Hour_3_10.coverage", "44effc0e-17cf-48e7-9f15-2eed9aa37148");
            Hour h = (Hour) o1;
            writeline("/home/ubuntu/results/coverage/Hour/Hour_3_10.coverage", "1fd4fd56-b8aa-4dda-8fa9-1a6aec7b1175");
            result = getDay().compareTo(h.getDay());
            writeline("/home/ubuntu/results/coverage/Hour/Hour_3_10.coverage", "d15592de-890f-41f9-ba93-0e1675ba692b");
            if (result == 0) {
                writeline("/home/ubuntu/results/coverage/Hour/Hour_3_10.coverage", "9f6864e7-e5ad-4001-ab92-59a34c44051a");
                result = this.hour - h.getHour();
            }
        } else if (o1 instanceof RegularTimePeriod) {
            writeline("/home/ubuntu/results/coverage/Hour/Hour_3_10.coverage", "959a23f3-b445-4529-98e3-d1728424e37a");
            result = 0;
        } else {
            writeline("/home/ubuntu/results/coverage/Hour/Hour_3_10.coverage", "7dc1180d-6917-48f9-b3c8-8a495c9cb661");
            result = 1;
        }
        writeline("/home/ubuntu/results/coverage/Hour/Hour_3_10.coverage", "72fd1317-13e0-4b6d-922c-0902af140de0");
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
        writeline("/home/ubuntu/results/coverage/Hour/Hour_3_10.coverage", "204b0ced-d826-4c88-bcbc-ab6e2f7b4b8c");
        Hour result = null;
        writeline("/home/ubuntu/results/coverage/Hour/Hour_3_10.coverage", "68f4f1c7-3050-4b3e-bd63-a478c9105569");
        s = s.trim();
        writeline("/home/ubuntu/results/coverage/Hour/Hour_3_10.coverage", "261c5d19-bc4f-456f-a211-72283aa50561");
        String daystr = s.substring(0, Math.min(10, s.length()));
        writeline("/home/ubuntu/results/coverage/Hour/Hour_3_10.coverage", "969657d0-67c6-4861-a4bf-639e37cfb350");
        Day day = Day.parseDay(daystr);
        writeline("/home/ubuntu/results/coverage/Hour/Hour_3_10.coverage", "955cd000-282d-40b4-be8a-9a357e9024b5");
        if (day != null) {
            writeline("/home/ubuntu/results/coverage/Hour/Hour_3_10.coverage", "7eeea966-f95c-4822-a224-776beca55beb");
            String hourstr = s.substring(Math.min(daystr.length() + 1, s.length()), s.length());
            writeline("/home/ubuntu/results/coverage/Hour/Hour_3_10.coverage", "387f10da-9023-423f-99b6-ac69985e983d");
            hourstr = hourstr.trim();
            writeline("/home/ubuntu/results/coverage/Hour/Hour_3_10.coverage", "f4da8383-652b-4d99-8e53-40e87008385f");
            int hour = Integer.parseInt(hourstr);
            writeline("/home/ubuntu/results/coverage/Hour/Hour_3_10.coverage", "6d4f013c-8c87-48ab-a587-dee793342d8e");
            if ((hour >= FIRST_HOUR_IN_DAY) && (hour <= LAST_HOUR_IN_DAY)) {
                writeline("/home/ubuntu/results/coverage/Hour/Hour_3_10.coverage", "89278ff0-4907-48e6-9f6b-8ac0561437d9");
                result = new Hour(hour, day);
            }
        }
        writeline("/home/ubuntu/results/coverage/Hour/Hour_3_10.coverage", "be36b9e3-e9d5-451e-8fc9-fdaee9c96676");
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
