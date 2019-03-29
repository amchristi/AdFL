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
        writeline("/home/ubuntu/results/coverage/Hour/Hour_6_10.coverage", "2bc52c91-afa3-4e1d-bab6-78f352cb3b13");
        return this.hour;
    }

    /**
     * Returns the day in which this hour falls.
     *
     * @return The day.
     */
    public Day getDay() {
        writeline("/home/ubuntu/results/coverage/Hour/Hour_6_10.coverage", "21bc913f-92ce-42a5-b286-b2c2a28bc77f");
        return this.day;
    }

    /**
     * Returns the year in which this hour falls.
     *
     * @return The year.
     */
    public int getYear() {
        writeline("/home/ubuntu/results/coverage/Hour/Hour_6_10.coverage", "cac7e50f-a939-482c-be39-19bf58c49b88");
        return this.day.getYear();
    }

    /**
     * Returns the month in which this hour falls.
     *
     * @return The month.
     */
    public int getMonth() {
        writeline("/home/ubuntu/results/coverage/Hour/Hour_6_10.coverage", "a2e6cd3f-6b27-4f74-92a2-3fb52007037c");
        return this.day.getMonth();
    }

    /**
     * Returns the day-of-the-month in which this hour falls.
     *
     * @return The day-of-the-month.
     */
    public int getDayOfMonth() {
        writeline("/home/ubuntu/results/coverage/Hour/Hour_6_10.coverage", "ef94ce90-3cb8-473e-b25d-34c54184a061");
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
        writeline("/home/ubuntu/results/coverage/Hour/Hour_6_10.coverage", "66223e9b-01ca-403f-8aa1-3dd67b5be2a9");
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
        writeline("/home/ubuntu/results/coverage/Hour/Hour_6_10.coverage", "d8a2e7f3-1626-493f-8129-e9afde22d386");
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
        writeline("/home/ubuntu/results/coverage/Hour/Hour_6_10.coverage", "d1671824-181b-454b-b644-fe0ce8f36017");
        this.firstMillisecond = getFirstMillisecond(calendar);
        writeline("/home/ubuntu/results/coverage/Hour/Hour_6_10.coverage", "2fdc95cd-579f-411d-a6f8-54fd9dfc77e5");
        this.lastMillisecond = getLastMillisecond(calendar);
    }

    /**
     * Returns the hour preceding this one.
     *
     * @return The hour preceding this one.
     */
    @Override
    public RegularTimePeriod previous() {
        writeline("/home/ubuntu/results/coverage/Hour/Hour_6_10.coverage", "6972eeba-69d0-4f7d-9e70-fc478098fbd2");
        Hour result;
        writeline("/home/ubuntu/results/coverage/Hour/Hour_6_10.coverage", "2d4f13ef-14fc-455f-86fb-21fc2dbde83b");
        if (this.hour != FIRST_HOUR_IN_DAY) {
            writeline("/home/ubuntu/results/coverage/Hour/Hour_6_10.coverage", "542fcc1e-78f4-4120-b515-898cb4eb15a7");
            result = new Hour(this.hour - 1, this.day);
        } else {
            writeline("/home/ubuntu/results/coverage/Hour/Hour_6_10.coverage", "f32c86af-7e4a-41f3-a654-ea078514d25e");
            Day prevDay = (Day) this.day.previous();
            writeline("/home/ubuntu/results/coverage/Hour/Hour_6_10.coverage", "f88ae34d-1b34-4e94-88b9-fb733e870c37");
            if (prevDay != null) {
                writeline("/home/ubuntu/results/coverage/Hour/Hour_6_10.coverage", "56eda658-67b0-4571-ad39-051a6cd98a11");
                result = new Hour(LAST_HOUR_IN_DAY, prevDay);
            } else {
                writeline("/home/ubuntu/results/coverage/Hour/Hour_6_10.coverage", "2be285f4-5392-4679-b4b1-5cba0d6694bf");
                result = null;
            }
        }
        writeline("/home/ubuntu/results/coverage/Hour/Hour_6_10.coverage", "a3cd77bc-d8ae-43b4-8888-e1fdf435515d");
        return result;
    }

    /**
     * Returns the hour following this one.
     *
     * @return The hour following this one.
     */
    @Override
    public RegularTimePeriod next() {
        writeline("/home/ubuntu/results/coverage/Hour/Hour_6_10.coverage", "648a1c4f-220b-4499-bad1-d5d484bb9a9a");
        Hour result;
        writeline("/home/ubuntu/results/coverage/Hour/Hour_6_10.coverage", "a936608f-6b9c-4912-a0d5-eeb253799c77");
        if (this.hour != LAST_HOUR_IN_DAY) {
            writeline("/home/ubuntu/results/coverage/Hour/Hour_6_10.coverage", "dee57edc-dad0-4a41-a252-1676b227fab1");
            result = new Hour(this.hour + 1, this.day);
        } else {
            writeline("/home/ubuntu/results/coverage/Hour/Hour_6_10.coverage", "5e50dede-dde5-478f-82b2-e1f34278600d");
            Day nextDay = (Day) this.day.next();
            writeline("/home/ubuntu/results/coverage/Hour/Hour_6_10.coverage", "f76a4020-fd0c-4a73-b932-d63b0429ff12");
            if (nextDay != null) {
                writeline("/home/ubuntu/results/coverage/Hour/Hour_6_10.coverage", "d7e14f3b-b77c-489d-8d11-7965dc373613");
                result = new Hour(FIRST_HOUR_IN_DAY, nextDay);
            } else {
                writeline("/home/ubuntu/results/coverage/Hour/Hour_6_10.coverage", "ace77896-e365-4260-aef3-4e59bf837454");
                result = null;
            }
        }
        writeline("/home/ubuntu/results/coverage/Hour/Hour_6_10.coverage", "7505e55d-89b6-49de-9efb-586296638031");
        return result;
    }

    /**
     * Returns a serial index number for the hour.
     *
     * @return The serial index number.
     */
    @Override
    public long getSerialIndex() {
        writeline("/home/ubuntu/results/coverage/Hour/Hour_6_10.coverage", "5822dc52-b89f-4db2-a33a-053c5bbc8437");
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
        writeline("/home/ubuntu/results/coverage/Hour/Hour_6_10.coverage", "3b340ebd-3311-4330-b1ac-f392c1a12d7b");
        int year = this.day.getYear();
        writeline("/home/ubuntu/results/coverage/Hour/Hour_6_10.coverage", "14a06bca-328f-48f0-ba5c-ad250bc34e20");
        int month = this.day.getMonth() - 1;
        writeline("/home/ubuntu/results/coverage/Hour/Hour_6_10.coverage", "629eb8cf-911e-4403-af85-1ff9f092ff9e");
        int dom = this.day.getDayOfMonth();
        writeline("/home/ubuntu/results/coverage/Hour/Hour_6_10.coverage", "12c2bbb4-9a38-415e-93f3-b8e7e80f320c");
        calendar.set(year, month, dom, this.hour, 0, 0);
        writeline("/home/ubuntu/results/coverage/Hour/Hour_6_10.coverage", "5ad28967-d750-4c5f-92d9-df6507cca6a2");
        calendar.set(Calendar.MILLISECOND, 0);
        writeline("/home/ubuntu/results/coverage/Hour/Hour_6_10.coverage", "e0d9d488-dd06-43b3-9c55-a799a60dc6e5");
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
        writeline("/home/ubuntu/results/coverage/Hour/Hour_6_10.coverage", "82f886e9-a96e-4362-8228-568640a406d5");
        int year = this.day.getYear();
        writeline("/home/ubuntu/results/coverage/Hour/Hour_6_10.coverage", "c3d582ea-4b8e-464f-8bd8-0e7e1d0bf706");
        int month = this.day.getMonth() - 1;
        writeline("/home/ubuntu/results/coverage/Hour/Hour_6_10.coverage", "6dd8e0f1-a9d7-4b68-b0f9-6b0f053a6f56");
        int dom = this.day.getDayOfMonth();
        writeline("/home/ubuntu/results/coverage/Hour/Hour_6_10.coverage", "49701d38-1a8a-4471-9a92-3d3ff27c5da0");
        calendar.set(year, month, dom, this.hour, 59, 59);
        writeline("/home/ubuntu/results/coverage/Hour/Hour_6_10.coverage", "bfda987f-94bd-489a-a7f2-6a9af60cc473");
        calendar.set(Calendar.MILLISECOND, 999);
        writeline("/home/ubuntu/results/coverage/Hour/Hour_6_10.coverage", "1c3b350a-c30e-4537-b83e-4c7700437717");
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
        writeline("/home/ubuntu/results/coverage/Hour/Hour_6_10.coverage", "b8f2c8e2-0277-417d-9ef0-00671d069285");
        if (obj == this) {
            writeline("/home/ubuntu/results/coverage/Hour/Hour_6_10.coverage", "d136638f-c333-46fb-aef3-dde953aed89f");
            return true;
        }
        writeline("/home/ubuntu/results/coverage/Hour/Hour_6_10.coverage", "4ec872bd-f84f-4d33-a2fa-a862ad0197a5");
        if (!(obj instanceof Hour)) {
            writeline("/home/ubuntu/results/coverage/Hour/Hour_6_10.coverage", "d29bae61-e679-4ba5-88df-bfbc1a2a0ddb");
            return false;
        }
        writeline("/home/ubuntu/results/coverage/Hour/Hour_6_10.coverage", "13fb89a4-d5be-4997-8d01-59cef7ffeeb5");
        Hour that = (Hour) obj;
        writeline("/home/ubuntu/results/coverage/Hour/Hour_6_10.coverage", "6ac9ae2d-ff9b-4a47-87c1-6a63a64b9531");
        if (this.hour != that.hour) {
            writeline("/home/ubuntu/results/coverage/Hour/Hour_6_10.coverage", "27446fec-68a1-4c80-869e-55865d91ce07");
            return false;
        }
        writeline("/home/ubuntu/results/coverage/Hour/Hour_6_10.coverage", "b0df322f-95a2-4345-b2d6-dc972c3c3f74");
        if (!this.day.equals(that.day)) {
            writeline("/home/ubuntu/results/coverage/Hour/Hour_6_10.coverage", "a85b956e-8627-4422-b30d-2fd5d2c13098");
            return false;
        }
        writeline("/home/ubuntu/results/coverage/Hour/Hour_6_10.coverage", "4c7319c9-490c-4426-9f27-34542ee3f31b");
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
        writeline("/home/ubuntu/results/coverage/Hour/Hour_6_10.coverage", "92ee65e5-de85-4354-86fa-f7af59f86724");
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
        writeline("/home/ubuntu/results/coverage/Hour/Hour_6_10.coverage", "6f6dbbd5-9769-405f-b2bb-0e9a58e3d1b8");
        int result = 17;
        writeline("/home/ubuntu/results/coverage/Hour/Hour_6_10.coverage", "0845476a-f883-48e0-9e00-c5ecf5e630c7");
        result = 37 * result + this.hour;
        writeline("/home/ubuntu/results/coverage/Hour/Hour_6_10.coverage", "737602ec-db46-4ae7-8616-11ecf1e5e0db");
        result = 37 * result + this.day.hashCode();
        writeline("/home/ubuntu/results/coverage/Hour/Hour_6_10.coverage", "01a22dc7-94cb-4a64-91af-679a5548aca9");
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
        writeline("/home/ubuntu/results/coverage/Hour/Hour_6_10.coverage", "6b043c9b-9103-4263-83e7-85e24d323e73");
        int result;
        writeline("/home/ubuntu/results/coverage/Hour/Hour_6_10.coverage", "ed409bd7-36a6-4092-8d18-d96e68f793ab");
        if (o1 instanceof Hour) {
            writeline("/home/ubuntu/results/coverage/Hour/Hour_6_10.coverage", "5e7ec4d3-5789-4929-ac68-5e33a88cc880");
            Hour h = (Hour) o1;
            writeline("/home/ubuntu/results/coverage/Hour/Hour_6_10.coverage", "8bff0b88-3d8f-40e4-8e10-1c3c49a7f9c4");
            result = getDay().compareTo(h.getDay());
            writeline("/home/ubuntu/results/coverage/Hour/Hour_6_10.coverage", "f6031a63-4552-43ee-b4ca-5ad7201a2603");
            if (result == 0) {
                writeline("/home/ubuntu/results/coverage/Hour/Hour_6_10.coverage", "48d34eca-84d8-4a0b-a8a0-ce079ac9118e");
                result = this.hour - h.getHour();
            }
        } else if (o1 instanceof RegularTimePeriod) {
            writeline("/home/ubuntu/results/coverage/Hour/Hour_6_10.coverage", "75954951-e489-4187-90a7-dcfa7e692d2c");
            result = 0;
        } else {
            writeline("/home/ubuntu/results/coverage/Hour/Hour_6_10.coverage", "50bb9472-db8d-4a75-843a-b664964f9596");
            result = 1;
        }
        writeline("/home/ubuntu/results/coverage/Hour/Hour_6_10.coverage", "bb10452f-e4a4-4644-8526-1f0e9aa7b347");
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
        writeline("/home/ubuntu/results/coverage/Hour/Hour_6_10.coverage", "95e9435e-b7ed-4a01-b090-8077f47ac454");
        Hour result = null;
        writeline("/home/ubuntu/results/coverage/Hour/Hour_6_10.coverage", "ecdd012c-492d-4fdf-87c5-2bcc76333359");
        s = s.trim();
        writeline("/home/ubuntu/results/coverage/Hour/Hour_6_10.coverage", "89ecadf7-6ec6-48f1-9e33-870ea212755d");
        String daystr = s.substring(0, Math.min(10, s.length()));
        writeline("/home/ubuntu/results/coverage/Hour/Hour_6_10.coverage", "9d62e977-0431-4fb9-94bb-bde0b3a8cd58");
        Day day = Day.parseDay(daystr);
        writeline("/home/ubuntu/results/coverage/Hour/Hour_6_10.coverage", "0135a781-2f6c-4da9-bb53-7927e32bfa25");
        if (day != null) {
            writeline("/home/ubuntu/results/coverage/Hour/Hour_6_10.coverage", "444dea40-1a4c-4485-be44-86a65988aaab");
            String hourstr = s.substring(Math.min(daystr.length() + 1, s.length()), s.length());
            writeline("/home/ubuntu/results/coverage/Hour/Hour_6_10.coverage", "b169f0e5-71f3-48c0-8386-9ceb7cfe9b0a");
            hourstr = hourstr.trim();
            writeline("/home/ubuntu/results/coverage/Hour/Hour_6_10.coverage", "71b4acb0-13eb-4724-9b0b-c780bf0a6f7f");
            int hour = Integer.parseInt(hourstr);
            writeline("/home/ubuntu/results/coverage/Hour/Hour_6_10.coverage", "7262a1af-91c1-4444-9b68-b446e6c86087");
            if ((hour >= FIRST_HOUR_IN_DAY) && (hour <= LAST_HOUR_IN_DAY)) {
                writeline("/home/ubuntu/results/coverage/Hour/Hour_6_10.coverage", "debb5daa-df72-4562-b2c3-377342489820");
                result = new Hour(hour, day);
            }
        }
        writeline("/home/ubuntu/results/coverage/Hour/Hour_6_10.coverage", "983542c0-3be3-4023-b590-0867e14f70d2");
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
