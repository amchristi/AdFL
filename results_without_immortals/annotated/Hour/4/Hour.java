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
        writeline("/home/ubuntu/results/coverage/Hour/Hour_4_10.coverage", "acca7f62-c1b8-4825-b25b-c411a51000de");
        return this.hour;
    }

    /**
     * Returns the day in which this hour falls.
     *
     * @return The day.
     */
    public Day getDay() {
        writeline("/home/ubuntu/results/coverage/Hour/Hour_4_10.coverage", "e4f5ab6c-62b7-4d9f-a484-12409893ab16");
        return this.day;
    }

    /**
     * Returns the year in which this hour falls.
     *
     * @return The year.
     */
    public int getYear() {
        writeline("/home/ubuntu/results/coverage/Hour/Hour_4_10.coverage", "5aec4444-6fb4-4165-9957-b4a6464c36b2");
        return this.day.getYear();
    }

    /**
     * Returns the month in which this hour falls.
     *
     * @return The month.
     */
    public int getMonth() {
        writeline("/home/ubuntu/results/coverage/Hour/Hour_4_10.coverage", "a783aff7-eac7-4771-9d0a-1d62b45319c5");
        return this.day.getMonth();
    }

    /**
     * Returns the day-of-the-month in which this hour falls.
     *
     * @return The day-of-the-month.
     */
    public int getDayOfMonth() {
        writeline("/home/ubuntu/results/coverage/Hour/Hour_4_10.coverage", "eb8a4f9f-5dd6-4b9f-88b3-c142b4d7518b");
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
        writeline("/home/ubuntu/results/coverage/Hour/Hour_4_10.coverage", "6977c5b2-dcde-4536-81cc-cf7580b2cccd");
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
        writeline("/home/ubuntu/results/coverage/Hour/Hour_4_10.coverage", "b1402698-6e90-4be1-90fa-74ac9033355b");
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
        writeline("/home/ubuntu/results/coverage/Hour/Hour_4_10.coverage", "8fc5e225-09ac-4d9a-8d53-f2b44b63f6ea");
        this.firstMillisecond = getFirstMillisecond(calendar);
        writeline("/home/ubuntu/results/coverage/Hour/Hour_4_10.coverage", "499be5eb-6871-43e8-9233-55d44283957e");
        this.lastMillisecond = getLastMillisecond(calendar);
    }

    /**
     * Returns the hour preceding this one.
     *
     * @return The hour preceding this one.
     */
    @Override
    public RegularTimePeriod previous() {
        writeline("/home/ubuntu/results/coverage/Hour/Hour_4_10.coverage", "ab6c2149-d047-4e75-b792-035adf3e1ca2");
        Hour result;
        writeline("/home/ubuntu/results/coverage/Hour/Hour_4_10.coverage", "db284e85-efda-4459-919c-a6466f9227a3");
        if (this.hour != FIRST_HOUR_IN_DAY) {
            writeline("/home/ubuntu/results/coverage/Hour/Hour_4_10.coverage", "cdd3023a-1176-4d57-9571-c903dd1ae50f");
            result = new Hour(this.hour - 1, this.day);
        } else {
            writeline("/home/ubuntu/results/coverage/Hour/Hour_4_10.coverage", "38664d7d-c21c-4952-a025-3fe97d6d8734");
            Day prevDay = (Day) this.day.previous();
            writeline("/home/ubuntu/results/coverage/Hour/Hour_4_10.coverage", "eb0f7c7d-be04-4c85-8e70-5ca298d0e70b");
            if (prevDay != null) {
                writeline("/home/ubuntu/results/coverage/Hour/Hour_4_10.coverage", "bd4b0961-c18f-43cb-a944-12e47dbfb34f");
                result = new Hour(LAST_HOUR_IN_DAY, prevDay);
            } else {
                writeline("/home/ubuntu/results/coverage/Hour/Hour_4_10.coverage", "bc5ee674-718f-4257-aa4b-8f01174f0192");
                result = null;
            }
        }
        writeline("/home/ubuntu/results/coverage/Hour/Hour_4_10.coverage", "51f7bb9e-3e62-4c86-8e00-d2c72513c19b");
        return result;
    }

    /**
     * Returns the hour following this one.
     *
     * @return The hour following this one.
     */
    @Override
    public RegularTimePeriod next() {
        writeline("/home/ubuntu/results/coverage/Hour/Hour_4_10.coverage", "d265d268-601d-4077-b20c-fd7e3e156f18");
        Hour result;
        writeline("/home/ubuntu/results/coverage/Hour/Hour_4_10.coverage", "1219d844-1235-41cb-b0f2-15231adec7cd");
        if (this.hour != LAST_HOUR_IN_DAY) {
            writeline("/home/ubuntu/results/coverage/Hour/Hour_4_10.coverage", "0ecede4c-835e-4856-bde0-0856ab8b232d");
            result = new Hour(this.hour + 1, this.day);
        } else {
            writeline("/home/ubuntu/results/coverage/Hour/Hour_4_10.coverage", "e6b33690-9fec-4b6e-9c66-f768e7ca8e94");
            Day nextDay = (Day) this.day.next();
            writeline("/home/ubuntu/results/coverage/Hour/Hour_4_10.coverage", "9131b217-f11b-4d0a-87c0-e6e285cfa61e");
            if (nextDay != null) {
                writeline("/home/ubuntu/results/coverage/Hour/Hour_4_10.coverage", "51521457-f621-4bed-ad5a-83531f03a904");
                result = new Hour(FIRST_HOUR_IN_DAY, nextDay);
            } else {
                writeline("/home/ubuntu/results/coverage/Hour/Hour_4_10.coverage", "5b2e9563-9049-401e-b03b-8f33d87d1802");
                result = null;
            }
        }
        writeline("/home/ubuntu/results/coverage/Hour/Hour_4_10.coverage", "5e36b1a9-e2dd-43dd-8a6f-00a8dd5bf516");
        return result;
    }

    /**
     * Returns a serial index number for the hour.
     *
     * @return The serial index number.
     */
    @Override
    public long getSerialIndex() {
        writeline("/home/ubuntu/results/coverage/Hour/Hour_4_10.coverage", "8304dadc-9e3a-41cc-8e20-19a6d2b4d136");
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
        writeline("/home/ubuntu/results/coverage/Hour/Hour_4_10.coverage", "af979725-41b9-4057-86f8-0f7aaa8ddb0a");
        int year = this.day.getYear();
        writeline("/home/ubuntu/results/coverage/Hour/Hour_4_10.coverage", "ef2d5957-c276-44d4-811f-a0a02a129580");
        int month = this.day.getMonth() - 1;
        writeline("/home/ubuntu/results/coverage/Hour/Hour_4_10.coverage", "4e951dba-85af-44b6-925d-782901a468a4");
        int dom = this.day.getDayOfMonth();
        writeline("/home/ubuntu/results/coverage/Hour/Hour_4_10.coverage", "02080866-eb71-491d-a344-925846b49ce3");
        calendar.set(year, month, dom, this.hour, 0, 0);
        writeline("/home/ubuntu/results/coverage/Hour/Hour_4_10.coverage", "f40ad370-27d9-4e40-a65d-0c41b25772af");
        calendar.set(Calendar.MILLISECOND, 0);
        writeline("/home/ubuntu/results/coverage/Hour/Hour_4_10.coverage", "7aa665c0-818f-4b9a-9176-006c19c76588");
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
        writeline("/home/ubuntu/results/coverage/Hour/Hour_4_10.coverage", "0fd2e91b-340c-4af5-b669-8584189692bc");
        int year = this.day.getYear();
        writeline("/home/ubuntu/results/coverage/Hour/Hour_4_10.coverage", "035a6fdb-bee8-4781-b7fb-895c79e12b76");
        int month = this.day.getMonth() - 1;
        writeline("/home/ubuntu/results/coverage/Hour/Hour_4_10.coverage", "d192e2f2-4f05-4cfd-b203-19e50539ae19");
        int dom = this.day.getDayOfMonth();
        writeline("/home/ubuntu/results/coverage/Hour/Hour_4_10.coverage", "34c214f5-124a-40fe-a9e0-014c65fe263d");
        calendar.set(year, month, dom, this.hour, 59, 59);
        writeline("/home/ubuntu/results/coverage/Hour/Hour_4_10.coverage", "5631a6f4-4519-4332-953d-b7d84af7c5ba");
        calendar.set(Calendar.MILLISECOND, 999);
        writeline("/home/ubuntu/results/coverage/Hour/Hour_4_10.coverage", "78b7a81e-ffa2-4151-a40e-827bd226eacf");
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
        writeline("/home/ubuntu/results/coverage/Hour/Hour_4_10.coverage", "840834eb-b5e9-4fe0-a594-377129318a9a");
        if (obj == this) {
            writeline("/home/ubuntu/results/coverage/Hour/Hour_4_10.coverage", "5b0ac022-9efb-4564-b39a-5155dbb79a09");
            return true;
        }
        writeline("/home/ubuntu/results/coverage/Hour/Hour_4_10.coverage", "e1e85704-6948-42b6-8592-30a24834e74f");
        if (!(obj instanceof Hour)) {
            writeline("/home/ubuntu/results/coverage/Hour/Hour_4_10.coverage", "efa82a33-178e-4990-97f5-2ceff9d741c5");
            return false;
        }
        writeline("/home/ubuntu/results/coverage/Hour/Hour_4_10.coverage", "8cd599fe-78f3-4cb0-aa36-b8eedd51fcd9");
        Hour that = (Hour) obj;
        writeline("/home/ubuntu/results/coverage/Hour/Hour_4_10.coverage", "30cd3e7b-7910-4647-9fef-2d75ef69827a");
        if (this.hour != that.hour) {
            writeline("/home/ubuntu/results/coverage/Hour/Hour_4_10.coverage", "1eb56131-2f03-4c4e-8190-472abb1adc0b");
            return false;
        }
        writeline("/home/ubuntu/results/coverage/Hour/Hour_4_10.coverage", "01e567b5-982e-40b9-b4cf-d11d348ffba7");
        if (!this.day.equals(that.day)) {
            writeline("/home/ubuntu/results/coverage/Hour/Hour_4_10.coverage", "730ab1de-7e34-46ae-b0a7-f17c032d440b");
            return false;
        }
        writeline("/home/ubuntu/results/coverage/Hour/Hour_4_10.coverage", "2adec66a-58aa-42f5-a2f8-1c8240742735");
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
        writeline("/home/ubuntu/results/coverage/Hour/Hour_4_10.coverage", "a38dcd02-2433-4532-99e5-4995f97d0808");
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
        writeline("/home/ubuntu/results/coverage/Hour/Hour_4_10.coverage", "0796ee6d-93bb-4f91-adf1-6a7905db9319");
        int result = 17;
        writeline("/home/ubuntu/results/coverage/Hour/Hour_4_10.coverage", "dd40b630-4e6e-4231-b590-620e926e27ce");
        result = 37 * result + this.hour;
        writeline("/home/ubuntu/results/coverage/Hour/Hour_4_10.coverage", "723f79ca-10ef-4935-926b-55f2b5f518bb");
        result = 37 * result + this.day.hashCode();
        writeline("/home/ubuntu/results/coverage/Hour/Hour_4_10.coverage", "c06aabfc-8f5f-4fdc-b916-40c86ac17d36");
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
        writeline("/home/ubuntu/results/coverage/Hour/Hour_4_10.coverage", "37deeaf6-2dc3-4455-b95d-8d643789ac8d");
        int result;
        writeline("/home/ubuntu/results/coverage/Hour/Hour_4_10.coverage", "c74d26a2-5ad1-428f-8392-c322b7c3bf86");
        if (o1 instanceof Hour) {
            writeline("/home/ubuntu/results/coverage/Hour/Hour_4_10.coverage", "e067e079-de74-4ded-89d3-a1116556a887");
            Hour h = (Hour) o1;
            writeline("/home/ubuntu/results/coverage/Hour/Hour_4_10.coverage", "bc28c77c-6e16-47c4-b723-1ff902837285");
            result = getDay().compareTo(h.getDay());
            writeline("/home/ubuntu/results/coverage/Hour/Hour_4_10.coverage", "d8691ddb-401b-47f2-86c8-2f58a14fb3b7");
            if (result == 0) {
                writeline("/home/ubuntu/results/coverage/Hour/Hour_4_10.coverage", "e8ec67a5-3381-4b79-8573-bc0c47371f3d");
                result = this.hour - h.getHour();
            }
        } else if (o1 instanceof RegularTimePeriod) {
            writeline("/home/ubuntu/results/coverage/Hour/Hour_4_10.coverage", "000e0e64-24bf-4988-aaed-e71a61e16231");
            result = 0;
        } else {
            writeline("/home/ubuntu/results/coverage/Hour/Hour_4_10.coverage", "041f1d3e-8a0f-4cca-a688-b7f6351602f6");
            result = 1;
        }
        writeline("/home/ubuntu/results/coverage/Hour/Hour_4_10.coverage", "ea971151-047a-49e3-84ee-ae13ca4cdd54");
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
        writeline("/home/ubuntu/results/coverage/Hour/Hour_4_10.coverage", "97e47a0a-46bf-406a-aff9-5d6b37abb113");
        Hour result = null;
        writeline("/home/ubuntu/results/coverage/Hour/Hour_4_10.coverage", "181a05bd-ae31-4bd3-b145-0e7428b5daaa");
        s = s.trim();
        writeline("/home/ubuntu/results/coverage/Hour/Hour_4_10.coverage", "2bccb57d-55d5-4916-8393-a72286382d4a");
        String daystr = s.substring(0, Math.min(10, s.length()));
        writeline("/home/ubuntu/results/coverage/Hour/Hour_4_10.coverage", "4d38d708-cef7-4be2-9de5-09701c4f948f");
        Day day = Day.parseDay(daystr);
        writeline("/home/ubuntu/results/coverage/Hour/Hour_4_10.coverage", "0a08a4a1-f191-4b87-bafd-1cb05c37b4c2");
        if (day != null) {
            writeline("/home/ubuntu/results/coverage/Hour/Hour_4_10.coverage", "76e827aa-f200-41ef-bdc4-8fd144ad5cd6");
            String hourstr = s.substring(Math.min(daystr.length() + 1, s.length()), s.length());
            writeline("/home/ubuntu/results/coverage/Hour/Hour_4_10.coverage", "3d29b489-657e-40ef-9425-b23d33b376c8");
            hourstr = hourstr.trim();
            writeline("/home/ubuntu/results/coverage/Hour/Hour_4_10.coverage", "f86ce251-a255-4b92-b5e4-d02d418f9bfa");
            int hour = Integer.parseInt(hourstr);
            writeline("/home/ubuntu/results/coverage/Hour/Hour_4_10.coverage", "53060736-5535-4c96-96ee-e3c9724c2419");
            if ((hour >= FIRST_HOUR_IN_DAY) && (hour <= LAST_HOUR_IN_DAY)) {
                writeline("/home/ubuntu/results/coverage/Hour/Hour_4_10.coverage", "9c3ddf85-9c01-469e-8ef5-900a4c780074");
                result = new Hour(hour, day);
            }
        }
        writeline("/home/ubuntu/results/coverage/Hour/Hour_4_10.coverage", "708c08f6-615d-4534-ab43-7b3bec30c69c");
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
