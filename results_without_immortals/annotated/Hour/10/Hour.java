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
        writeline("/home/ubuntu/results/coverage/Hour/Hour_10_10.coverage", "7f27c69c-ad4f-44c3-b179-8bffa9977c73");
        return this.hour;
    }

    /**
     * Returns the day in which this hour falls.
     *
     * @return The day.
     */
    public Day getDay() {
        writeline("/home/ubuntu/results/coverage/Hour/Hour_10_10.coverage", "fc2f3f70-9506-4b0b-a403-fc58ab1f2ee3");
        return this.day;
    }

    /**
     * Returns the year in which this hour falls.
     *
     * @return The year.
     */
    public int getYear() {
        writeline("/home/ubuntu/results/coverage/Hour/Hour_10_10.coverage", "02d0a889-bc9a-4378-ab65-812571bcbf86");
        return this.day.getYear();
    }

    /**
     * Returns the month in which this hour falls.
     *
     * @return The month.
     */
    public int getMonth() {
        writeline("/home/ubuntu/results/coverage/Hour/Hour_10_10.coverage", "77343ba0-827a-4d13-b607-d724f62fc7e9");
        return this.day.getMonth();
    }

    /**
     * Returns the day-of-the-month in which this hour falls.
     *
     * @return The day-of-the-month.
     */
    public int getDayOfMonth() {
        writeline("/home/ubuntu/results/coverage/Hour/Hour_10_10.coverage", "d889dbcd-c7e2-4fe5-aefd-2d5c2554f4d6");
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
        writeline("/home/ubuntu/results/coverage/Hour/Hour_10_10.coverage", "976de9e7-1077-418c-ab81-9aef269e0cea");
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
        writeline("/home/ubuntu/results/coverage/Hour/Hour_10_10.coverage", "0a9981cb-d663-47af-9143-b229b78e439f");
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
        writeline("/home/ubuntu/results/coverage/Hour/Hour_10_10.coverage", "ec77e340-aacb-4c40-9496-bd9d6c4868b6");
        this.firstMillisecond = getFirstMillisecond(calendar);
        writeline("/home/ubuntu/results/coverage/Hour/Hour_10_10.coverage", "c3409399-b671-4e14-a7cb-527618a7efda");
        this.lastMillisecond = getLastMillisecond(calendar);
    }

    /**
     * Returns the hour preceding this one.
     *
     * @return The hour preceding this one.
     */
    @Override
    public RegularTimePeriod previous() {
        writeline("/home/ubuntu/results/coverage/Hour/Hour_10_10.coverage", "8c383d24-6615-4f2b-96aa-15d5fbaae3ee");
        Hour result;
        writeline("/home/ubuntu/results/coverage/Hour/Hour_10_10.coverage", "afb513be-c765-42d5-ae19-696bfdf5797c");
        if (this.hour != FIRST_HOUR_IN_DAY) {
            writeline("/home/ubuntu/results/coverage/Hour/Hour_10_10.coverage", "89459714-735d-4d02-a546-d0077c7bba79");
            result = new Hour(this.hour - 1, this.day);
        } else {
            writeline("/home/ubuntu/results/coverage/Hour/Hour_10_10.coverage", "860cb912-fb4d-4973-8408-2c1e9d9d8859");
            Day prevDay = (Day) this.day.previous();
            writeline("/home/ubuntu/results/coverage/Hour/Hour_10_10.coverage", "0016112e-0341-4551-ad56-7ecf47ff9820");
            if (prevDay != null) {
                writeline("/home/ubuntu/results/coverage/Hour/Hour_10_10.coverage", "29468dee-672e-4227-8508-88709528e2ee");
                result = new Hour(LAST_HOUR_IN_DAY, prevDay);
            } else {
                writeline("/home/ubuntu/results/coverage/Hour/Hour_10_10.coverage", "5938e42c-207b-4e2b-95a9-bdef1dca3c6a");
                result = null;
            }
        }
        writeline("/home/ubuntu/results/coverage/Hour/Hour_10_10.coverage", "03745870-01ca-448a-8178-39825e12ccf1");
        return result;
    }

    /**
     * Returns the hour following this one.
     *
     * @return The hour following this one.
     */
    @Override
    public RegularTimePeriod next() {
        writeline("/home/ubuntu/results/coverage/Hour/Hour_10_10.coverage", "228f0a10-47aa-4b71-b2a4-8b99dba012ac");
        Hour result;
        writeline("/home/ubuntu/results/coverage/Hour/Hour_10_10.coverage", "2c956d22-0cf1-4a0b-83af-ebf37a1b9ff4");
        if (this.hour != LAST_HOUR_IN_DAY) {
            writeline("/home/ubuntu/results/coverage/Hour/Hour_10_10.coverage", "666ababa-dd2c-4b34-aa68-d9e41e7f5387");
            result = new Hour(this.hour + 1, this.day);
        } else {
            writeline("/home/ubuntu/results/coverage/Hour/Hour_10_10.coverage", "261ab275-5951-45a3-acfe-dca46ed9f6b2");
            Day nextDay = (Day) this.day.next();
            writeline("/home/ubuntu/results/coverage/Hour/Hour_10_10.coverage", "facf7689-0915-492d-a6de-1ba8af83e968");
            if (nextDay != null) {
                writeline("/home/ubuntu/results/coverage/Hour/Hour_10_10.coverage", "1e57699b-2f0c-4bb8-a56f-59c70f15f382");
                result = new Hour(FIRST_HOUR_IN_DAY, nextDay);
            } else {
                writeline("/home/ubuntu/results/coverage/Hour/Hour_10_10.coverage", "cb0bc2ad-e7e9-444f-aad7-412552b7be49");
                result = null;
            }
        }
        writeline("/home/ubuntu/results/coverage/Hour/Hour_10_10.coverage", "3c00ddda-9266-4ab6-814b-09862e46d00d");
        return result;
    }

    /**
     * Returns a serial index number for the hour.
     *
     * @return The serial index number.
     */
    @Override
    public long getSerialIndex() {
        writeline("/home/ubuntu/results/coverage/Hour/Hour_10_10.coverage", "2771c3d5-9c5b-4116-8a7b-def02c2d8fcd");
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
        writeline("/home/ubuntu/results/coverage/Hour/Hour_10_10.coverage", "17342670-7c45-4f65-8510-376a531b4445");
        int year = this.day.getYear();
        writeline("/home/ubuntu/results/coverage/Hour/Hour_10_10.coverage", "56ed7eb6-67d6-41b1-82f7-23dac6612c01");
        int month = this.day.getMonth() - 1;
        writeline("/home/ubuntu/results/coverage/Hour/Hour_10_10.coverage", "5f07b5ee-2cfe-4bab-b241-ab4c1e6dcde1");
        int dom = this.day.getDayOfMonth();
        writeline("/home/ubuntu/results/coverage/Hour/Hour_10_10.coverage", "4f7143c8-fc07-461c-ba95-0297b7de827b");
        calendar.set(year, month, dom, this.hour, 0, 0);
        writeline("/home/ubuntu/results/coverage/Hour/Hour_10_10.coverage", "4d3e7e90-3581-4ef1-a84a-441f6b118cbe");
        calendar.set(Calendar.MILLISECOND, 0);
        writeline("/home/ubuntu/results/coverage/Hour/Hour_10_10.coverage", "63d40536-a961-4cf3-9293-e756f5418f93");
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
        writeline("/home/ubuntu/results/coverage/Hour/Hour_10_10.coverage", "52282893-9477-4731-9478-6d4d55b65691");
        int year = this.day.getYear();
        writeline("/home/ubuntu/results/coverage/Hour/Hour_10_10.coverage", "92eb9292-85cb-4bcd-9a27-1f0edb625fdc");
        int month = this.day.getMonth() - 1;
        writeline("/home/ubuntu/results/coverage/Hour/Hour_10_10.coverage", "746f495f-1d62-4d81-a43d-652fbaa6b4c3");
        int dom = this.day.getDayOfMonth();
        writeline("/home/ubuntu/results/coverage/Hour/Hour_10_10.coverage", "9fca065f-37f4-471d-8a0b-3f56383c14f4");
        calendar.set(year, month, dom, this.hour, 59, 59);
        writeline("/home/ubuntu/results/coverage/Hour/Hour_10_10.coverage", "857f8f23-dbf7-4b98-9a46-ea6cf7246c6d");
        calendar.set(Calendar.MILLISECOND, 999);
        writeline("/home/ubuntu/results/coverage/Hour/Hour_10_10.coverage", "8380c366-aaf9-456e-8946-2465bc4ef09f");
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
        writeline("/home/ubuntu/results/coverage/Hour/Hour_10_10.coverage", "9bbe39f4-0577-4ed5-93e3-5caf76a79dbc");
        if (obj == this) {
            writeline("/home/ubuntu/results/coverage/Hour/Hour_10_10.coverage", "d1345183-816b-4d56-90f8-da6621d87848");
            return true;
        }
        writeline("/home/ubuntu/results/coverage/Hour/Hour_10_10.coverage", "d837d900-34ee-4de9-b811-df445ed69b81");
        if (!(obj instanceof Hour)) {
            writeline("/home/ubuntu/results/coverage/Hour/Hour_10_10.coverage", "62d5a2d0-b688-4e56-9193-fee8e2797998");
            return false;
        }
        writeline("/home/ubuntu/results/coverage/Hour/Hour_10_10.coverage", "b7033321-830a-4aa2-bcc5-df9d06cc8c08");
        Hour that = (Hour) obj;
        writeline("/home/ubuntu/results/coverage/Hour/Hour_10_10.coverage", "98c69349-dd4e-4ee6-b1fc-8403ebf95ba0");
        if (this.hour != that.hour) {
            writeline("/home/ubuntu/results/coverage/Hour/Hour_10_10.coverage", "44101528-a201-42e9-aae4-50ba83985271");
            return false;
        }
        writeline("/home/ubuntu/results/coverage/Hour/Hour_10_10.coverage", "4754c6a1-b689-4f67-9cd8-93ac5c6dece7");
        if (!this.day.equals(that.day)) {
            writeline("/home/ubuntu/results/coverage/Hour/Hour_10_10.coverage", "7fc2c15f-a1d0-432a-9644-18bf07a06eed");
            return false;
        }
        writeline("/home/ubuntu/results/coverage/Hour/Hour_10_10.coverage", "f2a8694a-f990-41fb-9398-d4e17e98c4be");
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
        writeline("/home/ubuntu/results/coverage/Hour/Hour_10_10.coverage", "4692155e-9638-4690-be4d-a96fcb8c74fb");
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
        writeline("/home/ubuntu/results/coverage/Hour/Hour_10_10.coverage", "5eda5663-7286-4b57-b421-9f3a31b3fa32");
        int result = 17;
        writeline("/home/ubuntu/results/coverage/Hour/Hour_10_10.coverage", "3dce0928-1d19-439b-876c-face3e067f73");
        result = 37 * result + this.hour;
        writeline("/home/ubuntu/results/coverage/Hour/Hour_10_10.coverage", "2c130d67-ffdf-47b6-9617-b880023d5a44");
        result = 37 * result + this.day.hashCode();
        writeline("/home/ubuntu/results/coverage/Hour/Hour_10_10.coverage", "2bd6b991-a6a2-4dff-b838-26cd2ec09222");
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
        writeline("/home/ubuntu/results/coverage/Hour/Hour_10_10.coverage", "1012cc97-13ca-4c8f-baa9-397c7ae17ffb");
        int result;
        writeline("/home/ubuntu/results/coverage/Hour/Hour_10_10.coverage", "5cfdfae7-a06a-45ef-a2ae-09e365c9d235");
        if (o1 instanceof Hour) {
            writeline("/home/ubuntu/results/coverage/Hour/Hour_10_10.coverage", "c00ce3a0-0ab2-4a2e-8eeb-b364a9161f84");
            Hour h = (Hour) o1;
            writeline("/home/ubuntu/results/coverage/Hour/Hour_10_10.coverage", "fc4f6bed-2687-4bc2-a587-91687245d226");
            result = getDay().compareTo(h.getDay());
            writeline("/home/ubuntu/results/coverage/Hour/Hour_10_10.coverage", "eaa15104-a861-4a9e-95fe-eb5f85ef0e9b");
            if (result == 0) {
                writeline("/home/ubuntu/results/coverage/Hour/Hour_10_10.coverage", "55d5f679-8eb8-4d5f-98e0-0be8d012243d");
                result = this.hour - h.getHour();
            }
        } else if (o1 instanceof RegularTimePeriod) {
            writeline("/home/ubuntu/results/coverage/Hour/Hour_10_10.coverage", "34b436b3-5028-43f5-a2ed-757e53d659e9");
            result = 0;
        } else {
            writeline("/home/ubuntu/results/coverage/Hour/Hour_10_10.coverage", "bf455aab-b1fe-4d92-aca4-02444712292b");
            result = 1;
        }
        writeline("/home/ubuntu/results/coverage/Hour/Hour_10_10.coverage", "883350b0-44fc-45a1-9914-6736b085532d");
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
        writeline("/home/ubuntu/results/coverage/Hour/Hour_10_10.coverage", "443f7148-2d8f-434e-a169-4a6d521afde5");
        Hour result = null;
        writeline("/home/ubuntu/results/coverage/Hour/Hour_10_10.coverage", "0b88a5bf-dc36-43f2-81f3-7b5a3bd20205");
        s = s.trim();
        writeline("/home/ubuntu/results/coverage/Hour/Hour_10_10.coverage", "c135620a-3a96-4d41-99e7-fe60cd2c59f4");
        String daystr = s.substring(0, Math.min(10, s.length()));
        writeline("/home/ubuntu/results/coverage/Hour/Hour_10_10.coverage", "7d8e021b-e5a9-4557-b6a9-2cc045340d91");
        Day day = Day.parseDay(daystr);
        writeline("/home/ubuntu/results/coverage/Hour/Hour_10_10.coverage", "733e612d-e214-42ca-a259-5a5def578a74");
        if (day != null) {
            writeline("/home/ubuntu/results/coverage/Hour/Hour_10_10.coverage", "1bc39b2d-d0fa-4ff1-aa8d-d003bfec6a91");
            String hourstr = s.substring(Math.min(daystr.length() + 1, s.length()), s.length());
            writeline("/home/ubuntu/results/coverage/Hour/Hour_10_10.coverage", "2f9b9cad-6fba-4779-9597-857dff18018c");
            hourstr = hourstr.trim();
            writeline("/home/ubuntu/results/coverage/Hour/Hour_10_10.coverage", "6576db56-d801-429b-bc63-7830c92bf06b");
            int hour = Integer.parseInt(hourstr);
            writeline("/home/ubuntu/results/coverage/Hour/Hour_10_10.coverage", "c73cead7-a485-4d14-a692-2adbab313da7");
            if ((hour >= FIRST_HOUR_IN_DAY) && (hour <= LAST_HOUR_IN_DAY)) {
                writeline("/home/ubuntu/results/coverage/Hour/Hour_10_10.coverage", "174e4c75-5d56-40d8-b53d-b91904f9a6ce");
                result = new Hour(hour, day);
            }
        }
        writeline("/home/ubuntu/results/coverage/Hour/Hour_10_10.coverage", "1eac188f-8179-47bc-bdd9-3a1d3424321d");
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
