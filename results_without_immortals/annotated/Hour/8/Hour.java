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
        writeline("/home/ubuntu/results/coverage/Hour/Hour_8_10.coverage", "b32a1fa7-6f9c-472d-a46b-d98591477115");
        return this.hour;
    }

    /**
     * Returns the day in which this hour falls.
     *
     * @return The day.
     */
    public Day getDay() {
        writeline("/home/ubuntu/results/coverage/Hour/Hour_8_10.coverage", "de9f1c64-6659-4780-b841-97e4a69f68e8");
        return this.day;
    }

    /**
     * Returns the year in which this hour falls.
     *
     * @return The year.
     */
    public int getYear() {
        writeline("/home/ubuntu/results/coverage/Hour/Hour_8_10.coverage", "d2f00045-70f4-43ed-b720-acfec8dd8a19");
        return this.day.getYear();
    }

    /**
     * Returns the month in which this hour falls.
     *
     * @return The month.
     */
    public int getMonth() {
        writeline("/home/ubuntu/results/coverage/Hour/Hour_8_10.coverage", "d9e23374-3228-42c9-84ee-43c391e5e472");
        return this.day.getMonth();
    }

    /**
     * Returns the day-of-the-month in which this hour falls.
     *
     * @return The day-of-the-month.
     */
    public int getDayOfMonth() {
        writeline("/home/ubuntu/results/coverage/Hour/Hour_8_10.coverage", "d25c011f-08af-4751-921f-36094f15ee34");
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
        writeline("/home/ubuntu/results/coverage/Hour/Hour_8_10.coverage", "a9563ec9-a1da-40ce-99e4-3d4031f0cbab");
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
        writeline("/home/ubuntu/results/coverage/Hour/Hour_8_10.coverage", "f67b1f18-32bb-4aeb-9f39-82cb1fe371c6");
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
        writeline("/home/ubuntu/results/coverage/Hour/Hour_8_10.coverage", "ad9435f3-6096-4b62-9f60-8b054959d9af");
        this.firstMillisecond = getFirstMillisecond(calendar);
        writeline("/home/ubuntu/results/coverage/Hour/Hour_8_10.coverage", "9336ede9-1e27-4171-9a94-6722359927cc");
        this.lastMillisecond = getLastMillisecond(calendar);
    }

    /**
     * Returns the hour preceding this one.
     *
     * @return The hour preceding this one.
     */
    @Override
    public RegularTimePeriod previous() {
        writeline("/home/ubuntu/results/coverage/Hour/Hour_8_10.coverage", "254546ad-a80d-4207-8213-5c61148ac1f0");
        Hour result;
        writeline("/home/ubuntu/results/coverage/Hour/Hour_8_10.coverage", "25a46a86-e073-4abb-b1a7-1987d3d90f56");
        if (this.hour != FIRST_HOUR_IN_DAY) {
            writeline("/home/ubuntu/results/coverage/Hour/Hour_8_10.coverage", "7775df3c-a072-46cc-a541-b74a8a907507");
            result = new Hour(this.hour - 1, this.day);
        } else {
            writeline("/home/ubuntu/results/coverage/Hour/Hour_8_10.coverage", "d7b6469b-18fc-4467-8739-6bc7d9e439e5");
            Day prevDay = (Day) this.day.previous();
            writeline("/home/ubuntu/results/coverage/Hour/Hour_8_10.coverage", "9c595f30-84b3-404a-baa0-8aaefcc2d330");
            if (prevDay != null) {
                writeline("/home/ubuntu/results/coverage/Hour/Hour_8_10.coverage", "0d39356c-72ef-4f04-ad60-0ac96651d2df");
                result = new Hour(LAST_HOUR_IN_DAY, prevDay);
            } else {
                writeline("/home/ubuntu/results/coverage/Hour/Hour_8_10.coverage", "3af1e6ac-5a89-4097-8e2e-a76101426d76");
                result = null;
            }
        }
        writeline("/home/ubuntu/results/coverage/Hour/Hour_8_10.coverage", "c455d382-2f5b-4859-8dfc-46d51a9b0baf");
        return result;
    }

    /**
     * Returns the hour following this one.
     *
     * @return The hour following this one.
     */
    @Override
    public RegularTimePeriod next() {
        writeline("/home/ubuntu/results/coverage/Hour/Hour_8_10.coverage", "28a0ed47-f7e7-45c6-931e-3b22aad06d39");
        Hour result;
        writeline("/home/ubuntu/results/coverage/Hour/Hour_8_10.coverage", "1c8ccfe6-0c41-4353-af91-cd17cce5a9ce");
        if (this.hour != LAST_HOUR_IN_DAY) {
            writeline("/home/ubuntu/results/coverage/Hour/Hour_8_10.coverage", "380607b6-fc36-49d2-949e-93d9e92ceb11");
            result = new Hour(this.hour + 1, this.day);
        } else {
            writeline("/home/ubuntu/results/coverage/Hour/Hour_8_10.coverage", "7edff196-928e-43f6-acb7-ada57e94ea24");
            Day nextDay = (Day) this.day.next();
            writeline("/home/ubuntu/results/coverage/Hour/Hour_8_10.coverage", "8d977ad3-fe24-4e30-9b9e-0e2345c7f98e");
            if (nextDay != null) {
                writeline("/home/ubuntu/results/coverage/Hour/Hour_8_10.coverage", "783d352a-e70e-47ff-8f26-71475254b11f");
                result = new Hour(FIRST_HOUR_IN_DAY, nextDay);
            } else {
                writeline("/home/ubuntu/results/coverage/Hour/Hour_8_10.coverage", "34c5958a-fe71-4a98-9b83-a30b15d98b2d");
                result = null;
            }
        }
        writeline("/home/ubuntu/results/coverage/Hour/Hour_8_10.coverage", "01fd5595-3dbc-497c-a630-f0433e96b08c");
        return result;
    }

    /**
     * Returns a serial index number for the hour.
     *
     * @return The serial index number.
     */
    @Override
    public long getSerialIndex() {
        writeline("/home/ubuntu/results/coverage/Hour/Hour_8_10.coverage", "66678296-cbd3-4799-afd6-13311ee4a71c");
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
        writeline("/home/ubuntu/results/coverage/Hour/Hour_8_10.coverage", "464fa4b3-0d26-4e6e-9f5e-2d0c9217f5d2");
        int year = this.day.getYear();
        writeline("/home/ubuntu/results/coverage/Hour/Hour_8_10.coverage", "ad24afb6-68e1-4dc3-86fd-9f447cc1fdd9");
        int month = this.day.getMonth() - 1;
        writeline("/home/ubuntu/results/coverage/Hour/Hour_8_10.coverage", "bbf83e0b-97b0-4bbd-951d-2d768387a43a");
        int dom = this.day.getDayOfMonth();
        writeline("/home/ubuntu/results/coverage/Hour/Hour_8_10.coverage", "002928cc-27bb-4c98-971a-cba1b042f6f5");
        calendar.set(year, month, dom, this.hour, 0, 0);
        writeline("/home/ubuntu/results/coverage/Hour/Hour_8_10.coverage", "2c6f4d37-6e71-4283-a529-20d351e578c7");
        calendar.set(Calendar.MILLISECOND, 0);
        writeline("/home/ubuntu/results/coverage/Hour/Hour_8_10.coverage", "e234b9a6-f454-4962-80d2-20f5d14dd3f5");
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
        writeline("/home/ubuntu/results/coverage/Hour/Hour_8_10.coverage", "ca86cf17-0bc0-4cfe-a533-48ebae3c926e");
        int year = this.day.getYear();
        writeline("/home/ubuntu/results/coverage/Hour/Hour_8_10.coverage", "bc0f8f5b-97f1-4399-b809-42b9a063038d");
        int month = this.day.getMonth() - 1;
        writeline("/home/ubuntu/results/coverage/Hour/Hour_8_10.coverage", "ed46283c-e4ad-43fa-988d-813f259f7e6a");
        int dom = this.day.getDayOfMonth();
        writeline("/home/ubuntu/results/coverage/Hour/Hour_8_10.coverage", "b003b884-6a38-4d8d-ace0-e9c3eb27a641");
        calendar.set(year, month, dom, this.hour, 59, 59);
        writeline("/home/ubuntu/results/coverage/Hour/Hour_8_10.coverage", "60f25f15-0702-4cb4-bdd7-af45485450bf");
        calendar.set(Calendar.MILLISECOND, 999);
        writeline("/home/ubuntu/results/coverage/Hour/Hour_8_10.coverage", "97817378-113e-4747-b155-08cc415b2dd0");
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
        writeline("/home/ubuntu/results/coverage/Hour/Hour_8_10.coverage", "856464f8-ca60-4d7f-8d5c-43dc156497ab");
        if (obj == this) {
            writeline("/home/ubuntu/results/coverage/Hour/Hour_8_10.coverage", "f171e18c-b341-48d8-85f1-ab228d88b372");
            return true;
        }
        writeline("/home/ubuntu/results/coverage/Hour/Hour_8_10.coverage", "05741468-efd8-4c3e-9876-068e5e17ee3d");
        if (!(obj instanceof Hour)) {
            writeline("/home/ubuntu/results/coverage/Hour/Hour_8_10.coverage", "87d5c5db-9b62-408d-adae-3d60ebfe8ebd");
            return false;
        }
        writeline("/home/ubuntu/results/coverage/Hour/Hour_8_10.coverage", "67c0cae7-e3a5-4746-9218-71babadc1dfa");
        Hour that = (Hour) obj;
        writeline("/home/ubuntu/results/coverage/Hour/Hour_8_10.coverage", "4246d411-ec32-4d1a-8969-0cedef859d67");
        if (this.hour != that.hour) {
            writeline("/home/ubuntu/results/coverage/Hour/Hour_8_10.coverage", "70e3053e-52be-4499-8a6a-62d4a7f3508d");
            return false;
        }
        writeline("/home/ubuntu/results/coverage/Hour/Hour_8_10.coverage", "09564c62-3e8a-4cd7-b244-fef74c5ada53");
        if (!this.day.equals(that.day)) {
            writeline("/home/ubuntu/results/coverage/Hour/Hour_8_10.coverage", "c680165c-ca94-48e5-90c2-6dd086230aa9");
            return false;
        }
        writeline("/home/ubuntu/results/coverage/Hour/Hour_8_10.coverage", "8cca2274-5681-40f1-9233-5b2a00db107e");
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
        writeline("/home/ubuntu/results/coverage/Hour/Hour_8_10.coverage", "c09479d4-0c11-417e-8781-1da8ccd48862");
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
        writeline("/home/ubuntu/results/coverage/Hour/Hour_8_10.coverage", "0e14cdb0-e618-43ae-ba95-f52e2022ab23");
        int result = 17;
        writeline("/home/ubuntu/results/coverage/Hour/Hour_8_10.coverage", "6a885881-e738-4c87-a241-a62c193e5922");
        result = 37 * result + this.hour;
        writeline("/home/ubuntu/results/coverage/Hour/Hour_8_10.coverage", "65cce1b8-0cc9-4516-8dd1-e89ffda2c8c8");
        result = 37 * result + this.day.hashCode();
        writeline("/home/ubuntu/results/coverage/Hour/Hour_8_10.coverage", "e319f93c-33a9-401c-babb-a8f5ceac759e");
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
        writeline("/home/ubuntu/results/coverage/Hour/Hour_8_10.coverage", "56df2cfd-ed28-4808-9738-88305472a1ce");
        int result;
        writeline("/home/ubuntu/results/coverage/Hour/Hour_8_10.coverage", "47296496-ddc3-4177-8dfd-78d53fead508");
        if (o1 instanceof Hour) {
            writeline("/home/ubuntu/results/coverage/Hour/Hour_8_10.coverage", "457ddde0-e584-46b2-9646-abdac18ba4b5");
            Hour h = (Hour) o1;
            writeline("/home/ubuntu/results/coverage/Hour/Hour_8_10.coverage", "d6680174-5bae-47cf-ae64-137b463303cb");
            result = getDay().compareTo(h.getDay());
            writeline("/home/ubuntu/results/coverage/Hour/Hour_8_10.coverage", "70e3779b-85d4-4bd4-a8a9-0c7b0416c183");
            if (result == 0) {
                writeline("/home/ubuntu/results/coverage/Hour/Hour_8_10.coverage", "06cd56d9-d4a2-416e-bc57-2b5b5f1b2120");
                result = this.hour - h.getHour();
            }
        } else if (o1 instanceof RegularTimePeriod) {
            writeline("/home/ubuntu/results/coverage/Hour/Hour_8_10.coverage", "fa99fd3e-243b-47ce-ba61-4f16a122c41c");
            result = 0;
        } else {
            writeline("/home/ubuntu/results/coverage/Hour/Hour_8_10.coverage", "b6b5561e-49b8-47d4-b83d-b052825d109f");
            result = 1;
        }
        writeline("/home/ubuntu/results/coverage/Hour/Hour_8_10.coverage", "3d05af28-0000-4236-9e6e-3a51652de483");
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
        writeline("/home/ubuntu/results/coverage/Hour/Hour_8_10.coverage", "273b6313-46be-4d3f-810c-943279041ab5");
        Hour result = null;
        writeline("/home/ubuntu/results/coverage/Hour/Hour_8_10.coverage", "e520c270-3b28-4372-abef-d0c87ace698e");
        s = s.trim();
        writeline("/home/ubuntu/results/coverage/Hour/Hour_8_10.coverage", "82c7487c-9e35-47d3-b44e-16824cb9f45c");
        String daystr = s.substring(0, Math.min(10, s.length()));
        writeline("/home/ubuntu/results/coverage/Hour/Hour_8_10.coverage", "250a7a8c-a657-4e4b-beb5-8cb019b4db1a");
        Day day = Day.parseDay(daystr);
        writeline("/home/ubuntu/results/coverage/Hour/Hour_8_10.coverage", "a8e381fd-67cc-4b33-8c6c-80c77416223a");
        if (day != null) {
            writeline("/home/ubuntu/results/coverage/Hour/Hour_8_10.coverage", "0e99be1c-d286-49a8-9612-7c68ac3c9c7c");
            String hourstr = s.substring(Math.min(daystr.length() + 1, s.length()), s.length());
            writeline("/home/ubuntu/results/coverage/Hour/Hour_8_10.coverage", "2922b4ad-2209-4611-b230-9861c04b3722");
            hourstr = hourstr.trim();
            writeline("/home/ubuntu/results/coverage/Hour/Hour_8_10.coverage", "d9169224-6440-4b64-8eef-79a59c80ba0a");
            int hour = Integer.parseInt(hourstr);
            writeline("/home/ubuntu/results/coverage/Hour/Hour_8_10.coverage", "aa88b657-a2b3-403b-95e0-68d9967328a8");
            if ((hour >= FIRST_HOUR_IN_DAY) && (hour <= LAST_HOUR_IN_DAY)) {
                writeline("/home/ubuntu/results/coverage/Hour/Hour_8_10.coverage", "73afe100-9162-4240-927b-ae6d15da2cc2");
                result = new Hour(hour, day);
            }
        }
        writeline("/home/ubuntu/results/coverage/Hour/Hour_8_10.coverage", "aad634f3-a532-43f4-85d8-cb04b373c2d4");
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
