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
        writeline("/home/ubuntu/results/coverage/Hour/Hour_5_10.coverage", "34f569b8-d1bd-454a-9033-331e3f90afb6");
        return this.hour;
    }

    /**
     * Returns the day in which this hour falls.
     *
     * @return The day.
     */
    public Day getDay() {
        writeline("/home/ubuntu/results/coverage/Hour/Hour_5_10.coverage", "b2193a82-73cb-417f-8ed2-f02436d7a7e4");
        return this.day;
    }

    /**
     * Returns the year in which this hour falls.
     *
     * @return The year.
     */
    public int getYear() {
        writeline("/home/ubuntu/results/coverage/Hour/Hour_5_10.coverage", "53d1455a-2119-4365-8b04-91b6c047bf20");
        return this.day.getYear();
    }

    /**
     * Returns the month in which this hour falls.
     *
     * @return The month.
     */
    public int getMonth() {
        writeline("/home/ubuntu/results/coverage/Hour/Hour_5_10.coverage", "86fb3acc-e21c-4f1e-8390-d3ae7028abdb");
        return this.day.getMonth();
    }

    /**
     * Returns the day-of-the-month in which this hour falls.
     *
     * @return The day-of-the-month.
     */
    public int getDayOfMonth() {
        writeline("/home/ubuntu/results/coverage/Hour/Hour_5_10.coverage", "c0499c34-9e50-442e-b408-349328e9032c");
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
        writeline("/home/ubuntu/results/coverage/Hour/Hour_5_10.coverage", "f8d6b442-f07b-4312-95d0-b1e1d3d9f624");
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
        writeline("/home/ubuntu/results/coverage/Hour/Hour_5_10.coverage", "ca90be5a-d9dc-461e-a32b-8a85a06d91c6");
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
        writeline("/home/ubuntu/results/coverage/Hour/Hour_5_10.coverage", "4dbfe4df-1293-4fb1-8b2a-5c3f99e2ebcc");
        this.firstMillisecond = getFirstMillisecond(calendar);
        writeline("/home/ubuntu/results/coverage/Hour/Hour_5_10.coverage", "6c7380dd-b45d-4323-9e63-4d68e88b445f");
        this.lastMillisecond = getLastMillisecond(calendar);
    }

    /**
     * Returns the hour preceding this one.
     *
     * @return The hour preceding this one.
     */
    @Override
    public RegularTimePeriod previous() {
        writeline("/home/ubuntu/results/coverage/Hour/Hour_5_10.coverage", "49cbe37d-0361-41f3-b045-b8aaaf144bcd");
        Hour result;
        writeline("/home/ubuntu/results/coverage/Hour/Hour_5_10.coverage", "e153f459-ced7-4ec7-a8bd-a74ad3eb2133");
        if (this.hour != FIRST_HOUR_IN_DAY) {
            writeline("/home/ubuntu/results/coverage/Hour/Hour_5_10.coverage", "2f52d722-72fb-4a32-9e2f-f513fdf1e4a3");
            result = new Hour(this.hour - 1, this.day);
        } else {
            writeline("/home/ubuntu/results/coverage/Hour/Hour_5_10.coverage", "7bf89c31-dea4-4d44-8a6a-1a4b1d1e59b5");
            Day prevDay = (Day) this.day.previous();
            writeline("/home/ubuntu/results/coverage/Hour/Hour_5_10.coverage", "378da843-682c-4449-a541-43dc98d02acf");
            if (prevDay != null) {
                writeline("/home/ubuntu/results/coverage/Hour/Hour_5_10.coverage", "8947a226-4a8e-45dd-ace6-7793863d9253");
                result = new Hour(LAST_HOUR_IN_DAY, prevDay);
            } else {
                writeline("/home/ubuntu/results/coverage/Hour/Hour_5_10.coverage", "29e667e3-6683-45b3-9f79-1a2943a5b2b2");
                result = null;
            }
        }
        writeline("/home/ubuntu/results/coverage/Hour/Hour_5_10.coverage", "316eacc9-7220-435c-9c3d-d27def704d36");
        return result;
    }

    /**
     * Returns the hour following this one.
     *
     * @return The hour following this one.
     */
    @Override
    public RegularTimePeriod next() {
        writeline("/home/ubuntu/results/coverage/Hour/Hour_5_10.coverage", "aff419c2-6131-497f-b2a3-2a602d9eb9c6");
        Hour result;
        writeline("/home/ubuntu/results/coverage/Hour/Hour_5_10.coverage", "e0d7330e-10ae-4622-a1b8-be9723165c56");
        if (this.hour != LAST_HOUR_IN_DAY) {
            writeline("/home/ubuntu/results/coverage/Hour/Hour_5_10.coverage", "ef6a34f0-9fb1-42bb-8687-39325959526d");
            result = new Hour(this.hour + 1, this.day);
        } else {
            writeline("/home/ubuntu/results/coverage/Hour/Hour_5_10.coverage", "87c62eb8-8a8d-4f2b-b592-773938cb6cb1");
            Day nextDay = (Day) this.day.next();
            writeline("/home/ubuntu/results/coverage/Hour/Hour_5_10.coverage", "c46feb2c-1c7c-4d08-96f3-575c59cda74c");
            if (nextDay != null) {
                writeline("/home/ubuntu/results/coverage/Hour/Hour_5_10.coverage", "12cb50ab-b3da-4912-b75b-43a7a28ecdda");
                result = new Hour(FIRST_HOUR_IN_DAY, nextDay);
            } else {
                writeline("/home/ubuntu/results/coverage/Hour/Hour_5_10.coverage", "80001832-7730-4ad1-b967-90d504fb621e");
                result = null;
            }
        }
        writeline("/home/ubuntu/results/coverage/Hour/Hour_5_10.coverage", "6c17cd46-3b10-428c-9e53-f89cca847660");
        return result;
    }

    /**
     * Returns a serial index number for the hour.
     *
     * @return The serial index number.
     */
    @Override
    public long getSerialIndex() {
        writeline("/home/ubuntu/results/coverage/Hour/Hour_5_10.coverage", "01ab7d46-2c9f-4d8a-89ba-6596f7a926d6");
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
        writeline("/home/ubuntu/results/coverage/Hour/Hour_5_10.coverage", "5cfd5057-69dc-4e8f-91f8-ff7aeb99cdeb");
        int year = this.day.getYear();
        writeline("/home/ubuntu/results/coverage/Hour/Hour_5_10.coverage", "d46bc326-7b40-40cb-ae00-273c48c1a617");
        int month = this.day.getMonth() - 1;
        writeline("/home/ubuntu/results/coverage/Hour/Hour_5_10.coverage", "54dda897-b202-450a-a32b-7d121c7f9bdf");
        int dom = this.day.getDayOfMonth();
        writeline("/home/ubuntu/results/coverage/Hour/Hour_5_10.coverage", "b7dadcc8-436c-4dae-9693-97aca88c84f0");
        calendar.set(year, month, dom, this.hour, 0, 0);
        writeline("/home/ubuntu/results/coverage/Hour/Hour_5_10.coverage", "a69ecdff-3340-4f01-9418-3a4ff1dd1194");
        calendar.set(Calendar.MILLISECOND, 0);
        writeline("/home/ubuntu/results/coverage/Hour/Hour_5_10.coverage", "4bdba619-b750-4ada-b9ab-f4ac4846a976");
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
        writeline("/home/ubuntu/results/coverage/Hour/Hour_5_10.coverage", "fc5571cf-692b-45bd-9cf9-caef5bba570e");
        int year = this.day.getYear();
        writeline("/home/ubuntu/results/coverage/Hour/Hour_5_10.coverage", "6ed66126-2959-4fd8-9f26-844ca7035b65");
        int month = this.day.getMonth() - 1;
        writeline("/home/ubuntu/results/coverage/Hour/Hour_5_10.coverage", "c1a457b3-142b-45df-a7a9-fd7d4384ae3d");
        int dom = this.day.getDayOfMonth();
        writeline("/home/ubuntu/results/coverage/Hour/Hour_5_10.coverage", "6cd534bc-f838-431c-9232-77767861613b");
        calendar.set(year, month, dom, this.hour, 59, 59);
        writeline("/home/ubuntu/results/coverage/Hour/Hour_5_10.coverage", "55d895f4-e38b-46a7-a485-bd1b7de64db5");
        calendar.set(Calendar.MILLISECOND, 999);
        writeline("/home/ubuntu/results/coverage/Hour/Hour_5_10.coverage", "8dcf9508-2acd-4737-bd33-64e6fd370788");
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
        writeline("/home/ubuntu/results/coverage/Hour/Hour_5_10.coverage", "ec7f32c1-5613-4d81-a5e6-2f28e2c0681e");
        if (obj == this) {
            writeline("/home/ubuntu/results/coverage/Hour/Hour_5_10.coverage", "c889f61a-c74d-40d8-ba89-8be01192ff95");
            return true;
        }
        writeline("/home/ubuntu/results/coverage/Hour/Hour_5_10.coverage", "158d2a45-e851-4de2-9ea9-e0b716ae9213");
        if (!(obj instanceof Hour)) {
            writeline("/home/ubuntu/results/coverage/Hour/Hour_5_10.coverage", "22c31131-ec48-4f3c-b63e-7b6f48c1319a");
            return false;
        }
        writeline("/home/ubuntu/results/coverage/Hour/Hour_5_10.coverage", "a0242c67-f36f-4d84-b7c2-89d5facc12fb");
        Hour that = (Hour) obj;
        writeline("/home/ubuntu/results/coverage/Hour/Hour_5_10.coverage", "0e907336-2439-4153-9714-d280efe77a78");
        if (this.hour != that.hour) {
            writeline("/home/ubuntu/results/coverage/Hour/Hour_5_10.coverage", "792baff9-a935-4ab8-a0c6-c17af0afd8b9");
            return false;
        }
        writeline("/home/ubuntu/results/coverage/Hour/Hour_5_10.coverage", "1b03ed2b-3cda-4164-8a66-9dae47a58ea5");
        if (!this.day.equals(that.day)) {
            writeline("/home/ubuntu/results/coverage/Hour/Hour_5_10.coverage", "47f55baf-4911-47bd-a64f-fdbae1cb6145");
            return false;
        }
        writeline("/home/ubuntu/results/coverage/Hour/Hour_5_10.coverage", "6ac24bb8-c7c9-4751-9c2d-2119c1ae1bd6");
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
        writeline("/home/ubuntu/results/coverage/Hour/Hour_5_10.coverage", "f375f883-b277-4a58-83d7-76a071a77132");
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
        writeline("/home/ubuntu/results/coverage/Hour/Hour_5_10.coverage", "5509296c-5870-4169-ab17-bd73d8586e90");
        int result = 17;
        writeline("/home/ubuntu/results/coverage/Hour/Hour_5_10.coverage", "ba25ac71-4bb8-4579-a654-174b5ed2fe4d");
        result = 37 * result + this.hour;
        writeline("/home/ubuntu/results/coverage/Hour/Hour_5_10.coverage", "8677e67c-b00d-4a02-b784-1a9109a0654c");
        result = 37 * result + this.day.hashCode();
        writeline("/home/ubuntu/results/coverage/Hour/Hour_5_10.coverage", "b82a5be9-c29b-46ad-aa9b-69054b016027");
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
        writeline("/home/ubuntu/results/coverage/Hour/Hour_5_10.coverage", "59514abc-0076-4289-8630-9e5291c6ad1d");
        int result;
        writeline("/home/ubuntu/results/coverage/Hour/Hour_5_10.coverage", "47348641-c0e0-486d-b5d0-233437b89e9f");
        if (o1 instanceof Hour) {
            writeline("/home/ubuntu/results/coverage/Hour/Hour_5_10.coverage", "bf0a3fc6-2144-42d4-b509-9183a454f2b6");
            Hour h = (Hour) o1;
            writeline("/home/ubuntu/results/coverage/Hour/Hour_5_10.coverage", "ab47cc2c-2406-436f-9770-9392c04e5a1c");
            result = getDay().compareTo(h.getDay());
            writeline("/home/ubuntu/results/coverage/Hour/Hour_5_10.coverage", "ceafaec9-54ee-435b-b9a8-ac05ddfc70d5");
            if (result == 0) {
                writeline("/home/ubuntu/results/coverage/Hour/Hour_5_10.coverage", "0ce0b360-ed8b-446d-a2de-bcfb2241b146");
                result = this.hour - h.getHour();
            }
        } else if (o1 instanceof RegularTimePeriod) {
            writeline("/home/ubuntu/results/coverage/Hour/Hour_5_10.coverage", "29f013a6-a2a6-46cd-932b-132ab7874ab0");
            result = 0;
        } else {
            writeline("/home/ubuntu/results/coverage/Hour/Hour_5_10.coverage", "ecf501a5-dc75-4ff5-a150-1bfa92b0d0ef");
            result = 1;
        }
        writeline("/home/ubuntu/results/coverage/Hour/Hour_5_10.coverage", "3eacfb07-9500-40a6-b5ac-316a3ffe3e50");
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
        writeline("/home/ubuntu/results/coverage/Hour/Hour_5_10.coverage", "6c8e0cb3-4da1-4200-8104-fddd788e7305");
        Hour result = null;
        writeline("/home/ubuntu/results/coverage/Hour/Hour_5_10.coverage", "90a41236-4cb5-4974-8fd2-2e0e7077c1c0");
        s = s.trim();
        writeline("/home/ubuntu/results/coverage/Hour/Hour_5_10.coverage", "354e787e-9485-445a-8225-67e3d83ea6b3");
        String daystr = s.substring(0, Math.min(10, s.length()));
        writeline("/home/ubuntu/results/coverage/Hour/Hour_5_10.coverage", "2bc64da8-2853-412b-9795-6653975b616c");
        Day day = Day.parseDay(daystr);
        writeline("/home/ubuntu/results/coverage/Hour/Hour_5_10.coverage", "ef60461d-6047-4b2c-8dfb-83ed1cab8af2");
        if (day != null) {
            writeline("/home/ubuntu/results/coverage/Hour/Hour_5_10.coverage", "035b2412-da1a-429e-ae5d-478f42c0cd7b");
            String hourstr = s.substring(Math.min(daystr.length() + 1, s.length()), s.length());
            writeline("/home/ubuntu/results/coverage/Hour/Hour_5_10.coverage", "52cb62b4-91fc-4824-81b7-ca1b76f7d3c6");
            hourstr = hourstr.trim();
            writeline("/home/ubuntu/results/coverage/Hour/Hour_5_10.coverage", "fe5510b1-587e-4fa7-b77b-46ca2a2e1980");
            int hour = Integer.parseInt(hourstr);
            writeline("/home/ubuntu/results/coverage/Hour/Hour_5_10.coverage", "f3d1328c-d57c-4283-bced-b5e6a07fe771");
            if ((hour >= FIRST_HOUR_IN_DAY) && (hour <= LAST_HOUR_IN_DAY)) {
                writeline("/home/ubuntu/results/coverage/Hour/Hour_5_10.coverage", "11eddf32-4cae-4a0b-867e-020b67694b4a");
                result = new Hour(hour, day);
            }
        }
        writeline("/home/ubuntu/results/coverage/Hour/Hour_5_10.coverage", "2bc4039a-f8f3-49b5-af12-55be982b4693");
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
