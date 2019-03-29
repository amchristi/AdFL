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
        writeline("/home/ubuntu/results/coverage/Hour/Hour_1_10.coverage", "c7b52014-4c54-424c-bea1-90c5814e430f");
        return this.hour;
    }

    /**
     * Returns the day in which this hour falls.
     *
     * @return The day.
     */
    public Day getDay() {
        writeline("/home/ubuntu/results/coverage/Hour/Hour_1_10.coverage", "33d6d6d0-cbe7-496e-b53d-b9dcbddcedd9");
        return this.day;
    }

    /**
     * Returns the year in which this hour falls.
     *
     * @return The year.
     */
    public int getYear() {
        writeline("/home/ubuntu/results/coverage/Hour/Hour_1_10.coverage", "d61bdd6f-b63f-48d9-a611-3f3d3d49f9ed");
        return this.day.getYear();
    }

    /**
     * Returns the month in which this hour falls.
     *
     * @return The month.
     */
    public int getMonth() {
        writeline("/home/ubuntu/results/coverage/Hour/Hour_1_10.coverage", "0150ed2c-e534-46cb-a75e-27c20bcd1846");
        return this.day.getMonth();
    }

    /**
     * Returns the day-of-the-month in which this hour falls.
     *
     * @return The day-of-the-month.
     */
    public int getDayOfMonth() {
        writeline("/home/ubuntu/results/coverage/Hour/Hour_1_10.coverage", "4528ffb6-b5fa-4423-90d0-bd6c048d6e41");
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
        writeline("/home/ubuntu/results/coverage/Hour/Hour_1_10.coverage", "4df82e24-975f-48d5-bb1d-1cc4a95b3c1f");
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
        writeline("/home/ubuntu/results/coverage/Hour/Hour_1_10.coverage", "b9b16851-0f6a-40c9-9bdc-5cf6fd9c24bf");
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
        writeline("/home/ubuntu/results/coverage/Hour/Hour_1_10.coverage", "4367e4fa-f1bd-4950-bd26-5947c4d5c58c");
        this.firstMillisecond = getFirstMillisecond(calendar);
        writeline("/home/ubuntu/results/coverage/Hour/Hour_1_10.coverage", "84272801-68c5-4c65-aa81-43be72075b0a");
        this.lastMillisecond = getLastMillisecond(calendar);
    }

    /**
     * Returns the hour preceding this one.
     *
     * @return The hour preceding this one.
     */
    @Override
    public RegularTimePeriod previous() {
        writeline("/home/ubuntu/results/coverage/Hour/Hour_1_10.coverage", "49314995-e3a6-42b7-a6b5-790b348ace3b");
        Hour result;
        writeline("/home/ubuntu/results/coverage/Hour/Hour_1_10.coverage", "cd1c78ed-3a0d-46d4-8124-f9112d40dd73");
        if (this.hour != FIRST_HOUR_IN_DAY) {
            writeline("/home/ubuntu/results/coverage/Hour/Hour_1_10.coverage", "e9462885-6278-4823-a66e-c3e76be42bb6");
            result = new Hour(this.hour - 1, this.day);
        } else {
            writeline("/home/ubuntu/results/coverage/Hour/Hour_1_10.coverage", "b084810c-979a-4cc9-9882-c8938197d490");
            Day prevDay = (Day) this.day.previous();
            writeline("/home/ubuntu/results/coverage/Hour/Hour_1_10.coverage", "00845bfe-7187-4ae6-ac6b-8d31604b2b0f");
            if (prevDay != null) {
                writeline("/home/ubuntu/results/coverage/Hour/Hour_1_10.coverage", "2022f885-2937-4a5a-9da9-a95216047976");
                result = new Hour(LAST_HOUR_IN_DAY, prevDay);
            } else {
                writeline("/home/ubuntu/results/coverage/Hour/Hour_1_10.coverage", "45a8b791-cdca-405c-a99a-70d2a3b7cab1");
                result = null;
            }
        }
        writeline("/home/ubuntu/results/coverage/Hour/Hour_1_10.coverage", "93d11977-feda-4f98-8d94-ee5b8822a6c4");
        return result;
    }

    /**
     * Returns the hour following this one.
     *
     * @return The hour following this one.
     */
    @Override
    public RegularTimePeriod next() {
        writeline("/home/ubuntu/results/coverage/Hour/Hour_1_10.coverage", "fc9ed67a-b542-457d-932e-440fece6f722");
        Hour result;
        writeline("/home/ubuntu/results/coverage/Hour/Hour_1_10.coverage", "833e8452-8935-45e9-8fcb-7f6f19bdba45");
        if (this.hour != LAST_HOUR_IN_DAY) {
            writeline("/home/ubuntu/results/coverage/Hour/Hour_1_10.coverage", "89a1e9c5-d9a4-4fca-ba5f-2c9dd35ed9bf");
            result = new Hour(this.hour + 1, this.day);
        } else {
            writeline("/home/ubuntu/results/coverage/Hour/Hour_1_10.coverage", "e56fbb69-6442-483c-9ce0-40c18aae4581");
            Day nextDay = (Day) this.day.next();
            writeline("/home/ubuntu/results/coverage/Hour/Hour_1_10.coverage", "228f822d-e236-4d1d-8b7b-cd4b4aaf9395");
            if (nextDay != null) {
                writeline("/home/ubuntu/results/coverage/Hour/Hour_1_10.coverage", "d4f0a907-f08a-4a5e-b5e8-95554e302640");
                result = new Hour(FIRST_HOUR_IN_DAY, nextDay);
            } else {
                writeline("/home/ubuntu/results/coverage/Hour/Hour_1_10.coverage", "6483a57f-b3ec-4e35-a9cd-0d5566d1058b");
                result = null;
            }
        }
        writeline("/home/ubuntu/results/coverage/Hour/Hour_1_10.coverage", "187ad7ba-828c-4c73-b581-71ed95e89526");
        return result;
    }

    /**
     * Returns a serial index number for the hour.
     *
     * @return The serial index number.
     */
    @Override
    public long getSerialIndex() {
        writeline("/home/ubuntu/results/coverage/Hour/Hour_1_10.coverage", "94fc0b0b-67f8-456b-8d57-fe4598ebf7bb");
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
        writeline("/home/ubuntu/results/coverage/Hour/Hour_1_10.coverage", "b5089566-942c-4851-8edb-6bebec8f6bb9");
        int year = this.day.getYear();
        writeline("/home/ubuntu/results/coverage/Hour/Hour_1_10.coverage", "8dbcff1f-604e-4104-87a5-b58888a08b97");
        int month = this.day.getMonth() - 1;
        writeline("/home/ubuntu/results/coverage/Hour/Hour_1_10.coverage", "3a5525eb-38fc-49fd-9731-36398511fa42");
        int dom = this.day.getDayOfMonth();
        writeline("/home/ubuntu/results/coverage/Hour/Hour_1_10.coverage", "27fcf29c-d4e6-4d27-9ffe-d5b2f653aa12");
        calendar.set(year, month, dom, this.hour, 0, 0);
        writeline("/home/ubuntu/results/coverage/Hour/Hour_1_10.coverage", "9ff6fd93-8987-4320-91d9-0d4b66a94878");
        calendar.set(Calendar.MILLISECOND, 0);
        writeline("/home/ubuntu/results/coverage/Hour/Hour_1_10.coverage", "a5874c02-eb03-4ae7-8922-5f457dd73055");
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
        writeline("/home/ubuntu/results/coverage/Hour/Hour_1_10.coverage", "f3df034f-d1c6-4e07-9e17-26d8a0a4a931");
        int year = this.day.getYear();
        writeline("/home/ubuntu/results/coverage/Hour/Hour_1_10.coverage", "ea8be9d4-c844-4e66-9082-0d0d0b08f1d4");
        int month = this.day.getMonth() - 1;
        writeline("/home/ubuntu/results/coverage/Hour/Hour_1_10.coverage", "05fd9634-29e0-4c54-9ac2-b0ce0050c583");
        int dom = this.day.getDayOfMonth();
        writeline("/home/ubuntu/results/coverage/Hour/Hour_1_10.coverage", "945a7ac3-de87-43fc-9e7c-38b36c280800");
        calendar.set(year, month, dom, this.hour, 59, 59);
        writeline("/home/ubuntu/results/coverage/Hour/Hour_1_10.coverage", "11737f03-9d22-4985-8952-a6f0dd00a0fc");
        calendar.set(Calendar.MILLISECOND, 999);
        writeline("/home/ubuntu/results/coverage/Hour/Hour_1_10.coverage", "bbb930e1-26c5-4687-8f19-b4b24378b292");
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
        writeline("/home/ubuntu/results/coverage/Hour/Hour_1_10.coverage", "537e3c03-36b7-4a6a-8382-94e04b60a19b");
        if (obj == this) {
            writeline("/home/ubuntu/results/coverage/Hour/Hour_1_10.coverage", "c3c12d58-e048-4cbe-b062-fa8ceb445024");
            return true;
        }
        writeline("/home/ubuntu/results/coverage/Hour/Hour_1_10.coverage", "54e10e84-c5fd-4b84-aa23-6c4a1488402b");
        if (!(obj instanceof Hour)) {
            writeline("/home/ubuntu/results/coverage/Hour/Hour_1_10.coverage", "4a308a22-b208-4f9c-9ed6-61f0d7a3f3b4");
            return false;
        }
        writeline("/home/ubuntu/results/coverage/Hour/Hour_1_10.coverage", "3e61fb04-9023-4a1d-976c-0cd71277f611");
        Hour that = (Hour) obj;
        writeline("/home/ubuntu/results/coverage/Hour/Hour_1_10.coverage", "dadf46b3-542f-4c09-ac30-8d022e9d6dc4");
        if (this.hour != that.hour) {
            writeline("/home/ubuntu/results/coverage/Hour/Hour_1_10.coverage", "a07839ad-a7f7-4e2f-a2f4-6fded6f7b328");
            return false;
        }
        writeline("/home/ubuntu/results/coverage/Hour/Hour_1_10.coverage", "99b4f0f8-4e63-4c87-8dca-f327709e3c71");
        if (!this.day.equals(that.day)) {
            writeline("/home/ubuntu/results/coverage/Hour/Hour_1_10.coverage", "ee4f3f2d-9e47-4720-9bc1-7b3277108d5a");
            return false;
        }
        writeline("/home/ubuntu/results/coverage/Hour/Hour_1_10.coverage", "61e80a93-d9d0-48dd-a8f5-d7923e895ed8");
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
        writeline("/home/ubuntu/results/coverage/Hour/Hour_1_10.coverage", "5b6a5b7f-1b27-465d-b545-30a5c1444e2d");
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
        writeline("/home/ubuntu/results/coverage/Hour/Hour_1_10.coverage", "ee125c26-492d-4e34-99d3-552227ea14fe");
        int result = 17;
        writeline("/home/ubuntu/results/coverage/Hour/Hour_1_10.coverage", "c228ca72-b249-4803-8fb5-c79a49a61a63");
        result = 37 * result + this.hour;
        writeline("/home/ubuntu/results/coverage/Hour/Hour_1_10.coverage", "238e3b77-f839-4cc7-b762-8668e7915d9e");
        result = 37 * result + this.day.hashCode();
        writeline("/home/ubuntu/results/coverage/Hour/Hour_1_10.coverage", "4b876761-2b20-4506-9a5a-280e83629543");
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
        writeline("/home/ubuntu/results/coverage/Hour/Hour_1_10.coverage", "ec23e22a-ac43-40bd-bdb8-c0202ba8988c");
        int result;
        writeline("/home/ubuntu/results/coverage/Hour/Hour_1_10.coverage", "77b55a53-2c7c-48d2-9d8a-3536a42f615b");
        if (o1 instanceof Hour) {
            writeline("/home/ubuntu/results/coverage/Hour/Hour_1_10.coverage", "6d4df851-27e7-48bd-929c-2f5ba019b038");
            Hour h = (Hour) o1;
            writeline("/home/ubuntu/results/coverage/Hour/Hour_1_10.coverage", "555c6d68-705d-4965-9aa8-36451aa08a78");
            result = getDay().compareTo(h.getDay());
            writeline("/home/ubuntu/results/coverage/Hour/Hour_1_10.coverage", "35880fe4-f2ff-499e-ab6e-b195bbc7dff8");
            if (result == 0) {
                writeline("/home/ubuntu/results/coverage/Hour/Hour_1_10.coverage", "f26a4e4e-b9e7-4311-94b6-f90d65e83db0");
                result = this.hour - h.getHour();
            }
        } else if (o1 instanceof RegularTimePeriod) {
            writeline("/home/ubuntu/results/coverage/Hour/Hour_1_10.coverage", "11d05b84-e7fc-4846-9b72-3febcb46f196");
            result = 0;
        } else {
            writeline("/home/ubuntu/results/coverage/Hour/Hour_1_10.coverage", "d594d117-7f6f-4ac1-96a7-62364b5baa07");
            result = 1;
        }
        writeline("/home/ubuntu/results/coverage/Hour/Hour_1_10.coverage", "073a57f7-ff74-4daa-9494-b76eba44e530");
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
        writelineStatic("/home/ubuntu/results/coverage/Hour/Hour_1_10.coverage", "94153c34-ec1c-43e1-a510-c3f973952306");
        Hour result = null;
        writelineStatic("/home/ubuntu/results/coverage/Hour/Hour_1_10.coverage", "d54d4698-ad42-4315-bf46-8e859b932117");
        s = s.trim();
        writelineStatic("/home/ubuntu/results/coverage/Hour/Hour_1_10.coverage", "07d00e7c-6421-4cd1-8b85-bc5417d214f4");
        String daystr = s.substring(0, Math.min(10, s.length()));
        writelineStatic("/home/ubuntu/results/coverage/Hour/Hour_1_10.coverage", "1c3e9130-53b1-442a-b154-b3df8e193a7f");
        Day day = Day.parseDay(daystr);
        writelineStatic("/home/ubuntu/results/coverage/Hour/Hour_1_10.coverage", "2a5156e1-6c4a-45fd-a4be-51e5e2e5ef93");
        if (day != null) {
            writelineStatic("/home/ubuntu/results/coverage/Hour/Hour_1_10.coverage", "06ff9f86-91a6-49aa-8a0c-e00d6211ceeb");
            String hourstr = s.substring(Math.min(daystr.length() + 1, s.length()), s.length());
            writelineStatic("/home/ubuntu/results/coverage/Hour/Hour_1_10.coverage", "c13438ae-a3ed-4eeb-9db0-cdb8eaea24d5");
            hourstr = hourstr.trim();
            writelineStatic("/home/ubuntu/results/coverage/Hour/Hour_1_10.coverage", "46ed1cda-b26c-42c1-9364-2e460b0d48b3");
            int hour = Integer.parseInt(hourstr);
            writelineStatic("/home/ubuntu/results/coverage/Hour/Hour_1_10.coverage", "d3a69655-bbbd-40e0-ad6d-28918b134710");
            if ((hour >= FIRST_HOUR_IN_DAY) && (hour <= LAST_HOUR_IN_DAY)) {
                writelineStatic("/home/ubuntu/results/coverage/Hour/Hour_1_10.coverage", "b0ca2166-784d-4076-a7cd-33942256567f");
                result = new Hour(hour, day);
            }
        }
        writelineStatic("/home/ubuntu/results/coverage/Hour/Hour_1_10.coverage", "3ef59afe-733e-497b-bd7c-ebf1a3ed6915");
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

    public static void writelineStatic(String fullFilePath, String text) {
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
