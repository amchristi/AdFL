package org.apache.commons.jexl3;

import org.apache.commons.jexl3.introspection.JexlMethod;
import java.lang.reflect.Array;
import java.math.BigDecimal;
import java.math.BigInteger;
import java.math.MathContext;
import java.util.Collection;
import java.util.Map;
import java.util.regex.Pattern;
import java.io.*;

/**
 * Perform arithmetic, implements JexlOperator methods.
 * 
 * <p>This is the class to derive to implement new operator behaviors.</p>
 * 
 * <p>The 5 base arithmetic operators (+, - , *, /, %) follow the same evaluation rules regarding their arguments.</p>
 * <ol>
 *   <li>If both are null, result is 0</li>
 *   <li>If either is a BigDecimal, coerce both to BigDecimal and perform operation</li>
 *   <li>If either is a floating point number, coerce both to Double and perform operation</li>
 *   <li>Else treat as BigInteger, perform operation and attempt to narrow result:
 *     <ol>
 *       <li>if both arguments can be narrowed to Integer, narrow result to Integer</li>
 *       <li>if both arguments can be narrowed to Long, narrow result to Long</li>
 *       <li>Else return result as BigInteger</li>
 *     </ol>
 *   </li>
 * </ol>
 * 
 * Note that the only exception thrown by JexlArithmetic is and must be ArithmeticException.
 * 
 * @see JexlOperator
 * @since 2.0
 */
public class JexlArithmetic {

    /** Marker class for null operand exceptions. */
    public static class NullOperand extends ArithmeticException {
    }

    /** Double.MAX_VALUE as BigDecimal. */
    protected static final BigDecimal BIGD_DOUBLE_MAX_VALUE = BigDecimal.valueOf(Double.MAX_VALUE);

    /** Double.MIN_VALUE as BigDecimal. */
    protected static final BigDecimal BIGD_DOUBLE_MIN_VALUE = BigDecimal.valueOf(Double.MIN_VALUE);

    /** Long.MAX_VALUE as BigInteger. */
    protected static final BigInteger BIGI_LONG_MAX_VALUE = BigInteger.valueOf(Long.MAX_VALUE);

    /** Long.MIN_VALUE as BigInteger. */
    protected static final BigInteger BIGI_LONG_MIN_VALUE = BigInteger.valueOf(Long.MIN_VALUE);

    /** Default BigDecimal scale. */
    protected static final int BIGD_SCALE = -1;

    /** Whether this JexlArithmetic instance behaves in strict or lenient mode. */
    private final boolean strict;

    /** The big decimal math context. */
    private final MathContext mathContext;

    /** The big decimal scale. */
    private final int mathScale;

    /**
     * Creates a JexlArithmetic.
     * 
     * @param astrict whether this arithmetic is strict or lenient
     */
    public JexlArithmetic(boolean astrict) {
        this(astrict, null, Integer.MIN_VALUE);
    }

    /**
     * Creates a JexlArithmetic.
     * 
     * @param astrict     whether this arithmetic is lenient or strict
     * @param bigdContext the math context instance to use for +,-,/,*,% operations on big decimals.
     * @param bigdScale   the scale used for big decimals.
     */
    public JexlArithmetic(boolean astrict, MathContext bigdContext, int bigdScale) {
        this.strict = astrict;
        this.mathContext = bigdContext == null ? MathContext.DECIMAL128 : bigdContext;
        this.mathScale = bigdScale == Integer.MIN_VALUE ? BIGD_SCALE : bigdScale;
    }

    /**
     * Apply options to this arithmetic which eventually may create another instance.
     * 
     * @param options the {@link JexlEngine.Options} to use
     * @return an arithmetic with those options set
     */
    public JexlArithmetic options(JexlEngine.Options options) {
        writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_2_10.coverage", "9f0979a1-3997-4765-9443-89d29ca4c02c");
        boolean ostrict = options.isStrictArithmetic() == null ? this.strict : options.isStrictArithmetic();
        writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_2_10.coverage", "4e8fbab9-9a80-4e03-8f8c-6cdbfa12e8ec");
        MathContext bigdContext = options.getArithmeticMathContext();
        writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_2_10.coverage", "935a8b59-749a-459b-a0ba-631d1062c494");
        if (bigdContext == null) {
            writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_2_10.coverage", "6a453e12-d1b7-4aee-a284-f7ac0505366d");
            bigdContext = mathContext;
        }
        writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_2_10.coverage", "53d0a393-3f6b-4fff-8315-04468d56da9e");
        int bigdScale = options.getArithmeticMathScale();
        writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_2_10.coverage", "35e9fc9a-a164-42f7-bade-310716d8f796");
        if (bigdScale == Integer.MIN_VALUE) {
            writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_2_10.coverage", "9627d2f3-1447-4178-b18b-38d8e6438ef5");
            bigdScale = mathScale;
        }
        writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_2_10.coverage", "50e826f1-643e-40c3-8111-82e3ac5977c1");
        if ((ostrict != this.strict) || bigdScale != this.mathScale || bigdContext != this.mathContext) {
            writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_2_10.coverage", "5251168e-954d-4ab7-99bc-3efd09be0716");
            return new JexlArithmetic(ostrict, bigdContext, bigdScale);
        } else {
            writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_2_10.coverage", "df4abbe8-355f-4b8a-b2a9-3f420a5958a3");
            return this;
        }
    }

    /**
     * The interface that uberspects JexlArithmetic classes.
     * <p>This allows overloaded operator methods discovery.</p>
     */
    public interface Uberspect {

        /**
         * Checks whether this uberspect has overloads for a given operator.
         * 
         * @param operator the operator to check
         * @return true if an overload exists, false otherwise
         */
        boolean overloads(JexlOperator operator);

        /**
         * Gets the most specific method for an operator.
         * 
         * @param operator the operator
         * @param arg      the arguments
         * @return the most specific method or null if no specific override could be found
         */
        JexlMethod getOperator(JexlOperator operator, Object... arg);
    }

    /**
     * Helper interface used when creating an array literal.
     * 
     * <p>The default implementation creates an array and attempts to type it strictly.</p>
     * 
     * <ul>
     *   <li>If all objects are of the same type, the array returned will be an array of that same type</li>
     *   <li>If all objects are Numbers, the array returned will be an array of Numbers</li>
     *   <li>If all objects are convertible to a primitive type, the array returned will be an array
     *       of the primitive type</li>
     * </ul>
     */
    public interface ArrayBuilder {

        /**
         * Adds a literal to the array.
         * 
         * @param value the item to add
         */
        void add(Object value);

        /**
         * Creates the actual "array" instance.
         * 
         * @param extended true when the last argument is ', ...'
         * @return the array
         */
        Object create(boolean extended);
    }

    /**
     * Called by the interpreter when evaluating a literal array.
     * 
     * @param size the number of elements in the array
     * @return the array builder
     */
    public ArrayBuilder arrayBuilder(int size) {
        writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_2_10.coverage", "e4638908-b578-4cf7-bc5e-a77fc76cbb14");
        return new org.apache.commons.jexl3.internal.ArrayBuilder(size);
    }

    /**
     * Helper interface used when creating a set literal.
     * <p>The default implementation creates a java.util.HashSet.</p>
     */
    public interface SetBuilder {

        /**
         * Adds a literal to the set.
         * 
         * @param value the item to add
         */
        void add(Object value);

        /**
         * Creates the actual "set" instance.
         * 
         * @return the set
         */
        Object create();
    }

    /**
     * Called by the interpreter when evaluating a literal set.
     * 
     * @param size the number of elements in the set
     * @return the array builder
     */
    public SetBuilder setBuilder(int size) {
        writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_2_10.coverage", "9523f1a6-3802-4374-9a03-5776dbe9aad9");
        return new org.apache.commons.jexl3.internal.SetBuilder(size);
    }

    /**
     * Helper interface used when creating a map literal.
     * <p>The default implementation creates a java.util.HashMap.</p>
     */
    public interface MapBuilder {

        /**
         * Adds a new entry to the map.
         * 
         * @param key   the map entry key
         * @param value the map entry value
         */
        void put(Object key, Object value);

        /**
         * Creates the actual "map" instance.
         * 
         * @return the map
         */
        Object create();
    }

    /**
     * Called by the interpreter when evaluating a literal map.
     * 
     * @param size the number of elements in the map
     * @return the map builder
     */
    public MapBuilder mapBuilder(int size) {
        writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_2_10.coverage", "adc284fc-9e6a-477f-858e-9b3bf4e9c06f");
        return new org.apache.commons.jexl3.internal.MapBuilder(size);
    }

    /**
     * Creates a literal range.
     * <p>The default implementation only accepts integers and longs.</p>
     * 
     * @param from the included lower bound value (null if none)
     * @param to   the included upper bound value (null if none)
     * @return the range as an iterable
     * @throws ArithmeticException as an option if creation fails
     */
    public Iterable<?> createRange(Object from, Object to) throws ArithmeticException {
        writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_2_10.coverage", "8a57d0de-bce2-436b-b7be-216ed0023a1c");
        final long lfrom = toLong(from);
        writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_2_10.coverage", "491881eb-6447-4c29-b965-0403f19ee4fb");
        final long lto = toLong(to);
        writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_2_10.coverage", "44244b2e-2de2-41a0-96af-adc34b4485fd");
        if ((lfrom >= Integer.MIN_VALUE && lfrom <= Integer.MAX_VALUE) && (lto >= Integer.MIN_VALUE && lto <= Integer.MAX_VALUE)) {
            writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_2_10.coverage", "ff970d27-5c89-464f-b97c-34bc03e7ef56");
            return org.apache.commons.jexl3.internal.IntegerRange.create((int) lfrom, (int) lto);
        } else {
            writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_2_10.coverage", "0577c287-91d3-4c27-a44d-f7948a9e2a3a");
            return org.apache.commons.jexl3.internal.LongRange.create(lfrom, lto);
        }
    }

    /**
     * Checks whether this JexlArithmetic instance
     * strictly considers null as an error when used as operand unexpectedly.
     * 
     * @return true if strict, false if lenient
     */
    public boolean isStrict() {
        writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_2_10.coverage", "1c026d3b-7f6d-4701-be53-46b8b23be0d6");
        return this.strict;
    }

    /**
     * The MathContext instance used for +,-,/,*,% operations on big decimals.
     * 
     * @return the math context
     */
    public MathContext getMathContext() {
        writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_2_10.coverage", "c3da4e42-eea5-431d-9d87-e6b26b32b3f9");
        return mathContext;
    }

    /**
     * The BigDecimal scale used for comparison and coericion operations.
     * 
     * @return the scale
     */
    public int getMathScale() {
        writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_2_10.coverage", "77f1b290-5163-4d8b-8e78-edb67ba421f4");
        return mathScale;
    }

    /**
     * Ensure a big decimal is rounded by this arithmetic scale and rounding mode.
     * 
     * @param number the big decimal to round
     * @return the rounded big decimal
     */
    protected BigDecimal roundBigDecimal(final BigDecimal number) {
        writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_2_10.coverage", "63e15c49-1a61-440d-8e7b-e8d6423f098d");
        int mscale = getMathScale();
        writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_2_10.coverage", "649239a3-240e-402b-9842-932ee3ef9456");
        if (mscale >= 0) {
            writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_2_10.coverage", "788b15db-2396-4602-8758-8de7bacbff67");
            return number.setScale(mscale, getMathContext().getRoundingMode());
        } else {
            writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_2_10.coverage", "685d0fd1-ebd2-42cb-bac2-4e78632faf3a");
            return number;
        }
    }

    /**
     * The result of +,/,-,*,% when both operands are null.
     * 
     * @return Integer(0) if lenient
     * @throws ArithmeticException if strict
     */
    protected Object controlNullNullOperands() {
        writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_2_10.coverage", "0a48f380-b96a-4dfb-b349-bf98b4681039");
        if (isStrict()) {
            writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_2_10.coverage", "42af1513-33c4-4f65-96c6-5b74f31e717d");
            throw new NullOperand();
        }
        writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_2_10.coverage", "e765f3ba-bf47-4079-b81b-3882b698dbbf");
        return 0;
    }

    /**
     * Throw a NPE if arithmetic is strict.
     * 
     * @throws ArithmeticException if strict
     */
    protected void controlNullOperand() {
        writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_2_10.coverage", "6d05eeb4-a5f1-47d9-9c49-79a475e1abf0");
        if (isStrict()) {
            writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_2_10.coverage", "daeb08a4-6094-4807-b20f-d69996bac152");
            throw new NullOperand();
        }
    }

    /**
     * The float regular expression pattern.
     */
    public static final Pattern FLOAT_PATTERN = Pattern.compile("^[+-]?\\d*(\\.\\d*)?([eE]?[+-]?\\d*)?$");

    /**
     * Test if the passed value is a floating point number, i.e. a float, double
     * or string with ( "." | "E" | "e").
     *
     * @param val the object to be tested
     * @return true if it is, false otherwise.
     */
    protected boolean isFloatingPointNumber(Object val) {
        writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_2_10.coverage", "5296d01f-bcc1-406b-a677-d287ae19f612");
        if (val instanceof Float || val instanceof Double) {
            writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_2_10.coverage", "50245c0d-475c-49e6-8946-72ac922683ef");
            return true;
        }
        writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_2_10.coverage", "4f0d1955-c9cf-4a4f-8732-d0ae2ac64af9");
        if (val instanceof String) {
            writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_2_10.coverage", "30b14158-c131-404f-bb7e-938c0578d680");
            String str = (String) val;
            writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_2_10.coverage", "3456f582-341a-49c2-9a74-b6959779571a");
            for (int c = 0; c < str.length(); ++c) {
                writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_2_10.coverage", "c46f87f2-7e4b-46dd-9716-b9aeeaf6597e");
                char ch = str.charAt(c);
                writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_2_10.coverage", "92c9f2cc-bcd5-4a0d-901b-1d3a81e42bd0");
                if (ch == '.' || ch == 'E' || ch == 'e') {
                    writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_2_10.coverage", "9fa4eaa7-e42c-4ba4-9eae-aa95a95c53f6");
                    return FLOAT_PATTERN.matcher(str).matches();
                }
                writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_2_10.coverage", "f16ab2c5-3a9b-45bb-91b8-45b4f243eec3");
                if (ch != '+' && ch != '-' && ch < '0' && ch > '9') {
                    writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_2_10.coverage", "6bb77c4e-562c-4bf0-9ed3-b9b475b5af0d");
                    break;
                }
            }
        }
        writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_2_10.coverage", "8bb0883c-22b6-4797-8fc0-e6c8f852a4f8");
        return false;
    }

    /**
     * Is Object a floating point number.
     *
     * @param o Object to be analyzed.
     * @return true if it is a Float or a Double.
     */
    protected boolean isFloatingPoint(final Object o) {
        writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_2_10.coverage", "e4698715-f279-4c53-ba28-2354dd3a8e35");
        return o instanceof Float || o instanceof Double;
    }

    /**
     * Is Object a whole number.
     *
     * @param o Object to be analyzed.
     * @return true if Integer, Long, Byte, Short or Character.
     */
    protected boolean isNumberable(final Object o) {
        writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_2_10.coverage", "d9f594ee-ba4c-4d9d-955e-51a6eab4d2a8");
        return o instanceof Integer || o instanceof Long || o instanceof Byte || o instanceof Short || o instanceof Character;
    }

    /**
     * Given a Number, return back the value using the smallest type the result
     * will fit into.
     * <p>This works hand in hand with parameter 'widening' in java
     * method calls, e.g. a call to substring(int,int) with an int and a long
     * will fail, but a call to substring(int,int) with an int and a short will
     * succeed.</p>
     *
     * @param original the original number.
     * @return a value of the smallest type the original number will fit into.
     */
    public Number narrow(Number original) {
        writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_2_10.coverage", "e257935f-6443-4c13-a287-e886d407fc0c");
        return narrowNumber(original, null);
    }

    /**
     * Whether we consider the narrow class as a potential candidate for narrowing the source.
     * 
     * @param narrow the target narrow class
     * @param source the orginal source class
     * @return true if attempt to narrow source to target is accepted
     */
    protected boolean narrowAccept(Class<?> narrow, Class<?> source) {
        writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_2_10.coverage", "d7eef40c-f900-462c-ae2a-9e95438aaa5c");
        return narrow == null || narrow.equals(source);
    }

    /**
     * Given a Number, return back the value attempting to narrow it to a target class.
     * 
     * @param original the original number
     * @param narrow   the attempted target class
     * @return the narrowed number or the source if no narrowing was possible
     */
    public Number narrowNumber(Number original, Class<?> narrow) {
        writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_2_10.coverage", "bf7f6d33-6d48-444d-afda-1db3d17e1571");
        if (original == null) {
            writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_2_10.coverage", "ae982564-7715-4db2-8e4e-f34d50931c6e");
            return null;
        }
        writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_2_10.coverage", "4978a2d4-ad23-403a-946d-10f50dd9afdd");
        Number result = original;
        writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_2_10.coverage", "1a6b929c-ddc1-4da4-909c-19d1e9be6599");
        if (original instanceof BigDecimal) {
            writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_2_10.coverage", "1c6d0ac3-38ef-47ea-aed5-d383b08f17e7");
            BigDecimal bigd = (BigDecimal) original;
            writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_2_10.coverage", "bd2f523e-81eb-47d8-9ac3-c76f6315ccd7");
            if (bigd.compareTo(BIGD_DOUBLE_MAX_VALUE) > 0 || bigd.compareTo(BIGD_DOUBLE_MIN_VALUE) < 0) {
                writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_2_10.coverage", "88add7d0-6bb6-437a-af4a-bc2c83c75b7b");
                return original;
            } else {
                writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_2_10.coverage", "e3839df3-19fa-4e40-8d8a-58fbcb78e0c2");
                try {
                    writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_2_10.coverage", "a15a92bd-db65-47aa-9313-89b2790dd827");
                    long l = bigd.longValueExact();
                    writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_2_10.coverage", "e7fce6ab-fefe-4c5a-89db-519afbce768b");
                    if (narrowAccept(narrow, Integer.class) && l <= Integer.MAX_VALUE && l >= Integer.MIN_VALUE) {
                        writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_2_10.coverage", "badd1ec3-2e25-450d-b8e4-4a8ddf50d7d1");
                        return (int) l;
                    } else if (narrowAccept(narrow, Long.class)) {
                        writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_2_10.coverage", "5d5148c4-881b-4afb-a60e-75707fb41ba5");
                        return l;
                    }
                } catch (ArithmeticException xa) {
                }
            }
        }
        writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_2_10.coverage", "ef8a251c-1701-47a4-a6ee-5c1b8cc481c3");
        if (original instanceof Double || original instanceof Float) {
            writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_2_10.coverage", "bc8bbe69-b45f-408c-bfd8-29ef4d3d0eca");
            double value = original.doubleValue();
            writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_2_10.coverage", "57bbf8e2-23d1-4efe-99bb-607aba399b2e");
            if (narrowAccept(narrow, Float.class) && value <= Float.MAX_VALUE && value >= Float.MIN_VALUE) {
                writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_2_10.coverage", "cf123b3b-803c-41d9-8d92-bddf1410b6e0");
                result = result.floatValue();
            }
        } else {
            writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_2_10.coverage", "877a57d3-4d74-4380-8271-fb9cfeea2829");
            if (original instanceof BigInteger) {
                writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_2_10.coverage", "c8c63255-084c-43bf-a8f0-04b317b1200f");
                BigInteger bigi = (BigInteger) original;
                writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_2_10.coverage", "10939064-e2b1-4bee-99c2-a54fd37e3c2d");
                if (bigi.compareTo(BIGI_LONG_MAX_VALUE) > 0 || bigi.compareTo(BIGI_LONG_MIN_VALUE) < 0) {
                    writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_2_10.coverage", "5d42d149-0a20-46d3-8d48-0093cbec5668");
                    return original;
                }
            }
            writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_2_10.coverage", "c3d1c033-37e5-4b4a-acf1-bfd865aa9aee");
            long value = original.longValue();
            writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_2_10.coverage", "3060f21b-328d-46c4-ae55-9453c042a5d0");
            if (narrowAccept(narrow, Byte.class) && value <= Byte.MAX_VALUE && value >= Byte.MIN_VALUE) {
                writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_2_10.coverage", "4a0bdf8e-8a8a-48bc-9839-69fd147fdcfd");
                result = (byte) value;
            } else if (narrowAccept(narrow, Short.class) && value <= Short.MAX_VALUE && value >= Short.MIN_VALUE) {
                writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_2_10.coverage", "25ecb2da-e628-43a8-b879-f099afc4dad6");
                result = (short) value;
            } else if (narrowAccept(narrow, Integer.class) && value <= Integer.MAX_VALUE && value >= Integer.MIN_VALUE) {
                writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_2_10.coverage", "428f96a6-afd8-484b-910f-95724cc2e21f");
                result = (int) value;
            }
        }
        writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_2_10.coverage", "1a7f08dd-f532-4dc3-9b8e-f67293af2b4c");
        return result;
    }

    /**
     * Given a BigInteger, narrow it to an Integer or Long if it fits and the arguments
     * class allow it.
     * <p>
     * The rules are:
     * if either arguments is a BigInteger, no narrowing will occur
     * if either arguments is a Long, no narrowing to Integer will occur
     * </p>
     * 
     * @param lhs  the left hand side operand that lead to the bigi result
     * @param rhs  the right hand side operand that lead to the bigi result
     * @param bigi the BigInteger to narrow
     * @return an Integer or Long if narrowing is possible, the original BigInteger otherwise
     */
    protected Number narrowBigInteger(Object lhs, Object rhs, BigInteger bigi) {
        writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_2_10.coverage", "4188e78e-d7bc-4c36-ad35-fc159ac56981");
        if (!(lhs instanceof BigInteger || rhs instanceof BigInteger) && bigi.compareTo(BIGI_LONG_MAX_VALUE) <= 0 && bigi.compareTo(BIGI_LONG_MIN_VALUE) >= 0) {
            writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_2_10.coverage", "188722e2-55f5-4434-91da-ad197027bce0");
            long l = bigi.longValue();
            writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_2_10.coverage", "24aa4c68-12a9-4d88-aa16-39a725f022f4");
            if (!(lhs instanceof Long || rhs instanceof Long) && l <= Integer.MAX_VALUE && l >= Integer.MIN_VALUE) {
                writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_2_10.coverage", "a846282f-d0ee-457c-aa02-5be5e95dbcfc");
                return (int) l;
            }
            writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_2_10.coverage", "5124c42b-5187-4563-9bd5-458485b40750");
            return l;
        }
        writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_2_10.coverage", "c780444c-7b6f-4a89-8b25-dc29ef86f63a");
        return bigi;
    }

    /**
     * Given a BigDecimal, attempt to narrow it to an Integer or Long if it fits if
     * one of the arguments is a numberable.
     *
     * @param lhs  the left hand side operand that lead to the bigd result
     * @param rhs  the right hand side operand that lead to the bigd result
     * @param bigd the BigDecimal to narrow
     * @return an Integer or Long if narrowing is possible, the original BigInteger otherwise
     */
    protected Number narrowBigDecimal(Object lhs, Object rhs, BigDecimal bigd) {
        writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_2_10.coverage", "e1ae312f-c63e-40eb-9ba9-a657d572ed7a");
        if (isNumberable(lhs) || isNumberable(rhs)) {
            writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_2_10.coverage", "33a2c727-4987-4a89-b77a-d7728cf7bb21");
            try {
                writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_2_10.coverage", "5105bafa-4aa4-4edc-bcbd-b472289837a1");
                long l = bigd.longValueExact();
                writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_2_10.coverage", "eb89e48c-fd58-4c91-b474-69d00a7b98dc");
                if (l <= Integer.MAX_VALUE && l >= Integer.MIN_VALUE) {
                    writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_2_10.coverage", "75f0e47c-d9fc-4ca5-a916-17e0a28a9e0d");
                    return (int) l;
                } else {
                    writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_2_10.coverage", "ccfca20b-23a5-4121-a087-88ae4045272b");
                    return l;
                }
            } catch (ArithmeticException xa) {
            }
        }
        writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_2_10.coverage", "1ca2c353-7ba7-4485-a147-b753e95cb47c");
        return bigd;
    }

    /**
     * Replace all numbers in an arguments array with the smallest type that will fit.
     * 
     * @param args the argument array
     * @return true if some arguments were narrowed and args array is modified,
     *         false if no narrowing occurred and args array has not been modified
     */
    public boolean narrowArguments(Object[] args) {
        writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_2_10.coverage", "98b84379-f86d-47d1-aa5b-b88516c4df05");
        boolean narrowed = false;
        writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_2_10.coverage", "f87f5dea-53e3-4b91-8dae-36c38f7a1da9");
        for (int a = 0; a < args.length; ++a) {
            writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_2_10.coverage", "68331611-f129-45bd-a4b7-a32cdc688de2");
            Object arg = args[a];
            writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_2_10.coverage", "bb49e4b7-5593-4511-81ea-f9c93607ec8c");
            if (arg instanceof Number) {
                writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_2_10.coverage", "8208b243-22b9-4f3a-9daf-7943e35a7883");
                Number narg = (Number) arg;
                writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_2_10.coverage", "a0b9909c-a649-489f-9715-8b7aa9c7f74e");
                Number narrow = narrow(narg);
                writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_2_10.coverage", "ba3879c3-8a9e-4575-8aa3-b9d0a3261c3b");
                if (!narg.equals(narrow)) {
                    writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_2_10.coverage", "10e0bf44-7bc1-4a33-8c6a-6137eab737d9");
                    args[a] = narrow;
                    writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_2_10.coverage", "a0a25c01-28c3-4420-be24-e99c92c94942");
                    narrowed = true;
                }
            }
        }
        writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_2_10.coverage", "93661414-faa6-490d-87ab-22b404267dcf");
        return narrowed;
    }

    /**
     * Add two values together.
     * <p>
     * If any numeric add fails on coercion to the appropriate type,
     * treat as Strings and do concatenation.
     * </p>
     * 
     * @param left  left argument
     * @param right  right argument
     * @return left + right.
     */
    public Object add(Object left, Object right) {
        writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_2_10.coverage", "8cc03373-0a7f-46bd-9fcb-30935e0bfa39");
        if (left == null && right == null) {
            writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_2_10.coverage", "39b808f8-b212-4b2d-a9d5-763eb384ea89");
            return controlNullNullOperands();
        }
        writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_2_10.coverage", "97ef7571-9b9b-443a-9b17-9bf07f5675ca");
        boolean strconcat = strict ? left instanceof String || right instanceof String : left instanceof String && right instanceof String;
        writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_2_10.coverage", "782474a5-f043-4949-be2b-afb3156cb863");
        if (!strconcat) {
            writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_2_10.coverage", "c7a5837b-6af4-4d26-bf6a-739398a5ee5b");
            try {
                writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_2_10.coverage", "76668a08-40f6-4c0a-aca2-9f8b3a793669");
                if (left instanceof BigDecimal || right instanceof BigDecimal) {
                    writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_2_10.coverage", "17805260-5870-4df3-897c-f554e550fbf7");
                    BigDecimal l = toBigDecimal(left);
                    writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_2_10.coverage", "65f31218-9412-4016-afcb-0c895253c101");
                    BigDecimal r = toBigDecimal(right);
                    writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_2_10.coverage", "581ff659-6fa1-4d5f-b4db-2d572f401ba3");
                    BigDecimal result = l.add(r, getMathContext());
                    writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_2_10.coverage", "8acef5b5-0b26-4769-9e14-d98bd93c766d");
                    return narrowBigDecimal(left, right, result);
                }
                writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_2_10.coverage", "2a42e2fe-1042-40a0-a27c-e1f4ba2353a6");
                if (isFloatingPointNumber(left) || isFloatingPointNumber(right)) {
                    writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_2_10.coverage", "293b79b4-9bf4-46ba-b98d-255e6e932a10");
                    double l = toDouble(left);
                    writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_2_10.coverage", "2f7bf311-4aa5-44e2-a82a-166719bafad7");
                    double r = toDouble(right);
                    writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_2_10.coverage", "5d727d88-3d98-41cc-892a-b7db8ff67072");
                    return l + r;
                }
                writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_2_10.coverage", "5589e5f6-c078-43b3-9303-7bcde7b23ed6");
                BigInteger l = toBigInteger(left);
                writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_2_10.coverage", "bc4e1143-92be-4d14-be97-bb2c3eb71e84");
                BigInteger r = toBigInteger(right);
                writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_2_10.coverage", "7eddbc50-2a25-4407-9e2d-acb136f13afe");
                BigInteger result = l.add(r);
                writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_2_10.coverage", "4945d691-0d80-41f8-b383-28f31e3601a0");
                return narrowBigInteger(left, right, result);
            } catch (java.lang.NumberFormatException nfe) {
                writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_2_10.coverage", "b0fb5663-b390-46b4-84ba-dbc0572b328e");
                if (left == null || right == null) {
                    writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_2_10.coverage", "d5deb447-b108-43c0-8b43-1ae9586c75ae");
                    controlNullOperand();
                }
            }
        }
        writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_2_10.coverage", "5daf07a3-99a2-4af8-85ed-e746cf6be85d");
        return toString(left).concat(toString(right));
    }

    /**
     * Divide the left value by the right.
     * 
     * @param left  left argument
     * @param right  right argument
     * @return left / right
     * @throws ArithmeticException if right == 0
     */
    public Object divide(Object left, Object right) {
        writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_2_10.coverage", "26f3c987-eebd-47c4-a8a3-0b523730a87b");
        if (left == null && right == null) {
            writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_2_10.coverage", "eaacf012-61a2-4384-bead-4989a9822821");
            return controlNullNullOperands();
        }
        writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_2_10.coverage", "0a104020-9ef9-45e6-b5ff-f07874c7460b");
        if (left instanceof BigDecimal || right instanceof BigDecimal) {
            writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_2_10.coverage", "08e53702-84bd-4fd8-8237-60a443b8d648");
            BigDecimal l = toBigDecimal(left);
            writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_2_10.coverage", "9ef594b8-96c9-4043-be31-77fcb78bd28b");
            BigDecimal r = toBigDecimal(right);
            writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_2_10.coverage", "0e4a21d8-ae46-482c-9b43-13fe8524f1d9");
            if (BigDecimal.ZERO.equals(r)) {
                writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_2_10.coverage", "f9607701-4f7a-4360-8a5c-e3146087f229");
                throw new ArithmeticException("/");
            }
            writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_2_10.coverage", "76e2e0ae-ccb8-46b3-94a9-40eef1edff9f");
            BigDecimal result = l.divide(r, getMathContext());
            writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_2_10.coverage", "05de2374-3f1c-496b-96e9-bb0a1a791d63");
            return narrowBigDecimal(left, right, result);
        }
        writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_2_10.coverage", "a220f321-ccaa-433f-86a8-e2bf9562b634");
        if (isFloatingPointNumber(left) || isFloatingPointNumber(right)) {
            writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_2_10.coverage", "4b152faa-3343-41ae-8c9e-da2ab5dbbd6c");
            double l = toDouble(left);
            writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_2_10.coverage", "10f76cb8-c0d6-48f9-b3e5-d643961319bd");
            double r = toDouble(right);
            writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_2_10.coverage", "a28ffd93-b8d0-49f8-b9a2-3c74b7040d78");
            if (r == 0.0) {
                writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_2_10.coverage", "d890a0d7-5c65-4d9e-9a28-5c2dcd641ac2");
                throw new ArithmeticException("/");
            }
            writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_2_10.coverage", "6a6a26c5-5f98-4bf1-b86a-de7b0b16c44f");
            return l / r;
        }
        writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_2_10.coverage", "e1c49294-fff7-4fd7-acdd-b98e3eeda1c5");
        BigInteger l = toBigInteger(left);
        writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_2_10.coverage", "0b15deed-fe21-4c81-abeb-0e3429627784");
        BigInteger r = toBigInteger(right);
        writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_2_10.coverage", "aec2e294-9f29-4f8f-a834-b9e97a41f61c");
        if (BigInteger.ZERO.equals(r)) {
            writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_2_10.coverage", "7d365bb2-319b-49f0-b65f-b7b9d09c1d7b");
            throw new ArithmeticException("/");
        }
        writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_2_10.coverage", "38892a4e-ed90-4761-b60e-e6282c2909b2");
        BigInteger result = l.divide(r);
        writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_2_10.coverage", "a81f5e40-2254-45c8-962a-447c6423c995");
        return narrowBigInteger(left, right, result);
    }

    /**
     * left value modulo right.
     * 
     * @param left  left argument
     * @param right  right argument
     * @return left % right
     * @throws ArithmeticException if right == 0.0
     */
    public Object mod(Object left, Object right) {
        writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_2_10.coverage", "07f30680-ee94-4b0b-9f60-db251572b584");
        if (left == null && right == null) {
            writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_2_10.coverage", "a6034b1e-9bd9-4608-b34c-22635ff7a53f");
            return controlNullNullOperands();
        }
        writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_2_10.coverage", "2e369892-f3a7-4a49-b1f1-e6232bee69eb");
        if (left instanceof BigDecimal || right instanceof BigDecimal) {
            writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_2_10.coverage", "7fb99867-3aaa-42eb-afea-faa0a99d0b09");
            BigDecimal l = toBigDecimal(left);
            writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_2_10.coverage", "767b67f8-cb5e-4a3f-891f-d1439cfa2cc6");
            BigDecimal r = toBigDecimal(right);
            writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_2_10.coverage", "ca2859c8-65a2-4995-b7d6-0709d333ed9f");
            if (BigDecimal.ZERO.equals(r)) {
                writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_2_10.coverage", "8488e7dc-34fa-4733-b227-94aeddfc9c98");
                throw new ArithmeticException("%");
            }
            writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_2_10.coverage", "fca28c16-76f6-4f7b-9d45-a4725044118c");
            BigDecimal remainder = l.remainder(r, getMathContext());
            writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_2_10.coverage", "d1724ff1-0002-4210-8de6-0402bd84f30f");
            return narrowBigDecimal(left, right, remainder);
        }
        writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_2_10.coverage", "c59ea82d-9222-42db-8bd8-90ff90b9fd1d");
        if (isFloatingPointNumber(left) || isFloatingPointNumber(right)) {
            writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_2_10.coverage", "44c98216-9116-49eb-a46a-57e3189ea96c");
            double l = toDouble(left);
            writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_2_10.coverage", "259c6a7e-3064-4904-a403-a9192bafd190");
            double r = toDouble(right);
            writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_2_10.coverage", "b062c713-6bcc-48c7-a02b-62db9330a498");
            if (r == 0.0) {
                writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_2_10.coverage", "b4bf9241-e643-48ca-8b92-2165799ee95e");
                throw new ArithmeticException("%");
            }
            writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_2_10.coverage", "70a3cd09-115e-4752-947d-684eb908035a");
            return l % r;
        }
        writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_2_10.coverage", "973d732e-6896-4e68-a9b8-a04311ae8e22");
        BigInteger l = toBigInteger(left);
        writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_2_10.coverage", "636a56c1-45ff-41cc-8092-a2655c9174a2");
        BigInteger r = toBigInteger(right);
        writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_2_10.coverage", "cacabd4f-ac0a-4ee5-8a62-321ee00f632d");
        BigInteger result = l.mod(r);
        writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_2_10.coverage", "e064f253-fd14-4d03-a085-5832902b8696");
        if (BigInteger.ZERO.equals(r)) {
            writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_2_10.coverage", "cc9ea098-d475-415b-a093-af2941a4a571");
            throw new ArithmeticException("%");
        }
        writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_2_10.coverage", "a2761487-f370-459d-b1e7-fa91c221ae20");
        return narrowBigInteger(left, right, result);
    }

    /**
     * Multiply the left value by the right.
     * 
     * @param left  left argument
     * @param right  right argument
     * @return left * right.
     */
    public Object multiply(Object left, Object right) {
        writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_2_10.coverage", "0fcdc556-7507-45b3-96b4-2c1c68119282");
        if (left == null && right == null) {
            writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_2_10.coverage", "b2d09c8c-ca6c-4fe8-af6c-a0bc9d34dcdb");
            return controlNullNullOperands();
        }
        writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_2_10.coverage", "c0ba307d-b69c-41f9-be67-6857dae7461d");
        if (left instanceof BigDecimal || right instanceof BigDecimal) {
            writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_2_10.coverage", "f51810a6-f808-4d26-b0e7-c9c6686970a0");
            BigDecimal l = toBigDecimal(left);
            writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_2_10.coverage", "7bad8322-88fa-44ba-8e53-c469977e5dd4");
            BigDecimal r = toBigDecimal(right);
            writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_2_10.coverage", "95889d86-9b55-41d4-b063-abdd985a3787");
            BigDecimal result = l.multiply(r, getMathContext());
            writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_2_10.coverage", "5a6698f4-e559-4e23-9eac-18276f94012f");
            return narrowBigDecimal(left, right, result);
        }
        writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_2_10.coverage", "b5068a7d-6479-459e-b136-6a9cddd8d95b");
        if (isFloatingPointNumber(left) || isFloatingPointNumber(right)) {
            writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_2_10.coverage", "5ca56972-42b9-48a3-b686-1cb43b448a54");
            double l = toDouble(left);
            writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_2_10.coverage", "ab3d4aba-41a7-4072-927e-0c3eadfdf06b");
            double r = toDouble(right);
            writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_2_10.coverage", "50b20d7a-421a-4c78-8762-ec387bb5be50");
            return l * r;
        }
        writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_2_10.coverage", "635de2ef-3c66-43b4-a49f-2f9383554fc2");
        BigInteger l = toBigInteger(left);
        writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_2_10.coverage", "ef5e40e0-13cf-405c-aa27-7325f2e637ca");
        BigInteger r = toBigInteger(right);
        writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_2_10.coverage", "5cd325f4-0960-4fa5-ba83-157895acb904");
        BigInteger result = l.multiply(r);
        writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_2_10.coverage", "56f21bb3-a933-4df5-9eb9-57172e2f42ae");
        return narrowBigInteger(left, right, result);
    }

    /**
     * Subtract the right value from the left.
     * 
     * @param left  left argument
     * @param right  right argument
     * @return left - right.
     */
    public Object subtract(Object left, Object right) {
        writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_2_10.coverage", "a9924bff-6eb0-4e7f-8baa-bb94ec1eec4b");
        if (left == null && right == null) {
            writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_2_10.coverage", "3c6c8be5-ab8d-4399-ae91-c48dfbc9ecfa");
            return controlNullNullOperands();
        }
        writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_2_10.coverage", "e4d55021-b805-4c5d-831f-f21f8737bd30");
        if (left instanceof BigDecimal || right instanceof BigDecimal) {
            writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_2_10.coverage", "1832e634-e9f3-46f6-9ee3-b5544c82adf3");
            BigDecimal l = toBigDecimal(left);
            writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_2_10.coverage", "37ae0a28-3e1f-45c7-b435-ebb5c8dd5ac7");
            BigDecimal r = toBigDecimal(right);
            writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_2_10.coverage", "8e23206b-8068-44a9-a24e-b7b7c14e5139");
            BigDecimal result = l.subtract(r, getMathContext());
            writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_2_10.coverage", "d292b83f-cc83-4396-91de-25d765e12ccf");
            return narrowBigDecimal(left, right, result);
        }
        writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_2_10.coverage", "6fc85b22-1fb4-45d9-ba2f-35a9260f8440");
        if (isFloatingPointNumber(left) || isFloatingPointNumber(right)) {
            writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_2_10.coverage", "7cf2daa1-426a-49c2-9cef-dc6cae553ca7");
            double l = toDouble(left);
            writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_2_10.coverage", "dc5e9f16-9fbf-4e14-b97f-f5199fdeb3a8");
            double r = toDouble(right);
            writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_2_10.coverage", "5779f691-8c52-429c-9aa2-a51909e265f4");
            return l - r;
        }
        writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_2_10.coverage", "94b62288-6709-4664-a703-324cc89ce261");
        BigInteger l = toBigInteger(left);
        writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_2_10.coverage", "564f7dde-2014-4111-88f7-9ed9fcc844b0");
        BigInteger r = toBigInteger(right);
        writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_2_10.coverage", "9bffd16d-7d6d-467d-937f-0bf36749e0d0");
        BigInteger result = l.subtract(r);
        writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_2_10.coverage", "3baa466a-f408-4b55-b485-768e5662bbd5");
        return narrowBigInteger(left, right, result);
    }

    /**
     * Negates a value (unary minus for numbers).
     * 
     * @param val the value to negate
     * @return the negated value
     */
    public Object negate(Object val) {
        writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_2_10.coverage", "eacc027a-6553-412a-94c6-7a0fbb6c7803");
        if (val instanceof Integer) {
            writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_2_10.coverage", "55ac61c3-68ce-4347-8856-47d93dfab212");
            return -((Integer) val);
        } else if (val instanceof Double) {
            writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_2_10.coverage", "e7171233-4392-4692-869c-e75872071b3f");
            return -((Double) val);
        } else if (val instanceof Long) {
            writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_2_10.coverage", "5aa6873a-d58b-4f39-92a6-aaf9f4751a82");
            return -((Long) val);
        } else if (val instanceof BigDecimal) {
            writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_2_10.coverage", "6fc4d46d-c9c4-46fa-a6d2-86fc1e973e94");
            return ((BigDecimal) val).negate();
        } else if (val instanceof BigInteger) {
            writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_2_10.coverage", "7d68333b-d9d8-454f-a5e4-377d651cffeb");
            return ((BigInteger) val).negate();
        } else if (val instanceof Float) {
            writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_2_10.coverage", "58910b65-49b5-4819-82a1-95fbac7ce9ed");
            return -((Float) val);
        } else if (val instanceof Short) {
            writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_2_10.coverage", "42a08b1a-fb3a-4d7b-ab62-04f17dfb5fb6");
            return (short) -((Short) val);
        } else if (val instanceof Byte) {
            writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_2_10.coverage", "a3fbf4ec-51db-4362-b4af-cf522e23483f");
            return (byte) -((Byte) val);
        } else if (val instanceof Boolean) {
            writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_2_10.coverage", "9adc23a9-dada-4fda-b052-259aabca81f4");
            return ((Boolean) val) ? Boolean.FALSE : Boolean.TRUE;
        }
        writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_2_10.coverage", "3df4810b-e99a-43a0-ba6f-95b7c63035b3");
        throw new ArithmeticException("Object negation:(" + val + ")");
    }

    /**
     * Test if left contains right (right matches/in left).
     * <p>Beware that this method arguments are the opposite of the operator arguments.
     * 'x in y' means 'y contains x'.</p>
     * 
     * @param container the container
     * @param value the value
     * @return test result or null if there is no arithmetic solution
     */
    public Boolean contains(Object container, Object value) {
        writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_2_10.coverage", "89f1842d-8323-4441-8559-c6b975140c74");
        if (value == null && container == null) {
            writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_2_10.coverage", "131fdaa7-14b7-4f3b-82fe-5e4229732f71");
            return true;
        }
        writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_2_10.coverage", "884d9e80-758f-4453-a275-0b779578dd41");
        if (value == null || container == null) {
            writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_2_10.coverage", "941cde6a-0a3d-435d-8242-d232a1ceb515");
            return false;
        }
        writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_2_10.coverage", "32a5117d-350d-4d9b-bbfd-dda1263f2bfa");
        if (container instanceof java.util.regex.Pattern) {
            writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_2_10.coverage", "1a71ff2b-41cf-4029-828f-0e6e9b9a60a0");
            return ((java.util.regex.Pattern) container).matcher(value.toString()).matches();
        }
        writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_2_10.coverage", "d10b2969-acb3-4d89-abd9-28b8826f8b66");
        if (container instanceof String) {
            writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_2_10.coverage", "86078f68-3f78-4b1d-baa1-4166b785edc7");
            return value.toString().matches(container.toString());
        }
        writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_2_10.coverage", "ebb0cd24-a752-499d-a9a5-5a45417dcfeb");
        if (container instanceof Map<?, ?>) {
            writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_2_10.coverage", "f8170588-56c0-4256-8e5f-409059f94fcc");
            if (value instanceof Map<?, ?>) {
                writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_2_10.coverage", "b7139a93-153c-4676-90c6-e04de37ce84b");
                return ((Map<?, ?>) container).keySet().containsAll(((Map<?, ?>) value).keySet());
            }
            writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_2_10.coverage", "3323b17f-5880-43f6-8968-bd509472b631");
            return ((Map<?, ?>) container).containsKey(value);
        }
        writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_2_10.coverage", "ff31690e-6031-4965-80eb-00d99c4f3e90");
        if (container instanceof Collection<?>) {
            writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_2_10.coverage", "76ed0a8e-532d-44b3-b4a6-47859107fee4");
            if (value instanceof Collection<?>) {
                writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_2_10.coverage", "8bed148e-c322-4c7e-a77d-1f5c08b22f63");
                return ((Collection<?>) container).containsAll((Collection<?>) value);
            }
            writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_2_10.coverage", "36af1f79-7395-4d47-abc5-970bac1b3d7c");
            return ((Collection<?>) container).contains(value);
        }
        writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_2_10.coverage", "3ceead61-6191-4c7a-aaae-2a560c94b6eb");
        return null;
    }

    /**
     * Test if left ends with right.
     *
     * @param left  left argument
     * @param right  right argument
     * @return left $= right if there is no arithmetic solution
     */
    public Boolean endsWith(Object left, Object right) {
        writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_2_10.coverage", "35b70340-f523-4dff-ad58-7df2dc207165");
        if (left == null && right == null) {
            writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_2_10.coverage", "622db047-86f4-4890-8514-f31842f92052");
            return true;
        }
        writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_2_10.coverage", "5d30481d-5bc6-4dda-9e4e-fa40145b1a7b");
        if (left == null || right == null) {
            writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_2_10.coverage", "c6e91c91-548d-4f0a-a22e-55941bd85f43");
            return false;
        }
        writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_2_10.coverage", "07ccaba2-ffcb-4b93-b437-f3f5fc84c544");
        if (left instanceof String) {
            writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_2_10.coverage", "e4f803a3-6744-40f6-9496-b84be964c101");
            return ((String) left).endsWith(toString(right));
        }
        writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_2_10.coverage", "f2673bc2-7949-4cf2-8cc9-25ade2c2f780");
        return null;
    }

    /**
     * Test if left starts with right.
     *
     * @param left  left argument
     * @param right  right argument
     * @return left ^= right or null if there is no arithmetic solution
     */
    public Boolean startsWith(Object left, Object right) {
        writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_2_10.coverage", "e58f15d0-7bb6-4f75-98eb-85e7c5547117");
        if (left == null && right == null) {
            writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_2_10.coverage", "79b730f9-5bc8-4df0-90e2-021f2ab738a9");
            return true;
        }
        writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_2_10.coverage", "78643302-42c5-4380-a3c1-cd712e7e82b7");
        if (left == null || right == null) {
            writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_2_10.coverage", "e420457d-f3bb-4457-8060-99658fd6f1a3");
            return false;
        }
        writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_2_10.coverage", "d6488a95-3f7e-411f-acbc-363ccbbda1e2");
        if (left instanceof String) {
            writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_2_10.coverage", "1021abdd-e28e-4b69-ba12-277f2c917911");
            return ((String) left).startsWith(toString(right));
        }
        writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_2_10.coverage", "99cf9cb2-9523-4bfd-8c21-b1b280c5e579");
        return null;
    }

    /**
     * Check for emptyness of various types: Number, Collection, Array, Map, String.
     *
     * @param object the object to check the emptyness of
     * @return the boolean or null of there is no arithmetic solution
     */
    public Boolean isEmpty(Object object) {
        writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_2_10.coverage", "d255bcf3-d512-4263-9b52-befa8d85eb59");
        if (object instanceof Number) {
            writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_2_10.coverage", "3bcac1c4-3fd6-48c2-bd02-20a4c87d1971");
            double d = ((Number) object).doubleValue();
            writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_2_10.coverage", "0b5f5a6f-ce19-46c3-9e63-f16b44af911c");
            return Double.isNaN(d) || d == 0.d ? Boolean.TRUE : Boolean.FALSE;
        }
        writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_2_10.coverage", "fe2eef1a-b0ff-4352-90aa-5b31922ec41c");
        if (object instanceof String) {
            writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_2_10.coverage", "08404dba-9589-45f2-8f3e-5fdb6b5d415d");
            return "".equals(object) ? Boolean.TRUE : Boolean.FALSE;
        }
        writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_2_10.coverage", "c870ebca-7735-449a-b431-4fa1d00785e6");
        if (object.getClass().isArray()) {
            writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_2_10.coverage", "156e0f81-123e-4f20-8c76-9466d659a509");
            return Array.getLength(object) == 0 ? Boolean.TRUE : Boolean.FALSE;
        }
        writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_2_10.coverage", "df54841f-ca16-4d2e-938a-7f31939bde5c");
        if (object instanceof Collection<?>) {
            writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_2_10.coverage", "c6893518-f44a-4719-aa35-9d077e32cace");
            return ((Collection<?>) object).isEmpty() ? Boolean.TRUE : Boolean.FALSE;
        }
        writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_2_10.coverage", "9ca2c0c0-ff2f-4369-b2f9-82b0327087a7");
        if (object instanceof Map<?, ?>) {
            writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_2_10.coverage", "2c5bb87b-b9e0-48be-a125-d0d1ee1b842c");
            return ((Map<?, ?>) object).isEmpty() ? Boolean.TRUE : Boolean.FALSE;
        }
        writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_2_10.coverage", "70068686-5b00-48fa-a032-aeaa6eb692cb");
        return null;
    }

    /**
     * Calculate the <code>size</code> of various types: Collection, Array, Map, String.
     *
     * @param object the object to get the size of
     * @return the size of object or null if there is no arithmetic solution
     */
    public Integer size(Object object) {
        writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_2_10.coverage", "e170976a-b47d-4dad-9f49-dc0dd47d9536");
        if (object instanceof String) {
            writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_2_10.coverage", "28851ab2-6029-48d1-933a-1fbb52c179bb");
            return ((String) object).length();
        }
        writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_2_10.coverage", "4de35a31-45b3-40ab-8703-065652b0f0ed");
        if (object.getClass().isArray()) {
            writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_2_10.coverage", "b646a73a-21ae-4e3c-99ad-d35fe80c2b8b");
            return Array.getLength(object);
        }
        writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_2_10.coverage", "ec538cf3-6512-4441-bef8-e12adef9331f");
        if (object instanceof Collection<?>) {
            writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_2_10.coverage", "e50ddafc-df10-492a-9d1e-9ef43fb35f72");
            return ((Collection<?>) object).size();
        }
        writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_2_10.coverage", "de836076-a2bd-4eb6-9768-9adb92a825a8");
        if (object instanceof Map<?, ?>) {
            writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_2_10.coverage", "c7689094-02d4-44a4-baac-ffbec0410016");
            return ((Map<?, ?>) object).size();
        }
        writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_2_10.coverage", "8cf8d725-abe7-404b-810c-0c19b039edbf");
        return null;
    }

    /**
     * Performs a bitwise and.
     * 
     * @param left  the left operand
     * @param right the right operator
     * @return left &amp; right
     */
    public Object and(Object left, Object right) {
        writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_2_10.coverage", "ea5585b4-6d97-46af-b1c6-fd87070b0150");
        long l = toLong(left);
        writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_2_10.coverage", "270feb3d-d7c3-4c80-a15e-fbab3d9c24ae");
        long r = toLong(right);
        writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_2_10.coverage", "15016a05-1447-4908-9678-155dc026b3f7");
        return l & r;
    }

    /**
     * Performs a bitwise or.
     * 
     * @param left  the left operand
     * @param right the right operator
     * @return left | right
     */
    public Object or(Object left, Object right) {
        writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_2_10.coverage", "0a95aff3-98e5-455a-8344-1f860f1f65dc");
        long l = toLong(left);
        writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_2_10.coverage", "ee8773ac-043f-4843-97f4-f73caa69f91f");
        long r = toLong(right);
        writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_2_10.coverage", "a79ecd24-0ca3-4240-9653-54f531714d97");
        return l | r;
    }

    /**
     * Performs a bitwise xor.
     * 
     * @param left  the left operand
     * @param right the right operator
     * @return left ^ right
     */
    public Object xor(Object left, Object right) {
        writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_2_10.coverage", "3f776ede-6a7b-4af8-9465-5fa3dd5f93a4");
        long l = toLong(left);
        writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_2_10.coverage", "bec4cd89-f260-462e-9676-7ac5252b99fa");
        long r = toLong(right);
        writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_2_10.coverage", "1891525a-1353-4b08-bc1a-9e4a8eb055c6");
        return l ^ r;
    }

    /**
     * Performs a bitwise complement.
     * 
     * @param val the operand
     * @return ~val
     */
    public Object complement(Object val) {
        writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_2_10.coverage", "469738aa-4d12-4521-9d47-a8fa075f7f8a");
        long l = toLong(val);
        writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_2_10.coverage", "28db119a-39f7-44a2-99b1-6da42e59dbbb");
        return ~l;
    }

    /**
     * Performs a logical not.
     * 
     * @param val the operand
     * @return !val
     */
    public Object not(Object val) {
        writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_2_10.coverage", "a42a90e4-1e7b-4fa5-8f5b-3cca1daebbfa");
        return toBoolean(val) ? Boolean.FALSE : Boolean.TRUE;
    }

    /**
     * Performs a comparison.
     * 
     * @param left     the left operand
     * @param right    the right operator
     * @param operator the operator
     * @return -1 if left &lt; right; +1 if left &gt; right; 0 if left == right
     * @throws ArithmeticException if either left or right is null
     */
    protected int compare(Object left, Object right, String operator) {
        writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_2_10.coverage", "3d1d2857-9eb2-405f-a9a9-30abc68630f0");
        if (left != null && right != null) {
            writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_2_10.coverage", "ba412bed-d1a8-4940-97b3-be7b9cf6bdf0");
            if (left instanceof BigDecimal || right instanceof BigDecimal) {
                writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_2_10.coverage", "be74ac18-3433-436e-af45-5ff549aed4d1");
                BigDecimal l = toBigDecimal(left);
                writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_2_10.coverage", "cf70759e-0877-4065-9849-8ce63b11e373");
                BigDecimal r = toBigDecimal(right);
                writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_2_10.coverage", "9ec513a8-d609-4251-b200-6186a0883808");
                return l.compareTo(r);
            } else if (left instanceof BigInteger || right instanceof BigInteger) {
                writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_2_10.coverage", "ff61605e-0110-4971-8f25-256a4d904b7f");
                BigInteger l = toBigInteger(left);
                writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_2_10.coverage", "cc48b061-9a6d-4ebb-bb7b-64e4646bc2bf");
                BigInteger r = toBigInteger(right);
                writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_2_10.coverage", "3310613f-656c-43a1-826e-9d0409027aac");
                return l.compareTo(r);
            } else if (isFloatingPoint(left) || isFloatingPoint(right)) {
                writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_2_10.coverage", "18e6dd17-5486-4ff6-b494-2ab615f74921");
                double lhs = toDouble(left);
                writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_2_10.coverage", "a8904e26-1001-4e86-b7cd-10170591e4d3");
                double rhs = toDouble(right);
                writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_2_10.coverage", "06dcef1f-eea1-468c-a1c1-f838c1e80118");
                if (Double.isNaN(lhs)) {
                    writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_2_10.coverage", "40ddda59-4525-4a7f-8b47-cb1777cbe7d3");
                    if (Double.isNaN(rhs)) {
                        writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_2_10.coverage", "86c191c7-4a50-44cd-a34b-d89e08ac0053");
                        return 0;
                    } else {
                        writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_2_10.coverage", "0e1ee203-8e0a-49ce-9276-e2634579c44d");
                        return -1;
                    }
                } else if (Double.isNaN(rhs)) {
                    writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_2_10.coverage", "f26b6848-544a-4663-833e-e0c5af8838be");
                    return +1;
                } else if (lhs < rhs) {
                    writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_2_10.coverage", "0a7c032d-1c91-42cc-8f09-ed400f7b1901");
                    return -1;
                } else if (lhs > rhs) {
                    writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_2_10.coverage", "80e896de-ecc3-457e-abeb-42144bf9b0d3");
                    return +1;
                } else {
                    writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_2_10.coverage", "a0ded4c8-ac69-47c6-9bcd-755bb657259f");
                    return 0;
                }
            } else if (isNumberable(left) || isNumberable(right)) {
                writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_2_10.coverage", "05928641-1086-47d5-8346-c438236cf747");
                long lhs = toLong(left);
                writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_2_10.coverage", "71ef2c17-9b6a-401c-b2c6-180fd791dfb2");
                long rhs = toLong(right);
                writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_2_10.coverage", "8fe9ddf3-47f6-4132-8383-418f00a9db03");
                if (lhs < rhs) {
                    writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_2_10.coverage", "87237eec-f8e3-4ac7-a912-ed06aa398495");
                    return -1;
                } else if (lhs > rhs) {
                    writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_2_10.coverage", "1b2c9934-2ae3-4afc-b779-3d9707fafe1e");
                    return +1;
                } else {
                    writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_2_10.coverage", "452cfb99-2417-4dbb-b19d-beb6eeb20bf7");
                    return 0;
                }
            } else if (left instanceof String || right instanceof String) {
                writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_2_10.coverage", "1f75a365-1d62-4c60-b00d-c10693829186");
                return toString(left).compareTo(toString(right));
            } else if ("==".equals(operator)) {
                writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_2_10.coverage", "0f1d81e3-1899-4a4f-b86d-97c12a498b49");
                return left.equals(right) ? 0 : -1;
            } else if (left instanceof Comparable<?>) {
                writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_2_10.coverage", "e6552170-80a0-498d-99d0-1c599e3fa2a0");
                @SuppressWarnings("unchecked") final Comparable<Object> comparable = (Comparable<Object>) left;
                writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_2_10.coverage", "ed4e26e8-17b0-43dc-8d0c-dbc4ac827247");
                return comparable.compareTo(right);
            } else if (right instanceof Comparable<?>) {
                writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_2_10.coverage", "69901118-0126-4653-aa58-45b1c959e94a");
                @SuppressWarnings("unchecked") final Comparable<Object> comparable = (Comparable<Object>) right;
                writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_2_10.coverage", "a6635995-56c9-4e4e-9a6e-afea35a3afca");
                return comparable.compareTo(left);
            }
        }
        writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_2_10.coverage", "56264ad4-8ca5-49b6-a657-02a3bcd5db41");
        throw new ArithmeticException("Object comparison:(" + left + " " + operator + " " + right + ")");
    }

    /**
     * Test if left and right are equal.
     *
     * @param left  left argument
     * @param right right argument
     * @return the test result
     */
    public boolean equals(Object left, Object right) {
        writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_2_10.coverage", "cd3a5dcc-578f-4add-a10d-414c6cb01664");
        if (left == right) {
            writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_2_10.coverage", "b5691501-e463-4f87-b159-74a72cdf3816");
            return true;
        } else if (left == null || right == null) {
            writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_2_10.coverage", "aa4087ae-b942-476e-8296-b664d54413e0");
            return false;
        } else if (left instanceof Boolean || right instanceof Boolean) {
            writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_2_10.coverage", "f695680a-e6e4-438f-bbc2-1d77858fbcee");
            return toBoolean(left) == toBoolean(right);
        } else {
            writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_2_10.coverage", "ce792d0d-bc7a-4feb-a8f5-83d7b4c5bb37");
            return compare(left, right, "==") == 0;
        }
    }

    /**
     * Test if left &lt; right.
     *
     * @param left  left argument
     * @param right right argument
     * @return the test result
     */
    public boolean lessThan(Object left, Object right) {
        writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_2_10.coverage", "5657fa3b-c06c-4088-ae01-85230909cb0d");
        if ((left == right) || (left == null) || (right == null)) {
            writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_2_10.coverage", "a624d097-d83f-459a-ab43-ae5f6e90410e");
            return false;
        } else {
            writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_2_10.coverage", "346d23e0-5489-4b8b-9064-681ebd841788");
            return compare(left, right, "<") < 0;
        }
    }

    /**
     * Test if left &gt; right.
     *
     * @param left  left argument
     * @param right right argument
     * @return the test result
     */
    public boolean greaterThan(Object left, Object right) {
        writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_2_10.coverage", "283492b3-644c-4b63-ac43-ec665f05a8a0");
        if ((left == right) || left == null || right == null) {
            writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_2_10.coverage", "d9d6c616-5692-4fe2-849a-59e5e7ac0472");
            return false;
        } else {
            writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_2_10.coverage", "4ba1f487-3657-4e1d-8aa3-96281cb8615d");
            return compare(left, right, ">") > 0;
        }
    }

    /**
     * Test if left &lt;= right.
     *
     * @param left  left argument
     * @param right right argument
     * @return the test result
     */
    public boolean lessThanOrEqual(Object left, Object right) {
        writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_2_10.coverage", "dbb4cca4-db5f-46af-b278-320f78929015");
        if (left == right) {
            writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_2_10.coverage", "ccf76076-dfe0-4b62-8498-1396f4674d98");
            return true;
        } else if (left == null || right == null) {
            writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_2_10.coverage", "fac6f290-589b-48ba-9f5b-ae8bbf073228");
            return false;
        } else {
            writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_2_10.coverage", "013cd872-f2af-4b4f-b7b0-783c87190cc0");
            return compare(left, right, "<=") <= 0;
        }
    }

    /**
     * Test if left &gt;= right.
     *
     * @param left  left argument
     * @param right right argument
     * @return the test result
     */
    public boolean greaterThanOrEqual(Object left, Object right) {
        writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_2_10.coverage", "1893932d-f91a-4b01-897a-7c0eceffec33");
        if (left == right) {
            writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_2_10.coverage", "0bf0651b-81cc-4c7a-857b-d1d185d25097");
            return true;
        } else if (left == null || right == null) {
            writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_2_10.coverage", "a8f0940f-a245-4c37-83ed-9e585abcda65");
            return false;
        } else {
            writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_2_10.coverage", "0da49e30-47fe-4b3c-a47a-5a3ddfae7a7f");
            return compare(left, right, ">=") >= 0;
        }
    }

    /**
     * Coerce to a primitive boolean.
     * <p>Double.NaN, null, "false" and empty string coerce to false.</p>
     *
     * @param val value to coerce
     * @return the boolean value if coercion is possible, true if value was not null.
     */
    public boolean toBoolean(Object val) {
        writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_2_10.coverage", "913f689b-7ddf-45f9-bf9c-ef232ae76d58");
        if (val == null) {
            writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_2_10.coverage", "3a4380bb-cd77-4dac-911a-cb2f9afb9b55");
            controlNullOperand();
            writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_2_10.coverage", "6e291cbb-6601-4b56-9b49-a58f17b29913");
            return false;
        } else if (val instanceof Boolean) {
            writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_2_10.coverage", "1f863c6c-361e-4daa-9386-4c130812215f");
            return ((Boolean) val);
        } else if (val instanceof Number) {
            writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_2_10.coverage", "31209f4c-f609-45c6-bfab-c0e7e4314f70");
            double number = toDouble(val);
            writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_2_10.coverage", "d7e4e0a4-a64c-43ad-97b1-27e329dbd080");
            return !Double.isNaN(number) && number != 0.d;
        } else if (val instanceof String) {
            writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_2_10.coverage", "fc35f8ad-a187-442b-9625-1f858f0e92cb");
            String strval = val.toString();
            writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_2_10.coverage", "f2be9e62-521e-4dc1-beb5-9a7cc7afbfae");
            return strval.length() > 0 && !"false".equals(strval);
        } else {
            writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_2_10.coverage", "84e144bb-4205-452f-8dc9-fe511ebd822a");
            return true;
        }
    }

    /**
     * Coerce to a primitive int.
     * <p>Double.NaN, null and empty string coerce to zero.</p>
     * <p>Boolean false is 0, true is 1.</p>
     *
     * @param val value to coerce
     * @return the value coerced to int
     * @throws ArithmeticException if val is null and mode is strict or if coercion is not possible
     */
    public int toInteger(Object val) {
        writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_2_10.coverage", "b44add39-709d-4d5a-afa8-5b231f176380");
        if (val == null) {
            writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_2_10.coverage", "fabd19ab-ecbf-449a-b8bc-fe3cce6db476");
            controlNullOperand();
            writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_2_10.coverage", "9e9dd042-d903-40f9-8899-aca5d71a6ffa");
            return 0;
        } else if (val instanceof Double) {
            writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_2_10.coverage", "1183cf49-403c-4e13-9c2b-f13b09c9da84");
            Double dval = (Double) val;
            writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_2_10.coverage", "3b7a1efb-13fa-4878-9b9f-9b82f2a6b852");
            if (Double.isNaN(dval)) {
                writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_2_10.coverage", "b2541aeb-de7f-4266-b427-7ea118979aea");
                return 0;
            } else {
                writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_2_10.coverage", "e98dc33a-e398-48ad-b47d-fafd22ef23c1");
                return dval.intValue();
            }
        } else if (val instanceof Number) {
            writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_2_10.coverage", "a15b328f-b24c-483b-a44c-7738f84b39b5");
            return ((Number) val).intValue();
        } else if (val instanceof String) {
            writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_2_10.coverage", "c4bf79db-3bc0-44a9-b03c-8645e6cc1d09");
            if ("".equals(val)) {
                writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_2_10.coverage", "96fbc225-1179-4025-9db3-81d10e74b54d");
                return 0;
            }
            writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_2_10.coverage", "ffcbb933-0f78-4afb-a019-c30c583b541f");
            return Integer.parseInt((String) val);
        } else if (val instanceof Boolean) {
            writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_2_10.coverage", "ea957c20-ddee-47b4-a1d2-892acd7f780b");
            return ((Boolean) val) ? 1 : 0;
        } else if (val instanceof Character) {
            writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_2_10.coverage", "239d0914-2036-4497-951f-3a95bcca89e5");
            return ((Character) val);
        }
        writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_2_10.coverage", "3bda6b18-3a0f-46a4-b78b-c3a298c1ef11");
        throw new ArithmeticException("Integer coercion: " + val.getClass().getName() + ":(" + val + ")");
    }

    /**
     * Coerce to a primitive long.
     * <p>Double.NaN, null and empty string coerce to zero.</p>
     * <p>Boolean false is 0, true is 1.</p>
     *
     * @param val value to coerce
     * @return the value coerced to long
     * @throws ArithmeticException if value is null and mode is strict or if coercion is not possible
     */
    public long toLong(Object val) {
        writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_2_10.coverage", "ed3f1437-2c11-4b4c-8778-5e3573763325");
        if (val == null) {
            writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_2_10.coverage", "36fd0bb7-a039-40c6-8fb8-5f0fa5fed26b");
            controlNullOperand();
            writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_2_10.coverage", "f1a39aa9-c03b-4c8a-b241-d421418cdd37");
            return 0L;
        } else if (val instanceof Double) {
            writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_2_10.coverage", "6264b01c-7d03-402e-a6ea-d669e0e3bfbc");
            Double dval = (Double) val;
            writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_2_10.coverage", "e3760130-8378-4857-bcc2-a793fcc82bc2");
            if (Double.isNaN(dval)) {
                writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_2_10.coverage", "50bee1a2-1360-4f1d-b25d-59586f780814");
                return 0L;
            } else {
                writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_2_10.coverage", "88a889ba-8263-4cd1-943c-45e8e811eda8");
                return dval.longValue();
            }
        } else if (val instanceof Number) {
            writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_2_10.coverage", "741a5b07-55b7-4e88-978e-395844a85759");
            return ((Number) val).longValue();
        } else if (val instanceof String) {
            writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_2_10.coverage", "c58d02e0-4673-4eed-addc-aeeb554120cb");
            if ("".equals(val)) {
                writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_2_10.coverage", "12acd0d7-5f33-4c13-8d46-de25fb8734cb");
                return 0L;
            } else {
                writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_2_10.coverage", "99549b5b-5f17-4410-b1e9-c48920916e1e");
                return Long.parseLong((String) val);
            }
        } else if (val instanceof Boolean) {
            writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_2_10.coverage", "153421d6-aeb8-4577-94a9-e96a335b4212");
            return ((Boolean) val) ? 1L : 0L;
        } else if (val instanceof Character) {
            writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_2_10.coverage", "cadfe6bc-fac9-4127-bc02-1b2db04d069b");
            return ((Character) val);
        }
        writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_2_10.coverage", "cc4236ce-b803-41d5-9348-f13389bfd199");
        throw new ArithmeticException("Long coercion: " + val.getClass().getName() + ":(" + val + ")");
    }

    /**
     * Coerce to a BigInteger.
     * <p>Double.NaN, null and empty string coerce to zero.</p>
     * <p>Boolean false is 0, true is 1.</p>
     *
     * @param val the object to be coerced.
     * @return a BigDecimal
     * @throws ArithmeticException if val is null and mode is strict or if coercion is not possible
     */
    public BigInteger toBigInteger(Object val) {
        writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_2_10.coverage", "d205ad32-1d86-44c4-a342-ad502e5cc0a6");
        if (val == null) {
            writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_2_10.coverage", "261801df-c002-47c8-b000-20cc5e50b936");
            controlNullOperand();
            writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_2_10.coverage", "4fd01d53-b839-4764-b865-51d2eacb7ba4");
            return BigInteger.ZERO;
        } else if (val instanceof BigInteger) {
            writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_2_10.coverage", "3a1b2550-8cb2-4414-9696-3aa087fdc744");
            return (BigInteger) val;
        } else if (val instanceof Double) {
            writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_2_10.coverage", "5a668568-f786-4053-9edf-5fed4a5b925e");
            Double dval = (Double) val;
            writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_2_10.coverage", "35b4b37b-d95f-4eca-bfc3-142574b230fd");
            if (Double.isNaN(dval)) {
                writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_2_10.coverage", "f56008d1-d174-4003-80bc-c024ae6109bf");
                return BigInteger.ZERO;
            } else {
                writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_2_10.coverage", "aaec656b-9c32-48d8-8431-be7c2510544e");
                return BigInteger.valueOf(dval.longValue());
            }
        } else if (val instanceof BigDecimal) {
            writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_2_10.coverage", "02dd755b-2344-4533-896e-dfb567955fdc");
            return ((BigDecimal) val).toBigInteger();
        } else if (val instanceof Number) {
            writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_2_10.coverage", "c0201f2b-36e2-4cdc-b573-5edd05800193");
            return BigInteger.valueOf(((Number) val).longValue());
        } else if (val instanceof Boolean) {
            writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_2_10.coverage", "6625dd11-379a-40c3-9b45-81257c7b5d55");
            return BigInteger.valueOf(((Boolean) val) ? 1L : 0L);
        } else if (val instanceof String) {
            writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_2_10.coverage", "01d46582-814d-4a41-a476-9f27672ccb4f");
            String string = (String) val;
            writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_2_10.coverage", "1c19b418-b18f-49c3-8800-fde80eb9170f");
            if ("".equals(string)) {
                writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_2_10.coverage", "dd456bc9-081f-4972-a04a-d30b93c73539");
                return BigInteger.ZERO;
            } else {
                writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_2_10.coverage", "ea2ed43c-b450-466a-a04d-fbdc55651787");
                return new BigInteger(string);
            }
        } else if (val instanceof Character) {
            writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_2_10.coverage", "fc4912ca-1083-469f-8914-a74bd6fca6d6");
            int i = ((Character) val);
            writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_2_10.coverage", "19fe2e76-5374-4153-b7fb-b699d5cffdd5");
            return BigInteger.valueOf(i);
        }
        writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_2_10.coverage", "a63652c2-d25d-44a2-9859-5316483af0cf");
        throw new ArithmeticException("BigInteger coercion: " + val.getClass().getName() + ":(" + val + ")");
    }

    /**
     * Coerce to a BigDecimal.
     * <p>Double.NaN, null and empty string coerce to zero.</p>
     * <p>Boolean false is 0, true is 1.</p>
     *
     * @param val the object to be coerced.
     * @return a BigDecimal.
     * @throws ArithmeticException if val is null and mode is strict or if coercion is not possible
     */
    public BigDecimal toBigDecimal(Object val) {
        writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_2_10.coverage", "39f4befb-e6ce-4c33-a4b6-26d2b2b6905e");
        if (val instanceof BigDecimal) {
            writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_2_10.coverage", "b16d467c-2c9d-43f2-950f-b4f13f8b1141");
            return roundBigDecimal((BigDecimal) val);
        } else if (val == null) {
            writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_2_10.coverage", "01e6f869-16d3-46f1-919a-384fbdf69010");
            controlNullOperand();
            writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_2_10.coverage", "40f222e5-6c16-4b30-922c-2133abbc3f10");
            return BigDecimal.ZERO;
        } else if (val instanceof Double) {
            writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_2_10.coverage", "e3319aaf-2372-41ce-a232-5d67f05a5cb5");
            if (Double.isNaN(((Double) val))) {
                writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_2_10.coverage", "c70a71bf-000c-42a5-8c8f-f3c5f3e1b328");
                return BigDecimal.ZERO;
            } else {
                writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_2_10.coverage", "b583848e-55c6-4094-be87-01fe10330744");
                return roundBigDecimal(new BigDecimal(val.toString(), getMathContext()));
            }
        } else if (val instanceof Number) {
            writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_2_10.coverage", "faf48855-3c9c-4289-b459-50589611ec1a");
            return roundBigDecimal(new BigDecimal(val.toString(), getMathContext()));
        } else if (val instanceof Boolean) {
            writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_2_10.coverage", "8c3b308e-e1c2-497c-b9b2-4b7c553db310");
            return BigDecimal.valueOf(((Boolean) val) ? 1. : 0.);
        } else if (val instanceof String) {
            writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_2_10.coverage", "488ab449-4d5e-4b33-b6b9-3e5fd3b1d445");
            String string = (String) val;
            writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_2_10.coverage", "ad9d6d7d-5aef-43b8-833b-8d03881beff4");
            if ("".equals(string)) {
                writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_2_10.coverage", "5ae4116a-a846-47ac-82f2-b10a977c135c");
                return BigDecimal.ZERO;
            }
            writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_2_10.coverage", "75320853-1626-4ad5-9962-17d0d375d820");
            return roundBigDecimal(new BigDecimal(string, getMathContext()));
        } else if (val instanceof Character) {
            writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_2_10.coverage", "7de02bea-1b5b-4eeb-94ef-56f1e9f6c47c");
            int i = ((Character) val);
            writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_2_10.coverage", "6ce309c6-7424-491e-833f-39b605fc188d");
            return new BigDecimal(i);
        }
        writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_2_10.coverage", "4f948aa7-b7cd-4816-bd8a-1c911e2f2628");
        throw new ArithmeticException("BigDecimal coercion: " + val.getClass().getName() + ":(" + val + ")");
    }

    /**
     * Coerce to a primitive double.
     * <p>Double.NaN, null and empty string coerce to zero.</p>
     * <p>Boolean false is 0, true is 1.</p>
     * 
     * @param val value to coerce.
     * @return The double coerced value.
     * @throws ArithmeticException if val is null and mode is strict or if coercion is not possible
     */
    public double toDouble(Object val) {
        writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_2_10.coverage", "c796a8cc-6eeb-4e5c-a75f-ae4b3092b3c6");
        if (val == null) {
            writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_2_10.coverage", "eefc1409-7f8c-42ef-ab8a-84525f2cd02d");
            controlNullOperand();
            writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_2_10.coverage", "6dcc025a-8285-4bbb-93ae-9107d5a2e4fb");
            return 0;
        } else if (val instanceof Double) {
            writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_2_10.coverage", "ed021673-0074-45bf-89ef-600c173e48bf");
            return ((Double) val);
        } else if (val instanceof Number) {
            writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_2_10.coverage", "aacf8dd7-c960-4c32-89fd-d92f3144754b");
            return Double.parseDouble(String.valueOf(val));
        } else if (val instanceof Boolean) {
            writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_2_10.coverage", "45b0af03-edc5-4112-91ce-fd1eaf9cb795");
            return ((Boolean) val) ? 1. : 0.;
        } else if (val instanceof String) {
            writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_2_10.coverage", "1dac5a90-4469-4694-bd40-b8b2731d1eef");
            String string = (String) val;
            writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_2_10.coverage", "3bcd91a8-d754-453d-8a52-4053e77e2a4d");
            if ("".equals(string)) {
                writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_2_10.coverage", "0a0d2266-8907-4009-b6e2-426d907e9feb");
                return Double.NaN;
            } else {
                writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_2_10.coverage", "f9b83546-6f24-482e-a2d5-37b2c55ac628");
                return Double.parseDouble(string);
            }
        } else if (val instanceof Character) {
            writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_2_10.coverage", "40f3217c-22ea-4657-9a4e-4a90f96a6549");
            int i = ((Character) val);
            writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_2_10.coverage", "f8d20a69-5ac5-4902-8ae0-592729dc262c");
            return i;
        }
        writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_2_10.coverage", "28f6e8e9-80b7-4aa1-9266-86395565d554");
        throw new ArithmeticException("Double coercion: " + val.getClass().getName() + ":(" + val + ")");
    }

    /**
     * Coerce to a string.
     * <p>Double.NaN coerce to the empty string.</p>
     *
     * @param val value to coerce.
     * @return The String coerced value.
     * @throws ArithmeticException if val is null and mode is strict or if coercion is not possible
     */
    public String toString(Object val) {
        writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_2_10.coverage", "35582550-a3e9-4bfd-9c42-24eba1e41c59");
        if (val == null) {
            writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_2_10.coverage", "194720b6-d340-4614-be0e-2fc24c25ab65");
            controlNullOperand();
            writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_2_10.coverage", "b87da0c3-0da3-4594-9e44-02a9c6c4cace");
            return "";
        } else if (val instanceof Double) {
            writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_2_10.coverage", "86e561e5-16ef-45cc-b984-a67320b4a87a");
            Double dval = (Double) val;
            writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_2_10.coverage", "3a0f4117-833d-4e84-8b08-82e18a8a36b8");
            if (Double.isNaN(dval)) {
                writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_2_10.coverage", "8c0a218b-d271-4fe6-81a4-91aea898fdd0");
                return "";
            } else {
                writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_2_10.coverage", "1ac9a790-efda-48ae-8816-89af4c6b71b8");
                return dval.toString();
            }
        } else {
            writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_2_10.coverage", "71272a62-97f2-4701-b3eb-ff151e08c1d2");
            return val.toString();
        }
    }

    /**
     * Use or overload and() instead.
     * @param lhs left hand side
     * @param rhs right hand side
     * @return lhs &amp; rhs
     * @see JexlArithmetic#and
     * @deprecated
     */
    @Deprecated
    public final Object bitwiseAnd(Object lhs, Object rhs) {
        writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_2_10.coverage", "7856d6e1-e454-4ad1-96cb-9df79d41f709");
        return and(lhs, rhs);
    }

    /**
     * Use or overload or() instead.
     * 
     * @param lhs left hand side
     * @param rhs right hand side
     * @return lhs | rhs
     * @see JexlArithmetic#or
     * @deprecated
     */
    @Deprecated
    public final Object bitwiseOr(Object lhs, Object rhs) {
        writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_2_10.coverage", "75ae1629-b57b-4cfb-be39-9e37badc8a06");
        return or(lhs, rhs);
    }

    /**
     * Use or overload xor() instead.
     * 
     * @param lhs left hand side
     * @param rhs right hand side
     * @return lhs ^ rhs
     * @see JexlArithmetic#xor
     * @deprecated
     */
    @Deprecated
    public final Object bitwiseXor(Object lhs, Object rhs) {
        writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_2_10.coverage", "a35a4ebd-eab3-4433-b8d8-416b260bc965");
        return xor(lhs, rhs);
    }

    /**
     * Use or overload not() instead.
     * 
     * @param arg argument
     * @return !arg
     * @see JexlArithmetic#not
     * @deprecated
     */
    @Deprecated
    public final Object logicalNot(Object arg) {
        writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_2_10.coverage", "7a1acff3-e3ec-4dd5-98cf-d0b325a40d49");
        return not(arg);
    }

    /**
     * Use or overload contains() instead.
     * 
     * @param lhs left hand side
     * @param rhs right hand side
     * @return contains(rhs, lhs)
     * @see JexlArithmetic#contains
     * @deprecated
     */
    @Deprecated
    public final Object matches(Object lhs, Object rhs) {
        writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_2_10.coverage", "7218b3de-07d8-4269-bb26-30776e9759f9");
        return contains(rhs, lhs);
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
