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
        writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_5_10.coverage", "ce5f1bb6-ebfb-4210-b5d9-cbd04b07c5c6");
        boolean ostrict = options.isStrictArithmetic() == null ? this.strict : options.isStrictArithmetic();
        writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_5_10.coverage", "fff3ba60-8231-4fb4-8ff2-38f51e2fc08a");
        MathContext bigdContext = options.getArithmeticMathContext();
        writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_5_10.coverage", "5652245d-81ee-4a89-b05a-863f832357f8");
        if (bigdContext == null) {
            writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_5_10.coverage", "d652981f-cb75-48be-a870-7164e52b0130");
            bigdContext = mathContext;
        }
        writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_5_10.coverage", "615ecba1-7aa8-4b2f-87e8-b80c4d45d353");
        int bigdScale = options.getArithmeticMathScale();
        writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_5_10.coverage", "e732b775-ba6c-4f3a-9c3e-1b8639f158d0");
        if (bigdScale == Integer.MIN_VALUE) {
            writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_5_10.coverage", "c86650cb-488c-4e34-b5ec-311e92a4d3f1");
            bigdScale = mathScale;
        }
        writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_5_10.coverage", "ddac43bd-5eed-481e-9bef-2de43a82075b");
        if ((ostrict != this.strict) || bigdScale != this.mathScale || bigdContext != this.mathContext) {
            writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_5_10.coverage", "834c734e-1f36-44c9-92c4-d0d8a3307070");
            return new JexlArithmetic(ostrict, bigdContext, bigdScale);
        } else {
            writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_5_10.coverage", "bc097fd2-5bdd-4fed-ab3e-d25b5801c808");
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
        writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_5_10.coverage", "af92403f-5263-4a1e-8b7b-afebc796f140");
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
        writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_5_10.coverage", "88b093b7-d6cf-40ca-a038-6daca53c322d");
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
        writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_5_10.coverage", "db149673-c1bf-4ccf-93be-f5acbd2349bf");
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
        writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_5_10.coverage", "6af59931-e292-4fb7-902b-9031449d5d92");
        final long lfrom = toLong(from);
        writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_5_10.coverage", "e4cf5daf-f909-49b4-b223-3e3a443802a4");
        final long lto = toLong(to);
        writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_5_10.coverage", "bd61339b-0842-4a2e-96a4-5582f55515a9");
        if ((lfrom >= Integer.MIN_VALUE && lfrom <= Integer.MAX_VALUE) && (lto >= Integer.MIN_VALUE && lto <= Integer.MAX_VALUE)) {
            writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_5_10.coverage", "7913bfaa-5902-4d60-a093-ae2a03c13df0");
            return org.apache.commons.jexl3.internal.IntegerRange.create((int) lfrom, (int) lto);
        } else {
            writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_5_10.coverage", "94a02cdf-5144-418d-82ad-f4378218f85e");
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
        writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_5_10.coverage", "61e2bbad-bc29-4604-84a6-2c4c01388268");
        return this.strict;
    }

    /**
     * The MathContext instance used for +,-,/,*,% operations on big decimals.
     * 
     * @return the math context
     */
    public MathContext getMathContext() {
        writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_5_10.coverage", "83ac7809-3a0e-43b3-a9c8-5c68c24eb6b5");
        return mathContext;
    }

    /**
     * The BigDecimal scale used for comparison and coericion operations.
     * 
     * @return the scale
     */
    public int getMathScale() {
        writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_5_10.coverage", "4da3090e-a67c-44d7-9fd6-ffc93d96f76a");
        return mathScale;
    }

    /**
     * Ensure a big decimal is rounded by this arithmetic scale and rounding mode.
     * 
     * @param number the big decimal to round
     * @return the rounded big decimal
     */
    protected BigDecimal roundBigDecimal(final BigDecimal number) {
        writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_5_10.coverage", "f46393c8-09bc-4bb4-b799-78bc3dbcb40c");
        int mscale = getMathScale();
        writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_5_10.coverage", "c88ccb29-d0ca-4b8e-a877-7e2ea59f5982");
        if (mscale >= 0) {
            writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_5_10.coverage", "df4b2f42-bd11-46ab-8a8a-68d6e1bc813a");
            return number.setScale(mscale, getMathContext().getRoundingMode());
        } else {
            writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_5_10.coverage", "d499b532-1bd8-4c34-a517-cd6af94a642d");
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
        writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_5_10.coverage", "814c9329-be8e-42b3-9209-8036ae70f69c");
        if (isStrict()) {
            writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_5_10.coverage", "5e15db75-3f56-46a0-a525-e3dad2ecd6b9");
            throw new NullOperand();
        }
        writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_5_10.coverage", "87f6ea14-b071-49c1-ab06-8d123ea7fdd9");
        return 0;
    }

    /**
     * Throw a NPE if arithmetic is strict.
     * 
     * @throws ArithmeticException if strict
     */
    protected void controlNullOperand() {
        writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_5_10.coverage", "0f328b01-ac36-4883-ad42-1b64495ae27a");
        if (isStrict()) {
            writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_5_10.coverage", "6386a8d8-c978-414e-b411-9c73c5d90bc3");
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
        writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_5_10.coverage", "4982fa7a-38ce-4c33-885b-ec815405d3d1");
        if (val instanceof Float || val instanceof Double) {
            writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_5_10.coverage", "4c80c74a-550f-4808-9c5c-c9a5c94716c8");
            return true;
        }
        writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_5_10.coverage", "1f75401c-d94f-4944-91b5-459d7d0ca40d");
        if (val instanceof String) {
            writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_5_10.coverage", "58b726b6-39a8-4ee0-840b-2aacbdac17a4");
            String str = (String) val;
            writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_5_10.coverage", "5aa82ba3-dea6-463e-ad97-fbb2ae82dfd6");
            for (int c = 0; c < str.length(); ++c) {
                writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_5_10.coverage", "9e0d6f09-ffd2-40aa-949c-2ec706f19e7b");
                char ch = str.charAt(c);
                writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_5_10.coverage", "0f76e593-66c2-4cac-be45-39c13aa30964");
                if (ch == '.' || ch == 'E' || ch == 'e') {
                    writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_5_10.coverage", "6627d9e3-5c58-431e-b385-c046627c3c48");
                    return FLOAT_PATTERN.matcher(str).matches();
                }
                writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_5_10.coverage", "84be1644-5db3-45e4-b8e3-0c61e9b33ca1");
                if (ch != '+' && ch != '-' && ch < '0' && ch > '9') {
                    writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_5_10.coverage", "2695320f-1f5a-46e3-98ad-e5753cb2dec2");
                    break;
                }
            }
        }
        writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_5_10.coverage", "c979f2b0-4a10-4483-9a60-a80e36f6382b");
        return false;
    }

    /**
     * Is Object a floating point number.
     *
     * @param o Object to be analyzed.
     * @return true if it is a Float or a Double.
     */
    protected boolean isFloatingPoint(final Object o) {
        writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_5_10.coverage", "b1c05ed9-ebd8-4035-8d8f-f6c7a6c9beba");
        return o instanceof Float || o instanceof Double;
    }

    /**
     * Is Object a whole number.
     *
     * @param o Object to be analyzed.
     * @return true if Integer, Long, Byte, Short or Character.
     */
    protected boolean isNumberable(final Object o) {
        writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_5_10.coverage", "b1eba4bc-cf20-4813-9161-8757db6d6c3f");
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
        writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_5_10.coverage", "8f6cd262-ad26-4d02-9b0c-94b0bb9e0672");
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
        writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_5_10.coverage", "af41bc39-0a1b-4aa6-9e0f-2972ff5dbbed");
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
        writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_5_10.coverage", "b7cff20a-a42f-4a68-ad02-abfad2c61325");
        if (original == null) {
            writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_5_10.coverage", "b7411f9b-bd74-47e5-a712-7c7ecdce50c9");
            return null;
        }
        writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_5_10.coverage", "49401225-80eb-4dd4-a01b-1f2802df7315");
        Number result = original;
        writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_5_10.coverage", "20904fb8-60b6-41e2-84f9-cd5e712d92cd");
        if (original instanceof BigDecimal) {
            writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_5_10.coverage", "31443c55-abd1-4917-8bd4-87a8b307968a");
            BigDecimal bigd = (BigDecimal) original;
            writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_5_10.coverage", "40b7b0be-bff6-4e40-a9f0-efb977597212");
            if (bigd.compareTo(BIGD_DOUBLE_MAX_VALUE) > 0 || bigd.compareTo(BIGD_DOUBLE_MIN_VALUE) < 0) {
                writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_5_10.coverage", "79bbdca5-de6a-47f7-a0da-678226e5c520");
                return original;
            } else {
                writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_5_10.coverage", "ee222682-e14f-46c6-9f15-f94438fe0594");
                try {
                    writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_5_10.coverage", "bb8c825e-6827-4790-8c1b-0fe4465f6db8");
                    long l = bigd.longValueExact();
                    writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_5_10.coverage", "2053cf96-30fd-41df-b331-832bba73f9fe");
                    if (narrowAccept(narrow, Integer.class) && l <= Integer.MAX_VALUE && l >= Integer.MIN_VALUE) {
                        writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_5_10.coverage", "744e3140-4760-4789-b74a-43acb6580a25");
                        return (int) l;
                    } else if (narrowAccept(narrow, Long.class)) {
                        writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_5_10.coverage", "018b09b4-3037-4ff5-9509-602b48175eed");
                        return l;
                    }
                } catch (ArithmeticException xa) {
                }
            }
        }
        writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_5_10.coverage", "17744417-f7db-4468-81ba-74f4ef96f1dd");
        if (original instanceof Double || original instanceof Float) {
            writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_5_10.coverage", "d33cb079-4aa9-4643-b92f-cf9c2bd4b6fb");
            double value = original.doubleValue();
            writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_5_10.coverage", "c2f98fdd-a079-404d-a775-08d3bb40a72d");
            if (narrowAccept(narrow, Float.class) && value <= Float.MAX_VALUE && value >= Float.MIN_VALUE) {
                writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_5_10.coverage", "5fa237cb-64fc-4aab-804a-a56bb2e97223");
                result = result.floatValue();
            }
        } else {
            writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_5_10.coverage", "ccff418c-16b0-4f62-bf3c-31776bf8fe9e");
            if (original instanceof BigInteger) {
                writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_5_10.coverage", "88fc42f9-2339-4573-b040-f57f9e3f1de2");
                BigInteger bigi = (BigInteger) original;
                writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_5_10.coverage", "1907e3a9-a415-4103-9341-197db4db254a");
                if (bigi.compareTo(BIGI_LONG_MAX_VALUE) > 0 || bigi.compareTo(BIGI_LONG_MIN_VALUE) < 0) {
                    writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_5_10.coverage", "0e89e08e-0c26-4740-a561-3e05af3cee61");
                    return original;
                }
            }
            writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_5_10.coverage", "e102f436-0d3d-4dd7-9168-dd26e9c7ad7b");
            long value = original.longValue();
            writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_5_10.coverage", "a68597a0-a403-443d-96e1-299f0c5c40dc");
            if (narrowAccept(narrow, Byte.class) && value <= Byte.MAX_VALUE && value >= Byte.MIN_VALUE) {
                writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_5_10.coverage", "c772cf78-2d9d-4a02-87f6-ae3cb162fac3");
                result = (byte) value;
            } else if (narrowAccept(narrow, Short.class) && value <= Short.MAX_VALUE && value >= Short.MIN_VALUE) {
                writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_5_10.coverage", "14824dee-75eb-4c80-a0b1-e6f4bf492126");
                result = (short) value;
            } else if (narrowAccept(narrow, Integer.class) && value <= Integer.MAX_VALUE && value >= Integer.MIN_VALUE) {
                writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_5_10.coverage", "3f157ce0-aa52-4639-8692-17292d76e5ab");
                result = (int) value;
            }
        }
        writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_5_10.coverage", "7d6b3cd8-4481-484e-a2bb-66e026e65ee0");
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
        writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_5_10.coverage", "68ef4f19-7b20-4854-918a-776e0f80970f");
        if (!(lhs instanceof BigInteger || rhs instanceof BigInteger) && bigi.compareTo(BIGI_LONG_MAX_VALUE) <= 0 && bigi.compareTo(BIGI_LONG_MIN_VALUE) >= 0) {
            writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_5_10.coverage", "5cd0e3a8-da3d-40c4-ad71-e2087a7fe2be");
            long l = bigi.longValue();
            writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_5_10.coverage", "adc72c51-812a-4160-907d-a180ae11064c");
            if (!(lhs instanceof Long || rhs instanceof Long) && l <= Integer.MAX_VALUE && l >= Integer.MIN_VALUE) {
                writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_5_10.coverage", "d784c8b4-8545-4ce1-b292-173fbb12483d");
                return (int) l;
            }
            writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_5_10.coverage", "718e4136-d170-46bd-ae66-87a26f0d4153");
            return l;
        }
        writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_5_10.coverage", "2a97eac5-faa5-4210-800d-97ba873720ee");
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
        writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_5_10.coverage", "119bafc0-4f6f-48c0-960c-f416d7d3797b");
        if (isNumberable(lhs) || isNumberable(rhs)) {
            writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_5_10.coverage", "9c1003eb-9ca6-4f4e-901c-9c1ff19f7db6");
            try {
                writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_5_10.coverage", "91fdbae7-098e-45f9-8379-869d38746ba8");
                long l = bigd.longValueExact();
                writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_5_10.coverage", "99cf2c02-3b4d-443b-a424-671f6812661a");
                if (l <= Integer.MAX_VALUE && l >= Integer.MIN_VALUE) {
                    writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_5_10.coverage", "823d0f7b-5139-4afc-a817-ba9491463956");
                    return (int) l;
                } else {
                    writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_5_10.coverage", "cb6dbbe3-9829-4cc6-b6e4-77224681a85e");
                    return l;
                }
            } catch (ArithmeticException xa) {
            }
        }
        writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_5_10.coverage", "356f1b4f-06f6-415b-8d19-3549e1ec1d37");
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
        writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_5_10.coverage", "08f0b624-8bc5-48aa-85d7-e9a0e86924ac");
        boolean narrowed = false;
        writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_5_10.coverage", "b620ad57-7f76-40da-a4cd-f119392818d8");
        for (int a = 0; a < args.length; ++a) {
            writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_5_10.coverage", "f28e9d37-1dc3-4b0d-acd9-2de1fc5e418b");
            Object arg = args[a];
            writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_5_10.coverage", "d7523880-4640-4aa7-826e-4860c8c926b3");
            if (arg instanceof Number) {
                writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_5_10.coverage", "5ea784dc-3f7c-46b9-8644-fa2f8e55ebdc");
                Number narg = (Number) arg;
                writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_5_10.coverage", "9561d74e-88fd-4368-a97d-20f83a5843f8");
                Number narrow = narrow(narg);
                writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_5_10.coverage", "81fc2991-44b7-49a8-9cde-75d4c9e0deaa");
                if (!narg.equals(narrow)) {
                    writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_5_10.coverage", "c7ca5833-1d07-4e4b-920f-4371393f7bb3");
                    args[a] = narrow;
                    writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_5_10.coverage", "1169df90-ece0-4753-aa70-f69024c553b5");
                    narrowed = true;
                }
            }
        }
        writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_5_10.coverage", "80eafcfb-6816-44ff-905e-334d23f322e8");
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
        writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_5_10.coverage", "8911d635-33d2-418c-aa8f-d0a1f3468218");
        if (left == null && right == null) {
            writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_5_10.coverage", "05b9f2a2-150e-4689-a577-aff6570d7d21");
            return controlNullNullOperands();
        }
        writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_5_10.coverage", "21f630e6-d86a-4a86-bb21-36c5779b21c5");
        boolean strconcat = strict ? left instanceof String || right instanceof String : left instanceof String && right instanceof String;
        writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_5_10.coverage", "8bc3c73e-347a-47ab-a5e9-a61209cf2c79");
        if (!strconcat) {
            writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_5_10.coverage", "3bed96a6-3024-4200-817f-afc5b3e23e3c");
            try {
                writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_5_10.coverage", "42ec068f-e753-4396-a00e-88e11de16ca2");
                if (left instanceof BigDecimal || right instanceof BigDecimal) {
                    writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_5_10.coverage", "1f2b8eab-406a-4e7b-a730-9f8b4ba4c461");
                    BigDecimal l = toBigDecimal(left);
                    writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_5_10.coverage", "9b348c40-5600-43b1-bc4a-7467aa0e877a");
                    BigDecimal r = toBigDecimal(right);
                    writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_5_10.coverage", "4a8bead4-0d1e-401e-841b-b5c5cf8319a7");
                    BigDecimal result = l.add(r, getMathContext());
                    writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_5_10.coverage", "e4ac2c74-334d-47b6-b4ee-1bed8e49e9c2");
                    return narrowBigDecimal(left, right, result);
                }
                writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_5_10.coverage", "b6d77517-c365-4213-9f5a-0e8533816ef9");
                if (isFloatingPointNumber(left) || isFloatingPointNumber(right)) {
                    writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_5_10.coverage", "c5894cb1-e1eb-4c83-adbe-e5b7c9a2b10d");
                    double l = toDouble(left);
                    writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_5_10.coverage", "47066b40-1a2c-45e0-a78f-67c6b451ff04");
                    double r = toDouble(right);
                    writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_5_10.coverage", "4f828d97-2ac9-4558-a565-ea873589d06d");
                    return l + r;
                }
                writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_5_10.coverage", "b3678103-88ad-4f0b-9f9c-b86e38311524");
                BigInteger l = toBigInteger(left);
                writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_5_10.coverage", "b771ded6-b8d1-4074-8c82-a1921562b8a9");
                BigInteger r = toBigInteger(right);
                writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_5_10.coverage", "292b5aa9-4119-4b86-b2be-62ca79b80008");
                BigInteger result = l.add(r);
                writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_5_10.coverage", "1e539697-879a-41cd-8fc8-abb092b7046a");
                return narrowBigInteger(left, right, result);
            } catch (java.lang.NumberFormatException nfe) {
                writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_5_10.coverage", "50a34301-97a4-4c33-92ab-7001260ac976");
                if (left == null || right == null) {
                    writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_5_10.coverage", "8967df6f-97ab-45ac-ab19-568e67f2b758");
                    controlNullOperand();
                }
            }
        }
        writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_5_10.coverage", "b25032c1-313c-46c3-ae5f-86c9af9e0d05");
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
        writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_5_10.coverage", "b5c3895b-d248-4a6f-99a8-e9e495472078");
        if (left == null && right == null) {
            writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_5_10.coverage", "c01826bc-a973-4e9d-a609-d04b8ffced45");
            return controlNullNullOperands();
        }
        writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_5_10.coverage", "3f61531c-b43b-4a39-8c64-a532a537d1aa");
        if (left instanceof BigDecimal || right instanceof BigDecimal) {
            writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_5_10.coverage", "d7e1d78d-1cb3-4384-9530-d729949d46c9");
            BigDecimal l = toBigDecimal(left);
            writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_5_10.coverage", "39b9d887-989e-44c7-8f5b-0ec09740baf8");
            BigDecimal r = toBigDecimal(right);
            writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_5_10.coverage", "0d2451fb-cf3b-46f1-b371-bfea7bb0ff0f");
            if (BigDecimal.ZERO.equals(r)) {
                writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_5_10.coverage", "c4699cbf-bed7-40df-8984-cd7ccb77f92b");
                throw new ArithmeticException("/");
            }
            writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_5_10.coverage", "442d5a20-893d-420c-9cf7-0f29d496ec39");
            BigDecimal result = l.divide(r, getMathContext());
            writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_5_10.coverage", "198b68ca-478b-4b98-ab89-dce3cc7cef89");
            return narrowBigDecimal(left, right, result);
        }
        writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_5_10.coverage", "836186ce-e579-41cc-9db1-eac14f9659c9");
        if (isFloatingPointNumber(left) || isFloatingPointNumber(right)) {
            writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_5_10.coverage", "1d8ed519-5a91-414d-bb72-adc725b0981d");
            double l = toDouble(left);
            writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_5_10.coverage", "48fcb568-2a8b-49c7-8340-f270cee9c2bc");
            double r = toDouble(right);
            writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_5_10.coverage", "59a0a814-9552-4a19-bd85-cd325e8e2e3f");
            if (r == 0.0) {
                writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_5_10.coverage", "3dd8e147-ca4a-4574-b2f0-8ac2d87d2649");
                throw new ArithmeticException("/");
            }
            writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_5_10.coverage", "09b23c6f-67d1-41af-b030-98403916378b");
            return l / r;
        }
        writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_5_10.coverage", "1a4cdb80-4383-4a4e-be5f-28a258cc77bf");
        BigInteger l = toBigInteger(left);
        writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_5_10.coverage", "010041d9-5136-44e2-afc8-40e5079e0c39");
        BigInteger r = toBigInteger(right);
        writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_5_10.coverage", "7d224cb9-09f7-4cb0-839a-3bfd917fb3c4");
        if (BigInteger.ZERO.equals(r)) {
            writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_5_10.coverage", "70fe60f0-a72b-4b89-a6aa-966f6d27fb7a");
            throw new ArithmeticException("/");
        }
        writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_5_10.coverage", "7989ef09-e433-4fed-9191-5ce5bf5f9489");
        BigInteger result = l.divide(r);
        writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_5_10.coverage", "d13c8627-e917-45fd-9c3b-e42b8753c074");
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
        writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_5_10.coverage", "2bb99abc-b9ac-463e-ba70-6133e1de0619");
        if (left == null && right == null) {
            writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_5_10.coverage", "9f6b1eda-424f-47bf-8821-03387bfba901");
            return controlNullNullOperands();
        }
        writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_5_10.coverage", "cc015718-a457-4dda-96f2-eccb4aec0d1d");
        if (left instanceof BigDecimal || right instanceof BigDecimal) {
            writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_5_10.coverage", "5982f823-5ca9-4813-a428-200fff849bb4");
            BigDecimal l = toBigDecimal(left);
            writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_5_10.coverage", "74bad0cb-8c39-4579-b219-42fb67faf0ef");
            BigDecimal r = toBigDecimal(right);
            writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_5_10.coverage", "846a8228-4390-4fbe-8302-eb0c91791673");
            if (BigDecimal.ZERO.equals(r)) {
                writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_5_10.coverage", "13e4e4dd-93b0-45ca-848d-310330cd71e2");
                throw new ArithmeticException("%");
            }
            writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_5_10.coverage", "85df6530-0145-4328-9a63-b5108621742d");
            BigDecimal remainder = l.remainder(r, getMathContext());
            writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_5_10.coverage", "474c5ec4-92a4-4aa1-80cf-24edb2f88616");
            return narrowBigDecimal(left, right, remainder);
        }
        writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_5_10.coverage", "b328027e-2d60-4034-bc29-ed242ce40475");
        if (isFloatingPointNumber(left) || isFloatingPointNumber(right)) {
            writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_5_10.coverage", "ff442ae8-e9e9-4422-b31d-7989bedb18bd");
            double l = toDouble(left);
            writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_5_10.coverage", "75357649-b96b-4379-8dba-cbf8e9691e96");
            double r = toDouble(right);
            writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_5_10.coverage", "56e5df41-f0fd-4bfb-b90d-be3ff22ddf08");
            if (r == 0.0) {
                writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_5_10.coverage", "2ba263a2-d38e-4990-b749-ef5fc61007b2");
                throw new ArithmeticException("%");
            }
            writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_5_10.coverage", "86c32df3-0f3f-4ec2-953d-0e558c67b359");
            return l % r;
        }
        writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_5_10.coverage", "589c316c-11e0-4a4b-915b-0eff9b67fa31");
        BigInteger l = toBigInteger(left);
        writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_5_10.coverage", "df452cbf-8be3-4947-942a-bc82bb169b3c");
        BigInteger r = toBigInteger(right);
        writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_5_10.coverage", "2b968af4-9e42-48fd-a414-eae0304ba09b");
        BigInteger result = l.mod(r);
        writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_5_10.coverage", "06a87b2e-7235-4ad7-9782-c0198b14fdcf");
        if (BigInteger.ZERO.equals(r)) {
            writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_5_10.coverage", "11e175a3-1551-49cb-a444-a364f5ccadca");
            throw new ArithmeticException("%");
        }
        writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_5_10.coverage", "83c8a7ea-8347-4b4c-b6bb-c9952ace4549");
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
        writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_5_10.coverage", "9ece23bc-8d29-4c58-a6fb-4afdff92d162");
        if (left == null && right == null) {
            writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_5_10.coverage", "3177fd86-c94b-464d-8b98-571d785051d2");
            return controlNullNullOperands();
        }
        writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_5_10.coverage", "bc17ecf0-772a-4c1f-a796-f82a3bd35327");
        if (left instanceof BigDecimal || right instanceof BigDecimal) {
            writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_5_10.coverage", "5e2fe505-5948-4e6b-bad3-0c13444b1703");
            BigDecimal l = toBigDecimal(left);
            writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_5_10.coverage", "ff095918-0083-4152-9c99-a6b1a596856b");
            BigDecimal r = toBigDecimal(right);
            writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_5_10.coverage", "37df635a-3f89-4165-a0cb-bced6533a03b");
            BigDecimal result = l.multiply(r, getMathContext());
            writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_5_10.coverage", "9a45fb65-a8c3-498e-bc7e-58d3064c6a1e");
            return narrowBigDecimal(left, right, result);
        }
        writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_5_10.coverage", "96e5ad43-3721-4fba-9d0c-3f20f44150e1");
        if (isFloatingPointNumber(left) || isFloatingPointNumber(right)) {
            writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_5_10.coverage", "0944938c-9f24-46d8-9324-5c4cb0fc1f4d");
            double l = toDouble(left);
            writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_5_10.coverage", "a4351cb7-656e-4b0c-814b-9ec3487d4b95");
            double r = toDouble(right);
            writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_5_10.coverage", "d4fb77ed-dfd0-425e-b8a4-f80980bf2b18");
            return l * r;
        }
        writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_5_10.coverage", "9e9079bb-0912-4027-82c7-029f5b12ffa2");
        BigInteger l = toBigInteger(left);
        writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_5_10.coverage", "fb7c5bc4-d1e9-4093-aa41-bba690e024ec");
        BigInteger r = toBigInteger(right);
        writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_5_10.coverage", "317166aa-7164-4685-8e43-c81e2a6cf7a4");
        BigInteger result = l.multiply(r);
        writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_5_10.coverage", "866f4287-9533-43b0-9696-628638c2b844");
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
        writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_5_10.coverage", "9e11d01b-af12-414e-b6b1-c39c65a91f7e");
        if (left == null && right == null) {
            writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_5_10.coverage", "eb1647f8-0070-4d76-ae1e-e710dda02f80");
            return controlNullNullOperands();
        }
        writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_5_10.coverage", "d0dc9b8f-879e-4988-82ba-de2e1506bb3c");
        if (left instanceof BigDecimal || right instanceof BigDecimal) {
            writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_5_10.coverage", "b574145e-d9c8-43fb-94c7-318af42d166d");
            BigDecimal l = toBigDecimal(left);
            writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_5_10.coverage", "acb56347-ab10-49f5-a168-fb4b3a84bdb1");
            BigDecimal r = toBigDecimal(right);
            writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_5_10.coverage", "5d1ed407-b371-42d5-bc5e-67373142f6f2");
            BigDecimal result = l.subtract(r, getMathContext());
            writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_5_10.coverage", "5a979af3-dd71-4c56-a79b-985680f4b600");
            return narrowBigDecimal(left, right, result);
        }
        writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_5_10.coverage", "28bb7568-1318-4b75-b298-c9d3faffc4e8");
        if (isFloatingPointNumber(left) || isFloatingPointNumber(right)) {
            writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_5_10.coverage", "9e295bfb-67e6-4c53-8df1-100b80ee7cda");
            double l = toDouble(left);
            writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_5_10.coverage", "caf8611f-3cd2-42ce-b466-74562ad912b5");
            double r = toDouble(right);
            writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_5_10.coverage", "f5f278f8-e1e5-46f8-ac02-0a0ffb180157");
            return l - r;
        }
        writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_5_10.coverage", "c6182a96-c424-48f4-9ab1-28a53e88db3f");
        BigInteger l = toBigInteger(left);
        writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_5_10.coverage", "21388695-7f46-40c4-83bc-9393eb7dc27b");
        BigInteger r = toBigInteger(right);
        writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_5_10.coverage", "03d07d68-5d01-4f2e-9c6d-0efd01b0ee08");
        BigInteger result = l.subtract(r);
        writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_5_10.coverage", "cf96a601-a5da-4dc8-bc46-071e51cfb0b5");
        return narrowBigInteger(left, right, result);
    }

    /**
     * Negates a value (unary minus for numbers).
     * 
     * @param val the value to negate
     * @return the negated value
     */
    public Object negate(Object val) {
        writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_5_10.coverage", "51508756-91a0-43d8-acc0-1f70e71b7ccb");
        if (val instanceof Integer) {
            writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_5_10.coverage", "40497a96-32c6-4bdf-8dbf-2e794d87a346");
            return -((Integer) val);
        } else if (val instanceof Double) {
            writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_5_10.coverage", "e8bf2b84-e641-4383-a587-437bf14cd9f2");
            return -((Double) val);
        } else if (val instanceof Long) {
            writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_5_10.coverage", "347c3de7-1cf3-4a90-9f07-256047a0f24a");
            return -((Long) val);
        } else if (val instanceof BigDecimal) {
            writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_5_10.coverage", "c74b2c89-f8e4-466b-97d7-d0578d03ec5e");
            return ((BigDecimal) val).negate();
        } else if (val instanceof BigInteger) {
            writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_5_10.coverage", "e748e92e-1b01-4313-bb9b-9cfa79a1599c");
            return ((BigInteger) val).negate();
        } else if (val instanceof Float) {
            writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_5_10.coverage", "32a6ec76-941d-4c20-9d85-cae10b501048");
            return -((Float) val);
        } else if (val instanceof Short) {
            writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_5_10.coverage", "66b1bb82-a133-4831-9a12-39f87b7db67f");
            return (short) -((Short) val);
        } else if (val instanceof Byte) {
            writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_5_10.coverage", "5e5afede-cdfe-482b-bb73-7ba017c34944");
            return (byte) -((Byte) val);
        } else if (val instanceof Boolean) {
            writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_5_10.coverage", "041ce8ea-c0a7-4f15-a509-24d8181d606c");
            return ((Boolean) val) ? Boolean.FALSE : Boolean.TRUE;
        }
        writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_5_10.coverage", "c55b0e0b-db5b-46bc-a0ed-c222d2558fe2");
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
        writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_5_10.coverage", "97f656ea-7feb-4f85-a54c-5f08d28b6a6d");
        if (value == null && container == null) {
            writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_5_10.coverage", "2dd4f73d-dd1c-49ac-a20b-b98e637bd474");
            return true;
        }
        writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_5_10.coverage", "be26156f-6bd7-4080-9fed-5e000381908b");
        if (value == null || container == null) {
            writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_5_10.coverage", "0cfec66b-9c7e-4967-a1bc-1cd3d6946804");
            return false;
        }
        writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_5_10.coverage", "27d5fd6e-259c-4ac5-8302-59842e9fee15");
        if (container instanceof java.util.regex.Pattern) {
            writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_5_10.coverage", "32312b88-72ba-40cc-b45a-b4bfb560d91c");
            return ((java.util.regex.Pattern) container).matcher(value.toString()).matches();
        }
        writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_5_10.coverage", "ac54fc75-0a28-431e-a9c7-e7f15e728f0e");
        if (container instanceof String) {
            writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_5_10.coverage", "70354e74-9a9b-4cd2-a045-12ede4cbab44");
            return value.toString().matches(container.toString());
        }
        writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_5_10.coverage", "e4ed70d1-a9eb-4656-84ea-dae0d22b0a3d");
        if (container instanceof Map<?, ?>) {
            writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_5_10.coverage", "015a21b4-11ef-44fb-aed7-ee584a6b0edd");
            if (value instanceof Map<?, ?>) {
                writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_5_10.coverage", "b65a102b-2f84-4cda-8c25-1362582ca71f");
                return ((Map<?, ?>) container).keySet().containsAll(((Map<?, ?>) value).keySet());
            }
            writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_5_10.coverage", "88b012c4-35f2-4263-8aa0-461053f81c15");
            return ((Map<?, ?>) container).containsKey(value);
        }
        writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_5_10.coverage", "110a1bad-1e38-4b7d-a31c-b575167f32de");
        if (container instanceof Collection<?>) {
            writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_5_10.coverage", "1baa3270-cf27-4a03-8caf-ae9dd7c343ba");
            if (value instanceof Collection<?>) {
                writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_5_10.coverage", "cac03aa7-94f6-45ad-b73c-6399cb0bd93d");
                return ((Collection<?>) container).containsAll((Collection<?>) value);
            }
            writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_5_10.coverage", "8dc7cff4-81b3-44d2-8e88-f77dbf4a745e");
            return ((Collection<?>) container).contains(value);
        }
        writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_5_10.coverage", "af64b9fd-9f9e-46df-add8-7919852f80d0");
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
        writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_5_10.coverage", "e5d303f8-b8f3-40e2-9dc0-a03d8d49118e");
        if (left == null && right == null) {
            writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_5_10.coverage", "fef942f1-53bd-41dc-9968-5c27b1627b2e");
            return true;
        }
        writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_5_10.coverage", "01fca004-f4ac-4179-bbfb-faa994af4f9a");
        if (left == null || right == null) {
            writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_5_10.coverage", "6f37c452-86d9-4c9c-b9dd-1db7226a859f");
            return false;
        }
        writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_5_10.coverage", "03856ee3-9f21-44d1-92e9-b9db1ef70c82");
        if (left instanceof String) {
            writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_5_10.coverage", "d9b9e635-297b-4af6-9266-3511bfa10652");
            return ((String) left).endsWith(toString(right));
        }
        writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_5_10.coverage", "73429f0a-4a80-4dfd-9f5d-bd7a084ba6ca");
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
        writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_5_10.coverage", "91dfede9-4832-479f-b323-66233d8af233");
        if (left == null && right == null) {
            writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_5_10.coverage", "105931f9-8d65-44a1-8b42-8d45e5d310d8");
            return true;
        }
        writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_5_10.coverage", "c4d2bd40-845d-42da-b85a-89de904d5281");
        if (left == null || right == null) {
            writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_5_10.coverage", "94e9b76d-22a7-4381-bb7c-96c73348d9fa");
            return false;
        }
        writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_5_10.coverage", "3a694376-9bcb-4396-8153-4e70a02492f9");
        if (left instanceof String) {
            writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_5_10.coverage", "0f518d2f-dece-4fff-b301-447922046c2e");
            return ((String) left).startsWith(toString(right));
        }
        writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_5_10.coverage", "57402e54-bdcb-493b-b319-84b13dbe069e");
        return null;
    }

    /**
     * Check for emptyness of various types: Number, Collection, Array, Map, String.
     *
     * @param object the object to check the emptyness of
     * @return the boolean or null of there is no arithmetic solution
     */
    public Boolean isEmpty(Object object) {
        writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_5_10.coverage", "13194f4a-481a-46df-8cb6-479a9b5b1a79");
        if (object instanceof Number) {
            writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_5_10.coverage", "285e83dd-6948-4d4e-873f-100d249c38e9");
            double d = ((Number) object).doubleValue();
            writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_5_10.coverage", "0dba48f4-3ddd-4a96-9ef5-6960cbd0cdea");
            return Double.isNaN(d) || d == 0.d ? Boolean.TRUE : Boolean.FALSE;
        }
        writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_5_10.coverage", "f95f3724-e752-4e6c-9a3e-ba537687afcb");
        if (object instanceof String) {
            writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_5_10.coverage", "ee630fc1-2779-42c6-9be7-63737acfc40c");
            return "".equals(object) ? Boolean.TRUE : Boolean.FALSE;
        }
        writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_5_10.coverage", "ed79b627-8412-4d05-9dda-17df9b3f0a1b");
        if (object.getClass().isArray()) {
            writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_5_10.coverage", "2d6364f5-9352-4c8e-8141-94b03e3c3ffc");
            return Array.getLength(object) == 0 ? Boolean.TRUE : Boolean.FALSE;
        }
        writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_5_10.coverage", "c07b4cad-847c-4685-99db-3b73fc775c7b");
        if (object instanceof Collection<?>) {
            writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_5_10.coverage", "76e4c314-ae95-49ad-8353-5ce7a1e69903");
            return ((Collection<?>) object).isEmpty() ? Boolean.TRUE : Boolean.FALSE;
        }
        writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_5_10.coverage", "08f68951-b098-44d2-8437-cd620fcedf70");
        if (object instanceof Map<?, ?>) {
            writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_5_10.coverage", "42d1e930-db70-42d7-91c7-4077ba26c396");
            return ((Map<?, ?>) object).isEmpty() ? Boolean.TRUE : Boolean.FALSE;
        }
        writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_5_10.coverage", "73859110-7e53-4ed7-a480-27f167353898");
        return null;
    }

    /**
     * Calculate the <code>size</code> of various types: Collection, Array, Map, String.
     *
     * @param object the object to get the size of
     * @return the size of object or null if there is no arithmetic solution
     */
    public Integer size(Object object) {
        writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_5_10.coverage", "6f616d06-8a3d-4392-8a95-cdc444fe5bba");
        if (object instanceof String) {
            writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_5_10.coverage", "36ebcb3f-8d6f-4960-95f2-704a127e1b1f");
            return ((String) object).length();
        }
        writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_5_10.coverage", "943ca457-7cc3-419f-b33e-f28a8dce0204");
        if (object.getClass().isArray()) {
            writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_5_10.coverage", "10c0a88e-7de4-49a1-b5f2-91fc8209cb0b");
            return Array.getLength(object);
        }
        writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_5_10.coverage", "c6e42216-aa0d-40ad-9f4d-ff39cc3897a6");
        if (object instanceof Collection<?>) {
            writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_5_10.coverage", "95cdcfc7-fd46-4594-ac44-f1b6b1610e45");
            return ((Collection<?>) object).size();
        }
        writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_5_10.coverage", "67ec6959-3b7e-4c0c-a4de-06638576cd38");
        if (object instanceof Map<?, ?>) {
            writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_5_10.coverage", "1d197181-93df-45f0-9bee-e561f7a583a2");
            return ((Map<?, ?>) object).size();
        }
        writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_5_10.coverage", "4f3ddd51-8d3d-4df0-a1c9-3de0129082a0");
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
        writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_5_10.coverage", "a467a497-af1a-4528-a164-0898cbcf3e7c");
        long l = toLong(left);
        writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_5_10.coverage", "8648ac69-0bec-485e-81d0-78f022d13552");
        long r = toLong(right);
        writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_5_10.coverage", "5742a57a-1e7e-4ce7-8f58-dc3117387684");
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
        writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_5_10.coverage", "debec49b-89af-4ffc-a0f1-4805e4a6fd88");
        long l = toLong(left);
        writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_5_10.coverage", "733924f5-8440-4722-bb52-52c8af776014");
        long r = toLong(right);
        writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_5_10.coverage", "3345ab64-2d1a-4a68-9ca3-e8357a964481");
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
        writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_5_10.coverage", "89731b2e-2446-4ad8-b598-18edd09b41fb");
        long l = toLong(left);
        writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_5_10.coverage", "e01f096a-3827-4363-bd42-cada13d0af47");
        long r = toLong(right);
        writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_5_10.coverage", "d0c7f468-8eb3-4fce-887f-a94fcb657863");
        return l ^ r;
    }

    /**
     * Performs a bitwise complement.
     * 
     * @param val the operand
     * @return ~val
     */
    public Object complement(Object val) {
        writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_5_10.coverage", "dcd31a46-ce0d-4d01-9dcf-4673bdba253d");
        long l = toLong(val);
        writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_5_10.coverage", "4a43b779-af5b-4983-aaf2-71144770404f");
        return ~l;
    }

    /**
     * Performs a logical not.
     * 
     * @param val the operand
     * @return !val
     */
    public Object not(Object val) {
        writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_5_10.coverage", "e5b958f1-9be7-4664-b0dd-6015efe1e623");
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
        writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_5_10.coverage", "6f2c8c7a-bce3-41b8-b12f-ca3c14b55d38");
        if (left != null && right != null) {
            writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_5_10.coverage", "24f2ee48-4cf5-4925-b02e-a0c92aa7c292");
            if (left instanceof BigDecimal || right instanceof BigDecimal) {
                writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_5_10.coverage", "96383730-ce15-4a44-a9e8-4ca045a85ee2");
                BigDecimal l = toBigDecimal(left);
                writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_5_10.coverage", "0350e637-31b7-416b-870f-965a0ea5db6b");
                BigDecimal r = toBigDecimal(right);
                writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_5_10.coverage", "3efc80ec-96e8-49a0-bc2e-afb9fedefe23");
                return l.compareTo(r);
            } else if (left instanceof BigInteger || right instanceof BigInteger) {
                writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_5_10.coverage", "5d8b9d51-1c01-47e8-962f-9de0cf73504e");
                BigInteger l = toBigInteger(left);
                writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_5_10.coverage", "b5cde3fb-95ec-47fd-9a48-09f952d471fa");
                BigInteger r = toBigInteger(right);
                writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_5_10.coverage", "9bda6c35-fb3d-45da-a0ec-aa079d8904f7");
                return l.compareTo(r);
            } else if (isFloatingPoint(left) || isFloatingPoint(right)) {
                writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_5_10.coverage", "95174d60-3bc7-4fdc-a853-403e54d37cfb");
                double lhs = toDouble(left);
                writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_5_10.coverage", "eb730dc6-7eef-40e6-8544-8d5d48d64916");
                double rhs = toDouble(right);
                writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_5_10.coverage", "19f58a93-6511-4a66-91c4-03aba5ed51e9");
                if (Double.isNaN(lhs)) {
                    writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_5_10.coverage", "6fde4e47-61db-42cf-83f2-6cbe9b4587ec");
                    if (Double.isNaN(rhs)) {
                        writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_5_10.coverage", "6a8f4338-2187-4425-be17-f8adf7a98510");
                        return 0;
                    } else {
                        writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_5_10.coverage", "4ac691b1-785d-42dd-abdb-bc517628a081");
                        return -1;
                    }
                } else if (Double.isNaN(rhs)) {
                    writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_5_10.coverage", "cfbb6679-8592-4531-bc31-e3256cb109d9");
                    return +1;
                } else if (lhs < rhs) {
                    writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_5_10.coverage", "209520d2-1611-438c-8fce-603886bf72d1");
                    return -1;
                } else if (lhs > rhs) {
                    writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_5_10.coverage", "5d340e5f-7d36-46b9-947b-c1c77bf74b01");
                    return +1;
                } else {
                    writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_5_10.coverage", "4d155e4a-81db-4801-b6dd-b439694547d0");
                    return 0;
                }
            } else if (isNumberable(left) || isNumberable(right)) {
                writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_5_10.coverage", "44fecef5-ba7d-4258-8327-f72975b47716");
                long lhs = toLong(left);
                writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_5_10.coverage", "d158453b-922d-4271-b980-45203e18b1b1");
                long rhs = toLong(right);
                writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_5_10.coverage", "2ea89333-47a1-483d-8820-5add550543de");
                if (lhs < rhs) {
                    writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_5_10.coverage", "d38a3639-61ff-4b47-95d3-2e600f56e223");
                    return -1;
                } else if (lhs > rhs) {
                    writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_5_10.coverage", "29985691-9066-433b-93a1-b5a2af0313fb");
                    return +1;
                } else {
                    writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_5_10.coverage", "225d4857-1374-4b15-9332-f2a119850237");
                    return 0;
                }
            } else if (left instanceof String || right instanceof String) {
                writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_5_10.coverage", "7797db91-6932-4cc6-a58b-c48c5cf5b1af");
                return toString(left).compareTo(toString(right));
            } else if ("==".equals(operator)) {
                writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_5_10.coverage", "73712c78-a68c-4cf5-965b-7cd2cf7cb82f");
                return left.equals(right) ? 0 : -1;
            } else if (left instanceof Comparable<?>) {
                writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_5_10.coverage", "778a85e0-0f2e-4d25-84d2-12119ab43c23");
                @SuppressWarnings("unchecked") final Comparable<Object> comparable = (Comparable<Object>) left;
                writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_5_10.coverage", "8aa9124c-cedb-4255-8134-8878473586a8");
                return comparable.compareTo(right);
            } else if (right instanceof Comparable<?>) {
                writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_5_10.coverage", "0057a339-4a4c-4f7f-b688-2d22e230099c");
                @SuppressWarnings("unchecked") final Comparable<Object> comparable = (Comparable<Object>) right;
                writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_5_10.coverage", "c42eb40c-e3e2-4ab5-a01f-5c1a7a7605a9");
                return comparable.compareTo(left);
            }
        }
        writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_5_10.coverage", "2a417359-72e4-44ff-ad51-05eaa0fb0f9f");
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
        writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_5_10.coverage", "c86e3ade-04fd-4417-9c00-fcccacd7d385");
        if (left == right) {
            writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_5_10.coverage", "0ada5589-89b9-4962-b164-ed4f708444ea");
            return true;
        } else if (left == null || right == null) {
            writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_5_10.coverage", "8c40a4b2-5c11-4b1b-a8f2-e08b378eafba");
            return false;
        } else if (left instanceof Boolean || right instanceof Boolean) {
            writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_5_10.coverage", "7fff8f6e-2728-440a-be39-92d7b6afe310");
            return toBoolean(left) == toBoolean(right);
        } else {
            writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_5_10.coverage", "853284bc-8e04-4677-9802-60f65fc01e24");
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
        writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_5_10.coverage", "5a2b9fb2-d62f-47c4-8d7b-600264f3f1eb");
        if ((left == right) || (left == null) || (right == null)) {
            writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_5_10.coverage", "d8051f18-a495-4321-9b94-b8b73d6c3094");
            return false;
        } else {
            writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_5_10.coverage", "8e767d41-5d04-4641-836f-d0e6c2808db6");
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
        writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_5_10.coverage", "1fdd584f-7322-4490-b8b8-ef16db0710f8");
        if ((left == right) || left == null || right == null) {
            writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_5_10.coverage", "6e0aba0e-d596-471f-8773-8042604443bf");
            return false;
        } else {
            writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_5_10.coverage", "f849748e-8834-458e-9dfa-830b2049f2dd");
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
        writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_5_10.coverage", "6e4d72a1-eaff-46f5-92fc-56f8d57f5328");
        if (left == right) {
            writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_5_10.coverage", "39762d08-8ebd-4c8b-ae81-48f33e63904c");
            return true;
        } else if (left == null || right == null) {
            writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_5_10.coverage", "55d7a546-edc2-41c4-9427-1cf9efcb3fd3");
            return false;
        } else {
            writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_5_10.coverage", "adb3b800-49ae-45b3-96d8-cb51473f50ab");
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
        writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_5_10.coverage", "6ef43e16-3a4d-4b3c-a835-d4cf9a37ca04");
        if (left == right) {
            writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_5_10.coverage", "3d4f38fe-9199-4408-8370-47bc76eb4676");
            return true;
        } else if (left == null || right == null) {
            writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_5_10.coverage", "67c2aa8d-42e8-4371-a152-8c868f4db742");
            return false;
        } else {
            writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_5_10.coverage", "d6f93b0d-151f-4e83-9d05-65a5dad2bbf8");
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
        writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_5_10.coverage", "e1e43787-23ba-42c2-9c2e-f5fb0c8116a2");
        if (val == null) {
            writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_5_10.coverage", "13f13000-4c9c-4d7e-9a1a-4cfb5ba34187");
            controlNullOperand();
            writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_5_10.coverage", "f5631f12-bf72-4be0-8a14-8c612ffaea6f");
            return false;
        } else if (val instanceof Boolean) {
            writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_5_10.coverage", "da1897aa-7a4b-4bb1-a8bc-ea9f5453e97a");
            return ((Boolean) val);
        } else if (val instanceof Number) {
            writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_5_10.coverage", "0ec55f8a-9e60-4734-a7b3-f8ef72ee63d4");
            double number = toDouble(val);
            writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_5_10.coverage", "0487d371-6e70-4c6c-824a-89623815a3ee");
            return !Double.isNaN(number) && number != 0.d;
        } else if (val instanceof String) {
            writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_5_10.coverage", "e543722d-c556-4096-b789-a9e09679350e");
            String strval = val.toString();
            writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_5_10.coverage", "7b373306-f692-419e-85ae-31eeabc81397");
            return strval.length() > 0 && !"false".equals(strval);
        } else {
            writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_5_10.coverage", "40c3aa7a-c2ad-4d6f-ad73-4c455db4e274");
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
        writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_5_10.coverage", "dc94f11f-eba9-43f8-b794-4a1a19caeff4");
        if (val == null) {
            writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_5_10.coverage", "e4c96bc1-823e-4eb1-980a-454067300f76");
            controlNullOperand();
            writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_5_10.coverage", "6fe8ccf5-ac1f-4c4c-a35c-1ad1e870257d");
            return 0;
        } else if (val instanceof Double) {
            writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_5_10.coverage", "0284a236-eb9e-4ea3-be27-9b2ff3dcd76f");
            Double dval = (Double) val;
            writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_5_10.coverage", "65c23021-bad0-4a51-a5de-ea45bab77684");
            if (Double.isNaN(dval)) {
                writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_5_10.coverage", "1faf2d10-5cf5-4b11-8fe4-0e7a0781b06a");
                return 0;
            } else {
                writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_5_10.coverage", "79963a69-20e5-4141-b8ca-3cd79364a878");
                return dval.intValue();
            }
        } else if (val instanceof Number) {
            writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_5_10.coverage", "d152f26d-edef-48b7-98ad-dc9d97a4b55c");
            return ((Number) val).intValue();
        } else if (val instanceof String) {
            writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_5_10.coverage", "7db59c15-b916-44e9-ab10-922c508a724e");
            if ("".equals(val)) {
                writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_5_10.coverage", "96755310-b2f1-412a-b883-a9bb959f8c90");
                return 0;
            }
            writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_5_10.coverage", "7ff3c560-6dfb-49db-96f5-87d536ffb51b");
            return Integer.parseInt((String) val);
        } else if (val instanceof Boolean) {
            writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_5_10.coverage", "071641ae-a49d-4c27-8912-301bfd71886a");
            return ((Boolean) val) ? 1 : 0;
        } else if (val instanceof Character) {
            writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_5_10.coverage", "912ec377-d813-463c-8476-74e0da312fee");
            return ((Character) val);
        }
        writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_5_10.coverage", "26a5755d-c21c-41c9-8866-b03c77019a29");
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
        writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_5_10.coverage", "9f30d2a0-97f2-4f7d-b784-a1a774d1b170");
        if (val == null) {
            writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_5_10.coverage", "91f9f7be-9700-4cc1-8454-d135b3d02356");
            controlNullOperand();
            writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_5_10.coverage", "b97a3967-94e6-4aca-b244-d1e92da47ee4");
            return 0L;
        } else if (val instanceof Double) {
            writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_5_10.coverage", "fd6e3dda-ffbc-49f0-a2e6-22a0ba8ad512");
            Double dval = (Double) val;
            writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_5_10.coverage", "3603f6a2-ca20-4931-ada6-ff6804d4f63e");
            if (Double.isNaN(dval)) {
                writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_5_10.coverage", "29039f89-1db5-4692-829d-a61e9904f29b");
                return 0L;
            } else {
                writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_5_10.coverage", "9bc4a84d-c93d-4ac4-aab2-757e3516a45e");
                return dval.longValue();
            }
        } else if (val instanceof Number) {
            writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_5_10.coverage", "e12170f0-a32b-47e6-8901-668bfbbf65ca");
            return ((Number) val).longValue();
        } else if (val instanceof String) {
            writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_5_10.coverage", "74279b9e-d282-40e4-8f69-eccb532c3fda");
            if ("".equals(val)) {
                writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_5_10.coverage", "e21249a2-8804-447a-8bdf-0d114ec3224c");
                return 0L;
            } else {
                writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_5_10.coverage", "97feece8-63f2-4010-b758-b5a25bee60a9");
                return Long.parseLong((String) val);
            }
        } else if (val instanceof Boolean) {
            writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_5_10.coverage", "cda313d7-c54b-4385-a08c-9ebde754e57a");
            return ((Boolean) val) ? 1L : 0L;
        } else if (val instanceof Character) {
            writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_5_10.coverage", "8d9c306f-cb8b-4775-a5f8-74900a156f18");
            return ((Character) val);
        }
        writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_5_10.coverage", "9a0a1540-d4f2-4481-9cd7-00ba8f62644a");
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
        writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_5_10.coverage", "eb395c02-45d9-4e81-bf4a-3e25e467b89c");
        if (val == null) {
            writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_5_10.coverage", "ae0cbbf3-a6e7-451c-9b15-f73ac195dc9e");
            controlNullOperand();
            writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_5_10.coverage", "34aa483b-d897-4bab-9b89-7dabe089b457");
            return BigInteger.ZERO;
        } else if (val instanceof BigInteger) {
            writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_5_10.coverage", "99217e05-cb83-4da9-a54c-08e7f09e2a8b");
            return (BigInteger) val;
        } else if (val instanceof Double) {
            writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_5_10.coverage", "01a7dfc1-c078-49f7-b4b1-e007cdedde02");
            Double dval = (Double) val;
            writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_5_10.coverage", "326f5835-22fe-4d16-8a7c-3e52f1b3a3ca");
            if (Double.isNaN(dval)) {
                writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_5_10.coverage", "d3356c97-29d4-4faf-8205-ed81b6f4e232");
                return BigInteger.ZERO;
            } else {
                writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_5_10.coverage", "5704d427-2f24-42f7-a37d-7b8cd248fc81");
                return BigInteger.valueOf(dval.longValue());
            }
        } else if (val instanceof BigDecimal) {
            writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_5_10.coverage", "51be9a7a-77ef-4f61-839f-f282041c85dc");
            return ((BigDecimal) val).toBigInteger();
        } else if (val instanceof Number) {
            writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_5_10.coverage", "22a4b9ba-2738-4c9f-91ff-16eec252347c");
            return BigInteger.valueOf(((Number) val).longValue());
        } else if (val instanceof Boolean) {
            writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_5_10.coverage", "997dd9d3-9c00-4444-b173-807524c5b8b1");
            return BigInteger.valueOf(((Boolean) val) ? 1L : 0L);
        } else if (val instanceof String) {
            writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_5_10.coverage", "31cea933-49af-4de4-a9cb-6cd793e04905");
            String string = (String) val;
            writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_5_10.coverage", "a7ab20cd-ca57-4de4-b045-67639c64565f");
            if ("".equals(string)) {
                writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_5_10.coverage", "f86ec9db-a2fa-4ab2-90c5-8d92031f74a2");
                return BigInteger.ZERO;
            } else {
                writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_5_10.coverage", "7d1c839a-a9aa-4a29-8a15-ec2f7fdc7e6e");
                return new BigInteger(string);
            }
        } else if (val instanceof Character) {
            writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_5_10.coverage", "b4d522bf-a95f-47e6-b995-c3063d560d3e");
            int i = ((Character) val);
            writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_5_10.coverage", "ef962a20-151f-4c60-b092-faa84b7ee17f");
            return BigInteger.valueOf(i);
        }
        writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_5_10.coverage", "4b7577fa-6fab-4c35-b7fd-7b13d7da2731");
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
        writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_5_10.coverage", "84781a9f-7947-4d11-a831-34fa0c1f2d6d");
        if (val instanceof BigDecimal) {
            writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_5_10.coverage", "24e2f83c-bac1-400f-8674-bebbc0dc07b0");
            return roundBigDecimal((BigDecimal) val);
        } else if (val == null) {
            writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_5_10.coverage", "38dd5b3a-788a-418a-be51-1988befe2b22");
            controlNullOperand();
            writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_5_10.coverage", "1845af8d-419b-4a6f-967c-251c52ffeb8a");
            return BigDecimal.ZERO;
        } else if (val instanceof Double) {
            writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_5_10.coverage", "25d22a4f-611c-4dba-96e4-851f742bff29");
            if (Double.isNaN(((Double) val))) {
                writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_5_10.coverage", "b9db7a37-db2a-4ab3-9c02-4a956010378b");
                return BigDecimal.ZERO;
            } else {
                writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_5_10.coverage", "a8d69cdd-7897-4d94-8b54-1b1ea63d8b7f");
                return roundBigDecimal(new BigDecimal(val.toString(), getMathContext()));
            }
        } else if (val instanceof Number) {
            writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_5_10.coverage", "cf3f600d-1452-41b2-a7fb-ab4cb3c06434");
            return roundBigDecimal(new BigDecimal(val.toString(), getMathContext()));
        } else if (val instanceof Boolean) {
            writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_5_10.coverage", "6d0a5d5c-87d3-4c35-b4a7-63fbae6819fc");
            return BigDecimal.valueOf(((Boolean) val) ? 1. : 0.);
        } else if (val instanceof String) {
            writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_5_10.coverage", "7866c358-cb47-401b-a937-9a622c75524e");
            String string = (String) val;
            writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_5_10.coverage", "4d4f7344-b25f-44b0-a917-94f460023760");
            if ("".equals(string)) {
                writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_5_10.coverage", "ac785783-b521-43bd-b6ae-7d2a3993635e");
                return BigDecimal.ZERO;
            }
            writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_5_10.coverage", "5958848c-f46d-493c-8948-4b39494286e7");
            return roundBigDecimal(new BigDecimal(string, getMathContext()));
        } else if (val instanceof Character) {
            writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_5_10.coverage", "168feda0-60e7-4b4c-b0ee-58d93408af6b");
            int i = ((Character) val);
            writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_5_10.coverage", "53c89b42-f642-4936-baeb-0ad45df365ea");
            return new BigDecimal(i);
        }
        writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_5_10.coverage", "69fcd270-3730-4ff8-ae02-c93039b13776");
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
        writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_5_10.coverage", "89f17287-38ca-41da-9fca-3437f346f505");
        if (val == null) {
            writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_5_10.coverage", "b5f5dd28-28b5-4097-85a5-8e0fccfcf605");
            controlNullOperand();
            writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_5_10.coverage", "af89f840-2670-445d-9205-7764f8b2ef15");
            return 0;
        } else if (val instanceof Double) {
            writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_5_10.coverage", "5f8cc79f-189c-4131-a8cd-d41d60323ee7");
            return ((Double) val);
        } else if (val instanceof Number) {
            writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_5_10.coverage", "df2750e2-b0d0-4089-98f4-01e84e80d701");
            return Double.parseDouble(String.valueOf(val));
        } else if (val instanceof Boolean) {
            writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_5_10.coverage", "09c1e36e-52be-4efd-9530-d897e9796316");
            return ((Boolean) val) ? 1. : 0.;
        } else if (val instanceof String) {
            writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_5_10.coverage", "651e04da-73d3-4a48-bc6d-d087f768e0a8");
            String string = (String) val;
            writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_5_10.coverage", "d4e75036-53ee-4afc-9b03-69e29271ff1a");
            if ("".equals(string)) {
                writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_5_10.coverage", "73302056-2b4c-413b-99d2-48cf6cebc4db");
                return Double.NaN;
            } else {
                writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_5_10.coverage", "dfa4cb58-18cd-4215-84f1-ff17df2d81b9");
                return Double.parseDouble(string);
            }
        } else if (val instanceof Character) {
            writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_5_10.coverage", "e0d6d1c5-36b6-467f-b82f-9b79a49dbcbd");
            int i = ((Character) val);
            writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_5_10.coverage", "3a6064f3-23d6-4eb9-b655-9c06e47e23a3");
            return i;
        }
        writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_5_10.coverage", "9ea8c709-2e01-416e-8af2-c0c8ced1aa47");
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
        writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_5_10.coverage", "480f2104-a5d3-4b75-96af-f6fe693c5faa");
        if (val == null) {
            writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_5_10.coverage", "bf2ed1da-d034-4f34-9045-53665cefe88d");
            controlNullOperand();
            writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_5_10.coverage", "88130d1a-9cb2-4faa-867e-7b9a013f3824");
            return "";
        } else if (val instanceof Double) {
            writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_5_10.coverage", "07ffc6d9-79c7-4f26-92b1-3e99d098d23c");
            Double dval = (Double) val;
            writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_5_10.coverage", "b8d51273-cb28-4f0d-a1d7-9fc5785a79ae");
            if (Double.isNaN(dval)) {
                writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_5_10.coverage", "2a8cb31e-5b29-420f-a016-ccc79106227c");
                return "";
            } else {
                writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_5_10.coverage", "c16313cc-44d3-482d-95ad-c37d99090593");
                return dval.toString();
            }
        } else {
            writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_5_10.coverage", "3c3b6ae4-570c-47dc-b780-426c39326c88");
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
        writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_5_10.coverage", "6659e536-fba5-4590-9677-410a23afec56");
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
        writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_5_10.coverage", "c6903fb8-8cfc-419c-8b70-ae43b25989a6");
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
        writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_5_10.coverage", "52937796-3a8b-4a87-a035-c1e26a2d766b");
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
        writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_5_10.coverage", "49609390-e8c9-44b9-99fe-6074da6a469f");
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
        writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_5_10.coverage", "bb968574-9888-4bcd-b6c1-e6f9333eb6d8");
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
