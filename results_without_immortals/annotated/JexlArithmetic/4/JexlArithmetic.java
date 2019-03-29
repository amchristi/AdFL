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
        writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_4_10.coverage", "0cc19008-2013-4b97-9c8a-155d747cded9");
        boolean ostrict = options.isStrictArithmetic() == null ? this.strict : options.isStrictArithmetic();
        writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_4_10.coverage", "aac3a208-a99d-41a7-9745-23ee1d3245ad");
        MathContext bigdContext = options.getArithmeticMathContext();
        writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_4_10.coverage", "754e94f4-0752-4109-90ba-10db28cf1d2a");
        if (bigdContext == null) {
            writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_4_10.coverage", "a2718401-9363-41c6-9df5-bbcfb30e41de");
            bigdContext = mathContext;
        }
        writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_4_10.coverage", "ab504611-9d06-4635-834a-e96ff8c63dfd");
        int bigdScale = options.getArithmeticMathScale();
        writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_4_10.coverage", "04b58b91-3790-4d3e-a395-55cf77cad851");
        if (bigdScale == Integer.MIN_VALUE) {
            writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_4_10.coverage", "821f1c48-fe65-4b31-a42b-0b1aaccf4958");
            bigdScale = mathScale;
        }
        writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_4_10.coverage", "46ff8b5c-f9a2-411f-8cca-fd1f76eeb10f");
        if ((ostrict != this.strict) || bigdScale != this.mathScale || bigdContext != this.mathContext) {
            writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_4_10.coverage", "45c0ab59-b92f-4cc5-b099-2b4c6a00dc1e");
            return new JexlArithmetic(ostrict, bigdContext, bigdScale);
        } else {
            writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_4_10.coverage", "1dae6209-eba0-4ac6-9da5-958565d340fb");
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
        writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_4_10.coverage", "119bffd1-4035-48a4-a470-0f2b6529a470");
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
        writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_4_10.coverage", "6dfa0b76-e193-47fd-b2ed-5c1b3cacf8d3");
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
        writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_4_10.coverage", "833de79c-bb4a-41eb-b63e-54c59b8bbedd");
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
        writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_4_10.coverage", "dd01136f-1bc8-49ed-bcbf-fc71d010304c");
        final long lfrom = toLong(from);
        writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_4_10.coverage", "54072f7b-dc8d-4111-82f2-2b7f55be87d1");
        final long lto = toLong(to);
        writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_4_10.coverage", "0ce6f646-9c09-4341-9b65-9838a927961d");
        if ((lfrom >= Integer.MIN_VALUE && lfrom <= Integer.MAX_VALUE) && (lto >= Integer.MIN_VALUE && lto <= Integer.MAX_VALUE)) {
            writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_4_10.coverage", "9126a420-2b51-4c4c-92da-1a19542fcf09");
            return org.apache.commons.jexl3.internal.IntegerRange.create((int) lfrom, (int) lto);
        } else {
            writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_4_10.coverage", "51849a01-eb86-4f1e-a812-66c408aba08d");
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
        writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_4_10.coverage", "fd0e5838-ee31-48c1-8dc6-31b428e7ef3a");
        return this.strict;
    }

    /**
     * The MathContext instance used for +,-,/,*,% operations on big decimals.
     * 
     * @return the math context
     */
    public MathContext getMathContext() {
        writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_4_10.coverage", "fb7b3961-c643-4be7-97cb-f3871013bcaa");
        return mathContext;
    }

    /**
     * The BigDecimal scale used for comparison and coericion operations.
     * 
     * @return the scale
     */
    public int getMathScale() {
        writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_4_10.coverage", "3601817c-5be4-44fb-9e06-b93ea8096812");
        return mathScale;
    }

    /**
     * Ensure a big decimal is rounded by this arithmetic scale and rounding mode.
     * 
     * @param number the big decimal to round
     * @return the rounded big decimal
     */
    protected BigDecimal roundBigDecimal(final BigDecimal number) {
        writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_4_10.coverage", "f24fbb4d-eaa5-4c29-803a-f45135768c4d");
        int mscale = getMathScale();
        writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_4_10.coverage", "8751ba0c-f158-4b17-a3c3-ebb7a2a52c22");
        if (mscale >= 0) {
            writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_4_10.coverage", "1f0dd5a9-4fde-4146-8274-92a9bbb4996e");
            return number.setScale(mscale, getMathContext().getRoundingMode());
        } else {
            writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_4_10.coverage", "7ec78b39-bdfe-4eb5-9293-8cd2743ea241");
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
        writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_4_10.coverage", "71f3dca6-ee99-4c70-aff0-de70c56f4b15");
        if (isStrict()) {
            writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_4_10.coverage", "27ab0afb-451e-4f66-abd7-5d8f905d1df1");
            throw new NullOperand();
        }
        writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_4_10.coverage", "88dad28e-b72c-4eef-9ef4-af20b6bcef7a");
        return 0;
    }

    /**
     * Throw a NPE if arithmetic is strict.
     * 
     * @throws ArithmeticException if strict
     */
    protected void controlNullOperand() {
        writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_4_10.coverage", "fc52b5f1-c26a-4976-87b4-18955ba66313");
        if (isStrict()) {
            writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_4_10.coverage", "8669458c-979e-4ee3-aab1-9599f242e1c4");
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
        writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_4_10.coverage", "9e68365b-76e6-4286-bde2-fea6edcca51d");
        if (val instanceof Float || val instanceof Double) {
            writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_4_10.coverage", "022a072b-e58d-406b-a583-df44f8b0ec22");
            return true;
        }
        writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_4_10.coverage", "62b149d2-1920-4ec8-beac-dc1f240a70a8");
        if (val instanceof String) {
            writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_4_10.coverage", "ee588936-3af7-4c02-a1c9-deddf48665ca");
            String str = (String) val;
            writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_4_10.coverage", "720c6a2e-d0cf-44a2-a422-451a7d859a4f");
            for (int c = 0; c < str.length(); ++c) {
                writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_4_10.coverage", "c5da0eeb-5928-48e5-b114-903591ccd082");
                char ch = str.charAt(c);
                writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_4_10.coverage", "d635fac2-b714-4e85-9b67-f558508ab80f");
                if (ch == '.' || ch == 'E' || ch == 'e') {
                    writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_4_10.coverage", "9666c553-7b92-4aaf-93ad-be7b0ab94c09");
                    return FLOAT_PATTERN.matcher(str).matches();
                }
                writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_4_10.coverage", "6a38c657-7bf8-4929-9adf-9d202cdac1cd");
                if (ch != '+' && ch != '-' && ch < '0' && ch > '9') {
                    writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_4_10.coverage", "ef402533-12de-4d36-8534-9991966fa3b0");
                    break;
                }
            }
        }
        writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_4_10.coverage", "7166a755-21a3-494c-972e-70ddfb5b2a55");
        return false;
    }

    /**
     * Is Object a floating point number.
     *
     * @param o Object to be analyzed.
     * @return true if it is a Float or a Double.
     */
    protected boolean isFloatingPoint(final Object o) {
        writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_4_10.coverage", "3f82c0f1-d360-4472-9304-dedcbb68d7fc");
        return o instanceof Float || o instanceof Double;
    }

    /**
     * Is Object a whole number.
     *
     * @param o Object to be analyzed.
     * @return true if Integer, Long, Byte, Short or Character.
     */
    protected boolean isNumberable(final Object o) {
        writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_4_10.coverage", "38b3e161-3856-4a4a-b7c5-1ceba5fd28a3");
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
        writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_4_10.coverage", "510675a2-9554-48f4-bda8-a5540b01bdc5");
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
        writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_4_10.coverage", "cdfb996d-1e41-4f25-b1bf-dc3c98e4b8d3");
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
        writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_4_10.coverage", "488107f3-f8d1-4bd2-919d-a00bf1330af1");
        if (original == null) {
            writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_4_10.coverage", "8283e962-4da8-4428-b38d-cceb780e6d1b");
            return null;
        }
        writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_4_10.coverage", "c693c4eb-00ee-48ee-ae64-aecdaf177643");
        Number result = original;
        writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_4_10.coverage", "baea05f4-ad39-46a0-8550-48f9d76c0cbb");
        if (original instanceof BigDecimal) {
            writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_4_10.coverage", "53a9297d-dd9e-413c-b5a0-632bfd20c039");
            BigDecimal bigd = (BigDecimal) original;
            writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_4_10.coverage", "62a6caac-1b9b-4f7f-b086-d6e4403a4561");
            if (bigd.compareTo(BIGD_DOUBLE_MAX_VALUE) > 0 || bigd.compareTo(BIGD_DOUBLE_MIN_VALUE) < 0) {
                writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_4_10.coverage", "ed0916e3-844a-4054-887f-3c19126b3ec1");
                return original;
            } else {
                writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_4_10.coverage", "32a3d9a9-3695-40bb-8692-40a0fedf841a");
                try {
                    writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_4_10.coverage", "80cfdad7-9468-4363-9a70-a14a8b5e82af");
                    long l = bigd.longValueExact();
                    writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_4_10.coverage", "c9a08d52-c467-42aa-b7eb-fbcc5e6ac0ed");
                    if (narrowAccept(narrow, Integer.class) && l <= Integer.MAX_VALUE && l >= Integer.MIN_VALUE) {
                        writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_4_10.coverage", "fda6a838-b26e-4be6-bcd7-431a2cfc92d3");
                        return (int) l;
                    } else if (narrowAccept(narrow, Long.class)) {
                        writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_4_10.coverage", "9c2b80e9-0106-476b-a489-5cd2caed91f7");
                        return l;
                    }
                } catch (ArithmeticException xa) {
                }
            }
        }
        writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_4_10.coverage", "8f2be9ef-11f3-4016-955b-3a5c50f59ef2");
        if (original instanceof Double || original instanceof Float) {
            writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_4_10.coverage", "d4c5055b-2c1a-4d65-abfd-c11a2f8fa394");
            double value = original.doubleValue();
            writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_4_10.coverage", "8ac55c0c-c062-4f52-a631-0df3a492fc64");
            if (narrowAccept(narrow, Float.class) && value <= Float.MAX_VALUE && value >= Float.MIN_VALUE) {
                writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_4_10.coverage", "54949d6f-96fa-4cea-8442-7c056a2d4799");
                result = result.floatValue();
            }
        } else {
            writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_4_10.coverage", "4c902184-0337-4eb0-8a1e-b7302859baf0");
            if (original instanceof BigInteger) {
                writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_4_10.coverage", "732021a0-4a00-40db-9668-5e012e8e8bb7");
                BigInteger bigi = (BigInteger) original;
                writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_4_10.coverage", "99158e8a-0c10-4e9b-b809-f4384e1ca74e");
                if (bigi.compareTo(BIGI_LONG_MAX_VALUE) > 0 || bigi.compareTo(BIGI_LONG_MIN_VALUE) < 0) {
                    writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_4_10.coverage", "4ecaa229-9514-4d83-85dc-b847f42ee224");
                    return original;
                }
            }
            writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_4_10.coverage", "ddd4cd52-e0ef-4f83-af36-3dbaf1f90972");
            long value = original.longValue();
            writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_4_10.coverage", "6517c02a-05ad-44f6-b4d2-52de463e4b32");
            if (narrowAccept(narrow, Byte.class) && value <= Byte.MAX_VALUE && value >= Byte.MIN_VALUE) {
                writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_4_10.coverage", "7a042269-b9c7-4c55-9e76-1c033411181a");
                result = (byte) value;
            } else if (narrowAccept(narrow, Short.class) && value <= Short.MAX_VALUE && value >= Short.MIN_VALUE) {
                writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_4_10.coverage", "ad411d32-ca22-4453-b8a2-f8244fd11ce5");
                result = (short) value;
            } else if (narrowAccept(narrow, Integer.class) && value <= Integer.MAX_VALUE && value >= Integer.MIN_VALUE) {
                writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_4_10.coverage", "ebf55484-7b1e-4a64-b2ae-1dd71b6e86be");
                result = (int) value;
            }
        }
        writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_4_10.coverage", "088080be-508e-453b-95de-628e149ee26f");
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
        writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_4_10.coverage", "07e86578-be98-4399-a3d8-b263140849d7");
        if (!(lhs instanceof BigInteger || rhs instanceof BigInteger) && bigi.compareTo(BIGI_LONG_MAX_VALUE) <= 0 && bigi.compareTo(BIGI_LONG_MIN_VALUE) >= 0) {
            writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_4_10.coverage", "267b4a57-7275-4e21-af72-4ccdbbb1851d");
            long l = bigi.longValue();
            writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_4_10.coverage", "0eecdb17-0bf5-47a3-a5c8-6c8ca78ff498");
            if (!(lhs instanceof Long || rhs instanceof Long) && l <= Integer.MAX_VALUE && l >= Integer.MIN_VALUE) {
                writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_4_10.coverage", "06adbc23-f574-46ff-8c5d-b6f5e3f997af");
                return (int) l;
            }
            writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_4_10.coverage", "a11517ad-cfe7-4be9-9a65-eed965f8b939");
            return l;
        }
        writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_4_10.coverage", "1fd45090-5f36-4fdd-a100-365204ee4c10");
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
        writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_4_10.coverage", "c1ace395-c6e4-46c0-9fe7-0bb41c6e5934");
        if (isNumberable(lhs) || isNumberable(rhs)) {
            writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_4_10.coverage", "fe772299-aef4-4f11-94b0-4c91e67965c6");
            try {
                writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_4_10.coverage", "26dad096-d61a-4b13-9514-d8e624b86f25");
                long l = bigd.longValueExact();
                writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_4_10.coverage", "3cb22c3d-d3ea-4cf0-bb7f-1a9ee90c0d29");
                if (l <= Integer.MAX_VALUE && l >= Integer.MIN_VALUE) {
                    writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_4_10.coverage", "78afc0e1-832f-4313-b06a-33c9d8c8d93d");
                    return (int) l;
                } else {
                    writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_4_10.coverage", "663707ba-b8fc-47b3-a22b-02e7bdc67bf7");
                    return l;
                }
            } catch (ArithmeticException xa) {
            }
        }
        writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_4_10.coverage", "004683ed-88cc-4dca-95f5-6ae3ef35a595");
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
        writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_4_10.coverage", "d480df4d-ebc8-4639-b7c4-eee6e93ab82c");
        boolean narrowed = false;
        writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_4_10.coverage", "8b75ef15-c5fe-4058-b1ec-81ea0dfc9311");
        for (int a = 0; a < args.length; ++a) {
            writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_4_10.coverage", "e79ac42f-9d8b-4a49-8d68-deeb817ba135");
            Object arg = args[a];
            writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_4_10.coverage", "77f01397-1ba6-4c00-8d58-4ba135895e5b");
            if (arg instanceof Number) {
                writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_4_10.coverage", "8892c044-a046-4843-bcf7-858db8379a08");
                Number narg = (Number) arg;
                writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_4_10.coverage", "072ccfb8-a80f-49c2-a5d6-0faadd7a91a3");
                Number narrow = narrow(narg);
                writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_4_10.coverage", "15e329b3-eeb8-4d86-a6d6-f7d9b4559042");
                if (!narg.equals(narrow)) {
                    writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_4_10.coverage", "e228631b-1716-4735-94f0-613c7ff178d4");
                    args[a] = narrow;
                    writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_4_10.coverage", "9e2c27cd-ad2c-49f3-a784-11e8d88827a4");
                    narrowed = true;
                }
            }
        }
        writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_4_10.coverage", "290f7254-77b1-4e95-a27d-307c98794aec");
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
        writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_4_10.coverage", "db124d99-674c-452e-89b4-241d77b5a57e");
        if (left == null && right == null) {
            writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_4_10.coverage", "bcd846a0-2adf-4d92-8854-a473fa5d2d91");
            return controlNullNullOperands();
        }
        writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_4_10.coverage", "302f29a3-36c2-4cfe-9698-4d471f5b71e1");
        boolean strconcat = strict ? left instanceof String || right instanceof String : left instanceof String && right instanceof String;
        writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_4_10.coverage", "a12686a6-7075-4ef9-ad95-b07c94d2b83d");
        if (!strconcat) {
            writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_4_10.coverage", "e182e34f-714d-45f5-8e53-43081daa0d7d");
            try {
                writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_4_10.coverage", "749ad68c-b2ba-4c25-abc0-e76dfeb09e80");
                if (left instanceof BigDecimal || right instanceof BigDecimal) {
                    writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_4_10.coverage", "5fe5973e-d8cd-4208-aa35-917ec5bf7aca");
                    BigDecimal l = toBigDecimal(left);
                    writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_4_10.coverage", "d9237024-4f10-4e2c-8cbe-f896fb14a1a7");
                    BigDecimal r = toBigDecimal(right);
                    writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_4_10.coverage", "4bb96355-c9ca-40d8-b019-a85a94911176");
                    BigDecimal result = l.add(r, getMathContext());
                    writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_4_10.coverage", "ea6a9759-66bf-4458-acc5-0abd7ab38e9e");
                    return narrowBigDecimal(left, right, result);
                }
                writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_4_10.coverage", "8a7f9610-26b8-4556-b1fc-c0d4060fa5fc");
                if (isFloatingPointNumber(left) || isFloatingPointNumber(right)) {
                    writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_4_10.coverage", "25444df2-e600-46e3-872d-7bac1a0dd5c5");
                    double l = toDouble(left);
                    writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_4_10.coverage", "ae336b17-0301-4b9f-a8f4-e8865e36e02b");
                    double r = toDouble(right);
                    writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_4_10.coverage", "9a94fed9-7a6e-4546-872f-1e0791243fbc");
                    return l + r;
                }
                writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_4_10.coverage", "c33ac38b-e1a8-433d-93ff-0c8f149f1570");
                BigInteger l = toBigInteger(left);
                writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_4_10.coverage", "8276c165-a9d5-440b-b2e6-8ed4aed3c050");
                BigInteger r = toBigInteger(right);
                writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_4_10.coverage", "ba009505-f397-4a9a-b507-de8f5d913b9e");
                BigInteger result = l.add(r);
                writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_4_10.coverage", "9f3c08e0-0dc8-4147-ba26-964ea49ec730");
                return narrowBigInteger(left, right, result);
            } catch (java.lang.NumberFormatException nfe) {
                writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_4_10.coverage", "7256d5e8-8812-40d3-a123-71a1085ef4b1");
                if (left == null || right == null) {
                    writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_4_10.coverage", "42be5a0b-04f5-4ace-a214-c83c93a7cce3");
                    controlNullOperand();
                }
            }
        }
        writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_4_10.coverage", "f907fe4b-1ac8-4bdb-a72e-f054f6ce8326");
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
        writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_4_10.coverage", "4987fa7a-702a-4f94-862f-5c3612963917");
        if (left == null && right == null) {
            writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_4_10.coverage", "0300488b-d386-4c6a-932f-1e4bbfc1d869");
            return controlNullNullOperands();
        }
        writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_4_10.coverage", "c288e8b0-076e-4906-8e45-2707c58f5834");
        if (left instanceof BigDecimal || right instanceof BigDecimal) {
            writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_4_10.coverage", "2469130e-ab28-4a93-b862-a6fd833db552");
            BigDecimal l = toBigDecimal(left);
            writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_4_10.coverage", "dc55b65d-aceb-4893-981a-084bd3fd1ea1");
            BigDecimal r = toBigDecimal(right);
            writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_4_10.coverage", "766642cf-2c83-4a88-8503-7a1fead10d90");
            if (BigDecimal.ZERO.equals(r)) {
                writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_4_10.coverage", "c41ba339-dc02-4cc8-a421-ac68eeaf33b1");
                throw new ArithmeticException("/");
            }
            writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_4_10.coverage", "cf4f6f99-ee7b-4cae-a20a-6bfe801184e6");
            BigDecimal result = l.divide(r, getMathContext());
            writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_4_10.coverage", "e2ae9fed-9525-4c8e-bf41-68ffd1d3a4aa");
            return narrowBigDecimal(left, right, result);
        }
        writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_4_10.coverage", "c9a24a1e-bdb2-4052-980e-9ce9e88519ef");
        if (isFloatingPointNumber(left) || isFloatingPointNumber(right)) {
            writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_4_10.coverage", "f7021aba-8b43-4c5b-b45e-ab1c5095e66d");
            double l = toDouble(left);
            writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_4_10.coverage", "da6475b3-faa2-4176-8969-81bef5c5c093");
            double r = toDouble(right);
            writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_4_10.coverage", "3c9584d7-33d6-4dc8-8197-04ee6b4aad91");
            if (r == 0.0) {
                writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_4_10.coverage", "7536d37f-638c-49b2-a4e0-b3e6b3189c3f");
                throw new ArithmeticException("/");
            }
            writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_4_10.coverage", "345e3519-11f5-4f24-9b8d-d33c3e7a8889");
            return l / r;
        }
        writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_4_10.coverage", "8da7e844-a636-4aa9-8280-4e68331146be");
        BigInteger l = toBigInteger(left);
        writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_4_10.coverage", "c3f9348b-aa1b-4e2c-b3e6-85c59c17c3a5");
        BigInteger r = toBigInteger(right);
        writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_4_10.coverage", "cd082a6b-eabf-4bfa-a52b-2503c58f9f14");
        if (BigInteger.ZERO.equals(r)) {
            writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_4_10.coverage", "a7b741d0-1f9b-45b7-90e2-12a72f46a152");
            throw new ArithmeticException("/");
        }
        writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_4_10.coverage", "ad06f87a-7ac5-412c-be2d-7dd710faca52");
        BigInteger result = l.divide(r);
        writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_4_10.coverage", "e55ea8c1-137f-4591-bc0e-dd95762802b5");
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
        writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_4_10.coverage", "b4b4a452-157a-48bb-9936-c1bba4f1d791");
        if (left == null && right == null) {
            writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_4_10.coverage", "8b39fe06-d8d0-46fc-ba75-154331b35872");
            return controlNullNullOperands();
        }
        writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_4_10.coverage", "8ff61fa7-28e1-436c-8e21-37efce1ec93e");
        if (left instanceof BigDecimal || right instanceof BigDecimal) {
            writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_4_10.coverage", "613c1223-944a-4634-896b-2b5afea1e20a");
            BigDecimal l = toBigDecimal(left);
            writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_4_10.coverage", "81692420-d725-4d87-aec1-54f3ee44a63a");
            BigDecimal r = toBigDecimal(right);
            writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_4_10.coverage", "3622e320-49f0-47b6-9b8d-7f79f9df4787");
            if (BigDecimal.ZERO.equals(r)) {
                writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_4_10.coverage", "934a8663-86ff-47c9-9a02-88d35ef33df4");
                throw new ArithmeticException("%");
            }
            writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_4_10.coverage", "09fe7eb6-36a2-40d5-a0d2-ba892f7fbd9f");
            BigDecimal remainder = l.remainder(r, getMathContext());
            writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_4_10.coverage", "6863542d-571d-4f1b-8b9b-87f8c6a35abc");
            return narrowBigDecimal(left, right, remainder);
        }
        writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_4_10.coverage", "54097b84-f0d4-475a-aed4-db5c1033312b");
        if (isFloatingPointNumber(left) || isFloatingPointNumber(right)) {
            writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_4_10.coverage", "54849782-112f-4a29-8413-02791a035979");
            double l = toDouble(left);
            writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_4_10.coverage", "3d527540-bd2d-4571-9366-4f17e440ccdf");
            double r = toDouble(right);
            writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_4_10.coverage", "f4ad6672-d849-4802-8915-e84d422bd52e");
            if (r == 0.0) {
                writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_4_10.coverage", "50b0dd59-ad38-493e-88fb-10a424e5d58d");
                throw new ArithmeticException("%");
            }
            writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_4_10.coverage", "5673e896-c841-44ea-ac87-b33fa3df11ed");
            return l % r;
        }
        writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_4_10.coverage", "84577f9e-25f8-4bf4-b873-5afb6241bed2");
        BigInteger l = toBigInteger(left);
        writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_4_10.coverage", "1c77b0cf-6b04-4dd0-bf5d-12b647cdcb15");
        BigInteger r = toBigInteger(right);
        writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_4_10.coverage", "cfc2141e-52cc-4d51-b863-76220f08602f");
        BigInteger result = l.mod(r);
        writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_4_10.coverage", "e55cae8f-d14a-4adc-ae51-e24c4957a5ff");
        if (BigInteger.ZERO.equals(r)) {
            writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_4_10.coverage", "7af5aae3-9f1b-4d09-8cbd-8c50ccc575e5");
            throw new ArithmeticException("%");
        }
        writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_4_10.coverage", "5b0f48fa-b4b7-4cf5-bcca-6a7c9a1cb65b");
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
        writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_4_10.coverage", "2cb4fba2-c6d6-4029-9b78-def44766da9c");
        if (left == null && right == null) {
            writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_4_10.coverage", "8bbafe0f-e82a-45ce-9763-3d8b7f4aa4d3");
            return controlNullNullOperands();
        }
        writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_4_10.coverage", "b7e6b924-eeb1-4dc3-af22-00b7e91cee5c");
        if (left instanceof BigDecimal || right instanceof BigDecimal) {
            writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_4_10.coverage", "4223f61d-c4a0-4e1c-9aa7-4b663dba35ec");
            BigDecimal l = toBigDecimal(left);
            writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_4_10.coverage", "e33d2b72-0b87-499f-a0d0-441874f90466");
            BigDecimal r = toBigDecimal(right);
            writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_4_10.coverage", "effbb1d9-7bdc-48ef-bc6f-fc474a2b61b3");
            BigDecimal result = l.multiply(r, getMathContext());
            writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_4_10.coverage", "cca034b5-ae5c-41b2-8992-1a0049b886c0");
            return narrowBigDecimal(left, right, result);
        }
        writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_4_10.coverage", "dc33f9c8-fe12-4ac0-a6d3-12c847272299");
        if (isFloatingPointNumber(left) || isFloatingPointNumber(right)) {
            writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_4_10.coverage", "e3bec3bd-265d-4297-bddb-b7d1ff440556");
            double l = toDouble(left);
            writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_4_10.coverage", "df6cea61-a833-4d1c-b267-7e675ca4ccba");
            double r = toDouble(right);
            writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_4_10.coverage", "ea5ecba3-9243-497b-9f83-1e11375ca62a");
            return l * r;
        }
        writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_4_10.coverage", "d062e819-8b12-4de8-b972-c2ae2e366608");
        BigInteger l = toBigInteger(left);
        writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_4_10.coverage", "b0f92b11-3cd4-4d52-9f61-0ba3459ab717");
        BigInteger r = toBigInteger(right);
        writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_4_10.coverage", "e5acfc00-574d-497c-ae96-d4cf518ebda7");
        BigInteger result = l.multiply(r);
        writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_4_10.coverage", "7cbffc6e-128d-4ec7-8d5b-fa8d5c3f69f4");
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
        writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_4_10.coverage", "8e4dc80e-b366-4831-b6dc-5a87a5a743fc");
        if (left == null && right == null) {
            writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_4_10.coverage", "020f7bbd-ac8f-46c4-8de2-8b0b38aa6c72");
            return controlNullNullOperands();
        }
        writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_4_10.coverage", "3b70fc07-5e4a-4563-82c7-363780e68275");
        if (left instanceof BigDecimal || right instanceof BigDecimal) {
            writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_4_10.coverage", "ee796335-a2ca-4fbc-af5c-9c847a40f583");
            BigDecimal l = toBigDecimal(left);
            writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_4_10.coverage", "9d1c346d-0834-4400-9923-230ceb53a066");
            BigDecimal r = toBigDecimal(right);
            writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_4_10.coverage", "886bcc09-6e29-4325-8db4-c06ec1b6b828");
            BigDecimal result = l.subtract(r, getMathContext());
            writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_4_10.coverage", "bad8a76f-d82d-4553-a5df-f442454c9235");
            return narrowBigDecimal(left, right, result);
        }
        writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_4_10.coverage", "3d2a88df-ba36-4255-be47-9f10b1ddd1cf");
        if (isFloatingPointNumber(left) || isFloatingPointNumber(right)) {
            writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_4_10.coverage", "f6e94c87-5763-4df0-b58a-9f9f44885ba7");
            double l = toDouble(left);
            writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_4_10.coverage", "a297fe04-0420-4e32-9cf9-33be76088fd2");
            double r = toDouble(right);
            writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_4_10.coverage", "f57915e1-31b5-453d-9af8-d5abf0e09d55");
            return l - r;
        }
        writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_4_10.coverage", "6d142826-6ab3-42f7-acde-2231fe989e28");
        BigInteger l = toBigInteger(left);
        writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_4_10.coverage", "b7b7f106-c89b-4153-8f11-5815629b43b7");
        BigInteger r = toBigInteger(right);
        writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_4_10.coverage", "b188576b-9cee-47a2-a09a-5e01438224ad");
        BigInteger result = l.subtract(r);
        writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_4_10.coverage", "d1aa12d8-cbbe-497c-a69a-2ebbe9ebe6ba");
        return narrowBigInteger(left, right, result);
    }

    /**
     * Negates a value (unary minus for numbers).
     * 
     * @param val the value to negate
     * @return the negated value
     */
    public Object negate(Object val) {
        writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_4_10.coverage", "e4cfa3b9-9bc3-462d-a125-c9b7307414f6");
        if (val instanceof Integer) {
            writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_4_10.coverage", "bc809c82-ee87-4a22-8f08-e205d1ff0502");
            return -((Integer) val);
        } else if (val instanceof Double) {
            writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_4_10.coverage", "e768c0bd-9c11-4104-9de5-4a29f805bc44");
            return -((Double) val);
        } else if (val instanceof Long) {
            writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_4_10.coverage", "c199c2fb-b895-4aab-96d8-2b699750b39a");
            return -((Long) val);
        } else if (val instanceof BigDecimal) {
            writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_4_10.coverage", "660b9ff6-b220-4c44-8d75-625060b987a9");
            return ((BigDecimal) val).negate();
        } else if (val instanceof BigInteger) {
            writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_4_10.coverage", "f7f3d660-7b37-46e9-a244-2229be8ce9ee");
            return ((BigInteger) val).negate();
        } else if (val instanceof Float) {
            writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_4_10.coverage", "b189b41c-3904-4e41-95c6-b3409d6dc8c2");
            return -((Float) val);
        } else if (val instanceof Short) {
            writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_4_10.coverage", "beb9463f-8b8a-48e8-9c30-a173a0d7e517");
            return (short) -((Short) val);
        } else if (val instanceof Byte) {
            writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_4_10.coverage", "b7f99382-3a0a-4b50-8c35-14b7be027707");
            return (byte) -((Byte) val);
        } else if (val instanceof Boolean) {
            writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_4_10.coverage", "bd96522d-cff3-460d-8215-00c59b1a89f3");
            return ((Boolean) val) ? Boolean.FALSE : Boolean.TRUE;
        }
        writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_4_10.coverage", "9d382d94-17bc-45a1-9dab-02e4de4ca956");
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
        writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_4_10.coverage", "f9bd47fd-eba6-47ac-a19b-8c0d4e4018c6");
        if (value == null && container == null) {
            writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_4_10.coverage", "85fad6bc-e919-43c2-a8ee-8416ac0245e4");
            return true;
        }
        writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_4_10.coverage", "d6572fdb-9bc2-4721-964e-b573673ad779");
        if (value == null || container == null) {
            writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_4_10.coverage", "50d32188-5650-45c7-9931-848d9cd9e06b");
            return false;
        }
        writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_4_10.coverage", "6e3fabd4-e898-4d98-834d-f9eadc31e928");
        if (container instanceof java.util.regex.Pattern) {
            writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_4_10.coverage", "52e9c40e-042c-4d37-83e0-03b9e96a74a6");
            return ((java.util.regex.Pattern) container).matcher(value.toString()).matches();
        }
        writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_4_10.coverage", "c66d5a95-337d-418a-8f15-debf1d983f03");
        if (container instanceof String) {
            writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_4_10.coverage", "556ac9ee-d66d-4054-bf7b-231f8f31fe30");
            return value.toString().matches(container.toString());
        }
        writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_4_10.coverage", "c8419c0b-f33a-4d1c-a868-cfc94f1ffcc7");
        if (container instanceof Map<?, ?>) {
            writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_4_10.coverage", "b2a29304-6dd0-4125-8735-4f114cf09c61");
            if (value instanceof Map<?, ?>) {
                writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_4_10.coverage", "75c6d12f-8795-497c-81b6-ef8b633d19ee");
                return ((Map<?, ?>) container).keySet().containsAll(((Map<?, ?>) value).keySet());
            }
            writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_4_10.coverage", "5454eb04-b280-4c4b-a098-60166d0d51c6");
            return ((Map<?, ?>) container).containsKey(value);
        }
        writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_4_10.coverage", "ad54f275-42ee-4827-90e3-39c9a5c1a5e0");
        if (container instanceof Collection<?>) {
            writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_4_10.coverage", "286bdf80-502d-494a-8ecc-490592c4ae21");
            if (value instanceof Collection<?>) {
                writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_4_10.coverage", "c919e699-8dda-42f0-a8bc-c8d72bc12521");
                return ((Collection<?>) container).containsAll((Collection<?>) value);
            }
            writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_4_10.coverage", "14e187e9-db6e-450a-b04a-bd717222b136");
            return ((Collection<?>) container).contains(value);
        }
        writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_4_10.coverage", "13fdacbd-dc8a-485d-9f04-aae6bd35315f");
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
        writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_4_10.coverage", "5ec6156f-39c1-439e-bfa3-12bc4d1d004f");
        if (left == null && right == null) {
            writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_4_10.coverage", "daa08f85-d0db-4f02-bd6f-c606fcff4905");
            return true;
        }
        writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_4_10.coverage", "86c3b276-3fc3-4664-8f69-9cf99f1ca4dd");
        if (left == null || right == null) {
            writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_4_10.coverage", "c5f54d91-d5af-47e8-b80f-f187035892b4");
            return false;
        }
        writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_4_10.coverage", "6925a71d-06eb-4269-805a-b19d1c68b4fa");
        if (left instanceof String) {
            writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_4_10.coverage", "5bf00028-4725-40e1-adf0-72e303b72a06");
            return ((String) left).endsWith(toString(right));
        }
        writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_4_10.coverage", "96e253e4-495c-45a4-b55d-13ada111820e");
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
        writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_4_10.coverage", "c8c41c5f-5cc4-42ac-9301-c0234f43dac2");
        if (left == null && right == null) {
            writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_4_10.coverage", "b080efa9-dcc6-44b1-b3d3-079934b1f7ad");
            return true;
        }
        writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_4_10.coverage", "db8fd58b-885e-4edc-a521-c26b46b37558");
        if (left == null || right == null) {
            writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_4_10.coverage", "fc3cb81a-b051-4db1-bb22-635082447291");
            return false;
        }
        writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_4_10.coverage", "74293906-8f6b-4e19-87b1-25dbf74612b7");
        if (left instanceof String) {
            writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_4_10.coverage", "e873151e-f706-4898-b290-3550d9f064cf");
            return ((String) left).startsWith(toString(right));
        }
        writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_4_10.coverage", "5d9c6234-e068-473b-9941-47f8271fd5a8");
        return null;
    }

    /**
     * Check for emptyness of various types: Number, Collection, Array, Map, String.
     *
     * @param object the object to check the emptyness of
     * @return the boolean or null of there is no arithmetic solution
     */
    public Boolean isEmpty(Object object) {
        writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_4_10.coverage", "f7a77108-f2ec-4672-ac37-4d49fa42a1ac");
        if (object instanceof Number) {
            writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_4_10.coverage", "11acc85f-5cdc-4d42-8412-360423952a84");
            double d = ((Number) object).doubleValue();
            writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_4_10.coverage", "bc039241-1571-453d-88b2-abb8426bca29");
            return Double.isNaN(d) || d == 0.d ? Boolean.TRUE : Boolean.FALSE;
        }
        writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_4_10.coverage", "045cc5c3-49d9-4cb8-954f-fe75060aa12c");
        if (object instanceof String) {
            writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_4_10.coverage", "d38179c1-0153-4394-99cc-0d370c287bfc");
            return "".equals(object) ? Boolean.TRUE : Boolean.FALSE;
        }
        writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_4_10.coverage", "38a7218a-5b89-4e5f-9f4e-b6545a25031d");
        if (object.getClass().isArray()) {
            writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_4_10.coverage", "f78e4ff1-1f4d-47be-80d0-53e18855f78c");
            return Array.getLength(object) == 0 ? Boolean.TRUE : Boolean.FALSE;
        }
        writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_4_10.coverage", "0de1457e-72e7-4eb6-9ee6-166bb503a0cb");
        if (object instanceof Collection<?>) {
            writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_4_10.coverage", "1263a49e-e397-495f-a567-d96228c1ff4f");
            return ((Collection<?>) object).isEmpty() ? Boolean.TRUE : Boolean.FALSE;
        }
        writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_4_10.coverage", "8eba87b4-d8a9-417e-9aa7-bed4d2a1856e");
        if (object instanceof Map<?, ?>) {
            writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_4_10.coverage", "3e16164d-4d2f-410e-bc44-40cc4cbb4c0c");
            return ((Map<?, ?>) object).isEmpty() ? Boolean.TRUE : Boolean.FALSE;
        }
        writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_4_10.coverage", "373d4859-f6f2-40de-a9ac-0f5342be13f4");
        return null;
    }

    /**
     * Calculate the <code>size</code> of various types: Collection, Array, Map, String.
     *
     * @param object the object to get the size of
     * @return the size of object or null if there is no arithmetic solution
     */
    public Integer size(Object object) {
        writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_4_10.coverage", "d5366db4-e4e8-4349-b3ef-9678d4005793");
        if (object instanceof String) {
            writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_4_10.coverage", "88b8d079-9e71-4eba-b927-805e76def514");
            return ((String) object).length();
        }
        writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_4_10.coverage", "8e825c06-634b-40fc-b5ff-80059e355874");
        if (object.getClass().isArray()) {
            writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_4_10.coverage", "c1ec1a70-7971-4dcb-8871-5444019b4771");
            return Array.getLength(object);
        }
        writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_4_10.coverage", "6fb9eb9b-8586-477e-8d8a-0ad1b17d2f0b");
        if (object instanceof Collection<?>) {
            writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_4_10.coverage", "3a85dac3-cbbb-4d78-8b93-4323d3226bcf");
            return ((Collection<?>) object).size();
        }
        writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_4_10.coverage", "71dac1d9-9474-4eb7-a523-c6837263cc8d");
        if (object instanceof Map<?, ?>) {
            writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_4_10.coverage", "5cd6c54a-9c45-46d2-89b7-a5f252c0211e");
            return ((Map<?, ?>) object).size();
        }
        writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_4_10.coverage", "7bb2c47e-9a98-44f9-baca-da50dcad9289");
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
        writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_4_10.coverage", "c0770b40-2268-428f-9a4f-1294e754f51b");
        long l = toLong(left);
        writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_4_10.coverage", "f5cc258d-341b-4d96-a214-59143551bb94");
        long r = toLong(right);
        writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_4_10.coverage", "8bc2fe3b-d59c-4a87-a1be-d0f95705d383");
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
        writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_4_10.coverage", "b3a8c6ea-fca8-4ef3-b79a-4532774f58f0");
        long l = toLong(left);
        writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_4_10.coverage", "6cfb4bf3-df66-44a6-b57c-de488b0020b2");
        long r = toLong(right);
        writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_4_10.coverage", "785f5e94-8b13-44f1-bea8-e738c66c0d4e");
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
        writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_4_10.coverage", "6aab9576-ad03-4630-b852-318cb8c71261");
        long l = toLong(left);
        writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_4_10.coverage", "415bbe82-2d80-4356-8282-35e745ea9b87");
        long r = toLong(right);
        writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_4_10.coverage", "110acc83-8058-401e-83cb-c867f70b3b9e");
        return l ^ r;
    }

    /**
     * Performs a bitwise complement.
     * 
     * @param val the operand
     * @return ~val
     */
    public Object complement(Object val) {
        writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_4_10.coverage", "33d465b0-5154-44f9-b437-cd1e4f1c18e3");
        long l = toLong(val);
        writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_4_10.coverage", "fbfbce50-5cc2-44a5-b8bd-bf8a6dfb909c");
        return ~l;
    }

    /**
     * Performs a logical not.
     * 
     * @param val the operand
     * @return !val
     */
    public Object not(Object val) {
        writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_4_10.coverage", "75023805-d714-4fb4-a1fa-7830803860ce");
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
        writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_4_10.coverage", "ba49a7c5-f7b6-48fd-b35d-0882811b72cc");
        if (left != null && right != null) {
            writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_4_10.coverage", "b8d62b5f-0261-4282-a29a-5e6cb020a5a4");
            if (left instanceof BigDecimal || right instanceof BigDecimal) {
                writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_4_10.coverage", "fbdb3903-212c-4d88-9912-c3ba1a079814");
                BigDecimal l = toBigDecimal(left);
                writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_4_10.coverage", "b1c0ee9d-9c72-417f-a55f-b156c5bf66a0");
                BigDecimal r = toBigDecimal(right);
                writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_4_10.coverage", "c96ca0a5-0498-4c56-a7fb-bc149531d2c5");
                return l.compareTo(r);
            } else if (left instanceof BigInteger || right instanceof BigInteger) {
                writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_4_10.coverage", "2c0f9207-997e-4c67-8a45-720e2102b4d2");
                BigInteger l = toBigInteger(left);
                writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_4_10.coverage", "e5ffee5b-0aef-464e-9a1d-79c818ebad5c");
                BigInteger r = toBigInteger(right);
                writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_4_10.coverage", "a46932b0-72af-4e6b-b9f5-bb8eb971350d");
                return l.compareTo(r);
            } else if (isFloatingPoint(left) || isFloatingPoint(right)) {
                writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_4_10.coverage", "122ea152-04e4-4d55-ae65-474fdf4bfe80");
                double lhs = toDouble(left);
                writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_4_10.coverage", "6dc83e5a-28f4-45f6-bf74-2c2021b7d17e");
                double rhs = toDouble(right);
                writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_4_10.coverage", "7fa164a8-6a6d-44bb-80e5-ad9d80bc391a");
                if (Double.isNaN(lhs)) {
                    writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_4_10.coverage", "a8acf950-d5f7-40f0-b45f-ddaa6b12a39a");
                    if (Double.isNaN(rhs)) {
                        writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_4_10.coverage", "5170e363-edf4-402c-9a8a-6119c0434ecf");
                        return 0;
                    } else {
                        writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_4_10.coverage", "7612637f-fe9f-41b5-ab89-5f93e63c79ec");
                        return -1;
                    }
                } else if (Double.isNaN(rhs)) {
                    writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_4_10.coverage", "5dd3fe06-87c5-48ea-9f71-7a438b245aa9");
                    return +1;
                } else if (lhs < rhs) {
                    writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_4_10.coverage", "09d1d81d-fba0-4add-b844-5d45ff4b6095");
                    return -1;
                } else if (lhs > rhs) {
                    writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_4_10.coverage", "d0506e25-1b17-47fd-a313-8c8fa1d68d67");
                    return +1;
                } else {
                    writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_4_10.coverage", "6d0f1344-50c7-46c0-a6c0-1ead49944667");
                    return 0;
                }
            } else if (isNumberable(left) || isNumberable(right)) {
                writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_4_10.coverage", "d6e66474-5b37-4ab3-81a8-7d9158748597");
                long lhs = toLong(left);
                writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_4_10.coverage", "92b2bab8-32f8-4bd4-b8ca-2f766d5596d0");
                long rhs = toLong(right);
                writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_4_10.coverage", "3cfe0e82-ca52-4dc5-a4d6-1c098038b11b");
                if (lhs < rhs) {
                    writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_4_10.coverage", "9f4755f9-336e-4042-a10b-22d7939b0521");
                    return -1;
                } else if (lhs > rhs) {
                    writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_4_10.coverage", "93e3f0ac-2ee4-4f7b-82a2-d55acf460e20");
                    return +1;
                } else {
                    writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_4_10.coverage", "5942a74d-6dde-412d-9b22-c485263deba4");
                    return 0;
                }
            } else if (left instanceof String || right instanceof String) {
                writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_4_10.coverage", "7e4a39d3-549c-4253-9d95-ecae7d763de3");
                return toString(left).compareTo(toString(right));
            } else if ("==".equals(operator)) {
                writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_4_10.coverage", "8b9eb3d6-9d7f-4850-a7a1-da2a885dfab6");
                return left.equals(right) ? 0 : -1;
            } else if (left instanceof Comparable<?>) {
                writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_4_10.coverage", "15964819-daf8-4b4c-a8e9-6049cd95d188");
                @SuppressWarnings("unchecked") final Comparable<Object> comparable = (Comparable<Object>) left;
                writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_4_10.coverage", "ef40af6c-da45-4123-9c9b-c2f6afed38f7");
                return comparable.compareTo(right);
            } else if (right instanceof Comparable<?>) {
                writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_4_10.coverage", "23d83bb3-2bc1-4667-8cae-2373589c4014");
                @SuppressWarnings("unchecked") final Comparable<Object> comparable = (Comparable<Object>) right;
                writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_4_10.coverage", "d6402623-76b9-460f-874d-139797d2ed23");
                return comparable.compareTo(left);
            }
        }
        writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_4_10.coverage", "30dc9ec4-d662-45f6-ae34-2fcd07d0ef05");
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
        writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_4_10.coverage", "03d4529f-0cbb-43ac-847a-d5e886d478cd");
        if (left == right) {
            writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_4_10.coverage", "1df82125-eb84-4bd4-81bf-1643f03d099e");
            return true;
        } else if (left == null || right == null) {
            writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_4_10.coverage", "003de8bc-d14d-4e4f-8dd4-fa20adcf1460");
            return false;
        } else if (left instanceof Boolean || right instanceof Boolean) {
            writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_4_10.coverage", "98c8ee4b-258e-4960-b2a9-4d5942fc1aa0");
            return toBoolean(left) == toBoolean(right);
        } else {
            writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_4_10.coverage", "05ad6c49-9a48-4785-bf19-5cb8c52e61eb");
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
        writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_4_10.coverage", "4317b9e7-c2ef-43e1-95d4-9c7209200a70");
        if ((left == right) || (left == null) || (right == null)) {
            writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_4_10.coverage", "8b6c1abb-b39b-4482-82d3-b695495400f9");
            return false;
        } else {
            writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_4_10.coverage", "60a82eb7-a96d-4f4b-a2ee-3861523e383f");
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
        writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_4_10.coverage", "a7154118-441a-40bc-ad34-3e9796fb1fa6");
        if ((left == right) || left == null || right == null) {
            writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_4_10.coverage", "2e86cad2-cf3e-43b7-8614-789d49143d0a");
            return false;
        } else {
            writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_4_10.coverage", "3e403364-efc3-4646-8446-e59c5bdc7bf5");
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
        writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_4_10.coverage", "cb8a4287-8188-4aec-a60a-f5a826832195");
        if (left == right) {
            writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_4_10.coverage", "378c9666-06cb-4641-835e-2f5489c53138");
            return true;
        } else if (left == null || right == null) {
            writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_4_10.coverage", "0a6fb2fa-c4ef-4094-b071-159643c0247b");
            return false;
        } else {
            writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_4_10.coverage", "380e3c4e-2072-4edc-bfe3-ba04d302dc81");
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
        writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_4_10.coverage", "acbf6217-d629-45ea-a24d-e69d1331ffbb");
        if (left == right) {
            writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_4_10.coverage", "4ea62934-6655-4f3b-8238-cad9c2e66e5d");
            return true;
        } else if (left == null || right == null) {
            writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_4_10.coverage", "3e2680b3-a7eb-4527-a6e4-8b879b4254e4");
            return false;
        } else {
            writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_4_10.coverage", "8a0ae3b2-fe97-40a7-9ada-708a2f64e257");
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
        writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_4_10.coverage", "bf845242-41af-4484-9876-ef8229c4aa97");
        if (val == null) {
            writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_4_10.coverage", "d4d70b99-60d9-46aa-88ac-96044e5b87c0");
            controlNullOperand();
            writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_4_10.coverage", "69a786c3-62ef-4099-ab54-46fba5721d53");
            return false;
        } else if (val instanceof Boolean) {
            writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_4_10.coverage", "b5141f52-cd1e-458b-88a9-3f387a5bc3ed");
            return ((Boolean) val);
        } else if (val instanceof Number) {
            writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_4_10.coverage", "e79fda34-aac7-41e0-a495-c549b331faee");
            double number = toDouble(val);
            writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_4_10.coverage", "00eacec4-ee8d-43db-a863-84b1ea865064");
            return !Double.isNaN(number) && number != 0.d;
        } else if (val instanceof String) {
            writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_4_10.coverage", "7ad64b77-6c4b-4b69-8dc7-14147b0044c0");
            String strval = val.toString();
            writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_4_10.coverage", "5c240192-1647-42f3-bb96-ac9bf8e1709f");
            return strval.length() > 0 && !"false".equals(strval);
        } else {
            writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_4_10.coverage", "9b09a4eb-8f62-44a8-b6a1-ab5a33a7f2e3");
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
        writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_4_10.coverage", "d73948e6-3efa-4c86-9bd6-9954c8ad5a9b");
        if (val == null) {
            writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_4_10.coverage", "123fe840-6fb3-4ca6-9961-6623bb0220e7");
            controlNullOperand();
            writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_4_10.coverage", "3b2a07b3-6633-49c1-94f6-8da51da1201f");
            return 0;
        } else if (val instanceof Double) {
            writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_4_10.coverage", "1e050dd0-705d-468f-a632-ba0c972b29e1");
            Double dval = (Double) val;
            writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_4_10.coverage", "a8a65020-8dca-4ae2-b56e-900d073815f1");
            if (Double.isNaN(dval)) {
                writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_4_10.coverage", "44096053-ff16-445a-8fcb-6dd253e46974");
                return 0;
            } else {
                writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_4_10.coverage", "c96adfdb-bc9f-4fab-b039-6b58dce04b88");
                return dval.intValue();
            }
        } else if (val instanceof Number) {
            writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_4_10.coverage", "e28b2e48-52eb-41af-9f1e-86bd1336f18b");
            return ((Number) val).intValue();
        } else if (val instanceof String) {
            writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_4_10.coverage", "1a7f4816-0c8c-4b3e-b238-362cba4bed44");
            if ("".equals(val)) {
                writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_4_10.coverage", "be5d6585-af98-4698-9b1f-d490d8379800");
                return 0;
            }
            writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_4_10.coverage", "28f90e6c-0853-458d-91e9-08da310067eb");
            return Integer.parseInt((String) val);
        } else if (val instanceof Boolean) {
            writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_4_10.coverage", "1a12f79f-7ea2-40a4-a8fc-ab5c9c484e77");
            return ((Boolean) val) ? 1 : 0;
        } else if (val instanceof Character) {
            writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_4_10.coverage", "cabf877c-6865-462a-91bd-2c70bd5fff2a");
            return ((Character) val);
        }
        writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_4_10.coverage", "f43362f3-54e1-415b-8471-df942e1a2731");
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
        writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_4_10.coverage", "52fe6e78-d04d-4e31-b469-0e9eb510824e");
        if (val == null) {
            writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_4_10.coverage", "5113f6ec-1f3e-414e-8e5a-c2eeaa8222b8");
            controlNullOperand();
            writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_4_10.coverage", "81953fcd-7576-4d21-a88e-c123b46f1ea1");
            return 0L;
        } else if (val instanceof Double) {
            writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_4_10.coverage", "2715de03-e36d-4408-93ce-87d255bf118e");
            Double dval = (Double) val;
            writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_4_10.coverage", "112dbd34-9f7c-4be8-a1f7-ab8ca5c6a9ca");
            if (Double.isNaN(dval)) {
                writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_4_10.coverage", "c84391fc-94fd-4511-a618-71207ca725d9");
                return 0L;
            } else {
                writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_4_10.coverage", "414e31ef-e7aa-4375-a01b-ea94d01a1f8f");
                return dval.longValue();
            }
        } else if (val instanceof Number) {
            writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_4_10.coverage", "88960142-b8ca-4596-9ac0-2dea884ec361");
            return ((Number) val).longValue();
        } else if (val instanceof String) {
            writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_4_10.coverage", "fc514205-9f11-4e11-8cb1-46e893a53271");
            if ("".equals(val)) {
                writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_4_10.coverage", "ab7b5a65-5957-42a0-92ff-62e49eda93bb");
                return 0L;
            } else {
                writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_4_10.coverage", "a0a11814-c3ad-4763-b59b-c4ba24e51a0a");
                return Long.parseLong((String) val);
            }
        } else if (val instanceof Boolean) {
            writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_4_10.coverage", "74e5b1fd-9b61-417d-a985-f6f01a29bd94");
            return ((Boolean) val) ? 1L : 0L;
        } else if (val instanceof Character) {
            writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_4_10.coverage", "22ba6931-2669-4719-b340-87d4877c54bd");
            return ((Character) val);
        }
        writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_4_10.coverage", "a94d8089-152a-468c-a408-48fe60736b5e");
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
        writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_4_10.coverage", "8827d954-0a1f-4f54-bd7b-86f167926877");
        if (val == null) {
            writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_4_10.coverage", "8097eba1-dbc5-4c5d-9c24-da60f8c47812");
            controlNullOperand();
            writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_4_10.coverage", "b703b5fb-db47-4258-8017-dce82b0b121b");
            return BigInteger.ZERO;
        } else if (val instanceof BigInteger) {
            writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_4_10.coverage", "ef3ae79f-3ef2-4576-9d81-5a246c7b1aba");
            return (BigInteger) val;
        } else if (val instanceof Double) {
            writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_4_10.coverage", "52515f8d-c455-4c7e-8006-75bd0afea711");
            Double dval = (Double) val;
            writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_4_10.coverage", "b4b8e4bb-bbec-41ef-b6e9-82d83ad36499");
            if (Double.isNaN(dval)) {
                writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_4_10.coverage", "497b4435-de09-4a74-9624-e30043257806");
                return BigInteger.ZERO;
            } else {
                writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_4_10.coverage", "970d5694-c163-4116-9e16-e05f04cf9473");
                return BigInteger.valueOf(dval.longValue());
            }
        } else if (val instanceof BigDecimal) {
            writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_4_10.coverage", "b28d9969-be37-4bc1-a917-d21a00b1889f");
            return ((BigDecimal) val).toBigInteger();
        } else if (val instanceof Number) {
            writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_4_10.coverage", "8e9e5583-225d-4cdf-8f21-a9e6972ff0c7");
            return BigInteger.valueOf(((Number) val).longValue());
        } else if (val instanceof Boolean) {
            writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_4_10.coverage", "b91fb37f-da77-4e00-979e-126f17128816");
            return BigInteger.valueOf(((Boolean) val) ? 1L : 0L);
        } else if (val instanceof String) {
            writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_4_10.coverage", "76a4b969-4a7c-4bab-8fe2-24435cc092b7");
            String string = (String) val;
            writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_4_10.coverage", "19158638-1509-48c7-ab17-7f2e4026cff7");
            if ("".equals(string)) {
                writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_4_10.coverage", "388935ce-38e3-4856-9c12-00f01325f95d");
                return BigInteger.ZERO;
            } else {
                writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_4_10.coverage", "d63eb762-8c56-4a40-ae46-c7aa9da0604e");
                return new BigInteger(string);
            }
        } else if (val instanceof Character) {
            writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_4_10.coverage", "a8f78980-0842-4fa8-b8a3-e339b8e135bc");
            int i = ((Character) val);
            writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_4_10.coverage", "6b8930bc-62a1-48b5-8a3e-6ddf9da8064f");
            return BigInteger.valueOf(i);
        }
        writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_4_10.coverage", "726f88a6-9ef0-47cc-a1a3-357db55a8855");
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
        writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_4_10.coverage", "f562abb1-e6e4-4422-aa94-e7a40e226770");
        if (val instanceof BigDecimal) {
            writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_4_10.coverage", "486c7c50-013c-41af-bc45-6b69e7c71540");
            return roundBigDecimal((BigDecimal) val);
        } else if (val == null) {
            writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_4_10.coverage", "300a7474-93cd-4279-95ff-69d85292526f");
            controlNullOperand();
            writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_4_10.coverage", "7d90863b-ed44-48a6-ba2a-b39b6b924e84");
            return BigDecimal.ZERO;
        } else if (val instanceof Double) {
            writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_4_10.coverage", "5e9df8ec-fb11-4059-80cb-768c68a8213b");
            if (Double.isNaN(((Double) val))) {
                writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_4_10.coverage", "66987534-cda8-40df-b458-3d123ec13b20");
                return BigDecimal.ZERO;
            } else {
                writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_4_10.coverage", "b7be542d-4124-41e5-af4e-41419629689c");
                return roundBigDecimal(new BigDecimal(val.toString(), getMathContext()));
            }
        } else if (val instanceof Number) {
            writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_4_10.coverage", "0a39ce97-734a-41b9-b9b2-030d28759cad");
            return roundBigDecimal(new BigDecimal(val.toString(), getMathContext()));
        } else if (val instanceof Boolean) {
            writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_4_10.coverage", "6694b59d-a6dc-456d-86d1-91e248024032");
            return BigDecimal.valueOf(((Boolean) val) ? 1. : 0.);
        } else if (val instanceof String) {
            writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_4_10.coverage", "626a540f-1434-4b5b-a1fd-eb8ac04437ed");
            String string = (String) val;
            writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_4_10.coverage", "9ccb0db6-af8d-4c6a-8d64-b2b8a4a03ec3");
            if ("".equals(string)) {
                writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_4_10.coverage", "092f6ae9-5305-4473-8315-4d3b10ab34f7");
                return BigDecimal.ZERO;
            }
            writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_4_10.coverage", "c6582dff-66ca-45ca-a3c5-d85721acdc36");
            return roundBigDecimal(new BigDecimal(string, getMathContext()));
        } else if (val instanceof Character) {
            writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_4_10.coverage", "24bbb5a0-33a8-4f6e-80a0-7d284d78bb2a");
            int i = ((Character) val);
            writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_4_10.coverage", "3849f795-fe5b-4275-9eb6-43aec9c6b46e");
            return new BigDecimal(i);
        }
        writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_4_10.coverage", "cc4ec50f-adb1-487f-b5d7-44f1ba01b09c");
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
        writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_4_10.coverage", "fb620f4a-06d4-4b5b-882d-a69d7917d46e");
        if (val == null) {
            writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_4_10.coverage", "2132a4bf-244b-4a64-8f6d-adad6ca5683f");
            controlNullOperand();
            writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_4_10.coverage", "5f901dd4-f0a2-4bcd-bfce-688122c7d4ad");
            return 0;
        } else if (val instanceof Double) {
            writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_4_10.coverage", "24178f79-eb79-43e3-be0e-a1da92bf039e");
            return ((Double) val);
        } else if (val instanceof Number) {
            writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_4_10.coverage", "8809cdcb-e3b2-412f-8918-565b992414a8");
            return Double.parseDouble(String.valueOf(val));
        } else if (val instanceof Boolean) {
            writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_4_10.coverage", "da0c3c54-616f-4fb9-8afa-24d24bf3dfde");
            return ((Boolean) val) ? 1. : 0.;
        } else if (val instanceof String) {
            writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_4_10.coverage", "209418c9-a387-49a0-ac56-70a760a6f94b");
            String string = (String) val;
            writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_4_10.coverage", "e4fc0dfe-b21f-480f-8e34-93011639a98c");
            if ("".equals(string)) {
                writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_4_10.coverage", "b709f0ed-0299-4830-b242-59d30dc3d498");
                return Double.NaN;
            } else {
                writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_4_10.coverage", "65d552ca-f989-420e-8ac6-a1648a11547b");
                return Double.parseDouble(string);
            }
        } else if (val instanceof Character) {
            writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_4_10.coverage", "c167a793-4bad-4892-8416-9d889f115754");
            int i = ((Character) val);
            writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_4_10.coverage", "2ede0a6f-23cd-4b50-9d28-7d4bfcb27ff6");
            return i;
        }
        writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_4_10.coverage", "5b6ee1db-badd-41c9-8969-8035f3080736");
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
        writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_4_10.coverage", "60831eb4-7c3f-42bd-9155-bfa1a76d4542");
        if (val == null) {
            writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_4_10.coverage", "b4b45e44-cd30-4579-8e5b-959df2688c3f");
            controlNullOperand();
            writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_4_10.coverage", "e74b9f48-514b-4fb9-8328-9d700f460ee9");
            return "";
        } else if (val instanceof Double) {
            writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_4_10.coverage", "69ee1156-d657-4623-b99b-753254d6a06a");
            Double dval = (Double) val;
            writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_4_10.coverage", "7e5856a1-7424-45cf-8a42-88a5214f0842");
            if (Double.isNaN(dval)) {
                writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_4_10.coverage", "b1c0599a-7f29-4347-b882-3ada3f41ea18");
                return "";
            } else {
                writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_4_10.coverage", "5735cd04-bf29-4949-aeef-939d74873dd1");
                return dval.toString();
            }
        } else {
            writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_4_10.coverage", "e7e6c4e5-83db-45b3-a41b-ad1b8cbcd44e");
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
        writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_4_10.coverage", "db171882-50e5-4195-9b53-4e8a19ed5fd9");
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
        writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_4_10.coverage", "84f01702-421a-4481-82fc-7d5577d4629d");
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
        writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_4_10.coverage", "5a5893f9-8c33-48a3-9c88-0b7502046776");
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
        writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_4_10.coverage", "b93eb4a9-266e-441b-9d02-ca1372df6ab9");
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
        writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_4_10.coverage", "7a6ca415-b4ad-4539-b17b-4654253e6464");
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
