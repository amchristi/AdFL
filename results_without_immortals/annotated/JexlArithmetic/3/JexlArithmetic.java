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
        writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_3_10.coverage", "e39ee127-8bcd-450d-a384-8f22dd7cf0b2");
        boolean ostrict = options.isStrictArithmetic() == null ? this.strict : options.isStrictArithmetic();
        writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_3_10.coverage", "2b5c5c91-e3a5-4fe2-bb81-b701690f3148");
        MathContext bigdContext = options.getArithmeticMathContext();
        writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_3_10.coverage", "f67f333a-f596-4f93-b872-8e7d790caf8e");
        if (bigdContext == null) {
            writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_3_10.coverage", "4d47e373-fc9b-4dda-a711-05f55bb09b88");
            bigdContext = mathContext;
        }
        writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_3_10.coverage", "f3eb3f76-09f5-4552-8e30-4d26440bb648");
        int bigdScale = options.getArithmeticMathScale();
        writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_3_10.coverage", "9b5d64f1-f688-47df-b6cb-b6d38b39db3c");
        if (bigdScale == Integer.MIN_VALUE) {
            writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_3_10.coverage", "0349f804-d1e1-40cb-95ff-b5ebbe6e6994");
            bigdScale = mathScale;
        }
        writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_3_10.coverage", "9edd3284-2765-45b5-8ff8-e525add40899");
        if ((ostrict != this.strict) || bigdScale != this.mathScale || bigdContext != this.mathContext) {
            writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_3_10.coverage", "6fbaa4fb-a751-4b8f-b965-afe46bc9117d");
            return new JexlArithmetic(ostrict, bigdContext, bigdScale);
        } else {
            writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_3_10.coverage", "85cfda81-02e2-442f-a61c-6650ee9da465");
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
        writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_3_10.coverage", "f5f18ba9-f75b-4fa7-a76d-496f08520366");
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
        writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_3_10.coverage", "85275db0-96f0-45bc-b2c7-99f2cf5e352e");
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
        writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_3_10.coverage", "c792e4c2-df12-4f25-a57e-c9a54f974037");
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
        writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_3_10.coverage", "88252adb-1684-406a-adf1-c93cb16b58d1");
        final long lfrom = toLong(from);
        writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_3_10.coverage", "d13e2a83-cf26-42f9-a94b-5dd767fb1358");
        final long lto = toLong(to);
        writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_3_10.coverage", "7a426b16-d72d-4739-afed-ffc56c7c1051");
        if ((lfrom >= Integer.MIN_VALUE && lfrom <= Integer.MAX_VALUE) && (lto >= Integer.MIN_VALUE && lto <= Integer.MAX_VALUE)) {
            writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_3_10.coverage", "2464d7e3-d895-46f1-b04f-e69d028b3b61");
            return org.apache.commons.jexl3.internal.IntegerRange.create((int) lfrom, (int) lto);
        } else {
            writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_3_10.coverage", "fb8d6c4a-774a-4a0f-874a-a8d97b57efdb");
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
        writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_3_10.coverage", "a79445dc-e784-4938-8782-223efd1773d3");
        return this.strict;
    }

    /**
     * The MathContext instance used for +,-,/,*,% operations on big decimals.
     * 
     * @return the math context
     */
    public MathContext getMathContext() {
        writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_3_10.coverage", "10a043a6-97aa-4b2a-af03-cb25e7b17007");
        return mathContext;
    }

    /**
     * The BigDecimal scale used for comparison and coericion operations.
     * 
     * @return the scale
     */
    public int getMathScale() {
        writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_3_10.coverage", "3eb399d8-71ff-4439-b951-09644f9af566");
        return mathScale;
    }

    /**
     * Ensure a big decimal is rounded by this arithmetic scale and rounding mode.
     * 
     * @param number the big decimal to round
     * @return the rounded big decimal
     */
    protected BigDecimal roundBigDecimal(final BigDecimal number) {
        writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_3_10.coverage", "bb4f3e1f-bf9d-4f6a-9ad3-d751841f7bb9");
        int mscale = getMathScale();
        writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_3_10.coverage", "7e0d534b-59de-4ecb-8ec0-5f8fc962b0dd");
        if (mscale >= 0) {
            writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_3_10.coverage", "a683c3ba-8861-48e1-8c1c-de3a1477c5a7");
            return number.setScale(mscale, getMathContext().getRoundingMode());
        } else {
            writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_3_10.coverage", "08776218-3ff4-4b10-abd0-12947ff73b11");
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
        writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_3_10.coverage", "2b62a1fe-c6ba-4051-a467-7dbdc5ae7583");
        if (isStrict()) {
            writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_3_10.coverage", "8adcbf80-4582-445e-bcbb-4f03015e39ac");
            throw new NullOperand();
        }
        writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_3_10.coverage", "aab35c7b-e3e2-4aba-be56-f909468e8082");
        return 0;
    }

    /**
     * Throw a NPE if arithmetic is strict.
     * 
     * @throws ArithmeticException if strict
     */
    protected void controlNullOperand() {
        writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_3_10.coverage", "0f1c6c31-21a1-49e1-b188-c02a38a3d55a");
        if (isStrict()) {
            writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_3_10.coverage", "aa1b6e22-47c8-4086-b5e8-582516985d57");
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
        writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_3_10.coverage", "5fd9cbe9-f50a-428f-80c1-cf362640ae48");
        if (val instanceof Float || val instanceof Double) {
            writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_3_10.coverage", "ac4e42a7-c94a-4fab-8adf-930083e5a082");
            return true;
        }
        writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_3_10.coverage", "4166d9a0-d17d-4801-b28c-36acb0af7cb9");
        if (val instanceof String) {
            writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_3_10.coverage", "45e323aa-458a-4eb9-bf84-3b9b01dbb5ef");
            String str = (String) val;
            writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_3_10.coverage", "b2cd992a-006b-444b-abe7-e60acff5803a");
            for (int c = 0; c < str.length(); ++c) {
                writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_3_10.coverage", "13b624de-80c6-4382-afff-ee9fd87af538");
                char ch = str.charAt(c);
                writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_3_10.coverage", "2007d3e4-3c36-4646-a0aa-13da79b9e839");
                if (ch == '.' || ch == 'E' || ch == 'e') {
                    writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_3_10.coverage", "04bcf6d0-4228-4e92-afcb-d98eb5a76f76");
                    return FLOAT_PATTERN.matcher(str).matches();
                }
                writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_3_10.coverage", "abd281b3-c1a2-4613-add3-bd8f6736a5da");
                if (ch != '+' && ch != '-' && ch < '0' && ch > '9') {
                    writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_3_10.coverage", "f090b0c1-95b3-4581-bd79-c100f3758cca");
                    break;
                }
            }
        }
        writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_3_10.coverage", "d3ad0e52-d20e-4f3b-a24d-1744bfd82d6f");
        return false;
    }

    /**
     * Is Object a floating point number.
     *
     * @param o Object to be analyzed.
     * @return true if it is a Float or a Double.
     */
    protected boolean isFloatingPoint(final Object o) {
        writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_3_10.coverage", "565d6d32-14ad-46f5-a56b-bd974353dfbc");
        return o instanceof Float || o instanceof Double;
    }

    /**
     * Is Object a whole number.
     *
     * @param o Object to be analyzed.
     * @return true if Integer, Long, Byte, Short or Character.
     */
    protected boolean isNumberable(final Object o) {
        writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_3_10.coverage", "ef672584-c0d5-4dca-9fbe-36f1f31f1173");
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
        writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_3_10.coverage", "355fb2cb-6243-43b6-b045-d480837d9617");
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
        writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_3_10.coverage", "c934d7e8-8849-45c6-92d5-ac91a59fab9b");
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
        writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_3_10.coverage", "4cfd6ac0-d7f7-4dae-8bc3-6bb81db6789b");
        if (original == null) {
            writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_3_10.coverage", "25216898-c6ff-41f8-bf3f-804b9e58a1a8");
            return null;
        }
        writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_3_10.coverage", "ab6b8010-143d-45c3-b3b5-03b5aed45f66");
        Number result = original;
        writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_3_10.coverage", "d7341dcb-4061-4beb-b1b9-0aa3c95b9680");
        if (original instanceof BigDecimal) {
            writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_3_10.coverage", "9dab6436-007e-4844-b670-70c9a06f0d08");
            BigDecimal bigd = (BigDecimal) original;
            writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_3_10.coverage", "4a8b45a8-360a-4041-a524-e6e1b8ee86b2");
            if (bigd.compareTo(BIGD_DOUBLE_MAX_VALUE) > 0 || bigd.compareTo(BIGD_DOUBLE_MIN_VALUE) < 0) {
                writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_3_10.coverage", "e06c7e85-ace3-475d-8e02-a8e9312f5172");
                return original;
            } else {
                writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_3_10.coverage", "128bf1a2-fc60-4b97-a5ee-4bb5fc15743e");
                try {
                    writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_3_10.coverage", "d1bfa1d3-4292-4b72-a0f5-b44d753dc588");
                    long l = bigd.longValueExact();
                    writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_3_10.coverage", "864a5c22-34c1-47a6-bab8-25b187f311ac");
                    if (narrowAccept(narrow, Integer.class) && l <= Integer.MAX_VALUE && l >= Integer.MIN_VALUE) {
                        writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_3_10.coverage", "b46e500b-ec83-4356-b34c-dca47998ff32");
                        return (int) l;
                    } else if (narrowAccept(narrow, Long.class)) {
                        writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_3_10.coverage", "d488a098-af54-44d7-833c-b1141c1dd9d1");
                        return l;
                    }
                } catch (ArithmeticException xa) {
                }
            }
        }
        writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_3_10.coverage", "11121600-f58b-424a-9581-f33276a9e724");
        if (original instanceof Double || original instanceof Float) {
            writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_3_10.coverage", "d592aaf1-6282-4f96-a798-5c2e6cecb00a");
            double value = original.doubleValue();
            writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_3_10.coverage", "ded13249-6187-4731-a449-5e7b94cae4b2");
            if (narrowAccept(narrow, Float.class) && value <= Float.MAX_VALUE && value >= Float.MIN_VALUE) {
                writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_3_10.coverage", "5a285896-e860-4277-bef0-9aaabd24cd96");
                result = result.floatValue();
            }
        } else {
            writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_3_10.coverage", "3237c599-4846-4727-b051-f27126517d24");
            if (original instanceof BigInteger) {
                writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_3_10.coverage", "fd6c6969-1414-4b17-a490-b3ca08c82fec");
                BigInteger bigi = (BigInteger) original;
                writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_3_10.coverage", "dee66c7d-8cfc-4b0e-bdde-2e5b65720732");
                if (bigi.compareTo(BIGI_LONG_MAX_VALUE) > 0 || bigi.compareTo(BIGI_LONG_MIN_VALUE) < 0) {
                    writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_3_10.coverage", "fa075bfb-8858-4d61-a090-281c703488ed");
                    return original;
                }
            }
            writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_3_10.coverage", "acf279f6-dd87-4774-80fb-c71b38a1e396");
            long value = original.longValue();
            writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_3_10.coverage", "e1b8fba2-5193-4d67-86e1-b3687da2b031");
            if (narrowAccept(narrow, Byte.class) && value <= Byte.MAX_VALUE && value >= Byte.MIN_VALUE) {
                writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_3_10.coverage", "3dfdbeb0-cdd5-44f0-867f-fe0a01836ece");
                result = (byte) value;
            } else if (narrowAccept(narrow, Short.class) && value <= Short.MAX_VALUE && value >= Short.MIN_VALUE) {
                writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_3_10.coverage", "268bbaca-17a0-4f4f-a352-efeebdf4cd84");
                result = (short) value;
            } else if (narrowAccept(narrow, Integer.class) && value <= Integer.MAX_VALUE && value >= Integer.MIN_VALUE) {
                writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_3_10.coverage", "09cbece1-3e09-4a91-9b08-b7d9da3bef13");
                result = (int) value;
            }
        }
        writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_3_10.coverage", "922787dd-778b-4479-966e-3cab002f93aa");
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
        writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_3_10.coverage", "55a69e2d-0738-4817-bed3-efa082cffd04");
        if (!(lhs instanceof BigInteger || rhs instanceof BigInteger) && bigi.compareTo(BIGI_LONG_MAX_VALUE) <= 0 && bigi.compareTo(BIGI_LONG_MIN_VALUE) >= 0) {
            writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_3_10.coverage", "36b0f733-3526-4f4c-93ac-c530a228f501");
            long l = bigi.longValue();
            writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_3_10.coverage", "346201ce-d704-4c9e-b4b0-3fd4a3569af9");
            if (!(lhs instanceof Long || rhs instanceof Long) && l <= Integer.MAX_VALUE && l >= Integer.MIN_VALUE) {
                writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_3_10.coverage", "c28829bc-1a27-4e7d-8b6a-ac515e2c79b8");
                return (int) l;
            }
            writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_3_10.coverage", "e7b04572-b62d-452b-8e33-b94a7c6aaa8a");
            return l;
        }
        writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_3_10.coverage", "c7835d24-408b-47b5-8501-bd159640a63b");
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
        writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_3_10.coverage", "08fd2254-5432-4848-b540-3df2709a1d54");
        if (isNumberable(lhs) || isNumberable(rhs)) {
            writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_3_10.coverage", "174cc0ea-0aeb-49fa-b030-5510b0b8e094");
            try {
                writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_3_10.coverage", "c5a22afb-f4e8-4fa4-86e4-8f7399966cf2");
                long l = bigd.longValueExact();
                writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_3_10.coverage", "e8e015b3-e123-485b-b49c-37c14fb0055e");
                if (l <= Integer.MAX_VALUE && l >= Integer.MIN_VALUE) {
                    writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_3_10.coverage", "54d76b10-9455-456b-bed9-3a160e03dea9");
                    return (int) l;
                } else {
                    writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_3_10.coverage", "a508fd95-69f8-4101-a8ee-e86b65d56765");
                    return l;
                }
            } catch (ArithmeticException xa) {
            }
        }
        writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_3_10.coverage", "15bd46cf-7437-4aaa-9332-ab9bac506548");
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
        writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_3_10.coverage", "4c2d8bc4-7d9c-4bb9-9628-ca215b46b303");
        boolean narrowed = false;
        writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_3_10.coverage", "32f2fd78-e546-4045-bd5c-e16c2b0fbf79");
        for (int a = 0; a < args.length; ++a) {
            writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_3_10.coverage", "7fe7dcda-4c6e-4900-8aa4-0f8afc319ea9");
            Object arg = args[a];
            writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_3_10.coverage", "c8e86d09-78ee-40bc-9fe2-76f0cdc2b205");
            if (arg instanceof Number) {
                writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_3_10.coverage", "866101f2-b20a-4fa9-89a7-211569c41bfa");
                Number narg = (Number) arg;
                writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_3_10.coverage", "6177218e-b326-418a-89a4-80c79f6e85df");
                Number narrow = narrow(narg);
                writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_3_10.coverage", "8a5ebb88-5109-471b-ac44-7e2bf6eb3da3");
                if (!narg.equals(narrow)) {
                    writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_3_10.coverage", "97eea962-1966-4030-a3b2-a4fd128a70be");
                    args[a] = narrow;
                    writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_3_10.coverage", "61860710-fc91-4cc6-9b48-ebea87ce31da");
                    narrowed = true;
                }
            }
        }
        writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_3_10.coverage", "e6c6ca9a-de89-498d-a59e-3af1cff82f64");
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
        writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_3_10.coverage", "f1ee0425-01c1-4b62-a0ae-cec59cf98318");
        if (left == null && right == null) {
            writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_3_10.coverage", "004912fd-073e-41b0-abae-077b006fb440");
            return controlNullNullOperands();
        }
        writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_3_10.coverage", "8d0af2df-1d42-4e79-9e09-5988c1ed5496");
        boolean strconcat = strict ? left instanceof String || right instanceof String : left instanceof String && right instanceof String;
        writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_3_10.coverage", "721f33b1-bcc7-4854-96d6-88b1d074f1d6");
        if (!strconcat) {
            writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_3_10.coverage", "23973793-f319-4025-ad8d-f7c8067f2e6f");
            try {
                writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_3_10.coverage", "a570c63f-f53e-41f2-bfb2-74e616a63109");
                if (left instanceof BigDecimal || right instanceof BigDecimal) {
                    writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_3_10.coverage", "084a6ebd-588a-491c-b07d-60918e8b892c");
                    BigDecimal l = toBigDecimal(left);
                    writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_3_10.coverage", "327646bf-6ea2-4eb9-9f8c-bf8c22a51374");
                    BigDecimal r = toBigDecimal(right);
                    writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_3_10.coverage", "1ad33913-27bf-44d6-a114-7e4929780ce3");
                    BigDecimal result = l.add(r, getMathContext());
                    writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_3_10.coverage", "79e79aa3-a73a-4bbc-ab39-2c9e4ffa12d2");
                    return narrowBigDecimal(left, right, result);
                }
                writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_3_10.coverage", "6abe6aa5-3ec4-4d86-9094-aa92005fafe4");
                if (isFloatingPointNumber(left) || isFloatingPointNumber(right)) {
                    writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_3_10.coverage", "7b563e8d-97f4-4617-b2c3-630b8e32d888");
                    double l = toDouble(left);
                    writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_3_10.coverage", "04db580f-f619-41db-96c7-baf46423ddaf");
                    double r = toDouble(right);
                    writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_3_10.coverage", "3037f29a-079c-47f5-9540-a83c3d575daa");
                    return l + r;
                }
                writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_3_10.coverage", "06e74456-d207-4383-bf04-40d8f1e940a6");
                BigInteger l = toBigInteger(left);
                writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_3_10.coverage", "efb7e58f-3c19-4312-8cdb-0b1d9a0a0e9e");
                BigInteger r = toBigInteger(right);
                writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_3_10.coverage", "df57719c-20c4-4a40-874e-8d4df4d90bd6");
                BigInteger result = l.add(r);
                writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_3_10.coverage", "70a0c595-1911-4d64-8dd9-e5bc1ccdb39e");
                return narrowBigInteger(left, right, result);
            } catch (java.lang.NumberFormatException nfe) {
                writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_3_10.coverage", "2ed6e465-05ed-4a70-9057-672935335638");
                if (left == null || right == null) {
                    writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_3_10.coverage", "ab8e59c1-039a-4fca-94a0-f15e91170085");
                    controlNullOperand();
                }
            }
        }
        writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_3_10.coverage", "d93c94e3-cf18-46b4-a8c0-fa511dc21043");
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
        writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_3_10.coverage", "3dac15e4-91b4-48e9-8c47-984d24d39b53");
        if (left == null && right == null) {
            writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_3_10.coverage", "5fb0de2a-5c8d-44ca-aaf0-f02bef5efaa0");
            return controlNullNullOperands();
        }
        writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_3_10.coverage", "d2ab5967-fde3-4f9b-9265-87602865c98d");
        if (left instanceof BigDecimal || right instanceof BigDecimal) {
            writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_3_10.coverage", "adaca474-a3c8-4e7b-8f29-9ad1a1c691b8");
            BigDecimal l = toBigDecimal(left);
            writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_3_10.coverage", "8dfafbce-352e-409a-86af-c400967e3b5f");
            BigDecimal r = toBigDecimal(right);
            writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_3_10.coverage", "dc1ead34-8a9e-431c-890c-ebc288e65b9d");
            if (BigDecimal.ZERO.equals(r)) {
                writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_3_10.coverage", "a066721f-bffb-4eb7-a149-7d9353b5d586");
                throw new ArithmeticException("/");
            }
            writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_3_10.coverage", "4c75cbb9-6476-48a3-903b-277f6fea0832");
            BigDecimal result = l.divide(r, getMathContext());
            writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_3_10.coverage", "b5ae7bd6-3bac-41d1-b936-9d9b174445bd");
            return narrowBigDecimal(left, right, result);
        }
        writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_3_10.coverage", "5a6b14b9-b359-41aa-987e-f025872336cb");
        if (isFloatingPointNumber(left) || isFloatingPointNumber(right)) {
            writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_3_10.coverage", "ed6fce5e-496d-4530-af71-4a92b64fab0d");
            double l = toDouble(left);
            writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_3_10.coverage", "bc2555af-50ee-4e46-a1a7-23f740420795");
            double r = toDouble(right);
            writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_3_10.coverage", "c988b1d6-1046-45b8-b289-f2770de7e4a0");
            if (r == 0.0) {
                writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_3_10.coverage", "20759772-2908-4f18-940e-b79ffa9c4815");
                throw new ArithmeticException("/");
            }
            writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_3_10.coverage", "cb1c3a66-c4e3-4a1b-96d2-a3e37bb4a9a2");
            return l / r;
        }
        writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_3_10.coverage", "ff38775d-88b5-4926-981b-a76989fadd87");
        BigInteger l = toBigInteger(left);
        writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_3_10.coverage", "37d11894-d86d-42ce-bac6-e209c3e8fded");
        BigInteger r = toBigInteger(right);
        writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_3_10.coverage", "bbffa42a-36a6-4e81-a694-cc92acab7fe3");
        if (BigInteger.ZERO.equals(r)) {
            writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_3_10.coverage", "4ccc0722-e58a-4918-9722-805e8039d5d5");
            throw new ArithmeticException("/");
        }
        writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_3_10.coverage", "0b8c2f73-7272-4612-bdc8-fb7f05f92ee2");
        BigInteger result = l.divide(r);
        writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_3_10.coverage", "689ff8d6-3761-4d0c-b8de-54f80e6b2a03");
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
        writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_3_10.coverage", "03364a11-b0c7-4162-87a5-ee41659e317d");
        if (left == null && right == null) {
            writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_3_10.coverage", "925419bd-f885-4094-912e-1c8507d7500b");
            return controlNullNullOperands();
        }
        writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_3_10.coverage", "ac31348a-4707-4db7-ba6b-9d788d4a8385");
        if (left instanceof BigDecimal || right instanceof BigDecimal) {
            writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_3_10.coverage", "0113bf51-e4e5-4251-b215-9350fb23666b");
            BigDecimal l = toBigDecimal(left);
            writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_3_10.coverage", "837152fc-f36c-45a3-92fc-404f3b17bc8b");
            BigDecimal r = toBigDecimal(right);
            writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_3_10.coverage", "ac7a5988-4529-4a79-ab30-8463c567dfe7");
            if (BigDecimal.ZERO.equals(r)) {
                writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_3_10.coverage", "e0f59e71-0225-4022-ba2f-8b3feb85f0d3");
                throw new ArithmeticException("%");
            }
            writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_3_10.coverage", "9d98f54a-c0b0-4e8f-b4d1-5b7ff070c124");
            BigDecimal remainder = l.remainder(r, getMathContext());
            writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_3_10.coverage", "58545a8a-6b64-4d73-b3fc-86776d202e91");
            return narrowBigDecimal(left, right, remainder);
        }
        writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_3_10.coverage", "23bf6049-4409-45e4-bdbc-d5ac3748ed60");
        if (isFloatingPointNumber(left) || isFloatingPointNumber(right)) {
            writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_3_10.coverage", "4ca53ef1-91c3-4d6a-be8e-35135a73a4f9");
            double l = toDouble(left);
            writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_3_10.coverage", "c9c38620-768f-45d8-aa44-10ff7156f3d5");
            double r = toDouble(right);
            writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_3_10.coverage", "51f46960-1a91-4f72-87d8-e92abd311714");
            if (r == 0.0) {
                writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_3_10.coverage", "f9a51861-ad93-4bb3-b65f-a97d66a6c36e");
                throw new ArithmeticException("%");
            }
            writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_3_10.coverage", "faeeeade-18a4-4f24-b7b5-ddec6fd83956");
            return l % r;
        }
        writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_3_10.coverage", "0e3251fa-613b-4af9-bdd8-c03347745c28");
        BigInteger l = toBigInteger(left);
        writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_3_10.coverage", "bf41e65c-cac3-437c-9a30-e127df7dbba3");
        BigInteger r = toBigInteger(right);
        writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_3_10.coverage", "b014f16c-ae3c-4350-a479-33fd1f4def4d");
        BigInteger result = l.mod(r);
        writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_3_10.coverage", "abf56988-bb55-4c17-9576-3e2066493607");
        if (BigInteger.ZERO.equals(r)) {
            writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_3_10.coverage", "8bec3dbb-97b3-460e-8a93-1f219ad70013");
            throw new ArithmeticException("%");
        }
        writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_3_10.coverage", "816d65a6-ee9d-48a5-8328-1a2d31f28cbf");
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
        writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_3_10.coverage", "86d3912c-aa11-4622-a824-06be8a074a9d");
        if (left == null && right == null) {
            writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_3_10.coverage", "787bf200-4094-4718-887a-421526dd5e6b");
            return controlNullNullOperands();
        }
        writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_3_10.coverage", "d13a1fea-89f2-42a6-9826-947cb1daa541");
        if (left instanceof BigDecimal || right instanceof BigDecimal) {
            writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_3_10.coverage", "c5ea0f56-379d-4e53-810f-d9c832a695e0");
            BigDecimal l = toBigDecimal(left);
            writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_3_10.coverage", "e799afde-b9bf-4bd2-a9db-553d63d5f09e");
            BigDecimal r = toBigDecimal(right);
            writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_3_10.coverage", "1fd9fad0-0c92-46d1-a56e-6e591b454c11");
            BigDecimal result = l.multiply(r, getMathContext());
            writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_3_10.coverage", "fd421d3c-1727-4d6a-972c-1ae4997e9bdc");
            return narrowBigDecimal(left, right, result);
        }
        writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_3_10.coverage", "468df6b3-7474-4743-a48f-a83f9886d1a4");
        if (isFloatingPointNumber(left) || isFloatingPointNumber(right)) {
            writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_3_10.coverage", "a13408ad-5734-40d2-90d2-f358248b2989");
            double l = toDouble(left);
            writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_3_10.coverage", "e68b9db3-7700-49f6-b8d7-4500b415973b");
            double r = toDouble(right);
            writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_3_10.coverage", "feb553bb-6968-4fb1-acc6-97d973cd42ac");
            return l * r;
        }
        writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_3_10.coverage", "ed299b00-2a50-4f8a-af35-1e7cf023cde8");
        BigInteger l = toBigInteger(left);
        writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_3_10.coverage", "0c2d79bc-851b-44fb-8312-792a0f445936");
        BigInteger r = toBigInteger(right);
        writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_3_10.coverage", "0ec3459a-96df-4c3f-9270-97dc14d8acbb");
        BigInteger result = l.multiply(r);
        writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_3_10.coverage", "3836b86b-c826-49f0-a3ef-e35ab7c7fd1d");
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
        writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_3_10.coverage", "0c3e08aa-870a-4834-9d12-74036cd92446");
        if (left == null && right == null) {
            writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_3_10.coverage", "4c5cbcbb-dd4b-4551-883b-092109342510");
            return controlNullNullOperands();
        }
        writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_3_10.coverage", "d1f423e2-1222-4b79-b11c-6588cf082c9c");
        if (left instanceof BigDecimal || right instanceof BigDecimal) {
            writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_3_10.coverage", "235f73df-1adf-4564-90c9-9b861f04d0a7");
            BigDecimal l = toBigDecimal(left);
            writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_3_10.coverage", "554486c2-0934-4322-bcd3-0996ae0ab971");
            BigDecimal r = toBigDecimal(right);
            writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_3_10.coverage", "76b199a9-e248-472d-9f6a-92a4133d29a5");
            BigDecimal result = l.subtract(r, getMathContext());
            writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_3_10.coverage", "b3ce45cd-2b9e-487f-9a6d-76e9f55e5e04");
            return narrowBigDecimal(left, right, result);
        }
        writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_3_10.coverage", "95b8194a-8bb2-4036-a821-5a3fe90e2d0a");
        if (isFloatingPointNumber(left) || isFloatingPointNumber(right)) {
            writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_3_10.coverage", "504a6d91-2a19-47ba-8af9-b4de8cc1161d");
            double l = toDouble(left);
            writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_3_10.coverage", "7c4b072d-9e07-4171-a988-910c4875d036");
            double r = toDouble(right);
            writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_3_10.coverage", "489a98e2-7012-4585-85e2-76662ed87f0b");
            return l - r;
        }
        writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_3_10.coverage", "10b9337d-9c3b-4add-ae60-e032e58706ff");
        BigInteger l = toBigInteger(left);
        writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_3_10.coverage", "b596c80a-5816-4ec5-a379-ae146351eb85");
        BigInteger r = toBigInteger(right);
        writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_3_10.coverage", "e746fc3e-2c11-4e42-bcd1-6f7e31406af2");
        BigInteger result = l.subtract(r);
        writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_3_10.coverage", "57d67e3d-3601-4c12-be06-7b88908f0aca");
        return narrowBigInteger(left, right, result);
    }

    /**
     * Negates a value (unary minus for numbers).
     * 
     * @param val the value to negate
     * @return the negated value
     */
    public Object negate(Object val) {
        writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_3_10.coverage", "a7e44b3a-9949-4fd9-ac25-6793c962f39e");
        if (val instanceof Integer) {
            writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_3_10.coverage", "a2ee27df-d80b-41eb-b0d0-489599417eab");
            return -((Integer) val);
        } else if (val instanceof Double) {
            writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_3_10.coverage", "d21db42e-776d-4de9-9a4e-02e26eae0542");
            return -((Double) val);
        } else if (val instanceof Long) {
            writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_3_10.coverage", "40d0f3e2-a80b-4986-9e79-c512fab90099");
            return -((Long) val);
        } else if (val instanceof BigDecimal) {
            writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_3_10.coverage", "63af1229-5e3d-4b01-8412-d41fec947398");
            return ((BigDecimal) val).negate();
        } else if (val instanceof BigInteger) {
            writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_3_10.coverage", "e68f7790-ef8e-4fc7-9b05-6b410ec7d207");
            return ((BigInteger) val).negate();
        } else if (val instanceof Float) {
            writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_3_10.coverage", "c5986d1a-e6ed-4013-93f9-6e49b23432b1");
            return -((Float) val);
        } else if (val instanceof Short) {
            writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_3_10.coverage", "898c8f27-eba2-4c39-98db-5ea418ee49ad");
            return (short) -((Short) val);
        } else if (val instanceof Byte) {
            writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_3_10.coverage", "629ed1f1-7383-4157-9a44-acc1d613886d");
            return (byte) -((Byte) val);
        } else if (val instanceof Boolean) {
            writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_3_10.coverage", "7b33ad16-a5f7-4e2b-a8f5-e90a7968adc1");
            return ((Boolean) val) ? Boolean.FALSE : Boolean.TRUE;
        }
        writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_3_10.coverage", "3caf02d8-92f6-423e-86e1-a606a2b74541");
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
        writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_3_10.coverage", "dad6daca-77ed-40d5-aa40-88def86403b2");
        if (value == null && container == null) {
            writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_3_10.coverage", "df66fc3a-4122-47a7-8e72-c68e8e1899cc");
            return true;
        }
        writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_3_10.coverage", "ebf30964-9c6a-4326-87a0-a35c7bf7cb03");
        if (value == null || container == null) {
            writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_3_10.coverage", "f3bd5699-51ba-4841-b095-cf1561eeabd5");
            return false;
        }
        writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_3_10.coverage", "7d173c3d-676c-497b-b8e2-381eaa194a74");
        if (container instanceof java.util.regex.Pattern) {
            writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_3_10.coverage", "19a9c6fc-02b7-41c0-9162-e10d95acb4b9");
            return ((java.util.regex.Pattern) container).matcher(value.toString()).matches();
        }
        writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_3_10.coverage", "15a4b3eb-679a-4a17-96af-9ddc8b69a820");
        if (container instanceof String) {
            writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_3_10.coverage", "de91025b-3f6c-4eba-86a9-47368b522c59");
            return value.toString().matches(container.toString());
        }
        writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_3_10.coverage", "bf2b66ca-6943-425b-8118-5209292bb9c9");
        if (container instanceof Map<?, ?>) {
            writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_3_10.coverage", "9530a31b-5f37-4979-84dc-8cf64460f7e2");
            if (value instanceof Map<?, ?>) {
                writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_3_10.coverage", "f70f26bf-9e72-4e4b-aac3-43b010c4ed6b");
                return ((Map<?, ?>) container).keySet().containsAll(((Map<?, ?>) value).keySet());
            }
            writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_3_10.coverage", "a46c1d3e-e723-40b9-9214-b31876ed62dc");
            return ((Map<?, ?>) container).containsKey(value);
        }
        writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_3_10.coverage", "9f42241f-b10c-4774-b2ea-ef909be74b2b");
        if (container instanceof Collection<?>) {
            writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_3_10.coverage", "101cc747-3ced-4600-9a41-4231245c4980");
            if (value instanceof Collection<?>) {
                writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_3_10.coverage", "cd527985-82ec-4408-8382-a2520f8500ec");
                return ((Collection<?>) container).containsAll((Collection<?>) value);
            }
            writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_3_10.coverage", "baff117a-862e-47f6-b0fd-cfb3bf24c393");
            return ((Collection<?>) container).contains(value);
        }
        writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_3_10.coverage", "e1fdf312-52c2-47ee-b789-d9565f282c77");
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
        writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_3_10.coverage", "9267c047-938b-4c52-ad37-9debc5514d88");
        if (left == null && right == null) {
            writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_3_10.coverage", "e6755d3a-f078-4998-b122-5deaaa459426");
            return true;
        }
        writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_3_10.coverage", "842afc56-3809-421e-a74d-1bb1a89e2950");
        if (left == null || right == null) {
            writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_3_10.coverage", "6d0de39d-afe3-47c3-9879-8c2cb94a3fa9");
            return false;
        }
        writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_3_10.coverage", "e5ea9908-e895-4672-ae25-0f58e8ab8aca");
        if (left instanceof String) {
            writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_3_10.coverage", "024b47ef-f6e2-4950-b543-cf115e95e0af");
            return ((String) left).endsWith(toString(right));
        }
        writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_3_10.coverage", "24db7db3-4362-4474-bf02-54ce237ce03e");
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
        writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_3_10.coverage", "6f3ac7f2-ceab-4508-b788-20e3295d4372");
        if (left == null && right == null) {
            writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_3_10.coverage", "00c1199d-5547-4384-9aaa-42ffba2ee731");
            return true;
        }
        writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_3_10.coverage", "04429ed2-a269-472f-ba37-bc094aa06b87");
        if (left == null || right == null) {
            writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_3_10.coverage", "dcaf184a-8f39-4953-a3a4-e19f7766f2a0");
            return false;
        }
        writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_3_10.coverage", "2e6e73e5-4bed-4999-826b-dcaf49c8db5d");
        if (left instanceof String) {
            writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_3_10.coverage", "3c3551af-f191-41cc-903a-d27a9fde1cc6");
            return ((String) left).startsWith(toString(right));
        }
        writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_3_10.coverage", "486bae7b-952a-4f5f-805d-678ca3f0508f");
        return null;
    }

    /**
     * Check for emptyness of various types: Number, Collection, Array, Map, String.
     *
     * @param object the object to check the emptyness of
     * @return the boolean or null of there is no arithmetic solution
     */
    public Boolean isEmpty(Object object) {
        writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_3_10.coverage", "bfd4fd25-2cfe-4ba0-8ec0-8ab17eccc738");
        if (object instanceof Number) {
            writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_3_10.coverage", "320bc692-fdee-407d-a44b-19f056a1f7c5");
            double d = ((Number) object).doubleValue();
            writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_3_10.coverage", "4e5b3b83-945c-4695-b4c6-7b98a2e2f46e");
            return Double.isNaN(d) || d == 0.d ? Boolean.TRUE : Boolean.FALSE;
        }
        writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_3_10.coverage", "53080c24-261d-45d0-9019-b3f400184c95");
        if (object instanceof String) {
            writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_3_10.coverage", "84d03ddc-8724-452f-8eb9-9dba81a50031");
            return "".equals(object) ? Boolean.TRUE : Boolean.FALSE;
        }
        writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_3_10.coverage", "67609df3-30b5-43f6-bad0-210c5bbc7063");
        if (object.getClass().isArray()) {
            writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_3_10.coverage", "430bf710-acff-4068-b737-b3bdda8d7394");
            return Array.getLength(object) == 0 ? Boolean.TRUE : Boolean.FALSE;
        }
        writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_3_10.coverage", "12acf52b-7030-4735-ba95-6a24df914d79");
        if (object instanceof Collection<?>) {
            writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_3_10.coverage", "19d1baaf-0903-4fa8-b263-41f073ce2c95");
            return ((Collection<?>) object).isEmpty() ? Boolean.TRUE : Boolean.FALSE;
        }
        writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_3_10.coverage", "a1454218-6470-4f9c-8e6b-724a61dab0a7");
        if (object instanceof Map<?, ?>) {
            writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_3_10.coverage", "a493cc46-769f-48fc-a71d-78f939081b76");
            return ((Map<?, ?>) object).isEmpty() ? Boolean.TRUE : Boolean.FALSE;
        }
        writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_3_10.coverage", "26ed3364-c68e-44cf-80de-f4364523084b");
        return null;
    }

    /**
     * Calculate the <code>size</code> of various types: Collection, Array, Map, String.
     *
     * @param object the object to get the size of
     * @return the size of object or null if there is no arithmetic solution
     */
    public Integer size(Object object) {
        writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_3_10.coverage", "78f9fe76-51cb-4060-a3cf-aef04d3715f3");
        if (object instanceof String) {
            writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_3_10.coverage", "2c855875-8309-4485-80ee-fc37b58f33e2");
            return ((String) object).length();
        }
        writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_3_10.coverage", "4a022651-a937-43a5-838f-6f234a9966ea");
        if (object.getClass().isArray()) {
            writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_3_10.coverage", "4c1031c6-3121-449d-aa9c-d2c7d5276d19");
            return Array.getLength(object);
        }
        writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_3_10.coverage", "a68bd57b-3975-4e74-b482-ea91fc095bbb");
        if (object instanceof Collection<?>) {
            writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_3_10.coverage", "718afedd-45fe-4aa6-a269-5a7debcda387");
            return ((Collection<?>) object).size();
        }
        writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_3_10.coverage", "1a34e065-88d3-4b21-8191-99c6b81a15b5");
        if (object instanceof Map<?, ?>) {
            writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_3_10.coverage", "91183e60-8ab7-4177-b788-2c4c8f89c339");
            return ((Map<?, ?>) object).size();
        }
        writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_3_10.coverage", "0324f56c-9116-410f-af01-90f536bf54f8");
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
        writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_3_10.coverage", "e6988b36-ae5d-4fa1-b4c3-531f0c2c9dfa");
        long l = toLong(left);
        writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_3_10.coverage", "963dc246-4870-4e36-bf74-7022afb29e0a");
        long r = toLong(right);
        writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_3_10.coverage", "c2e5ea4e-b303-4b16-be3f-09d4c9cc55df");
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
        writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_3_10.coverage", "8745c28e-29b7-4ea3-97f3-c4bd80170f7a");
        long l = toLong(left);
        writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_3_10.coverage", "85ee735a-430a-4e3d-a1e7-fcaad17abe92");
        long r = toLong(right);
        writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_3_10.coverage", "5d9b2b9d-08dd-46f5-8ab1-b710cbbe67ce");
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
        writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_3_10.coverage", "c1607101-2ad5-4951-8e86-f1376abceed1");
        long l = toLong(left);
        writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_3_10.coverage", "d8979ed1-6eb4-44a4-bc64-2722dbf8a9dc");
        long r = toLong(right);
        writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_3_10.coverage", "f4c45405-b78d-4830-9f49-170f2a83b59e");
        return l ^ r;
    }

    /**
     * Performs a bitwise complement.
     * 
     * @param val the operand
     * @return ~val
     */
    public Object complement(Object val) {
        writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_3_10.coverage", "c6464f2d-4b2c-4e16-b8e6-82350c8f45ec");
        long l = toLong(val);
        writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_3_10.coverage", "f747ced8-c46d-4795-af2c-55d910fdb5e2");
        return ~l;
    }

    /**
     * Performs a logical not.
     * 
     * @param val the operand
     * @return !val
     */
    public Object not(Object val) {
        writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_3_10.coverage", "d6b4f79e-47b6-4575-9f96-b766e7b4548b");
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
        writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_3_10.coverage", "62825af6-9ce9-491b-a98c-ba3c6806d81b");
        if (left != null && right != null) {
            writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_3_10.coverage", "dca7ec01-8a9a-455c-a003-ed87e292bca0");
            if (left instanceof BigDecimal || right instanceof BigDecimal) {
                writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_3_10.coverage", "ca1638e2-7b1f-4462-946a-3c9fcdd92649");
                BigDecimal l = toBigDecimal(left);
                writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_3_10.coverage", "8192c42d-3e43-42ec-8d7b-d763d72da3a3");
                BigDecimal r = toBigDecimal(right);
                writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_3_10.coverage", "d3d962a7-ba14-4571-a250-b8aade9f1673");
                return l.compareTo(r);
            } else if (left instanceof BigInteger || right instanceof BigInteger) {
                writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_3_10.coverage", "8039cdcc-dace-4cd3-ab83-c7fe6eeaac05");
                BigInteger l = toBigInteger(left);
                writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_3_10.coverage", "05c4fdeb-9cd5-4f3f-bc41-ec10b6a276f9");
                BigInteger r = toBigInteger(right);
                writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_3_10.coverage", "e8d2ac7a-2e6d-4cd2-a53a-db1f436bfd73");
                return l.compareTo(r);
            } else if (isFloatingPoint(left) || isFloatingPoint(right)) {
                writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_3_10.coverage", "5c876df8-989a-4c45-8f34-4f74ab99a05b");
                double lhs = toDouble(left);
                writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_3_10.coverage", "72e445cc-d6a3-4444-854a-5b1c029145b0");
                double rhs = toDouble(right);
                writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_3_10.coverage", "057d9c15-a4e2-4ae2-9eb8-886259fdfb9d");
                if (Double.isNaN(lhs)) {
                    writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_3_10.coverage", "bb203635-9988-4811-90c9-e7a3c560aae1");
                    if (Double.isNaN(rhs)) {
                        writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_3_10.coverage", "610dd31c-ddb9-42fc-b64a-a516031cf2bb");
                        return 0;
                    } else {
                        writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_3_10.coverage", "63100e83-2395-4cf0-8969-9721a0724e2c");
                        return -1;
                    }
                } else if (Double.isNaN(rhs)) {
                    writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_3_10.coverage", "69e6f23b-1cea-46f5-9ad9-f1905f4c2421");
                    return +1;
                } else if (lhs < rhs) {
                    writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_3_10.coverage", "75c55864-b642-4f60-8f5b-ac9ecef7a498");
                    return -1;
                } else if (lhs > rhs) {
                    writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_3_10.coverage", "87594bb5-d864-4fc2-a178-abc81cf7432d");
                    return +1;
                } else {
                    writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_3_10.coverage", "8a48fc45-408a-4dd6-8568-6aa79830ea22");
                    return 0;
                }
            } else if (isNumberable(left) || isNumberable(right)) {
                writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_3_10.coverage", "aa0d45d1-8edd-485a-bc70-daf9a8b8cb23");
                long lhs = toLong(left);
                writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_3_10.coverage", "e3ee90d7-d3eb-4f94-8ee6-61c242484f44");
                long rhs = toLong(right);
                writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_3_10.coverage", "444763b8-5be4-47ee-b105-ffee398a2bd3");
                if (lhs < rhs) {
                    writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_3_10.coverage", "be9d5899-ed0c-46d8-aff6-d3c11e5adaaa");
                    return -1;
                } else if (lhs > rhs) {
                    writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_3_10.coverage", "23a19bc5-a2b3-4e38-bc22-bbd270224331");
                    return +1;
                } else {
                    writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_3_10.coverage", "96f3566e-bfb1-4134-a31a-ed4f2630d99c");
                    return 0;
                }
            } else if (left instanceof String || right instanceof String) {
                writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_3_10.coverage", "10c4a87b-092b-4433-a483-f5a31d67b26b");
                return toString(left).compareTo(toString(right));
            } else if ("==".equals(operator)) {
                writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_3_10.coverage", "2bd73b4c-c900-4298-9cde-6eb719c93d33");
                return left.equals(right) ? 0 : -1;
            } else if (left instanceof Comparable<?>) {
                writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_3_10.coverage", "452fbb37-eb31-4102-8822-779597096710");
                @SuppressWarnings("unchecked") final Comparable<Object> comparable = (Comparable<Object>) left;
                writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_3_10.coverage", "fdcfebaa-6352-4841-9f59-e7c3e58f27a2");
                return comparable.compareTo(right);
            } else if (right instanceof Comparable<?>) {
                writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_3_10.coverage", "b17ab950-028d-4b06-b27f-b047d75bb25b");
                @SuppressWarnings("unchecked") final Comparable<Object> comparable = (Comparable<Object>) right;
                writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_3_10.coverage", "895e356a-efca-4973-bd08-a1c8c29ba113");
                return comparable.compareTo(left);
            }
        }
        writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_3_10.coverage", "892f27ff-9645-4559-846f-1960292e19f7");
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
        writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_3_10.coverage", "a75cf757-f603-4b11-91f9-fb3c4c03a372");
        if (left == right) {
            writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_3_10.coverage", "f8f90e7b-dcda-47e3-bbde-bd7e354cf68d");
            return true;
        } else if (left == null || right == null) {
            writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_3_10.coverage", "d5a5f5e6-1c69-40da-bc04-088335e5eed4");
            return false;
        } else if (left instanceof Boolean || right instanceof Boolean) {
            writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_3_10.coverage", "e979fb6f-9e9d-4e8b-a212-8bc34fd4f3d2");
            return toBoolean(left) == toBoolean(right);
        } else {
            writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_3_10.coverage", "929c9c86-8e0d-43a3-9a41-bc818263705f");
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
        writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_3_10.coverage", "d1d22d5a-34bc-43bd-a301-dec098edb7bd");
        if ((left == right) || (left == null) || (right == null)) {
            writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_3_10.coverage", "16699032-8a28-464a-a228-54b9155aa327");
            return false;
        } else {
            writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_3_10.coverage", "a8a9078e-f610-4f31-a99c-81cb7604fb42");
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
        writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_3_10.coverage", "21b9774d-0a65-4f02-af9f-d7143d1b434f");
        if ((left == right) || left == null || right == null) {
            writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_3_10.coverage", "e0f21f7f-7e04-4761-8cf0-70e23f970a4c");
            return false;
        } else {
            writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_3_10.coverage", "9b64e9f1-b425-482b-b420-046850f79070");
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
        writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_3_10.coverage", "3f80d0c6-6c4f-4809-b696-61991ddc7a07");
        if (left == right) {
            writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_3_10.coverage", "6949d3dc-6116-414a-b5c2-8633f5d53908");
            return true;
        } else if (left == null || right == null) {
            writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_3_10.coverage", "7ad14ae6-f0db-4d94-a280-0678aaae6de6");
            return false;
        } else {
            writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_3_10.coverage", "31067661-878e-4e74-85f9-d53f3ea2512b");
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
        writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_3_10.coverage", "0f5fa565-02b3-43ea-9124-2f830c61adc7");
        if (left == right) {
            writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_3_10.coverage", "9466bb1e-5f7f-4a92-b923-467507dfa4fa");
            return true;
        } else if (left == null || right == null) {
            writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_3_10.coverage", "d42f7c10-8625-4aa2-864d-583f5802ea35");
            return false;
        } else {
            writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_3_10.coverage", "0a37361c-ddbb-487e-a608-c8f8843a31e6");
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
        writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_3_10.coverage", "7d3048b7-3094-4255-b11f-2149e5b19f17");
        if (val == null) {
            writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_3_10.coverage", "0b725aed-1cf2-470e-b14f-8b67aa5eed09");
            controlNullOperand();
            writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_3_10.coverage", "a8586f05-0aad-4c5b-8506-32012197c113");
            return false;
        } else if (val instanceof Boolean) {
            writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_3_10.coverage", "dc1d3f21-5e41-4627-aa9e-d9bd7e0264f0");
            return ((Boolean) val);
        } else if (val instanceof Number) {
            writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_3_10.coverage", "c6cab21e-fe03-4bad-8f42-6f5a1decfed3");
            double number = toDouble(val);
            writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_3_10.coverage", "a6cabdb1-4ff2-4385-95a8-791024a9c313");
            return !Double.isNaN(number) && number != 0.d;
        } else if (val instanceof String) {
            writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_3_10.coverage", "ddfb7e47-d5c5-435d-b41c-4d3a19cc7857");
            String strval = val.toString();
            writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_3_10.coverage", "c84bb097-8dbe-4be9-b415-045f33d2453e");
            return strval.length() > 0 && !"false".equals(strval);
        } else {
            writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_3_10.coverage", "18595472-2181-41c3-9fab-e7e18b8106ef");
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
        writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_3_10.coverage", "a2e336a0-37a2-494d-a9b8-27bc4f559ba6");
        if (val == null) {
            writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_3_10.coverage", "80047a58-0f82-474b-aff5-abd0da2a7940");
            controlNullOperand();
            writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_3_10.coverage", "bbf4012c-c591-4f1b-a427-fc66d8e78074");
            return 0;
        } else if (val instanceof Double) {
            writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_3_10.coverage", "195bf805-3945-4160-8ae1-220329af6452");
            Double dval = (Double) val;
            writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_3_10.coverage", "270c4167-c132-46e1-ac25-adadf993d7a1");
            if (Double.isNaN(dval)) {
                writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_3_10.coverage", "b2089913-cff8-403d-a10c-346ad950b2a1");
                return 0;
            } else {
                writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_3_10.coverage", "e1cf2300-1e6a-4fec-a8f8-713e166d9477");
                return dval.intValue();
            }
        } else if (val instanceof Number) {
            writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_3_10.coverage", "459dd888-55dc-409e-b73a-93239c50fca6");
            return ((Number) val).intValue();
        } else if (val instanceof String) {
            writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_3_10.coverage", "52b829ce-a003-4b0c-81a5-91a028b1c1f2");
            if ("".equals(val)) {
                writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_3_10.coverage", "e14ef56a-3fe5-43a8-ba06-edabed9b4b04");
                return 0;
            }
            writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_3_10.coverage", "1ca88a56-93a1-4642-8485-0ef5de738a84");
            return Integer.parseInt((String) val);
        } else if (val instanceof Boolean) {
            writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_3_10.coverage", "033b3081-8217-4f6c-90e5-79f93b2ca736");
            return ((Boolean) val) ? 1 : 0;
        } else if (val instanceof Character) {
            writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_3_10.coverage", "4d09deea-102e-4159-8c22-f53b7aa491d0");
            return ((Character) val);
        }
        writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_3_10.coverage", "4dea0807-73a3-4c07-9d2b-b85899344723");
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
        writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_3_10.coverage", "d8758c8e-e117-4380-bb7d-a46d93cad798");
        if (val == null) {
            writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_3_10.coverage", "10c82832-b597-4d18-b4e3-b17ec59a673c");
            controlNullOperand();
            writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_3_10.coverage", "18b9a343-eceb-4002-b6ed-6e6a0b29072a");
            return 0L;
        } else if (val instanceof Double) {
            writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_3_10.coverage", "169ae1da-d38b-41c9-b7f9-c700c49c4a55");
            Double dval = (Double) val;
            writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_3_10.coverage", "3d691eab-26b3-4735-b07f-7ee52d475fbe");
            if (Double.isNaN(dval)) {
                writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_3_10.coverage", "7817b1d7-17c9-465c-a91d-7206f516121c");
                return 0L;
            } else {
                writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_3_10.coverage", "4b8bb474-e064-4fb4-94b5-7288157ce939");
                return dval.longValue();
            }
        } else if (val instanceof Number) {
            writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_3_10.coverage", "8dfd3c0f-fd1c-416a-b696-30a643b39a74");
            return ((Number) val).longValue();
        } else if (val instanceof String) {
            writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_3_10.coverage", "a37e9781-7adb-4476-a4bb-05559720acea");
            if ("".equals(val)) {
                writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_3_10.coverage", "c1c221c1-f41d-4d5e-8507-f3525d7395c3");
                return 0L;
            } else {
                writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_3_10.coverage", "96ba8f88-14c1-4913-a623-90ac79e81595");
                return Long.parseLong((String) val);
            }
        } else if (val instanceof Boolean) {
            writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_3_10.coverage", "f7d245fc-02d4-4d76-8bec-cd56756bf71a");
            return ((Boolean) val) ? 1L : 0L;
        } else if (val instanceof Character) {
            writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_3_10.coverage", "17d6c3b2-f717-4901-9ed4-427cd709c0a3");
            return ((Character) val);
        }
        writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_3_10.coverage", "73f8cb15-ea7e-4e0e-840c-5c4c0feaace1");
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
        writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_3_10.coverage", "ec845a62-733a-43ac-baa9-ffe0b0d9d1b4");
        if (val == null) {
            writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_3_10.coverage", "46da37b6-4286-458c-a865-d02c820c4231");
            controlNullOperand();
            writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_3_10.coverage", "0843adae-e115-4bc2-9560-ba39f60124c2");
            return BigInteger.ZERO;
        } else if (val instanceof BigInteger) {
            writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_3_10.coverage", "abcf9028-b0a4-4f97-8970-00b430d51c45");
            return (BigInteger) val;
        } else if (val instanceof Double) {
            writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_3_10.coverage", "07497100-4674-4680-83b4-3c7803d74503");
            Double dval = (Double) val;
            writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_3_10.coverage", "a89879eb-7ffd-4b65-938c-1ca23dd4d564");
            if (Double.isNaN(dval)) {
                writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_3_10.coverage", "9432cada-d588-4b86-9d06-4ff066f4c2d8");
                return BigInteger.ZERO;
            } else {
                writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_3_10.coverage", "3b0a8e79-42b6-4191-a7ae-74a80ab91834");
                return BigInteger.valueOf(dval.longValue());
            }
        } else if (val instanceof BigDecimal) {
            writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_3_10.coverage", "295b35c8-58df-49da-85b7-2b9fc20ca03d");
            return ((BigDecimal) val).toBigInteger();
        } else if (val instanceof Number) {
            writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_3_10.coverage", "c8b18f0d-ea79-4152-b631-c47c677fe864");
            return BigInteger.valueOf(((Number) val).longValue());
        } else if (val instanceof Boolean) {
            writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_3_10.coverage", "7113650e-fe54-4cd2-a437-738e66f8d4ea");
            return BigInteger.valueOf(((Boolean) val) ? 1L : 0L);
        } else if (val instanceof String) {
            writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_3_10.coverage", "803b6835-b05d-4831-84a0-415b6da8cd85");
            String string = (String) val;
            writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_3_10.coverage", "0905e9aa-bc25-4a35-a891-a181744f57f9");
            if ("".equals(string)) {
                writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_3_10.coverage", "8b9dec28-fed3-404d-b38b-30c427f82631");
                return BigInteger.ZERO;
            } else {
                writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_3_10.coverage", "42a9eba4-8e21-4808-bca4-f57c4016eb88");
                return new BigInteger(string);
            }
        } else if (val instanceof Character) {
            writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_3_10.coverage", "8523a604-fc49-4762-9aba-4cd2c0c7daa6");
            int i = ((Character) val);
            writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_3_10.coverage", "c1da878b-b55d-44ea-a943-a236e4b716ec");
            return BigInteger.valueOf(i);
        }
        writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_3_10.coverage", "de00794c-f40c-4f01-887a-764b6059a235");
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
        writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_3_10.coverage", "5c025c7d-c87b-4166-8768-8339f94ad2d9");
        if (val instanceof BigDecimal) {
            writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_3_10.coverage", "9fadea21-0a6b-4646-9e6d-dcd9c445eb17");
            return roundBigDecimal((BigDecimal) val);
        } else if (val == null) {
            writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_3_10.coverage", "f02d4c40-9bc1-479d-bd55-32df3b7c08dd");
            controlNullOperand();
            writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_3_10.coverage", "740eb119-ba95-46fa-8259-025b94ab444d");
            return BigDecimal.ZERO;
        } else if (val instanceof Double) {
            writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_3_10.coverage", "88f85d02-9754-4852-a79e-c075ded2784f");
            if (Double.isNaN(((Double) val))) {
                writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_3_10.coverage", "c1c8aabd-263a-45f5-b974-9f8b250e036d");
                return BigDecimal.ZERO;
            } else {
                writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_3_10.coverage", "c50cca18-9295-41b3-9d92-5667f17544b9");
                return roundBigDecimal(new BigDecimal(val.toString(), getMathContext()));
            }
        } else if (val instanceof Number) {
            writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_3_10.coverage", "750b0d19-24c8-48aa-b36c-2a40b7bcb348");
            return roundBigDecimal(new BigDecimal(val.toString(), getMathContext()));
        } else if (val instanceof Boolean) {
            writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_3_10.coverage", "b7e0e799-deaa-4995-bded-b200a3fa0c67");
            return BigDecimal.valueOf(((Boolean) val) ? 1. : 0.);
        } else if (val instanceof String) {
            writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_3_10.coverage", "7b49ae27-03c2-41de-8d73-6a93237e9da2");
            String string = (String) val;
            writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_3_10.coverage", "e63aadd2-c5f1-4dd4-87fe-5c1806774061");
            if ("".equals(string)) {
                writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_3_10.coverage", "6174f651-c2e1-4c05-99c8-f84c4b294eba");
                return BigDecimal.ZERO;
            }
            writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_3_10.coverage", "a2998d5e-e579-4b62-a5c3-740b06e862d3");
            return roundBigDecimal(new BigDecimal(string, getMathContext()));
        } else if (val instanceof Character) {
            writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_3_10.coverage", "19d8a644-f7db-4a28-a179-64aa7ce6c27c");
            int i = ((Character) val);
            writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_3_10.coverage", "58286f2a-d33e-40bd-9051-7372c6893719");
            return new BigDecimal(i);
        }
        writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_3_10.coverage", "82f815f8-9765-477f-a7da-2f40f7757f7f");
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
        writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_3_10.coverage", "95b3dbdb-193d-41c6-94ee-2b6dc86d2465");
        if (val == null) {
            writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_3_10.coverage", "e5c6960c-092b-4d3c-b069-16c4a743b42b");
            controlNullOperand();
            writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_3_10.coverage", "e3cac1b3-6e08-45cf-999c-54140ed2d0bd");
            return 0;
        } else if (val instanceof Double) {
            writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_3_10.coverage", "c20a9782-565b-460a-b56f-a7f7b2e49d06");
            return ((Double) val);
        } else if (val instanceof Number) {
            writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_3_10.coverage", "f8b4ea14-711f-465d-b9a9-ec2f77406a2e");
            return Double.parseDouble(String.valueOf(val));
        } else if (val instanceof Boolean) {
            writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_3_10.coverage", "bef0a842-15e6-45d3-bf4c-00ce16e19430");
            return ((Boolean) val) ? 1. : 0.;
        } else if (val instanceof String) {
            writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_3_10.coverage", "089665c5-47c3-4f16-9a68-8a6bc112b31b");
            String string = (String) val;
            writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_3_10.coverage", "e321a72b-939d-48ab-8c05-d7df9ed10fe5");
            if ("".equals(string)) {
                writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_3_10.coverage", "d54d75cd-2dd4-4749-9ca3-e45d158a19c2");
                return Double.NaN;
            } else {
                writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_3_10.coverage", "8fcb37cb-2553-4241-b2ed-fd4021059e3d");
                return Double.parseDouble(string);
            }
        } else if (val instanceof Character) {
            writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_3_10.coverage", "46b55e4a-a7f4-4013-9753-2f2311ffa540");
            int i = ((Character) val);
            writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_3_10.coverage", "cc9f6c23-924c-43a8-9e86-b484b82e8105");
            return i;
        }
        writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_3_10.coverage", "2d2c144d-57c3-413f-a88f-ea9dedf69c3f");
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
        writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_3_10.coverage", "8c07d6ae-e125-477b-834b-7c1cd10d1032");
        if (val == null) {
            writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_3_10.coverage", "27e04787-99ec-46d2-983e-bed34e08fd08");
            controlNullOperand();
            writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_3_10.coverage", "cc93ae30-67ed-427a-b418-078eb465d4fe");
            return "";
        } else if (val instanceof Double) {
            writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_3_10.coverage", "c6160c95-5513-4560-b35c-195e09492849");
            Double dval = (Double) val;
            writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_3_10.coverage", "f3a832fb-432b-4553-b279-2011db50e0ed");
            if (Double.isNaN(dval)) {
                writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_3_10.coverage", "f978fc5b-37e2-419f-9874-2b1eddffdbcf");
                return "";
            } else {
                writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_3_10.coverage", "108937c3-3162-4f32-bbb7-102195223afd");
                return dval.toString();
            }
        } else {
            writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_3_10.coverage", "ea07896d-909a-4608-a4d8-b34af5c1da97");
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
        writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_3_10.coverage", "5b4edf0a-2a5c-4b53-9391-19c33bd7a3c0");
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
        writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_3_10.coverage", "00647d14-8b39-4218-a3c9-d203b2c241e6");
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
        writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_3_10.coverage", "0e729a3f-0401-48ec-baeb-91d65b852568");
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
        writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_3_10.coverage", "b493f2d3-d567-4a64-8b03-09a353fb1904");
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
        writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_3_10.coverage", "3a335854-4656-432a-a088-71272a590fe8");
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
