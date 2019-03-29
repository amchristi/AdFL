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
        writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_1_10.coverage", "d70fe034-1b66-471d-a426-85ddb84af671");
        boolean ostrict = options.isStrictArithmetic() == null ? this.strict : options.isStrictArithmetic();
        writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_1_10.coverage", "7f9538d8-4123-4e31-adae-2d353d098146");
        MathContext bigdContext = options.getArithmeticMathContext();
        writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_1_10.coverage", "16b1d61e-d7bf-40a3-83d6-8e15222ed6d9");
        if (bigdContext == null) {
            writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_1_10.coverage", "3ed38276-78b9-48dd-a2fa-aeea77b83734");
            bigdContext = mathContext;
        }
        writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_1_10.coverage", "14cfca00-f336-447c-acba-af074f01316d");
        int bigdScale = options.getArithmeticMathScale();
        writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_1_10.coverage", "e5e9b10b-21ac-4910-8780-324cd729a0fb");
        if (bigdScale == Integer.MIN_VALUE) {
            writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_1_10.coverage", "5c854397-1078-42ea-a7aa-bf90b650f14a");
            bigdScale = mathScale;
        }
        writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_1_10.coverage", "3add1c39-36f3-40fe-b2d4-7c4d0fcfc336");
        if ((ostrict != this.strict) || bigdScale != this.mathScale || bigdContext != this.mathContext) {
            writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_1_10.coverage", "4d94c9bb-aab2-4276-a13c-f21575887484");
            return new JexlArithmetic(ostrict, bigdContext, bigdScale);
        } else {
            writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_1_10.coverage", "57c55494-ff5b-4866-9b94-525506e0c860");
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
        writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_1_10.coverage", "e66c6aa9-f10f-4ddf-b29a-239a8caa3fec");
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
        writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_1_10.coverage", "01670403-d2ed-458f-8d0d-f31deff92618");
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
        writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_1_10.coverage", "34c992ab-4768-45f2-95e9-9892e3b423d0");
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
        writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_1_10.coverage", "0435f930-62bf-4b30-a69e-09d3838aca7a");
        final long lfrom = toLong(from);
        writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_1_10.coverage", "d5302547-351e-4d39-bc82-66a48439c9e4");
        final long lto = toLong(to);
        writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_1_10.coverage", "c9b183d2-88f1-4f66-bd10-3f85fcd27232");
        if ((lfrom >= Integer.MIN_VALUE && lfrom <= Integer.MAX_VALUE) && (lto >= Integer.MIN_VALUE && lto <= Integer.MAX_VALUE)) {
            writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_1_10.coverage", "f0ccb478-9fd5-4b4d-9351-81660999f98a");
            return org.apache.commons.jexl3.internal.IntegerRange.create((int) lfrom, (int) lto);
        } else {
            writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_1_10.coverage", "f2c475da-2472-4e44-b02b-a2b1c45cd0c3");
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
        writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_1_10.coverage", "ef212f14-c22f-455f-ac5a-f35f069df79a");
        return this.strict;
    }

    /**
     * The MathContext instance used for +,-,/,*,% operations on big decimals.
     * 
     * @return the math context
     */
    public MathContext getMathContext() {
        writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_1_10.coverage", "9bdcd901-dbb7-492b-87cd-e9e82dce6798");
        return mathContext;
    }

    /**
     * The BigDecimal scale used for comparison and coericion operations.
     * 
     * @return the scale
     */
    public int getMathScale() {
        writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_1_10.coverage", "f3fe4e0b-6429-4aa6-b7c8-ad19eadce40b");
        return mathScale;
    }

    /**
     * Ensure a big decimal is rounded by this arithmetic scale and rounding mode.
     * 
     * @param number the big decimal to round
     * @return the rounded big decimal
     */
    protected BigDecimal roundBigDecimal(final BigDecimal number) {
        writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_1_10.coverage", "144c6094-9720-46c8-a233-0de2bc9412d6");
        int mscale = getMathScale();
        writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_1_10.coverage", "e4ea7cd7-d01e-4d92-a936-fe770e07626a");
        if (mscale >= 0) {
            writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_1_10.coverage", "2356203b-745e-4944-8042-a0c1969aa2ef");
            return number.setScale(mscale, getMathContext().getRoundingMode());
        } else {
            writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_1_10.coverage", "7e14c345-01a5-4b77-b34f-04b311ae1700");
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
        writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_1_10.coverage", "386683b7-4a23-4feb-a51d-81ef0034cae8");
        if (isStrict()) {
            writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_1_10.coverage", "48ef3c04-34bf-4c81-94fd-64757514f18b");
            throw new NullOperand();
        }
        writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_1_10.coverage", "fc627311-a6f8-4466-96db-43a52760377c");
        return 0;
    }

    /**
     * Throw a NPE if arithmetic is strict.
     * 
     * @throws ArithmeticException if strict
     */
    protected void controlNullOperand() {
        writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_1_10.coverage", "8fb333fe-e1fc-42a3-b678-929de0ac06c7");
        if (isStrict()) {
            writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_1_10.coverage", "89392ce0-6c7a-4f9c-89a0-aa818dd4128c");
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
        writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_1_10.coverage", "3b2b252c-fd04-464b-a2da-4bc3fb3b22dd");
        if (val instanceof Float || val instanceof Double) {
            writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_1_10.coverage", "75db47aa-75dd-41d5-8d64-4110f481cd14");
            return true;
        }
        writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_1_10.coverage", "8c6ea9a0-7e29-4768-8633-ea916315e3b0");
        if (val instanceof String) {
            writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_1_10.coverage", "217879de-e819-4604-9983-97b8c3e822c9");
            String str = (String) val;
            writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_1_10.coverage", "f0332085-ab31-48ad-871f-ee474343b19d");
            for (int c = 0; c < str.length(); ++c) {
                writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_1_10.coverage", "b733033b-1009-488c-ad51-750b387f7722");
                char ch = str.charAt(c);
                writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_1_10.coverage", "e3620637-5a5b-4155-b69a-3c3958fed928");
                if (ch == '.' || ch == 'E' || ch == 'e') {
                    writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_1_10.coverage", "4328afe9-c711-4183-8d53-25f6252f375e");
                    return FLOAT_PATTERN.matcher(str).matches();
                }
                writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_1_10.coverage", "ad794d12-798d-4756-833c-b5552bf00a47");
                if (ch != '+' && ch != '-' && ch < '0' && ch > '9') {
                    writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_1_10.coverage", "44f49c8e-fb3e-4a63-93a5-d016abc121cd");
                    break;
                }
            }
        }
        writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_1_10.coverage", "0344cca5-4b5b-47a8-99fa-0e2dcb01aa89");
        return false;
    }

    /**
     * Is Object a floating point number.
     *
     * @param o Object to be analyzed.
     * @return true if it is a Float or a Double.
     */
    protected boolean isFloatingPoint(final Object o) {
        writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_1_10.coverage", "e0f78fea-685a-452c-8481-7aeb78fa08d1");
        return o instanceof Float || o instanceof Double;
    }

    /**
     * Is Object a whole number.
     *
     * @param o Object to be analyzed.
     * @return true if Integer, Long, Byte, Short or Character.
     */
    protected boolean isNumberable(final Object o) {
        writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_1_10.coverage", "e55de2a9-35b0-4368-bf08-91f42e997fcd");
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
        writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_1_10.coverage", "92c53331-9efc-48bc-b0d6-af3d18aab8fe");
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
        writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_1_10.coverage", "0c33c9f3-fda7-4d49-8124-3819e2168f37");
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
        writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_1_10.coverage", "ed0b4121-c92a-415f-ad3f-ac900f1b9d0c");
        if (original == null) {
            writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_1_10.coverage", "3e0a8a43-c39f-4f1e-976e-b808a0fe110e");
            return null;
        }
        writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_1_10.coverage", "5658989e-e4b5-4449-93d7-988db1ccf8e8");
        Number result = original;
        writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_1_10.coverage", "cecc3fdf-ea51-4c3d-9b54-60f5c3dbfd92");
        if (original instanceof BigDecimal) {
            writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_1_10.coverage", "383e49f3-e366-43fd-946b-75dd27dbba5e");
            BigDecimal bigd = (BigDecimal) original;
            writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_1_10.coverage", "cd466b5b-4355-4970-b5a7-43201f072241");
            if (bigd.compareTo(BIGD_DOUBLE_MAX_VALUE) > 0 || bigd.compareTo(BIGD_DOUBLE_MIN_VALUE) < 0) {
                writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_1_10.coverage", "ff9fcc74-eb84-4532-b526-49c40665139d");
                return original;
            } else {
                writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_1_10.coverage", "55d7f7e4-a071-4b7a-b8e7-b61fed49a4d8");
                try {
                    writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_1_10.coverage", "f747f800-d0cc-409c-9f89-c71da1c86823");
                    long l = bigd.longValueExact();
                    writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_1_10.coverage", "e6fb705a-0ca9-45aa-a408-584ce891e09d");
                    if (narrowAccept(narrow, Integer.class) && l <= Integer.MAX_VALUE && l >= Integer.MIN_VALUE) {
                        writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_1_10.coverage", "e4cf66f6-e167-43ae-bba0-ffd77d7696fe");
                        return (int) l;
                    } else if (narrowAccept(narrow, Long.class)) {
                        writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_1_10.coverage", "11961c2e-8ffc-4f20-b756-5166a193c564");
                        return l;
                    }
                } catch (ArithmeticException xa) {
                }
            }
        }
        writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_1_10.coverage", "d6569a13-7dfd-4610-bcea-f187a9abdde5");
        if (original instanceof Double || original instanceof Float) {
            writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_1_10.coverage", "f853ab69-0f5e-4320-8e67-8ad5032553e4");
            double value = original.doubleValue();
            writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_1_10.coverage", "34ac875d-cb06-4425-a54c-2865b15582f1");
            if (narrowAccept(narrow, Float.class) && value <= Float.MAX_VALUE && value >= Float.MIN_VALUE) {
                writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_1_10.coverage", "0012cb51-52b7-4c32-81b5-6f1fd1a664aa");
                result = result.floatValue();
            }
        } else {
            writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_1_10.coverage", "0fe0e5af-676c-48db-812b-06df12c3f2b9");
            if (original instanceof BigInteger) {
                writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_1_10.coverage", "95828484-9c43-415d-bc2e-983fdafa4f49");
                BigInteger bigi = (BigInteger) original;
                writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_1_10.coverage", "6840ee5e-afab-420b-8c67-cfae355ac650");
                if (bigi.compareTo(BIGI_LONG_MAX_VALUE) > 0 || bigi.compareTo(BIGI_LONG_MIN_VALUE) < 0) {
                    writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_1_10.coverage", "db6df1cd-5978-4d8c-a11d-a759e5974185");
                    return original;
                }
            }
            writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_1_10.coverage", "ff5eb3f2-0903-4dee-a5c2-c2f3c97f319d");
            long value = original.longValue();
            writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_1_10.coverage", "369772d7-79dc-4e2d-8519-e0ad4d7e80db");
            if (narrowAccept(narrow, Byte.class) && value <= Byte.MAX_VALUE && value >= Byte.MIN_VALUE) {
                writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_1_10.coverage", "b29d15b6-bdfb-4dda-a7da-f88df9976cd7");
                result = (byte) value;
            } else if (narrowAccept(narrow, Short.class) && value <= Short.MAX_VALUE && value >= Short.MIN_VALUE) {
                writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_1_10.coverage", "2052787f-dff0-4fa7-81d4-7d5f3b75adf7");
                result = (short) value;
            } else if (narrowAccept(narrow, Integer.class) && value <= Integer.MAX_VALUE && value >= Integer.MIN_VALUE) {
                writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_1_10.coverage", "8a102901-fc51-4b96-977e-a3da5f1f9a88");
                result = (int) value;
            }
        }
        writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_1_10.coverage", "2bbb1475-64c1-4c12-9f45-c8853649dc5f");
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
        writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_1_10.coverage", "b924990a-3087-4819-b01b-d0a738accf71");
        if (!(lhs instanceof BigInteger || rhs instanceof BigInteger) && bigi.compareTo(BIGI_LONG_MAX_VALUE) <= 0 && bigi.compareTo(BIGI_LONG_MIN_VALUE) >= 0) {
            writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_1_10.coverage", "7e6c64c4-b066-49c7-b71e-9ad407e01b64");
            long l = bigi.longValue();
            writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_1_10.coverage", "d01856a1-1b8d-42bf-9057-ca52cf937245");
            if (!(lhs instanceof Long || rhs instanceof Long) && l <= Integer.MAX_VALUE && l >= Integer.MIN_VALUE) {
                writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_1_10.coverage", "2b6464ff-d5d0-47e4-9361-b613138f43b4");
                return (int) l;
            }
            writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_1_10.coverage", "785a6b74-39be-49fa-9344-5ac2da92b786");
            return l;
        }
        writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_1_10.coverage", "372a1cf6-2986-4112-a838-ab345816237e");
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
        writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_1_10.coverage", "ecf58aa9-6355-43ef-a0d9-651b2ad643e0");
        if (isNumberable(lhs) || isNumberable(rhs)) {
            writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_1_10.coverage", "969bc046-d14c-4ae6-ab36-a419e7fd9fd9");
            try {
                writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_1_10.coverage", "37fcbc17-b31c-439c-8616-4e410008dba8");
                long l = bigd.longValueExact();
                writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_1_10.coverage", "79ce7b5e-4055-4f1f-a576-14eb1d3ed85a");
                if (l <= Integer.MAX_VALUE && l >= Integer.MIN_VALUE) {
                    writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_1_10.coverage", "fd1e9271-e817-4ef7-afae-583da9263d5a");
                    return (int) l;
                } else {
                    writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_1_10.coverage", "c14414eb-1c6c-4d9d-a767-7e57fea4df5b");
                    return l;
                }
            } catch (ArithmeticException xa) {
            }
        }
        writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_1_10.coverage", "23ba1a5a-bb34-4086-8704-f6452a2840a6");
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
        writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_1_10.coverage", "891960f4-1055-486c-8739-2da3e75a348a");
        boolean narrowed = false;
        writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_1_10.coverage", "b2b626bd-0ab1-4278-bf35-afe20662cd10");
        for (int a = 0; a < args.length; ++a) {
            writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_1_10.coverage", "04ca54c0-6ade-45a9-8fa0-b27d476f0eac");
            Object arg = args[a];
            writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_1_10.coverage", "1e3b5030-c750-4a63-ac06-d8f3814fee33");
            if (arg instanceof Number) {
                writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_1_10.coverage", "a0b007df-6473-42b0-a734-de53815966c6");
                Number narg = (Number) arg;
                writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_1_10.coverage", "a6822787-c6a1-44d3-89f8-14afedf38a53");
                Number narrow = narrow(narg);
                writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_1_10.coverage", "9c59ac8d-1bc1-4c2e-a5a2-1e4cfdec27a0");
                if (!narg.equals(narrow)) {
                    writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_1_10.coverage", "7241e77e-8055-45f8-aea7-da3301e40c8d");
                    args[a] = narrow;
                    writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_1_10.coverage", "2fc99a9b-5ab2-4874-81b9-a86ffa607347");
                    narrowed = true;
                }
            }
        }
        writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_1_10.coverage", "96430386-4ae8-4099-af8b-579f157be60d");
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
        writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_1_10.coverage", "47d93f0e-82f8-4907-b51e-42c71e3b5b2b");
        if (left == null && right == null) {
            writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_1_10.coverage", "4e450374-6c8e-4b3b-862b-6f6d3ab14dde");
            return controlNullNullOperands();
        }
        writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_1_10.coverage", "bb18ccd6-6c5a-4573-8ac7-281f4ff9292d");
        boolean strconcat = strict ? left instanceof String || right instanceof String : left instanceof String && right instanceof String;
        writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_1_10.coverage", "7647d494-b49c-4126-ae1d-de28553679f7");
        if (!strconcat) {
            writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_1_10.coverage", "d316c4ea-e459-4305-9228-ba062e0c5c9b");
            try {
                writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_1_10.coverage", "971dd103-c696-43c0-a7ea-0765066d6cd0");
                if (left instanceof BigDecimal || right instanceof BigDecimal) {
                    writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_1_10.coverage", "44fa0536-8cb4-4423-bf88-ecd24c098895");
                    BigDecimal l = toBigDecimal(left);
                    writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_1_10.coverage", "5a63452e-4799-4b0d-9205-c3f80657031e");
                    BigDecimal r = toBigDecimal(right);
                    writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_1_10.coverage", "08dcf5cf-5aa2-4bdd-b434-0594157fc691");
                    BigDecimal result = l.add(r, getMathContext());
                    writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_1_10.coverage", "1a96bab8-85fc-487a-b0de-071d8ab99f8d");
                    return narrowBigDecimal(left, right, result);
                }
                writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_1_10.coverage", "8bb16ffa-cf81-4061-ad6a-9581dd3ef072");
                if (isFloatingPointNumber(left) || isFloatingPointNumber(right)) {
                    writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_1_10.coverage", "9b7a0004-ddf2-45c6-b4ba-79108db762b2");
                    double l = toDouble(left);
                    writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_1_10.coverage", "350193af-b639-410f-b038-440e7c649870");
                    double r = toDouble(right);
                    writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_1_10.coverage", "d82dfb72-ee87-4455-a14b-30c46563748c");
                    return l + r;
                }
                writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_1_10.coverage", "58aa2e06-7b60-4cb3-a6fe-26fbcb9d87f8");
                BigInteger l = toBigInteger(left);
                writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_1_10.coverage", "2f4853e3-3ac6-45a7-8e7b-572647a40406");
                BigInteger r = toBigInteger(right);
                writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_1_10.coverage", "8215f269-56ed-48a9-9101-556df402684d");
                BigInteger result = l.add(r);
                writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_1_10.coverage", "e1f47b0b-961a-44bc-bc36-abc80ea92b7a");
                return narrowBigInteger(left, right, result);
            } catch (java.lang.NumberFormatException nfe) {
                writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_1_10.coverage", "8581ed5e-946f-473a-a309-fcf58c261507");
                if (left == null || right == null) {
                    writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_1_10.coverage", "e95a377a-867c-4858-853d-b7cebb68e3c9");
                    controlNullOperand();
                }
            }
        }
        writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_1_10.coverage", "35d9231e-13ea-4f07-a02c-ef001787e723");
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
        writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_1_10.coverage", "7d819e5e-8da9-49e6-96ac-1bf5a2e08fd0");
        if (left == null && right == null) {
            writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_1_10.coverage", "47c3eeaa-ab65-40e0-b0d5-74456b3cba99");
            return controlNullNullOperands();
        }
        writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_1_10.coverage", "fcbe883c-846d-4096-a963-56b6af31aeb3");
        if (left instanceof BigDecimal || right instanceof BigDecimal) {
            writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_1_10.coverage", "ae7a2093-1c8d-42b4-a319-10e8fd6caac9");
            BigDecimal l = toBigDecimal(left);
            writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_1_10.coverage", "5ee7d013-1deb-4b6b-aad6-dec7c412fc2f");
            BigDecimal r = toBigDecimal(right);
            writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_1_10.coverage", "de58e862-a156-4e81-8859-587e05e27ed7");
            if (BigDecimal.ZERO.equals(r)) {
                writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_1_10.coverage", "2c83bd5a-8307-4257-8469-3e8cfd2a7aa6");
                throw new ArithmeticException("/");
            }
            writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_1_10.coverage", "fa06feb3-5940-4583-a118-8427c02bb1e4");
            BigDecimal result = l.divide(r, getMathContext());
            writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_1_10.coverage", "33a285f7-3821-45b4-a158-b066cbf257fb");
            return narrowBigDecimal(left, right, result);
        }
        writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_1_10.coverage", "938e5987-1a6a-44b1-8eed-27e910d09357");
        if (isFloatingPointNumber(left) || isFloatingPointNumber(right)) {
            writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_1_10.coverage", "8fdf2ded-506d-4d0f-b822-56c7e9c273ef");
            double l = toDouble(left);
            writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_1_10.coverage", "4885c4d1-8cc8-47be-b423-2858b61ba4fd");
            double r = toDouble(right);
            writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_1_10.coverage", "87f6d89e-ba70-4b14-a64f-a319fa2610f7");
            if (r == 0.0) {
                writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_1_10.coverage", "a473ac6e-8eb4-4f5b-8ec2-c56e90161efe");
                throw new ArithmeticException("/");
            }
            writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_1_10.coverage", "1b62cd63-a5fc-434c-9dfa-a9ba33ef75d9");
            return l / r;
        }
        writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_1_10.coverage", "6c6981f9-d6e8-44c4-9657-00c0ab9c2de8");
        BigInteger l = toBigInteger(left);
        writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_1_10.coverage", "7eabfc9e-3fd9-4bbd-b4c7-486d9e499f34");
        BigInteger r = toBigInteger(right);
        writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_1_10.coverage", "3776ed47-d6da-4d6f-86f9-d8498a661f75");
        if (BigInteger.ZERO.equals(r)) {
            writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_1_10.coverage", "384acbeb-7fe7-405b-8de5-9c10dc23aaf8");
            throw new ArithmeticException("/");
        }
        writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_1_10.coverage", "b3d89a06-bf75-42fd-bdca-449e3dab9ee6");
        BigInteger result = l.divide(r);
        writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_1_10.coverage", "8fcd163d-1823-404f-b2de-42a517b14014");
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
        writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_1_10.coverage", "3ffd25b8-5191-4201-917a-ebd9c904d058");
        if (left == null && right == null) {
            writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_1_10.coverage", "f249c845-5a5f-4de5-97e0-ead0be8d549c");
            return controlNullNullOperands();
        }
        writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_1_10.coverage", "d4df0eb6-842b-4e22-afe9-ce0dea2fa833");
        if (left instanceof BigDecimal || right instanceof BigDecimal) {
            writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_1_10.coverage", "f58ebdba-a2da-4030-a641-60fad272d0c4");
            BigDecimal l = toBigDecimal(left);
            writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_1_10.coverage", "2d5f4287-74ba-4624-b535-18354324963b");
            BigDecimal r = toBigDecimal(right);
            writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_1_10.coverage", "369e6760-bb24-41d8-b3c0-69285421f8a4");
            if (BigDecimal.ZERO.equals(r)) {
                writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_1_10.coverage", "addac3a3-8795-4fd1-bf54-9c6ec5912ef2");
                throw new ArithmeticException("%");
            }
            writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_1_10.coverage", "56aa1ae1-d142-45f4-b08a-acc0fce7a5bc");
            BigDecimal remainder = l.remainder(r, getMathContext());
            writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_1_10.coverage", "2f4fccd5-6435-433b-b825-92e6a64cb861");
            return narrowBigDecimal(left, right, remainder);
        }
        writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_1_10.coverage", "2086a433-7048-4a64-96d1-3cb91c7fcb9f");
        if (isFloatingPointNumber(left) || isFloatingPointNumber(right)) {
            writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_1_10.coverage", "8fe1be3f-6bb2-4643-a079-0bda5ce8428f");
            double l = toDouble(left);
            writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_1_10.coverage", "d40fc1b7-1bc0-4956-b3e7-44e0ec2c0ca0");
            double r = toDouble(right);
            writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_1_10.coverage", "4be04870-e437-48a1-ab21-e0d26a6bc591");
            if (r == 0.0) {
                writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_1_10.coverage", "ff8aa7fd-eb40-4bcc-93e6-21dc67aa9f5f");
                throw new ArithmeticException("%");
            }
            writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_1_10.coverage", "904d1f80-aa81-4c85-ab30-b7bac00d30cc");
            return l % r;
        }
        writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_1_10.coverage", "153bb205-5ded-45d2-aede-35f7d12268d4");
        BigInteger l = toBigInteger(left);
        writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_1_10.coverage", "0a4b278b-bf00-439c-8caa-bfb99798f227");
        BigInteger r = toBigInteger(right);
        writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_1_10.coverage", "e34520c4-4bc6-431d-8d75-e3d28256631f");
        BigInteger result = l.mod(r);
        writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_1_10.coverage", "77885dae-018c-405c-b06e-6ee1a3187e50");
        if (BigInteger.ZERO.equals(r)) {
            writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_1_10.coverage", "f38fa9a6-b7b0-40c5-a5d7-8e13c8bf65aa");
            throw new ArithmeticException("%");
        }
        writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_1_10.coverage", "89dd7126-d0ca-43bc-8f43-1078617a870f");
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
        writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_1_10.coverage", "e066f59c-1095-4293-a99b-b425e9ae78e7");
        if (left == null && right == null) {
            writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_1_10.coverage", "1c6ccf78-a236-456e-b52e-f3ce8630c57d");
            return controlNullNullOperands();
        }
        writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_1_10.coverage", "db6153d1-147f-4b95-bc1a-cd0a63620dbf");
        if (left instanceof BigDecimal || right instanceof BigDecimal) {
            writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_1_10.coverage", "5b098c75-3839-4fb1-a3f2-f97854c1d553");
            BigDecimal l = toBigDecimal(left);
            writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_1_10.coverage", "c6803fe3-0747-423d-b7a4-0ad72365d4d5");
            BigDecimal r = toBigDecimal(right);
            writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_1_10.coverage", "70a94bb1-a95a-4063-bf46-9e7ab1a69745");
            BigDecimal result = l.multiply(r, getMathContext());
            writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_1_10.coverage", "88190ccd-8b92-46a6-b055-e8f1e722bbd1");
            return narrowBigDecimal(left, right, result);
        }
        writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_1_10.coverage", "b7c8e616-2dd0-46c3-a622-cdd64695d720");
        if (isFloatingPointNumber(left) || isFloatingPointNumber(right)) {
            writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_1_10.coverage", "52f0289d-fef1-475f-810b-032d49deab1b");
            double l = toDouble(left);
            writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_1_10.coverage", "2d380558-156e-4bfe-8fb8-336b3b7499a7");
            double r = toDouble(right);
            writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_1_10.coverage", "d6df502c-bfcc-4315-ba25-358ae2095c22");
            return l * r;
        }
        writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_1_10.coverage", "97b27d46-cfa8-417c-b47b-1d501262cfb7");
        BigInteger l = toBigInteger(left);
        writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_1_10.coverage", "c5bee7c4-deca-4d2e-8e32-16645865d897");
        BigInteger r = toBigInteger(right);
        writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_1_10.coverage", "98a2ff5c-8e34-4e51-bcaf-6c22df5fe6ad");
        BigInteger result = l.multiply(r);
        writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_1_10.coverage", "6cc347eb-0605-4581-bbdb-3c2ac420fa7a");
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
        writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_1_10.coverage", "f72c5b58-3a28-474c-9181-a333a4491e89");
        if (left == null && right == null) {
            writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_1_10.coverage", "d3745e1a-178b-47c5-8fdd-573c9266ddd1");
            return controlNullNullOperands();
        }
        writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_1_10.coverage", "5f90502e-6e0c-40fd-bfac-1df20ab37eb3");
        if (left instanceof BigDecimal || right instanceof BigDecimal) {
            writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_1_10.coverage", "db44f45e-6ec3-413c-a45e-516ae9fc9ad1");
            BigDecimal l = toBigDecimal(left);
            writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_1_10.coverage", "2200d359-277b-45ba-a472-b24c223b9945");
            BigDecimal r = toBigDecimal(right);
            writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_1_10.coverage", "cdd14f4d-3d6f-45ce-b6ee-e2c0953794d8");
            BigDecimal result = l.subtract(r, getMathContext());
            writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_1_10.coverage", "513d7abc-1fb7-4736-9d36-12c4d8b11bc5");
            return narrowBigDecimal(left, right, result);
        }
        writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_1_10.coverage", "f2961505-6860-4e78-9fac-4ceac4851147");
        if (isFloatingPointNumber(left) || isFloatingPointNumber(right)) {
            writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_1_10.coverage", "d753c265-27d6-46b0-82ab-e069286181af");
            double l = toDouble(left);
            writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_1_10.coverage", "4313b523-1082-4516-8600-a76d50a07148");
            double r = toDouble(right);
            writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_1_10.coverage", "bf2092a2-0e65-4b4c-b24e-d12ca1d4f64d");
            return l - r;
        }
        writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_1_10.coverage", "32326d45-d03c-4a29-aeeb-79612cb0483e");
        BigInteger l = toBigInteger(left);
        writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_1_10.coverage", "eea6b247-602d-41e5-b683-18b994297216");
        BigInteger r = toBigInteger(right);
        writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_1_10.coverage", "ea8dc0a7-47a4-4387-8541-68ddbda73329");
        BigInteger result = l.subtract(r);
        writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_1_10.coverage", "5076fc79-a3dd-4dbc-96dc-252cb0e99c3d");
        return narrowBigInteger(left, right, result);
    }

    /**
     * Negates a value (unary minus for numbers).
     * 
     * @param val the value to negate
     * @return the negated value
     */
    public Object negate(Object val) {
        writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_1_10.coverage", "0f0dafe5-94aa-434a-be64-06b2ef9eafa0");
        if (val instanceof Integer) {
            writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_1_10.coverage", "e3428402-fdc5-4c97-907b-d3ea934f24e6");
            return -((Integer) val);
        } else if (val instanceof Double) {
            writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_1_10.coverage", "2b0979aa-b020-4500-919b-65af29fb2470");
            return -((Double) val);
        } else if (val instanceof Long) {
            writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_1_10.coverage", "38348750-892b-4808-93d1-3364bb26e53b");
            return -((Long) val);
        } else if (val instanceof BigDecimal) {
            writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_1_10.coverage", "477d184c-e036-45ea-802c-ff19f2854bd3");
            return ((BigDecimal) val).negate();
        } else if (val instanceof BigInteger) {
            writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_1_10.coverage", "96c11ace-393c-45ad-af66-8f8ef9c89f6c");
            return ((BigInteger) val).negate();
        } else if (val instanceof Float) {
            writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_1_10.coverage", "146d651f-bae4-4d14-a932-c42d93e735f7");
            return -((Float) val);
        } else if (val instanceof Short) {
            writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_1_10.coverage", "7278498b-7602-4cbb-98fe-e5f1ee97d348");
            return (short) -((Short) val);
        } else if (val instanceof Byte) {
            writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_1_10.coverage", "a1ef46d6-1c78-4fc9-ba6d-00181fa5a28a");
            return (byte) -((Byte) val);
        } else if (val instanceof Boolean) {
            writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_1_10.coverage", "f5442a09-ca84-416f-b530-bee6ed21ef69");
            return ((Boolean) val) ? Boolean.FALSE : Boolean.TRUE;
        }
        writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_1_10.coverage", "4cf1e296-0603-40ed-ba29-3be6f358e2dd");
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
        writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_1_10.coverage", "59386af1-15f0-47d7-9822-b8a239fe2014");
        if (value == null && container == null) {
            writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_1_10.coverage", "a72c0e5b-5653-47e3-a4f6-5736babd65b3");
            return true;
        }
        writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_1_10.coverage", "f1d42fd6-801d-4e9b-bcd0-eb6d6e028015");
        if (value == null || container == null) {
            writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_1_10.coverage", "d5af6054-20f4-462f-91bc-24e09b316227");
            return false;
        }
        writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_1_10.coverage", "495ca723-f235-46cc-84e6-8e6a1c8c74a8");
        if (container instanceof java.util.regex.Pattern) {
            writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_1_10.coverage", "ebd57696-7a87-4885-873a-c968eb128c28");
            return ((java.util.regex.Pattern) container).matcher(value.toString()).matches();
        }
        writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_1_10.coverage", "df8d6a3a-a45a-4043-84d0-de896e22bc22");
        if (container instanceof String) {
            writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_1_10.coverage", "62142409-70ed-4314-b4be-1a7f04cbe0f0");
            return value.toString().matches(container.toString());
        }
        writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_1_10.coverage", "1d8337a4-f918-48ce-8092-872dd3ce98cf");
        if (container instanceof Map<?, ?>) {
            writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_1_10.coverage", "285fb2d0-c50f-4119-813b-55f3cd7e1a69");
            if (value instanceof Map<?, ?>) {
                writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_1_10.coverage", "28836f23-f963-4e28-93b2-76da39899517");
                return ((Map<?, ?>) container).keySet().containsAll(((Map<?, ?>) value).keySet());
            }
            writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_1_10.coverage", "4121d61e-84ef-4d02-9a8a-cdb6647938ed");
            return ((Map<?, ?>) container).containsKey(value);
        }
        writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_1_10.coverage", "82ff65b5-3c64-4a7b-adca-d9d0381e40cd");
        if (container instanceof Collection<?>) {
            writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_1_10.coverage", "5e232942-e3ed-4027-9cab-20e602d3ccf5");
            if (value instanceof Collection<?>) {
                writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_1_10.coverage", "a557b3cd-1d36-46b0-8382-20db9710d76d");
                return ((Collection<?>) container).containsAll((Collection<?>) value);
            }
            writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_1_10.coverage", "c75ad635-ff4e-44f4-a733-3a6bc3638dee");
            return ((Collection<?>) container).contains(value);
        }
        writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_1_10.coverage", "95c8d49c-0494-4e52-9b5a-ad2cf318564e");
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
        writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_1_10.coverage", "bdf4624f-b506-42d5-94d3-e359a5957fbe");
        if (left == null && right == null) {
            writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_1_10.coverage", "d9461085-7d59-4d59-9433-faa769fea515");
            return true;
        }
        writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_1_10.coverage", "3fc54d82-79c9-421f-a5c0-b0480ba0cb8a");
        if (left == null || right == null) {
            writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_1_10.coverage", "a9f9639a-988a-4e4d-a24d-47eb0fe89a2d");
            return false;
        }
        writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_1_10.coverage", "f3f07cc1-de62-4b21-8074-dac512b0854c");
        if (left instanceof String) {
            writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_1_10.coverage", "562748df-76f4-49df-9a48-da81ae92314a");
            return ((String) left).endsWith(toString(right));
        }
        writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_1_10.coverage", "9cffcca2-3e62-4461-90d5-6d5996f4692f");
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
        writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_1_10.coverage", "58b0c010-b397-4681-8d54-703badf80946");
        if (left == null && right == null) {
            writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_1_10.coverage", "86dbac78-31be-4415-b50e-010520c13aa6");
            return true;
        }
        writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_1_10.coverage", "c46202cd-9eda-47f9-89ba-814707c73255");
        if (left == null || right == null) {
            writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_1_10.coverage", "4aa96838-15d8-478d-87be-36c60c14e702");
            return false;
        }
        writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_1_10.coverage", "c2f92b2a-171f-4773-9026-637a7e63ad6c");
        if (left instanceof String) {
            writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_1_10.coverage", "32bec83c-e547-4bf4-8273-2527d29d77fe");
            return ((String) left).startsWith(toString(right));
        }
        writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_1_10.coverage", "1a174448-5089-4a79-bfb0-ad7a6ed582bb");
        return null;
    }

    /**
     * Check for emptyness of various types: Number, Collection, Array, Map, String.
     *
     * @param object the object to check the emptyness of
     * @return the boolean or null of there is no arithmetic solution
     */
    public Boolean isEmpty(Object object) {
        writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_1_10.coverage", "835c9dfa-293a-48bf-8476-c6b6559f0ef6");
        if (object instanceof Number) {
            writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_1_10.coverage", "ab5c6bb0-2002-4821-a9e8-d0217fece96a");
            double d = ((Number) object).doubleValue();
            writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_1_10.coverage", "4085df34-8c22-4387-bdd0-4b571e69c2ce");
            return Double.isNaN(d) || d == 0.d ? Boolean.TRUE : Boolean.FALSE;
        }
        writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_1_10.coverage", "e42b47a2-45e6-4c46-92f3-42d93a1cf658");
        if (object instanceof String) {
            writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_1_10.coverage", "6051853e-667c-43f1-b787-ec0325a725db");
            return "".equals(object) ? Boolean.TRUE : Boolean.FALSE;
        }
        writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_1_10.coverage", "f75142b7-d800-4a65-a267-62d18300f6ce");
        if (object.getClass().isArray()) {
            writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_1_10.coverage", "e69ef086-152e-4daa-9534-037a4ea832b0");
            return Array.getLength(object) == 0 ? Boolean.TRUE : Boolean.FALSE;
        }
        writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_1_10.coverage", "c345be81-71fd-46b7-945f-356c6cf367f1");
        if (object instanceof Collection<?>) {
            writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_1_10.coverage", "36b8efa4-9ff1-4514-8a83-d570a7480ce9");
            return ((Collection<?>) object).isEmpty() ? Boolean.TRUE : Boolean.FALSE;
        }
        writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_1_10.coverage", "eab05618-fac7-4e04-ae3c-851e92d93552");
        if (object instanceof Map<?, ?>) {
            writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_1_10.coverage", "68bf0dc8-79e0-44f9-b2e6-e229f8561ba0");
            return ((Map<?, ?>) object).isEmpty() ? Boolean.TRUE : Boolean.FALSE;
        }
        writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_1_10.coverage", "48519336-7ef9-4f5d-9ea0-e2735777736f");
        return null;
    }

    /**
     * Calculate the <code>size</code> of various types: Collection, Array, Map, String.
     *
     * @param object the object to get the size of
     * @return the size of object or null if there is no arithmetic solution
     */
    public Integer size(Object object) {
        writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_1_10.coverage", "e1bedca5-a5f7-43f6-9eb7-3d68302a0b9c");
        if (object instanceof String) {
            writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_1_10.coverage", "598be8c1-157f-4c0c-b197-d3002b5d9001");
            return ((String) object).length();
        }
        writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_1_10.coverage", "f670ddc4-2fa1-4886-ac4a-a3995b49ed02");
        if (object.getClass().isArray()) {
            writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_1_10.coverage", "bc7e8a55-80d2-4c99-8bc9-5f6eb87011c8");
            return Array.getLength(object);
        }
        writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_1_10.coverage", "23cad3c1-b63f-4b1b-85e7-d15f56d59466");
        if (object instanceof Collection<?>) {
            writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_1_10.coverage", "ab5471fc-3361-479c-98f2-ab0b0e63ba22");
            return ((Collection<?>) object).size();
        }
        writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_1_10.coverage", "73d49ea4-cd45-4b2b-8e34-c32d6dafa603");
        if (object instanceof Map<?, ?>) {
            writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_1_10.coverage", "083a6e93-771c-45c9-bc75-3e3299c8c114");
            return ((Map<?, ?>) object).size();
        }
        writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_1_10.coverage", "b5790a1b-58dc-4315-9607-dd4fd8bc1169");
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
        writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_1_10.coverage", "7ad28619-0b8f-4510-9503-bb5282bc2c7f");
        long l = toLong(left);
        writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_1_10.coverage", "f0bc0242-5f39-41ae-8fa6-dabe90e942e7");
        long r = toLong(right);
        writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_1_10.coverage", "e6c8ec08-4e4f-4859-a73d-bb2350e1347a");
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
        writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_1_10.coverage", "88926d92-d868-4ac4-aa50-f16ae76e0f8d");
        long l = toLong(left);
        writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_1_10.coverage", "74da9199-7994-425b-a034-d7000761ea7d");
        long r = toLong(right);
        writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_1_10.coverage", "b647754a-6202-4c73-a686-c14815d24c7d");
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
        writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_1_10.coverage", "e81e95f4-2ea4-455e-8795-da8920cee657");
        long l = toLong(left);
        writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_1_10.coverage", "2a0e6717-eb4e-4022-914e-1d3bc8dc0c17");
        long r = toLong(right);
        writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_1_10.coverage", "e37fcf55-2fd4-47f9-83c9-be2f4636ea4a");
        return l ^ r;
    }

    /**
     * Performs a bitwise complement.
     * 
     * @param val the operand
     * @return ~val
     */
    public Object complement(Object val) {
        writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_1_10.coverage", "9505ccb7-6544-4ce0-8815-fe18e555578d");
        long l = toLong(val);
        writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_1_10.coverage", "d1f2d148-77e5-4a54-850a-0a14ec653e1b");
        return ~l;
    }

    /**
     * Performs a logical not.
     * 
     * @param val the operand
     * @return !val
     */
    public Object not(Object val) {
        writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_1_10.coverage", "9104194f-a5ec-4578-80c6-0ce0ca6c0b1e");
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
        writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_1_10.coverage", "b23b898e-2279-4d97-8369-0bff6edc2ce0");
        if (left != null && right != null) {
            writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_1_10.coverage", "4dde4827-7a3e-43b2-8c00-ea89f6d0740f");
            if (left instanceof BigDecimal || right instanceof BigDecimal) {
                writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_1_10.coverage", "809b7610-bf4d-4e5a-9f3e-01cc28c4857d");
                BigDecimal l = toBigDecimal(left);
                writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_1_10.coverage", "731743af-7117-4620-8ecd-391ba24d0b9b");
                BigDecimal r = toBigDecimal(right);
                writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_1_10.coverage", "e1505690-ee1e-4024-8987-5620431d36ba");
                return l.compareTo(r);
            } else if (left instanceof BigInteger || right instanceof BigInteger) {
                writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_1_10.coverage", "b537a719-f005-414c-afa4-f8d27f693f27");
                BigInteger l = toBigInteger(left);
                writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_1_10.coverage", "eb593dfb-d4b9-40ea-a381-a8138e0c0a62");
                BigInteger r = toBigInteger(right);
                writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_1_10.coverage", "c7686061-a535-42d1-9a29-912f1b3c6343");
                return l.compareTo(r);
            } else if (isFloatingPoint(left) || isFloatingPoint(right)) {
                writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_1_10.coverage", "a79e1a58-61ef-4d28-b7d1-a44048edc07e");
                double lhs = toDouble(left);
                writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_1_10.coverage", "d0815cbb-ba87-4ec0-aa30-3103dbb8a851");
                double rhs = toDouble(right);
                writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_1_10.coverage", "3c3efaa5-8ac1-41ff-bb5b-a5bd7ca89a0d");
                if (Double.isNaN(lhs)) {
                    writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_1_10.coverage", "2c3bd941-d8c9-47d4-8610-cadd978206c0");
                    if (Double.isNaN(rhs)) {
                        writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_1_10.coverage", "e9ff28c3-4b2a-47b0-a77d-c4ec3c7cabcd");
                        return 0;
                    } else {
                        writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_1_10.coverage", "555a6d02-0387-49e4-a0dd-5cc083235aa9");
                        return -1;
                    }
                } else if (Double.isNaN(rhs)) {
                    writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_1_10.coverage", "75578ae9-7173-4b22-9fd5-6cb77b03d8bd");
                    return +1;
                } else if (lhs < rhs) {
                    writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_1_10.coverage", "7402a0ed-a63a-40f7-adf2-c9883de08695");
                    return -1;
                } else if (lhs > rhs) {
                    writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_1_10.coverage", "e3b81dbc-6c6f-4968-8b06-f3c96950a0c7");
                    return +1;
                } else {
                    writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_1_10.coverage", "3a311221-7f41-403e-b873-602e3c97e35b");
                    return 0;
                }
            } else if (isNumberable(left) || isNumberable(right)) {
                writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_1_10.coverage", "3562f0c4-1cc6-4ad8-ac64-208ef60fd883");
                long lhs = toLong(left);
                writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_1_10.coverage", "e6a9a52a-91e6-4551-a50f-888e5a48c280");
                long rhs = toLong(right);
                writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_1_10.coverage", "045e329b-e0d4-4f6d-97d8-76305f19725b");
                if (lhs < rhs) {
                    writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_1_10.coverage", "b88dec91-1c4e-4c38-a41f-985b8a185f41");
                    return -1;
                } else if (lhs > rhs) {
                    writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_1_10.coverage", "b3f99322-05fd-4dc3-a635-5a0979bcfc69");
                    return +1;
                } else {
                    writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_1_10.coverage", "32985a3d-0d1a-44cc-9da7-195e46053a04");
                    return 0;
                }
            } else if (left instanceof String || right instanceof String) {
                writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_1_10.coverage", "2d2b4110-80fd-4b31-abaf-ae7d65161770");
                return toString(left).compareTo(toString(right));
            } else if ("==".equals(operator)) {
                writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_1_10.coverage", "66f6fd99-c408-4641-aa9e-e9d745c3d6f6");
                return left.equals(right) ? 0 : -1;
            } else if (left instanceof Comparable<?>) {
                writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_1_10.coverage", "3aa9b470-5ae5-4237-a9c5-31bb3677e605");
                @SuppressWarnings("unchecked") final Comparable<Object> comparable = (Comparable<Object>) left;
                writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_1_10.coverage", "9af126f3-e7e1-49c0-ac99-3059a08d4119");
                return comparable.compareTo(right);
            } else if (right instanceof Comparable<?>) {
                writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_1_10.coverage", "ca6ab164-47c3-4481-be13-b077ea99c729");
                @SuppressWarnings("unchecked") final Comparable<Object> comparable = (Comparable<Object>) right;
                writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_1_10.coverage", "d14e0a6a-c83c-4a1a-8627-746aa26b90ac");
                return comparable.compareTo(left);
            }
        }
        writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_1_10.coverage", "86379f59-1e80-4bb0-8970-ed62c04bc1b3");
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
        writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_1_10.coverage", "94ef6e45-c9b8-4add-b8c0-4d00ea21f21c");
        if (left == right) {
            writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_1_10.coverage", "053f36ee-4af1-4021-858e-fede08eedad4");
            return true;
        } else if (left == null || right == null) {
            writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_1_10.coverage", "c17770f1-9506-43f5-b3de-ac19d3eb8240");
            return false;
        } else if (left instanceof Boolean || right instanceof Boolean) {
            writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_1_10.coverage", "87e4a5e9-5647-422f-94a7-61c337eaac2e");
            return toBoolean(left) == toBoolean(right);
        } else {
            writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_1_10.coverage", "6f73c9c4-5d31-402c-9470-e7e5fa8479ac");
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
        writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_1_10.coverage", "758adf88-2f47-4102-86df-7dea7617fd3c");
        if ((left == right) || (left == null) || (right == null)) {
            writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_1_10.coverage", "786d6fb2-bf2d-4706-8841-9f0996d2a138");
            return false;
        } else {
            writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_1_10.coverage", "8e961222-55f6-492f-913b-5792837bb7e1");
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
        writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_1_10.coverage", "6cb9aa72-869c-4050-8142-0572d3dd1ad9");
        if ((left == right) || left == null || right == null) {
            writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_1_10.coverage", "360a04a0-d525-4aa4-aed6-4f0eaf5be4eb");
            return false;
        } else {
            writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_1_10.coverage", "9ccefe41-7cf4-4ba8-8757-94e40bb8c823");
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
        writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_1_10.coverage", "22fb1763-07c5-4a46-877d-a4bcc845e401");
        if (left == right) {
            writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_1_10.coverage", "64dfecd3-1b97-422b-825f-5f1c6d00eb8a");
            return true;
        } else if (left == null || right == null) {
            writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_1_10.coverage", "15f6a14c-d3d9-4791-9fc7-79fdc1bf0faa");
            return false;
        } else {
            writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_1_10.coverage", "b2003e2a-8ec7-4c63-97ca-658020d0bb3d");
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
        writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_1_10.coverage", "a92821fe-3f3e-4b5d-b7cb-826fc86d94d0");
        if (left == right) {
            writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_1_10.coverage", "9bf96d2d-a2bb-4f71-879e-77d8c7407650");
            return true;
        } else if (left == null || right == null) {
            writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_1_10.coverage", "d8177fda-2482-45e9-9bfe-085bae960bb7");
            return false;
        } else {
            writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_1_10.coverage", "de3f650d-cbbd-4db2-a20a-4cd10561527e");
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
        writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_1_10.coverage", "770992e1-6f78-42c2-bc0d-90db1bdadc74");
        if (val == null) {
            writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_1_10.coverage", "3bca519b-3ac7-4d2b-a636-347f07ecf580");
            controlNullOperand();
            writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_1_10.coverage", "b5115b00-af0b-4a8f-af56-bc8ec3dbbf44");
            return false;
        } else if (val instanceof Boolean) {
            writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_1_10.coverage", "772ebcdf-c5cf-448a-a319-a050a20c8480");
            return ((Boolean) val);
        } else if (val instanceof Number) {
            writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_1_10.coverage", "ce176c2e-f271-4d53-b0c5-96c8aa4ebadc");
            double number = toDouble(val);
            writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_1_10.coverage", "088d2f3e-1c9d-464a-9044-72dd6647b21c");
            return !Double.isNaN(number) && number != 0.d;
        } else if (val instanceof String) {
            writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_1_10.coverage", "6cb93add-680f-42ca-966d-fa9b3d079009");
            String strval = val.toString();
            writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_1_10.coverage", "5b08f5dc-2894-4948-a8c0-bd683a05582a");
            return strval.length() > 0 && !"false".equals(strval);
        } else {
            writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_1_10.coverage", "5fbd84c8-4e30-47e7-9098-df0f4dd2c4a7");
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
        writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_1_10.coverage", "3f53ac8d-0f08-43aa-9765-5ec67b73edad");
        if (val == null) {
            writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_1_10.coverage", "dae3749d-0245-49c4-a435-5f0ee3ee17da");
            controlNullOperand();
            writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_1_10.coverage", "d87d56a8-c73a-47a6-9584-0d5b69779d83");
            return 0;
        } else if (val instanceof Double) {
            writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_1_10.coverage", "2d9ab815-b660-4902-b19a-64613f585f59");
            Double dval = (Double) val;
            writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_1_10.coverage", "ee3b3a31-54a9-4635-91a7-d9a77ced087e");
            if (Double.isNaN(dval)) {
                writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_1_10.coverage", "8d1f148d-88f6-40bd-9031-00e66e2f761f");
                return 0;
            } else {
                writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_1_10.coverage", "63e9149f-6fa6-4cc3-b488-84baad955f65");
                return dval.intValue();
            }
        } else if (val instanceof Number) {
            writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_1_10.coverage", "f481b8d1-9a84-40f2-90c7-f4725d90c50e");
            return ((Number) val).intValue();
        } else if (val instanceof String) {
            writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_1_10.coverage", "db49eafb-ac4e-42b6-b3ff-32d9e316edf7");
            if ("".equals(val)) {
                writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_1_10.coverage", "67d130bb-53ad-461a-8ea0-9e064c289567");
                return 0;
            }
            writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_1_10.coverage", "023b219b-1b37-4ee3-8c7f-466489dbc682");
            return Integer.parseInt((String) val);
        } else if (val instanceof Boolean) {
            writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_1_10.coverage", "2c77ca79-a551-4e74-b1fc-9f8d90f57a71");
            return ((Boolean) val) ? 1 : 0;
        } else if (val instanceof Character) {
            writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_1_10.coverage", "5c3a5a46-4527-478b-9d65-cfbbc4112e26");
            return ((Character) val);
        }
        writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_1_10.coverage", "9fa39395-c946-4483-9058-8f3e54e9a3f7");
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
        writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_1_10.coverage", "2b3d52c0-28f9-448e-a784-fe646fad1fd2");
        if (val == null) {
            writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_1_10.coverage", "d9533d6d-3f27-436f-b446-9fb90554bd6b");
            controlNullOperand();
            writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_1_10.coverage", "ed7a04d1-541e-4690-bf41-675b42081cd1");
            return 0L;
        } else if (val instanceof Double) {
            writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_1_10.coverage", "156cb212-aca3-4ecc-a3d5-76a21b65b637");
            Double dval = (Double) val;
            writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_1_10.coverage", "cfcad288-1b96-4fab-a24a-71e248649b7c");
            if (Double.isNaN(dval)) {
                writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_1_10.coverage", "4d7f13f7-3ff5-4357-aa1f-769c4484209c");
                return 0L;
            } else {
                writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_1_10.coverage", "43c7dc14-cddb-4220-bab2-2b2ce01fcbbd");
                return dval.longValue();
            }
        } else if (val instanceof Number) {
            writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_1_10.coverage", "c1cb8d0e-3781-4369-9178-79b44c1e48e8");
            return ((Number) val).longValue();
        } else if (val instanceof String) {
            writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_1_10.coverage", "e1d5eea2-fb9d-4456-b505-b72fcfbbc760");
            if ("".equals(val)) {
                writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_1_10.coverage", "ddcf277e-15a7-4de1-97b8-b8f6984a1d25");
                return 0L;
            } else {
                writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_1_10.coverage", "ccf4eb27-0dab-4ebe-a77c-d9964d9be4b3");
                return Long.parseLong((String) val);
            }
        } else if (val instanceof Boolean) {
            writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_1_10.coverage", "3d9c42ae-b71b-442a-82f4-d99a05d7b00d");
            return ((Boolean) val) ? 1L : 0L;
        } else if (val instanceof Character) {
            writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_1_10.coverage", "b04400fc-ed08-4761-aa78-6d9e6dbf7a27");
            return ((Character) val);
        }
        writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_1_10.coverage", "24101233-efa1-4059-af51-095c19290cee");
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
        writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_1_10.coverage", "e63cf55c-04b2-4b7f-afef-ee358f720e83");
        if (val == null) {
            writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_1_10.coverage", "fc9b8070-4802-4748-8192-0d6386bc9cbe");
            controlNullOperand();
            writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_1_10.coverage", "2f973d0e-1055-483c-8e53-be92e1de319e");
            return BigInteger.ZERO;
        } else if (val instanceof BigInteger) {
            writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_1_10.coverage", "720f2e10-a732-423c-bb8f-9060e79660c4");
            return (BigInteger) val;
        } else if (val instanceof Double) {
            writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_1_10.coverage", "20cafbe2-cf81-4ac1-9341-f4c1f266f03c");
            Double dval = (Double) val;
            writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_1_10.coverage", "23ed4ef5-3e6d-4941-980c-4c8856388f0e");
            if (Double.isNaN(dval)) {
                writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_1_10.coverage", "508514d5-7da0-4e90-8d4e-de661d2042dc");
                return BigInteger.ZERO;
            } else {
                writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_1_10.coverage", "dbc41a01-8442-4168-a9cf-ab7108dce4bf");
                return BigInteger.valueOf(dval.longValue());
            }
        } else if (val instanceof BigDecimal) {
            writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_1_10.coverage", "ed9f17c9-f3a0-4f10-b5c3-e4ded4d7327a");
            return ((BigDecimal) val).toBigInteger();
        } else if (val instanceof Number) {
            writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_1_10.coverage", "9aed1c1c-8a48-4f69-a3b2-02db1b304dd9");
            return BigInteger.valueOf(((Number) val).longValue());
        } else if (val instanceof Boolean) {
            writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_1_10.coverage", "77b1513d-e0a6-4496-8f12-18a018f36017");
            return BigInteger.valueOf(((Boolean) val) ? 1L : 0L);
        } else if (val instanceof String) {
            writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_1_10.coverage", "9af43645-1641-47cd-853d-7d5eaa1f5402");
            String string = (String) val;
            writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_1_10.coverage", "e6f13402-1cee-49de-a9fc-283909953218");
            if ("".equals(string)) {
                writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_1_10.coverage", "f69d5c49-8e7b-4796-8b32-adbae7687bbc");
                return BigInteger.ZERO;
            } else {
                writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_1_10.coverage", "82bf821d-8838-4351-80c4-e67c3a60686b");
                return new BigInteger(string);
            }
        } else if (val instanceof Character) {
            writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_1_10.coverage", "0d5a6b9f-81c3-442a-890b-bfb53a08e15c");
            int i = ((Character) val);
            writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_1_10.coverage", "99c18cc9-6be1-480a-bd96-30ba5b6a3f77");
            return BigInteger.valueOf(i);
        }
        writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_1_10.coverage", "a3a82635-398b-4ab7-ae5f-d9ef04ab813c");
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
        writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_1_10.coverage", "1edb9c07-0f22-4381-b087-54b93390c084");
        if (val instanceof BigDecimal) {
            writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_1_10.coverage", "66387809-0982-4a66-b4d3-4a186d39833a");
            return roundBigDecimal((BigDecimal) val);
        } else if (val == null) {
            writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_1_10.coverage", "96ac9f57-18b8-4a5b-bf77-8ad6ea2b6df8");
            controlNullOperand();
            writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_1_10.coverage", "b5f1ea18-49e4-404f-82b3-bef61fbae828");
            return BigDecimal.ZERO;
        } else if (val instanceof Double) {
            writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_1_10.coverage", "8469500d-cfbd-4de0-99bd-489f9721bedd");
            if (Double.isNaN(((Double) val))) {
                writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_1_10.coverage", "88cbc049-2173-410d-a1f3-91637410d4af");
                return BigDecimal.ZERO;
            } else {
                writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_1_10.coverage", "49b0c8ee-0654-4993-b7fd-e327bec66efb");
                return roundBigDecimal(new BigDecimal(val.toString(), getMathContext()));
            }
        } else if (val instanceof Number) {
            writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_1_10.coverage", "ab12bf70-838b-4a30-9112-68817192c277");
            return roundBigDecimal(new BigDecimal(val.toString(), getMathContext()));
        } else if (val instanceof Boolean) {
            writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_1_10.coverage", "ccdc4da8-298c-4f00-a255-fc528b456662");
            return BigDecimal.valueOf(((Boolean) val) ? 1. : 0.);
        } else if (val instanceof String) {
            writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_1_10.coverage", "6732f000-e389-4e61-88f2-8bc679459208");
            String string = (String) val;
            writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_1_10.coverage", "6fa847ba-7624-4c8e-89ba-f6366288b56b");
            if ("".equals(string)) {
                writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_1_10.coverage", "8f49794a-8eec-48ec-8e7c-33a110c9ea3c");
                return BigDecimal.ZERO;
            }
            writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_1_10.coverage", "17d92a09-653b-4738-8ccb-6b936fc7330a");
            return roundBigDecimal(new BigDecimal(string, getMathContext()));
        } else if (val instanceof Character) {
            writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_1_10.coverage", "70251b88-c921-46e3-a99b-c6515c1127c4");
            int i = ((Character) val);
            writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_1_10.coverage", "482abdc2-7d6e-46c5-b06d-53e44b2843d7");
            return new BigDecimal(i);
        }
        writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_1_10.coverage", "cf1b4195-e265-43ca-90e8-dcf97c7caf5f");
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
        writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_1_10.coverage", "423b9a51-3ed1-47a2-9c23-ac1373fa4d9d");
        if (val == null) {
            writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_1_10.coverage", "66d3b887-8c84-4a39-a628-d68a52e5e080");
            controlNullOperand();
            writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_1_10.coverage", "5ce726d3-a8c8-43db-9e3b-aa84c28a69b3");
            return 0;
        } else if (val instanceof Double) {
            writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_1_10.coverage", "5b994092-310a-4712-8a33-601a625e3f50");
            return ((Double) val);
        } else if (val instanceof Number) {
            writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_1_10.coverage", "72db8726-4b30-47aa-96e5-0b02f43d744a");
            return Double.parseDouble(String.valueOf(val));
        } else if (val instanceof Boolean) {
            writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_1_10.coverage", "679ffab8-6276-4ac4-8966-a036d940ef5c");
            return ((Boolean) val) ? 1. : 0.;
        } else if (val instanceof String) {
            writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_1_10.coverage", "02830907-5909-452e-8340-1379ac67270f");
            String string = (String) val;
            writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_1_10.coverage", "e343b1ae-431a-46d2-bd2c-4e9f9f70e02a");
            if ("".equals(string)) {
                writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_1_10.coverage", "733f1960-60d8-4739-b83e-1d1e075e4c90");
                return Double.NaN;
            } else {
                writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_1_10.coverage", "c237c254-aadb-48cf-a029-7b6ccf10c53a");
                return Double.parseDouble(string);
            }
        } else if (val instanceof Character) {
            writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_1_10.coverage", "22ed7b55-312f-49bb-9e39-71e11c2828d2");
            int i = ((Character) val);
            writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_1_10.coverage", "2bb3a52f-d366-42fa-abf1-b743694f54ef");
            return i;
        }
        writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_1_10.coverage", "1cd32fb9-2523-44c5-bbe8-e94b9c37df0a");
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
        writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_1_10.coverage", "3555a516-e765-4d06-9131-ecfaf741d4ae");
        if (val == null) {
            writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_1_10.coverage", "a679e739-c5ea-4d8b-9e26-6d1e9b98fb72");
            controlNullOperand();
            writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_1_10.coverage", "fd4f417d-a19b-45d1-b65f-ab64f600f59a");
            return "";
        } else if (val instanceof Double) {
            writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_1_10.coverage", "ce20b1ce-f93c-4824-a1e0-ef168f10ea2e");
            Double dval = (Double) val;
            writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_1_10.coverage", "1385386b-a0ea-41c6-9cca-215e6d843bb5");
            if (Double.isNaN(dval)) {
                writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_1_10.coverage", "e3265aaf-d4e0-4d16-b691-6e0434175ae8");
                return "";
            } else {
                writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_1_10.coverage", "5025ea4e-7ae5-4420-abb0-206779eaacf3");
                return dval.toString();
            }
        } else {
            writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_1_10.coverage", "f1252761-f364-4893-a43d-d8b4d7b9d565");
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
        writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_1_10.coverage", "22b401fa-0fbc-4d48-a802-21a8b930eacb");
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
        writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_1_10.coverage", "4ee8f56f-55ac-472c-82ab-5af725b56b45");
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
        writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_1_10.coverage", "b6ee5463-564d-412d-8931-9b7b5a1186b0");
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
        writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_1_10.coverage", "00df505e-157f-47c4-8fd3-ab5beb484d2e");
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
        writeline("/home/ubuntu/results/coverage/JexlArithmetic/JexlArithmetic_1_10.coverage", "4a2212b1-2af7-4398-a4f7-17e135a9b36a");
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
