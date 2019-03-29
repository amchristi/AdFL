package org.apache.commons.jexl3;

import java.math.MathContext;
import java.nio.charset.Charset;
import java.util.Collections;
import java.util.Map;
import java.io.*;

/**
 * A JEXL evaluation environment wrapping variables, namespace and options.
 */
public class JexlEvalContext implements JexlContext, JexlContext.NamespaceResolver, JexlEngine.Options {

    /** The marker for the empty vars. */
    private static final Map<String, Object> EMPTY_MAP = Collections.<String, Object>emptyMap();

    /** The variables.*/
    private final JexlContext vars;

    /** The namespace. */
    private final JexlContext.NamespaceResolver ns;

    /** Whether the engine should be silent. */
    private Boolean silent = null;

    /** Whether the engine should be strict. */
    private Boolean strict = null;

    /** Whether the arithmetic should be strict. */
    private Boolean mathStrict = null;

    /** The math scale the arithmetic should use. */
    private int mathScale = Integer.MIN_VALUE;

    /** The math context the arithmetic should use. */
    private MathContext mathContext = null;

    /**
     * Default constructor.
     */
    public JexlEvalContext() {
        this(EMPTY_MAP);
    }

    @Override
    public Charset getCharset() {
        writeline("/home/ubuntu/results/coverage/JexlEvalContext/JexlEvalContext_1_10.coverage", "10188e05-2635-417a-ae74-674afcd94a89");
        return Charset.defaultCharset();
    }

    /**
     * Creates an evaluation environment wrapping an existing user provided vars.
     * <p>The supplied vars should be null only in derived classes that override the get/set/has methods.
     * For a default vars context with a code supplied vars, use the default no-parameter contructor.</p>
     * @param map the variables map
     */
    public JexlEvalContext(Map<String, Object> map) {
        this.vars = map == EMPTY_MAP ? new MapContext() : new MapContext(map);
        this.ns = null;
    }

    /**
     * Creates an evaluation environment from a context.
     * @param context the context (may be null, implies readonly)
     */
    public JexlEvalContext(JexlContext context) {
        this(context, context instanceof JexlContext.NamespaceResolver ? (JexlContext.NamespaceResolver) context : null);
    }

    /**
     * Creates an evaluation environment from a context and a namespace.
     * @param context the context (may be null, implies readonly)
     * @param namespace the namespace (may be null, implies empty namespace)
     */
    public JexlEvalContext(JexlContext context, JexlContext.NamespaceResolver namespace) {
        this.vars = context != null ? context : JexlEngine.EMPTY_CONTEXT;
        this.ns = namespace != null ? namespace : JexlEngine.EMPTY_NS;
    }

    @Override
    public boolean has(String name) {
        writeline("/home/ubuntu/results/coverage/JexlEvalContext/JexlEvalContext_1_10.coverage", "de7250f7-6770-4480-8ae7-b061c358742f");
        return vars.has(name);
    }

    @Override
    public Object get(String name) {
        writeline("/home/ubuntu/results/coverage/JexlEvalContext/JexlEvalContext_1_10.coverage", "e9a2fd8c-c619-4a99-9194-2b163625d9cf");
        return vars.get(name);
    }

    @Override
    public void set(String name, Object value) {
        writeline("/home/ubuntu/results/coverage/JexlEvalContext/JexlEvalContext_1_10.coverage", "feb95434-2d52-4d0d-be58-9d265671aaf0");
        vars.set(name, value);
    }

    @Override
    public Object resolveNamespace(String name) {
        writeline("/home/ubuntu/results/coverage/JexlEvalContext/JexlEvalContext_1_10.coverage", "514490f6-5121-4363-a138-a72f5c14604c");
        return ns != null ? ns.resolveNamespace(name) : null;
    }

    /**
     * Clear all options.
     */
    public void clearOptions() {
        writeline("/home/ubuntu/results/coverage/JexlEvalContext/JexlEvalContext_1_10.coverage", "079d2183-4d3a-47c3-ab7e-e4e8d2ece26b");
        silent = null;
        writeline("/home/ubuntu/results/coverage/JexlEvalContext/JexlEvalContext_1_10.coverage", "abf2b994-f726-4404-8a7d-21a92e3ced29");
        strict = null;
        writeline("/home/ubuntu/results/coverage/JexlEvalContext/JexlEvalContext_1_10.coverage", "043949a2-0165-4dc7-9017-b3b06bb67856");
        mathScale = -1;
        writeline("/home/ubuntu/results/coverage/JexlEvalContext/JexlEvalContext_1_10.coverage", "b70e76c7-3ad1-4e63-b971-f25158588010");
        mathContext = null;
    }

    /**
     * Sets whether the engine will throw JexlException during evaluation when an error is triggered.
     * @param s true means no JexlException will occur, false allows them
     */
    public void setSilent(boolean s) {
        writeline("/home/ubuntu/results/coverage/JexlEvalContext/JexlEvalContext_1_10.coverage", "b6306ecf-152d-4d3b-9e23-ce3bc0acf207");
        this.silent = s ? Boolean.TRUE : Boolean.FALSE;
    }

    @Override
    public Boolean isSilent() {
        writeline("/home/ubuntu/results/coverage/JexlEvalContext/JexlEvalContext_1_10.coverage", "72547a94-98f0-4ab3-952c-63031aa006c2");
        return this.silent;
    }

    /**
     * Sets the engine and arithmetic strict flags in one call.
     * @param se the engine strict flag
     * @param sa the arithmetic strict flag
     */
    public void setStrict(boolean se, boolean sa) {
        writeline("/home/ubuntu/results/coverage/JexlEvalContext/JexlEvalContext_1_10.coverage", "a8e2670d-dfcb-47c0-8a08-b5cf75b6688e");
        this.strict = se ? Boolean.TRUE : Boolean.FALSE;
        writeline("/home/ubuntu/results/coverage/JexlEvalContext/JexlEvalContext_1_10.coverage", "a34e5c76-41c7-409c-9ae7-3ff9462ff610");
        this.mathStrict = sa ? Boolean.TRUE : Boolean.FALSE;
    }

    /**
     * Sets whether the engine will consider unknown variables, methods and constructors as errors or evaluates them
     * as null.
     * @param se true means strict error reporting, false allows mentioned conditions to be evaluated as null
     */
    public void setStrict(boolean se) {
        writeline("/home/ubuntu/results/coverage/JexlEvalContext/JexlEvalContext_1_10.coverage", "e033075f-f122-4537-9dc1-0da17c833358");
        setStrict(se, se);
    }

    @Override
    public Boolean isStrict() {
        writeline("/home/ubuntu/results/coverage/JexlEvalContext/JexlEvalContext_1_10.coverage", "518771f9-1e37-4c8b-b171-324727475a08");
        if (strict == null) {
            writeline("/home/ubuntu/results/coverage/JexlEvalContext/JexlEvalContext_1_10.coverage", "5062c174-a1dd-42c5-a168-3a58844ad569");
            return null;
        } else {
            writeline("/home/ubuntu/results/coverage/JexlEvalContext/JexlEvalContext_1_10.coverage", "21821207-b5db-4aea-a0b2-ea2ff21927aa");
            return strict.booleanValue() ? Boolean.TRUE : Boolean.FALSE;
        }
    }

    /**
     * Sets whether the arithmetic will consider null arguments as errors during evaluation.
     * @param s true means strict error reporting, false allows mentioned conditions to be evaluated as 0
     */
    public void setStrictArithmetic(boolean s) {
        writeline("/home/ubuntu/results/coverage/JexlEvalContext/JexlEvalContext_1_10.coverage", "cd5832bd-d80f-421f-b334-d651ea81b8e5");
        this.mathStrict = s ? Boolean.TRUE : Boolean.FALSE;
    }

    @Override
    public Boolean isStrictArithmetic() {
        writeline("/home/ubuntu/results/coverage/JexlEvalContext/JexlEvalContext_1_10.coverage", "242dca96-8dba-4fa6-9062-179b17fc65d4");
        if (mathStrict == null) {
            writeline("/home/ubuntu/results/coverage/JexlEvalContext/JexlEvalContext_1_10.coverage", "74d56925-029d-476b-96d2-1ce25e06d035");
            return null;
        } else {
            writeline("/home/ubuntu/results/coverage/JexlEvalContext/JexlEvalContext_1_10.coverage", "2427e1c7-2731-4cf8-a080-9005cc3b0572");
            return mathStrict.booleanValue() ? Boolean.TRUE : Boolean.FALSE;
        }
    }

    @Override
    public MathContext getArithmeticMathContext() {
        writeline("/home/ubuntu/results/coverage/JexlEvalContext/JexlEvalContext_1_10.coverage", "8ff68f3d-e229-43d1-9984-9a24b48a1ee9");
        return mathContext;
    }

    /**
     * Sets the {@link MathContext} to use by the {@link JexlArithmetic} during evaluation.
     * @param mc the math context
     */
    public void setMathContext(MathContext mc) {
        writeline("/home/ubuntu/results/coverage/JexlEvalContext/JexlEvalContext_1_10.coverage", "9a2197ec-862e-41cd-8f0b-511b3b204c8f");
        mathContext = mc;
    }

    @Override
    public int getArithmeticMathScale() {
        writeline("/home/ubuntu/results/coverage/JexlEvalContext/JexlEvalContext_1_10.coverage", "4cf8c6ba-5117-42d2-b630-04d0b49b7c40");
        return mathScale;
    }

    /**
     * Sets the math scale to use to use by the {@link JexlArithmetic} during evaluation.
     * @param scale the math scale
     */
    public void setMathScale(int scale) {
        writeline("/home/ubuntu/results/coverage/JexlEvalContext/JexlEvalContext_1_10.coverage", "691653b4-7e84-4558-8665-d81fce4812b6");
        mathScale = scale;
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
