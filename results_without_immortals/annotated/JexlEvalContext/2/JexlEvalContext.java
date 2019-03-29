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
        writeline("/home/ubuntu/results/coverage/JexlEvalContext/JexlEvalContext_2_10.coverage", "46dc8462-84ef-4d20-a807-ff6c0cac713e");
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
        writeline("/home/ubuntu/results/coverage/JexlEvalContext/JexlEvalContext_2_10.coverage", "eb1188bd-66cf-4248-b699-f6c900fd3148");
        return vars.has(name);
    }

    @Override
    public Object get(String name) {
        writeline("/home/ubuntu/results/coverage/JexlEvalContext/JexlEvalContext_2_10.coverage", "803033e6-1b71-49e4-b8b5-d2cb33d6ad0a");
        return vars.get(name);
    }

    @Override
    public void set(String name, Object value) {
        writeline("/home/ubuntu/results/coverage/JexlEvalContext/JexlEvalContext_2_10.coverage", "636839eb-bb6f-412a-ac14-a709ec91d6d2");
        vars.set(name, value);
    }

    @Override
    public Object resolveNamespace(String name) {
        writeline("/home/ubuntu/results/coverage/JexlEvalContext/JexlEvalContext_2_10.coverage", "f4276b5d-94e8-4c82-bd20-f4369d273dc6");
        return ns != null ? ns.resolveNamespace(name) : null;
    }

    /**
     * Clear all options.
     */
    public void clearOptions() {
        writeline("/home/ubuntu/results/coverage/JexlEvalContext/JexlEvalContext_2_10.coverage", "4ed27ab4-0474-4b51-8070-85ee60f6e018");
        silent = null;
        writeline("/home/ubuntu/results/coverage/JexlEvalContext/JexlEvalContext_2_10.coverage", "9f24273e-a189-46ce-8485-6db9a95fe63b");
        strict = null;
        writeline("/home/ubuntu/results/coverage/JexlEvalContext/JexlEvalContext_2_10.coverage", "a046103b-9703-431a-908a-da3a6295fd32");
        mathScale = -1;
        writeline("/home/ubuntu/results/coverage/JexlEvalContext/JexlEvalContext_2_10.coverage", "96dee46a-6b5c-4773-a8fc-6d9aef615862");
        mathContext = null;
    }

    /**
     * Sets whether the engine will throw JexlException during evaluation when an error is triggered.
     * @param s true means no JexlException will occur, false allows them
     */
    public void setSilent(boolean s) {
        writeline("/home/ubuntu/results/coverage/JexlEvalContext/JexlEvalContext_2_10.coverage", "9073d17f-b43b-4424-97f9-99b034da522b");
        this.silent = s ? Boolean.TRUE : Boolean.FALSE;
    }

    @Override
    public Boolean isSilent() {
        writeline("/home/ubuntu/results/coverage/JexlEvalContext/JexlEvalContext_2_10.coverage", "6bdb9d76-94e5-4df1-9ef6-5cccb86c3400");
        return this.silent;
    }

    /**
     * Sets the engine and arithmetic strict flags in one call.
     * @param se the engine strict flag
     * @param sa the arithmetic strict flag
     */
    public void setStrict(boolean se, boolean sa) {
        writeline("/home/ubuntu/results/coverage/JexlEvalContext/JexlEvalContext_2_10.coverage", "b47c0e96-d374-4975-b4f8-2b5984f760d8");
        this.strict = se ? Boolean.TRUE : Boolean.FALSE;
        writeline("/home/ubuntu/results/coverage/JexlEvalContext/JexlEvalContext_2_10.coverage", "615099f3-eb21-42ad-aa01-4d5a50915070");
        this.mathStrict = sa ? Boolean.TRUE : Boolean.FALSE;
    }

    /**
     * Sets whether the engine will consider unknown variables, methods and constructors as errors or evaluates them
     * as null.
     * @param se true means strict error reporting, false allows mentioned conditions to be evaluated as null
     */
    public void setStrict(boolean se) {
        writeline("/home/ubuntu/results/coverage/JexlEvalContext/JexlEvalContext_2_10.coverage", "10e75fab-ed70-4626-ae8e-dec65ede562b");
        setStrict(se, se);
    }

    @Override
    public Boolean isStrict() {
        writeline("/home/ubuntu/results/coverage/JexlEvalContext/JexlEvalContext_2_10.coverage", "4f9648f3-a68b-48e5-ac14-f8f7c9d9a2bf");
        if (strict == null) {
            writeline("/home/ubuntu/results/coverage/JexlEvalContext/JexlEvalContext_2_10.coverage", "23636e0a-2d36-48fd-a036-56d00109bd38");
            return null;
        } else {
            writeline("/home/ubuntu/results/coverage/JexlEvalContext/JexlEvalContext_2_10.coverage", "8669f8f1-dbfd-4831-9f96-18eca45c4c42");
            return strict.booleanValue() ? Boolean.TRUE : Boolean.FALSE;
        }
    }

    /**
     * Sets whether the arithmetic will consider null arguments as errors during evaluation.
     * @param s true means strict error reporting, false allows mentioned conditions to be evaluated as 0
     */
    public void setStrictArithmetic(boolean s) {
        writeline("/home/ubuntu/results/coverage/JexlEvalContext/JexlEvalContext_2_10.coverage", "dc81ea19-8a73-407b-8489-c76cbb7bca87");
        this.mathStrict = s ? Boolean.TRUE : Boolean.FALSE;
    }

    @Override
    public Boolean isStrictArithmetic() {
        writeline("/home/ubuntu/results/coverage/JexlEvalContext/JexlEvalContext_2_10.coverage", "0c5e6013-023d-470f-8a63-db36aeadb07c");
        if (mathStrict == null) {
            writeline("/home/ubuntu/results/coverage/JexlEvalContext/JexlEvalContext_2_10.coverage", "c6acca3a-e173-443e-bb50-e81cbaf2501d");
            return null;
        } else {
            writeline("/home/ubuntu/results/coverage/JexlEvalContext/JexlEvalContext_2_10.coverage", "0532d6ef-80eb-4b0c-8db4-ec6c382a02e3");
            return mathStrict.booleanValue() ? Boolean.TRUE : Boolean.FALSE;
        }
    }

    @Override
    public MathContext getArithmeticMathContext() {
        writeline("/home/ubuntu/results/coverage/JexlEvalContext/JexlEvalContext_2_10.coverage", "9a1ca0d2-55be-47fa-8ae6-1338e22908b2");
        return mathContext;
    }

    /**
     * Sets the {@link MathContext} to use by the {@link JexlArithmetic} during evaluation.
     * @param mc the math context
     */
    public void setMathContext(MathContext mc) {
        writeline("/home/ubuntu/results/coverage/JexlEvalContext/JexlEvalContext_2_10.coverage", "cc39b544-4e1c-4bdb-b413-7fba762c2e5c");
        mathContext = mc;
    }

    @Override
    public int getArithmeticMathScale() {
        writeline("/home/ubuntu/results/coverage/JexlEvalContext/JexlEvalContext_2_10.coverage", "bee3b610-1029-4423-bd93-5eec2439a56d");
        return mathScale;
    }

    /**
     * Sets the math scale to use to use by the {@link JexlArithmetic} during evaluation.
     * @param scale the math scale
     */
    public void setMathScale(int scale) {
        writeline("/home/ubuntu/results/coverage/JexlEvalContext/JexlEvalContext_2_10.coverage", "b9384594-6693-47b7-ba7e-7da85b925736");
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
