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
        writeline("/home/ubuntu/results/coverage/JexlEvalContext/JexlEvalContext_4_10.coverage", "ef06868d-22c8-4332-b48d-737bc59fa2d8");
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
        writeline("/home/ubuntu/results/coverage/JexlEvalContext/JexlEvalContext_4_10.coverage", "e5207b56-7915-45d0-9839-a94c386c7451");
        return vars.has(name);
    }

    @Override
    public Object get(String name) {
        writeline("/home/ubuntu/results/coverage/JexlEvalContext/JexlEvalContext_4_10.coverage", "979da52a-c2df-4b2f-8c14-8c3ac49b47e6");
        return vars.get(name);
    }

    @Override
    public void set(String name, Object value) {
        writeline("/home/ubuntu/results/coverage/JexlEvalContext/JexlEvalContext_4_10.coverage", "6f1a398b-6a07-4321-92a4-104b9d10aaa1");
        vars.set(name, value);
    }

    @Override
    public Object resolveNamespace(String name) {
        writeline("/home/ubuntu/results/coverage/JexlEvalContext/JexlEvalContext_4_10.coverage", "629ebc87-127a-48b0-866c-4070f50aa586");
        return ns != null ? ns.resolveNamespace(name) : null;
    }

    /**
     * Clear all options.
     */
    public void clearOptions() {
        writeline("/home/ubuntu/results/coverage/JexlEvalContext/JexlEvalContext_4_10.coverage", "03dc9c9a-02fe-4d96-bee8-bca75fe63089");
        silent = null;
        writeline("/home/ubuntu/results/coverage/JexlEvalContext/JexlEvalContext_4_10.coverage", "9de2ebd0-92b0-4be4-bc88-6b7f90218d78");
        strict = null;
        writeline("/home/ubuntu/results/coverage/JexlEvalContext/JexlEvalContext_4_10.coverage", "2bd1ff5a-9f51-4fc2-8154-1c474c20a815");
        mathScale = -1;
        writeline("/home/ubuntu/results/coverage/JexlEvalContext/JexlEvalContext_4_10.coverage", "3589361b-cdc5-45bb-b933-ffa0d9daa5ae");
        mathContext = null;
    }

    /**
     * Sets whether the engine will throw JexlException during evaluation when an error is triggered.
     * @param s true means no JexlException will occur, false allows them
     */
    public void setSilent(boolean s) {
        writeline("/home/ubuntu/results/coverage/JexlEvalContext/JexlEvalContext_4_10.coverage", "6fa5b40e-9471-4964-869d-c8374eb7a30e");
        this.silent = s ? Boolean.TRUE : Boolean.FALSE;
    }

    @Override
    public Boolean isSilent() {
        writeline("/home/ubuntu/results/coverage/JexlEvalContext/JexlEvalContext_4_10.coverage", "b45a2126-e894-4787-a4a2-53a25733b021");
        return this.silent;
    }

    /**
     * Sets the engine and arithmetic strict flags in one call.
     * @param se the engine strict flag
     * @param sa the arithmetic strict flag
     */
    public void setStrict(boolean se, boolean sa) {
        writeline("/home/ubuntu/results/coverage/JexlEvalContext/JexlEvalContext_4_10.coverage", "b8a288ed-e7f1-4c85-a218-98fbf42e3490");
        this.strict = se ? Boolean.TRUE : Boolean.FALSE;
        writeline("/home/ubuntu/results/coverage/JexlEvalContext/JexlEvalContext_4_10.coverage", "8290968b-87e9-424e-b4ab-af2bc44756be");
        this.mathStrict = sa ? Boolean.TRUE : Boolean.FALSE;
    }

    /**
     * Sets whether the engine will consider unknown variables, methods and constructors as errors or evaluates them
     * as null.
     * @param se true means strict error reporting, false allows mentioned conditions to be evaluated as null
     */
    public void setStrict(boolean se) {
        writeline("/home/ubuntu/results/coverage/JexlEvalContext/JexlEvalContext_4_10.coverage", "037cff77-cf02-4b66-af60-56eea3d56b7c");
        setStrict(se, se);
    }

    @Override
    public Boolean isStrict() {
        writeline("/home/ubuntu/results/coverage/JexlEvalContext/JexlEvalContext_4_10.coverage", "77741819-90b8-472a-8cc5-690ae853d11d");
        if (strict == null) {
            writeline("/home/ubuntu/results/coverage/JexlEvalContext/JexlEvalContext_4_10.coverage", "7e16f551-cebd-4e82-b18a-f69d4e7368bd");
            return null;
        } else {
            writeline("/home/ubuntu/results/coverage/JexlEvalContext/JexlEvalContext_4_10.coverage", "1fbb50c6-a2d5-4341-9ba0-df0cb07834c1");
            return strict.booleanValue() ? Boolean.TRUE : Boolean.FALSE;
        }
    }

    /**
     * Sets whether the arithmetic will consider null arguments as errors during evaluation.
     * @param s true means strict error reporting, false allows mentioned conditions to be evaluated as 0
     */
    public void setStrictArithmetic(boolean s) {
        writeline("/home/ubuntu/results/coverage/JexlEvalContext/JexlEvalContext_4_10.coverage", "8bca1075-08e1-4687-a52a-b22ec557ecbb");
        this.mathStrict = s ? Boolean.TRUE : Boolean.FALSE;
    }

    @Override
    public Boolean isStrictArithmetic() {
        writeline("/home/ubuntu/results/coverage/JexlEvalContext/JexlEvalContext_4_10.coverage", "759d5070-6b12-4016-8a12-c1a403193f2c");
        if (mathStrict == null) {
            writeline("/home/ubuntu/results/coverage/JexlEvalContext/JexlEvalContext_4_10.coverage", "8af64284-ec80-4f57-b990-1547659e51d9");
            return null;
        } else {
            writeline("/home/ubuntu/results/coverage/JexlEvalContext/JexlEvalContext_4_10.coverage", "20340aac-0b60-4907-9fa9-f488f6d3bfb1");
            return mathStrict.booleanValue() ? Boolean.TRUE : Boolean.FALSE;
        }
    }

    @Override
    public MathContext getArithmeticMathContext() {
        writeline("/home/ubuntu/results/coverage/JexlEvalContext/JexlEvalContext_4_10.coverage", "85802fbf-b659-4822-a087-e01742d09c45");
        return mathContext;
    }

    /**
     * Sets the {@link MathContext} to use by the {@link JexlArithmetic} during evaluation.
     * @param mc the math context
     */
    public void setMathContext(MathContext mc) {
        writeline("/home/ubuntu/results/coverage/JexlEvalContext/JexlEvalContext_4_10.coverage", "1b3de307-ba7d-4700-90bd-fccc48b00e04");
        mathContext = mc;
    }

    @Override
    public int getArithmeticMathScale() {
        writeline("/home/ubuntu/results/coverage/JexlEvalContext/JexlEvalContext_4_10.coverage", "e67dcdbb-9816-4a8c-8d89-11ed0ca4b15e");
        return mathScale;
    }

    /**
     * Sets the math scale to use to use by the {@link JexlArithmetic} during evaluation.
     * @param scale the math scale
     */
    public void setMathScale(int scale) {
        writeline("/home/ubuntu/results/coverage/JexlEvalContext/JexlEvalContext_4_10.coverage", "7b64c8c6-c1b8-4ae3-a8ec-b26b81b12b05");
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
