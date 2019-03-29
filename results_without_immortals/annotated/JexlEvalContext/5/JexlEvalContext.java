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
        writeline("/home/ubuntu/results/coverage/JexlEvalContext/JexlEvalContext_5_10.coverage", "c43374c8-0be2-4ff1-9cf4-58c2d3fd88f4");
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
        writeline("/home/ubuntu/results/coverage/JexlEvalContext/JexlEvalContext_5_10.coverage", "93430810-2759-4fb2-9f04-5ba96d4ac369");
        return vars.has(name);
    }

    @Override
    public Object get(String name) {
        writeline("/home/ubuntu/results/coverage/JexlEvalContext/JexlEvalContext_5_10.coverage", "0cdf47b7-8ab9-433d-8921-38d9adecd15a");
        return vars.get(name);
    }

    @Override
    public void set(String name, Object value) {
        writeline("/home/ubuntu/results/coverage/JexlEvalContext/JexlEvalContext_5_10.coverage", "ecd99a49-fca4-43de-8ec1-ba70d0774ea5");
        vars.set(name, value);
    }

    @Override
    public Object resolveNamespace(String name) {
        writeline("/home/ubuntu/results/coverage/JexlEvalContext/JexlEvalContext_5_10.coverage", "51963199-d6cf-4538-a2c9-b26361c2f203");
        return ns != null ? ns.resolveNamespace(name) : null;
    }

    /**
     * Clear all options.
     */
    public void clearOptions() {
        writeline("/home/ubuntu/results/coverage/JexlEvalContext/JexlEvalContext_5_10.coverage", "e1ed15a7-9bf9-4599-aa0b-783a17df0d21");
        silent = null;
        writeline("/home/ubuntu/results/coverage/JexlEvalContext/JexlEvalContext_5_10.coverage", "121fd683-b517-481f-b743-8e38d4033ae8");
        strict = null;
        writeline("/home/ubuntu/results/coverage/JexlEvalContext/JexlEvalContext_5_10.coverage", "ee7337d0-a336-4f6b-87a5-93b52c49b771");
        mathScale = -1;
        writeline("/home/ubuntu/results/coverage/JexlEvalContext/JexlEvalContext_5_10.coverage", "e9b6a922-13f7-4431-b18f-3c1323c19ddb");
        mathContext = null;
    }

    /**
     * Sets whether the engine will throw JexlException during evaluation when an error is triggered.
     * @param s true means no JexlException will occur, false allows them
     */
    public void setSilent(boolean s) {
        writeline("/home/ubuntu/results/coverage/JexlEvalContext/JexlEvalContext_5_10.coverage", "3a352384-fdf0-41af-aa7c-08550422a0ab");
        this.silent = s ? Boolean.TRUE : Boolean.FALSE;
    }

    @Override
    public Boolean isSilent() {
        writeline("/home/ubuntu/results/coverage/JexlEvalContext/JexlEvalContext_5_10.coverage", "249923f3-55cb-4c38-85e1-9b7334694e7c");
        return this.silent;
    }

    /**
     * Sets the engine and arithmetic strict flags in one call.
     * @param se the engine strict flag
     * @param sa the arithmetic strict flag
     */
    public void setStrict(boolean se, boolean sa) {
        writeline("/home/ubuntu/results/coverage/JexlEvalContext/JexlEvalContext_5_10.coverage", "fb0226f8-c45b-44e7-9c6d-bd5d4551372c");
        this.strict = se ? Boolean.TRUE : Boolean.FALSE;
        writeline("/home/ubuntu/results/coverage/JexlEvalContext/JexlEvalContext_5_10.coverage", "45cdfa94-e564-4313-b21d-c3de64c2f5f3");
        this.mathStrict = sa ? Boolean.TRUE : Boolean.FALSE;
    }

    /**
     * Sets whether the engine will consider unknown variables, methods and constructors as errors or evaluates them
     * as null.
     * @param se true means strict error reporting, false allows mentioned conditions to be evaluated as null
     */
    public void setStrict(boolean se) {
        writeline("/home/ubuntu/results/coverage/JexlEvalContext/JexlEvalContext_5_10.coverage", "73f9a273-bcc3-486d-bf99-df7e4743d3e7");
        setStrict(se, se);
    }

    @Override
    public Boolean isStrict() {
        writeline("/home/ubuntu/results/coverage/JexlEvalContext/JexlEvalContext_5_10.coverage", "22ad2eae-9d8e-451d-b30d-756194d4427e");
        if (strict == null) {
            writeline("/home/ubuntu/results/coverage/JexlEvalContext/JexlEvalContext_5_10.coverage", "a3f1dd4b-73fe-4f8e-a3ef-d7d7b59d64a9");
            return null;
        } else {
            writeline("/home/ubuntu/results/coverage/JexlEvalContext/JexlEvalContext_5_10.coverage", "8c681bc5-55a4-43d9-a9c7-f882bbbe3e7f");
            return strict.booleanValue() ? Boolean.TRUE : Boolean.FALSE;
        }
    }

    /**
     * Sets whether the arithmetic will consider null arguments as errors during evaluation.
     * @param s true means strict error reporting, false allows mentioned conditions to be evaluated as 0
     */
    public void setStrictArithmetic(boolean s) {
        writeline("/home/ubuntu/results/coverage/JexlEvalContext/JexlEvalContext_5_10.coverage", "b129e550-40d7-4a21-90a3-032e5ba108ee");
        this.mathStrict = s ? Boolean.TRUE : Boolean.FALSE;
    }

    @Override
    public Boolean isStrictArithmetic() {
        writeline("/home/ubuntu/results/coverage/JexlEvalContext/JexlEvalContext_5_10.coverage", "91f54bca-86bf-4fd4-9eab-5c75b9d500c2");
        if (mathStrict == null) {
            writeline("/home/ubuntu/results/coverage/JexlEvalContext/JexlEvalContext_5_10.coverage", "a8c95734-b54a-4e59-95af-3b21f0523be2");
            return null;
        } else {
            writeline("/home/ubuntu/results/coverage/JexlEvalContext/JexlEvalContext_5_10.coverage", "ceb5dd4c-a68c-457f-aeed-61c8b889acf2");
            return mathStrict.booleanValue() ? Boolean.TRUE : Boolean.FALSE;
        }
    }

    @Override
    public MathContext getArithmeticMathContext() {
        writeline("/home/ubuntu/results/coverage/JexlEvalContext/JexlEvalContext_5_10.coverage", "b793287a-f062-4019-b452-51c1da5801b0");
        return mathContext;
    }

    /**
     * Sets the {@link MathContext} to use by the {@link JexlArithmetic} during evaluation.
     * @param mc the math context
     */
    public void setMathContext(MathContext mc) {
        writeline("/home/ubuntu/results/coverage/JexlEvalContext/JexlEvalContext_5_10.coverage", "414929bc-3499-45a3-ade9-e0cbf1bb688c");
        mathContext = mc;
    }

    @Override
    public int getArithmeticMathScale() {
        writeline("/home/ubuntu/results/coverage/JexlEvalContext/JexlEvalContext_5_10.coverage", "0425ad80-ee3f-4265-8fa5-e5ac3f9e3b30");
        return mathScale;
    }

    /**
     * Sets the math scale to use to use by the {@link JexlArithmetic} during evaluation.
     * @param scale the math scale
     */
    public void setMathScale(int scale) {
        writeline("/home/ubuntu/results/coverage/JexlEvalContext/JexlEvalContext_5_10.coverage", "388bf526-8406-49d3-9669-5ba1b1c82a1f");
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
