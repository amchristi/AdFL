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
        writeline("/home/ubuntu/results/coverage/JexlEvalContext/JexlEvalContext_3_10.coverage", "9accbc1d-30d8-4985-8946-ac4bfafbb520");
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
        writeline("/home/ubuntu/results/coverage/JexlEvalContext/JexlEvalContext_3_10.coverage", "ac9d9085-8dd5-4d6f-962f-fb37a2f212e2");
        return vars.has(name);
    }

    @Override
    public Object get(String name) {
        writeline("/home/ubuntu/results/coverage/JexlEvalContext/JexlEvalContext_3_10.coverage", "8e15463a-626a-4635-84fd-6ea77436bc52");
        return vars.get(name);
    }

    @Override
    public void set(String name, Object value) {
        writeline("/home/ubuntu/results/coverage/JexlEvalContext/JexlEvalContext_3_10.coverage", "6f28628c-fa50-4b60-99c4-3cdfa13341a3");
        vars.set(name, value);
    }

    @Override
    public Object resolveNamespace(String name) {
        writeline("/home/ubuntu/results/coverage/JexlEvalContext/JexlEvalContext_3_10.coverage", "e9026cc8-a5e3-4071-b920-9906761339c1");
        return ns != null ? ns.resolveNamespace(name) : null;
    }

    /**
     * Clear all options.
     */
    public void clearOptions() {
        writeline("/home/ubuntu/results/coverage/JexlEvalContext/JexlEvalContext_3_10.coverage", "bce7143a-c88d-4dfd-95a9-065535d8eae9");
        silent = null;
        writeline("/home/ubuntu/results/coverage/JexlEvalContext/JexlEvalContext_3_10.coverage", "19808a6e-5de1-432e-8c9e-c6e1e39567ff");
        strict = null;
        writeline("/home/ubuntu/results/coverage/JexlEvalContext/JexlEvalContext_3_10.coverage", "46bb21f8-f9e5-4b93-b2a7-152849621e3f");
        mathScale = -1;
        writeline("/home/ubuntu/results/coverage/JexlEvalContext/JexlEvalContext_3_10.coverage", "33db56bf-64a6-4d19-8e9c-29c5272a6677");
        mathContext = null;
    }

    /**
     * Sets whether the engine will throw JexlException during evaluation when an error is triggered.
     * @param s true means no JexlException will occur, false allows them
     */
    public void setSilent(boolean s) {
        writeline("/home/ubuntu/results/coverage/JexlEvalContext/JexlEvalContext_3_10.coverage", "88785b06-b3f5-4c72-9ef6-8ee73d3c2752");
        this.silent = s ? Boolean.TRUE : Boolean.FALSE;
    }

    @Override
    public Boolean isSilent() {
        writeline("/home/ubuntu/results/coverage/JexlEvalContext/JexlEvalContext_3_10.coverage", "8dd72b07-c924-4c6f-971b-3f18995b7d47");
        return this.silent;
    }

    /**
     * Sets the engine and arithmetic strict flags in one call.
     * @param se the engine strict flag
     * @param sa the arithmetic strict flag
     */
    public void setStrict(boolean se, boolean sa) {
        writeline("/home/ubuntu/results/coverage/JexlEvalContext/JexlEvalContext_3_10.coverage", "ff8bc037-7aeb-4e91-b6d7-4d237740e2bb");
        this.strict = se ? Boolean.TRUE : Boolean.FALSE;
        writeline("/home/ubuntu/results/coverage/JexlEvalContext/JexlEvalContext_3_10.coverage", "500acdf9-193d-4775-b190-7b1b28f59c96");
        this.mathStrict = sa ? Boolean.TRUE : Boolean.FALSE;
    }

    /**
     * Sets whether the engine will consider unknown variables, methods and constructors as errors or evaluates them
     * as null.
     * @param se true means strict error reporting, false allows mentioned conditions to be evaluated as null
     */
    public void setStrict(boolean se) {
        writeline("/home/ubuntu/results/coverage/JexlEvalContext/JexlEvalContext_3_10.coverage", "05eacc74-bdba-419a-9206-e83f31322077");
        setStrict(se, se);
    }

    @Override
    public Boolean isStrict() {
        writeline("/home/ubuntu/results/coverage/JexlEvalContext/JexlEvalContext_3_10.coverage", "73d36528-51ab-4fa2-a2cb-2d324b4c5e21");
        if (strict == null) {
            writeline("/home/ubuntu/results/coverage/JexlEvalContext/JexlEvalContext_3_10.coverage", "7e8af2d1-2cda-43e4-9b09-2ece97abd123");
            return null;
        } else {
            writeline("/home/ubuntu/results/coverage/JexlEvalContext/JexlEvalContext_3_10.coverage", "f9284a26-c1b3-4080-93ad-188e25e50775");
            return strict.booleanValue() ? Boolean.TRUE : Boolean.FALSE;
        }
    }

    /**
     * Sets whether the arithmetic will consider null arguments as errors during evaluation.
     * @param s true means strict error reporting, false allows mentioned conditions to be evaluated as 0
     */
    public void setStrictArithmetic(boolean s) {
        writeline("/home/ubuntu/results/coverage/JexlEvalContext/JexlEvalContext_3_10.coverage", "82195e0b-d424-4b48-9c54-6040b54311d9");
        this.mathStrict = s ? Boolean.TRUE : Boolean.FALSE;
    }

    @Override
    public Boolean isStrictArithmetic() {
        writeline("/home/ubuntu/results/coverage/JexlEvalContext/JexlEvalContext_3_10.coverage", "9d4e8abf-c36a-4b1d-8ad2-190d0eaa8c37");
        if (mathStrict == null) {
            writeline("/home/ubuntu/results/coverage/JexlEvalContext/JexlEvalContext_3_10.coverage", "1092336f-feda-434f-a197-058cdaedcba2");
            return null;
        } else {
            writeline("/home/ubuntu/results/coverage/JexlEvalContext/JexlEvalContext_3_10.coverage", "d01826ba-aef8-44fe-8346-095122cc01d6");
            return mathStrict.booleanValue() ? Boolean.TRUE : Boolean.FALSE;
        }
    }

    @Override
    public MathContext getArithmeticMathContext() {
        writeline("/home/ubuntu/results/coverage/JexlEvalContext/JexlEvalContext_3_10.coverage", "92a27d3a-5db8-4345-90a2-0316dd85a4ef");
        return mathContext;
    }

    /**
     * Sets the {@link MathContext} to use by the {@link JexlArithmetic} during evaluation.
     * @param mc the math context
     */
    public void setMathContext(MathContext mc) {
        writeline("/home/ubuntu/results/coverage/JexlEvalContext/JexlEvalContext_3_10.coverage", "1bc91abb-62e7-41d0-a866-d1973b09f67a");
        mathContext = mc;
    }

    @Override
    public int getArithmeticMathScale() {
        writeline("/home/ubuntu/results/coverage/JexlEvalContext/JexlEvalContext_3_10.coverage", "41e1d349-3a1a-47c1-8b66-70b08f0f3e07");
        return mathScale;
    }

    /**
     * Sets the math scale to use to use by the {@link JexlArithmetic} during evaluation.
     * @param scale the math scale
     */
    public void setMathScale(int scale) {
        writeline("/home/ubuntu/results/coverage/JexlEvalContext/JexlEvalContext_3_10.coverage", "6e60cb2c-5513-44e8-955c-03bb6ad7e0b4");
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
