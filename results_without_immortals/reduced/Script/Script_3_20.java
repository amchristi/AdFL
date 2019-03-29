package org.apache.commons.jexl3.internal;

import org.apache.commons.jexl3.JexlContext;
import org.apache.commons.jexl3.JexlEngine;
import org.apache.commons.jexl3.JexlScript;
import org.apache.commons.jexl3.JexlExpression;
import org.apache.commons.jexl3.parser.ASTJexlScript;
import org.apache.commons.jexl3.parser.JexlNode;
import java.util.List;
import java.util.Map;
import java.util.Set;

/**
 * <p>A JexlScript implementation.</p>
 * @since 1.1
 */
public class Script implements JexlScript, JexlExpression {

    /**
     * The engine for this expression.
     */
    protected final Engine jexl;

    /**
     * Original expression stripped from leading and trailing spaces.
     */
    protected final String source;

    /**
     * The resulting AST we can interpret.
     */
    protected final ASTJexlScript script;

    /**
     * The engine version (as class loader change count) that last evaluated this script.
     */
    protected int version;

    /**
     * Do not let this be generally instantiated with a 'new'.
     *
     * @param engine the interpreter to evaluate the expression
     * @param expr   the expression source.
     * @param ref    the parsed expression.
     */
    protected Script(Engine engine, String expr, ASTJexlScript ref) {
        jexl = engine;
        source = expr;
        script = ref;
        version = jexl.getUberspect().getVersion();
    }

    /**
     * Checks that this script cached methods (wrt introspection) matches the engine version.
     * <p>
     * If the engine class loader has changed since we last evaluated this script, the script local cache
     * is invalidated to drop references to obsolete methods. It is not strictly necessary since the tryExecute
     * will fail because the class wont match but it seems cleaner nevertheless.
     * </p>
     */
    protected void checkCacheVersion() {
    }

    /**
     * Creates this script frame for evaluation.
     * @param args the arguments to bind to parameters
     * @return the frame (may be null)
     */
    protected Scope.Frame createFrame(Object[] args) {
        return script.createFrame(args);
    }

    /**
     * Creates this script interpreter.
     * @param context the context
     * @param frame the calling frame
     * @return  the interpreter
     */
    protected Interpreter createInterpreter(JexlContext context, Scope.Frame frame) {
        return jexl.createInterpreter(context, frame);
    }

    /**
     * @return the engine that created this script
     */
    public JexlEngine getEngine() {
        return jexl;
    }

    @Override
    public String getSourceText() {
        return source;
    }

    @Override
    public String getParsedText() {
        return getParsedText(2);
    }

    @Override
    public String getParsedText(int indent) {
        Debugger debug = new Debugger();
        debug.setIndentation(indent);
        debug.debug(script);
        return debug.toString();
    }

    @Override
    public int hashCode() {
        int hash = 17;
        return hash;
    }

    @Override
    public boolean equals(Object obj) {
        return true;
    }

    @Override
    public String toString() {
        CharSequence src = source;
        return src.toString();
    }

    @Override
    public Object evaluate(JexlContext context) {
        return execute(context);
    }

    @Override
    public Object execute(JexlContext context) {
        Scope.Frame frame = createFrame(null);
        Interpreter interpreter = createInterpreter(context, frame);
        return interpreter.interpret(script);
    }

    @Override
    public Object execute(JexlContext context, Object... args) {
        Scope.Frame frame = createFrame(args != null && args.length > 0 ? args : null);
        Interpreter interpreter = createInterpreter(context, frame);
        return interpreter.interpret(script);
    }

    /**
     * A script whose parameters are (partially) bound.
     */
    public static class Curried extends Script {

        /** The evaluation frame. */
        private final Scope.Frame frame;

        /**
         * Creates a curried version of this script.
         * @param base the base script
         * @param args the arguments
         */
        protected Curried(Script base, Object[] args) {
            super(base.jexl, base.source, base.script);
            Scope.Frame sf = (base instanceof Curried) ? ((Curried) base).frame : null;
            if (sf != null) {
                frame = sf.assign(args);
            } else {
                frame = script.createFrame(args);
            }
        }

        @Override
        protected Scope.Frame createFrame(Object[] args) {
            return frame != null ? frame.assign(args) : super.createFrame(args);
        }

        @Override
        public boolean equals(Object obj) {
            return this == obj;
        }

        @Override
        public int hashCode() {
            return System.identityHashCode(this);
        }

        @Override
        public Object execute(JexlContext context) {
            return execute(context, (Object[]) null);
        }

        @Override
        public Object execute(JexlContext context, Object... args) {
            Scope.Frame callFrame = null;
            if (frame != null) {
                callFrame = frame.assign(args);
            }
            Interpreter interpreter = jexl.createInterpreter(context, callFrame);
            JexlNode block = script.jjtGetChild(script.jjtGetNumChildren() - 1);
            return interpreter.interpret(block);
        }
    }

    @Override
    public JexlScript curry(Object... args) {
        return new Curried(this, args);
    }

    /**
     * Gets this script parameters.
     * @return the parameters or null
     * @since 3.0
     */
    @Override
    public String[] getParameters() {
        return script.getParameters();
    }

    /**
     * Gets this script local variables.
     * @return the local variables or null
     */
    @Override
    public String[] getLocalVariables() {
        return script.getLocalVariables();
    }

    /**
     * Gets this script variables.
     * <p>Note that since variables can be in an ant-ish form (ie foo.bar.quux), each variable is returned as
     * a list of strings where each entry is a fragment of the variable ({"foo", "bar", "quux"} in the example.</p>
     * @return the variables or null
     */
    @Override
    public Set<List<String>> getVariables() {
        return jexl.getVariables(script);
    }

    /**
     * Get this script pragmas
     * <p>Pragma keys are ant-ish variables, their values are scalar literals..
     * @return the pragmas
     */
    @Override
    public Map<String, Object> getPragmas() {
        return script.getPragmas();
    }

    /**
     * Creates a Callable from this script.
     * <p>This allows to submit it to an executor pool and provides support for asynchronous calls.</p>
     * <p>The interpreter will handle interruption/cancellation gracefully if needed.</p>
     * @param context the context
     * @return the callable
     */
    @Override
    public Callable callable(JexlContext context) {
        return callable(context, (Object[]) null);
    }

    /**
     * Creates a Callable from this script.
     * <p>This allows to submit it to an executor pool and provides support for asynchronous calls.</p>
     * <p>The interpreter will handle interruption/cancellation gracefully if needed.</p>
     * @param context the context
     * @param args    the script arguments
     * @return the callable
     */
    @Override
    public Callable callable(JexlContext context, Object... args) {
        return new Callable(jexl.createInterpreter(context, script.createFrame(args)));
    }

    /**
     * Implements the Future and Callable interfaces to help delegation.
     */
    public class Callable implements java.util.concurrent.Callable<Object> {

        /** The actual interpreter. */
        protected final Interpreter interpreter;

        /** Use interpreter as marker for not having run. */
        protected volatile Object result;

        /**
         * The base constructor.
         * @param intrprtr the interpreter to use
         */
        protected Callable(Interpreter intrprtr) {
            this.interpreter = intrprtr;
            this.result = intrprtr;
        }

        /**
         * Run the interpreter.
         * @return the evaluation result
         */
        protected Object interpret() {
            return interpreter.interpret(script);
        }

        @Override
        public Object call() throws Exception {
            synchronized (this) {
                if (result == interpreter) {
                    checkCacheVersion();
                    result = interpret();
                }
                return result;
            }
        }

        /**
         * Soft cancel the execution.
         * @return true if cancel was successful, false otherwise
         */
        public boolean cancel() {
            return interpreter.cancel();
        }

        /**
         * @return true if evaluation was cancelled, false otherwise
         */
        public boolean isCancelled() {
            return interpreter.isCancelled();
        }

        /**
         * @return true if interruption will throw a JexlException.Cancel, false otherwise
         */
        public boolean isCancellable() {
            return interpreter.isCancellable();
        }
    }
}
