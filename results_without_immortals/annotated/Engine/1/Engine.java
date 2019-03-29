/*
 * Licensed to the Apache Software Foundation (ASF) under one or more
 * contributor license agreements.  See the NOTICE file distributed with
 * this work for additional information regarding copyright ownership.
 * The ASF licenses this file to You under the Apache License, Version 2.0
 * (the "License"); you may not use this file except in compliance with
 * the License.  You may obtain a copy of the License at
 *
 *      http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */
package org.apache.commons.jexl3.internal;

import org.apache.commons.jexl3.JexlArithmetic;
import org.apache.commons.jexl3.JexlBuilder;
import org.apache.commons.jexl3.JexlContext;
import org.apache.commons.jexl3.JexlEngine;
import org.apache.commons.jexl3.JexlException;
import org.apache.commons.jexl3.JexlInfo;
import org.apache.commons.jexl3.JexlScript;
import org.apache.commons.jexl3.internal.introspection.SandboxUberspect;
import org.apache.commons.jexl3.internal.introspection.Uberspect;
import org.apache.commons.jexl3.introspection.JexlMethod;
import org.apache.commons.jexl3.introspection.JexlSandbox;
import org.apache.commons.jexl3.introspection.JexlUberspect;
import org.apache.commons.jexl3.parser.ASTArrayAccess;
import org.apache.commons.jexl3.parser.ASTFunctionNode;
import org.apache.commons.jexl3.parser.ASTIdentifier;
import org.apache.commons.jexl3.parser.ASTIdentifierAccess;
import org.apache.commons.jexl3.parser.ASTJexlScript;
import org.apache.commons.jexl3.parser.ASTMethodNode;
import org.apache.commons.jexl3.parser.JexlNode;
import org.apache.commons.jexl3.parser.Parser;
import java.io.StringReader;
import java.util.ArrayList;
import java.util.Collections;
import java.util.LinkedHashSet;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import java.util.Set;
import java.lang.ref.SoftReference;
import java.nio.charset.Charset;
import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;
import org.apache.commons.jexl3.internal.*;
import org.apache.commons.jexl3.internal.UberspectHolder;
import java.io.*;

/**
 * A JexlEngine implementation.
 * @since 2.0
 */
public class Engine extends JexlEngine {

    /**
     * Gets the default instance of Uberspect.
     * <p>This is lazily initialized to avoid building a default instance if there
     * is no use for it. The main reason for not using the default Uberspect instance is to
     * be able to use a (low level) introspector created with a given logger
     * instead of the default one.</p>
     * <p>Implemented as on demand holder idiom.</p>
     */
    /**
     * The JexlUberspect instance.
     */
    protected final JexlUberspect uberspect;

    /**
     * The {@link JexlArithmetic} instance.
     */
    protected final JexlArithmetic arithmetic;

    /**
     * The Log to which all JexlEngine messages will be logged.
     */
    protected final Log logger;

    /**
     * The {@link Parser}; when parsing expressions, this engine synchronizes on the parser.
     */
    // $NON-NLS-1$
    protected final Parser parser = new Parser(new StringReader(";"));

    /**
     * Whether this engine considers unknown variables, methods and constructors as errors.
     */
    protected final boolean strict;

    /**
     * Whether expressions evaluated by this engine will throw exceptions (false) or return null (true) on errors.
     * Default is false.
     */
    protected final boolean silent;

    /**
     * Whether error messages will carry debugging information.
     */
    protected final boolean debug;

    /**
     * The map of 'prefix:function' to object implementing the namespaces.
     */
    protected final Map<String, Object> functions;

    /**
     * The expression cache.
     */
    protected final SoftCache<String, ASTJexlScript> cache;

    /**
     * The expression max length to hit the cache.
     */
    protected final int cacheThreshold;

    /**
     * The default charset.
     */
    protected final Charset charset;

    /**
     * The default jxlt engine.
     */
    protected volatile TemplateEngine jxlt = null;

    /**
     * The default cache load factor.
     */
    private static final float LOAD_FACTOR = 0.75f;

    /**
     * Creates an engine with default arguments.
     */
    public Engine() {
        this(new JexlBuilder());
    }

    /**
     * Creates a JEXL engine using the provided {@link JexlBuilder}.
     * @param conf the builder
     */
    public Engine(JexlBuilder conf) {
        JexlSandbox sandbox = conf.sandbox();
        JexlUberspect uber = conf.uberspect() == null ? getUberspect(conf.logger(), conf.strategy()) : conf.uberspect();
        ClassLoader loader = conf.loader();
        if (loader != null) {
            uber.setClassLoader(loader);
        }
        if (sandbox == null) {
            this.uberspect = uber;
        } else {
            this.uberspect = new SandboxUberspect(uber, sandbox);
        }
        this.logger = conf.logger() == null ? LogFactory.getLog(JexlEngine.class) : conf.logger();
        this.functions = conf.namespaces() == null ? Collections.<String, Object>emptyMap() : conf.namespaces();
        this.silent = conf.silent() == null ? false : conf.silent();
        this.debug = conf.debug() == null ? true : conf.debug();
        this.strict = conf.strict() == null ? true : conf.strict();
        this.arithmetic = conf.arithmetic() == null ? new JexlArithmetic(this.strict) : conf.arithmetic();
        this.cache = conf.cache() <= 0 ? null : new SoftCache<String, ASTJexlScript>(conf.cache());
        this.cacheThreshold = conf.cacheThreshold();
        this.charset = conf.charset();
        if (uberspect == null) {
            throw new IllegalArgumentException("uberspect can not be null");
        }
    }

    /**
     * Gets the default instance of Uberspect.
     * <p>This is lazily initialized to avoid building a default instance if there
     * is no use for it. The main reason for not using the default Uberspect instance is to
     * be able to use a (low level) introspector created with a given logger
     * instead of the default one.</p>
     * @param logger the logger to use for the underlying Uberspect
     * @param strategy the property resolver strategy
     * @return Uberspect the default uberspector instance.
     */
    public static Uberspect getUberspect(Log logger, JexlUberspect.ResolverStrategy strategy) {
        writelineStatic("/home/ubuntu/results/coverage/Engine/Engine_1_10.coverage", "81a7f94d-75dc-423a-b70a-93ac999a63fb");
        if ((logger == null || logger.equals(LogFactory.getLog(JexlEngine.class))) && (strategy == null || strategy == JexlUberspect.JEXL_STRATEGY)) {
            writelineStatic("/home/ubuntu/results/coverage/Engine/Engine_1_10.coverage", "1d599457-7dbd-491b-9d32-c9734d65249f");
            return UberspectHolder.UBERSPECT;
        }
        writelineStatic("/home/ubuntu/results/coverage/Engine/Engine_1_10.coverage", "5fd4bf6b-d786-49ad-97ee-85c67a0feba5");
        return new Uberspect(logger, strategy);
    }

    @Override
    public JexlUberspect getUberspect() {
        writeline("/home/ubuntu/results/coverage/Engine/Engine_1_10.coverage", "b0325f3a-e16c-4046-9fda-d02988dff978");
        return uberspect;
    }

    @Override
    public JexlArithmetic getArithmetic() {
        writeline("/home/ubuntu/results/coverage/Engine/Engine_1_10.coverage", "307a3daf-27c1-4cf4-95f9-0ac2dcc3f306");
        return arithmetic;
    }

    @Override
    public boolean isDebug() {
        writeline("/home/ubuntu/results/coverage/Engine/Engine_1_10.coverage", "c06b4368-0b04-479e-a990-c0a242a057bc");
        return this.debug;
    }

    @Override
    public boolean isSilent() {
        writeline("/home/ubuntu/results/coverage/Engine/Engine_1_10.coverage", "ebf82699-33dd-4898-86d3-7eb148652c2a");
        return this.silent;
    }

    @Override
    public boolean isStrict() {
        writeline("/home/ubuntu/results/coverage/Engine/Engine_1_10.coverage", "62e6c9a5-0ec3-4468-9457-07a00c90c272");
        return strict;
    }

    @Override
    public void setClassLoader(ClassLoader loader) {
        writeline("/home/ubuntu/results/coverage/Engine/Engine_1_10.coverage", "36d98206-57c0-4335-a2b4-53d52719d3b6");
        uberspect.setClassLoader(loader);
    }

    @Override
    public Charset getCharset() {
        writeline("/home/ubuntu/results/coverage/Engine/Engine_1_10.coverage", "6c7c515e-7b1d-49c4-8f7c-d0d074955639");
        return charset;
    }

    @Override
    public TemplateEngine createJxltEngine(boolean noScript, int cacheSize, char immediate, char deferred) {
        writeline("/home/ubuntu/results/coverage/Engine/Engine_1_10.coverage", "9ad6de08-9d9b-4c16-ab30-cf5f3e02d606");
        return new TemplateEngine(this, noScript, cacheSize, immediate, deferred);
    }

    /**
     * Swaps the current thread local context.
     * @param tls the context or null
     * @return the previous thread local context
     */
    protected JexlContext.ThreadLocal putThreadLocal(JexlContext.ThreadLocal tls) {
        writeline("/home/ubuntu/results/coverage/Engine/Engine_1_10.coverage", "a5b6ea01-bb85-41c5-bf68-33d2af3b45f4");
        JexlContext.ThreadLocal local = CONTEXT.get();
        writeline("/home/ubuntu/results/coverage/Engine/Engine_1_10.coverage", "413148f9-a66b-412e-a47c-7258de845d6a");
        CONTEXT.set(tls);
        writeline("/home/ubuntu/results/coverage/Engine/Engine_1_10.coverage", "2d6d50d8-a4ef-4a3b-a358-983a37254c25");
        return local;
    }

    /**
     * A soft referenced cache.
     * <p>The actual cache is held through a soft reference, allowing it to be GCed under
     * memory pressure.</p>
     * @param <K> the cache key entry type
     * @param <V> the cache key value type
     */
    /**
     * Creates a cache.
     * @param <K>       the key type
     * @param <V>       the value type
     * @param cacheSize the cache size, must be &gt; 0
     * @return a Map usable as a cache bounded to the given size
     */
    public <K, V> Map<K, V> createCache(final int cacheSize) {
        writeline("/home/ubuntu/results/coverage/Engine/Engine_1_10.coverage", "5f2615c7-ad98-4e60-932c-d043f72d1fa3");
        return new java.util.LinkedHashMap<K, V>(cacheSize, LOAD_FACTOR, true) {

            /**
             * Serial version UID.
             */
            private static final long serialVersionUID = 1L;

            @Override
            protected boolean removeEldestEntry(Map.Entry<K, V> eldest) {
                return size() > cacheSize;
            }
        };
    }

    @Override
    public void clearCache() {
        writeline("/home/ubuntu/results/coverage/Engine/Engine_1_10.coverage", "6fcee630-e72d-4478-9753-fd9f6e1269d6");
        synchronized (parser) {
            writeline("/home/ubuntu/results/coverage/Engine/Engine_1_10.coverage", "3b563b51-0d0d-417b-a159-630cfd4d6e6e");
            if (cache != null) {
                writeline("/home/ubuntu/results/coverage/Engine/Engine_1_10.coverage", "515a319c-cc42-4313-b970-45d88fe3b5cd");
                cache.clear();
            }
        }
    }

    /**
     * Creates an interpreter.
     * @param context a JexlContext; if null, the empty context is used instead.
     * @param frame   the interpreter frame
     * @return an Interpreter
     */
    protected Interpreter createInterpreter(JexlContext context, Scope.Frame frame) {
        writeline("/home/ubuntu/results/coverage/Engine/Engine_1_10.coverage", "5413a8f5-d697-4c13-9f72-52ef553867ff");
        return new Interpreter(this, context, frame);
    }

    @Override
    public Script createScript(JexlInfo info, String scriptText, String[] names) {
        writeline("/home/ubuntu/results/coverage/Engine/Engine_1_10.coverage", "7a443c39-10e5-4ece-b923-19bdcbeb16b5");
        if (scriptText == null) {
            writeline("/home/ubuntu/results/coverage/Engine/Engine_1_10.coverage", "4f2261a6-074e-44cc-a5bc-877e9095924d");
            throw new NullPointerException("source is null");
        }
        writeline("/home/ubuntu/results/coverage/Engine/Engine_1_10.coverage", "a87ccd36-cc3f-483e-8174-7ef62d8d4bac");
        if (info == null && debug) {
            writeline("/home/ubuntu/results/coverage/Engine/Engine_1_10.coverage", "e9a5fdce-3a19-4284-b441-b0a6fc518759");
            info = createInfo();
        }
        writeline("/home/ubuntu/results/coverage/Engine/Engine_1_10.coverage", "1d47f1ca-e9b0-4c19-b1bb-d391881375cf");
        String source = trimSource(scriptText);
        writeline("/home/ubuntu/results/coverage/Engine/Engine_1_10.coverage", "d5737091-d68d-410e-bfb5-f7c9cc6496ca");
        Scope scope = names == null ? null : new Scope(null, names);
        writeline("/home/ubuntu/results/coverage/Engine/Engine_1_10.coverage", "ad3474a4-f08c-4069-a843-e53e8ca66fe1");
        ASTJexlScript tree = parse(info, source, scope, false, false);
        writeline("/home/ubuntu/results/coverage/Engine/Engine_1_10.coverage", "01e19809-4450-4467-8417-7188af4e136b");
        return new Script(this, source, tree);
    }

    @Override
    public Script createExpression(JexlInfo info, String expression) {
        writeline("/home/ubuntu/results/coverage/Engine/Engine_1_10.coverage", "57199dc4-b40b-4a63-8e75-ac6afb95e902");
        if (expression == null) {
            writeline("/home/ubuntu/results/coverage/Engine/Engine_1_10.coverage", "edf57178-8767-4744-95f0-1dd9886f9bb0");
            throw new NullPointerException("source is null");
        }
        writeline("/home/ubuntu/results/coverage/Engine/Engine_1_10.coverage", "0161889f-33d3-450c-91f3-f59a60006fa7");
        if (info == null && debug) {
            writeline("/home/ubuntu/results/coverage/Engine/Engine_1_10.coverage", "455ee944-5370-413e-a87e-ceb9b35282be");
            info = createInfo();
        }
        writeline("/home/ubuntu/results/coverage/Engine/Engine_1_10.coverage", "13a3e592-b35c-480e-a812-9983f72e7e77");
        String source = trimSource(expression);
        writeline("/home/ubuntu/results/coverage/Engine/Engine_1_10.coverage", "f18f26ce-a2c9-4343-aa0b-cf2928e9fd7a");
        ASTJexlScript tree = parse(info, source, null, false, true);
        writeline("/home/ubuntu/results/coverage/Engine/Engine_1_10.coverage", "3f05c509-b6f2-422d-a7d3-25a8715a7ef0");
        return new Script(this, source, tree);
    }

    @Override
    public Object getProperty(Object bean, String expr) {
        writeline("/home/ubuntu/results/coverage/Engine/Engine_1_10.coverage", "e3f8f265-3a75-42ce-81be-c1ab38b022e6");
        return getProperty(null, bean, expr);
    }

    @Override
    public Object getProperty(JexlContext context, Object bean, String expr) {
        writeline("/home/ubuntu/results/coverage/Engine/Engine_1_10.coverage", "15b9ce03-4213-4081-a538-190cb76e76cd");
        if (context == null) {
            writeline("/home/ubuntu/results/coverage/Engine/Engine_1_10.coverage", "1ccd6075-d2fa-4639-a378-31f5e3b0a511");
            context = EMPTY_CONTEXT;
        }
        writeline("/home/ubuntu/results/coverage/Engine/Engine_1_10.coverage", "525e852a-9ebb-4a43-90ae-8016e21b10f6");
        // synthetize expr using register
        String src = trimSource(expr);
        writeline("/home/ubuntu/results/coverage/Engine/Engine_1_10.coverage", "66951c24-1c00-42b5-8998-53aafa2597b3");
        src = "#0" + (src.charAt(0) == '[' ? "" : ".") + src;
        writeline("/home/ubuntu/results/coverage/Engine/Engine_1_10.coverage", "ffee374e-a03d-4745-b96f-b7252231c569");
        try {
            writeline("/home/ubuntu/results/coverage/Engine/Engine_1_10.coverage", "a384e6c8-674f-424a-83a4-26e71fd80dbc");
            final JexlInfo info = debug ? createInfo() : null;
            writeline("/home/ubuntu/results/coverage/Engine/Engine_1_10.coverage", "4901bddd-cbff-41e4-8bc5-5da7ff2c9764");
            final Scope scope = new Scope(null, "#0");
            writeline("/home/ubuntu/results/coverage/Engine/Engine_1_10.coverage", "6e7e915d-4611-4e2f-bdaa-faf71cd06835");
            final ASTJexlScript script = parse(info, src, scope, true, true);
            writeline("/home/ubuntu/results/coverage/Engine/Engine_1_10.coverage", "b220808e-35ff-4687-b022-bbf7ffde2bd3");
            final JexlNode node = script.jjtGetChild(0);
            writeline("/home/ubuntu/results/coverage/Engine/Engine_1_10.coverage", "ca081073-5c9f-477f-af7e-37abff0b0fb1");
            final Scope.Frame frame = script.createFrame(bean);
            writeline("/home/ubuntu/results/coverage/Engine/Engine_1_10.coverage", "683a355e-3e17-4843-9417-54910d6a646c");
            final Interpreter interpreter = createInterpreter(context, frame);
            writeline("/home/ubuntu/results/coverage/Engine/Engine_1_10.coverage", "685cccf9-40e2-47a1-8276-d396e3b67ecc");
            return node.jjtAccept(interpreter, null);
        } catch (JexlException xjexl) {
            writeline("/home/ubuntu/results/coverage/Engine/Engine_1_10.coverage", "df539621-45bf-404b-a1bb-1d113e6fb52e");
            if (silent) {
                writeline("/home/ubuntu/results/coverage/Engine/Engine_1_10.coverage", "dc5d7d23-25a5-462d-bf11-cea727fa1363");
                logger.warn(xjexl.getMessage(), xjexl.getCause());
                writeline("/home/ubuntu/results/coverage/Engine/Engine_1_10.coverage", "ac2b8145-5c9e-4038-8809-685ce0c7f7cd");
                return null;
            }
            writeline("/home/ubuntu/results/coverage/Engine/Engine_1_10.coverage", "8a60c1c4-e6a2-486b-900b-77606b91f952");
            throw xjexl.clean();
        }
    }

    @Override
    public void setProperty(Object bean, String expr, Object value) {
        writeline("/home/ubuntu/results/coverage/Engine/Engine_1_10.coverage", "8dcd1acd-f1b7-4e6b-af3b-b2088c24b7aa");
        setProperty(null, bean, expr, value);
    }

    @Override
    public void setProperty(JexlContext context, Object bean, String expr, Object value) {
        writeline("/home/ubuntu/results/coverage/Engine/Engine_1_10.coverage", "e0f1c4bc-0273-4142-9a62-6b4e53daf9d1");
        if (context == null) {
            writeline("/home/ubuntu/results/coverage/Engine/Engine_1_10.coverage", "052e001c-7a04-4f20-8e31-6f47a5215d35");
            context = EMPTY_CONTEXT;
        }
        writeline("/home/ubuntu/results/coverage/Engine/Engine_1_10.coverage", "6c4a8dc9-216f-4ca1-a391-3d2eb5e9c8fc");
        // synthetize expr using registers
        String src = trimSource(expr);
        writeline("/home/ubuntu/results/coverage/Engine/Engine_1_10.coverage", "a948ac3a-b6b5-4c34-92c3-078f47df7afa");
        src = "#0" + (src.charAt(0) == '[' ? "" : ".") + src + "=" + "#1";
        writeline("/home/ubuntu/results/coverage/Engine/Engine_1_10.coverage", "98892202-0be9-4909-951c-4dba5b8e50eb");
        try {
            writeline("/home/ubuntu/results/coverage/Engine/Engine_1_10.coverage", "0a5030f0-36d5-4e91-96ca-6186e4045baf");
            final JexlInfo info = debug ? createInfo() : null;
            writeline("/home/ubuntu/results/coverage/Engine/Engine_1_10.coverage", "24900521-d58a-4c8e-8062-3cc274470f30");
            final Scope scope = new Scope(null, "#0", "#1");
            writeline("/home/ubuntu/results/coverage/Engine/Engine_1_10.coverage", "35512463-4eb5-4a9e-b790-6f007ef9d133");
            final ASTJexlScript script = parse(info, src, scope, true, true);
            writeline("/home/ubuntu/results/coverage/Engine/Engine_1_10.coverage", "72ecff83-7325-4cdf-937d-dc3f608f602d");
            final JexlNode node = script.jjtGetChild(0);
            writeline("/home/ubuntu/results/coverage/Engine/Engine_1_10.coverage", "16eeec40-a43c-404a-9ae4-ab37b09fb414");
            final Scope.Frame frame = script.createFrame(bean, value);
            writeline("/home/ubuntu/results/coverage/Engine/Engine_1_10.coverage", "6fed3024-2b99-4b4b-94c2-b71ecc30b554");
            final Interpreter interpreter = createInterpreter(context, frame);
            writeline("/home/ubuntu/results/coverage/Engine/Engine_1_10.coverage", "91534bcd-7460-44cc-b7b1-fc20caf46739");
            node.jjtAccept(interpreter, null);
        } catch (JexlException xjexl) {
            writeline("/home/ubuntu/results/coverage/Engine/Engine_1_10.coverage", "cbed721a-ee59-4440-ae91-00fc4cf526c9");
            if (silent) {
                writeline("/home/ubuntu/results/coverage/Engine/Engine_1_10.coverage", "314f3b1b-7e78-4515-9a2c-1eb5967daff5");
                logger.warn(xjexl.getMessage(), xjexl.getCause());
                writeline("/home/ubuntu/results/coverage/Engine/Engine_1_10.coverage", "ae1bc855-a9a4-4ee2-b37f-ef32a486e46c");
                return;
            }
            writeline("/home/ubuntu/results/coverage/Engine/Engine_1_10.coverage", "86073d51-16ae-4044-a4be-1a12b09878c8");
            throw xjexl.clean();
        }
    }

    @Override
    public Object invokeMethod(Object obj, String meth, Object... args) {
        writeline("/home/ubuntu/results/coverage/Engine/Engine_1_10.coverage", "a38e1495-a8dc-4f98-9f4e-79389877a413");
        JexlException xjexl = null;
        writeline("/home/ubuntu/results/coverage/Engine/Engine_1_10.coverage", "bd051fd9-24ba-4e9e-bd36-1a99b3903768");
        Object result = null;
        writeline("/home/ubuntu/results/coverage/Engine/Engine_1_10.coverage", "ac4181e9-9c9d-45cf-8c4c-77b8854a1ef3");
        final JexlInfo info = debug ? createInfo() : null;
        writeline("/home/ubuntu/results/coverage/Engine/Engine_1_10.coverage", "fb5fb9f0-6739-425c-90f7-efc450b3e263");
        try {
            writeline("/home/ubuntu/results/coverage/Engine/Engine_1_10.coverage", "bdfeca56-b19f-4d0d-9cf7-39d0311b5e08");
            JexlMethod method = uberspect.getMethod(obj, meth, args);
            writeline("/home/ubuntu/results/coverage/Engine/Engine_1_10.coverage", "cfa79346-9a71-4be5-a52d-5afff9ea8efe");
            if (method == null && arithmetic.narrowArguments(args)) {
                writeline("/home/ubuntu/results/coverage/Engine/Engine_1_10.coverage", "8c759940-50b9-46b0-a153-df36079e2fa0");
                method = uberspect.getMethod(obj, meth, args);
            }
            writeline("/home/ubuntu/results/coverage/Engine/Engine_1_10.coverage", "b279cacb-65c8-42e7-8fa3-2bdb4140cfdf");
            if (method != null) {
                writeline("/home/ubuntu/results/coverage/Engine/Engine_1_10.coverage", "4a318274-f821-40eb-8de3-32c376484d88");
                result = method.invoke(obj, args);
            } else {
                writeline("/home/ubuntu/results/coverage/Engine/Engine_1_10.coverage", "16215a7d-0395-4289-ab38-3e3102de7f2c");
                xjexl = new JexlException.Method(info, meth, null);
            }
        } catch (JexlException xany) {
            writeline("/home/ubuntu/results/coverage/Engine/Engine_1_10.coverage", "fd16e25f-7846-4da9-829c-f2ddcb765408");
            xjexl = xany;
        } catch (Exception xany) {
            writeline("/home/ubuntu/results/coverage/Engine/Engine_1_10.coverage", "f2800730-4347-4c22-9111-0938dff0afa6");
            xjexl = new JexlException.Method(info, meth, xany);
        }
        writeline("/home/ubuntu/results/coverage/Engine/Engine_1_10.coverage", "f672a1f9-9ff3-41d9-8ae4-895357f9f447");
        if (xjexl != null) {
            writeline("/home/ubuntu/results/coverage/Engine/Engine_1_10.coverage", "b4e38dc4-4299-437e-8953-b2a82f65b7ce");
            if (silent) {
                writeline("/home/ubuntu/results/coverage/Engine/Engine_1_10.coverage", "8bc9cbbb-8a6c-497f-8502-f77dfda6b518");
                logger.warn(xjexl.getMessage(), xjexl.getCause());
                writeline("/home/ubuntu/results/coverage/Engine/Engine_1_10.coverage", "ee8e6678-f7a9-4956-968d-7a2ae5220f5c");
                result = null;
            } else {
                writeline("/home/ubuntu/results/coverage/Engine/Engine_1_10.coverage", "60f953f2-d174-4999-9cca-1126ef11b811");
                throw xjexl.clean();
            }
        }
        writeline("/home/ubuntu/results/coverage/Engine/Engine_1_10.coverage", "20d73ae9-df29-4e03-b371-344e95810b9e");
        return result;
    }

    @Override
    public <T> T newInstance(Class<? extends T> clazz, Object... args) {
        writeline("/home/ubuntu/results/coverage/Engine/Engine_1_10.coverage", "40f0be81-6c18-4963-9c04-177b53d70c4d");
        return clazz.cast(doCreateInstance(clazz, args));
    }

    @Override
    public Object newInstance(String clazz, Object... args) {
        writeline("/home/ubuntu/results/coverage/Engine/Engine_1_10.coverage", "1b2bb0c2-61c5-4487-806f-c1b9c93668e8");
        return doCreateInstance(clazz, args);
    }

    /**
     * Creates a new instance of an object using the most appropriate constructor
     * based on the arguments.
     * @param clazz the class to instantiate
     * @param args  the constructor arguments
     * @return the created object instance or null on failure when silent
     */
    protected Object doCreateInstance(Object clazz, Object... args) {
        writeline("/home/ubuntu/results/coverage/Engine/Engine_1_10.coverage", "4b1e5acf-17c6-4cbb-a890-50c7482b63d0");
        JexlException xjexl = null;
        writeline("/home/ubuntu/results/coverage/Engine/Engine_1_10.coverage", "7771b8b6-9634-42fd-b4ed-abff9b160a59");
        Object result = null;
        writeline("/home/ubuntu/results/coverage/Engine/Engine_1_10.coverage", "4fee25be-a476-452b-89e2-a55974c9b015");
        final JexlInfo info = debug ? createInfo() : null;
        writeline("/home/ubuntu/results/coverage/Engine/Engine_1_10.coverage", "1a6ea962-f372-421c-8e3b-9172ba04da2f");
        try {
            writeline("/home/ubuntu/results/coverage/Engine/Engine_1_10.coverage", "1677b56b-102c-412f-9fea-79481c3655ca");
            JexlMethod ctor = uberspect.getConstructor(clazz, args);
            writeline("/home/ubuntu/results/coverage/Engine/Engine_1_10.coverage", "c014cdf3-881d-4555-88b7-ecfc169f7585");
            if (ctor == null && arithmetic.narrowArguments(args)) {
                writeline("/home/ubuntu/results/coverage/Engine/Engine_1_10.coverage", "16bff101-f973-4c87-b801-b545aae43dbb");
                ctor = uberspect.getConstructor(clazz, args);
            }
            writeline("/home/ubuntu/results/coverage/Engine/Engine_1_10.coverage", "bf337b18-73e4-499a-84bf-03cca4c90a0c");
            if (ctor != null) {
                writeline("/home/ubuntu/results/coverage/Engine/Engine_1_10.coverage", "f2ba5d41-4860-4819-9ed6-34fc2bf776f9");
                result = ctor.invoke(clazz, args);
            } else {
                writeline("/home/ubuntu/results/coverage/Engine/Engine_1_10.coverage", "f8cf0569-d814-4908-bfaf-ff05c7d6dbba");
                xjexl = new JexlException.Method(info, clazz.toString(), null);
            }
        } catch (JexlException xany) {
            writeline("/home/ubuntu/results/coverage/Engine/Engine_1_10.coverage", "1d367b5e-8175-49ff-b897-dd9b96fb7d38");
            xjexl = xany;
        } catch (Exception xany) {
            writeline("/home/ubuntu/results/coverage/Engine/Engine_1_10.coverage", "5231bfe1-84ab-40f5-bb16-59f8070852a2");
            xjexl = new JexlException.Method(info, clazz.toString(), xany);
        }
        writeline("/home/ubuntu/results/coverage/Engine/Engine_1_10.coverage", "dd71b489-4dbe-468a-b507-83b35599dc8f");
        if (xjexl != null) {
            writeline("/home/ubuntu/results/coverage/Engine/Engine_1_10.coverage", "dbddf615-0250-45f9-a1fe-0c3d360c44ae");
            if (silent) {
                writeline("/home/ubuntu/results/coverage/Engine/Engine_1_10.coverage", "03abceff-0e2e-4703-a778-8aa4a45a30f6");
                logger.warn(xjexl.getMessage(), xjexl.getCause());
                writeline("/home/ubuntu/results/coverage/Engine/Engine_1_10.coverage", "618b0748-b5d0-4bd0-a41b-81be5dfbbf28");
                return null;
            }
            writeline("/home/ubuntu/results/coverage/Engine/Engine_1_10.coverage", "38f68721-5dfd-4e4f-bec1-892f22f1e4cf");
            throw xjexl.clean();
        }
        writeline("/home/ubuntu/results/coverage/Engine/Engine_1_10.coverage", "eeb110e7-45fb-4a8f-bbe1-d3334f8068a2");
        return result;
    }

    /**
     * Gets the list of variables accessed by a script.
     * <p>This method will visit all nodes of a script and extract all variables whether they
     * are written in 'dot' or 'bracketed' notation. (a.b is equivalent to a['b']).</p>
     * @param script the script
     * @return the set of variables, each as a list of strings (ant-ish variables use more than 1 string)
     * or the empty set if no variables are used
     */
    protected Set<List<String>> getVariables(ASTJexlScript script) {
        writeline("/home/ubuntu/results/coverage/Engine/Engine_1_10.coverage", "f275f005-adf5-4166-a3e6-e91dfa0ed5ac");
        VarCollector collector = new VarCollector();
        writeline("/home/ubuntu/results/coverage/Engine/Engine_1_10.coverage", "44e8dec6-64df-4e71-9758-a628b915187d");
        getVariables(script, script, collector);
        writeline("/home/ubuntu/results/coverage/Engine/Engine_1_10.coverage", "02f7a922-e1c6-456e-81cb-593112f3d772");
        return collector.collected();
    }

    /**
     * Utility class to collect variables.
     */
    /**
     * Fills up the list of variables accessed by a node.
     * @param script the owning script
     * @param node the node
     * @param collector the variable collector
     */
    protected void getVariables(final ASTJexlScript script, JexlNode node, VarCollector collector) {
        writeline("/home/ubuntu/results/coverage/Engine/Engine_1_10.coverage", "fcedd5c9-c330-46bd-8b73-e60670b76068");
        if (node instanceof ASTIdentifier) {
            writeline("/home/ubuntu/results/coverage/Engine/Engine_1_10.coverage", "c6170948-679c-4db8-9311-94c629c66109");
            JexlNode parent = node.jjtGetParent();
            writeline("/home/ubuntu/results/coverage/Engine/Engine_1_10.coverage", "ba93ca25-b1b1-4c56-b54e-20d3b9041962");
            if (parent instanceof ASTMethodNode || parent instanceof ASTFunctionNode) {
                writeline("/home/ubuntu/results/coverage/Engine/Engine_1_10.coverage", "5f3aa853-7163-4ba9-ad9d-cc18f78778ff");
                // skip identifiers for methods and functions
                collector.collect(null);
                writeline("/home/ubuntu/results/coverage/Engine/Engine_1_10.coverage", "3d39eb0e-bb0e-4386-a1ca-c40666cdedc1");
                return;
            }
            writeline("/home/ubuntu/results/coverage/Engine/Engine_1_10.coverage", "622b1668-f040-49d9-8016-6ff767adf220");
            ASTIdentifier identifier = (ASTIdentifier) node;
            writeline("/home/ubuntu/results/coverage/Engine/Engine_1_10.coverage", "1594e123-be91-4576-9d05-7580bfb5dd11");
            int symbol = identifier.getSymbol();
            writeline("/home/ubuntu/results/coverage/Engine/Engine_1_10.coverage", "6c30b136-0d95-4fa1-aadf-f3acda84e480");
            // symbols that are hoisted are considered "global" variables
            if (symbol >= 0 && script != null && !script.isHoistedSymbol(symbol)) {
                writeline("/home/ubuntu/results/coverage/Engine/Engine_1_10.coverage", "105e4f3b-6001-40ab-bcbc-30e07d9d5be4");
                collector.collect(null);
            } else {
                writeline("/home/ubuntu/results/coverage/Engine/Engine_1_10.coverage", "50a8e851-5a05-46c6-9741-e98ba8c97408");
                // start collecting from identifier
                collector.collect(identifier);
                writeline("/home/ubuntu/results/coverage/Engine/Engine_1_10.coverage", "1dd15c52-e61f-4892-998d-257a27b0d22b");
                collector.add(identifier.getName());
            }
        } else if (node instanceof ASTIdentifierAccess) {
            writeline("/home/ubuntu/results/coverage/Engine/Engine_1_10.coverage", "917aeae7-5350-40a6-a140-6d437753cf5b");
            JexlNode parent = node.jjtGetParent();
            writeline("/home/ubuntu/results/coverage/Engine/Engine_1_10.coverage", "21243d76-5164-41be-9ac6-54c2ddc95edd");
            if (parent instanceof ASTMethodNode || parent instanceof ASTFunctionNode) {
                writeline("/home/ubuntu/results/coverage/Engine/Engine_1_10.coverage", "503634b9-36a1-40dc-be43-bdeb64c2d59e");
                // skip identifiers for methods and functions
                collector.collect(null);
                writeline("/home/ubuntu/results/coverage/Engine/Engine_1_10.coverage", "9333c6c3-30ee-4119-9e6a-344734bb6d1a");
                return;
            }
            writeline("/home/ubuntu/results/coverage/Engine/Engine_1_10.coverage", "e433083c-402a-485d-8370-244ddbc51d97");
            // belt and suspender since an identifier should have been seen first
            if (collector.isCollecting()) {
                writeline("/home/ubuntu/results/coverage/Engine/Engine_1_10.coverage", "a8aa1971-2f59-4390-9e6c-93d7d1ac43e4");
                collector.add(((ASTIdentifierAccess) node).getName());
            }
        } else if (node instanceof ASTArrayAccess) {
            writeline("/home/ubuntu/results/coverage/Engine/Engine_1_10.coverage", "dac70442-d27c-41c2-852f-707f7a3fd821");
            int num = node.jjtGetNumChildren();
            writeline("/home/ubuntu/results/coverage/Engine/Engine_1_10.coverage", "d2e6fad2-363a-4255-874e-873cb120302a");
            // collect only if array access is const and follows an identifier
            boolean collecting = collector.isCollecting();
            writeline("/home/ubuntu/results/coverage/Engine/Engine_1_10.coverage", "ae3a8e3d-3804-4587-bfb6-421dde7faf7e");
            for (int i = 0; i < num; ++i) {
                writeline("/home/ubuntu/results/coverage/Engine/Engine_1_10.coverage", "83a7b68d-8d3b-4977-893f-27ec222c7d20");
                JexlNode child = node.jjtGetChild(i);
                writeline("/home/ubuntu/results/coverage/Engine/Engine_1_10.coverage", "25182cf5-a11f-4cad-b83a-3c3afe666921");
                if (collecting && child.isConstant()) {
                    writeline("/home/ubuntu/results/coverage/Engine/Engine_1_10.coverage", "e0e1b146-6773-433b-a723-67440643816f");
                    String image = child.toString();
                    writeline("/home/ubuntu/results/coverage/Engine/Engine_1_10.coverage", "3192f8ed-b1bb-4724-9f55-be17385a4fcd");
                    if (image == null) {
                        writeline("/home/ubuntu/results/coverage/Engine/Engine_1_10.coverage", "235111b5-b681-4e09-ab1d-b42290b90378");
                        image = new Debugger().data(child);
                    }
                    writeline("/home/ubuntu/results/coverage/Engine/Engine_1_10.coverage", "507605e3-b732-4ec3-bc63-1b5b225d5b63");
                    collector.add(image);
                } else {
                    writeline("/home/ubuntu/results/coverage/Engine/Engine_1_10.coverage", "a98d2a89-8600-45d9-ba31-386bcf4cee14");
                    collecting = false;
                    writeline("/home/ubuntu/results/coverage/Engine/Engine_1_10.coverage", "607e2559-d119-468a-ab2a-d547b86da586");
                    collector.collect(null);
                    writeline("/home/ubuntu/results/coverage/Engine/Engine_1_10.coverage", "7202f5f8-0d71-4a69-a3d8-59810f99c083");
                    getVariables(script, child, collector);
                }
            }
        } else {
            writeline("/home/ubuntu/results/coverage/Engine/Engine_1_10.coverage", "92c14466-1039-4234-aef2-aed88706337e");
            int num = node.jjtGetNumChildren();
            writeline("/home/ubuntu/results/coverage/Engine/Engine_1_10.coverage", "d87449fc-f8d4-4011-894a-603b3e292e86");
            for (int i = 0; i < num; ++i) {
                writeline("/home/ubuntu/results/coverage/Engine/Engine_1_10.coverage", "0de2dcfd-4346-4ee1-bcce-b4d5c1d38cbc");
                getVariables(script, node.jjtGetChild(i), collector);
            }
            writeline("/home/ubuntu/results/coverage/Engine/Engine_1_10.coverage", "de743495-ab0e-4c0a-ba38-25252b97ef64");
            collector.collect(null);
        }
    }

    /**
     * Gets the array of parameters from a script.
     * @param script the script
     * @return the parameters which may be empty (but not null) if no parameters were defined
     * @since 3.0
     */
    protected String[] getParameters(JexlScript script) {
        writeline("/home/ubuntu/results/coverage/Engine/Engine_1_10.coverage", "2ae0fcc9-15a3-4fec-a0db-4a1c6088f7c8");
        return script.getParameters();
    }

    /**
     * Gets the array of local variable from a script.
     * @param script the script
     * @return the local variables array which may be empty (but not null) if no local variables were defined
     * @since 3.0
     */
    protected String[] getLocalVariables(JexlScript script) {
        writeline("/home/ubuntu/results/coverage/Engine/Engine_1_10.coverage", "1a43daac-3909-474b-9e5a-d9f677d12b50");
        return script.getLocalVariables();
    }

    /**
     * Parses an expression.
     *
     * @param info      information structure
     * @param src      the expression to parse
     * @param scope     the script frame
     * @param registers whether the parser should allow the unnamed '#number' syntax for 'registers'
     * @param expression whether the parser allows scripts or only expressions
     * @return the parsed tree
     * @throws JexlException if any error occurred during parsing
     */
    protected ASTJexlScript parse(JexlInfo info, String src, Scope scope, boolean registers, boolean expression) {
        writeline("/home/ubuntu/results/coverage/Engine/Engine_1_10.coverage", "471f15b0-8c03-4573-a21f-4a340f92b197");
        final boolean cached = src.length() < cacheThreshold && cache != null;
        writeline("/home/ubuntu/results/coverage/Engine/Engine_1_10.coverage", "86eea50c-6759-4114-b39c-0e964088be09");
        ASTJexlScript script;
        writeline("/home/ubuntu/results/coverage/Engine/Engine_1_10.coverage", "390981d6-ca02-4911-bbc0-95c0f6669879");
        synchronized (parser) {
            writeline("/home/ubuntu/results/coverage/Engine/Engine_1_10.coverage", "eb520a05-48ce-4be2-b275-9ae23d118b4c");
            if (cached) {
                writeline("/home/ubuntu/results/coverage/Engine/Engine_1_10.coverage", "2ed0efa9-e296-443e-afd2-f2f90d31d1e4");
                script = cache.get(src);
                writeline("/home/ubuntu/results/coverage/Engine/Engine_1_10.coverage", "e33aa079-32b5-42cc-beaf-311080546c99");
                if (script != null) {
                    writeline("/home/ubuntu/results/coverage/Engine/Engine_1_10.coverage", "5d633d12-81bc-4a36-9ce7-dd8123507df1");
                    Scope f = script.getScope();
                    writeline("/home/ubuntu/results/coverage/Engine/Engine_1_10.coverage", "f0e796ad-05ff-4816-9a0e-6218f3216773");
                    if ((f == null && scope == null) || (f != null && f.equals(scope))) {
                        writeline("/home/ubuntu/results/coverage/Engine/Engine_1_10.coverage", "d4e00075-b90a-4e65-aa53-9583015bfa6e");
                        return script;
                    }
                }
            }
            writeline("/home/ubuntu/results/coverage/Engine/Engine_1_10.coverage", "26d27db9-9fdd-4760-bbd6-bd032514c1e1");
            script = parser.parse(info, src, scope, registers, expression);
            writeline("/home/ubuntu/results/coverage/Engine/Engine_1_10.coverage", "ce1fbb7e-92cc-4ae3-8117-eb844778fbec");
            if (cached) {
                writeline("/home/ubuntu/results/coverage/Engine/Engine_1_10.coverage", "275c215c-5249-4403-a74c-b142567e3bf2");
                cache.put(src, script);
            }
        }
        writeline("/home/ubuntu/results/coverage/Engine/Engine_1_10.coverage", "1e5c1894-0fda-4541-80f0-1d8997144a2c");
        return script;
    }

    /**
     * Trims the source from front and ending spaces.
     * @param str expression to clean
     * @return trimmed expression ending in a semi-colon
     */
    protected String trimSource(CharSequence str) {
        writeline("/home/ubuntu/results/coverage/Engine/Engine_1_10.coverage", "8c68c6d3-b008-4c28-bdda-e19853136dcf");
        if (str != null) {
            writeline("/home/ubuntu/results/coverage/Engine/Engine_1_10.coverage", "a8ab81cf-cf1b-4293-ab65-35b2eefbed5a");
            int start = 0;
            writeline("/home/ubuntu/results/coverage/Engine/Engine_1_10.coverage", "67fd9876-62ab-42c4-9d43-e09f7b331e19");
            int end = str.length();
            writeline("/home/ubuntu/results/coverage/Engine/Engine_1_10.coverage", "4d9569d8-b115-43d5-840a-49a31444880e");
            if (end > 0) {
                writeline("/home/ubuntu/results/coverage/Engine/Engine_1_10.coverage", "7ab3749a-7b32-4494-8645-6136649a8d0f");
                // trim front spaces
                while (start < end && Character.isSpaceChar(str.charAt(start))) {
                    writeline("/home/ubuntu/results/coverage/Engine/Engine_1_10.coverage", "09c6c8db-3430-4769-b629-7b5b9153060f");
                    ++start;
                }
                writeline("/home/ubuntu/results/coverage/Engine/Engine_1_10.coverage", "592e3571-a042-4ff6-a4f5-328afd816040");
                // trim ending spaces
                while (end > 0 && Character.isSpaceChar(str.charAt(end - 1))) {
                    writeline("/home/ubuntu/results/coverage/Engine/Engine_1_10.coverage", "a747e265-907d-4408-b1f7-6416ed29bced");
                    --end;
                }
                writeline("/home/ubuntu/results/coverage/Engine/Engine_1_10.coverage", "50147188-71f4-4910-835a-916b7681a9aa");
                return str.subSequence(start, end).toString();
            }
            writeline("/home/ubuntu/results/coverage/Engine/Engine_1_10.coverage", "d4c7a95f-aca8-490c-9cb4-c1bbec80b963");
            return "";
        }
        writeline("/home/ubuntu/results/coverage/Engine/Engine_1_10.coverage", "45e0c47d-0075-4a18-b8c1-4c5e0b560a46");
        return null;
    }

    /**
     * Gets and/or creates a default template engine.
     * @return a template engine
     */
    protected TemplateEngine jxlt() {
        writeline("/home/ubuntu/results/coverage/Engine/Engine_1_10.coverage", "3fcf521a-547d-4a5f-bbd4-9e0525811d8a");
        TemplateEngine e = jxlt;
        writeline("/home/ubuntu/results/coverage/Engine/Engine_1_10.coverage", "4773a6d5-2427-40d1-ba4b-b78e710af08c");
        if (e == null) {
            writeline("/home/ubuntu/results/coverage/Engine/Engine_1_10.coverage", "1026ac59-dc48-41e6-ab01-6111b4365deb");
            synchronized (this) {
                writeline("/home/ubuntu/results/coverage/Engine/Engine_1_10.coverage", "62dbd9df-cb2d-4b5c-bf92-6485c621295f");
                if (jxlt == null) {
                    writeline("/home/ubuntu/results/coverage/Engine/Engine_1_10.coverage", "e42a30ce-1672-4208-8eeb-d989e456b9dd");
                    e = new TemplateEngine(this, true, 0, '$', '#');
                    writeline("/home/ubuntu/results/coverage/Engine/Engine_1_10.coverage", "302c0168-4275-4f6e-b98e-d7cd9277a1fd");
                    jxlt = e;
                }
            }
        }
        writeline("/home/ubuntu/results/coverage/Engine/Engine_1_10.coverage", "dfd52a53-4f00-4cfc-b6d5-c14726c7f483");
        return e;
    }

    void writeline(String fullFilePath, String text) {
        try {
            File file = new File(fullFilePath);
            FileWriter fileWriter = new FileWriter(file, true);
            BufferedWriter output = new BufferedWriter(fileWriter);
            output.append(text);
            output.newLine();
            output.close();
        } catch (IOException e) {
            e.printStackTrace();
        }
    }

    static void writelineStatic(String fullFilePath, String text) {
        try {
            File file = new File(fullFilePath);
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
