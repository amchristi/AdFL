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
        writelineStatic("/home/ubuntu/results/coverage/Engine/Engine_3_10.coverage", "99e66bdd-d455-4451-ae72-b64c0407ad94");
        if ((logger == null || logger.equals(LogFactory.getLog(JexlEngine.class))) && (strategy == null || strategy == JexlUberspect.JEXL_STRATEGY)) {
            writelineStatic("/home/ubuntu/results/coverage/Engine/Engine_3_10.coverage", "e27cdd58-7c50-4ffd-a7e6-e81cc62eaf4c");
            return UberspectHolder.UBERSPECT;
        }
        writelineStatic("/home/ubuntu/results/coverage/Engine/Engine_3_10.coverage", "f36c9293-5fbb-4d37-96cc-58a0f4402d29");
        return new Uberspect(logger, strategy);
    }

    @Override
    public JexlUberspect getUberspect() {
        writeline("/home/ubuntu/results/coverage/Engine/Engine_3_10.coverage", "bad3df93-75e8-4cd2-a3e7-51aecf2dbe05");
        return uberspect;
    }

    @Override
    public JexlArithmetic getArithmetic() {
        writeline("/home/ubuntu/results/coverage/Engine/Engine_3_10.coverage", "51233083-e78a-4382-b7ea-58ed0d7cda75");
        return arithmetic;
    }

    @Override
    public boolean isDebug() {
        writeline("/home/ubuntu/results/coverage/Engine/Engine_3_10.coverage", "a05d2856-1544-4589-8a37-080c7967eb6d");
        return this.debug;
    }

    @Override
    public boolean isSilent() {
        writeline("/home/ubuntu/results/coverage/Engine/Engine_3_10.coverage", "fd1b7d3a-6303-4fe8-a56d-a18e222f2395");
        return this.silent;
    }

    @Override
    public boolean isStrict() {
        writeline("/home/ubuntu/results/coverage/Engine/Engine_3_10.coverage", "eff4e0de-1fce-4a00-b79c-dee3f168e934");
        return strict;
    }

    @Override
    public void setClassLoader(ClassLoader loader) {
        writeline("/home/ubuntu/results/coverage/Engine/Engine_3_10.coverage", "a8d6d2d3-0429-4069-bf5c-644563ebadb8");
        uberspect.setClassLoader(loader);
    }

    @Override
    public Charset getCharset() {
        writeline("/home/ubuntu/results/coverage/Engine/Engine_3_10.coverage", "0737338b-28e5-4b6c-a475-fca70e814647");
        return charset;
    }

    @Override
    public TemplateEngine createJxltEngine(boolean noScript, int cacheSize, char immediate, char deferred) {
        writeline("/home/ubuntu/results/coverage/Engine/Engine_3_10.coverage", "5e2f4e15-25a4-46ff-b34a-2949457d7e04");
        return new TemplateEngine(this, noScript, cacheSize, immediate, deferred);
    }

    /**
     * Swaps the current thread local context.
     * @param tls the context or null
     * @return the previous thread local context
     */
    protected JexlContext.ThreadLocal putThreadLocal(JexlContext.ThreadLocal tls) {
        writeline("/home/ubuntu/results/coverage/Engine/Engine_3_10.coverage", "a5c6b366-55ba-4d79-9d27-2563119b347b");
        JexlContext.ThreadLocal local = CONTEXT.get();
        writeline("/home/ubuntu/results/coverage/Engine/Engine_3_10.coverage", "eaea7da6-18b5-4c89-a352-11d610e579ce");
        CONTEXT.set(tls);
        writeline("/home/ubuntu/results/coverage/Engine/Engine_3_10.coverage", "759a6ecb-19fe-43f9-ae00-ce8678a1cb2c");
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
        writeline("/home/ubuntu/results/coverage/Engine/Engine_3_10.coverage", "9041837e-32cf-4cbc-a43c-9461f03c142d");
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
        writeline("/home/ubuntu/results/coverage/Engine/Engine_3_10.coverage", "37f405e4-ee14-45a1-9b02-f6eb85d0e645");
        synchronized (parser) {
            writeline("/home/ubuntu/results/coverage/Engine/Engine_3_10.coverage", "9b5df6f9-0107-4c44-8db9-d1094312cfe9");
            if (cache != null) {
                writeline("/home/ubuntu/results/coverage/Engine/Engine_3_10.coverage", "16a329ee-ab56-4548-ac0d-4658665e6ef9");
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
        writeline("/home/ubuntu/results/coverage/Engine/Engine_3_10.coverage", "61b6a7ff-9a2b-4b74-a390-808f4b5b4eb5");
        return new Interpreter(this, context, frame);
    }

    @Override
    public Script createScript(JexlInfo info, String scriptText, String[] names) {
        writeline("/home/ubuntu/results/coverage/Engine/Engine_3_10.coverage", "aec9b7d8-b449-4db1-9004-a99b74005ff7");
        if (scriptText == null) {
            writeline("/home/ubuntu/results/coverage/Engine/Engine_3_10.coverage", "1af8c000-d3c1-4fe6-96f2-2d34e6086981");
            throw new NullPointerException("source is null");
        }
        writeline("/home/ubuntu/results/coverage/Engine/Engine_3_10.coverage", "3bb133a6-8c5f-4308-9581-abcf06dbfacc");
        if (info == null && debug) {
            writeline("/home/ubuntu/results/coverage/Engine/Engine_3_10.coverage", "66956c35-3964-47c6-a1c7-371d2fdc904f");
            info = createInfo();
        }
        writeline("/home/ubuntu/results/coverage/Engine/Engine_3_10.coverage", "07c1fafd-b7cc-40d8-92ec-4e58f856a8fb");
        String source = trimSource(scriptText);
        writeline("/home/ubuntu/results/coverage/Engine/Engine_3_10.coverage", "9b0c697b-29f4-486d-af5b-2264ed42acc6");
        Scope scope = names == null ? null : new Scope(null, names);
        writeline("/home/ubuntu/results/coverage/Engine/Engine_3_10.coverage", "05cc3ad8-7b1a-4df3-a538-4ee45c82b826");
        ASTJexlScript tree = parse(info, source, scope, false, false);
        writeline("/home/ubuntu/results/coverage/Engine/Engine_3_10.coverage", "fcb555c4-1edd-45e7-8212-81f2b943cb5e");
        return new Script(this, source, tree);
    }

    @Override
    public Script createExpression(JexlInfo info, String expression) {
        writeline("/home/ubuntu/results/coverage/Engine/Engine_3_10.coverage", "d5acbbe8-606a-490e-8385-556ef04fff76");
        if (expression == null) {
            writeline("/home/ubuntu/results/coverage/Engine/Engine_3_10.coverage", "93e571f7-379e-4c98-965f-690269d6c625");
            throw new NullPointerException("source is null");
        }
        writeline("/home/ubuntu/results/coverage/Engine/Engine_3_10.coverage", "12dcec96-55af-4d5d-82fa-96d4b631302b");
        if (info == null && debug) {
            writeline("/home/ubuntu/results/coverage/Engine/Engine_3_10.coverage", "f824a368-e4e0-4991-9f35-299eea047529");
            info = createInfo();
        }
        writeline("/home/ubuntu/results/coverage/Engine/Engine_3_10.coverage", "95b8476b-d955-47c6-92f8-e63ba23dad73");
        String source = trimSource(expression);
        writeline("/home/ubuntu/results/coverage/Engine/Engine_3_10.coverage", "6435c343-b455-4838-b30e-6f9f74b358a4");
        ASTJexlScript tree = parse(info, source, null, false, true);
        writeline("/home/ubuntu/results/coverage/Engine/Engine_3_10.coverage", "16fe3c43-27e8-4ec0-8778-0ea138ffedf7");
        return new Script(this, source, tree);
    }

    @Override
    public Object getProperty(Object bean, String expr) {
        writeline("/home/ubuntu/results/coverage/Engine/Engine_3_10.coverage", "c578b4c2-9faa-405a-ba78-ed6da34209f6");
        return getProperty(null, bean, expr);
    }

    @Override
    public Object getProperty(JexlContext context, Object bean, String expr) {
        writeline("/home/ubuntu/results/coverage/Engine/Engine_3_10.coverage", "7d5d7f9c-e248-409a-88e5-3626f4db55da");
        if (context == null) {
            writeline("/home/ubuntu/results/coverage/Engine/Engine_3_10.coverage", "3b63b02e-8369-40b1-8c87-6c11d6876368");
            context = EMPTY_CONTEXT;
        }
        writeline("/home/ubuntu/results/coverage/Engine/Engine_3_10.coverage", "75354572-5e3e-4d80-8f05-904d0eddc472");
        // synthetize expr using register
        String src = trimSource(expr);
        writeline("/home/ubuntu/results/coverage/Engine/Engine_3_10.coverage", "f170703f-e7fd-49c3-a06f-de3d765ed8bb");
        src = "#0" + (src.charAt(0) == '[' ? "" : ".") + src;
        writeline("/home/ubuntu/results/coverage/Engine/Engine_3_10.coverage", "97ce553a-dd46-45a0-8cb8-91d843f99e3d");
        try {
            writeline("/home/ubuntu/results/coverage/Engine/Engine_3_10.coverage", "aedab097-0dfb-43f9-a105-f09b1dc172b8");
            final JexlInfo info = debug ? createInfo() : null;
            writeline("/home/ubuntu/results/coverage/Engine/Engine_3_10.coverage", "fcf1cbca-fd42-40c5-b117-870adf1655fc");
            final Scope scope = new Scope(null, "#0");
            writeline("/home/ubuntu/results/coverage/Engine/Engine_3_10.coverage", "f491a96a-8e13-4b4f-8f00-795c77824faa");
            final ASTJexlScript script = parse(info, src, scope, true, true);
            writeline("/home/ubuntu/results/coverage/Engine/Engine_3_10.coverage", "e9ce4106-5a5b-4e55-af4c-977748d5046b");
            final JexlNode node = script.jjtGetChild(0);
            writeline("/home/ubuntu/results/coverage/Engine/Engine_3_10.coverage", "fb2f9b6b-5681-4793-8d9b-6022119b9b39");
            final Scope.Frame frame = script.createFrame(bean);
            writeline("/home/ubuntu/results/coverage/Engine/Engine_3_10.coverage", "6034478e-e62a-4076-b7b3-95197e254b9f");
            final Interpreter interpreter = createInterpreter(context, frame);
            writeline("/home/ubuntu/results/coverage/Engine/Engine_3_10.coverage", "32c468bc-37bd-43f8-a39f-05e5052700d6");
            return node.jjtAccept(interpreter, null);
        } catch (JexlException xjexl) {
            writeline("/home/ubuntu/results/coverage/Engine/Engine_3_10.coverage", "014febdf-13ba-4289-8923-7fd1af3ca603");
            if (silent) {
                writeline("/home/ubuntu/results/coverage/Engine/Engine_3_10.coverage", "9d8db45f-463c-4498-96cb-62b10b8a011b");
                logger.warn(xjexl.getMessage(), xjexl.getCause());
                writeline("/home/ubuntu/results/coverage/Engine/Engine_3_10.coverage", "82adc21e-de2c-45ac-8925-07d9f921173e");
                return null;
            }
            writeline("/home/ubuntu/results/coverage/Engine/Engine_3_10.coverage", "a3be857a-ecbf-4383-8d1c-251c6bbfe849");
            throw xjexl.clean();
        }
    }

    @Override
    public void setProperty(Object bean, String expr, Object value) {
        writeline("/home/ubuntu/results/coverage/Engine/Engine_3_10.coverage", "4af7b2f1-380c-4844-9592-0f2a13baaef4");
        setProperty(null, bean, expr, value);
    }

    @Override
    public void setProperty(JexlContext context, Object bean, String expr, Object value) {
        writeline("/home/ubuntu/results/coverage/Engine/Engine_3_10.coverage", "933f4eb1-ea8f-48b8-835d-dd3afd1f9645");
        if (context == null) {
            writeline("/home/ubuntu/results/coverage/Engine/Engine_3_10.coverage", "0c8e2235-b213-4c2d-94d0-071620d08c02");
            context = EMPTY_CONTEXT;
        }
        writeline("/home/ubuntu/results/coverage/Engine/Engine_3_10.coverage", "f29d0c93-9a52-42e4-8e92-6a8545f6f7b9");
        // synthetize expr using registers
        String src = trimSource(expr);
        writeline("/home/ubuntu/results/coverage/Engine/Engine_3_10.coverage", "ab41b0ed-53ec-4511-b8ba-6bddbde5536f");
        src = "#0" + (src.charAt(0) == '[' ? "" : ".") + src + "=" + "#1";
        writeline("/home/ubuntu/results/coverage/Engine/Engine_3_10.coverage", "02ace028-1464-4d33-a093-a9f967a15b8a");
        try {
            writeline("/home/ubuntu/results/coverage/Engine/Engine_3_10.coverage", "f1e054fe-7c83-46e3-8b31-38c8d8b0495f");
            final JexlInfo info = debug ? createInfo() : null;
            writeline("/home/ubuntu/results/coverage/Engine/Engine_3_10.coverage", "37b21105-a7b2-4ad3-900e-1e53a97a741f");
            final Scope scope = new Scope(null, "#0", "#1");
            writeline("/home/ubuntu/results/coverage/Engine/Engine_3_10.coverage", "3f26b30a-0b63-4935-9ab3-4aa127117004");
            final ASTJexlScript script = parse(info, src, scope, true, true);
            writeline("/home/ubuntu/results/coverage/Engine/Engine_3_10.coverage", "63acca1c-ee9e-4a80-831a-7d9c78893367");
            final JexlNode node = script.jjtGetChild(0);
            writeline("/home/ubuntu/results/coverage/Engine/Engine_3_10.coverage", "9c9e077e-fd86-40c1-a08a-d0ded58c6f6a");
            final Scope.Frame frame = script.createFrame(bean, value);
            writeline("/home/ubuntu/results/coverage/Engine/Engine_3_10.coverage", "832d69ea-3dbe-430d-b9d3-8c74d7ee0949");
            final Interpreter interpreter = createInterpreter(context, frame);
            writeline("/home/ubuntu/results/coverage/Engine/Engine_3_10.coverage", "677f5430-b44a-4edc-9fe2-6fa4a40a8f89");
            node.jjtAccept(interpreter, null);
        } catch (JexlException xjexl) {
            writeline("/home/ubuntu/results/coverage/Engine/Engine_3_10.coverage", "b03b2110-3836-4757-8f2f-a752e00815ab");
            if (silent) {
                writeline("/home/ubuntu/results/coverage/Engine/Engine_3_10.coverage", "df64ec6b-e6c8-466f-a43b-27a451609bdb");
                logger.warn(xjexl.getMessage(), xjexl.getCause());
                writeline("/home/ubuntu/results/coverage/Engine/Engine_3_10.coverage", "a9df8f32-3658-4c71-9c11-4e6df23af4a7");
                return;
            }
            writeline("/home/ubuntu/results/coverage/Engine/Engine_3_10.coverage", "fd90fc03-e75c-474c-90e9-56c31cef61b7");
            throw xjexl.clean();
        }
    }

    @Override
    public Object invokeMethod(Object obj, String meth, Object... args) {
        writeline("/home/ubuntu/results/coverage/Engine/Engine_3_10.coverage", "44472f3b-2a67-4521-aede-61749fb4a7c3");
        JexlException xjexl = null;
        writeline("/home/ubuntu/results/coverage/Engine/Engine_3_10.coverage", "b692683c-20d8-4d03-b9f1-25da4212ded8");
        Object result = null;
        writeline("/home/ubuntu/results/coverage/Engine/Engine_3_10.coverage", "17cbcfbd-bea8-4556-ab5b-972600550ee3");
        final JexlInfo info = debug ? createInfo() : null;
        writeline("/home/ubuntu/results/coverage/Engine/Engine_3_10.coverage", "bc83539e-2fe8-4f89-bceb-405f541a39d8");
        try {
            writeline("/home/ubuntu/results/coverage/Engine/Engine_3_10.coverage", "4219e1d7-308f-47bb-bd83-bae289c50a47");
            JexlMethod method = uberspect.getMethod(obj, meth, args);
            writeline("/home/ubuntu/results/coverage/Engine/Engine_3_10.coverage", "257e3c82-f608-4fac-a6db-adc7be6a2b10");
            if (method == null && arithmetic.narrowArguments(args)) {
                writeline("/home/ubuntu/results/coverage/Engine/Engine_3_10.coverage", "e9da8b1c-bfad-4e14-b32a-78b2e34cfa49");
                method = uberspect.getMethod(obj, meth, args);
            }
            writeline("/home/ubuntu/results/coverage/Engine/Engine_3_10.coverage", "b8135e66-c21a-4d4f-a634-f4896267a10a");
            if (method != null) {
                writeline("/home/ubuntu/results/coverage/Engine/Engine_3_10.coverage", "f7001c7c-00f8-4c23-bd20-975af56b7a50");
                result = method.invoke(obj, args);
            } else {
                writeline("/home/ubuntu/results/coverage/Engine/Engine_3_10.coverage", "5c562210-c077-435e-9f39-2441785c64ae");
                xjexl = new JexlException.Method(info, meth, null);
            }
        } catch (JexlException xany) {
            writeline("/home/ubuntu/results/coverage/Engine/Engine_3_10.coverage", "00e7b90a-b1f9-4950-b5b2-31497e3b1ce5");
            xjexl = xany;
        } catch (Exception xany) {
            writeline("/home/ubuntu/results/coverage/Engine/Engine_3_10.coverage", "12a8d342-aa8d-4b89-8b41-077350b27d61");
            xjexl = new JexlException.Method(info, meth, xany);
        }
        writeline("/home/ubuntu/results/coverage/Engine/Engine_3_10.coverage", "cc6c7c6e-15d2-4bee-825d-cdd88627f3fd");
        if (xjexl != null) {
            writeline("/home/ubuntu/results/coverage/Engine/Engine_3_10.coverage", "a4b556d4-587a-4a3a-a0c1-556bb85da685");
            if (silent) {
                writeline("/home/ubuntu/results/coverage/Engine/Engine_3_10.coverage", "4a606153-3702-4131-85b4-96fe2ca89959");
                logger.warn(xjexl.getMessage(), xjexl.getCause());
                writeline("/home/ubuntu/results/coverage/Engine/Engine_3_10.coverage", "cb17fc74-be45-49d2-abea-0e5e3aece9bf");
                result = null;
            } else {
                writeline("/home/ubuntu/results/coverage/Engine/Engine_3_10.coverage", "ba4970a2-baac-4111-b339-21258cf1fcb9");
                throw xjexl.clean();
            }
        }
        writeline("/home/ubuntu/results/coverage/Engine/Engine_3_10.coverage", "a8a10676-c5a3-4c7d-b8d6-21d41120337d");
        return result;
    }

    @Override
    public <T> T newInstance(Class<? extends T> clazz, Object... args) {
        writeline("/home/ubuntu/results/coverage/Engine/Engine_3_10.coverage", "8468bf2f-70d9-44e2-aa7e-0dff6a7314aa");
        return clazz.cast(doCreateInstance(clazz, args));
    }

    @Override
    public Object newInstance(String clazz, Object... args) {
        writeline("/home/ubuntu/results/coverage/Engine/Engine_3_10.coverage", "47560a23-907d-4ab8-b36c-888e67473a4d");
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
        writeline("/home/ubuntu/results/coverage/Engine/Engine_3_10.coverage", "7c86a938-59a4-437f-a2fb-4c96e5980e12");
        JexlException xjexl = null;
        writeline("/home/ubuntu/results/coverage/Engine/Engine_3_10.coverage", "34839e65-06db-4099-ac11-7da5b75bb59e");
        Object result = null;
        writeline("/home/ubuntu/results/coverage/Engine/Engine_3_10.coverage", "4bae1ae8-e879-4885-b6ad-baa255b2a20f");
        final JexlInfo info = debug ? createInfo() : null;
        writeline("/home/ubuntu/results/coverage/Engine/Engine_3_10.coverage", "a07cc4de-e4b6-4444-b26e-4ffe9cfa9144");
        try {
            writeline("/home/ubuntu/results/coverage/Engine/Engine_3_10.coverage", "2b7a2263-b492-4a4f-8ffe-d35c80e92cf6");
            JexlMethod ctor = uberspect.getConstructor(clazz, args);
            writeline("/home/ubuntu/results/coverage/Engine/Engine_3_10.coverage", "a0dc4454-1a9f-470c-acc7-566d51766f99");
            if (ctor == null && arithmetic.narrowArguments(args)) {
                writeline("/home/ubuntu/results/coverage/Engine/Engine_3_10.coverage", "88b34c17-fa15-47d8-826b-bb94f75cc3f5");
                ctor = uberspect.getConstructor(clazz, args);
            }
            writeline("/home/ubuntu/results/coverage/Engine/Engine_3_10.coverage", "5235a05c-0b70-4a62-b202-ebf78867764d");
            if (ctor != null) {
                writeline("/home/ubuntu/results/coverage/Engine/Engine_3_10.coverage", "b9a8e1ff-0526-4250-9496-f241e38cead6");
                result = ctor.invoke(clazz, args);
            } else {
                writeline("/home/ubuntu/results/coverage/Engine/Engine_3_10.coverage", "98ac3673-9388-4c83-873b-de6404dc1a99");
                xjexl = new JexlException.Method(info, clazz.toString(), null);
            }
        } catch (JexlException xany) {
            writeline("/home/ubuntu/results/coverage/Engine/Engine_3_10.coverage", "c68893ff-db0f-41bd-a359-e7f5cf79f87f");
            xjexl = xany;
        } catch (Exception xany) {
            writeline("/home/ubuntu/results/coverage/Engine/Engine_3_10.coverage", "d35fbfd9-2683-4557-a46b-ab7b0c36e967");
            xjexl = new JexlException.Method(info, clazz.toString(), xany);
        }
        writeline("/home/ubuntu/results/coverage/Engine/Engine_3_10.coverage", "3ebebe8f-94c4-4f8c-bf1c-383dada52fd5");
        if (xjexl != null) {
            writeline("/home/ubuntu/results/coverage/Engine/Engine_3_10.coverage", "b3ef074c-7f5a-4b02-be2e-72b8f00eb86c");
            if (silent) {
                writeline("/home/ubuntu/results/coverage/Engine/Engine_3_10.coverage", "7caab4df-16ce-401c-8980-5e51b7d84aef");
                logger.warn(xjexl.getMessage(), xjexl.getCause());
                writeline("/home/ubuntu/results/coverage/Engine/Engine_3_10.coverage", "3f7c9d58-c4b1-41b7-bcb9-c51d85f6647c");
                return null;
            }
            writeline("/home/ubuntu/results/coverage/Engine/Engine_3_10.coverage", "78206e4e-3b1f-41e2-ba39-8f96e886a029");
            throw xjexl.clean();
        }
        writeline("/home/ubuntu/results/coverage/Engine/Engine_3_10.coverage", "b57f10f9-4b0b-4124-87ae-b843ab84a431");
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
        writeline("/home/ubuntu/results/coverage/Engine/Engine_3_10.coverage", "e13e3207-291b-4b29-9b00-20b08cca8216");
        VarCollector collector = new VarCollector();
        writeline("/home/ubuntu/results/coverage/Engine/Engine_3_10.coverage", "109497eb-7323-40e3-a054-a2fb9b543cf5");
        getVariables(script, script, collector);
        writeline("/home/ubuntu/results/coverage/Engine/Engine_3_10.coverage", "b1fb323c-6ef8-4ecc-9210-f447a838b1ce");
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
        writeline("/home/ubuntu/results/coverage/Engine/Engine_3_10.coverage", "9cb9528f-1e66-4ee8-a37a-17c40d79186e");
        if (node instanceof ASTIdentifier) {
            writeline("/home/ubuntu/results/coverage/Engine/Engine_3_10.coverage", "06307896-90ba-45b6-9625-1290aeef146e");
            JexlNode parent = node.jjtGetParent();
            writeline("/home/ubuntu/results/coverage/Engine/Engine_3_10.coverage", "23417b70-dc57-48ac-9017-fa1ee4e0ffa4");
            if (parent instanceof ASTMethodNode || parent instanceof ASTFunctionNode) {
                writeline("/home/ubuntu/results/coverage/Engine/Engine_3_10.coverage", "bdcb1953-7b84-4bee-8a24-3edd51f163bd");
                // skip identifiers for methods and functions
                collector.collect(null);
                writeline("/home/ubuntu/results/coverage/Engine/Engine_3_10.coverage", "b94451c1-8b9f-43d6-b642-ef48aca926a0");
                return;
            }
            writeline("/home/ubuntu/results/coverage/Engine/Engine_3_10.coverage", "756c4494-806a-4638-8c62-1552bf7f8745");
            ASTIdentifier identifier = (ASTIdentifier) node;
            writeline("/home/ubuntu/results/coverage/Engine/Engine_3_10.coverage", "66e5a988-e5a4-4c94-a8aa-e12ebc764fcc");
            int symbol = identifier.getSymbol();
            writeline("/home/ubuntu/results/coverage/Engine/Engine_3_10.coverage", "4861e7ea-4c5d-46bf-9744-dd653225e535");
            // symbols that are hoisted are considered "global" variables
            if (symbol >= 0 && script != null && !script.isHoistedSymbol(symbol)) {
                writeline("/home/ubuntu/results/coverage/Engine/Engine_3_10.coverage", "20a0cc5c-c69e-457f-b8a4-04c70862b668");
                collector.collect(null);
            } else {
                writeline("/home/ubuntu/results/coverage/Engine/Engine_3_10.coverage", "d093769b-1b87-47a7-bc59-d18ecbf2dba0");
                // start collecting from identifier
                collector.collect(identifier);
                writeline("/home/ubuntu/results/coverage/Engine/Engine_3_10.coverage", "2ef5da1f-4660-4f92-b475-4fd279fa7792");
                collector.add(identifier.getName());
            }
        } else if (node instanceof ASTIdentifierAccess) {
            writeline("/home/ubuntu/results/coverage/Engine/Engine_3_10.coverage", "44a3dcf1-859e-4e0e-b802-33136085cd1b");
            JexlNode parent = node.jjtGetParent();
            writeline("/home/ubuntu/results/coverage/Engine/Engine_3_10.coverage", "b92791ea-b9d3-4823-a0b2-849dbe7fadd8");
            if (parent instanceof ASTMethodNode || parent instanceof ASTFunctionNode) {
                writeline("/home/ubuntu/results/coverage/Engine/Engine_3_10.coverage", "4e4ff9c0-8f78-46bd-9433-526a701b51b7");
                // skip identifiers for methods and functions
                collector.collect(null);
                writeline("/home/ubuntu/results/coverage/Engine/Engine_3_10.coverage", "21af46cd-4dca-4f8a-9f72-de2ab769c989");
                return;
            }
            writeline("/home/ubuntu/results/coverage/Engine/Engine_3_10.coverage", "d88828bd-8592-4f64-bf83-917f05b59693");
            // belt and suspender since an identifier should have been seen first
            if (collector.isCollecting()) {
                writeline("/home/ubuntu/results/coverage/Engine/Engine_3_10.coverage", "8e0f3e32-bfe7-4af8-b1a9-5011e2c35264");
                collector.add(((ASTIdentifierAccess) node).getName());
            }
        } else if (node instanceof ASTArrayAccess) {
            writeline("/home/ubuntu/results/coverage/Engine/Engine_3_10.coverage", "12898452-cfc6-4107-9515-f313101d7b28");
            int num = node.jjtGetNumChildren();
            writeline("/home/ubuntu/results/coverage/Engine/Engine_3_10.coverage", "78715c3d-0be5-4d22-9cea-b80ad56cc5ea");
            // collect only if array access is const and follows an identifier
            boolean collecting = collector.isCollecting();
            writeline("/home/ubuntu/results/coverage/Engine/Engine_3_10.coverage", "290ec752-c33c-46a9-96d2-4b0efe298aaf");
            for (int i = 0; i < num; ++i) {
                writeline("/home/ubuntu/results/coverage/Engine/Engine_3_10.coverage", "33ec5199-194a-40c9-b4d7-4838bb5f3823");
                JexlNode child = node.jjtGetChild(i);
                writeline("/home/ubuntu/results/coverage/Engine/Engine_3_10.coverage", "bfb3c9db-61ff-48cf-a504-64eca49d4e5b");
                if (collecting && child.isConstant()) {
                    writeline("/home/ubuntu/results/coverage/Engine/Engine_3_10.coverage", "df872fe6-5b1b-45fe-acaf-9ae8177e292c");
                    String image = child.toString();
                    writeline("/home/ubuntu/results/coverage/Engine/Engine_3_10.coverage", "8f9dd70e-e969-427a-95eb-967e07f7cfad");
                    if (image == null) {
                        writeline("/home/ubuntu/results/coverage/Engine/Engine_3_10.coverage", "2d8e8e5c-e841-4be9-85c1-045829d8871f");
                        image = new Debugger().data(child);
                    }
                    writeline("/home/ubuntu/results/coverage/Engine/Engine_3_10.coverage", "54160751-163f-4164-82cb-ed0b0c3f51e9");
                    collector.add(image);
                } else {
                    writeline("/home/ubuntu/results/coverage/Engine/Engine_3_10.coverage", "9b4eed03-d273-4a9a-8192-a025df596f7a");
                    collecting = false;
                    writeline("/home/ubuntu/results/coverage/Engine/Engine_3_10.coverage", "e2d6186a-a9e0-46c2-a8c7-45aebfa56683");
                    collector.collect(null);
                    writeline("/home/ubuntu/results/coverage/Engine/Engine_3_10.coverage", "6157e3a6-8fa8-4b9c-ab92-e77d69cc0d93");
                    getVariables(script, child, collector);
                }
            }
        } else {
            writeline("/home/ubuntu/results/coverage/Engine/Engine_3_10.coverage", "64e7ee9d-3840-44fd-b9fd-00d62d433400");
            int num = node.jjtGetNumChildren();
            writeline("/home/ubuntu/results/coverage/Engine/Engine_3_10.coverage", "5971ca81-14ed-47b8-86e9-ceb4f79b2104");
            for (int i = 0; i < num; ++i) {
                writeline("/home/ubuntu/results/coverage/Engine/Engine_3_10.coverage", "c2338b41-142b-46a8-a5b9-da7f98f6845e");
                getVariables(script, node.jjtGetChild(i), collector);
            }
            writeline("/home/ubuntu/results/coverage/Engine/Engine_3_10.coverage", "e2f6a170-6c9f-43f8-b415-245df6ae3a7a");
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
        writeline("/home/ubuntu/results/coverage/Engine/Engine_3_10.coverage", "d4e1994b-8099-4f46-83ea-26960ee257e2");
        return script.getParameters();
    }

    /**
     * Gets the array of local variable from a script.
     * @param script the script
     * @return the local variables array which may be empty (but not null) if no local variables were defined
     * @since 3.0
     */
    protected String[] getLocalVariables(JexlScript script) {
        writeline("/home/ubuntu/results/coverage/Engine/Engine_3_10.coverage", "76ae5751-58b3-4647-8cd5-93e0f2dfd4f1");
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
        writeline("/home/ubuntu/results/coverage/Engine/Engine_3_10.coverage", "8edbeb23-a98f-4037-a170-32acd80e427d");
        final boolean cached = src.length() < cacheThreshold && cache != null;
        writeline("/home/ubuntu/results/coverage/Engine/Engine_3_10.coverage", "cf9f86dc-2023-48a9-8f46-532657e956ba");
        ASTJexlScript script;
        writeline("/home/ubuntu/results/coverage/Engine/Engine_3_10.coverage", "8235d137-1de9-4ed1-a3e0-c90cf62755a3");
        synchronized (parser) {
            writeline("/home/ubuntu/results/coverage/Engine/Engine_3_10.coverage", "377b820b-d435-4e43-8321-fb9717441389");
            if (cached) {
                writeline("/home/ubuntu/results/coverage/Engine/Engine_3_10.coverage", "0d26db53-c047-4434-a056-3283a5356923");
                script = cache.get(src);
                writeline("/home/ubuntu/results/coverage/Engine/Engine_3_10.coverage", "13b25341-0b1b-4c32-b60f-675009a446a3");
                if (script != null) {
                    writeline("/home/ubuntu/results/coverage/Engine/Engine_3_10.coverage", "d21d9492-2ebb-47d5-ad32-6b915d54f67d");
                    Scope f = script.getScope();
                    writeline("/home/ubuntu/results/coverage/Engine/Engine_3_10.coverage", "ac3759ed-838f-4b71-a316-05eccdbc189e");
                    if ((f == null && scope == null) || (f != null && f.equals(scope))) {
                        writeline("/home/ubuntu/results/coverage/Engine/Engine_3_10.coverage", "a70d8f7b-6317-4cd5-b82b-7943d7740859");
                        return script;
                    }
                }
            }
            writeline("/home/ubuntu/results/coverage/Engine/Engine_3_10.coverage", "1159c58a-9cdf-4e46-afd7-3b3a0259000b");
            script = parser.parse(info, src, scope, registers, expression);
            writeline("/home/ubuntu/results/coverage/Engine/Engine_3_10.coverage", "b7ed807a-7a87-4cb7-b03d-143d9b4eb9a7");
            if (cached) {
                writeline("/home/ubuntu/results/coverage/Engine/Engine_3_10.coverage", "15297cb0-e7ef-4344-963f-5a324eb54c0d");
                cache.put(src, script);
            }
        }
        writeline("/home/ubuntu/results/coverage/Engine/Engine_3_10.coverage", "1a4326fc-68ef-4f9d-a1e5-ee036b978d69");
        return script;
    }

    /**
     * Trims the source from front and ending spaces.
     * @param str expression to clean
     * @return trimmed expression ending in a semi-colon
     */
    protected String trimSource(CharSequence str) {
        writeline("/home/ubuntu/results/coverage/Engine/Engine_3_10.coverage", "f0c54eba-55bb-4030-a5ea-cf8ff42128ac");
        if (str != null) {
            writeline("/home/ubuntu/results/coverage/Engine/Engine_3_10.coverage", "23966960-2197-488e-9731-df6ba963afcd");
            int start = 0;
            writeline("/home/ubuntu/results/coverage/Engine/Engine_3_10.coverage", "e2dca911-a55c-44d7-98fa-6dc8283afb93");
            int end = str.length();
            writeline("/home/ubuntu/results/coverage/Engine/Engine_3_10.coverage", "624588cd-20eb-4ade-a696-99182d0fb646");
            if (end > 0) {
                writeline("/home/ubuntu/results/coverage/Engine/Engine_3_10.coverage", "61e42747-052c-4219-a05b-fd1ba503d8d9");
                // trim front spaces
                while (start < end && Character.isSpaceChar(str.charAt(start))) {
                    writeline("/home/ubuntu/results/coverage/Engine/Engine_3_10.coverage", "177b4d9b-b01b-4210-8211-36737bba987a");
                    ++start;
                }
                writeline("/home/ubuntu/results/coverage/Engine/Engine_3_10.coverage", "bc183a26-1cc1-41eb-8f9b-2c3f299abd2b");
                // trim ending spaces
                while (end > 0 && Character.isSpaceChar(str.charAt(end - 1))) {
                    writeline("/home/ubuntu/results/coverage/Engine/Engine_3_10.coverage", "7db09a96-3fae-478f-9fe2-32b9f482d814");
                    --end;
                }
                writeline("/home/ubuntu/results/coverage/Engine/Engine_3_10.coverage", "194f7b79-d4d0-447b-9c20-3e098770fac2");
                return str.subSequence(start, end).toString();
            }
            writeline("/home/ubuntu/results/coverage/Engine/Engine_3_10.coverage", "5a8f2e2a-868e-4552-a79c-56e6b66b6080");
            return "";
        }
        writeline("/home/ubuntu/results/coverage/Engine/Engine_3_10.coverage", "aa148ea6-4678-4ba9-9abc-668c26fc393e");
        return null;
    }

    /**
     * Gets and/or creates a default template engine.
     * @return a template engine
     */
    protected TemplateEngine jxlt() {
        writeline("/home/ubuntu/results/coverage/Engine/Engine_3_10.coverage", "8ff5c748-6a32-4934-a7fd-dedec71f3a8a");
        TemplateEngine e = jxlt;
        writeline("/home/ubuntu/results/coverage/Engine/Engine_3_10.coverage", "1699dbef-61b3-4275-9b20-9177e5e3d6d8");
        if (e == null) {
            writeline("/home/ubuntu/results/coverage/Engine/Engine_3_10.coverage", "20f58354-33c1-4971-8431-00e78a39276a");
            synchronized (this) {
                writeline("/home/ubuntu/results/coverage/Engine/Engine_3_10.coverage", "d7f8ba10-c576-49da-9e8e-7054142cfef4");
                if (jxlt == null) {
                    writeline("/home/ubuntu/results/coverage/Engine/Engine_3_10.coverage", "359a9486-00a0-478f-8d37-98c30d3aaa13");
                    e = new TemplateEngine(this, true, 0, '$', '#');
                    writeline("/home/ubuntu/results/coverage/Engine/Engine_3_10.coverage", "a7c0164f-544f-4989-902a-dd4736b209af");
                    jxlt = e;
                }
            }
        }
        writeline("/home/ubuntu/results/coverage/Engine/Engine_3_10.coverage", "353b3252-4087-409d-ac21-764c464f6c02");
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
