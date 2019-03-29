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
        writelineStatic("/home/ubuntu/results/coverage/Engine/Engine_10_10.coverage", "d5d74d80-5af0-447a-bf11-cf592e7cd9e7");
        if ((logger == null || logger.equals(LogFactory.getLog(JexlEngine.class))) && (strategy == null || strategy == JexlUberspect.JEXL_STRATEGY)) {
            writelineStatic("/home/ubuntu/results/coverage/Engine/Engine_10_10.coverage", "b87d4c1e-e0bc-4c59-b8c3-c2b101bd3c16");
            return UberspectHolder.UBERSPECT;
        }
        writelineStatic("/home/ubuntu/results/coverage/Engine/Engine_10_10.coverage", "94b496a9-8fb4-4481-bbc3-c66d34ec7563");
        return new Uberspect(logger, strategy);
    }

    @Override
    public JexlUberspect getUberspect() {
        writeline("/home/ubuntu/results/coverage/Engine/Engine_10_10.coverage", "8e1a5c58-fa3e-4915-81ca-98a622b0d7ef");
        return uberspect;
    }

    @Override
    public JexlArithmetic getArithmetic() {
        writeline("/home/ubuntu/results/coverage/Engine/Engine_10_10.coverage", "71fc6281-6dce-42f7-a1f0-207d45caaefa");
        return arithmetic;
    }

    @Override
    public boolean isDebug() {
        writeline("/home/ubuntu/results/coverage/Engine/Engine_10_10.coverage", "579f4812-58b1-4426-ab40-024ed1ce1039");
        return this.debug;
    }

    @Override
    public boolean isSilent() {
        writeline("/home/ubuntu/results/coverage/Engine/Engine_10_10.coverage", "85f50acf-1763-41c6-b649-015a24e24ae0");
        return this.silent;
    }

    @Override
    public boolean isStrict() {
        writeline("/home/ubuntu/results/coverage/Engine/Engine_10_10.coverage", "ae6f2f7f-3702-4a65-acac-c6dc07824492");
        return strict;
    }

    @Override
    public void setClassLoader(ClassLoader loader) {
        writeline("/home/ubuntu/results/coverage/Engine/Engine_10_10.coverage", "c63a6674-7649-4a17-8b8e-fa645501e245");
        uberspect.setClassLoader(loader);
    }

    @Override
    public Charset getCharset() {
        writeline("/home/ubuntu/results/coverage/Engine/Engine_10_10.coverage", "28d7738d-c75e-4435-980c-4aeb3daf1364");
        return charset;
    }

    @Override
    public TemplateEngine createJxltEngine(boolean noScript, int cacheSize, char immediate, char deferred) {
        writeline("/home/ubuntu/results/coverage/Engine/Engine_10_10.coverage", "c9752bc1-fd19-425d-80de-11fa88e74187");
        return new TemplateEngine(this, noScript, cacheSize, immediate, deferred);
    }

    /**
     * Swaps the current thread local context.
     * @param tls the context or null
     * @return the previous thread local context
     */
    protected JexlContext.ThreadLocal putThreadLocal(JexlContext.ThreadLocal tls) {
        writeline("/home/ubuntu/results/coverage/Engine/Engine_10_10.coverage", "89b5662e-4202-4b45-b851-7c5f8023a74f");
        JexlContext.ThreadLocal local = CONTEXT.get();
        writeline("/home/ubuntu/results/coverage/Engine/Engine_10_10.coverage", "bdac8e67-c192-4b76-b0dd-8d48ed721ad9");
        CONTEXT.set(tls);
        writeline("/home/ubuntu/results/coverage/Engine/Engine_10_10.coverage", "2b0654f7-11b5-437c-b7ad-6d95e55c4e21");
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
        writeline("/home/ubuntu/results/coverage/Engine/Engine_10_10.coverage", "a895102a-4b26-4ed1-9886-1406e4fe3ff4");
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
        writeline("/home/ubuntu/results/coverage/Engine/Engine_10_10.coverage", "6f454b13-7d9b-47b2-b63d-f525cec89e49");
        synchronized (parser) {
            writeline("/home/ubuntu/results/coverage/Engine/Engine_10_10.coverage", "5bb45791-5086-4c06-b371-7cfd212628b0");
            if (cache != null) {
                writeline("/home/ubuntu/results/coverage/Engine/Engine_10_10.coverage", "096659ff-8555-467d-9d9a-a77275f1737d");
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
        writeline("/home/ubuntu/results/coverage/Engine/Engine_10_10.coverage", "608aa69f-51aa-4286-b674-5c81f8be7165");
        return new Interpreter(this, context, frame);
    }

    @Override
    public Script createScript(JexlInfo info, String scriptText, String[] names) {
        writeline("/home/ubuntu/results/coverage/Engine/Engine_10_10.coverage", "f9465d3e-0e23-4e11-8210-034edb0ed9c4");
        if (scriptText == null) {
            writeline("/home/ubuntu/results/coverage/Engine/Engine_10_10.coverage", "29a219f5-bc59-4c46-97ff-614d9618962d");
            throw new NullPointerException("source is null");
        }
        writeline("/home/ubuntu/results/coverage/Engine/Engine_10_10.coverage", "3206658c-435f-4b5f-a28f-4a952e7639ec");
        if (info == null && debug) {
            writeline("/home/ubuntu/results/coverage/Engine/Engine_10_10.coverage", "f6a7f31c-d5ce-457e-b07c-ba0464278a13");
            info = createInfo();
        }
        writeline("/home/ubuntu/results/coverage/Engine/Engine_10_10.coverage", "9d28f270-5669-4b49-bd91-9691111b6fe1");
        String source = trimSource(scriptText);
        writeline("/home/ubuntu/results/coverage/Engine/Engine_10_10.coverage", "50539071-ab8a-4dad-baaf-3be311561385");
        Scope scope = names == null ? null : new Scope(null, names);
        writeline("/home/ubuntu/results/coverage/Engine/Engine_10_10.coverage", "9a2765a0-f4d4-44f9-bd58-6edaeb2f98f8");
        ASTJexlScript tree = parse(info, source, scope, false, false);
        writeline("/home/ubuntu/results/coverage/Engine/Engine_10_10.coverage", "d31a790c-49a2-42a1-bafd-652871125e04");
        return new Script(this, source, tree);
    }

    @Override
    public Script createExpression(JexlInfo info, String expression) {
        writeline("/home/ubuntu/results/coverage/Engine/Engine_10_10.coverage", "bde1bf96-a262-4d85-86ad-3f19b93410a6");
        if (expression == null) {
            writeline("/home/ubuntu/results/coverage/Engine/Engine_10_10.coverage", "9d8c14ae-82e1-470d-be3e-46a9c0f1ff35");
            throw new NullPointerException("source is null");
        }
        writeline("/home/ubuntu/results/coverage/Engine/Engine_10_10.coverage", "5c9a7681-18ce-4fc7-a65b-66a1739c4906");
        if (info == null && debug) {
            writeline("/home/ubuntu/results/coverage/Engine/Engine_10_10.coverage", "bead4398-4d80-4d61-8b72-c7cf1d20fd0e");
            info = createInfo();
        }
        writeline("/home/ubuntu/results/coverage/Engine/Engine_10_10.coverage", "78f3bb0a-2f89-4482-b472-fafdb35f7dfa");
        String source = trimSource(expression);
        writeline("/home/ubuntu/results/coverage/Engine/Engine_10_10.coverage", "0ca43da3-b410-4f18-abe1-8b7182d749d0");
        ASTJexlScript tree = parse(info, source, null, false, true);
        writeline("/home/ubuntu/results/coverage/Engine/Engine_10_10.coverage", "ca213aef-9d1a-4a65-bf35-dfa42c26ac0f");
        return new Script(this, source, tree);
    }

    @Override
    public Object getProperty(Object bean, String expr) {
        writeline("/home/ubuntu/results/coverage/Engine/Engine_10_10.coverage", "00683575-75d2-4da7-89f4-61c5cd129fe1");
        return getProperty(null, bean, expr);
    }

    @Override
    public Object getProperty(JexlContext context, Object bean, String expr) {
        writeline("/home/ubuntu/results/coverage/Engine/Engine_10_10.coverage", "9cb81e34-fbd7-4d42-a59f-9ad315527e80");
        if (context == null) {
            writeline("/home/ubuntu/results/coverage/Engine/Engine_10_10.coverage", "e0c6882c-f2e2-41c1-a6f9-c803ef118076");
            context = EMPTY_CONTEXT;
        }
        writeline("/home/ubuntu/results/coverage/Engine/Engine_10_10.coverage", "ebc531cc-ae1a-46ea-b86a-babac727ff11");
        // synthetize expr using register
        String src = trimSource(expr);
        writeline("/home/ubuntu/results/coverage/Engine/Engine_10_10.coverage", "dca7fc16-8618-4414-ab2a-95752884f4e9");
        src = "#0" + (src.charAt(0) == '[' ? "" : ".") + src;
        writeline("/home/ubuntu/results/coverage/Engine/Engine_10_10.coverage", "5e2aee33-5c4d-4511-86ca-687f50d459aa");
        try {
            writeline("/home/ubuntu/results/coverage/Engine/Engine_10_10.coverage", "3193aa70-3308-4483-b4bf-2430868c8dc6");
            final JexlInfo info = debug ? createInfo() : null;
            writeline("/home/ubuntu/results/coverage/Engine/Engine_10_10.coverage", "439f5e2b-6309-4cc9-a6b8-c47e0826f1c9");
            final Scope scope = new Scope(null, "#0");
            writeline("/home/ubuntu/results/coverage/Engine/Engine_10_10.coverage", "16141625-00e3-40c9-8494-bc3f488aa66e");
            final ASTJexlScript script = parse(info, src, scope, true, true);
            writeline("/home/ubuntu/results/coverage/Engine/Engine_10_10.coverage", "304dbd3d-6140-4adf-a9b3-64a0dfe85aeb");
            final JexlNode node = script.jjtGetChild(0);
            writeline("/home/ubuntu/results/coverage/Engine/Engine_10_10.coverage", "287729b9-20e4-491e-afda-b0ce8c2c569b");
            final Scope.Frame frame = script.createFrame(bean);
            writeline("/home/ubuntu/results/coverage/Engine/Engine_10_10.coverage", "99ba75c0-3c94-4fa9-a0c8-d8ba184b3d4b");
            final Interpreter interpreter = createInterpreter(context, frame);
            writeline("/home/ubuntu/results/coverage/Engine/Engine_10_10.coverage", "a520cdf9-9fcb-4737-b86c-effdc9faac67");
            return node.jjtAccept(interpreter, null);
        } catch (JexlException xjexl) {
            writeline("/home/ubuntu/results/coverage/Engine/Engine_10_10.coverage", "1c19e4a0-54e7-48ca-81b8-cdd876374a73");
            if (silent) {
                writeline("/home/ubuntu/results/coverage/Engine/Engine_10_10.coverage", "93ead809-304c-45da-a49a-ae2e9921defb");
                logger.warn(xjexl.getMessage(), xjexl.getCause());
                writeline("/home/ubuntu/results/coverage/Engine/Engine_10_10.coverage", "a7f9bd87-21ff-47a9-a300-fab1cec97495");
                return null;
            }
            writeline("/home/ubuntu/results/coverage/Engine/Engine_10_10.coverage", "120d2dbc-15b8-4adb-9ea0-aca4c6056848");
            throw xjexl.clean();
        }
    }

    @Override
    public void setProperty(Object bean, String expr, Object value) {
        writeline("/home/ubuntu/results/coverage/Engine/Engine_10_10.coverage", "1046727e-0ec2-412a-99e6-f8b18b95186f");
        setProperty(null, bean, expr, value);
    }

    @Override
    public void setProperty(JexlContext context, Object bean, String expr, Object value) {
        writeline("/home/ubuntu/results/coverage/Engine/Engine_10_10.coverage", "6577abba-c91b-4b74-97fe-ad4954e490f2");
        if (context == null) {
            writeline("/home/ubuntu/results/coverage/Engine/Engine_10_10.coverage", "1ac87a0d-9c1f-4ad1-a4de-ea58c0e8eec5");
            context = EMPTY_CONTEXT;
        }
        writeline("/home/ubuntu/results/coverage/Engine/Engine_10_10.coverage", "f0b90792-2e6b-4348-9514-07f37be92be3");
        // synthetize expr using registers
        String src = trimSource(expr);
        writeline("/home/ubuntu/results/coverage/Engine/Engine_10_10.coverage", "22fdfa05-2e9f-414b-9e31-53d36b8476cf");
        src = "#0" + (src.charAt(0) == '[' ? "" : ".") + src + "=" + "#1";
        writeline("/home/ubuntu/results/coverage/Engine/Engine_10_10.coverage", "4483f406-f004-4c69-9d0d-29bb398c228f");
        try {
            writeline("/home/ubuntu/results/coverage/Engine/Engine_10_10.coverage", "c55f6699-1773-453f-953f-9186709d7cf4");
            final JexlInfo info = debug ? createInfo() : null;
            writeline("/home/ubuntu/results/coverage/Engine/Engine_10_10.coverage", "290fc5fb-f41b-43e5-9539-e3d3c4f49a5f");
            final Scope scope = new Scope(null, "#0", "#1");
            writeline("/home/ubuntu/results/coverage/Engine/Engine_10_10.coverage", "f1d170df-a2a8-40a1-a33e-387fbe3d4842");
            final ASTJexlScript script = parse(info, src, scope, true, true);
            writeline("/home/ubuntu/results/coverage/Engine/Engine_10_10.coverage", "af16be99-0a27-44d1-aedb-88e5dc18dfe8");
            final JexlNode node = script.jjtGetChild(0);
            writeline("/home/ubuntu/results/coverage/Engine/Engine_10_10.coverage", "20452c50-3071-41d0-89cf-5886f7758a12");
            final Scope.Frame frame = script.createFrame(bean, value);
            writeline("/home/ubuntu/results/coverage/Engine/Engine_10_10.coverage", "bb53b70a-a671-4da0-a2f6-cdbb4c646654");
            final Interpreter interpreter = createInterpreter(context, frame);
            writeline("/home/ubuntu/results/coverage/Engine/Engine_10_10.coverage", "922c161c-ff44-4718-b639-20d3bd362dba");
            node.jjtAccept(interpreter, null);
        } catch (JexlException xjexl) {
            writeline("/home/ubuntu/results/coverage/Engine/Engine_10_10.coverage", "8d68fd92-1831-477b-8d78-be609d3dd1c5");
            if (silent) {
                writeline("/home/ubuntu/results/coverage/Engine/Engine_10_10.coverage", "3efd60ad-b988-4e04-ba1a-c139f92096c4");
                logger.warn(xjexl.getMessage(), xjexl.getCause());
                writeline("/home/ubuntu/results/coverage/Engine/Engine_10_10.coverage", "043665c5-0eb7-4c30-815b-aa697d81fb6f");
                return;
            }
            writeline("/home/ubuntu/results/coverage/Engine/Engine_10_10.coverage", "59ba0c76-a67e-49fe-8e67-5a9e08b8d4c7");
            throw xjexl.clean();
        }
    }

    @Override
    public Object invokeMethod(Object obj, String meth, Object... args) {
        writeline("/home/ubuntu/results/coverage/Engine/Engine_10_10.coverage", "d910c4ce-9567-4928-8912-66145a906751");
        JexlException xjexl = null;
        writeline("/home/ubuntu/results/coverage/Engine/Engine_10_10.coverage", "78b45eff-6f98-4ff8-8e5d-86be8e7ddf71");
        Object result = null;
        writeline("/home/ubuntu/results/coverage/Engine/Engine_10_10.coverage", "8e63bc0a-9a18-4d8b-a307-4dfb92af0e01");
        final JexlInfo info = debug ? createInfo() : null;
        writeline("/home/ubuntu/results/coverage/Engine/Engine_10_10.coverage", "6ea0e9ea-ec81-42d5-91f6-8eaabba654a1");
        try {
            writeline("/home/ubuntu/results/coverage/Engine/Engine_10_10.coverage", "60fadbfe-69a8-4b18-ade9-85e0157ff681");
            JexlMethod method = uberspect.getMethod(obj, meth, args);
            writeline("/home/ubuntu/results/coverage/Engine/Engine_10_10.coverage", "1788240d-47d9-4c3d-987c-d9a2b6ef28cd");
            if (method == null && arithmetic.narrowArguments(args)) {
                writeline("/home/ubuntu/results/coverage/Engine/Engine_10_10.coverage", "53f1bf13-3d9e-4d54-acd8-74ac3f66999b");
                method = uberspect.getMethod(obj, meth, args);
            }
            writeline("/home/ubuntu/results/coverage/Engine/Engine_10_10.coverage", "7c50b5df-6a20-4ea2-9ab3-fb763c12d094");
            if (method != null) {
                writeline("/home/ubuntu/results/coverage/Engine/Engine_10_10.coverage", "4614432a-1e11-45af-84b4-81e6d7cc81ce");
                result = method.invoke(obj, args);
            } else {
                writeline("/home/ubuntu/results/coverage/Engine/Engine_10_10.coverage", "5381613f-e8cb-478d-bf4b-027bbc15658d");
                xjexl = new JexlException.Method(info, meth, null);
            }
        } catch (JexlException xany) {
            writeline("/home/ubuntu/results/coverage/Engine/Engine_10_10.coverage", "950386d8-cfb8-437c-8fbf-78fc502583aa");
            xjexl = xany;
        } catch (Exception xany) {
            writeline("/home/ubuntu/results/coverage/Engine/Engine_10_10.coverage", "6d6a4676-d05a-4c32-b526-11642f644cb2");
            xjexl = new JexlException.Method(info, meth, xany);
        }
        writeline("/home/ubuntu/results/coverage/Engine/Engine_10_10.coverage", "b9a187d6-974a-45ae-a05b-4f8c81eb603a");
        if (xjexl != null) {
            writeline("/home/ubuntu/results/coverage/Engine/Engine_10_10.coverage", "13c42cfd-20ea-4224-8b17-044db2d13449");
            if (silent) {
                writeline("/home/ubuntu/results/coverage/Engine/Engine_10_10.coverage", "7473228c-2334-4b50-bc1e-f70c08fae092");
                logger.warn(xjexl.getMessage(), xjexl.getCause());
                writeline("/home/ubuntu/results/coverage/Engine/Engine_10_10.coverage", "08d820a7-60d1-4c47-b321-c98b7618c161");
                result = null;
            } else {
                writeline("/home/ubuntu/results/coverage/Engine/Engine_10_10.coverage", "623061db-25dc-49b2-bad9-8cef1362b99e");
                throw xjexl.clean();
            }
        }
        writeline("/home/ubuntu/results/coverage/Engine/Engine_10_10.coverage", "3e881907-6b92-42ef-b112-c0275154c613");
        return result;
    }

    @Override
    public <T> T newInstance(Class<? extends T> clazz, Object... args) {
        writeline("/home/ubuntu/results/coverage/Engine/Engine_10_10.coverage", "f17e29a2-6fff-4659-a118-e9a37c9e0907");
        return clazz.cast(doCreateInstance(clazz, args));
    }

    @Override
    public Object newInstance(String clazz, Object... args) {
        writeline("/home/ubuntu/results/coverage/Engine/Engine_10_10.coverage", "8edd68a9-570b-4e99-9041-81fdd0bc3d12");
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
        writeline("/home/ubuntu/results/coverage/Engine/Engine_10_10.coverage", "fc6ab479-3c23-46be-a6a8-0106570ce9eb");
        JexlException xjexl = null;
        writeline("/home/ubuntu/results/coverage/Engine/Engine_10_10.coverage", "4f2f6394-efe9-474f-90f5-d3ebfb27f33d");
        Object result = null;
        writeline("/home/ubuntu/results/coverage/Engine/Engine_10_10.coverage", "6bf8dd74-a426-473a-9b96-0d158c79e6d0");
        final JexlInfo info = debug ? createInfo() : null;
        writeline("/home/ubuntu/results/coverage/Engine/Engine_10_10.coverage", "0684cc6b-5428-4774-b155-6095ef304fcd");
        try {
            writeline("/home/ubuntu/results/coverage/Engine/Engine_10_10.coverage", "d348cdf9-8fde-4183-8f7e-1982af500943");
            JexlMethod ctor = uberspect.getConstructor(clazz, args);
            writeline("/home/ubuntu/results/coverage/Engine/Engine_10_10.coverage", "4dba534a-fac4-44cd-ae45-f7c12f5bada4");
            if (ctor == null && arithmetic.narrowArguments(args)) {
                writeline("/home/ubuntu/results/coverage/Engine/Engine_10_10.coverage", "28e57d6d-b2f9-40fd-8b3d-dc2c8d513003");
                ctor = uberspect.getConstructor(clazz, args);
            }
            writeline("/home/ubuntu/results/coverage/Engine/Engine_10_10.coverage", "41d8c6de-d980-4f92-abaf-495a33c206d1");
            if (ctor != null) {
                writeline("/home/ubuntu/results/coverage/Engine/Engine_10_10.coverage", "8ce590fc-50a2-4176-94f7-360d312878ee");
                result = ctor.invoke(clazz, args);
            } else {
                writeline("/home/ubuntu/results/coverage/Engine/Engine_10_10.coverage", "35df015b-b42d-4656-a54b-1cea5604dca9");
                xjexl = new JexlException.Method(info, clazz.toString(), null);
            }
        } catch (JexlException xany) {
            writeline("/home/ubuntu/results/coverage/Engine/Engine_10_10.coverage", "5012b627-84a5-4570-8a0d-e6c46bcd09b2");
            xjexl = xany;
        } catch (Exception xany) {
            writeline("/home/ubuntu/results/coverage/Engine/Engine_10_10.coverage", "15ddff96-3e02-4fcf-95d8-046934093532");
            xjexl = new JexlException.Method(info, clazz.toString(), xany);
        }
        writeline("/home/ubuntu/results/coverage/Engine/Engine_10_10.coverage", "1da5aa04-36a7-4c10-a4cb-441f20d39fc4");
        if (xjexl != null) {
            writeline("/home/ubuntu/results/coverage/Engine/Engine_10_10.coverage", "f4c1a3c1-e564-499d-b892-464c234325e5");
            if (silent) {
                writeline("/home/ubuntu/results/coverage/Engine/Engine_10_10.coverage", "54870d6d-751b-46cb-9563-811b2dd2dc43");
                logger.warn(xjexl.getMessage(), xjexl.getCause());
                writeline("/home/ubuntu/results/coverage/Engine/Engine_10_10.coverage", "7b08b20d-a675-4fdf-96f0-c0c8e055f8a0");
                return null;
            }
            writeline("/home/ubuntu/results/coverage/Engine/Engine_10_10.coverage", "fb044965-9ef1-46b1-8954-9a80c0a5fe77");
            throw xjexl.clean();
        }
        writeline("/home/ubuntu/results/coverage/Engine/Engine_10_10.coverage", "221ead56-1306-494a-b82b-782d5f9c2a1d");
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
        writeline("/home/ubuntu/results/coverage/Engine/Engine_10_10.coverage", "4c5ca466-af58-417a-90db-e2eac4610cdc");
        VarCollector collector = new VarCollector();
        writeline("/home/ubuntu/results/coverage/Engine/Engine_10_10.coverage", "7f112971-2cbe-4351-a359-70ede21cbdc3");
        getVariables(script, script, collector);
        writeline("/home/ubuntu/results/coverage/Engine/Engine_10_10.coverage", "ad617116-5d52-4102-bf83-a4988676d4fe");
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
        writeline("/home/ubuntu/results/coverage/Engine/Engine_10_10.coverage", "3bf8e8fe-c3b3-4c8f-bbee-17ab560cf854");
        if (node instanceof ASTIdentifier) {
            writeline("/home/ubuntu/results/coverage/Engine/Engine_10_10.coverage", "0f2fc141-2f27-4907-ac47-5cce7dd87844");
            JexlNode parent = node.jjtGetParent();
            writeline("/home/ubuntu/results/coverage/Engine/Engine_10_10.coverage", "2269e4df-0393-4b92-a3c2-eb2369198b96");
            if (parent instanceof ASTMethodNode || parent instanceof ASTFunctionNode) {
                writeline("/home/ubuntu/results/coverage/Engine/Engine_10_10.coverage", "755a798c-f27a-4adf-be83-d753960de7da");
                // skip identifiers for methods and functions
                collector.collect(null);
                writeline("/home/ubuntu/results/coverage/Engine/Engine_10_10.coverage", "eebf20ac-47e5-4af2-a23f-8228536753cf");
                return;
            }
            writeline("/home/ubuntu/results/coverage/Engine/Engine_10_10.coverage", "20d8b548-93d7-44ce-8fbf-dfe2f884abb2");
            ASTIdentifier identifier = (ASTIdentifier) node;
            writeline("/home/ubuntu/results/coverage/Engine/Engine_10_10.coverage", "6ba9078b-5331-491c-b2d0-596551fd7ae7");
            int symbol = identifier.getSymbol();
            writeline("/home/ubuntu/results/coverage/Engine/Engine_10_10.coverage", "534c8fef-2645-46b9-8515-17df24caebf3");
            // symbols that are hoisted are considered "global" variables
            if (symbol >= 0 && script != null && !script.isHoistedSymbol(symbol)) {
                writeline("/home/ubuntu/results/coverage/Engine/Engine_10_10.coverage", "b53efb89-de01-424c-ae77-e6c71e2217a8");
                collector.collect(null);
            } else {
                writeline("/home/ubuntu/results/coverage/Engine/Engine_10_10.coverage", "1a53daa7-9f1f-468e-b972-2aae335c8787");
                // start collecting from identifier
                collector.collect(identifier);
                writeline("/home/ubuntu/results/coverage/Engine/Engine_10_10.coverage", "00720c79-0104-4eec-8d3d-c7b1ca385c4b");
                collector.add(identifier.getName());
            }
        } else if (node instanceof ASTIdentifierAccess) {
            writeline("/home/ubuntu/results/coverage/Engine/Engine_10_10.coverage", "671525a9-a078-485b-8dcf-1e21ef8b1d28");
            JexlNode parent = node.jjtGetParent();
            writeline("/home/ubuntu/results/coverage/Engine/Engine_10_10.coverage", "3e158359-430d-4657-a62e-a51338c26ce6");
            if (parent instanceof ASTMethodNode || parent instanceof ASTFunctionNode) {
                writeline("/home/ubuntu/results/coverage/Engine/Engine_10_10.coverage", "13da4f02-10ee-45a8-83f6-2ba7b490cea8");
                // skip identifiers for methods and functions
                collector.collect(null);
                writeline("/home/ubuntu/results/coverage/Engine/Engine_10_10.coverage", "131015fd-e5e6-4bba-9234-24ed9023bcf1");
                return;
            }
            writeline("/home/ubuntu/results/coverage/Engine/Engine_10_10.coverage", "d7e7f910-e769-4abc-9e04-cdb643552076");
            // belt and suspender since an identifier should have been seen first
            if (collector.isCollecting()) {
                writeline("/home/ubuntu/results/coverage/Engine/Engine_10_10.coverage", "76ed3eb9-cfaa-4e8f-852a-9cad479b6b4b");
                collector.add(((ASTIdentifierAccess) node).getName());
            }
        } else if (node instanceof ASTArrayAccess) {
            writeline("/home/ubuntu/results/coverage/Engine/Engine_10_10.coverage", "f043b822-d865-4319-af3e-2561405d4293");
            int num = node.jjtGetNumChildren();
            writeline("/home/ubuntu/results/coverage/Engine/Engine_10_10.coverage", "d18767c0-2b6c-453e-91d1-8aa8dc985225");
            // collect only if array access is const and follows an identifier
            boolean collecting = collector.isCollecting();
            writeline("/home/ubuntu/results/coverage/Engine/Engine_10_10.coverage", "22b2f452-c4af-4488-a981-bfab34e5ffa1");
            for (int i = 0; i < num; ++i) {
                writeline("/home/ubuntu/results/coverage/Engine/Engine_10_10.coverage", "012df1b6-888e-49c0-bdf9-7acd8c2e3548");
                JexlNode child = node.jjtGetChild(i);
                writeline("/home/ubuntu/results/coverage/Engine/Engine_10_10.coverage", "1cee8d75-c4d2-4093-9b4d-bbc40866ede2");
                if (collecting && child.isConstant()) {
                    writeline("/home/ubuntu/results/coverage/Engine/Engine_10_10.coverage", "3ae64a7c-3bb7-497f-bebe-9d20b470f057");
                    String image = child.toString();
                    writeline("/home/ubuntu/results/coverage/Engine/Engine_10_10.coverage", "1764ffdf-f1da-4eb4-922e-d932d3e9cd0a");
                    if (image == null) {
                        writeline("/home/ubuntu/results/coverage/Engine/Engine_10_10.coverage", "9fd5a88d-e742-4e11-8b23-553f89d3c3bd");
                        image = new Debugger().data(child);
                    }
                    writeline("/home/ubuntu/results/coverage/Engine/Engine_10_10.coverage", "06dd53ba-e26a-4a4f-8a70-6f7b55d1c121");
                    collector.add(image);
                } else {
                    writeline("/home/ubuntu/results/coverage/Engine/Engine_10_10.coverage", "51241a6a-5301-4301-9baa-0d4723e9342e");
                    collecting = false;
                    writeline("/home/ubuntu/results/coverage/Engine/Engine_10_10.coverage", "71619d6b-0441-4bd8-8a03-00415bb713b8");
                    collector.collect(null);
                    writeline("/home/ubuntu/results/coverage/Engine/Engine_10_10.coverage", "51b7cfa5-5258-4848-b790-74a97dbc8eb2");
                    getVariables(script, child, collector);
                }
            }
        } else {
            writeline("/home/ubuntu/results/coverage/Engine/Engine_10_10.coverage", "77b2ff4f-8ff6-4658-b778-1da369579968");
            int num = node.jjtGetNumChildren();
            writeline("/home/ubuntu/results/coverage/Engine/Engine_10_10.coverage", "c25cb1e3-5294-4250-bae4-ff8eeaf1dcdf");
            for (int i = 0; i < num; ++i) {
                writeline("/home/ubuntu/results/coverage/Engine/Engine_10_10.coverage", "cbd28d92-7b39-4959-95e1-1cd3d037dc1b");
                getVariables(script, node.jjtGetChild(i), collector);
            }
            writeline("/home/ubuntu/results/coverage/Engine/Engine_10_10.coverage", "a83b52b6-047f-49ff-b0a1-8586ee40d541");
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
        writeline("/home/ubuntu/results/coverage/Engine/Engine_10_10.coverage", "a9cbd57c-6f9d-4a0f-a16d-1830a75efabf");
        return script.getParameters();
    }

    /**
     * Gets the array of local variable from a script.
     * @param script the script
     * @return the local variables array which may be empty (but not null) if no local variables were defined
     * @since 3.0
     */
    protected String[] getLocalVariables(JexlScript script) {
        writeline("/home/ubuntu/results/coverage/Engine/Engine_10_10.coverage", "719902cc-5e8c-4cd2-9ef8-ab26f3ee6273");
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
        writeline("/home/ubuntu/results/coverage/Engine/Engine_10_10.coverage", "296e4cac-a620-4b9a-8334-3ee5ccc0f1a5");
        final boolean cached = src.length() < cacheThreshold && cache != null;
        writeline("/home/ubuntu/results/coverage/Engine/Engine_10_10.coverage", "191462f4-8a0b-4d19-aece-57d246893e67");
        ASTJexlScript script;
        writeline("/home/ubuntu/results/coverage/Engine/Engine_10_10.coverage", "8e0e1991-a54a-4efd-ab85-5e2a6be5fc02");
        synchronized (parser) {
            writeline("/home/ubuntu/results/coverage/Engine/Engine_10_10.coverage", "18affd18-1ba8-4121-a4d6-fe6a5ba136d0");
            if (cached) {
                writeline("/home/ubuntu/results/coverage/Engine/Engine_10_10.coverage", "d44dc6d1-d758-481c-a192-30e5fed16065");
                script = cache.get(src);
                writeline("/home/ubuntu/results/coverage/Engine/Engine_10_10.coverage", "080ca15b-f190-48d2-b354-722f382a82b1");
                if (script != null) {
                    writeline("/home/ubuntu/results/coverage/Engine/Engine_10_10.coverage", "f8e3d388-2109-493c-8632-f584f3640ed4");
                    Scope f = script.getScope();
                    writeline("/home/ubuntu/results/coverage/Engine/Engine_10_10.coverage", "50b4630c-118f-4fdf-a681-543aca225eca");
                    if ((f == null && scope == null) || (f != null && f.equals(scope))) {
                        writeline("/home/ubuntu/results/coverage/Engine/Engine_10_10.coverage", "f7de0896-e6b9-408d-b9fe-fc5c00f178fe");
                        return script;
                    }
                }
            }
            writeline("/home/ubuntu/results/coverage/Engine/Engine_10_10.coverage", "1077ac5d-862b-4b6d-ad60-a716bce4ea6a");
            script = parser.parse(info, src, scope, registers, expression);
            writeline("/home/ubuntu/results/coverage/Engine/Engine_10_10.coverage", "c96341d6-9b6b-4a2e-b352-4cda564bb737");
            if (cached) {
                writeline("/home/ubuntu/results/coverage/Engine/Engine_10_10.coverage", "0e020a8e-75e0-4482-ad97-0f2a610f38b8");
                cache.put(src, script);
            }
        }
        writeline("/home/ubuntu/results/coverage/Engine/Engine_10_10.coverage", "56500c54-b821-4b1a-a354-86c7c5b82ffe");
        return script;
    }

    /**
     * Trims the source from front and ending spaces.
     * @param str expression to clean
     * @return trimmed expression ending in a semi-colon
     */
    protected String trimSource(CharSequence str) {
        writeline("/home/ubuntu/results/coverage/Engine/Engine_10_10.coverage", "412431a3-a0da-4125-b02a-ed05c828c480");
        if (str != null) {
            writeline("/home/ubuntu/results/coverage/Engine/Engine_10_10.coverage", "87636c54-2dbf-4aff-9fbd-e8162623ada7");
            int start = 0;
            writeline("/home/ubuntu/results/coverage/Engine/Engine_10_10.coverage", "cd777eac-3179-40e6-99e5-a69a40ab7249");
            int end = str.length();
            writeline("/home/ubuntu/results/coverage/Engine/Engine_10_10.coverage", "2f587fc5-2cfe-4157-bac4-7abf42b633d5");
            if (end > 0) {
                writeline("/home/ubuntu/results/coverage/Engine/Engine_10_10.coverage", "3a7b28c8-28b1-47af-986c-61294cb45722");
                // trim front spaces
                while (start < end && Character.isSpaceChar(str.charAt(start))) {
                    writeline("/home/ubuntu/results/coverage/Engine/Engine_10_10.coverage", "0c7b4b6d-928c-4c1f-99ba-af0e6deb6900");
                    ++start;
                }
                writeline("/home/ubuntu/results/coverage/Engine/Engine_10_10.coverage", "37985b18-14d4-4a6b-bd82-caeeb4a23077");
                // trim ending spaces
                while (end > 0 && Character.isSpaceChar(str.charAt(end - 1))) {
                    writeline("/home/ubuntu/results/coverage/Engine/Engine_10_10.coverage", "f38a450c-468c-48e2-8ae6-33219a0ff306");
                    --end;
                }
                writeline("/home/ubuntu/results/coverage/Engine/Engine_10_10.coverage", "84884142-aa93-453b-9276-e5b80dd26f4e");
                return str.subSequence(start, end).toString();
            }
            writeline("/home/ubuntu/results/coverage/Engine/Engine_10_10.coverage", "c7b384ac-48df-4e71-a06c-88cdbacb5526");
            return "";
        }
        writeline("/home/ubuntu/results/coverage/Engine/Engine_10_10.coverage", "da6d21d8-a696-4c20-9255-310d22e6be8e");
        return null;
    }

    /**
     * Gets and/or creates a default template engine.
     * @return a template engine
     */
    protected TemplateEngine jxlt() {
        writeline("/home/ubuntu/results/coverage/Engine/Engine_10_10.coverage", "354d21f7-fe88-4ce1-beeb-f8fdb7752489");
        TemplateEngine e = jxlt;
        writeline("/home/ubuntu/results/coverage/Engine/Engine_10_10.coverage", "b4685169-4d0e-40d7-b3ee-27cb307e3882");
        if (e == null) {
            writeline("/home/ubuntu/results/coverage/Engine/Engine_10_10.coverage", "a55aeb6d-c9aa-4026-8385-009524bd011a");
            synchronized (this) {
                writeline("/home/ubuntu/results/coverage/Engine/Engine_10_10.coverage", "98c63e67-2f39-4565-bce4-2259af197ca1");
                if (jxlt == null) {
                    writeline("/home/ubuntu/results/coverage/Engine/Engine_10_10.coverage", "0483a914-f8a6-46d5-95de-35bf56b7c63a");
                    e = new TemplateEngine(this, true, 0, '$', '#');
                    writeline("/home/ubuntu/results/coverage/Engine/Engine_10_10.coverage", "74675395-52f5-4b9c-a565-f2e945872ce3");
                    jxlt = e;
                }
            }
        }
        writeline("/home/ubuntu/results/coverage/Engine/Engine_10_10.coverage", "098fb9f8-4bdd-4159-99f2-65a061fb8752");
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
