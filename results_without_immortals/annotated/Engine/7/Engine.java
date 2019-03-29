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
        writelineStatic("/home/ubuntu/results/coverage/Engine/Engine_7_10.coverage", "ce383f04-72d6-482b-b83a-f2b3285dc454");
        if ((logger == null || logger.equals(LogFactory.getLog(JexlEngine.class))) && (strategy == null || strategy == JexlUberspect.JEXL_STRATEGY)) {
            writelineStatic("/home/ubuntu/results/coverage/Engine/Engine_7_10.coverage", "bdf42516-d5f8-471a-8b89-3852518f0b20");
            return UberspectHolder.UBERSPECT;
        }
        writelineStatic("/home/ubuntu/results/coverage/Engine/Engine_7_10.coverage", "c01b2cb6-2db6-4303-8af5-151e21538d8f");
        return new Uberspect(logger, strategy);
    }

    @Override
    public JexlUberspect getUberspect() {
        writeline("/home/ubuntu/results/coverage/Engine/Engine_7_10.coverage", "49fdf052-792e-4821-a430-ecaa995a8117");
        return uberspect;
    }

    @Override
    public JexlArithmetic getArithmetic() {
        writeline("/home/ubuntu/results/coverage/Engine/Engine_7_10.coverage", "4a046525-df21-496e-91a7-f912fecc6d91");
        return arithmetic;
    }

    @Override
    public boolean isDebug() {
        writeline("/home/ubuntu/results/coverage/Engine/Engine_7_10.coverage", "fc61ef43-de55-47c2-bf9a-2325b6f1a418");
        return this.debug;
    }

    @Override
    public boolean isSilent() {
        writeline("/home/ubuntu/results/coverage/Engine/Engine_7_10.coverage", "66329f9f-946c-45f8-8b2c-921645fd696c");
        return this.silent;
    }

    @Override
    public boolean isStrict() {
        writeline("/home/ubuntu/results/coverage/Engine/Engine_7_10.coverage", "b7f0395b-bc2f-4919-86d3-4b3a21739c82");
        return strict;
    }

    @Override
    public void setClassLoader(ClassLoader loader) {
        writeline("/home/ubuntu/results/coverage/Engine/Engine_7_10.coverage", "db3d813c-cfb0-4972-aafe-14b8fa12c566");
        uberspect.setClassLoader(loader);
    }

    @Override
    public Charset getCharset() {
        writeline("/home/ubuntu/results/coverage/Engine/Engine_7_10.coverage", "7e556c3c-026a-4c62-8371-83224e838c48");
        return charset;
    }

    @Override
    public TemplateEngine createJxltEngine(boolean noScript, int cacheSize, char immediate, char deferred) {
        writeline("/home/ubuntu/results/coverage/Engine/Engine_7_10.coverage", "0ee83afe-18ae-4063-b1f1-35c9d565e9c7");
        return new TemplateEngine(this, noScript, cacheSize, immediate, deferred);
    }

    /**
     * Swaps the current thread local context.
     * @param tls the context or null
     * @return the previous thread local context
     */
    protected JexlContext.ThreadLocal putThreadLocal(JexlContext.ThreadLocal tls) {
        writeline("/home/ubuntu/results/coverage/Engine/Engine_7_10.coverage", "b8c3a549-a2d1-4c3d-bed1-28805c7ef039");
        JexlContext.ThreadLocal local = CONTEXT.get();
        writeline("/home/ubuntu/results/coverage/Engine/Engine_7_10.coverage", "19fa23ac-4916-4ea1-9555-970bca41a8dc");
        CONTEXT.set(tls);
        writeline("/home/ubuntu/results/coverage/Engine/Engine_7_10.coverage", "efc4cc55-91f1-49fd-a8e3-f8c5ac09a93f");
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
        writeline("/home/ubuntu/results/coverage/Engine/Engine_7_10.coverage", "8d7e680f-1eff-4749-8b32-2658ed5601e0");
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
        writeline("/home/ubuntu/results/coverage/Engine/Engine_7_10.coverage", "ae6e316b-a987-4eba-bb9f-8be04e6c5d3d");
        synchronized (parser) {
            writeline("/home/ubuntu/results/coverage/Engine/Engine_7_10.coverage", "19c4f535-f35a-4bd3-99cf-31651526d619");
            if (cache != null) {
                writeline("/home/ubuntu/results/coverage/Engine/Engine_7_10.coverage", "30b3642d-2604-4aba-84a5-f8b97c7082b9");
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
        writeline("/home/ubuntu/results/coverage/Engine/Engine_7_10.coverage", "83cc14ba-f997-4e28-8a75-8d609069adaa");
        return new Interpreter(this, context, frame);
    }

    @Override
    public Script createScript(JexlInfo info, String scriptText, String[] names) {
        writeline("/home/ubuntu/results/coverage/Engine/Engine_7_10.coverage", "df98f7a7-a811-4412-b106-73224ef42d9d");
        if (scriptText == null) {
            writeline("/home/ubuntu/results/coverage/Engine/Engine_7_10.coverage", "ecc99e6e-5d22-400b-9227-541d095096e5");
            throw new NullPointerException("source is null");
        }
        writeline("/home/ubuntu/results/coverage/Engine/Engine_7_10.coverage", "ce0e989a-e6a8-457c-8aab-bca1198b65d8");
        if (info == null && debug) {
            writeline("/home/ubuntu/results/coverage/Engine/Engine_7_10.coverage", "851d726c-42f9-4b13-b745-9d2015d515bd");
            info = createInfo();
        }
        writeline("/home/ubuntu/results/coverage/Engine/Engine_7_10.coverage", "2509d7f4-6ad3-4b1b-b5f1-1adbe4e2a9af");
        String source = trimSource(scriptText);
        writeline("/home/ubuntu/results/coverage/Engine/Engine_7_10.coverage", "b5190261-d2c3-4b55-bd60-81b60374d9f5");
        Scope scope = names == null ? null : new Scope(null, names);
        writeline("/home/ubuntu/results/coverage/Engine/Engine_7_10.coverage", "17139089-69bf-4733-ad36-82f54044d4a8");
        ASTJexlScript tree = parse(info, source, scope, false, false);
        writeline("/home/ubuntu/results/coverage/Engine/Engine_7_10.coverage", "ea307d2d-b06f-4215-b778-ce881c3ff688");
        return new Script(this, source, tree);
    }

    @Override
    public Script createExpression(JexlInfo info, String expression) {
        writeline("/home/ubuntu/results/coverage/Engine/Engine_7_10.coverage", "cca00b6f-a86f-44d6-9611-00ae6bc7e4b1");
        if (expression == null) {
            writeline("/home/ubuntu/results/coverage/Engine/Engine_7_10.coverage", "69d1cc1c-4a5b-468f-9de1-e31b26bd8bd8");
            throw new NullPointerException("source is null");
        }
        writeline("/home/ubuntu/results/coverage/Engine/Engine_7_10.coverage", "cb0de749-5c4a-4372-9658-f2180fe04c51");
        if (info == null && debug) {
            writeline("/home/ubuntu/results/coverage/Engine/Engine_7_10.coverage", "33385909-46b4-4717-878e-7e0728701d0e");
            info = createInfo();
        }
        writeline("/home/ubuntu/results/coverage/Engine/Engine_7_10.coverage", "7a7531d9-4d2f-4395-8521-94a21c1b86cc");
        String source = trimSource(expression);
        writeline("/home/ubuntu/results/coverage/Engine/Engine_7_10.coverage", "d5b68a5d-a123-43bc-ade9-24ce1255e4f1");
        ASTJexlScript tree = parse(info, source, null, false, true);
        writeline("/home/ubuntu/results/coverage/Engine/Engine_7_10.coverage", "b276bf03-dc11-4bbc-bdf1-ef0c95ac69c5");
        return new Script(this, source, tree);
    }

    @Override
    public Object getProperty(Object bean, String expr) {
        writeline("/home/ubuntu/results/coverage/Engine/Engine_7_10.coverage", "bc52cd28-888e-4f52-bfbc-19cc57e3e772");
        return getProperty(null, bean, expr);
    }

    @Override
    public Object getProperty(JexlContext context, Object bean, String expr) {
        writeline("/home/ubuntu/results/coverage/Engine/Engine_7_10.coverage", "c5b31eae-b8cf-4901-9909-20aeeb65050f");
        if (context == null) {
            writeline("/home/ubuntu/results/coverage/Engine/Engine_7_10.coverage", "b89bbe7c-83a4-4dcf-9540-3c50f50e44a0");
            context = EMPTY_CONTEXT;
        }
        writeline("/home/ubuntu/results/coverage/Engine/Engine_7_10.coverage", "e28cc64a-46ad-455e-8bff-5ed39c3cf53a");
        // synthetize expr using register
        String src = trimSource(expr);
        writeline("/home/ubuntu/results/coverage/Engine/Engine_7_10.coverage", "6bf7bda8-8ad9-4e4e-b1fa-bc1c1fe0e07c");
        src = "#0" + (src.charAt(0) == '[' ? "" : ".") + src;
        writeline("/home/ubuntu/results/coverage/Engine/Engine_7_10.coverage", "102bcfd3-1514-45f6-b27b-b8ff937371f7");
        try {
            writeline("/home/ubuntu/results/coverage/Engine/Engine_7_10.coverage", "daf0c873-65e0-407d-805c-82ac06d4828d");
            final JexlInfo info = debug ? createInfo() : null;
            writeline("/home/ubuntu/results/coverage/Engine/Engine_7_10.coverage", "8610d21b-9d2a-4fb0-a97a-2da0bbe7c74a");
            final Scope scope = new Scope(null, "#0");
            writeline("/home/ubuntu/results/coverage/Engine/Engine_7_10.coverage", "21b9066a-97d6-4eac-9252-26d364dba52d");
            final ASTJexlScript script = parse(info, src, scope, true, true);
            writeline("/home/ubuntu/results/coverage/Engine/Engine_7_10.coverage", "f8b515d2-fe0c-4dc5-819c-985859e87e2b");
            final JexlNode node = script.jjtGetChild(0);
            writeline("/home/ubuntu/results/coverage/Engine/Engine_7_10.coverage", "31f67cca-ee39-4f36-b0c1-0e0e3aa9d498");
            final Scope.Frame frame = script.createFrame(bean);
            writeline("/home/ubuntu/results/coverage/Engine/Engine_7_10.coverage", "d6a749c5-3ef3-4e41-aa31-34fa1e2073f5");
            final Interpreter interpreter = createInterpreter(context, frame);
            writeline("/home/ubuntu/results/coverage/Engine/Engine_7_10.coverage", "83571070-3db3-443a-9a0e-962f5c35d722");
            return node.jjtAccept(interpreter, null);
        } catch (JexlException xjexl) {
            writeline("/home/ubuntu/results/coverage/Engine/Engine_7_10.coverage", "748d5859-1552-4603-aa59-738cb46fee18");
            if (silent) {
                writeline("/home/ubuntu/results/coverage/Engine/Engine_7_10.coverage", "c5dab114-fc9d-415f-9ed1-9560e24b9c80");
                logger.warn(xjexl.getMessage(), xjexl.getCause());
                writeline("/home/ubuntu/results/coverage/Engine/Engine_7_10.coverage", "28863ec6-888f-4715-a855-a3af39c2c7a2");
                return null;
            }
            writeline("/home/ubuntu/results/coverage/Engine/Engine_7_10.coverage", "946db931-94ea-4744-9b88-0d83cb2ff584");
            throw xjexl.clean();
        }
    }

    @Override
    public void setProperty(Object bean, String expr, Object value) {
        writeline("/home/ubuntu/results/coverage/Engine/Engine_7_10.coverage", "78c6f229-8cfd-4fb7-951a-23a05abdc333");
        setProperty(null, bean, expr, value);
    }

    @Override
    public void setProperty(JexlContext context, Object bean, String expr, Object value) {
        writeline("/home/ubuntu/results/coverage/Engine/Engine_7_10.coverage", "b8b9b12f-1a97-4099-9718-437a84126d5e");
        if (context == null) {
            writeline("/home/ubuntu/results/coverage/Engine/Engine_7_10.coverage", "3d9dacfa-4df1-494f-be54-f533008019a0");
            context = EMPTY_CONTEXT;
        }
        writeline("/home/ubuntu/results/coverage/Engine/Engine_7_10.coverage", "6b37182c-711f-433e-9b2f-3bbc68dae057");
        // synthetize expr using registers
        String src = trimSource(expr);
        writeline("/home/ubuntu/results/coverage/Engine/Engine_7_10.coverage", "9ada3847-43f4-438c-a3ed-cd02214e4b29");
        src = "#0" + (src.charAt(0) == '[' ? "" : ".") + src + "=" + "#1";
        writeline("/home/ubuntu/results/coverage/Engine/Engine_7_10.coverage", "e1a474e9-d61a-4b69-b9a2-2d17941834cb");
        try {
            writeline("/home/ubuntu/results/coverage/Engine/Engine_7_10.coverage", "4b34812c-a00d-44cd-893e-fdf9461e4b3b");
            final JexlInfo info = debug ? createInfo() : null;
            writeline("/home/ubuntu/results/coverage/Engine/Engine_7_10.coverage", "585684d2-0adf-4296-b3d0-b43eb52db02e");
            final Scope scope = new Scope(null, "#0", "#1");
            writeline("/home/ubuntu/results/coverage/Engine/Engine_7_10.coverage", "88744f87-a26b-4321-be00-55788e89c950");
            final ASTJexlScript script = parse(info, src, scope, true, true);
            writeline("/home/ubuntu/results/coverage/Engine/Engine_7_10.coverage", "8234ec14-ffa5-4e27-9eed-798d9e3e1746");
            final JexlNode node = script.jjtGetChild(0);
            writeline("/home/ubuntu/results/coverage/Engine/Engine_7_10.coverage", "e521c253-e600-4fca-8017-5751dfddf8e2");
            final Scope.Frame frame = script.createFrame(bean, value);
            writeline("/home/ubuntu/results/coverage/Engine/Engine_7_10.coverage", "22eee39f-105b-47fc-a447-7e1decf3f2ca");
            final Interpreter interpreter = createInterpreter(context, frame);
            writeline("/home/ubuntu/results/coverage/Engine/Engine_7_10.coverage", "ac171c57-b66b-403c-ac68-51f20bb4503d");
            node.jjtAccept(interpreter, null);
        } catch (JexlException xjexl) {
            writeline("/home/ubuntu/results/coverage/Engine/Engine_7_10.coverage", "c561ac57-3878-4c72-bfd0-2a62dbb80aa4");
            if (silent) {
                writeline("/home/ubuntu/results/coverage/Engine/Engine_7_10.coverage", "263e87d9-1e34-4f74-a75a-e7f6d448a56b");
                logger.warn(xjexl.getMessage(), xjexl.getCause());
                writeline("/home/ubuntu/results/coverage/Engine/Engine_7_10.coverage", "bf1e9104-079b-451a-9dbe-753eeabc2963");
                return;
            }
            writeline("/home/ubuntu/results/coverage/Engine/Engine_7_10.coverage", "ce642905-2e0b-4285-8104-1f1d58730460");
            throw xjexl.clean();
        }
    }

    @Override
    public Object invokeMethod(Object obj, String meth, Object... args) {
        writeline("/home/ubuntu/results/coverage/Engine/Engine_7_10.coverage", "71c0a293-83a9-4218-8987-24b9b54ac690");
        JexlException xjexl = null;
        writeline("/home/ubuntu/results/coverage/Engine/Engine_7_10.coverage", "815372ef-cab0-4dd2-94e9-0d1d07fa8079");
        Object result = null;
        writeline("/home/ubuntu/results/coverage/Engine/Engine_7_10.coverage", "1389093b-50ff-45c5-9970-241a0f5de41a");
        final JexlInfo info = debug ? createInfo() : null;
        writeline("/home/ubuntu/results/coverage/Engine/Engine_7_10.coverage", "82955a02-0a65-4d6f-b1ae-d6f74544e834");
        try {
            writeline("/home/ubuntu/results/coverage/Engine/Engine_7_10.coverage", "d70defaa-c041-4a7d-a0be-ba72b5c9acf8");
            JexlMethod method = uberspect.getMethod(obj, meth, args);
            writeline("/home/ubuntu/results/coverage/Engine/Engine_7_10.coverage", "3eff54da-9fd2-4ac5-bac0-dc761517f463");
            if (method == null && arithmetic.narrowArguments(args)) {
                writeline("/home/ubuntu/results/coverage/Engine/Engine_7_10.coverage", "231e7bc5-6a85-4528-86a3-63cbbb33911c");
                method = uberspect.getMethod(obj, meth, args);
            }
            writeline("/home/ubuntu/results/coverage/Engine/Engine_7_10.coverage", "449d6b31-1566-4b55-b3b0-a4c30148649d");
            if (method != null) {
                writeline("/home/ubuntu/results/coverage/Engine/Engine_7_10.coverage", "5d0e24ff-8bac-4d8c-8933-3ccbd9fb6a10");
                result = method.invoke(obj, args);
            } else {
                writeline("/home/ubuntu/results/coverage/Engine/Engine_7_10.coverage", "ac1d4670-2e9e-4f5a-9256-f07757a8e90d");
                xjexl = new JexlException.Method(info, meth, null);
            }
        } catch (JexlException xany) {
            writeline("/home/ubuntu/results/coverage/Engine/Engine_7_10.coverage", "bd9e2eaa-7b07-43ab-a29a-4de79e3efcb8");
            xjexl = xany;
        } catch (Exception xany) {
            writeline("/home/ubuntu/results/coverage/Engine/Engine_7_10.coverage", "c1d9af5e-8774-425e-899c-4e25efc40ec0");
            xjexl = new JexlException.Method(info, meth, xany);
        }
        writeline("/home/ubuntu/results/coverage/Engine/Engine_7_10.coverage", "8518229f-eee8-4e33-9625-a22b1d4485a5");
        if (xjexl != null) {
            writeline("/home/ubuntu/results/coverage/Engine/Engine_7_10.coverage", "07c30114-1c54-453c-8eff-d8d1a085ad5f");
            if (silent) {
                writeline("/home/ubuntu/results/coverage/Engine/Engine_7_10.coverage", "e1e3e1ed-e5cd-4ea1-9c8d-9ef533b50fc0");
                logger.warn(xjexl.getMessage(), xjexl.getCause());
                writeline("/home/ubuntu/results/coverage/Engine/Engine_7_10.coverage", "78260ced-1222-49fa-accd-eebe2a5af720");
                result = null;
            } else {
                writeline("/home/ubuntu/results/coverage/Engine/Engine_7_10.coverage", "c2d65d86-f1dc-45ed-a9a3-a35f54690646");
                throw xjexl.clean();
            }
        }
        writeline("/home/ubuntu/results/coverage/Engine/Engine_7_10.coverage", "c82485e8-ea4d-4a93-9e40-7ff1b3ba57ee");
        return result;
    }

    @Override
    public <T> T newInstance(Class<? extends T> clazz, Object... args) {
        writeline("/home/ubuntu/results/coverage/Engine/Engine_7_10.coverage", "5e6283a1-7416-4a3f-814c-c622590e2f0e");
        return clazz.cast(doCreateInstance(clazz, args));
    }

    @Override
    public Object newInstance(String clazz, Object... args) {
        writeline("/home/ubuntu/results/coverage/Engine/Engine_7_10.coverage", "39bc255e-b189-4722-b15c-3898be32a068");
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
        writeline("/home/ubuntu/results/coverage/Engine/Engine_7_10.coverage", "89887d02-8753-4ca7-8667-bbf8cf6db41a");
        JexlException xjexl = null;
        writeline("/home/ubuntu/results/coverage/Engine/Engine_7_10.coverage", "9094a212-e4e8-4b67-941e-8b8dd7f2fec4");
        Object result = null;
        writeline("/home/ubuntu/results/coverage/Engine/Engine_7_10.coverage", "4567ef3b-b9e2-46b8-8d32-3055cd0f3cb4");
        final JexlInfo info = debug ? createInfo() : null;
        writeline("/home/ubuntu/results/coverage/Engine/Engine_7_10.coverage", "14ed3b36-5da0-43a6-80f9-2f0db4f0f14f");
        try {
            writeline("/home/ubuntu/results/coverage/Engine/Engine_7_10.coverage", "e194748f-22a4-4df5-aa9a-e7ff891d639a");
            JexlMethod ctor = uberspect.getConstructor(clazz, args);
            writeline("/home/ubuntu/results/coverage/Engine/Engine_7_10.coverage", "1f53d093-ec97-49fb-a8fa-29f5645e6f57");
            if (ctor == null && arithmetic.narrowArguments(args)) {
                writeline("/home/ubuntu/results/coverage/Engine/Engine_7_10.coverage", "1c9effbc-da18-4d05-8a9f-837b7864d8b2");
                ctor = uberspect.getConstructor(clazz, args);
            }
            writeline("/home/ubuntu/results/coverage/Engine/Engine_7_10.coverage", "c271272b-f8c9-46ad-add7-775394224e31");
            if (ctor != null) {
                writeline("/home/ubuntu/results/coverage/Engine/Engine_7_10.coverage", "7eae61d0-b0d2-4d5b-9232-d0dca1375e20");
                result = ctor.invoke(clazz, args);
            } else {
                writeline("/home/ubuntu/results/coverage/Engine/Engine_7_10.coverage", "47052dc8-e11d-4fc5-ad2b-4b1a84f878e9");
                xjexl = new JexlException.Method(info, clazz.toString(), null);
            }
        } catch (JexlException xany) {
            writeline("/home/ubuntu/results/coverage/Engine/Engine_7_10.coverage", "3f9d8080-ba73-41a0-890c-8cf6a50d7eb0");
            xjexl = xany;
        } catch (Exception xany) {
            writeline("/home/ubuntu/results/coverage/Engine/Engine_7_10.coverage", "58cb1b92-14e1-4ae2-b2df-20d7d4763870");
            xjexl = new JexlException.Method(info, clazz.toString(), xany);
        }
        writeline("/home/ubuntu/results/coverage/Engine/Engine_7_10.coverage", "f6185bc4-158e-4868-aeec-c0aac4d0661b");
        if (xjexl != null) {
            writeline("/home/ubuntu/results/coverage/Engine/Engine_7_10.coverage", "6e01d533-c947-4326-a9c3-eec21283531a");
            if (silent) {
                writeline("/home/ubuntu/results/coverage/Engine/Engine_7_10.coverage", "2b0c06f5-b020-4b8c-a2d7-36b5e87d1729");
                logger.warn(xjexl.getMessage(), xjexl.getCause());
                writeline("/home/ubuntu/results/coverage/Engine/Engine_7_10.coverage", "1afd6a5d-bcfd-4fd9-8192-65115b68b359");
                return null;
            }
            writeline("/home/ubuntu/results/coverage/Engine/Engine_7_10.coverage", "a15fe6bb-d4c6-48c9-bde0-044f887a67b5");
            throw xjexl.clean();
        }
        writeline("/home/ubuntu/results/coverage/Engine/Engine_7_10.coverage", "678cc067-d930-4753-8b47-dcb93833a0c9");
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
        writeline("/home/ubuntu/results/coverage/Engine/Engine_7_10.coverage", "90c6abef-9e71-4917-ad26-ad41c2beaafb");
        VarCollector collector = new VarCollector();
        writeline("/home/ubuntu/results/coverage/Engine/Engine_7_10.coverage", "38f4bf77-a45a-4028-a5cf-00f7520cc566");
        getVariables(script, script, collector);
        writeline("/home/ubuntu/results/coverage/Engine/Engine_7_10.coverage", "56f0f173-2ba1-42e3-866b-a4b0b34ef1a2");
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
        writeline("/home/ubuntu/results/coverage/Engine/Engine_7_10.coverage", "02713869-5516-4b8c-9c5f-ec368d63e357");
        if (node instanceof ASTIdentifier) {
            writeline("/home/ubuntu/results/coverage/Engine/Engine_7_10.coverage", "19bbc5d2-982b-4a29-91b2-cb9a67318952");
            JexlNode parent = node.jjtGetParent();
            writeline("/home/ubuntu/results/coverage/Engine/Engine_7_10.coverage", "dc83b275-a45f-4f93-8901-4993ec58e3ad");
            if (parent instanceof ASTMethodNode || parent instanceof ASTFunctionNode) {
                writeline("/home/ubuntu/results/coverage/Engine/Engine_7_10.coverage", "662cd2a5-cd92-4e95-9971-84fe1233e000");
                // skip identifiers for methods and functions
                collector.collect(null);
                writeline("/home/ubuntu/results/coverage/Engine/Engine_7_10.coverage", "2f9e23e0-0674-4a07-b511-a12b09804cde");
                return;
            }
            writeline("/home/ubuntu/results/coverage/Engine/Engine_7_10.coverage", "1a90e961-382f-469d-868f-bf1e63450080");
            ASTIdentifier identifier = (ASTIdentifier) node;
            writeline("/home/ubuntu/results/coverage/Engine/Engine_7_10.coverage", "6b5ddfb4-31c4-41b9-a970-cf449d97f23d");
            int symbol = identifier.getSymbol();
            writeline("/home/ubuntu/results/coverage/Engine/Engine_7_10.coverage", "90dc837d-7dbe-4f39-917b-0f3f8b374e13");
            // symbols that are hoisted are considered "global" variables
            if (symbol >= 0 && script != null && !script.isHoistedSymbol(symbol)) {
                writeline("/home/ubuntu/results/coverage/Engine/Engine_7_10.coverage", "16f23593-a0f4-4c0a-acaf-cc60b2c60b93");
                collector.collect(null);
            } else {
                writeline("/home/ubuntu/results/coverage/Engine/Engine_7_10.coverage", "9795ca94-1f59-4dff-90d6-0f55d2e0ebe2");
                // start collecting from identifier
                collector.collect(identifier);
                writeline("/home/ubuntu/results/coverage/Engine/Engine_7_10.coverage", "ef703018-be07-42c1-a988-ad246293ccfc");
                collector.add(identifier.getName());
            }
        } else if (node instanceof ASTIdentifierAccess) {
            writeline("/home/ubuntu/results/coverage/Engine/Engine_7_10.coverage", "68c73b56-378c-4df4-b467-0ed3038d2271");
            JexlNode parent = node.jjtGetParent();
            writeline("/home/ubuntu/results/coverage/Engine/Engine_7_10.coverage", "d36e81ba-90fa-446c-97a3-4c1b7c86ae04");
            if (parent instanceof ASTMethodNode || parent instanceof ASTFunctionNode) {
                writeline("/home/ubuntu/results/coverage/Engine/Engine_7_10.coverage", "3735da67-2810-40a8-bdfb-eda629027868");
                // skip identifiers for methods and functions
                collector.collect(null);
                writeline("/home/ubuntu/results/coverage/Engine/Engine_7_10.coverage", "309288ec-a887-4ec4-b1a2-f04847a3619b");
                return;
            }
            writeline("/home/ubuntu/results/coverage/Engine/Engine_7_10.coverage", "7fddd0b7-54f8-4b8e-9009-97a7705eacbd");
            // belt and suspender since an identifier should have been seen first
            if (collector.isCollecting()) {
                writeline("/home/ubuntu/results/coverage/Engine/Engine_7_10.coverage", "cc0bd0ba-35e0-4ffe-b654-5007a4d1fdc0");
                collector.add(((ASTIdentifierAccess) node).getName());
            }
        } else if (node instanceof ASTArrayAccess) {
            writeline("/home/ubuntu/results/coverage/Engine/Engine_7_10.coverage", "03886627-7b72-42e8-90f9-2b04b4f23b63");
            int num = node.jjtGetNumChildren();
            writeline("/home/ubuntu/results/coverage/Engine/Engine_7_10.coverage", "9054e0e9-454c-4e2f-a12b-6316d52fbb06");
            // collect only if array access is const and follows an identifier
            boolean collecting = collector.isCollecting();
            writeline("/home/ubuntu/results/coverage/Engine/Engine_7_10.coverage", "a1368cc4-1012-4b06-86a2-b08c6526dac5");
            for (int i = 0; i < num; ++i) {
                writeline("/home/ubuntu/results/coverage/Engine/Engine_7_10.coverage", "aef69f76-9129-4f48-af86-c0f46fca6eb3");
                JexlNode child = node.jjtGetChild(i);
                writeline("/home/ubuntu/results/coverage/Engine/Engine_7_10.coverage", "b37e450d-8cb6-4922-9158-4c5cf0bfe675");
                if (collecting && child.isConstant()) {
                    writeline("/home/ubuntu/results/coverage/Engine/Engine_7_10.coverage", "15b1a214-0209-45df-986a-d021a2104cfc");
                    String image = child.toString();
                    writeline("/home/ubuntu/results/coverage/Engine/Engine_7_10.coverage", "ce1a9712-5116-480b-94e7-c17f26371b4f");
                    if (image == null) {
                        writeline("/home/ubuntu/results/coverage/Engine/Engine_7_10.coverage", "4bd78c1e-4816-4f57-8e20-d6134501c4f4");
                        image = new Debugger().data(child);
                    }
                    writeline("/home/ubuntu/results/coverage/Engine/Engine_7_10.coverage", "550d450d-02ae-42a4-95f8-92cb93f63f1c");
                    collector.add(image);
                } else {
                    writeline("/home/ubuntu/results/coverage/Engine/Engine_7_10.coverage", "0f81ac4a-ee01-4ad0-b2cd-1b31a83113e6");
                    collecting = false;
                    writeline("/home/ubuntu/results/coverage/Engine/Engine_7_10.coverage", "5b34c262-0511-407e-a72e-fa8623eef53f");
                    collector.collect(null);
                    writeline("/home/ubuntu/results/coverage/Engine/Engine_7_10.coverage", "80b5e014-a9d1-4ec7-be5d-3a3e08bbff42");
                    getVariables(script, child, collector);
                }
            }
        } else {
            writeline("/home/ubuntu/results/coverage/Engine/Engine_7_10.coverage", "dfb86ff2-36c3-4480-acf3-696df0329ec4");
            int num = node.jjtGetNumChildren();
            writeline("/home/ubuntu/results/coverage/Engine/Engine_7_10.coverage", "84bee1d9-318a-461a-b2b4-39d9206dd271");
            for (int i = 0; i < num; ++i) {
                writeline("/home/ubuntu/results/coverage/Engine/Engine_7_10.coverage", "4e4e74d4-8a2e-4fdb-b4dc-bba0a4ae149d");
                getVariables(script, node.jjtGetChild(i), collector);
            }
            writeline("/home/ubuntu/results/coverage/Engine/Engine_7_10.coverage", "90ccc98e-3aad-4e1c-add7-92a49bf7aec0");
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
        writeline("/home/ubuntu/results/coverage/Engine/Engine_7_10.coverage", "8fc5e572-002c-4bf2-a22c-fd57c638d4ae");
        return script.getParameters();
    }

    /**
     * Gets the array of local variable from a script.
     * @param script the script
     * @return the local variables array which may be empty (but not null) if no local variables were defined
     * @since 3.0
     */
    protected String[] getLocalVariables(JexlScript script) {
        writeline("/home/ubuntu/results/coverage/Engine/Engine_7_10.coverage", "03ed991b-55ca-4069-88b8-0a479d05c598");
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
        writeline("/home/ubuntu/results/coverage/Engine/Engine_7_10.coverage", "d310236d-53a4-424f-b617-7e93a9c89e2d");
        final boolean cached = src.length() < cacheThreshold && cache != null;
        writeline("/home/ubuntu/results/coverage/Engine/Engine_7_10.coverage", "fef287c2-751e-49b2-ae6c-eff7dfdf4688");
        ASTJexlScript script;
        writeline("/home/ubuntu/results/coverage/Engine/Engine_7_10.coverage", "e97b2e57-61fa-4419-a636-4e0a707ef9d4");
        synchronized (parser) {
            writeline("/home/ubuntu/results/coverage/Engine/Engine_7_10.coverage", "de38ecbd-6a31-4c12-85bc-81ab5de93c07");
            if (cached) {
                writeline("/home/ubuntu/results/coverage/Engine/Engine_7_10.coverage", "cf820a80-f17b-47a4-b94d-6c5b4da9abd4");
                script = cache.get(src);
                writeline("/home/ubuntu/results/coverage/Engine/Engine_7_10.coverage", "5a6e9e48-6cec-45f5-9b8e-d30771c10c60");
                if (script != null) {
                    writeline("/home/ubuntu/results/coverage/Engine/Engine_7_10.coverage", "f1f50422-33a0-4b87-abd0-a25cf563028d");
                    Scope f = script.getScope();
                    writeline("/home/ubuntu/results/coverage/Engine/Engine_7_10.coverage", "57afd4f2-afdc-447d-924e-8435cd4ef7c2");
                    if ((f == null && scope == null) || (f != null && f.equals(scope))) {
                        writeline("/home/ubuntu/results/coverage/Engine/Engine_7_10.coverage", "ca6e324f-e1fa-4896-8fcc-97f80814cc57");
                        return script;
                    }
                }
            }
            writeline("/home/ubuntu/results/coverage/Engine/Engine_7_10.coverage", "91a7e0b4-8107-4ca6-ac6e-81ce23998c60");
            script = parser.parse(info, src, scope, registers, expression);
            writeline("/home/ubuntu/results/coverage/Engine/Engine_7_10.coverage", "bf24c43c-1130-4533-ad66-c2a1674e1523");
            if (cached) {
                writeline("/home/ubuntu/results/coverage/Engine/Engine_7_10.coverage", "0bb7b16d-cd3a-44db-93f1-c0a6fecc2f2d");
                cache.put(src, script);
            }
        }
        writeline("/home/ubuntu/results/coverage/Engine/Engine_7_10.coverage", "6ec92969-d69d-4b71-97db-cc28ec6fa642");
        return script;
    }

    /**
     * Trims the source from front and ending spaces.
     * @param str expression to clean
     * @return trimmed expression ending in a semi-colon
     */
    protected String trimSource(CharSequence str) {
        writeline("/home/ubuntu/results/coverage/Engine/Engine_7_10.coverage", "99aecc09-1091-4f0f-a461-a74b953b44b0");
        if (str != null) {
            writeline("/home/ubuntu/results/coverage/Engine/Engine_7_10.coverage", "d4082fec-9145-4f4a-ab1b-f68f2529729e");
            int start = 0;
            writeline("/home/ubuntu/results/coverage/Engine/Engine_7_10.coverage", "c011b116-4eed-40d3-b16e-9ae9ca2bbb2d");
            int end = str.length();
            writeline("/home/ubuntu/results/coverage/Engine/Engine_7_10.coverage", "ffd6b498-a9a6-408d-9db7-869b71f64ca7");
            if (end > 0) {
                writeline("/home/ubuntu/results/coverage/Engine/Engine_7_10.coverage", "044a21ef-b9fd-4e73-9cf7-ae2652da028d");
                // trim front spaces
                while (start < end && Character.isSpaceChar(str.charAt(start))) {
                    writeline("/home/ubuntu/results/coverage/Engine/Engine_7_10.coverage", "ae9f50e1-0231-4875-9f86-1577965c43da");
                    ++start;
                }
                writeline("/home/ubuntu/results/coverage/Engine/Engine_7_10.coverage", "14fc2c2b-79de-4a77-9ab5-a735dd019797");
                // trim ending spaces
                while (end > 0 && Character.isSpaceChar(str.charAt(end - 1))) {
                    writeline("/home/ubuntu/results/coverage/Engine/Engine_7_10.coverage", "d61f4816-01b4-4441-bb57-d61856319d75");
                    --end;
                }
                writeline("/home/ubuntu/results/coverage/Engine/Engine_7_10.coverage", "08836598-f922-4eb8-9f30-5bb1428c43df");
                return str.subSequence(start, end).toString();
            }
            writeline("/home/ubuntu/results/coverage/Engine/Engine_7_10.coverage", "089cb7d6-f1cc-4873-bc6d-8ce5d173d52d");
            return "";
        }
        writeline("/home/ubuntu/results/coverage/Engine/Engine_7_10.coverage", "b3d7c6a6-c672-4a0f-b836-08bcd39e0a6c");
        return null;
    }

    /**
     * Gets and/or creates a default template engine.
     * @return a template engine
     */
    protected TemplateEngine jxlt() {
        writeline("/home/ubuntu/results/coverage/Engine/Engine_7_10.coverage", "25063fe3-ff5e-480c-a92b-da6f357526c9");
        TemplateEngine e = jxlt;
        writeline("/home/ubuntu/results/coverage/Engine/Engine_7_10.coverage", "defa0b52-aeac-490c-a4b2-a5f30971af6f");
        if (e == null) {
            writeline("/home/ubuntu/results/coverage/Engine/Engine_7_10.coverage", "7a9d7da1-cf94-4282-833c-69b6b3b8fa5c");
            synchronized (this) {
                writeline("/home/ubuntu/results/coverage/Engine/Engine_7_10.coverage", "6188c169-3c18-49d3-900d-ac4d523a95c6");
                if (jxlt == null) {
                    writeline("/home/ubuntu/results/coverage/Engine/Engine_7_10.coverage", "37280d45-89aa-4432-9e04-2c08fe1c2217");
                    e = new TemplateEngine(this, true, 0, '$', '#');
                    writeline("/home/ubuntu/results/coverage/Engine/Engine_7_10.coverage", "56e1e5c2-3129-4faf-aff7-558b1d2e3413");
                    jxlt = e;
                }
            }
        }
        writeline("/home/ubuntu/results/coverage/Engine/Engine_7_10.coverage", "c3182a9a-ba7c-4853-8988-6f0162771e9f");
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
