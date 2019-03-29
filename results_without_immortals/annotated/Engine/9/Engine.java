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
        writelineStatic("/home/ubuntu/results/coverage/Engine/Engine_9_10.coverage", "2d310f89-1efe-4e17-81b8-07f0ce61d230");
        if ((logger == null || logger.equals(LogFactory.getLog(JexlEngine.class))) && (strategy == null || strategy == JexlUberspect.JEXL_STRATEGY)) {
            writelineStatic("/home/ubuntu/results/coverage/Engine/Engine_9_10.coverage", "cfb38bd2-da1f-4837-ada4-299cdf97d387");
            return UberspectHolder.UBERSPECT;
        }
        writelineStatic("/home/ubuntu/results/coverage/Engine/Engine_9_10.coverage", "6829d5ab-2d91-4f81-ab66-bbc3f7b6aebc");
        return new Uberspect(logger, strategy);
    }

    @Override
    public JexlUberspect getUberspect() {
        writeline("/home/ubuntu/results/coverage/Engine/Engine_9_10.coverage", "52465fe7-4fa0-4f9b-8bce-e2e7da4b8483");
        return uberspect;
    }

    @Override
    public JexlArithmetic getArithmetic() {
        writeline("/home/ubuntu/results/coverage/Engine/Engine_9_10.coverage", "8cb9ecd3-4714-4ada-b8ea-210b1787b6a2");
        return arithmetic;
    }

    @Override
    public boolean isDebug() {
        writeline("/home/ubuntu/results/coverage/Engine/Engine_9_10.coverage", "45f08f84-569b-4054-9bf6-b3602c961322");
        return this.debug;
    }

    @Override
    public boolean isSilent() {
        writeline("/home/ubuntu/results/coverage/Engine/Engine_9_10.coverage", "5e3cd3d2-4d8e-4395-89af-d636a16fab85");
        return this.silent;
    }

    @Override
    public boolean isStrict() {
        writeline("/home/ubuntu/results/coverage/Engine/Engine_9_10.coverage", "1dea98ec-2e01-4ee0-8b2a-0cca9dc6266b");
        return strict;
    }

    @Override
    public void setClassLoader(ClassLoader loader) {
        writeline("/home/ubuntu/results/coverage/Engine/Engine_9_10.coverage", "2a040d12-7026-48bf-b48d-680dfcbbbb7b");
        uberspect.setClassLoader(loader);
    }

    @Override
    public Charset getCharset() {
        writeline("/home/ubuntu/results/coverage/Engine/Engine_9_10.coverage", "39307bc2-f573-42a8-a301-7530f39e968b");
        return charset;
    }

    @Override
    public TemplateEngine createJxltEngine(boolean noScript, int cacheSize, char immediate, char deferred) {
        writeline("/home/ubuntu/results/coverage/Engine/Engine_9_10.coverage", "d6d99ee7-f68f-4dc1-9aa5-ed1b653d8d51");
        return new TemplateEngine(this, noScript, cacheSize, immediate, deferred);
    }

    /**
     * Swaps the current thread local context.
     * @param tls the context or null
     * @return the previous thread local context
     */
    protected JexlContext.ThreadLocal putThreadLocal(JexlContext.ThreadLocal tls) {
        writeline("/home/ubuntu/results/coverage/Engine/Engine_9_10.coverage", "bbd3ce30-4bc6-46fb-bbd9-c051c0701ac9");
        JexlContext.ThreadLocal local = CONTEXT.get();
        writeline("/home/ubuntu/results/coverage/Engine/Engine_9_10.coverage", "79a639b0-e022-4f16-8482-f9752d71e3e4");
        CONTEXT.set(tls);
        writeline("/home/ubuntu/results/coverage/Engine/Engine_9_10.coverage", "43483cfa-f36b-4a43-816d-224584244fb7");
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
        writeline("/home/ubuntu/results/coverage/Engine/Engine_9_10.coverage", "4812692d-aebc-4b99-851b-1bb947eb01d8");
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
        writeline("/home/ubuntu/results/coverage/Engine/Engine_9_10.coverage", "91f7bfb3-b3d5-4a38-9999-290bfdbfb916");
        synchronized (parser) {
            writeline("/home/ubuntu/results/coverage/Engine/Engine_9_10.coverage", "b55534de-ae35-4b0c-8b0c-efb5c9d1488b");
            if (cache != null) {
                writeline("/home/ubuntu/results/coverage/Engine/Engine_9_10.coverage", "08426188-1539-4db0-b1f2-a06292d202e8");
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
        writeline("/home/ubuntu/results/coverage/Engine/Engine_9_10.coverage", "b73d1336-967e-49a4-a00c-7a33a24e3a2f");
        return new Interpreter(this, context, frame);
    }

    @Override
    public Script createScript(JexlInfo info, String scriptText, String[] names) {
        writeline("/home/ubuntu/results/coverage/Engine/Engine_9_10.coverage", "d38e0020-ce84-4c77-9af2-c8443a9e7aa3");
        if (scriptText == null) {
            writeline("/home/ubuntu/results/coverage/Engine/Engine_9_10.coverage", "f436d999-d2f9-48fa-a30c-8594c2e7af61");
            throw new NullPointerException("source is null");
        }
        writeline("/home/ubuntu/results/coverage/Engine/Engine_9_10.coverage", "c7daf213-b279-4bae-82d2-933bbf05638f");
        if (info == null && debug) {
            writeline("/home/ubuntu/results/coverage/Engine/Engine_9_10.coverage", "053ee1b5-673e-44eb-81b2-f59112014952");
            info = createInfo();
        }
        writeline("/home/ubuntu/results/coverage/Engine/Engine_9_10.coverage", "d3260f79-c662-4a77-b5d9-5d618bff1952");
        String source = trimSource(scriptText);
        writeline("/home/ubuntu/results/coverage/Engine/Engine_9_10.coverage", "a3d8bf98-fbd9-4103-a009-7474534d2d6d");
        Scope scope = names == null ? null : new Scope(null, names);
        writeline("/home/ubuntu/results/coverage/Engine/Engine_9_10.coverage", "887f8474-7633-40ad-9cb9-444bd2e09e15");
        ASTJexlScript tree = parse(info, source, scope, false, false);
        writeline("/home/ubuntu/results/coverage/Engine/Engine_9_10.coverage", "7483f693-77fc-4bb0-816c-fee855a49991");
        return new Script(this, source, tree);
    }

    @Override
    public Script createExpression(JexlInfo info, String expression) {
        writeline("/home/ubuntu/results/coverage/Engine/Engine_9_10.coverage", "fd34fd32-c4b6-4352-9c93-74b3de066a52");
        if (expression == null) {
            writeline("/home/ubuntu/results/coverage/Engine/Engine_9_10.coverage", "6f8fc1d3-be08-4137-97e3-e662fd125265");
            throw new NullPointerException("source is null");
        }
        writeline("/home/ubuntu/results/coverage/Engine/Engine_9_10.coverage", "1e362da7-8297-4eba-86cf-403347a632e1");
        if (info == null && debug) {
            writeline("/home/ubuntu/results/coverage/Engine/Engine_9_10.coverage", "54a44abe-7cf6-4f9f-b5a0-e8029fddeb14");
            info = createInfo();
        }
        writeline("/home/ubuntu/results/coverage/Engine/Engine_9_10.coverage", "619aa9c1-bb64-4415-8804-144202a5a509");
        String source = trimSource(expression);
        writeline("/home/ubuntu/results/coverage/Engine/Engine_9_10.coverage", "7eacc14b-c137-47d8-a8b3-8837b8e636a5");
        ASTJexlScript tree = parse(info, source, null, false, true);
        writeline("/home/ubuntu/results/coverage/Engine/Engine_9_10.coverage", "18a1e8b2-64a7-4988-a433-105e5ca10df6");
        return new Script(this, source, tree);
    }

    @Override
    public Object getProperty(Object bean, String expr) {
        writeline("/home/ubuntu/results/coverage/Engine/Engine_9_10.coverage", "44530da8-baca-4b40-bbe0-7ee3c7f26502");
        return getProperty(null, bean, expr);
    }

    @Override
    public Object getProperty(JexlContext context, Object bean, String expr) {
        writeline("/home/ubuntu/results/coverage/Engine/Engine_9_10.coverage", "638d00d6-dcbe-44ef-a35a-51653858a967");
        if (context == null) {
            writeline("/home/ubuntu/results/coverage/Engine/Engine_9_10.coverage", "ef5905c9-147a-47ea-9d89-6b377c834b27");
            context = EMPTY_CONTEXT;
        }
        writeline("/home/ubuntu/results/coverage/Engine/Engine_9_10.coverage", "d44032cf-9e6c-4189-9916-32c2521c92fd");
        // synthetize expr using register
        String src = trimSource(expr);
        writeline("/home/ubuntu/results/coverage/Engine/Engine_9_10.coverage", "0399878d-da8f-4bee-9e26-b5f40afc99e1");
        src = "#0" + (src.charAt(0) == '[' ? "" : ".") + src;
        writeline("/home/ubuntu/results/coverage/Engine/Engine_9_10.coverage", "d1bc27f1-a563-4724-9eaa-7ac6b38e9100");
        try {
            writeline("/home/ubuntu/results/coverage/Engine/Engine_9_10.coverage", "d23b1f42-906a-4733-bf6e-88c2afcce74b");
            final JexlInfo info = debug ? createInfo() : null;
            writeline("/home/ubuntu/results/coverage/Engine/Engine_9_10.coverage", "964d375a-584c-4e08-b9c5-9b19dd8a9559");
            final Scope scope = new Scope(null, "#0");
            writeline("/home/ubuntu/results/coverage/Engine/Engine_9_10.coverage", "4721bfac-d905-4e30-8ba9-06ccdcf720d3");
            final ASTJexlScript script = parse(info, src, scope, true, true);
            writeline("/home/ubuntu/results/coverage/Engine/Engine_9_10.coverage", "e9bb341a-6e96-4165-99cf-73c8017c8171");
            final JexlNode node = script.jjtGetChild(0);
            writeline("/home/ubuntu/results/coverage/Engine/Engine_9_10.coverage", "62f3b63c-9f34-4b9c-8c7c-a525d6e2e34a");
            final Scope.Frame frame = script.createFrame(bean);
            writeline("/home/ubuntu/results/coverage/Engine/Engine_9_10.coverage", "f09204b0-12bd-445c-88eb-24bf779969c0");
            final Interpreter interpreter = createInterpreter(context, frame);
            writeline("/home/ubuntu/results/coverage/Engine/Engine_9_10.coverage", "8425eee8-6b2d-42ef-bfe8-b34fe4f6622c");
            return node.jjtAccept(interpreter, null);
        } catch (JexlException xjexl) {
            writeline("/home/ubuntu/results/coverage/Engine/Engine_9_10.coverage", "8bae118e-048e-4e98-ac7a-a72a338f7ce7");
            if (silent) {
                writeline("/home/ubuntu/results/coverage/Engine/Engine_9_10.coverage", "2bd2ed90-5d06-4eb8-9390-949545efdf8d");
                logger.warn(xjexl.getMessage(), xjexl.getCause());
                writeline("/home/ubuntu/results/coverage/Engine/Engine_9_10.coverage", "3f7fa560-cf71-4f7f-86b9-f3b08c344acf");
                return null;
            }
            writeline("/home/ubuntu/results/coverage/Engine/Engine_9_10.coverage", "958d0d76-2b80-4d1e-b7e1-fa4831c2f7a2");
            throw xjexl.clean();
        }
    }

    @Override
    public void setProperty(Object bean, String expr, Object value) {
        writeline("/home/ubuntu/results/coverage/Engine/Engine_9_10.coverage", "5b3aaeac-6215-4a7a-b294-265b22d3a144");
        setProperty(null, bean, expr, value);
    }

    @Override
    public void setProperty(JexlContext context, Object bean, String expr, Object value) {
        writeline("/home/ubuntu/results/coverage/Engine/Engine_9_10.coverage", "8d532cf2-e9e7-4b44-83c3-cf1d0d03bec1");
        if (context == null) {
            writeline("/home/ubuntu/results/coverage/Engine/Engine_9_10.coverage", "deaa85a5-1490-44cb-b621-c2c517bedf5d");
            context = EMPTY_CONTEXT;
        }
        writeline("/home/ubuntu/results/coverage/Engine/Engine_9_10.coverage", "1e43d155-e89c-493c-bccd-014bf1e7cb3c");
        // synthetize expr using registers
        String src = trimSource(expr);
        writeline("/home/ubuntu/results/coverage/Engine/Engine_9_10.coverage", "65164731-d882-4be7-912e-b75948b4e432");
        src = "#0" + (src.charAt(0) == '[' ? "" : ".") + src + "=" + "#1";
        writeline("/home/ubuntu/results/coverage/Engine/Engine_9_10.coverage", "b7d75290-b51b-4d89-a706-7235e2686baf");
        try {
            writeline("/home/ubuntu/results/coverage/Engine/Engine_9_10.coverage", "df5cca38-d105-4973-9ae4-e2a847962266");
            final JexlInfo info = debug ? createInfo() : null;
            writeline("/home/ubuntu/results/coverage/Engine/Engine_9_10.coverage", "063529fb-68c0-490b-8399-cc7f68bb6272");
            final Scope scope = new Scope(null, "#0", "#1");
            writeline("/home/ubuntu/results/coverage/Engine/Engine_9_10.coverage", "e5b16934-3e6c-46f1-a697-cb4e03854e80");
            final ASTJexlScript script = parse(info, src, scope, true, true);
            writeline("/home/ubuntu/results/coverage/Engine/Engine_9_10.coverage", "626641f1-e334-4372-b625-78830b884696");
            final JexlNode node = script.jjtGetChild(0);
            writeline("/home/ubuntu/results/coverage/Engine/Engine_9_10.coverage", "78668725-7071-4d36-b861-204865822e13");
            final Scope.Frame frame = script.createFrame(bean, value);
            writeline("/home/ubuntu/results/coverage/Engine/Engine_9_10.coverage", "93fdc2da-3544-428d-a0a2-a2adbe5db7cc");
            final Interpreter interpreter = createInterpreter(context, frame);
            writeline("/home/ubuntu/results/coverage/Engine/Engine_9_10.coverage", "2da6054b-e823-4127-8a2d-7b3d175c6687");
            node.jjtAccept(interpreter, null);
        } catch (JexlException xjexl) {
            writeline("/home/ubuntu/results/coverage/Engine/Engine_9_10.coverage", "3529cfd8-775f-4731-8ea3-537dcfe8ade2");
            if (silent) {
                writeline("/home/ubuntu/results/coverage/Engine/Engine_9_10.coverage", "cb64cfb4-55d7-4825-9754-5904f2f70438");
                logger.warn(xjexl.getMessage(), xjexl.getCause());
                writeline("/home/ubuntu/results/coverage/Engine/Engine_9_10.coverage", "75ffb5bc-eafd-4da6-9880-190a85ae4dd1");
                return;
            }
            writeline("/home/ubuntu/results/coverage/Engine/Engine_9_10.coverage", "0aaf1c73-c971-45d4-b5e2-9c36a8e38336");
            throw xjexl.clean();
        }
    }

    @Override
    public Object invokeMethod(Object obj, String meth, Object... args) {
        writeline("/home/ubuntu/results/coverage/Engine/Engine_9_10.coverage", "d3ae0b12-fea3-419b-844b-308ab920bb1b");
        JexlException xjexl = null;
        writeline("/home/ubuntu/results/coverage/Engine/Engine_9_10.coverage", "f563dbc8-17a0-4d2a-afe0-980e0774a3e7");
        Object result = null;
        writeline("/home/ubuntu/results/coverage/Engine/Engine_9_10.coverage", "848fe35f-e35c-4bed-90d7-d25a910d4dc9");
        final JexlInfo info = debug ? createInfo() : null;
        writeline("/home/ubuntu/results/coverage/Engine/Engine_9_10.coverage", "96ca7835-e1dd-413d-9b3c-693232094463");
        try {
            writeline("/home/ubuntu/results/coverage/Engine/Engine_9_10.coverage", "48758c0d-c2a9-4eb1-8e13-3747f4888b01");
            JexlMethod method = uberspect.getMethod(obj, meth, args);
            writeline("/home/ubuntu/results/coverage/Engine/Engine_9_10.coverage", "b360a7e0-7288-4575-99d2-63f06603c240");
            if (method == null && arithmetic.narrowArguments(args)) {
                writeline("/home/ubuntu/results/coverage/Engine/Engine_9_10.coverage", "45b9d78e-e93c-44b8-9be8-9b559066b934");
                method = uberspect.getMethod(obj, meth, args);
            }
            writeline("/home/ubuntu/results/coverage/Engine/Engine_9_10.coverage", "b177cc6d-168b-451d-bb64-ab5274ed3b49");
            if (method != null) {
                writeline("/home/ubuntu/results/coverage/Engine/Engine_9_10.coverage", "98151c91-d714-4270-bb52-3ed56880ada2");
                result = method.invoke(obj, args);
            } else {
                writeline("/home/ubuntu/results/coverage/Engine/Engine_9_10.coverage", "c56251bd-c39d-49d6-9c9d-3a4ce8974b1f");
                xjexl = new JexlException.Method(info, meth, null);
            }
        } catch (JexlException xany) {
            writeline("/home/ubuntu/results/coverage/Engine/Engine_9_10.coverage", "8311db86-53da-4e1f-be6e-7c76081faeb9");
            xjexl = xany;
        } catch (Exception xany) {
            writeline("/home/ubuntu/results/coverage/Engine/Engine_9_10.coverage", "f374f9cd-4af6-4277-9b90-33b14e3bcea6");
            xjexl = new JexlException.Method(info, meth, xany);
        }
        writeline("/home/ubuntu/results/coverage/Engine/Engine_9_10.coverage", "76fed7e5-cb25-4daa-856c-bad9317ab133");
        if (xjexl != null) {
            writeline("/home/ubuntu/results/coverage/Engine/Engine_9_10.coverage", "5d383710-d27b-44ae-951c-97d0b8d96658");
            if (silent) {
                writeline("/home/ubuntu/results/coverage/Engine/Engine_9_10.coverage", "f6b2dc11-9a67-4047-b8f2-c676da692467");
                logger.warn(xjexl.getMessage(), xjexl.getCause());
                writeline("/home/ubuntu/results/coverage/Engine/Engine_9_10.coverage", "9be52481-455b-4dc9-b7ec-bdfa1bf97a11");
                result = null;
            } else {
                writeline("/home/ubuntu/results/coverage/Engine/Engine_9_10.coverage", "d607461b-296b-4389-8147-095a673d3e03");
                throw xjexl.clean();
            }
        }
        writeline("/home/ubuntu/results/coverage/Engine/Engine_9_10.coverage", "9e75b81c-06c2-4443-b5ea-70c92e317786");
        return result;
    }

    @Override
    public <T> T newInstance(Class<? extends T> clazz, Object... args) {
        writeline("/home/ubuntu/results/coverage/Engine/Engine_9_10.coverage", "1704decc-7411-4d59-a2cd-87279305905f");
        return clazz.cast(doCreateInstance(clazz, args));
    }

    @Override
    public Object newInstance(String clazz, Object... args) {
        writeline("/home/ubuntu/results/coverage/Engine/Engine_9_10.coverage", "ef42b5a0-aae5-48d6-8692-965457b521a6");
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
        writeline("/home/ubuntu/results/coverage/Engine/Engine_9_10.coverage", "079b0bb8-a571-469f-8e2c-ea0206350832");
        JexlException xjexl = null;
        writeline("/home/ubuntu/results/coverage/Engine/Engine_9_10.coverage", "306b2813-56e9-4748-aaef-f92d207299f9");
        Object result = null;
        writeline("/home/ubuntu/results/coverage/Engine/Engine_9_10.coverage", "fbb04a72-d19d-406f-8a65-f57f3668696c");
        final JexlInfo info = debug ? createInfo() : null;
        writeline("/home/ubuntu/results/coverage/Engine/Engine_9_10.coverage", "87497437-ba03-455c-bc2a-b9ff99247bc8");
        try {
            writeline("/home/ubuntu/results/coverage/Engine/Engine_9_10.coverage", "0ec10126-2caf-4065-bc23-cffae9954325");
            JexlMethod ctor = uberspect.getConstructor(clazz, args);
            writeline("/home/ubuntu/results/coverage/Engine/Engine_9_10.coverage", "fc6fe5b3-d599-4ffd-be5b-6c7abca8e2c6");
            if (ctor == null && arithmetic.narrowArguments(args)) {
                writeline("/home/ubuntu/results/coverage/Engine/Engine_9_10.coverage", "30e809ac-bd17-41a2-91a1-45b5d8b5c624");
                ctor = uberspect.getConstructor(clazz, args);
            }
            writeline("/home/ubuntu/results/coverage/Engine/Engine_9_10.coverage", "53c0da36-0030-4f70-b9b9-932b7e2bc33c");
            if (ctor != null) {
                writeline("/home/ubuntu/results/coverage/Engine/Engine_9_10.coverage", "6ac32288-63dc-4e39-be97-e3b2db60d53d");
                result = ctor.invoke(clazz, args);
            } else {
                writeline("/home/ubuntu/results/coverage/Engine/Engine_9_10.coverage", "473d4497-6f5c-476c-938c-54dd381c164e");
                xjexl = new JexlException.Method(info, clazz.toString(), null);
            }
        } catch (JexlException xany) {
            writeline("/home/ubuntu/results/coverage/Engine/Engine_9_10.coverage", "0ec4f488-a39e-4854-957c-fdc95dd14edc");
            xjexl = xany;
        } catch (Exception xany) {
            writeline("/home/ubuntu/results/coverage/Engine/Engine_9_10.coverage", "f890b9f8-e919-4c4c-84c6-9e3e23a3253c");
            xjexl = new JexlException.Method(info, clazz.toString(), xany);
        }
        writeline("/home/ubuntu/results/coverage/Engine/Engine_9_10.coverage", "62d1f923-e51d-45ea-9169-34023d2a43b8");
        if (xjexl != null) {
            writeline("/home/ubuntu/results/coverage/Engine/Engine_9_10.coverage", "80b53b4d-4d86-4612-9847-d0ad69d98ca5");
            if (silent) {
                writeline("/home/ubuntu/results/coverage/Engine/Engine_9_10.coverage", "1e0d841f-780b-436e-b00f-84fef95a48e6");
                logger.warn(xjexl.getMessage(), xjexl.getCause());
                writeline("/home/ubuntu/results/coverage/Engine/Engine_9_10.coverage", "7f09cd3c-71bd-40f1-b016-3c062a62edc9");
                return null;
            }
            writeline("/home/ubuntu/results/coverage/Engine/Engine_9_10.coverage", "c34abe6e-bb09-4a18-9128-b57aabcddc64");
            throw xjexl.clean();
        }
        writeline("/home/ubuntu/results/coverage/Engine/Engine_9_10.coverage", "0ef22757-b877-4671-aea3-5117eadb132d");
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
        writeline("/home/ubuntu/results/coverage/Engine/Engine_9_10.coverage", "58ec72c8-9bad-4de8-b50d-d877bb11f5cc");
        VarCollector collector = new VarCollector();
        writeline("/home/ubuntu/results/coverage/Engine/Engine_9_10.coverage", "1673fe61-8f56-4f74-9330-d64d163aaf9c");
        getVariables(script, script, collector);
        writeline("/home/ubuntu/results/coverage/Engine/Engine_9_10.coverage", "15bf1247-f2e6-49ac-9378-34540db6a8d4");
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
        writeline("/home/ubuntu/results/coverage/Engine/Engine_9_10.coverage", "f64a908a-fb2e-4501-a74c-aac5df1f224d");
        if (node instanceof ASTIdentifier) {
            writeline("/home/ubuntu/results/coverage/Engine/Engine_9_10.coverage", "0ef56649-b92f-410f-a546-3d2bfaba7743");
            JexlNode parent = node.jjtGetParent();
            writeline("/home/ubuntu/results/coverage/Engine/Engine_9_10.coverage", "4c33d19a-9253-44d1-a1a9-3458f20ce9bc");
            if (parent instanceof ASTMethodNode || parent instanceof ASTFunctionNode) {
                writeline("/home/ubuntu/results/coverage/Engine/Engine_9_10.coverage", "555e070a-12a8-4aa1-b3be-343177c8ce58");
                // skip identifiers for methods and functions
                collector.collect(null);
                writeline("/home/ubuntu/results/coverage/Engine/Engine_9_10.coverage", "594f83a2-6ace-46dc-a1ec-87ee314cbbcb");
                return;
            }
            writeline("/home/ubuntu/results/coverage/Engine/Engine_9_10.coverage", "dfb6b510-7011-4621-abcd-abdd36d26707");
            ASTIdentifier identifier = (ASTIdentifier) node;
            writeline("/home/ubuntu/results/coverage/Engine/Engine_9_10.coverage", "85028d90-37e3-4694-9b5f-606ecac02188");
            int symbol = identifier.getSymbol();
            writeline("/home/ubuntu/results/coverage/Engine/Engine_9_10.coverage", "a7806bbb-872b-4b7a-93c9-cfbe6d45c4c1");
            // symbols that are hoisted are considered "global" variables
            if (symbol >= 0 && script != null && !script.isHoistedSymbol(symbol)) {
                writeline("/home/ubuntu/results/coverage/Engine/Engine_9_10.coverage", "5e53e3b3-e01e-44f0-a144-8b8d77cba2cb");
                collector.collect(null);
            } else {
                writeline("/home/ubuntu/results/coverage/Engine/Engine_9_10.coverage", "40a26bf1-deec-4a20-9ab8-9639e8b11bd2");
                // start collecting from identifier
                collector.collect(identifier);
                writeline("/home/ubuntu/results/coverage/Engine/Engine_9_10.coverage", "876f3b57-ff9c-46c7-9c5c-4df657878ba4");
                collector.add(identifier.getName());
            }
        } else if (node instanceof ASTIdentifierAccess) {
            writeline("/home/ubuntu/results/coverage/Engine/Engine_9_10.coverage", "42fd9b20-85a2-4f1c-b2b4-1c0525e71268");
            JexlNode parent = node.jjtGetParent();
            writeline("/home/ubuntu/results/coverage/Engine/Engine_9_10.coverage", "d4ffd9f0-0e42-4888-af8c-c446ae194209");
            if (parent instanceof ASTMethodNode || parent instanceof ASTFunctionNode) {
                writeline("/home/ubuntu/results/coverage/Engine/Engine_9_10.coverage", "4a09bd11-d9a6-4183-8b90-81beb8227083");
                // skip identifiers for methods and functions
                collector.collect(null);
                writeline("/home/ubuntu/results/coverage/Engine/Engine_9_10.coverage", "80836831-8e41-4868-a4ea-8329b2b1aa84");
                return;
            }
            writeline("/home/ubuntu/results/coverage/Engine/Engine_9_10.coverage", "443bcb93-4ac2-4211-8249-9b6c80230d5d");
            // belt and suspender since an identifier should have been seen first
            if (collector.isCollecting()) {
                writeline("/home/ubuntu/results/coverage/Engine/Engine_9_10.coverage", "ae59af02-caea-42e3-b6f5-1ef88b0d3660");
                collector.add(((ASTIdentifierAccess) node).getName());
            }
        } else if (node instanceof ASTArrayAccess) {
            writeline("/home/ubuntu/results/coverage/Engine/Engine_9_10.coverage", "46bdd4a8-1951-47f7-ad75-4e0c19f39b7f");
            int num = node.jjtGetNumChildren();
            writeline("/home/ubuntu/results/coverage/Engine/Engine_9_10.coverage", "1493f837-3f5e-4ace-a3cb-b69d0e641f5e");
            // collect only if array access is const and follows an identifier
            boolean collecting = collector.isCollecting();
            writeline("/home/ubuntu/results/coverage/Engine/Engine_9_10.coverage", "0eaa60a1-7a40-438a-abf2-23ba5ab43532");
            for (int i = 0; i < num; ++i) {
                writeline("/home/ubuntu/results/coverage/Engine/Engine_9_10.coverage", "e46ab399-358a-4a6c-b9d3-dacdf20bc201");
                JexlNode child = node.jjtGetChild(i);
                writeline("/home/ubuntu/results/coverage/Engine/Engine_9_10.coverage", "9b39c272-f2e9-4490-a8ae-b7c5ae95e37e");
                if (collecting && child.isConstant()) {
                    writeline("/home/ubuntu/results/coverage/Engine/Engine_9_10.coverage", "1ce11268-f88c-4dfb-aaee-8399680456b2");
                    String image = child.toString();
                    writeline("/home/ubuntu/results/coverage/Engine/Engine_9_10.coverage", "0a02359e-40f6-4e90-9e35-40a6c5b8634d");
                    if (image == null) {
                        writeline("/home/ubuntu/results/coverage/Engine/Engine_9_10.coverage", "483a2023-b06c-4ef7-9fa5-47776ec9e6c9");
                        image = new Debugger().data(child);
                    }
                    writeline("/home/ubuntu/results/coverage/Engine/Engine_9_10.coverage", "9aea4375-5633-40d0-ae93-301e9b35a1d9");
                    collector.add(image);
                } else {
                    writeline("/home/ubuntu/results/coverage/Engine/Engine_9_10.coverage", "6371bfcf-abb3-4ceb-b1dd-0f0345abe0f5");
                    collecting = false;
                    writeline("/home/ubuntu/results/coverage/Engine/Engine_9_10.coverage", "d2f09d39-0ae9-472d-b9b5-e50b998b0c9d");
                    collector.collect(null);
                    writeline("/home/ubuntu/results/coverage/Engine/Engine_9_10.coverage", "19dba8f1-815c-472e-8761-46fbb24dd98b");
                    getVariables(script, child, collector);
                }
            }
        } else {
            writeline("/home/ubuntu/results/coverage/Engine/Engine_9_10.coverage", "7c79d465-8399-4438-8370-07f751a112d5");
            int num = node.jjtGetNumChildren();
            writeline("/home/ubuntu/results/coverage/Engine/Engine_9_10.coverage", "da717e07-55d9-466d-a725-19e05b17c3b5");
            for (int i = 0; i < num; ++i) {
                writeline("/home/ubuntu/results/coverage/Engine/Engine_9_10.coverage", "5af05ad3-e80a-468f-a16f-dddc1b1ebe83");
                getVariables(script, node.jjtGetChild(i), collector);
            }
            writeline("/home/ubuntu/results/coverage/Engine/Engine_9_10.coverage", "1cc1684c-624b-41af-b4d9-643688d7a0ca");
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
        writeline("/home/ubuntu/results/coverage/Engine/Engine_9_10.coverage", "e6fd0bc0-27ff-46a5-b0fd-ec4eb5d07810");
        return script.getParameters();
    }

    /**
     * Gets the array of local variable from a script.
     * @param script the script
     * @return the local variables array which may be empty (but not null) if no local variables were defined
     * @since 3.0
     */
    protected String[] getLocalVariables(JexlScript script) {
        writeline("/home/ubuntu/results/coverage/Engine/Engine_9_10.coverage", "56b52dce-83b5-4335-836c-45592be76e6e");
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
        writeline("/home/ubuntu/results/coverage/Engine/Engine_9_10.coverage", "5ed73c3e-12c2-40fe-81cf-c125eb8f4464");
        final boolean cached = src.length() < cacheThreshold && cache != null;
        writeline("/home/ubuntu/results/coverage/Engine/Engine_9_10.coverage", "b8c7ee13-3874-4cf0-86f6-a27ae2e4be5e");
        ASTJexlScript script;
        writeline("/home/ubuntu/results/coverage/Engine/Engine_9_10.coverage", "7c2495da-324b-4264-8218-0eb23e0ad649");
        synchronized (parser) {
            writeline("/home/ubuntu/results/coverage/Engine/Engine_9_10.coverage", "213cc2ea-daa6-487f-baec-c01ecef5f7a2");
            if (cached) {
                writeline("/home/ubuntu/results/coverage/Engine/Engine_9_10.coverage", "64446d2d-434d-4758-8ced-9f01c53bdd2d");
                script = cache.get(src);
                writeline("/home/ubuntu/results/coverage/Engine/Engine_9_10.coverage", "38e15d11-5e87-4048-a8af-429c30878030");
                if (script != null) {
                    writeline("/home/ubuntu/results/coverage/Engine/Engine_9_10.coverage", "a9aab9ba-079b-4894-9170-230accf1a2db");
                    Scope f = script.getScope();
                    writeline("/home/ubuntu/results/coverage/Engine/Engine_9_10.coverage", "448d21de-d468-4215-803d-ce14fa385f93");
                    if ((f == null && scope == null) || (f != null && f.equals(scope))) {
                        writeline("/home/ubuntu/results/coverage/Engine/Engine_9_10.coverage", "9f972068-2b70-4e88-b0af-649589e24833");
                        return script;
                    }
                }
            }
            writeline("/home/ubuntu/results/coverage/Engine/Engine_9_10.coverage", "20f8001f-ed0f-4c6f-b4fd-01b06034becc");
            script = parser.parse(info, src, scope, registers, expression);
            writeline("/home/ubuntu/results/coverage/Engine/Engine_9_10.coverage", "003c1b2d-f9fd-4f50-ad6b-f57bafe37206");
            if (cached) {
                writeline("/home/ubuntu/results/coverage/Engine/Engine_9_10.coverage", "78b3a8aa-ec0e-41e7-a72b-0577d31568b0");
                cache.put(src, script);
            }
        }
        writeline("/home/ubuntu/results/coverage/Engine/Engine_9_10.coverage", "e98bbe3d-4258-4b80-842e-488e8446eb97");
        return script;
    }

    /**
     * Trims the source from front and ending spaces.
     * @param str expression to clean
     * @return trimmed expression ending in a semi-colon
     */
    protected String trimSource(CharSequence str) {
        writeline("/home/ubuntu/results/coverage/Engine/Engine_9_10.coverage", "37fbcd8f-ba8f-43d8-9c6e-068e5536dc85");
        if (str != null) {
            writeline("/home/ubuntu/results/coverage/Engine/Engine_9_10.coverage", "c09fc3b1-3c74-4c74-85bf-66120af77a86");
            int start = 0;
            writeline("/home/ubuntu/results/coverage/Engine/Engine_9_10.coverage", "f1a28aec-a940-4980-a2f2-b447c6f029a7");
            int end = str.length();
            writeline("/home/ubuntu/results/coverage/Engine/Engine_9_10.coverage", "eddf978e-ae62-49b6-9fcc-0e29cdff94f0");
            if (end > 0) {
                writeline("/home/ubuntu/results/coverage/Engine/Engine_9_10.coverage", "5f091814-eeb7-4774-b550-c627244b27fa");
                // trim front spaces
                while (start < end && Character.isSpaceChar(str.charAt(start))) {
                    writeline("/home/ubuntu/results/coverage/Engine/Engine_9_10.coverage", "ba5080b6-ecc2-467b-add3-ef2c45751701");
                    ++start;
                }
                writeline("/home/ubuntu/results/coverage/Engine/Engine_9_10.coverage", "bec73428-52e4-46d2-bc04-c176a2602f98");
                // trim ending spaces
                while (end > 0 && Character.isSpaceChar(str.charAt(end - 1))) {
                    writeline("/home/ubuntu/results/coverage/Engine/Engine_9_10.coverage", "6ba7992c-437d-4f04-a2df-c943051e9d99");
                    --end;
                }
                writeline("/home/ubuntu/results/coverage/Engine/Engine_9_10.coverage", "e5035de0-b0de-493d-9200-12eb530e9c17");
                return str.subSequence(start, end).toString();
            }
            writeline("/home/ubuntu/results/coverage/Engine/Engine_9_10.coverage", "b56291c0-9369-4b18-999d-f1927a9dd6b0");
            return "";
        }
        writeline("/home/ubuntu/results/coverage/Engine/Engine_9_10.coverage", "ec390db3-1588-4566-9fa5-3b84afe4b1d7");
        return null;
    }

    /**
     * Gets and/or creates a default template engine.
     * @return a template engine
     */
    protected TemplateEngine jxlt() {
        writeline("/home/ubuntu/results/coverage/Engine/Engine_9_10.coverage", "ba1c1432-66e0-45f2-a111-c33573183116");
        TemplateEngine e = jxlt;
        writeline("/home/ubuntu/results/coverage/Engine/Engine_9_10.coverage", "deab4e04-fc1b-4ee6-ac7f-90226776608f");
        if (e == null) {
            writeline("/home/ubuntu/results/coverage/Engine/Engine_9_10.coverage", "a248eafc-cf67-472e-80a6-efd8e618838c");
            synchronized (this) {
                writeline("/home/ubuntu/results/coverage/Engine/Engine_9_10.coverage", "346b406d-c94b-45c3-8f29-90a456d47d75");
                if (jxlt == null) {
                    writeline("/home/ubuntu/results/coverage/Engine/Engine_9_10.coverage", "7f3e58bb-f22f-402d-a4bc-79fa87a09086");
                    e = new TemplateEngine(this, true, 0, '$', '#');
                    writeline("/home/ubuntu/results/coverage/Engine/Engine_9_10.coverage", "553804c1-fac6-4a22-a87a-65873b329ec1");
                    jxlt = e;
                }
            }
        }
        writeline("/home/ubuntu/results/coverage/Engine/Engine_9_10.coverage", "fb6f2c4e-f508-49ac-9ef2-d373ba99c939");
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
