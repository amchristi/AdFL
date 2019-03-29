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
        writelineStatic("/home/ubuntu/results/coverage/Engine/Engine_4_10.coverage", "671a59ab-6b5f-4d6f-b8c6-e1e65b3e3026");
        if ((logger == null || logger.equals(LogFactory.getLog(JexlEngine.class))) && (strategy == null || strategy == JexlUberspect.JEXL_STRATEGY)) {
            writelineStatic("/home/ubuntu/results/coverage/Engine/Engine_4_10.coverage", "03b6f69c-95de-42c6-8d4a-4a8cf7cf5f5e");
            return UberspectHolder.UBERSPECT;
        }
        writelineStatic("/home/ubuntu/results/coverage/Engine/Engine_4_10.coverage", "5f43408d-35e4-48b4-9985-b4d56d8b6ce7");
        return new Uberspect(logger, strategy);
    }

    @Override
    public JexlUberspect getUberspect() {
        writeline("/home/ubuntu/results/coverage/Engine/Engine_4_10.coverage", "1739b9a4-6b5a-417e-b032-304619d04fbd");
        return uberspect;
    }

    @Override
    public JexlArithmetic getArithmetic() {
        writeline("/home/ubuntu/results/coverage/Engine/Engine_4_10.coverage", "2dea7f3f-06df-4dd4-8869-01b5e4a079b6");
        return arithmetic;
    }

    @Override
    public boolean isDebug() {
        writeline("/home/ubuntu/results/coverage/Engine/Engine_4_10.coverage", "722d3d66-9867-4609-9d36-4f93c3217775");
        return this.debug;
    }

    @Override
    public boolean isSilent() {
        writeline("/home/ubuntu/results/coverage/Engine/Engine_4_10.coverage", "816ffa1b-5fb7-4556-b29e-0496e75b4f99");
        return this.silent;
    }

    @Override
    public boolean isStrict() {
        writeline("/home/ubuntu/results/coverage/Engine/Engine_4_10.coverage", "8405e477-91cc-4fb3-be4a-7e1b832bbd89");
        return strict;
    }

    @Override
    public void setClassLoader(ClassLoader loader) {
        writeline("/home/ubuntu/results/coverage/Engine/Engine_4_10.coverage", "906712ed-dbd4-4f78-a644-e0a2d8044c56");
        uberspect.setClassLoader(loader);
    }

    @Override
    public Charset getCharset() {
        writeline("/home/ubuntu/results/coverage/Engine/Engine_4_10.coverage", "1dc059a3-fa9b-455e-bc93-44ec67ebf48f");
        return charset;
    }

    @Override
    public TemplateEngine createJxltEngine(boolean noScript, int cacheSize, char immediate, char deferred) {
        writeline("/home/ubuntu/results/coverage/Engine/Engine_4_10.coverage", "8532412a-11e1-4f15-a999-7d3f7d7d338a");
        return new TemplateEngine(this, noScript, cacheSize, immediate, deferred);
    }

    /**
     * Swaps the current thread local context.
     * @param tls the context or null
     * @return the previous thread local context
     */
    protected JexlContext.ThreadLocal putThreadLocal(JexlContext.ThreadLocal tls) {
        writeline("/home/ubuntu/results/coverage/Engine/Engine_4_10.coverage", "5fc3d98f-ed85-4e62-8ed5-6178b5256b2c");
        JexlContext.ThreadLocal local = CONTEXT.get();
        writeline("/home/ubuntu/results/coverage/Engine/Engine_4_10.coverage", "845fd398-0f76-44d6-b1c8-b6ac495253ea");
        CONTEXT.set(tls);
        writeline("/home/ubuntu/results/coverage/Engine/Engine_4_10.coverage", "3a859085-987e-47bb-8a2b-12e48146731e");
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
        writeline("/home/ubuntu/results/coverage/Engine/Engine_4_10.coverage", "89bdebba-a439-4e61-900e-56e9391ece48");
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
        writeline("/home/ubuntu/results/coverage/Engine/Engine_4_10.coverage", "656d58c9-3d9c-43c0-9585-38dc0d8c493c");
        synchronized (parser) {
            writeline("/home/ubuntu/results/coverage/Engine/Engine_4_10.coverage", "9eb008c4-fd8a-48fa-b846-9afed394a4a2");
            if (cache != null) {
                writeline("/home/ubuntu/results/coverage/Engine/Engine_4_10.coverage", "925b8c68-8ac9-4672-b3ab-c02e395470cc");
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
        writeline("/home/ubuntu/results/coverage/Engine/Engine_4_10.coverage", "5cc11fd8-8a89-476f-9395-ec563c90eedf");
        return new Interpreter(this, context, frame);
    }

    @Override
    public Script createScript(JexlInfo info, String scriptText, String[] names) {
        writeline("/home/ubuntu/results/coverage/Engine/Engine_4_10.coverage", "8071a13e-f2e5-483a-9558-c7f3b8a1db55");
        if (scriptText == null) {
            writeline("/home/ubuntu/results/coverage/Engine/Engine_4_10.coverage", "18894238-a098-4981-90f2-6040e13c5e2b");
            throw new NullPointerException("source is null");
        }
        writeline("/home/ubuntu/results/coverage/Engine/Engine_4_10.coverage", "e79a222c-04d7-4b3b-833e-7ba1ec587c23");
        if (info == null && debug) {
            writeline("/home/ubuntu/results/coverage/Engine/Engine_4_10.coverage", "f7dca0ec-9f3c-4919-ac96-6174d4b7be3c");
            info = createInfo();
        }
        writeline("/home/ubuntu/results/coverage/Engine/Engine_4_10.coverage", "39e8ce7c-ea7a-4161-b339-0c4272452f74");
        String source = trimSource(scriptText);
        writeline("/home/ubuntu/results/coverage/Engine/Engine_4_10.coverage", "448e9c30-e66b-40ac-a216-1e9c3316a80d");
        Scope scope = names == null ? null : new Scope(null, names);
        writeline("/home/ubuntu/results/coverage/Engine/Engine_4_10.coverage", "9d29f2c7-850d-404b-89ba-64a441c0a25f");
        ASTJexlScript tree = parse(info, source, scope, false, false);
        writeline("/home/ubuntu/results/coverage/Engine/Engine_4_10.coverage", "a7313d48-cfd9-42a0-99c0-84d97940562c");
        return new Script(this, source, tree);
    }

    @Override
    public Script createExpression(JexlInfo info, String expression) {
        writeline("/home/ubuntu/results/coverage/Engine/Engine_4_10.coverage", "a4f072cb-19db-49ad-af17-1b72f74c2dcc");
        if (expression == null) {
            writeline("/home/ubuntu/results/coverage/Engine/Engine_4_10.coverage", "244624a7-1a06-424e-92ce-9a2c48435660");
            throw new NullPointerException("source is null");
        }
        writeline("/home/ubuntu/results/coverage/Engine/Engine_4_10.coverage", "36f48b78-bbed-4b83-a812-f23ad4850d86");
        if (info == null && debug) {
            writeline("/home/ubuntu/results/coverage/Engine/Engine_4_10.coverage", "cda7fd5b-9274-4b37-8791-3c595c85b522");
            info = createInfo();
        }
        writeline("/home/ubuntu/results/coverage/Engine/Engine_4_10.coverage", "6702a853-9a8a-40a0-8cb0-a0e2bf7f8d79");
        String source = trimSource(expression);
        writeline("/home/ubuntu/results/coverage/Engine/Engine_4_10.coverage", "afe9b7ef-7daa-4c35-8e67-13cb927f757e");
        ASTJexlScript tree = parse(info, source, null, false, true);
        writeline("/home/ubuntu/results/coverage/Engine/Engine_4_10.coverage", "2fc97aff-e1c9-4986-bf07-c1ab951a1452");
        return new Script(this, source, tree);
    }

    @Override
    public Object getProperty(Object bean, String expr) {
        writeline("/home/ubuntu/results/coverage/Engine/Engine_4_10.coverage", "19650345-55f1-4038-bf5c-b3b64fea5f6f");
        return getProperty(null, bean, expr);
    }

    @Override
    public Object getProperty(JexlContext context, Object bean, String expr) {
        writeline("/home/ubuntu/results/coverage/Engine/Engine_4_10.coverage", "acf847f4-0522-41b3-a52e-ef6461f55d2e");
        if (context == null) {
            writeline("/home/ubuntu/results/coverage/Engine/Engine_4_10.coverage", "ab177a15-f323-4f3d-9eeb-6d4571ed8f69");
            context = EMPTY_CONTEXT;
        }
        writeline("/home/ubuntu/results/coverage/Engine/Engine_4_10.coverage", "4728ef48-645e-43d2-9078-8a1e60de2d3d");
        // synthetize expr using register
        String src = trimSource(expr);
        writeline("/home/ubuntu/results/coverage/Engine/Engine_4_10.coverage", "33158a2d-2c4f-4a29-971e-26f813e1f978");
        src = "#0" + (src.charAt(0) == '[' ? "" : ".") + src;
        writeline("/home/ubuntu/results/coverage/Engine/Engine_4_10.coverage", "8dc62498-28ca-49e0-9ebc-0397c2149043");
        try {
            writeline("/home/ubuntu/results/coverage/Engine/Engine_4_10.coverage", "b9f5f028-1688-4809-a66e-56f7d7fd6d85");
            final JexlInfo info = debug ? createInfo() : null;
            writeline("/home/ubuntu/results/coverage/Engine/Engine_4_10.coverage", "8f8b5d16-7785-4b01-be14-c4eb0bfd1321");
            final Scope scope = new Scope(null, "#0");
            writeline("/home/ubuntu/results/coverage/Engine/Engine_4_10.coverage", "7ae71bd1-0703-404a-b27c-c08bc112df18");
            final ASTJexlScript script = parse(info, src, scope, true, true);
            writeline("/home/ubuntu/results/coverage/Engine/Engine_4_10.coverage", "fd41c990-6832-491a-8c47-c4dceae63cd1");
            final JexlNode node = script.jjtGetChild(0);
            writeline("/home/ubuntu/results/coverage/Engine/Engine_4_10.coverage", "9c17c105-b582-447a-838a-93ba2424a999");
            final Scope.Frame frame = script.createFrame(bean);
            writeline("/home/ubuntu/results/coverage/Engine/Engine_4_10.coverage", "ddd0a795-c92e-4e0f-9205-0633ae019271");
            final Interpreter interpreter = createInterpreter(context, frame);
            writeline("/home/ubuntu/results/coverage/Engine/Engine_4_10.coverage", "778c6e15-240a-4d3e-a1c4-f1b788903e22");
            return node.jjtAccept(interpreter, null);
        } catch (JexlException xjexl) {
            writeline("/home/ubuntu/results/coverage/Engine/Engine_4_10.coverage", "6983cfe2-ecc6-4f16-baae-042617a0bcfc");
            if (silent) {
                writeline("/home/ubuntu/results/coverage/Engine/Engine_4_10.coverage", "e8607810-8af6-4cfe-a209-dc072871e63d");
                logger.warn(xjexl.getMessage(), xjexl.getCause());
                writeline("/home/ubuntu/results/coverage/Engine/Engine_4_10.coverage", "341a1ee4-61e7-42a2-981b-21cac16a9ad7");
                return null;
            }
            writeline("/home/ubuntu/results/coverage/Engine/Engine_4_10.coverage", "e4d2f71e-aa94-4ddb-99f9-6c96bc0591d0");
            throw xjexl.clean();
        }
    }

    @Override
    public void setProperty(Object bean, String expr, Object value) {
        writeline("/home/ubuntu/results/coverage/Engine/Engine_4_10.coverage", "6fd36d99-a56e-41f8-ac49-b205588822bb");
        setProperty(null, bean, expr, value);
    }

    @Override
    public void setProperty(JexlContext context, Object bean, String expr, Object value) {
        writeline("/home/ubuntu/results/coverage/Engine/Engine_4_10.coverage", "b2b56c7b-74dc-4aab-ae3c-c7d7f699286d");
        if (context == null) {
            writeline("/home/ubuntu/results/coverage/Engine/Engine_4_10.coverage", "c9f5d058-2b99-4eec-9360-e4619344f9cc");
            context = EMPTY_CONTEXT;
        }
        writeline("/home/ubuntu/results/coverage/Engine/Engine_4_10.coverage", "a96f34e4-91d0-4f74-838e-d786e1f695e9");
        // synthetize expr using registers
        String src = trimSource(expr);
        writeline("/home/ubuntu/results/coverage/Engine/Engine_4_10.coverage", "b6a8e80d-f17d-4d38-b46c-8c2adf90ae58");
        src = "#0" + (src.charAt(0) == '[' ? "" : ".") + src + "=" + "#1";
        writeline("/home/ubuntu/results/coverage/Engine/Engine_4_10.coverage", "4a2bf973-a7e7-4f4d-bbf2-0d12bb22100f");
        try {
            writeline("/home/ubuntu/results/coverage/Engine/Engine_4_10.coverage", "0f45d6cf-d75b-419f-a8cd-6d6ab14f8107");
            final JexlInfo info = debug ? createInfo() : null;
            writeline("/home/ubuntu/results/coverage/Engine/Engine_4_10.coverage", "7132971b-b88d-434c-a8a6-13238b4640ef");
            final Scope scope = new Scope(null, "#0", "#1");
            writeline("/home/ubuntu/results/coverage/Engine/Engine_4_10.coverage", "c2b6b6f2-1778-4e1a-b0b7-0b52f072fa9f");
            final ASTJexlScript script = parse(info, src, scope, true, true);
            writeline("/home/ubuntu/results/coverage/Engine/Engine_4_10.coverage", "bd4c9fe4-5afa-4fbd-ab88-fbe3494a3341");
            final JexlNode node = script.jjtGetChild(0);
            writeline("/home/ubuntu/results/coverage/Engine/Engine_4_10.coverage", "28871fd8-0032-4b48-bd3e-eb99f810d946");
            final Scope.Frame frame = script.createFrame(bean, value);
            writeline("/home/ubuntu/results/coverage/Engine/Engine_4_10.coverage", "67e838af-301f-4bfa-89a5-1e6eae75de5d");
            final Interpreter interpreter = createInterpreter(context, frame);
            writeline("/home/ubuntu/results/coverage/Engine/Engine_4_10.coverage", "3a6768c9-f416-419d-b94b-0a5a9d304874");
            node.jjtAccept(interpreter, null);
        } catch (JexlException xjexl) {
            writeline("/home/ubuntu/results/coverage/Engine/Engine_4_10.coverage", "4a7a8534-05d1-42f9-9fc0-34f0e368ad9c");
            if (silent) {
                writeline("/home/ubuntu/results/coverage/Engine/Engine_4_10.coverage", "7e2bbeba-a898-4604-be51-a5c9998b61a3");
                logger.warn(xjexl.getMessage(), xjexl.getCause());
                writeline("/home/ubuntu/results/coverage/Engine/Engine_4_10.coverage", "59537602-cc8e-44f0-a8f0-9818207df6a5");
                return;
            }
            writeline("/home/ubuntu/results/coverage/Engine/Engine_4_10.coverage", "bd19eb59-b874-47a1-8c80-decd51c91f1c");
            throw xjexl.clean();
        }
    }

    @Override
    public Object invokeMethod(Object obj, String meth, Object... args) {
        writeline("/home/ubuntu/results/coverage/Engine/Engine_4_10.coverage", "f4fbabfe-29db-4764-9ecd-00cf7dd43199");
        JexlException xjexl = null;
        writeline("/home/ubuntu/results/coverage/Engine/Engine_4_10.coverage", "4c5643f2-08eb-437f-8975-535f21d7bf7b");
        Object result = null;
        writeline("/home/ubuntu/results/coverage/Engine/Engine_4_10.coverage", "14b0dbb8-f4dd-4b40-ac85-9bb4737b31be");
        final JexlInfo info = debug ? createInfo() : null;
        writeline("/home/ubuntu/results/coverage/Engine/Engine_4_10.coverage", "fac32252-f665-43b2-88e5-c2c83d4c1f3c");
        try {
            writeline("/home/ubuntu/results/coverage/Engine/Engine_4_10.coverage", "db76c240-b36b-4e7a-a22c-80ad1b7ce606");
            JexlMethod method = uberspect.getMethod(obj, meth, args);
            writeline("/home/ubuntu/results/coverage/Engine/Engine_4_10.coverage", "9e88bfe1-e7b1-4313-8885-378901e3b753");
            if (method == null && arithmetic.narrowArguments(args)) {
                writeline("/home/ubuntu/results/coverage/Engine/Engine_4_10.coverage", "4a0665b9-e4cd-4cb5-8417-46db5f5ec0d2");
                method = uberspect.getMethod(obj, meth, args);
            }
            writeline("/home/ubuntu/results/coverage/Engine/Engine_4_10.coverage", "aa084a6d-1178-464b-bf53-d6020cfa8099");
            if (method != null) {
                writeline("/home/ubuntu/results/coverage/Engine/Engine_4_10.coverage", "af8422ab-6e39-44f9-bd51-6fc1c9a9fb56");
                result = method.invoke(obj, args);
            } else {
                writeline("/home/ubuntu/results/coverage/Engine/Engine_4_10.coverage", "57af1dce-3011-4b1a-b33a-12e6b825248d");
                xjexl = new JexlException.Method(info, meth, null);
            }
        } catch (JexlException xany) {
            writeline("/home/ubuntu/results/coverage/Engine/Engine_4_10.coverage", "d3e95831-bc58-4ee6-ac98-16ccde687f79");
            xjexl = xany;
        } catch (Exception xany) {
            writeline("/home/ubuntu/results/coverage/Engine/Engine_4_10.coverage", "7ea24ce4-d2a2-444e-89dc-3199c40b6f2f");
            xjexl = new JexlException.Method(info, meth, xany);
        }
        writeline("/home/ubuntu/results/coverage/Engine/Engine_4_10.coverage", "b9894425-2c26-473b-96b5-670688e209e3");
        if (xjexl != null) {
            writeline("/home/ubuntu/results/coverage/Engine/Engine_4_10.coverage", "55fda77a-1c09-42d2-93d0-f5350d42d3f1");
            if (silent) {
                writeline("/home/ubuntu/results/coverage/Engine/Engine_4_10.coverage", "ad165d71-ad5a-44e6-ab4d-c698bde65d56");
                logger.warn(xjexl.getMessage(), xjexl.getCause());
                writeline("/home/ubuntu/results/coverage/Engine/Engine_4_10.coverage", "75834825-d2f4-4531-827c-a67920237bfb");
                result = null;
            } else {
                writeline("/home/ubuntu/results/coverage/Engine/Engine_4_10.coverage", "2b7e2914-34d9-4d98-a9b1-38847975261f");
                throw xjexl.clean();
            }
        }
        writeline("/home/ubuntu/results/coverage/Engine/Engine_4_10.coverage", "65680bd0-3198-473d-8193-2a66b94c2006");
        return result;
    }

    @Override
    public <T> T newInstance(Class<? extends T> clazz, Object... args) {
        writeline("/home/ubuntu/results/coverage/Engine/Engine_4_10.coverage", "f23f65ef-8996-43f4-a087-9bc616681c1f");
        return clazz.cast(doCreateInstance(clazz, args));
    }

    @Override
    public Object newInstance(String clazz, Object... args) {
        writeline("/home/ubuntu/results/coverage/Engine/Engine_4_10.coverage", "0f4ea212-3837-4f84-be84-e76c7a0c7f06");
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
        writeline("/home/ubuntu/results/coverage/Engine/Engine_4_10.coverage", "d0aaa5c6-9418-4af2-9d13-64f498041a2d");
        JexlException xjexl = null;
        writeline("/home/ubuntu/results/coverage/Engine/Engine_4_10.coverage", "cbd40c0f-cb37-4fb3-98f9-ffa6b8672215");
        Object result = null;
        writeline("/home/ubuntu/results/coverage/Engine/Engine_4_10.coverage", "2062fddb-5e12-47f0-96df-b80789eae1e6");
        final JexlInfo info = debug ? createInfo() : null;
        writeline("/home/ubuntu/results/coverage/Engine/Engine_4_10.coverage", "ec0ec5d3-9c4b-4244-a6f6-660245fa1ea6");
        try {
            writeline("/home/ubuntu/results/coverage/Engine/Engine_4_10.coverage", "7b0171c2-eec1-4e67-a666-9f0705a9c92e");
            JexlMethod ctor = uberspect.getConstructor(clazz, args);
            writeline("/home/ubuntu/results/coverage/Engine/Engine_4_10.coverage", "699a01ff-7494-42fa-990d-46138d6f6204");
            if (ctor == null && arithmetic.narrowArguments(args)) {
                writeline("/home/ubuntu/results/coverage/Engine/Engine_4_10.coverage", "96c9358e-e72b-42a2-8bbc-7527a317763e");
                ctor = uberspect.getConstructor(clazz, args);
            }
            writeline("/home/ubuntu/results/coverage/Engine/Engine_4_10.coverage", "f691d619-9f53-4ee2-be86-a3ba3c8c7706");
            if (ctor != null) {
                writeline("/home/ubuntu/results/coverage/Engine/Engine_4_10.coverage", "9dc93347-7e45-4090-a73f-d79b958a5020");
                result = ctor.invoke(clazz, args);
            } else {
                writeline("/home/ubuntu/results/coverage/Engine/Engine_4_10.coverage", "c6a3c3d4-7858-4b1e-9cd9-45e80116939b");
                xjexl = new JexlException.Method(info, clazz.toString(), null);
            }
        } catch (JexlException xany) {
            writeline("/home/ubuntu/results/coverage/Engine/Engine_4_10.coverage", "993d324c-710a-4b91-a564-37c936e115e8");
            xjexl = xany;
        } catch (Exception xany) {
            writeline("/home/ubuntu/results/coverage/Engine/Engine_4_10.coverage", "d6d0bf65-9196-465f-8561-1f6640d889c7");
            xjexl = new JexlException.Method(info, clazz.toString(), xany);
        }
        writeline("/home/ubuntu/results/coverage/Engine/Engine_4_10.coverage", "0d60418d-63e5-430f-bd7e-8da74dfa22ed");
        if (xjexl != null) {
            writeline("/home/ubuntu/results/coverage/Engine/Engine_4_10.coverage", "ce1fc13c-89ce-4ab6-9312-2ac3f3a35ead");
            if (silent) {
                writeline("/home/ubuntu/results/coverage/Engine/Engine_4_10.coverage", "364f36a7-a765-4519-9926-dd0fcbe60d35");
                logger.warn(xjexl.getMessage(), xjexl.getCause());
                writeline("/home/ubuntu/results/coverage/Engine/Engine_4_10.coverage", "1f28f3e1-95ff-485f-8bc2-e47279343a0b");
                return null;
            }
            writeline("/home/ubuntu/results/coverage/Engine/Engine_4_10.coverage", "9b756495-ff8e-49b0-a2d3-35b38d4059c7");
            throw xjexl.clean();
        }
        writeline("/home/ubuntu/results/coverage/Engine/Engine_4_10.coverage", "3e594acf-8741-4eb6-ae5e-e5f16f13dce1");
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
        writeline("/home/ubuntu/results/coverage/Engine/Engine_4_10.coverage", "42120bd1-ad5b-4171-b29f-664cc540dc40");
        VarCollector collector = new VarCollector();
        writeline("/home/ubuntu/results/coverage/Engine/Engine_4_10.coverage", "747e15ce-9953-4c18-a4b3-56cf32437979");
        getVariables(script, script, collector);
        writeline("/home/ubuntu/results/coverage/Engine/Engine_4_10.coverage", "49c7e348-3b22-4f53-b03e-8952b9f53518");
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
        writeline("/home/ubuntu/results/coverage/Engine/Engine_4_10.coverage", "89bfe7b6-e6a5-422e-8c04-b13971337db0");
        if (node instanceof ASTIdentifier) {
            writeline("/home/ubuntu/results/coverage/Engine/Engine_4_10.coverage", "10a5abdb-8c90-478b-8f63-f3d5a5e2e233");
            JexlNode parent = node.jjtGetParent();
            writeline("/home/ubuntu/results/coverage/Engine/Engine_4_10.coverage", "4f25f336-603a-4b19-a28d-4f7ebcdafdc8");
            if (parent instanceof ASTMethodNode || parent instanceof ASTFunctionNode) {
                writeline("/home/ubuntu/results/coverage/Engine/Engine_4_10.coverage", "3017d5b6-2de7-42f2-ad22-7c75f9b55577");
                // skip identifiers for methods and functions
                collector.collect(null);
                writeline("/home/ubuntu/results/coverage/Engine/Engine_4_10.coverage", "540c6208-07e9-4b45-8ba2-2bedb5af1283");
                return;
            }
            writeline("/home/ubuntu/results/coverage/Engine/Engine_4_10.coverage", "21e21dd7-4a1e-4e11-afb8-1f5e9b22a05c");
            ASTIdentifier identifier = (ASTIdentifier) node;
            writeline("/home/ubuntu/results/coverage/Engine/Engine_4_10.coverage", "e1992c5e-5bcf-46dd-b888-0011ca3f942f");
            int symbol = identifier.getSymbol();
            writeline("/home/ubuntu/results/coverage/Engine/Engine_4_10.coverage", "8c87fc9c-dac6-4fe4-b449-d61923aa6fad");
            // symbols that are hoisted are considered "global" variables
            if (symbol >= 0 && script != null && !script.isHoistedSymbol(symbol)) {
                writeline("/home/ubuntu/results/coverage/Engine/Engine_4_10.coverage", "38cff98a-0428-49bc-b834-2beafa0415de");
                collector.collect(null);
            } else {
                writeline("/home/ubuntu/results/coverage/Engine/Engine_4_10.coverage", "fd4490d1-b7ae-43bf-800b-bb8c7ab752ec");
                // start collecting from identifier
                collector.collect(identifier);
                writeline("/home/ubuntu/results/coverage/Engine/Engine_4_10.coverage", "25304f4c-6c0c-419d-991e-462bcc17a0cf");
                collector.add(identifier.getName());
            }
        } else if (node instanceof ASTIdentifierAccess) {
            writeline("/home/ubuntu/results/coverage/Engine/Engine_4_10.coverage", "cb08eff4-daa1-4125-8c03-5e696a24467e");
            JexlNode parent = node.jjtGetParent();
            writeline("/home/ubuntu/results/coverage/Engine/Engine_4_10.coverage", "3f0b2bf7-6c38-455c-a19c-7b5c2c144ea4");
            if (parent instanceof ASTMethodNode || parent instanceof ASTFunctionNode) {
                writeline("/home/ubuntu/results/coverage/Engine/Engine_4_10.coverage", "6e52970f-9e5b-4253-af28-1cd6b087f563");
                // skip identifiers for methods and functions
                collector.collect(null);
                writeline("/home/ubuntu/results/coverage/Engine/Engine_4_10.coverage", "d7c92349-84d6-4e13-b37d-a2aa9ad1abf2");
                return;
            }
            writeline("/home/ubuntu/results/coverage/Engine/Engine_4_10.coverage", "b1254513-17fa-48d1-b3fc-24690047c091");
            // belt and suspender since an identifier should have been seen first
            if (collector.isCollecting()) {
                writeline("/home/ubuntu/results/coverage/Engine/Engine_4_10.coverage", "f84a8123-afc2-40d9-9a92-9b77629ca90e");
                collector.add(((ASTIdentifierAccess) node).getName());
            }
        } else if (node instanceof ASTArrayAccess) {
            writeline("/home/ubuntu/results/coverage/Engine/Engine_4_10.coverage", "75115efc-c31d-4989-bb01-daa31a7ea288");
            int num = node.jjtGetNumChildren();
            writeline("/home/ubuntu/results/coverage/Engine/Engine_4_10.coverage", "8e4cf8b2-196c-428f-a21f-c83139da9f4d");
            // collect only if array access is const and follows an identifier
            boolean collecting = collector.isCollecting();
            writeline("/home/ubuntu/results/coverage/Engine/Engine_4_10.coverage", "dc5e1919-fe47-4488-a235-8eabc17bdd61");
            for (int i = 0; i < num; ++i) {
                writeline("/home/ubuntu/results/coverage/Engine/Engine_4_10.coverage", "d3172ad5-c756-4167-b32d-5c55575ebf2e");
                JexlNode child = node.jjtGetChild(i);
                writeline("/home/ubuntu/results/coverage/Engine/Engine_4_10.coverage", "ae7b4cc5-a3e3-4031-a5b2-59d59ef887ed");
                if (collecting && child.isConstant()) {
                    writeline("/home/ubuntu/results/coverage/Engine/Engine_4_10.coverage", "8c2ed3c6-29e4-450d-849a-df39731eff7b");
                    String image = child.toString();
                    writeline("/home/ubuntu/results/coverage/Engine/Engine_4_10.coverage", "b2be820f-5004-4ce1-b096-0bf7c7230d93");
                    if (image == null) {
                        writeline("/home/ubuntu/results/coverage/Engine/Engine_4_10.coverage", "8ac3fc9d-6e70-42b5-a56d-b45db1cb6a5a");
                        image = new Debugger().data(child);
                    }
                    writeline("/home/ubuntu/results/coverage/Engine/Engine_4_10.coverage", "e3101b14-0b14-4cf3-addb-cb152e3b8314");
                    collector.add(image);
                } else {
                    writeline("/home/ubuntu/results/coverage/Engine/Engine_4_10.coverage", "4d00e579-e80d-486e-bd1c-0d464710b683");
                    collecting = false;
                    writeline("/home/ubuntu/results/coverage/Engine/Engine_4_10.coverage", "3e0da5ba-5542-4858-8e16-764d757775df");
                    collector.collect(null);
                    writeline("/home/ubuntu/results/coverage/Engine/Engine_4_10.coverage", "57589622-cede-4593-812c-4e27b6603cb5");
                    getVariables(script, child, collector);
                }
            }
        } else {
            writeline("/home/ubuntu/results/coverage/Engine/Engine_4_10.coverage", "1d3f8a59-4530-47c2-8bc3-309b77acb718");
            int num = node.jjtGetNumChildren();
            writeline("/home/ubuntu/results/coverage/Engine/Engine_4_10.coverage", "0a58f308-95a1-4511-9db9-d13f9db22376");
            for (int i = 0; i < num; ++i) {
                writeline("/home/ubuntu/results/coverage/Engine/Engine_4_10.coverage", "f52c65c6-2773-4aab-a225-807a6d55b17a");
                getVariables(script, node.jjtGetChild(i), collector);
            }
            writeline("/home/ubuntu/results/coverage/Engine/Engine_4_10.coverage", "b2c7ef95-5eba-4e26-b1eb-1d4c8140965f");
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
        writeline("/home/ubuntu/results/coverage/Engine/Engine_4_10.coverage", "d55b84f8-af46-4f60-92ed-68484c861c3d");
        return script.getParameters();
    }

    /**
     * Gets the array of local variable from a script.
     * @param script the script
     * @return the local variables array which may be empty (but not null) if no local variables were defined
     * @since 3.0
     */
    protected String[] getLocalVariables(JexlScript script) {
        writeline("/home/ubuntu/results/coverage/Engine/Engine_4_10.coverage", "8187ca27-39d7-47ad-ab4c-93c6136a501f");
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
        writeline("/home/ubuntu/results/coverage/Engine/Engine_4_10.coverage", "37da1712-0e84-43fe-b5d4-0f7944a62983");
        final boolean cached = src.length() < cacheThreshold && cache != null;
        writeline("/home/ubuntu/results/coverage/Engine/Engine_4_10.coverage", "e137c783-99d5-44b4-9547-83996de27b7f");
        ASTJexlScript script;
        writeline("/home/ubuntu/results/coverage/Engine/Engine_4_10.coverage", "9adb98e5-9383-432f-95e9-8477e4e7b213");
        synchronized (parser) {
            writeline("/home/ubuntu/results/coverage/Engine/Engine_4_10.coverage", "e428ff1c-8403-4303-bb1e-9aef33b47d91");
            if (cached) {
                writeline("/home/ubuntu/results/coverage/Engine/Engine_4_10.coverage", "b4c0bdcd-1b0f-41c9-8346-c2b352219ab2");
                script = cache.get(src);
                writeline("/home/ubuntu/results/coverage/Engine/Engine_4_10.coverage", "595e6439-1554-4d5d-8b5c-104af719f01b");
                if (script != null) {
                    writeline("/home/ubuntu/results/coverage/Engine/Engine_4_10.coverage", "56fec33c-2177-49af-b4c9-8d05d4a00b00");
                    Scope f = script.getScope();
                    writeline("/home/ubuntu/results/coverage/Engine/Engine_4_10.coverage", "e5486d98-d571-4c79-a2c4-ab395ba5544d");
                    if ((f == null && scope == null) || (f != null && f.equals(scope))) {
                        writeline("/home/ubuntu/results/coverage/Engine/Engine_4_10.coverage", "bba9938b-2f62-4f5b-b1f6-e123d63d081e");
                        return script;
                    }
                }
            }
            writeline("/home/ubuntu/results/coverage/Engine/Engine_4_10.coverage", "5d024ae4-175e-413f-ae74-8b361786af7e");
            script = parser.parse(info, src, scope, registers, expression);
            writeline("/home/ubuntu/results/coverage/Engine/Engine_4_10.coverage", "78278c4c-99be-48e8-9db0-fbcd86bf7d3b");
            if (cached) {
                writeline("/home/ubuntu/results/coverage/Engine/Engine_4_10.coverage", "b1fbd92c-e96e-43c3-944e-cb521837f63e");
                cache.put(src, script);
            }
        }
        writeline("/home/ubuntu/results/coverage/Engine/Engine_4_10.coverage", "9dffc892-9e4f-4e3e-9955-17b56d8dea9a");
        return script;
    }

    /**
     * Trims the source from front and ending spaces.
     * @param str expression to clean
     * @return trimmed expression ending in a semi-colon
     */
    protected String trimSource(CharSequence str) {
        writeline("/home/ubuntu/results/coverage/Engine/Engine_4_10.coverage", "d07b4a3f-5956-4c94-89a9-d457e410bdf2");
        if (str != null) {
            writeline("/home/ubuntu/results/coverage/Engine/Engine_4_10.coverage", "50edf6b0-2d95-4e63-a848-f7c0a41f0691");
            int start = 0;
            writeline("/home/ubuntu/results/coverage/Engine/Engine_4_10.coverage", "1772c8ac-754b-4a12-8c58-e3e46c39bf78");
            int end = str.length();
            writeline("/home/ubuntu/results/coverage/Engine/Engine_4_10.coverage", "5813e082-06df-4c09-92d2-b8946272da4f");
            if (end > 0) {
                writeline("/home/ubuntu/results/coverage/Engine/Engine_4_10.coverage", "23bde384-a03f-431c-a07c-9eba0dab12ca");
                // trim front spaces
                while (start < end && Character.isSpaceChar(str.charAt(start))) {
                    writeline("/home/ubuntu/results/coverage/Engine/Engine_4_10.coverage", "c92ca1d6-55d3-43cc-9b1f-90b031a5d86e");
                    ++start;
                }
                writeline("/home/ubuntu/results/coverage/Engine/Engine_4_10.coverage", "053b336f-09cd-4631-bb65-ec2180f331c4");
                // trim ending spaces
                while (end > 0 && Character.isSpaceChar(str.charAt(end - 1))) {
                    writeline("/home/ubuntu/results/coverage/Engine/Engine_4_10.coverage", "5693a654-1c4a-4bd3-ae83-a678979a5812");
                    --end;
                }
                writeline("/home/ubuntu/results/coverage/Engine/Engine_4_10.coverage", "f0deacfc-6cf5-4d09-88ad-3f1b51dfca7e");
                return str.subSequence(start, end).toString();
            }
            writeline("/home/ubuntu/results/coverage/Engine/Engine_4_10.coverage", "8bb5a1b2-fee0-4c09-b9be-dc382914ba0d");
            return "";
        }
        writeline("/home/ubuntu/results/coverage/Engine/Engine_4_10.coverage", "76d92075-4a73-4cf1-8b89-4d382292e57f");
        return null;
    }

    /**
     * Gets and/or creates a default template engine.
     * @return a template engine
     */
    protected TemplateEngine jxlt() {
        writeline("/home/ubuntu/results/coverage/Engine/Engine_4_10.coverage", "846cc2f5-fa48-4e5e-9e9e-8d1fd8b8431e");
        TemplateEngine e = jxlt;
        writeline("/home/ubuntu/results/coverage/Engine/Engine_4_10.coverage", "c099ed6b-8558-4706-913a-f1455e7a6b29");
        if (e == null) {
            writeline("/home/ubuntu/results/coverage/Engine/Engine_4_10.coverage", "ffe24836-ff45-41aa-8633-5c517ac593c4");
            synchronized (this) {
                writeline("/home/ubuntu/results/coverage/Engine/Engine_4_10.coverage", "fb9e2119-9682-406a-bad7-b4d28f26581b");
                if (jxlt == null) {
                    writeline("/home/ubuntu/results/coverage/Engine/Engine_4_10.coverage", "e01869c2-3392-44e5-88b5-e6dc3e42d0bb");
                    e = new TemplateEngine(this, true, 0, '$', '#');
                    writeline("/home/ubuntu/results/coverage/Engine/Engine_4_10.coverage", "88dd15ad-0125-408b-a928-6464b56886da");
                    jxlt = e;
                }
            }
        }
        writeline("/home/ubuntu/results/coverage/Engine/Engine_4_10.coverage", "cc1e7793-1ed1-4318-8d66-4fa9d15bcd03");
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
