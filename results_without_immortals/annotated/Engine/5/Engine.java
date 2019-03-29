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
        writelineStatic("/home/ubuntu/results/coverage/Engine/Engine_5_10.coverage", "32e5f518-f29e-4120-8460-f9a41c83f6de");
        if ((logger == null || logger.equals(LogFactory.getLog(JexlEngine.class))) && (strategy == null || strategy == JexlUberspect.JEXL_STRATEGY)) {
            writelineStatic("/home/ubuntu/results/coverage/Engine/Engine_5_10.coverage", "7efc24d0-af8a-430c-897d-71cde3420e83");
            return UberspectHolder.UBERSPECT;
        }
        writelineStatic("/home/ubuntu/results/coverage/Engine/Engine_5_10.coverage", "7c6a1e08-2ec7-426a-a389-328aeb7dc3ba");
        return new Uberspect(logger, strategy);
    }

    @Override
    public JexlUberspect getUberspect() {
        writeline("/home/ubuntu/results/coverage/Engine/Engine_5_10.coverage", "9c9671d7-291e-4909-a13d-f228e739a0c4");
        return uberspect;
    }

    @Override
    public JexlArithmetic getArithmetic() {
        writeline("/home/ubuntu/results/coverage/Engine/Engine_5_10.coverage", "d4020a68-da6b-4810-94df-06d193ef3eaf");
        return arithmetic;
    }

    @Override
    public boolean isDebug() {
        writeline("/home/ubuntu/results/coverage/Engine/Engine_5_10.coverage", "03a1756e-b6ef-4405-ba50-dd9620325dd7");
        return this.debug;
    }

    @Override
    public boolean isSilent() {
        writeline("/home/ubuntu/results/coverage/Engine/Engine_5_10.coverage", "1e427fdd-876b-43ed-8932-06369b9c1f04");
        return this.silent;
    }

    @Override
    public boolean isStrict() {
        writeline("/home/ubuntu/results/coverage/Engine/Engine_5_10.coverage", "ae1799dc-315b-47e1-b6b0-bbdc10226ca4");
        return strict;
    }

    @Override
    public void setClassLoader(ClassLoader loader) {
        writeline("/home/ubuntu/results/coverage/Engine/Engine_5_10.coverage", "b2b1055f-f8fc-4614-ba0d-2691991bd8bb");
        uberspect.setClassLoader(loader);
    }

    @Override
    public Charset getCharset() {
        writeline("/home/ubuntu/results/coverage/Engine/Engine_5_10.coverage", "3b91470d-ff1a-49ad-a39a-dea16ad914b0");
        return charset;
    }

    @Override
    public TemplateEngine createJxltEngine(boolean noScript, int cacheSize, char immediate, char deferred) {
        writeline("/home/ubuntu/results/coverage/Engine/Engine_5_10.coverage", "c0545cf4-5d80-4152-9268-36a254e25d1e");
        return new TemplateEngine(this, noScript, cacheSize, immediate, deferred);
    }

    /**
     * Swaps the current thread local context.
     * @param tls the context or null
     * @return the previous thread local context
     */
    protected JexlContext.ThreadLocal putThreadLocal(JexlContext.ThreadLocal tls) {
        writeline("/home/ubuntu/results/coverage/Engine/Engine_5_10.coverage", "56f34709-210f-4088-8a15-941c2981f401");
        JexlContext.ThreadLocal local = CONTEXT.get();
        writeline("/home/ubuntu/results/coverage/Engine/Engine_5_10.coverage", "a1da0cfa-60dc-4597-9602-dd52b486f64d");
        CONTEXT.set(tls);
        writeline("/home/ubuntu/results/coverage/Engine/Engine_5_10.coverage", "451eb04a-e000-42cf-956c-700576210461");
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
        writeline("/home/ubuntu/results/coverage/Engine/Engine_5_10.coverage", "acd0796c-965b-47fa-94d3-a3b3b371efb6");
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
        writeline("/home/ubuntu/results/coverage/Engine/Engine_5_10.coverage", "68359a56-1e89-49ac-8432-fc22b1a8dcb2");
        synchronized (parser) {
            writeline("/home/ubuntu/results/coverage/Engine/Engine_5_10.coverage", "b6be3d9d-26f0-4d0d-a8df-54fcf8069259");
            if (cache != null) {
                writeline("/home/ubuntu/results/coverage/Engine/Engine_5_10.coverage", "b21bff4a-fabe-438b-abd8-75b5cc93ec49");
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
        writeline("/home/ubuntu/results/coverage/Engine/Engine_5_10.coverage", "026c1bca-05d1-4f73-a9b9-ec9ec07f5040");
        return new Interpreter(this, context, frame);
    }

    @Override
    public Script createScript(JexlInfo info, String scriptText, String[] names) {
        writeline("/home/ubuntu/results/coverage/Engine/Engine_5_10.coverage", "e8a882fb-3e57-47b2-b96a-56f4366f9699");
        if (scriptText == null) {
            writeline("/home/ubuntu/results/coverage/Engine/Engine_5_10.coverage", "b9ceb164-8a71-49b4-9373-b56c70ca5609");
            throw new NullPointerException("source is null");
        }
        writeline("/home/ubuntu/results/coverage/Engine/Engine_5_10.coverage", "e5ddea70-f2bd-4834-a737-9ac6dcb4ab02");
        if (info == null && debug) {
            writeline("/home/ubuntu/results/coverage/Engine/Engine_5_10.coverage", "71898103-25dd-4301-b578-117ed537991c");
            info = createInfo();
        }
        writeline("/home/ubuntu/results/coverage/Engine/Engine_5_10.coverage", "1b9d1504-1697-4731-858b-d1c4be470175");
        String source = trimSource(scriptText);
        writeline("/home/ubuntu/results/coverage/Engine/Engine_5_10.coverage", "b419a7ed-8191-4d2a-a99d-3e4971a04203");
        Scope scope = names == null ? null : new Scope(null, names);
        writeline("/home/ubuntu/results/coverage/Engine/Engine_5_10.coverage", "d0b5447a-0b19-4df0-ab68-479cc317eedf");
        ASTJexlScript tree = parse(info, source, scope, false, false);
        writeline("/home/ubuntu/results/coverage/Engine/Engine_5_10.coverage", "1a079f4f-6112-4bae-8350-c7bf9f2ce2c6");
        return new Script(this, source, tree);
    }

    @Override
    public Script createExpression(JexlInfo info, String expression) {
        writeline("/home/ubuntu/results/coverage/Engine/Engine_5_10.coverage", "00d0b89c-22a2-4d7e-bf8d-a326a3f3223c");
        if (expression == null) {
            writeline("/home/ubuntu/results/coverage/Engine/Engine_5_10.coverage", "75b240d6-7e96-4df0-b540-bd027b9ac798");
            throw new NullPointerException("source is null");
        }
        writeline("/home/ubuntu/results/coverage/Engine/Engine_5_10.coverage", "7e1b8ba9-bd4a-4033-ae11-77831a9f768f");
        if (info == null && debug) {
            writeline("/home/ubuntu/results/coverage/Engine/Engine_5_10.coverage", "78a16806-bce5-4bcb-9261-dbfe44ee5262");
            info = createInfo();
        }
        writeline("/home/ubuntu/results/coverage/Engine/Engine_5_10.coverage", "3ebef166-8cb6-41b1-a627-af2925029767");
        String source = trimSource(expression);
        writeline("/home/ubuntu/results/coverage/Engine/Engine_5_10.coverage", "3ab7bc23-c383-4909-89e2-986b47c11b1c");
        ASTJexlScript tree = parse(info, source, null, false, true);
        writeline("/home/ubuntu/results/coverage/Engine/Engine_5_10.coverage", "40e64cd6-c62a-4ab7-aa0e-8d0ac39e46fa");
        return new Script(this, source, tree);
    }

    @Override
    public Object getProperty(Object bean, String expr) {
        writeline("/home/ubuntu/results/coverage/Engine/Engine_5_10.coverage", "f355c657-0283-4aff-9c6c-2f5efb34fc92");
        return getProperty(null, bean, expr);
    }

    @Override
    public Object getProperty(JexlContext context, Object bean, String expr) {
        writeline("/home/ubuntu/results/coverage/Engine/Engine_5_10.coverage", "c6b98857-74e5-4405-a1e9-84c3d3d0856c");
        if (context == null) {
            writeline("/home/ubuntu/results/coverage/Engine/Engine_5_10.coverage", "ffe9ae2e-bf9f-4fe9-841d-101ba3fc8854");
            context = EMPTY_CONTEXT;
        }
        writeline("/home/ubuntu/results/coverage/Engine/Engine_5_10.coverage", "d78a23af-95fb-472b-b20d-7d29e6d61a48");
        // synthetize expr using register
        String src = trimSource(expr);
        writeline("/home/ubuntu/results/coverage/Engine/Engine_5_10.coverage", "87b2bad1-23c1-4906-8a5f-31a048010d11");
        src = "#0" + (src.charAt(0) == '[' ? "" : ".") + src;
        writeline("/home/ubuntu/results/coverage/Engine/Engine_5_10.coverage", "b849de21-eba4-4f25-a949-b339661224b8");
        try {
            writeline("/home/ubuntu/results/coverage/Engine/Engine_5_10.coverage", "2356b1fe-c6da-4231-9e17-b38e5caefe2c");
            final JexlInfo info = debug ? createInfo() : null;
            writeline("/home/ubuntu/results/coverage/Engine/Engine_5_10.coverage", "8ab1fc7c-7805-4512-8f83-32bef2190fc6");
            final Scope scope = new Scope(null, "#0");
            writeline("/home/ubuntu/results/coverage/Engine/Engine_5_10.coverage", "56f92494-7e11-4c04-8ddc-3f52ff9f93c1");
            final ASTJexlScript script = parse(info, src, scope, true, true);
            writeline("/home/ubuntu/results/coverage/Engine/Engine_5_10.coverage", "b5682deb-2568-4e8b-b97d-bf49fce6cb2a");
            final JexlNode node = script.jjtGetChild(0);
            writeline("/home/ubuntu/results/coverage/Engine/Engine_5_10.coverage", "8bbe1f75-833e-4788-bedf-c0e436932663");
            final Scope.Frame frame = script.createFrame(bean);
            writeline("/home/ubuntu/results/coverage/Engine/Engine_5_10.coverage", "6adea478-c7a7-4472-b1d3-6e9e44a045e1");
            final Interpreter interpreter = createInterpreter(context, frame);
            writeline("/home/ubuntu/results/coverage/Engine/Engine_5_10.coverage", "4ffc238d-f122-4367-8abd-3357ec4b5e85");
            return node.jjtAccept(interpreter, null);
        } catch (JexlException xjexl) {
            writeline("/home/ubuntu/results/coverage/Engine/Engine_5_10.coverage", "e4ecd323-d1e9-4106-93b4-2f6d6b9073af");
            if (silent) {
                writeline("/home/ubuntu/results/coverage/Engine/Engine_5_10.coverage", "a585ba8e-daed-45f8-bb25-2f411b01d945");
                logger.warn(xjexl.getMessage(), xjexl.getCause());
                writeline("/home/ubuntu/results/coverage/Engine/Engine_5_10.coverage", "e79ebc15-e8e5-4c75-a264-2d8ba02a64dc");
                return null;
            }
            writeline("/home/ubuntu/results/coverage/Engine/Engine_5_10.coverage", "f5e26903-09d3-49b4-8bf1-09e6fdb12602");
            throw xjexl.clean();
        }
    }

    @Override
    public void setProperty(Object bean, String expr, Object value) {
        writeline("/home/ubuntu/results/coverage/Engine/Engine_5_10.coverage", "d1f62082-b299-4374-b719-328e04efe323");
        setProperty(null, bean, expr, value);
    }

    @Override
    public void setProperty(JexlContext context, Object bean, String expr, Object value) {
        writeline("/home/ubuntu/results/coverage/Engine/Engine_5_10.coverage", "210df0cc-0fa9-40de-9308-c62fa4f0b148");
        if (context == null) {
            writeline("/home/ubuntu/results/coverage/Engine/Engine_5_10.coverage", "3f06e9a5-c584-478d-8699-8c15f71018d6");
            context = EMPTY_CONTEXT;
        }
        writeline("/home/ubuntu/results/coverage/Engine/Engine_5_10.coverage", "0914dc14-8329-4c9c-b328-5bf88ee7d49d");
        // synthetize expr using registers
        String src = trimSource(expr);
        writeline("/home/ubuntu/results/coverage/Engine/Engine_5_10.coverage", "e9cf4373-bed2-4269-b71e-4651c798e6a7");
        src = "#0" + (src.charAt(0) == '[' ? "" : ".") + src + "=" + "#1";
        writeline("/home/ubuntu/results/coverage/Engine/Engine_5_10.coverage", "9c809276-03d3-44b4-9e65-fd42459883d5");
        try {
            writeline("/home/ubuntu/results/coverage/Engine/Engine_5_10.coverage", "3fb6a989-e7a8-4e21-b174-c371b588f108");
            final JexlInfo info = debug ? createInfo() : null;
            writeline("/home/ubuntu/results/coverage/Engine/Engine_5_10.coverage", "385fd7d5-e903-4aa4-a965-c94543b9045d");
            final Scope scope = new Scope(null, "#0", "#1");
            writeline("/home/ubuntu/results/coverage/Engine/Engine_5_10.coverage", "4d14d31d-24f1-40ef-b1ba-fb43b3902666");
            final ASTJexlScript script = parse(info, src, scope, true, true);
            writeline("/home/ubuntu/results/coverage/Engine/Engine_5_10.coverage", "d61caacd-6d1f-42c9-b107-d1257977d6dd");
            final JexlNode node = script.jjtGetChild(0);
            writeline("/home/ubuntu/results/coverage/Engine/Engine_5_10.coverage", "89301d83-4650-4b8d-b05e-fa975da4cec0");
            final Scope.Frame frame = script.createFrame(bean, value);
            writeline("/home/ubuntu/results/coverage/Engine/Engine_5_10.coverage", "e93f12f6-6a7f-4f76-8ffe-a09b8cb55fc1");
            final Interpreter interpreter = createInterpreter(context, frame);
            writeline("/home/ubuntu/results/coverage/Engine/Engine_5_10.coverage", "1597d26a-2ba3-4bcd-b9ec-40f66a6aa365");
            node.jjtAccept(interpreter, null);
        } catch (JexlException xjexl) {
            writeline("/home/ubuntu/results/coverage/Engine/Engine_5_10.coverage", "9a5e480d-5c80-4095-a2cd-7dcfac08f7b3");
            if (silent) {
                writeline("/home/ubuntu/results/coverage/Engine/Engine_5_10.coverage", "0b16425e-fd76-4508-8d31-8f07a00e0704");
                logger.warn(xjexl.getMessage(), xjexl.getCause());
                writeline("/home/ubuntu/results/coverage/Engine/Engine_5_10.coverage", "ab9ad889-067b-46d5-802e-a59f761f3610");
                return;
            }
            writeline("/home/ubuntu/results/coverage/Engine/Engine_5_10.coverage", "63a03faa-03f0-4bbf-82cb-13a5fad2ef2f");
            throw xjexl.clean();
        }
    }

    @Override
    public Object invokeMethod(Object obj, String meth, Object... args) {
        writeline("/home/ubuntu/results/coverage/Engine/Engine_5_10.coverage", "89e55dc9-5599-4225-aa25-391570377a4a");
        JexlException xjexl = null;
        writeline("/home/ubuntu/results/coverage/Engine/Engine_5_10.coverage", "1993326f-c7cd-467a-8510-2ccb2ce18e0c");
        Object result = null;
        writeline("/home/ubuntu/results/coverage/Engine/Engine_5_10.coverage", "3ac211f4-f4d9-4929-8929-e59aed12c496");
        final JexlInfo info = debug ? createInfo() : null;
        writeline("/home/ubuntu/results/coverage/Engine/Engine_5_10.coverage", "7fb115c7-7da0-434a-baca-0e6dcaa27a52");
        try {
            writeline("/home/ubuntu/results/coverage/Engine/Engine_5_10.coverage", "656770ff-4182-43f5-8c26-1bbc1e7c4458");
            JexlMethod method = uberspect.getMethod(obj, meth, args);
            writeline("/home/ubuntu/results/coverage/Engine/Engine_5_10.coverage", "221d38d0-9b2e-47f8-9cb4-fdedb79b3e9b");
            if (method == null && arithmetic.narrowArguments(args)) {
                writeline("/home/ubuntu/results/coverage/Engine/Engine_5_10.coverage", "62312b52-8600-4e1e-92f6-733f2a6f6cfb");
                method = uberspect.getMethod(obj, meth, args);
            }
            writeline("/home/ubuntu/results/coverage/Engine/Engine_5_10.coverage", "44f05d98-ab68-4cdb-87ce-57a4ee8eda12");
            if (method != null) {
                writeline("/home/ubuntu/results/coverage/Engine/Engine_5_10.coverage", "8725730d-c3e0-4902-be74-40fad766e23a");
                result = method.invoke(obj, args);
            } else {
                writeline("/home/ubuntu/results/coverage/Engine/Engine_5_10.coverage", "0b31dff8-6562-4eef-9e4c-daea8434172e");
                xjexl = new JexlException.Method(info, meth, null);
            }
        } catch (JexlException xany) {
            writeline("/home/ubuntu/results/coverage/Engine/Engine_5_10.coverage", "c9ef49ea-223e-4614-aabf-3a8dc30bd6a7");
            xjexl = xany;
        } catch (Exception xany) {
            writeline("/home/ubuntu/results/coverage/Engine/Engine_5_10.coverage", "27dc5c09-7c40-4d07-b7ca-a1174d024ff5");
            xjexl = new JexlException.Method(info, meth, xany);
        }
        writeline("/home/ubuntu/results/coverage/Engine/Engine_5_10.coverage", "49fadb9f-409b-4e8d-bcfa-f3923ed7c219");
        if (xjexl != null) {
            writeline("/home/ubuntu/results/coverage/Engine/Engine_5_10.coverage", "4b2bd06a-851a-424d-82ed-4217df3c3087");
            if (silent) {
                writeline("/home/ubuntu/results/coverage/Engine/Engine_5_10.coverage", "778f3f24-abbc-40b8-8b6f-0a82cbb0847e");
                logger.warn(xjexl.getMessage(), xjexl.getCause());
                writeline("/home/ubuntu/results/coverage/Engine/Engine_5_10.coverage", "783ca1f5-969a-476f-b7d5-c339ac7488c0");
                result = null;
            } else {
                writeline("/home/ubuntu/results/coverage/Engine/Engine_5_10.coverage", "8ce7f01a-273c-4269-926e-0b51728b0944");
                throw xjexl.clean();
            }
        }
        writeline("/home/ubuntu/results/coverage/Engine/Engine_5_10.coverage", "318ccf33-52b9-431f-b996-e8a84685722d");
        return result;
    }

    @Override
    public <T> T newInstance(Class<? extends T> clazz, Object... args) {
        writeline("/home/ubuntu/results/coverage/Engine/Engine_5_10.coverage", "6cf90d3b-9365-44bd-8247-296724f35c1e");
        return clazz.cast(doCreateInstance(clazz, args));
    }

    @Override
    public Object newInstance(String clazz, Object... args) {
        writeline("/home/ubuntu/results/coverage/Engine/Engine_5_10.coverage", "aa0e6d41-ac5a-480e-92d4-6eac32706bfb");
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
        writeline("/home/ubuntu/results/coverage/Engine/Engine_5_10.coverage", "306f8244-4c83-461b-885b-6c02ee7259e7");
        JexlException xjexl = null;
        writeline("/home/ubuntu/results/coverage/Engine/Engine_5_10.coverage", "b36c782f-1cbe-4cd8-8c78-1677ad74c6c8");
        Object result = null;
        writeline("/home/ubuntu/results/coverage/Engine/Engine_5_10.coverage", "9f23a2e2-1cf0-41c1-a912-52f23df5656b");
        final JexlInfo info = debug ? createInfo() : null;
        writeline("/home/ubuntu/results/coverage/Engine/Engine_5_10.coverage", "d514009c-d09a-43e5-9e28-42d24f948a81");
        try {
            writeline("/home/ubuntu/results/coverage/Engine/Engine_5_10.coverage", "b606a5e1-91aa-437c-b3b0-eba3eef8fb34");
            JexlMethod ctor = uberspect.getConstructor(clazz, args);
            writeline("/home/ubuntu/results/coverage/Engine/Engine_5_10.coverage", "8f77a740-1546-4b38-a3a2-bb030ac114d2");
            if (ctor == null && arithmetic.narrowArguments(args)) {
                writeline("/home/ubuntu/results/coverage/Engine/Engine_5_10.coverage", "95e309c3-30e6-445a-abdd-fc338ced691e");
                ctor = uberspect.getConstructor(clazz, args);
            }
            writeline("/home/ubuntu/results/coverage/Engine/Engine_5_10.coverage", "4f435988-7b29-4f15-823e-73f32043d81c");
            if (ctor != null) {
                writeline("/home/ubuntu/results/coverage/Engine/Engine_5_10.coverage", "54b1bad1-feff-49b3-b79b-ed46a431dd51");
                result = ctor.invoke(clazz, args);
            } else {
                writeline("/home/ubuntu/results/coverage/Engine/Engine_5_10.coverage", "c88b4d09-bfd9-419d-a8de-164154f689c3");
                xjexl = new JexlException.Method(info, clazz.toString(), null);
            }
        } catch (JexlException xany) {
            writeline("/home/ubuntu/results/coverage/Engine/Engine_5_10.coverage", "c301b02b-e81f-4784-936f-4d49a9e382b3");
            xjexl = xany;
        } catch (Exception xany) {
            writeline("/home/ubuntu/results/coverage/Engine/Engine_5_10.coverage", "8bc43c48-b83d-47e9-b089-645801565a04");
            xjexl = new JexlException.Method(info, clazz.toString(), xany);
        }
        writeline("/home/ubuntu/results/coverage/Engine/Engine_5_10.coverage", "d2cbec19-6f05-441d-bceb-2f8469927d9d");
        if (xjexl != null) {
            writeline("/home/ubuntu/results/coverage/Engine/Engine_5_10.coverage", "8dd94fa4-2ed7-498a-bd28-618babeead8b");
            if (silent) {
                writeline("/home/ubuntu/results/coverage/Engine/Engine_5_10.coverage", "247a4fd9-3c2b-4b32-9b00-f75d5216a2bd");
                logger.warn(xjexl.getMessage(), xjexl.getCause());
                writeline("/home/ubuntu/results/coverage/Engine/Engine_5_10.coverage", "803a231b-0613-4ddf-a40b-3bbe883a95ad");
                return null;
            }
            writeline("/home/ubuntu/results/coverage/Engine/Engine_5_10.coverage", "56f39e43-e290-441f-8986-8d1c6b472857");
            throw xjexl.clean();
        }
        writeline("/home/ubuntu/results/coverage/Engine/Engine_5_10.coverage", "d2267a99-2010-45ad-a88f-7ff866cc4997");
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
        writeline("/home/ubuntu/results/coverage/Engine/Engine_5_10.coverage", "1f88c856-9aa4-4891-a797-b040b159f1ca");
        VarCollector collector = new VarCollector();
        writeline("/home/ubuntu/results/coverage/Engine/Engine_5_10.coverage", "19bae19e-4f3d-4cff-b26a-670d96fddc4d");
        getVariables(script, script, collector);
        writeline("/home/ubuntu/results/coverage/Engine/Engine_5_10.coverage", "b1b2153f-5007-440f-b800-7b7cc173908a");
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
        writeline("/home/ubuntu/results/coverage/Engine/Engine_5_10.coverage", "eb45fdb5-2bce-4dab-9ad1-8eb678b3d3d3");
        if (node instanceof ASTIdentifier) {
            writeline("/home/ubuntu/results/coverage/Engine/Engine_5_10.coverage", "add2f6c2-2a48-4f1b-abef-b596bed72696");
            JexlNode parent = node.jjtGetParent();
            writeline("/home/ubuntu/results/coverage/Engine/Engine_5_10.coverage", "e6cf34ca-3c50-48ec-a3ee-c721289c1f12");
            if (parent instanceof ASTMethodNode || parent instanceof ASTFunctionNode) {
                writeline("/home/ubuntu/results/coverage/Engine/Engine_5_10.coverage", "2242b15b-008d-48a2-a676-4a7174b3bb2c");
                // skip identifiers for methods and functions
                collector.collect(null);
                writeline("/home/ubuntu/results/coverage/Engine/Engine_5_10.coverage", "e86b435f-0f20-4f7e-8334-cd00a979b935");
                return;
            }
            writeline("/home/ubuntu/results/coverage/Engine/Engine_5_10.coverage", "8cad6853-b41d-4c3b-a1f7-09efc3e47ed2");
            ASTIdentifier identifier = (ASTIdentifier) node;
            writeline("/home/ubuntu/results/coverage/Engine/Engine_5_10.coverage", "cd6f399b-8371-4616-a8e8-65f8a2d51aea");
            int symbol = identifier.getSymbol();
            writeline("/home/ubuntu/results/coverage/Engine/Engine_5_10.coverage", "21f5bb67-7cf8-4e15-be5a-b31248226cbe");
            // symbols that are hoisted are considered "global" variables
            if (symbol >= 0 && script != null && !script.isHoistedSymbol(symbol)) {
                writeline("/home/ubuntu/results/coverage/Engine/Engine_5_10.coverage", "0fce11cc-8c7b-4d24-ae18-5d16b85ff962");
                collector.collect(null);
            } else {
                writeline("/home/ubuntu/results/coverage/Engine/Engine_5_10.coverage", "05c2cad1-3a1c-41fd-8e5d-1489ec0602b7");
                // start collecting from identifier
                collector.collect(identifier);
                writeline("/home/ubuntu/results/coverage/Engine/Engine_5_10.coverage", "89c73975-6a43-4811-b890-145a3ef58584");
                collector.add(identifier.getName());
            }
        } else if (node instanceof ASTIdentifierAccess) {
            writeline("/home/ubuntu/results/coverage/Engine/Engine_5_10.coverage", "a1dd9eaf-9924-4557-906f-4954324ecf52");
            JexlNode parent = node.jjtGetParent();
            writeline("/home/ubuntu/results/coverage/Engine/Engine_5_10.coverage", "75a25945-766c-48f0-8175-c86d2b2ff977");
            if (parent instanceof ASTMethodNode || parent instanceof ASTFunctionNode) {
                writeline("/home/ubuntu/results/coverage/Engine/Engine_5_10.coverage", "7b8ef2a4-a416-4fdd-bd40-ec8252b8a8b2");
                // skip identifiers for methods and functions
                collector.collect(null);
                writeline("/home/ubuntu/results/coverage/Engine/Engine_5_10.coverage", "72feeeaa-d43e-4c06-b9f7-5fdcb4015187");
                return;
            }
            writeline("/home/ubuntu/results/coverage/Engine/Engine_5_10.coverage", "3c141161-c396-4651-aafd-fb8fadc91cce");
            // belt and suspender since an identifier should have been seen first
            if (collector.isCollecting()) {
                writeline("/home/ubuntu/results/coverage/Engine/Engine_5_10.coverage", "bf695b49-b89a-4735-8a5f-319fd879829b");
                collector.add(((ASTIdentifierAccess) node).getName());
            }
        } else if (node instanceof ASTArrayAccess) {
            writeline("/home/ubuntu/results/coverage/Engine/Engine_5_10.coverage", "1981f1e8-c39d-4618-8e4f-bd75485e3f0f");
            int num = node.jjtGetNumChildren();
            writeline("/home/ubuntu/results/coverage/Engine/Engine_5_10.coverage", "7283a798-40ce-4c8e-81bf-462e56e6a224");
            // collect only if array access is const and follows an identifier
            boolean collecting = collector.isCollecting();
            writeline("/home/ubuntu/results/coverage/Engine/Engine_5_10.coverage", "f8b478fc-7181-404d-b57b-981e00f1297a");
            for (int i = 0; i < num; ++i) {
                writeline("/home/ubuntu/results/coverage/Engine/Engine_5_10.coverage", "8f8c427a-954f-47d1-81af-2a616044a169");
                JexlNode child = node.jjtGetChild(i);
                writeline("/home/ubuntu/results/coverage/Engine/Engine_5_10.coverage", "8825849b-d709-4605-a140-9c215b7bb6eb");
                if (collecting && child.isConstant()) {
                    writeline("/home/ubuntu/results/coverage/Engine/Engine_5_10.coverage", "de99cfbb-acb6-4e1f-b132-eada098cb532");
                    String image = child.toString();
                    writeline("/home/ubuntu/results/coverage/Engine/Engine_5_10.coverage", "603ea46b-2b55-4091-99b3-896b2a8ec808");
                    if (image == null) {
                        writeline("/home/ubuntu/results/coverage/Engine/Engine_5_10.coverage", "48dfb84b-7b16-4b82-a180-1236c7767e9f");
                        image = new Debugger().data(child);
                    }
                    writeline("/home/ubuntu/results/coverage/Engine/Engine_5_10.coverage", "d5b34b14-db95-4158-b775-670c5acfd929");
                    collector.add(image);
                } else {
                    writeline("/home/ubuntu/results/coverage/Engine/Engine_5_10.coverage", "9fadee62-bc21-4311-874b-91f4f24aaefd");
                    collecting = false;
                    writeline("/home/ubuntu/results/coverage/Engine/Engine_5_10.coverage", "581c107d-4930-46ba-a84a-ffb6185365f5");
                    collector.collect(null);
                    writeline("/home/ubuntu/results/coverage/Engine/Engine_5_10.coverage", "7974c778-bad0-418f-8f2e-00db3136f43d");
                    getVariables(script, child, collector);
                }
            }
        } else {
            writeline("/home/ubuntu/results/coverage/Engine/Engine_5_10.coverage", "f234c873-7412-4223-8a74-7894c10febe9");
            int num = node.jjtGetNumChildren();
            writeline("/home/ubuntu/results/coverage/Engine/Engine_5_10.coverage", "0fc0d3d5-dc3e-4c00-bef2-ddcede95f017");
            for (int i = 0; i < num; ++i) {
                writeline("/home/ubuntu/results/coverage/Engine/Engine_5_10.coverage", "b32a3f69-eb9d-4b24-bd88-b0700c384f0c");
                getVariables(script, node.jjtGetChild(i), collector);
            }
            writeline("/home/ubuntu/results/coverage/Engine/Engine_5_10.coverage", "0c6c486e-7c45-4680-9097-b51687390309");
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
        writeline("/home/ubuntu/results/coverage/Engine/Engine_5_10.coverage", "74c96d7f-234e-42d8-9e11-b98ec0fcb153");
        return script.getParameters();
    }

    /**
     * Gets the array of local variable from a script.
     * @param script the script
     * @return the local variables array which may be empty (but not null) if no local variables were defined
     * @since 3.0
     */
    protected String[] getLocalVariables(JexlScript script) {
        writeline("/home/ubuntu/results/coverage/Engine/Engine_5_10.coverage", "771c0726-06ba-406d-9e61-e398c12e8433");
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
        writeline("/home/ubuntu/results/coverage/Engine/Engine_5_10.coverage", "def24902-1fb0-487b-b9e1-3e68fb6cb5d0");
        final boolean cached = src.length() < cacheThreshold && cache != null;
        writeline("/home/ubuntu/results/coverage/Engine/Engine_5_10.coverage", "0cb50be9-2eb7-4d7c-8ca5-a411e410f977");
        ASTJexlScript script;
        writeline("/home/ubuntu/results/coverage/Engine/Engine_5_10.coverage", "9f20d8dc-a043-4730-8a7d-2c9c595fe140");
        synchronized (parser) {
            writeline("/home/ubuntu/results/coverage/Engine/Engine_5_10.coverage", "f45731f8-a361-470d-a507-bc69ceed9a6b");
            if (cached) {
                writeline("/home/ubuntu/results/coverage/Engine/Engine_5_10.coverage", "d3efb917-7b39-4c4a-93cf-bb464302e2a4");
                script = cache.get(src);
                writeline("/home/ubuntu/results/coverage/Engine/Engine_5_10.coverage", "63048496-bb8a-44b6-b892-f998cdb1a30d");
                if (script != null) {
                    writeline("/home/ubuntu/results/coverage/Engine/Engine_5_10.coverage", "667f4251-4112-4b77-998a-64bf9bbd1ef6");
                    Scope f = script.getScope();
                    writeline("/home/ubuntu/results/coverage/Engine/Engine_5_10.coverage", "3822370f-711c-4a4e-b566-e91ef1583deb");
                    if ((f == null && scope == null) || (f != null && f.equals(scope))) {
                        writeline("/home/ubuntu/results/coverage/Engine/Engine_5_10.coverage", "02e7a90e-68e5-47b9-a035-5d5eefcc9868");
                        return script;
                    }
                }
            }
            writeline("/home/ubuntu/results/coverage/Engine/Engine_5_10.coverage", "e0b1a231-f66c-4d1f-a408-d271a5a7e242");
            script = parser.parse(info, src, scope, registers, expression);
            writeline("/home/ubuntu/results/coverage/Engine/Engine_5_10.coverage", "f97540e4-ddb5-479b-b6b9-b98b31af8c62");
            if (cached) {
                writeline("/home/ubuntu/results/coverage/Engine/Engine_5_10.coverage", "4295c18c-8e6b-4a19-b93b-80483eaf6ce9");
                cache.put(src, script);
            }
        }
        writeline("/home/ubuntu/results/coverage/Engine/Engine_5_10.coverage", "5b6b0381-2432-4971-a6c4-5b6d2aa3bc97");
        return script;
    }

    /**
     * Trims the source from front and ending spaces.
     * @param str expression to clean
     * @return trimmed expression ending in a semi-colon
     */
    protected String trimSource(CharSequence str) {
        writeline("/home/ubuntu/results/coverage/Engine/Engine_5_10.coverage", "5a546fd4-5f79-4c44-97c0-faa9e2538777");
        if (str != null) {
            writeline("/home/ubuntu/results/coverage/Engine/Engine_5_10.coverage", "4499d64f-2c17-4715-a7e2-2877e7683861");
            int start = 0;
            writeline("/home/ubuntu/results/coverage/Engine/Engine_5_10.coverage", "024b9f3d-d817-4af5-b39b-0ee106e04d1b");
            int end = str.length();
            writeline("/home/ubuntu/results/coverage/Engine/Engine_5_10.coverage", "3af3ad28-efde-4c1f-b7d0-a62c8e1b7f37");
            if (end > 0) {
                writeline("/home/ubuntu/results/coverage/Engine/Engine_5_10.coverage", "1f97e6bc-528e-4482-bece-cbf891cd545b");
                // trim front spaces
                while (start < end && Character.isSpaceChar(str.charAt(start))) {
                    writeline("/home/ubuntu/results/coverage/Engine/Engine_5_10.coverage", "c8de7cba-0162-45c5-b238-eb6fefdd4876");
                    ++start;
                }
                writeline("/home/ubuntu/results/coverage/Engine/Engine_5_10.coverage", "da92d763-5830-44eb-88af-e5f98e05d68a");
                // trim ending spaces
                while (end > 0 && Character.isSpaceChar(str.charAt(end - 1))) {
                    writeline("/home/ubuntu/results/coverage/Engine/Engine_5_10.coverage", "77a4b51f-9222-48e8-a85f-507ae86ed7be");
                    --end;
                }
                writeline("/home/ubuntu/results/coverage/Engine/Engine_5_10.coverage", "98479022-c57b-4e79-99b7-dd5f77a36c88");
                return str.subSequence(start, end).toString();
            }
            writeline("/home/ubuntu/results/coverage/Engine/Engine_5_10.coverage", "53afd99d-6112-44aa-be54-200899f388fe");
            return "";
        }
        writeline("/home/ubuntu/results/coverage/Engine/Engine_5_10.coverage", "176a7846-5a8a-499c-a199-c2e897a19f00");
        return null;
    }

    /**
     * Gets and/or creates a default template engine.
     * @return a template engine
     */
    protected TemplateEngine jxlt() {
        writeline("/home/ubuntu/results/coverage/Engine/Engine_5_10.coverage", "530f8490-8b6f-49ea-ad8d-39d41ef48919");
        TemplateEngine e = jxlt;
        writeline("/home/ubuntu/results/coverage/Engine/Engine_5_10.coverage", "1351b388-b4c0-47b3-808f-5b2fde428b1d");
        if (e == null) {
            writeline("/home/ubuntu/results/coverage/Engine/Engine_5_10.coverage", "b80612f5-3251-43a2-843f-f95f4bc5da03");
            synchronized (this) {
                writeline("/home/ubuntu/results/coverage/Engine/Engine_5_10.coverage", "3800cccf-ecb6-45c7-ac23-9d9d52b3260f");
                if (jxlt == null) {
                    writeline("/home/ubuntu/results/coverage/Engine/Engine_5_10.coverage", "b9cc5f82-b2f5-4732-8232-b9a386a3303d");
                    e = new TemplateEngine(this, true, 0, '$', '#');
                    writeline("/home/ubuntu/results/coverage/Engine/Engine_5_10.coverage", "570cf92f-6677-44de-b829-5cffaa953b5c");
                    jxlt = e;
                }
            }
        }
        writeline("/home/ubuntu/results/coverage/Engine/Engine_5_10.coverage", "e472f8c7-4d40-45f6-ae4a-a461ca775e25");
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
