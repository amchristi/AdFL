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
        writelineStatic("/home/ubuntu/results/coverage/Engine/Engine_8_10.coverage", "a47f19ec-93bd-46d7-8226-d26452c2ff67");
        if ((logger == null || logger.equals(LogFactory.getLog(JexlEngine.class))) && (strategy == null || strategy == JexlUberspect.JEXL_STRATEGY)) {
            writelineStatic("/home/ubuntu/results/coverage/Engine/Engine_8_10.coverage", "97dc829b-4621-434c-8e6d-35d1a168b194");
            return UberspectHolder.UBERSPECT;
        }
        writelineStatic("/home/ubuntu/results/coverage/Engine/Engine_8_10.coverage", "a5610ba7-ae93-48d3-b77b-e345979fc4d4");
        return new Uberspect(logger, strategy);
    }

    @Override
    public JexlUberspect getUberspect() {
        writeline("/home/ubuntu/results/coverage/Engine/Engine_8_10.coverage", "87da92f6-a50e-4c37-b632-2fb024cccccb");
        return uberspect;
    }

    @Override
    public JexlArithmetic getArithmetic() {
        writeline("/home/ubuntu/results/coverage/Engine/Engine_8_10.coverage", "3a8b36cb-090c-45b9-9648-6606546e60c0");
        return arithmetic;
    }

    @Override
    public boolean isDebug() {
        writeline("/home/ubuntu/results/coverage/Engine/Engine_8_10.coverage", "e34c6c57-1a6d-47e0-8495-b31d952c8bf8");
        return this.debug;
    }

    @Override
    public boolean isSilent() {
        writeline("/home/ubuntu/results/coverage/Engine/Engine_8_10.coverage", "71555e1d-78d3-4bef-9ac9-26ac132feeaf");
        return this.silent;
    }

    @Override
    public boolean isStrict() {
        writeline("/home/ubuntu/results/coverage/Engine/Engine_8_10.coverage", "94acd265-be42-4735-afac-0f61278e968c");
        return strict;
    }

    @Override
    public void setClassLoader(ClassLoader loader) {
        writeline("/home/ubuntu/results/coverage/Engine/Engine_8_10.coverage", "c6615cd0-a512-422f-a5a8-c2ff968b4ab0");
        uberspect.setClassLoader(loader);
    }

    @Override
    public Charset getCharset() {
        writeline("/home/ubuntu/results/coverage/Engine/Engine_8_10.coverage", "b5e2bf0c-67fe-4839-8efb-baa3023ba075");
        return charset;
    }

    @Override
    public TemplateEngine createJxltEngine(boolean noScript, int cacheSize, char immediate, char deferred) {
        writeline("/home/ubuntu/results/coverage/Engine/Engine_8_10.coverage", "d2489225-5e44-400a-aee8-efa68210cdb9");
        return new TemplateEngine(this, noScript, cacheSize, immediate, deferred);
    }

    /**
     * Swaps the current thread local context.
     * @param tls the context or null
     * @return the previous thread local context
     */
    protected JexlContext.ThreadLocal putThreadLocal(JexlContext.ThreadLocal tls) {
        writeline("/home/ubuntu/results/coverage/Engine/Engine_8_10.coverage", "70768bb5-a396-4188-aa7f-407d45fb58d7");
        JexlContext.ThreadLocal local = CONTEXT.get();
        writeline("/home/ubuntu/results/coverage/Engine/Engine_8_10.coverage", "6482d250-6e4d-45ba-a79a-e6af31545e31");
        CONTEXT.set(tls);
        writeline("/home/ubuntu/results/coverage/Engine/Engine_8_10.coverage", "49f174a7-7e0f-4606-a6e6-6e413951c1d8");
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
        writeline("/home/ubuntu/results/coverage/Engine/Engine_8_10.coverage", "07953e32-e35e-43ef-bce1-7c2396b9252b");
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
        writeline("/home/ubuntu/results/coverage/Engine/Engine_8_10.coverage", "ba4ac6a2-959a-402b-b348-b921256689ad");
        synchronized (parser) {
            writeline("/home/ubuntu/results/coverage/Engine/Engine_8_10.coverage", "d5fc36f6-7d93-4342-8090-4a37c8fca6e0");
            if (cache != null) {
                writeline("/home/ubuntu/results/coverage/Engine/Engine_8_10.coverage", "271dc698-2296-4b3c-b04b-e7c9a3e63da3");
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
        writeline("/home/ubuntu/results/coverage/Engine/Engine_8_10.coverage", "c36ba817-74b1-454f-a455-0b55083c52e5");
        return new Interpreter(this, context, frame);
    }

    @Override
    public Script createScript(JexlInfo info, String scriptText, String[] names) {
        writeline("/home/ubuntu/results/coverage/Engine/Engine_8_10.coverage", "d9f9ce07-ca63-4bf1-91dd-cd4935228dc0");
        if (scriptText == null) {
            writeline("/home/ubuntu/results/coverage/Engine/Engine_8_10.coverage", "c0302a93-6321-4af1-9bef-8ae5a59e449a");
            throw new NullPointerException("source is null");
        }
        writeline("/home/ubuntu/results/coverage/Engine/Engine_8_10.coverage", "311ea983-0f16-4995-97b1-a17cbeb205c2");
        if (info == null && debug) {
            writeline("/home/ubuntu/results/coverage/Engine/Engine_8_10.coverage", "5e6342be-7389-428c-bd37-7af8904de38b");
            info = createInfo();
        }
        writeline("/home/ubuntu/results/coverage/Engine/Engine_8_10.coverage", "21755776-d936-4fc9-8184-37cb502319ed");
        String source = trimSource(scriptText);
        writeline("/home/ubuntu/results/coverage/Engine/Engine_8_10.coverage", "ae61a154-bd05-4c00-b8d0-38e01766b90a");
        Scope scope = names == null ? null : new Scope(null, names);
        writeline("/home/ubuntu/results/coverage/Engine/Engine_8_10.coverage", "d983e53b-ec9c-4e9a-9d19-b7777bf1ee89");
        ASTJexlScript tree = parse(info, source, scope, false, false);
        writeline("/home/ubuntu/results/coverage/Engine/Engine_8_10.coverage", "9136c0ca-8567-48d9-bca2-3aea36ff9e19");
        return new Script(this, source, tree);
    }

    @Override
    public Script createExpression(JexlInfo info, String expression) {
        writeline("/home/ubuntu/results/coverage/Engine/Engine_8_10.coverage", "1d34d727-d204-4048-bbf3-5a7d0c2ed507");
        if (expression == null) {
            writeline("/home/ubuntu/results/coverage/Engine/Engine_8_10.coverage", "eec07fdc-f5d6-4fc8-9db5-8ec6cbdba9fa");
            throw new NullPointerException("source is null");
        }
        writeline("/home/ubuntu/results/coverage/Engine/Engine_8_10.coverage", "a51b866d-5f9c-4757-b89a-681cf997c085");
        if (info == null && debug) {
            writeline("/home/ubuntu/results/coverage/Engine/Engine_8_10.coverage", "b6e7d4ca-8022-47be-9f2b-43ddb6af97f1");
            info = createInfo();
        }
        writeline("/home/ubuntu/results/coverage/Engine/Engine_8_10.coverage", "97c003f7-7ab5-4223-ac36-7b7fc9d05c96");
        String source = trimSource(expression);
        writeline("/home/ubuntu/results/coverage/Engine/Engine_8_10.coverage", "8cb006af-e41d-476f-8de5-7d71e079167b");
        ASTJexlScript tree = parse(info, source, null, false, true);
        writeline("/home/ubuntu/results/coverage/Engine/Engine_8_10.coverage", "7c46b6be-7ed7-44a6-acec-cf35998c2b95");
        return new Script(this, source, tree);
    }

    @Override
    public Object getProperty(Object bean, String expr) {
        writeline("/home/ubuntu/results/coverage/Engine/Engine_8_10.coverage", "ab38d1b1-c10c-4422-93f5-d09479954f87");
        return getProperty(null, bean, expr);
    }

    @Override
    public Object getProperty(JexlContext context, Object bean, String expr) {
        writeline("/home/ubuntu/results/coverage/Engine/Engine_8_10.coverage", "6c1919d3-ad3a-4005-b1f2-bb6181990406");
        if (context == null) {
            writeline("/home/ubuntu/results/coverage/Engine/Engine_8_10.coverage", "2568f711-6dd0-4b65-b353-392f94512062");
            context = EMPTY_CONTEXT;
        }
        writeline("/home/ubuntu/results/coverage/Engine/Engine_8_10.coverage", "01260c0a-9961-4270-8935-0605fe699a6b");
        // synthetize expr using register
        String src = trimSource(expr);
        writeline("/home/ubuntu/results/coverage/Engine/Engine_8_10.coverage", "965bb292-b405-41a7-97ce-9745deb1de39");
        src = "#0" + (src.charAt(0) == '[' ? "" : ".") + src;
        writeline("/home/ubuntu/results/coverage/Engine/Engine_8_10.coverage", "3d704c81-5010-4f39-bb3e-7803c3d060da");
        try {
            writeline("/home/ubuntu/results/coverage/Engine/Engine_8_10.coverage", "5ca9fdec-0e34-409d-b26c-688a06bf4344");
            final JexlInfo info = debug ? createInfo() : null;
            writeline("/home/ubuntu/results/coverage/Engine/Engine_8_10.coverage", "6b39c336-5d42-4036-8569-5cdf71cb7212");
            final Scope scope = new Scope(null, "#0");
            writeline("/home/ubuntu/results/coverage/Engine/Engine_8_10.coverage", "7dd20f4a-479b-4287-9b4b-43da80bc7997");
            final ASTJexlScript script = parse(info, src, scope, true, true);
            writeline("/home/ubuntu/results/coverage/Engine/Engine_8_10.coverage", "a8b0689f-bb8d-41bb-a82e-f6aee48665ac");
            final JexlNode node = script.jjtGetChild(0);
            writeline("/home/ubuntu/results/coverage/Engine/Engine_8_10.coverage", "9f0d39c6-9ca0-4a98-9c51-bd4cf7bde290");
            final Scope.Frame frame = script.createFrame(bean);
            writeline("/home/ubuntu/results/coverage/Engine/Engine_8_10.coverage", "5b668a7d-8f66-4c10-ac00-e541636470ad");
            final Interpreter interpreter = createInterpreter(context, frame);
            writeline("/home/ubuntu/results/coverage/Engine/Engine_8_10.coverage", "fe0efd78-9eef-4a66-9336-3fcd8b725972");
            return node.jjtAccept(interpreter, null);
        } catch (JexlException xjexl) {
            writeline("/home/ubuntu/results/coverage/Engine/Engine_8_10.coverage", "1e7666b1-f1ff-4682-97c8-8631c82d6140");
            if (silent) {
                writeline("/home/ubuntu/results/coverage/Engine/Engine_8_10.coverage", "2cbc473c-bed6-4c94-85eb-eefa2cd3e0f4");
                logger.warn(xjexl.getMessage(), xjexl.getCause());
                writeline("/home/ubuntu/results/coverage/Engine/Engine_8_10.coverage", "d0b1b47b-8cee-4c68-8989-3b0faad91a12");
                return null;
            }
            writeline("/home/ubuntu/results/coverage/Engine/Engine_8_10.coverage", "55d7ade7-0887-4519-84e4-c44ddc5c0a65");
            throw xjexl.clean();
        }
    }

    @Override
    public void setProperty(Object bean, String expr, Object value) {
        writeline("/home/ubuntu/results/coverage/Engine/Engine_8_10.coverage", "84033f3b-bc52-4f18-b0d6-ea67eadad639");
        setProperty(null, bean, expr, value);
    }

    @Override
    public void setProperty(JexlContext context, Object bean, String expr, Object value) {
        writeline("/home/ubuntu/results/coverage/Engine/Engine_8_10.coverage", "befb8e6c-50a8-4d7a-98ce-781824aed272");
        if (context == null) {
            writeline("/home/ubuntu/results/coverage/Engine/Engine_8_10.coverage", "5e960308-0039-4876-87cf-28bf4c414a07");
            context = EMPTY_CONTEXT;
        }
        writeline("/home/ubuntu/results/coverage/Engine/Engine_8_10.coverage", "84355149-a932-4ffe-90d7-213c0fd8a177");
        // synthetize expr using registers
        String src = trimSource(expr);
        writeline("/home/ubuntu/results/coverage/Engine/Engine_8_10.coverage", "3650d869-3395-4c10-bb14-55e2600e9b97");
        src = "#0" + (src.charAt(0) == '[' ? "" : ".") + src + "=" + "#1";
        writeline("/home/ubuntu/results/coverage/Engine/Engine_8_10.coverage", "1d6c2ada-0421-4b20-9d69-c2f5294a4107");
        try {
            writeline("/home/ubuntu/results/coverage/Engine/Engine_8_10.coverage", "27a917c3-0a95-4047-ac10-fcce6806a8f0");
            final JexlInfo info = debug ? createInfo() : null;
            writeline("/home/ubuntu/results/coverage/Engine/Engine_8_10.coverage", "5034ed7d-93b5-4d1c-9f9f-6a7fb19fadd6");
            final Scope scope = new Scope(null, "#0", "#1");
            writeline("/home/ubuntu/results/coverage/Engine/Engine_8_10.coverage", "a5828ae7-6f17-4449-a8a7-f451808c55db");
            final ASTJexlScript script = parse(info, src, scope, true, true);
            writeline("/home/ubuntu/results/coverage/Engine/Engine_8_10.coverage", "951f742e-b7d8-4708-b901-7a8910e46dc5");
            final JexlNode node = script.jjtGetChild(0);
            writeline("/home/ubuntu/results/coverage/Engine/Engine_8_10.coverage", "f74a2ff4-1174-4eb5-8209-13955b935873");
            final Scope.Frame frame = script.createFrame(bean, value);
            writeline("/home/ubuntu/results/coverage/Engine/Engine_8_10.coverage", "4e7091fc-5036-4766-9fda-f9bd5e6beecc");
            final Interpreter interpreter = createInterpreter(context, frame);
            writeline("/home/ubuntu/results/coverage/Engine/Engine_8_10.coverage", "dc2d2e1b-bbd1-4fb9-aa04-60a35664d435");
            node.jjtAccept(interpreter, null);
        } catch (JexlException xjexl) {
            writeline("/home/ubuntu/results/coverage/Engine/Engine_8_10.coverage", "a88ec0db-4505-4538-8e0c-13f6b548bb4c");
            if (silent) {
                writeline("/home/ubuntu/results/coverage/Engine/Engine_8_10.coverage", "b2de097c-3cb6-4616-9285-954c3039ff64");
                logger.warn(xjexl.getMessage(), xjexl.getCause());
                writeline("/home/ubuntu/results/coverage/Engine/Engine_8_10.coverage", "51718f06-bece-4241-9481-1ef60e2b9db8");
                return;
            }
            writeline("/home/ubuntu/results/coverage/Engine/Engine_8_10.coverage", "2f43c836-f10d-4069-91f0-dcf31daf9025");
            throw xjexl.clean();
        }
    }

    @Override
    public Object invokeMethod(Object obj, String meth, Object... args) {
        writeline("/home/ubuntu/results/coverage/Engine/Engine_8_10.coverage", "b53f8b86-2ec0-4387-9e3a-2475d919906f");
        JexlException xjexl = null;
        writeline("/home/ubuntu/results/coverage/Engine/Engine_8_10.coverage", "37b362c8-f7ab-42db-8b08-a6a8e8f47e7b");
        Object result = null;
        writeline("/home/ubuntu/results/coverage/Engine/Engine_8_10.coverage", "badb159f-3b48-49d0-bb21-b65c662c70de");
        final JexlInfo info = debug ? createInfo() : null;
        writeline("/home/ubuntu/results/coverage/Engine/Engine_8_10.coverage", "fccaf77a-9d71-4f65-a7b9-5dd246fe9b33");
        try {
            writeline("/home/ubuntu/results/coverage/Engine/Engine_8_10.coverage", "fc5146a0-1cea-41cf-96ee-62f575278b20");
            JexlMethod method = uberspect.getMethod(obj, meth, args);
            writeline("/home/ubuntu/results/coverage/Engine/Engine_8_10.coverage", "bade5b5b-7266-4b02-90ee-a068e0f143d3");
            if (method == null && arithmetic.narrowArguments(args)) {
                writeline("/home/ubuntu/results/coverage/Engine/Engine_8_10.coverage", "1ceec595-3bf5-4c83-ac08-80a0deb8ac3d");
                method = uberspect.getMethod(obj, meth, args);
            }
            writeline("/home/ubuntu/results/coverage/Engine/Engine_8_10.coverage", "ea5c8be8-7998-4e68-88e7-727c17e4a5fc");
            if (method != null) {
                writeline("/home/ubuntu/results/coverage/Engine/Engine_8_10.coverage", "47b30dc2-e113-43e0-984e-e1b4a01d8a7d");
                result = method.invoke(obj, args);
            } else {
                writeline("/home/ubuntu/results/coverage/Engine/Engine_8_10.coverage", "9ef31a3c-fdcb-427b-9c77-e52e31fb13b7");
                xjexl = new JexlException.Method(info, meth, null);
            }
        } catch (JexlException xany) {
            writeline("/home/ubuntu/results/coverage/Engine/Engine_8_10.coverage", "2a77c369-5ad4-4c69-9c71-3d4355a6ba29");
            xjexl = xany;
        } catch (Exception xany) {
            writeline("/home/ubuntu/results/coverage/Engine/Engine_8_10.coverage", "4c0007b9-ce5b-4c3d-ae7b-ae928d26d79b");
            xjexl = new JexlException.Method(info, meth, xany);
        }
        writeline("/home/ubuntu/results/coverage/Engine/Engine_8_10.coverage", "35130b11-37c3-4443-b814-87e2a79674fa");
        if (xjexl != null) {
            writeline("/home/ubuntu/results/coverage/Engine/Engine_8_10.coverage", "26fc0e08-cfc4-443a-ad50-a0b51ced687f");
            if (silent) {
                writeline("/home/ubuntu/results/coverage/Engine/Engine_8_10.coverage", "1b6f60b4-98f3-4604-a785-ecdb6c120976");
                logger.warn(xjexl.getMessage(), xjexl.getCause());
                writeline("/home/ubuntu/results/coverage/Engine/Engine_8_10.coverage", "2e7c65fd-7330-4125-9c76-fa912dd96c31");
                result = null;
            } else {
                writeline("/home/ubuntu/results/coverage/Engine/Engine_8_10.coverage", "43073af0-5200-4c63-a90b-2262ea69ded1");
                throw xjexl.clean();
            }
        }
        writeline("/home/ubuntu/results/coverage/Engine/Engine_8_10.coverage", "4360b0de-ddc4-4f89-8f52-99914b566a5d");
        return result;
    }

    @Override
    public <T> T newInstance(Class<? extends T> clazz, Object... args) {
        writeline("/home/ubuntu/results/coverage/Engine/Engine_8_10.coverage", "a9efe541-09e0-4e2e-89d0-0a799349d57d");
        return clazz.cast(doCreateInstance(clazz, args));
    }

    @Override
    public Object newInstance(String clazz, Object... args) {
        writeline("/home/ubuntu/results/coverage/Engine/Engine_8_10.coverage", "cc5c2d20-4377-486c-9e2e-4225ed2a864d");
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
        writeline("/home/ubuntu/results/coverage/Engine/Engine_8_10.coverage", "eec1c741-a4b8-4d0e-be05-bb7fb07a2fa5");
        JexlException xjexl = null;
        writeline("/home/ubuntu/results/coverage/Engine/Engine_8_10.coverage", "8ff59a64-f67f-4b18-972f-aa572c07a87f");
        Object result = null;
        writeline("/home/ubuntu/results/coverage/Engine/Engine_8_10.coverage", "61c7fab8-e48e-46c7-b965-e7762a31de85");
        final JexlInfo info = debug ? createInfo() : null;
        writeline("/home/ubuntu/results/coverage/Engine/Engine_8_10.coverage", "ad8aeca4-93a2-4837-af96-06bb7912a32f");
        try {
            writeline("/home/ubuntu/results/coverage/Engine/Engine_8_10.coverage", "c1c8dbe7-bb89-4659-bf82-58dfeb70bea8");
            JexlMethod ctor = uberspect.getConstructor(clazz, args);
            writeline("/home/ubuntu/results/coverage/Engine/Engine_8_10.coverage", "a401e572-9b9a-4793-a4b9-997efbcc1a4e");
            if (ctor == null && arithmetic.narrowArguments(args)) {
                writeline("/home/ubuntu/results/coverage/Engine/Engine_8_10.coverage", "ddc02889-9909-48f4-96be-42bd7420138c");
                ctor = uberspect.getConstructor(clazz, args);
            }
            writeline("/home/ubuntu/results/coverage/Engine/Engine_8_10.coverage", "fff7b2ab-bc8f-4efc-9833-db960df6a4ad");
            if (ctor != null) {
                writeline("/home/ubuntu/results/coverage/Engine/Engine_8_10.coverage", "73171d2c-be53-4b1e-9546-21706abb37e4");
                result = ctor.invoke(clazz, args);
            } else {
                writeline("/home/ubuntu/results/coverage/Engine/Engine_8_10.coverage", "a7863f48-eccf-40e1-aa9e-93a505baa969");
                xjexl = new JexlException.Method(info, clazz.toString(), null);
            }
        } catch (JexlException xany) {
            writeline("/home/ubuntu/results/coverage/Engine/Engine_8_10.coverage", "228457ed-2340-4659-ab88-a9d59e1a446c");
            xjexl = xany;
        } catch (Exception xany) {
            writeline("/home/ubuntu/results/coverage/Engine/Engine_8_10.coverage", "6ce39184-872a-4b15-ad4b-1778e04367fe");
            xjexl = new JexlException.Method(info, clazz.toString(), xany);
        }
        writeline("/home/ubuntu/results/coverage/Engine/Engine_8_10.coverage", "765a16c9-9fa3-41d2-bc38-60f84dd358b4");
        if (xjexl != null) {
            writeline("/home/ubuntu/results/coverage/Engine/Engine_8_10.coverage", "d47637af-ee92-42ca-825d-12e4d3a67d60");
            if (silent) {
                writeline("/home/ubuntu/results/coverage/Engine/Engine_8_10.coverage", "2989d77d-2e8c-452a-b748-b6411a148a46");
                logger.warn(xjexl.getMessage(), xjexl.getCause());
                writeline("/home/ubuntu/results/coverage/Engine/Engine_8_10.coverage", "8eaf7262-58fc-44bf-8fd0-c3f5743fbf45");
                return null;
            }
            writeline("/home/ubuntu/results/coverage/Engine/Engine_8_10.coverage", "f7abcbe4-99c6-4260-87f4-a1668c03822e");
            throw xjexl.clean();
        }
        writeline("/home/ubuntu/results/coverage/Engine/Engine_8_10.coverage", "fa0b41f0-f851-4d72-93d7-58f020f67b52");
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
        writeline("/home/ubuntu/results/coverage/Engine/Engine_8_10.coverage", "fd5514d7-c858-4505-b969-bf6c2a9dfafd");
        VarCollector collector = new VarCollector();
        writeline("/home/ubuntu/results/coverage/Engine/Engine_8_10.coverage", "abbc354e-0136-4fad-821c-9f097d3946a7");
        getVariables(script, script, collector);
        writeline("/home/ubuntu/results/coverage/Engine/Engine_8_10.coverage", "94d8caa6-e645-4471-a466-03987dffcf36");
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
        writeline("/home/ubuntu/results/coverage/Engine/Engine_8_10.coverage", "6e4d6b35-ccae-4a04-99fc-719a275c3e20");
        if (node instanceof ASTIdentifier) {
            writeline("/home/ubuntu/results/coverage/Engine/Engine_8_10.coverage", "827bf400-8eeb-4624-bb35-a4cab5fd6d2c");
            JexlNode parent = node.jjtGetParent();
            writeline("/home/ubuntu/results/coverage/Engine/Engine_8_10.coverage", "2f703fb0-6587-4118-800c-a828b2013a4b");
            if (parent instanceof ASTMethodNode || parent instanceof ASTFunctionNode) {
                writeline("/home/ubuntu/results/coverage/Engine/Engine_8_10.coverage", "696cf712-fe5a-4785-9eac-7965c26b02c5");
                // skip identifiers for methods and functions
                collector.collect(null);
                writeline("/home/ubuntu/results/coverage/Engine/Engine_8_10.coverage", "36aad125-0384-4bd5-a120-e9fd4800bd11");
                return;
            }
            writeline("/home/ubuntu/results/coverage/Engine/Engine_8_10.coverage", "a9ef57a9-f406-43a5-adca-31e9277607fc");
            ASTIdentifier identifier = (ASTIdentifier) node;
            writeline("/home/ubuntu/results/coverage/Engine/Engine_8_10.coverage", "fcacfd84-0b51-4a3c-9bc9-8af25c06b1ab");
            int symbol = identifier.getSymbol();
            writeline("/home/ubuntu/results/coverage/Engine/Engine_8_10.coverage", "184d5482-0e6f-4bee-9692-26711a38234e");
            // symbols that are hoisted are considered "global" variables
            if (symbol >= 0 && script != null && !script.isHoistedSymbol(symbol)) {
                writeline("/home/ubuntu/results/coverage/Engine/Engine_8_10.coverage", "2dbb8185-7491-4cc3-8420-281dc15df302");
                collector.collect(null);
            } else {
                writeline("/home/ubuntu/results/coverage/Engine/Engine_8_10.coverage", "7a0e6582-aeb5-4c2f-b583-eba91ae4fe70");
                // start collecting from identifier
                collector.collect(identifier);
                writeline("/home/ubuntu/results/coverage/Engine/Engine_8_10.coverage", "4aa1c5cf-6388-43d4-bbba-a0b277e649ee");
                collector.add(identifier.getName());
            }
        } else if (node instanceof ASTIdentifierAccess) {
            writeline("/home/ubuntu/results/coverage/Engine/Engine_8_10.coverage", "20e865c2-b3cf-443d-95b9-32044a2e93f1");
            JexlNode parent = node.jjtGetParent();
            writeline("/home/ubuntu/results/coverage/Engine/Engine_8_10.coverage", "351bcd4f-8f5d-44ff-9f0e-5e115befa54a");
            if (parent instanceof ASTMethodNode || parent instanceof ASTFunctionNode) {
                writeline("/home/ubuntu/results/coverage/Engine/Engine_8_10.coverage", "4a3714c1-054e-42c4-9c1c-13059c380fca");
                // skip identifiers for methods and functions
                collector.collect(null);
                writeline("/home/ubuntu/results/coverage/Engine/Engine_8_10.coverage", "3c28a644-01c7-40b0-b967-df30c3e16f95");
                return;
            }
            writeline("/home/ubuntu/results/coverage/Engine/Engine_8_10.coverage", "d02c3d4b-5a96-4c02-ace5-3106dfbfdb91");
            // belt and suspender since an identifier should have been seen first
            if (collector.isCollecting()) {
                writeline("/home/ubuntu/results/coverage/Engine/Engine_8_10.coverage", "d8ae98e0-843c-4d3f-8627-acb4f643a1f4");
                collector.add(((ASTIdentifierAccess) node).getName());
            }
        } else if (node instanceof ASTArrayAccess) {
            writeline("/home/ubuntu/results/coverage/Engine/Engine_8_10.coverage", "493a99ed-1629-4bc3-94b7-d271d15269e4");
            int num = node.jjtGetNumChildren();
            writeline("/home/ubuntu/results/coverage/Engine/Engine_8_10.coverage", "213696ed-6031-4450-8394-0f241eb642ca");
            // collect only if array access is const and follows an identifier
            boolean collecting = collector.isCollecting();
            writeline("/home/ubuntu/results/coverage/Engine/Engine_8_10.coverage", "753d5526-ae02-4157-8f54-210a52d460c4");
            for (int i = 0; i < num; ++i) {
                writeline("/home/ubuntu/results/coverage/Engine/Engine_8_10.coverage", "72ae56a4-8ca7-4123-b6db-fb555355e12e");
                JexlNode child = node.jjtGetChild(i);
                writeline("/home/ubuntu/results/coverage/Engine/Engine_8_10.coverage", "bcf448c5-8be5-474d-adcf-975486a4d98c");
                if (collecting && child.isConstant()) {
                    writeline("/home/ubuntu/results/coverage/Engine/Engine_8_10.coverage", "5e461de7-0815-4e52-b3a0-451dd52e9d57");
                    String image = child.toString();
                    writeline("/home/ubuntu/results/coverage/Engine/Engine_8_10.coverage", "1adcdd5e-ab33-473d-aeca-44db83a7f17f");
                    if (image == null) {
                        writeline("/home/ubuntu/results/coverage/Engine/Engine_8_10.coverage", "c1c1ba50-3b6c-4bb1-9095-835a15f2bc88");
                        image = new Debugger().data(child);
                    }
                    writeline("/home/ubuntu/results/coverage/Engine/Engine_8_10.coverage", "33038a4c-32da-4d40-86d8-475f5a70cecd");
                    collector.add(image);
                } else {
                    writeline("/home/ubuntu/results/coverage/Engine/Engine_8_10.coverage", "640b89d7-11f5-45b4-a988-37aeeb5ea0f2");
                    collecting = false;
                    writeline("/home/ubuntu/results/coverage/Engine/Engine_8_10.coverage", "9d048f38-bdb2-421f-a9a9-f20a94f04e3a");
                    collector.collect(null);
                    writeline("/home/ubuntu/results/coverage/Engine/Engine_8_10.coverage", "e48f2e9d-909a-43ac-94fd-96ae8eba4baf");
                    getVariables(script, child, collector);
                }
            }
        } else {
            writeline("/home/ubuntu/results/coverage/Engine/Engine_8_10.coverage", "4e3a78c7-b2f8-4417-b188-9819ffcc0003");
            int num = node.jjtGetNumChildren();
            writeline("/home/ubuntu/results/coverage/Engine/Engine_8_10.coverage", "e4f6f861-9c66-4a62-8fd0-43ccdb05b87b");
            for (int i = 0; i < num; ++i) {
                writeline("/home/ubuntu/results/coverage/Engine/Engine_8_10.coverage", "452d5a74-0e38-4a07-801e-00fc262d5ef0");
                getVariables(script, node.jjtGetChild(i), collector);
            }
            writeline("/home/ubuntu/results/coverage/Engine/Engine_8_10.coverage", "b8951280-86cb-4803-a2f3-28e2e3adc4b9");
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
        writeline("/home/ubuntu/results/coverage/Engine/Engine_8_10.coverage", "b94520f2-2ce1-4c77-8eb0-a6651ffe30e7");
        return script.getParameters();
    }

    /**
     * Gets the array of local variable from a script.
     * @param script the script
     * @return the local variables array which may be empty (but not null) if no local variables were defined
     * @since 3.0
     */
    protected String[] getLocalVariables(JexlScript script) {
        writeline("/home/ubuntu/results/coverage/Engine/Engine_8_10.coverage", "bde78566-ac15-4d91-b953-440457688426");
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
        writeline("/home/ubuntu/results/coverage/Engine/Engine_8_10.coverage", "467b4e8b-a717-4332-ae8b-17ae95e6f60b");
        final boolean cached = src.length() < cacheThreshold && cache != null;
        writeline("/home/ubuntu/results/coverage/Engine/Engine_8_10.coverage", "0894a9d0-8dd2-4bf8-8c2f-5290b731f7ba");
        ASTJexlScript script;
        writeline("/home/ubuntu/results/coverage/Engine/Engine_8_10.coverage", "2f750628-3b14-4fb9-ad6f-27cf4b3f194c");
        synchronized (parser) {
            writeline("/home/ubuntu/results/coverage/Engine/Engine_8_10.coverage", "8d17b036-76dc-4bff-a8a0-6f152cebb096");
            if (cached) {
                writeline("/home/ubuntu/results/coverage/Engine/Engine_8_10.coverage", "dcad63a5-1e64-4ad6-92e8-f12a8fab5d2c");
                script = cache.get(src);
                writeline("/home/ubuntu/results/coverage/Engine/Engine_8_10.coverage", "951c8125-bab6-4a04-96cc-0e8cf7abc6aa");
                if (script != null) {
                    writeline("/home/ubuntu/results/coverage/Engine/Engine_8_10.coverage", "ca23a03d-0f8c-4e73-8a47-309dfbdc57e2");
                    Scope f = script.getScope();
                    writeline("/home/ubuntu/results/coverage/Engine/Engine_8_10.coverage", "c82bb7e4-c378-4659-bd55-12b6d82c7bc4");
                    if ((f == null && scope == null) || (f != null && f.equals(scope))) {
                        writeline("/home/ubuntu/results/coverage/Engine/Engine_8_10.coverage", "69b6a0dd-6b53-4085-8c3e-6f6f6a1aea90");
                        return script;
                    }
                }
            }
            writeline("/home/ubuntu/results/coverage/Engine/Engine_8_10.coverage", "7b8c7c49-19f9-484f-b4f2-0d0787416038");
            script = parser.parse(info, src, scope, registers, expression);
            writeline("/home/ubuntu/results/coverage/Engine/Engine_8_10.coverage", "9d491798-5439-4c8a-8217-9771426f564a");
            if (cached) {
                writeline("/home/ubuntu/results/coverage/Engine/Engine_8_10.coverage", "ce7d1a74-1b04-4792-92f0-15f661fd53a9");
                cache.put(src, script);
            }
        }
        writeline("/home/ubuntu/results/coverage/Engine/Engine_8_10.coverage", "72d57264-8c9d-46ab-bdd2-6d44505b4698");
        return script;
    }

    /**
     * Trims the source from front and ending spaces.
     * @param str expression to clean
     * @return trimmed expression ending in a semi-colon
     */
    protected String trimSource(CharSequence str) {
        writeline("/home/ubuntu/results/coverage/Engine/Engine_8_10.coverage", "c858b2e9-612f-49db-b267-30cddcfe4a77");
        if (str != null) {
            writeline("/home/ubuntu/results/coverage/Engine/Engine_8_10.coverage", "1e1a8d31-716f-4e9d-958f-9e5429f5fcd0");
            int start = 0;
            writeline("/home/ubuntu/results/coverage/Engine/Engine_8_10.coverage", "8daf6301-4ace-4a56-8932-6f9cd607efce");
            int end = str.length();
            writeline("/home/ubuntu/results/coverage/Engine/Engine_8_10.coverage", "adf612a9-3263-4668-8b9c-8760985aa078");
            if (end > 0) {
                writeline("/home/ubuntu/results/coverage/Engine/Engine_8_10.coverage", "a1519a46-d02c-4d8d-b273-fc873e75d8ff");
                // trim front spaces
                while (start < end && Character.isSpaceChar(str.charAt(start))) {
                    writeline("/home/ubuntu/results/coverage/Engine/Engine_8_10.coverage", "64ad19e9-ae40-4d74-b047-5b90f755aa6a");
                    ++start;
                }
                writeline("/home/ubuntu/results/coverage/Engine/Engine_8_10.coverage", "edd3887b-dd2f-4cfa-924c-e43193edc0f9");
                // trim ending spaces
                while (end > 0 && Character.isSpaceChar(str.charAt(end - 1))) {
                    writeline("/home/ubuntu/results/coverage/Engine/Engine_8_10.coverage", "f5a656e5-fa42-4786-a30d-8e83c5fd6dbd");
                    --end;
                }
                writeline("/home/ubuntu/results/coverage/Engine/Engine_8_10.coverage", "fa66f5c4-9ec4-4500-bdaa-3585169382d4");
                return str.subSequence(start, end).toString();
            }
            writeline("/home/ubuntu/results/coverage/Engine/Engine_8_10.coverage", "54934c1a-7442-48f5-8d0e-c9145a46b21e");
            return "";
        }
        writeline("/home/ubuntu/results/coverage/Engine/Engine_8_10.coverage", "9e0a3ba9-346f-4131-a5b7-a72a61da9aed");
        return null;
    }

    /**
     * Gets and/or creates a default template engine.
     * @return a template engine
     */
    protected TemplateEngine jxlt() {
        writeline("/home/ubuntu/results/coverage/Engine/Engine_8_10.coverage", "bacc1121-3fad-4d29-87eb-ddcf2cb05ed8");
        TemplateEngine e = jxlt;
        writeline("/home/ubuntu/results/coverage/Engine/Engine_8_10.coverage", "b6041cf5-e23b-4b17-9efe-ba8d09ff4fc5");
        if (e == null) {
            writeline("/home/ubuntu/results/coverage/Engine/Engine_8_10.coverage", "bd2c6b7d-67a4-4ff5-aaed-04d9803af272");
            synchronized (this) {
                writeline("/home/ubuntu/results/coverage/Engine/Engine_8_10.coverage", "f0e89705-1b59-4319-abff-8fd894bc6a0a");
                if (jxlt == null) {
                    writeline("/home/ubuntu/results/coverage/Engine/Engine_8_10.coverage", "fa427122-771d-4e85-a63f-eab8c56efeb4");
                    e = new TemplateEngine(this, true, 0, '$', '#');
                    writeline("/home/ubuntu/results/coverage/Engine/Engine_8_10.coverage", "c99392bf-4cd6-4453-a4be-c2fb78266116");
                    jxlt = e;
                }
            }
        }
        writeline("/home/ubuntu/results/coverage/Engine/Engine_8_10.coverage", "247d5217-c975-4dc2-a8b8-063e25a062e9");
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
