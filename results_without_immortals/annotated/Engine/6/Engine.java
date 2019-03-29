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
        writelineStatic("/home/ubuntu/results/coverage/Engine/Engine_6_10.coverage", "f8783315-8bdf-49e1-b387-b49e69ed2278");
        if ((logger == null || logger.equals(LogFactory.getLog(JexlEngine.class))) && (strategy == null || strategy == JexlUberspect.JEXL_STRATEGY)) {
            writelineStatic("/home/ubuntu/results/coverage/Engine/Engine_6_10.coverage", "7e37e200-59fb-49c2-8981-4928a1741a19");
            return UberspectHolder.UBERSPECT;
        }
        writelineStatic("/home/ubuntu/results/coverage/Engine/Engine_6_10.coverage", "f74811a9-9a2d-4402-af4a-ff8a96c099ab");
        return new Uberspect(logger, strategy);
    }

    @Override
    public JexlUberspect getUberspect() {
        writeline("/home/ubuntu/results/coverage/Engine/Engine_6_10.coverage", "419a2459-2a74-4af4-8904-e013f4c6abbe");
        return uberspect;
    }

    @Override
    public JexlArithmetic getArithmetic() {
        writeline("/home/ubuntu/results/coverage/Engine/Engine_6_10.coverage", "7d2441aa-161d-4106-8fdd-bb465e3183a6");
        return arithmetic;
    }

    @Override
    public boolean isDebug() {
        writeline("/home/ubuntu/results/coverage/Engine/Engine_6_10.coverage", "54ecabdc-2ac7-4829-9837-9904ff056c6a");
        return this.debug;
    }

    @Override
    public boolean isSilent() {
        writeline("/home/ubuntu/results/coverage/Engine/Engine_6_10.coverage", "bb0105a7-c859-4bb8-8ed3-b9e46122b7fb");
        return this.silent;
    }

    @Override
    public boolean isStrict() {
        writeline("/home/ubuntu/results/coverage/Engine/Engine_6_10.coverage", "e7ae17d5-0509-48db-94f0-8c39dc106c22");
        return strict;
    }

    @Override
    public void setClassLoader(ClassLoader loader) {
        writeline("/home/ubuntu/results/coverage/Engine/Engine_6_10.coverage", "687b4588-aa0e-4b6e-9a13-77fbb67d52c0");
        uberspect.setClassLoader(loader);
    }

    @Override
    public Charset getCharset() {
        writeline("/home/ubuntu/results/coverage/Engine/Engine_6_10.coverage", "9c2fc13c-1919-4af8-af62-b9e26ede1eb8");
        return charset;
    }

    @Override
    public TemplateEngine createJxltEngine(boolean noScript, int cacheSize, char immediate, char deferred) {
        writeline("/home/ubuntu/results/coverage/Engine/Engine_6_10.coverage", "45b462c0-ffdc-4bc1-8749-d1299484904f");
        return new TemplateEngine(this, noScript, cacheSize, immediate, deferred);
    }

    /**
     * Swaps the current thread local context.
     * @param tls the context or null
     * @return the previous thread local context
     */
    protected JexlContext.ThreadLocal putThreadLocal(JexlContext.ThreadLocal tls) {
        writeline("/home/ubuntu/results/coverage/Engine/Engine_6_10.coverage", "4223166a-96ce-4a1f-b9fe-1957fde49482");
        JexlContext.ThreadLocal local = CONTEXT.get();
        writeline("/home/ubuntu/results/coverage/Engine/Engine_6_10.coverage", "8509c12c-494a-45d6-9f3d-ba630e16cdbf");
        CONTEXT.set(tls);
        writeline("/home/ubuntu/results/coverage/Engine/Engine_6_10.coverage", "5bb9c7ea-1b4a-40b1-9fb8-1e3c16516316");
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
        writeline("/home/ubuntu/results/coverage/Engine/Engine_6_10.coverage", "a769f979-13c9-4495-81ca-7decba9e7384");
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
        writeline("/home/ubuntu/results/coverage/Engine/Engine_6_10.coverage", "d608b3f2-7c7e-40af-98ce-e1bec1fc0eb1");
        synchronized (parser) {
            writeline("/home/ubuntu/results/coverage/Engine/Engine_6_10.coverage", "dbe0526a-87cf-4cfa-abab-4db3fc941812");
            if (cache != null) {
                writeline("/home/ubuntu/results/coverage/Engine/Engine_6_10.coverage", "5f9c87f7-927a-44ed-a100-271e48d4d69a");
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
        writeline("/home/ubuntu/results/coverage/Engine/Engine_6_10.coverage", "d8695419-79cb-4d61-ac4f-6c0eae80726d");
        return new Interpreter(this, context, frame);
    }

    @Override
    public Script createScript(JexlInfo info, String scriptText, String[] names) {
        writeline("/home/ubuntu/results/coverage/Engine/Engine_6_10.coverage", "2cb19123-3724-4e6e-a13f-777f7ba33647");
        if (scriptText == null) {
            writeline("/home/ubuntu/results/coverage/Engine/Engine_6_10.coverage", "815a8602-0d9b-4e61-be55-b7c63d7f10e8");
            throw new NullPointerException("source is null");
        }
        writeline("/home/ubuntu/results/coverage/Engine/Engine_6_10.coverage", "77bf8a1c-a467-45a7-a6da-aa9e92a41c68");
        if (info == null && debug) {
            writeline("/home/ubuntu/results/coverage/Engine/Engine_6_10.coverage", "e09eceb9-c01b-4bee-92f2-b24ef4f2fa12");
            info = createInfo();
        }
        writeline("/home/ubuntu/results/coverage/Engine/Engine_6_10.coverage", "3c4fa363-bd02-4efb-b940-398d11fb16f6");
        String source = trimSource(scriptText);
        writeline("/home/ubuntu/results/coverage/Engine/Engine_6_10.coverage", "a5b01794-9fb4-4a9d-8dd7-1d057077d522");
        Scope scope = names == null ? null : new Scope(null, names);
        writeline("/home/ubuntu/results/coverage/Engine/Engine_6_10.coverage", "32c613ca-124c-4646-b8fe-4bfea5ae8258");
        ASTJexlScript tree = parse(info, source, scope, false, false);
        writeline("/home/ubuntu/results/coverage/Engine/Engine_6_10.coverage", "b38dd80f-7bf4-409b-ba64-bb7b26a633d3");
        return new Script(this, source, tree);
    }

    @Override
    public Script createExpression(JexlInfo info, String expression) {
        writeline("/home/ubuntu/results/coverage/Engine/Engine_6_10.coverage", "860bf155-0060-47e3-9f43-7c0272909e67");
        if (expression == null) {
            writeline("/home/ubuntu/results/coverage/Engine/Engine_6_10.coverage", "9897466a-cf06-43e0-b4f9-89af73cc3424");
            throw new NullPointerException("source is null");
        }
        writeline("/home/ubuntu/results/coverage/Engine/Engine_6_10.coverage", "705ae8ca-0477-4af6-9146-ff60ce4f90b5");
        if (info == null && debug) {
            writeline("/home/ubuntu/results/coverage/Engine/Engine_6_10.coverage", "8af4223b-bff0-4f36-9c3a-224f944e41d4");
            info = createInfo();
        }
        writeline("/home/ubuntu/results/coverage/Engine/Engine_6_10.coverage", "54d21134-e78c-4398-bc79-a410dd8bea8f");
        String source = trimSource(expression);
        writeline("/home/ubuntu/results/coverage/Engine/Engine_6_10.coverage", "95757efb-7f1d-4ea3-bba7-b50a1a2a630e");
        ASTJexlScript tree = parse(info, source, null, false, true);
        writeline("/home/ubuntu/results/coverage/Engine/Engine_6_10.coverage", "c5baa148-f713-4cf3-8c69-7fffb5d8f7fd");
        return new Script(this, source, tree);
    }

    @Override
    public Object getProperty(Object bean, String expr) {
        writeline("/home/ubuntu/results/coverage/Engine/Engine_6_10.coverage", "b0ee7d7c-a12e-49fe-8a23-6f734ecca2f5");
        return getProperty(null, bean, expr);
    }

    @Override
    public Object getProperty(JexlContext context, Object bean, String expr) {
        writeline("/home/ubuntu/results/coverage/Engine/Engine_6_10.coverage", "7cec1cc5-c003-4337-96b7-6e266caafe6f");
        if (context == null) {
            writeline("/home/ubuntu/results/coverage/Engine/Engine_6_10.coverage", "83259312-dfc7-4395-a911-43c7f5becd03");
            context = EMPTY_CONTEXT;
        }
        writeline("/home/ubuntu/results/coverage/Engine/Engine_6_10.coverage", "edaa1291-4825-418b-b9e4-889ef164d69d");
        // synthetize expr using register
        String src = trimSource(expr);
        writeline("/home/ubuntu/results/coverage/Engine/Engine_6_10.coverage", "2770f321-32d5-4b44-8777-20e95334a29c");
        src = "#0" + (src.charAt(0) == '[' ? "" : ".") + src;
        writeline("/home/ubuntu/results/coverage/Engine/Engine_6_10.coverage", "3d250504-b48a-4200-9a65-425b170a610a");
        try {
            writeline("/home/ubuntu/results/coverage/Engine/Engine_6_10.coverage", "54172b6f-16ec-4f5f-a3e6-dc6bb688527f");
            final JexlInfo info = debug ? createInfo() : null;
            writeline("/home/ubuntu/results/coverage/Engine/Engine_6_10.coverage", "f1cc0aaf-bfe4-470e-9cfb-0beab1a3e3c4");
            final Scope scope = new Scope(null, "#0");
            writeline("/home/ubuntu/results/coverage/Engine/Engine_6_10.coverage", "cfb1248c-6170-4af5-b118-36e0abd168ac");
            final ASTJexlScript script = parse(info, src, scope, true, true);
            writeline("/home/ubuntu/results/coverage/Engine/Engine_6_10.coverage", "15d14877-1c2f-4453-8fbe-12c3ee223a8a");
            final JexlNode node = script.jjtGetChild(0);
            writeline("/home/ubuntu/results/coverage/Engine/Engine_6_10.coverage", "db0486bb-7df6-429b-aa7f-899fc7989021");
            final Scope.Frame frame = script.createFrame(bean);
            writeline("/home/ubuntu/results/coverage/Engine/Engine_6_10.coverage", "64bc0f52-7b8f-485b-b58e-c426c5ca5571");
            final Interpreter interpreter = createInterpreter(context, frame);
            writeline("/home/ubuntu/results/coverage/Engine/Engine_6_10.coverage", "be2ad1e7-1b47-4905-b852-6b095c1a1605");
            return node.jjtAccept(interpreter, null);
        } catch (JexlException xjexl) {
            writeline("/home/ubuntu/results/coverage/Engine/Engine_6_10.coverage", "bc1fa705-83a1-4fba-ab0f-f56b234a671c");
            if (silent) {
                writeline("/home/ubuntu/results/coverage/Engine/Engine_6_10.coverage", "e25ccb17-0b51-42fd-a5d4-48bcc859ff2f");
                logger.warn(xjexl.getMessage(), xjexl.getCause());
                writeline("/home/ubuntu/results/coverage/Engine/Engine_6_10.coverage", "4edb5df3-f872-4546-b91a-2ededff78415");
                return null;
            }
            writeline("/home/ubuntu/results/coverage/Engine/Engine_6_10.coverage", "91d00372-57ba-4ea9-838a-414e9e042a3b");
            throw xjexl.clean();
        }
    }

    @Override
    public void setProperty(Object bean, String expr, Object value) {
        writeline("/home/ubuntu/results/coverage/Engine/Engine_6_10.coverage", "61cfe1a8-1668-41bd-af20-5c65eee2a10b");
        setProperty(null, bean, expr, value);
    }

    @Override
    public void setProperty(JexlContext context, Object bean, String expr, Object value) {
        writeline("/home/ubuntu/results/coverage/Engine/Engine_6_10.coverage", "66830170-559e-4e44-a404-653c47a4a237");
        if (context == null) {
            writeline("/home/ubuntu/results/coverage/Engine/Engine_6_10.coverage", "96da43a9-5497-4d90-a0e7-485b77996fdf");
            context = EMPTY_CONTEXT;
        }
        writeline("/home/ubuntu/results/coverage/Engine/Engine_6_10.coverage", "fe66eb6a-046a-453d-b378-8801930113fb");
        // synthetize expr using registers
        String src = trimSource(expr);
        writeline("/home/ubuntu/results/coverage/Engine/Engine_6_10.coverage", "4eea155b-5ae3-4e9c-b354-bb88945192c5");
        src = "#0" + (src.charAt(0) == '[' ? "" : ".") + src + "=" + "#1";
        writeline("/home/ubuntu/results/coverage/Engine/Engine_6_10.coverage", "5d6e072d-f48a-48f7-be82-02cbe329a905");
        try {
            writeline("/home/ubuntu/results/coverage/Engine/Engine_6_10.coverage", "9fddc23e-a24d-493a-a7fb-41b9d9714ef3");
            final JexlInfo info = debug ? createInfo() : null;
            writeline("/home/ubuntu/results/coverage/Engine/Engine_6_10.coverage", "6fdaf948-9395-4adc-96d8-ac674fb9f6ba");
            final Scope scope = new Scope(null, "#0", "#1");
            writeline("/home/ubuntu/results/coverage/Engine/Engine_6_10.coverage", "e5fb473b-b07a-4823-91f2-0157c72f6fcd");
            final ASTJexlScript script = parse(info, src, scope, true, true);
            writeline("/home/ubuntu/results/coverage/Engine/Engine_6_10.coverage", "a0fbba59-0635-4750-9218-c0110f73a011");
            final JexlNode node = script.jjtGetChild(0);
            writeline("/home/ubuntu/results/coverage/Engine/Engine_6_10.coverage", "6bc808c9-f5f7-457a-bf5f-34ce76e6529e");
            final Scope.Frame frame = script.createFrame(bean, value);
            writeline("/home/ubuntu/results/coverage/Engine/Engine_6_10.coverage", "ce33e9f1-eec5-4ddc-b5a2-bd140729d0a2");
            final Interpreter interpreter = createInterpreter(context, frame);
            writeline("/home/ubuntu/results/coverage/Engine/Engine_6_10.coverage", "6812add3-3c4b-45f2-b760-87ca70ca7a76");
            node.jjtAccept(interpreter, null);
        } catch (JexlException xjexl) {
            writeline("/home/ubuntu/results/coverage/Engine/Engine_6_10.coverage", "0b782fda-79f2-4390-bd07-b5f12fcdad54");
            if (silent) {
                writeline("/home/ubuntu/results/coverage/Engine/Engine_6_10.coverage", "527c58d3-856c-423e-8250-4c5dce81b8c5");
                logger.warn(xjexl.getMessage(), xjexl.getCause());
                writeline("/home/ubuntu/results/coverage/Engine/Engine_6_10.coverage", "43a6b401-a0e9-4604-9496-7c33c82e86eb");
                return;
            }
            writeline("/home/ubuntu/results/coverage/Engine/Engine_6_10.coverage", "6c6495bd-a35c-4018-9042-16df761543ef");
            throw xjexl.clean();
        }
    }

    @Override
    public Object invokeMethod(Object obj, String meth, Object... args) {
        writeline("/home/ubuntu/results/coverage/Engine/Engine_6_10.coverage", "0018c8c1-a7a8-4a03-bca9-6fbe85c5e4d3");
        JexlException xjexl = null;
        writeline("/home/ubuntu/results/coverage/Engine/Engine_6_10.coverage", "f2081e15-05c7-4e90-8d2f-6c541a5a4001");
        Object result = null;
        writeline("/home/ubuntu/results/coverage/Engine/Engine_6_10.coverage", "a44f6904-c951-4fc6-a94b-ac6c61baa8eb");
        final JexlInfo info = debug ? createInfo() : null;
        writeline("/home/ubuntu/results/coverage/Engine/Engine_6_10.coverage", "85838f21-bd34-4cdb-aebf-a711fceba54f");
        try {
            writeline("/home/ubuntu/results/coverage/Engine/Engine_6_10.coverage", "ca83c00c-a6c5-46dd-8ff6-fd3f73b8d6db");
            JexlMethod method = uberspect.getMethod(obj, meth, args);
            writeline("/home/ubuntu/results/coverage/Engine/Engine_6_10.coverage", "9e8420c0-3a01-4dc7-adb9-486a455f0d76");
            if (method == null && arithmetic.narrowArguments(args)) {
                writeline("/home/ubuntu/results/coverage/Engine/Engine_6_10.coverage", "1a28c5e6-cdfa-4502-96f6-652a0a84de30");
                method = uberspect.getMethod(obj, meth, args);
            }
            writeline("/home/ubuntu/results/coverage/Engine/Engine_6_10.coverage", "39a4fd2f-c43a-4ca9-b248-b8291a8ad218");
            if (method != null) {
                writeline("/home/ubuntu/results/coverage/Engine/Engine_6_10.coverage", "e697fcb3-800b-4721-aadc-eb6f75bd8197");
                result = method.invoke(obj, args);
            } else {
                writeline("/home/ubuntu/results/coverage/Engine/Engine_6_10.coverage", "7fbc4eaa-e534-41ea-845e-1548b2aa8990");
                xjexl = new JexlException.Method(info, meth, null);
            }
        } catch (JexlException xany) {
            writeline("/home/ubuntu/results/coverage/Engine/Engine_6_10.coverage", "1709449b-14fa-4eb6-936b-ceff0ca625d8");
            xjexl = xany;
        } catch (Exception xany) {
            writeline("/home/ubuntu/results/coverage/Engine/Engine_6_10.coverage", "291b3340-0072-453f-891a-ef088c2c9c70");
            xjexl = new JexlException.Method(info, meth, xany);
        }
        writeline("/home/ubuntu/results/coverage/Engine/Engine_6_10.coverage", "141b99cb-31a8-44fe-a115-961c63ff906a");
        if (xjexl != null) {
            writeline("/home/ubuntu/results/coverage/Engine/Engine_6_10.coverage", "5c2833d2-3488-41ea-9c68-c871cd5cf7b1");
            if (silent) {
                writeline("/home/ubuntu/results/coverage/Engine/Engine_6_10.coverage", "d4b7153a-90c1-428a-882a-3a5b01df5ad2");
                logger.warn(xjexl.getMessage(), xjexl.getCause());
                writeline("/home/ubuntu/results/coverage/Engine/Engine_6_10.coverage", "4a59eb5f-fc81-4350-ba4f-729ba131f36a");
                result = null;
            } else {
                writeline("/home/ubuntu/results/coverage/Engine/Engine_6_10.coverage", "b307e907-75f0-490b-9b97-1c60fc1145b4");
                throw xjexl.clean();
            }
        }
        writeline("/home/ubuntu/results/coverage/Engine/Engine_6_10.coverage", "cb5cf9b2-b228-41fa-8efa-f6bacd9f715c");
        return result;
    }

    @Override
    public <T> T newInstance(Class<? extends T> clazz, Object... args) {
        writeline("/home/ubuntu/results/coverage/Engine/Engine_6_10.coverage", "cc2be2f9-fdbb-400b-8633-d45dffc57007");
        return clazz.cast(doCreateInstance(clazz, args));
    }

    @Override
    public Object newInstance(String clazz, Object... args) {
        writeline("/home/ubuntu/results/coverage/Engine/Engine_6_10.coverage", "0d78983d-5b3f-4a69-8e34-be4ac2c26a4e");
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
        writeline("/home/ubuntu/results/coverage/Engine/Engine_6_10.coverage", "905cb6f5-7f4c-4bf0-b988-725d7e52a0db");
        JexlException xjexl = null;
        writeline("/home/ubuntu/results/coverage/Engine/Engine_6_10.coverage", "865ad074-43d6-4506-b4d9-fd309021fcda");
        Object result = null;
        writeline("/home/ubuntu/results/coverage/Engine/Engine_6_10.coverage", "c7348b30-c860-4624-b970-bf45e634b152");
        final JexlInfo info = debug ? createInfo() : null;
        writeline("/home/ubuntu/results/coverage/Engine/Engine_6_10.coverage", "670b68db-28d2-45bf-98f0-962eeafb3227");
        try {
            writeline("/home/ubuntu/results/coverage/Engine/Engine_6_10.coverage", "cd04c4ff-1033-4896-a644-2f17a665e697");
            JexlMethod ctor = uberspect.getConstructor(clazz, args);
            writeline("/home/ubuntu/results/coverage/Engine/Engine_6_10.coverage", "3780f674-9e30-4efd-b49a-b8ef32f2280e");
            if (ctor == null && arithmetic.narrowArguments(args)) {
                writeline("/home/ubuntu/results/coverage/Engine/Engine_6_10.coverage", "21f57aa0-0182-46a5-ae05-63216b10c0a4");
                ctor = uberspect.getConstructor(clazz, args);
            }
            writeline("/home/ubuntu/results/coverage/Engine/Engine_6_10.coverage", "4e25633f-ef84-4c23-9e21-ae6f6d040c0a");
            if (ctor != null) {
                writeline("/home/ubuntu/results/coverage/Engine/Engine_6_10.coverage", "9b47818a-d9ef-4623-bb41-08ad67d1c791");
                result = ctor.invoke(clazz, args);
            } else {
                writeline("/home/ubuntu/results/coverage/Engine/Engine_6_10.coverage", "27c31e5e-b5b8-4b71-a594-d23c38634756");
                xjexl = new JexlException.Method(info, clazz.toString(), null);
            }
        } catch (JexlException xany) {
            writeline("/home/ubuntu/results/coverage/Engine/Engine_6_10.coverage", "a84f0b7a-ae4e-46e3-bafc-c9c574ccc3a1");
            xjexl = xany;
        } catch (Exception xany) {
            writeline("/home/ubuntu/results/coverage/Engine/Engine_6_10.coverage", "bc203365-e9f6-4809-9916-58a6394f9cd2");
            xjexl = new JexlException.Method(info, clazz.toString(), xany);
        }
        writeline("/home/ubuntu/results/coverage/Engine/Engine_6_10.coverage", "59de89ce-0c95-4554-8b5c-2330c7ab7d0e");
        if (xjexl != null) {
            writeline("/home/ubuntu/results/coverage/Engine/Engine_6_10.coverage", "fc7329df-cca8-4432-b51e-dc7b179d03d9");
            if (silent) {
                writeline("/home/ubuntu/results/coverage/Engine/Engine_6_10.coverage", "7a6b6209-b03a-4cd2-9744-48e519429fc1");
                logger.warn(xjexl.getMessage(), xjexl.getCause());
                writeline("/home/ubuntu/results/coverage/Engine/Engine_6_10.coverage", "6ef94ac8-e157-4e22-830b-74fe2efbef4f");
                return null;
            }
            writeline("/home/ubuntu/results/coverage/Engine/Engine_6_10.coverage", "61190b56-d093-4c52-bea7-8e901bce263c");
            throw xjexl.clean();
        }
        writeline("/home/ubuntu/results/coverage/Engine/Engine_6_10.coverage", "307acb03-d324-4055-99fb-15693a845f9a");
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
        writeline("/home/ubuntu/results/coverage/Engine/Engine_6_10.coverage", "8e5e1dce-8123-4db3-bc8a-ade51aab21c8");
        VarCollector collector = new VarCollector();
        writeline("/home/ubuntu/results/coverage/Engine/Engine_6_10.coverage", "8a941df9-708b-4871-a35d-ad66419ff423");
        getVariables(script, script, collector);
        writeline("/home/ubuntu/results/coverage/Engine/Engine_6_10.coverage", "ec86eb78-b2bf-487d-bcd5-bd00041736e8");
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
        writeline("/home/ubuntu/results/coverage/Engine/Engine_6_10.coverage", "7cfec950-9541-4469-9e67-f6ed63eea1cd");
        if (node instanceof ASTIdentifier) {
            writeline("/home/ubuntu/results/coverage/Engine/Engine_6_10.coverage", "647ea917-78ad-4fb9-9c94-bfdc1cc223e3");
            JexlNode parent = node.jjtGetParent();
            writeline("/home/ubuntu/results/coverage/Engine/Engine_6_10.coverage", "e57917fe-07b7-436b-9dd7-1b26c149fda5");
            if (parent instanceof ASTMethodNode || parent instanceof ASTFunctionNode) {
                writeline("/home/ubuntu/results/coverage/Engine/Engine_6_10.coverage", "86587a76-4622-465f-915a-f5cb85e3772c");
                // skip identifiers for methods and functions
                collector.collect(null);
                writeline("/home/ubuntu/results/coverage/Engine/Engine_6_10.coverage", "091cf2e5-19e8-48fa-9c10-6885a1263735");
                return;
            }
            writeline("/home/ubuntu/results/coverage/Engine/Engine_6_10.coverage", "828e208d-8a27-4763-b3a8-ac993e88f47b");
            ASTIdentifier identifier = (ASTIdentifier) node;
            writeline("/home/ubuntu/results/coverage/Engine/Engine_6_10.coverage", "4892d569-37f1-40e2-8d9a-53ee06154beb");
            int symbol = identifier.getSymbol();
            writeline("/home/ubuntu/results/coverage/Engine/Engine_6_10.coverage", "439c05ec-b8c9-49b7-966c-d9397ba2189c");
            // symbols that are hoisted are considered "global" variables
            if (symbol >= 0 && script != null && !script.isHoistedSymbol(symbol)) {
                writeline("/home/ubuntu/results/coverage/Engine/Engine_6_10.coverage", "72395ac2-0472-4723-bd44-c5c3d76c5d99");
                collector.collect(null);
            } else {
                writeline("/home/ubuntu/results/coverage/Engine/Engine_6_10.coverage", "f46cedfa-4167-4a1f-af80-982b7974652c");
                // start collecting from identifier
                collector.collect(identifier);
                writeline("/home/ubuntu/results/coverage/Engine/Engine_6_10.coverage", "cb40b34a-cf05-424a-a0c7-09065f805a42");
                collector.add(identifier.getName());
            }
        } else if (node instanceof ASTIdentifierAccess) {
            writeline("/home/ubuntu/results/coverage/Engine/Engine_6_10.coverage", "0559b435-d1b1-4fd6-b1d9-0c6794e00063");
            JexlNode parent = node.jjtGetParent();
            writeline("/home/ubuntu/results/coverage/Engine/Engine_6_10.coverage", "06e14467-4905-4e21-83f6-2ceef798c6ba");
            if (parent instanceof ASTMethodNode || parent instanceof ASTFunctionNode) {
                writeline("/home/ubuntu/results/coverage/Engine/Engine_6_10.coverage", "8e431aa3-67f1-4e36-97db-ac390c5d7e00");
                // skip identifiers for methods and functions
                collector.collect(null);
                writeline("/home/ubuntu/results/coverage/Engine/Engine_6_10.coverage", "888615f5-fa42-4f30-8c1e-78b66d1ca58d");
                return;
            }
            writeline("/home/ubuntu/results/coverage/Engine/Engine_6_10.coverage", "cafc2b33-a083-46bb-a6be-367f71255f47");
            // belt and suspender since an identifier should have been seen first
            if (collector.isCollecting()) {
                writeline("/home/ubuntu/results/coverage/Engine/Engine_6_10.coverage", "3ad85098-aa43-4f4b-93ab-0ab6280aa22f");
                collector.add(((ASTIdentifierAccess) node).getName());
            }
        } else if (node instanceof ASTArrayAccess) {
            writeline("/home/ubuntu/results/coverage/Engine/Engine_6_10.coverage", "dff8d550-5856-44c0-8342-d012e301f5a6");
            int num = node.jjtGetNumChildren();
            writeline("/home/ubuntu/results/coverage/Engine/Engine_6_10.coverage", "fd14e82d-9cab-4bf7-b439-0432fce6748d");
            // collect only if array access is const and follows an identifier
            boolean collecting = collector.isCollecting();
            writeline("/home/ubuntu/results/coverage/Engine/Engine_6_10.coverage", "51b7a810-d3ff-4950-9557-1f3ad4f2bb56");
            for (int i = 0; i < num; ++i) {
                writeline("/home/ubuntu/results/coverage/Engine/Engine_6_10.coverage", "09fea4d7-910e-4023-96df-19dd82494d1f");
                JexlNode child = node.jjtGetChild(i);
                writeline("/home/ubuntu/results/coverage/Engine/Engine_6_10.coverage", "69f82f1e-dca0-4c94-a133-1ae3a590f7a2");
                if (collecting && child.isConstant()) {
                    writeline("/home/ubuntu/results/coverage/Engine/Engine_6_10.coverage", "11488886-fff2-42c0-a772-46de8377e364");
                    String image = child.toString();
                    writeline("/home/ubuntu/results/coverage/Engine/Engine_6_10.coverage", "c618b6ea-c5a3-4d03-b4d0-98eb81c2a623");
                    if (image == null) {
                        writeline("/home/ubuntu/results/coverage/Engine/Engine_6_10.coverage", "d1e0c5b8-5de5-40ac-88d9-4df9d05bb480");
                        image = new Debugger().data(child);
                    }
                    writeline("/home/ubuntu/results/coverage/Engine/Engine_6_10.coverage", "a38a24f0-1f32-47c0-871f-159148440adb");
                    collector.add(image);
                } else {
                    writeline("/home/ubuntu/results/coverage/Engine/Engine_6_10.coverage", "6931285b-cec8-4992-9440-b61efc9990b5");
                    collecting = false;
                    writeline("/home/ubuntu/results/coverage/Engine/Engine_6_10.coverage", "58b712ce-b308-4cc1-bced-7f923495385c");
                    collector.collect(null);
                    writeline("/home/ubuntu/results/coverage/Engine/Engine_6_10.coverage", "5f28e718-d0d9-4ecd-ad92-c103eff25c3c");
                    getVariables(script, child, collector);
                }
            }
        } else {
            writeline("/home/ubuntu/results/coverage/Engine/Engine_6_10.coverage", "9a01d150-cc26-4074-b7b5-afb152fc70d3");
            int num = node.jjtGetNumChildren();
            writeline("/home/ubuntu/results/coverage/Engine/Engine_6_10.coverage", "616382e6-35c2-4443-8c27-ba386bc152e9");
            for (int i = 0; i < num; ++i) {
                writeline("/home/ubuntu/results/coverage/Engine/Engine_6_10.coverage", "56ccec24-6973-4c36-a18e-31cef8761539");
                getVariables(script, node.jjtGetChild(i), collector);
            }
            writeline("/home/ubuntu/results/coverage/Engine/Engine_6_10.coverage", "87124caa-30cd-4159-9850-75f449b151be");
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
        writeline("/home/ubuntu/results/coverage/Engine/Engine_6_10.coverage", "d2a9b608-ad16-4e07-9209-ca0ae364ee93");
        return script.getParameters();
    }

    /**
     * Gets the array of local variable from a script.
     * @param script the script
     * @return the local variables array which may be empty (but not null) if no local variables were defined
     * @since 3.0
     */
    protected String[] getLocalVariables(JexlScript script) {
        writeline("/home/ubuntu/results/coverage/Engine/Engine_6_10.coverage", "990ec5bd-8e6a-46d5-9136-0e038765f4f7");
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
        writeline("/home/ubuntu/results/coverage/Engine/Engine_6_10.coverage", "28112497-3c0d-4855-94a9-1516b37e821f");
        final boolean cached = src.length() < cacheThreshold && cache != null;
        writeline("/home/ubuntu/results/coverage/Engine/Engine_6_10.coverage", "14391468-6de9-4909-82aa-fd034ad19f4a");
        ASTJexlScript script;
        writeline("/home/ubuntu/results/coverage/Engine/Engine_6_10.coverage", "c9c60861-26af-4cd8-b39e-ec5e171b456a");
        synchronized (parser) {
            writeline("/home/ubuntu/results/coverage/Engine/Engine_6_10.coverage", "97bf3c53-c362-4e60-9c3d-d1624574f466");
            if (cached) {
                writeline("/home/ubuntu/results/coverage/Engine/Engine_6_10.coverage", "7015506a-46a2-4384-9549-3808a1ad6772");
                script = cache.get(src);
                writeline("/home/ubuntu/results/coverage/Engine/Engine_6_10.coverage", "1541eea8-f350-4bbc-8a94-09ef16d7f917");
                if (script != null) {
                    writeline("/home/ubuntu/results/coverage/Engine/Engine_6_10.coverage", "0896ca4f-425e-4220-b192-f0c56851c861");
                    Scope f = script.getScope();
                    writeline("/home/ubuntu/results/coverage/Engine/Engine_6_10.coverage", "e85a13c2-6e3b-4a1f-99a9-7983358d458a");
                    if ((f == null && scope == null) || (f != null && f.equals(scope))) {
                        writeline("/home/ubuntu/results/coverage/Engine/Engine_6_10.coverage", "bf4814b3-0a52-42cc-980f-fbb3cb741976");
                        return script;
                    }
                }
            }
            writeline("/home/ubuntu/results/coverage/Engine/Engine_6_10.coverage", "e12ff862-91d8-4e2a-a40d-8520e613a9bc");
            script = parser.parse(info, src, scope, registers, expression);
            writeline("/home/ubuntu/results/coverage/Engine/Engine_6_10.coverage", "af50d742-e03d-4724-82b6-722e46a8a8e8");
            if (cached) {
                writeline("/home/ubuntu/results/coverage/Engine/Engine_6_10.coverage", "aa8e41c6-1199-4b96-89dc-c33dc92db54b");
                cache.put(src, script);
            }
        }
        writeline("/home/ubuntu/results/coverage/Engine/Engine_6_10.coverage", "5b56cc2a-0286-4eaa-a3ff-d175c4cfbdc7");
        return script;
    }

    /**
     * Trims the source from front and ending spaces.
     * @param str expression to clean
     * @return trimmed expression ending in a semi-colon
     */
    protected String trimSource(CharSequence str) {
        writeline("/home/ubuntu/results/coverage/Engine/Engine_6_10.coverage", "b748487b-87c1-45b5-b74f-d528b00f3a6a");
        if (str != null) {
            writeline("/home/ubuntu/results/coverage/Engine/Engine_6_10.coverage", "f96d6b58-cfb2-48d0-8e83-04dc578ae87c");
            int start = 0;
            writeline("/home/ubuntu/results/coverage/Engine/Engine_6_10.coverage", "5196e8d6-e5d9-465a-8c03-838c46cfda58");
            int end = str.length();
            writeline("/home/ubuntu/results/coverage/Engine/Engine_6_10.coverage", "2e6c15df-ca71-4f64-bb43-d36e6c22c92b");
            if (end > 0) {
                writeline("/home/ubuntu/results/coverage/Engine/Engine_6_10.coverage", "9b48f3af-c965-4325-a8b9-c66ca090be13");
                // trim front spaces
                while (start < end && Character.isSpaceChar(str.charAt(start))) {
                    writeline("/home/ubuntu/results/coverage/Engine/Engine_6_10.coverage", "9bbd3cfd-671a-411e-bec9-82a19289204b");
                    ++start;
                }
                writeline("/home/ubuntu/results/coverage/Engine/Engine_6_10.coverage", "7f7c4beb-9f12-4030-b7ee-610abdb10926");
                // trim ending spaces
                while (end > 0 && Character.isSpaceChar(str.charAt(end - 1))) {
                    writeline("/home/ubuntu/results/coverage/Engine/Engine_6_10.coverage", "e9a9bf7d-f6d3-4d84-89fb-adca6d287909");
                    --end;
                }
                writeline("/home/ubuntu/results/coverage/Engine/Engine_6_10.coverage", "6c80b1b0-45be-4b76-a2ef-fc7d5b41d3be");
                return str.subSequence(start, end).toString();
            }
            writeline("/home/ubuntu/results/coverage/Engine/Engine_6_10.coverage", "3aeef123-e2b6-450a-a20a-91cb455b8bc6");
            return "";
        }
        writeline("/home/ubuntu/results/coverage/Engine/Engine_6_10.coverage", "a6634741-88e1-43b2-9028-fd99ffb1cd19");
        return null;
    }

    /**
     * Gets and/or creates a default template engine.
     * @return a template engine
     */
    protected TemplateEngine jxlt() {
        writeline("/home/ubuntu/results/coverage/Engine/Engine_6_10.coverage", "4c81e5bd-9b67-4fa5-b88b-a6fc4e012c2a");
        TemplateEngine e = jxlt;
        writeline("/home/ubuntu/results/coverage/Engine/Engine_6_10.coverage", "ebff8486-7cdf-4889-ba61-b7666884aac4");
        if (e == null) {
            writeline("/home/ubuntu/results/coverage/Engine/Engine_6_10.coverage", "10eada4a-5153-448b-b87c-fa832bf06efd");
            synchronized (this) {
                writeline("/home/ubuntu/results/coverage/Engine/Engine_6_10.coverage", "ed0ae1bc-b412-42c3-a87b-be6d597b195d");
                if (jxlt == null) {
                    writeline("/home/ubuntu/results/coverage/Engine/Engine_6_10.coverage", "fa537f70-0f69-4de9-b187-4867af833fa7");
                    e = new TemplateEngine(this, true, 0, '$', '#');
                    writeline("/home/ubuntu/results/coverage/Engine/Engine_6_10.coverage", "6094fb20-b401-4285-8da5-5559fd5f251b");
                    jxlt = e;
                }
            }
        }
        writeline("/home/ubuntu/results/coverage/Engine/Engine_6_10.coverage", "221da842-e20d-4881-a375-0bc658fc8ab0");
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
