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
        writelineStatic("/home/ubuntu/results/coverage/Engine/Engine_2_10.coverage", "fa93130b-23be-4859-bd1f-c0d8d47ca3ff");
        if ((logger == null || logger.equals(LogFactory.getLog(JexlEngine.class))) && (strategy == null || strategy == JexlUberspect.JEXL_STRATEGY)) {
            writelineStatic("/home/ubuntu/results/coverage/Engine/Engine_2_10.coverage", "d5cd484d-a21f-45d6-a3e4-e7f06cffd48d");
            return UberspectHolder.UBERSPECT;
        }
        writelineStatic("/home/ubuntu/results/coverage/Engine/Engine_2_10.coverage", "839ad5b5-c7d2-4085-889f-74a64517baad");
        return new Uberspect(logger, strategy);
    }

    @Override
    public JexlUberspect getUberspect() {
        writeline("/home/ubuntu/results/coverage/Engine/Engine_2_10.coverage", "c08c22f1-7e5d-4e34-ac0d-5a89c3e8c46d");
        return uberspect;
    }

    @Override
    public JexlArithmetic getArithmetic() {
        writeline("/home/ubuntu/results/coverage/Engine/Engine_2_10.coverage", "ed879e2f-273d-48bb-8bcc-e8415a8a9af3");
        return arithmetic;
    }

    @Override
    public boolean isDebug() {
        writeline("/home/ubuntu/results/coverage/Engine/Engine_2_10.coverage", "df815c94-0dcf-455f-a88d-86ecd6b79ddc");
        return this.debug;
    }

    @Override
    public boolean isSilent() {
        writeline("/home/ubuntu/results/coverage/Engine/Engine_2_10.coverage", "710f22d0-3e25-4b41-86f5-4a76c85bc1a2");
        return this.silent;
    }

    @Override
    public boolean isStrict() {
        writeline("/home/ubuntu/results/coverage/Engine/Engine_2_10.coverage", "10aa12d2-3b29-4b90-bd54-63dacec78af1");
        return strict;
    }

    @Override
    public void setClassLoader(ClassLoader loader) {
        writeline("/home/ubuntu/results/coverage/Engine/Engine_2_10.coverage", "5b52a483-e2e7-453a-b4a4-3761aeb0a48f");
        uberspect.setClassLoader(loader);
    }

    @Override
    public Charset getCharset() {
        writeline("/home/ubuntu/results/coverage/Engine/Engine_2_10.coverage", "99fb29ef-601a-4623-9e1f-fef415460771");
        return charset;
    }

    @Override
    public TemplateEngine createJxltEngine(boolean noScript, int cacheSize, char immediate, char deferred) {
        writeline("/home/ubuntu/results/coverage/Engine/Engine_2_10.coverage", "c6d2d206-0cb4-41f4-acc8-93852b3a040f");
        return new TemplateEngine(this, noScript, cacheSize, immediate, deferred);
    }

    /**
     * Swaps the current thread local context.
     * @param tls the context or null
     * @return the previous thread local context
     */
    protected JexlContext.ThreadLocal putThreadLocal(JexlContext.ThreadLocal tls) {
        writeline("/home/ubuntu/results/coverage/Engine/Engine_2_10.coverage", "984232b9-8a08-4e67-aa59-efedb1f14857");
        JexlContext.ThreadLocal local = CONTEXT.get();
        writeline("/home/ubuntu/results/coverage/Engine/Engine_2_10.coverage", "9d2c3272-90f8-4ae1-ac32-775a6996c092");
        CONTEXT.set(tls);
        writeline("/home/ubuntu/results/coverage/Engine/Engine_2_10.coverage", "6e0cd5e9-26dd-4f14-80c5-9fd69dd76996");
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
        writeline("/home/ubuntu/results/coverage/Engine/Engine_2_10.coverage", "593b0192-c190-4427-8fc9-4c05baa26d67");
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
        writeline("/home/ubuntu/results/coverage/Engine/Engine_2_10.coverage", "95876117-d72d-4f5b-98c9-d0c9ba2d4d40");
        synchronized (parser) {
            writeline("/home/ubuntu/results/coverage/Engine/Engine_2_10.coverage", "3ae8d675-4aad-4e30-ba2d-2c8f5ea54828");
            if (cache != null) {
                writeline("/home/ubuntu/results/coverage/Engine/Engine_2_10.coverage", "c34e8e40-a98b-4dc9-874d-87f2c3142dc1");
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
        writeline("/home/ubuntu/results/coverage/Engine/Engine_2_10.coverage", "47c8b183-32f6-4215-833a-4f0b846648e6");
        return new Interpreter(this, context, frame);
    }

    @Override
    public Script createScript(JexlInfo info, String scriptText, String[] names) {
        writeline("/home/ubuntu/results/coverage/Engine/Engine_2_10.coverage", "2395980d-360a-4a19-8297-243c3e2fb0ff");
        if (scriptText == null) {
            writeline("/home/ubuntu/results/coverage/Engine/Engine_2_10.coverage", "b376063b-8071-4a27-8906-ef341af12060");
            throw new NullPointerException("source is null");
        }
        writeline("/home/ubuntu/results/coverage/Engine/Engine_2_10.coverage", "7bab1ba0-26bd-444c-bfb8-efa9c8edc06f");
        if (info == null && debug) {
            writeline("/home/ubuntu/results/coverage/Engine/Engine_2_10.coverage", "f004def9-f8f7-4a84-8426-bc77a571416c");
            info = createInfo();
        }
        writeline("/home/ubuntu/results/coverage/Engine/Engine_2_10.coverage", "b4e3dbc6-adb2-4a01-ae88-1e4fdcefe736");
        String source = trimSource(scriptText);
        writeline("/home/ubuntu/results/coverage/Engine/Engine_2_10.coverage", "25c39ccc-0ff3-4b7b-ae39-5b1cef5b0921");
        Scope scope = names == null ? null : new Scope(null, names);
        writeline("/home/ubuntu/results/coverage/Engine/Engine_2_10.coverage", "8f1f74eb-a54d-43c9-acf8-0a65f15b6c41");
        ASTJexlScript tree = parse(info, source, scope, false, false);
        writeline("/home/ubuntu/results/coverage/Engine/Engine_2_10.coverage", "dae563f2-8f33-4234-ae58-225fd31237ca");
        return new Script(this, source, tree);
    }

    @Override
    public Script createExpression(JexlInfo info, String expression) {
        writeline("/home/ubuntu/results/coverage/Engine/Engine_2_10.coverage", "cdf6bedc-8440-4c6c-a1e1-129059eab171");
        if (expression == null) {
            writeline("/home/ubuntu/results/coverage/Engine/Engine_2_10.coverage", "6989b4a0-5e50-43ed-b364-2fdd2aa22af5");
            throw new NullPointerException("source is null");
        }
        writeline("/home/ubuntu/results/coverage/Engine/Engine_2_10.coverage", "ba47d464-e273-49b9-bf97-272c259ed744");
        if (info == null && debug) {
            writeline("/home/ubuntu/results/coverage/Engine/Engine_2_10.coverage", "d290669b-8caf-4dd1-b109-ede6e8cfc241");
            info = createInfo();
        }
        writeline("/home/ubuntu/results/coverage/Engine/Engine_2_10.coverage", "9890d155-75c8-40cf-9ffc-b0bc57877a0c");
        String source = trimSource(expression);
        writeline("/home/ubuntu/results/coverage/Engine/Engine_2_10.coverage", "1edd8a38-cf45-4703-aff0-42a1aff63915");
        ASTJexlScript tree = parse(info, source, null, false, true);
        writeline("/home/ubuntu/results/coverage/Engine/Engine_2_10.coverage", "8ae7bfe3-e853-4464-bdcd-6bbc646b6314");
        return new Script(this, source, tree);
    }

    @Override
    public Object getProperty(Object bean, String expr) {
        writeline("/home/ubuntu/results/coverage/Engine/Engine_2_10.coverage", "f1ce54ac-8336-4f27-8d87-22d76ce13be0");
        return getProperty(null, bean, expr);
    }

    @Override
    public Object getProperty(JexlContext context, Object bean, String expr) {
        writeline("/home/ubuntu/results/coverage/Engine/Engine_2_10.coverage", "79e36abc-8ceb-4ff8-818c-e7a89192b21b");
        if (context == null) {
            writeline("/home/ubuntu/results/coverage/Engine/Engine_2_10.coverage", "b1032e57-d863-42f7-8511-90f8072c94ff");
            context = EMPTY_CONTEXT;
        }
        writeline("/home/ubuntu/results/coverage/Engine/Engine_2_10.coverage", "b468128b-4381-4d12-bfe6-19fcf92c8180");
        // synthetize expr using register
        String src = trimSource(expr);
        writeline("/home/ubuntu/results/coverage/Engine/Engine_2_10.coverage", "5e09104d-8736-4c50-a767-e092d0b16f62");
        src = "#0" + (src.charAt(0) == '[' ? "" : ".") + src;
        writeline("/home/ubuntu/results/coverage/Engine/Engine_2_10.coverage", "8b6ab21c-74d7-4a6c-9da9-6427200e2caf");
        try {
            writeline("/home/ubuntu/results/coverage/Engine/Engine_2_10.coverage", "1b5bbad1-bb50-4b43-aad3-5c47339eead1");
            final JexlInfo info = debug ? createInfo() : null;
            writeline("/home/ubuntu/results/coverage/Engine/Engine_2_10.coverage", "8bf341e0-7b8f-48a6-a7f3-4672c6409e7f");
            final Scope scope = new Scope(null, "#0");
            writeline("/home/ubuntu/results/coverage/Engine/Engine_2_10.coverage", "9b392521-d089-4f1d-bf13-b624763556c0");
            final ASTJexlScript script = parse(info, src, scope, true, true);
            writeline("/home/ubuntu/results/coverage/Engine/Engine_2_10.coverage", "3cbff882-fb6d-4f76-baf3-6e0519ad4d51");
            final JexlNode node = script.jjtGetChild(0);
            writeline("/home/ubuntu/results/coverage/Engine/Engine_2_10.coverage", "121a4bed-db00-4f10-8a8d-7bf508b9be84");
            final Scope.Frame frame = script.createFrame(bean);
            writeline("/home/ubuntu/results/coverage/Engine/Engine_2_10.coverage", "74265200-4399-4d6d-9cb8-77e2010f95a4");
            final Interpreter interpreter = createInterpreter(context, frame);
            writeline("/home/ubuntu/results/coverage/Engine/Engine_2_10.coverage", "74c56be2-00cc-4a83-b31a-462fb9a73b3c");
            return node.jjtAccept(interpreter, null);
        } catch (JexlException xjexl) {
            writeline("/home/ubuntu/results/coverage/Engine/Engine_2_10.coverage", "8a60b27e-a9ea-4227-bc44-1abbf35dbeb8");
            if (silent) {
                writeline("/home/ubuntu/results/coverage/Engine/Engine_2_10.coverage", "f87220ab-ce7c-460b-9f35-8872754b44f5");
                logger.warn(xjexl.getMessage(), xjexl.getCause());
                writeline("/home/ubuntu/results/coverage/Engine/Engine_2_10.coverage", "7e8195f9-5054-409f-84a6-9fd6df03cf07");
                return null;
            }
            writeline("/home/ubuntu/results/coverage/Engine/Engine_2_10.coverage", "0370769c-389a-4983-906e-f678b2d11692");
            throw xjexl.clean();
        }
    }

    @Override
    public void setProperty(Object bean, String expr, Object value) {
        writeline("/home/ubuntu/results/coverage/Engine/Engine_2_10.coverage", "ad4add1c-7083-4a9f-8815-10b3c17f05f4");
        setProperty(null, bean, expr, value);
    }

    @Override
    public void setProperty(JexlContext context, Object bean, String expr, Object value) {
        writeline("/home/ubuntu/results/coverage/Engine/Engine_2_10.coverage", "7dd8d369-9dd3-47b5-ac3d-978fa9aeae08");
        if (context == null) {
            writeline("/home/ubuntu/results/coverage/Engine/Engine_2_10.coverage", "c7acba20-a047-46ba-a7e1-050fa013b038");
            context = EMPTY_CONTEXT;
        }
        writeline("/home/ubuntu/results/coverage/Engine/Engine_2_10.coverage", "d8448f52-52af-4860-b38b-c3b0f051910d");
        // synthetize expr using registers
        String src = trimSource(expr);
        writeline("/home/ubuntu/results/coverage/Engine/Engine_2_10.coverage", "c61b1c68-10fb-4c54-a339-e7d6eabbf489");
        src = "#0" + (src.charAt(0) == '[' ? "" : ".") + src + "=" + "#1";
        writeline("/home/ubuntu/results/coverage/Engine/Engine_2_10.coverage", "1531ea3a-2417-49e3-8663-5f80a691e317");
        try {
            writeline("/home/ubuntu/results/coverage/Engine/Engine_2_10.coverage", "4ea797ea-c1ec-4b02-abe9-2e89e8ea2021");
            final JexlInfo info = debug ? createInfo() : null;
            writeline("/home/ubuntu/results/coverage/Engine/Engine_2_10.coverage", "1e9b942e-6c6c-4f08-9873-3fbc8e57ea86");
            final Scope scope = new Scope(null, "#0", "#1");
            writeline("/home/ubuntu/results/coverage/Engine/Engine_2_10.coverage", "c25acea7-fe04-42ff-9af1-869c4941875e");
            final ASTJexlScript script = parse(info, src, scope, true, true);
            writeline("/home/ubuntu/results/coverage/Engine/Engine_2_10.coverage", "9e1dfcaf-d40d-42e7-ae16-7d5c4075fa39");
            final JexlNode node = script.jjtGetChild(0);
            writeline("/home/ubuntu/results/coverage/Engine/Engine_2_10.coverage", "2ea9984a-3122-4b58-8a71-e9aa5e4f92d6");
            final Scope.Frame frame = script.createFrame(bean, value);
            writeline("/home/ubuntu/results/coverage/Engine/Engine_2_10.coverage", "ad38042a-10e8-491f-a214-35cfab9e6019");
            final Interpreter interpreter = createInterpreter(context, frame);
            writeline("/home/ubuntu/results/coverage/Engine/Engine_2_10.coverage", "7a70b669-397e-4a42-abf9-182bbb6f9c2b");
            node.jjtAccept(interpreter, null);
        } catch (JexlException xjexl) {
            writeline("/home/ubuntu/results/coverage/Engine/Engine_2_10.coverage", "645b3eea-46eb-4500-be56-3091c14613c1");
            if (silent) {
                writeline("/home/ubuntu/results/coverage/Engine/Engine_2_10.coverage", "8243680a-6cfa-4509-bbee-890040e1096d");
                logger.warn(xjexl.getMessage(), xjexl.getCause());
                writeline("/home/ubuntu/results/coverage/Engine/Engine_2_10.coverage", "7baced47-6111-4e1b-af98-e7096fae8d6f");
                return;
            }
            writeline("/home/ubuntu/results/coverage/Engine/Engine_2_10.coverage", "6f672077-e6c0-4e4f-af4a-61764a68a851");
            throw xjexl.clean();
        }
    }

    @Override
    public Object invokeMethod(Object obj, String meth, Object... args) {
        writeline("/home/ubuntu/results/coverage/Engine/Engine_2_10.coverage", "5198902a-5b18-46b6-b844-56bdfd179901");
        JexlException xjexl = null;
        writeline("/home/ubuntu/results/coverage/Engine/Engine_2_10.coverage", "957ee9b9-e8a0-4779-8831-0efcbff3dc34");
        Object result = null;
        writeline("/home/ubuntu/results/coverage/Engine/Engine_2_10.coverage", "dd11a81e-8cc0-4891-915d-a5a3246938e6");
        final JexlInfo info = debug ? createInfo() : null;
        writeline("/home/ubuntu/results/coverage/Engine/Engine_2_10.coverage", "d17c9c17-c87a-438a-b61a-5d410d728f58");
        try {
            writeline("/home/ubuntu/results/coverage/Engine/Engine_2_10.coverage", "920313a4-fc33-4956-9d65-b74f95368992");
            JexlMethod method = uberspect.getMethod(obj, meth, args);
            writeline("/home/ubuntu/results/coverage/Engine/Engine_2_10.coverage", "97a714be-d82b-44d2-8061-010f5ee0b266");
            if (method == null && arithmetic.narrowArguments(args)) {
                writeline("/home/ubuntu/results/coverage/Engine/Engine_2_10.coverage", "7893141f-879c-4648-8792-d94bc9dc4d9e");
                method = uberspect.getMethod(obj, meth, args);
            }
            writeline("/home/ubuntu/results/coverage/Engine/Engine_2_10.coverage", "c581b929-1bd1-4227-acb8-cf8285488bdc");
            if (method != null) {
                writeline("/home/ubuntu/results/coverage/Engine/Engine_2_10.coverage", "03f51ef0-39c4-4902-a24b-436ef350545e");
                result = method.invoke(obj, args);
            } else {
                writeline("/home/ubuntu/results/coverage/Engine/Engine_2_10.coverage", "322b317c-a339-4c18-be25-756853629fa5");
                xjexl = new JexlException.Method(info, meth, null);
            }
        } catch (JexlException xany) {
            writeline("/home/ubuntu/results/coverage/Engine/Engine_2_10.coverage", "2550811f-a855-428e-9d2a-e02bfc64cf95");
            xjexl = xany;
        } catch (Exception xany) {
            writeline("/home/ubuntu/results/coverage/Engine/Engine_2_10.coverage", "44a44520-b8e4-436c-8b5b-52c2751d9776");
            xjexl = new JexlException.Method(info, meth, xany);
        }
        writeline("/home/ubuntu/results/coverage/Engine/Engine_2_10.coverage", "0595f015-7031-48fc-8725-0ea935d31a81");
        if (xjexl != null) {
            writeline("/home/ubuntu/results/coverage/Engine/Engine_2_10.coverage", "423e4a43-3702-4410-862b-311f2eb36970");
            if (silent) {
                writeline("/home/ubuntu/results/coverage/Engine/Engine_2_10.coverage", "aca2e711-5792-4430-83b0-8762e013e97b");
                logger.warn(xjexl.getMessage(), xjexl.getCause());
                writeline("/home/ubuntu/results/coverage/Engine/Engine_2_10.coverage", "827837e1-181c-4919-b64a-abe94052c256");
                result = null;
            } else {
                writeline("/home/ubuntu/results/coverage/Engine/Engine_2_10.coverage", "c1b08471-41c8-4e35-9f49-21cfb9f25e75");
                throw xjexl.clean();
            }
        }
        writeline("/home/ubuntu/results/coverage/Engine/Engine_2_10.coverage", "4de283ea-6c3e-45e8-8681-9fb813950a30");
        return result;
    }

    @Override
    public <T> T newInstance(Class<? extends T> clazz, Object... args) {
        writeline("/home/ubuntu/results/coverage/Engine/Engine_2_10.coverage", "c2ec50db-24ad-4dc5-95f7-5b7623e32880");
        return clazz.cast(doCreateInstance(clazz, args));
    }

    @Override
    public Object newInstance(String clazz, Object... args) {
        writeline("/home/ubuntu/results/coverage/Engine/Engine_2_10.coverage", "881b0ba4-86fd-4b7c-9881-f65df31d7d0a");
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
        writeline("/home/ubuntu/results/coverage/Engine/Engine_2_10.coverage", "1de3ab6e-5e1f-42e9-8876-517c7132f350");
        JexlException xjexl = null;
        writeline("/home/ubuntu/results/coverage/Engine/Engine_2_10.coverage", "b02a59a1-5f60-457d-a327-ab001c95fd82");
        Object result = null;
        writeline("/home/ubuntu/results/coverage/Engine/Engine_2_10.coverage", "8f76b832-757f-4688-8e90-1caa1b8e6afd");
        final JexlInfo info = debug ? createInfo() : null;
        writeline("/home/ubuntu/results/coverage/Engine/Engine_2_10.coverage", "dae782e6-2ac0-439c-8a82-3363a2085695");
        try {
            writeline("/home/ubuntu/results/coverage/Engine/Engine_2_10.coverage", "303538f1-43d2-421e-a8f3-b9dd42c54aca");
            JexlMethod ctor = uberspect.getConstructor(clazz, args);
            writeline("/home/ubuntu/results/coverage/Engine/Engine_2_10.coverage", "d3c14fd4-a905-48c8-93b8-458bab0340a8");
            if (ctor == null && arithmetic.narrowArguments(args)) {
                writeline("/home/ubuntu/results/coverage/Engine/Engine_2_10.coverage", "16885304-c169-4a97-b7c2-17baad266b32");
                ctor = uberspect.getConstructor(clazz, args);
            }
            writeline("/home/ubuntu/results/coverage/Engine/Engine_2_10.coverage", "002f28ad-848f-4eb2-879d-a761362b21d9");
            if (ctor != null) {
                writeline("/home/ubuntu/results/coverage/Engine/Engine_2_10.coverage", "a75092bc-51cd-450c-b20b-57c304f910ac");
                result = ctor.invoke(clazz, args);
            } else {
                writeline("/home/ubuntu/results/coverage/Engine/Engine_2_10.coverage", "6f5c4d94-aaf4-4033-b80b-8f3a3a79eee0");
                xjexl = new JexlException.Method(info, clazz.toString(), null);
            }
        } catch (JexlException xany) {
            writeline("/home/ubuntu/results/coverage/Engine/Engine_2_10.coverage", "14e9474e-585c-41c4-b49d-bce54c5f131d");
            xjexl = xany;
        } catch (Exception xany) {
            writeline("/home/ubuntu/results/coverage/Engine/Engine_2_10.coverage", "30c31ace-bc78-4b7b-8012-ce0106cc8004");
            xjexl = new JexlException.Method(info, clazz.toString(), xany);
        }
        writeline("/home/ubuntu/results/coverage/Engine/Engine_2_10.coverage", "364d076a-8edb-4988-ada0-1b98aa1eb369");
        if (xjexl != null) {
            writeline("/home/ubuntu/results/coverage/Engine/Engine_2_10.coverage", "769569c9-8b6b-41ba-9136-b3f770941870");
            if (silent) {
                writeline("/home/ubuntu/results/coverage/Engine/Engine_2_10.coverage", "21e83924-f521-42f7-9524-40c519af1300");
                logger.warn(xjexl.getMessage(), xjexl.getCause());
                writeline("/home/ubuntu/results/coverage/Engine/Engine_2_10.coverage", "f0e0b123-6110-4686-a855-67f2bfce68f4");
                return null;
            }
            writeline("/home/ubuntu/results/coverage/Engine/Engine_2_10.coverage", "c8f38d0d-2dc1-4bc9-b7b7-55d3545eae01");
            throw xjexl.clean();
        }
        writeline("/home/ubuntu/results/coverage/Engine/Engine_2_10.coverage", "59d8136a-7aa1-4a29-a646-b0b71ecfa4f1");
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
        writeline("/home/ubuntu/results/coverage/Engine/Engine_2_10.coverage", "666edadb-b8a3-4c76-99b7-5f71bea4e3b7");
        VarCollector collector = new VarCollector();
        writeline("/home/ubuntu/results/coverage/Engine/Engine_2_10.coverage", "87ea6057-d4f4-40e0-be54-e0125b8aab2a");
        getVariables(script, script, collector);
        writeline("/home/ubuntu/results/coverage/Engine/Engine_2_10.coverage", "e1598230-7ffb-4dcc-b919-cfe91ee71d35");
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
        writeline("/home/ubuntu/results/coverage/Engine/Engine_2_10.coverage", "0e99a84f-0df8-49a3-8953-e91f10f3b51d");
        if (node instanceof ASTIdentifier) {
            writeline("/home/ubuntu/results/coverage/Engine/Engine_2_10.coverage", "24bae77c-de5a-4c81-a9d9-ed5316b7d6a8");
            JexlNode parent = node.jjtGetParent();
            writeline("/home/ubuntu/results/coverage/Engine/Engine_2_10.coverage", "2f4de7d5-3e4f-4b67-81e4-cbdc91403cdf");
            if (parent instanceof ASTMethodNode || parent instanceof ASTFunctionNode) {
                writeline("/home/ubuntu/results/coverage/Engine/Engine_2_10.coverage", "25b7dbdb-2d19-4930-8071-d0e316fdcb5c");
                // skip identifiers for methods and functions
                collector.collect(null);
                writeline("/home/ubuntu/results/coverage/Engine/Engine_2_10.coverage", "9f92947b-eca8-4085-af5e-2ad2c65cbecf");
                return;
            }
            writeline("/home/ubuntu/results/coverage/Engine/Engine_2_10.coverage", "5aa3c8a3-8178-4eb7-8e64-67b871e8380b");
            ASTIdentifier identifier = (ASTIdentifier) node;
            writeline("/home/ubuntu/results/coverage/Engine/Engine_2_10.coverage", "d08f8b95-af30-408d-ac48-6234aaebd0e9");
            int symbol = identifier.getSymbol();
            writeline("/home/ubuntu/results/coverage/Engine/Engine_2_10.coverage", "b1a414f1-2adc-4649-838b-a8f665354cbb");
            // symbols that are hoisted are considered "global" variables
            if (symbol >= 0 && script != null && !script.isHoistedSymbol(symbol)) {
                writeline("/home/ubuntu/results/coverage/Engine/Engine_2_10.coverage", "d4b8121f-b43d-4ef0-accb-61cefcda27e9");
                collector.collect(null);
            } else {
                writeline("/home/ubuntu/results/coverage/Engine/Engine_2_10.coverage", "dfc0685a-d15c-4857-855f-a648324e8e52");
                // start collecting from identifier
                collector.collect(identifier);
                writeline("/home/ubuntu/results/coverage/Engine/Engine_2_10.coverage", "27c7c2e3-35b2-4f7a-b152-dc8c3597fe61");
                collector.add(identifier.getName());
            }
        } else if (node instanceof ASTIdentifierAccess) {
            writeline("/home/ubuntu/results/coverage/Engine/Engine_2_10.coverage", "8dd18141-427f-4910-98e4-c4e753d0e503");
            JexlNode parent = node.jjtGetParent();
            writeline("/home/ubuntu/results/coverage/Engine/Engine_2_10.coverage", "d3ad239a-5d19-41d2-b1fc-c52b6819b2d6");
            if (parent instanceof ASTMethodNode || parent instanceof ASTFunctionNode) {
                writeline("/home/ubuntu/results/coverage/Engine/Engine_2_10.coverage", "ff6f6995-ba9a-47a6-93df-c8d57fa69857");
                // skip identifiers for methods and functions
                collector.collect(null);
                writeline("/home/ubuntu/results/coverage/Engine/Engine_2_10.coverage", "e759cbc4-9fc6-472c-ae62-fde747d7310b");
                return;
            }
            writeline("/home/ubuntu/results/coverage/Engine/Engine_2_10.coverage", "14c6abae-3903-4b22-91e1-812262f7a159");
            // belt and suspender since an identifier should have been seen first
            if (collector.isCollecting()) {
                writeline("/home/ubuntu/results/coverage/Engine/Engine_2_10.coverage", "c2db1758-1e2a-4916-a13a-db4158a353ff");
                collector.add(((ASTIdentifierAccess) node).getName());
            }
        } else if (node instanceof ASTArrayAccess) {
            writeline("/home/ubuntu/results/coverage/Engine/Engine_2_10.coverage", "80a554a2-3388-4864-9f10-d447712972ed");
            int num = node.jjtGetNumChildren();
            writeline("/home/ubuntu/results/coverage/Engine/Engine_2_10.coverage", "3fe1de48-2949-494d-b5d6-bc86e737da75");
            // collect only if array access is const and follows an identifier
            boolean collecting = collector.isCollecting();
            writeline("/home/ubuntu/results/coverage/Engine/Engine_2_10.coverage", "afc81cae-97ab-464d-a04c-1a5b7b35895f");
            for (int i = 0; i < num; ++i) {
                writeline("/home/ubuntu/results/coverage/Engine/Engine_2_10.coverage", "de0f23a5-6aaf-4de0-962a-14f343f7b626");
                JexlNode child = node.jjtGetChild(i);
                writeline("/home/ubuntu/results/coverage/Engine/Engine_2_10.coverage", "b845974a-9121-4998-aee2-0084d466e799");
                if (collecting && child.isConstant()) {
                    writeline("/home/ubuntu/results/coverage/Engine/Engine_2_10.coverage", "54b8d963-516c-4acd-8d31-1a795885b20b");
                    String image = child.toString();
                    writeline("/home/ubuntu/results/coverage/Engine/Engine_2_10.coverage", "c00ca4c2-53d1-4f8c-b049-f195af5b47d1");
                    if (image == null) {
                        writeline("/home/ubuntu/results/coverage/Engine/Engine_2_10.coverage", "e74d270a-0067-44a7-9562-9653ac22605b");
                        image = new Debugger().data(child);
                    }
                    writeline("/home/ubuntu/results/coverage/Engine/Engine_2_10.coverage", "e5d49103-2fd9-46ea-8572-7345e1714dcd");
                    collector.add(image);
                } else {
                    writeline("/home/ubuntu/results/coverage/Engine/Engine_2_10.coverage", "368da79f-2718-419f-a1b0-36874fb8a331");
                    collecting = false;
                    writeline("/home/ubuntu/results/coverage/Engine/Engine_2_10.coverage", "0394df07-d933-4b31-9f10-99fdcce66039");
                    collector.collect(null);
                    writeline("/home/ubuntu/results/coverage/Engine/Engine_2_10.coverage", "0e05be6f-81ab-4be5-a91d-d916382faa28");
                    getVariables(script, child, collector);
                }
            }
        } else {
            writeline("/home/ubuntu/results/coverage/Engine/Engine_2_10.coverage", "da176806-969a-4249-8465-a0cda1640d98");
            int num = node.jjtGetNumChildren();
            writeline("/home/ubuntu/results/coverage/Engine/Engine_2_10.coverage", "900c323d-56a9-44fd-872e-282ab25521bd");
            for (int i = 0; i < num; ++i) {
                writeline("/home/ubuntu/results/coverage/Engine/Engine_2_10.coverage", "ae6d60ea-403c-41b3-bd78-b61689c4e573");
                getVariables(script, node.jjtGetChild(i), collector);
            }
            writeline("/home/ubuntu/results/coverage/Engine/Engine_2_10.coverage", "ebf70918-8784-405f-bcc0-bd92b2add072");
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
        writeline("/home/ubuntu/results/coverage/Engine/Engine_2_10.coverage", "d388da4f-e197-419c-8baa-def17964502d");
        return script.getParameters();
    }

    /**
     * Gets the array of local variable from a script.
     * @param script the script
     * @return the local variables array which may be empty (but not null) if no local variables were defined
     * @since 3.0
     */
    protected String[] getLocalVariables(JexlScript script) {
        writeline("/home/ubuntu/results/coverage/Engine/Engine_2_10.coverage", "0696ff34-12ea-40a4-b888-142abe644b2f");
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
        writeline("/home/ubuntu/results/coverage/Engine/Engine_2_10.coverage", "d24e3480-30a4-41a0-8843-0edd3dbb8f4b");
        final boolean cached = src.length() < cacheThreshold && cache != null;
        writeline("/home/ubuntu/results/coverage/Engine/Engine_2_10.coverage", "867b3376-7c09-47e9-abad-be4ee3d4e58e");
        ASTJexlScript script;
        writeline("/home/ubuntu/results/coverage/Engine/Engine_2_10.coverage", "898e783d-8d12-4d01-a10e-a89399cfac38");
        synchronized (parser) {
            writeline("/home/ubuntu/results/coverage/Engine/Engine_2_10.coverage", "b893b435-3503-4946-a5e6-1538ebf8c993");
            if (cached) {
                writeline("/home/ubuntu/results/coverage/Engine/Engine_2_10.coverage", "800ffa86-7d5f-4bfc-b351-a3c722af3982");
                script = cache.get(src);
                writeline("/home/ubuntu/results/coverage/Engine/Engine_2_10.coverage", "9221a6f8-fd0c-49f6-9fa1-fa0c0a808fea");
                if (script != null) {
                    writeline("/home/ubuntu/results/coverage/Engine/Engine_2_10.coverage", "b76dd8a3-cdaa-4300-a8c9-e523442e65a8");
                    Scope f = script.getScope();
                    writeline("/home/ubuntu/results/coverage/Engine/Engine_2_10.coverage", "a8aae013-d59b-494e-8924-67918fd16381");
                    if ((f == null && scope == null) || (f != null && f.equals(scope))) {
                        writeline("/home/ubuntu/results/coverage/Engine/Engine_2_10.coverage", "bd072880-651b-471d-9c56-39ce746533a6");
                        return script;
                    }
                }
            }
            writeline("/home/ubuntu/results/coverage/Engine/Engine_2_10.coverage", "6349b129-41be-416f-b146-a55489f0d0d6");
            script = parser.parse(info, src, scope, registers, expression);
            writeline("/home/ubuntu/results/coverage/Engine/Engine_2_10.coverage", "900b8aa4-eabb-4c79-a674-6458ae718c53");
            if (cached) {
                writeline("/home/ubuntu/results/coverage/Engine/Engine_2_10.coverage", "4c3f158a-e7c6-4f9d-81f5-176879fa8c6f");
                cache.put(src, script);
            }
        }
        writeline("/home/ubuntu/results/coverage/Engine/Engine_2_10.coverage", "b5b83e4e-0fd7-4750-b8f4-ef21addeaf11");
        return script;
    }

    /**
     * Trims the source from front and ending spaces.
     * @param str expression to clean
     * @return trimmed expression ending in a semi-colon
     */
    protected String trimSource(CharSequence str) {
        writeline("/home/ubuntu/results/coverage/Engine/Engine_2_10.coverage", "8710cac0-8188-4c0b-b440-5b26e4d956c8");
        if (str != null) {
            writeline("/home/ubuntu/results/coverage/Engine/Engine_2_10.coverage", "8e25d82e-9cda-4fb4-b574-a9bbb4e4231f");
            int start = 0;
            writeline("/home/ubuntu/results/coverage/Engine/Engine_2_10.coverage", "7eb367d7-3b47-482e-a2f1-f02d535fa19a");
            int end = str.length();
            writeline("/home/ubuntu/results/coverage/Engine/Engine_2_10.coverage", "0f1cec57-f6be-403a-8f57-e35e4f8b834a");
            if (end > 0) {
                writeline("/home/ubuntu/results/coverage/Engine/Engine_2_10.coverage", "fde21dff-8642-47c6-a9c4-4fbdf0044a54");
                // trim front spaces
                while (start < end && Character.isSpaceChar(str.charAt(start))) {
                    writeline("/home/ubuntu/results/coverage/Engine/Engine_2_10.coverage", "a382265e-8286-4242-bdec-aa31eb11846d");
                    ++start;
                }
                writeline("/home/ubuntu/results/coverage/Engine/Engine_2_10.coverage", "65497fc1-0b70-4d8b-9c98-8978d6c265c0");
                // trim ending spaces
                while (end > 0 && Character.isSpaceChar(str.charAt(end - 1))) {
                    writeline("/home/ubuntu/results/coverage/Engine/Engine_2_10.coverage", "d65eac32-cf22-4394-9a8e-99a06f877c16");
                    --end;
                }
                writeline("/home/ubuntu/results/coverage/Engine/Engine_2_10.coverage", "555d9547-76fe-4dd5-8af5-6f2b60c7c9e3");
                return str.subSequence(start, end).toString();
            }
            writeline("/home/ubuntu/results/coverage/Engine/Engine_2_10.coverage", "d646711e-c52f-46d7-a9f9-3d4219fba7bc");
            return "";
        }
        writeline("/home/ubuntu/results/coverage/Engine/Engine_2_10.coverage", "2fa554fd-f825-418a-b925-564c5ecc4768");
        return null;
    }

    /**
     * Gets and/or creates a default template engine.
     * @return a template engine
     */
    protected TemplateEngine jxlt() {
        writeline("/home/ubuntu/results/coverage/Engine/Engine_2_10.coverage", "3208dfec-99e0-4ccd-a863-4bc49cf37897");
        TemplateEngine e = jxlt;
        writeline("/home/ubuntu/results/coverage/Engine/Engine_2_10.coverage", "0f6e259b-4170-47ff-add3-b70b7e941c32");
        if (e == null) {
            writeline("/home/ubuntu/results/coverage/Engine/Engine_2_10.coverage", "2c8dc93a-4c18-4afc-b539-59c24b9243fc");
            synchronized (this) {
                writeline("/home/ubuntu/results/coverage/Engine/Engine_2_10.coverage", "4a510e67-e2e0-41e8-ae7d-5ac56443b392");
                if (jxlt == null) {
                    writeline("/home/ubuntu/results/coverage/Engine/Engine_2_10.coverage", "3fb753e8-470a-4aed-a0b6-e2840b24103a");
                    e = new TemplateEngine(this, true, 0, '$', '#');
                    writeline("/home/ubuntu/results/coverage/Engine/Engine_2_10.coverage", "0eb65994-ec52-4f1d-b964-d1220c02435e");
                    jxlt = e;
                }
            }
        }
        writeline("/home/ubuntu/results/coverage/Engine/Engine_2_10.coverage", "d046f27a-0203-4da5-a37d-72f49e0444ff");
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
