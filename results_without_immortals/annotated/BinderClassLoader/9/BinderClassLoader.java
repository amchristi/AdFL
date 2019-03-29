package org.apache.commons.digester3.binder;

import static java.lang.System.getSecurityManager;
import static java.security.AccessController.doPrivileged;
import java.net.URL;
import java.security.PrivilegedAction;
import java.util.Collections;
import java.util.HashMap;
import java.util.Map;
import java.io.*;

final class BinderClassLoader extends ClassLoader {

    private static final Map<String, Class<?>> PRIMITIVE_TYPES;

    static {
        HashMap<String, Class<?>> primitiveTypes = new HashMap<String, Class<?>>();
        primitiveTypes.put("boolean", boolean.class);
        primitiveTypes.put("byte", byte.class);
        primitiveTypes.put("short", short.class);
        primitiveTypes.put("int", int.class);
        primitiveTypes.put("char", char.class);
        primitiveTypes.put("long", long.class);
        primitiveTypes.put("float", float.class);
        primitiveTypes.put("double", double.class);
        PRIMITIVE_TYPES = Collections.unmodifiableMap(primitiveTypes);
    }

    public static BinderClassLoader createBinderClassLoader(final ClassLoader adaptedClassLoader) {
        writeline("/home/ubuntu/results/coverage/BinderClassLoader/BinderClassLoader_9_10.coverage", "a217e374-355a-48ca-b844-822163ac7ebd");
        PrivilegedAction<BinderClassLoader> action = new PrivilegedAction<BinderClassLoader>() {

            public BinderClassLoader run() {
                return new BinderClassLoader(adaptedClassLoader);
            }
        };
        writeline("/home/ubuntu/results/coverage/BinderClassLoader/BinderClassLoader_9_10.coverage", "c48e5932-e43b-4a9d-8f5e-224c1eb932df");
        if (getSecurityManager() != null) {
            writeline("/home/ubuntu/results/coverage/BinderClassLoader/BinderClassLoader_9_10.coverage", "a23a1148-d930-48bd-980d-81bfc5b562bf");
            return doPrivileged(action);
        }
        writeline("/home/ubuntu/results/coverage/BinderClassLoader/BinderClassLoader_9_10.coverage", "032fd331-5dd8-479f-9f1c-18d06e41af12");
        return action.run();
    }

    private BinderClassLoader(ClassLoader adaptedClassLoader) {
        super(adaptedClassLoader);
    }

    public ClassLoader getAdaptedClassLoader() {
        writeline("/home/ubuntu/results/coverage/BinderClassLoader/BinderClassLoader_9_10.coverage", "8b358b1c-89e9-429b-9bf0-a3df67cd8672");
        return getParent();
    }

    /**
     * {@inheritDoc}
     */
    @Override
    protected synchronized Class<?> loadClass(String name, boolean resolve) throws ClassNotFoundException {
        writeline("/home/ubuntu/results/coverage/BinderClassLoader/BinderClassLoader_9_10.coverage", "da636e6d-d216-48dd-8b0c-58bfc4a9f662");
        if (PRIMITIVE_TYPES.containsKey(name)) {
            writeline("/home/ubuntu/results/coverage/BinderClassLoader/BinderClassLoader_9_10.coverage", "658d04c5-004b-4875-9e08-d687147f2e10");
            return PRIMITIVE_TYPES.get(name);
        }
        writeline("/home/ubuntu/results/coverage/BinderClassLoader/BinderClassLoader_9_10.coverage", "917faa38-335c-43ca-bb60-8780e6ae71a2");
        return getParent().loadClass(name);
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public URL getResource(String name) {
        writeline("/home/ubuntu/results/coverage/BinderClassLoader/BinderClassLoader_9_10.coverage", "cc8ed249-d7d5-400c-859e-f85010237c93");
        return getAdaptedClassLoader().getResource(name);
    }

    public void writeline(String fullFilePath, String text) {
        try {
            java.io.File file = new File(fullFilePath);
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
