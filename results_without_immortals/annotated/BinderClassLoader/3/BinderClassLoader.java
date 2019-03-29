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
        writeline("/home/ubuntu/results/coverage/BinderClassLoader/BinderClassLoader_3_10.coverage", "aed5e797-0a07-43c6-bc98-66d70ae119aa");
        PrivilegedAction<BinderClassLoader> action = new PrivilegedAction<BinderClassLoader>() {

            public BinderClassLoader run() {
                return new BinderClassLoader(adaptedClassLoader);
            }
        };
        writeline("/home/ubuntu/results/coverage/BinderClassLoader/BinderClassLoader_3_10.coverage", "ebacc638-3e95-45f7-9d02-2eb114bac1c1");
        if (getSecurityManager() != null) {
            writeline("/home/ubuntu/results/coverage/BinderClassLoader/BinderClassLoader_3_10.coverage", "abaddd28-6bdc-4b5f-85f4-e3ad019a2b98");
            return doPrivileged(action);
        }
        writeline("/home/ubuntu/results/coverage/BinderClassLoader/BinderClassLoader_3_10.coverage", "c4de8fd2-e890-4bca-9a30-4ad8e79606dc");
        return action.run();
    }

    private BinderClassLoader(ClassLoader adaptedClassLoader) {
        super(adaptedClassLoader);
    }

    public ClassLoader getAdaptedClassLoader() {
        writeline("/home/ubuntu/results/coverage/BinderClassLoader/BinderClassLoader_3_10.coverage", "66666e19-868d-4e37-b02a-8acf739e03af");
        return getParent();
    }

    /**
     * {@inheritDoc}
     */
    @Override
    protected synchronized Class<?> loadClass(String name, boolean resolve) throws ClassNotFoundException {
        writeline("/home/ubuntu/results/coverage/BinderClassLoader/BinderClassLoader_3_10.coverage", "458c3751-bf99-433c-b7f4-2776a04aa5fc");
        if (PRIMITIVE_TYPES.containsKey(name)) {
            writeline("/home/ubuntu/results/coverage/BinderClassLoader/BinderClassLoader_3_10.coverage", "88064b62-e9fe-4ca5-a596-859c1f30ef78");
            return PRIMITIVE_TYPES.get(name);
        }
        writeline("/home/ubuntu/results/coverage/BinderClassLoader/BinderClassLoader_3_10.coverage", "414c3036-198e-4fd3-b803-7c2b90d7328a");
        return getParent().loadClass(name);
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public URL getResource(String name) {
        writeline("/home/ubuntu/results/coverage/BinderClassLoader/BinderClassLoader_3_10.coverage", "84a53ab6-83c2-4a95-9c3a-bf623f94560f");
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
