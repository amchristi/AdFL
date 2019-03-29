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
        writeline("/home/ubuntu/results/coverage/BinderClassLoader/BinderClassLoader_1_10.coverage", "9c193c35-354e-4ced-bf50-0c222366b44b");
        PrivilegedAction<BinderClassLoader> action = new PrivilegedAction<BinderClassLoader>() {

            public BinderClassLoader run() {
                return new BinderClassLoader(adaptedClassLoader);
            }
        };
        writeline("/home/ubuntu/results/coverage/BinderClassLoader/BinderClassLoader_1_10.coverage", "b3ba5f18-df13-4a82-9dca-98a75646016d");
        if (getSecurityManager() != null) {
            writeline("/home/ubuntu/results/coverage/BinderClassLoader/BinderClassLoader_1_10.coverage", "9ad62287-94af-4bc7-9f3f-493d4ee58e62");
            return doPrivileged(action);
        }
        writeline("/home/ubuntu/results/coverage/BinderClassLoader/BinderClassLoader_1_10.coverage", "69f400bd-63dd-4068-a425-796c12dc81c8");
        return action.run();
    }

    private BinderClassLoader(ClassLoader adaptedClassLoader) {
        super(adaptedClassLoader);
    }

    public ClassLoader getAdaptedClassLoader() {
        writeline("/home/ubuntu/results/coverage/BinderClassLoader/BinderClassLoader_1_10.coverage", "7068b73a-af87-43ee-84ff-b44b8f713731");
        return getParent();
    }

    /**
     * {@inheritDoc}
     */
    @Override
    protected synchronized Class<?> loadClass(String name, boolean resolve) throws ClassNotFoundException {
        writeline("/home/ubuntu/results/coverage/BinderClassLoader/BinderClassLoader_1_10.coverage", "1fbc7849-027a-4bd0-9801-1a772548e20f");
        if (PRIMITIVE_TYPES.containsKey(name)) {
            writeline("/home/ubuntu/results/coverage/BinderClassLoader/BinderClassLoader_1_10.coverage", "b44632dd-dbc8-4b19-8946-c794de222caf");
            return PRIMITIVE_TYPES.get(name);
        }
        writeline("/home/ubuntu/results/coverage/BinderClassLoader/BinderClassLoader_1_10.coverage", "1eaf63d0-547b-4166-98c1-7aee872658f1");
        return getParent().loadClass(name);
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public URL getResource(String name) {
        writeline("/home/ubuntu/results/coverage/BinderClassLoader/BinderClassLoader_1_10.coverage", "afb3e2b3-31b0-4b72-a4ed-bc7f6bc2cd22");
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
