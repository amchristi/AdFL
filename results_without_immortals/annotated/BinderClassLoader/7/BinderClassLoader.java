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
        writeline("/home/ubuntu/results/coverage/BinderClassLoader/BinderClassLoader_7_10.coverage", "0231cdca-bbf3-4cd8-9b0d-a8e62ca2ff72");
        PrivilegedAction<BinderClassLoader> action = new PrivilegedAction<BinderClassLoader>() {

            public BinderClassLoader run() {
                return new BinderClassLoader(adaptedClassLoader);
            }
        };
        writeline("/home/ubuntu/results/coverage/BinderClassLoader/BinderClassLoader_7_10.coverage", "8e02d636-43e7-4bd0-9777-bcded1e85810");
        if (getSecurityManager() != null) {
            writeline("/home/ubuntu/results/coverage/BinderClassLoader/BinderClassLoader_7_10.coverage", "2432352d-33d6-45ff-9b77-5e2c3520f1e1");
            return doPrivileged(action);
        }
        writeline("/home/ubuntu/results/coverage/BinderClassLoader/BinderClassLoader_7_10.coverage", "bddc80cf-2f80-429e-90e3-af4af4a16510");
        return action.run();
    }

    private BinderClassLoader(ClassLoader adaptedClassLoader) {
        super(adaptedClassLoader);
    }

    public ClassLoader getAdaptedClassLoader() {
        writeline("/home/ubuntu/results/coverage/BinderClassLoader/BinderClassLoader_7_10.coverage", "d7958186-6a62-4ad2-a598-a8148bae30b6");
        return getParent();
    }

    /**
     * {@inheritDoc}
     */
    @Override
    protected synchronized Class<?> loadClass(String name, boolean resolve) throws ClassNotFoundException {
        writeline("/home/ubuntu/results/coverage/BinderClassLoader/BinderClassLoader_7_10.coverage", "95e0be5f-a238-46a7-af7c-5c0d232b1c4d");
        if (PRIMITIVE_TYPES.containsKey(name)) {
            writeline("/home/ubuntu/results/coverage/BinderClassLoader/BinderClassLoader_7_10.coverage", "91bd5187-e10a-4440-9729-9be9de5a7350");
            return PRIMITIVE_TYPES.get(name);
        }
        writeline("/home/ubuntu/results/coverage/BinderClassLoader/BinderClassLoader_7_10.coverage", "07b8de98-bb82-4099-88c3-24ee93ef3058");
        return getParent().loadClass(name);
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public URL getResource(String name) {
        writeline("/home/ubuntu/results/coverage/BinderClassLoader/BinderClassLoader_7_10.coverage", "03468548-248b-45c1-b4e6-e690661069a6");
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
