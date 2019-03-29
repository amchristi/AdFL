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
        writeline("/home/ubuntu/results/coverage/BinderClassLoader/BinderClassLoader_4_10.coverage", "b51aef27-da26-41f1-847b-8454b833f83a");
        PrivilegedAction<BinderClassLoader> action = new PrivilegedAction<BinderClassLoader>() {

            public BinderClassLoader run() {
                return new BinderClassLoader(adaptedClassLoader);
            }
        };
        writeline("/home/ubuntu/results/coverage/BinderClassLoader/BinderClassLoader_4_10.coverage", "8252f466-4eae-4df6-9786-3e2a5307b551");
        if (getSecurityManager() != null) {
            writeline("/home/ubuntu/results/coverage/BinderClassLoader/BinderClassLoader_4_10.coverage", "3fb69859-ba03-4aea-9a9f-800cd20727dc");
            return doPrivileged(action);
        }
        writeline("/home/ubuntu/results/coverage/BinderClassLoader/BinderClassLoader_4_10.coverage", "756e6095-1eca-42eb-ba3e-9faf84b99448");
        return action.run();
    }

    private BinderClassLoader(ClassLoader adaptedClassLoader) {
        super(adaptedClassLoader);
    }

    public ClassLoader getAdaptedClassLoader() {
        writeline("/home/ubuntu/results/coverage/BinderClassLoader/BinderClassLoader_4_10.coverage", "ff9b13cc-b8a7-4d68-9707-e6789655254a");
        return getParent();
    }

    /**
     * {@inheritDoc}
     */
    @Override
    protected synchronized Class<?> loadClass(String name, boolean resolve) throws ClassNotFoundException {
        writeline("/home/ubuntu/results/coverage/BinderClassLoader/BinderClassLoader_4_10.coverage", "bef36162-bd42-43de-8d21-ae7c316b0625");
        if (PRIMITIVE_TYPES.containsKey(name)) {
            writeline("/home/ubuntu/results/coverage/BinderClassLoader/BinderClassLoader_4_10.coverage", "63a257fb-c8ac-48c8-9746-990468255b62");
            return PRIMITIVE_TYPES.get(name);
        }
        writeline("/home/ubuntu/results/coverage/BinderClassLoader/BinderClassLoader_4_10.coverage", "c127a8ab-ce1a-4164-b8eb-e71a72402a19");
        return getParent().loadClass(name);
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public URL getResource(String name) {
        writeline("/home/ubuntu/results/coverage/BinderClassLoader/BinderClassLoader_4_10.coverage", "0a6ecf11-08ed-4532-9abf-2fad9ad04f2b");
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
