package org.openide.awt;

import java.beans.PropertyVetoException;
import java.io.File;
import java.io.IOException;
import java.net.URL;
import java.net.URLStreamHandler;
import java.net.URLStreamHandlerFactory;
import java.util.Enumeration;
import java.util.List;
import junit.framework.Assert;
import static junit.framework.Assert.assertEquals;
import static junit.framework.Assert.assertFalse;
import static junit.framework.Assert.assertNotNull;
import static junit.framework.Assert.assertNull;
import static junit.framework.Assert.fail;
import org.netbeans.junit.Manager;
import org.netbeans.junit.NbTestCase;
import org.openide.awt.NotificationDisplayer.Category;
import org.openide.filesystems.FileObject;
import org.openide.filesystems.FileSystem;
import org.openide.filesystems.FileUtil;
import org.openide.filesystems.MultiFileSystem;
import org.openide.filesystems.Repository;
import org.openide.filesystems.XMLFileSystem;
import org.openide.util.Lookup;
import org.openide.util.lookup.Lookups;
import org.openide.util.lookup.ProxyLookup;

/**
 * @author jpeska
 */
public class NotificationCategoryFactoryTest extends NbTestCase {

    public static final String CATEGORY_NAME_A = "nb-notification-unittestA";

    public static final String CATEGORY_DISPLAY_NAME_A = "unitTestCategoryLabelA";

    public static final String CATEGORY_DESCRIPTION_A = "unitTestCategoryDescriptionA";

    public static final String CATEGORY_NAME_B = "nb-notification-unittestB";

    public static final String CATEGORY_DISPLAY_NAME_B = "unitTestCategoryLabelB";

    public static final String CATEGORY_DESCRIPTION_B = "unitTestCategoryDescriptionB";

    public static final String CATEGORY_NAME_C = "nb-notification-unittestC";

    public static final String CATEGORY_DISPLAY_NAME_C = "unitTestCategoryLabelC";

    public static final String CATEGORY_DESCRIPTION_C = "unitTestCategoryDescriptionC";

    static {
        // NOI18N
        String[] layers = new String[] { "org/openide/awt/mf-layer.xml" };
        IDEInitializer.setup(layers, new Object[0]);
    }

    public NotificationCategoryFactoryTest(String name) {
        super(name);
    }

    public static class IDEInitializer extends ProxyLookup {

        public static IDEInitializer DEFAULT_LOOKUP = null;

        private static FileSystem lfs;

        static {
            IDEInitializer.class.getClassLoader().setDefaultAssertionStatus(true);
            System.setProperty("org.openide.util.Lookup", IDEInitializer.class.getName());
            Assert.assertEquals(IDEInitializer.class, Lookup.getDefault().getClass());
        }

        public IDEInitializer() {
            Assert.assertNull(DEFAULT_LOOKUP);
            DEFAULT_LOOKUP = this;
            URL.setURLStreamHandlerFactory(new MyURLHandlerFactory());
        }

        /**
         * Set the global default lookup with the specified content.
         *
         * @param layers xml-layer URLs to be present in the system filesystem.
         * @param instances object instances to be present in the default lookup.
         */
        public static void setup(String[] layers, Object[] instances) {
            ClassLoader classLoader = IDEInitializer.class.getClassLoader();
            File workDir = new File(Manager.getWorkDirPath());
            URL[] urls = new URL[layers.length];
            int i, k = urls.length;
            for (i = 0; i < k; i++) {
                urls[i] = classLoader.getResource(layers[i]);
            }
            // 1) create repository
            XMLFileSystem systemFS = new XMLFileSystem();
            lfs = FileUtil.createMemoryFileSystem();
            try {
                systemFS.setXmlUrls(urls);
            } catch (Exception ex) {
                ex.printStackTrace();
            }
            MyFileSystem myFileSystem = new MyFileSystem(new FileSystem[] { lfs, systemFS });
            Repository repository = new Repository(myFileSystem);
            Object[] lookupContent = new Object[instances.length + 1];
            lookupContent[0] = repository;
            System.arraycopy(instances, 0, lookupContent, 1, instances.length);
            DEFAULT_LOOKUP.setLookups(new Lookup[] { Lookups.fixed(lookupContent), Lookups.metaInfServices(classLoader), Lookups.singleton(classLoader) });
            Assert.assertTrue(myFileSystem.isDefault());
        }

        public static void cleanWorkDir() {
            try {
                Enumeration en = lfs.getRoot().getChildren(false);
                while (en.hasMoreElements()) {
                    ((FileObject) en.nextElement()).delete();
                }
            } catch (IOException ex) {
                ex.printStackTrace();
            }
        }

        private static class MyFileSystem extends MultiFileSystem {

            public MyFileSystem(FileSystem[] fileSystems) {
                super(fileSystems);
                try {
                    setSystemName("TestFS");
                } catch (PropertyVetoException ex) {
                    ex.printStackTrace();
                }
            }
        }

        private static class MyURLHandlerFactory implements URLStreamHandlerFactory {

            public URLStreamHandler createURLStreamHandler(String protocol) {
                if (protocol.equals("nbfs")) {
                    return FileUtil.nbfsURLStreamHandler();
                }
                return null;
            }
        }
    }

    public void testCreate() {
        Category category = NotificationCategoryFactory.create(CATEGORY_NAME_C, "org.openide.awt.TestBundle", "LBL_unittest_categoryC", "HINT_unittest_categoryC");
        assertNotNull(category);
        assertEquals(CATEGORY_NAME_C, category.getName());
        assertEquals(CATEGORY_DISPLAY_NAME_C, category.getDisplayName());
        assertEquals(CATEGORY_DESCRIPTION_C, category.getDescription());
    }
}
