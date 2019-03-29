package org.openide.awt;

import java.awt.Component;
import java.awt.GraphicsEnvironment;
import java.io.ByteArrayInputStream;
import java.io.IOException;
import java.io.InputStream;
import java.lang.reflect.InvocationTargetException;
import java.net.HttpURLConnection;
import java.net.URL;
import java.net.URLConnection;
import java.net.URLStreamHandler;
import java.util.concurrent.Semaphore;
import javax.swing.JFrame;
import javax.swing.SwingUtilities;
import junit.framework.Test;
import junit.framework.TestCase;
import junit.framework.TestSuite;

/**
 * @author Radim
 */
public class SwingBrowserTest extends TestCase {

    public SwingBrowserTest(String testName) {
        super(testName);
    }

    private static class MyStreamHandler extends URLStreamHandler {

        private Semaphore sIn;

        private Semaphore sOut;

        MyStreamHandler() {
        }

        MyStreamHandler(Semaphore s, Semaphore s2) {
            sIn = s;
            sOut = s2;
            try {
                if (sOut != null) {
                    sOut.acquire();
                }
            } catch (InterruptedException ex) {
                ex.printStackTrace();
                fail(ex.getMessage());
            }
        }

        protected URLConnection openConnection(URL u) throws IOException {
            return new MyConnection(sIn, sOut, u);
        }
    }

    private static class MyConnection extends HttpURLConnection {

        private Semaphore sIn;

        private Semaphore sOut;

        protected MyConnection(Semaphore s, Semaphore s2, URL u) {
            super(u);
            sIn = s;
            sOut = s2;
        }

        public void connect() throws IOException {
        }

        @Override
        public InputStream getInputStream() throws IOException {
            return new ByteArrayInputStream("blabla".getBytes());
        }

        public void disconnect() {
        }

        public boolean usingProxy() {
            return false;
        }

        @Override
        public int getResponseCode() throws IOException {
            System.out.println("connecting " + toString() + " ... isEDT = " + SwingUtilities.isEventDispatchThread());
            // Thread.dumpStack();
            if (sIn != null) {
                try {
                    sIn.acquire();
                    if (sOut != null) {
                        sOut.release();
                    }
                    System.out.println("aquired in lock, released out " + toString());
                } catch (InterruptedException ex) {
                    ex.printStackTrace();
                    fail(ex.getMessage());
                }
                sIn.release();
            }
            System.out.println("... connected " + toString());
            return super.getResponseCode();
        }

        @Override
        public String toString() {
            return "MyConnection [" + url.toExternalForm() + " sIn " + sIn + " sOut " + sOut + "]";
        }
    }

    public static Test suite() {
        return GraphicsEnvironment.isHeadless() ? new TestSuite() : new TestSuite(SwingBrowserTest.class);
    }

    public void testDeadlockWithJdk6() throws Exception {
        System.out.println("testDeadlockWithJdk6");
        // fails on JDK6.0 b99 (passes on JDK6b92 or JDK5u9)
        final HtmlBrowser.Impl impl = new SwingBrowserImpl();
        final JFrame f = new JFrame();
        final Semaphore s = new Semaphore(1);
        final Semaphore s2 = new Semaphore(1);
        final URL url = new URL("test", "localhost", -1, "simple", new MyStreamHandler(s, s2));
        final URL url2 = new URL("test", "localhost", -1, "simple2", new MyStreamHandler(null, null));
        s.acquire();
        SwingUtilities.invokeAndWait(new Runnable() {

            public void run() {
                Component comp = impl.getComponent();
                f.add(comp);
                f.setVisible(true);
                impl.setURL(url);
            }
        });
        System.out.println("browser visible, URL set");
        SwingUtilities.invokeAndWait(new Runnable() {

            public void run() {
                System.out.println("before 2nd setURL");
                // allow to read the stream
                s.release();
                try {
                    s2.acquire();
                } catch (InterruptedException ex) {
                    ex.printStackTrace();
                    fail(ex.getMessage());
                }
                impl.setURL(url2);
                s2.release();
                System.out.println("after 2nd setURL");
            }
        });
        System.out.println("new URL requested");
        SwingUtilities.invokeAndWait(new Runnable() {

            public void run() {
                impl.getURL();
            }
        });
        System.out.println("getURL called");
        waitForLoading(url2, f, impl);
    }

    private void waitForLoading(final URL url, final JFrame f, final HtmlBrowser.Impl impl) throws InvocationTargetException, InterruptedException {
        for (int i = 0; i < 10 && f.isVisible(); i++) {
            SwingUtilities.invokeAndWait(new Runnable() {

                public void run() {
                    URL current = impl.getURL();
                    if (url.equals(current)) {
                        f.setVisible(false);
                        f.dispose();
                    }
                }
            });
            Thread.sleep(i * 100);
        }
    }
}
