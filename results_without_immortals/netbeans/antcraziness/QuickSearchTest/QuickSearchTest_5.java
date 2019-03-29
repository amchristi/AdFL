package org.openide.awt;

import java.awt.Component;
import java.awt.GraphicsEnvironment;
import java.awt.KeyboardFocusManager;
import java.awt.event.KeyEvent;
import java.awt.event.KeyListener;
import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;
import java.util.ArrayList;
import java.util.List;
import javax.swing.JComponent;
import javax.swing.JFrame;
import javax.swing.JTextField;
import javax.swing.SwingUtilities;
import junit.framework.Test;
import junit.framework.TestSuite;
import static org.junit.Assert.*;
import org.netbeans.junit.NbTestCase;

/**
 * Test of QuickSearch.
 *
 * @author Martin Entlicher
 */
public class QuickSearchTest extends NbTestCase {

    public QuickSearchTest(String name) {
        super(name);
    }

    private static final class TestComponent extends JComponent {

        List<KeyListener> addedKeyListeners = new ArrayList<KeyListener>();

        Component added;

        Object constraints;

        public TestComponent() {
            // To have a parent
            new JFrame().add(this);
        }

        @Override
        public boolean isShowing() {
            return true;
        }

        @Override
        public Component add(Component comp) {
            this.added = comp;
            return super.add(comp);
        }

        @Override
        public void add(Component comp, Object constraints) {
            this.added = comp;
            this.constraints = constraints;
            super.add(comp, constraints);
        }

        @Override
        public void remove(Component comp) {
            if (comp == this.added) {
                this.added = null;
            }
            super.remove(comp);
        }

        @Override
        public synchronized void addKeyListener(KeyListener l) {
            addedKeyListeners.add(l);
            super.addKeyListener(l);
        }

        @Override
        public synchronized void removeKeyListener(KeyListener l) {
            addedKeyListeners.remove(l);
            super.removeKeyListener(l);
        }
    }

    private static final class DummyCallback implements QuickSearch.Callback {

        @Override
        public void quickSearchUpdate(String searchText) {
        }

        @Override
        public void showNextSelection(boolean forward) {
        }

        @Override
        public String findMaxPrefix(String prefix) {
            return prefix;
        }

        @Override
        public void quickSearchConfirmed() {
        }

        @Override
        public void quickSearchCanceled() {
        }
    }

    public static Test suite() {
        return GraphicsEnvironment.isHeadless() ? new TestSuite() : new TestSuite(QuickSearchTest.class);
    }

    /**
     * Test of asynchronous calls, of class QuickSearch.
     */
    public void testAsynchronous() {
        final TestComponent[] componentPtr = new TestComponent[] { null };
        final String[] searchTextPtr = new String[] { null };
        final Boolean[] biasPtr = new Boolean[] { null };
        final Object findMaxPrefixLock = new Object();
        final boolean[] confirmedPtr = new boolean[] { false };
        final boolean[] canceledPtr = new boolean[] { false };
        final sync[] syncPtr = new sync[] { null };
        final QuickSearch.Callback qsc = new QuickSearch.Callback() {

            @Override
            public void quickSearchUpdate(String searchText) {
                assertFalse(SwingUtilities.isEventDispatchThread());
                synchronized (searchTextPtr) {
                    if (syncPtr[0] == null) {
                        syncPtr[0] = sync.W;
                        // Wait for the notification first
                        try {
                            searchTextPtr.wait();
                        } catch (InterruptedException iex) {
                        }
                    }
                    searchTextPtr[0] = searchText;
                    searchTextPtr.notifyAll();
                    syncPtr[0] = null;
                }
            }

            @Override
            public void showNextSelection(boolean forward) {
                assertFalse(SwingUtilities.isEventDispatchThread());
                synchronized (biasPtr) {
                    if (syncPtr[0] == null) {
                        syncPtr[0] = sync.W;
                        // Wait for the notification first
                        try {
                            biasPtr.wait();
                        } catch (InterruptedException iex) {
                        }
                    }
                    biasPtr[0] = forward;
                    biasPtr.notifyAll();
                    syncPtr[0] = null;
                }
            }

            @Override
            public String findMaxPrefix(String prefix) {
                assertFalse(SwingUtilities.isEventDispatchThread());
                synchronized (findMaxPrefixLock) {
                    if (syncPtr[0] == null) {
                        syncPtr[0] = sync.W;
                        // Wait for the notification first
                        try {
                            findMaxPrefixLock.wait();
                        } catch (InterruptedException iex) {
                        }
                    }
                    prefix = prefix + "endPrefix";
                    findMaxPrefixLock.notifyAll();
                    syncPtr[0] = null;
                }
                return prefix;
            }

            @Override
            public void quickSearchConfirmed() {
                assertTrue(SwingUtilities.isEventDispatchThread());
                confirmedPtr[0] = true;
            }

            @Override
            public void quickSearchCanceled() {
                assertTrue(SwingUtilities.isEventDispatchThread());
                canceledPtr[0] = true;
            }
        };
        final QuickSearch[] qsPtr = new QuickSearch[] { null };
        try {
            SwingUtilities.invokeAndWait(new Runnable() {

                @Override
                public void run() {
                    componentPtr[0] = new TestComponent();
                    qsPtr[0] = QuickSearch.attach(componentPtr[0], null, qsc, true);
                    componentPtr[0].addNotify();
                }
            });
        } catch (InterruptedException iex) {
            fail("interrupted.");
        } catch (InvocationTargetException itex) {
            Throwable cause = itex.getCause();
            if (cause instanceof AssertionError) {
                throw (AssertionError) cause;
            }
            itex.getCause().printStackTrace();
            throw new AssertionError(cause);
        }
        // Test that a key event passed to the component triggers the asynchronous quick search:
        try {
            SwingUtilities.invokeAndWait(new Runnable() {

                @Override
                public void run() {
                    try {
                        Method setGlobalFocusOwner = KeyboardFocusManager.class.getDeclaredMethod("setGlobalFocusOwner", Component.class);
                        setGlobalFocusOwner.setAccessible(true);
                        setGlobalFocusOwner.invoke(KeyboardFocusManager.getCurrentKeyboardFocusManager(), componentPtr[0]);
                    } catch (Exception ex) {
                        ex.printStackTrace();
                        throw new AssertionError(ex);
                    }
                    KeyEvent ke = new KeyEvent(componentPtr[0], KeyEvent.KEY_TYPED, System.currentTimeMillis(), 0, KeyEvent.VK_UNDEFINED, 'A');
                    componentPtr[0].dispatchEvent(ke);
                }
            });
        } catch (InterruptedException iex) {
            fail("interrupted.");
        } catch (InvocationTargetException itex) {
            Throwable cause = itex.getCause();
            if (cause instanceof AssertionError) {
                throw (AssertionError) cause;
            }
            itex.getCause().printStackTrace();
            throw new AssertionError(cause);
        }
        synchronized (searchTextPtr) {
            assertNull(searchTextPtr[0]);
            syncPtr[0] = sync.N;
            searchTextPtr.notifyAll();
            // Wait to set the value
            try {
                searchTextPtr.wait();
            } catch (InterruptedException iex) {
            }
            assertEquals("A", searchTextPtr[0]);
        }
        // Test the up/down keys resulting to asynchronous selection navigation:
        try {
            SwingUtilities.invokeAndWait(new Runnable() {

                @Override
                public void run() {
                    KeyEvent ke = new KeyEvent(qsPtr[0].getSearchField(), KeyEvent.KEY_PRESSED, System.currentTimeMillis(), 0, KeyEvent.VK_UP, (char) KeyEvent.VK_UP);
                    qsPtr[0].getSearchField().dispatchEvent(ke);
                    ke = new KeyEvent(qsPtr[0].getSearchField(), KeyEvent.KEY_PRESSED, System.currentTimeMillis(), 0, KeyEvent.VK_DOWN, (char) KeyEvent.VK_DOWN);
                    qsPtr[0].getSearchField().dispatchEvent(ke);
                }
            });
        } catch (InterruptedException iex) {
            fail("interrupted.");
        } catch (InvocationTargetException itex) {
            Throwable cause = itex.getCause();
            if (cause instanceof AssertionError) {
                throw (AssertionError) cause;
            }
            itex.getCause().printStackTrace();
            throw new AssertionError(cause);
        }
        synchronized (biasPtr) {
            assertNull(biasPtr[0]);
            syncPtr[0] = sync.N;
            biasPtr.notifyAll();
            // Wait to set the value
            try {
                biasPtr.wait();
            } catch (InterruptedException iex) {
            }
            assertEquals(Boolean.FALSE, biasPtr[0]);
        }
        synchronized (biasPtr) {
            assertEquals(Boolean.FALSE, biasPtr[0]);
            syncPtr[0] = sync.N;
            biasPtr.notifyAll();
            // Wait to set the value
            try {
                biasPtr.wait();
            } catch (InterruptedException iex) {
            }
            assertEquals(Boolean.TRUE, biasPtr[0]);
        }
        // Test that tab adds max prefix asynchronously:
        try {
            SwingUtilities.invokeAndWait(new Runnable() {

                @Override
                public void run() {
                    KeyEvent ke = new KeyEvent(qsPtr[0].getSearchField(), KeyEvent.KEY_PRESSED, System.currentTimeMillis(), 0, KeyEvent.VK_TAB, '\t');
                    qsPtr[0].getSearchField().dispatchEvent(ke);
                }
            });
        } catch (InterruptedException iex) {
            fail("interrupted.");
        } catch (InvocationTargetException itex) {
            Throwable cause = itex.getCause();
            if (cause instanceof AssertionError) {
                throw (AssertionError) cause;
            }
            itex.getCause().printStackTrace();
            throw new AssertionError(cause);
        }
        synchronized (findMaxPrefixLock) {
            assertEquals("A", qsPtr[0].getSearchField().getText());
            syncPtr[0] = sync.N;
            findMaxPrefixLock.notifyAll();
            // Wait to set the value
            try {
                findMaxPrefixLock.wait();
            } catch (InterruptedException iex) {
            }
        }
        try {
            Thread.sleep(200);
        } catch (InterruptedException iex) {
        }
        try {
            SwingUtilities.invokeAndWait(new Runnable() {

                @Override
                public void run() {
                    assertEquals("AendPrefix", qsPtr[0].getSearchField().getText());
                }
            });
        } catch (InterruptedException iex) {
            fail("interrupted.");
        } catch (InvocationTargetException itex) {
            Throwable cause = itex.getCause();
            if (cause instanceof AssertionError) {
                throw (AssertionError) cause;
            }
            itex.getCause().printStackTrace();
            throw new AssertionError(cause);
        }
    }
}
