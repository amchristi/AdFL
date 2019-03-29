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
     * Test of the quick search listener.
     */
    public void testQuickSearchListener() {
        if (!SwingUtilities.isEventDispatchThread()) {
            try {
                SwingUtilities.invokeAndWait(new Runnable() {

                    @Override
                    public void run() {
                        testQuickSearchListener();
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
            return;
        }
        TestComponent component = new TestComponent();
        Object constraints = null;
        final String[] searchTextPtr = new String[] { null };
        final Boolean[] biasPtr = new Boolean[] { null };
        final boolean[] confirmedPtr = new boolean[] { false };
        final boolean[] canceledPtr = new boolean[] { false };
        QuickSearch.Callback qsc = new QuickSearch.Callback() {

            @Override
            public void quickSearchUpdate(String searchText) {
                assertTrue(SwingUtilities.isEventDispatchThread());
                searchTextPtr[0] = searchText;
            }

            @Override
            public void showNextSelection(boolean forward) {
                assertTrue(SwingUtilities.isEventDispatchThread());
                biasPtr[0] = forward;
            }

            @Override
            public String findMaxPrefix(String prefix) {
                assertTrue(SwingUtilities.isEventDispatchThread());
                return prefix + "endPrefix";
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
        QuickSearch qs = QuickSearch.attach(component, constraints, qsc);
        component.addNotify();
        // Test that a key event passed to the component triggers the quick search:
        try {
            Method setGlobalFocusOwner = KeyboardFocusManager.class.getDeclaredMethod("setGlobalFocusOwner", Component.class);
            setGlobalFocusOwner.setAccessible(true);
            setGlobalFocusOwner.invoke(KeyboardFocusManager.getCurrentKeyboardFocusManager(), component);
        } catch (Exception ex) {
            ex.printStackTrace();
            throw new AssertionError(ex);
        }
        KeyEvent ke = new KeyEvent(component, KeyEvent.KEY_TYPED, System.currentTimeMillis(), 0, KeyEvent.VK_UNDEFINED, 'A');
        component.dispatchEvent(ke);
        assertEquals("A", qs.getSearchField().getText());
        assertEquals("A", searchTextPtr[0]);
        assertNull(biasPtr[0]);
        // Test that further key events passed to the quick search field trigger the quick search listener:
        qs.getSearchField().setCaretPosition(1);
        try {
            Method setGlobalFocusOwner = KeyboardFocusManager.class.getDeclaredMethod("setGlobalFocusOwner", Component.class);
            setGlobalFocusOwner.setAccessible(true);
            setGlobalFocusOwner.invoke(KeyboardFocusManager.getCurrentKeyboardFocusManager(), qs.getSearchField());
        } catch (Exception ex) {
            ex.printStackTrace();
            throw new AssertionError(ex);
        }
        ke = new KeyEvent(qs.getSearchField(), KeyEvent.KEY_TYPED, System.currentTimeMillis(), 0, KeyEvent.VK_UNDEFINED, 'b');
        qs.getSearchField().dispatchEvent(ke);
        assertEquals("Ab", searchTextPtr[0]);
        // Test the up/down keys resulting to selection navigation:
        ke = new KeyEvent(qs.getSearchField(), KeyEvent.KEY_PRESSED, System.currentTimeMillis(), 0, KeyEvent.VK_UP, (char) KeyEvent.VK_UP);
        qs.getSearchField().dispatchEvent(ke);
        assertEquals(Boolean.FALSE, biasPtr[0]);
        ke = new KeyEvent(qs.getSearchField(), KeyEvent.KEY_PRESSED, System.currentTimeMillis(), 0, KeyEvent.VK_DOWN, (char) KeyEvent.VK_DOWN);
        qs.getSearchField().dispatchEvent(ke);
        assertEquals(Boolean.TRUE, biasPtr[0]);
        // Test that tab adds max prefix:
        ke = new KeyEvent(qs.getSearchField(), KeyEvent.KEY_PRESSED, System.currentTimeMillis(), 0, KeyEvent.VK_TAB, '\t');
        qs.getSearchField().dispatchEvent(ke);
        assertEquals("AbendPrefix", qs.getSearchField().getText());
        // Test the quick search confirmation on Enter key:
        assertFalse(confirmedPtr[0]);
        ke = new KeyEvent(qs.getSearchField(), KeyEvent.KEY_PRESSED, System.currentTimeMillis(), 0, KeyEvent.VK_ENTER, '\n');
        qs.getSearchField().dispatchEvent(ke);
        assertTrue(confirmedPtr[0]);
        // Test the quick search cancel on ESC key:
        ke = new KeyEvent(component, KeyEvent.KEY_TYPED, System.currentTimeMillis(), 0, KeyEvent.VK_UNDEFINED, 'A');
        component.dispatchEvent(ke);
        assertEquals("A", searchTextPtr[0]);
        assertFalse(canceledPtr[0]);
        ke = new KeyEvent(qs.getSearchField(), KeyEvent.KEY_PRESSED, System.currentTimeMillis(), 0, KeyEvent.VK_ESCAPE, (char) 27);
        qs.getSearchField().dispatchEvent(ke);
        assertTrue(canceledPtr[0]);
    }
}
