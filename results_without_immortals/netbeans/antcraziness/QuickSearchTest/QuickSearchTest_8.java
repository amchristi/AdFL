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

    public void testClearOnESC() {
        if (!SwingUtilities.isEventDispatchThread()) {
            try {
                SwingUtilities.invokeAndWait(new Runnable() {

                    @Override
                    public void run() {
                        testClearOnESC();
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
        QuickSearch qs = QuickSearch.attach(component, null, new DummyCallback());
        component.addNotify();
        KeyEvent ke = new KeyEvent(component, KeyEvent.KEY_TYPED, System.currentTimeMillis(), 0, KeyEvent.VK_UNDEFINED, 'A');
        component.dispatchEvent(ke);
        JTextField searchField = qs.getSearchField();
        assertTrue(searchField.isDisplayable());
        assertEquals("A", searchField.getText());
        // assertFalse(canceledPtr[0]);
        ke = new KeyEvent(searchField, KeyEvent.KEY_PRESSED, System.currentTimeMillis(), 0, KeyEvent.VK_ESCAPE, (char) 27);
        searchField.dispatchEvent(ke);
        assertTrue(searchField.getText().isEmpty());
        assertFalse(searchField.isDisplayable());
        // Was not always shown
        assertFalse(qs.isAlwaysShown());
        // Force to show
        qs.setAlwaysShown(true);
        assertTrue(searchField.isDisplayable());
        ke = new KeyEvent(component, KeyEvent.KEY_TYPED, System.currentTimeMillis(), 0, KeyEvent.VK_UNDEFINED, 'B');
        component.dispatchEvent(ke);
        assertEquals("B", searchField.getText());
        ke = new KeyEvent(searchField, KeyEvent.KEY_PRESSED, System.currentTimeMillis(), 0, KeyEvent.VK_ESCAPE, (char) 27);
        searchField.dispatchEvent(ke);
        assertTrue(searchField.getText().isEmpty());
        assertTrue(searchField.isDisplayable());
        ke = new KeyEvent(component, KeyEvent.KEY_TYPED, System.currentTimeMillis(), 0, KeyEvent.VK_UNDEFINED, 'B');
        component.dispatchEvent(ke);
        assertEquals("B", searchField.getText());
        // Hide
        qs.setAlwaysShown(false);
        assertEquals("B", searchField.getText());
        assertFalse(searchField.isDisplayable());
        // Show
        qs.setAlwaysShown(true);
        // B is still there
        assertEquals("B", searchField.getText());
        assertTrue(searchField.isDisplayable());
        ke = new KeyEvent(searchField, KeyEvent.KEY_PRESSED, System.currentTimeMillis(), 0, KeyEvent.VK_ESCAPE, (char) 27);
        searchField.dispatchEvent(ke);
        assertTrue(searchField.getText().isEmpty());
    }
}
