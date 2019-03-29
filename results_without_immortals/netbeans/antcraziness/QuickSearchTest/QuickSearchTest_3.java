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
     * Test of the addition of quick search component.
     */
    public void testQuickSearchAdd() {
        if (!SwingUtilities.isEventDispatchThread()) {
            try {
                SwingUtilities.invokeAndWait(new Runnable() {

                    @Override
                    public void run() {
                        testQuickSearchAdd();
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
        QuickSearch qs = QuickSearch.attach(component, constraints, new DummyCallback());
        component.addNotify();
        KeyEvent ke = new KeyEvent(component, KeyEvent.KEY_TYPED, System.currentTimeMillis(), 0, KeyEvent.VK_UNDEFINED, 'A');
        // KeyboardFocusManager.getCurrentKeyboardFocusManager().setGlobalFocusOwner(component);
        try {
            Method setGlobalFocusOwner = KeyboardFocusManager.class.getDeclaredMethod("setGlobalFocusOwner", Component.class);
            setGlobalFocusOwner.setAccessible(true);
            setGlobalFocusOwner.invoke(KeyboardFocusManager.getCurrentKeyboardFocusManager(), component);
        } catch (Exception ex) {
            ex.printStackTrace();
            throw new AssertionError(ex);
        }
        component.dispatchEvent(ke);
        assertNotNull(component.added);
        assertNull(component.constraints);
        qs.detach();
        assertNull(component.added);
        constraints = new Object();
        qs = QuickSearch.attach(component, constraints, new DummyCallback());
        ke = new KeyEvent(component, KeyEvent.KEY_TYPED, System.currentTimeMillis(), 0, KeyEvent.VK_UNDEFINED, 'A');
        component.dispatchEvent(ke);
        assertNotNull(component.added);
        assertEquals(constraints, component.constraints);
        qs.detach();
        assertNull(component.added);
    }
}
