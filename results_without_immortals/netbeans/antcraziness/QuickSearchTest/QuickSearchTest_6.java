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
     * Test of processKeyEvent method, of class QuickSearch.
     */
    public void testProcessKeyEvent() {
        TestComponent component = new TestComponent();
        Object constraints = null;
        final String[] searchTextPtr = new String[] { null };
        final Boolean[] biasPtr = new Boolean[] { null };
        final boolean[] confirmedPtr = new boolean[] { false };
        final boolean[] canceledPtr = new boolean[] { false };
        final QuickSearch.Callback qsc = new QuickSearch.Callback() {

            @Override
            public void quickSearchUpdate(String searchText) {
                searchTextPtr[0] = searchText;
            }

            @Override
            public void showNextSelection(boolean forward) {
                biasPtr[0] = forward;
            }

            @Override
            public String findMaxPrefix(String prefix) {
                return prefix + "endPrefix";
            }

            @Override
            public void quickSearchConfirmed() {
                confirmedPtr[0] = true;
            }

            @Override
            public void quickSearchCanceled() {
                canceledPtr[0] = true;
            }
        };
        QuickSearch qs = QuickSearch.attach(component, constraints, qsc);
        KeyEvent ke = new KeyEvent(component, KeyEvent.KEY_TYPED, System.currentTimeMillis(), 0, KeyEvent.VK_UNDEFINED, 'A');
        qs.processKeyEvent(ke);
        assertEquals("A", qs.getSearchField().getText());
        assertEquals("A", searchTextPtr[0]);
        assertNull(biasPtr[0]);
        ke = new KeyEvent(qs.getSearchField(), KeyEvent.KEY_TYPED, System.currentTimeMillis(), 0, KeyEvent.VK_UNDEFINED, 'b');
        qs.processKeyEvent(ke);
        assertEquals("Ab", qs.getSearchField().getText());
        assertEquals("Ab", searchTextPtr[0]);
    }
}
