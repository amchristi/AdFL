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
     * Test of findMaxCommonSubstring method, of class QuickSearch.
     */
    public void testFindMaxCommonSubstring() {
        System.out.println("findMaxCommonSubstring");
        String str1 = "annotation";
        String str2 = "antenna";
        boolean ignoreCase = false;
        String expResult = "an";
        String result = QuickSearch.findMaxPrefix(str1, str2, ignoreCase);
        assertEquals(expResult, result);
        str1 = "Annotation";
        expResult = "";
        result = QuickSearch.findMaxPrefix(str1, str2, ignoreCase);
        assertEquals(expResult, result);
        str1 = "AbCdEf";
        str2 = "AbCxxx";
        expResult = "AbC";
        result = QuickSearch.findMaxPrefix(str1, str2, ignoreCase);
        assertEquals(expResult, result);
    }
}
