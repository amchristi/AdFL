package org.openide.awt;

import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.beans.PropertyChangeEvent;
import java.beans.PropertyChangeListener;
import java.lang.ref.Reference;
import java.lang.ref.WeakReference;
import java.net.URL;
import java.util.Collections;
import java.util.concurrent.atomic.AtomicInteger;
import java.util.logging.Level;
import java.util.prefs.BackingStoreException;
import java.util.prefs.PreferenceChangeEvent;
import java.util.prefs.PreferenceChangeListener;
import java.util.prefs.Preferences;
import javax.swing.AbstractAction;
import javax.swing.Action;
import javax.swing.Icon;
import javax.swing.ImageIcon;
import javax.swing.JMenuItem;
import junit.framework.TestCase;
import org.netbeans.junit.Log;
import org.netbeans.junit.NbTestCase;
import org.netbeans.junit.RandomlyFails;
import org.openide.filesystems.FileObject;
import org.openide.filesystems.FileUtil;
import org.openide.util.ContextAwareAction;
import org.openide.util.Exceptions;
import org.openide.util.Lookup;
import org.openide.util.NbBundle;
import org.openide.util.NbPreferences;
import org.openide.util.actions.Presenter;
import org.openide.util.lookup.AbstractLookup;
import org.openide.util.lookup.InstanceContent;
import org.openide.util.lookup.Lookups;

/**
 * @author Jaroslav Tulach
 */
public class AlwaysEnabledActionTest extends NbTestCase implements PropertyChangeListener {

    private FileObject folder;

    private int changeCounter;

    private static int myListenerCounter;

    private static int myListenerCalled;

    private static int myIconResourceCounter;

    private static final Preferences customPrefs;

    static {
        customPrefs = Preferences.userRoot().node("/myNode");
        try {
            customPrefs.sync();
        } catch (BackingStoreException ex) {
            Exceptions.printStackTrace(ex);
        }
    }

    public AlwaysEnabledActionTest(String testName) {
        super(testName);
    }

    private static class MyListener implements ActionListener {

        public void actionPerformed(ActionEvent e) {
            myListenerCalled++;
        }
    }

    private static class MyAction extends AbstractAction {

        static MyAction last;

        MyAction() {
            last = this;
            setEnabled(false);
        }

        public void actionPerformed(ActionEvent e) {
            myListenerCalled++;
        }
    }

    private static class MyContextAction extends MyAction implements ContextAwareAction {

        static int clones;

        static Lookup lkp;

        public Action createContextAwareInstance(Lookup actionContext) {
            clones++;
            lkp = actionContext;
            return new MyContextAction();
        }
    }

    private static final class PreferencesAction extends AbstractAction {

        static PreferencesAction last;

        int performedCount;

        PreferencesAction() {
            last = this;
        }

        public void actionPerformed(ActionEvent e) {
            performedCount++;
        }
    }

    @Override
    protected void setUp() throws Exception {
        NbBundle.setBranding("big");
        folder = FileUtil.getConfigFile("actions/support/test");
        assertNotNull("testing layer is loaded: ", folder);
        myIconResourceCounter = 0;
        myListenerCalled = 0;
        myListenerCounter = 0;
        MyAction.last = null;
    }

    @Override
    protected boolean runInEQ() {
        return true;
    }

    public void testIconIsCorrect() throws Exception {
        myListenerCounter = 0;
        myIconResourceCounter = 0;
        Action a = readAction("testIconIsCorrect.instance");
        assertNotNull("Action created", a);
        assertEquals("No myListener called", 0, myListenerCounter);
        assertEquals("No myIconURL called", 0, myIconResourceCounter);
        Object name = a.getValue(a.NAME);
        Object mnem = a.getValue(a.MNEMONIC_KEY);
        Object smallIcon = a.getValue(a.SMALL_ICON);
        if (smallIcon instanceof Icon) {
            Icon icon = (Icon) smallIcon;
            assertEquals("Icon height", 32, icon.getIconHeight());
            assertEquals("Icon widht", 32, icon.getIconWidth());
        } else {
            fail("Icon shall be Icon: " + smallIcon);
        }
        assertEquals("Right localized name", "Icon &Name Action", name);
        assertEquals("Mnemonic is N", (int) 'N', mnem);
        assertNotNull("small icon present", smallIcon);
        assertEquals("once icon called", 1, myIconResourceCounter);
        Object base = a.getValue("iconBase");
        assertEquals("iconBase attribute is delegated", 2, myIconResourceCounter);
        assertTrue("Always enabled", a.isEnabled());
        a.setEnabled(false);
        assertTrue("Still Always enabled", a.isEnabled());
        a.actionPerformed(new ActionEvent(this, 0, "kuk"));
        assertEquals("Listener invoked", 1, myListenerCounter);
        assertEquals("No icon in menu", Boolean.TRUE, a.getValue("noIconInMenu"));
        assertContextAware(a);
    }

    public static URL myURL() {
        return AlwaysEnabledActionTest.class.getResource("TestIcon.png");
    }

    private void assertContextAware(Action a) {
        assertTrue("We want context aware actions", a instanceof ContextAwareAction);
    }

    private void checkPreferencesAction(String actionFileName, String preferencesNodePrefix, Preferences prefsRoot) throws Exception {
        Action a = readAction(actionFileName);
        Preferences prefsNode = prefsRoot.node("myNode");
        checkPreferencesAction(a, prefsNode);
        a = Actions.checkbox(preferencesNodePrefix + "/myNode", "myKey", null, null, false);
        checkPreferencesAction(a, prefsNode);
    }

    private void checkPreferencesAction(Action a, Preferences prefsNode) throws Exception {
        prefsNode.putBoolean("myKey", true);
        prefsNode.sync();
        class L implements PreferenceChangeListener {

            boolean notified;

            public synchronized void preferenceChange(PreferenceChangeEvent evt) {
                notified = true;
                notifyAll();
            }

            public synchronized void waitFor() throws Exception {
                while (!notified) {
                    wait();
                }
                notified = false;
            }
        }
        L listener = new L();
        // Verify value
        assertTrue("Expected true as preference value", prefsNode.getBoolean("myKey", false));
        TestCase.assertTrue("Expected to be instance of Presenter.Menu", a instanceof Presenter.Menu);
        JMenuItem item = ((Presenter.Menu) a).getMenuPresenter();
        TestCase.assertTrue("Expected to be selected", item.isSelected());
        prefsNode.addPreferenceChangeListener(listener);
        prefsNode.putBoolean("myKey", false);
        prefsNode.sync();
        listener.waitFor();
        TestCase.assertFalse("Expected to not be selected", item.isSelected());
        // new ActionEvent(null, 0, ""));
        a.actionPerformed(null);
        listener.waitFor();
        TestCase.assertTrue("Expected to be selected", item.isSelected());
        prefsNode.putBoolean("myKey", false);
        prefsNode.sync();
        listener.waitFor();
    }

    private static void assertPropertyPropagated(String propertyName, Object value, Action a, Action delegate) {
        assertEquals("Action's property \"" + propertyName + "\"", value, a.getValue(propertyName));
        assertEquals("Delegate's property \"" + propertyName + "\"", value, delegate.getValue(propertyName));
    }

    private static ActionListener myListener() {
        myListenerCounter++;
        return new MyListener();
    }

    private static ActionListener myAction() {
        myListenerCounter++;
        return new MyAction();
    }

    private static Action myNamedAction() {
        MyAction a = new MyAction();
        a.putValue(Action.NAME, "MyNamedAction");
        return a;
    }

    private static Action myIconAction() {
        MyAction a = new MyAction();
        final ImageIcon ii = new ImageIcon();
        a.putValue(MyAction.SMALL_ICON, ii);
        a.putValue("v", ii);
        return a;
    }

    private static ActionListener myContextAction() {
        myListenerCounter++;
        return new MyContextAction();
    }

    private static String myIconResource() {
        myIconResourceCounter++;
        return "org/openide/awt/TestIcon.png";
    }

    private Action readAction(String fileName) throws Exception {
        FileObject fo = this.folder.getFileObject(fileName);
        assertNotNull("file " + fileName, fo);
        Object obj = fo.getAttribute("instanceCreate");
        assertNotNull("File object has not null instanceCreate attribute", obj);
        if (!(obj instanceof Action)) {
            fail("Object needs to be action: " + obj);
        }
        return (Action) obj;
    }

    public void propertyChange(PropertyChangeEvent evt) {
        changeCounter++;
    }

    static Action preferencesAction() {
        return new PreferencesAction();
    }

    public static Preferences customPreferences() {
        return customPrefs;
    }
}
