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

    public void testMemoryLeak() throws Exception {
        final AtomicInteger count = new AtomicInteger();
        Action singleton = new AbstractAction() {

            @Override
            public void actionPerformed(ActionEvent e) {
                count.incrementAndGet();
            }
        };
        Object heavy = new Object();
        AlwaysEnabledAction always = AlwaysEnabledAction.create(Collections.singletonMap("delegate", singleton));
        Action clone = always.createContextAwareInstance(Lookups.singleton(heavy));
        clone.actionPerformed(null);
        assertEquals(1, count.get());
        Reference<?> r = new WeakReference<Object>(heavy);
        clone = null;
        heavy = null;
        assertGC("should not leak context", r, Collections.singleton(singleton));
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
