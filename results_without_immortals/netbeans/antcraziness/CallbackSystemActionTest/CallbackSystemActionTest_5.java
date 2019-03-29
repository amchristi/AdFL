package org.openide.awt;

import java.awt.event.ActionEvent;
import java.beans.PropertyChangeEvent;
import java.beans.PropertyChangeListener;
import java.lang.ref.Reference;
import java.lang.ref.WeakReference;
import java.util.Arrays;
import java.util.logging.Level;
import java.util.logging.Logger;
import javax.swing.AbstractAction;
import javax.swing.Action;
import javax.swing.ActionMap;
import javax.swing.text.DefaultEditorKit;
import org.netbeans.junit.NbTestCase;
import org.openide.util.ContextAwareAction;
import org.openide.util.Lookup;
import org.openide.util.Utilities;
import org.openide.util.lookup.AbstractLookup;
import org.openide.util.lookup.InstanceContent;
import org.openide.util.lookup.Lookups;

/**
 * Copied from org.openide.util.actions and modified to work on
 * new Actions Support.
 */
public class CallbackSystemActionTest extends NbTestCase {

    private Logger LOG;

    public CallbackSystemActionTest(String name) {
        super(name);
    }

    private static final class CntListener extends Object implements PropertyChangeListener {

        private int cnt;

        public void propertyChange(PropertyChangeEvent evt) {
            cnt++;
        }

        public void assertCnt(String msg, int count) {
            assertEquals(msg, count, this.cnt);
            this.cnt = 0;
        }
    }

    @Override
    protected void setUp() throws Exception {
        LOG = Logger.getLogger("TEST-" + getName());
        LOG.info("setUp");
    }

    @Override
    protected void tearDown() throws Exception {
        LOG.info("tearDown");
        super.tearDown();
        LOG.info("tearDown super finished");
    }

    @Override
    protected Level logLevel() {
        return Level.FINE;
    }

    @Override
    protected int timeOut() {
        return 5000;
    }

    @Override
    protected boolean runInEQ() {
        return true;
    }

    private void doSurviveFocusChangeInTheNewWay(boolean doGC) throws Exception {
        class MyAction extends AbstractAction {

            public int cntEnabled;

            public int cntPerformed;

            @Override
            public boolean isEnabled() {
                cntEnabled++;
                return true;
            }

            public void actionPerformed(ActionEvent ev) {
                cntPerformed++;
            }
        }
        class Disabled extends AbstractAction {

            @Override
            public boolean isEnabled() {
                return false;
            }

            @Override
            public void actionPerformed(ActionEvent e) {
                throw new UnsupportedOperationException("Not supported yet.");
            }
        }
        MyAction myAction = new MyAction();
        ActionMap other = new ActionMap();
        ActionMap tc = new ActionMap();
        ActionMap disabled = new ActionMap();
        disabled.put("somekey", new Disabled());
        InstanceContent ic = new InstanceContent();
        AbstractLookup al = new AbstractLookup(ic);
        ContextAwareAction a = CallbackActionTest.callback("somekey", null, al, true);
        tc.put("somekey", myAction);
        ic.add(other);
        assertFalse("Disabled on other component", a.isEnabled());
        ic.remove(other);
        ic.add(tc);
        assertTrue("MyAction is enabled", a.isEnabled());
        assertEquals("isEnabled called once", 1, myAction.cntEnabled);
        if (doGC) {
            WeakReference<?> ref = new WeakReference<Object>(a);
            a = null;
            assertGC("Action can disappear", ref);
            a = CallbackActionTest.callback("somekey", null, al, true);
        }
        ic.remove(tc);
        ic.add(other);
        assertTrue("Remains enabled", a.isEnabled());
        ic.remove(other);
        ic.add(disabled);
        assertFalse("Becomes disabled", a.isEnabled());
        WeakReference<?> ref = new WeakReference<Object>(a);
        WeakReference<?> ref2 = new WeakReference<Object>(myAction);
        WeakReference<?> ref3 = new WeakReference<Object>(tc);
        a = null;
        myAction = null;
        tc = null;
        assertGC("We are able to clear global action", ref);
        assertGC("Even our action", ref2);
        assertGC("Even our component", ref3);
    }

    public void testLookupOfStateInActionMap() throws Exception {
        class MyAction extends AbstractAction {

            int actionPerformed;

            public void actionPerformed(ActionEvent ev) {
                actionPerformed++;
            }
        }
        MyAction action = new MyAction();
        ActionMap map = new ActionMap();
        InstanceContent ic = new InstanceContent();
        AbstractLookup al = new AbstractLookup(ic);
        ContextAwareAction system = CallbackActionTest.callback("systemKey", null, al, false);
        map.put("systemKey", action);
        Action clone;
        clone = system.createContextAwareInstance(Lookup.EMPTY);
        assertTrue("Action should not be enabled if no callback provided", !clone.isEnabled());
        // 
        action.setEnabled(false);
        Lookup context = Lookups.singleton(map);
        clone = system.createContextAwareInstance(context);
        Action clone2 = system.createContextAwareInstance(context);
        CntListener listener = new CntListener();
        clone.addPropertyChangeListener(listener);
        CntListener listener2 = new CntListener();
        clone2.addPropertyChangeListener(listener2);
        assertTrue("Not enabled now", !clone.isEnabled());
        action.setEnabled(true);
        assertTrue("Clone is enabled because the action in ActionMap is", clone.isEnabled());
        listener.assertCnt("One change expected", 1);
        listener2.assertCnt("One change expected", 1);
        clone.actionPerformed(new ActionEvent(this, 0, ""));
        assertEquals("MyAction.actionPerformed invoked", 1, action.actionPerformed);
        action.setEnabled(false);
        assertTrue("Clone is disabled because the action in ActionMap is", !clone.isEnabled());
        listener.assertCnt("Another change expected", 1);
        listener2.assertCnt("One change expected", 1);
        clone.actionPerformed(new ActionEvent(this, 0, ""));
        assertEquals("MyAction.actionPerformed invoked again", 2, action.actionPerformed);
    }
}
