package org.openide.awt;

import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.beans.PropertyChangeEvent;
import java.beans.PropertyChangeListener;
import java.lang.ref.WeakReference;
import javax.swing.Action;
import org.netbeans.junit.NbTestCase;
import org.netbeans.junit.RandomlyFails;
import org.openide.util.ContextAwareAction;
import org.openide.util.Lookup;
import org.openide.util.lookup.AbstractLookup;
import org.openide.util.lookup.InstanceContent;

/**
 * @author Jaroslav Tulach <jtulach@netbeans.org>
 */
public class ContextManagerTest extends NbTestCase {

    private AbstractLookup lkp;

    private ContextManager cm;

    public ContextManagerTest(String name) {
        super(name);
    }

    private static class L implements PropertyChangeListener {

        int cnt;

        @Override
        public void propertyChange(PropertyChangeEvent evt) {
            cnt++;
        }
    }

    @ActionID(category = "cat", id = "survive")
    @ActionRegistration(displayName = "Survive", surviveFocusChange = true)
    public static final class Survival implements ActionListener {

        static int value;

        private Integer context;

        public Survival(Integer context) {
            this.context = context;
        }

        @Override
        public void actionPerformed(ActionEvent ae) {
            value += context;
        }
    }

    @Override
    protected boolean runInEQ() {
        return true;
    }

    public void testAllResultListenersRemoved() throws Exception {
        InstanceContent ic = new InstanceContent();
        lkp = new AbstractLookup(ic);
        Lookup.Result<Integer> lookupResult = lkp.lookupResult(Integer.class);
        Action action = ((ContextAwareAction) Actions.forID("cat", "survive")).createContextAwareInstance(lkp);
        Action fallbackAction = ((GeneralAction.DelegateAction) action).fallback;
        WeakReference<Action> fallbackActionRef = new WeakReference<Action>(fallbackAction);
        WeakReference<Action> clone = new WeakReference<Action>(action);
        cm = ContextManager.findManager(lkp, true);
        WeakReference<ContextManager.LSet> lsetRef = new WeakReference<ContextManager.LSet>(cm.findLSet(Integer.class));
        WeakReference<Lookup.Result> lookupResultRef = new WeakReference<Lookup.Result>(lsetRef.get().result);
        action = null;
        assertGC("Action should be GCed", clone);
        fallbackAction = null;
        assertGC("Fallback action should be GCed", fallbackActionRef);
        assertGC("Action LSet Should be GCed", lsetRef);
        if (lookupResultRef.get() == lookupResult) {
        } else {
            // LSet holds ref to a wrapper class NeverEmptyResult, which should have been GCed
            assertGC("NeverEmptyResult should be GCed", lookupResultRef);
        }
    }
}
