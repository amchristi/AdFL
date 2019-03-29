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

    public void testListenerGCed() throws Exception {
        InstanceContent ic = new InstanceContent();
        lkp = new AbstractLookup(ic);
        Lookup.Result<Integer> lookupResult = lkp.lookupResult(Integer.class);
        Action action = ((ContextAwareAction) Actions.forID("cat", "survive")).createContextAwareInstance(lkp);
        Action fallbackAction = ((GeneralAction.DelegateAction) action).fallback;
        WeakReference<Action> fallbackActionRef = new WeakReference<Action>(fallbackAction);
        WeakReference<Action> clone = new WeakReference<Action>(action);
        cm = ContextManager.findManager(lkp, true);
        WeakReference lsetRef = new WeakReference<Object>(cm.findLSet(Integer.class));
        action = null;
        assertGC("Action should be GCed", clone);
        fallbackAction = null;
        assertGC("Fallback action should be GCed", fallbackActionRef);
        assertGC("Action LSet Should be GCed", lsetRef);
        lookupResult.allInstances();
    }
}
