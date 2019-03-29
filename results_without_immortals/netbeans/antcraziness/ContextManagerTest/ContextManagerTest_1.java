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

    public void testSurviveFocusChange() throws Exception {
        InstanceContent ic = new InstanceContent();
        Lookup lkp = new AbstractLookup(ic);
        Action clone = ((ContextAwareAction) Actions.forID("cat", "survive")).createContextAwareInstance(lkp);
        L listener = new L();
        clone.addPropertyChangeListener(listener);
        assertFalse("Disabled", clone.isEnabled());
        Object val = Integer.valueOf(1);
        ic.add(val);
        assertTrue("Enabled now", clone.isEnabled());
        assertEquals("One change", 1, listener.cnt);
        ic.remove(val);
        assertTrue("Still Enabled", clone.isEnabled());
        Survival.value = 0;
        clone.actionPerformed(new ActionEvent(this, 0, ""));
        assertEquals("Added one", 1, Survival.value);
    }
}
