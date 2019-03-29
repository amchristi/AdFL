package org.openide.awt;

import java.awt.EventQueue;
import java.awt.event.ActionEvent;
import java.beans.PropertyChangeEvent;
import java.beans.PropertyChangeListener;
import java.lang.ref.WeakReference;
import java.lang.reflect.InvocationTargetException;
import java.net.URL;
import java.util.ArrayList;
import java.util.Collection;
import java.util.List;
import javax.swing.AbstractAction;
import javax.swing.Action;
import javax.swing.ActionMap;
import static junit.framework.Assert.assertFalse;
import static junit.framework.Assert.assertTrue;
import org.netbeans.junit.NbTestCase;
import org.openide.filesystems.FileObject;
import org.openide.filesystems.FileUtil;
import org.openide.util.ContextAwareAction;
import org.openide.util.Lookup;
import org.openide.util.lookup.AbstractLookup;
import org.openide.util.lookup.InstanceContent;
import org.openide.util.lookup.Lookups;
import org.openide.util.lookup.ProxyLookup;

/**
 * Test that cookie actions are in fact sensitive to the correct cookies in the
 * correct numbers, and that changes to either node selection or cookies on the
 * selected nodes trigger a change in the selected state.
 * @author Jesse Glick
 */
public class ContextActionTest extends NbTestCase implements Lookup.Provider, ContextActionEnabler<ContextActionTest.Openable> {

    private Lookup lookup;

    private Lookup lookupProxy;

    private ContextAwareAction a1, a2, any, each, all;

    private LookupWithOpenable n1, n2;

    private Lookup n3, n4;

    private int expectedEnabledmentCount = 2;

    public ContextActionTest(String name) {
        super(name);
    }

    public static interface Openable {

        public void open();
    }

    public static class SimpleCookieAction implements ContextActionPerformer<Openable> {

        public void actionPerformed(ActionEvent ev, List<? extends Openable> toOpen) {
            runOn.add(toOpen);
            for (Openable o : toOpen) {
                o.open();
            }
        }

        public static final List<List<? extends Openable>> runOn = new ArrayList<List<? extends Openable>>();
    }

    private static final class LookupWithOpenable extends AbstractLookup implements Lookup.Provider {

        private InstanceContent ic;

        private static final class Open implements Openable {

            public void open() {
            }
        }

        public LookupWithOpenable() {
            this(true);
        }

        public LookupWithOpenable(boolean add) {
            this(new InstanceContent(), add ? new Open() : null);
        }

        public LookupWithOpenable(Openable open) {
            this(new InstanceContent(), open);
        }

        private LookupWithOpenable(InstanceContent ic, Openable open) {
            super(ic);
            this.ic = ic;
            if (open != null) {
                ic.add(open);
            }
            ic.add(this);
        }

        public void setHasCookie(boolean b) {
            if (b && lookup(Openable.class) == null) {
                ic.add(new Open());
            } else if (!b) {
                Openable o = lookup(Openable.class);
                if (o != null) {
                    ic.remove(o);
                }
            }
        }

        public Lookup getLookup() {
            return this;
        }
    }

    @Override
    protected void setUp() throws Exception {
        lookup = Lookup.EMPTY;
        lookupProxy = Lookups.proxy(this);
        a1 = context(new SimpleCookieAction(), null, ContextSelection.EXACTLY_ONE, lookupProxy, Openable.class);
        a2 = context(new SimpleCookieAction(), this, ContextSelection.ANY, lookupProxy, Openable.class);
        any = context(new SimpleCookieAction(), null, ContextSelection.ANY, lookupProxy, Openable.class);
        each = context(new SimpleCookieAction(), null, ContextSelection.EACH, lookupProxy, Openable.class);
        all = context(new SimpleCookieAction(), null, ContextSelection.ALL, lookupProxy, Openable.class);
        n1 = new LookupWithOpenable();
        n2 = new LookupWithOpenable();
        n3 = new LookupWithOpenable(false);
        // share the same cookie instance with n1
        n4 = new LookupWithOpenable(n1.lookup(Openable.class));
        SimpleCookieAction.runOn.clear();
    }

    @Override
    protected boolean runInEQ() {
        return true;
    }

    public void testContextXMLDefinitionNoKey() throws Exception {
        FileObject folder;
        folder = FileUtil.getConfigFile("actions/support/test");
        assertNotNull("testing layer is loaded: ", folder);
        FileObject fo = folder.getFileObject("testContextNoKey.instance");
        Object obj = fo.getAttribute("instanceCreate");
        if (!(obj instanceof ContextAwareAction)) {
            fail("Shall create an action: " + obj);
        }
        ContextAwareAction caa = (ContextAwareAction) obj;
        Action action = caa.createContextAwareInstance(lookupProxy);
        assertEquals("Both actions are equal", action, caa);
        assertEquals("and have the same hash", action.hashCode(), caa.hashCode());
        class SimpleAction extends AbstractAction {

            public int cnt;

            public void actionPerformed(ActionEvent e) {
                cnt++;
            }
        }
        SimpleAction simpleAction = new SimpleAction();
        ActionMap map = new ActionMap();
        LookupWithOpenable openLookup = new LookupWithOpenable();
        activate(openLookup);
        assertTrue("Our action is enabled", this.getIsEnabled(action));
        openLookup.setHasCookie(false);
        assertFalse("Our action is not enabled", this.getIsEnabled(action));
        activate(openLookup, Lookups.singleton(map));
        assertFalse("Still disabled", this.getIsEnabled(action));
        map.put("contextKey", simpleAction);
        assertFalse("Action does not react to any key", this.getIsEnabled(action));
        openLookup.setHasCookie(true);
        assertTrue("Still enabled", this.getIsEnabled(action));
        doActionPerformed(action, new ActionEvent(this, 0, ""));
        assertEquals("no meaning in simple action", 0, simpleAction.cnt);
        assertEquals("Our SimpleCookieAction invoked", 1, SimpleCookieAction.runOn.size());
        List<? extends Openable> open = SimpleCookieAction.runOn.get(0);
        assertEquals("Our SimpleCookieAction invoked", 1, open.size());
        assertSame("the right instance", openLookup.lookup(Openable.class), open.get(0));
        String n = (String) action.getValue(Action.NAME);
        assertEquals("Open", n);
    }

    static URL myIconResource() {
        return ContextAwareAction.class.getResource("TestIcon.png");
    }

    private ActionsInfraHid.WaitPCL doBasicUsageWithEnabler(Action operateOn) throws Exception {
        // Check enablement logic.
        ActionsInfraHid.WaitPCL l = new ActionsInfraHid.WaitPCL("enabled");
        operateOn.addPropertyChangeListener(l);
        assertFalse(getIsEnabled(operateOn));
        activate(new Lookup[] { n1 });
        assertFalse("We need two nodes to become enabled", l.changed());
        l.gotit = 0;
        assertFalse("and there is just one", getIsEnabled(operateOn));
        activate(new Lookup[] { n1, n2 });
        assertTrue("Ok, now we are enabled", l.changed());
        l.gotit = 0;
        assertTrue("Yes", getIsEnabled(operateOn));
        activate(new Lookup[] { n2 });
        assertTrue("Disabled again", l.changed());
        l.gotit = 0;
        assertFalse("Disabled", getIsEnabled(operateOn));
        activate(new Lookup[] { n3 });
        assertFalse(l.changed());
        l.gotit = 0;
        assertFalse(getIsEnabled(operateOn));
        activate(new Lookup[] { n3 });
        assertFalse("Again not changed", l.changed());
        l.gotit = 0;
        assertFalse(getIsEnabled(operateOn));
        activate(new Lookup[] { n1 });
        assertFalse(l.changed());
        l.gotit = 0;
        assertFalse(getIsEnabled(operateOn));
        activate(new Lookup[] { n1 });
        assertFalse("No change", l.changed());
        l.gotit = 0;
        assertFalse(getIsEnabled(operateOn));
        activate(new Lookup[] { n1, n2 });
        assertTrue("now there is enabledment", l.changed());
        l.gotit = 0;
        assertTrue(getIsEnabled(operateOn));
        return l;
    }

    public Lookup getLookup() {
        return lookup;
    }

    private void activate(Lookup... lkps) {
        if (lkps.length == 1) {
            lookup = lkps[0];
        } else if (lkps.length == 0) {
            lookup = Lookup.EMPTY;
        } else {
            lookup = new ProxyLookup(lkps);
        }
        // refresh
        lookupProxy.lookup(Object.class);
    }

    protected boolean getIsEnabled(final Action a1) throws InterruptedException, InvocationTargetException {
        assertTrue("In AWT", EventQueue.isDispatchThread());
        return a1.isEnabled();
    }

    protected void doActionPerformed(final Action a1, final ActionEvent ev) throws InterruptedException, InvocationTargetException {
        assertTrue("In AWT", EventQueue.isDispatchThread());
        a1.actionPerformed(ev);
    }

    public boolean enabled(List<? extends Openable> data) {
        return data.size() == expectedEnabledmentCount;
    }

    static ContextActionEnabler<?> getEnabler() {
        return new ContextActionTest("");
    }

    private static <T> ContextAwareAction context(ContextActionPerformer<T> a, ContextActionEnabler<T> e, ContextSelection s, Lookup lookupProxy, Class<T> c) {
        ContextAction.Performer<T> perf = new ContextAction.Performer<T>(a, e);
        return GeneralAction.context(perf, s, lookupProxy, c);
    }
}
