package org.openide.awt;

import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.util.List;
import javax.swing.AbstractAction;
import javax.swing.Action;
import org.netbeans.junit.NbTestCase;
import org.openide.filesystems.FileObject;
import org.openide.filesystems.FileUtil;
import org.openide.util.ContextAwareAction;
import org.openide.util.Lookup;
import org.openide.util.lookup.AbstractLookup;
import org.openide.util.lookup.InstanceContent;
import static org.junit.Assert.*;

/**
 * @author Jaroslav Tulach <jtulach@netbeans.org>
 */
public class ContextActionInjectTest extends NbTestCase {

    private static final InstanceContent contextI = new InstanceContent();

    private static final Lookup context = new AbstractLookup(contextI);

    public ContextActionInjectTest(String n) {
        super(n);
    }

    public static final class Context implements ActionListener {

        private final int context;

        public Context(Integer context) {
            this.context = context;
        }

        static int cnt;

        public void actionPerformed(ActionEvent e) {
            cnt += context;
        }
    }

    public static final class MultiContext implements ActionListener {

        private final List<Number> context;

        public MultiContext(List<Number> context) {
            this.context = context;
        }

        static int cnt;

        public void actionPerformed(ActionEvent e) {
            for (Number n : context) {
                cnt += n.intValue();
            }
        }
    }

    public static final class LookupContext extends AbstractAction implements ContextAwareAction {

        private final Lookup context;

        public LookupContext() {
            this(Lookup.EMPTY);
        }

        private LookupContext(Lookup context) {
            this.context = context;
        }

        static int cnt;

        public void actionPerformed(ActionEvent e) {
            for (Number n : context.lookupAll(Number.class)) {
                cnt += n.intValue();
            }
        }

        public Action createContextAwareInstance(Lookup actionContext) {
            return new LookupContext(actionContext);
        }
    }

    @Override
    protected boolean runInEQ() {
        return true;
    }

    public static Lookup context() {
        return context;
    }

    public void testOwnContextAction() throws Exception {
        MultiContext.cnt = 0;
        FileObject fo = FileUtil.getConfigFile("actions/support/test/testOwnContext.instance");
        assertNotNull("File found", fo);
        Object obj = fo.getAttribute("instanceCreate");
        assertNotNull("Attribute present", obj);
        assertTrue("It is action", obj instanceof Action);
        assertFalse("It is not context aware action: " + obj, obj instanceof ContextAwareAction);
        Action a = (Action) obj;
        InstanceContent ic = contextI;
        ic.add(10);
        assertEquals("Number lover!", a.getValue(Action.NAME));
        a.actionPerformed(new ActionEvent(this, 300, ""));
        assertEquals("Global Action not called", 10, MultiContext.cnt);
        ic.remove(10);
        a.actionPerformed(new ActionEvent(this, 200, ""));
        assertEquals("Global Action stays same", 10, MultiContext.cnt);
    }
}
