package org.openide.awt;

import java.awt.event.ActionEvent;
import java.util.logging.Level;
import javax.swing.AbstractAction;
import javax.swing.Action;
import org.netbeans.junit.Log;
import org.netbeans.junit.NbTestCase;
import org.openide.filesystems.FileUtil;
import org.openide.util.ContextAwareAction;
import org.openide.util.Lookup;

/**
 * Verifies asynchronous aspects of actions systems are close to the
 * original behaviour of SystemAction one.
 * Taken from org.openide.util.actions.AsynchronousTest
 * @author Jaroslav Tulach
 */
public class AsynchronousTest extends NbTestCase {

    private CharSequence err;

    public AsynchronousTest(String name) {
        super(name);
    }

    public static class AC extends AbstractAction {

        static boolean finished;

        public void actionPerformed(ActionEvent ev) {
            synchronized (AsynchronousTest.class) {
                AsynchronousTest.class.notifyAll();
                finished = true;
            }
        }
    }

    public static class CAC extends AC implements ContextAwareAction {

        public Action createContextAwareInstance(Lookup actionContext) {
            return this;
        }
    }

    @Override
    protected int timeOut() {
        return 5000;
    }

    @Override
    protected boolean runInEQ() {
        return true;
    }

    @Override
    protected void setUp() {
        err = Log.enable("", Level.WARNING);
        AC.finished = false;
    }

    public void testExecutionCanBeForcedToBeSynchronous() throws Exception {
        Action action = (Action) FileUtil.getConfigFile("actions/async/true.instance").getAttribute("instanceCreate");
        synchronized (AsynchronousTest.class) {
            action.actionPerformed(new ActionEvent(this, 0, "waitFinished"));
            assertTrue("When asked for synchronous the action is finished immediatelly", AC.finished);
        }
        if (err.length() > 0) {
            fail("No warning about the class: " + err);
        }
    }
}
