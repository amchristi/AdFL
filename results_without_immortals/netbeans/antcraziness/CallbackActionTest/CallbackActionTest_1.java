package org.openide.awt;

import java.awt.event.ActionEvent;
import java.beans.PropertyChangeEvent;
import java.beans.PropertyChangeListener;
import javax.swing.AbstractAction;
import javax.swing.Action;
import javax.swing.ActionMap;
import org.netbeans.junit.MockServices;
import org.netbeans.junit.NbTestCase;
import org.openide.filesystems.FileObject;
import org.openide.filesystems.FileUtil;
import org.openide.util.ContextAwareAction;
import org.openide.util.Lookup;
import org.openide.util.lookup.AbstractLookup;
import org.openide.util.lookup.InstanceContent;

/**
 * @author Jaroslav Tulach
 */
public class CallbackActionTest extends NbTestCase {

    private FileObject folder;

    public CallbackActionTest(String testName) {
        super(testName);
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

    class MyAction extends AbstractAction {

        public int cntEnabled;

        public int cntPerformed;

        @Override
        public boolean isEnabled() {
            cntEnabled++;
            return super.isEnabled();
        }

        @Override
        public void actionPerformed(ActionEvent ev) {
            cntPerformed++;
        }
    }

    @Override
    protected boolean runInEQ() {
        return true;
    }

    @Override
    protected void setUp() throws Exception {
        folder = FileUtil.getConfigFile("actions/support/test");
        assertNotNull("testing layer is loaded: ", folder);
    }

    public void testKeyMustBeProvided() {
        String key = null;
        Action defaultDelegate = null;
        Lookup context = Lookup.EMPTY;
        ContextAwareAction expResult = null;
        try {
            ContextAwareAction result = Actions.callback(key, defaultDelegate, false, "displayName", "org/openide/awt/TestIcon.png", false);
            fail("Shall fail as key is null");
        } catch (NullPointerException ex) {
        }
    }

    static ContextAwareAction callback(String key, AbstractAction fallAction, Lookup al, boolean b) {
        return GeneralAction.callback(key, fallAction, al, b, false);
    }
}
