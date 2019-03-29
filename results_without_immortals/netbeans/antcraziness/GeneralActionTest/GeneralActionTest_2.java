package org.openide.awt;

import java.awt.event.ActionListener;
import javax.swing.Action;
import org.netbeans.junit.NbTestCase;
import org.openide.filesystems.FileObject;
import org.openide.filesystems.FileSystem;
import org.openide.filesystems.FileUtil;
import org.openide.filesystems.Repository;
import org.openide.util.ContextAwareAction;
import org.openide.util.Lookup;

/**
 * @author Jaroslav Tulach
 */
public class GeneralActionTest extends NbTestCase {

    private FileObject folder;

    private static int myListenerCounter;

    private static int myIconResourceCounter;

    public GeneralActionTest(String testName) {
        super(testName);
    }

    protected void setUp() throws Exception {
        folder = FileUtil.getConfigFile("actions/support/test");
        assertNotNull("testing layer is loaded: ", folder);
    }

    protected void tearDown() throws Exception {
    }

    protected boolean runInEQ() {
        return true;
    }

    public void testAttributesOnClone() throws Exception {
        // #182601
        Action a = GeneralAction.callback("whatever", null, Lookup.EMPTY, false, false);
        a.putValue("attr", "value");
        Action a2 = ((ContextAwareAction) a).createContextAwareInstance(Lookup.EMPTY);
        assertEquals("value", a2.getValue("attr"));
    }

    private static ActionListener myListener() {
        myListenerCounter++;
        return null;
    }

    private static String myIconResource() {
        myIconResourceCounter++;
        return "/org/netbeans/modules/actions/support/TestIcon.png";
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
}
