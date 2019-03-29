package org.openide.awt;

import java.awt.EventQueue;
import javax.swing.plaf.ComponentUI;
import javax.swing.plaf.ToolBarUI;
import org.netbeans.junit.NbTestCase;

public class ToolbarWithOverflowTest extends NbTestCase {

    public ToolbarWithOverflowTest(String n) {
        super(n);
    }

    public void testInitOutsideOfEDT() throws Exception {
        class MyToolbar extends ToolbarWithOverflow implements Runnable {

            @Override
            protected void setUI(ComponentUI newUI) {
                assertTrue("Can only be called in EDT", EventQueue.isDispatchThread());
                super.setUI(newUI);
            }

            @Override
            public void setUI(ToolBarUI ui) {
                assertTrue("Can only be called in EDT", EventQueue.isDispatchThread());
                super.setUI(ui);
            }

            private void assertUI() throws Exception {
                EventQueue.invokeAndWait(this);
            }

            @Override
            public void run() {
                assertNotNull("UI delegate is specified", getUI());
            }
        }
        assertFalse("We are not in EDT", EventQueue.isDispatchThread());
        MyToolbar mt = new MyToolbar();
        assertNotNull("Instance created", mt);
        mt.assertUI();
    }
}
