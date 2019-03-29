package org.openide.awt;

import java.awt.Component;
import java.awt.EventQueue;
import java.awt.GraphicsEnvironment;
import java.awt.event.ActionEvent;
import java.awt.event.KeyEvent;
import java.awt.image.BufferedImage;
import javax.swing.AbstractAction;
import javax.swing.AbstractButton;
import javax.swing.Action;
import javax.swing.Icon;
import javax.swing.JButton;
import javax.swing.JFrame;
import javax.swing.JMenu;
import javax.swing.JMenuItem;
import javax.swing.KeyStroke;
import junit.framework.Test;
import junit.framework.TestSuite;
import org.netbeans.junit.MockServices;
import org.netbeans.junit.NbTestCase;
import org.openide.util.HelpCtx;
import org.openide.util.ImageUtilities;
import org.openide.util.Lookup;
import org.openide.util.Utilities;
import org.openide.util.actions.SystemAction;

/**
 * Tests for the Actions class.
 * @author David Strupl
 */
public class ActionsTest extends NbTestCase {

    // 7 testIcon24_disabled.gif     15 testIcon24_disabledSelected.gif
    private static int[][] RESULT_COLORS_00 = { { 255, 255, 255 }, { 0, 0, 0 }, { 255, 255, 255 }, { 0, 0, 0 }, { 255, 255, 255 }, { 0, 0, 0 }, { 255, 255, 255 }, { 0, 0, 0 }, { 255, 255, 255 }, { 0, 0, 0 }, { 255, 255, 255 }, { 0, 0, 0 }, { 255, 255, 255 }, { 0, 0, 0 }, { 255, 255, 255 }, { 0, 0, 0 } };

    private static int[][] RESULT_COLORS_01 = { { 255, 255, 255 }, { 255, 255, 255 }, { 0, 0, 0 }, { 0, 0, 0 }, { 255, 255, 255 }, { 255, 255, 255 }, { 0, 0, 0 }, { 0, 0, 0 }, { 255, 255, 255 }, { 255, 255, 255 }, { 0, 0, 0 }, { 0, 0, 0 }, { 255, 255, 255 }, { 255, 255, 255 }, { 0, 0, 0 }, { 0, 0, 0 } };

    private static int[][] RESULT_COLORS_11 = { { 255, 255, 255 }, { 255, 255, 255 }, { 255, 255, 255 }, { 255, 255, 255 }, { 0, 0, 0 }, { 0, 0, 0 }, { 0, 0, 0 }, { 0, 0, 0 }, { 255, 255, 255 }, { 255, 255, 255 }, { 255, 255, 255 }, { 255, 255, 255 }, { 0, 0, 0 }, { 0, 0, 0 }, { 0, 0, 0 }, { 0, 0, 0 } };

    private static int[][] RESULT_COLORS_10 = { { 255, 255, 255 }, { 255, 255, 255 }, { 255, 255, 255 }, { 255, 255, 255 }, { 255, 255, 255 }, { 255, 255, 255 }, { 255, 255, 255 }, { 255, 255, 255 }, { 0, 0, 0 }, { 0, 0, 0 }, { 0, 0, 0 }, { 0, 0, 0 }, { 0, 0, 0 }, { 0, 0, 0 }, { 0, 0, 0 }, { 0, 0, 0 } };

    static {
        MockServices.setServices(TestConnector.class);
        assertFalse("Initialized Actions class outside of AWT thread", EventQueue.isDispatchThread());
        Actions.cutAmpersand("None");
    }

    public ActionsTest(String name) {
        super(name);
    }

    private static final class TestSystemAction extends SystemAction {

        public void actionPerformed(ActionEvent e) {
        }

        public HelpCtx getHelpCtx() {
            return null;
        }

        public String getName() {
            return "TestSystemAction";
        }

        protected String iconResource() {
            return "org/openide/awt/data/testIcon.gif";
        }
    }

    private static final class TestAction extends AbstractAction {

        public TestAction() {
            putValue("iconBase", "org/openide/awt/data/testIcon.gif");
            putValue(NAME, "test");
        }

        public void actionPerformed(ActionEvent e) {
        }
    }

    private static final class TestNoMenuIconAction extends AbstractAction {

        public TestNoMenuIconAction() {
            putValue("iconBase", "org/openide/awt/data/testIcon.gif");
            putValue("noIconInMenu", Boolean.TRUE);
        }

        public void actionPerformed(ActionEvent e) {
        }
    }

    private static final class TestActionWithTooltip extends AbstractAction {

        private static String TOOLTIP = "tooltip";

        public TestActionWithTooltip() {
            putValue(NAME, "name");
            putValue(SHORT_DESCRIPTION, TOOLTIP);
        }

        public void actionPerformed(ActionEvent e) {
        }
    }

    public static final class TestConnector implements Actions.ButtonActionConnector {

        private int called = 0;

        private boolean active = false;

        public TestConnector() {
            assertFalse("Don't initialize while calling connect on AWT dispatch thread", EventQueue.isDispatchThread());
        }

        public boolean connect(AbstractButton button, Action action) {
            if (!active) {
                return false;
            }
            called += 1;
            return true;
        }

        public boolean connect(JMenuItem item, Action action, boolean popup) {
            if (!active) {
                return false;
            }
            called += 2;
            return true;
        }

        public int getConnectCalled() {
            return called;
        }

        public void setActive(boolean a) {
            called = 0;
            active = a;
        }
    }

    public static Test suite() {
        return GraphicsEnvironment.isHeadless() ? new TestSuite() : new TestSuite(ActionsTest.class);
    }

    /**
     * Tests whether the ButtonActionConnector is being called. The testing
     * implementation is set to "active" only for this test - so the other
     * tests should retain the behaviour like running without the
     * ButtonActionConnector.
     */
    public void testButtonActionConnector() throws Exception {
        TestConnector tc = Lookup.getDefault().lookup(TestConnector.class);
        tc.setActive(true);
        Action action = new ActionsTest.TestAction();
        JButton button = new JButton();
        Actions.connect(button, action);
        assertEquals(1, tc.getConnectCalled());
        JMenuItem jmi = new JMenuItem();
        Actions.connect(jmi, action, false);
        assertEquals(3, tc.getConnectCalled());
        tc.setActive(false);
    }

    @Override
    protected boolean runInEQ() {
        return true;
    }

    private void checkIfLoadedCorrectIcon(Icon icon, Component c, int rowToCheck, String nameOfIcon) {
        checkIfIconOk(icon, c, 0, 0, RESULT_COLORS_00[rowToCheck], nameOfIcon);
        checkIfIconOk(icon, c, 0, 1, RESULT_COLORS_01[rowToCheck], nameOfIcon);
        checkIfIconOk(icon, c, 1, 1, RESULT_COLORS_11[rowToCheck], nameOfIcon);
        checkIfIconOk(icon, c, 1, 0, RESULT_COLORS_10[rowToCheck], nameOfIcon);
    }

    /**
     * Checks colors on coordinates X,Y of the icon and compares them
     * to expectedResult.
     */
    private void checkIfIconOk(Icon icon, Component c, int pixelX, int pixelY, int[] expectedResult, String nameOfIcon) {
        BufferedImage bufImg = new BufferedImage(16, 16, BufferedImage.TYPE_INT_RGB);
        icon.paintIcon(c, bufImg.getGraphics(), 0, 0);
        int[] res = bufImg.getData().getPixel(pixelX, pixelY, (int[]) null);
        log("Icon height is " + icon.getIconHeight());
        log("Icon width is " + icon.getIconWidth());
        for (int i = 0; i < res.length; i++) {
            // this hack doesn't broken the functionality which should testing
            assertTrue(nameOfIcon + ": Color of the [" + pixelX + "," + pixelY + "] pixel is " + res[i] + ", expected was " + expectedResult[i], Math.abs(res[i] - expectedResult[i]) < 10);
        }
    }
}
