package org.openide.awt;

import java.awt.Component;
import javax.swing.AbstractAction;
import javax.swing.Action;
import javax.swing.JComponent;
import javax.swing.JMenuItem;
import javax.swing.JPopupMenu;
import junit.framework.*;
import org.netbeans.junit.*;
import org.openide.util.Lookup;
import org.openide.util.Utilities;
import org.openide.util.actions.Presenter;

/**
 * Tests of actions related methods in Utilities class.
 */
public class UtilitiesActionsTest extends NbTestCase {

    public UtilitiesActionsTest(java.lang.String testName) {
        super(testName);
    }

    private class Act1 extends AbstractAction implements Presenter.Popup, Presenter.Menu {

        public void actionPerformed(java.awt.event.ActionEvent actionEvent) {
        }

        public JMenuItem getPopupPresenter() {
            return new JMenuItem("0");
        }

        public JMenuItem getMenuPresenter() {
            return new JMenuItem("0");
        }
    }

    private class Act2 extends AbstractAction implements Presenter.Menu, Presenter.Popup {

        public void actionPerformed(java.awt.event.ActionEvent actionEvent) {
        }

        public JMenuItem getPopupPresenter() {
            return new Dyna();
        }

        public JMenuItem getMenuPresenter() {
            return new Dyna();
        }
    }

    private class Dyna extends JMenuItem implements DynamicMenuContent {

        private JMenuItem itm1;

        private JMenuItem itm2;

        public JComponent[] getMenuPresenters() {
            itm1 = new JMenuItem();
            itm1.setText("1");
            itm2 = new Dyna2();
            itm2.setText("2");
            return new JComponent[] { itm1, itm2 };
        }

        public JComponent[] synchMenuPresenters(JComponent[] items) {
            ((JMenuItem) items[0]).setText("1x");
            ((JMenuItem) items[1]).setText("2x");
            return items;
        }
    }

    private class Dyna2 extends JMenuItem implements DynamicMenuContent {

        private JMenuItem itm1;

        private JMenuItem itm2;

        public JComponent[] getMenuPresenters() {
            itm1 = new JMenuItem();
            itm1.setText("3");
            itm2 = new JMenuItem();
            itm2.setText("4");
            return new JComponent[] { itm1, itm2, this };
        }

        public JComponent[] synchMenuPresenters(JComponent[] items) {
            ((JMenuItem) items[0]).setText("3x");
            ((JMenuItem) items[1]).setText("4x");
            return items;
        }
    }

    public static void main(java.lang.String[] args) {
        junit.textui.TestRunner.run(new NbTestSuite(UtilitiesActionsTest.class));
    }

    public void testDynamicMenuContent() {
    }
}
