package org.openide.awt;

import java.awt.event.ActionEvent;
import java.awt.event.KeyEvent;
import java.util.Locale;
import java.util.Locale;
import javax.swing.AbstractAction;
import javax.swing.ButtonModel;
import javax.swing.DefaultButtonModel;
import javax.swing.JButton;
import javax.swing.JMenuItem;
import org.netbeans.junit.NbTestCase;
import org.openide.util.NbBundle;
import org.openide.util.Utilities;

/**
 * Test use of mnemonics.
 * @author Jesse Glick
 */
public class MnemonicsTest extends NbTestCase {

    public MnemonicsTest(String name) {
        super(name);
    }

    private static class MnemAction extends AbstractAction {

        public MnemAction() {
            putValue(NAME, "M&nem");
        }

        @Override
        public void actionPerformed(ActionEvent e) {
        }
    }

    public void testNeverMnemonics() {
        Locale.setDefault(new Locale("te", "NO"));
        MnemAction mnem = new MnemAction();
        JMenuItem item = new JMenuItem();
        Actions.connect(item, mnem, true);
        assertEquals("Plain text", "Mnem", item.getText());
        assertEquals("No mnenonic", 0, item.getMnemonic());
    }
}
