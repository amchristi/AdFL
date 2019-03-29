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

    /**
     * @see #31093
     */
    public void testMnemonicAfterParens() throws Exception {
        JButton b = new JButton();
        Mnemonics.setLocalizedText(b, "Execute (&Force Reload)");
        assertEquals("Execute (Force Reload)", b.getText());
        if (Mnemonics.isAquaLF()) {
            assertEquals(0, b.getMnemonic());
            assertEquals(-1, b.getDisplayedMnemonicIndex());
        } else {
            assertEquals(KeyEvent.VK_F, b.getMnemonic());
            assertEquals(9, b.getDisplayedMnemonicIndex());
        }
        assertEquals("Execute (Force Reload)", Actions.cutAmpersand("Execute (&Force Reload)"));
    }
}
