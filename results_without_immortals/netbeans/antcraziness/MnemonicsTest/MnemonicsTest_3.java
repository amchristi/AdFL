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

    public void testSetLocalizedTextWithModel() throws Exception {
        ButtonModel m = new DefaultButtonModel();
        JButton b = new JButton();
        Mnemonics.setLocalizedText(b, "Hello &There");
        assertEquals("Hello There", b.getText());
        if (Mnemonics.isAquaLF()) {
            assertEquals(0, b.getMnemonic());
            assertEquals(-1, b.getDisplayedMnemonicIndex());
        } else {
            assertEquals('T', b.getMnemonic());
            assertEquals(6, b.getDisplayedMnemonicIndex());
        }
        b.setModel(m);
        assertEquals("Hello There", b.getText());
        if (Mnemonics.isAquaLF()) {
            assertEquals(0, b.getMnemonic());
            assertEquals(-1, b.getDisplayedMnemonicIndex());
        } else {
            assertEquals('T', b.getMnemonic());
            assertEquals(6, b.getDisplayedMnemonicIndex());
        }
    }
}
