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

    public void testMnemonicHTML() throws Exception {
        JButton b = new JButton();
        Mnemonics.setLocalizedText(b, "<html><b>R&amp;D</b> department");
        assertEquals("<html><b>R&amp;D</b> department", b.getText());
        assertEquals(0, b.getMnemonic());
        assertEquals(-1, b.getDisplayedMnemonicIndex());
        String underStart = Mnemonics.isAquaLF() ? "" : "<u>";
        String underEnd = Mnemonics.isAquaLF() ? "" : "</u>";
        Mnemonics.setLocalizedText(b, "<html><b>R&amp;D</b> departmen&t");
        assertEquals("<html><b>R&amp;D</b> departmen" + underStart + "t" + underEnd, b.getText());
        if (Mnemonics.isAquaLF()) {
            assertEquals(0, b.getMnemonic());
            assertEquals(-1, b.getDisplayedMnemonicIndex());
        } else {
            assertEquals(KeyEvent.VK_T, b.getMnemonic());
        }
        Mnemonics.setLocalizedText(b, "<html>Smith &amp; &Wesson");
        assertEquals("<html>Smith &amp; " + underStart + "W" + underEnd + "esson", b.getText());
        if (Mnemonics.isAquaLF()) {
            assertEquals(0, b.getMnemonic());
            assertEquals(-1, b.getDisplayedMnemonicIndex());
        } else {
            assertEquals(KeyEvent.VK_W, b.getMnemonic());
        }
        Mnemonics.setLocalizedText(b, "<html>&Advanced Mode <em>(experimental)</em></html>");
        assertEquals("<html>" + underStart + "A" + underEnd + "dvanced Mode <em>(experimental)</em></html>", b.getText());
        if (Mnemonics.isAquaLF()) {
            assertEquals(0, b.getMnemonic());
            assertEquals(-1, b.getDisplayedMnemonicIndex());
        } else {
            assertEquals(KeyEvent.VK_A, b.getMnemonic());
            assertEquals('A', b.getText().charAt(b.getDisplayedMnemonicIndex()));
        }
    }
}
