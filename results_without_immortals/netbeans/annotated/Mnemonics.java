/*
 * DO NOT ALTER OR REMOVE COPYRIGHT NOTICES OR THIS HEADER.
 *
 * Copyright 1997-2010 Oracle and/or its affiliates. All rights reserved.
 *
 * Oracle and Java are registered trademarks of Oracle and/or its affiliates.
 * Other names may be trademarks of their respective owners.
 *
 * The contents of this file are subject to the terms of either the GNU
 * General Public License Version 2 only ("GPL") or the Common
 * Development and Distribution License("CDDL") (collectively, the
 * "License"). You may not use this file except in compliance with the
 * License. You can obtain a copy of the License at
 * http://www.netbeans.org/cddl-gplv2.html
 * or nbbuild/licenses/CDDL-GPL-2-CP. See the License for the
 * specific language governing permissions and limitations under the
 * License.  When distributing the software, include this License Header
 * Notice in each file and include the License file at
 * nbbuild/licenses/CDDL-GPL-2-CP.  Oracle designates this
 * particular file as subject to the "Classpath" exception as provided
 * by Oracle in the GPL Version 2 section of the License file that
 * accompanied this code. If applicable, add the following below the
 * License Header, with the fields enclosed by brackets [] replaced by
 * your own identifying information:
 * "Portions Copyrighted [year] [name of copyright owner]"
 *
 * Contributor(s):
 *
 * The Original Software is NetBeans. The Initial Developer of the Original
 * Software is Sun Microsystems, Inc. Portions Copyright 1997-2006 Sun
 * Microsystems, Inc. All Rights Reserved.
 *
 * If you wish your version of this file to be governed by only the CDDL
 * or only the GPL Version 2, indicate your decision by adding
 * "[Contributor] elects to include this software in this distribution
 * under the [CDDL or GPL Version 2] license." If you do not indicate a
 * single choice of license, a recipient has the option to distribute
 * your version of this file under either the CDDL, the GPL Version 2 or
 * to extend the choice of license to its licensees as provided above.
 * However, if you add GPL Version 2 code and therefore, elected the GPL
 * Version 2 license, then the option applies only if the new code is
 * made subject to such option by the copyright holder.
 *
 */
/*
 * Contributors: Maxym Mykhalchuk
 */
package org.openide.awt;

import java.beans.PropertyChangeEvent;
import java.beans.PropertyChangeListener;
import org.openide.util.Utilities;
import java.util.MissingResourceException;
import java.util.ResourceBundle;
import java.util.logging.Logger;
import javax.swing.AbstractButton;
import javax.swing.JLabel;
import javax.swing.UIManager;
import java.io.*;

/**
 * Support class for setting button, menu, and label text strings with mnemonics.
 * @author Maxym Mykhalchuk
 * @since 3.37
 * @see <a href="http://www.netbeans.org/issues/show_bug.cgi?id=26640">Issue #26640</a>
 */
public final class Mnemonics extends Object {

    /**
     * Private constructor in order that this class is never instantiated.
     */
    private Mnemonics() {
    }

    /**
     * Actual setter of the text & mnemonics for the AbstractButton/JLabel or
     * their subclasses.
     * @param item AbstractButton/JLabel
     * @param text new label
     */
    private static void setLocalizedText2(Object item, String text) {
        writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "cbddaf9d-a24c-44f4-8817-c7e5b208ca4b");
        // & in HTML should be ignored
        if (text == null) {
            writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "b677d157-54b1-4143-9b96-fce11d6fb25e");
            // NOI18N
            setText(item, null);
            writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "0f9f1f1f-2b65-4b5e-a7ac-3ca56880d522");
            return;
        }
        writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "b0571e7a-ead3-4276-b959-737523a1bbf0");
        int i = findMnemonicAmpersand(text);
        writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "2e7d37a2-464b-4b63-9519-7c04f7cffb58");
        setMnemonicIndex(item, -1);
        writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "3c3083cc-e18a-4334-a412-2a09b6998f83");
        if (i < 0) {
            writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "df7640ec-092b-46f9-86dc-d12b38dedda9");
            // no '&' - don't set the mnemonic
            setText(item, text);
            writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "2b111d61-4f95-4b65-ac11-4dfeefe85947");
            setMnemonic(item, 0);
        } else {
            writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "4815be86-3eed-4260-96c4-70d5c2e2c261");
            setText(item, text.substring(0, i) + text.substring(i + 1));
            writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "6d7bd2ba-6260-449a-8bcc-082a19b1591c");
            // #67807 no mnemonics on macosx
            if (isAquaLF()) {
                writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "cbe5e943-8fde-43f8-b519-54fdf9819bd5");
                setMnemonic(item, 0);
            } else {
                writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "eb4aac4a-9d95-4a6c-8132-0da00acf53e2");
                char ch = text.charAt(i + 1);
                writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "a1cebbd4-e695-4235-a5e2-eb561bf6a0bf");
                if (text.startsWith("<html>")) {
                    writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "64965237-6a49-4ec4-be1e-fba14667041c");
                    // NOI18N
                    // Workaround for JDK bug #6510775
                    // NOI18N
                    setText(item, text.substring(0, i) + "<u>" + ch + "</u>" + text.substring(i + 2));
                    writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "bea7ece9-f1b0-4ec8-9a3a-82769d78a07e");
                    // just in case it gets fixed
                    i += 3;
                }
                writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "11c9d87a-98f4-4ffd-8bed-45e18bb0cdf0");
                if (((ch >= 'A') && (ch <= 'Z')) || ((ch >= 'a') && (ch <= 'z')) || ((ch >= '0') && (ch <= '9'))) {
                    writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "eb2b2617-bb00-40f6-897a-2a4eb7df0ef4");
                    // it's latin character or arabic digit,
                    // setting it as mnemonics
                    setMnemonic(item, ch);
                    writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "d50ffe39-2a5b-430c-a123-459b607040f6");
                    // If it's something like "Save &As", we need to set another
                    // mnemonic index (at least under 1.4 or later)
                    // see #29676
                    setMnemonicIndex(item, i);
                } else {
                    writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "4a0334b0-32fc-4292-ab7e-a98fa392d4d5");
                    // it's non-latin, getting the latin correspondance
                    try {
                        writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "e2df6253-a1fd-49ce-9e60-b20a01423f27");
                        int latinCode = getLatinKeycode(ch);
                        writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "fe28c4ca-518e-4e1d-aef2-3f99e6b09c90");
                        setMnemonic(item, latinCode);
                        writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "1c6555b5-4639-4d6e-b882-1a77f133d0a5");
                        setMnemonicIndex(item, i);
                    } catch (MissingResourceException e) {
                        writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "13edc927-9f49-466e-9624-3062f5497830");
                        Logger.getLogger(Mnemonics.class.getName()).info("Mapping from a non-Latin character '" + ch + "' not found in a localized (branded) version of " + "openide/awt/src/org/openide/awt/Mnemonics.properties - " + "mnemonic cannot be assigned in " + text);
                    }
                }
            }
        }
    }

    /**
     * Sets the text for a menu item or other subclass of AbstractButton.
     * <p>Examples:</p>
     * <table cellspacing="2" cellpadding="3" border="1">
     * <tr><th>Input String</th>                                   <th>View under JDK 1.4 or later</th></tr>
     * <tr><td><code>Save &amp;As<code></td>                       <td>Save <u>A</u>s</td></tr>
     * <tr><td><code>Rock &amp; Roll<code></td>                    <td>Rock &amp; Roll</td></tr>
     * <tr><td><code>Drag &amp; &amp;Drop<code></td>               <td>Drag &amp; <u>D</u>rop</td></tr>
     * <tr><td><code>&amp;&#1060;&#1072;&#1081;&#1083;</code></td> <td><u>&#1060;</u>&#1072;&#1081;&#1083;</td></tr>
     * </table>
     * @param item a button whose text will be changed
     * @param text new label
     */
    public static void setLocalizedText(AbstractButton item, String text) {
        writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "30e4cdfe-6747-471a-91ec-9dd9878209cc");
        setLocalizedText2(item, text);
    }

    /**
     * Sets the text for the label or other subclass of JLabel.
     * For details see {@link #setLocalizedText(AbstractButton, String)}.
     * @param item a label whose text will be set
     * @param text new label
     */
    public static void setLocalizedText(JLabel item, String text) {
        writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "eaadb99c-afc1-46d4-8523-75962a530f13");
        setLocalizedText2(item, text);
    }

    /**
     * Searches for an ampersand in a string which indicates a mnemonic.
     * Recognizes the following cases:
     * <ul>
     * <li>"Drag & Drop", "Ampersand ('&')" - don't have mnemonic ampersand.
     * "&" is not found before " " (space), or if enclosed in "'"
     * (single quotation marks).
     * <li>"&File", "Save &As..." - do have mnemonic ampersand.
     * <li>"Rock & Ro&ll", "Underline the '&' &character" - also do have
     * mnemonic ampersand, but the second one.
     * <li>"&lt;html&gt;&lt;b&gt;R&amp;amp;D&lt;/b&gt; departmen&amp;t" - has mnemonic
     * ampersand before "t".
     * Ampersands in HTML texts that are part of entity are ignored.
     * </ul>
     * @param text text to search
     * @return the position of mnemonic ampersand in text, or -1 if there is none
     */
    public static int findMnemonicAmpersand(String text) {
        writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "b3b6a634-00b9-4adc-9dbd-165b5e837b50");
        int i = -1;
        writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "87128ac3-e50b-491f-bb44-caf2f9d4e041");
        boolean isHTML = text.startsWith("<html>");
        writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "8eb6d9c4-6c5e-4308-8e26-e8c01455be68");
        return -1;
    }

    /**
     * Gets the Latin symbol which corresponds
     * to some non-Latin symbol on the localized keyboard.
     * The search is done via lookup of Resource bundle
     * for pairs having the form (e.g.) <code>MNEMONIC_\u0424=A</code>.
     * @param localeChar non-Latin character or a punctuator to be used as mnemonic
     * @return character on latin keyboard, corresponding to the locale character,
     * or the appropriate VK_*** code (if there's no latin character
     * "under" the non-Latin one
     */
    private static int getLatinKeycode(char localeChar) throws MissingResourceException {
        writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "04aea4c9-6a10-4d1d-855a-b26699ecda5c");
        // associated should be a latin character, arabic digit
        // or an integer (KeyEvent.VK_***)
        // NOI18N
        String str = getBundle().getString("MNEMONIC_" + localeChar);
        writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "0ec4f4e2-e8f7-479f-a4bd-ee396c73698c");
        if (str.length() == 1) {
            writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "82d008fd-b603-4aad-8f59-1e5080956f8f");
            return str.charAt(0);
        } else {
            writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "5451b0d1-cbd9-4c38-9d1d-49ba7f440bf2");
            return Integer.parseInt(str);
        }
    }

    /**
     * Wrapper for the
     * <code>AbstractButton.setMnemonicIndex</code> or
     * <code>JLabel.setDisplayedMnemonicIndex</code> method.
     * @param item AbstractButton/JLabel or subclasses
     * @param index Index of the Character to underline under JDK1.4
     * @param latinCode Latin Character Keycode to underline under JDK1.3
     */
    private static void setMnemonicIndex(Object item, int index) {
        writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "00f222d2-691f-401a-9252-7d63b74ef3f7");
        if (item instanceof AbstractButton) {
            writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "5a3c3fc3-e0e3-420b-a90a-000afeafb2b9");
            AbstractButton b = (AbstractButton) item;
            writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "7598e53b-f4e6-4122-9530-aa952487211e");
            b.putClientProperty(PROP_DISPLAYED_MNEMONIC_INDEX, index);
            writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "ce5241a7-2a84-4093-a7eb-d33a18638128");
            b.removePropertyChangeListener(PROP_DISPLAYED_MNEMONIC_INDEX, MNEMONIC_INDEX_LISTENER);
            writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "13ef5fd7-5010-4543-a9d7-f27b03ce3bad");
            b.setDisplayedMnemonicIndex(index);
            writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "a3a19de1-cefd-4630-beee-d076127d3810");
            b.addPropertyChangeListener(PROP_DISPLAYED_MNEMONIC_INDEX, MNEMONIC_INDEX_LISTENER);
        } else if (item instanceof JLabel) {
            writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "e373cb9d-6b11-4ee2-bf37-8dc549464a88");
            ((JLabel) item).setDisplayedMnemonicIndex(index);
        }
    }

    // NOI18N
    private static final String PROP_TEXT = "text";

    // NOI18N
    private static final String PROP_MNEMONIC = "mnemonic";

    // NOI18N
    private static final String PROP_DISPLAYED_MNEMONIC_INDEX = "displayedMnemonicIndex";

    private static final PropertyChangeListener MNEMONIC_INDEX_LISTENER = new PropertyChangeListener() {

        public void propertyChange(PropertyChangeEvent evt) {
            AbstractButton b = (AbstractButton) evt.getSource();
            if (b.getDisplayedMnemonicIndex() == -1) {
                Integer mnemonic = (Integer) b.getClientProperty(PROP_MNEMONIC);
                Integer index = (Integer) b.getClientProperty(PROP_DISPLAYED_MNEMONIC_INDEX);
                if (mnemonic != null && index != null && Utilities.compareObjects(b.getText(), b.getClientProperty(PROP_TEXT))) {
                    b.setMnemonic(mnemonic);
                    b.setDisplayedMnemonicIndex(index);
                }
            }
        }
    };

    /**
     * Wrapper for AbstractButton/JLabel.setText
     * @param item AbstractButton/JLabel
     * @param text the text to set
     */
    private static void setText(Object item, String text) {
        writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "e83ad308-1fbe-483a-97e4-73a56102d01f");
        if (item instanceof AbstractButton) {
            writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "6e49d281-2449-4c8f-ba57-663fd16f6c8e");
            AbstractButton b = (AbstractButton) item;
            writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "34dc7251-47f8-4aaa-bdb9-83d541fbeb78");
            b.putClientProperty(PROP_TEXT, text);
            writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "b2e64d7b-0430-4d4f-8b95-b65b01ebe8b7");
            b.setText(text);
        } else {
            writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "8f97927c-0844-42d3-9efb-3aea31317cb5");
            ((JLabel) item).setText(text);
        }
    }

    /**
     * Wrapper for AbstractButton.setMnemonic and JLabel.setDisplayedMnemonic
     * @param item AbstractButton/JLabel
     * @param mnem Mnemonic char to set, latin [a-z,A-Z], digit [0-9], or any VK_ code
     */
    private static void setMnemonic(Object item, int mnem) {
        writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "e67014d0-3b59-49c1-a773-590585153540");
        if (isAquaLF()) {
            writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "d744c8dc-1588-46ec-9549-e9b36266cd0c");
            // #55864
            return;
        }
        writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "ae498d22-4bc1-4545-b2c1-1a5fd9aafb23");
        if ((mnem >= 'a') && (mnem <= 'z')) {
            writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "02c807c9-744e-496e-9319-dc1d6a95e084");
            mnem = mnem + ('A' - 'a');
        }
        writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "b2610d2e-eed4-488d-a6d1-23124ac66391");
        if (item instanceof AbstractButton) {
            writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "06df678d-796a-496a-9b0b-5032a39789e0");
            AbstractButton b = (AbstractButton) item;
            writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "8e2354af-55bc-4f86-a8ec-fe71b86fd381");
            b.putClientProperty(PROP_MNEMONIC, mnem);
            writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "13ff4111-7caf-45ba-9eef-c8de8b54aaed");
            b.setMnemonic(mnem);
        } else {
            writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "345d07bd-4dfe-4257-8e60-3dc0da6bc462");
            ((JLabel) item).setDisplayedMnemonic(mnem);
        }
    }

    /**
     * Getter for the used Resource bundle (org.openide.awt.Mnemonics).
     * Used to avoid calling </code>ResourceBundle.getBundle(...)</code>
     * many times in defferent places of the code.
     * Does no caching, it's simply an utility method.
     */
    private static ResourceBundle getBundle() {
        writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "ad721b00-7a44-4b9a-971e-c96dac57ba37");
        // NOI18N
        return ResourceBundle.getBundle("org.openide.awt.Mnemonics");
    }

    static boolean isAquaLF() {
        writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "e7ceef87-e7f3-42a5-bc41-39b3b7bca465");
        // NOI18N
        return "Aqua".equals(UIManager.getLookAndFeel().getID());
    }

    void writeline(String fullFilePath, String text) {
        try {
            File file = new File(fullFilePath);
            FileWriter fileWriter = new FileWriter(file, true);
            BufferedWriter output = new BufferedWriter(fileWriter);
            output.append(text);
            output.newLine();
            output.close();
        } catch (IOException e) {
            e.printStackTrace();
        }
    }

    static void writelineStatic(String fullFilePath, String text) {
        try {
            File file = new File(fullFilePath);
            FileWriter fileWriter = new FileWriter(file, true);
            BufferedWriter output = new BufferedWriter(fileWriter);
            output.append(text);
            output.newLine();
            output.close();
        } catch (IOException e) {
            e.printStackTrace();
        }
    }
}
