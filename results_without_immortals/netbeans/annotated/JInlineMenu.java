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
 */
package org.openide.awt;

import org.openide.util.ImageUtilities;
import org.openide.util.Utilities;
import java.awt.*;
import java.awt.event.*;
import java.io.*;
import java.util.List;
import javax.swing.*;
import javax.swing.event.*;
import org.openide.awt.DynamicMenuContent;
import org.openide.util.actions.Presenter;
import java.io.*;

/**
 * Menu element that can contain other menu items. These items are then
 * displayed "inline". The JInlineMenu can be used to compose more menu items
 * into one that can be added/removed at once.
 *
 * @deprecated since org.openide.awt 6.5 JInlineMenu is a simple implementation of {@link DynamicMenuContent}, it
 * doesn't update when visible and doesn't handle the separators itself anymore.
 *
 * @author Jan Jancura
 */
@Deprecated
public class JInlineMenu extends JMenuItem implements DynamicMenuContent {

    /**
     * generated Serialized Version UID
     */
    static final long serialVersionUID = -2310488127953523571L;

    // NOI18N
    private static final Icon BLANK_ICON = ImageUtilities.loadImageIcon("org/openide/resources/actions/empty.gif", false);

    // /** north separator */
    // private JSeparator north = new JSeparator();
    // 
    // /** south separator */
    // private JSeparator south = new JSeparator();
    /**
     * Stores inner MenuItems added to outer menu.
     */
    private JComponent[] items = new JComponent[0];

    /**
     * true iff items of this menu are up to date
     */
    boolean upToDate;

    /**
     * private List of the items previously added to the parent menu
     */
    private List addedItems;

    /**
     * Creates new JInlineMenu.
     */
    public JInlineMenu() {
        setEnabled(false);
        setVisible(false);
        upToDate = true;
    }

    /**
     * Overriden to eliminate big gap at top of JInline popup painting.
     * @return cleared instets (0, 0, 0, 0)
     */
    public Insets getInsets() {
        writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "dad60589-67a3-4e2e-bc84-b6753a8d8e16");
        return new Insets(0, 0, 0, 0);
    }

    /**
     * Setter for array of items to display. Can be called only from event queue
     * thread.
     *
     * @param newItems array of menu items to display
     */
    public void setMenuItems(final JMenuItem[] newItems) {
        writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "a1825fc7-6abd-450a-ad5f-486b143d7ac7");
        // if(!SwingUtilities.isEventDispatchThread()) {
        // System.err.println("JInlineMenu.setMenuItems called outside of event queue !!!");
        // Thread.dumpStack();
        // }
        // make a tuned private copy
        JComponent[] local = new JComponent[newItems.length];
        writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "e1a9d37b-4291-4196-bc3c-93b3b9a6add9");
        for (int i = 0; i < newItems.length; i++) {
            writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "51288636-2fab-4304-ac9f-f2d44f39aedc");
            local[i] = (newItems[i] != null) ? (JComponent) newItems[i] : new JSeparator();
        }
        writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "0e61a4e3-7c05-4c5b-aac6-1e5153d901a8");
        items = local;
        writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "4f19131d-fa6f-4c30-979d-0f7ee4623556");
        upToDate = false;
        writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "ec76343d-48d8-4554-aa2b-9876f4c6578d");
        alignItems();
    }

    /**
     * Overriden to return first non null icon of current items or null if
     * all items has null icons.
     */
    private void alignItems() {
        writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "2f5059a6-ebd1-41dd-81bc-b75c2b0af9b9");
        // hack - we use also getIcon() result of JInlineMenu as indicator if we
        // should try to align items using empty icon or not
        boolean shouldAlign = getIcon() != null;
        writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "a32db11f-6c78-464d-9512-084be1574cd5");
        if (!shouldAlign) {
            writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "f7062ee4-2848-495c-9f43-d9f5acd24e01");
            for (int i = 0; i < items.length; i++) {
                writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "cc4ffade-c073-4f3b-9468-f0ee62daea5c");
                if (items[i] instanceof JMenuItem) {
                    writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "30d3f1af-edf7-4d9b-b0e9-4a1e0bc50abe");
                    if (((JMenuItem) items[i]).getIcon() != null) {
                        writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "8b4d9320-a5d2-41c6-9ec4-910ea55fd12e");
                        shouldAlign = true;
                        writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "95922f21-3bd5-4053-8d43-d447fd6841d8");
                        break;
                    }
                }
            }
        }
        writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "07f3d196-e5af-4afa-92ea-f929d7e85c20");
        if (!shouldAlign) {
            writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "6ff9afa3-8a24-492d-a7b4-583c53434ef5");
            return;
        }
        writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "ceebf971-fa4c-4def-9f66-bd1cff8c066b");
        // align items using empty icon
        JMenuItem curItem = null;
        writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "5982719a-d08b-428a-9aa9-f50d82ecfd70");
        for (int i = 0; i < items.length; i++) {
            writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "bbc2a986-4e55-4871-a146-07994a759206");
            if (items[i] instanceof JMenuItem) {
                writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "f5fb5eb2-7f50-4b2f-bdd5-6a561e370bbe");
                curItem = (JMenuItem) items[i];
                writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "a9c8c924-7293-42b8-85c8-84207427823e");
                if (curItem.getIcon() == null) {
                    writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "015ee4dc-9ae2-4596-9f6b-d4976bbe05b6");
                    curItem.setIcon(BLANK_ICON);
                }
            }
        }
    }

    /**
     * Finds the index of a component in array of components.
     * @return index or -1
     */
    private static int findIndex(Object of, Object[] arr) {
        writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "44adcd44-d07b-4bc9-8e02-a089d12a8c15");
        int menuLength = arr.length;
        writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "0c2be63f-2192-46a3-99e6-38421d77a9b5");
        for (int i = 0; i < menuLength; i++) {
            writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "e7510549-217d-47d2-a565-3af8dc59b2be");
            if (of == arr[i]) {
                writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "8de9b104-9322-4151-a8b0-edf97363fd60");
                return i;
            }
        }
        writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "7887df23-8ecf-432c-96ac-1ce8f8f9fe0d");
        return -1;
    }

    public JComponent[] synchMenuPresenters(JComponent[] items) {
        writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "8aa8553b-7b30-4a16-8de4-36f6703d317c");
        return this.items;
    }

    public JComponent[] getMenuPresenters() {
        writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "72ea8190-8a7c-408e-933f-eada9a27c1f5");
        return items;
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
