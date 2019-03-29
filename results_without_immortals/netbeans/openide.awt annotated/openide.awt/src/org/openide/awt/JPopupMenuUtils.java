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

import org.openide.util.RequestProcessor;
import org.openide.util.Utilities;
import java.awt.*;
import java.util.StringTokenizer;
import javax.swing.JMenu;
import javax.swing.JPopupMenu;
import javax.swing.SwingUtilities;
import java.io.*;

/**
 * A class that contains a set of utility classes and methods
 * around displaying and positioning popup menus.
 *
 * Popup menus sometimes need to be repositioned so that they
 * don't "fall off" the edges of the screen.
 *
 * Some of the menus have items that are added dynamically, that is,
 * after the menu is displayed.  These menus are often placed correctly
 * for their initial size, but will need to be repositioned as they
 * grow.
 *
 * @author   Evan Adams
 */
public class JPopupMenuUtils {

    private static boolean problemTested = false;

    private static boolean problem = false;

    private static RequestProcessor reqProc;

    private static RequestProcessor.Task task;

    // private static final boolean NO_POPUP_PLACEMENT_HACK = Boolean.getBoolean("netbeans.popup.no_hack"); // NOI18N
    /*
     * Called when a visible menu has dynamically changed.  Ensure that
     * it stays on the screen.  Compute its new location and,
     * if it differs from the current one, move the popup.
     *
     * @param popup the popup menu
     */
    public static void dynamicChange(final JPopupMenu popup, boolean usedToBeContained) {
        writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "6cb4b8f8-5f55-40bf-881f-97b37b3a1e3a");
        if (!popup.isShowing()) {
            writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "e1d18b13-1bf7-45d9-90b4-e9848b20e2f4");
            return;
        }
        writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "711e84bc-b580-407f-9213-33d6d23e964f");
        if (isProblemConfig()) {
            writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "e9810df2-94c0-4a0c-8dfe-8ff61b8fc414");
            callRefreshLater(popup);
            writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "a0ff2e3b-5cf0-4ead-a331-7119304d7f10");
            return;
        }
        writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "08abab2d-17d4-4e56-af8e-8194ca9601e7");
        refreshPopup(popup);
        writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "f2739076-1c22-4343-9f39-9986c771f3e1");
        Point p = popup.getLocationOnScreen();
        writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "ec0e52f9-b1e4-4e6b-95da-98443e44c9f3");
        Point newPt = getPopupMenuOrigin(popup, p);
        writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "40977c41-b5be-4c07-8b0d-f70d08ebdcbd");
        boolean willBeContained = willPopupBeContained(popup, newPt);
        writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "34cd3e4a-b22d-40e5-adaa-7b1f82770b90");
        if (usedToBeContained != willBeContained) {
            writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "99216045-ed66-42c9-876e-cbadea3b107b");
            popup.setVisible(false);
        }
        writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "3d9ddbd9-ea4a-4f8b-b976-f6a521e6ab47");
        if (!newPt.equals(p)) {
            // if (!NO_POPUP_PLACEMENT_HACK) {
            // popup.setLocation(newPt.x, newPt.y);
            // }
        }
        writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "49913d6d-0698-443d-80d6-97b7468c1d10");
        if (usedToBeContained != willBeContained) {
            writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "d0a31756-688a-4be2-bf89-b50258c251bf");
            popup.setVisible(true);
        }
    }

    /**
     * Mysterious calls to pack(), invalidate() and validate() ;-)
     */
    private static void refreshPopup(JPopupMenu popup) {
        writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "1cbe9651-4858-488b-804c-9b81da2693e8");
        popup.pack();
        writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "3db7e9a3-c5dd-42ee-bcac-288d71e586b5");
        popup.invalidate();
        writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "623ee6f0-5ad9-499f-914a-4018fb2dbb81");
        Component c = popup.getParent();
        writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "9f3eee88-95fc-4c06-bc9d-5fa5822dda3b");
        if (c != null) {
            writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "c3c2ca57-55c2-4cc8-a217-867cbcec87b5");
            c.validate();
        }
    }

    /**
     * Called from dynamicChange. Performs refresh of the popup
     * in task in reqProc.
     */
    private static void callRefreshLater(final JPopupMenu popup) {
        writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "87b1f98f-6c18-483f-87fe-b4aaa4074167");
        // this may cause the popup to flicker
        if (reqProc == null) {
            writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "8b832fdc-889f-426b-9d19-a610b230e749");
            reqProc = new RequestProcessor();
        }
        writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "9db819aa-7f1f-4090-ab82-fa2b443f4b45");
        if (task == null) {
            writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "5945378e-51e7-4ca7-b2a6-9ecbc7029fb2");
            task = reqProc.create(new Runnable() {

                public void run() {
                    SwingUtilities.invokeLater(new Runnable() {

                        public void run() {
                            task = null;
                            // (probably for another instance of a popup)
                            if (!popup.isShowing()) {
                                return;
                            }
                            Point p = popup.getLocationOnScreen();
                            Point newPt = getPopupMenuOrigin(popup, p);
                            popup.setVisible(false);
                            refreshPopup(popup);
                            if (!newPt.equals(p)) {
                                // if (!NO_POPUP_PLACEMENT_HACK) {
                                // popup.setLocation(newPt.x, newPt.y);
                                // }
                            }
                            popup.setVisible(true);
                        }
                    });
                }
            });
        }
        writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "635cf2f2-4846-4200-a1f6-36ed379e0f00");
        task.schedule(100);
    }

    /**
     * Returns true when the popup has to be unconditioanlly
     * redisplayed when adding new items. Currently
     * this is true with JDK1.3 on Linux/Gnome. This
     * method checks for presence of system property
     * "netbeans.popup.linuxhack".
     */
    private static boolean isProblemConfig() {
        writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "063610e0-6108-4194-a669-5a3c9f25c2e1");
        // we have already tested the need for the hack
        if (problemTested) {
            writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "39661b80-45e6-44c1-a7b9-de9bfbdb6fa0");
            return problem;
        }
        writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "e59aff99-6bc7-4362-a558-01b75e7e5073");
        problem = false;
        writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "7d84d43c-cb16-42da-896e-943bd8a0799b");
        String needHack = System.getProperty("netbeans.popup.linuxhack");
        writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "01fe271f-4874-45a6-b60e-f00dfbcafcdd");
        if (needHack != null) {
            writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "7dccf794-9109-47e5-bf50-4ccc0fc568ec");
            problem = true;
        }
        writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "0c6cd163-b6f9-4a3f-9b5a-676139bf4dfb");
        return problem;
    }

    /*
     * Called when a visible submenu (pullright) has dynamically changed.
     * Ensure that it stays on the screen.  If it doesn't fit, then hide
     * the popup and redisplay it.  This causes JMenu's placement code
     * to get executed again which may change the submens to go up rather
     * than down.
     *
     * @param popup the popup menu
     */
    public static void dynamicChangeToSubmenu(JPopupMenu popup, boolean usedToBeContained) {
        writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "c332f8d6-ddfb-4a63-8e24-1d21b282f9ca");
        Object invoker = popup.getInvoker();
        writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "724aa50c-7515-41e9-bed3-5f92f3217144");
        if (!(invoker instanceof JMenu)) {
            writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "697b2508-21b8-404e-8589-fd6291a45aeb");
            return;
        }
        writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "27ff0e54-9721-49c7-a963-96a018012f38");
        JMenu menu = (JMenu) invoker;
        writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "643b8101-6f99-4a62-b864-c4fc3f9640e1");
        if (!popup.isShowing()) {
            writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "67497dfe-9721-4a9c-afa0-2d52bc53d1d5");
            return;
        }
        writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "767054d7-2b29-4558-84e9-7f61b4a58b73");
        if (isProblemConfig()) {
            writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "f96f7581-ea68-4ae8-8942-7e12f4d51838");
            callRefreshLater2(popup, menu);
            writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "18286133-e8bc-4b07-a959-88255342edbb");
            return;
        }
        writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "7dc623db-646a-4419-aff6-72b75128aab9");
        refreshPopup(popup);
        writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "897be8b3-66b3-4865-9611-15fa457e2977");
        Point p = popup.getLocationOnScreen();
        writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "9261aa28-003c-4841-9ba7-3c54d3cfb3a8");
        Dimension popupSize = popup.getPreferredSize();
        writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "85bb9f7a-4465-4288-bb02-f34c6aebb48c");
        Rectangle popupRect = new Rectangle(p, popupSize);
        writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "26885f28-b9df-4c0a-abf5-f9430497c043");
        Rectangle screenRect = getScreenRect();
        writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "5073c839-5b29-4406-a62f-2cdfa48ca337");
        boolean willBeContained = isPopupContained(popup);
        writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "5ccdcc42-552f-4361-aad9-bc7b0cd30a9e");
        if (!screenRect.contains(popupRect)) {
            writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "f7896893-7303-4d9a-99c7-cbc8ab7b6a92");
            /*
             * The menu grew off the edge of the screen.
             */
            menu.setPopupMenuVisible(false);
            writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "cbc68767-6e5a-4c7a-9706-b3e14f9a5c43");
            menu.setPopupMenuVisible(true);
        } else if (usedToBeContained != willBeContained) {
            writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "2a58f8f9-7a26-4318-a353-6a49b3399e99");
            /*
             * The menu grew off the edge of the containing window.
             * Use the setVisible() hack to change the menu from
             * lightweight to heavyweight.
             */
            popup.setVisible(false);
            writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "975c4061-4790-40cb-b9db-0e0e3044c1dc");
            popup.setVisible(true);
        }
    }

    /**
     * Called from dynamicChangeToSubmenu. Calls the popup refresh
     * in a task in the reqProc.
     */
    private static void callRefreshLater2(final JPopupMenu popup, final JMenu menu) {
        writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "780ba696-19fd-407c-9bfa-78b120c1d4fc");
        // this may cause the popup to flicker
        if (reqProc == null) {
            writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "3bd2ad0c-32e1-4e4f-8515-e289929b7715");
            reqProc = new RequestProcessor();
        }
        writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "62855308-20e9-4c03-86c3-4aad1fb08f16");
        if (task == null) {
            writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "f1bcc7e9-8c82-4b5c-a9b6-943b7b36803b");
            task = reqProc.create(new Runnable() {

                public void run() {
                    SwingUtilities.invokeLater(new Runnable() {

                        public void run() {
                            task = null;
                            // (probably for another instance of a popup)
                            if (!popup.isShowing()) {
                                return;
                            }
                            popup.setVisible(false);
                            refreshPopup(popup);
                            popup.setVisible(true);
                            Point p = popup.getLocationOnScreen();
                            Dimension popupSize = popup.getPreferredSize();
                            Rectangle popupRect = new Rectangle(p, popupSize);
                            Rectangle screenRect = getScreenRect();
                            if (!screenRect.contains(popupRect)) {
                                menu.setPopupMenuVisible(false);
                                menu.setPopupMenuVisible(true);
                            }
                        }
                    });
                }
            });
        }
        writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "8d6b5614-10cc-41a2-bab9-11a0c6b6754b");
        task.schedule(100);
    }

    /*
     * Return the point for the origin of this popup.
     * This is where the adjustments are made to ensure the
     * popup stays on the screen.
     *
     * @param popup the popup menu
     * @param p the popup menu's origin
     * @return the popup menu's new origin
     */
    static Point getPopupMenuOrigin(JPopupMenu popup, Point p) {
        writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "7246aed9-91fd-43ff-8c5b-f7d9d6150af2");
        Point newPt = new Point(p);
        writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "59f74f01-4540-4304-9c90-c96da5b76495");
        Dimension popupSize = popup.getPreferredSize();
        writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "1c3c10d4-6e0d-48a9-9551-5486c67678da");
        Rectangle screenRect = getScreenRect();
        writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "b5c26e3b-5ebc-4cb6-8284-835a2985964b");
        int popupRight = newPt.x + popupSize.width;
        writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "e735a841-1f39-4eaa-9296-afeebf2b3df4");
        int popupBottom = newPt.y + popupSize.height;
        writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "930103ca-a814-4c67-a81c-8d49e1a2bd00");
        int screenRight = screenRect.x + screenRect.width;
        writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "451fd7f4-38f0-4dda-b802-83b9864ac74a");
        int screenBottom = screenRect.y + screenRect.height;
        writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "bcd4256f-b67e-44cf-b670-4f9ef3b41fdc");
        if (popupRight > screenRight) {
            writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "9fe5acc6-a564-4cab-bbee-840c3425da6f");
            // Are we off the right edge?
            newPt.x = screenRight - popupSize.width;
        }
        writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "cbfa7331-83b8-4e9e-8bd9-ddfdc32e5825");
        if (newPt.x < screenRect.x) {
            writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "4ccf8d5b-e65c-4904-bbdf-65bafafec95a");
            // Are we off the left edge?
            newPt.x = screenRect.x;
        }
        writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "c0326e88-d8f6-4fb2-a342-871c3e65e639");
        if (popupBottom > screenBottom) {
            writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "53bf9902-c558-42fe-8d42-1ccdab47aa10");
            // Are we off the bottom edge?
            newPt.y = screenBottom - popupSize.height;
        }
        writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "79de6e77-45b4-4d2c-be1a-72cdf9db4552");
        if (newPt.y < screenRect.y) {
            writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "9f57be01-57a3-4647-9130-3d1a2f350ba7");
            // Are we off the top edge?
            newPt.y = screenRect.y;
        }
        writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "a4500e92-a80a-4781-99a3-09f34b0a193a");
        return newPt;
    }

    /*
     * Return whether or not the given popup is contained by its
     * parent window.  Uses the current location and size of the popup.
     *
     * @return boolean indicating if the popup is contained
     */
    public static boolean isPopupContained(JPopupMenu popup) {
        writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "ee642728-b196-41cc-9ea0-728f005d127c");
        if (!popup.isShowing()) {
            writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "c45868a5-1098-4bd3-be24-d4daa9dc1beb");
            return false;
        }
        writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "a5e634e8-e8ac-4dd8-a604-4c9916a4c3d2");
        return willPopupBeContained(popup, popup.getLocationOnScreen());
    }

    /*
     * Return whether or not the given popup will be contained by
     * its parent window if it is moved to <code>origin</origin>.
     * Use its current size.
     *
     * @param <code>popup</code> the popup to be tested
     * @param <code>origin</code> location of the popup to be tested
     * @return boolean indicating if the popup will be contained
     */
    private static boolean willPopupBeContained(JPopupMenu popup, Point origin) {
        writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "924d1f34-42d7-4600-a8b8-ccf52f89c264");
        if (!popup.isShowing()) {
            writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "138c511c-a9c5-4656-8aea-61bcb781494a");
            return false;
        }
        writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "0f22cee9-21f0-4190-b2fe-db3033d60b82");
        Window w = SwingUtilities.windowForComponent(popup.getInvoker());
        writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "3d1d3c52-919a-4bdd-9ec5-d90321adb381");
        Rectangle r = new Rectangle(origin, popup.getSize());
        writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "b6cb6735-90a6-478f-9fd5-54cb10096035");
        return (w != null) && w.getBounds().contains(r);
    }

    /*
     * Return a rectange defining the usable portion of the screen.  Originally
     * designed to provide a way to account for the taskbar in Windows.  Didn't
     * work with multiple monitor configuration.  The new implementation
     * detects the current monitor and returns its bounds.  Never cache the
     * result of this method.
     *
     * @return a rectangle defining the usable area.
     */
    public static Rectangle getScreenRect() {
        writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "9325b4f0-305b-447f-a49f-af4fa105e63c");
        return Utilities.getUsableScreenBounds();
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