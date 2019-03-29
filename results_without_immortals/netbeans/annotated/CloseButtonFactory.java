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
 * Software is Sun Microsystems, Inc. Portions Copyright 1997-2007 Sun
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

import java.awt.Dimension;
import java.awt.Toolkit;
import javax.swing.BorderFactory;
import javax.swing.Icon;
import javax.swing.JButton;
import javax.swing.UIManager;
import org.openide.util.ImageUtilities;
import java.io.*;

/**
 * Factory class for Close Buttons.
 *
 * @author M. Kristofic
 * @since 7.38
 */
public final class CloseButtonFactory {

    private static Icon closeTabImage;

    private static Icon closeTabPressedImage;

    private static Icon closeTabMouseOverImage;

    private static Icon bigCloseTabImage;

    private static Icon bigCloseTabPressedImage;

    private static Icon bigCloseTabMouseOverImage;

    private CloseButtonFactory() {
    }

    /**
     * Creates a small 'close' JButton with close icon, rollover icon and pressed icon according to Look and Feel
     *
     * @return JButton with close icons.
     */
    public static JButton createCloseButton() {
        writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "95746f5e-363c-49e6-9a1e-9a7f79bb4029");
        JButton closeButton = new JButton();
        writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "932bf847-4539-40de-b1f0-6ca540079752");
        int size = 16;
        writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "e3fbe9bf-2e16-4102-b345-193018e1db50");
        closeButton.setPreferredSize(new Dimension(size, size));
        writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "0173e077-12f3-4aaa-b7b8-68a38393ba52");
        closeButton.setContentAreaFilled(false);
        writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "1530afb8-d75a-44c4-af98-49511363faed");
        closeButton.setFocusable(false);
        writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "f58d3694-bde7-4f7f-bc96-374a60527df7");
        closeButton.setBorder(BorderFactory.createEmptyBorder());
        writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "839c5c6c-d36b-4420-968f-9132e6c2f3d5");
        closeButton.setBorderPainted(false);
        writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "97ffad7f-ddd2-4ddb-bdba-a5399bc0246d");
        closeButton.setRolloverEnabled(true);
        writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "5e649978-2258-4fae-bb4d-5f490a2da933");
        closeButton.setIcon(getCloseTabImage());
        writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "c65c08b2-e1e6-4897-afbb-0be985f8ab50");
        closeButton.setRolloverIcon(getCloseTabRolloverImage());
        writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "2059bdce-3702-45f6-aed3-443aa14a934c");
        closeButton.setPressedIcon(getCloseTabPressedImage());
        writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "179a84d3-b1ec-4f5d-a184-7757924f746b");
        return closeButton;
    }

    /**
     * Creates a big 'close' JButton with close icon, rollover icon and pressed icon according to Look and Feel
     *
     * @return JButton with close icons.
     */
    public static JButton createBigCloseButton() {
        writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "23651461-bf22-4c8e-bc87-6fc495bcc0e2");
        JButton closeButton = new JButton();
        writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "7092d1ff-7d76-4672-8525-40e1c4015e43");
        int size = 19;
        writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "aef94fd5-beaf-464a-aca6-1eaeac02c114");
        closeButton.setPreferredSize(new Dimension(size, size));
        writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "083f6953-fbcc-4d1a-99b7-29887f82b8b5");
        closeButton.setContentAreaFilled(false);
        writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "b7a6f6c5-1539-4969-9624-90f0ecc25a6c");
        closeButton.setFocusable(false);
        writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "0321fc94-57f0-4dfe-9c2f-be7f809dae96");
        closeButton.setBorder(BorderFactory.createEmptyBorder());
        writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "3f9bbfce-ded7-427c-b849-0a5ceed21c21");
        closeButton.setBorderPainted(false);
        writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "423235dd-76dc-45e7-b730-5d3f4091757c");
        closeButton.setRolloverEnabled(true);
        writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "df916439-2658-4d96-88a8-be0fe6dc6d0b");
        closeButton.setIcon(getBigCloseTabImage());
        writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "473df6c5-b60f-402d-88f1-f8a8882a2740");
        closeButton.setRolloverIcon(getBigCloseTabRolloverImage());
        writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "021cffc9-8a0a-42ce-9bf5-efc49e51fcb5");
        closeButton.setPressedIcon(getBigCloseTabPressedImage());
        writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "0939cd84-a982-4af7-ab83-999fde763709");
        return closeButton;
    }

    private static boolean isWindowsVistaLaF() {
        writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "2171ca37-4f4c-4aa7-a09e-14b86d6893c3");
        return isWindowsLaF() && (isWindowsVista() || isWindows7()) && isWindowsXPLaF();
    }

    private static boolean isWindows8LaF() {
        writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "09ca1e74-d678-457e-99e5-82e4134c9bb6");
        return isWindowsLaF() && isWindows8() && isWindowsXPLaF();
    }

    private static boolean isWindows10LaF() {
        writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "b054f3b1-faad-42ac-b28a-464e4482c703");
        return isWindowsLaF() && isWindows10() && isWindowsXPLaF();
    }

    private static boolean isWindowsVista() {
        writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "10a22f76-3f08-4139-abc4-64cd587c8d49");
        String osName = System.getProperty("os.name");
        writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "907c51f4-45de-4484-a6df-2daaf912207e");
        return osName.indexOf("Vista") >= 0 || (osName.equals("Windows NT (unknown)") && "6.0".equals(System.getProperty("os.version")));
    }

    private static boolean isWindows10() {
        writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "a0353aaa-46a3-441a-b1ae-9a55f4aaa56b");
        String osName = System.getProperty("os.name");
        writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "0bda02c3-e706-4790-a029-f1af8f499d32");
        return osName.indexOf("Windows 10") >= 0 || (osName.equals("Windows NT (unknown)") && "10.0".equals(System.getProperty("os.version")));
    }

    private static boolean isWindows8() {
        writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "677855ec-532a-4220-aafb-1dcae592f9e7");
        String osName = System.getProperty("os.name");
        writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "ccf399f3-890c-49c6-bf82-7c36d047051c");
        return osName.indexOf("Windows 8") >= 0 || (osName.equals("Windows NT (unknown)") && "6.2".equals(System.getProperty("os.version")));
    }

    private static boolean isWindows7() {
        writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "949caba2-a88c-4185-afa4-de060c187299");
        String osName = System.getProperty("os.name");
        writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "323e2085-23a7-4e4c-8ab8-429776fb776c");
        return osName.indexOf("Windows 7") >= 0 || (osName.equals("Windows NT (unknown)") && "6.1".equals(System.getProperty("os.version")));
    }

    private static boolean isWindowsXPLaF() {
        writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "20b358a7-a143-4fe7-a204-eee1a0623335");
        Boolean isXP = (Boolean) Toolkit.getDefaultToolkit().getDesktopProperty(// NOI18N
        "win.xpstyle.themeActive");
        writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "bb5d4281-b85e-4c49-811c-b59fa83a4b5f");
        return isWindowsLaF() && (isXP == null ? false : isXP.booleanValue());
    }

    private static boolean isWindowsLaF() {
        writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "578d622f-9811-47e7-ae6f-c6be0e8ecb4a");
        String lfID = UIManager.getLookAndFeel().getID();
        writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "94fd9242-6c1e-427d-89b4-74db2dd105f1");
        // NOI18N
        return lfID.endsWith("Windows");
    }

    private static boolean isAquaLaF() {
        writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "5909a87a-867e-4eee-8cd7-f6a7a4bed276");
        return "Aqua".equals(UIManager.getLookAndFeel().getID());
    }

    private static boolean isGTKLaF() {
        writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "d60fe521-65e4-4cce-bdbc-b5734ce09693");
        // NOI18N
        return "GTK".equals(UIManager.getLookAndFeel().getID());
    }

    private static Icon getCloseTabImage() {
        writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "cd33fb53-9be9-47a5-81dd-45ef5bfae243");
        if (null == closeTabImage) {
            writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "175f9bd6-4fc1-4892-8ca0-92c595ef9d58");
            // NOI18N
            String path = UIManager.getString("nb.close.tab.icon.enabled.name");
            writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "7a216867-8b18-4049-9953-192c4f82c210");
            if (null != path) {
                writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "8e19a3d8-583c-44bf-b968-e7b4a622a43e");
                // NOI18N
                closeTabImage = ImageUtilities.loadImageIcon(path, true);
            }
        }
        writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "418ed5f9-f622-4905-b992-8334d3006288");
        if (null == closeTabImage) {
            writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "8b7d5fe9-b2f9-4e67-9042-e4ab1d08805b");
            if (isWindows8LaF() || isWindows10LaF()) {
                writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "4d2c3f93-8e88-43e4-8b94-809f86bbd4c9");
                // NOI18N
                closeTabImage = ImageUtilities.loadImageIcon("org/openide/awt/resources/win8_bigclose_enabled.png", true);
            } else if (isWindowsVistaLaF()) {
                writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "2f34d119-97f2-4516-9946-a8b4a9d5d559");
                // NOI18N
                closeTabImage = ImageUtilities.loadImageIcon("org/openide/awt/resources/vista_close_enabled.png", true);
            } else if (isWindowsXPLaF()) {
                writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "ec1ae3bd-9c26-49c9-8c57-7aa8a05df74d");
                // NOI18N
                closeTabImage = ImageUtilities.loadImageIcon("org/openide/awt/resources/xp_close_enabled.png", true);
            } else if (isWindowsLaF()) {
                writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "06c11ee4-4c24-42ea-b5ed-5ebbfb43f8f9");
                // NOI18N
                closeTabImage = ImageUtilities.loadImageIcon("org/openide/awt/resources/win_close_enabled.png", true);
            } else if (isAquaLaF()) {
                writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "ad4c2a69-e3cd-4303-8f36-307c5fdb1bac");
                // NOI18N
                closeTabImage = ImageUtilities.loadImageIcon("org/openide/awt/resources/mac_close_enabled.png", true);
            } else if (isGTKLaF()) {
                writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "d027dc1d-84a6-42c1-9db6-b30697746044");
                // NOI18N
                closeTabImage = ImageUtilities.loadImageIcon("org/openide/awt/resources/gtk_close_enabled.png", true);
            } else {
                writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "6235bacb-1b00-4ee5-80fe-3df7ea471b60");
                // NOI18N
                closeTabImage = ImageUtilities.loadImageIcon("org/openide/awt/resources/metal_close_enabled.png", true);
            }
        }
        writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "af4061e0-6c9f-49ec-ab31-e279bef0450d");
        return closeTabImage;
    }

    private static Icon getCloseTabPressedImage() {
        writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "032a614c-f5e2-471d-92a6-07e3ff0740f9");
        if (null == closeTabPressedImage) {
            writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "0f6ac8d6-454b-4ffd-a673-62dd566c20b4");
            // NOI18N
            String path = UIManager.getString("nb.close.tab.icon.pressed.name");
            writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "8e267621-01e6-4980-aa9e-890c07a8b310");
            if (null != path) {
                writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "5cc98eca-840e-47ef-8c89-c24e6f99bd54");
                // NOI18N
                closeTabPressedImage = ImageUtilities.loadImageIcon(path, true);
            }
        }
        writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "e309510d-1c32-41b8-88b3-784f2bf1b82e");
        if (null == closeTabPressedImage) {
            writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "4491bd2a-a8db-414e-95e8-3882e32a6e82");
            if (isWindows8LaF() || isWindows10LaF()) {
                writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "e94bafbc-44bd-4084-9941-34cef9453e39");
                // NOI18N
                closeTabPressedImage = ImageUtilities.loadImageIcon("org/openide/awt/resources/win8_bigclose_pressed.png", true);
            } else if (isWindowsVistaLaF()) {
                writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "aafb9a11-0f4c-4b57-8f1c-9e43ff025580");
                // NOI18N
                closeTabPressedImage = ImageUtilities.loadImageIcon("org/openide/awt/resources/vista_close_pressed.png", true);
            } else if (isWindowsXPLaF()) {
                writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "391a3d90-c71b-418a-8c28-ee2f4e0a7ec8");
                // NOI18N
                closeTabPressedImage = ImageUtilities.loadImageIcon("org/openide/awt/resources/xp_close_pressed.png", true);
            } else if (isWindowsLaF()) {
                writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "4d91e88f-ee6c-4f28-a85c-09f7042b11fc");
                // NOI18N
                closeTabPressedImage = ImageUtilities.loadImageIcon("org/openide/awt/resources/win_close_pressed.png", true);
            } else if (isAquaLaF()) {
                writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "48722bd3-335e-41d6-9ee7-2e9f323da64c");
                // NOI18N
                closeTabPressedImage = ImageUtilities.loadImageIcon("org/openide/awt/resources/mac_close_pressed.png", true);
            } else if (isGTKLaF()) {
                writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "eb670d58-4b96-4a14-80a0-8a61c9028794");
                // NOI18N
                closeTabPressedImage = ImageUtilities.loadImageIcon("org/openide/awt/resources/gtk_close_pressed.png", true);
            } else {
                writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "2e2a7e72-b99e-4812-9d66-b06f725e1830");
                // NOI18N
                closeTabPressedImage = ImageUtilities.loadImageIcon("org/openide/awt/resources/metal_close_pressed.png", true);
            }
        }
        writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "06e43cf1-b3ef-4f2f-8649-1b9c5b41d6b2");
        return closeTabPressedImage;
    }

    private static Icon getCloseTabRolloverImage() {
        writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "032627ce-44c4-4d1f-8255-c682b6e6e0a0");
        if (null == closeTabMouseOverImage) {
            writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "b6f33738-5246-4415-a912-d70a0b3c5ccb");
            // NOI18N
            String path = UIManager.getString("nb.close.tab.icon.rollover.name");
            writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "cf474440-c489-4e24-a901-7ad81866948a");
            if (null != path) {
                writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "a428c292-03e9-4936-8ac4-6e4a75ce9cac");
                // NOI18N
                closeTabMouseOverImage = ImageUtilities.loadImageIcon(path, true);
            }
        }
        writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "cd9748ab-bfad-4cfe-bd28-f6e44e48eb9c");
        if (null == closeTabMouseOverImage) {
            writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "0544cd28-b164-4416-a730-1468160182e1");
            if (isWindows8LaF() || isWindows10LaF()) {
                writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "b64bb7d6-d77f-4e59-aee9-b506f719b55f");
                // NOI18N
                closeTabMouseOverImage = ImageUtilities.loadImageIcon("org/openide/awt/resources/win8_bigclose_rollover.png", true);
            } else if (isWindowsVistaLaF()) {
                writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "334acb5c-1cd6-414f-a951-984411ab27b8");
                // NOI18N
                closeTabMouseOverImage = ImageUtilities.loadImageIcon("org/openide/awt/resources/vista_close_rollover.png", true);
            } else if (isWindowsXPLaF()) {
                writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "bb36b802-7866-4de4-af66-2e85dfe6b310");
                // NOI18N
                closeTabMouseOverImage = ImageUtilities.loadImageIcon("org/openide/awt/resources/xp_close_rollover.png", true);
            } else if (isWindowsLaF()) {
                writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "663274af-1492-40a0-9ebb-694c9ba1f3ef");
                // NOI18N
                closeTabMouseOverImage = ImageUtilities.loadImageIcon("org/openide/awt/resources/win_close_rollover.png", true);
            } else if (isAquaLaF()) {
                writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "028d27bb-a55a-4f81-a5ee-cc112561560f");
                // NOI18N
                closeTabMouseOverImage = ImageUtilities.loadImageIcon("org/openide/awt/resources/mac_close_rollover.png", true);
            } else if (isGTKLaF()) {
                writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "cfbe6402-2441-4f18-8db0-bebbc03b274f");
                // NOI18N
                closeTabMouseOverImage = ImageUtilities.loadImageIcon("org/openide/awt/resources/gtk_close_rollover.png", true);
            } else {
                writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "2c6e70f7-5b7e-4dca-a436-d44300d60eb4");
                // NOI18N
                closeTabMouseOverImage = ImageUtilities.loadImageIcon("org/openide/awt/resources/metal_close_rollover.png", true);
            }
        }
        writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "291ebcaa-bda3-4759-b0d6-8d656b3f447e");
        return closeTabMouseOverImage;
    }

    private static Icon getBigCloseTabImage() {
        writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "88ee0a43-c251-4cb6-9bc0-adb38e072a02");
        if (null == bigCloseTabImage) {
            writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "4484d8e6-dfac-4409-8107-50e3e6635d8e");
            // NOI18N
            String path = UIManager.getString("nb.bigclose.tab.icon.enabled.name");
            writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "00642a20-53f2-44e2-a173-5ceae65192e9");
            if (null != path) {
                writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "ba21d5d4-f171-478f-8aa8-e2536c363528");
                // NOI18N
                bigCloseTabImage = ImageUtilities.loadImageIcon(path, true);
            }
        }
        writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "3d76ae32-fb82-49c5-9862-5519dfe3a31c");
        if (null == bigCloseTabImage) {
            writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "862b5466-be42-4d94-a5ba-5287a3a6dba0");
            if (isWindows8LaF() || isWindows10LaF()) {
                writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "f0098c44-712e-45ff-af2e-4042af5f811f");
                // NOI18N
                bigCloseTabImage = ImageUtilities.loadImageIcon("org/openide/awt/resources/win8_bigclose_enabled.png", true);
            } else if (isWindowsVistaLaF()) {
                writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "fde5db15-7f09-4ae2-bfcf-c4d989356188");
                // NOI18N
                bigCloseTabImage = ImageUtilities.loadImageIcon("org/openide/awt/resources/vista_bigclose_enabled.png", true);
            } else if (isWindowsXPLaF()) {
                writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "e0d08d7e-f1f2-4bff-9395-f03dda31ad48");
                // NOI18N
                bigCloseTabImage = ImageUtilities.loadImageIcon("org/openide/awt/resources/xp_bigclose_enabled.png", true);
            } else if (isWindowsLaF()) {
                writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "a1391553-bc04-4094-b29e-404ed01411de");
                // NOI18N
                bigCloseTabImage = ImageUtilities.loadImageIcon("org/openide/awt/resources/win_bigclose_enabled.png", true);
            } else if (isAquaLaF()) {
                writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "3192fd58-3868-4c89-80c1-a9341057fa63");
                // NOI18N
                bigCloseTabImage = ImageUtilities.loadImageIcon("org/openide/awt/resources/mac_bigclose_enabled.png", true);
            } else if (isGTKLaF()) {
                writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "89e61f70-5285-40ed-aa4c-eebe4fe1d984");
                // NOI18N
                bigCloseTabImage = ImageUtilities.loadImageIcon("org/openide/awt/resources/gtk_bigclose_enabled.png", true);
            } else {
                writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "516c474d-da4b-4f6a-a258-c1167243bd7a");
                // NOI18N
                bigCloseTabImage = ImageUtilities.loadImageIcon("org/openide/awt/resources/metal_bigclose_enabled.png", true);
            }
        }
        writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "9142a598-654a-4db3-96b3-5baa57f4ade8");
        return bigCloseTabImage;
    }

    private static Icon getBigCloseTabPressedImage() {
        writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "ea2b91c2-abd6-47f1-86d8-bac3e21a4f71");
        if (null == bigCloseTabPressedImage) {
            writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "6ceef6d5-9394-42ca-9a0d-13776382a26d");
            // NOI18N
            String path = UIManager.getString("nb.bigclose.tab.icon.pressed.name");
            writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "ec78ab13-fb16-48a6-a213-5c9cc19f33b1");
            if (null != path) {
                writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "de9d0967-faba-4988-982e-ac3eddd37b80");
                // NOI18N
                bigCloseTabPressedImage = ImageUtilities.loadImageIcon(path, true);
            }
        }
        writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "3fba8d99-234e-4364-8231-bccedea7d850");
        if (null == bigCloseTabPressedImage) {
            writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "502f4c40-01de-4566-acf8-b2df3267a9ca");
            if (isWindows8LaF() || isWindows10LaF()) {
                writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "e0d36aa1-6a5a-4c42-b7e8-f3928621e65e");
                // NOI18N
                bigCloseTabPressedImage = ImageUtilities.loadImageIcon("org/openide/awt/resources/win8_bigclose_pressed.png", true);
            } else if (isWindowsVistaLaF()) {
                writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "fdf5aeb2-26fb-4de0-acb1-82ec6410d75e");
                // NOI18N
                bigCloseTabPressedImage = ImageUtilities.loadImageIcon("org/openide/awt/resources/vista_bigclose_pressed.png", true);
            } else if (isWindowsXPLaF()) {
                writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "14f45854-8940-448f-9760-a9ebdbaaa66c");
                // NOI18N
                bigCloseTabPressedImage = ImageUtilities.loadImageIcon("org/openide/awt/resources/xp_bigclose_pressed.png", true);
            } else if (isWindowsLaF()) {
                writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "6ed64c11-ba58-4c11-9843-9ef40c336e60");
                // NOI18N
                bigCloseTabPressedImage = ImageUtilities.loadImageIcon("org/openide/awt/resources/win_bigclose_pressed.png", true);
            } else if (isAquaLaF()) {
                writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "63e7e024-e2be-45ac-a295-50174b7ad1b6");
                // NOI18N
                bigCloseTabPressedImage = ImageUtilities.loadImageIcon("org/openide/awt/resources/mac_bigclose_pressed.png", true);
            } else if (isGTKLaF()) {
                writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "1861e770-d64d-4e07-aad5-5b60d8783121");
                // NOI18N
                bigCloseTabPressedImage = ImageUtilities.loadImageIcon("org/openide/awt/resources/gtk_bigclose_pressed.png", true);
            } else {
                writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "3c7cdb1c-dd08-493a-b61e-c274dca75d61");
                // NOI18N
                bigCloseTabPressedImage = ImageUtilities.loadImageIcon("org/openide/awt/resources/metal_bigclose_pressed.png", true);
            }
        }
        writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "52d0cb29-056e-4920-8e5e-5fc07b462e2b");
        return bigCloseTabPressedImage;
    }

    private static Icon getBigCloseTabRolloverImage() {
        writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "d36eb609-db64-4764-925d-d8dc0a2710d2");
        if (null == bigCloseTabMouseOverImage) {
            writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "a507f291-e071-461a-842e-2cca11069db2");
            // NOI18N
            String path = UIManager.getString("nb.bigclose.tab.icon.rollover.name");
            writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "338de51c-853e-4085-89f2-fb07cb880329");
            if (null != path) {
                writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "e99bae2e-0d89-4ac4-be49-fe854e58a2d2");
                // NOI18N
                bigCloseTabMouseOverImage = ImageUtilities.loadImageIcon(path, true);
            }
        }
        writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "40cc17a8-a391-4332-89bb-12ae3161614a");
        if (null == bigCloseTabMouseOverImage) {
            writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "909153f8-8662-4211-95ad-b02d5102e1be");
            if (isWindows8LaF() || isWindows10LaF()) {
                writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "524a1903-a2e8-42c2-888d-dab58a429501");
                // NOI18N
                bigCloseTabMouseOverImage = ImageUtilities.loadImageIcon("org/openide/awt/resources/win8_bigclose_rollover.png", true);
            } else if (isWindowsVistaLaF()) {
                writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "ec9cbb99-5a16-4ffb-8ee3-696838020f9e");
                // NOI18N
                bigCloseTabMouseOverImage = ImageUtilities.loadImageIcon("org/openide/awt/resources/vista_bigclose_rollover.png", true);
            } else if (isWindowsXPLaF()) {
                writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "b8407aed-3fcb-4136-9016-b4717c25e27f");
                // NOI18N
                bigCloseTabMouseOverImage = ImageUtilities.loadImageIcon("org/openide/awt/resources/xp_bigclose_rollover.png", true);
            } else if (isWindowsLaF()) {
                writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "9ebc8682-f685-4c97-8e9d-df400813e792");
                // NOI18N
                bigCloseTabMouseOverImage = ImageUtilities.loadImageIcon("org/openide/awt/resources/win_bigclose_rollover.png", true);
            } else if (isAquaLaF()) {
                writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "66b25f82-6e09-47cc-8dbc-8819878ea1f0");
                // NOI18N
                bigCloseTabMouseOverImage = ImageUtilities.loadImageIcon("org/openide/awt/resources/mac_bigclose_rollover.png", true);
            } else if (isGTKLaF()) {
                writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "36c3d555-257e-4546-9457-f9bb5ac8d4a3");
                // NOI18N
                bigCloseTabMouseOverImage = ImageUtilities.loadImageIcon("org/openide/awt/resources/gtk_bigclose_rollover.png", true);
            } else {
                writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "25d20e92-e989-4577-b40c-a27843118037");
                // NOI18N
                bigCloseTabMouseOverImage = ImageUtilities.loadImageIcon("org/openide/awt/resources/metal_bigclose_rollover.png", true);
            }
        }
        writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "7247b0e2-74de-461e-afd6-e0c49c8dd3cc");
        return bigCloseTabMouseOverImage;
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
