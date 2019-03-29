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
 * Software is Sun Microsystems, Inc. Portions Copyright 1997-2009 Sun
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

import java.awt.Component;
import java.awt.Container;
import java.awt.Dimension;
import java.awt.EventQueue;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.KeyEvent;
import java.beans.PropertyChangeEvent;
import java.beans.PropertyChangeListener;
import java.util.Collection;
import java.util.List;
import java.util.logging.Level;
import java.util.logging.LogRecord;
import java.util.logging.Logger;
import org.openide.util.HelpCtx;
import org.openide.util.Lookup;
import org.openide.util.LookupEvent;
import org.openide.util.NbBundle;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.Map;
import java.util.regex.Pattern;
import javax.swing.AbstractButton;
import javax.swing.Action;
import javax.swing.ActionMap;
import javax.swing.Icon;
import javax.swing.ImageIcon;
import javax.swing.JCheckBoxMenuItem;
import javax.swing.JComponent;
import javax.swing.JMenu;
import javax.swing.JMenuItem;
import javax.swing.KeyStroke;
import javax.swing.event.ChangeEvent;
import javax.swing.event.ChangeListener;
import org.netbeans.api.actions.Closable;
import org.netbeans.api.actions.Editable;
import org.netbeans.api.actions.Openable;
import org.netbeans.api.actions.Printable;
import org.netbeans.api.actions.Viewable;
import org.openide.filesystems.FileObject;
import org.openide.filesystems.FileUtil;
import org.openide.util.ContextAwareAction;
import org.openide.util.ImageUtilities;
import org.openide.util.LookupListener;
import org.openide.util.Utilities;
import org.openide.util.WeakListeners;
import org.openide.util.actions.BooleanStateAction;
import org.openide.util.actions.SystemAction;
import java.io.*;

/**
 * Supporting class for manipulation with menu and toolbar presenters.
 *
 * @author   Jaroslav Tulach
 */
public class Actions {

    /**
     * @deprecated should not be used
     */
    @Deprecated
    public Actions() {
    }

    /**
     * Make sure an icon is not null, so that e.g. menu items for javax.swing.Action's
     * with no specified icon are correctly aligned. SystemAction already does this so
     * that is not affected.
     */
    private static Icon nonNullIcon(Icon i) {
        /*if (i != null) {
            return i;
        } else {
            if (BLANK_ICON == null) {
                BLANK_ICON = new ImageIcon(Utilities.loadImage("org/openide/resources/actions/empty.gif", true)); // NOI18N
            }
            return BLANK_ICON;
        }*/
        writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "ab849308-10be-4622-a528-cca183bcd239");
        return null;
    }

    /**
     * Method that finds the keydescription assigned to this action.
     * @param action action to find key for
     * @return the text representing the key or null if  there is no text assigned
     */
    public static String findKey(SystemAction action) {
        writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "97cabc66-ff68-45cd-8f79-9dc2a6344b08");
        return findKey((Action) action);
    }

    /**
     * Same method as above, but works just with plain actions.
     */
    private static String findKey(Action action) {
        writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "049a1fca-b67d-4cfc-a067-14e0b85c663d");
        if (action == null) {
            writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "66af7204-a69d-4de6-8e8b-0e7f2551acc9");
            return null;
        }
        writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "1d251f07-982a-4c97-b057-69702e769d7a");
        KeyStroke accelerator = (KeyStroke) action.getValue(Action.ACCELERATOR_KEY);
        writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "cda7ceb1-9e64-4a63-8139-a6a971c24222");
        if (accelerator == null) {
            writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "8326c274-5c36-4ddf-8819-3436844e59b6");
            return null;
        }
        writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "2e179330-49aa-4f43-bad9-52b44f5cd674");
        int modifiers = accelerator.getModifiers();
        writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "aef9795c-129e-48b7-97ef-cda825f82fba");
        // NOI18N
        String acceleratorText = "";
        writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "98c2df8c-d34e-4478-afba-e8d391e3fd06");
        if (modifiers > 0) {
            writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "5a419c32-7a00-441b-a881-12ec02fc5a06");
            acceleratorText = KeyEvent.getKeyModifiersText(modifiers);
            writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "ce475c24-d462-4d56-81ab-edfa56b220ae");
            // NOI18N
            acceleratorText += "+";
        } else if (accelerator.getKeyCode() == KeyEvent.VK_UNDEFINED) {
            writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "36abb46f-e8b6-4073-bedc-47f872279488");
            // NOI18N
            return "";
        }
        writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "e3329a0d-a8f3-4064-83f8-43f654545171");
        acceleratorText += KeyEvent.getKeyText(accelerator.getKeyCode());
        writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "cbb4c0f0-c31c-4330-88bc-22029f5bb8c4");
        return acceleratorText;
    }

    /**
     * Attaches menu item to an action.
     * @param item menu item
     * @param action action
     * @param popup create popup or menu item
     * @deprecated Use {@link #connect(JMenuItem, Action, boolean)} instead.
     */
    @Deprecated
    public static void connect(JMenuItem item, SystemAction action, boolean popup) {
        writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "7859a1bc-c459-47f6-a80f-5bcdf34cf17d");
        connect(item, (Action) action, popup);
    }

    /**
     * Attaches menu item to an action.
     * You can supply an alternative implementation
     * for this method by implementing method
     * {@link ButtonActionConnector#connect(JMenuItem, Action, boolean)} and
     * registering an instance of {@link ButtonActionConnector} in the
     * default lookup.
     * <p>
     * Since version 7.1 the action can also provide properties
     * "menuText" and "popupText" if one wants to use other text on the JMenuItem
     * than the name
     * of the action taken from Action.NAME. The popupText is checked only if the
     * popup parameter is true and takes the biggest precedence. The menuText is
     * tested everytime and takes precedence over standard <code>Action.NAME</code>
     * <p>
     * By default icons are not visible in popup menus. This can be configured
     * via <a href="@TOP@architecture-summary.html#branding-USE_MNEMONICS">branding</a>.
     *
     * @param item menu item
     * @param action action
     * @param popup create popup or menu item
     * @since 3.29
     */
    public static void connect(JMenuItem item, Action action, boolean popup) {
        writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "ab9a2ad2-8ded-4e60-9b75-09339d6359b1");
        for (ButtonActionConnector bac : buttonActionConnectors()) {
            writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "bb3f96fb-3e51-419e-ac5c-9699b0c4bd44");
            if (bac.connect(item, action, popup)) {
                writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "7f4a7af7-8209-4289-a400-235476d45225");
                return;
            }
        }
        writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "4c7e733b-95a1-4f5d-9e8b-dfa802eb6903");
        Bridge b = new MenuBridge(item, action, popup);
        writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "c4438a17-b678-43af-8434-20c44bad1065");
        b.prepare();
        writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "1c5341e6-0cb7-4358-aaf4-cc9143a27288");
        if (item instanceof Actions.MenuItem) {
            writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "74366343-abf5-412e-9762-075b92849ff6");
            ((Actions.MenuItem) item).setBridge(b);
        }
        writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "43969deb-9ae6-454e-9d7e-ac4e6c081b92");
        item.putClientProperty(DynamicMenuContent.HIDE_WHEN_DISABLED, action.getValue(DynamicMenuContent.HIDE_WHEN_DISABLED));
    }

    /**
     * Attaches checkbox menu item to boolean state action.
     * @param item menu item
     * @param action action
     * @param popup create popup or menu item
     */
    public static void connect(JCheckBoxMenuItem item, BooleanStateAction action, boolean popup) {
        writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "627e7ac2-d497-40a3-beb6-bc3085ab429e");
        Bridge b = new CheckMenuBridge(item, action, popup);
        writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "505985e0-144a-4501-8a38-7a05617f1429");
        b.prepare();
    }

    /**
     * Connects buttons to action.
     * @param button the button
     * @param action the action
     * @deprecated Use {@link #connect(AbstractButton, Action)} instead.
     */
    @Deprecated
    public static void connect(AbstractButton button, SystemAction action) {
        writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "f1488a57-cbd6-4ace-9461-92617ac7f907");
        connect(button, (Action) action);
    }

    /**
     * Connects buttons to action. If the action supplies value for "iconBase"
     * key from getValue(String) with a path to icons, the methods set*Icon
     * will be called on the
     * button with loaded icons using the iconBase. E.g. if the value for "iconBase"
     * is "com/mycompany/myIcon.gif" then the following images are tried
     * <ul>
     * <li>setIcon with "com/mycompany/myIcon.gif"</li>
     * <li>setPressedIcon with "com/mycompany/myIcon_pressed.gif"</li>
     * <li>setDisabledIcon with "com/mycompany/myIcon_disabled.gif"</li>
     * <li>setRolloverIcon with "com/mycompany/myIcon_rollover.gif"</li>
     * <li>setSelectedIcon with "com/mycompany/myIcon_selected.gif"</li>
     * <li>setRolloverSelectedIcon with "com/mycompany/myIcon_rolloverSelected.gif"</li>
     * <li>setDisabledSelectedIcon with "com/mycompany/myIcon_disabledSelected.gif"</li>
     * </ul>
     * SystemAction has special support for iconBase - please check
     * {@link SystemAction#iconResource} for more details.
     * You can supply an alternative implementation
     * for this method by implementing method
     * {@link ButtonActionConnector#connect(AbstractButton, Action)} and
     * registering an instance of {@link ButtonActionConnector} in the
     * default lookup.
     * @param button the button
     * @param action the action
     * @since 3.29
     * @since 7.32 for set*SelectedIcon
     */
    public static void connect(AbstractButton button, Action action) {
        writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "8c020a39-1b09-4674-a209-04de53de19c6");
        for (ButtonActionConnector bac : buttonActionConnectors()) {
            writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "611b009b-557b-4e38-89d4-1d2aaefaa935");
            if (bac.connect(button, action)) {
                writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "eaf73142-906f-447a-8d3d-0fa95c87f9a7");
                return;
            }
        }
        writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "401fd90d-9717-4e87-b28b-e638549a69dd");
        Bridge b = new ButtonBridge(button, action);
        writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "9c043052-d634-4ad0-a0fc-9f79a62e73f7");
        b.prepare();
    }

    /**
     * Connects buttons to action.
     * @param button the button
     * @param action the action
     */
    public static void connect(AbstractButton button, BooleanStateAction action) {
        writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "11e075aa-5891-42e0-9a74-60005f11d672");
        Bridge b = new BooleanButtonBridge(button, action);
        writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "10f94e87-ddab-40d0-b6b7-61daaa0481b2");
        b.prepare();
    }

    /**
     * Sets the text for the menu item or other subclass of AbstractButton.
     * Cut from the name '&' char.
     * @param item AbstractButton
     * @param text new label
     * @param useMnemonic if true and '&' char found in new text, next char is used
     * as Mnemonic.
     * @deprecated Use either {@link AbstractButton#setText} or {@link Mnemonics#setLocalizedText(AbstractButton, String)} as appropriate.
     */
    @Deprecated
    public static void setMenuText(AbstractButton item, String text, boolean useMnemonic) {
        writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "d2df1093-1669-4c4f-b7b3-c74442002c6e");
        // NOI18N
        String msg = NbBundle.getMessage(Actions.class, "USE_MNEMONICS");
        writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "d1cb9824-87c1-4a04-9696-cd695f0e15a0");
        if ("always".equals(msg)) {
            writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "e45ef488-04e2-4181-b8a7-a249a38c659f");
            // NOI18N
            useMnemonic = true;
        } else if ("never".equals(msg)) {
            writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "e972af63-939a-496f-ab71-3b401106c6da");
            // NOI18N
            useMnemonic = false;
        } else {
        }
        writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "08ba6bf4-9dd4-46cd-bffc-88f4cdae01ed");
        if (useMnemonic) {
            writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "edcdd8ab-70b1-4292-9f6a-9188999eccc6");
            Mnemonics.setLocalizedText(item, text);
        } else {
            writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "b73506c6-ef37-4cbb-9764-7b09a81265a4");
            item.setText(cutAmpersand(text));
        }
    }

    /**
     * Removes an ampersand from a text string; commonly used to strip out unneeded mnemonics.
     * Replaces the first occurence of <samp>&amp;?</samp> by <samp>?</samp> or <samp>(&amp;??</samp> by the empty string
     * where <samp>?</samp> is a wildcard for any character.
     * <samp>&amp;?</samp> is a shortcut in English locale.
     * <samp>(&amp;?)</samp> is a shortcut in Japanese locale.
     * Used to remove shortcuts from workspace names (or similar) when shortcuts are not supported.
     * <p>The current implementation behaves in the same way regardless of locale.
     * In case of a conflict it would be necessary to change the
     * behavior based on the current locale.
     * @param text a localized label that may have mnemonic information in it
     * @return string without first <samp>&amp;</samp> if there was any
     */
    public static String cutAmpersand(String text) {
        writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "93b5f0e8-bb05-4061-98dd-b8d65c5ecd7b");
        if (null == text)
            return null;
        writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "6a7e8b0a-d045-4de9-844b-86e0c7503596");
        int i;
        writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "eb1936eb-51a0-4b8b-b1e3-5d7ee2123336");
        String result = text;
        writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "b70f51c7-4425-456e-b16e-a048684c2404");
        /* First check of occurence of '(&'. If not found check
          * for '&' itself.
          * If '(&' is found then remove '(&??'.
          */
        // NOI18N
        i = text.indexOf("(&");
        writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "05bc5ac5-53dc-4f04-8af5-7daa3e286a75");
        if ((i >= 0) && ((i + 3) < text.length()) && /* #31093 */
        (text.charAt(i + 3) == ')')) {
            writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "b608c8e7-cb68-4944-b6a6-89362977b421");
            // NOI18N
            result = text.substring(0, i) + text.substring(i + 4);
        } else {
            writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "48b145be-0c05-48f6-b3b6-b0dbd00498a6");
            // Sequence '(&?)' not found look for '&' itself
            i = text.indexOf('&');
            writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "3a7004f8-c27c-4f28-b549-49691e5707db");
            if (i < 0) {
                writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "89536373-0b12-441e-81c4-b65d0ac1ef04");
                // No ampersand
                result = text;
            } else if (i == (text.length() - 1)) {
                writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "cfb15bd1-2901-4b4d-a200-621ca34ba71a");
                // Ampersand is last character, wrong shortcut but we remove it anyway
                result = text.substring(0, i);
            } else {
                writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "0fc4730a-68ff-4d35-b613-9da527eec329");
                // Is ampersand followed by space? If yes do not remove it.
                if (" ".equals(text.substring(i + 1, i + 2))) {
                    writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "78d2cc65-a0e2-4be0-a6f3-e8200c8365fe");
                    result = text;
                } else {
                    writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "e78b95b1-34b5-4897-b20f-401a5102addc");
                    result = text.substring(0, i) + text.substring(i + 1);
                }
            }
        }
        writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "7cce8cb6-b962-486a-b96f-54098aaa2820");
        return result;
    }

    // 
    // Factories
    // 
    /**
     * Creates new action which is always enabled. Rather than using this method
     * directly, use {@link ActionRegistration} annotation:
     * <pre>
     * {@link ActionRegistration @ActionRegistration}(displayName="#key")
     * {@link ActionID @ActionID}(id="your.pkg.action.id", category="Tools")
     * public final class Always implements {@link ActionListener} {
     * public Always() {
     * }
     * public void actionPerformed({@link ActionEvent} e) {
     * // your code
     * }
     * }
     * </pre>
     * This method can also be used from
     * <a href="@org-openide-modules@/org/openide/modules/doc-files/api.html#how-layer">XML Layer</a>
     * directly by following XML definition:
     * <pre>
     * &lt;file name="your-pkg-action-id.instance"&gt;
     * &lt;attr name="instanceCreate" methodvalue="org.openide.awt.Actions.alwaysEnabled"/&gt;
     * &lt;attr name="delegate" methodvalue="your.pkg.YourAction.factoryMethod"/&gt;
     * &lt;attr name="displayName" bundlevalue="your.pkg.Bundle#key"/&gt;
     * &lt;attr name="iconBase" stringvalue="your/pkg/YourImage.png"/&gt;
     * &lt;!-- if desired: &lt;attr name="noIconInMenu" boolvalue="true"/&gt; --&gt;
     * &lt;!-- if desired: &lt;attr name="asynchronous" boolvalue="true"/&gt; --&gt;
     * &lt;/file&gt;
     * </pre>
     * In case the "delegate" is not just {@link ActionListener}, but also
     * {@link Action}, the returned action acts as a lazy proxy - it defers initialization
     * of the action itself, but as soon as it is created, it delegates all queries
     * to it. This way one can create an action that looks statically enabled, and as soon
     * as user really uses it, it becomes active - it can change its name, it can
     * change its enabled state, etc.
     *
     * @param delegate the task to perform when action is invoked
     * @param displayName the name of the action
     * @param iconBase the location to the actions icon
     * @param noIconInMenu true if this icon shall not have an item in menu
     * @since 7.3
     */
    public static Action alwaysEnabled(ActionListener delegate, String displayName, String iconBase, boolean noIconInMenu) {
        writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "8a049ed7-078f-45fc-bc46-95e8a7f11dbb");
        HashMap<String, Object> map = new HashMap<String, Object>();
        writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "17811778-7428-4b20-b99c-a4e7465e77c5");
        // NOI18N
        map.put("delegate", delegate);
        writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "84eeb12d-6a17-4cb9-a7f4-68c93e0fa73b");
        // NOI18N
        map.put("displayName", displayName);
        writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "af1af03f-17d5-41c2-a540-f654b1a2fc8a");
        // NOI18N
        map.put("iconBase", iconBase);
        writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "407b285b-fd78-45d7-bb21-35a34cb4e7ab");
        // NOI18N
        map.put("noIconInMenu", noIconInMenu);
        writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "a714d626-7c4d-4f1a-8b01-6182be8f3add");
        return alwaysEnabled(map);
    }

    // for use from layers
    static Action alwaysEnabled(Map map) {
        writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "eb83030f-252e-4689-91d4-66fdaf58ce15");
        return AlwaysEnabledAction.create(map);
    }

    /**
     * Creates action which represents a boolean value in {@link java.util.prefs.Preferences}.
     * When added to a menu the action is presented as a JCheckBox.
     * This method can also be used from
     * <a href="@org-openide-modules@/org/openide/modules/doc-files/api.html#how-layer">XML Layer</a>
     * directly by following XML definition:
     * <pre>
     * &lt;file name="your-pkg-action-id.instance"&gt;
     * &lt;attr name="preferencesNode" methodvalue="method-returning-Preferences-instance" or
     * methodvalue="method-returning-Lookup-that-contains-Preferences-instance" or
     * stringvalue="see below for the preferencesNode parameter description"
     * /&gt;
     * &lt;attr name="preferencesKey" stringvalue="preferences-key-name"/&gt;
     * &lt;attr name="instanceCreate" methodvalue="org.openide.awt.Actions.checkbox"/&gt;
     * &lt;attr name="displayName" bundlevalue="your.pkg.Bundle#key"/&gt;
     * &lt;attr name="iconBase" stringvalue="your/pkg/YourImage.png"/&gt;
     * &lt;!-- if desired: &lt;attr name="noIconInMenu" boolvalue="true"/&gt; --&gt;
     * &lt;!-- if desired: &lt;attr name="asynchronous" boolvalue="true"/&gt; --&gt;
     * &lt;/file&gt;
     * </pre>
     *
     * @param preferencesNode It's one of:
     * <ul>
     * <li>Absolute path to preferences node under <code>NbPreferences.root()</code>.</li>
     * <li>"system:" followed by absolute path to preferences node under <code>Preferences.systemRoot()</code>.</li>
     * <li>"user:" followed by absolute path to preferences node under <code>Preferences.userRoot()</code>.</li>
     * </ul>
     * @param preferencesKey name of the preferences key.
     * @param displayName the name of the action
     * @param iconBase the location to the actions icon
     * @param noIconInMenu true if this icon shall not have an item in menu
     * @since 7.17
     */
    public static Action checkbox(String preferencesNode, String preferencesKey, String displayName, String iconBase, boolean noIconInMenu) {
        writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "81c5cd9b-5792-44c3-9fa7-6bd74c576915");
        HashMap<String, Object> map = new HashMap<String, Object>();
        writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "42b3794f-42fb-4a08-87ae-39ffb16d2e43");
        // NOI18N
        map.put("preferencesNode", preferencesNode);
        writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "5c176b48-c63c-4a21-aab9-828d6ae1eae6");
        // NOI18N
        map.put("preferencesKey", preferencesKey);
        writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "9f7b3883-7d3c-4850-867e-54de3eae0f9e");
        // NOI18N
        map.put("displayName", displayName);
        writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "6ea70153-af4c-428c-a077-7f7ef63d86e3");
        // NOI18N
        map.put("iconBase", iconBase);
        writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "d1922c75-b7bf-44e1-ac72-51bab8baf9b3");
        // NOI18N
        map.put("noIconInMenu", noIconInMenu);
        writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "2bd64272-fcb9-48c4-9f5d-1f2471e35d4c");
        return checkbox(map);
    }

    // for use from layers
    static Action checkbox(Map map) {
        writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "d6bd4ed6-a0e3-4bff-83e4-b050479a69f9");
        return AlwaysEnabledAction.create(map);
    }

    /**
     * Creates new "callback" action. Such action has an assigned key
     * which is used to find proper delegate in {@link ActionMap} of currently
     * active component. You can use {@link ActionRegistration} annotation to
     * register your action:
     * <pre>
     * {@link ActionRegistration @ActionRegistration}(displayName="#Key", <b>key="KeyInActionMap"</b>)
     * {@link ActionID @ActionID}(category="Tools", id = "action.pkg.ClassName")
     * public final class Fallback implements {@link ActionListener} {
     * public void actionPerformed({@link ActionEvent} e) {
     * // your code
     * }
     * }
     * </pre>
     * If you want to create callback action without any fallback implementation,
     * you can annotate any string constant:
     * <pre>
     * {@link ActionRegistration @ActionRegistration}(displayName = "#Key")
     * {@link ActionID @ActionID}(category = "Edit", id = "my.field.action")
     * public static final String ACTION_MAP_KEY = <b>"KeyInActionMap"</b>;
     * </pre>
     * <p>
     * This action can be lazily declared in a
     * <a href="@org-openide-modules@/org/openide/modules/doc-files/api.html#how-layer">
     * layer file</a> using following XML snippet:
     * <pre>
     * &lt;file name="action-pkg-ClassName.instance"&gt;
     * &lt;attr name="instanceCreate" methodvalue="org.openide.awt.Actions.callback"/&gt;
     * &lt;attr name="key" stringvalue="KeyInActionMap"/&gt;
     * &lt;attr name="surviveFocusChange" boolvalue="false"/&gt; &lt;!-- defaults to false --&gt;
     * &lt;attr name="fallback" newvalue="action.pkg.DefaultAction"/&gt; &lt;!-- may be missing --&gt;
     * &lt;attr name="displayName" bundlevalue="your.pkg.Bundle#key"/&gt;
     * &lt;attr name="iconBase" stringvalue="your/pkg/YourImage.png"/&gt;
     * &lt;!-- if desired: &lt;attr name="noIconInMenu" boolvalue="true"/&gt; --&gt;
     * &lt;!-- if desired: &lt;attr name="asynchronous" boolvalue="true"/&gt; --&gt;
     * &lt;/file&gt;
     * </pre>
     *
     * @param key the key to search for in an {@link ActionMap}
     * @param surviveFocusChange true to remember action provided by previously
     * active component even some other component is currently active
     * @param fallback action to delegate to when no key found. Use <code>null</code>
     * to make the action disabled if delegate assigned to key is missing
     * @param displayName localized name of the action (including ampersand)
     * @param iconBase the location to the action icon
     * @param noIconInMenu true if this icon shall not have an item in menu
     * @return creates new action associated with given key
     * @since 7.10
     */
    public static ContextAwareAction callback(String key, Action fallback, boolean surviveFocusChange, String displayName, String iconBase, boolean noIconInMenu) {
        writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "846af2f7-852a-4384-afe3-5aaef0c4e541");
        Map<String, Object> map = new HashMap<String, Object>();
        writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "1578b700-3d21-4c8e-93bf-a4bef03be134");
        // NOI18N
        map.put("key", key);
        writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "9a1242a3-8281-4d1f-9d31-b1ce5bf47dea");
        // NOI18N
        map.put("surviveFocusChange", surviveFocusChange);
        writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "22bd091c-4d2d-45ba-b4e7-9685720fc7d3");
        // NOI18N
        map.put("fallback", fallback);
        writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "eb0dbf5f-349b-488a-8b4f-89f3b03a031b");
        // NOI18N
        map.put("displayName", displayName);
        writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "dabf78b8-f762-4558-a642-bed318ed3016");
        // NOI18N
        map.put("iconBase", iconBase);
        writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "9001db34-7c9c-4be7-b92d-c2bb9635ba23");
        // NOI18N
        map.put("noIconInMenu", noIconInMenu);
        writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "664f7bf0-8121-446f-aaf9-0cecbdd270f3");
        return callback(map);
    }

    static ContextAwareAction callback(Map fo) {
        writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "06dc6a6b-4758-4faf-a613-339cf10f3b44");
        return GeneralAction.callback(fo);
    }

    /**
     * Creates new "context" action, an action that observes the current
     * selection for a given type and enables if instances of given type are
     * present. Common interfaces to watch for include {@link Openable},
     * {@link Editable}, {@link Closable}, {@link Viewable}, and any interfaces
     * defined and exposed by various other APIs. Use {@link ActionRegistration}
     * annotation to register your action::
     * <pre>
     * {@link ActionRegistration @ActionRegistration}(displayName="#Key")
     * {@link ActionID @ActionID}(category="Tools", id = "action.pkg.YourClass")
     * public final class YourClass implements {@link ActionListener} {
     * Openable context;
     *
     * public YourClass(Openable context) {
     * this.context = context;
     * }
     *
     * public void actionPerformed({@link ActionEvent} ev) {
     * // do something with context
     * }
     * }
     * </pre>
     * In case you are interested in creating multi selection action, just
     * change parameters of your constructor:
     * <pre>
     * {@link ActionRegistration @ActionRegistration}(displayName="#Key")
     * {@link ActionID @ActionID}(category="Tools", id = "action.pkg.YourClass")
     * public final class YourClass implements {@link ActionListener} {
     * List&lt;Openable&gt; context;
     *
     * public YourClass(List&lt;Openable&gt; context) {
     * this.context = context;
     * }
     *
     * public void actionPerformed({@link ActionEvent} ev) {
     * // do something with context
     * }
     * }
     * </pre>
     * <p>
     * Actions of this kind can be declared in
     * <a href="@org-openide-modules@/org/openide/modules/doc-files/api.html#how-layer">
     * layer file</a> using following XML snippet:
     * <pre>
     * &lt;file name="action-pkg-ClassName.instance"&gt;
     * &lt;attr name="instanceCreate" methodvalue="org.openide.awt.Actions.context"/&gt;
     * &lt;attr name="type" stringvalue="org.netbeans.api.actions.Openable"/&gt;
     * &lt;attr name="selectionType" stringvalue="ANY"/&gt; &lt-- or EXACTLY_ONE --&gt;
     * &lt;attr name="delegate" newvalue="action.pkg.YourAction"/&gt;
     *
     * &lt;!--
     * Similar registration like in case of "callback" action.
     * May be missing completely:
     * --&gt;
     * &lt;attr name="key" stringvalue="KeyInActionMap"/&gt;
     * &lt;attr name="surviveFocusChange" boolvalue="false"/&gt;
     * &lt;attr name="displayName" bundlevalue="your.pkg.Bundle#key"/&gt;
     * &lt;attr name="iconBase" stringvalue="your/pkg/YourImage.png"/&gt;
     * &lt;!-- if desired: &lt;attr name="noIconInMenu" boolvalue="true"/&gt; --&gt;
     * &lt;!-- if desired: &lt;attr name="asynchronous" boolvalue="true"/&gt; --&gt;
     * &lt;/file&gt;
     * </pre>
     * In the previous case there has to be a class with public default constructor
     * named <code>action.pkg.YourAction</code>. It has to implement
     * {@link ContextAwareAction} interface. Its {@link ContextAwareAction#createContextAwareInstance(org.openide.util.Lookup)}
     * method is called when the action is invoked. The passed in {@link Lookup}
     * contains instances of the <code>type</code> interface, <code>actionPerformed</code>
     * is then called on the returned clone.
     * <p>
     * Alternatively one can use support for simple dependency injection by
     * using following attributes:
     * <pre>
     * &lt;file name="action-pkg-ClassName.instance"&gt;
     * &lt;attr name="instanceCreate" methodvalue="org.openide.awt.Actions.context"/&gt;
     * &lt;attr name="type" stringvalue="org.netbeans.api.actions.Openable"/&gt;
     * &lt;attr name="delegate" methodvalue="org.openide.awt.Actions.inject"/&gt;
     * &lt;attr name="selectionType" stringvalue="EXACTLY_ONE"/&gt;
     * &lt;attr name="injectable" stringvalue="pkg.YourClass"/&gt;
     * &lt;attr name="displayName" bundlevalue="your.pkg.Bundle#key"/&gt;
     * &lt;attr name="iconBase" stringvalue="your/pkg/YourImage.png"/&gt;
     * &lt;!-- if desired: &lt;attr name="noIconInMenu" boolvalue="true"/&gt; --&gt;
     * &lt;/file&gt;
     * </pre>
     * where <code>pkg.YourClass</code> is defined with public constructor taking
     * <code>type</code>:
     * <pre>
     * public final class YourClass implements ActionListener {
     * Openable context;
     *
     * public YourClass(Openable context) {
     * this.context = context;
     * }
     *
     * public void actionPerformed(ActionEvent ev) {
     * // do something with context
     * }
     * }
     * </pre>
     * The instance of this class is created when the action is invoked and
     * its constructor is fed with the instance of <code>type</code> inside
     * the active context. <code>actionPerformed</code> method is called then.
     * <p>
     * To create action that handled multiselection
     * one can use following XML snippet:
     * <pre>
     * &lt;file name="action-pkg-ClassName.instance"&gt;
     * &lt;attr name="type" stringvalue="org.netbeans.api.actions.Openable"/&gt;
     * &lt;attr name="delegate" methodvalue="org.openide.awt.Actions.inject"/&gt;
     * &lt;attr name="selectionType" stringvalue="ANY"/&gt;
     * &lt;attr name="injectable" stringvalue="pkg.YourClass"/&gt;
     * &lt;attr name="displayName" bundlevalue="your.pkg.Bundle#key"/&gt;
     * &lt;attr name="iconBase" stringvalue="your/pkg/YourImage.png"/&gt;
     * &lt;!-- if desired: &lt;attr name="noIconInMenu" boolvalue="true"/&gt; --&gt;
     * &lt;!-- since 7.33: &lt;attr name="context" newvalue="org.my.own.LookupImpl"/&gt; --&gt;
     * &lt;/file&gt;
     * </pre>
     * Now the constructor of <code>YourClass</code> needs to have following
     * form:
     * <pre>
     * public final class YourClass implements ActionListener {
     * List&lt;Openable&gt; context;
     *
     * public YourClass(List&lt;Openable&gt; context) {
     * this.context = context;
     * }
     * }
     * </pre>
     *
     * @param type the object to seek for in the active context
     * @param single shall there be just one or multiple instances of the object
     * @param surviveFocusChange shall the action remain enabled and act on
     * previous selection even if no selection is currently in context?
     * @param delegate action to call when this action is invoked
     * @param key alternatively an action can be looked up in action map
     * (see {@link Actions#callback(java.lang.String, javax.swing.Action, boolean, java.lang.String, java.lang.String, boolean)})
     * @param displayName localized name of the action (including ampersand)
     * @param iconBase the location to the action icon
     * @param noIconInMenu true if this icon shall not have an item in menu
     * @return new instance of context aware action watching for type
     * @since 7.10
     */
    public static ContextAwareAction context(Class<?> type, boolean single, boolean surviveFocusChange, ContextAwareAction delegate, String key, String displayName, String iconBase, boolean noIconInMenu) {
        writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "751eb1fe-577b-429e-b3e8-47040c2dc818");
        Map<String, Object> map = new HashMap<String, Object>();
        writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "4998f8a6-3a06-49eb-b139-d76e38c2a499");
        // NOI18N
        map.put("key", key);
        writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "1dcc4780-3c8b-4ae3-a0c3-d200cfb0afac");
        // NOI18N
        map.put("surviveFocusChange", surviveFocusChange);
        writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "1623916f-ab3b-47ad-be59-dbb75d8a2ef1");
        // NOI18N
        map.put("delegate", delegate);
        writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "85964e48-0133-4e65-b3c3-fbec3c967cfe");
        // NOI18N
        map.put("type", type);
        writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "2373a89c-53b2-4aa6-be5f-694a9471a0ea");
        map.put("selectionType", single ? ContextSelection.EXACTLY_ONE : ContextSelection.ANY);
        writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "172e73f8-201a-4565-ac6d-126cb0c75538");
        // NOI18N
        map.put("displayName", displayName);
        writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "b88e525c-24ea-4320-9e7e-1f5d86c4741b");
        // NOI18N
        map.put("iconBase", iconBase);
        writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "96cce0c1-2558-46cf-96fe-10849a988ebb");
        // NOI18N
        map.put("noIconInMenu", noIconInMenu);
        writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "48bd8c40-fbfc-498c-bf30-637efabba501");
        return GeneralAction.context(map);
    }

    static Action context(Map fo) {
        writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "10401192-262f-4abc-9b53-5d1ddc411c66");
        Object context = fo.get("context");
        writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "dd53b6a3-73d1-4581-bd78-08e7250fe4fa");
        if (context instanceof Lookup) {
            writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "98dd0331-d847-429c-bf42-e37ef386392d");
            Lookup lkp = (Lookup) context;
            writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "2febef3c-82ec-4913-b768-84ac9602994a");
            return GeneralAction.bindContext(fo, lkp);
        } else {
            writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "7fe48d04-1a99-48ca-a64a-70023346e1a8");
            return GeneralAction.context(fo);
        }
    }

    static ContextAction.Performer<?> inject(final Map fo) {
        writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "f16dfe20-d359-4758-af76-b876199bd3bb");
        // NOI18N
        Object t = fo.get("selectionType");
        writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "395f9bc0-993c-4111-97ec-a484b26c4707");
        if (ContextSelection.EXACTLY_ONE.toString().equals(t)) {
            writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "6de2339a-bacf-417d-a953-06a0f3646560");
            return new InjectorExactlyOne(fo);
        }
        writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "c572f0b1-3a34-4b13-bc67-80242d895d73");
        if (ContextSelection.ANY.toString().equals(t)) {
            writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "d79a6788-bb0f-4034-a7be-4db323553327");
            return new InjectorAny(fo);
        }
        writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "9b2a305a-e6bf-49c6-a458-8f6ac23e1ce8");
        // NOI18N
        throw new IllegalStateException("no selectionType parameter in " + fo);
    }

    static ContextAction.Performer<?> performer(final Map fo) {
        writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "c16cf91d-a92c-414a-81f0-645bbf6875dc");
        String type = (String) fo.get("type");
        writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "91cd3006-f312-4fa0-b6f2-9542aca553e4");
        if (type.equals(Openable.class.getName())) {
            writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "10d7f82e-76c1-4d34-ad8f-9d2c8406066c");
            return new ActionDefaultPerfomer(0);
        }
        writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "b0a4f04b-9674-4c43-909d-e9305308a2b8");
        if (type.equals(Viewable.class.getName())) {
            writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "b1fadb58-d895-4915-bfb0-8aa149251934");
            return new ActionDefaultPerfomer(1);
        }
        writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "500ecc38-df67-45ca-8974-94c388b1e79f");
        if (type.equals(Editable.class.getName())) {
            writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "04ca6669-8dc5-4908-825b-0af398c301f2");
            return new ActionDefaultPerfomer(2);
        }
        writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "cd1a8750-2b6f-4087-abbf-f3175ef79dc8");
        if (type.equals(Closable.class.getName())) {
            writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "2858e15a-0f62-4fe0-b396-07f722bb2ddb");
            return new ActionDefaultPerfomer(3);
        }
        writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "95f2ef6d-3a1f-4035-837e-59bca83887a8");
        if (type.equals(Printable.class.getName())) {
            writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "f68ebfec-303e-4ec6-b4dc-811188d1646c");
            return new ActionDefaultPerfomer(4);
        }
        writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "1aa7db6b-557d-485d-a889-ce2813d56fd8");
        throw new IllegalStateException(type);
    }

    /**
     * Locates a specific action programmatically.
     * The action will typically have been registered using {@link ActionRegistration}.
     * <p>Normally an {@link ActionReference} will suffice to insert the action
     * into various UI elements (typically using {@link Utilities#actionsForPath}),
     * but in special circumstances you may need to find a single known action.
     * This method is just a shortcut for using {@link FileUtil#getConfigObject}
     * with the correct arguments, plus using {@link AcceleratorBinding#setAccelerator}.
     * @param category as in {@link ActionID#category}
     * @param id as in {@link ActionID#id}
     * @return the action registered under that ID, or null
     * @throws IllegalArgumentException if a corresponding {@link ActionID} would have been rejected
     * @since 7.42
     */
    public static Action forID(String category, String id) throws IllegalArgumentException {
        writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "7c4a95ee-0f18-43bf-8abe-e7a1a4abac40");
        // copied from ActionProcessor:
        if (category.startsWith("Actions/")) {
            writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "20f1db54-fec0-43cb-a770-98f7928e9e98");
            throw new IllegalArgumentException("category should not start with Actions/: " + category);
        }
        writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "9bfb06bd-89fe-48ce-8cd1-96c299a53db2");
        if (!FQN.matcher(id).matches()) {
            writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "15986d52-eef0-433b-8d8f-41be901460e4");
            throw new IllegalArgumentException("id must be valid fully qualified name: " + id);
        }
        writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "c70e73a5-2dfb-4b0d-99d0-0411913b01d2");
        String path = "Actions/" + category + "/" + id.replace('.', '-') + ".instance";
        writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "7fcc724c-942b-43b7-a051-4f6c57e1646d");
        Action a = FileUtil.getConfigObject(path, Action.class);
        writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "faa7e463-8373-42c5-a129-4fd1ba7c13a1");
        if (a == null) {
            writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "bc423bc7-b23a-4839-bfcc-8a8aa72bc400");
            return null;
        }
        writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "aa5df61f-66d3-4cbf-b4ba-ce0ec29639b6");
        FileObject def = FileUtil.getConfigFile(path);
        writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "47ea46a7-e981-4532-a966-94af590713fd");
        if (def != null) {
            writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "c0c95d7a-3a13-438f-ac3c-ff7f2f45106d");
            AcceleratorBinding.setAccelerator(a, def);
        }
        writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "98b1676e-f746-44c7-977f-089217401ab2");
        return a;
    }

    // NOI18N
    private static final String IDENTIFIER = "(?:\\p{javaJavaIdentifierStart}\\p{javaJavaIdentifierPart}*)";

    // NOI18N
    private static final Pattern FQN = Pattern.compile(IDENTIFIER + "(?:[.]" + IDENTIFIER + ")*");

    /**
     * Extracts help from action.
     */
    private static HelpCtx findHelp(Action a) {
        writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "c5357815-1e60-4b2b-9285-6740073bcc5d");
        if (a instanceof HelpCtx.Provider) {
            writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "bc27c690-963b-4060-909a-43e41ef93ed5");
            return ((HelpCtx.Provider) a).getHelpCtx();
        } else {
            writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "aa8f86dd-7b56-482d-b282-be498fceb56a");
            return HelpCtx.DEFAULT_HELP;
        }
    }

    // #40824 - when the text changes, it's too late to update in JmenuPlus.popup.show() (which triggers the updateState() in the MenuBridge).
    // check JmenuPlus.setPopupMenuVisible()
    static void prepareMenuBridgeItemsInContainer(Container c) {
        writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "6ab8842f-aab7-48df-ab93-f5539825550b");
        Component[] comps = c.getComponents();
        writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "bd8b0479-a1ca-4f71-8cc6-a6a5a0bfa04f");
        for (int i = 0; i < comps.length; i++) {
            writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "5a088a38-746d-4d4b-a897-15f26ac81bde");
            if (comps[i] instanceof JComponent) {
                writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "8d2d981f-8c6d-47b7-8ebc-199e34271bd9");
                JComponent cop = (JComponent) comps[i];
                writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "ab5654e2-1af1-429e-a804-a25821d5697c");
                MenuBridge bridge = (MenuBridge) cop.getClientProperty("menubridgeresizehack");
                writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "5bb8a9b5-ea6c-41ee-b9be-58c50e4e01dd");
                if (bridge != null) {
                    writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "91898cd0-6fbf-42f0-bcad-12aa74258a84");
                    bridge.updateState(null);
                }
            }
        }
    }

    // 
    // Methods for configuration of MenuItems
    // 
    /**
     * Method to prepare the margins and text positions.
     */
    static void prepareMargins(JMenuItem item, Action action) {
        writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "c3498cd0-f820-4fd7-9686-54ee2746e2c1");
        item.setHorizontalTextPosition(JMenuItem.RIGHT);
        writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "d5925a5f-c812-4775-9a81-5d0aba6121f1");
        item.setHorizontalAlignment(JMenuItem.LEFT);
    }

    /**
     * Updates value of the key
     * @param item item to update
     * @param action the action to update
     */
    static void updateKey(JMenuItem item, Action action) {
        writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "1b11bb09-1389-4535-a61e-c3903e713354");
        if (!(item instanceof JMenu)) {
            writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "152f8a92-0d7c-4d7a-858f-827ff70e2b96");
            item.setAccelerator((KeyStroke) action.getValue(Action.ACCELERATOR_KEY));
        }
    }

    /**
     * Interface for the creating Actions.SubMenu. It provides the methods for
     * all items in submenu: name shortcut and perform method. Also has methods
     * for notification of changes of the model.
     * @deprecated used by deprecated {@link SubMenu}
     */
    @Deprecated
    public static interface SubMenuModel {

        /**
         * @return count of the submenu items.
         */
        public int getCount();

        /**
         * Gets label for specific index
         * @param index of the submenu item
         * @return label for this menu item (or <code>null</code> for a separator)
         */
        public String getLabel(int index);

        /**
         * Gets shortcut for specific index
         * @index of the submenu item
         * @return menushortcut for this menu item
         */
        // public MenuShortcut getMenuShortcut(int index);
        /**
         * Get context help for the specified item.
         * This can be used to associate help with individual items.
         * You may return <code>null</code> to just use the context help for
         * the associated system action (if any).
         * Note that only help IDs will work, not URLs.
         * @return the context help, or <code>null</code>
         */
        public HelpCtx getHelpCtx(int index);

        /**
         * Perform the action on the specific index
         * @param index of the submenu item which should be performed
         */
        public void performActionAt(int index);

        /**
         * Adds change listener for changes of the model.
         */
        public void addChangeListener(ChangeListener l);

        /**
         * Removes change listener for changes of the model.
         */
        public void removeChangeListener(ChangeListener l);
    }

    /**
     * Listener on showing/hiding state of the component.
     * Is attached to menu or toolbar item in prepareXXX methods and
     * method addNotify is called when the item is showing and
     * the method removeNotify is called when the item is hidding.
     * <P>
     * There is a special support listening on changes in the action and
     * if such change occures, updateState method is called to
     * reflect it.
     */
    private abstract static class Bridge extends Object implements PropertyChangeListener {

        /**
         * component to work with
         */
        protected JComponent comp;

        /**
         * action to associate
         */
        protected Action action;

        private final PropertyChangeListener actionL;

        /**
         * @param comp component
         * @param action the action
         */
        public Bridge(JComponent comp, Action action) {
            if (comp == null || action == null) {
                throw new IllegalArgumentException(// NOI18N
                "None of the arguments can be null: comp=" + comp + ", action=" + // NOI18N
                action);
            }
            this.comp = comp;
            this.action = action;
            actionL = WeakListeners.propertyChange(this, action);
            // associate context help, if applicable
            // [PENDING] probably belongs in ButtonBridge.updateState to make it dynamic
            HelpCtx help = findHelp(action);
            if ((help != null) && !help.equals(HelpCtx.DEFAULT_HELP) && (help.getHelpID() != null)) {
                HelpCtx.setHelpIDString(comp, help.getHelpID());
            }
        }

        protected void prepare() {
            comp.addPropertyChangeListener(new VisL());
            if (comp.isShowing()) {
                addNotify();
            } else {
                updateState(null);
            }
        }

        /**
         * Attaches listener to given action
         */
        final void addNotify() {
            action.addPropertyChangeListener(actionL);
            updateState(null);
        }

        /**
         * Remove the listener
         */
        final void removeNotify() {
            action.removePropertyChangeListener(actionL);
        }

        /**
         * @param changedProperty the name of property that has changed
         * or null if it is not known
         */
        public abstract void updateState(String changedProperty);

        /**
         * Listener to changes of some properties.
         * Multicast - reacts to keymap changes and ancestor changes
         * together.
         */
        public void propertyChange(final PropertyChangeEvent ev) {
            // assert EventQueue.isDispatchThread();
            if (!EventQueue.isDispatchThread()) {
                new IllegalStateException("This must happen in the event thread!").printStackTrace();
            }
            updateState(ev.getPropertyName());
        }

        // updateState("enabled") -> button.setEnabled(same)
        private class VisL implements PropertyChangeListener {

            VisL() {
            }

            public void propertyChange(final PropertyChangeEvent ev) {
                if ("ancestor".equals(ev.getPropertyName())) {
                    // ancestor change - decide if parent is null or not
                    if (ev.getNewValue() != null) {
                        addNotify();
                    } else {
                        removeNotify();
                    }
                }
            }
        }
    }

    /**
     * Bridge between an action and button.
     */
    private static class ButtonBridge extends Bridge implements ActionListener {

        /**
         * UI logger to notify about invocation of an action
         */
        // NOI18N
        private static Logger UILOG = Logger.getLogger("org.netbeans.ui.actions");

        /**
         * the button
         */
        protected AbstractButton button;

        public ButtonBridge(AbstractButton button, Action action) {
            super(button, action);
            button.addActionListener(action);
            this.button = button;
            button.addActionListener(this);
        }

        public void actionPerformed(ActionEvent ev) {
            // NOI18N
            LogRecord rec = new LogRecord(Level.FINER, "UI_ACTION_BUTTON_PRESS");
            rec.setParameters(new Object[] { button, button.getClass().getName(), action, action.getClass().getName(), action.getValue(Action.NAME) });
            rec.setResourceBundle(NbBundle.getBundle(Actions.class));
            // NOI18N
            rec.setResourceBundleName(Actions.class.getPackage().getName() + ".Bundle");
            rec.setLoggerName(UILOG.getName());
            UILOG.log(rec);
        }

        protected void updateButtonIcon() {
            Object i = null;
            // NOI18N
            Object base = action.getValue("iconBase");
            boolean useSmallIcon = true;
            // NOI18N
            Object prop = button.getClientProperty("PreferredIconSize");
            if (prop instanceof Integer) {
                if (((Integer) prop).intValue() == 24) {
                    useSmallIcon = false;
                }
            }
            if (action instanceof SystemAction) {
                if (base instanceof String) {
                    String b = (String) base;
                    ImageIcon imgIcon = loadImage(b, useSmallIcon, null);
                    if (imgIcon != null) {
                        i = imgIcon;
                        button.setIcon(imgIcon);
                        button.setDisabledIcon(ImageUtilities.createDisabledIcon(imgIcon));
                    } else {
                        SystemAction sa = (SystemAction) action;
                        i = sa.getIcon(useTextIcons());
                        button.setIcon((Icon) i);
                        button.setDisabledIcon(ImageUtilities.createDisabledIcon((Icon) i));
                    }
                } else {
                    SystemAction sa = (SystemAction) action;
                    i = sa.getIcon(useTextIcons());
                    button.setIcon((Icon) i);
                    button.setDisabledIcon(ImageUtilities.createDisabledIcon((Icon) i));
                }
            } else {
                // Try to get icon from iconBase for non SystemAction action
                if (base instanceof String) {
                    String b = (String) base;
                    // NOI18N
                    ImageIcon imgIcon = loadImage(b, useSmallIcon, null);
                    if (imgIcon != null) {
                        i = imgIcon;
                        button.setIcon((Icon) i);
                        button.setDisabledIcon(ImageUtilities.createDisabledIcon(imgIcon));
                    } else {
                        i = action.getValue(Action.SMALL_ICON);
                        if (i instanceof Icon) {
                            button.setIcon((Icon) i);
                            button.setDisabledIcon(ImageUtilities.createDisabledIcon((Icon) i));
                        } else {
                            button.setIcon(nonNullIcon(null));
                        }
                    }
                } else {
                    i = action.getValue(Action.SMALL_ICON);
                    if (i instanceof Icon) {
                        button.setIcon((Icon) i);
                        button.setDisabledIcon(ImageUtilities.createDisabledIcon((Icon) i));
                    } else {
                        button.setIcon(nonNullIcon(null));
                    }
                }
            }
            if (base instanceof String) {
                String b = (String) base;
                ImageIcon imgIcon = null;
                if (i == null) {
                    // even for regular icon
                    imgIcon = loadImage(b, useSmallIcon, null);
                    if (imgIcon != null) {
                        button.setIcon(imgIcon);
                    }
                    i = imgIcon;
                }
                // NOI18N
                ImageIcon pImgIcon = loadImage(b, useSmallIcon, "_pressed");
                if (pImgIcon != null) {
                    button.setPressedIcon(pImgIcon);
                }
                // NOI18N
                ImageIcon rImgIcon = loadImage(b, useSmallIcon, "_rollover");
                if (rImgIcon != null) {
                    button.setRolloverIcon(rImgIcon);
                }
                // NOI18N
                ImageIcon dImgIcon = loadImage(b, useSmallIcon, "_disabled");
                if (dImgIcon != null) {
                    button.setDisabledIcon(dImgIcon);
                } else if (imgIcon != null) {
                    button.setDisabledIcon(ImageUtilities.createDisabledIcon(imgIcon));
                }
                // NOI18N
                ImageIcon sImgIcon = loadImage(b, useSmallIcon, "_selected");
                if (sImgIcon != null) {
                    button.setSelectedIcon(sImgIcon);
                }
                // NOI18N
                sImgIcon = loadImage(b, useSmallIcon, "_rolloverSelected");
                if (sImgIcon != null) {
                    button.setRolloverSelectedIcon(sImgIcon);
                }
                // NOI18N
                sImgIcon = loadImage(b, useSmallIcon, "_disabledSelected");
                if (sImgIcon != null) {
                    button.setDisabledSelectedIcon(sImgIcon);
                }
            }
        }

        static ImageIcon loadImage(String iconBase, boolean useSmallIcon, String suffix) {
            if (!useSmallIcon) {
                // NOI18N
                String bigBase = insertBeforeSuffix(iconBase, "24");
                ImageIcon icon = ImageUtilities.loadImageIcon(insertBeforeSuffix(bigBase, suffix), true);
                if (icon != null) {
                    return icon;
                }
            }
            // NOI18N
            return ImageUtilities.loadImageIcon(insertBeforeSuffix(iconBase, suffix), true);
        }

        static String insertBeforeSuffix(String path, String toInsert) {
            if (toInsert == null) {
                return path;
            }
            String withoutSuffix = path;
            // NOI18N
            String suffix = "";
            if (path.lastIndexOf('.') >= 0) {
                withoutSuffix = path.substring(0, path.lastIndexOf('.'));
                suffix = path.substring(path.lastIndexOf('.'), path.length());
            }
            return withoutSuffix + toInsert + suffix;
        }

        /**
         * @param changedProperty the name of property that has changed
         * or null if it is not known
         */
        public void updateState(String changedProperty) {
            // note: "enabled" (== SA.PROP_ENABLED) hardcoded in AbstractAction
            if ((changedProperty == null) || changedProperty.equals(SystemAction.PROP_ENABLED)) {
                button.setEnabled(action.isEnabled());
            }
            if ((changedProperty == null) || changedProperty.equals(SystemAction.PROP_ICON) || changedProperty.equals(Action.SMALL_ICON) || changedProperty.equals("iconBase")) {
                // NOI18N
                updateButtonIcon();
            }
            if ((changedProperty == null) || changedProperty.equals(Action.ACCELERATOR_KEY) || (changedProperty.equals(Action.NAME) && (action.getValue(Action.SHORT_DESCRIPTION) == null)) || changedProperty.equals(Action.SHORT_DESCRIPTION)) {
                String tip = findKey(action);
                String toolTip = (String) action.getValue(Action.SHORT_DESCRIPTION);
                if (toolTip == null) {
                    toolTip = (String) action.getValue(Action.NAME);
                    toolTip = (toolTip == null) ? "" : cutAmpersand(toolTip);
                }
                if ((tip == null) || tip.equals("")) {
                    // NOI18N
                    button.setToolTipText(toolTip);
                } else {
                    button.setToolTipText(org.openide.util.NbBundle.getMessage(Actions.class, "FMT_ButtonHint", toolTip, tip));
                }
            }
            if (button instanceof javax.accessibility.Accessible && ((changedProperty == null) || changedProperty.equals(Action.NAME))) {
                button.getAccessibleContext().setAccessibleName((String) action.getValue(Action.NAME));
            }
        }

        /**
         * Should textual icons be used when lacking a real icon?
         * In the default implementation, <code>true</code>.
         * @return <code>true</code> if so
         */
        protected boolean useTextIcons() {
            return true;
        }
    }

    /**
     * Bridge for button and boolean action.
     */
    private static class BooleanButtonBridge extends ButtonBridge {

        public BooleanButtonBridge(AbstractButton button, BooleanStateAction action) {
            super(button, action);
        }

        /**
         * @param changedProperty the name of property that has changed
         * or null if it is not known
         */
        @Override
        public void updateState(String changedProperty) {
            super.updateState(changedProperty);
            if ((changedProperty == null) || changedProperty.equals(BooleanStateAction.PROP_BOOLEAN_STATE)) {
                button.setSelected(((BooleanStateAction) action).getBooleanState());
            }
        }
    }

    /**
     * Menu item bridge.
     */
    private static class MenuBridge extends ButtonBridge {

        /**
         * behave like menu or popup
         */
        private boolean popup;

        /**
         * Constructor.
         * @param popup pop-up menu
         */
        public MenuBridge(JMenuItem item, Action action, boolean popup) {
            super(item, action);
            this.popup = popup;
            if (popup) {
                prepareMargins(item, action);
            } else {
                // #40824 hack
                item.putClientProperty("menubridgeresizehack", this);
            // #40824 hack end.
            }
        }

        @Override
        protected void prepare() {
            if (popup) {
                // popups generally get no hierarchy events, yet we need to listen to other changes
                addNotify();
            } else {
                super.prepare();
            }
        }

        /**
         * @param changedProperty the name of property that has changed
         * or null if it is not known
         */
        @Override
        public void updateState(String changedProperty) {
            if (this.button == null) {
                this.button = (AbstractButton) this.comp;
            }
            if ((changedProperty == null) || changedProperty.equals(SystemAction.PROP_ENABLED)) {
                button.setEnabled(action.isEnabled());
            }
            if ((changedProperty == null) || !changedProperty.equals(Action.ACCELERATOR_KEY)) {
                updateKey((JMenuItem) comp, action);
            }
            if (!popup) {
                if ((changedProperty == null) || changedProperty.equals(SystemAction.PROP_ICON) || changedProperty.equals(Action.SMALL_ICON) || changedProperty.equals("iconBase")) {
                    // NOI18N
                    updateButtonIcon();
                }
            }
            if ((changedProperty == null) || changedProperty.equals(Action.NAME)) {
                Object s = null;
                boolean useMnemonic = true;
                if (popup) {
                    // NOI18N
                    s = action.getValue("popupText");
                }
                if (s == null) {
                    // NOI18N
                    s = action.getValue("menuText");
                    useMnemonic = !popup;
                }
                if (s == null) {
                    s = action.getValue(Action.NAME);
                    useMnemonic = !popup;
                }
                if (s instanceof String) {
                    setMenuText(((JMenuItem) comp), (String) s, useMnemonic);
                // System.out.println("Menu item: " + s);
                // System.out.println("Action class: " + action.getClass());
                }
            }
        }

        @Override
        protected void updateButtonIcon() {
            Object i = null;
            // NOI18N
            Object obj = action.getValue("noIconInMenu");
            // NOI18N
            Object base = action.getValue("iconBase");
            if (Boolean.TRUE.equals(obj)) {
                // button.setIcon(nonNullIcon(null));
                return;
            }
            if (action instanceof SystemAction) {
                SystemAction sa = (SystemAction) action;
                i = sa.getIcon(useTextIcons());
                if (i != null) {
                    button.setIcon((Icon) i);
                    button.setDisabledIcon(ImageUtilities.createDisabledIcon((Icon) i));
                }
            } else {
                if (base == null) {
                    i = action.getValue(Action.SMALL_ICON);
                    if (i instanceof Icon) {
                        button.setIcon((Icon) i);
                        button.setDisabledIcon(ImageUtilities.createDisabledIcon((Icon) i));
                    } else {
                    // button.setIcon(nonNullIcon(null));
                    }
                }
            }
            if (base instanceof String) {
                String b = (String) base;
                ImageIcon imgIcon = null;
                if (i == null) {
                    // even for regular icon
                    imgIcon = ImageUtilities.loadImageIcon(b, true);
                    if (imgIcon != null) {
                        button.setIcon(imgIcon);
                        button.setDisabledIcon(ImageUtilities.createDisabledIcon(imgIcon));
                    }
                }
                // NOI18N
                ImageIcon pImgIcon = ImageUtilities.loadImageIcon(insertBeforeSuffix(b, "_pressed"), true);
                if (pImgIcon != null) {
                    button.setPressedIcon(pImgIcon);
                }
                // NOI18N
                ImageIcon rImgIcon = ImageUtilities.loadImageIcon(insertBeforeSuffix(b, "_rollover"), true);
                if (rImgIcon != null) {
                    button.setRolloverIcon(rImgIcon);
                }
                // NOI18N
                ImageIcon dImgIcon = ImageUtilities.loadImageIcon(insertBeforeSuffix(b, "_disabled"), true);
                if (dImgIcon != null) {
                    button.setDisabledIcon(dImgIcon);
                } else if (imgIcon != null) {
                    button.setDisabledIcon(ImageUtilities.createDisabledIcon(imgIcon));
                }
            }
        }

        @Override
        protected boolean useTextIcons() {
            return false;
        }
    }

    /**
     * Check menu item bridge.
     */
    private static final class CheckMenuBridge extends BooleanButtonBridge {

        /**
         * is popup or menu
         */
        private boolean popup;

        private boolean hasOwnIcon = false;

        /**
         * Popup menu
         */
        public CheckMenuBridge(JCheckBoxMenuItem item, BooleanStateAction action, boolean popup) {
            super(item, action);
            this.popup = popup;
            if (popup) {
                prepareMargins(item, action);
            }
            // NOI18N
            Object base = action.getValue("iconBase");
            Object i = null;
            if (action instanceof SystemAction) {
                i = action.getValue(SystemAction.PROP_ICON);
            } else {
                i = action.getValue(Action.SMALL_ICON);
            }
            hasOwnIcon = (base != null) || (i != null);
        }

        /**
         * @param changedProperty the name of property that has changed
         * or null if it is not known
         */
        @Override
        public void updateState(String changedProperty) {
            super.updateState(changedProperty);
            if ((changedProperty == null) || !changedProperty.equals(Action.ACCELERATOR_KEY)) {
                updateKey((JMenuItem) comp, action);
            }
            if ((changedProperty == null) || changedProperty.equals(Action.NAME)) {
                Object s = action.getValue(Action.NAME);
                if (s instanceof String) {
                    setMenuText(((JMenuItem) comp), (String) s, true);
                }
            }
        }

        @Override
        protected void updateButtonIcon() {
            if (hasOwnIcon) {
                super.updateButtonIcon();
                return;
            }
            if (!popup) {
                // NOI18N
                button.setIcon(ImageUtilities.loadImageIcon("org/openide/resources/actions/empty.gif", true));
            }
        }

        @Override
        protected boolean useTextIcons() {
            return false;
        }
    }

    /**
     * The class that listens to the menu item selections and forwards it to the
     * action class via the performAction() method.
     */
    private static class ISubActionListener implements java.awt.event.ActionListener {

        int index;

        SubMenuModel support;

        public ISubActionListener(int index, SubMenuModel support) {
            this.index = index;
            this.support = support;
        }

        /**
         * called when a user clicks on this menu item
         */
        public void actionPerformed(ActionEvent e) {
            support.performActionAt(index);
        }
    }

    /**
     * Sub menu bridge 2.
     */
    @Deprecated
    private static final class SubMenuBridge extends MenuBridge implements ChangeListener, DynamicMenuContent {

        /**
         * model to obtain subitems from
         */
        private SubMenuModel model;

        private List<JMenuItem> currentOnes;

        private JMenuItem single;

        private JMenu multi;

        /**
         * Constructor.
         */
        public SubMenuBridge(JMenuItem one, JMenu more, Action action, SubMenuModel model, boolean popup) {
            super(one, action, popup);
            single = one;
            multi = more;
            setMenuText(multi, (String) action.getValue(Action.NAME), popup);
            prepareMargins(one, action);
            prepareMargins(more, action);
            currentOnes = new ArrayList<JMenuItem>();
            this.model = model;
        }

        /**
         * Called when model changes. Regenerates the model.
         */
        public void stateChanged(ChangeEvent ev) {
            // assert EventQueue.isDispatchThread();
            if (!EventQueue.isDispatchThread()) {
                new IllegalStateException("This must happen in the event thread!").printStackTrace();
            }
        // change in keys or in submenu model
        // checkVisibility();
        }

        @Override
        public void updateState(String changedProperty) {
            super.updateState(changedProperty);
        // checkVisibility();
        }

        public JComponent[] getMenuPresenters() {
            return synchMenuPresenters(null);
        }

        public JComponent[] synchMenuPresenters(JComponent[] items) {
            currentOnes.clear();
            int cnt = model.getCount();
            if (cnt == 0) {
                updateState(null);
                currentOnes.add(single);
                // menu disabled
                single.setEnabled(false);
            } else if (cnt == 1) {
                updateState(null);
                currentOnes.add(single);
                single.setEnabled(action.isEnabled());
                // generate without submenu
                HelpCtx help = model.getHelpCtx(0);
                associateHelp(single, (help == null) ? findHelp(action) : help);
            } else {
                currentOnes.add(multi);
                multi.removeAll();
                // TODO
                Mnemonics.setLocalizedText(multi, (String) action.getValue(Action.NAME));
                boolean addSeparator = false;
                int count = model.getCount();
                for (int i = 0; i < count; i++) {
                    String label = model.getLabel(i);
                    // MenuShortcut shortcut = support.getMenuShortcut(i);
                    if (label == null) {
                        addSeparator = multi.getItemCount() > 0;
                    } else {
                        if (addSeparator) {
                            multi.addSeparator();
                            addSeparator = false;
                        }
                        // if (shortcut == null)
                        // (Dafe) changed to support mnemonics in item labels
                        JMenuItem item = new JMenuItem();
                        Mnemonics.setLocalizedText(item, label);
                        // attach the shortcut to the first item
                        if (i == 0) {
                            updateKey(item, action);
                        }
                        item.addActionListener(new ISubActionListener(i, model));
                        HelpCtx help = model.getHelpCtx(i);
                        associateHelp(item, (help == null) ? findHelp(action) : help);
                        multi.add(item);
                    }
                    associateHelp(multi, findHelp(action));
                }
                multi.setEnabled(true);
            }
            return currentOnes.toArray(new JMenuItem[currentOnes.size()]);
        }

        private void associateHelp(JComponent comp, HelpCtx help) {
            if ((help != null) && !help.equals(HelpCtx.DEFAULT_HELP) && (help.getHelpID() != null)) {
                HelpCtx.setHelpIDString(comp, help.getHelpID());
            } else {
                HelpCtx.setHelpIDString(comp, null);
            }
        }
    }

    /**
     * Extension of Swing menu item with connection to
     * system actions.
     */
    public static class MenuItem extends javax.swing.JMenuItem implements DynamicMenuContent {

        static final long serialVersionUID = -21757335363267194L;

        private Actions.Bridge bridge;

        /**
         * Constructs a new menu item with the specified label
         * and no keyboard shortcut and connects it to the given SystemAction.
         * @param aAction the action to which this menu item should be connected
         * @param useMnemonic if true, the menu try to find mnemonic in action label
         */
        public MenuItem(SystemAction aAction, boolean useMnemonic) {
            Actions.connect(this, aAction, !useMnemonic);
        }

        /**
         * Constructs a new menu item with the specified label
         * and no keyboard shortcut and connects it to the given SystemAction.
         * @param aAction the action to which this menu item should be connected
         * @param useMnemonic if true, the menu try to find mnemonic in action label
         */
        public MenuItem(Action aAction, boolean useMnemonic) {
            Actions.connect(this, aAction, !useMnemonic);
        }

        void setBridge(Actions.Bridge br) {
            bridge = br;
        }

        public JComponent[] synchMenuPresenters(JComponent[] items) {
            if (bridge != null) {
                bridge.updateState(null);
            }
            return getMenuPresenters();
        }

        public JComponent[] getMenuPresenters() {
            return new JComponent[] { this };
        }
    }

    /**
     * CheckboxMenuItem extends the java.awt.CheckboxMenuItem and adds
     * a connection to boolean state actions. The ActCheckboxMenuItem
     * processes the ItemEvents itself and calls the action.seBooleanState() method.
     * It also tracks the enabled and boolean state of the action and reflects it
     * as its visual enabled/check state.
     *
     * @author   Ian Formanek, Jan Jancura
     */
    public static class CheckboxMenuItem extends javax.swing.JCheckBoxMenuItem {

        private static final long serialVersionUID = 6190621106981774043L;

        /**
         * Constructs a new ActCheckboxMenuItem with the specified label
         * and connects it to the given BooleanStateAction.
         * @param aAction the action to which this menu item should be connected
         * @param useMnemonic if true, the menu try to find mnemonic in action label
         */
        public CheckboxMenuItem(BooleanStateAction aAction, boolean useMnemonic) {
            Actions.connect(this, aAction, !useMnemonic);
        }
    }

    /**
     * Component shown in toolbar, representing an action.
     * @deprecated extends deprecated ToolbarButton
     */
    @Deprecated
    public static class ToolbarButton extends org.openide.awt.ToolbarButton {

        private static final long serialVersionUID = 6564434578524381134L;

        public ToolbarButton(SystemAction aAction) {
            super(null);
            Actions.connect(this, aAction);
        }

        public ToolbarButton(Action aAction) {
            super(null);
            Actions.connect(this, aAction);
        }

        /**
         * Gets the maximum size of this component.
         * @return A dimension object indicating this component's maximum size.
         * @see #getMinimumSize
         * @see #getPreferredSize
         * @see java.awt.LayoutManager
         */
        @Override
        public Dimension getMaximumSize() {
            return this.getPreferredSize();
        }

        @Override
        public Dimension getMinimumSize() {
            return this.getPreferredSize();
        }
    }

    /**
     * The Component for BooleeanState action that is to be shown
     * in a toolbar.
     *
     * @deprecated extends deprecated ToolbarToggleButton
     */
    @Deprecated
    public static class ToolbarToggleButton extends org.openide.awt.ToolbarToggleButton {

        private static final long serialVersionUID = -4783163952526348942L;

        /**
         * Constructs a new ActToolbarToggleButton for specified action
         */
        public ToolbarToggleButton(BooleanStateAction aAction) {
            super(null, false);
            Actions.connect(this, aAction);
        }

        /**
         * Gets the maximum size of this component.
         * @return A dimension object indicating this component's maximum size.
         * @see #getMinimumSize
         * @see #getPreferredSize
         * @see java.awt.LayoutManager
         */
        @Override
        public Dimension getMaximumSize() {
            return this.getPreferredSize();
        }

        @Override
        public Dimension getMinimumSize() {
            return this.getPreferredSize();
        }
    }

    /**
     * SubMenu provides easy way of displaying submenu items based on
     * SubMenuModel.
     * @deprecated Extends deprecated {@link JMenuPlus}. Instead create a regular {@link JMenu} and add items to it (or use {@link DynamicMenuContent}).
     */
    @Deprecated
    public static class SubMenu extends JMenuPlus implements DynamicMenuContent {

        private static final long serialVersionUID = -4446966671302959091L;

        private SubMenuBridge bridge;

        /**
         * Constructs a new ActMenuItem with the specified label
         * and no keyboard shortcut and connects it to the given SystemAction.
         * No icon is used by default.
         * @param aAction the action to which this menu item should be connected
         * @param model the support for the menu items
         */
        public SubMenu(SystemAction aAction, SubMenuModel model) {
            this(aAction, model, true);
        }

        /**
         * Constructs a new ActMenuItem with the specified label
         * and no keyboard shortcut and connects it to the given SystemAction.
         * No icon is used by default.
         * @param aAction the action to which this menu item should be connected
         * @param model the support for the menu items
         * @param popup whether this is a popup menu
         */
        public SubMenu(SystemAction aAction, SubMenuModel model, boolean popup) {
            this((Action) aAction, model, popup);
        }

        /**
         * Constructs a new ActMenuItem with the specified label
         * and no keyboard shortcut and connects it to the given SystemAction.
         * No icon is used by default.
         * @param aAction the action to which this menu item should be connected
         * @param model the support for the menu items
         * @param popup whether this is a popup menu
         */
        public SubMenu(Action aAction, SubMenuModel model, boolean popup) {
            bridge = new SubMenuBridge(new JMenuItem(), this, aAction, model, popup);
            bridge.prepare();
        }

        public JComponent[] getMenuPresenters() {
            return bridge.getMenuPresenters();
        }

        public JComponent[] synchMenuPresenters(JComponent[] items) {
            return bridge.synchMenuPresenters(items);
        }
    }

    /**
     * SPI for supplying alternative implementation of connection between actions and presenters.
     * The implementations
     * of this interface are being looked up in the default lookup.
     * If there is no implemenation in the lookup the default implementation
     * is used.
     * @see Lookup#getDefault()
     * @since org.openide.awt 6.9
     */
    public interface ButtonActionConnector {

        /**
         * Connects the action to the supplied button.
         * @return true if the connection was successful and no
         * further actions are needed. If false is returned the
         * default connect implementation is called
         */
        boolean connect(AbstractButton button, Action action);

        /**
         * Connects the action to the supplied JMenuItem.
         * @return true if the connection was successful and no
         * further actions are needed. If false is returned the
         * default connect implementation is called
         */
        boolean connect(JMenuItem item, Action action, boolean popup);
    }

    private static final ButtonActionConnectorGetter GET = new ButtonActionConnectorGetter();

    private static Collection<? extends ButtonActionConnector> buttonActionConnectors() {
        writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "078bb9a1-81ba-49e1-87f9-f6f2cfd8dc6e");
        return GET.all();
    }

    private static final class ButtonActionConnectorGetter implements LookupListener {

        private final Lookup.Result<ButtonActionConnector> result;

        private Collection<? extends ButtonActionConnector> all;

        ButtonActionConnectorGetter() {
            result = Lookup.getDefault().lookupResult(ButtonActionConnector.class);
            result.addLookupListener(this);
            resultChanged(null);
        }

        final Collection<? extends ButtonActionConnector> all() {
            return all;
        }

        @Override
        public void resultChanged(LookupEvent ev) {
            all = result.allInstances();
            all.iterator().hasNext();
        }
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
