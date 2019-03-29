/*
 * DO NOT ALTER OR REMOVE COPYRIGHT NOTICES OR THIS HEADER.
 *
 * Copyright 2012 Oracle and/or its affiliates. All rights reserved.
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
 * Contributor(s):
 *
 * Portions Copyrighted 2012 Sun Microsystems, Inc.
 */
package org.openide.awt;

import java.awt.AWTEvent;
import java.awt.Color;
import java.awt.Component;
import java.awt.Dimension;
import java.awt.Insets;
import java.awt.Toolkit;
import java.awt.event.AWTEventListener;
import java.awt.event.ComponentAdapter;
import java.awt.event.ComponentEvent;
import java.awt.event.ComponentListener;
import java.awt.event.MouseAdapter;
import java.awt.event.MouseEvent;
import javax.swing.BorderFactory;
import javax.swing.JButton;
import javax.swing.JComponent;
import javax.swing.JPopupMenu;
import javax.swing.JToolBar;
import javax.swing.UIManager;
import org.openide.util.ImageUtilities;
import org.openide.util.Mutex;
import java.io.*;

/**
 * ToolbarWithOverflow provides a component which is useful for displaying commonly used
 * actions.  It adds an overflow button when the toolbar becomes too small to show all the
 * available actions.
 *
 * @author Th. Oikonomou
 * @since 7.51
 */
public class ToolbarWithOverflow extends JToolBar {

    private JButton overflowButton;

    private JPopupMenu popup;

    private JToolBar overflowToolbar;

    private boolean displayOverflowOnHover = true;

    // NOI18N
    private final String toolbarArrowHorizontal = "org/openide/awt/resources/toolbar_arrow_horizontal.png";

    // NOI18N
    private final String toolbarArrowVertical = "org/openide/awt/resources/toolbar_arrow_vertical.png";

    // NOI18N
    private final String PROP_PREF_ICON_SIZE = "PreferredIconSize";

    // NOI18N
    private final String PROP_DRAGGER = "_toolbar_dragger_";

    // NOI18N
    private final String PROP_JDEV_DISABLE_OVERFLOW = "nb.toolbar.overflow.disable";

    private AWTEventListener awtEventListener;

    private ComponentAdapter componentAdapter;

    // keep track of the overflow popup that is showing, possibly from another overflow button, in order to hide it if necessary
    private static JPopupMenu showingPopup = null;

    /**
     * Creates a new tool bar; orientation defaults to
     * <code>HORIZONTAL</code>.
     */
    public ToolbarWithOverflow() {
        this(HORIZONTAL);
    }

    /**
     * Creates a new tool bar with the specified
     * <code>orientation</code>. The
     * <code>orientation</code> must be either
     * <code>HORIZONTAL</code> or
     * <code>VERTICAL</code>.
     *
     * @param orientation the orientation desired
     */
    public ToolbarWithOverflow(int orientation) {
        this(null, orientation);
    }

    /**
     * Creates a new tool bar with the specified
     * <code>name</code>. The name is used as the title of the undocked tool
     * bar. The default orientation is
     * <code>HORIZONTAL</code>.
     *
     * @param name the name of the tool bar
     */
    public ToolbarWithOverflow(String name) {
        this(name, HORIZONTAL);
    }

    /**
     * Creates a new tool bar with a specified
     * <code>name</code> and
     * <code>orientation</code>. All other constructors call this constructor.
     * If
     * <code>orientation</code> is an invalid value, an exception will be
     * thrown.
     *
     * @param name the name of the tool bar
     * @param orientation the initial orientation -- it must be     *		either <code>HORIZONTAL</code> or <code>VERTICAL</code>
     * @exception IllegalArgumentException if orientation is neither
     * <code>HORIZONTAL</code> nor <code>VERTICAL</code>
     */
    public ToolbarWithOverflow(String name, int orientation) {
        super(name, orientation);
        setupOverflowButton();
        popup = new SafePopupMenu();
        popup.setBorderPainted(false);
        popup.setBorder(BorderFactory.createEmptyBorder());
        overflowToolbar = new SafeToolBar("overflowToolbar", orientation == HORIZONTAL ? VERTICAL : HORIZONTAL);
        overflowToolbar.setFloatable(false);
        overflowToolbar.setBorder(BorderFactory.createLineBorder(UIManager.getColor("controlShadow"), 1));
    }

    private ComponentListener getComponentListener() {
        writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "21cf5457-070d-4879-b34a-898db6bc8a5b");
        if (componentAdapter == null) {
            writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "def1dbe8-21ea-4431-a5d7-fb3c800c4362");
            componentAdapter = new ComponentAdapter() {

                @Override
                public void componentResized(ComponentEvent e) {
                    maybeAddOverflow();
                }
            };
        }
        writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "acc5a1e9-d7a9-4485-a457-cb85adc3e03d");
        return componentAdapter;
    }

    private AWTEventListener getAWTEventListener() {
        writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "7dc93f55-5446-425c-97ab-a112ea4cd35b");
        if (awtEventListener == null) {
            writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "e5ed14f4-fb6f-4fff-a129-4e745490f641");
            awtEventListener = new AWTEventListener() {

                @Override
                public void eventDispatched(AWTEvent event) {
                    MouseEvent e = (MouseEvent) event;
                    if (isVisible() && !isShowing() && popup.isShowing()) {
                        showingPopup = null;
                        popup.setVisible(false);
                        return;
                    }
                    if (event.getSource() == popup) {
                        if (popup.isShowing() && e.getID() == MouseEvent.MOUSE_EXITED) {
                            int minX = popup.getLocationOnScreen().x;
                            int maxX = popup.getLocationOnScreen().x + popup.getWidth();
                            int minY = popup.getLocationOnScreen().y;
                            int maxY = popup.getLocationOnScreen().y + popup.getHeight();
                            if (e.getXOnScreen() < minX || e.getXOnScreen() >= maxX || e.getYOnScreen() < minY || e.getYOnScreen() >= maxY) {
                                showingPopup = null;
                                popup.setVisible(false);
                            }
                        }
                    } else {
                        if (popup.isShowing() && overflowButton.isShowing() && (e.getID() == MouseEvent.MOUSE_MOVED || e.getID() == MouseEvent.MOUSE_EXITED)) {
                            int minX = overflowButton.getLocationOnScreen().x;
                            int maxX = getOrientation() == HORIZONTAL ? minX + popup.getWidth() : minX + overflowButton.getWidth() + popup.getWidth();
                            int minY = overflowButton.getLocationOnScreen().y;
                            int maxY = getOrientation() == HORIZONTAL ? minY + overflowButton.getHeight() + popup.getHeight() : minY + popup.getHeight();
                            if (e.getXOnScreen() < minX || e.getYOnScreen() < minY || e.getXOnScreen() > maxX || e.getYOnScreen() > maxY) {
                                showingPopup = null;
                                popup.setVisible(false);
                            }
                        }
                    }
                }
            };
        }
        writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "c69eea80-50f7-434b-8e74-a4b053974dcb");
        return awtEventListener;
    }

    @Override
    public void addNotify() {
        writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "cff8b97f-0502-4107-8683-d3e932f91923");
        super.addNotify();
        writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "5c7bb845-43e2-481d-a4ec-d4850e0c44fd");
        if (!Boolean.TRUE.equals(getClientProperty(PROP_JDEV_DISABLE_OVERFLOW))) {
            writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "400aa67d-155a-488f-8b17-14ad178ac8fa");
            addComponentListener(getComponentListener());
            writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "19cdfa43-5708-474e-985a-294ee768f5c2");
            Toolkit.getDefaultToolkit().addAWTEventListener(getAWTEventListener(), AWTEvent.MOUSE_EVENT_MASK | AWTEvent.MOUSE_MOTION_EVENT_MASK);
        }
    }

    @Override
    public void removeNotify() {
        writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "8c3b1ac1-28d3-450c-8721-45ff0eb5ec62");
        super.removeNotify();
        writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "f44c6bc0-99fb-4fdc-a8cc-7dc45b543046");
        if (componentAdapter != null) {
            writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "ff584177-5e74-4b3b-84bb-121d12314a08");
            removeComponentListener(componentAdapter);
        }
        writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "6f81b1f0-a485-4855-821c-bb42b286efa9");
        if (awtEventListener != null) {
            writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "80670fe9-c4fc-4379-ad49-fd9ee2e716cf");
            Toolkit.getDefaultToolkit().removeAWTEventListener(awtEventListener);
        }
    }

    @Override
    public void updateUI() {
        writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "07a98339-57a8-4a66-bbad-2cab1f4e96e9");
        Mutex.EVENT.readAccess(new Runnable() {

            @Override
            public void run() {
                superUpdateUI();
            }
        });
    }

    final void superUpdateUI() {
        writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "830c243e-b659-41ed-a7a8-81ceb26c60f0");
        super.updateUI();
    }

    /**
     * Returns whether the overflow should be displayed on hover or not. The
     * default value is <code>true</code>.
     *
     * @return <code>true</code> if overflow is displayed on hover; <code>false</code> otherwise
     */
    public boolean isDisplayOverflowOnHover() {
        writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "7b3ae997-b36b-4396-b416-bb48b823c2f6");
        return displayOverflowOnHover;
    }

    /**
     * Sets whether the overflow should be displayed on hover or not. The
     * default value is <code>true</code>.
     *
     * @param displayOverflowOnHover if <code>true</code>, the overflow will be displayed on hover;
     * <code>false</code> otherwise
     */
    public void setDisplayOverflowOnHover(boolean displayOverflowOnHover) {
        writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "de0cb650-18b9-402b-bc65-955adf87a2bc");
        this.displayOverflowOnHover = displayOverflowOnHover;
        writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "c907faae-e5b6-467a-9bc5-0ebc223fdd3f");
        setupOverflowButton();
    }

    @Override
    public Dimension getPreferredSize() {
        writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "65ce000c-9e5e-41cc-b94b-78e30fb12b35");
        Component[] comps = getAllComponents();
        writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "405c34be-bfab-4971-9186-97a29919d2fc");
        Insets insets = getInsets();
        writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "14cc74d0-9556-4ff2-9a78-4f177e6e141a");
        int width = null == insets ? 0 : insets.left + insets.right;
        writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "0b7f6ab3-334f-4c8a-99f6-de18d58dd4bd");
        int height = null == insets ? 0 : insets.top + insets.bottom;
        writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "8947ca3b-43fa-4060-81a2-a0dc370f9319");
        for (int i = 0; i < comps.length; i++) {
            writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "fcb0320e-93ea-4e1f-ae5c-1f114c1041cc");
            Component comp = comps[i];
            writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "e418b354-3ff4-4c53-9748-6ee935e74ec7");
            if (!comp.isVisible()) {
                writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "9f795c3e-da58-4668-a8d2-b969f50401f5");
                continue;
            }
            writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "d7062b0d-41fa-4ee6-9d62-07640ed371d9");
            width += getOrientation() == HORIZONTAL ? comp.getPreferredSize().width : comp.getPreferredSize().height;
            writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "5c2497cd-991c-4c91-8178-b23a78f8c536");
            height = Math.max(height, (getOrientation() == HORIZONTAL ? (comp.getPreferredSize().height + (insets == null ? 0 : insets.top + insets.bottom)) : (comp.getPreferredSize().width) + (insets == null ? 0 : insets.left + insets.right)));
        }
        writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "88ec79b4-b16c-4d28-be61-5388998e0307");
        if (overflowToolbar.getComponentCount() > 0) {
            writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "403a3e77-9a39-41ce-8eb0-cee358f68259");
            width += getOrientation() == HORIZONTAL ? overflowButton.getPreferredSize().width : overflowButton.getPreferredSize().height;
        }
        writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "f9db7046-ace8-4af3-9e3f-540b1b08ed1b");
        Dimension dim = getOrientation() == HORIZONTAL ? new Dimension(width, height) : new Dimension(height, width);
        writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "ba758462-cbb8-4430-b30f-86556ef8d4aa");
        return dim;
    }

    @Override
    public void setOrientation(int o) {
        writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "6880e631-e05a-4827-8750-fb2aab89fbce");
        super.setOrientation(o);
        writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "46f6d5b8-cb92-4ae8-bc5d-3bd3516f1d72");
        setupOverflowButton();
    }

    @Override
    public void removeAll() {
        writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "d7cbe592-06da-4278-a430-045e42d9ad14");
        super.removeAll();
        writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "c85371db-c3e3-4cc2-908b-caa7a220f92c");
        overflowToolbar.removeAll();
    }

    @Override
    public void validate() {
        writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "9af0e0f1-7776-44a1-b8dd-a1de6bccf57e");
        if (!Boolean.TRUE.equals(getClientProperty(PROP_JDEV_DISABLE_OVERFLOW))) {
            writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "dae15baa-65b5-4881-8987-dcdc8053adcf");
            int visibleButtons = computeVisibleButtons();
            writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "d817cd3b-b63b-4d23-b72b-dbcf34157db5");
            if (visibleButtons == -1) {
                writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "5911f91a-ce55-46db-ac5d-98601fb62eef");
                handleOverflowRemoval();
            } else {
                writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "16ff306f-df7d-4db8-8355-46847bd99a9d");
                handleOverflowAddittion(visibleButtons);
            }
        }
        writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "52677a32-81f8-49a3-9030-522cb79ea4b8");
        super.validate();
    }

    private void setupOverflowButton() {
        writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "cb339a71-d9eb-4092-a029-cea1cfc7794a");
        overflowButton = new JButton(ImageUtilities.loadImageIcon(getOrientation() == HORIZONTAL ? toolbarArrowVertical : toolbarArrowHorizontal, false)) {

            @Override
            public void updateUI() {
                Mutex.EVENT.readAccess(new Runnable() {

                    @Override
                    public void run() {
                        superUpdateUI();
                    }
                });
            }

            private void superUpdateUI() {
                super.updateUI();
            }
        };
        writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "032c5bd9-d217-448e-b557-ece6e1743298");
        overflowButton.addMouseListener(new MouseAdapter() {

            @Override
            public void mouseClicked(MouseEvent e) {
                if (popup.isShowing()) {
                    showingPopup = null;
                    popup.setVisible(false);
                } else {
                    displayOverflow();
                }
            }

            @Override
            public void mouseEntered(MouseEvent e) {
                if (showingPopup != null && showingPopup != popup) {
                    showingPopup.setVisible(false);
                    showingPopup = null;
                }
                if (displayOverflowOnHover) {
                    displayOverflow();
                }
            }
        });
    }

    private void displayOverflow() {
        writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "a0240d6f-89c7-4911-a9e5-05eca9c68d7a");
        if (!overflowButton.isShowing()) {
            writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "72e9407d-1766-4da9-9e4f-018cf5d98fc7");
            return;
        }
        writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "db0aa947-094f-466d-ada2-ddc78124afb3");
        int x = getOrientation() == HORIZONTAL ? overflowButton.getLocationOnScreen().x : overflowButton.getLocationOnScreen().x + overflowButton.getWidth();
        writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "bf4dca3d-102a-4cb5-af07-8586147a04cb");
        int y = getOrientation() == HORIZONTAL ? overflowButton.getLocationOnScreen().y + overflowButton.getHeight() : overflowButton.getLocationOnScreen().y;
        writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "cbbe3e5c-026b-44b8-ac66-130574d474a5");
        popup.setLocation(x, y);
        writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "93e24b09-4082-4c15-b2cc-26ed25ebf7b2");
        showingPopup = popup;
        writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "3490f79c-d8a1-4998-a2d1-ac264a4ad8d4");
        popup.setVisible(true);
    }

    /**
     * Determines if an overflow button should be added to or removed from the toolbar.
     */
    private void maybeAddOverflow() {
        writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "25859c40-3562-441a-b927-7f60216fb45f");
        validate();
        writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "27d74cda-12e2-4dc0-8ce3-aa41bbad9bfe");
        repaint();
    }

    private int computeVisibleButtons() {
        writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "80f9c707-7d74-4ead-bf0e-d27214db5302");
        if (isShowing()) {
            writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "ad0b5992-e209-4a3a-8e31-a3f425fce584");
            int w = getOrientation() == HORIZONTAL ? overflowButton.getIcon().getIconWidth() + 4 : getWidth() - getInsets().left - getInsets().right;
            writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "405de7bc-3da1-47af-8113-736ef9360474");
            int h = getOrientation() == HORIZONTAL ? getHeight() - getInsets().top - getInsets().bottom : overflowButton.getIcon().getIconHeight() + 4;
            writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "13f30a04-51c5-41db-a0a6-5071b5b3e72d");
            overflowButton.setMaximumSize(new Dimension(w, h));
            writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "a49b0f69-58d2-4325-860c-700d15a2034c");
            overflowButton.setMinimumSize(new Dimension(w, h));
            writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "bd4422e9-96a7-4eed-8705-6313fa5cecaa");
            overflowButton.setPreferredSize(new Dimension(w, h));
        }
        writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "3f13f028-859c-4466-875b-cc14867109b4");
        handleIconResize();
        writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "d01c29b3-2e43-46df-8ef1-792d5f56613f");
        Component[] comps = getAllComponents();
        writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "3597dd74-5326-4ce4-9181-57a01577e363");
        int sizeSoFar = 0;
        writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "71e3615c-c5cc-42b8-a775-b6ab897c8bc6");
        int maxSize = getOrientation() == HORIZONTAL ? getWidth() : getHeight();
        writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "c514f4c0-cdad-4861-a0ba-a4d8b0e4ac28");
        int overflowButtonSize = getOrientation() == HORIZONTAL ? overflowButton.getPreferredSize().width : overflowButton.getPreferredSize().height;
        writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "696d2271-2534-41a1-84d4-229fcf8f0a8a");
        // all that return true from isVisible()
        int showingButtons = 0;
        writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "8438d20e-1cb9-487a-ba73-b236dca5f8c8");
        // all visible that fit into the given space (maxSize)
        int visibleButtons = 0;
        writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "f1b26f76-89f1-496d-a1bb-b4fca2ab7e9b");
        Insets insets = getInsets();
        writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "a8631224-6491-4b5e-845d-040d23cd8211");
        if (null != insets) {
            writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "deea42fa-682d-4745-8dc0-e0c4d563e62d");
            sizeSoFar = getOrientation() == HORIZONTAL ? insets.left + insets.right : insets.top + insets.bottom;
        }
        writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "ebb2242e-0a84-48b4-8d14-463a832afd9f");
        for (int i = 0; i < comps.length; i++) {
            writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "5b088b42-ecf9-4503-9118-84c18f16ad22");
            Component comp = comps[i];
            writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "017a6fd0-74f3-4e8e-93c4-ea5822907bf8");
            if (!comp.isVisible()) {
                writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "ff1db487-232d-44c8-ae19-c1b3c7ab5d4e");
                continue;
            }
            writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "c74ea710-b20d-4446-aa03-0de5d8460f12");
            if (showingButtons == visibleButtons) {
                writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "b784120c-66a8-4d51-90bb-ffd5aa5a1eb3");
                int size = getOrientation() == HORIZONTAL ? comp.getPreferredSize().width : comp.getPreferredSize().height;
                writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "2212e4ae-3fdc-43a8-8e25-207ae0cd54ab");
                if (sizeSoFar + size <= maxSize) {
                    writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "1dfbfc75-9891-4a04-9fe0-60b11f0f271e");
                    sizeSoFar += size;
                    writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "3403e016-63c6-465e-aea2-7b8352eccf78");
                    visibleButtons++;
                }
            }
            writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "27e0cff7-b733-44fe-bc16-18540201a013");
            showingButtons++;
        }
        writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "d8abe64f-3f7c-4141-af18-ba37dc73ddc9");
        if (visibleButtons < showingButtons && visibleButtons > 0 && sizeSoFar + overflowButtonSize > maxSize) {
            writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "157f636f-ef43-4065-8be8-16df70d82bb4");
            // overflow button needed but would not have enough space, remove one more button
            visibleButtons--;
        }
        writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "5139080d-4ce8-476e-a5a9-b3149419baa3");
        if (visibleButtons == 0 && comps.length > 0 && comps[0] instanceof JComponent && Boolean.TRUE.equals(((JComponent) comps[0]).getClientProperty(PROP_DRAGGER))) {
            writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "ee3872e7-0cad-428d-8a64-4d5774b5c3fe");
            // always include the dragger if present
            visibleButtons = 1;
        }
        writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "16c8c878-303e-4a52-9f3e-67c8b47a64d3");
        if (visibleButtons == showingButtons) {
            writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "beaed449-85ad-4d9b-a9e6-855221ba98b6");
            visibleButtons = -1;
        }
        writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "da96fa55-9961-4995-a35c-691569a6fc7a");
        return visibleButtons;
    }

    private void handleOverflowAddittion(int visibleButtons) {
        writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "6e1d52a1-df36-4dd2-908c-e100de55b4e9");
        Component[] comps = getAllComponents();
        writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "66a3f051-aa46-4813-80eb-d16a4a71e3a6");
        removeAll();
        writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "6ebbd545-c9ff-4127-9ac0-49e79edb1f3e");
        overflowToolbar.setOrientation(getOrientation() == HORIZONTAL ? VERTICAL : HORIZONTAL);
        writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "3ef7c912-f78a-4da3-807a-92022d89c600");
        popup.removeAll();
        writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "83aaebae-6835-400b-a9e4-df4db8eadbbb");
        for (Component comp : comps) {
            writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "78ea77f2-6474-4de3-99f9-7ac413a1f0fc");
            if (visibleButtons > 0) {
                writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "90de58a3-9d95-4425-93d4-8591b132306c");
                add(comp);
                writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "1f138512-7a8c-4d49-8b25-173053a8c61e");
                if (comp.isVisible()) {
                    writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "0049ef90-8bef-4fac-af09-2808a0111c3e");
                    visibleButtons--;
                }
            } else {
                writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "f804c1bc-6460-4cc4-8160-4e482dce74cb");
                overflowToolbar.add(comp);
            }
        }
        writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "1b99edf8-5a5a-4267-9762-16b0d9a6c628");
        popup.add(overflowToolbar);
        writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "ef842192-6d60-4c81-9553-9aa6d9022749");
        add(overflowButton);
    }

    private void handleOverflowRemoval() {
        writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "817a895b-7e75-4686-9628-d3173d70c3a8");
        if (overflowToolbar.getComponents().length > 0) {
            writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "3a66b5e7-3e2e-4b13-b831-02e230f25925");
            remove(overflowButton);
            writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "231a1f85-ae50-4fda-b338-06abd11a65a4");
            handleIconResize();
            writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "1ba71053-0345-49bc-a0f3-b1bb215f2dc5");
            for (Component comp : overflowToolbar.getComponents()) {
                writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "603b506d-d6c9-41c1-9fae-688bb005083f");
                add(comp);
            }
            writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "c3f6fbf5-5cb2-4517-8ded-34ae75a659f2");
            overflowToolbar.removeAll();
            writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "ede59e1e-6b93-4e5c-afa9-37467448fb39");
            popup.removeAll();
        }
    }

    private void handleIconResize() {
        writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "19cc5bc8-5f43-435f-a0d5-232c2b8f2ee1");
        for (Component comp : overflowToolbar.getComponents()) {
            writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "c5df6724-8395-40ed-bd04-d596722518eb");
            boolean smallToolbarIcons = getClientProperty(PROP_PREF_ICON_SIZE) == null;
            writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "2479abe5-3e1f-4790-8609-087e56ce3c8d");
            if (smallToolbarIcons) {
                writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "22d89bb8-0d8a-4767-a3b6-81abdd8a4091");
                ((JComponent) comp).putClientProperty(PROP_PREF_ICON_SIZE, null);
            } else {
                writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "7b680484-f8be-4a1d-9caf-6415e7bd095d");
                ((JComponent) comp).putClientProperty(PROP_PREF_ICON_SIZE, Integer.valueOf(24));
            }
        }
    }

    private Component[] getAllComponents() {
        writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "1b62f8d9-4c0f-4f95-a008-f55533cf17f2");
        Component[] toolbarComps;
        writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "008fbcab-a2ac-4526-9b64-5caced864316");
        Component[] overflowComps = overflowToolbar.getComponents();
        writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "9034915a-445d-42cb-ac40-791c9db610d6");
        if (overflowComps.length == 0) {
            writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "4166a9d9-1ca4-4f50-8e5d-7c38465bbf96");
            toolbarComps = getComponents();
        } else {
            writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "c49a5616-a394-4fca-b512-c545853d81f3");
            if (getComponentCount() > 0) {
                writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "585be9e9-7986-40b9-8268-a1fac4ad98ef");
                toolbarComps = new Component[getComponents().length - 1];
                writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "6f52fb6f-6bad-4f27-8161-9e14fdc8bfbe");
                System.arraycopy(getComponents(), 0, toolbarComps, 0, toolbarComps.length);
            } else {
                writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "d76a7179-028f-4ed9-bd8a-d8efefc0e537");
                toolbarComps = new Component[0];
            }
        }
        writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "2b02f812-9ccc-4b48-8843-1ef03903859f");
        Component[] comps = new Component[toolbarComps.length + overflowComps.length];
        writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "e60d4e67-1491-40b8-b4da-2339bfd782cb");
        System.arraycopy(toolbarComps, 0, comps, 0, toolbarComps.length);
        writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "67212cef-7e81-4f6e-89db-20dff3de04cd");
        System.arraycopy(overflowComps, 0, comps, toolbarComps.length, overflowComps.length);
        writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "9eb7b741-9a1e-461b-8eff-6eb10e7c1fd1");
        return comps;
    }

    private static class SafeToolBar extends JToolBar {

        public SafeToolBar(String name, int orientation) {
            super(name, orientation);
        }

        @Override
        public void updateUI() {
            Mutex.EVENT.readAccess(new Runnable() {

                @Override
                public void run() {
                    superUpdateUI();
                }
            });
        }

        final void superUpdateUI() {
            super.updateUI();
        }
    }

    private static class SafePopupMenu extends JPopupMenu {

        @Override
        public void updateUI() {
            Mutex.EVENT.readAccess(new Runnable() {

                @Override
                public void run() {
                    superUpdateUI();
                }
            });
        }

        final void superUpdateUI() {
            super.updateUI();
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
