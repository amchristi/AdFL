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

import java.awt.Point;
import java.awt.event.MouseAdapter;
import java.awt.event.MouseEvent;
import java.awt.event.MouseMotionAdapter;
import java.beans.PropertyChangeEvent;
import java.beans.PropertyChangeListener;
import java.util.HashMap;
import java.util.Map;
import java.util.logging.Level;
import java.util.logging.Logger;
import javax.swing.DefaultButtonModel;
import javax.swing.Icon;
import javax.swing.ImageIcon;
import javax.swing.JButton;
import javax.swing.JPopupMenu;
import javax.swing.event.PopupMenuEvent;
import javax.swing.event.PopupMenuListener;
import org.openide.util.ImageUtilities;
import org.openide.util.Parameters;
import java.io.*;

/**
 * JButton with a small arrow that displays popup menu when clicked.
 *
 * @author S. Aubrecht
 * @since 6.11
 */
class DropDownButton extends JButton {

    private boolean mouseInButton = false;

    private boolean mouseInArrowArea = false;

    private Map<String, Icon> regIcons = new HashMap<String, Icon>(5);

    private Map<String, Icon> arrowIcons = new HashMap<String, Icon>(5);

    // NOI18N
    private static final String ICON_NORMAL = "normal";

    // NOI18N
    private static final String ICON_PRESSED = "pressed";

    // NOI18N
    private static final String ICON_ROLLOVER = "rollover";

    // NOI18N
    private static final String ICON_ROLLOVER_SELECTED = "rolloverSelected";

    // NOI18N
    private static final String ICON_SELECTED = "selected";

    // NOI18N
    private static final String ICON_DISABLED = "disabled";

    // NOI18N
    private static final String ICON_DISABLED_SELECTED = "disabledSelected";

    // NOI18N
    private static final String ICON_ROLLOVER_LINE = "rolloverLine";

    // NOI18N
    private static final String ICON_ROLLOVER_SELECTED_LINE = "rolloverSelectedLine";

    private PopupMenuListener menuListener;

    /**
     * Creates a new instance of MenuToggleButton
     */
    public DropDownButton(Icon icon, JPopupMenu popup) {
        // NOI18N
        Parameters.notNull("icon", icon);
        assert null != icon;
        putClientProperty(DropDownButtonFactory.PROP_DROP_DOWN_MENU, popup);
        setIcon(icon);
        setDisabledIcon(ImageUtilities.createDisabledIcon(icon));
        resetIcons();
        addPropertyChangeListener(DropDownButtonFactory.PROP_DROP_DOWN_MENU, new PropertyChangeListener() {

            public void propertyChange(PropertyChangeEvent e) {
                resetIcons();
            }
        });
        addMouseMotionListener(new MouseMotionAdapter() {

            @Override
            public void mouseMoved(MouseEvent e) {
                if (null != getPopupMenu()) {
                    mouseInArrowArea = isInArrowArea(e.getPoint());
                    updateRollover(_getRolloverIcon(), _getRolloverSelectedIcon());
                }
            }
        });
        addMouseListener(new MouseAdapter() {

            private boolean popupMenuOperation = false;

            @Override
            public void mousePressed(MouseEvent e) {
                popupMenuOperation = false;
                JPopupMenu menu = getPopupMenu();
                if (menu != null && getModel() instanceof Model) {
                    Model model = (Model) getModel();
                    if (!model._isPressed()) {
                        if (isInArrowArea(e.getPoint()) && menu.getComponentCount() > 0) {
                            model._press();
                            menu.addPopupMenuListener(getMenuListener());
                            menu.show(DropDownButton.this, 0, getHeight());
                            popupMenuOperation = true;
                        }
                    } else {
                        model._release();
                        menu.removePopupMenuListener(getMenuListener());
                        popupMenuOperation = true;
                    }
                }
            }

            @Override
            public void mouseReleased(MouseEvent e) {
                // the event, otherwise the button's action will be triggered.
                if (popupMenuOperation) {
                    popupMenuOperation = false;
                    e.consume();
                }
            }

            @Override
            public void mouseEntered(MouseEvent e) {
                mouseInButton = true;
                if (hasPopupMenu()) {
                    mouseInArrowArea = isInArrowArea(e.getPoint());
                    updateRollover(_getRolloverIcon(), _getRolloverSelectedIcon());
                }
            }

            @Override
            public void mouseExited(MouseEvent e) {
                mouseInButton = false;
                mouseInArrowArea = false;
                if (hasPopupMenu()) {
                    updateRollover(_getRolloverIcon(), _getRolloverSelectedIcon());
                }
            }
        });
        setModel(new Model());
    }

    private PopupMenuListener getMenuListener() {
        writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "483659be-b90d-4c79-9bce-4d1981cbff13");
        if (null == menuListener) {
            writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "d4c4b312-8ce4-43b5-9e6b-e369e25bb5f6");
            menuListener = new PopupMenuListener() {

                public void popupMenuWillBecomeVisible(PopupMenuEvent e) {
                }

                public void popupMenuWillBecomeInvisible(PopupMenuEvent e) {
                    // we should not show it again.
                    if (getModel() instanceof Model) {
                        ((Model) getModel())._release();
                    }
                    JPopupMenu menu = getPopupMenu();
                    if (null != menu) {
                        menu.removePopupMenuListener(this);
                    }
                }

                public void popupMenuCanceled(PopupMenuEvent e) {
                }
            };
        }
        writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "4872ed42-674c-4cce-86ab-a575558412b5");
        return menuListener;
    }

    private void updateRollover(Icon rollover, Icon rolloverSelected) {
        writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "49775ea9-7255-4708-84e6-d218802c8e47");
        super.setRolloverIcon(rollover);
        writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "fdf5f8c8-8480-4f70-9e1d-15018fab88fd");
        super.setRolloverSelectedIcon(rolloverSelected);
    }

    private void resetIcons() {
        writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "702c64d9-debe-4e0e-ba5a-1958f4e96d15");
        Icon icon = regIcons.get(ICON_NORMAL);
        writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "0063e366-73c7-4fa5-8f78-c73d6b5a20b6");
        if (null != icon) {
            writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "f29090f9-77f8-455d-a52d-fad9d65982fa");
            setIcon(icon);
        }
        writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "1bbfb734-e3ee-44c9-b3c3-72a0646ec986");
        icon = regIcons.get(ICON_PRESSED);
        writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "1419719f-c817-42c3-bbfe-8327e2c93d2c");
        if (null != icon) {
            writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "39de2694-5a4c-4715-83e8-3177275b6cdc");
            setPressedIcon(icon);
        }
        writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "1cf58bef-c4e8-48ac-a013-7e5ba7fbee61");
        icon = regIcons.get(ICON_ROLLOVER);
        writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "853d66d8-6b4d-4924-897b-70428eab11df");
        if (null != icon) {
            writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "55903a30-29d5-4b77-8d25-4fb465c021fd");
            setRolloverIcon(icon);
        }
        writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "04846da2-03f0-47ff-bcaf-1e89712d00f3");
        icon = regIcons.get(ICON_ROLLOVER_SELECTED);
        writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "b921ea6e-80af-42f5-a9b8-002294131a2c");
        if (null != icon) {
            writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "1d37e2ab-c03c-47f8-aaae-4cce1479dbfe");
            setRolloverSelectedIcon(icon);
        }
        writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "b59f59a0-553f-4c90-8372-c32de50762ed");
        icon = regIcons.get(ICON_SELECTED);
        writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "a94cf248-03bb-4e35-92fe-2721ff0702ca");
        if (null != icon) {
            writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "bc43ad1b-5f8c-4b51-b3da-edfbbae1c930");
            setSelectedIcon(icon);
        }
        writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "3d946380-11cc-4f82-b8cb-562b7d904046");
        icon = regIcons.get(ICON_DISABLED);
        writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "68eda009-e106-485b-b846-09def5882b39");
        if (null != icon) {
            writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "50a28865-afcd-4893-aa2f-9c8c7ebdd645");
            setDisabledIcon(icon);
        }
        writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "be3a596d-8b90-4991-a678-d6ddd064ade2");
        icon = regIcons.get(ICON_DISABLED_SELECTED);
        writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "dd69d084-96d1-412a-baaf-e44d542fc317");
        if (null != icon) {
            writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "e8966c25-5022-4454-ab8d-a4648ee791c1");
            setDisabledSelectedIcon(icon);
        }
    }

    private Icon _getRolloverIcon() {
        writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "dc1d22d8-9ac5-458b-9540-87b8148ed07a");
        Icon icon = null;
        writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "a5d9325b-557f-4a29-a9b5-c0422ff07f9b");
        icon = arrowIcons.get(mouseInArrowArea ? ICON_ROLLOVER : ICON_ROLLOVER_LINE);
        writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "c065276d-518b-4400-9263-1fa1825aebe8");
        if (null == icon) {
            writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "913861ff-77f9-4d13-bd9f-8e3f31e4d525");
            Icon orig = regIcons.get(ICON_ROLLOVER);
            writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "a817ef1a-43c7-4f7e-924c-9d4085fb1256");
            if (null == orig) {
                writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "1b9281fb-683c-4a1d-a777-fdc098b376d0");
                orig = regIcons.get(ICON_NORMAL);
            }
            writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "a9b53e77-80e9-4e7d-971e-aa61db42bff9");
            icon = new IconWithArrow(orig, !mouseInArrowArea);
            writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "dfbd6722-135f-43df-8682-a0f85caf7564");
            arrowIcons.put(mouseInArrowArea ? ICON_ROLLOVER : ICON_ROLLOVER_LINE, icon);
        }
        writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "e1b3ac1f-5352-46f8-88fb-128624fe357a");
        return icon;
    }

    private Icon _getRolloverSelectedIcon() {
        writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "e2032bdb-ff83-4dea-9fe7-1066427f3c90");
        Icon icon = null;
        writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "0cb00103-b927-4446-8f31-252cabd23fcd");
        icon = arrowIcons.get(mouseInArrowArea ? ICON_ROLLOVER_SELECTED : ICON_ROLLOVER_SELECTED_LINE);
        writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "4b8124cd-c256-4e8d-b592-26307e7eeb57");
        if (null == icon) {
            writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "58c82514-0d82-40f3-8d1a-35dc74e67d0b");
            Icon orig = regIcons.get(ICON_ROLLOVER_SELECTED);
            writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "49cc4541-e321-40ce-a67e-f58b43c68d3c");
            if (null == orig) {
                writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "a6c32179-173b-493f-b261-49aa33b64b90");
                orig = regIcons.get(ICON_ROLLOVER);
            }
            writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "ab47026a-9b77-462b-8c41-d55bbcaf5b1b");
            if (null == orig) {
                writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "7ba3e8a4-7a7a-4dbb-a8d8-0bf811ae4b28");
                orig = regIcons.get(ICON_NORMAL);
            }
            writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "007d8dfe-afd5-44d8-93a6-d67ea55278ba");
            icon = new IconWithArrow(orig, !mouseInArrowArea);
            writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "3dde77be-a7f8-42a5-ad9e-fc89a6e772ac");
            arrowIcons.put(mouseInArrowArea ? ICON_ROLLOVER_SELECTED : ICON_ROLLOVER_SELECTED_LINE, icon);
        }
        writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "6526c3db-42ce-43b3-a137-facc9748147c");
        return icon;
    }

    JPopupMenu getPopupMenu() {
        writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "37e75f92-f75f-43d1-9fba-7c178f9518dc");
        Object menu = getClientProperty(DropDownButtonFactory.PROP_DROP_DOWN_MENU);
        writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "5f7bd4a7-20e0-47dc-b91b-6ec393ceb8ab");
        if (menu instanceof JPopupMenu) {
            writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "c7e75ac2-79b4-4d29-94f4-2f2168a3fb28");
            return (JPopupMenu) menu;
        }
        writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "ae4effe3-3752-46d9-9162-3aec1708006e");
        return null;
    }

    boolean hasPopupMenu() {
        writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "3a406643-f210-4d57-84a4-0dbeb6648182");
        return null != getPopupMenu();
    }

    private boolean isInArrowArea(Point p) {
        writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "71588313-e711-45cb-b78a-fd19ddad8a90");
        return p.getLocation().x >= getWidth() - IconWithArrow.getArrowAreaWidth() - getInsets().right;
    }

    @Override
    public void setIcon(Icon icon) {
        writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "c79fe43f-7e56-45f2-acb9-e9cbd0a89453");
        Icon arrow = updateIcons(icon, ICON_NORMAL);
        writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "4b5eb763-f192-4a64-962e-103ed8786498");
        arrowIcons.remove(ICON_ROLLOVER_LINE);
        writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "8c558317-abd8-4c7a-8f9a-852ef1985b4d");
        arrowIcons.remove(ICON_ROLLOVER_SELECTED_LINE);
        writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "096b02ac-7f25-4b6b-9358-af254bb1a36d");
        arrowIcons.remove(ICON_ROLLOVER);
        writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "f339e46c-baa4-4501-aa1b-03e3d73c26c8");
        arrowIcons.remove(ICON_ROLLOVER_SELECTED);
        writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "9c8ff473-dde1-4044-86cd-d9123123a7ae");
        super.setIcon(hasPopupMenu() ? arrow : icon);
        writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "ecd7288a-3528-4df9-9c63-93f10cd28e4a");
        updateRollover(_getRolloverIcon(), _getRolloverSelectedIcon());
    }

    private Icon updateIcons(Icon orig, String iconType) {
        writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "50d40866-29a8-49bc-8c64-07123fa954f6");
        Icon arrow = null;
        writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "31043768-a090-4033-9671-63023a81d910");
        if (null == orig) {
            writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "bf016c1f-a147-4e29-8499-9b4355bc4220");
            regIcons.remove(iconType);
            writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "90d862aa-2de8-4c29-bb9d-b542eed77ec9");
            arrowIcons.remove(iconType);
        } else {
            writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "dc083213-1f2b-4507-9275-1fae9c07b03f");
            regIcons.put(iconType, orig);
            writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "21b8b70a-fa97-4145-b554-69841f99332d");
            arrow = new ImageIcon(ImageUtilities.icon2Image(new IconWithArrow(orig, false)));
            writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "a516a147-cd07-4ff1-81a4-55d839d1982b");
            arrowIcons.put(iconType, arrow);
        }
        writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "e58d1c81-baa7-4dbb-b56b-4454767bdd9f");
        return arrow;
    }

    @Override
    public void setPressedIcon(Icon icon) {
        writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "4dcb9fc4-5126-48ed-873c-3fb1c9ae5ebe");
        Icon arrow = updateIcons(icon, ICON_PRESSED);
        writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "1d5f34af-ec3d-403a-84d1-698791bab780");
        super.setPressedIcon(hasPopupMenu() ? arrow : icon);
    }

    @Override
    public void setSelectedIcon(Icon icon) {
        writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "fafde774-a4eb-47e7-955d-2c74dc13ffb0");
        Icon arrow = updateIcons(icon, ICON_SELECTED);
        writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "a83f2ac8-cbfd-4fa9-80fa-4269d53ba01e");
        super.setSelectedIcon(hasPopupMenu() ? arrow : icon);
    }

    @Override
    public void setRolloverIcon(Icon icon) {
        writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "52551560-3c66-4e9f-8e4b-a676de7df4ba");
        Icon arrow = updateIcons(icon, ICON_ROLLOVER);
        writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "d2adcad7-9864-4b64-87c9-cffeb74a223e");
        arrowIcons.remove(ICON_ROLLOVER_LINE);
        writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "f4c3c891-3739-4727-b7c3-d8bfde208d9e");
        arrowIcons.remove(ICON_ROLLOVER_SELECTED_LINE);
        writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "c651cc84-0015-48a6-86d6-7fe48d31f5d9");
        super.setRolloverIcon(hasPopupMenu() ? arrow : icon);
    }

    @Override
    public void setRolloverSelectedIcon(Icon icon) {
        writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "9e90c4eb-8abc-436b-abe6-6990e0213779");
        Icon arrow = updateIcons(icon, ICON_ROLLOVER_SELECTED);
        writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "3aa04193-393f-4568-b3c2-ea145ad8d1b9");
        arrowIcons.remove(ICON_ROLLOVER_SELECTED_LINE);
        writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "7cdbef84-9fcb-472d-8005-711816069bd7");
        super.setRolloverSelectedIcon(hasPopupMenu() ? arrow : icon);
    }

    @Override
    public void setDisabledIcon(Icon icon) {
        writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "cf965b1d-3046-45a5-8c33-ce3074362d11");
        // TODO use 'disabled' arrow icon
        Icon arrow = updateIcons(icon, ICON_DISABLED);
        writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "bc833816-347e-4b3b-9bf7-ae2e0078a526");
        super.setDisabledIcon(hasPopupMenu() ? arrow : icon);
    }

    @Override
    public void setDisabledSelectedIcon(Icon icon) {
        writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "f747dcf3-2734-4aca-b776-06cd92467a9d");
        // TODO use 'disabled' arrow icon
        Icon arrow = updateIcons(icon, ICON_DISABLED_SELECTED);
        writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "23a72ebd-55f6-4ff3-afab-bea121d82601");
        super.setDisabledSelectedIcon(hasPopupMenu() ? arrow : icon);
    }

    @Override
    public void setText(String text) {
        writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "1d3602b2-70c8-4cfc-b8ce-9c3f42fee863");
        // does nothing
        // NOI18N
        Logger.getLogger(DropDownToggleButton.class.getName()).log(Level.FINER, "DropDownButton cannot display text.");
    }

    @Override
    public String getText() {
        writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "2466a408-4558-495e-b949-fc1d36f933ea");
        return null;
    }

    private class Model extends DefaultButtonModel {

        private boolean _pressed = false;

        @Override
        public void setPressed(boolean b) {
            if (mouseInArrowArea || _pressed)
                return;
            super.setPressed(b);
        }

        public void _press() {
            if ((isPressed()) || !isEnabled()) {
                return;
            }
            stateMask |= PRESSED + ARMED;
            fireStateChanged();
            _pressed = true;
        }

        public void _release() {
            _pressed = false;
            mouseInArrowArea = false;
            setArmed(false);
            setPressed(false);
            setRollover(false);
            setSelected(false);
        }

        public boolean _isPressed() {
            return _pressed;
        }

        @Override
        protected void fireStateChanged() {
            if (_pressed)
                return;
            super.fireStateChanged();
        }

        @Override
        public void setArmed(boolean b) {
            if (_pressed)
                return;
            super.setArmed(b);
        }

        @Override
        public void setEnabled(boolean b) {
            if (_pressed)
                return;
            super.setEnabled(b);
        }

        @Override
        public void setSelected(boolean b) {
            if (_pressed)
                return;
            super.setSelected(b);
        }

        @Override
        public void setRollover(boolean b) {
            if (_pressed)
                return;
            super.setRollover(b);
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
