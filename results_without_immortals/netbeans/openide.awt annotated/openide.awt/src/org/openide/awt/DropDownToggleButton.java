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
import javax.swing.Icon;
import javax.swing.ImageIcon;
import javax.swing.JPopupMenu;
import javax.swing.JToggleButton;
import javax.swing.event.PopupMenuEvent;
import javax.swing.event.PopupMenuListener;
import org.openide.util.ImageUtilities;
import org.openide.util.Parameters;
import java.io.*;

/**
 * JToggleButton with a small arrow that displays popup menu when clicked.
 *
 * @author S. Aubrecht
 * @since 6.11
 */
class DropDownToggleButton extends JToggleButton {

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
     * Creates a new instance of DropDownToggleButton
     */
    public DropDownToggleButton(Icon icon, JPopupMenu popup) {
        // NOI18N
        Parameters.notNull("icon", icon);
        putClientProperty(DropDownButtonFactory.PROP_DROP_DOWN_MENU, popup);
        setIcon(icon);
        resetIcons();
        addPropertyChangeListener(DropDownButtonFactory.PROP_DROP_DOWN_MENU, new PropertyChangeListener() {

            @Override
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
                            menu.show(DropDownToggleButton.this, 0, getHeight());
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
        writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "898c73db-52b7-42e2-ad08-2e965e9d17c5");
        if (null == menuListener) {
            writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "c7bb3fd7-6936-4ce6-ac93-4db762b14331");
            menuListener = new PopupMenuListener() {

                @Override
                public void popupMenuWillBecomeVisible(PopupMenuEvent e) {
                }

                @Override
                public void popupMenuWillBecomeInvisible(PopupMenuEvent e) {
                    // we should not show it again.
                    if (!mouseInButton) {
                        if (getModel() instanceof Model) {
                            ((Model) getModel())._release();
                        }
                        JPopupMenu menu = getPopupMenu();
                        if (null != menu) {
                            menu.removePopupMenuListener(this);
                        }
                    }
                }

                @Override
                public void popupMenuCanceled(PopupMenuEvent e) {
                }
            };
        }
        writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "67ce800f-0b69-43a8-bf19-92e4ba58ffd4");
        return menuListener;
    }

    private void updateRollover(Icon rollover, Icon rolloverSelected) {
        writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "271898c7-5383-4c78-9e4e-007bc5845594");
        super.setRolloverIcon(rollover);
        writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "69817dc1-0d0c-470e-8295-001ae6e75d45");
        super.setRolloverSelectedIcon(rolloverSelected);
    }

    private void resetIcons() {
        writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "301299fe-030e-4629-9bfa-951b0049bd54");
        Icon icon = regIcons.get(ICON_NORMAL);
        writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "ff7e14c3-eef9-4d83-a2f4-2fb21ecdad4c");
        if (null != icon) {
            writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "7100ffac-9a34-4dc2-8648-df4596bbd48b");
            setIcon(icon);
        }
        writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "c5cdaae3-24c9-4110-ab29-19c384762064");
        icon = regIcons.get(ICON_PRESSED);
        writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "aa8b157d-aafd-4e60-9e9c-c8d97d748d70");
        if (null != icon) {
            writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "2dd13b11-9307-440b-96a5-bebca8b15675");
            setPressedIcon(icon);
        }
        writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "ca0fb376-d281-4144-a5d6-55330a6fedec");
        icon = regIcons.get(ICON_ROLLOVER);
        writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "6ecb4aa0-e0d3-466e-943a-61b88c7a2570");
        if (null != icon) {
            writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "63033a99-9206-4525-ba4b-da24636c78bf");
            setRolloverIcon(icon);
        }
        writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "2abb481f-8968-481e-9c4b-6f3a531d7aab");
        icon = regIcons.get(ICON_ROLLOVER_SELECTED);
        writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "1498f516-a56e-4771-a010-d059d9754272");
        if (null != icon) {
            writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "79822d4d-04ec-4d4a-985f-47585ecd12e2");
            setRolloverSelectedIcon(icon);
        }
        writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "ee05d755-d066-4302-b1a9-dcd8892f45d1");
        icon = regIcons.get(ICON_SELECTED);
        writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "394689a5-308c-4311-9d77-4e2bc86a07c2");
        if (null != icon) {
            writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "7e9b4f1f-7589-4a1c-a442-77738d687dc3");
            setSelectedIcon(icon);
        }
        writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "52522968-19a7-4da4-8062-779392c2e5a5");
        icon = regIcons.get(ICON_DISABLED);
        writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "e679cd48-bd54-4267-a925-a628316c7f79");
        if (null != icon) {
            writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "fdd27eb8-8d40-4564-89b9-d597d78355e3");
            setDisabledIcon(icon);
        }
        writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "3c2b5c8e-91be-4653-9da9-ccf4062bbfe1");
        icon = regIcons.get(ICON_DISABLED_SELECTED);
        writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "495d0cf0-e6a7-4781-8d5e-e7cc24e44e40");
        if (null != icon) {
            writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "65102be1-e8ae-4692-94b4-87afbcfceca1");
            setDisabledSelectedIcon(icon);
        }
    }

    private Icon _getRolloverIcon() {
        writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "c20aa269-3506-41e0-94ab-ae9da94f9537");
        Icon icon = null;
        writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "d82d7942-4027-4f62-a883-b504e8c419f8");
        icon = arrowIcons.get(mouseInArrowArea ? ICON_ROLLOVER : ICON_ROLLOVER_LINE);
        writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "3bc8e107-ef06-4b73-b634-13b2fc46a2f3");
        if (null == icon) {
            writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "dd038ba5-4c7b-45bd-8a9f-b752a0d8fb96");
            Icon orig = regIcons.get(ICON_ROLLOVER);
            writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "8217efdd-44a4-4fcd-8b0e-bbe5226b9dd6");
            if (null == orig) {
                writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "290a64a7-479e-41fe-b7c2-c790379d558d");
                orig = regIcons.get(ICON_NORMAL);
            }
            writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "d76697af-19bf-400b-89f5-b5139c79546a");
            icon = new IconWithArrow(orig, !mouseInArrowArea);
            writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "4a6b82d4-cfc5-4fa2-8be3-6cc31ef14b83");
            arrowIcons.put(mouseInArrowArea ? ICON_ROLLOVER : ICON_ROLLOVER_LINE, icon);
        }
        writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "a4cd9a50-d3e3-45e0-b565-70d4be597e10");
        return icon;
    }

    private Icon _getRolloverSelectedIcon() {
        writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "55507e1f-0af0-4fe9-8d28-058367c349dd");
        Icon icon = null;
        writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "526cd37f-7b6a-4a70-bfa2-fd09be066599");
        icon = arrowIcons.get(mouseInArrowArea ? ICON_ROLLOVER_SELECTED : ICON_ROLLOVER_SELECTED_LINE);
        writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "d1734a91-555c-44fd-b9cb-544ebb1bf963");
        if (null == icon) {
            writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "b8ee93a8-1fa9-4a2c-bbce-6c5d70e31cac");
            Icon orig = regIcons.get(ICON_ROLLOVER_SELECTED);
            writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "325e7dfd-fc95-4e5b-858c-9da560071b66");
            if (null == orig) {
                writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "fc38a4ef-0e74-4aa6-9949-b93121c2eca5");
                orig = regIcons.get(ICON_ROLLOVER);
            }
            writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "70e48264-be37-4faa-bfcc-0a5532faf535");
            if (null == orig) {
                writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "2ecc5671-9397-4a37-a411-bf7918ac0f87");
                orig = regIcons.get(ICON_NORMAL);
            }
            writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "153e29d6-31fe-4881-b164-2154ee8a070a");
            icon = new IconWithArrow(orig, !mouseInArrowArea);
            writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "f41a9418-5d3e-4f4c-aafa-340b527a4078");
            arrowIcons.put(mouseInArrowArea ? ICON_ROLLOVER_SELECTED : ICON_ROLLOVER_SELECTED_LINE, icon);
        }
        writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "2eca8442-8d73-4067-8aef-b2285df4302f");
        return icon;
    }

    JPopupMenu getPopupMenu() {
        writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "58c7752f-c52b-4289-9216-9f5a5c381d73");
        Object menu = getClientProperty(DropDownButtonFactory.PROP_DROP_DOWN_MENU);
        writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "06e2bfed-a76b-4ba0-8abd-0b7744661ccd");
        if (menu instanceof JPopupMenu) {
            writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "4e9f1bf5-a8fe-4855-b7e3-714f7daf4a89");
            return (JPopupMenu) menu;
        }
        writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "bcc8ac72-a8b1-4254-838f-71a13b3fccee");
        return null;
    }

    boolean hasPopupMenu() {
        writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "36a7468a-f232-43a0-acdf-76e6006604a2");
        return null != getPopupMenu();
    }

    private boolean isInArrowArea(Point p) {
        writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "9d440b3a-d769-44d7-bcd4-c76d8036bfd0");
        return p.getLocation().x >= getWidth() - IconWithArrow.getArrowAreaWidth() - getInsets().right;
    }

    @Override
    public void setIcon(Icon icon) {
        writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "20a623e4-6108-4dd0-b971-ec900cfa26ed");
        Icon arrow = updateIcons(icon, ICON_NORMAL);
        writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "b8acaf1d-9fb2-4b90-8d12-db05d233da91");
        arrowIcons.remove(ICON_ROLLOVER_LINE);
        writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "532c4a61-83ac-4830-bf29-c032a83b0830");
        arrowIcons.remove(ICON_ROLLOVER_SELECTED_LINE);
        writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "b48f4ce5-53cd-4bdf-b4a3-66ca2065c42f");
        arrowIcons.remove(ICON_ROLLOVER);
        writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "242cffb9-bffe-42f4-a4d3-81f76718a360");
        arrowIcons.remove(ICON_ROLLOVER_SELECTED);
        writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "90e2bbf5-1dda-4da2-97c0-e6e5c94072cd");
        super.setIcon(hasPopupMenu() ? arrow : icon);
    }

    private Icon updateIcons(Icon orig, String iconType) {
        writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "08667514-704c-455b-8f34-e5c940a0eb22");
        Icon arrow = null;
        writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "af158dd5-60bc-4738-8d0a-4f8551996ae2");
        if (null == orig) {
            writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "a6600318-dd79-4ae2-9098-a124a2eec259");
            regIcons.remove(iconType);
            writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "009c5d2d-89c5-457f-b38d-8d9ced7ec146");
            arrowIcons.remove(iconType);
        } else {
            writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "593442dd-d081-4d4a-9332-cb06ac32ba4f");
            regIcons.put(iconType, orig);
            writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "336b3a26-4b5d-4887-a518-0a09f73c26e7");
            arrow = new ImageIcon(ImageUtilities.icon2Image(new IconWithArrow(orig, false)));
            writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "43ecd94b-dec1-44ed-acc9-2aba05716b9e");
            arrowIcons.put(iconType, arrow);
        }
        writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "b8a1f1e7-2647-460c-8df7-b8fa8bf0ef81");
        return arrow;
    }

    @Override
    public void setPressedIcon(Icon icon) {
        writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "c8d3f945-bc7a-4091-b71b-6e6d34ab4e16");
        Icon arrow = updateIcons(icon, ICON_PRESSED);
        writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "7291272b-4e2f-487d-9f03-fc86a1c49bec");
        super.setPressedIcon(hasPopupMenu() ? arrow : icon);
    }

    @Override
    public void setSelectedIcon(Icon icon) {
        writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "696e1b28-e309-4ca1-ac73-a7c3338c7582");
        Icon arrow = updateIcons(icon, ICON_SELECTED);
        writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "f7d00367-563b-490a-b3df-c776111ee74e");
        super.setSelectedIcon(hasPopupMenu() ? arrow : icon);
    }

    @Override
    public void setRolloverIcon(Icon icon) {
        writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "c250cb04-675e-4085-9a90-7a3b29be0f55");
        Icon arrow = updateIcons(icon, ICON_ROLLOVER);
        writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "4f400fc5-95e3-44e9-8822-8e989c354342");
        arrowIcons.remove(ICON_ROLLOVER_LINE);
        writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "59992187-5ab7-401a-b9d3-b26595eb963a");
        arrowIcons.remove(ICON_ROLLOVER_SELECTED_LINE);
        writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "121570ca-538d-490c-9040-91a3c92209d1");
        super.setRolloverIcon(hasPopupMenu() ? arrow : icon);
    }

    @Override
    public void setRolloverSelectedIcon(Icon icon) {
        writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "f364211d-7665-4811-9965-26647ffa5ba9");
        Icon arrow = updateIcons(icon, ICON_ROLLOVER_SELECTED);
        writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "a400e3ba-cff7-4427-9017-9bf0ffdb5005");
        arrowIcons.remove(ICON_ROLLOVER_SELECTED_LINE);
        writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "4db6e63e-abe3-447d-bf2f-12b10d37ebd2");
        super.setRolloverSelectedIcon(hasPopupMenu() ? arrow : icon);
    }

    @Override
    public void setDisabledIcon(Icon icon) {
        writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "e8370246-7dec-45b8-9a8c-d706047bba9b");
        // TODO use 'disabled' arrow icon
        Icon arrow = updateIcons(icon, ICON_DISABLED);
        writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "903b2b67-3c24-4852-807f-c347d6dbbeef");
        super.setDisabledIcon(hasPopupMenu() ? arrow : icon);
    }

    @Override
    public void setDisabledSelectedIcon(Icon icon) {
        writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "92fc72ca-2dac-43d2-a521-5c8d2d4e9709");
        // TODO use 'disabled' arrow icon
        Icon arrow = updateIcons(icon, ICON_DISABLED_SELECTED);
        writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "8e32d77c-4c18-4570-b0f7-565b991344a0");
        super.setDisabledSelectedIcon(hasPopupMenu() ? arrow : icon);
    }

    @Override
    public void setText(String text) {
        writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "18fe318f-3a7a-4341-8aba-a5a7ed7f7420");
        // does nothing
        // NOI18N
        Logger.getLogger(DropDownToggleButton.class.getName()).log(Level.FINER, "DropDownToggleButton cannot display text.");
    }

    @Override
    public String getText() {
        writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "cb97da7d-3fbb-4df2-ac9c-0ce3b7b61ad8");
        return null;
    }

    private class Model extends JToggleButton.ToggleButtonModel {

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