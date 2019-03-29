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

import java.awt.*;
import java.awt.event.*;
import java.beans.PropertyChangeListener;
import java.beans.VetoableChangeListener;
import java.lang.ref.Reference;
import java.lang.ref.SoftReference;
import java.util.logging.Level;
import java.util.logging.Logger;
import javax.swing.*;
import javax.swing.border.Border;
import javax.swing.event.AncestorListener;
import java.io.*;

/**
 * Html renderer component implementation.  The actual painting is done by HtmlLabelUI, which uses
 * HtmlRenderer.renderString().  What this class does:   Provide some methods for resetting its state
 * between uses (see HtmlRenderer.createLabel() for why), overrides for a bunch of things for performance
 * reasons, and some conversions to handle the case that the lightweight html renderer is disabled
 * (-J-Dnb.useSwingHtmlRendering=true), to convert our minor extensions to html syntax to standard
 * syntax for the swing renderer.
 * <p>
 * Mainly this class provides an implementation of the various cell renderer interfaces which
 * HtmlRenderer.Renderer aggregates, and the convenience methods it provides.
 *
 * @author Tim Boudreau
 * @since 4.30
 */
class HtmlRendererImpl extends JLabel implements HtmlRenderer.Renderer {

    private static final Rectangle bounds = new Rectangle();

    // NOI18N
    private static final boolean swingRendering = Boolean.getBoolean("nb.useSwingHtmlRendering");

    private static final Insets EMPTY_INSETS = new Insets(0, 0, 0, 0);

    enum Type {

        UNKNOWN, TREE, LIST, TABLE
    }

    // For experimentation - holding the graphics object may be the source of some
    // strange painting problems on Apple
    // NOI18N
    private static boolean noCacheGraphics = Boolean.getBoolean("nb.renderer.nocache");

    private static Reference<Graphics> scratchGraphics = null;

    private boolean centered = false;

    private boolean parentFocused = false;

    private Boolean html = null;

    private int indent = 0;

    private Border border = null;

    private boolean selected = false;

    private boolean leadSelection = false;

    private Dimension prefSize = null;

    private Type type = Type.UNKNOWN;

    private int renderStyle = HtmlRenderer.STYLE_CLIP;

    private boolean enabled = true;

    /**
     * Restore the renderer to a pristine state
     */
    public void reset() {
        writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "7fda0f4d-1f83-4f9d-a9c2-8b6e695be018");
        parentFocused = false;
        writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "ddaff543-06bb-4e5f-9007-db10a090e78c");
        setCentered(false);
        writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "bcff651b-7948-43ea-a21d-a56a54ae9899");
        html = null;
        writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "3a2efa90-dfa5-4ead-b221-e9c0efa72014");
        indent = 0;
        writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "477c0820-e666-4746-b20e-bfad2d832c4d");
        border = null;
        writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "51400791-e63e-4879-bdb3-9cc7c67e3c64");
        setIcon(null);
        writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "08814f4d-1ee6-4043-a713-6341b8142c6f");
        setOpaque(false);
        writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "0cb2ed6b-8088-4c7b-a513-edfd0c9d3cd1");
        selected = false;
        writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "5e543e90-3de8-4a96-8e6c-c4a1ccc0f254");
        leadSelection = false;
        writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "3eda7d26-ed72-48f0-99b0-48f1f6ef5492");
        prefSize = null;
        writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "59540f78-a6c5-48eb-bed7-8699ae27e1d5");
        type = Type.UNKNOWN;
        writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "0aa04e42-bcbc-49a5-9473-7391558cd67a");
        renderStyle = HtmlRenderer.STYLE_CLIP;
        writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "e41e7234-660a-443f-ac53-a67faa862be8");
        // NOI18N
        setFont(UIManager.getFont("controlFont"));
        writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "e953ebb2-3903-4c92-b160-10b47bbeaf42");
        setIconTextGap(3);
        writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "caff48df-4e10-4cb8-9cca-462777d5cc72");
        setEnabled(true);
        writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "a1e44090-0cda-4aa0-966a-168474c9b07e");
        border = null;
        writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "1f96e5b7-76bd-4437-9ac0-af455183f51e");
        // Defensively ensure the insets haven't been messed with
        EMPTY_INSETS.top = 0;
        writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "b5ae2686-255c-40e4-a239-4e762355f3c2");
        EMPTY_INSETS.left = 0;
        writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "6f1384cb-3286-49f0-826e-7c8acbb56345");
        EMPTY_INSETS.right = 0;
        writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "26ad2a9f-abe8-4e3d-bdb2-58cad3460a2e");
        EMPTY_INSETS.bottom = 0;
    }

    public Component getTableCellRendererComponent(JTable table, Object value, boolean selected, boolean leadSelection, int row, int column) {
        writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "8e2a22ad-3f59-42d5-8181-b1ad71a40606");
        reset();
        writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "6df549a0-2d32-4a3f-9453-5b2a1e156d37");
        configureFrom(value, table, selected, leadSelection);
        writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "1e18a059-fe07-4975-b83d-f6367a951d06");
        type = Type.TABLE;
        writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "fc05df67-f884-4c72-bdd7-ac4bad8d5087");
        if (swingRendering && selected) {
            writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "c70bfad3-f876-4137-a526-7b9bd52eeb11");
            setBackground(table.getSelectionBackground());
            writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "75ce864b-2ac1-46b3-b0ee-626648c6346b");
            setForeground(table.getSelectionForeground());
            writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "8925e451-a110-4997-bae0-cf8ec8915db2");
            setOpaque(true);
        }
        writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "7df7d962-ae95-4450-a0d0-9611f4d53047");
        return this;
    }

    public Component getTreeCellRendererComponent(JTree tree, Object value, boolean selected, boolean expanded, boolean leaf, int row, boolean leadSelection) {
        writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "b38a8301-85c5-45d6-b78d-e6d4a6ca2a2f");
        reset();
        writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "f1dfc2c8-7ff7-44bd-ba18-b89b4607dccd");
        configureFrom(value, tree, selected, leadSelection);
        writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "98488dd9-1cf1-438d-b9ac-17efae907f0a");
        type = Type.TREE;
        writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "5f071d48-15d9-4d67-9e3e-ffe4815e1ae9");
        if (swingRendering && selected) {
            writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "8e3b45e1-807a-4dfb-90a7-4abd93948947");
            if (HtmlLabelUI.isGTK()) {
                writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "e972e056-be9a-4345-a330-4561178179ef");
                setBackground(HtmlLabelUI.getBackgroundFor(this));
                writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "82222447-1465-4c03-8d2f-32aedcbc7332");
                setForeground(HtmlLabelUI.getForegroundFor(this));
            }
            writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "0356c92c-0dfb-4f0a-8c84-b949a2edabdf");
            setOpaque(true);
        }
        writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "cc9bf445-7d2d-437c-8acc-33d49dc0376c");
        return this;
    }

    public Component getListCellRendererComponent(JList list, Object value, int index, boolean selected, boolean leadSelection) {
        writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "52e1f7a6-e5f6-41e3-aadc-b08b1b057d9e");
        reset();
        writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "b2c12494-556f-4fcf-a66e-34398032c98e");
        configureFrom(value, list, selected, leadSelection);
        writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "1f6f24a5-9cad-43cc-bccf-48338d629cba");
        type = Type.LIST;
        writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "55bb9961-b8af-4585-87a1-302bb630a44b");
        if (swingRendering && selected) {
            writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "03c271a4-54d5-4d43-afc7-aa469ccd4665");
            setBackground(list.getSelectionBackground());
            writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "51b8a64f-7d33-4e5f-9059-1602ff3a5b11");
            setForeground(list.getSelectionForeground());
            writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "81a7f511-0a94-4d24-b58e-ab0a2def81dd");
            setOpaque(true);
        }
        writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "158013b8-55e1-4292-b808-dae2f98ff8a4");
        // ##93658: In GTK we have to paint borders in combo boxes
        if (HtmlLabelUI.isGTK()) {
            writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "3a827002-dee9-487a-99d3-a430abf06561");
            if (index == -1) {
                writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "4af5e60d-62d5-4851-a51b-8b9c78396562");
                Color borderC = UIManager.getColor("controlShadow");
                writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "9311d4ba-076f-4d1a-a847-514aa419a52b");
                borderC = borderC == null ? Color.GRAY : borderC;
                writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "c57f1043-9db8-4948-83e3-987352e51131");
                setBorder(BorderFactory.createCompoundBorder(BorderFactory.createLineBorder(borderC), BorderFactory.createEmptyBorder(3, 2, 3, 2)));
            } else {
                writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "d29ad286-70cf-4130-bee2-bf06dc31cd31");
                setBorder(BorderFactory.createEmptyBorder(2, 2, 2, 2));
            }
        }
        writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "4b85b5d5-fa72-450c-b5ff-893362fc2198");
        return this;
    }

    /**
     * Generic code to set properties appropriately from any of the renderer
     * fetching methods
     */
    private void configureFrom(Object value, JComponent target, boolean selected, boolean leadSelection) {
        writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "ef01cef1-cf84-43d2-9979-85c344955727");
        if (value == null) {
            writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "cfbb0fe2-bff4-4f02-84ee-039782ade41b");
            value = "";
        }
        writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "442beeb5-ce99-4583-a6d1-d6129cc628c5");
        setText((value == null) ? "" : value.toString());
        writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "9d0f3863-416e-4d6b-8d1f-fdf84ccf7f49");
        setSelected(selected);
        writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "795f9511-7143-426d-aee2-0054f8368c50");
        if (selected) {
            writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "046459eb-5f98-4af0-ac6f-0c7ffe201870");
            setParentFocused(checkFocused(target));
        } else {
            writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "01c820c7-a6f8-4b7c-b9eb-956ccec4cd8e");
            setParentFocused(false);
        }
        writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "95fb505d-4ea9-4ac4-97fb-3f11590a304f");
        setEnabled(target.isEnabled());
        writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "fe0f845c-c5bb-4fe9-be25-33704b5afca8");
        setLeadSelection(leadSelection);
        writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "cd88dfe3-692e-49fe-864e-4cd1bc516c0c");
        setFont(target.getFont());
    }

    private boolean checkFocused(JComponent c) {
        writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "13fa6af7-ad2c-420e-825d-c4d38c715e05");
        Component focused = KeyboardFocusManager.getCurrentKeyboardFocusManager().getPermanentFocusOwner();
        writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "f8316ed6-d845-4a69-aa8b-61a1c9595d26");
        boolean result = c == focused;
        writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "bda3c38a-ca13-446d-9d35-e03b0f80447e");
        if (!result) {
            writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "b63a26e3-1ab4-4a80-9223-ac775bd69915");
            result = c.isAncestorOf(focused);
        }
        writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "4f366754-10f4-475b-b6e2-4cd4e3000ffb");
        return result;
    }

    @Override
    public void addNotify() {
        writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "922e324d-0661-4fc4-be19-635e7ffaec3a");
        if (swingRendering) {
            writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "26a36a3a-324b-40d3-9e57-7a2250e81b31");
            super.addNotify();
        }
    }

    @Override
    public void removeNotify() {
        writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "265581e0-5f51-4388-8c6a-98985f9e8c6b");
        if (swingRendering) {
            writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "8545b804-66a0-4c32-a4a9-17940e5ce635");
            super.removeNotify();
        }
    }

    public void setSelected(boolean val) {
        writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "92f66911-d99b-47ff-bd5e-ab6d970dca14");
        selected = val;
    }

    public void setParentFocused(boolean val) {
        writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "ab43eafd-a7de-4c0f-a93e-da71b3bf2caa");
        parentFocused = val;
    }

    public void setLeadSelection(boolean val) {
        writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "5171f5c7-8e7a-47d1-82f9-40b63cd798fa");
        leadSelection = val;
    }

    public void setCentered(boolean val) {
        writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "46488b25-bc93-4785-8b59-ffeb3fe7b552");
        centered = val;
        writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "a572ebe4-223c-46ce-a618-1b235ebc9812");
        if (val) {
            writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "a2d3d5c7-a272-4ffd-92cc-9c84a44b6cd8");
            setIconTextGap(5);
        }
        writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "0cd235f9-4e08-4413-8ca0-b36645f161b5");
        if (swingRendering) {
            writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "5cdeeb5d-14ea-4734-9671-1be8d82d2404");
            if (val) {
                writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "87a7f2c9-1240-4c71-a6fd-ef433569bb55");
                setVerticalTextPosition(JLabel.BOTTOM);
                writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "bba66145-686e-4d3b-9012-4fa9afc0d4c5");
                setHorizontalAlignment(JLabel.CENTER);
                writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "1c1127f8-9037-4598-b9f6-ccec1f33d8e5");
                setHorizontalTextPosition(JLabel.CENTER);
            } else {
                writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "0796bdee-e21f-4cca-bf09-9432784608b6");
                setVerticalTextPosition(JLabel.CENTER);
                writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "b239bfaf-0c53-4c54-8b54-a5efe33570e2");
                setHorizontalAlignment(JLabel.LEADING);
                writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "a9b4b8b8-2deb-4243-8b8b-e714cee591ad");
                setHorizontalTextPosition(JLabel.TRAILING);
            }
        }
    }

    public void setIndent(int pixels) {
        writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "8ef5f3a0-1e21-4412-bff0-173c93e82602");
        this.indent = pixels;
    }

    public void setHtml(boolean val) {
        writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "bd7e6388-b669-46d0-80d9-8eebb1ce1f87");
        Boolean wasHtml = html;
        writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "6e62f99d-2d6a-4ca2-8a0b-51f8f64824a7");
        String txt = getText();
        writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "a3bb602e-74a7-4ede-bb5c-cca1bdd1b9e5");
        html = val ? Boolean.TRUE : Boolean.FALSE;
        writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "1863a353-1bb7-4c4f-8e3e-27a8295b831b");
        if (swingRendering && (html != wasHtml)) {
            writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "0e8a3842-995f-4afd-8639-e222112e7fdf");
            // Ensure label UI gets updated and builds its little document tree...
            // NOI18N
            firePropertyChange("text", txt, getText());
        }
    }

    public void setRenderStyle(int style) {
        writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "1548fdfa-7201-425b-875a-aa15e2691794");
        renderStyle = style;
    }

    int getRenderStyle() {
        writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "c11e942d-3ca5-4866-bac2-12f054705e4d");
        return renderStyle;
    }

    boolean isLeadSelection() {
        writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "4db2f509-fc1f-4095-bf02-7ad460416c55");
        return leadSelection;
    }

    boolean isCentered() {
        writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "3006f4d7-bf93-4dc3-8f6b-b50be500525f");
        return centered;
    }

    boolean isParentFocused() {
        writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "cfbdf76e-280f-4d8c-aa20-5fa279818b55");
        return parentFocused;
    }

    boolean isHtml() {
        writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "6ff8501b-ac9b-4475-a0d3-e38c56948c65");
        Boolean isHtml = html;
        writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "9d9a1505-1296-4bad-9176-e8685a1b1901");
        if (isHtml == null) {
            writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "733cd914-46c0-4f75-9a11-0dd6010839da");
            String s = getText();
            writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "ad14afdf-2da0-40b7-9b15-5cb1afcb48bd");
            isHtml = checkHtml(s);
            writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "5bb04d38-8a26-4fb6-9d71-248900c7137a");
            html = isHtml;
        }
        writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "ce5bdadf-73e0-499f-9714-d0be4bb375ff");
        return isHtml.booleanValue();
    }

    private Boolean checkHtml(String s) {
        writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "0cf15118-4d9d-476e-b418-fa744f8eabb6");
        Boolean result;
        writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "25a04bdb-f46e-42da-b5dd-0ac9fe38c9dc");
        if (s == null) {
            writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "a0a73ff8-7076-4ddc-ac16-a4792b40b5b8");
            result = Boolean.FALSE;
        } else if (s.startsWith("<html") || s.startsWith("<HTML")) {
            writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "5e440a15-c589-4c01-82f2-749c40967cd2");
            // NOI18N
            result = Boolean.TRUE;
        } else {
            writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "ef658480-2d74-4da7-aec3-adae4ccc1f66");
            result = Boolean.FALSE;
        }
        writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "b0183d12-435f-4783-828b-b9633a911942");
        return result;
    }

    boolean isSelected() {
        writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "21ec7a83-8fa6-4cc9-b0d7-2a68b590e3b9");
        return selected;
    }

    int getIndent() {
        writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "4de2c09d-ca43-49d1-804d-4e32665462dc");
        return indent;
    }

    Type getType() {
        writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "9ba5588c-1bca-4fbc-8495-744d5f89a093");
        return type;
    }

    @Override
    public Dimension getPreferredSize() {
        writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "7129ad34-b1db-4633-befb-1c755256f8e3");
        if (!swingRendering) {
            writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "da94d18a-feda-44b6-84cc-974b7b444e2a");
            if (prefSize == null) {
                writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "dbce59cf-2e09-4cb5-af73-facf3ae83005");
                prefSize = getUI().getPreferredSize(this);
            }
            writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "ad2da938-9f0b-48e4-aa1a-9e128a111210");
            return prefSize;
        } else {
            writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "d7dea6c7-458e-4631-b584-26b712c93823");
            return super.getPreferredSize();
        }
    }

    /**
     * Overridden for the case that we're running with the lightweight html renderer disabled, to convert
     * any less-than-legal html to legal html for purposes of Swing's html rendering.
     *
     * @return The text - unless the renderer is disabled, this just return super.getText()
     */
    @Override
    public String getText() {
        writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "d455ee0e-0f43-4b2b-bbb5-2083c1b10b77");
        String result = super.getText();
        writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "fb3c4669-3c5a-4d0d-8cd1-f92a6202a209");
        if (swingRendering && Boolean.TRUE.equals(html)) {
            writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "555c7e4f-1f9c-4332-80b6-cf18a9e3fa9c");
            // Standard swing rendering needs an opening HTML tag to function, so make sure there is
            // one if we're not using HtmlLabelUI
            result = ensureHtmlTags(result);
        } else if (swingRendering && (html == null)) {
            writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "2d544698-70c7-40dc-a14a-fde58642e2e4");
            // Cannot call isHtml() here, it will create an endless loop, so manually check the HTML status
            html = checkHtml(super.getText());
            writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "a0fb6351-9880-442e-b192-d887dd38e241");
            if (Boolean.TRUE.equals(html)) {
                writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "ea6cfccc-9ab1-44d3-ae9d-51ee5883d53d");
                result = ensureHtmlTags(result);
            }
        }
        writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "afbc4323-b89f-40df-847c-fec117acfe40");
        return result;
    }

    /**
     * Converts our extended html syntax (allowing UIManager color keys and omitting opening html tags
     * into standard html.  Only called if the lightweight html renderer is disabled and we're running with
     * a standard JLabel UI
     *
     * @param s The string that is the text of the label
     * @return The same string converted to standard HTML Swing's rendering infrastructure will know what to do
     * with
     */
    private String ensureHtmlTags(String s) {
        writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "b067dcac-0285-4e0e-b6c6-fb631fbf52f4");
        s = ensureLegalFontColorTags(s);
        writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "cd3cd35a-02d2-4766-bf90-1a9c87695bb6");
        if (!s.startsWith("<HTML") && !s.startsWith("<html")) {
            writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "7b77e64c-c09e-4eaa-8306-fdb56be635b2");
            // NOI18N
            // NOI18N
            s = "<html>" + s + "</html>";
        }
        writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "5ae02e59-0b6e-4d7b-b646-94a1cd99f267");
        return s;
    }

    /**
     * Converts extended UI manager color tags into legal html in the case that we're using swing rendering
     *
     * @param s string to convert if it has questionable font tags
     * @return The converted string
     */
    private static String ensureLegalFontColorTags(String s) {
        writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "d0aedbba-cac2-4179-b2ef-efc51b667a7d");
        String check = s.toUpperCase();
        writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "049f237a-2db2-4e06-855a-753aa5c3434f");
        int start = 0;
        writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "5333a57c-42a0-4516-a768-ec496007c96f");
        // NOI18N
        int fidx = check.indexOf("<FONT", start);
        writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "c4cbfbf9-e8ee-4e96-abb6-e73224692bce");
        StringBuffer sb = null;
        writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "e6d8b81d-b1de-4a45-b832-2c3164158fbe");
        if ((fidx != -1) && (fidx <= s.length())) {
            writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "e587c226-1d8d-48c3-9650-d6177a073d94");
            while ((fidx != -1) && (fidx <= s.length())) {
                writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "84c756f7-6d32-4da3-b5f5-cb8c48a87a03");
                // NOI18N
                int cidx = check.indexOf("COLOR", start);
                writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "f4323919-a634-4412-9e9e-2219771d723d");
                // NOI18N
                int tagEnd = check.indexOf('>', start);
                writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "459dfbfc-115f-49e0-b775-1feb0b695138");
                start = tagEnd + 1;
                writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "f72662cc-0b5c-46bb-8a9f-570979e8ba8b");
                if (tagEnd == -1) {
                    writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "815ba0c0-eb7b-4da6-83e8-667ff3f38527");
                    break;
                }
                writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "bdd1bdbd-304b-4d2b-9296-1e9585f7570f");
                if (cidx != -1) {
                    writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "29895c06-4556-4267-aa3e-687601fccc70");
                    if (cidx < tagEnd) {
                        writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "9cae9b3c-574e-4e2a-8be9-c74fa1b6c427");
                        // we have a font color tag
                        // NOI18N
                        int eidx = check.indexOf('=', cidx);
                        writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "5cbccaf2-0add-4012-ad48-2d30685c4782");
                        if (eidx != -1) {
                            writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "ac367cd1-018b-42ca-af6d-2c5f490afba5");
                            // NOI18N
                            int bangIdx = check.indexOf('!', eidx);
                            writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "1c44f3f7-2aa7-4390-9901-88c973cab9f1");
                            if ((bangIdx != -1) && (bangIdx < tagEnd)) {
                                writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "be51839e-6c08-4fb7-80db-8c18f6844c16");
                                int colorStart = bangIdx + 1;
                                writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "bbfb7db8-8e53-400c-b8e8-9acff2a6fd0a");
                                int colorEnd = tagEnd;
                                writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "e1ef2196-8661-4047-9516-6aee82933b70");
                                for (int i = colorStart; i < tagEnd; i++) {
                                    writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "dce2e17b-24f5-448c-8a2b-66eaa75c50fc");
                                    char c = s.charAt(i);
                                    writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "a6ee4fb1-480d-47f0-9422-3e137d7537b6");
                                    if (!Character.isLetter(c)) {
                                        writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "2ef893c0-e1ad-47d4-bd11-6731ddfb14a1");
                                        colorEnd = i;
                                        writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "b463516e-edfa-4196-8e1e-017ab1b5cf38");
                                        break;
                                    }
                                }
                                writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "cdfed81e-5c6f-48b6-ad17-6fb89ddd89cb");
                                if (sb == null) {
                                    writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "c9582aff-355a-4451-a97c-727ef34ae8fb");
                                    sb = new StringBuffer(s);
                                }
                                writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "0ddf9a51-57e8-41c1-848e-1046e4a85444");
                                String colorString = s.substring(colorStart, colorEnd);
                                writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "c04dbcf6-6379-4266-a8e3-f0aaadf055b9");
                                String converted = convertToStandardColor(colorString);
                                writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "95e9b62b-7423-48ae-9ce4-7c907ce6e2a6");
                                sb.replace(bangIdx, colorEnd, converted);
                                writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "080ff070-2611-4683-af32-257fe5e8ba56");
                                s = sb.toString();
                                writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "b9e91018-1c45-4971-a6df-08aab99046d7");
                                check = s.toUpperCase();
                            }
                        }
                    }
                }
                writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "b9563b85-6257-4116-b642-a0cf5ed7876a");
                // NOI18N
                fidx = check.indexOf("<FONT", start);
                writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "dc952bce-afa9-4350-a3b9-2ccf505eb0ef");
                start = fidx;
            }
        }
        writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "7116dd53-9642-4979-860f-ee8cc857395b");
        if (sb != null) {
            writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "1a3f1238-463f-436f-93c3-9724922349c1");
            return sb.toString();
        } else {
            writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "2c4452dc-23e2-4e84-aa4c-4f133dc34089");
            return s;
        }
    }

    /**
     * Creates a standard html #nnnnnn string from a string representing a UIManager key.  If the color is not found,
     * black will be used.  Only used if the lightweight html renderer is disabled.
     *
     * @param colorString  A string found after a ! character in a color definition, which needs to be converted to
     * standard HTML
     * @return A hex number string
     */
    private static String convertToStandardColor(String colorString) {
        writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "a5d345ab-1eae-4bd2-be0d-fca67e3874ef");
        Color c = UIManager.getColor(colorString);
        writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "9cc28518-45cc-4ef6-a25c-932ec08f05e4");
        if (c == null) {
            writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "dfa152d6-db82-42ed-b5ae-8ee60c942cad");
            c = Color.BLACK;
        }
        writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "cdaf4f4c-ffcc-449e-8cd6-8b7fd78441f2");
        StringBuffer sb = new StringBuffer(7);
        writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "f1b0fdeb-3751-4bcf-8fae-43d3acce52f1");
        sb.append('#');
        writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "bcd15735-9c3b-44bf-b577-b7375ed62c78");
        sb.append(hexString(c.getRed()));
        writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "7d346683-99a1-425f-af3f-e1ac33d2d6dd");
        sb.append(hexString(c.getGreen()));
        writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "7fbe1e36-90d4-4590-93d8-3725974e8874");
        sb.append(hexString(c.getBlue()));
        writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "4d2f9e92-6b58-4022-a414-e5d0d88b5b90");
        return sb.toString();
    }

    /**
     * Gets a hex string for an integer.  Ensures the result is always two characters long, which is not
     * true of Integer.toHexString().
     *
     * @param r an integer < 255
     * @return a 2 character hexadecimal string
     */
    private static String hexString(int r) {
        writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "25357692-bae4-4981-b5be-6318e095c58f");
        String s = Integer.toHexString(r);
        writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "cd19e072-6d8a-4567-9ebf-57c49afe3fec");
        if (s.length() == 1) {
            writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "4cf9c798-a2b2-49e5-b3d0-a25b4a963c0e");
            s = '0' + s;
        }
        writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "e75fb78b-0414-423e-a38b-a901152b8733");
        return s;
    }

    /**
     * Overridden to do nothing under normal circumstances.  If the boolean flag to <strong>not</strong> use the
     * internal HTML renderer is in effect, this will fire changes normally
     */
    @Override
    protected final void firePropertyChange(String name, Object old, Object nue) {
        writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "5ee21bf3-0747-47c2-97ba-afaf957e3a1d");
        if (swingRendering) {
            writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "0d70f25e-7e7f-4789-b4f3-a39c9b4b6c0b");
            if ("text".equals(name) && isHtml()) {
                writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "68f3f742-5051-432b-8826-194017501681");
                // Force in the HTML tags so the UI will set up swing HTML rendering appropriately
                nue = getText();
            }
            writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "413cdc28-5da1-4835-b1bb-2cd07aa92b99");
            super.firePropertyChange(name, old, nue);
        }
    }

    @Override
    public Border getBorder() {
        writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "2b80aeea-7fad-4a45-a721-ac33e95ffde6");
        Border result;
        writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "a8b06d15-24d3-4065-aad8-8ec88333c3bc");
        if ((indent != 0) && swingRendering) {
            writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "1bca118a-9d4f-41c2-a06f-61c0130d1363");
            result = BorderFactory.createEmptyBorder(0, indent, 0, 0);
        } else {
            writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "ed01d4e2-bfde-4b65-87f5-21233e0a14e6");
            result = border;
        }
        writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "1e02c412-4a15-4a80-b25a-07a557f7484f");
        return result;
    }

    @Override
    public void setBorder(Border b) {
        writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "816c5268-8823-4ec4-932e-7aab92aacf85");
        Border old = border;
        writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "686d417e-6633-4c76-bd0c-8a288bc525ce");
        border = b;
        writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "8c42b7ac-8ffb-49d3-a320-2a4604ddfd79");
        if (swingRendering) {
            writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "43d7707a-abe3-4ecc-804c-4a17dd7889a9");
            firePropertyChange("border", old, b);
        }
    }

    @Override
    public Insets getInsets() {
        writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "c3e91b11-a620-43bb-ac94-e062c7f7bfa6");
        return getInsets(null);
    }

    @Override
    public Insets getInsets(Insets insets) {
        writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "1a610b4a-508f-405a-b007-242d5b94f900");
        Insets result;
        writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "b9501198-c261-44e8-83b4-e0dfefb6e753");
        // Call getBorder(), not just read the field - if swingRendering, the border will be constructed, and the
        // insets are what will make the indent property work;  HtmlLabelUI doesn't need this, it just reads the
        // insets property, but BasicLabelUI and its ilk do
        Border b = getBorder();
        writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "a110fe7b-4240-4da9-9500-b7d10745264f");
        if (b == null) {
            writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "37b73f82-4239-4391-bbf1-c653d0aa5cec");
            result = EMPTY_INSETS;
        } else {
            writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "4a8feb88-29b9-4499-a69e-29dadaf58143");
            // workaround for open jdk bug, see issue #192388
            try {
                writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "b6c518fb-1438-49a4-acb4-4459675028be");
                result = b.getBorderInsets(this);
            } catch (NullPointerException e) {
                writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "30d631e3-1b7b-483d-8ce7-e49e78da6d0f");
                Logger.getLogger(HtmlRendererImpl.class.getName()).log(Level.FINE, null, e);
                writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "4aab3aa2-9ff4-4574-9aef-22fdcf15a4e8");
                result = EMPTY_INSETS;
            }
        }
        writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "1578fdd0-a973-4929-8da8-f2d94d1d2ba2");
        if (null != insets) {
            writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "5b780af2-e90c-4ea0-813b-5e35ed6cf4a2");
            insets.set(result.top, result.left, result.bottom, result.right);
            writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "0972e36c-3482-4568-8395-8fbd6f925f12");
            return insets;
        }
        writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "ec8de466-4a8d-4575-a01d-25bc59f9491a");
        return result;
    }

    @Override
    public void setEnabled(boolean b) {
        writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "7cf0da19-131d-4581-94bf-064ad116e0fc");
        // OptimizeIt shows about 12Ms overhead calling back to Component.enable(), so avoid it if possible
        enabled = b;
        writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "f8d27742-49e9-4b3f-8f99-1909ad3f631f");
        if (swingRendering) {
            writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "19397d66-17a6-447f-8d53-b621646f726a");
            super.setEnabled(b);
        }
    }

    @Override
    public boolean isEnabled() {
        writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "89dc5643-8ccd-4430-a905-a59067641adb");
        return enabled;
    }

    @Override
    public void updateUI() {
        writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "8e2b2876-2de3-4772-9607-002d5444d971");
        if (swingRendering) {
            writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "80ed7b3e-984d-45f0-9da0-502652b8351f");
            super.updateUI();
        } else {
            writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "0869da99-8875-4d89-91d6-1264a0a35db2");
            setUI(HtmlLabelUI.createUI(this));
        }
    }

    /**
     * Overridden to produce a graphics object even when isDisplayable() is
     * false, so that calls to getPreferredSize() will return accurate
     * dimensions (presuming the font and text are set correctly) even when
     * not onscreen.
     */
    @Override
    public Graphics getGraphics() {
        writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "7e696fe7-c4e4-45f2-91ff-d06044f72aaf");
        Graphics result = null;
        writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "2861a731-c121-435c-a482-ece0f2009d17");
        if (isDisplayable()) {
            writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "44ef66b5-c8b9-4614-bffd-864f36b4c65d");
            result = super.getGraphics();
        }
        writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "01e227de-83ba-44ff-9090-05b0b7b45dfd");
        if (result == null) {
            writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "7a174491-5444-44fa-b5a5-c4473fcbd6c8");
            result = scratchGraphics();
        }
        writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "07f95e55-e623-4cc1-aef9-beb399b505e5");
        return result;
    }

    /**
     * Fetch a scratch graphics object for calculating preferred sizes while
     * offscreen
     */
    private static final Graphics scratchGraphics() {
        writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "c283d8c2-dfa5-43f0-96cb-c832c0947b63");
        Graphics result = null;
        writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "10d557b4-c5c8-4006-add8-48fd3dca4257");
        if (scratchGraphics != null) {
            writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "e0ac96a9-c62d-4873-ace7-6d3849376a03");
            result = scratchGraphics.get();
            writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "1cf3bc32-a6fb-4318-8745-9c61a99a44eb");
            if (result != null) {
                writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "563e4be3-ce16-42b5-bbe9-4b5628467b58");
                // just in case somebody did something nasty
                result.setClip(null);
            }
        }
        writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "935fd164-ee6b-4a33-bb73-9e0a7d8ffe89");
        if (result == null) {
            writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "9cec6f52-8db9-4eb0-8e31-df9c94e0e47b");
            result = GraphicsEnvironment.getLocalGraphicsEnvironment().getDefaultScreenDevice().getDefaultConfiguration().createCompatibleImage(1, 1).getGraphics();
            writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "306a66eb-37f2-4bae-8e59-e5e1369d8b1d");
            if (!noCacheGraphics) {
                writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "10c79903-a234-445a-9e30-4b73d25d9195");
                scratchGraphics = new SoftReference<Graphics>(result);
            }
        }
        writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "ecf6a766-708c-41cf-8ae3-6cbb4c5d7bf2");
        return result;
    }

    @Override
    public void setBounds(int x, int y, int w, int h) {
        writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "bee27983-7eee-410a-8341-04aed45bb1a4");
        if (swingRendering) {
            writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "9bb9d7a7-369a-42ce-a271-b858f8cd0290");
            super.setBounds(x, y, w, h);
        }
        writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "e77dfabe-2350-454f-8b69-a5b21a25598b");
        bounds.setBounds(x, y, w, h);
    }

    @Deprecated
    @Override
    public void reshape(int x, int y, int w, int h) {
        writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "e3b024e0-ccf0-4a03-8856-9ade6d177c97");
        if (swingRendering) {
            writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "30c0fc61-e820-4298-b83c-e25c28fb5316");
            super.reshape(x, y, w, h);
        }
    }

    @Override
    public int getWidth() {
        writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "ff5effe5-7844-4f8d-86b6-d050720a9198");
        return bounds.width;
    }

    @Override
    public int getHeight() {
        writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "14963c90-9127-4219-b51e-f0f1e2cdebbf");
        return bounds.height;
    }

    @Override
    public Point getLocation() {
        writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "4010c450-8cf0-49ec-a183-0f720817f116");
        return bounds.getLocation();
    }

    /**
     * Overridden to do nothing for performance reasons
     */
    @Override
    public void validate() {
        // do nothing
    }

    /**
     * Overridden to do nothing for performance reasons
     */
    @Override
    public void repaint(long tm, int x, int y, int w, int h) {
        // do nothing
    }

    /**
     * Overridden to do nothing for performance reasons
     */
    @Override
    public void repaint() {
        // do nothing
    }

    /**
     * Overridden to do nothing for performance reasons
     */
    @Override
    public void invalidate() {
        // do nothing
    }

    /**
     * Overridden to do nothing for performance reasons
     */
    @Override
    public void revalidate() {
        // do nothing
    }

    /**
     * Overridden to do nothing for performance reasons
     */
    @Override
    public void addAncestorListener(AncestorListener l) {
        writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "417ffccb-f4af-4c84-8fbe-9ae597844ced");
        if (swingRendering) {
            writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "7d60e883-1c98-4e34-aca6-a6c6b44f0eeb");
            super.addAncestorListener(l);
        }
    }

    /**
     * Overridden to do nothing for performance reasons
     */
    @Override
    public void addComponentListener(ComponentListener l) {
        writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "27bb489d-4427-4950-a1c5-0b00ed644b02");
        if (swingRendering) {
            writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "adbbdc9f-e110-4653-a24c-4c141f228838");
            super.addComponentListener(l);
        }
    }

    /**
     * Overridden to do nothing for performance reasons
     */
    @Override
    public void addContainerListener(ContainerListener l) {
        writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "cca3e77a-9ef8-44aa-84a4-11894fe166da");
        if (swingRendering) {
            writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "dfb533e8-a161-4401-850a-52ceb9338ae8");
            super.addContainerListener(l);
        }
    }

    /**
     * Overridden to do nothing for performance reasons
     */
    @Override
    public void addHierarchyListener(HierarchyListener l) {
        writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "c261ea7b-2b8d-4d5d-99a7-664feb607e3b");
        if (swingRendering) {
            writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "3186ae1f-d3ad-48c7-94e2-40098b77408b");
            super.addHierarchyListener(l);
        }
    }

    /**
     * Overridden to do nothing for performance reasons
     */
    @Override
    public void addHierarchyBoundsListener(HierarchyBoundsListener l) {
        writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "24180b0c-bb2f-4713-aa4f-fc5c9bf51e16");
        if (swingRendering) {
            writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "f2b90c41-2240-4e8d-8316-1ed5cee16d3d");
            super.addHierarchyBoundsListener(l);
        }
    }

    /**
     * Overridden to do nothing for performance reasons
     */
    @Override
    public void addInputMethodListener(InputMethodListener l) {
        writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "26b0f8c7-d427-4345-be13-b8fe82dbc28a");
        if (swingRendering) {
            writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "2496fbb0-f83d-42e9-9f1c-64d51c915c7e");
            super.addInputMethodListener(l);
        }
    }

    /**
     * Overridden to do nothing for performance reasons
     */
    @Override
    public void addFocusListener(FocusListener fl) {
        writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "6c54b7ee-0fe0-408b-9b83-b371396ddc5f");
        if (swingRendering) {
            writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "80e36278-037c-418a-9fc7-3f022a576508");
            super.addFocusListener(fl);
        }
    }

    /**
     * Overridden to do nothing for performance reasons
     */
    @Override
    public void addMouseListener(MouseListener ml) {
        writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "4f98b4ab-727d-4994-a76f-f011fdc765db");
        if (swingRendering) {
            writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "a85003b5-8f50-43d0-95b1-32e6d5ffd0d7");
            super.addMouseListener(ml);
        }
    }

    /**
     * Overridden to do nothing for performance reasons
     */
    @Override
    public void addMouseWheelListener(MouseWheelListener ml) {
        writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "95cefa0a-c3e5-403e-a1ff-8041e3373be0");
        if (swingRendering) {
            writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "80589251-2d0e-4ab4-8fa2-5dcdb0d2c88c");
            super.addMouseWheelListener(ml);
        }
    }

    /**
     * Overridden to do nothing for performance reasons
     */
    @Override
    public void addMouseMotionListener(MouseMotionListener ml) {
        writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "5727cadb-3d27-4a3d-988f-1c613c541f12");
        if (swingRendering) {
            writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "f80924d4-90e8-4ac2-8a9c-6279886aed40");
            super.addMouseMotionListener(ml);
        }
    }

    /**
     * Overridden to do nothing for performance reasons
     */
    @Override
    public void addVetoableChangeListener(VetoableChangeListener vl) {
        writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "e3559068-ab81-45fb-bdb9-a65270b2dc36");
        if (swingRendering) {
            writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "60c79c48-a8ff-4cbf-8598-5197705365ea");
            super.addVetoableChangeListener(vl);
        }
    }

    /**
     * Overridden to do nothing for performance reasons, unless using standard swing rendering
     */
    @Override
    public void addPropertyChangeListener(String s, PropertyChangeListener l) {
        writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "b0fa5f76-1b79-438b-ae41-aa3b22db4298");
        if (swingRendering) {
            writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "a89c8784-1c73-40e9-81b8-2b4908cdd698");
            super.addPropertyChangeListener(s, l);
        }
    }

    @Override
    public void addPropertyChangeListener(PropertyChangeListener l) {
        writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "014ee616-40a1-4e68-ac11-c7b546d02911");
        if (swingRendering) {
            writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "2ff4d390-2d91-48fb-bef8-b35c3d527f0c");
            super.addPropertyChangeListener(l);
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