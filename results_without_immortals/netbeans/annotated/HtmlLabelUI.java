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

import java.awt.Color;
import java.awt.Dimension;
import java.awt.Font;
import java.awt.FontMetrics;
import java.awt.Graphics;
import java.awt.Graphics2D;
import java.awt.GraphicsEnvironment;
import java.awt.Insets;
import java.awt.RenderingHints;
import java.awt.Toolkit;
import java.util.HashMap;
import java.util.Map;
import javax.swing.Icon;
import javax.swing.JComponent;
import javax.swing.UIManager;
import javax.swing.plaf.ComponentUI;
import javax.swing.plaf.LabelUI;
import org.openide.util.Exceptions;
import java.io.*;

/**
 * A LabelUI which uses the lightweight HTML renderer. Stateless - only one
 * instance should ever exist.
 */
class HtmlLabelUI extends LabelUI {

    /**
     * System property to automatically turn on antialiasing for html strings
     */
    static final boolean antialias = // NOI18N
    Boolean.getBoolean("nb.cellrenderer.antialiasing") || // NOI18N
    Boolean.getBoolean("swing.aatext") || // NOI18N
    (isGTK() && gtkShouldAntialias()) || isAqua();

    private static HtmlLabelUI uiInstance;

    private static int FIXED_HEIGHT;

    static {
        // Jesse mode
        // NOI18N
        String ht = System.getProperty("nb.cellrenderer.fixedheight");
        if (ht != null) {
            try {
                FIXED_HEIGHT = Integer.parseInt(ht);
            } catch (Exception e) {
            // do nothing
            }
        }
    }

    private static Map<Object, Object> hintsMap;

    private static Color unfocusedSelBg;

    private static Color unfocusedSelFg;

    private static Boolean gtkAA;

    public static ComponentUI createUI(JComponent c) {
        writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "016e7c66-eba7-411b-827c-3a1c478287d0");
        if (uiInstance == null) {
            writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "0e3cbb28-927e-4314-8fd4-94891b1a8d0c");
            uiInstance = new HtmlLabelUI();
        }
        writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "b4ee4dca-7d97-4546-862f-86ed16e6e864");
        return uiInstance;
    }

    @Override
    public Dimension getPreferredSize(JComponent c) {
        writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "fb25c41f-22aa-4913-8f88-6e11e2e31f06");
        if (GraphicsEnvironment.isHeadless()) {
            writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "74385cb0-35ba-4dfb-8aab-115b6d3f1fcf");
            // cannot create scratch graphics, so don't bother
            return super.getPreferredSize(c);
        }
        writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "f2e36a8d-a57d-43fc-a51a-3b517938148c");
        return calcPreferredSize((HtmlRendererImpl) c);
    }

    /**
     * Get the width of the text
     */
    private static int textWidth(String text, Graphics g, Font f, boolean html) {
        writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "265f646e-ef6a-403b-829c-515e8445a630");
        if (text != null) {
            writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "3979995f-d37a-40fa-a1a7-8cfe39f03d9a");
            if (html) {
                writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "78a15e71-a2b9-4cd0-aa34-bb50616fe07b");
                return Math.round(Math.round(Math.ceil(HtmlRenderer.renderHTML(text, g, 0, 0, Integer.MAX_VALUE, Integer.MAX_VALUE, f, Color.BLACK, HtmlRenderer.STYLE_CLIP, false))));
            } else {
                writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "cc7a83e3-c429-4f5e-8de9-06089ef2040b");
                return Math.round(Math.round(Math.ceil(HtmlRenderer.renderPlainString(text, g, 0, 0, Integer.MAX_VALUE, Integer.MAX_VALUE, f, Color.BLACK, HtmlRenderer.STYLE_CLIP, false))));
            }
        } else {
            writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "370a517c-3387-4c78-96ff-0f382666df68");
            return 0;
        }
    }

    private Dimension calcPreferredSize(HtmlRendererImpl r) {
        writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "589d88c0-4149-491f-842f-4ed4dd21f651");
        Insets ins = r.getInsets();
        writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "2791da3c-de2d-496c-a170-700f1e4990f6");
        Dimension prefSize = new java.awt.Dimension(ins.left + ins.right, ins.top + ins.bottom);
        writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "104d415b-f912-4273-9eac-2de6935a5cd2");
        String text = r.getText();
        writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "5fc9bb91-e30b-4a57-97ff-b55e3a64e55e");
        Graphics g = r.getGraphics();
        writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "984d93cf-0221-435c-85d6-506ab05bc7b9");
        Icon icon = r.getIcon();
        writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "e15db324-03f3-49fd-9759-40ea18baea6e");
        if (text != null) {
            writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "f23e4242-f777-47c6-9ee0-1b74369312cd");
            FontMetrics fm = g.getFontMetrics(r.getFont());
            writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "51a6800b-ef18-46b9-9aba-54be2cf41792");
            prefSize.height += (fm.getMaxAscent() + fm.getMaxDescent());
        }
        writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "5906495d-9597-480e-8d35-fdb83d7b8f46");
        if (icon != null) {
            writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "7c063675-b576-4483-97d9-1c320c47c29c");
            if (r.isCentered()) {
                writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "11383e40-c7f9-4e50-b720-a8ef32bdce24");
                prefSize.height += (icon.getIconHeight() + r.getIconTextGap());
                writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "7ca33889-a880-4463-9148-a5c8e383c232");
                prefSize.width += icon.getIconWidth();
            } else {
                writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "2529116a-d80e-46cf-a7f0-e963ae37f66c");
                prefSize.height = Math.max(icon.getIconHeight() + ins.top + ins.bottom, prefSize.height);
                writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "6867874d-3bf9-4d27-be8c-ac4ab1b979eb");
                prefSize.width += (icon.getIconWidth() + r.getIconTextGap());
            }
        }
        writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "53189b56-4648-4acc-b38a-5dc4f65d94c1");
        // Antialiasing affects the text metrics, so use it if needed when
        // calculating preferred size or the result here will be narrower
        // than the space actually needed
        ((Graphics2D) g).addRenderingHints(getHints());
        writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "85aa3fea-abef-46ae-aebe-428f2ca5912f");
        int textwidth = textWidth(text, g, r.getFont(), r.isHtml()) + 4;
        writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "2063ea4a-9563-4dc1-b9ba-9a818020f159");
        if (r.isCentered()) {
            writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "9e7e3f0f-79cb-4fd9-a098-dd3726a00449");
            prefSize.width = Math.max(prefSize.width, textwidth + ins.right + ins.left);
        } else {
            writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "5c0ec26f-11e3-40a5-a1d6-90b9e707d28b");
            prefSize.width += (textwidth + r.getIndent());
        }
        writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "829fbbbf-f8b8-4c75-a3c4-d5aa1c498697");
        if (FIXED_HEIGHT > 0) {
            writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "857aec6a-727b-4414-bebe-9dcdf55f1834");
            prefSize.height = FIXED_HEIGHT;
        }
        writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "97176ee1-6226-4c6e-84dc-8ce8a360077a");
        return prefSize;
    }

    @SuppressWarnings("unchecked")
    static final Map<?, ?> getHints() {
        writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "70f3f95a-a2f3-430a-bb5a-b24df7e825df");
        // XXX We REALLY need to put this in a graphics utils lib
        if (hintsMap == null) {
            writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "b168a867-02cc-436c-b3c3-a67edf26a637");
            // Thanks to Phil Race for making this possible
            // NOI18N
            hintsMap = (Map) (Toolkit.getDefaultToolkit().getDesktopProperty("awt.font.desktophints"));
            writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "a0d99bb4-c9ad-4ba8-886c-1b8c4f597b39");
            if (hintsMap == null) {
                writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "c9bd710a-e8e2-4de8-bd56-3438b789a0ea");
                hintsMap = new HashMap<Object, Object>();
                writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "ba754912-9ac8-48c5-8770-aba8e9c07d8d");
                if (antialias) {
                    writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "30db066a-5362-41e6-a80c-859d2459e9d5");
                    hintsMap.put(RenderingHints.KEY_TEXT_ANTIALIASING, RenderingHints.VALUE_TEXT_ANTIALIAS_ON);
                }
            }
        }
        writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "a40b947e-a518-4fec-a9c7-d2da6e03ab1c");
        Map<?, ?> ret = hintsMap;
        writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "1578a893-a493-41c4-9db1-675a85af5769");
        return ret;
    }

    @Override
    public void update(Graphics g, JComponent c) {
        writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "c85ba7c5-0309-4ff8-8032-9fbd7dfc954c");
        Color bg = getBackgroundFor((HtmlRendererImpl) c);
        writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "e82606db-e522-4018-b693-cbfac25661ce");
        HtmlRendererImpl h = (HtmlRendererImpl) c;
        writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "bf96b7ee-fe7d-47ca-b3a3-f91dc6e47f2a");
        if (bg != null && !(isNimbus() && h.getType() == HtmlRendererImpl.Type.TREE)) {
            writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "bc6f51f6-0fc7-4ddd-95ed-3f5e2fe9d3ba");
            int x;
            writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "c1f7d97d-0001-4d4e-9b77-6b1921f22e60");
            if (h.getType() == HtmlRendererImpl.Type.TABLE) {
                writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "e6eb5360-a8b1-45a4-b157-4ab488e9d6a7");
                // in a table we want to have the whole row selected
                x = 0;
            } else {
                writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "e1811844-f849-44c8-a5f3-d9542aeccb93");
                x = h.isSelected() ? ((h.getIcon() == null) ? 0 : (h.getIcon().getIconWidth() + h.getIconTextGap())) : 0;
                writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "56404ec8-0b2a-4525-a422-d2588ee1d54f");
                x += h.getIndent();
            }
            writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "b4b47988-545e-4935-807c-fb2899b6f4a9");
            g.setColor(bg);
            writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "738352a0-5dcf-4775-b6cd-147ab64f500c");
            g.fillRect(x, 0, c.getWidth() - x, c.getHeight());
        }
        writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "ebbcbace-5cb7-4743-9013-2e841b300e53");
        if (h.isLeadSelection()) {
            writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "cea1b5f4-a77a-45a4-b72a-d49a6d66db7d");
            // NOI18N
            Color focus = UIManager.getColor("Tree.selectionBorderColor");
            writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "06d06cca-361b-4c2c-9df6-1c09734db03f");
            if ((focus == null) || focus.equals(bg)) {
                writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "7296f545-0177-44b3-ab9e-a33aefb22373");
                focus = Color.BLUE;
            }
            writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "e97da297-d3e4-4325-bb3f-165f9377af25");
            if (!isGTK() && !isAqua() && !isNimbus()) {
                writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "8b52626b-df2f-4f75-9f46-9a16964846ef");
                int x;
                writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "920612d8-c100-45a0-a19f-2e22e5267aa6");
                if (h.getType() == HtmlRendererImpl.Type.TABLE) {
                    writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "226f848e-f901-4bc0-9fb9-2b58e93534e3");
                    // in a table we want to have the whole row selected
                    x = 0;
                } else {
                    writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "f17bf6c1-9d6a-44fc-9bff-bf968eda9ee7");
                    x = ((h.getIcon() == null) ? 0 : (h.getIcon().getIconWidth() + h.getIconTextGap()));
                }
                writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "8f1b5a2c-1a93-4aae-9199-feccb9f54f4b");
                g.setColor(focus);
                writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "6415607f-b2c1-48c5-9e0f-2ca92d612a21");
                g.drawRect(x, 0, c.getWidth() - (x + 1), c.getHeight() - 1);
            }
        }
        writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "e360f41c-c538-4f33-bd72-e328d3f0b07d");
        paint(g, c);
    }

    @Override
    public void paint(Graphics g, JComponent c) {
        writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "230ef725-8021-4724-b153-8d78868e5571");
        ((Graphics2D) g).addRenderingHints(getHints());
        writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "b51f5020-c497-456e-9b9d-93b2c0f91f6b");
        HtmlRendererImpl r = (HtmlRendererImpl) c;
        writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "7ed2fdb5-825b-4700-b6d4-6bcd3cb46973");
        if (r.isCentered()) {
            writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "30a7965d-b189-4750-85cb-3b45ff232c37");
            paintIconAndTextCentered(g, r);
        } else {
            writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "593a75c6-9522-484d-80b2-9f67d14e0f4b");
            paintIconAndText(g, r);
        }
    }

    /**
     * Actually paint the icon and text using our own html rendering engine.
     */
    private void paintIconAndText(Graphics g, HtmlRendererImpl r) {
        writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "6750f75a-1f23-403e-a709-1e752bd9fc44");
        Font f = r.getFont();
        writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "b1d4f32f-c199-4e26-bff5-f12cf17b83c0");
        g.setFont(f);
        writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "5adb9c3e-51bb-4052-b32e-bab05ba05b77");
        FontMetrics fm = g.getFontMetrics();
        writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "31f4218c-dd7a-4ff9-804d-c22b9c8595a2");
        // Find out what height we need
        int txtH = fm.getMaxAscent() + fm.getMaxDescent();
        writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "8180a628-8d53-49c3-a1fe-aa5f4f44dfa3");
        Insets ins = r.getInsets();
        writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "35c8e7c7-4481-4f48-aacf-a2845a6287ed");
        // find out the available height less the insets
        int rHeight = r.getHeight();
        writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "988f03e7-53ab-4386-bcd1-274de8e1d3a6");
        int availH = rHeight - (ins.top + ins.bottom);
        writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "47a0ea1d-620c-4e09-ae07-d67c3746485b");
        int txtY;
        writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "223ab6fc-8cab-4ccd-b2b1-b693f08c75b5");
        if (availH >= txtH) {
            writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "f01da9a3-0353-43ca-aaed-3f3c483d60ca");
            // Center the text if we have space
            txtY = (txtH + ins.top + ((availH / 2) - (txtH / 2))) - fm.getMaxDescent();
        } else if (r.getHeight() > txtH) {
            writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "bc88a39c-a8ca-4916-a0ba-83f91535b617");
            txtY = txtH + (rHeight - txtH) / 2 - fm.getMaxDescent();
        } else {
            writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "5b92fde1-6668-4b14-9875-cf0da4ec8ce8");
            // Okay, it's not going to fit, punt.
            txtY = fm.getMaxAscent();
        }
        writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "1f07435b-0d13-40fb-b636-05f1b0376287");
        int txtX = r.getIndent();
        writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "8de6c62a-f712-418b-b126-ee3f5f6bbe3c");
        Icon icon = r.getIcon();
        writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "0722710e-5733-4edd-bccd-0bd5d7cbe523");
        // Check the icon non-null and height (see TabData.NO_ICON for why)
        if ((icon != null) && (icon.getIconWidth() > 0) && (icon.getIconHeight() > 0)) {
            writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "1b4c9bb9-4cd0-470d-813d-35d95310c35f");
            int iconY;
            writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "c872733c-a270-4412-9920-db9a222690c7");
            if (availH > icon.getIconHeight()) {
                writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "126a808b-cbc0-4440-986b-bfd67a732233");
                // add 2 to make sure icon top pixels are not cut off by outline
                // + 2;
                iconY = ins.top + ((availH / 2) - (icon.getIconHeight() / 2));
            } else if (availH == icon.getIconHeight()) {
                writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "4d43d663-e501-4663-affb-6c42ec09c93d");
                // They're an exact match, make it 0
                iconY = ins.top;
            } else {
                writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "ca8a61f7-7fc4-4557-9030-b7f6ff5cff83");
                // Won't fit; make the top visible and cut the rest off (option:
                // center it and clip it on top and bottom - probably even harder
                // to recognize that way, though)
                iconY = ins.top;
            }
            writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "b4bbc746-131a-42b4-b260-90fa7154c4e8");
            // add in the insets
            // +1 to get it out of the way of the focus border
            int iconX = ins.left + r.getIndent() + 1;
            writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "5ab0290d-f749-46ba-9553-f2b667a809b3");
            try {
                writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "de5b4707-c9b1-40bd-ad9d-7f0a2cda4baa");
                // Diagnostic - the CPP module currently is constructing
                // some ImageIcon from a null image in Options.  So, catch it and at
                // least give a meaningful message that indicates what node
                // is the culprit
                icon.paintIcon(r, g, iconX, iconY);
            } catch (NullPointerException npe) {
                writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "4170531b-f20f-463d-ad9b-6c5dd88e4a1d");
                Exceptions.attachMessage(npe, "Probably an ImageIcon with a null source image: " + icon + " - " + // NOI18N
                r.getText());
                writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "b76bb160-cc05-46da-9572-418ccd3924f6");
                Exceptions.printStackTrace(npe);
            }
            writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "5292040a-db80-4a43-8b35-ef5aef94220c");
            txtX = iconX + icon.getIconWidth() + r.getIconTextGap();
        } else {
            writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "b73a9945-f624-4740-832f-dd5d8a0ba680");
            // If there's no icon, paint the text where the icon would start
            txtX += ins.left;
        }
        writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "8fa747c0-01bf-42ed-ac48-a1f01350396c");
        String text = r.getText();
        writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "1af3e96d-1155-4218-a1c6-1d0017fcc6bf");
        if (text == null) {
            writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "1d2bbfc0-ca8b-4e87-bde8-db9f1d5ca76d");
            // No text, we're done
            return;
        }
        writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "5d7205b0-697a-4fba-bedd-f3ef01b639dd");
        // Get the available horizontal pixels for text
        int txtW = (icon != null) ? (r.getWidth() - (ins.left + ins.right + icon.getIconWidth() + r.getIconTextGap() + r.getIndent())) : (r.getWidth() - (ins.left + ins.right + r.getIndent()));
        writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "35a44839-2c32-4007-be43-47bfb0fba579");
        Color background = getBackgroundFor(r);
        writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "cb1b273e-b802-4f31-8f8b-d5f2fce3db80");
        Color foreground = ensureContrastingColor(getForegroundFor(r), background);
        writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "1cc60bc8-56d4-4e6a-83cd-242e236c5966");
        if (r.isHtml()) {
            writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "97fe6b24-1ed0-4d4f-927a-ca9a8a389334");
            HtmlRenderer._renderHTML(text, 0, g, txtX, txtY, txtW, txtH, f, foreground, r.getRenderStyle(), true, background, r.isSelected());
        } else {
            writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "32fc8aa6-7022-4f92-b524-e482d5406e08");
            HtmlRenderer.renderPlainString(text, g, txtX, txtY, txtW, txtH, f, foreground, r.getRenderStyle(), true);
        }
    }

    private void paintIconAndTextCentered(Graphics g, HtmlRendererImpl r) {
        writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "69fce3c5-d94c-4ed7-a968-79d2ebb8dc50");
        Insets ins = r.getInsets();
        writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "c10d6c5e-5038-41dd-b0a8-fc60f9dcfc0a");
        Icon ic = r.getIcon();
        writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "94bb941d-5352-4d23-beeb-b3d3d66af7d5");
        int w = r.getWidth() - (ins.left + ins.right);
        writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "0bb6178f-9e9a-4e9a-bc98-a8f59d18d13a");
        int txtX = ins.left;
        writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "6f8a364b-f9d2-41bc-b8d9-4a90a2b41eb4");
        int txtY = 0;
        writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "e697eca0-c821-40cc-90bd-cedf8d7fcc8f");
        if ((ic != null) && (ic.getIconWidth() > 0) && (ic.getIconHeight() > 0)) {
            writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "97aec14f-a271-4454-831b-420e8e438d06");
            int iconx = (w > ic.getIconWidth()) ? ((w / 2) - (ic.getIconWidth() / 2)) : txtX;
            writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "e1eaadb1-0ca5-48d4-a418-74dbc6dd8f11");
            int icony = 0;
            writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "ed26dc84-ded1-4f3f-954c-56c939ed2d97");
            ic.paintIcon(r, g, iconx, icony);
            writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "8333d1ec-07e1-44e2-8b9f-f122b8044a93");
            txtY += (ic.getIconHeight() + r.getIconTextGap());
        }
        writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "f60b6dc1-e0a0-4806-b66f-0af990056a4c");
        int txtW = r.getPreferredSize().width;
        writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "054ace32-89a7-4757-9ccd-9423729b481e");
        txtX = (txtW < r.getWidth()) ? ((r.getWidth() / 2) - (txtW / 2)) : 0;
        writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "d573770d-23c9-4b9e-9712-610a31709f78");
        int txtH = r.getHeight() - txtY;
        writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "31c3f8a0-4581-40a1-a10a-cafb4a5643bd");
        Font f = r.getFont();
        writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "4607b5ef-1700-4a1c-a2f6-905cc0e10c73");
        g.setFont(f);
        writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "77d29a35-2226-4ee8-8820-95a1043eb6f1");
        FontMetrics fm = g.getFontMetrics(f);
        writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "76d5b595-f5cd-4a20-917e-8883bdfb4427");
        txtY += fm.getMaxAscent();
        writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "321f9751-751b-4540-8d17-6f8dc30e7c2e");
        Color background = getBackgroundFor(r);
        writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "d68d71ee-2c54-4c4b-8037-7cefadb6b4f4");
        Color foreground = ensureContrastingColor(getForegroundFor(r), background);
        writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "4474ec19-edf9-4ec4-a669-bc4b02a88d68");
        if (r.isHtml()) {
            writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "c86291cf-8f82-4ffd-8c3f-aa1f3720edbe");
            HtmlRenderer._renderHTML(r.getText(), 0, g, txtX, txtY, txtW, txtH, f, foreground, r.getRenderStyle(), true, background, r.isSelected());
        } else {
            writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "7011aef6-528c-4299-9b48-b7f5f227a51f");
            HtmlRenderer.renderString(r.getText(), g, txtX, txtY, txtW, txtH, r.getFont(), foreground, r.getRenderStyle(), true);
        }
    }

    /*
    (int pos, String s, Graphics g, int x,
    int y, int w, int h, Font f, Color defaultColor, int style,
    boolean paint, Color background) {  */
    static Color ensureContrastingColor(Color fg, Color bg) {
        writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "ebc50770-6013-49d9-be14-0b93362c2e86");
        if (bg == null) {
            writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "ef380b8a-533a-45d2-9cae-f90f458bea4f");
            if (isNimbus()) {
                writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "676ac507-0503-4720-a605-a5373ad4a036");
                // NOI18N
                bg = UIManager.getColor("Tree.background");
                writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "5b851490-3fd7-4d4c-bb42-58e4d43ad93e");
                if (null == bg) {
                    writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "908949f5-7b81-4f3d-bb74-4994b409195f");
                    bg = Color.WHITE;
                }
            } else {
                writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "35e85db8-4786-41eb-a962-48d3f3382fdd");
                // NOI18N
                bg = UIManager.getColor("text");
                writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "bc3a3168-e1e0-4c70-9a22-936886fc66fc");
                if (bg == null) {
                    writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "aa7ecee6-a688-4699-bc1e-d1b0c6e17ef4");
                    bg = Color.WHITE;
                }
            }
        }
        writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "0a9f089e-4fdc-40f7-88ef-bf3cb35c92c4");
        if (fg == null) {
            writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "29755259-0064-4b09-94a8-5b996840e18d");
            if (isNimbus()) {
                writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "d8d73e1d-52da-4efb-abfc-0cad1035f021");
                // NOI18N
                fg = UIManager.getColor("Tree.foreground");
                writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "6c06bfc2-743a-49db-902c-857471160011");
                if (null == fg) {
                    writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "1f5780e3-4f6c-4ef3-b596-f2a3e725393e");
                    fg = Color.BLACK;
                }
            } else {
                writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "1da18be2-6b90-4a30-9bf1-f44c16797048");
                // NOI18N
                fg = UIManager.getColor("textText");
                writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "dce29cf1-dad3-4fe1-bdeb-e6da8e0b45e3");
                if (fg == null) {
                    writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "f3d91ece-ba87-4292-bff4-4f7a8368a629");
                    fg = Color.BLACK;
                }
            }
        }
        writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "eeda8861-f973-4948-a07e-34c3a08af40b");
        if (Color.BLACK.equals(fg) && Color.WHITE.equals(fg)) {
            writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "85653fda-4ccd-4f81-b83b-d381dbac56fa");
            return fg;
        }
        writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "b206d399-a751-4b0e-b190-91a75386986c");
        boolean replace = fg.equals(bg);
        writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "6a9c950e-8e08-4fce-9b79-af68be510646");
        int dif = 0;
        writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "62ef8295-a30a-491b-8d5c-f630ce6f8ad3");
        if (!replace) {
            writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "4b1448b4-ce29-4ba6-ad05-5ec90f41c9dd");
            dif = difference(fg, bg);
            writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "1e7624d3-4486-44d8-9552-e892d44aab74");
            replace = dif < 80;
        }
        writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "6fe820f3-22d4-4fbf-9cfe-f1f38c85a7d1");
        if (replace) {
            writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "f391e381-ef61-42d8-87c0-7bbff9231507");
            int lum = luminance(bg);
            writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "64bf31a8-d8b6-4098-9034-da4d63ebbb87");
            boolean darker = lum >= 128;
            writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "63b55086-e174-4cfd-b22c-d41d5f11b4ba");
            if (darker) {
                writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "63751fe3-954c-4f0c-9f3e-f2bbbaa6e384");
                fg = Color.BLACK;
            } else {
                writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "d6b250e0-66dc-4dc4-923f-331acb90feb5");
                fg = Color.WHITE;
            }
        }
        writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "e3b331d9-c496-4260-8bb2-21d666038f5b");
        return fg;
    }

    private static int difference(Color a, Color b) {
        writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "4db2025a-dd9e-4945-b1d9-52bb16f8cdb2");
        return Math.abs(luminance(a) - luminance(b));
    }

    private static int luminance(Color c) {
        writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "b2da4940-3acb-4c45-baae-3f3784eb75c5");
        return (299 * c.getRed() + 587 * c.getGreen() + 114 * c.getBlue()) / 1000;
    }

    static Color getBackgroundFor(HtmlRendererImpl r) {
        writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "59ed6644-6b04-47dd-89ac-f453f314600d");
        if (r.isOpaque()) {
            writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "c7df68e0-6e6c-4b7a-ae0b-e20a7d45adcf");
            return r.getBackground();
        }
        writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "979b4f30-5691-4cf5-92c2-72fc56df7f64");
        if (r.isSelected() && !r.isParentFocused() && !isGTK() && !isNimbus()) {
            writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "f855318c-2431-4632-a9ca-aa965e889f59");
            return getUnfocusedSelectionBackground();
        }
        writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "7a49e6ba-09ea-4737-a28b-a634852446a3");
        Color result = null;
        writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "4e3406e6-834b-4adc-98c8-8aaad655eb3d");
        if (r.isSelected()) {
            writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "558e7d93-a6ee-4e71-b5a9-d9619d9a15e4");
            switch(r.getType()) {
                case LIST:
                    writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "dc8f92a2-c9ab-4ed9-b9f6-eb7939695804");
                    // NOI18N
                    result = UIManager.getColor("List.selectionBackground");
                    writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "b18dd970-0957-4ea3-8b43-550a8203ab59");
                    if (result == null) {
                        writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "1cc9dcb5-fd41-42a2-bd67-8be89be2251f");
                        // GTK
                        // plaf library guarantees this one:
                        // NOI18N
                        result = UIManager.getColor("Tree.selectionBackground");
                    }
                    writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "c189ee19-65bb-4397-bca5-030ad4ac7c4e");
                    // System.err.println("  now " + result);
                    break;
                case TABLE:
                    writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "e7050e1b-abf0-46fb-8d3f-dd139b169943");
                    // NOI18N
                    result = UIManager.getColor("Table.selectionBackground");
                    writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "63443ea3-2f6c-4606-8174-89e27a446873");
                    break;
                case TREE:
                    writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "befca1d7-c4ba-4f42-9505-d79b27599f2c");
                    // NOI18N
                    return UIManager.getColor("Tree.selectionBackground");
            }
            writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "fba6e75f-8259-4981-9580-8fc0700a3b98");
            return (result == null) ? r.getBackground() : result;
        }
        writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "bd2ee184-43db-4507-aadf-841864c1d81a");
        return null;
    }

    static Color getForegroundFor(HtmlRendererImpl r) {
        writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "513126e1-5f10-4942-a465-3b88fab3a627");
        if (r.isSelected() && !r.isParentFocused() && !isGTK() && !isNimbus()) {
            writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "f6564e6e-5423-4644-af6f-25195beb7f52");
            return getUnfocusedSelectionForeground();
        }
        writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "48039a73-281e-458d-a47d-836c6b86cc17");
        if (!r.isEnabled()) {
            writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "b647c3bc-0997-4416-a838-ebe51484b7a1");
            // NOI18N
            return UIManager.getColor("textInactiveText");
        }
        writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "3e4c2ae3-44bd-4adc-84e3-90b65bd9bac1");
        Color result = null;
        writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "bdbbba83-a549-4456-bdcb-9f736f2ff115");
        if (r.isSelected()) {
            writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "0f67799d-f48d-4c19-bdaf-d7866a87200a");
            switch(r.getType()) {
                case LIST:
                    writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "feb771ea-6a67-4f83-a7f5-0ca3bfff7154");
                    // NOI18N
                    result = UIManager.getColor("List.selectionForeground");
                    writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "6b72b93d-5d91-4453-9b74-0c5628850bea");
                    break;
                case TABLE:
                    writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "4a921ab1-58c3-4112-8d3c-7cf7bfda1980");
                    // NOI18N
                    result = UIManager.getColor("Table.selectionForeground");
                    writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "4c0f647c-7b86-4427-9eb3-e531ad277fb4");
                    break;
                case TREE:
                    writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "d00bc54b-9764-467b-bc9b-623654d4bb5a");
                    // NOI18N
                    result = UIManager.getColor("Tree.selectionForeground");
            }
        }
        writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "02dea13b-7f38-434d-ae43-2c0019c85ab4");
        return (result == null) ? r.getForeground() : result;
    }

    static boolean isAqua() {
        writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "d29c8f51-0ecb-4c1b-9f14-cde98f5a310f");
        return "Aqua".equals(UIManager.getLookAndFeel().getID());
    }

    static boolean isGTK() {
        writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "7a432909-e9c5-42fb-9395-4c67512b5f32");
        return "GTK".equals(UIManager.getLookAndFeel().getID());
    }

    static boolean isNimbus() {
        writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "e16cfb68-6efb-4e2f-b803-e843787d7028");
        return "Nimbus".equals(UIManager.getLookAndFeel().getID());
    }

    /**
     * Get the system-wide unfocused selection background color
     */
    private static Color getUnfocusedSelectionBackground() {
        writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "fcb23d11-c5da-4c1e-af5a-621f4f903bad");
        if (unfocusedSelBg == null) {
            writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "f2353039-273c-4822-b2e9-6b051375c9cd");
            // allow theme/ui custom definition
            // NOI18N
            unfocusedSelBg = UIManager.getColor("nb.explorer.unfocusedSelBg");
            writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "7f6afcce-f9b5-4aa6-8f08-1c56ee9be419");
            if (unfocusedSelBg == null) {
                writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "04620e4d-89bc-47ac-8b00-b9c4bf0d01b2");
                // try to get standard shadow color
                // NOI18N
                unfocusedSelBg = UIManager.getColor("controlShadow");
                writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "655e97f9-f5b5-44db-8832-de713f2fa093");
                if (unfocusedSelBg == null) {
                    writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "e0f77b17-d8ea-4776-a0d3-dae21584a2aa");
                    // Okay, the look and feel doesn't suport it, punt
                    unfocusedSelBg = Color.lightGray;
                }
                writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "d0b73b1b-c233-4801-8645-2ef82e74e1e2");
                // gray
                if (!Color.WHITE.equals(unfocusedSelBg.brighter())) {
                    writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "2bac3c0d-fb01-45d0-9fc2-92669215fcee");
                    unfocusedSelBg = unfocusedSelBg.brighter();
                }
            }
        }
        writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "959dc226-4c23-4c59-b2f0-06cc15a7233c");
        return unfocusedSelBg;
    }

    /**
     * Get the system-wide unfocused selection foreground color
     */
    private static Color getUnfocusedSelectionForeground() {
        writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "09e7df78-db3a-4375-b849-6749f81ca473");
        if (unfocusedSelFg == null) {
            writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "28589224-f35e-4c93-bcdc-55290e928d99");
            // allow theme/ui custom definition
            // NOI18N
            unfocusedSelFg = UIManager.getColor("nb.explorer.unfocusedSelFg");
            writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "16fd43e7-90f3-48b3-ac0c-1ea103f87edd");
            if (unfocusedSelFg == null) {
                writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "7f5653b7-6ae0-451e-8c5a-c3a90e40e57d");
                // try to get standard shadow color
                // NOI18N
                unfocusedSelFg = UIManager.getColor("textText");
                writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "86783a2b-3067-4e44-b6b7-649db82d63f9");
                if (unfocusedSelFg == null) {
                    writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "d568c4d7-330a-4d2f-9d60-db5621638cb2");
                    // Okay, the look and feel doesn't suport it, punt
                    unfocusedSelFg = Color.BLACK;
                }
            }
        }
        writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "6fc7bfc2-2bc9-49f8-93a0-d7c0ef673f4f");
        return unfocusedSelFg;
    }

    public static final boolean gtkShouldAntialias() {
        writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "f2a5a2fc-0fe4-4e66-baed-bd185f3b9ca1");
        if (gtkAA == null) {
            writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "0e4a4fb8-b462-483d-a7b0-79114b05b69d");
            // NOI18N
            Object o = Toolkit.getDefaultToolkit().getDesktopProperty("gnome.Xft/Antialias");
            writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "4ab483fa-eb9a-4a34-be7e-e1993331c814");
            gtkAA = new Integer(1).equals(o) ? Boolean.TRUE : Boolean.FALSE;
        }
        writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "1e6548ef-20c2-4708-8569-61b9ad0591e0");
        return gtkAA.booleanValue();
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
