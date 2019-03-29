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
import java.io.*;

/**
 * EqualFlowLayout is a layout manager that works the same way as FlowLayout.
 * The only difference is that it sizes the components so that they all have the same width
 * (a width of widest component).
 *
 * @author   Ian Formanek
 * @version  1.00, Nov 12, 1998
 * @deprecated This class is a relic of NetBeans 2.0.  It is not used anywhere
 * in the NetBeans codebase, but is retained for backward compatibility in the
 * case it is used elsewhere.
 */
@Deprecated
public class EqualFlowLayout extends FlowLayout {

    /**
     * A JDK 1.1 serial version UID
     */
    static final long serialVersionUID = -1996929627282401218L;

    /**
     * Constructs a new Flow Layout with a centered alignment and a
     * default 5-unit horizontal and vertical gap.
     * @since JDK1.0
     */
    public EqualFlowLayout() {
        super();
    }

    /**
     * Constructs a new Flow Layout with the specified alignment and a
     * default 5-unit horizontal and vertical gap.
     * The value of the alignment argument must be one of
     * <code>FlowLayout.LEFT</code>, <code>FlowLayout.RIGHT</code>,
     * or <code>FlowLayout.CENTER</code>.
     * @param align the alignment value
     * @since JDK1.0
     */
    public EqualFlowLayout(int align) {
        super(align);
    }

    /**
     * Creates a new flow layout manager with the indicated alignment
     * and the indicated horizontal and vertical gaps.
     * <p>
     * The value of the alignment argument must be one of
     * <code>FlowLayout.LEFT</code>, <code>FlowLayout.RIGHT</code>,
     * or <code>FlowLayout.CENTER</code>.
     * @param      align   the alignment value.
     * @param      hgap    the horizontal gap between components.
     * @param      vgap    the vertical gap between components.
     * @since      JDK1.0
     */
    public EqualFlowLayout(int align, int hgap, int vgap) {
        super(align, hgap, vgap);
    }

    private int getMaximumWidth(Container target) {
        writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "4ff0bf1a-6f08-4fde-8874-482ae82ec048");
        int maxWidth = 0;
        writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "cd41481a-e020-466a-95ab-4e1da0aa9924");
        synchronized (target.getTreeLock()) {
            writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "a8e41b57-998f-47d5-a8ca-b0821200292b");
            int nmembers = target.getComponentCount();
            writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "95c34683-ccb1-4db2-afd6-d4ad2104aead");
            for (int i = 0; i < nmembers; i++) {
                writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "07e1d14c-c11a-42a6-804a-cee8886f3fa5");
                Component m = target.getComponent(i);
                writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "04495148-3508-4875-8155-2f64138fada3");
                if (m.isVisible()) {
                    writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "5caa8da6-db4e-4b1b-9ed9-b081ad6ad22a");
                    Dimension d = m.getPreferredSize();
                    writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "fd512667-cd31-4e0b-95e1-c05c17a69ddb");
                    maxWidth = Math.max(d.width, maxWidth);
                }
            }
        }
        writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "dedc6b24-af77-4408-a68b-e4579a8928c1");
        return maxWidth;
    }

    /**
     * Returns the preferred dimensions for this layout given the components
     * in the specified target container.
     * @param target the component which needs to be laid out
     * @return    the preferred dimensions to lay out the
     * subcomponents of the specified container.
     * @see Container
     * @see #minimumLayoutSize
     * @see       java.awt.Container#getPreferredSize
     * @since     JDK1.0
     */
    public Dimension preferredLayoutSize(Container target) {
        writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "8331e381-2f3c-4eec-9f2d-dcdc3f514bcf");
        int maxWidth = getMaximumWidth(target);
        writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "e387edf2-8e6d-43d5-9254-be328458a5b8");
        synchronized (target.getTreeLock()) {
            writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "53a48951-cae1-4261-a205-432e3ecf04e6");
            Dimension dim = new Dimension(0, 0);
            writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "aae3f4d3-6b17-4522-99f8-f074dd2a4b40");
            int nmembers = target.getComponentCount();
            writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "64e0452f-2ff3-4a42-8d0d-b076c533506e");
            for (int i = 0; i < nmembers; i++) {
                writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "f5f87e2f-c47e-453c-bea1-f66e38d65f8f");
                Component m = target.getComponent(i);
                writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "0bfdae86-e0d1-49e9-94c7-4f00ca025361");
                if (m.isVisible()) {
                    writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "22970271-693c-40f9-a374-e4d614621b1e");
                    Dimension d = m.getPreferredSize();
                    writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "982d90a7-3c0d-4ce5-968c-b252bde14786");
                    dim.height = Math.max(dim.height, d.height);
                    writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "c777d1d1-2c0a-4f24-abde-566b8111ba74");
                    if (i > 0) {
                        writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "f71b7a8f-d48b-44f1-b95f-502adb9f1f5b");
                        dim.width += getHgap();
                    }
                    writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "ad1fb202-a9f0-451d-8f91-55e8ae550b7b");
                    dim.width += maxWidth;
                }
            }
            writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "9700848b-bf44-4359-848b-e086b31440af");
            Insets insets = target.getInsets();
            writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "6ba0a1e0-b1dd-49de-b0a7-d15d962aa427");
            dim.width += (insets.left + insets.right + (getHgap() * 2));
            writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "e5e0c1ba-8e9a-40ce-9237-997ae5e299f7");
            dim.height += (insets.top + insets.bottom + (getVgap() * 2));
            writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "4d47363c-e80d-4822-93fd-675967c98b90");
            return dim;
        }
    }

    /**
     * Returns the minimum dimensions needed to layout the components
     * contained in the specified target container.
     * @param target the component which needs to be laid out
     * @return    the minimum dimensions to lay out the
     * subcomponents of the specified container.
     * @see #preferredLayoutSize
     * @see       java.awt.Container
     * @see       java.awt.Container#doLayout
     * @since     JDK1.0
     */
    public Dimension minimumLayoutSize(Container target) {
        writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "1e350757-667f-4c9e-832f-eea95136c439");
        synchronized (target.getTreeLock()) {
            writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "4acd423b-f101-401b-a0c4-4cbb0d54021b");
            Dimension dim = new Dimension(0, 0);
            writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "45e4db31-07f8-4c64-83d0-7c9e54d1d91d");
            int nmembers = target.getComponentCount();
            writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "9f9ded4a-7c7c-43f4-8b66-7e57936cd171");
            for (int i = 0; i < nmembers; i++) {
                writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "96f149d4-d333-4e8c-b4fc-44f3687ae209");
                Component m = target.getComponent(i);
                writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "3a0e55a0-f35b-42e4-b295-b796f47d243a");
                if (m.isVisible()) {
                    writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "e27bd1f9-d62a-4b5c-8b1b-2b9b6ed4b498");
                    Dimension d = m.getMinimumSize();
                    writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "f169a6a9-d75c-497c-9171-fad760062d48");
                    dim.height = Math.max(dim.height, d.height);
                    writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "a126dd05-5f2e-47af-b5f8-6ecfa026022a");
                    if (i > 0) {
                        writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "431a5262-472a-4009-8a36-3cd310979e70");
                        dim.width += getHgap();
                    }
                    writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "84806bfb-55b2-4c46-b311-75aed83d96ef");
                    dim.width += d.width;
                }
            }
            writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "a26ebed3-a284-43bf-a4ba-f49f98aa0a64");
            Insets insets = target.getInsets();
            writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "bd5ce2c5-0bda-4564-9f4b-acaf36bdea8f");
            dim.width += (insets.left + insets.right + (getHgap() * 2));
            writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "19da6144-239f-41c8-96ad-a6d692323d6e");
            dim.height += (insets.top + insets.bottom + (getVgap() * 2));
            writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "4d9afbc8-f9d9-4e71-b2ee-88fed3bbb169");
            return dim;
        }
    }

    /**
     * Centers the elements in the specified row, if there is any slack.
     * @param target the component which needs to be moved
     * @param x the x coordinate
     * @param y the y coordinate
     * @param width the width dimensions
     * @param height the height dimensions
     * @param rowStart the beginning of the row
     * @param rowEnd the the ending of the row
     */
    private void moveComponents2(Container target, int x, int y, int width, int height, int rowStart, int rowEnd) {
        writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "2cb5436c-450b-4307-ac8a-89c74c260bf7");
        synchronized (target.getTreeLock()) {
            writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "891131c3-18fc-4b43-b29d-17ad96cff3bc");
            switch(getAlignment()) {
                case LEFT:
                    writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "958948b2-1e58-48bf-a9a1-a4ef0b043f64");
                    break;
                case CENTER:
                    writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "9bdfef16-4e6a-432a-9619-b9c2c903921a");
                    x += (width / 2);
                    writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "4c9857c4-54c8-40c0-998e-c62bc38bd3e1");
                    break;
                case RIGHT:
                    writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "7f934e22-2ee0-4216-93ad-190a04c5ea98");
                    x += width;
                    writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "99caca02-bd50-4741-ae6e-48bebc6c79a0");
                    break;
            }
            writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "d35cd1c3-7012-4190-b07d-e134800a50a1");
            for (int i = rowStart; i < rowEnd; i++) {
                writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "8dc256e1-e5a2-40bb-babd-1608ac7abedb");
                Component m = target.getComponent(i);
                writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "9e7e993f-75fd-468c-b959-214c3b949fe8");
                if (m.isVisible()) {
                    writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "d055c7b9-8bd5-4f3e-9435-5a43e6dfd915");
                    m.setLocation(x, y + ((height - m.getSize().height) / 2));
                    writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "a26c6fe4-fcfe-4520-85c2-e378624e868e");
                    x += (getHgap() + m.getSize().width);
                }
            }
        }
    }

    /**
     * Lays out the container. This method lets each component take
     * its preferred size by reshaping the components in the
     * target container in order to satisfy the constraints of
     * this <code>FlowLayout</code> object.
     * @param target the specified component being laid out.
     * @see Container
     * @see       java.awt.Container#doLayout
     * @since     JDK1.0
     */
    public void layoutContainer(Container target) {
        writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "b809267e-a616-4ac4-b881-5ea6e6e21fd0");
        int maxWidth = getMaximumWidth(target);
        writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "ee2cf69a-f93c-4f93-bc63-df2e54648f8a");
        synchronized (target.getTreeLock()) {
            writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "c053bda4-c7c3-4823-aecf-c4ea01a1a803");
            Insets insets = target.getInsets();
            writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "f2fb4a9d-3c80-4ee3-8191-05a98fbcaade");
            int maxwidth = target.getSize().width - (insets.left + insets.right + (getHgap() * 2));
            writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "61ea7d71-96f6-4e9c-963f-bcde005a1eb4");
            int nmembers = target.getComponentCount();
            writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "a9f97e59-7ce5-4944-aeb6-07f29bb2f61b");
            int x = 0;
            writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "98e6d5f2-ae03-4dd3-af24-d9df4d4c2493");
            int y = insets.top + getVgap();
            writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "a39fdfd7-216b-4e6f-9f56-e123e7672cb3");
            int rowh = 0;
            writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "047f9a03-ad23-4da2-9c87-6bab714329da");
            int start = 0;
            writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "d9d4d346-8490-4ebf-892e-659c3c1f6d43");
            for (int i = 0; i < nmembers; i++) {
                writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "0e94f8c8-b84b-40af-9544-52b774ee9661");
                Component m = target.getComponent(i);
                writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "29278fde-f847-415b-860a-2fecc1e7e331");
                if (m.isVisible()) {
                    writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "371e83d0-9b5c-4987-a837-3b1aadaee566");
                    Dimension d = m.getPreferredSize();
                    writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "9cc039b5-e69c-43ba-b516-1bdb2f19df6e");
                    d.width = maxWidth;
                    writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "69d63be9-34d0-4836-bc82-76e40a78690a");
                    m.setSize(d.width, d.height);
                    writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "52769696-1fb6-4e3e-b34a-24b0bdd6b523");
                    if ((x == 0) || ((x + d.width) <= maxwidth)) {
                        writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "50adaa3f-f859-42e1-8d00-256b12237175");
                        if (x > 0) {
                            writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "0138991d-50a9-40d7-abda-5dfce7f64436");
                            x += getHgap();
                        }
                        writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "ddcd654c-d170-472d-a4d6-cae290ae27fe");
                        x += d.width;
                        writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "0909eaad-df78-4e67-bf4f-730cd2920025");
                        rowh = Math.max(rowh, d.height);
                    } else {
                        writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "a532e573-5e3c-40ad-80be-5383609e21ef");
                        moveComponents2(target, insets.left + getHgap(), y, maxwidth - x, rowh, start, i);
                        writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "64995390-18b9-426c-85a9-d8d2edb10886");
                        x = d.width;
                        writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "c49bc2ce-8cf9-4992-baa8-46655dbf93f9");
                        y += (getVgap() + rowh);
                        writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "f1261eeb-dd9a-4307-b132-d5de7743fa48");
                        rowh = d.height;
                        writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "87a31886-91f1-46f4-9a67-58bd2d7fe924");
                        start = i;
                    }
                }
            }
            writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "d4cd2afb-0e42-47aa-ba19-b5ae1d51666a");
            moveComponents2(target, insets.left + getHgap(), y, maxwidth - x, rowh, start, nmembers);
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
