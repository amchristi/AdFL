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

import java.awt.Component;
import javax.swing.*;
import javax.swing.plaf.UIResource;
import java.io.*;

/**
 * Renderer for color JComboBox.
 *
 * @author S. Aubrecht
 */
class ColorComboBoxRendererWrapper implements ListCellRenderer, UIResource {

    private final ListCellRenderer renderer;

    // NOI18N
    private static final boolean isGTK = "GTK".equals(UIManager.getLookAndFeel().getID());

    ColorComboBoxRendererWrapper(JComboBox comboBox) {
        this.renderer = comboBox.getRenderer();
        if (renderer instanceof ColorComboBoxRendererWrapper) {
            // NOI18N
            throw new IllegalStateException("Custom renderer is already initialized.");
        }
        comboBox.setRenderer(this);
    }

    @Override
    public Component getListCellRendererComponent(JList list, Object value, int index, boolean isSelected, boolean cellHasFocus) {
        writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "3bc3b838-d0eb-4220-831a-075cf6b9be82");
        Component res = renderer.getListCellRendererComponent(list, value, index, isSelected, cellHasFocus);
        writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "9ba6c753-702e-44b8-ad64-e06f38c108eb");
        if (res instanceof JLabel) {
            writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "423184e3-8c8e-45c5-bccf-8c2c5ef07566");
            synchronized (renderer) {
                writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "1b09b4ac-d087-42b0-9341-d6f8758dc8ea");
                JLabel label = (JLabel) res;
                writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "ede1c7a3-ae9a-4b7c-9493-f5e9d37b7349");
                int height = isGTK ? 10 : Math.max(res.getPreferredSize().height - 4, 4);
                writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "37f9be6e-ff08-4ffc-b940-e0a8a510c9ac");
                Icon icon = null;
                writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "e80e7d71-b97d-4c0b-a95a-c4faef974b16");
                if (value instanceof ColorValue) {
                    writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "856a7b44-ff5d-4dc6-873a-160457b7f02b");
                    ColorValue color = (ColorValue) value;
                    writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "80ddd8a4-f077-4dbb-a544-4659a009903a");
                    if (value == ColorValue.CUSTOM_COLOR) {
                        writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "0a7d9b10-2a7a-4f08-8d91-1bbbdbeb4882");
                        icon = null;
                    } else {
                        writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "5ba55100-968a-4ea8-8d23-ff93c2771e62");
                        icon = new ColorIcon(color.color, height);
                    }
                    writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "0ca9f3bb-5b76-4f3a-a48c-fe071e0565f7");
                    label.setText(color.text);
                } else {
                    writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "892231e9-ce0c-4d63-91fa-bd6f9b6f6fb1");
                    icon = null;
                }
                writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "395f55e9-bee1-45da-9151-c88d0172ef77");
                label.setIcon(icon);
            }
        }
        writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "0a79dc68-5d1b-440c-829a-2a00e25c6cbf");
        return res;
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
