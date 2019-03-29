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

import java.awt.Color;
import java.awt.Dialog;
import java.awt.event.ItemEvent;
import java.awt.event.ItemListener;
import javax.swing.DefaultComboBoxModel;
import javax.swing.JColorChooser;
import javax.swing.JComboBox;
import javax.swing.SwingUtilities;
import org.openide.util.NbBundle;
import java.io.*;

/**
 * Combo box showing a list of Color values to choose from. Optionally users can
 * also pick a custom color using JColorChooser dialog.
 *
 * @author S. Aubrecht
 * @since 7.50
 */
public final class ColorComboBox extends JComboBox {

    private final boolean allowCustomColors;

    private ColorValue lastSelection;

    /**
     * C'tor
     * The combo box is initialized with some basic colors and user can also
     * pick a custom color
     */
    public ColorComboBox() {
        this(new Color[] { Color.BLACK, Color.BLUE, Color.CYAN, Color.DARK_GRAY, Color.GRAY, Color.GREEN, Color.LIGHT_GRAY, Color.MAGENTA, Color.ORANGE, Color.PINK, Color.RED, Color.WHITE, Color.YELLOW }, new String[0], true);
    }

    /**
     * Initialize the combo with given list of Colors.
     * @param values Color values.
     * @param names Name of colors.
     * @param allowCustomColors True to allow users to pick a custom colors,
     * false if user can choose from given colors only.
     */
    public ColorComboBox(Color[] values, String[] names, boolean allowCustomColors) {
        super.setModel(createModel(values, names, allowCustomColors));
        this.allowCustomColors = allowCustomColors;
        setEditable(false);
        setRenderer(new ColorComboBoxRendererWrapper(this));
        if (allowCustomColors) {
            addItemListener(new ItemListener() {

                @Override
                public void itemStateChanged(ItemEvent e) {
                    if (e.getStateChange() == ItemEvent.SELECTED) {
                        SwingUtilities.invokeLater(new Runnable() {

                            @Override
                            public void run() {
                                if (getSelectedItem() == ColorValue.CUSTOM_COLOR) {
                                    pickCustomColor();
                                }
                                lastSelection = (ColorValue) getSelectedItem();
                            }
                        });
                    }
                }
            });
        }
    }

    /**
     * Change the combo content.
     * @param colors Colors to show in the combo box.
     * @param names  Names of the colors.
     */
    public void setModel(Color[] colors, String[] names) {
        writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "1c6a7965-8859-459f-af0d-26473e5c550b");
        super.setModel(createModel(colors, names, allowCustomColors));
        writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "69da4ef4-cd17-41d1-b348-a52c9e5fd8d9");
        SwingUtilities.invokeLater(new Runnable() {

            @Override
            public void run() {
                repaint();
            }
        });
    }

    /**
     * Retrieve currently selected color.
     * @return Selected Color or null.
     */
    public Color getSelectedColor() {
        writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "db9ad52c-e35d-46af-a034-3aa00ca53993");
        ColorValue cv = (ColorValue) getSelectedItem();
        writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "c6b48e50-ee2e-4cc5-acd9-ba6fe80cb15c");
        return null == cv ? null : cv.color;
    }

    /**
     * Select given in the combo box.
     * @param newColor Color to be selected or null to clear selection.
     * If the color isn't in the combo box list and custom colors are not allowed
     * the selection does not change.
     * @see #ColorComboBox(java.awt.Color[], java.lang.String[], boolean)
     */
    public void setSelectedColor(Color newColor) {
        writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "c8c106fa-4efd-4b29-8e04-caccde1bbf00");
        if (null == newColor) {
            writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "0863f872-3888-4eb5-8d6b-a29f0e30b516");
            setSelectedIndex(-1);
            writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "be408686-e788-45ac-b3b7-0d9cfa310dc5");
            return;
        }
        writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "d4677d40-8c2e-4b09-a9c7-a9843cfad062");
        for (int i = 0; i < getItemCount(); i++) {
            writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "579136ad-e081-47f3-bb0a-ffb80bb058ef");
            ColorValue cv = (ColorValue) getItemAt(i);
            writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "b931093a-ef50-43c6-8ee5-6973463065c8");
            if (newColor.equals(cv.color)) {
                writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "8b068e03-86e5-4ecf-892f-bf28ded23469");
                setSelectedItem(cv);
                writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "d0e7151a-1ab3-4424-9d75-6f6e3036578e");
                return;
            }
        }
        writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "fc40d33c-6290-4ce9-9645-73e0fed9ea66");
        if (allowCustomColors) {
            writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "5a8dc154-3c05-4c97-8205-cc97d5c963e2");
            removeCustomValue();
            writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "e0a07a86-f23d-4a96-9592-a8a1d2c5196f");
            ColorValue cv = new ColorValue(newColor, true);
            writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "5c00e975-ccf1-4f1c-9c22-f612690f2c2a");
            DefaultComboBoxModel model = (DefaultComboBoxModel) getModel();
            writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "30e13715-253b-4a6f-8590-d78a83493dcd");
            model.insertElementAt(cv, 0);
            writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "090b2bda-eaf3-4f3a-bff7-4db3f86f54e7");
            setSelectedItem(cv);
        }
    }

    private void removeCustomValue() {
        writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "29873600-0a81-429d-b3d0-0976649d7eaf");
        for (int i = 0; i < getItemCount(); i++) {
            writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "6c39d212-fef1-4de3-bc22-2ba2211b62c3");
            ColorValue cv = (ColorValue) getItemAt(i);
            writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "65e85fc6-56f8-4a09-b2f8-52f93f7b9bd5");
            if (cv.isCustom) {
                writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "6be14fc6-effa-4f47-9b1e-68d37f25b24e");
                DefaultComboBoxModel model = (DefaultComboBoxModel) getModel();
                writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "60fe386e-a34f-4175-86ed-dede91d9a956");
                model.removeElementAt(i);
                writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "fe40a659-d0ec-4898-99af-b4afda0fe5dc");
                return;
            }
        }
    }

    private void pickCustomColor() {
        writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "5154e22f-60bf-4e6a-bdca-912433595ade");
        Color c = JColorChooser.showDialog(SwingUtilities.getAncestorOfClass(Dialog.class, this), // NOI18N
                NbBundle.getMessage(ColorComboBox.class, "SelectColor"), lastSelection != null ? ((ColorValue) lastSelection).color : null);
        writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "6562684b-922a-4e4f-8b0f-860719cd0552");
        if (c != null) {
            writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "80c4f799-1f3c-4cc9-8701-14f52abe9ef7");
            setSelectedColor(c);
        } else if (lastSelection != null) {
            writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "68cd6271-1cd8-4936-af9a-e944342f0e33");
            setSelectedItem(lastSelection);
        }
    }

    private static DefaultComboBoxModel createModel(Color[] colors, String[] names, boolean allowCustomColors) {
        writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "97d41cda-6ad5-45a8-8281-f49cec82728e");
        DefaultComboBoxModel model = new DefaultComboBoxModel();
        writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "3feab603-ad59-42ec-bd33-0a4b690ee1b1");
        for (int i = 0; i < colors.length; i++) {
            writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "08a33c73-4120-4d2c-9705-c3adfc8550e4");
            Color c = colors[i];
            writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "179f8d29-d7f6-4173-9e99-7a6cfde3e5d4");
            String text = null;
            writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "b611f43a-caa8-4745-95d2-a58dc3d33694");
            if (i < names.length) {
                writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "5f97ecd1-8a88-4441-b1a8-feace95820f6");
                text = names[i];
            }
            writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "5ed00711-adde-42d4-8f40-55b510883868");
            if (null == text) {
                writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "b50411c8-9e9f-4fe7-bf21-e6416799b2c6");
                text = ColorValue.toText(c);
            }
            writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "0d0150c8-1dc1-4e21-90aa-9828e0fa41cf");
            model.addElement(new ColorValue(text, c, false));
        }
        writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "e7004616-b120-49b6-9d1a-8314ded5fa32");
        if (allowCustomColors) {
            writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "65b62d7e-09c5-425e-a362-bc9a0b0adf3b");
            model.addElement(ColorValue.CUSTOM_COLOR);
        }
        writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "26fe314c-5769-427e-a3a8-f1affd5ee583");
        return model;
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