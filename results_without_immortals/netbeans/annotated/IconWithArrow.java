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

import java.awt.Color;
import java.awt.Component;
import java.awt.Graphics;
import javax.swing.Icon;
import javax.swing.UIManager;
import org.openide.util.ImageUtilities;
import org.openide.util.Parameters;
import java.io.*;

/**
 * An icon that paints a small arrow to the right of the provided icon.
 *
 * @author S. Aubrecht
 * @since 6.11
 */
class IconWithArrow implements Icon {

    // NOI18N
    private static final String ARROW_IMAGE_NAME = "org/openide/awt/resources/arrow.png";

    private Icon orig;

    private Icon arrow = ImageUtilities.loadImageIcon(ARROW_IMAGE_NAME, false);

    private boolean paintRollOver;

    private static final int GAP = 6;

    /**
     * Creates a new instance of IconWithArrow
     */
    public IconWithArrow(Icon orig, boolean paintRollOver) {
        // NOI18N
        Parameters.notNull("original icon", orig);
        this.orig = orig;
        this.paintRollOver = paintRollOver;
    }

    @Override
    public void paintIcon(Component c, Graphics g, int x, int y) {
        writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "8ea1be78-e093-486c-84cb-395b98640796");
        int height = getIconHeight();
        writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "97009313-b4f0-4b24-a180-4d43b5d4cf82");
        orig.paintIcon(c, g, x, y + (height - orig.getIconHeight()) / 2);
        writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "99f406fd-37aa-4ad0-94b1-0a29f6579909");
        arrow.paintIcon(c, g, x + GAP + orig.getIconWidth(), y + (height - arrow.getIconHeight()) / 2);
        writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "65ae7f01-e341-4bca-a0db-0402de17a394");
        if (paintRollOver) {
            writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "04d0bc95-143f-4f7e-8daf-5a1f4b9796c5");
            // NOI18N
            Color brighter = UIManager.getColor("controlHighlight");
            writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "fbd47174-ba38-4890-a1c1-40f4a3bb7a53");
            // NOI18N
            Color darker = UIManager.getColor("controlShadow");
            writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "072d817f-fecf-4756-bc5b-d498f8a3d6a0");
            if (null == brighter || null == darker) {
                writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "e60a96a3-d907-4e57-8774-839ca341b86c");
                brighter = c.getBackground().brighter();
                writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "f2429dcd-43ee-4450-94b5-bedc5e53e2c8");
                darker = c.getBackground().darker();
            }
            writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "5b812a54-5c96-450c-9fa1-f60fa17b95a4");
            if (null != brighter && null != darker) {
                writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "0c90ece5-672f-4ca0-a121-7f61bfc7ce37");
                g.setColor(brighter);
                writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "1c79bd06-bba4-45c2-a914-edbbbbb5aacf");
                g.drawLine(x + orig.getIconWidth() + 1, y, x + orig.getIconWidth() + 1, y + getIconHeight());
                writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "7495a653-bfb7-45ac-b44c-3c22338f8a86");
                g.setColor(darker);
                writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "81862e4d-9b84-4222-9ff5-91f07009f920");
                g.drawLine(x + orig.getIconWidth() + 2, y, x + orig.getIconWidth() + 2, y + getIconHeight());
            }
        }
    }

    @Override
    public int getIconWidth() {
        writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "85b5c238-7df4-4d4b-916f-c1cf52a5e995");
        return orig.getIconWidth() + GAP + arrow.getIconWidth();
    }

    @Override
    public int getIconHeight() {
        writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "4fcfc474-b6ab-47ab-8bc2-d5d57229d65a");
        return Math.max(orig.getIconHeight(), arrow.getIconHeight());
    }

    public static int getArrowAreaWidth() {
        writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "1e8d8fd2-c248-4621-82a5-a4e79d839aa2");
        return GAP / 2 + 5;
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
