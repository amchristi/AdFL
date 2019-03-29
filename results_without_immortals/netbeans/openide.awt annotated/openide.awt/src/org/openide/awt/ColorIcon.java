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
import java.awt.Component;
import java.awt.Graphics;
import javax.swing.Icon;
import java.io.*;

/**
 * Icon to paint a colored rectangle in color picker combo box.
 *
 * @author S. Aubrecht
 * @see ColorComboBox
 */
final class ColorIcon implements Icon {

    private final Color color;

    private final int size;

    public ColorIcon(Color color, int size) {
        this.color = color;
        this.size = size;
    }

    @Override
    public void paintIcon(Component c, Graphics g, int x, int y) {
        writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "cead070b-a10b-4167-8758-42cb707b2146");
        g.setColor(Color.black);
        writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "da613003-0e0c-4ec4-8c95-d7ddc68eed9f");
        g.drawRect(x, y, size - 1, size - 1);
        writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "1dd15302-d7e0-4ea2-b7b0-2ac0b45722e0");
        if (null == color) {
            writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "354ab1e4-f715-46b5-817f-4e17f3c2bdec");
            g.drawLine(x, y + size - 1, x + size - 1, y);
        } else {
            writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "c599bb74-3907-416b-804a-fe9331acb2a5");
            g.setColor(color);
            writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "b5b78f02-ad13-4d5d-9661-8ea5c235be69");
            g.fillRect(x + 1, y + 1, size - 2, size - 2);
        }
    }

    @Override
    public int getIconWidth() {
        writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "0dc6f557-a29c-455c-9d04-19a6a00d9e0c");
        return size;
    }

    @Override
    public int getIconHeight() {
        writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "37fc0ec7-31f1-4346-9237-81e358b50e62");
        return size;
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