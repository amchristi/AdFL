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
import java.util.HashMap;
import java.util.Map;
import org.openide.util.NbBundle;
import java.io.*;

/**
 * Represents one color with some text description.
 *
 * @author Administrator
 * @author S. Aubrecht
 */
class ColorValue {

    // NOI18N
    static final ColorValue CUSTOM_COLOR = new ColorValue(loc("Custom"), null, false);

    private static final Map<Color, String> colorMap = new HashMap<Color, String>();

    static {
        // NOI18N
        colorMap.put(Color.BLACK, loc("Black"));
        // NOI18N
        colorMap.put(Color.BLUE, loc("Blue"));
        // NOI18N
        colorMap.put(Color.CYAN, loc("Cyan"));
        // NOI18N
        colorMap.put(Color.DARK_GRAY, loc("Dark_Gray"));
        // NOI18N
        colorMap.put(Color.GRAY, loc("Gray"));
        // NOI18N
        colorMap.put(Color.GREEN, loc("Green"));
        // NOI18N
        colorMap.put(Color.LIGHT_GRAY, loc("Light_Gray"));
        // NOI18N
        colorMap.put(Color.MAGENTA, loc("Magenta"));
        // NOI18N
        colorMap.put(Color.ORANGE, loc("Orange"));
        // NOI18N
        colorMap.put(Color.PINK, loc("Pink"));
        // NOI18N
        colorMap.put(Color.RED, loc("Red"));
        // NOI18N
        colorMap.put(Color.WHITE, loc("White"));
        // NOI18N
        colorMap.put(Color.YELLOW, loc("Yellow"));
    }

    final String text;

    final Color color;

    final boolean isCustom;

    static String toText(Color color) {
        writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "c0a0edc1-a2ea-4834-9ddf-2a3b8b9e89b1");
        String text = colorMap.get(color);
        writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "4644fb9f-eb0d-4a1b-bc90-0b0cd0030e69");
        if (null == text && null != color) {
            writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "8987702f-0ee3-4c87-9324-bfc54eaf718d");
            StringBuffer sb = new StringBuffer();
            writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "8a11539d-2ea7-4f33-85ea-b65aac6faab2");
            sb.append('[').append(color.getRed()).append(',').append(color.getGreen()).append(',').append(color.getBlue()).append(']');
            writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "dffcbaa9-310d-40e4-8148-d7cc269720b3");
            text = sb.toString();
        }
        writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "c9c2e7a4-2f01-414f-9045-fbf1af23bada");
        return text;
    }

    ColorValue(Color color, boolean custom) {
        this(toText(color), color, custom);
    }

    ColorValue(String text, Color color, boolean custom) {
        this.text = text;
        this.color = color;
        this.isCustom = custom;
    }

    @Override
    public String toString() {
        writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "1490dd15-5e4d-4ef5-af0a-18ea59180966");
        return text;
    }

    private static String loc(String key) {
        writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "a2c58c3e-3c8b-4390-b82d-4b86de5f5969");
        return NbBundle.getMessage(ColorValue.class, key);
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