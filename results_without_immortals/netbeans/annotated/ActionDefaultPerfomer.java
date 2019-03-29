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
 * Software is Sun Microsystems, Inc. Portions Copyright 1997-2009 Sun
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

import java.awt.event.ActionEvent;
import java.util.Collections;
import java.util.List;
import org.netbeans.api.actions.Closable;
import org.netbeans.api.actions.Editable;
import org.netbeans.api.actions.Openable;
import org.netbeans.api.actions.Printable;
import org.netbeans.api.actions.Viewable;
import org.openide.util.Lookup.Provider;
import java.io.*;

final class ActionDefaultPerfomer extends ContextAction.Performer<Object> {

    final int type;

    public ActionDefaultPerfomer(int type) {
        super(Collections.emptyMap());
        this.type = type;
    }

    @Override
    public void actionPerformed(ActionEvent ev, List<? extends Object> data, Provider everything) {
        writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "de7d58e6-9d36-436b-9071-4d0b9b5772fd");
        for (Object o : data) {
            writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "578e9d65-2e65-453e-ae72-7f78277a8bfc");
            switch(type) {
                case 0:
                    writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "eaab998d-cf4c-4aaf-ac70-b9f8c32352d1");
                    ((Openable) o).open();
                    writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "fc22a3e1-5d07-40d5-b538-94387d5e90b5");
                    break;
                case 1:
                    writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "a7180f11-e9f7-4920-a8ab-e54f3e66bf3d");
                    ((Viewable) o).view();
                    writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "08aa9a0e-2152-41ce-b898-23ffafffc681");
                    break;
                case 2:
                    writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "6d35f24e-7621-46e8-ba00-dd59ad33c1ce");
                    ((Editable) o).edit();
                    writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "26450223-e4c7-4d32-9f2a-877248f72323");
                    break;
                case 3:
                    writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "2866a2c2-99b2-48a4-8702-9f4b0b4d2067");
                    ((Closable) o).close();
                    writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "98946cac-2e1d-47c6-a5aa-efcc6502dcf3");
                    break;
                case 4:
                    writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "6d511b27-e2b4-4ccf-8c64-427cd90dcd25");
                    ((Printable) o).print();
                    writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "cfff0c76-b81b-404b-899b-fa52c1044673");
                    break;
                default:
                    writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "bdcd95c0-f1ec-4601-a730-6d362fedbf00");
                    assert false : "Wrong type: " + type;
            }
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
