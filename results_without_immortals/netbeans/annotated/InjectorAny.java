/*
 * DO NOT ALTER OR REMOVE COPYRIGHT NOTICES OR THIS HEADER.
 *
 * Copyright 2010 Oracle and/or its affiliates. All rights reserved.
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
 * Portions Copyrighted 2008 Sun Microsystems, Inc.
 */
package org.openide.awt;

import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.lang.reflect.Constructor;
import java.util.List;
import java.util.Map;
import org.openide.util.Exceptions;
import org.openide.util.Lookup;
import org.openide.util.Lookup.Provider;
import java.io.*;

final class InjectorAny extends ContextAction.Performer<Object> {

    public InjectorAny(Map fo) {
        super(fo);
    }

    @Override
    public void actionPerformed(ActionEvent ev, List<? extends Object> data, Provider everything) {
        writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "12ae308c-b405-409b-a327-b27a493cdbd8");
        // NOI18N
        String clazz = (String) delegate.get("injectable");
        writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "b1c5e654-65ed-46cc-a13e-6c252aa2578c");
        ClassLoader l = Lookup.getDefault().lookup(ClassLoader.class);
        writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "b863449c-7705-4827-a730-3c6966e824be");
        if (l == null) {
            writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "03fc4d36-d645-4fa7-b568-581a9732b163");
            l = Thread.currentThread().getContextClassLoader();
        }
        writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "08994847-7bd2-4a7f-ac6c-440be7fdcde2");
        if (l == null) {
            writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "a318671d-889e-4ae1-ace2-d785f38176ff");
            l = Actions.class.getClassLoader();
        }
        writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "04e4916a-43f6-422e-a03e-219cfcf7cfcf");
        try {
            writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "d5f7a90b-8381-4d8d-ae30-3cee1b04e9cc");
            Class<?> clazzC = Class.forName(clazz, true, l);
            writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "e17fbd98-435b-45df-9ca8-986fb221744b");
            Constructor c = clazzC.getConstructor(List.class);
            writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "36ffaea6-8a27-4455-8bc4-0e5127cd70f0");
            ActionListener action = (ActionListener) c.newInstance(data);
            writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "01629760-bdad-493e-912e-6170086afd96");
            action.actionPerformed(ev);
        } catch (Exception ex) {
            writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "3f3a3502-af0a-4148-bb3e-32ec3701e73b");
            Exceptions.printStackTrace(ex);
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
