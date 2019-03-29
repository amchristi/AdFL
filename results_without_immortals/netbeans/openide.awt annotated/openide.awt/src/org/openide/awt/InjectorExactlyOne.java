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

final class InjectorExactlyOne extends ContextAction.Performer<Object> {

    public InjectorExactlyOne(Map fo) {
        super(fo);
    }

    @Override
    public void actionPerformed(ActionEvent ev, List<? extends Object> data, Provider everything) {
        writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "022b6243-8cfc-4470-9f30-4014277a89aa");
        if (data.size() != 1) {
            writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "c49ac278-88ae-400e-9357-78459214c404");
            return;
        }
        writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "ef633106-8fbc-4907-8d82-571ecf89126d");
        // NOI18N
        String clazz = (String) delegate.get("injectable");
        writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "80a406f3-8e3e-4641-ae37-0dfbb5202cde");
        // NOI18N
        String type = (String) delegate.get("type");
        writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "1b3a2f63-28a3-4970-96de-894d7054d82e");
        ClassLoader l = Lookup.getDefault().lookup(ClassLoader.class);
        writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "a8cb77f3-c34c-441e-80c4-e37fffd9f098");
        if (l == null) {
            writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "e98953c3-06bd-4e34-b791-b9a8d5990c0e");
            l = Thread.currentThread().getContextClassLoader();
        }
        writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "bbe24bed-bf23-400c-9e4b-5daecf5a42b6");
        if (l == null) {
            writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "3fb779bb-184c-44d4-a9b0-f135350e026f");
            l = Actions.class.getClassLoader();
        }
        writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "a83ed984-db5b-4c32-bb3e-1c8fe8dbf397");
        try {
            writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "b87876c5-7ce0-4eb1-9e31-31bf425c94a2");
            Class<?> typeC = Class.forName(type, true, l);
            writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "3597b671-728a-4f88-a322-c60d42e3d6ad");
            Class<?> clazzC = Class.forName(clazz, true, l);
            writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "ed36648d-46f2-43e7-b7f9-1e7d8492929c");
            Constructor c = clazzC.getConstructor(typeC);
            writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "1697dfd7-c8b0-4d1c-a4db-8ac8aa24a96d");
            ActionListener action = (ActionListener) c.newInstance(data.get(0));
            writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "8aad5cfa-3dd4-46ed-b511-73fa63cee906");
            action.actionPerformed(ev);
        } catch (Exception ex) {
            writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "360b2ee8-0251-43f9-aba6-91409b138e3a");
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