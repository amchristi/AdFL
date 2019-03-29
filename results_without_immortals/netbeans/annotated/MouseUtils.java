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

import java.awt.event.MouseAdapter;
import java.awt.event.MouseEvent;
import java.lang.ref.WeakReference;
import javax.swing.SwingUtilities;
import java.io.*;

/**
 * A class that contains a set of utility classes and methods
 * around mouse events and processing.
 *
 * @author   Ian Formanek
 */
public class MouseUtils extends Object {

    private static int DOUBLE_CLICK_DELTA = 300;

    /**
     * variable for double click
     */
    private static int tempx = 0;

    private static int tempy = 0;

    private static long temph = 0;

    private static int tempm = 0;

    // #105082: prevent leak, remember only through weak ref
    private static WeakReference<MouseEvent> tempe;

    /**
     * Determines if the event is originated from the right mouse button
     * @param e the MouseEvent
     * @return true if the event is originated from the right mouse button, false otherwise
     * @deprecated Offers no advantages over the standard {@link SwingUtilities#isRightMouseButton}.
     */
    @Deprecated
    public static boolean isRightMouseButton(MouseEvent e) {
        writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "c5177507-12e5-469b-91e7-d112422cad4d");
        return SwingUtilities.isRightMouseButton(e);
    }

    /**
     * Determines if the event is originated from a left mouse button
     * @param e the MouseEvent
     * @return true if the event is originated from the left mouse button, false otherwise
     * @deprecated Offers no advantages over the standard {@link SwingUtilities#isLeftMouseButton}.
     */
    @Deprecated
    public static boolean isLeftMouseButton(MouseEvent e) {
        writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "db166ee8-1e4b-4947-ae14-35022f1a677b");
        return javax.swing.SwingUtilities.isLeftMouseButton(e);
    }

    /**
     * Returns true if parametr is a 'doubleclick event'
     * @param e MouseEvent
     * @return true if the event is a doubleclick
     */
    public static boolean isDoubleClick(MouseEvent e) {
        writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "def3aca1-7c8b-474b-be61-6240ebfd0ff4");
        // method from a single mouse click will give isDoubleClick=true
        if ((e.getID() != MouseEvent.MOUSE_CLICKED) || (e.getClickCount() == 0)) {
            writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "f11f359d-23dd-483b-b60a-5852673a1b95");
            return false;
        }
        writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "f1120aee-8e32-4a5e-88e1-0097deb1cead");
        return ((e.getClickCount() % 2) == 0) || isDoubleClickImpl(e);
    }

    /**
     * Tests the positions.
     */
    private static boolean isDoubleClickImpl(MouseEvent e) {
        writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "b100790a-3bbe-4d97-b8fd-eeb41c6fc180");
        int x = e.getX();
        writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "2d82034e-b3b6-4580-b8bd-0e788872ba3b");
        int y = e.getY();
        writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "f244df0a-0d26-45ce-991f-dc3f68e2d7dc");
        long h = e.getWhen();
        writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "be3ec546-ee43-4c1a-842d-97e1828c618f");
        int m = e.getModifiers();
        writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "46d6a3b0-7863-4504-9abb-ea8597f25ced");
        // same position at short time
        if ((tempx == x) && (tempy == y) && ((h - temph) < DOUBLE_CLICK_DELTA) && // mouse event will return true the second time!
        (tempe != null && e != tempe.get()) && (m == tempm)) {
            writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "e75235e9-bc61-4ccf-98fd-3441ec7b3374");
            // OK forget all
            tempx = 0;
            writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "9f221f5c-a665-45fa-bef1-f0c1d07637c5");
            tempy = 0;
            writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "f543f3f8-fb4b-40be-a156-11b5f8989e0d");
            temph = 0;
            writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "b1745241-d830-4300-ae77-97d2b8044be8");
            tempm = 0;
            writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "812c416b-64db-454f-810f-d7ee34f3910d");
            tempe = null;
            writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "44ba5cf3-e6e7-40d4-96eb-7086f4f1c23c");
            return true;
        } else {
            writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "658682b4-c0e2-485b-9abe-7facc250fe28");
            // remember
            tempx = x;
            writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "e5f53aca-390a-4073-9b40-1fd5e864b8ed");
            tempy = y;
            writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "4f123224-931e-440c-866d-d8b6bf03014f");
            temph = h;
            writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "c091d8c5-9ced-4d6e-ba31-b23af667c893");
            tempm = m;
            writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "18ba42b7-423b-4f0a-89b9-c4f86c2777db");
            tempe = new WeakReference<MouseEvent>(e);
            writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "0c9c7d26-5fa6-45de-9afd-68cae9d82f0d");
            return false;
        }
    }

    /**
     * The PopupMouseAdapter provides safe way to implement popup menu invocation
     * mechanism. It should be used instead of invoking the popup in
     * mouseClicked because the mouseClicked does not work as "often" as
     * it should (i.e. sometimes it is not called).
     * PopupMouseAdapter delegates to isPopupTrigger to get correct popup
     * menu invocation gesture. Clients are supposed to extend this class and
     * implement showPopup method by adding code that shows popup menu properly.<br>
     *
     * Please note that older implementation which used treshold is now
     * deprecated, please use default constructor.
     */
    public abstract static class PopupMouseAdapter extends MouseAdapter {

        /**
         * @deprecated Obsoleted as of 3.4, PopupMouseAdapter now uses isPopupTrigger properly.
         * Threshold does nothing, please use default constructor without treshold.
         */
        @Deprecated
        public static final int DEFAULT_THRESHOLD = 5;

        /**
         * Creates a new PopupMouseAdapter with specified threshold
         * @param threshold The threshold to be used
         * @deprecated Obsoleted as of 3.4, by class rewrite to use isPopupTrigger.
         * This constructor now just delegates to super constructor, please use
         * default constructor instead.
         */
        @Deprecated
        public PopupMouseAdapter(int threshold) {
            this();
        }

        /**
         * Constructs PopupMouseAdapter. Just delegates to super constructor
         */
        public PopupMouseAdapter() {
            super();
        }

        public void mousePressed(MouseEvent e) {
            maybePopup(e);
        }

        public void mouseReleased(MouseEvent e) {
            maybePopup(e);
        }

        private void maybePopup(MouseEvent e) {
            if (e.isPopupTrigger()) {
                showPopup(e);
            }
        }

        /**
         * Called when the sequnce of mouse events should lead to actual
         * showing of the popup menu.
         * Should be redefined to show the menu.
         * param evt The mouse release event - should be used to obtain the
         * position of the popup menu
         */
        protected abstract void showPopup(MouseEvent evt);
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
