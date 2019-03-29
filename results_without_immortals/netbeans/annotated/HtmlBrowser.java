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
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.MouseAdapter;
import java.awt.event.MouseEvent;
import java.beans.PropertyChangeEvent;
import java.beans.PropertyChangeListener;
import java.net.MalformedURLException;
import java.net.URL;
import java.util.logging.Level;
import java.util.logging.Logger;
import javax.accessibility.Accessible;
import javax.accessibility.AccessibleContext;
import javax.swing.*;
import org.openide.util.*;
import java.io.*;

/**
 * Object that provides viewer for HTML pages.
 * <p>If all you want to do is to show some URL, this
 * is overkill. Just use {@link HtmlBrowser.URLDisplayer#showURL} instead. Using <code>HtmlBrowser</code>
 * is appropriate mainly if you want to embed a web browser in some other GUI component
 * (if the user has selected an external browser, this will fall back to a simple Swing
 * renderer). Similarly <code>Impl</code> (coming from a <code>Factory</code>) is the lower-level
 * renderer itself (sans toolbar).
 * <p>Summary: for client use, try <code>URLDisplayer.showURL</code>, or for more control
 * or where embedding is needed, create an <code>HtmlBrowser</code>. For provider use,
 * create a <code>Factory</code> and register an instance of it to lookup.
 */
public class HtmlBrowser extends JPanel {

    // static ....................................................................
    /**
     * generated Serialized Version UID
     */
    private static final long serialVersionUID = 2912844785502987960L;

    /**
     * Preferred width of the browser
     */
    public static final int DEFAULT_WIDTH = 400;

    /**
     * Preferred height of the browser
     */
    public static final int DEFAULT_HEIGHT = 600;

    /**
     * current implementation of html browser
     */
    private static Factory browserFactory;

    /**
     * home page URL
     */
    private static String homePage = null;

    // variables .................................................................
    /**
     * currently used implementation of browser
     */
    final Impl browserImpl;

    /**
     * true = ignore changes in location field
     */
    private boolean ignoreChangeInLocationField = false;

    /**
     * toolbar visible property
     */
    private boolean toolbarVisible = false;

    /**
     * status line visible property
     */
    private boolean statusLineVisible = false;

    /**
     * Listens on changes in HtmlBrowser.Impl and HtmlBrowser visual components.
     */
    private BrowserListener browserListener;

    // visual components .........................................................
    private JButton bBack;

    // visual components .........................................................
    private JButton bForward;

    // visual components .........................................................
    private JButton bReload;

    // visual components .........................................................
    private JButton bStop;

    /**
     * URL chooser
     */
    private JTextField txtLocation;

    private JLabel lStatusLine;

    final Component browserComponent;

    private JPanel head;

    private RequestProcessor rp = new RequestProcessor();

    private final Component extraToolbar;

    /**
     * Creates new html browser with toolbar and status line.
     */
    public HtmlBrowser() {
        this(true, true);
    }

    /**
     * Creates new html browser.
     *
     * @param toolbar visibility of toolbar
     * @param statusLine visibility of statusLine
     */
    public HtmlBrowser(boolean toolbar, boolean statusLine) {
        this(null, toolbar, statusLine);
    }

    /**
     * Creates new html browser.
     *
     * @param fact Factory that is used for creation. If null is passed it searches for
     * a factory providing displayable component.
     * @param toolbar visibility of toolbar
     * @param statusLine visibility of statusLine
     */
    public HtmlBrowser(Factory fact, boolean toolbar, boolean statusLine) {
        this(fact, toolbar, statusLine, null);
    }

    /**
     * Creates new html browser.
     *
     * @param fact Factory that is used for creation. If null is passed it searches for
     * a factory providing displayable component.
     * @param toolbar visibility of toolbar
     * @param statusLine visibility of statusLine
     * @param extraToolbar Additional toolbar to be displayed under the default
     * toolbar with location field and back/forward buttons.
     * @since 7.52
     */
    public HtmlBrowser(Factory fact, boolean toolbar, boolean statusLine, Component extraToolbar) {
        Impl impl = null;
        Component comp = null;
        try {
            if (fact == null) {
                Impl[] arr = new Impl[1];
                comp = findComponent(arr);
                impl = arr[0];
            } else {
                try {
                    impl = fact.createHtmlBrowserImpl();
                    comp = impl.getComponent();
                } catch (UnsupportedOperationException ex) {
                    Exceptions.printStackTrace(ex);
                    impl = new SwingBrowserImpl();
                    comp = impl.getComponent();
                }
            }
        } catch (RuntimeException e) {
            // browser was uninstlled ?
            Exceptions.attachLocalizedMessage(e, NbBundle.getMessage(HtmlBrowser.class, "EXC_Module"));
            Exceptions.printStackTrace(e);
        }
        browserImpl = impl;
        browserComponent = comp;
        this.extraToolbar = extraToolbar;
        setLayout(new BorderLayout(0, 2));
        // NOI18N
        add((browserComponent != null) ? browserComponent : new JScrollPane(), "Center");
        browserListener = new BrowserListener();
        if (toolbar) {
            initToolbar();
        }
        if (statusLine) {
            initStatusLine();
        }
        browserImpl.addPropertyChangeListener(browserListener);
        getAccessibleContext().setAccessibleName(NbBundle.getMessage(HtmlBrowser.class, "ACS_HtmlBrowser"));
        getAccessibleContext().setAccessibleDescription(NbBundle.getMessage(HtmlBrowser.class, "ACSD_HtmlBrowser"));
    }

    /**
     * Sets the home page.
     * @param u the home page
     */
    public static void setHomePage(String u) {
        writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "9702644c-d86b-4cc8-a4bf-1f374db0820b");
        homePage = u;
    }

    /**
     * Getter for the home page
     * @return the home page
     */
    public static String getHomePage() {
        writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "f476e30e-92ca-4200-8bc2-cb5e9f2bec39");
        if (homePage == null) {
            writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "c7306f4c-17f1-4936-b0dd-2ea0e87c29d3");
            return NbBundle.getMessage(HtmlBrowser.class, "PROP_HomePage");
        }
        writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "d7dd53aa-2ba1-40ad-9ecc-4b4e61981db8");
        return homePage;
    }

    /**
     * Sets a new implementation of browser visual component
     * for all HtmlBrowers.
     * @deprecated Use Lookup instead to register factories
     */
    @Deprecated
    public static void setFactory(Factory brFactory) {
        writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "7eb41a6b-57f6-4aec-9046-16f425de09d2");
        browserFactory = brFactory;
    }

    /**
     * Find Impl of HtmlBrowser. Searches for registered factories in lookup folder.
     * Tries to create Impl and check if it provides displayable component.
     * Both Component and used Impl are returned to avoid resource consuming of new
     * Component/Impl.
     * </P>
     * <P>
     * If no browser is found then it tries to use registered factory (now deprecated method
     * of setting browser) or it uses browser based on swing editor in the worst case.
     *
     * @param handle used browser implementation is in first element when method
     * is finished
     * @return Component for content displaying
     */
    private static Component findComponent(Impl[] handle) {
        writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "a203b781-6f60-469d-b64e-0f6aae0b210e");
        Lookup.Result<Factory> r = Lookup.getDefault().lookup(new Lookup.Template<Factory>(Factory.class));
        writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "a6af278a-f3ce-460b-ac65-7a5e3ce9abd1");
        for (Factory f : r.allInstances()) {
            writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "a4b53a41-cebd-4c3d-b36c-97be229ff30a");
            try {
                writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "18225d8c-e9b6-4253-b566-123677d1e621");
                Impl impl = f.createHtmlBrowserImpl();
                writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "892f6fa4-7cde-4f57-88e6-816bb8bc693e");
                Component c = (impl != null) ? impl.getComponent() : null;
                writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "f3a74262-f0a5-4d8c-a620-1b0c0e87cb58");
                if (c != null) {
                    writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "5fb46c95-6b60-4362-93ad-ddbaa55826b3");
                    handle[0] = impl;
                    writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "28005e6b-8fd9-4222-bb65-7a8805bbf852");
                    return c;
                }
            } catch (UnsupportedOperationException ex) {
            // do nothing: thrown if browser doesn't work on given platform
            }
        }
        writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "dd24287b-c1be-4ba4-8bdc-6ffb4e8a66b1");
        // 1st fallback to our deprecated method
        Factory f = browserFactory;
        writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "9ea2cc61-0270-4109-9729-a8b8ffe4d76b");
        if (f != null) {
            writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "a3885a5d-304a-4f62-8086-feacc85bd0b4");
            try {
                writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "d75564b3-8478-4ca3-8b9e-0ca70294faa4");
                handle[0] = f.createHtmlBrowserImpl();
                writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "60a0ea15-9d12-4171-9589-4c444c3d1361");
                return handle[0].getComponent();
            } catch (UnsupportedOperationException ex) {
            // do nothing: thrown if browser doesn't work on given platform
            }
        }
        writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "3d9c2831-7cc5-4776-9480-e991abea385a");
        // last fallback is to swing
        handle[0] = new SwingBrowserImpl();
        writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "dd257bad-26de-4ffd-8001-d2fe1e5e2c4c");
        return handle[0].getComponent();
    }

    /**
     * Default initialization of toolbar.
     */
    private void initToolbar() {
        writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "c4f2964c-045e-4b08-b143-1af2420bbf3c");
        toolbarVisible = true;
        writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "163a07a6-f0cc-4df3-b0b4-275e5c4ba8e7");
        // create visual compoments .............................
        head = new JPanel(new GridBagLayout());
        writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "4afd8bd4-0d97-4d33-a0c4-71865ac9fef9");
        bBack = new JButton();
        writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "b6e51cc8-ba10-4f29-9ded-c7abb4f4dcf2");
        bBack.setBorder(BorderFactory.createEmptyBorder());
        writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "4865f91d-d53d-4e10-95d0-26feae8e2990");
        bBack.setBorderPainted(false);
        writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "31734725-70dd-4e6e-8e58-8b3688d97dda");
        bBack.setContentAreaFilled(false);
        writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "8d4aa382-8414-4da5-806a-169655c0120d");
        // NOI18N
        bBack.setIcon(ImageUtilities.loadImageIcon("org/openide/resources/html/back_normal.png", true));
        writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "464efe08-c52c-4eca-b73e-77322712ea91");
        // NOI18N
        bBack.setRolloverIcon(ImageUtilities.loadImageIcon("org/openide/resources/html/back_hover.png", true));
        writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "a6904d07-3b69-46cc-b470-7923c4129c88");
        // NOI18N
        bBack.setDisabledIcon(ImageUtilities.loadImageIcon("org/openide/resources/html/back_disabled.png", true));
        writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "ec6d9d89-2fdb-4bb6-83d4-0f5753f43d93");
        bBack.setSelectedIcon(bBack.getIcon());
        writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "e2b34e4d-cc14-44b7-85d7-b7da70577751");
        // NOI18N
        bBack.setToolTipText(NbBundle.getMessage(HtmlBrowser.class, "CTL_Back"));
        writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "3927afc1-7296-4964-94f8-7d79347e63ca");
        bForward = new JButton();
        writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "baf98263-29dd-40d9-ba5b-519875437cd2");
        bForward.setBorder(BorderFactory.createEmptyBorder());
        writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "eb3521df-6fe4-42d8-9e7d-9f8f00267fbc");
        bForward.setBorderPainted(false);
        writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "c3018b65-fd3a-45de-982d-375e96d6e378");
        bForward.setContentAreaFilled(false);
        writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "ea66f143-2e92-441f-b61f-bb97d5ddd243");
        // NOI18N
        bForward.setIcon(ImageUtilities.loadImageIcon("org/openide/resources/html/forward_normal.png", true));
        writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "e2069a4b-11b2-4842-a675-7ca4c2f3089e");
        // NOI18N
        bForward.setRolloverIcon(ImageUtilities.loadImageIcon("org/openide/resources/html/forward_hover.png", true));
        writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "07ce98aa-75ed-4c8a-ab60-380b92b53872");
        // NOI18N
        bForward.setDisabledIcon(ImageUtilities.loadImageIcon("org/openide/resources/html/forward_disabled.png", true));
        writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "6687cb03-1f3f-44d8-9a89-ff136e7a0690");
        bForward.setSelectedIcon(bForward.getIcon());
        writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "33ab5e8d-6ce0-4087-b369-f5d175e6a63c");
        // NOI18N
        bForward.setToolTipText(NbBundle.getMessage(HtmlBrowser.class, "CTL_Forward"));
        writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "94bfdbe2-9ae9-4a49-83d7-3890bf50feea");
        bReload = new JButton();
        writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "fcd24242-a4aa-4491-95e9-d07dd667f39f");
        bReload.setBorder(BorderFactory.createEmptyBorder());
        writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "4f831553-3672-4a47-97d9-aa85adeec1a6");
        bReload.setBorderPainted(false);
        writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "6b05f5bb-fff8-4938-807a-6e720db3e86c");
        bReload.setContentAreaFilled(false);
        writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "7df3fa5e-a990-4720-863c-67c9654d7bfd");
        // NOI18N
        bReload.setIcon(ImageUtilities.loadImageIcon("org/openide/resources/html/refresh.png", true));
        writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "c87c0d18-1dba-4918-803b-158f4829fead");
        // NOI18N
        bReload.setRolloverIcon(ImageUtilities.loadImageIcon("org/openide/resources/html/refresh_hover.png", true));
        writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "8832cbda-fdf4-41de-8de3-4fd3ea7f79b8");
        // NOI18N
        bReload.setDisabledIcon(ImageUtilities.loadImageIcon("org/openide/resources/html/refresh.png", true));
        writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "0142457f-f9a6-4292-ab45-9209b71e0c27");
        bReload.setSelectedIcon(bReload.getIcon());
        writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "6f76a71b-adf8-4edb-b40b-7a69dfb339c2");
        // NOI18N
        bReload.setToolTipText(NbBundle.getMessage(HtmlBrowser.class, "CTL_Reload"));
        writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "60e6fbd6-3264-4ded-9953-e539f740f8e9");
        bReload.setFocusPainted(false);
        writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "59e3094f-f45b-4d03-a837-e498ec029111");
        bStop = new JButton();
        writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "3d49755f-1bf3-4d86-be9f-4a74b345d167");
        bStop.setBorderPainted(false);
        writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "ec40cafe-a363-4381-9a49-c50482f8000b");
        bStop.setBorder(BorderFactory.createEmptyBorder());
        writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "37b23f3c-c23b-4a1b-bf97-6908d7273f70");
        bStop.setContentAreaFilled(false);
        writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "e22375d7-fcbd-4eae-a42a-758c752802b4");
        // NOI18N
        bStop.setIcon(ImageUtilities.loadImageIcon("org/openide/resources/html/stop.png", true));
        writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "d8921a92-b902-48bd-b3aa-f5314d2466de");
        // NOI18N
        bStop.setRolloverIcon(ImageUtilities.loadImageIcon("org/openide/resources/html/stop_hover.png", true));
        writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "d0cb6e6c-3e0c-4d2b-9b1f-3239ce4c4adf");
        // NOI18N
        bStop.setDisabledIcon(ImageUtilities.loadImageIcon("org/openide/resources/html/stop.png", true));
        writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "74a4ea57-4b61-42ab-9800-0f3495498bf7");
        bStop.setSelectedIcon(bStop.getIcon());
        writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "4d86d2b5-e6f3-46f2-8662-fc9e2df6efe6");
        // NOI18N
        bStop.setToolTipText(NbBundle.getMessage(HtmlBrowser.class, "CTL_Stop"));
        writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "76e0df69-01de-467d-bb92-a487709ad77d");
        bStop.setFocusPainted(false);
        writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "2e9fe041-a782-4c0a-98db-11804c72df0b");
        txtLocation = new JTextField();
        writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "1b8b775b-8a2c-4baf-8fc0-47ff837cf06d");
        txtLocation.setEditable(true);
        writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "4ab84dc5-339b-4740-adc6-6ee235a3eb63");
        txtLocation.addMouseListener(new MouseAdapter() {

            @Override
            public void mouseClicked(MouseEvent e) {
                if (e.getClickCount() == 1) {
                    if (null != txtLocation.getSelectedText() || txtLocation.isFocusOwner())
                        return;
                    txtLocation.selectAll();
                }
            }
        });
        writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "73616b7a-bc73-4ae9-8981-1d5f1b94ffb5");
        head.add(bBack, new GridBagConstraints(0, 0, 1, 1, 0.0, 0.0, GridBagConstraints.WEST, GridBagConstraints.NONE, new Insets(0, 0, 0, 1), 0, 0));
        writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "e6c530d6-e70f-4ed4-8eac-0682ddf08f6f");
        head.add(bForward, new GridBagConstraints(1, 0, 1, 1, 0.0, 0.0, GridBagConstraints.WEST, GridBagConstraints.NONE, new Insets(0, 0, 0, 4), 0, 0));
        writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "a47874a4-f723-4a7b-99a0-9be535484a3a");
        head.add(txtLocation, new GridBagConstraints(2, 0, 1, 1, 1.0, 0.0, GridBagConstraints.CENTER, GridBagConstraints.HORIZONTAL, new Insets(0, 0, 0, 4), 0, 0));
        writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "c27d2b5b-0b08-4e2f-8e45-b0254745edc7");
        head.add(bReload, new GridBagConstraints(3, 0, 1, 1, 0.0, 0.0, GridBagConstraints.EAST, GridBagConstraints.NONE, new Insets(0, 0, 0, 4), 0, 0));
        writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "20059a81-91b3-4bb5-8ef9-f4e3515ecfa5");
        head.add(bStop, new GridBagConstraints(4, 0, 1, 1, 0.0, 0.0, GridBagConstraints.EAST, GridBagConstraints.NONE, new Insets(0, 0, 0, 0), 0, 0));
        writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "ae3b8def-da16-4dfb-a926-c7727d456903");
        if (null != extraToolbar) {
            writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "eb19608e-6e95-49aa-9dd2-5b0a9b6ab070");
            head.add(extraToolbar, new GridBagConstraints(0, 1, 5, 1, 1.0, 0.0, GridBagConstraints.WEST, GridBagConstraints.HORIZONTAL, new Insets(3, 0, 0, 0), 0, 0));
        }
        writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "92e9c12f-9c1e-4b8c-9ac7-c0bdfd7539af");
        head.setBorder(BorderFactory.createEmptyBorder(8, 10, null == extraToolbar ? 8 : 3, 10));
        writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "09057c17-4b88-423c-9c57-0bf249be830a");
        if (browserImpl != null) {
            writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "5351d0f1-418d-45cc-bb2e-729c8bfc8230");
            bBack.setEnabled(browserImpl.isBackward());
            writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "c081953f-a8f8-4b6b-8eb6-e7988bdfa448");
            bForward.setEnabled(browserImpl.isForward());
        }
        writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "4ffbc139-3b46-4523-89ba-43a59b505454");
        // add listeners ..................... .............................
        txtLocation.addActionListener(browserListener);
        writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "58836112-580e-4afd-9301-77bd6bb82179");
        bBack.addActionListener(browserListener);
        writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "cfafc21f-6d59-421b-80ac-14a689bafb60");
        bForward.addActionListener(browserListener);
        writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "7420719b-f331-4fb3-87a6-eec04001aeca");
        bReload.addActionListener(browserListener);
        writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "a87718ca-6257-4c2f-b37b-e93e4947c4b4");
        bStop.addActionListener(browserListener);
        writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "5a26140b-5c0d-413d-b2e7-74ce5f714901");
        bBack.getAccessibleContext().setAccessibleName(bBack.getToolTipText());
        writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "c2d32cf8-3ff8-4842-a774-5868f5a64241");
        bForward.getAccessibleContext().setAccessibleName(bForward.getToolTipText());
        writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "f8c8f85f-c77a-4d5c-8c6b-e80ba785a1ad");
        bReload.getAccessibleContext().setAccessibleName(bReload.getToolTipText());
        writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "2fdbd7ce-2faf-47aa-b832-4b2f40b90381");
        bStop.getAccessibleContext().setAccessibleName(bStop.getToolTipText());
        writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "b3805abf-da9a-475c-84d2-edb9906360dd");
        txtLocation.getAccessibleContext().setAccessibleDescription(NbBundle.getMessage(HtmlBrowser.class, "ACSD_HtmlBrowser_Location"));
        writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "5c88c2dc-4244-4f6d-b00b-30f325f1735d");
        add(head, BorderLayout.NORTH);
    }

    /**
     * Default initialization of toolbar.
     */
    private void destroyToolbar() {
        writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "cf85fb9a-b3f8-41aa-99e7-7f3450cda7f0");
        remove(head);
        writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "920abf41-e9b9-45cc-8895-3fea810b2ba2");
        head = null;
        writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "a20cc411-4cfc-4690-9f33-1b1a50de1854");
        toolbarVisible = false;
    }

    /**
     * Default initialization of status line.
     */
    private void initStatusLine() {
        writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "280f660a-27a2-44a5-9858-4e102c69414f");
        statusLineVisible = true;
        writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "d41d9df7-342c-4e7f-b8ab-dbe8bb318a13");
        add(// NOI18N
        lStatusLine = new JLabel(NbBundle.getMessage(HtmlBrowser.class, "CTL_Loading")), // NOI18N
        "South");
        writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "0d91742f-9150-42a9-932e-45a8c228aff7");
        lStatusLine.setLabelFor(this);
    }

    /**
     * Destroyes status line.
     */
    private void destroyStatusLine() {
        writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "5fc89d5a-216c-48d3-a31a-b4e87fa6c4ff");
        remove(lStatusLine);
        writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "4da330f5-55a0-45dd-a785-b418e4cdaa24");
        lStatusLine = null;
        writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "098ac2c9-3c3f-43c9-8418-a3abe6207762");
        statusLineVisible = false;
    }

    // public methods ............................................................
    /**
     * Sets new URL.
     *
     * @param str URL to show in this browser.
     */
    public void setURL(String str) {
        writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "29251228-8134-4126-a64c-65a9cf7d4f15");
        if (null != str) {
            writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "7964608a-1db6-4e25-97fc-a9bd9d4f8f76");
            try {
                writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "f40e10e6-71b5-450d-8e68-d6195dabed06");
                str = new URL(str).toExternalForm();
            } catch (ThreadDeath td) {
                writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "bb640d87-d780-4662-8b35-099db301b357");
                throw td;
            } catch (Throwable e) {
            // ignore
            }
        }
        writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "a7187a54-927f-486f-9da2-aa4e25ff699b");
        browserImpl.setLocation(str);
    }

    /**
     * Sets new URL.
     *
     * @param url URL to show in this browser.
     */
    public void setURL(final URL url) {
        writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "116375ac-ec30-4b46-8710-01cb7647ab7e");
        if (url == null) {
            writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "62802a75-d324-4638-b750-da9cfa29787a");
            txtLocation.setText(null);
            writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "e4f91740-7262-4989-ab2c-03b5a6a6606d");
            return;
        }
        writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "b40f32b3-b9c5-44be-8bae-bf2d72cb0d54");
        rp.post(new URLSetter());
    }

    /**
     * Gets current document url.
     * @return
     */
    public final URL getDocumentURL() {
        writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "4e60f828-847d-4574-ba29-61deb341a12b");
        return browserImpl.getURL();
    }

    /**
     * Enables/disables Home button.
     * @param b
     */
    public final void setEnableHome(boolean b) {
    }

    /**
     * Enables/disables location.
     * @param b
     */
    public final void setEnableLocation(boolean b) {
        writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "7b9d7918-8d41-4c1e-a851-52e5649e3b06");
        txtLocation.setEnabled(b);
        writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "dbdf643d-9d3d-48ff-a2dc-6f7197a3f0ba");
        txtLocation.setVisible(b);
    }

    /**
     * Gets status line state.
     * @return
     */
    public boolean isStatusLineVisible() {
        writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "30488be2-fb00-4e2a-b8b9-83228ad5bbe9");
        return statusLineVisible;
    }

    /**
     * Shows/hides status line.
     * @param v
     */
    public void setStatusLineVisible(boolean v) {
        writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "d23634c3-a37b-44a1-a40f-abac41a03164");
        if (v == statusLineVisible) {
            writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "03ae4133-3aed-4707-aad1-8cd20bf48860");
            return;
        }
        writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "e7444255-b115-4dc6-a7fc-21553768b0f9");
        if (v) {
            writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "c3784616-2380-4262-b7a0-98287ea6e0e4");
            initStatusLine();
        } else {
            writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "7efc9027-859b-4293-8f9c-f9db7b0ac4f9");
            destroyStatusLine();
        }
    }

    /**
     * Gets status toolbar.
     * @return
     */
    public boolean isToolbarVisible() {
        writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "d34da77f-6ed1-4bf2-9274-7b437cfc5d2a");
        return toolbarVisible;
    }

    /**
     * Shows/hides toolbar.
     * @param v
     */
    public void setToolbarVisible(boolean v) {
        writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "42904bc3-45da-4e00-8df0-9f97022a257d");
        if (v == toolbarVisible) {
            writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "4695b939-4670-4aef-8e9a-42d102a0e05f");
            return;
        }
        writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "4afc4b00-d9f0-4bd4-8a71-bd8c151dfe2e");
        if (v) {
            writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "21ddc5dc-9f7d-43a3-a763-2ec715972c40");
            initToolbar();
        } else {
            writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "e9823d00-f40a-42e2-b99c-60830c0d5b34");
            destroyToolbar();
        }
    }

    /**
     * Get the browser implementation.
     * @return the implementation
     * @since org.openide/1 4.27
     */
    public final Impl getBrowserImpl() {
        writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "d6b623e1-627b-43fe-a33a-c3b6f1ae331b");
        return browserImpl;
    }

    /**
     * Get the browser component.
     * @return a component or null
     * @since org.openide/1 4.27
     */
    public final Component getBrowserComponent() {
        writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "b8709660-d56a-4eb6-b995-398105002fde");
        return browserComponent;
    }

    // helper methods .......................................................................
    /**
     * Returns preferred size.
     */
    @Override
    public java.awt.Dimension getPreferredSize() {
        writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "919017ef-346a-4f82-b6bb-7f0b377369f8");
        java.awt.Dimension superPref = super.getPreferredSize();
        writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "a6ba0f99-fd3b-453c-83b5-28ac858f5757");
        return new java.awt.Dimension(Math.max(DEFAULT_WIDTH, superPref.width), Math.max(DEFAULT_HEIGHT, superPref.height));
    }

    /**
     * Show current brower's URL in the location bar combo box.
     */
    private void updateLocationBar() {
        writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "a4cb9f26-7f93-4025-9889-6bb9429ad3ac");
        if (toolbarVisible) {
            writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "b0331175-91a8-4168-b08a-4547ffb4524f");
            ignoreChangeInLocationField = true;
            writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "7e4df7ec-3c0d-41a2-b397-52e75df3a222");
            String url = browserImpl.getLocation();
            writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "6e66572e-eeb8-4823-b000-e235349db9f4");
            txtLocation.setText(url);
            writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "eaff8f28-159f-4c2a-85ff-d60afcd6cb8f");
            ignoreChangeInLocationField = false;
        }
    }

    // //// Accessibility //////
    @Override
    public void requestFocus() {
        writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "702c9e44-d1d7-439c-ba9a-406ffd69cb4c");
        if (browserComponent != null) {
            writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "ca67ac54-72af-4a18-b9fe-1e7460cc6a48");
            boolean ownerFound = false;
            writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "f1b9d2ea-0492-4be9-a50c-bf4806cfa1b0");
            if (browserComponent instanceof JComponent) {
                writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "9dcb05be-b41d-48d7-8a6d-8c83cacde66d");
                ownerFound = ((JComponent) browserComponent).requestDefaultFocus();
            }
            writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "11ef46e9-537d-48b4-8229-e12bc62e8b62");
            if (!ownerFound) {
                writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "8ff12e59-d80b-41ee-9a3e-fda2237e2e2a");
                browserComponent.requestFocus();
            }
        } else {
            writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "51062f5c-d5d7-4c86-93b5-4ecc89f883f9");
            super.requestFocus();
        }
    }

    @Override
    public boolean requestFocusInWindow() {
        writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "ee629d30-2d54-4228-aa12-056025690ecc");
        if (browserComponent != null) {
            writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "3b0889ba-409c-45f0-8cd1-51da66c98cda");
            boolean ownerFound = false;
            writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "953fa63d-ecab-4b51-9c82-084f3aeac56e");
            if (browserComponent instanceof JComponent) {
                writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "a216e689-0044-4308-a5b5-3f51871a83d2");
                ownerFound = ((JComponent) browserComponent).requestDefaultFocus();
            }
            writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "a8678180-6c9c-497f-8a4e-b3ee2e1f2050");
            if (!ownerFound) {
                writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "7d5e356e-d969-4c48-9cd9-12aed4539b36");
                return browserComponent.requestFocusInWindow();
            } else {
                writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "2100acce-61fa-471f-88a4-8bf6d65c8efd");
                return true;
            }
        } else {
            writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "d1d33185-4a8a-45bd-97f5-68c9265a85b6");
            return super.requestFocusInWindow();
        }
    }

    @Override
    public AccessibleContext getAccessibleContext() {
        writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "2ffec95d-4717-434e-855c-a29a917e91ee");
        if (accessibleContext == null) {
            writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "90471486-8adc-4619-bc6c-a8cbeed862cb");
            accessibleContext = new AccessibleHtmlBrowser();
        }
        writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "47c38fc6-1ba8-4caa-b30d-492aae3e6ba4");
        return accessibleContext;
    }

    /**
     * Implementation of BrowerFactory creates new instances of some Browser implementation.
     *
     * @see HtmlBrowser.Impl
     */
    public interface Factory {

        /**
         * Returns a new instance of BrowserImpl implementation.
         * @return
         */
        public Impl createHtmlBrowserImpl();
    }

    /**
     * Listens on changes in HtmlBrowser.Impl and HtmlBrowser visual components.
     */
    private class BrowserListener implements ActionListener, PropertyChangeListener {

        BrowserListener() {
        }

        /**
         * Listens on changes in HtmlBrowser.Impl.
         */
        @Override
        public void propertyChange(final PropertyChangeEvent evt) {
            String property = evt.getPropertyName();
            if (property == null) {
                return;
            }
            if (property.equals(Impl.PROP_URL) || property.equals(Impl.PROP_TITLE)) {
                HtmlBrowser.this.firePropertyChange(evt.getPropertyName(), evt.getOldValue(), evt.getNewValue());
            }
            if (EventQueue.isDispatchThread()) {
                propertyChangeInAWT(evt);
            } else {
                EventQueue.invokeLater(new Runnable() {

                    @Override
                    public void run() {
                        propertyChangeInAWT(evt);
                    }
                });
            }
        }

        private void propertyChangeInAWT(PropertyChangeEvent evt) {
            String property = evt.getPropertyName();
            if (property.equals(Impl.PROP_URL)) {
                updateLocationBar();
            } else if (property.equals(Impl.PROP_STATUS_MESSAGE)) {
                String s = browserImpl.getStatusMessage();
                if ((s == null) || (s.length() < 1)) {
                    s = NbBundle.getMessage(HtmlBrowser.class, "CTL_Document_done");
                }
                if (lStatusLine != null) {
                    lStatusLine.setText(s);
                }
            } else if (property.equals(Impl.PROP_FORWARD) && (bForward != null)) {
                bForward.setEnabled(browserImpl.isForward());
            } else if (property.equals(Impl.PROP_BACKWARD) && (bBack != null)) {
                bBack.setEnabled(browserImpl.isBackward());
            } else if (property.equals(Impl.PROP_LOADING) && (bStop != null)) {
                bStop.setEnabled(((Boolean) evt.getNewValue()).booleanValue());
            }
        }

        /**
         * Listens on changes in HtmlBrowser visual components.
         */
        @Override
        public void actionPerformed(ActionEvent e) {
            if (e.getSource() == txtLocation) {
                // URL manually changed
                if (ignoreChangeInLocationField) {
                    return;
                }
                String txt = txtLocation.getText();
                if (txt == null || txt.length() == 0) {
                    return;
                }
                setURL(txt);
            } else if (e.getSource() == bBack) {
                browserImpl.backward();
            } else if (e.getSource() == bForward) {
                browserImpl.forward();
            } else if (e.getSource() == bReload) {
                updateLocationBar();
                browserImpl.reloadDocument();
            } else if (e.getSource() == bStop) {
                browserImpl.stopLoading();
            }
        }
    }

    /**
     * This interface represents an implementation of html browser used in HtmlBrowser. Each BrowserImpl
     * implementation corresponds with some BrowserFactory implementation.
     */
    public abstract static class Impl {

        /**
         * generated Serialized Version UID
         */
        static final long serialVersionUID = 2912844785502962114L;

        /**
         * The name of property representing status of html browser.
         */
        // NOI18N
        public static final String PROP_STATUS_MESSAGE = "statusMessage";

        /**
         * The name of property representing current URL.
         */
        // NOI18N
        public static final String PROP_URL = "url";

        /**
         * Title property
         */
        // NOI18N
        public static final String PROP_TITLE = "title";

        /**
         * forward property
         */
        // NOI18N
        public static final String PROP_FORWARD = "forward";

        /**
         * backward property name
         */
        // NOI18N
        public static final String PROP_BACKWARD = "backward";

        /**
         * history property name
         */
        // NOI18N
        public static final String PROP_HISTORY = "history";

        // NOI18N
        public static final String PROP_BROWSER_WAS_CLOSED = "browser.was.closed";

        /**
         * Name of boolean property which is fired when the browser is busy loading
         * its content.
         *
         * @since 7.52
         */
        // NOI18N
        public static final String PROP_LOADING = "loading";

        /**
         * Returns visual component of html browser.
         *
         * @return visual component of html browser.
         */
        public abstract java.awt.Component getComponent();

        /**
         * Reloads current html page.
         */
        public abstract void reloadDocument();

        /**
         * Stops loading of current html page.
         */
        public abstract void stopLoading();

        /**
         * Sets current URL.
         *
         * @param url URL to show in the browser.
         */
        public abstract void setURL(URL url);

        /**
         * Returns current URL.
         *
         * @return current URL.
         */
        public abstract URL getURL();

        /**
         * Retrieve current browser location. It doesn't have to be a valid URL,
         * e.g. "about:config".
         * @return Browser location or null.
         * @since 7.13
         */
        public String getLocation() {
            URL url = getURL();
            return null == url ? null : url.toString();
        }

        /**
         * Change current browser location.
         * @param location New location to show in the browser. It doesn't
         * have to be a valid URL, e.g. "about:config" may be accepted as well.
         * @since 7.13
         */
        public void setLocation(String location) {
            URL url;
            try {
                url = new URL(location);
            } catch (MalformedURLException ee) {
                try {
                    // NOI18N
                    url = new URL("http://" + location);
                } catch (MalformedURLException e) {
                    // NOI18N
                    String errorMessage = NbBundle.getMessage(SwingBrowserImpl.class, "FMT_InvalidURL", new Object[] { location });
                    if (this instanceof SwingBrowserImpl) {
                        ((SwingBrowserImpl) this).setStatusText(errorMessage);
                    } else {
                        Logger.getLogger(HtmlBrowser.class.getName()).log(Level.INFO, errorMessage, ee);
                    }
                    return;
                }
            }
            setURL(url);
        }

        /**
         * Returns status message representing status of html browser.
         *
         * @return status message.
         */
        public abstract String getStatusMessage();

        /**
         * Returns title of the displayed page.
         * @return title
         */
        public abstract String getTitle();

        /**
         * Is forward button enabled?
         * @return true if it is
         */
        public abstract boolean isForward();

        /**
         * Moves the browser forward. Failure is ignored.
         */
        public abstract void forward();

        /**
         * Is backward button enabled?
         * @return true if it is
         */
        public abstract boolean isBackward();

        /**
         * Moves the browser forward. Failure is ignored.
         */
        public abstract void backward();

        /**
         * Is history button enabled?
         * @return true if it is
         */
        public abstract boolean isHistory();

        /**
         * Invoked when the history button is pressed.
         */
        public abstract void showHistory();

        /**
         * Adds PropertyChangeListener to this browser.
         *
         * @param l Listener to add.
         */
        public abstract void addPropertyChangeListener(PropertyChangeListener l);

        /**
         * Removes PropertyChangeListener from this browser.
         *
         * @param l Listener to remove.
         */
        public abstract void removePropertyChangeListener(PropertyChangeListener l);

        /**
         * Method invoked by the infrastructure when the browser component is no
         * longer needed. The default implementation does nothing.
         * @since 7.11
         */
        public void dispose() {
        }

        /**
         * The content of this Lookup will be merged into browser's TopComponent Lookup.
         * @return Browser's Lookup
         * @since 7.52
         */
        public Lookup getLookup() {
            return Lookup.EMPTY;
        }
    }

    /**
     * A manager class which can display URLs in the proper way.
     * Might open a selected HTML browser, knows about embedded vs. external
     * browsers, etc.
     * @since 3.14
     */
    public abstract static class URLDisplayer {

        /**
         * Subclass constructor.
         */
        protected URLDisplayer() {
        }

        /**
         * Get the default URL displayer.
         * @return the default instance from lookup
         */
        public static URLDisplayer getDefault() {
            URLDisplayer dflt = Lookup.getDefault().lookup(URLDisplayer.class);
            if (dflt == null) {
                // Fallback.
                dflt = new TrivialURLDisplayer();
            }
            return dflt;
        }

        /**
         * API clients usage: Call this method to display your URL in some browser.
         * Typically for external browsers this method is
         * non-blocking, doesn't wait until page gets displayed. Also, failures
         * are reported using dialog. However note that as there are other
         * implementations of this method, actual behaviour may be different.
         *
         * <p>
         * SPI clients usage: Implement this method to display given URL to the user.
         * </p>
         *
         * @param u the URL to show
         */
        public abstract void showURL(URL u);

        /**
         * Attempts to display given URL in preferred external browser.
         * The default implementation just delegates to showURL(URL).
         * The URL may be still rendered using an internal browser implementation
         * if no external browser is available.
         *
         * @param u the URL to show
         * @since 7.14
         */
        public void showURLExternal(URL u) {
            showURL(u);
        }
    }

    private static final class TrivialURLDisplayer extends URLDisplayer {

        public TrivialURLDisplayer() {
        }

        @Override
        public void showURL(URL u) {
            if (Desktop.isDesktopSupported()) {
                Desktop d = Desktop.getDesktop();
                if (d.isSupported(Desktop.Action.BROWSE)) {
                    try {
                        d.browse(u.toURI());
                        return;
                    } catch (Exception x) {
                        Logger.getLogger(HtmlBrowser.class.getName()).log(Level.INFO, "Showing: " + u, x);
                    }
                }
            }
            // Fallback implementation:
            HtmlBrowser browser = new HtmlBrowser();
            browser.setURL(u);
            JFrame frame = new JFrame();
            frame.setDefaultCloseOperation(WindowConstants.DISPOSE_ON_CLOSE);
            frame.getContentPane().add(browser);
            frame.pack();
            frame.setVisible(true);
        }
    }

    private class AccessibleHtmlBrowser extends JPanel.AccessibleJPanel {

        AccessibleHtmlBrowser() {
        }

        @Override
        public void setAccessibleName(String name) {
            super.setAccessibleName(name);
            if (browserComponent instanceof Accessible) {
                browserComponent.getAccessibleContext().setAccessibleName(name);
            }
        }

        @Override
        public void setAccessibleDescription(String desc) {
            super.setAccessibleDescription(desc);
            if (browserComponent instanceof Accessible) {
                browserComponent.getAccessibleContext().setAccessibleDescription(desc);
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
