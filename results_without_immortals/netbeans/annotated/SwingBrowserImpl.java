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

import java.util.logging.Level;
import java.util.logging.Logger;
import javax.swing.text.EditorKit;
import javax.swing.text.MutableAttributeSet;
import javax.swing.text.html.CSS.Attribute;
import org.openide.util.Mutex;
import org.openide.util.NbBundle;
import org.openide.util.RequestProcessor;
import java.awt.Dimension;
import java.awt.Graphics;
import java.awt.Rectangle;
import java.beans.PropertyChangeEvent;
import java.beans.PropertyChangeListener;
import java.beans.PropertyChangeSupport;
import java.io.FilterInputStream;
import java.io.IOException;
import java.io.InputStream;
import java.net.MalformedURLException;
import java.net.URL;
import java.net.URLConnection;
import java.util.Vector;
import javax.swing.AbstractAction;
import javax.swing.ActionMap;
import javax.swing.JEditorPane;
import javax.swing.JScrollPane;
import javax.swing.SwingConstants;
import javax.swing.SwingUtilities;
import javax.swing.event.HyperlinkEvent;
import javax.swing.event.HyperlinkListener;
import javax.swing.text.AbstractDocument;
import javax.swing.text.DefaultEditorKit;
import javax.swing.text.Document;
import javax.swing.text.html.*;
import java.io.*;

/**
 * Implementation of BrowserImpl in Swing.
 */
final class SwingBrowserImpl extends HtmlBrowser.Impl implements Runnable {

    /**
     * state of history management
     */
    private static final int NO_NAVIGATION = 1;

    private static final int NAVIGATION_BACK = 2;

    private static final int NAVIGATION_FWD = 3;

    // NOI18N
    private static final RequestProcessor rp = new RequestProcessor("Swing Browser");

    /**
     * Current URL.
     */
    private URL url;

    /**
     * URL loaded by JEditorPane
     */
    private URL loadingURL;

    private PropertyChangeSupport pcs;

    // NOI18N
    private String statusMessage = "";

    private SwingBrowser swingBrowser;

    private final JScrollPane scroll;

    /**
     * list of accessed URLs for back/fwd navigation
     */
    private Vector<Object> historyList;

    /**
     * current position in history
     */
    private int historyIndex;

    /**
     * navigation indication
     */
    private int historyNavigating = NO_NAVIGATION;

    private String title = null;

    boolean fetchingTitle = false;

    private static Logger LOG = Logger.getLogger(SwingBrowserImpl.class.getName());

    SwingBrowserImpl() {
        pcs = new PropertyChangeSupport(this);
        swingBrowser = new SwingBrowser();
        scroll = new JScrollPane(swingBrowser);
        historyList = new Vector<Object>(5, 3);
        historyIndex = -1;
        swingBrowser.addPropertyChangeListener(// NOI18N
        "page", new PropertyChangeListener() {

            public void propertyChange(PropertyChangeEvent evt) {
                if (evt.getNewValue() instanceof URL) {
                    URL old = SwingBrowserImpl.this.url;
                    SwingBrowserImpl.this.url = (URL) evt.getNewValue();
                    SwingBrowserImpl.this.pcs.firePropertyChange(PROP_URL, old, url);
                    if (((URL) evt.getNewValue()).equals(loadingURL)) {
                        loadingURL = null;
                    }
                    // update history
                    if (historyNavigating == NAVIGATION_BACK) {
                        int idx = historyList.lastIndexOf(evt.getNewValue(), historyIndex - 1);
                        if (idx != -1) {
                            historyIndex = idx;
                        }
                    } else if (historyNavigating == NAVIGATION_FWD) {
                        int idx = historyList.indexOf(evt.getNewValue(), historyIndex + 1);
                        if (idx != -1) {
                            historyIndex = idx;
                        }
                    } else {
                        while (historyList.size() > (historyIndex + 1)) historyList.remove(historyList.size() - 1);
                        historyList.add(evt.getNewValue());
                        historyIndex = historyList.size() - 1;
                    }
                    historyNavigating = NO_NAVIGATION;
                    pcs.firePropertyChange(PROP_BACKWARD, null, null);
                    pcs.firePropertyChange(PROP_FORWARD, null, null);
                    SwingUtilities.invokeLater(SwingBrowserImpl.this);
                }
            }
        });
    }

    /**
     * Returns visual component of html browser.
     *
     * @return visual component of html browser.
     */
    public java.awt.Component getComponent() {
        writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "8151833c-23e7-48d0-82d3-8791444eae6d");
        return scroll;
    }

    /**
     * Reloads current html page.
     */
    public void reloadDocument() {
        writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "ce41e654-8ef0-4833-848b-7e96880006a3");
        synchronized (rp) {
            writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "f77311ee-48dd-49da-8c92-31b947fd4609");
            try {
                writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "143f402f-f6e1-42a5-a5b7-0971970603e5");
                if ((url == null) || (loadingURL != null)) {
                    writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "23327760-f1c2-497a-a189-6c6106398337");
                    return;
                }
                writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "878e4ca4-7456-4922-84c3-040f44d2a99f");
                Document doc = swingBrowser.getDocument();
                writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "b5d7ada3-a2bf-4117-b117-d175374f382d");
                loadingURL = url;
                writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "2b784900-1d3f-4ba1-94ba-648663497ff1");
                if (doc instanceof AbstractDocument) {
                    writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "06594339-6d6e-47e9-a3ff-92f3b13ef026");
                    String protocol = url.getProtocol();
                    writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "1f0d98f0-e54e-460a-886c-1f71e2f16d4f");
                    if (// NOI18N
                    "ftp".equalsIgnoreCase(protocol) || // NOI18N
                    "http".equalsIgnoreCase(protocol)) {
                        writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "ab6a2e49-a547-4723-a442-3eca00a1c803");
                        ((AbstractDocument) doc).setAsynchronousLoadPriority(Thread.NORM_PRIORITY);
                    } else {
                        writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "527aa535-03b7-457e-ab59-b50ca4c993fa");
                        ((AbstractDocument) doc).setAsynchronousLoadPriority(-1);
                    }
                }
                writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "74384d6b-81e1-406b-bf0c-115e47c8ecd8");
                rp.post(this);
            } catch (Exception e) {
                writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "28669c16-44cf-4deb-818f-565305257e7f");
                LOG.log(Level.WARNING, null, e);
                writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "9b852451-a1d9-4458-93c1-b9d4afc21c25");
                // NOI18N
                pcs.firePropertyChange(PROP_STATUS_MESSAGE, null, statusMessage = "" + e);
            }
        }
    }

    /**
     * Stops loading of current html page.
     */
    public void stopLoading() {
    }

    /**
     * Sets current URL.
     *
     * @param url URL to show in the browser.
     */
    public void setURL(URL url) {
        writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "916c07e7-3585-4cc4-839e-425a3851cbee");
        synchronized (rp) {
            writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "008bd98f-46be-4d44-845d-ce3eddbe7b67");
            try {
                writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "f1b2d348-ca8a-452a-9d26-d6aea0cb3867");
                if (url == null) {
                    writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "f0890cfe-a556-4742-b03b-b702986d3051");
                    return;
                }
                writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "9d69f735-baf8-40cc-9408-3552a485519c");
                loadingURL = url;
                writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "3afb8241-02bd-4b17-9fc6-ebf92b297d13");
                rp.post(this);
            } catch (Exception e) {
                writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "8e11b043-fff7-4221-889f-53ead5d6c190");
                LOG.log(Level.WARNING, null, e);
                writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "8566a647-706c-48d5-a03d-c2d7d051924d");
                // NOI18N
                pcs.firePropertyChange(PROP_STATUS_MESSAGE, null, statusMessage = "" + e);
            }
        }
    }

    /**
     * Returns current URL.
     *
     * @return current URL.
     */
    public URL getURL() {
        writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "170a078e-8557-4b54-b3d1-2799099e5a62");
        return url;
    }

    /**
     * Returns status message representing status of html browser.
     *
     * @return status message.
     */
    public String getStatusMessage() {
        writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "a94a3213-c8d0-4296-af02-a2fc9fd939c5");
        return statusMessage;
    }

    /**
     * Returns title of the displayed page.
     * @return title
     */
    public String getTitle() {
        writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "999b94f8-7810-4488-a1b8-1aabf27fffcc");
        if (title == null) {
            writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "fc48cd75-ba55-4065-b652-979a587ef269");
            Mutex.EVENT.readAccess(this);
        }
        writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "178b762f-ee89-4e43-9a0d-19e7f9e60c29");
        // NOI18N
        return (title == null) ? NbBundle.getMessage(SwingBrowserImpl.class, "LBL_Loading") : title;
    }

    void updateTitle() {
        writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "24daba0c-3eed-4d81-bbc8-97461f8acfac");
        // System.err.println("Update title");
        if (fetchingTitle) {
            writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "415e6862-4dca-404a-bac1-7c3489733175");
            // System.err.println("  ...already updating");
            return;
        }
        writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "cc867b62-6f56-43d2-8c83-217c01db9b60");
        fetchingTitle = true;
        writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "b2c28402-3dd9-4ec5-8cc5-726c2c5aebc5");
        String oldTitle = getTitle();
        writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "ea2d72df-f116-409a-9cdb-54d05bf9efdb");
        try {
            writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "8dd7b8ec-2da9-430e-93f9-d30f3c0170a1");
            Document d = swingBrowser.getDocument();
            writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "a2d445d7-7ef4-471e-a4f9-e38b6bfa4b2f");
            title = (String) d.getProperty(HTMLDocument.TitleProperty);
            writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "3d2b51df-c3a8-4977-a886-958d8e5f2a57");
            // System.err.println("Title from document is " + title);
            if ((title == null) || (title.trim().length() == 0)) {
                writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "85cc4028-2f94-4838-b4af-13d139af014e");
                // System.err.println("No title from document, trying from url ");
                URL u = getURL();
                writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "eede5c8d-e9cb-4a2a-9f42-a7df6e152703");
                if (u != null) {
                    // System.err.println("Using from url: " + title);
                    writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "abd93518-ae4b-4121-a353-84667d6de896");
                    title = u.getFile();
                    writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "b0d2850e-e524-4a24-8fc9-a807d25189cc");
                    if (title.length() == 0) {
                        writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "e890c3be-42a7-4d66-be32-0f9260b5c295");
                        // NOI18N
                        title = NbBundle.getMessage(SwingBrowserImpl.class, "LBL_Untitled");
                    } else {
                        writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "eb910a48-f1bc-4f17-898d-36c97a7865cd");
                        // Trim any extraneous path info
                        // NOI18N
                        int i = title.lastIndexOf("/");
                        writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "93885bc3-d603-4e40-91e0-eca739b306ba");
                        if ((i != -1) && (i != (title.length() - 1))) {
                            writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "cb5f59de-b7c6-44d6-a647-7bca564a60dc");
                            title = title.substring(i + 1);
                        }
                    }
                }
            }
            writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "2f765642-8493-423e-ac6b-3e74cc450bef");
            if (title != null) {
                writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "2e556af0-3e5d-47d7-ae0a-56958a1e8b72");
                if (title.length() > 60) {
                    writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "ea43025b-af9c-4bc7-b51d-fc561ca01349");
                    // Truncate to a reasonable tab length
                    title = NbBundle.getMessage(SwingBrowserImpl.class, "LBL_Title", new Object[] { title.substring(0, 57) });
                }
                writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "185516d8-e0cc-4fa5-a932-36fb6d33411f");
                if (!oldTitle.equals(title)) {
                    writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "a8219610-1773-4339-a0cc-5ad328e849ef");
                    // System.err.println("Firing prop change from " + oldTitle + " to " + title);
                    // NOI18N
                    pcs.firePropertyChange(PROP_TITLE, oldTitle, title);
                }
            }
        } finally {
            writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "72fead2c-fddf-4224-b170-bd325bf644cf");
            fetchingTitle = false;
        }
    }

    public void run() {
        writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "28319f54-863c-4615-a53b-cba32d166f06");
        if (SwingUtilities.isEventDispatchThread()) {
            writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "11376acb-459c-4115-8155-8b6e4e776c07");
            title = null;
            writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "ece30f43-75cb-4265-85a1-1412cd54e843");
            updateTitle();
        } else {
            writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "6e311cf0-c580-4f66-84d2-d12a0eec78dc");
            URL requestedURL;
            writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "b7da60fd-2cf5-4087-bd45-369687a77178");
            synchronized (rp) {
                writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "850a4e79-4dfa-4a27-810d-73ab71a00655");
                if ((this.url != null) && this.url.sameFile(url)) {
                    writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "1321f6e4-65c4-4e5e-9c9d-5de5aeb288ba");
                    Document doc = swingBrowser.getDocument();
                    writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "cd32db77-b9f2-45fc-8c0e-be7b14982b68");
                    if (doc != null) {
                        writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "d9c58173-25eb-4e62-be1d-172344f42f01");
                        // force reload
                        doc.putProperty(Document.StreamDescriptionProperty, null);
                    }
                }
                writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "78d97427-f025-447e-b284-51cbf4158de7");
                requestedURL = loadingURL;
                writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "13554ed5-b196-4cc1-a8d8-2b77bd56964b");
                loadingURL = null;
            }
            writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "dbb03f3e-77e8-49dc-b964-d100cd951069");
            try {
                writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "332c9eb0-0380-4700-a319-fb21d6a1dd46");
                swingBrowser.setPage(requestedURL);
                writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "b14cb2cc-c974-4390-a5dc-fc2f360d80d0");
                setStatusText(null);
            } catch (java.net.UnknownHostException uhe) {
                // NOI18N
                writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "f223c583-7add-4638-a8b9-bd7a3944e207");
                setStatusText(NbBundle.getMessage(SwingBrowserImpl.class, "FMT_UnknownHost", new Object[] { requestedURL }));
            } catch (java.net.NoRouteToHostException nrthe) {
                // NOI18N
                writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "c24715c4-7e6c-40ea-a7e9-75ba23076ef8");
                setStatusText(NbBundle.getMessage(SwingBrowserImpl.class, "FMT_NoRouteToHost", new Object[] { requestedURL }));
            } catch (IOException ioe) {
                // NOI18N
                writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "20bca739-d2bb-4ce7-86ff-4000909ca0a8");
                setStatusText(NbBundle.getMessage(SwingBrowserImpl.class, "FMT_InvalidURL", new Object[] { requestedURL }));
            }
            writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "ef64abf7-b3ab-4f6e-a795-5b13aadb6836");
            SwingUtilities.invokeLater(this);
        }
    }

    /**
     * Accessor to allow a message about bad urls to be displayed - see
     * HtmlBrowser.setURL().
     */
    void setStatusText(String s) {
        writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "4115311e-edf3-44f8-a93b-9ab7df53503b");
        // NOI18N
        pcs.firePropertyChange(PROP_STATUS_MESSAGE, null, statusMessage = s);
    }

    /**
     * Is forward button enabled?
     * @return true if it is
     */
    public boolean isForward() {
        writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "f46c197b-803b-4a2e-95ce-7268c8496b76");
        return (historyIndex >= 0) && (historyIndex < (historyList.size() - 1)) && (historyNavigating == NO_NAVIGATION);
    }

    /**
     * Moves the browser forward. Failure is ignored.
     */
    public void forward() {
        writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "083c12d7-cb9a-436a-a3c2-40d92e882d53");
        if (isForward()) {
            writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "032ec379-e161-4643-988c-8bf78f3f5db9");
            historyNavigating = NAVIGATION_FWD;
            writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "90f4e73e-03c5-40c2-b3dc-3643a018cff0");
            setURL((URL) historyList.elementAt(historyIndex + 1));
        }
    }

    /**
     * Is backward button enabled?
     * @return true if it is
     */
    public boolean isBackward() {
        writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "54b9f47f-f047-4d41-bd1b-ea2d3ac2cc79");
        return (historyIndex > 0) && (historyIndex < historyList.size()) && (historyNavigating == NO_NAVIGATION);
    }

    /**
     * Moves the browser forward. Failure is ignored.
     */
    public void backward() {
        writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "11da47a2-4aa0-4850-9087-3ff1cbe0ff3b");
        if (isBackward()) {
            writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "b8619c31-3b3c-4527-ba8f-6f65f307048f");
            historyNavigating = NAVIGATION_BACK;
            writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "713f379a-0c33-4a34-88b6-61991561d798");
            setURL((URL) historyList.elementAt(historyIndex - 1));
        }
    }

    /**
     * Is history button enabled?
     * @return true if it is
     */
    public boolean isHistory() {
        writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "c42c9967-51bb-4588-aba3-b8e422ec3188");
        return false;
    }

    /**
     * Invoked when the history button is pressed.
     */
    public void showHistory() {
    }

    /**
     * Adds PropertyChangeListener to this browser.
     *
     * @param l Listener to add.
     */
    public void addPropertyChangeListener(PropertyChangeListener l) {
        writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "3670a5e1-0757-4ab6-9da5-e15947674e39");
        pcs.addPropertyChangeListener(l);
    }

    /**
     * Removes PropertyChangeListener from this browser.
     *
     * @param l Listener to remove.
     */
    public void removePropertyChangeListener(PropertyChangeListener l) {
        writeline("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "deae41f3-2aeb-4e46-a5bf-ca0725488bea");
        pcs.removePropertyChangeListener(l);
    }

    // encoding support; copied from html/HtmlEditorSupport
    private static String findEncodingFromURL(InputStream stream) {
        writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "cf995f34-f091-428d-acd3-ecea10e1262f");
        try {
            writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "3885b3f9-1585-42ab-a32b-0c85f2bfd9db");
            byte[] arr = new byte[4096];
            writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "d7f42b02-ade7-4ec5-be3b-62de2c6d9432");
            int len = stream.read(arr, 0, arr.length);
            writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "b7b4fa33-7619-4042-a26d-61ded54606e6");
            String txt = new String(arr, 0, (len >= 0) ? len : 0).toUpperCase();
            writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "3fc10a01-08ce-448f-9e01-1d16f4711e24");
            // encoding
            return findEncoding(txt);
        } catch (Exception x) {
            writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "17e4b026-fd89-491c-976f-a8dcc9d05448");
            x.printStackTrace();
        }
        writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "97a94888-a265-405c-ab85-e533768418e3");
        return null;
    }

    /**
     * Tries to guess the mime type from given input stream. Tries to find
     * <em>&lt;meta http-equiv="Content-Type" content="text/html; charset=iso-8859-1"&gt;</em>
     * @param txt the string to search in (should be in upper case)
     * @return the encoding or null if no has been found
     */
    private static String findEncoding(String txt) {
        writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "4ca1ee85-cb23-4bad-b801-0408e141d5f9");
        // NOI18N
        int headLen = txt.indexOf("</HEAD>");
        writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "d9ab17b4-9f39-47b6-978c-1a243de10f95");
        if (headLen == -1) {
            writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "39f70900-bb01-4382-afb2-e2abfc1c97b0");
            headLen = txt.length();
        }
        writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "40715c5a-1a5d-40e3-b079-f343ecef5b02");
        // NOI18N
        int content = txt.indexOf("CONTENT-TYPE");
        writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "334188d1-b85c-44d3-95b3-3bcc1019511a");
        if ((content == -1) || (content > headLen)) {
            writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "c98318e7-e785-4489-a4a0-1d5d4deafdf8");
            return null;
        }
        writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "bc0c8080-6ea4-43bf-838d-1c588e5bc78a");
        // NOI18N
        int charset = txt.indexOf("CHARSET=", content);
        writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "e74151db-3c2c-401d-8b0c-9c597c706878");
        if (charset == -1) {
            writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "19274906-5a11-460a-873a-21c0b130c672");
            return null;
        }
        writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "225fa7ec-ab0b-4758-9d6b-bdb766a8d3dd");
        int charend = txt.indexOf('"', charset);
        writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "7fac83e3-e7e2-453f-b3e7-4975e0cd69e8");
        int charend2 = txt.indexOf('\'', charset);
        writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "4ec5cb38-f216-4cf3-95c9-c4231f184ea3");
        if ((charend == -1) && (charend2 == -1)) {
            writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "ef4a143b-ee18-4d53-8e3f-7db41b1b58bf");
            return null;
        }
        writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "e1c30cc5-0c4c-4f2f-8ea3-9bcaf49b9e22");
        if (charend2 != -1) {
            writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "47567fe3-9d99-4575-9d21-8d71b8d7e0cd");
            if ((charend == -1) || (charend > charend2)) {
                writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "3e1fd3c0-6001-46ca-b0ca-eb9d7730efa9");
                charend = charend2;
            }
        }
        writelineStatic("/home/ubuntu/results/netbeans/coverage/netbeans.coverage", "8a759888-9300-4633-b5c4-1dd853b681c1");
        // NOI18N
        return txt.substring(charset + "CHARSET=".length(), charend);
    }

    // innerclasses ..............................................................
    private class SwingBrowser extends JEditorPane {

        private boolean lastPaintException = false;

        private SwingBrowser() {
            setEditable(false);
            addHyperlinkListener(new HyperlinkListener() {

                public void hyperlinkUpdate(HyperlinkEvent e) {
                    if (e.getEventType() == HyperlinkEvent.EventType.ACTIVATED) {
                        if (e instanceof HTMLFrameHyperlinkEvent) {
                            HTMLFrameHyperlinkEvent evt = (HTMLFrameHyperlinkEvent) e;
                            HTMLDocument doc = (HTMLDocument) getDocument();
                            URL old = getURL();
                            doc.processHTMLFrameHyperlinkEvent(evt);
                            pcs.firePropertyChange(PROP_URL, old, e.getURL());
                        } else {
                            try {
                                SwingBrowserImpl.this.setURL(e.getURL());
                            } catch (Exception ex) {
                                LOG.log(Level.WARNING, null, ex);
                            }
                        }
                    }
                }
            });
            // when up/down arrow keys are pressed, ensure the whole browser content
            // scrolls up/down instead of moving the caret position only
            ActionMap actionMap = getActionMap();
            actionMap.put(DefaultEditorKit.upAction, new ScrollAction(-1));
            actionMap.put(DefaultEditorKit.downAction, new ScrollAction(1));
        }

        @Override
        public EditorKit getEditorKitForContentType(String type) {
            if ("text/html".equals(type)) {
                return new HTMLEditorKit() {

                    @Override
                    public Document createDefaultDocument() {
                        StyleSheet styles = getStyleSheet();
                        // #200472 - hack to make JDK 1.7 javadoc readable
                        StyleSheet ss = new FilteredStyleSheet();
                        ss.addStyleSheet(styles);
                        HTMLDocument doc = new HTMLDocument(ss);
                        doc.setParser(getParser());
                        doc.setAsynchronousLoadPriority(4);
                        doc.setTokenThreshold(100);
                        return doc;
                    }
                };
            }
            return super.getEditorKitForContentType(type);
        }

        /**
         * Fetches a stream for the given URL, which is about to
         * be loaded by the <code>setPage</code> method.
         * This method is expected to have the the side effect of
         * establishing the content type, and therefore setting the
         * appropriate <code>EditorKit</code> to use for loading the stream.
         * <p>
         * If debugger is not running returns super implementation.
         * <p>
         * If debugger runs it will set content type to text/html.
         * Forwarding is not supported is that case.
         * <p>Control using sysprop org.openide.awt.SwingBrowserImpl.do-not-block-awt=true.
         *
         * @param page  the URL of the page
         */
        @Override
        protected InputStream getStream(URL page) throws IOException {
            SwingUtilities.invokeLater(SwingBrowserImpl.this);
            try {
                // #53207: pre-read encoding from loaded URL
                String charset = findEncodingFromURL(page.openStream());
                // NOI18N
                LOG.log(Level.FINE, "Url " + page + " has charset " + charset);
                if (charset != null) {
                    putClientProperty("charset", charset);
                }
            } catch (IllegalArgumentException iaE) {
                // #165266 - empty url
                MalformedURLException e = new MalformedURLException();
                e.initCause(iaE);
                throw e;
            }
            // XXX debugger ought to set this temporarily
            if (Boolean.getBoolean("org.openide.awt.SwingBrowserImpl.do-not-block-awt")) {
                // try to set contentType quickly and return (don't block AWT Thread)
                // NOI18N
                setContentType("text/html");
                return new FilteredInputStream(page.openConnection(), SwingBrowserImpl.this);
            } else {
                return super.getStream(page);
            }
        }

        @Override
        public Dimension getPreferredSize() {
            try {
                return super.getPreferredSize();
            } catch (RuntimeException e) {
                // Bug in javax.swing.text.html.BlockView
                return new Dimension(400, 600);
            }
        }

        @Override
        public void paint(Graphics g) {
            try {
                super.paint(g);
                lastPaintException = false;
            } catch (RuntimeException e) {
                // do nothing
                if (!lastPaintException) {
                    repaint();
                }
                lastPaintException = true;
            }
        }

        @Override
        public void scrollToReference(String reference) {
            if (!isShowing() || null == getParent() || getWidth() < 1 || getHeight() < 1)
                return;
            super.scrollToReference(reference);
        }

        @Override
        @Deprecated
        public void layout() {
            try {
                super.layout();
            } catch (ArrayIndexOutOfBoundsException aioobE) {
                // HACK - workaround for issue #168988
                StackTraceElement[] stack = aioobE.getStackTrace();
                if (stack.length > 0 && stack[0].getClassName().endsWith("BoxView")) {
                    // NOI18N
                    Logger.getLogger(SwingBrowser.class.getName()).log(Level.INFO, null, aioobE);
                } else {
                    throw aioobE;
                }
            }
        }

        /**
         * An action to scroll the browser content up or down.
         */
        private class ScrollAction extends AbstractAction {

            int direction;

            public ScrollAction(int direction) {
                this.direction = direction;
            }

            public void actionPerformed(java.awt.event.ActionEvent e) {
                Rectangle r = getVisibleRect();
                int increment = getScrollableUnitIncrement(r, SwingConstants.VERTICAL, direction);
                r.y += (increment * direction);
                scrollRectToVisible(r);
            }
        }
    }

    /**
     * FilterInputStream that delays opening of stream.
     * The purpose is not to initialize the stream when it is created in getStream()
     * but to do it later when the content is asynchronously loaded in separate thread.
     */
    private static class FilteredInputStream extends FilterInputStream {

        private final URLConnection conn;

        private final SwingBrowserImpl browser;

        FilteredInputStream(URLConnection conn, SwingBrowserImpl browser) {
            super((FilterInputStream) null);
            this.conn = conn;
            this.browser = browser;
        }

        private synchronized void openStream() throws IOException {
            if (in == null) {
                in = conn.getInputStream();
            }
        }

        @Override
        public int available() throws IOException {
            openStream();
            return super.available();
        }

        @Override
        public long skip(long n) throws IOException {
            openStream();
            return super.skip(n);
        }

        @Override
        public void reset() throws IOException {
            openStream();
            super.reset();
        }

        @Override
        public void close() throws IOException {
            openStream();
            super.close();
            Mutex.EVENT.readAccess(browser);
        }

        @Override
        public int read(byte[] b) throws IOException {
            openStream();
            return super.read(b);
        }

        @Override
        public int read(byte[] b, int off, int len) throws IOException {
            openStream();
            return super.read(b, off, len);
        }

        @Override
        public int read() throws IOException {
            openStream();
            return super.read();
        }
    }

    private static class FilteredStyleSheet extends StyleSheet {

        @Override
        public void addCSSAttribute(MutableAttributeSet attr, Attribute key, String value) {
            value = fixFontSize(key, value);
            super.addCSSAttribute(attr, key, value);
        }

        @Override
        public boolean addCSSAttributeFromHTML(MutableAttributeSet attr, Attribute key, String value) {
            value = fixFontSize(key, value);
            return super.addCSSAttributeFromHTML(attr, key, value);
        }

        /**
         * CSS with e.g. 'font-size: 75%' makes the HTML unreadable in the default JEditorPane
         * @param key
         * @param value
         * @return
         */
        private static String fixFontSize(Attribute key, String value) {
            if ("font-size".equals(key.toString()) && null != value && value.endsWith("%")) {
                String strPercentage = value.replace("%", "");
                int percentage = Integer.parseInt(strPercentage);
                if (percentage < 100) {
                    value = "100%";
                }
            }
            return value;
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
